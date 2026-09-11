use crate::{
    BindId, CFlag, Event, ExecCtx, PrintFlag, Rt, Scope, Tag, TagValue, UserEvent,
    env::Env,
    expr::{ExprId, Origin, Pattern, StructurePattern},
    format_with_flags,
    node::{Held, compiler},
    typ::{AbstractId, IsAFlags, Type, TypeRef},
};
use ahash::AHashMap;
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::BytesMut;
use combine::stream::position::SourcePosition;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Typ, Value};
use smallvec::{SmallVec, smallvec};
use std::fmt::Debug;
use triomphe::Arc;

/// The three shapes the exact-length slice pattern compiles against:
/// a tuple (fixed arity), an array, or the native List.
#[derive(Debug, Clone, Copy, PartialEq, Eq, netidx_derive::Pack)]
#[pack(unwrapped)]
pub enum SliceKind {
    Tuple,
    Array,
    List,
}

#[derive(Debug)]
pub enum StructPatternNode {
    Ignore,
    Literal(Value),
    Bind(BindId),
    Slice {
        kind: SliceKind,
        all: Option<BindId>,
        binds: Box<[StructPatternNode]>,
    },
    SlicePrefix {
        /// true = the native-list prefix form `[<h, rest..>]`; `tail`
        /// binds the tail as a List, sharing structure.
        list: bool,
        all: Option<BindId>,
        prefix: Box<[StructPatternNode]>,
        tail: Option<BindId>,
    },
    SliceSuffix {
        all: Option<BindId>,
        head: Option<BindId>,
        suffix: Box<[StructPatternNode]>,
    },
    Struct {
        all: Option<BindId>,
        binds: Box<[(ArcStr, usize, StructPatternNode)]>,
    },
    Variant {
        tag: ArcStr,
        all: Option<BindId>,
        binds: Box<[StructPatternNode]>,
    },
    /// `T(p)`: a value of the abstract type `id`, its payload
    /// destructured by `bind` against `rep` (the type's
    /// representation at this scrutinee's parameters)
    Abstract {
        id: AbstractId,
        all: Option<BindId>,
        rep: Type,
        bind: Box<StructPatternNode>,
    },
    /// Or-alternatives share alternative 0's BindIds, so the id walks
    /// (`ids`, `unbind`, `delete`) visit alternative 0 only; `is_match`
    /// is any-of and `bind` delivers the first matching alternative.
    Or {
        alts: Box<[StructPatternNode]>,
    },
}

/// How pattern-leaf names bind during compile: `Fresh` allocates;
/// `Record` allocates and records `name → (id, type)` (an or-pattern's
/// first alternative); `Reuse` looks the id up instead of allocating and
/// adds nothing to the env. A reused payload leaf must have exactly the
/// first alternative's type (open cells unify); a reused `@`-capture
/// widens to the union of the alternatives' types.
enum BindMode<'a> {
    Fresh,
    Record(&'a mut AHashMap<ArcStr, (BindId, Type)>),
    Reuse(&'a mut AHashMap<ArcStr, (BindId, Type)>),
}

impl BindMode<'_> {
    fn reborrow(&mut self) -> BindMode<'_> {
        match self {
            Self::Fresh => BindMode::Fresh,
            Self::Record(m) => BindMode::Record(&mut **m),
            Self::Reuse(m) => BindMode::Reuse(&mut **m),
        }
    }
}

fn leaf_bind<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    scope: &Scope,
    name: &ArcStr,
    typ: &Type,
    pos: SourcePosition,
    ori: &Arc<Origin>,
    mode: &mut BindMode,
    capture: bool,
) -> Result<BindId> {
    match mode {
        BindMode::Fresh => Ok(ctx
            .env
            .bind_variable(&scope.lexical, name, typ.clone(), pos, ori.clone())
            .id),
        BindMode::Record(map) => {
            let id = ctx
                .env
                .bind_variable(&scope.lexical, name, typ.clone(), pos, ori.clone())
                .id;
            map.insert(name.clone(), (id, typ.clone()));
            Ok(id)
        }
        BindMode::Reuse(map) => match map.get(name) {
            Some((id, t0)) => {
                let id = *id;
                if !(t0.contains(&ctx.env, typ)? && typ.contains(&ctx.env, t0)?) {
                    if !capture {
                        format_with_flags(PrintFlag::DerefTVars, || {
                            bail!(
                                "or-pattern alternatives must bind {name} at exactly \
                                 equal types (first alternative: {t0}, here: {typ})"
                            )
                        })?
                    }
                    // widen both the recorded entry and the env binding
                    let u = Type::union(&ctx.env, &[t0, typ])?;
                    map.insert(name.clone(), (id, u.clone()));
                    if let Some(b) = ctx.env.by_id.get(&id) {
                        let mut b = b.clone();
                        b.typ = u;
                        ctx.env.by_id.insert_cow(id, b);
                    }
                }
                Ok(id)
            }
            None => bail!(
                "or-pattern alternatives must bind the same names ({name} is \
                 not bound by the first alternative)"
            ),
        },
    }
}

impl StructPatternNode {
    /// Re-derive the struct binders' field indexes from a completed
    /// type predicate: a partial pattern compiles against the fields it
    /// names, so its indexes are wrong once the select typecheck
    /// completes the predicate from the scrutinee.
    pub(super) fn realign(&mut self, env: &Env, typ: &Type) -> Result<()> {
        match self {
            Self::Ignore | Self::Literal(_) | Self::Bind(_) => Ok(()),
            Self::Or { alts } => {
                let ts = typ.with_deref(|t| match t {
                    Some(Type::Set(ts)) if ts.len() == alts.len() => Some(ts.clone()),
                    _ => None,
                });
                match ts {
                    Some(ts) => {
                        for (a, t) in alts.iter_mut().zip(ts.iter()) {
                            a.realign(env, t)?
                        }
                    }
                    None => {
                        for a in alts.iter_mut() {
                            a.realign(env, typ)?
                        }
                    }
                }
                Ok(())
            }
            Self::Struct { binds, all: _ } => {
                let elts = typ.with_deref(|t| match t {
                    Some(t @ Type::Ref(_)) => {
                        t.lookup_ref(env).ok().and_then(|t| match t {
                            Type::Struct(elts) => Some(elts.clone()),
                            _ => None,
                        })
                    }
                    Some(Type::Struct(elts)) => Some(elts.clone()),
                    _ => None,
                });
                let elts = match elts {
                    Some(elts) => elts,
                    None => return Ok(()),
                };
                for (name, index, sub) in binds.iter_mut() {
                    match elts.iter().position(|(n, _)| n == name) {
                        Some(i) => {
                            *index = i;
                            sub.realign(env, &elts[i].1)?
                        }
                        None => bail!("no such struct field {name} in {typ}"),
                    }
                }
                Ok(())
            }
            Self::Variant { binds, all: _, tag: _ } => {
                let ts = typ.with_deref(|t| match t {
                    Some(Type::Variant(_, ts)) => Some(ts.clone()),
                    _ => None,
                });
                if let Some(ts) = ts {
                    if ts.len() == binds.len() {
                        for (b, t) in binds.iter_mut().zip(ts.iter()) {
                            b.realign(env, t)?
                        }
                    }
                }
                Ok(())
            }
            Self::Abstract { bind, rep, .. } => {
                let rep = rep.clone();
                bind.realign(env, &rep)
            }
            Self::Slice { kind: SliceKind::Tuple, binds, all: _ } => {
                let ts = typ.with_deref(|t| match t {
                    Some(Type::Tuple(ts)) => Some(ts.clone()),
                    _ => None,
                });
                if let Some(ts) = ts {
                    if ts.len() == binds.len() {
                        for (b, t) in binds.iter_mut().zip(ts.iter()) {
                            b.realign(env, t)?
                        }
                    }
                }
                Ok(())
            }
            Self::Slice { kind: SliceKind::Array, binds, all: _ }
            | Self::SlicePrefix { list: false, prefix: binds, all: _, .. }
            | Self::SliceSuffix { suffix: binds, all: _, .. } => {
                let et = typ.with_deref(|t| match t {
                    Some(Type::Array(et)) => Some(et.clone()),
                    _ => None,
                });
                if let Some(et) = et {
                    for b in binds.iter_mut() {
                        b.realign(env, &et)?
                    }
                }
                Ok(())
            }
            Self::Slice { kind: SliceKind::List, binds, all: _ }
            | Self::SlicePrefix { list: true, prefix: binds, all: _, .. } => {
                let et = typ.with_deref(|t| match t {
                    Some(Type::List(et)) => Some(et.clone()),
                    _ => None,
                });
                if let Some(et) = et {
                    for b in binds.iter_mut() {
                        b.realign(env, &et)?
                    }
                }
                Ok(())
            }
        }
    }

    pub fn compile<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        type_predicate: &Type,
        spec: &StructurePattern,
        scope: &Scope,
        pos: SourcePosition,
        ori: Arc<Origin>,
    ) -> Result<Self> {
        if !spec.binds_uniq() {
            bail!("bound variables must have unique names")
        }
        Self::compile_int(ctx, type_predicate, spec, scope, pos, ori, BindMode::Fresh)
    }

    fn compile_int<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        type_predicate: &Type,
        spec: &StructurePattern,
        scope: &Scope,
        pos: SourcePosition,
        ori: Arc<Origin>,
        mode: BindMode,
    ) -> Result<Self> {
        crate::stack::ensure_sufficient(|| {
            Self::compile_int_inner(ctx, type_predicate, spec, scope, pos, ori, mode)
        })
    }

    fn compile_int_inner<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        type_predicate: &Type,
        spec: &StructurePattern,
        scope: &Scope,
        pos: SourcePosition,
        ori: Arc<Origin>,
        mut mode: BindMode,
    ) -> Result<Self> {
        macro_rules! with_pref_suf {
            ($list:expr, $all:expr, $single:expr, $multi:expr) => {{
                let want = if $list {
                    Type::List(Arc::new(Type::empty_tvar()))
                } else {
                    Type::Array(Arc::new(Type::empty_tvar()))
                };
                type_predicate.check_contains(&ctx.env, &want)?;
                match &type_predicate.with_deref(|t| match t {
                    Some(Type::Array(et)) if !$list => Some(et.clone()),
                    Some(Type::List(et)) if $list => Some(et.clone()),
                    _ => None,
                }) {
                    Some(et) => {
                        let all = match $all.as_ref() {
                            None => None,
                            Some(n) => Some(leaf_bind(
                                ctx,
                                scope,
                                n,
                                type_predicate,
                                pos,
                                &ori,
                                &mut mode,
                                true,
                            )?),
                        };
                        let single = match $single.as_ref() {
                            None => None,
                            Some(n) => Some(leaf_bind(
                                ctx,
                                scope,
                                n,
                                type_predicate,
                                pos,
                                &ori,
                                &mut mode,
                                false,
                            )?),
                        };
                        let multi = $multi
                            .iter()
                            .map(|n| {
                                Self::compile_int(
                                    ctx,
                                    et,
                                    n,
                                    scope,
                                    pos,
                                    ori.clone(),
                                    mode.reborrow(),
                                )
                            })
                            .collect::<Result<Box<[Self]>>>()?;
                        (all, single, multi)
                    }
                    _ => format_with_flags(PrintFlag::DerefTVars, || {
                        bail!("slice patterns can't match {type_predicate}")
                    })?,
                }
            }};
        }
        let type_predicate = match type_predicate {
            Type::Ref(TypeRef { .. }) => type_predicate.lookup_ref(&ctx.env)?,
            t => t.clone(),
        };
        let type_predicate = &type_predicate;
        let t = match &spec {
            StructurePattern::Or(alts) => {
                if alts.len() < 2 {
                    bail!("an or-pattern needs at least two alternatives")
                }
                let mut names0: SmallVec<[&ArcStr; 8]> = smallvec![];
                alts[0].with_names(&mut |n| names0.push(n));
                names0.sort();
                for alt in &alts[1..] {
                    let mut names: SmallVec<[&ArcStr; 8]> = smallvec![];
                    alt.with_names(&mut |n| names.push(n));
                    names.sort();
                    let mut d = names.clone();
                    d.dedup();
                    if d.len() != names.len() {
                        bail!("bound variables must have unique names")
                    }
                    if names != names0 {
                        bail!("or-pattern alternatives must bind the same names")
                    }
                }
                for (i, alt) in alts.iter().enumerate() {
                    if alts[..i].iter().any(|prev| prev == alt) {
                        bail!(
                            "unreachable or-pattern alternative: duplicate of an \
                             earlier alternative"
                        )
                    }
                }
                // each alternative compiles against its own member of the
                // inferred predicate; under an explicit `T as p1 | p2`
                // every alternative checks against T
                let alt_types: Option<Arc<[Type]>> =
                    type_predicate.with_deref(|t| match t {
                        Some(Type::Set(ts)) if ts.len() == alts.len() => Some(ts.clone()),
                        _ => None,
                    });
                let alt_type = |i: usize| {
                    alt_types.as_ref().map(|ts| &ts[i]).unwrap_or(type_predicate)
                };
                let compiled = match mode.reborrow() {
                    BindMode::Reuse(m) => {
                        let mut out = Vec::with_capacity(alts.len());
                        for (i, alt) in alts.iter().enumerate() {
                            out.push(Self::compile_int(
                                ctx,
                                alt_type(i),
                                alt,
                                scope,
                                pos,
                                ori.clone(),
                                BindMode::Reuse(m),
                            )?)
                        }
                        out.into_boxed_slice()
                    }
                    BindMode::Fresh => {
                        let mut map = AHashMap::default();
                        let mut out = Vec::with_capacity(alts.len());
                        out.push(Self::compile_int(
                            ctx,
                            alt_type(0),
                            &alts[0],
                            scope,
                            pos,
                            ori.clone(),
                            BindMode::Record(&mut map),
                        )?);
                        for (i, alt) in alts.iter().enumerate().skip(1) {
                            out.push(Self::compile_int(
                                ctx,
                                alt_type(i),
                                alt,
                                scope,
                                pos,
                                ori.clone(),
                                BindMode::Reuse(&mut map),
                            )?)
                        }
                        out.into_boxed_slice()
                    }
                    BindMode::Record(map) => {
                        let mut out = Vec::with_capacity(alts.len());
                        out.push(Self::compile_int(
                            ctx,
                            alt_type(0),
                            &alts[0],
                            scope,
                            pos,
                            ori.clone(),
                            BindMode::Record(&mut *map),
                        )?);
                        for (i, alt) in alts.iter().enumerate().skip(1) {
                            out.push(Self::compile_int(
                                ctx,
                                alt_type(i),
                                alt,
                                scope,
                                pos,
                                ori.clone(),
                                BindMode::Reuse(&mut *map),
                            )?)
                        }
                        out.into_boxed_slice()
                    }
                };
                for i in 1..compiled.len() {
                    if compiled[..i].iter().any(|p| p.matches_anything()) {
                        bail!(
                            "unreachable or-pattern alternative: an earlier \
                             alternative already matches anything"
                        )
                    }
                }
                Self::Or { alts: compiled }
            }
            StructurePattern::Ignore => Self::Ignore,
            StructurePattern::Literal(v) => {
                type_predicate
                    .check_contains(&ctx.env, &Type::Primitive(Typ::get(v).into()))?;
                Self::Literal(v.clone())
            }
            StructurePattern::Bind(name) => {
                let id = leaf_bind(
                    ctx,
                    scope,
                    name,
                    type_predicate,
                    pos,
                    &ori,
                    &mut mode,
                    false,
                )?;
                Self::Bind(id)
            }
            StructurePattern::SlicePrefix { list, all, prefix, tail } => {
                let (all, tail, prefix) = with_pref_suf!(*list, all, tail, prefix);
                Self::SlicePrefix { list: *list, all, prefix, tail }
            }
            StructurePattern::SliceSuffix { all, head, suffix } => {
                let (all, head, suffix) = with_pref_suf!(false, all, head, suffix);
                Self::SliceSuffix { all, head, suffix }
            }
            StructurePattern::Slice { list, all, binds } => {
                let want = if *list {
                    Type::List(Arc::new(Type::empty_tvar()))
                } else {
                    Type::Array(Arc::new(Type::empty_tvar()))
                };
                type_predicate.check_contains(&ctx.env, &want)?;
                match &type_predicate.with_deref(|t| match t {
                    Some(Type::Array(et)) if !*list => Some(et.clone()),
                    Some(Type::List(et)) if *list => Some(et.clone()),
                    _ => None,
                }) {
                    Some(et) => {
                        let all = match all.as_ref() {
                            None => None,
                            Some(n) => Some(leaf_bind(
                                ctx,
                                scope,
                                n,
                                type_predicate,
                                pos,
                                &ori,
                                &mut mode,
                                true,
                            )?),
                        };
                        let binds = binds
                            .iter()
                            .map(|b| {
                                Self::compile_int(
                                    ctx,
                                    et,
                                    b,
                                    scope,
                                    pos,
                                    ori.clone(),
                                    mode.reborrow(),
                                )
                            })
                            .collect::<Result<Box<[Self]>>>()?;
                        Self::Slice {
                            kind: if *list { SliceKind::List } else { SliceKind::Array },
                            all,
                            binds,
                        }
                    }
                    _ => format_with_flags(PrintFlag::DerefTVars, || {
                        bail!("slice patterns can't match {type_predicate}")
                    })?,
                }
            }
            StructurePattern::Tuple { all, binds } => {
                type_predicate.check_contains(
                    &ctx.env,
                    &Type::Tuple(Arc::from_iter(
                        binds.iter().map(|_| Type::empty_tvar()),
                    )),
                )?;
                match &type_predicate.deref_cloned() {
                    Some(Type::Tuple(elts)) => {
                        if binds.len() != elts.len() {
                            bail!("expected a tuple of length {}", elts.len())
                        }
                        let all = match all.as_ref() {
                            None => None,
                            Some(n) => Some(leaf_bind(
                                ctx,
                                scope,
                                n,
                                type_predicate,
                                pos,
                                &ori,
                                &mut mode,
                                true,
                            )?),
                        };
                        let binds = elts
                            .iter()
                            .zip(binds.iter())
                            .map(|(t, b)| {
                                Self::compile_int(
                                    ctx,
                                    t,
                                    b,
                                    scope,
                                    pos,
                                    ori.clone(),
                                    mode.reborrow(),
                                )
                            })
                            .collect::<Result<Box<[Self]>>>()?;
                        Self::Slice { kind: SliceKind::Tuple, all, binds }
                    }
                    _ => format_with_flags(PrintFlag::DerefTVars, || {
                        bail!("tuple patterns can't match {type_predicate}")
                    })?,
                }
            }
            StructurePattern::Variant { all, tag, binds } => {
                type_predicate.check_contains(
                    &ctx.env,
                    &Type::Variant(
                        tag.clone(),
                        Arc::from_iter(binds.iter().map(|_| Type::empty_tvar())),
                    ),
                )?;
                match &type_predicate.deref_cloned() {
                    Some(Type::Variant(ttag, elts)) => {
                        if ttag != tag {
                            bail!(
                                "pattern cannot match type, tag mismatch {ttag} vs {tag}"
                            )
                        }
                        if binds.len() != elts.len() {
                            bail!("expected a variant with {} args", elts.len())
                        }
                        let all = match all.as_ref() {
                            None => None,
                            Some(n) => Some(leaf_bind(
                                ctx,
                                scope,
                                n,
                                type_predicate,
                                pos,
                                &ori,
                                &mut mode,
                                true,
                            )?),
                        };
                        let binds = elts
                            .iter()
                            .zip(binds.iter())
                            .map(|(t, b)| {
                                Self::compile_int(
                                    ctx,
                                    t,
                                    b,
                                    scope,
                                    pos,
                                    ori.clone(),
                                    mode.reborrow(),
                                )
                            })
                            .collect::<Result<Box<[Self]>>>()?;
                        Self::Variant { tag: tag.clone(), all, binds }
                    }
                    _ => format_with_flags(PrintFlag::DerefTVars, || {
                        bail!("variant patterns can't match {type_predicate}")
                    })?,
                }
            }
            StructurePattern::Abstract { all, name, bind } => {
                let td = ctx
                    .env
                    .lookup_typedef(&scope.lexical, name)?
                    .ok_or_else(|| anyhow!("unknown type {name}"))?;
                let Type::Abstract { id, .. } = &td.typ else {
                    bail!("{name} is not an abstract type, so it has no constructor")
                };
                let id = *id;
                let Some(r) = ctx.env.abstract_rep(id, &scope.lexical) else {
                    bail!(
                        "the definition of {name} is not visible here, so its values \
                         cannot be destructured"
                    )
                };
                let (atyp, rep) = r.instantiate(id);
                type_predicate.check_contains(&ctx.env, &atyp)?;
                let all = match all.as_ref() {
                    None => None,
                    Some(n) => Some(leaf_bind(
                        ctx,
                        scope,
                        n,
                        type_predicate,
                        pos,
                        &ori,
                        &mut mode,
                        true,
                    )?),
                };
                let bind = Box::new(Self::compile_int(
                    ctx,
                    &rep,
                    bind,
                    scope,
                    pos,
                    ori.clone(),
                    mode.reborrow(),
                )?);
                Self::Abstract { id, all, rep, bind }
            }
            StructurePattern::Struct { exhaustive, all, binds } => {
                struct Ifo {
                    name: ArcStr,
                    index: usize,
                    pattern: StructurePattern,
                    typ: Type,
                }
                match &type_predicate {
                    Type::Struct(_) => (),
                    _ if *exhaustive => type_predicate.check_contains(
                        &ctx.env,
                        &Type::Struct(Arc::from_iter(
                            binds
                                .iter()
                                .map(|(name, _)| (name.clone(), Type::empty_tvar())),
                        )),
                    )?,
                    _ => bail!("non exhaustive struct matches require type annotations"),
                }
                match &type_predicate.deref_cloned() {
                    Some(Type::Struct(elts)) => {
                        let binds = binds
                            .iter()
                            .map(|(field, pat)| {
                                let r = elts.iter().enumerate().find_map(
                                    |(i, (name, typ))| {
                                        if field == name {
                                            Some(Ifo {
                                                name: name.clone(),
                                                index: i,
                                                pattern: pat.clone(),
                                                typ: typ.clone(),
                                            })
                                        } else {
                                            None
                                        }
                                    },
                                );
                                r.ok_or_else(|| anyhow!("no such struct field {field}"))
                            })
                            .collect::<Result<SmallVec<[Ifo; 8]>>>()?;
                        if *exhaustive && binds.len() < elts.len() {
                            bail!("missing bindings for struct fields")
                        }
                        let all = match all.as_ref() {
                            None => None,
                            Some(n) => Some(leaf_bind(
                                ctx,
                                scope,
                                n,
                                type_predicate,
                                pos,
                                &ori,
                                &mut mode,
                                true,
                            )?),
                        };
                        let binds = binds
                            .into_iter()
                            .map(|ifo| {
                                Ok((
                                    ifo.name,
                                    ifo.index,
                                    Self::compile_int(
                                        ctx,
                                        &ifo.typ,
                                        &ifo.pattern,
                                        scope,
                                        pos,
                                        ori.clone(),
                                        mode.reborrow(),
                                    )?,
                                ))
                            })
                            .collect::<Result<Box<[(ArcStr, usize, Self)]>>>()?;
                        Self::Struct { all, binds }
                    }
                    _ => format_with_flags(PrintFlag::DerefTVars, || {
                        bail!("struct patterns can't match {type_predicate}")
                    })?,
                }
            }
        };
        Ok(t)
    }

    /// For a tuple destructure pattern `(a, b, …)` with only simple
    /// `Bind`/`Ignore` leaves and no whole-binding, return each `Bind`
    /// leaf's `(BindId, tuple position)`. `None` for any other shape.
    pub fn tuple_leaves(&self) -> Option<Vec<(BindId, usize)>> {
        match self {
            Self::Slice { kind: SliceKind::Tuple, all: None, binds } => {
                let mut out = Vec::with_capacity(binds.len());
                for (i, b) in binds.iter().enumerate() {
                    match b {
                        Self::Bind(id) => out.push((*id, i)),
                        Self::Ignore => {}
                        _ => return None,
                    }
                }
                Some(out)
            }
            _ => None,
        }
    }

    /// For a single-name binding pattern (`x` in `|x| body`), the bound
    /// `BindId`; `None` for destructures / ignores / literals.
    pub fn single_bind_id(&self) -> Option<BindId> {
        match self {
            Self::Bind(id) => Some(*id),
            _ => None,
        }
    }

    pub fn ids<'a>(&'a self, f: &mut (dyn FnMut(BindId) + 'a)) {
        crate::stack::ensure_sufficient(|| self.ids_inner(f))
    }

    fn ids_inner<'a>(&'a self, f: &mut (dyn FnMut(BindId) + 'a)) {
        match &self {
            Self::Abstract { all, bind, .. } => {
                if let Some(id) = all {
                    f(*id)
                }
                bind.ids(f)
            }
            Self::Or { alts } => alts[0].ids(f),
            Self::Ignore | Self::Literal(_) => (),
            Self::Bind(id) => f(*id),
            Self::Slice { kind: _, all, binds } => {
                if let Some(id) = all {
                    f(*id);
                }
                for n in binds.iter() {
                    n.ids(f)
                }
            }
            Self::Variant { tag: _, all, binds } => {
                if let Some(id) = all {
                    f(*id)
                }
                for n in binds.iter() {
                    n.ids(f)
                }
            }
            Self::SlicePrefix { list: _, all, prefix, tail } => {
                if let Some(id) = all {
                    f(*id)
                }
                for n in prefix.iter() {
                    n.ids(f)
                }
                if let Some(id) = tail {
                    f(*id)
                }
            }
            Self::SliceSuffix { all, head, suffix } => {
                if let Some(id) = all {
                    f(*id)
                }
                if let Some(id) = head {
                    f(*id)
                }
                for n in suffix.iter() {
                    n.ids(f)
                }
            }
            Self::Struct { all, binds } => {
                if let Some(id) = all {
                    f(*id)
                }
                for (_, _, n) in binds.iter() {
                    n.ids(f)
                }
            }
        }
    }

    pub fn bind<F: FnMut(BindId, Value)>(&self, v: &Value, f: &mut F) {
        crate::stack::ensure_sufficient(|| self.bind_inner(v, f))
    }

    fn bind_inner<F: FnMut(BindId, Value)>(&self, v: &Value, f: &mut F) {
        match &self {
            Self::Abstract { id, all, bind, .. } => {
                if let Some(bid) = all {
                    f(*bid, v.clone())
                }
                if let Some(g) = crate::abstract_value::get(v)
                    && g.id == *id
                {
                    bind.bind(&g.payload, f)
                }
            }
            Self::Ignore | Self::Literal(_) => (),
            Self::Bind(id) => f(*id, v.clone()),
            // the first matching alternative delivers the shared ids
            Self::Or { alts } => {
                for a in alts.iter() {
                    if a.is_match(v) {
                        return a.bind(v, f);
                    }
                }
            }
            Self::Slice { kind: SliceKind::Tuple | SliceKind::Array, all, binds } => {
                match v {
                    Value::Array(a) if a.len() == binds.len() => {
                        if let Some(id) = all {
                            f(*id, v.clone());
                        }
                        for (j, n) in binds.iter().enumerate() {
                            n.bind(&a[j], f)
                        }
                    }
                    _ => (),
                }
            }
            Self::Slice { kind: SliceKind::List, all, binds } => {
                use crate::node::collection::list;
                if let Some(id) = all {
                    f(*id, v.clone());
                }
                let mut cur = v.clone();
                for n in binds.iter() {
                    match list::split(&cur) {
                        Some((h, t)) => {
                            n.bind(h, f);
                            let t = t.clone();
                            cur = t;
                        }
                        None => break,
                    }
                }
            }
            Self::Variant { tag: _, all, binds } => {
                if let Some(id) = all {
                    f(*id, v.clone())
                }
                match v {
                    Value::Array(a) if a.len() == binds.len() + 1 => {
                        for (j, n) in binds.iter().enumerate() {
                            n.bind(&a[j + 1], f)
                        }
                    }
                    _ => (),
                }
            }
            Self::SlicePrefix { list: false, all, prefix, tail } => match v {
                Value::Array(a) if a.len() >= prefix.len() => {
                    if let Some(id) = all {
                        f(*id, v.clone())
                    }
                    for (j, n) in prefix.iter().enumerate() {
                        n.bind(&a[j], f)
                    }
                    if let Some(id) = tail {
                        let ss = a.subslice(prefix.len()..).unwrap();
                        f(*id, Value::Array(ss))
                    }
                }
                _ => (),
            },
            // heads bind by walking the spine; the tail bind is the k-th
            // tail itself
            Self::SlicePrefix { list: true, all, prefix, tail } => {
                use crate::node::collection::list;
                if let Some(id) = all {
                    f(*id, v.clone())
                }
                let mut cur = v.clone();
                let mut ok = true;
                for n in prefix.iter() {
                    match list::split(&cur) {
                        Some((h, t)) => {
                            n.bind(h, f);
                            let t = t.clone();
                            cur = t;
                        }
                        None => {
                            ok = false;
                            break;
                        }
                    }
                }
                if ok && let Some(id) = tail {
                    f(*id, cur)
                }
            }
            Self::SliceSuffix { all, head, suffix } => match v {
                Value::Array(a) if a.len() >= suffix.len() => {
                    // binds read from the same offset `is_match` skips to
                    let split = a.len() - suffix.len();
                    if let Some(id) = all {
                        f(*id, v.clone())
                    }
                    if let Some(id) = head {
                        let ss = a.subslice(..split).unwrap();
                        f(*id, Value::Array(ss))
                    }
                    let tail = a.subslice(split..).unwrap();
                    for (j, n) in suffix.iter().enumerate() {
                        n.bind(&tail[j], f)
                    }
                }
                _ => (),
            },
            Self::Struct { all, binds } => match v {
                Value::Array(a) if a.len() >= binds.len() => {
                    if let Some(id) = all {
                        f(*id, v.clone())
                    }
                    for (_, i, n) in binds.iter() {
                        if let Some(v) = a.get(*i) {
                            match v {
                                Value::Array(a) if a.len() == 2 => n.bind(&a[1], f),
                                _ => (),
                            }
                        }
                    }
                }
                _ => (),
            },
        }
    }

    pub fn unbind<F: FnMut(BindId)>(&self, f: &mut F) {
        crate::stack::ensure_sufficient(|| self.unbind_inner(f))
    }

    fn unbind_inner<F: FnMut(BindId)>(&self, f: &mut F) {
        match &self {
            Self::Abstract { all, bind, .. } => {
                if let Some(id) = all {
                    f(*id)
                }
                bind.unbind(f)
            }
            Self::Or { alts } => alts[0].unbind(f),
            Self::Ignore | Self::Literal(_) => (),
            Self::Bind(id) => f(*id),
            Self::Slice { kind: _, all, binds }
            | Self::Variant { tag: _, all, binds } => {
                if let Some(id) = all {
                    f(*id)
                }
                for n in binds.iter() {
                    n.unbind(f)
                }
            }
            Self::SlicePrefix { list: _, all, prefix, tail } => {
                if let Some(id) = all {
                    f(*id)
                }
                if let Some(id) = tail {
                    f(*id)
                }
                for n in prefix.iter() {
                    n.unbind(f)
                }
            }
            Self::SliceSuffix { all, head, suffix } => {
                if let Some(id) = all {
                    f(*id)
                }
                if let Some(id) = head {
                    f(*id)
                }
                for n in suffix.iter() {
                    n.unbind(f)
                }
            }
            Self::Struct { all, binds } => {
                if let Some(id) = all {
                    f(*id)
                }
                for (_, _, n) in binds.iter() {
                    n.unbind(f)
                }
            }
        }
    }

    pub fn is_match(&self, v: &Value) -> bool {
        crate::stack::ensure_sufficient(|| self.is_match_inner(v))
    }

    fn is_match_inner(&self, v: &Value) -> bool {
        match &self {
            Self::Abstract { id, bind, .. } => match crate::abstract_value::get(v) {
                Some(g) => g.id == *id && bind.is_match(&g.payload),
                None => false,
            },
            Self::Ignore | Self::Bind(_) => true,
            Self::Or { alts } => alts.iter().any(|a| a.is_match(v)),
            Self::Literal(o) => v == o,
            Self::Slice { kind: SliceKind::Tuple | SliceKind::Array, all: _, binds } => {
                match v {
                    Value::Array(a) => {
                        a.len() == binds.len()
                            && binds.iter().zip(a.iter()).all(|(b, v)| b.is_match(v))
                    }
                    _ => false,
                }
            }
            // exactly `binds.len()` cells, each head matching, ending at nil
            Self::Slice { kind: SliceKind::List, all: _, binds } => {
                use crate::node::collection::list;
                let mut cur = v.clone();
                let mut ok = true;
                for b in binds.iter() {
                    match list::split(&cur) {
                        Some((h, t)) => {
                            if !b.is_match(h) {
                                ok = false;
                                break;
                            }
                            let t = t.clone();
                            cur = t;
                        }
                        None => {
                            ok = false;
                            break;
                        }
                    }
                }
                ok && list::is_nil(&cur)
            }
            Self::Variant { tag, all: _, binds } if binds.len() == 0 => match v {
                Value::String(s) => tag == s,
                _ => false,
            },
            Self::Variant { tag, all: _, binds } => match v {
                Value::Array(a) => {
                    a.len() == binds.len() + 1
                        && match &a[0] {
                            Value::String(s) => s == tag,
                            _ => false,
                        }
                        && binds.iter().zip(a[1..].iter()).all(|(b, v)| b.is_match(v))
                }
                _ => false,
            },
            Self::SlicePrefix { list: false, all: _, prefix, tail: _ } => match v {
                Value::Array(a) => {
                    a.len() >= prefix.len()
                        && prefix.iter().zip(a.iter()).all(|(b, v)| b.is_match(v))
                }
                _ => false,
            },
            Self::SlicePrefix { list: true, all: _, prefix, tail: _ } => {
                use crate::node::collection::list;
                let mut cur = v.clone();
                let mut ok = true;
                for b in prefix.iter() {
                    match list::split(&cur) {
                        Some((h, t)) => {
                            if !b.is_match(h) {
                                ok = false;
                                break;
                            }
                            let t = t.clone();
                            cur = t;
                        }
                        None => {
                            ok = false;
                            break;
                        }
                    }
                }
                ok
            }
            Self::SliceSuffix { all: _, head: _, suffix } => match v {
                Value::Array(a) => {
                    a.len() >= suffix.len()
                        && suffix
                            .iter()
                            .zip(a.iter().skip(a.len() - suffix.len()))
                            .all(|(b, v)| b.is_match(v))
                }
                _ => false,
            },
            Self::Struct { all: _, binds } => match v {
                Value::Array(a) => {
                    a.len() >= binds.len()
                        && binds.iter().all(|(_, i, p)| match a.get(*i) {
                            Some(Value::Array(a)) if a.len() == 2 => p.is_match(&a[1]),
                            _ => false,
                        })
                }
                _ => false,
            },
        }
    }

    pub fn is_refutable(&self) -> bool {
        crate::stack::ensure_sufficient(|| self.is_refutable_inner())
    }

    fn is_refutable_inner(&self) -> bool {
        match &self {
            Self::Abstract { bind, .. } => bind.is_refutable(),
            Self::Bind(_) | Self::Ignore => false,
            Self::Or { .. } => true,
            Self::Literal(_) => true,
            Self::Slice { kind: SliceKind::Tuple, all: _, binds } => {
                binds.iter().any(|p| p.is_refutable())
            }
            Self::Struct { all: _, binds } => {
                binds.iter().any(|(_, _, p)| p.is_refutable())
            }
            Self::Variant { all: _, tag: _, binds } => {
                binds.len() > 0 && binds.iter().any(|p| p.is_refutable())
            }
            Self::Slice { kind: SliceKind::Array | SliceKind::List, .. }
            | Self::SlicePrefix { .. }
            | Self::SliceSuffix { .. } => true,
        }
    }

    /// True when the pattern matches any value of the scrutinee's type:
    /// `Select`'s wildcard test. Not `!is_refutable()`: a variant
    /// pattern with an all-bind payload is irrefutable given its tag
    /// matched, but its inferred predicate still carries the tag test.
    pub fn matches_anything(&self) -> bool {
        crate::stack::ensure_sufficient(|| self.matches_anything_inner())
    }

    /// Shape test only: an array slice pattern of any element
    /// refutability.
    pub fn is_array_slice(&self) -> bool {
        match self {
            Self::Slice { kind: SliceKind::Array | SliceKind::List, .. }
            | Self::SlicePrefix { .. }
            | Self::SliceSuffix { .. } => true,
            Self::Or { alts } => alts.iter().any(|a| a.is_array_slice()),
            _ => false,
        }
    }

    /// The length range of an array slice pattern, `Some((k, exact))`:
    /// it matches only arrays of length == k (`exact`) or >= k,
    /// regardless of whether its element sub-patterns can refute.
    pub fn array_len_range(&self) -> Option<(usize, bool)> {
        match self {
            Self::Slice { kind: SliceKind::Array | SliceKind::List, all: _, binds } => {
                Some((binds.len(), true))
            }
            Self::SlicePrefix { list: _, all: _, prefix, tail: _ } => {
                Some((prefix.len(), false))
            }
            Self::SliceSuffix { all: _, head: _, suffix } => Some((suffix.len(), false)),
            _ => None,
        }
    }

    /// The pattern's array-length coverage claim: the length range, but
    /// only when every element sub-pattern matches anything. The type
    /// half of the claim is the caller's to verify.
    pub fn array_len_coverage(&self) -> Option<(usize, bool)> {
        let all_cover = match self {
            Self::Slice { kind: SliceKind::Array | SliceKind::List, all: _, binds } => {
                binds.iter().all(|p| p.matches_anything())
            }
            Self::SlicePrefix { list: _, all: _, prefix, tail: _ } => {
                prefix.iter().all(|p| p.matches_anything())
            }
            Self::SliceSuffix { all: _, head: _, suffix } => {
                suffix.iter().all(|p| p.matches_anything())
            }
            _ => false,
        };
        if all_cover { self.array_len_range() } else { None }
    }

    fn matches_anything_inner(&self) -> bool {
        match &self {
            Self::Bind(_) | Self::Ignore => true,
            Self::Or { alts } => alts.iter().any(|a| a.matches_anything()),
            Self::Literal(_) | Self::Variant { .. } | Self::Abstract { .. } => false,
            Self::Slice { kind: SliceKind::Tuple, all: _, binds } => {
                binds.iter().all(|p| p.matches_anything())
            }
            Self::Struct { all: _, binds } => {
                binds.iter().all(|(_, _, p)| p.matches_anything())
            }
            Self::Slice { kind: SliceKind::Array | SliceKind::List, .. }
            | Self::SlicePrefix { .. }
            | Self::SliceSuffix { .. } => false,
        }
    }

    pub fn delete<R: Rt, E: UserEvent>(&self, ctx: &mut ExecCtx<R, E>) {
        crate::stack::ensure_sufficient(|| self.delete_inner(ctx))
    }

    fn delete_inner<R: Rt, E: UserEvent>(&self, ctx: &mut ExecCtx<R, E>) {
        match self {
            Self::Abstract { all, bind, .. } => {
                if let Some(id) = all {
                    ctx.rt.store_remove(id);
                    ctx.env.unbind_variable(*id);
                }
                bind.delete(ctx)
            }
            Self::Ignore | Self::Literal(_) => (),
            Self::Bind(id) => {
                ctx.rt.store_remove(&id);
                ctx.env.unbind_variable(*id);
            }
            Self::Or { alts } => alts[0].delete(ctx),
            Self::Struct { all, binds } => {
                if let Some(id) = all {
                    ctx.rt.store_remove(id);
                    ctx.env.unbind_variable(*id);
                }
                for (_, _, n) in binds {
                    n.delete(ctx)
                }
            }
            Self::Slice { kind: _, all, binds }
            | Self::Variant { tag: _, all, binds } => {
                if let Some(id) = all {
                    ctx.rt.store_remove(id);
                    ctx.env.unbind_variable(*id);
                }
                for n in binds {
                    n.delete(ctx)
                }
            }
            Self::SlicePrefix { list: _, all, prefix, tail } => {
                if let Some(id) = all {
                    ctx.rt.store_remove(id);
                    ctx.env.unbind_variable(*id);
                }
                if let Some(id) = tail {
                    ctx.rt.store_remove(id);
                    ctx.env.unbind_variable(*id);
                }
                for n in prefix {
                    n.delete(ctx)
                }
            }
            Self::SliceSuffix { all, head, suffix } => {
                if let Some(id) = all {
                    ctx.rt.store_remove(id);
                    ctx.env.unbind_variable(*id);
                }
                if let Some(id) = head {
                    ctx.rt.store_remove(id);
                    ctx.env.unbind_variable(*id);
                }
                for n in suffix {
                    n.delete(ctx);
                }
            }
        }
    }
}

/// One arm's consultation verdict. `NoStruct`: the type/structure test
/// failed and the guard was not consulted. `GuardBottom`: the structure
/// matched and the guard's current channel is bottom, so the selection
/// is undecidable and the select bottoms.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ArmMatch {
    NoStruct,
    GuardFalse,
    GuardBottom,
    Matched,
}

#[derive(Debug)]
pub struct PatternNode<R: Rt, E: UserEvent> {
    pub explicit_type_predicate: bool,
    pub type_predicate: Type,
    /// The O(1) shallow discriminator for an inferred predicate, sealed
    /// at the select's first consult ([`Type::shallow_discriminant`]);
    /// `None` = run the full `is_a` walk.
    pub shallow_predicate: Option<Type>,
    pub structure_predicate: StructPatternNode,
    pub guard: Option<Held<R, E>>,
}

impl<R: Rt, E: UserEvent> PatternNode<R, E> {
    pub(crate) fn image_len(&self) -> usize {
        self.explicit_type_predicate.encoded_len()
            + self.type_predicate.encoded_len()
            + self.shallow_predicate.encoded_len()
            + self.structure_predicate.encoded_len()
            + 1
            + self.guard.as_ref().map_or(0, |g| g.image_len())
    }

    pub(crate) fn image_encode(&self, buf: &mut BytesMut) -> Result<(), PackError> {
        self.explicit_type_predicate.encode(buf)?;
        self.type_predicate.encode(buf)?;
        self.shallow_predicate.encode(buf)?;
        self.structure_predicate.encode(buf)?;
        self.guard.is_some().encode(buf)?;
        match &self.guard {
            Some(g) => g.image_encode(buf),
            None => Ok(()),
        }
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let explicit_type_predicate = bool::decode(buf)?;
        let type_predicate = Type::decode(buf)?;
        let shallow_predicate = Option::<Type>::decode(buf)?;
        let structure_predicate = StructPatternNode::decode(buf)?;
        let guard = match bool::decode(buf)? {
            true => Some(Held::image_decode(ctx, buf)?),
            false => None,
        };
        Ok(PatternNode {
            explicit_type_predicate,
            type_predicate,
            shallow_predicate,
            structure_predicate,
            guard,
        })
    }

    pub(super) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: &Pattern,
        scope: &Scope,
        top_id: ExprId,
        pos: SourcePosition,
        ori: Arc<Origin>,
    ) -> Result<Self> {
        let (explicit, type_predicate) = match &spec.type_predicate {
            Some(t) => (true, t.scope_refs(&scope.lexical).lookup_ref(&ctx.env)?),
            None => {
                let typ = spec
                    .structure_predicate
                    .infer_type_predicate(&ctx.env, &scope.lexical)?;
                (false, typ)
            }
        };
        // an explicit predicate on an abstract type is a nominal tag
        // test; parameters are not carried at runtime, so `Box<i64> as b`
        // also matches a `Box<string>`
        match &type_predicate {
            Type::Fn(_) => bail!("can't match on Fn type"),
            Type::App(..) | Type::Hole => bail!("can't match on a type constructor"),
            Type::Bottom
            | Type::Abstract { .. }
            | Type::Any
            | Type::Primitive(_)
            | Type::Set(_)
            | Type::TVar(_)
            | Type::Error(_)
            | Type::Array(_)
            | Type::List(_)
            | Type::Map { .. }
            | Type::ByRef(_)
            | Type::Tuple(_)
            | Type::Variant(_, _)
            | Type::Struct(_)
            | Type::Ref(TypeRef { .. }) => (),
        }
        let structure_predicate = StructPatternNode::compile(
            ctx,
            &type_predicate,
            &spec.structure_predicate,
            scope,
            pos,
            ori,
        )?;
        let guard = spec
            .guard
            .as_ref()
            .map(|g| compiler::compile(ctx, flags, g.clone(), &scope, top_id))
            .transpose()?
            .map(Held::new);
        Ok(PatternNode {
            explicit_type_predicate: explicit,
            type_predicate,
            shallow_predicate: None,
            structure_predicate,
            guard,
        })
    }

    /// Deliver the scrutinee's destructured leaves to this arm's binds,
    /// carrying the scrutinee's production tag: a stale scrutinee binds
    /// stale leaves, never fired ones.
    pub(super) fn bind_event(
        &self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
        v: &Value,
        tag: crate::Tag,
    ) {
        self.structure_predicate.bind(v, &mut |id, v| {
            event.variables.insert(id, TagValue::tagged(v.clone(), tag));
            // the store twin carries the same tag; frames never write the store
            if ctx.frame_depth == 0 {
                ctx.rt.store_insert(id, TagValue::tagged(v, tag));
            }
        })
    }

    pub(super) fn unbind_event(&self, event: &mut Event<E>) {
        self.structure_predicate.unbind(&mut |id| {
            event.variables.remove(&id);
        })
    }

    /// Tick the guard (it must see every cycle) and return its
    /// production tag (`None` = no guard). The caller reads channel
    /// bottomness off `guard.tag`.
    pub(super) fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<Tag> {
        match &mut self.guard {
            None => None,
            Some(g) => Some(g.update(ctx, event)),
        }
    }

    /// Seal the shallow discriminator for this arm's inferred predicate
    /// against the select's scrutinee type. Called lazily at the
    /// select's first consult, when every tvar in the predicate is
    /// settled; a pure function of static types, never cleared.
    pub(super) fn seal_shallow(&mut self, env: &Env, scrutinee: &Type) {
        if !self.explicit_type_predicate {
            self.shallow_predicate =
                self.type_predicate.shallow_discriminant(env, scrutinee);
            if crate::dbgenv::gxdbg_shallow() {
                eprintln!(
                    "SHALLOW {} => {}",
                    self.type_predicate,
                    match &self.shallow_predicate {
                        Some(t) => format!("{t}"),
                        None => "deep".into(),
                    }
                );
            }
        }
    }

    pub(super) fn arm_match(&self, env: &Env, v: &Value) -> ArmMatch {
        // the type predicate is checked whether written or inferred: a
        // tuple and an array are the same `Value::Array` at runtime. An
        // inferred predicate is checked permissively (an abstract's
        // hidden rep cannot be verified); an explicit one strictly.
        let typed = if self.explicit_type_predicate {
            self.type_predicate.is_a(env, v)
        } else if let Some(shallow) = &self.shallow_predicate {
            shallow.is_a_with(env, IsAFlags::MatchAbstract.into(), v)
        } else {
            self.type_predicate.is_a_with(env, IsAFlags::MatchAbstract.into(), v)
        };
        if !(typed && self.structure_predicate.is_match(v)) {
            return ArmMatch::NoStruct;
        }
        match &self.guard {
            None => ArmMatch::Matched,
            Some(g) => {
                if g.tag.is_bottom() {
                    return ArmMatch::GuardBottom;
                }
                match g.value.as_ref().and_then(|v| v.clone().get_as::<bool>()) {
                    Some(true) => ArmMatch::Matched,
                    // a non-bool guard is a type error upstream
                    Some(false) | None => ArmMatch::GuardFalse,
                }
            }
        }
    }

    pub(super) fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(n) = &mut self.guard {
            n.node.delete(ctx)
        }
        self.structure_predicate.delete(ctx)
    }
}

fn boxed_len(items: &[StructPatternNode]) -> usize {
    netidx_core::pack::varint_len(items.len() as u64)
        + items.iter().map(|p| p.encoded_len()).sum::<usize>()
}

fn boxed_encode(
    items: &[StructPatternNode],
    buf: &mut impl bytes::BufMut,
) -> Result<(), netidx_core::pack::PackError> {
    netidx_core::pack::encode_varint(items.len() as u64, buf);
    for p in items {
        p.encode(buf)?;
    }
    Ok(())
}

fn boxed_decode(
    buf: &mut impl bytes::Buf,
) -> Result<Box<[StructPatternNode]>, netidx_core::pack::PackError> {
    let n = netidx_core::pack::decode_varint(buf)? as usize;
    let mut out = Vec::with_capacity(n);
    for _ in 0..n {
        out.push(StructPatternNode::decode(buf)?);
    }
    Ok(out.into_boxed_slice())
}

/// The compiled pattern is data: bind ids, literals and shapes.
impl netidx_core::pack::Pack for StructPatternNode {
    fn encoded_len(&self) -> usize {
        1 + match self {
            Self::Ignore => 0,
            Self::Literal(v) => v.encoded_len(),
            Self::Bind(id) => id.encoded_len(),
            Self::Slice { kind, all, binds } => {
                kind.encoded_len() + all.encoded_len() + boxed_len(binds)
            }
            Self::SlicePrefix { list, all, prefix, tail } => {
                list.encoded_len()
                    + all.encoded_len()
                    + boxed_len(prefix)
                    + tail.encoded_len()
            }
            Self::SliceSuffix { all, head, suffix } => {
                all.encoded_len() + head.encoded_len() + boxed_len(suffix)
            }
            Self::Struct { all, binds } => {
                all.encoded_len()
                    + netidx_core::pack::varint_len(binds.len() as u64)
                    + binds
                        .iter()
                        .map(|(n, i, p)| {
                            n.encoded_len() + i.encoded_len() + p.encoded_len()
                        })
                        .sum::<usize>()
            }
            Self::Variant { tag, all, binds } => {
                tag.encoded_len() + all.encoded_len() + boxed_len(binds)
            }
            Self::Abstract { id, all, rep, bind } => {
                id.encoded_len()
                    + all.encoded_len()
                    + rep.encoded_len()
                    + bind.encoded_len()
            }
            Self::Or { alts } => boxed_len(alts),
        }
    }

    fn encode(
        &self,
        buf: &mut impl bytes::BufMut,
    ) -> Result<(), netidx_core::pack::PackError> {
        match self {
            Self::Ignore => buf.put_u8(0),
            Self::Literal(v) => {
                buf.put_u8(1);
                v.encode(buf)?
            }
            Self::Bind(id) => {
                buf.put_u8(2);
                id.encode(buf)?
            }
            Self::Slice { kind, all, binds } => {
                buf.put_u8(3);
                kind.encode(buf)?;
                all.encode(buf)?;
                boxed_encode(binds, buf)?
            }
            Self::SlicePrefix { list, all, prefix, tail } => {
                buf.put_u8(4);
                list.encode(buf)?;
                all.encode(buf)?;
                boxed_encode(prefix, buf)?;
                tail.encode(buf)?
            }
            Self::SliceSuffix { all, head, suffix } => {
                buf.put_u8(5);
                all.encode(buf)?;
                head.encode(buf)?;
                boxed_encode(suffix, buf)?
            }
            Self::Struct { all, binds } => {
                buf.put_u8(6);
                all.encode(buf)?;
                netidx_core::pack::encode_varint(binds.len() as u64, buf);
                for (n, i, p) in binds.iter() {
                    n.encode(buf)?;
                    i.encode(buf)?;
                    p.encode(buf)?;
                }
            }
            Self::Variant { tag, all, binds } => {
                buf.put_u8(7);
                tag.encode(buf)?;
                all.encode(buf)?;
                boxed_encode(binds, buf)?
            }
            Self::Abstract { id, all, rep, bind } => {
                buf.put_u8(8);
                id.encode(buf)?;
                all.encode(buf)?;
                rep.encode(buf)?;
                bind.encode(buf)?
            }
            Self::Or { alts } => {
                buf.put_u8(9);
                boxed_encode(alts, buf)?
            }
        }
        Ok(())
    }

    fn decode(buf: &mut impl bytes::Buf) -> Result<Self, netidx_core::pack::PackError> {
        use netidx_core::pack::{Pack, PackError};
        if !buf.has_remaining() {
            return Err(PackError::BufferShort);
        }
        Ok(match buf.get_u8() {
            0 => Self::Ignore,
            1 => Self::Literal(Pack::decode(buf)?),
            2 => Self::Bind(Pack::decode(buf)?),
            3 => Self::Slice {
                kind: Pack::decode(buf)?,
                all: Pack::decode(buf)?,
                binds: boxed_decode(buf)?,
            },
            4 => Self::SlicePrefix {
                list: Pack::decode(buf)?,
                all: Pack::decode(buf)?,
                prefix: boxed_decode(buf)?,
                tail: Pack::decode(buf)?,
            },
            5 => Self::SliceSuffix {
                all: Pack::decode(buf)?,
                head: Pack::decode(buf)?,
                suffix: boxed_decode(buf)?,
            },
            6 => {
                let all = Pack::decode(buf)?;
                let n = netidx_core::pack::decode_varint(buf)? as usize;
                let mut binds = Vec::with_capacity(n);
                for _ in 0..n {
                    binds.push((
                        Pack::decode(buf)?,
                        Pack::decode(buf)?,
                        Pack::decode(buf)?,
                    ));
                }
                Self::Struct { all, binds: binds.into_boxed_slice() }
            }
            7 => Self::Variant {
                tag: Pack::decode(buf)?,
                all: Pack::decode(buf)?,
                binds: boxed_decode(buf)?,
            },
            8 => Self::Abstract {
                id: Pack::decode(buf)?,
                all: Pack::decode(buf)?,
                rep: Pack::decode(buf)?,
                bind: Box::new(Pack::decode(buf)?),
            },
            9 => Self::Or { alts: boxed_decode(buf)? },
            _ => return Err(PackError::UnknownTag),
        })
    }
}
