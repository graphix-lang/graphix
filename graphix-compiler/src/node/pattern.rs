use crate::{
    BindId, CFlag, Event, ExecCtx, PrintFlag, Rt, Scope, TagValue, UserEvent,
    env::Env,
    expr::{ExprId, Origin, Pattern, StructurePattern},
    format_with_flags,
    node::{Held, compiler},
    typ::{IsAFlags, Type, TypeRef},
};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use combine::stream::position::SourcePosition;
use enumflags2::BitFlags;
use netidx_value::{Typ, Value};
use smallvec::SmallVec;
use std::fmt::Debug;
use triomphe::Arc;

#[derive(Debug)]
pub enum StructPatternNode {
    Ignore,
    Literal(Value),
    Bind(BindId),
    Slice {
        tuple: bool,
        all: Option<BindId>,
        binds: Box<[StructPatternNode]>,
    },
    SlicePrefix {
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
}

impl StructPatternNode {
    /// Re-derive the struct binders' field INDEXES from a COMPLETED
    /// type predicate. A partial pattern compiles against its inferred
    /// (fields-it-names-only) type, so its indexes point into the wrong
    /// layout once the select typecheck completes the predicate from
    /// the scrutinee; nothing else about the compiled pattern depends
    /// on the layout. Positions the completion didn't touch re-derive
    /// to the same indexes. Bind ids and sub-patterns are untouched.
    pub(super) fn realign(&mut self, env: &Env, typ: &Type) -> Result<()> {
        match self {
            Self::Ignore | Self::Literal(_) | Self::Bind(_) => Ok(()),
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
            Self::Slice { tuple: true, binds, all: _ } => {
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
            Self::Slice { tuple: false, binds, all: _ }
            | Self::SlicePrefix { prefix: binds, all: _, .. }
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
        Self::compile_int(ctx, type_predicate, spec, scope, pos, ori)
    }

    fn compile_int<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        type_predicate: &Type,
        spec: &StructurePattern,
        scope: &Scope,
        pos: SourcePosition,
        ori: Arc<Origin>,
    ) -> Result<Self> {
        crate::stack::ensure_sufficient(|| {
            Self::compile_int_inner(ctx, type_predicate, spec, scope, pos, ori)
        })
    }

    fn compile_int_inner<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        type_predicate: &Type,
        spec: &StructurePattern,
        scope: &Scope,
        pos: SourcePosition,
        ori: Arc<Origin>,
    ) -> Result<Self> {
        macro_rules! with_pref_suf {
            ($all:expr, $single:expr, $multi:expr) => {{
                type_predicate.check_contains(
                    &ctx.env,
                    &Type::Array(Arc::new(Type::empty_tvar())),
                )?;
                match &type_predicate.with_deref(|t| t.cloned()) {
                    Some(Type::Array(et)) => {
                        let all = $all.as_ref().map(|n| {
                            ctx.env
                                .bind_variable(
                                    &scope.lexical,
                                    n,
                                    type_predicate.clone(),
                                    pos,
                                    ori.clone(),
                                )
                                .id
                        });
                        let single = $single.as_ref().map(|n| {
                            ctx.env
                                .bind_variable(
                                    &scope.lexical,
                                    n,
                                    type_predicate.clone(),
                                    pos,
                                    ori.clone(),
                                )
                                .id
                        });
                        let multi = $multi
                            .iter()
                            .map(|n| {
                                Self::compile_int(ctx, et, n, scope, pos, ori.clone())
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
            StructurePattern::Ignore => Self::Ignore,
            StructurePattern::Literal(v) => {
                type_predicate
                    .check_contains(&ctx.env, &Type::Primitive(Typ::get(v).into()))?;
                Self::Literal(v.clone())
            }
            StructurePattern::Bind(name) => {
                let id = ctx
                    .env
                    .bind_variable(
                        &scope.lexical,
                        name,
                        type_predicate.clone(),
                        pos,
                        ori.clone(),
                    )
                    .id;
                Self::Bind(id)
            }
            StructurePattern::SlicePrefix { all, prefix, tail } => {
                let (all, tail, prefix) = with_pref_suf!(all, tail, prefix);
                Self::SlicePrefix { all, prefix, tail }
            }
            StructurePattern::SliceSuffix { all, head, suffix } => {
                let (all, head, suffix) = with_pref_suf!(all, head, suffix);
                Self::SliceSuffix { all, head, suffix }
            }
            StructurePattern::Slice { all, binds } => {
                type_predicate.check_contains(
                    &ctx.env,
                    &Type::Array(Arc::new(Type::empty_tvar())),
                )?;
                match &type_predicate.with_deref(|t| t.cloned()) {
                    Some(Type::Array(et)) => {
                        let all = all.as_ref().map(|n| {
                            ctx.env
                                .bind_variable(
                                    &scope.lexical,
                                    n,
                                    type_predicate.clone(),
                                    pos,
                                    ori.clone(),
                                )
                                .id
                        });
                        let binds = binds
                            .iter()
                            .map(|b| {
                                Self::compile_int(ctx, et, b, scope, pos, ori.clone())
                            })
                            .collect::<Result<Box<[Self]>>>()?;
                        Self::Slice { tuple: false, all, binds }
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
                match &type_predicate.with_deref(|t| t.cloned()) {
                    Some(Type::Tuple(elts)) => {
                        if binds.len() != elts.len() {
                            bail!("expected a tuple of length {}", elts.len())
                        }
                        let all = all.as_ref().map(|n| {
                            ctx.env
                                .bind_variable(
                                    &scope.lexical,
                                    n,
                                    type_predicate.clone(),
                                    pos,
                                    ori.clone(),
                                )
                                .id
                        });
                        let binds = elts
                            .iter()
                            .zip(binds.iter())
                            .map(|(t, b)| {
                                Self::compile_int(ctx, t, b, scope, pos, ori.clone())
                            })
                            .collect::<Result<Box<[Self]>>>()?;
                        Self::Slice { tuple: true, all, binds }
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
                match &type_predicate.with_deref(|t| t.cloned()) {
                    Some(Type::Variant(ttag, elts)) => {
                        if ttag != tag {
                            bail!(
                                "pattern cannot match type, tag mismatch {ttag} vs {tag}"
                            )
                        }
                        if binds.len() != elts.len() {
                            bail!("expected a variant with {} args", elts.len())
                        }
                        let all = all.as_ref().map(|n| {
                            ctx.env
                                .bind_variable(
                                    &scope.lexical,
                                    n,
                                    type_predicate.clone(),
                                    pos,
                                    ori.clone(),
                                )
                                .id
                        });
                        let binds = elts
                            .iter()
                            .zip(binds.iter())
                            .map(|(t, b)| {
                                Self::compile_int(ctx, t, b, scope, pos, ori.clone())
                            })
                            .collect::<Result<Box<[Self]>>>()?;
                        Self::Variant { tag: tag.clone(), all, binds }
                    }
                    _ => format_with_flags(PrintFlag::DerefTVars, || {
                        bail!("variant patterns can't match {type_predicate}")
                    })?,
                }
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
                match &type_predicate.with_deref(|t| t.cloned()) {
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
                        let all = all.as_ref().map(|n| {
                            ctx.env
                                .bind_variable(
                                    &scope.lexical,
                                    n,
                                    type_predicate.clone(),
                                    pos,
                                    ori.clone(),
                                )
                                .id
                        });
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
    /// leaf's `(BindId, tuple position)` (skipping `Ignore`). `None` for
    /// any other pattern shape. Used by HOF fusion to lower a `|(k, v)|`
    /// callback's arg destructure to per-leaf `TupleGet` bindings —
    /// `node::pattern` is `pub(crate)`, so callers outside the compiler
    /// (e.g. `MapQ`'s `emit_clif`) reach the leaves through this accessor
    /// rather than matching the enum.
    pub fn tuple_leaves(&self) -> Option<Vec<(BindId, usize)>> {
        match self {
            Self::Slice { tuple: true, all: None, binds } => {
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
    /// `BindId`; `None` for destructures / ignores / literals. The
    /// body's `Ref`s to the arg carry this id — HOF emission passes it
    /// through so the direct JIT path's BindId-first resolution finds
    /// the loop-element slot exactly (see [`Self::tuple_leaves`] for
    /// why this is an accessor rather than a public enum match).
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
            Self::Ignore | Self::Literal(_) => (),
            Self::Bind(id) => f(*id),
            Self::Slice { tuple: _, all, binds } => {
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
            Self::SlicePrefix { all, prefix, tail } => {
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
            Self::Ignore | Self::Literal(_) => (),
            Self::Bind(id) => f(*id, v.clone()),
            Self::Slice { tuple: _, all, binds } => match v {
                Value::Array(a) if a.len() == binds.len() => {
                    if let Some(id) = all {
                        f(*id, v.clone());
                    }
                    for (j, n) in binds.iter().enumerate() {
                        n.bind(&a[j], f)
                    }
                }
                _ => (),
            },
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
            Self::SlicePrefix { all, prefix, tail } => match v {
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
            Self::SliceSuffix { all, head, suffix } => match v {
                Value::Array(a) if a.len() >= suffix.len() => {
                    // The suffix patterns match the LAST `suffix.len()`
                    // elements (`is_match` skips `len - N`), so the binds
                    // must read from the same offset — and `head` is
                    // everything BEFORE the suffix.
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
            Self::Ignore | Self::Literal(_) => (),
            Self::Bind(id) => f(*id),
            Self::Slice { tuple: _, all, binds }
            | Self::Variant { tag: _, all, binds } => {
                if let Some(id) = all {
                    f(*id)
                }
                for n in binds.iter() {
                    n.unbind(f)
                }
            }
            Self::SlicePrefix { all, prefix, tail } => {
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
            Self::Ignore | Self::Bind(_) => true,
            Self::Literal(o) => v == o,
            Self::Slice { tuple: _, all: _, binds } => match v {
                Value::Array(a) => {
                    a.len() == binds.len()
                        && binds.iter().zip(a.iter()).all(|(b, v)| b.is_match(v))
                }
                _ => false,
            },
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
            Self::SlicePrefix { all: _, prefix, tail: _ } => match v {
                Value::Array(a) => {
                    a.len() >= prefix.len()
                        && prefix.iter().zip(a.iter()).all(|(b, v)| b.is_match(v))
                }
                _ => false,
            },
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
            Self::Bind(_) | Self::Ignore => false,
            Self::Literal(_) => true,
            Self::Slice { tuple: true, all: _, binds } => {
                binds.iter().any(|p| p.is_refutable())
            }
            Self::Struct { all: _, binds } => {
                binds.iter().any(|(_, _, p)| p.is_refutable())
            }
            Self::Variant { all: _, tag: _, binds } => {
                binds.len() > 0 && binds.iter().any(|p| p.is_refutable())
            }
            Self::Slice { tuple: false, .. }
            | Self::SlicePrefix { .. }
            | Self::SliceSuffix { .. } => true,
        }
    }

    /// True when the pattern matches ANY value of the scrutinee's type
    /// — a bind-all / destructure of binds whose inferred type
    /// predicate is a fresh TVar (or a composite of them) carrying no
    /// information. This is `Select`'s wildcard test. NOT the same as
    /// `!is_refutable()`: a variant pattern with an all-bind payload is
    /// structure-irrefutable GIVEN its tag matched (`is_refutable`'s
    /// contract — a `let` over a single-variant type depends on it),
    /// but its inferred type predicate carries the TAG test, so as a
    /// select arm it must join the coverage unions, not bypass them.
    /// Classifying `` `A ``/`` `B `` arms as wildcards skipped
    /// exhaustiveness entirely (a select missing a tag compiled) and
    /// left an OPEN scrutinee cell (a knotted rec self-call's rtype)
    /// to be greedily bound by the first arm's narrowing walk.
    pub fn matches_anything(&self) -> bool {
        crate::stack::ensure_sufficient(|| self.matches_anything_inner())
    }

    fn matches_anything_inner(&self) -> bool {
        match &self {
            Self::Bind(_) | Self::Ignore => true,
            Self::Literal(_) | Self::Variant { .. } => false,
            Self::Slice { tuple: true, all: _, binds } => {
                binds.iter().all(|p| p.matches_anything())
            }
            Self::Struct { all: _, binds } => {
                binds.iter().all(|(_, _, p)| p.matches_anything())
            }
            Self::Slice { tuple: false, .. }
            | Self::SlicePrefix { .. }
            | Self::SliceSuffix { .. } => false,
        }
    }

    pub fn delete<R: Rt, E: UserEvent>(&self, ctx: &mut ExecCtx<R, E>) {
        crate::stack::ensure_sufficient(|| self.delete_inner(ctx))
    }

    fn delete_inner<R: Rt, E: UserEvent>(&self, ctx: &mut ExecCtx<R, E>) {
        match self {
            Self::Ignore | Self::Literal(_) => (),
            Self::Bind(id) => {
                ctx.rt.store_remove(&id);
                ctx.env.unbind_variable(*id);
            }
            Self::Struct { all, binds } => {
                if let Some(id) = all {
                    ctx.rt.store_remove(id);
                    ctx.env.unbind_variable(*id);
                }
                for (_, _, n) in binds {
                    n.delete(ctx)
                }
            }
            Self::Slice { tuple: _, all, binds }
            | Self::Variant { tag: _, all, binds } => {
                if let Some(id) = all {
                    ctx.rt.store_remove(id);
                    ctx.env.unbind_variable(*id);
                }
                for n in binds {
                    n.delete(ctx)
                }
            }
            Self::SlicePrefix { all, prefix, tail } => {
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

/// Does `t` mention an abstract type at any position a runtime type
/// check would have to verify? Refs are expanded through the env;
/// pathological depth answers true (refusing is the conservative
/// direction — this gates a compile error, not a match).
fn mentions_abstract(env: &Env, t: &Type, depth: usize) -> bool {
    if depth > 64 {
        return true;
    }
    t.with_deref(|t| match t {
        None => false,
        Some(t) => match t {
            Type::Abstract { .. } => true,
            Type::Ref(_) => match t.lookup_ref(env) {
                Ok(t) => mentions_abstract(env, &t, depth + 1),
                Err(_) => false,
            },
            Type::Set(s) | Type::Tuple(s) | Type::Variant(_, s) => {
                s.iter().any(|t| mentions_abstract(env, t, depth + 1))
            }
            Type::Struct(fs) => {
                fs.iter().any(|(_, t)| mentions_abstract(env, t, depth + 1))
            }
            Type::Array(t) | Type::ByRef(t) | Type::Error(t) => {
                mentions_abstract(env, t, depth + 1)
            }
            Type::Map { key, value } => {
                mentions_abstract(env, key, depth + 1)
                    || mentions_abstract(env, value, depth + 1)
            }
            Type::Bottom
            | Type::Any
            | Type::Primitive(_)
            | Type::Fn(_)
            | Type::TVar(_) => false,
        },
    })
}

#[derive(Debug)]
pub struct PatternNode<R: Rt, E: UserEvent> {
    pub explicit_type_predicate: bool,
    pub type_predicate: Type,
    pub structure_predicate: StructPatternNode,
    pub guard: Option<Held<R, E>>,
    /// The guard's held ride is BLOCKED this cycle (Eric's
    /// whole-derivation depth-trip ruling, 2026-08-14): set by
    /// `update` when a depth-trip unwind is in flight and the guard's
    /// production is tainted — `is_match` then reads the guard FALSE
    /// (the no-history phantom rule) instead of riding the held bool,
    /// exactly the kernel's tainted-guard mask. Outside an unwind a
    /// bottomed guard keeps THE GUARD RIDE (aug13b).
    guard_ride_blocked: bool,
}

impl<R: Rt, E: UserEvent> PatternNode<R, E> {
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
                let typ = spec.structure_predicate.infer_type_predicate(&ctx.env)?;
                (false, typ)
            }
        };
        match &type_predicate {
            Type::Fn(_) => bail!("can't match on Fn type"),
            Type::Bottom
            | Type::Abstract { .. }
            | Type::Any
            | Type::Primitive(_)
            | Type::Set(_)
            | Type::TVar(_)
            | Type::Error(_)
            | Type::Array(_)
            | Type::Map { .. }
            | Type::ByRef(_)
            | Type::Tuple(_)
            | Type::Variant(_, _)
            | Type::Struct(_)
            | Type::Ref(TypeRef { .. }) => (),
        }
        // An EXPLICIT predicate is the user's claim, checked strictly at
        // runtime — and an abstract type's representation is hidden, so
        // the check can never succeed (`is_a` refuses to claim what it
        // can't verify; matching by carrier id would claim wrong
        // parameterizations and can't see hidden non-Abstract reps).
        // Accepting the pattern made a guaranteed-dead arm the wildcard
        // silently won — the exact class the typechecker exists to
        // refuse. Found by the netidx-admin dogfood campaign
        // (2026-08-18).
        if explicit && mentions_abstract(&ctx.env, &type_predicate, 0) {
            bail!(
                "can't match on the abstract type {type_predicate}: its \
                 representation is hidden, so runtime dispatch cannot verify \
                 it. Dissect a result union with `?` or `$`, or use an \
                 accessor exported by the type's module"
            )
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
            structure_predicate,
            guard,
            guard_ride_blocked: false,
        })
    }

    /// Deliver the scrutinee's destructured leaves to this arm's
    /// binds, carrying the SCRUTINEE's production tag (Eric's ruling
    /// 2026-07-18, tail_jump_fired_plumbing): the kernel's arm-bind
    /// leaves carry the scrutinee's disc, so a value-channel refresh
    /// (stale scrutinee — a framed re-derivation from a quiet entry)
    /// binds STALE leaves instead of minting FIRED ones. The
    /// becoming-selected FIRE comes from the selection-change rule at
    /// the select's emit, never from poisoning the binds.
    pub(super) fn bind_event(
        &self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
        v: &Value,
        tag: crate::Tag,
    ) {
        self.structure_predicate.bind(v, &mut |id, v| {
            event.variables.insert(id, TagValue::tagged(v.clone(), tag));
            // The store twin carries the SAME honest tag as the overlay
            // entry, and only at depth 0 (R3: frames never write the
            // store). An unconditional `fired` here was the aug13b
            // free-run class: the guard tick's bind/unbind window left
            // a this-cycle-stamped FIRED store entry behind, the taken
            // arm's body read it back Delivered(FIRED) on an otherwise
            // quiet poll, the select emitted per the strict rule, and
            // any result-observing writer (a ByRef's write-through)
            // converted the phantom fire into a next-cycle wake — an
            // unquiesceable interp livelock the trace oracle capped.
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

    pub(super) fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> bool {
        match &mut self.guard {
            None => false,
            Some(g) => {
                let up = g.update_triggers(ctx, event);
                self.guard_ride_blocked = ctx.depth_tripped && g.tag.is_tainted();
                up
            }
        }
    }

    pub(super) fn is_match(&self, env: &Env, v: &Value) -> bool {
        // The type predicate holds whether it was WRITTEN or INFERRED.
        // Skipping the inferred one treated the structural test as a
        // sufficient proxy for the type, and it is not: a tuple and an
        // array are the SAME `Value::Array` at runtime, so `[x, y]` —
        // whose inferred predicate is `Array<_>` — matched a 2-tuple
        // out of a union scrutinee. The typechecker forbids exactly
        // that ("pattern Array<..> will never match (bool, bool),
        // unused match cases"), so the interp was binding leaves at
        // types the arm's body had already been compiled against:
        // `(true, true)` bound x,y:u8 and `x + y` added two bools,
        // emitting a u32 where the arm's type said `[u8, bool]` — the
        // node-walk even logged its own violation. A type error, so it
        // must not match (Eric, 2026-08-15); the tuple falls through
        // to the wildcard like any other unmatched member.
        //
        // An INFERRED predicate is checked PERMISSIVELY, though: it is
        // not a claim the user made, so it must not add refusals the
        // user never asked for. `MatchAbstract` is the difference —
        // an abstract type's representation is hidden by design, so a
        // runtime check cannot verify it, and refusing made ordinary
        // destructuring of a module-opaque value stop matching
        // (`select (m0::mk(i64:1), i64:0) {(x, _) => ..}` produced
        // NOTHING; caught within minutes by the fleet's generate
        // lanes). The other unverifiable leaves — `Any`, `⊥`, an
        // unbound tvar — are already permissive without `Strict`.
        // An EXPLICIT predicate (`x as T`) keeps the strict reading:
        // there the check IS the user's claim, and runtime dispatch
        // must never claim a value it cannot verify.
        let typed = if self.explicit_type_predicate {
            self.type_predicate.is_a(env, v)
        } else {
            self.type_predicate.is_a_with(env, IsAFlags::MatchAbstract.into(), v)
        };
        typed
            && self.structure_predicate.is_match(v)
            && match &self.guard {
                None => true,
                Some(_) if self.guard_ride_blocked => false,
                Some(g) => g
                    .value
                    .as_ref()
                    .and_then(|v| v.clone().get_as::<bool>())
                    .unwrap_or(false),
            }
    }

    pub(super) fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(n) = &mut self.guard {
            n.node.delete(ctx)
        }
        self.structure_predicate.delete(ctx)
    }
}
