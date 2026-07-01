use crate::{
    BindId, CFlag, Event, ExecCtx, PrintFlag, Rt, Scope, UserEvent,
    env::Env,
    expr::{ExprId, Origin, Pattern, StructurePattern},
    format_with_flags,
    node::{Cached, compiler},
    typ::{Type, TypeRef},
};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use combine::stream::position::SourcePosition;
use enumflags2::BitFlags;
use netidx::{publisher::Typ, subscriber::Value};
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

/// Re-mint one bound id for [`StructPatternNode::clone_rebind`]: read the
/// original binding's record and bind a fresh id under the same
/// name/type/pos/ori in `scope`. The fresh id enters `scope`'s name map,
/// so a subsequent `lookup_bind` by that name (during the cloned body's
/// `clone_rebind`) resolves to it. Unknown ids are kept as-is (defensive;
/// every pattern id has a `by_id` record).
fn remint_bind_id<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    scope: &Scope,
    old: BindId,
) -> BindId {
    match ctx.env.by_id.get(&old) {
        Some(b) => {
            let name = b.name.clone();
            let typ = b.typ.clone();
            let pos = b.pos;
            let ori = b.ori.clone();
            ctx.env.bind_variable(&scope.lexical, &name, typ, pos, ori).id
        }
        None => old,
    }
}

impl StructPatternNode {
    /// Produce a structurally-identical pattern with every bound id
    /// re-minted to a fresh env-registered `BindId` in `scope`. See
    /// [`crate::Update::clone_rebind`] — the fresh names enter `scope`'s
    /// name map so the cloned body's `Ref`s resolve to these fresh ids.
    pub fn clone_rebind<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
    ) -> Self {
        let opt = |ctx: &mut ExecCtx<R, E>, o: &Option<BindId>| {
            o.as_ref().map(|id| remint_bind_id(ctx, scope, *id))
        };
        match self {
            Self::Ignore => Self::Ignore,
            Self::Literal(v) => Self::Literal(v.clone()),
            Self::Bind(id) => Self::Bind(remint_bind_id(ctx, scope, *id)),
            Self::Slice { tuple, all, binds } => Self::Slice {
                tuple: *tuple,
                all: opt(ctx, all),
                binds: binds.iter().map(|b| b.clone_rebind(ctx, scope)).collect(),
            },
            Self::SlicePrefix { all, prefix, tail } => Self::SlicePrefix {
                all: opt(ctx, all),
                prefix: prefix.iter().map(|b| b.clone_rebind(ctx, scope)).collect(),
                tail: opt(ctx, tail),
            },
            Self::SliceSuffix { all, head, suffix } => Self::SliceSuffix {
                all: opt(ctx, all),
                head: opt(ctx, head),
                suffix: suffix.iter().map(|b| b.clone_rebind(ctx, scope)).collect(),
            },
            Self::Struct { all, binds } => Self::Struct {
                all: opt(ctx, all),
                binds: binds
                    .iter()
                    .map(|(n, i, b)| (n.clone(), *i, b.clone_rebind(ctx, scope)))
                    .collect(),
            },
            Self::Variant { tag, all, binds } => Self::Variant {
                tag: tag.clone(),
                all: opt(ctx, all),
                binds: binds.iter().map(|b| b.clone_rebind(ctx, scope)).collect(),
            },
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

    pub fn delete<R: Rt, E: UserEvent>(&self, ctx: &mut ExecCtx<R, E>) {
        match self {
            Self::Ignore | Self::Literal(_) => (),
            Self::Bind(id) => {
                ctx.cached.remove(&id);
                ctx.env.unbind_variable(*id);
            }
            Self::Struct { all, binds } => {
                if let Some(id) = all {
                    ctx.cached.remove(id);
                    ctx.env.unbind_variable(*id);
                }
                for (_, _, n) in binds {
                    n.delete(ctx)
                }
            }
            Self::Slice { tuple: _, all, binds }
            | Self::Variant { tag: _, all, binds } => {
                if let Some(id) = all {
                    ctx.cached.remove(id);
                    ctx.env.unbind_variable(*id);
                }
                for n in binds {
                    n.delete(ctx)
                }
            }
            Self::SlicePrefix { all, prefix, tail } => {
                if let Some(id) = all {
                    ctx.cached.remove(id);
                    ctx.env.unbind_variable(*id);
                }
                if let Some(id) = tail {
                    ctx.cached.remove(id);
                    ctx.env.unbind_variable(*id);
                }
                for n in prefix {
                    n.delete(ctx)
                }
            }
            Self::SliceSuffix { all, head, suffix } => {
                if let Some(id) = all {
                    ctx.cached.remove(id);
                    ctx.env.unbind_variable(*id);
                }
                if let Some(id) = head {
                    ctx.cached.remove(id);
                    ctx.env.unbind_variable(*id);
                }
                for n in suffix {
                    n.delete(ctx);
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct PatternNode<R: Rt, E: UserEvent> {
    pub explicit_type_predicate: bool,
    pub type_predicate: Type,
    pub structure_predicate: StructPatternNode,
    pub guard: Option<Cached<R, E>>,
}

impl<R: Rt, E: UserEvent> PatternNode<R, E> {
    /// Structural clone for [`crate::Update::clone_rebind`]: re-mint the
    /// structure's bound ids first (they enter `scope`'s name map), then
    /// clone the guard so its `Ref`s resolve to the fresh ids.
    pub(super) fn clone_rebind(&self, ctx: &mut ExecCtx<R, E>, scope: &Scope) -> Self {
        let structure_predicate = self.structure_predicate.clone_rebind(ctx, scope);
        let guard =
            self.guard.as_ref().map(|c| Cached::new(c.node.clone_rebind(ctx, scope)));
        Self {
            explicit_type_predicate: self.explicit_type_predicate,
            type_predicate: self.type_predicate.clone(),
            structure_predicate,
            guard,
        }
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
            .map(Cached::new);
        Ok(PatternNode {
            explicit_type_predicate: explicit,
            type_predicate,
            structure_predicate,
            guard,
        })
    }

    pub(super) fn bind_event(
        &self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
        v: &Value,
    ) {
        self.structure_predicate.bind(v, &mut |id, v| {
            ctx.cached.insert(id, v.clone());
            event.variables.insert(id, v);
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
            Some(g) => g.update(ctx, event),
        }
    }

    pub(super) fn is_match(&self, env: &Env, v: &Value) -> bool {
        (!self.explicit_type_predicate || self.type_predicate.is_a(env, v))
            && self.structure_predicate.is_match(v)
            && match &self.guard {
                None => true,
                Some(g) => g
                    .cached
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
