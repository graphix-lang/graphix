use super::{Expr, ModPath};
use crate::{env::Env, typ::Type};
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
    Bind(ArcStr),
    Slice {
        all: Option<ArcStr>,
        binds: Arc<[StructurePattern]>,
    },
    SlicePrefix {
        all: Option<ArcStr>,
        prefix: Arc<[StructurePattern]>,
        tail: Option<ArcStr>,
    },
    SliceSuffix {
        all: Option<ArcStr>,
        head: Option<ArcStr>,
        suffix: Arc<[StructurePattern]>,
    },
    Tuple {
        all: Option<ArcStr>,
        binds: Arc<[StructurePattern]>,
    },
    Variant {
        all: Option<ArcStr>,
        tag: ArcStr,
        binds: Arc<[StructurePattern]>,
    },
    /// `T(p)` — destructure a value of the abstract type at `name`
    /// into its payload (`design/nominal_abstract_types.md`)
    Abstract {
        all: Option<ArcStr>,
        name: ModPath,
        bind: Arc<StructurePattern>,
    },
    Struct {
        exhaustive: bool,
        all: Option<ArcStr>,
        binds: Arc<[(ArcStr, StructurePattern)]>,
    },
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
            | Self::Abstract { .. } => None,
        }
    }

    pub fn with_names<'a>(&'a self, f: &mut impl FnMut(&'a ArcStr)) {
        crate::stack::ensure_sufficient(|| self.with_names_inner(f))
    }

    fn with_names_inner<'a>(&'a self, f: &mut impl FnMut(&'a ArcStr)) {
        match self {
            Self::Bind(n) => f(n),
            Self::Ignore | Self::Literal(_) => (),
            Self::Slice { all, binds } => {
                if let Some(n) = all {
                    f(n)
                }
                for t in binds.iter() {
                    t.with_names(f)
                }
            }
            Self::SlicePrefix { all, prefix, tail } => {
                if let Some(n) = all {
                    f(n)
                }
                if let Some(n) = tail {
                    f(n)
                }
                for t in prefix.iter() {
                    t.with_names(f)
                }
            }
            Self::SliceSuffix { all, head, suffix } => {
                if let Some(n) = all {
                    f(n)
                }
                if let Some(n) = head {
                    f(n)
                }
                for t in suffix.iter() {
                    t.with_names(f)
                }
            }
            Self::Tuple { all, binds } => {
                if let Some(n) = all {
                    f(n)
                }
                for t in binds.iter() {
                    t.with_names(f)
                }
            }
            Self::Variant { all, tag: _, binds } => {
                if let Some(n) = all {
                    f(n)
                }
                for t in binds.iter() {
                    t.with_names(f)
                }
            }
            Self::Abstract { all, name: _, bind } => {
                if let Some(n) = all {
                    f(n)
                }
                bind.with_names(f)
            }
            Self::Struct { exhaustive: _, all, binds } => {
                if let Some(n) = all {
                    f(n)
                }
                for (_, t) in binds.iter() {
                    t.with_names(f)
                }
            }
        }
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
            // `Any` is load-bearing here: a catch-all `_` arm's
            // predicate must match EVERYTHING for exhaustiveness,
            // dead-arm analysis, and runtime dispatch. It does make
            // select's unification-by-contains walk short-circuit at
            // `_` slots (`T.contains(Any)` is false) — the select
            // typecheck compensates by unifying through a view that
            // substitutes fresh TVars for Any (`Type::any_as_tvar`),
            // so slots AFTER a `_` still narrow.
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
                Ok(Type::Variant(tag.clone(), Arc::from_iter(a)))
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
            Self::Slice { all: _, binds }
            | Self::SlicePrefix { all: _, prefix: binds, tail: _ }
            | Self::SliceSuffix { all: _, head: _, suffix: binds } => {
                let t =
                    binds.iter().fold(Ok::<_, anyhow::Error>(Type::Bottom), |t, p| {
                        Ok(t?.union(env, &p.infer_type_predicate(env, scope)?)?)
                    })?;
                let t = match t {
                    Type::Bottom => Type::empty_tvar(),
                    t => t,
                };
                Ok(Type::Array(Arc::new(t)))
            }
            Self::Struct { all: _, exhaustive: _, binds } => {
                let mut typs = binds
                    .iter()
                    .map(|(n, p)| Ok((n.clone(), p.infer_type_predicate(env, scope)?)))
                    .collect::<Result<SmallVec<[(ArcStr, Type); 8]>>>()?;
                typs.sort_by_key(|(n, _)| n.clone());
                Ok(Type::Struct(Arc::from_iter(typs.into_iter())))
            }
        }
    }

    /// Complete a PARTIAL struct pattern's inferred type against the
    /// scrutinee. `{x, ..}` infers `{x: 'a}` — an exact one-field
    /// struct that can never match the real `{x: .., y: ..}` member —
    /// because inference is bottom-up and the pattern doesn't name the
    /// rest. When the scrutinee type is known, the rest IS known: for
    /// each scrutinee member that is a struct carrying all the named
    /// fields, take the member's full field list with the named
    /// fields' types replaced by the pattern's, and union the results.
    /// Recurses through tuples, variants, exhaustive structs, and
    /// slices so a partial pattern completes at any nesting depth.
    /// Returns `None` when nothing changed (no partial struct below,
    /// or no scrutinee member fits — the coverage checks then report
    /// as before).
    pub fn complete_type_predicate(
        &self,
        env: &Env,
        ptype: &Type,
        scrutinee: &Type,
    ) -> Result<Option<Type>> {
        crate::stack::ensure_sufficient(|| {
            self.complete_type_predicate_inner(env, ptype, scrutinee, 0)
        })
    }

    fn complete_type_predicate_inner(
        &self,
        env: &Env,
        ptype: &Type,
        scrutinee: &Type,
        depth: usize,
    ) -> Result<Option<Type>> {
        if depth > 128 {
            return Ok(None);
        }
        // The scrutinee members a pattern position could be matching:
        // deref tvars, expand refs, flatten unions.
        fn members(env: &Env, t: &Type, depth: usize, out: &mut SmallVec<[Type; 8]>) {
            if depth > 128 {
                return;
            }
            t.with_deref(|t| match t {
                None => (),
                Some(Type::Set(s)) => {
                    for t in s.iter() {
                        members(env, t, depth + 1, out)
                    }
                }
                Some(t @ Type::Ref(_)) => match t.lookup_ref(env) {
                    Ok(t) => members(env, &t, depth + 1, out),
                    Err(_) => (),
                },
                Some(t) => out.push(t.clone()),
            })
        }
        macro_rules! complete_elems {
            ($binds:expr, $ptypes:expr, $stypes:expr) => {{
                let mut changed = false;
                let mut out: SmallVec<[Type; 8]> = SmallVec::new();
                for ((p, pt), st) in $binds.iter().zip($ptypes.iter()).zip($stypes.iter())
                {
                    match p.complete_type_predicate_inner(env, pt, st, depth + 1)? {
                        Some(t) => {
                            changed = true;
                            out.push(t)
                        }
                        None => out.push(pt.clone()),
                    }
                }
                (changed, out)
            }};
        }
        match self {
            Self::Struct { all: _, exhaustive: false, binds } => {
                let pfields = match ptype {
                    Type::Struct(f) => f,
                    _ => return Ok(None),
                };
                let mut ms: SmallVec<[Type; 8]> = SmallVec::new();
                members(env, scrutinee, depth, &mut ms);
                let matching: SmallVec<[&Type; 8]> = ms
                    .iter()
                    .filter(|m| match m {
                        Type::Struct(sf) => {
                            binds.iter().all(|(n, _)| sf.iter().any(|(sn, _)| sn == n))
                        }
                        _ => false,
                    })
                    .collect();
                let sf = match &matching[..] {
                    [] => return Ok(None),
                    [Type::Struct(sf)] => sf,
                    _ => bail!(
                        "the partial pattern {self} matches more than one member \
                         of {scrutinee}; annotate the member you mean, e.g. \
                         `T as {self}`"
                    ),
                };
                let fields = sf
                    .iter()
                    .map(|(sn, st)| match binds.iter().find(|(n, _)| n == sn) {
                        Some((_, p)) => {
                            let pt = &pfields
                                .iter()
                                .find(|(pn, _)| pn == sn)
                                .expect("inferred field missing")
                                .1;
                            let t = p
                                .complete_type_predicate_inner(env, pt, st, depth + 1)?
                                .unwrap_or_else(|| (*pt).clone());
                            Ok((sn.clone(), t))
                        }
                        None => Ok((sn.clone(), st.clone())),
                    })
                    .collect::<Result<SmallVec<[(ArcStr, Type); 8]>>>()?;
                Ok(Some(Type::Struct(Arc::from_iter(fields.into_iter()))))
            }
            Self::Struct { all: _, exhaustive: true, binds } => {
                let pfields = match ptype {
                    Type::Struct(f) => f,
                    _ => return Ok(None),
                };
                let mut ms: SmallVec<[Type; 8]> = SmallVec::new();
                members(env, scrutinee, depth, &mut ms);
                let sf = match ms.iter().find(|m| matches!(m, Type::Struct(_))) {
                    Some(Type::Struct(sf)) => sf.clone(),
                    _ => return Ok(None),
                };
                let mut changed = false;
                let mut fields: SmallVec<[(ArcStr, Type); 8]> = SmallVec::new();
                for (n, pt) in pfields.iter() {
                    let sub = binds.iter().find(|(bn, _)| bn == n);
                    let st = sf.iter().find(|(sn, _)| sn == n);
                    match (sub, st) {
                        (Some((_, p)), Some((_, st))) => {
                            match p.complete_type_predicate_inner(
                                env,
                                pt,
                                st,
                                depth + 1,
                            )? {
                                Some(t) => {
                                    changed = true;
                                    fields.push((n.clone(), t))
                                }
                                None => fields.push((n.clone(), pt.clone())),
                            }
                        }
                        _ => fields.push((n.clone(), pt.clone())),
                    }
                }
                Ok(changed.then(|| Type::Struct(Arc::from_iter(fields.into_iter()))))
            }
            Self::Tuple { all: _, binds } => {
                let pts = match ptype {
                    Type::Tuple(pts) if pts.len() == binds.len() => pts,
                    _ => return Ok(None),
                };
                let mut ms: SmallVec<[Type; 8]> = SmallVec::new();
                members(env, scrutinee, depth, &mut ms);
                let sts = match ms
                    .iter()
                    .find(|m| matches!(m, Type::Tuple(s) if s.len() == binds.len()))
                {
                    Some(Type::Tuple(sts)) => sts.clone(),
                    _ => return Ok(None),
                };
                let (changed, out) = complete_elems!(binds, pts, sts);
                Ok(changed.then(|| Type::Tuple(Arc::from_iter(out.into_iter()))))
            }
            Self::Abstract { .. } => Ok(None),
            Self::Variant { all: _, tag, binds } => {
                let pts = match ptype {
                    Type::Variant(_, pts) if pts.len() == binds.len() => pts,
                    _ => return Ok(None),
                };
                let mut ms: SmallVec<[Type; 8]> = SmallVec::new();
                members(env, scrutinee, depth, &mut ms);
                let sts = match ms.iter().find(
                    |m| matches!(m, Type::Variant(t, s) if t == tag && s.len() == binds.len()),
                ) {
                    Some(Type::Variant(_, sts)) => sts.clone(),
                    _ => return Ok(None),
                };
                let (changed, out) = complete_elems!(binds, pts, sts);
                Ok(changed
                    .then(|| Type::Variant(tag.clone(), Arc::from_iter(out.into_iter()))))
            }
            Self::Slice { all: _, binds }
            | Self::SlicePrefix { all: _, prefix: binds, tail: _ }
            | Self::SliceSuffix { all: _, head: _, suffix: binds } => {
                let pt = match ptype {
                    Type::Array(t) => t,
                    _ => return Ok(None),
                };
                let mut ms: SmallVec<[Type; 8]> = SmallVec::new();
                members(env, scrutinee, depth, &mut ms);
                let st = match ms.iter().find(|m| matches!(m, Type::Array(_))) {
                    Some(Type::Array(st)) => st.clone(),
                    _ => return Ok(None),
                };
                let mut changed = false;
                let mut t = Type::Bottom;
                for p in binds.iter() {
                    let sub = p
                        .complete_type_predicate_inner(env, pt, &st, depth + 1)?
                        .inspect(|_| changed = true)
                        .unwrap_or_else(|| (**pt).clone());
                    t = t.union(env, &sub)?;
                }
                Ok(changed.then(|| Type::Array(Arc::new(t))))
            }
            Self::Ignore | Self::Bind(_) | Self::Literal(_) => Ok(None),
        }
    }
}

impl fmt::Display for StructurePattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
        match self {
            StructurePattern::Ignore => write!(f, "_"),
            StructurePattern::Literal(v) => write!(f, "{v}"),
            StructurePattern::Bind(n) => write!(f, "{n}"),
            StructurePattern::Slice { all, binds } => {
                if let Some(all) = all {
                    write!(f, "{all}@ ")?
                }
                write!(f, "[")?;
                with_sep!(binds);
                write!(f, "]")
            }
            StructurePattern::SlicePrefix { all, prefix, tail } => {
                if let Some(all) = all {
                    write!(f, "{all}@ ")?
                }
                write!(f, "[")?;
                for b in prefix.iter() {
                    write!(f, "{b}, ")?
                }
                match tail {
                    None => write!(f, "..]"),
                    Some(name) => write!(f, "{name}..]"),
                }
            }
            StructurePattern::SliceSuffix { all, head, suffix } => {
                if let Some(all) = all {
                    write!(f, "{all}@ ")?
                }
                write!(f, "[")?;
                match head {
                    None => write!(f, ".., ")?,
                    Some(name) => write!(f, "{name}.., ")?,
                }
                with_sep!(suffix);
                write!(f, "]")
            }
            StructurePattern::Tuple { all, binds } => {
                if let Some(all) = all {
                    write!(f, "{all}@ ")?
                }
                write!(f, "(")?;
                with_sep!(binds);
                write!(f, ")")
            }
            StructurePattern::Variant { all, tag, binds } if binds.len() == 0 => {
                if let Some(all) = all {
                    write!(f, "{all}@")?
                }
                write!(f, "`{tag}")
            }
            StructurePattern::Variant { all, tag, binds } => {
                if let Some(all) = all {
                    write!(f, "{all}@")?
                }
                write!(f, "`{tag}(")?;
                with_sep!(binds);
                write!(f, ")")
            }
            StructurePattern::Abstract { all, name, bind } => {
                if let Some(all) = all {
                    write!(f, "{all}@")?
                }
                write!(f, "{name}({bind})")
            }
            StructurePattern::Struct { exhaustive, all, binds } => {
                if let Some(all) = all {
                    write!(f, "{all}@ ")?
                }
                write!(f, "{{")?;
                for (i, (name, pat)) in binds.iter().enumerate() {
                    write!(f, "{name}: {pat}")?;
                    if !exhaustive || i < binds.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                if !exhaustive {
                    write!(f, "..")?
                }
                write!(f, "}}")
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
