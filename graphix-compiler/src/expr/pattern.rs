use super::{Expr, ModPath, Name, WrittenAt, parser, print::Literal};
use crate::{env::Env, print_as_written, typ::Type};
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

    // CR claude for eric: [structure] The `all@` capture is handled by hand in every
    // arm here and again in Display. An `all(&self) -> Option<&Name>` and a
    // sub-pattern iterator would shrink both to a few lines.
    fn with_names_inner<'a>(&'a self, f: &mut impl FnMut(&'a ArcStr)) {
        match self {
            Self::Bind(n) => f(n),
            Self::Ignore | Self::Literal(_) => (),
            Self::Slice { list: _, all, binds } => {
                if let Some(n) = all {
                    f(n)
                }
                for t in binds.iter() {
                    t.with_names(f)
                }
            }
            Self::SlicePrefix { list: _, all, prefix, tail } => {
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
                for (_, t, _) in binds.iter() {
                    t.with_names(f)
                }
            }
            Self::Or(alts) => alts[0].with_names(f),
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
            // CR claude for eric: [structure] `SliceSuffix` below repeats this arm
            // with `list: false`, and `complete_type_predicate_inner` repeats its
            // List arm as its Array arm but for the constructor. Bind
            // `(list, binds)` once per slice form and share one body.
            Self::Slice { list, all: _, binds }
            | Self::SlicePrefix { list, all: _, prefix: binds, tail: _ } => {
                let mut ts: SmallVec<[Type; 8]> = smallvec![Type::Bottom];
                for p in binds.iter() {
                    ts.push(p.infer_type_predicate(env, scope)?);
                }
                let t = match Type::union(env, &ts.iter().collect::<SmallVec<[_; 8]>>())?
                {
                    Type::Bottom => Type::empty_tvar(),
                    t => t,
                };
                Ok(if *list { Type::List(Arc::new(t)) } else { Type::Array(Arc::new(t)) })
            }
            Self::SliceSuffix { all: _, head: _, suffix: binds } => {
                let mut ts: SmallVec<[Type; 8]> = smallvec![Type::Bottom];
                for p in binds.iter() {
                    ts.push(p.infer_type_predicate(env, scope)?);
                }
                let t = match Type::union(env, &ts.iter().collect::<SmallVec<[_; 8]>>())?
                {
                    Type::Bottom => Type::empty_tvar(),
                    t => t,
                };
                Ok(Type::Array(Arc::new(t)))
            }
            Self::Struct { all: _, exhaustive: _, binds } => {
                let mut typs = binds
                    .iter()
                    .map(|(n, p, _)| {
                        let t = p.infer_type_predicate(env, scope)?;
                        Ok((n.clone(), t, WrittenAt::NOWHERE))
                    })
                    .collect::<Result<SmallVec<[(ArcStr, Type, WrittenAt); 8]>>>()?;
                // CR claude for eric: [perf] `sort_by_key` clones an `ArcStr` per
                // comparison: `sort_by(|a, b| a.0.cmp(&b.0))`.
                typs.sort_by_key(|(n, _, _)| n.clone());
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

    /// Complete a partial struct pattern's inferred type (`{x, ..}` infers
    /// `{x: 'a}`) against the scrutinee: the union, over scrutinee members
    /// carrying the named fields, of the member with those fields replaced
    /// by the pattern's. Recurses through composites. `None` if unchanged.
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
        // CR claude for eric: [risk] 128 (here and in `members`) is an unnamed
        // limit that silently stops the completion: a partial struct pattern
        // nested deeper types differently from a shallow one, and the parser
        // admits deeper patterns. The levels below are not stack-guarded either;
        // recurse through the guarded `complete_type_predicate` and drop it.
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
                        Type::Struct(sf) => binds
                            .iter()
                            .all(|(n, _, _)| sf.iter().any(|(sn, _, _)| sn == n)),
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
                    .map(|(sn, st, at)| match binds.iter().find(|(n, _, _)| n == sn) {
                        Some((_, p, _)) => {
                            let pt = &pfields
                                .iter()
                                .find(|(pn, _, _)| pn == sn)
                                .expect("inferred field missing")
                                .1;
                            let t = p
                                .complete_type_predicate_inner(env, pt, st, depth + 1)?
                                .unwrap_or_else(|| (*pt).clone());
                            Ok((sn.clone(), t, *at))
                        }
                        None => Ok((sn.clone(), st.clone(), *at)),
                    })
                    .collect::<Result<SmallVec<[(ArcStr, Type, WrittenAt); 8]>>>()?;
                Ok(Some(Type::Struct(Arc::from_iter(fields.into_iter()))))
            }
            Self::Struct { all: _, exhaustive: true, binds } => {
                let pfields = match ptype {
                    Type::Struct(f) => f,
                    _ => return Ok(None),
                };
                let mut ms: SmallVec<[Type; 8]> = SmallVec::new();
                members(env, scrutinee, depth, &mut ms);
                // CR claude for eric: [bug] Completes against the FIRST struct
                // member of the scrutinee, whatever its fields. probe: `select v
                // { {x: {z, ..}} => .., _ => .. }` over `[{a: i64, x: {z: i64, w:
                // i64}}, {x: {z: string, q: i64}}]` is refused ("pattern { x: { w:
                // i64, z: _ } } will never match") though it matches the second
                // member. The Tuple, Variant, List and Array arms below pick the
                // first member of their shape too: `select v { (s, {a, ..}) => ..
                // }` over `[(i64, {a: i64, b: i64}), (string, {a: string, c:
                // i64})]` passes the exhaustiveness check, then is bottom for
                // `("x", {a: "s", c: 1})` in both engines. Complete against the
                // members the pattern can match; refuse two, as the partial arm does.
                let sf = match ms.iter().find(|m| matches!(m, Type::Struct(_))) {
                    Some(Type::Struct(sf)) => sf.clone(),
                    _ => return Ok(None),
                };
                let mut changed = false;
                let mut fields: SmallVec<[(ArcStr, Type, WrittenAt); 8]> =
                    SmallVec::new();
                for (n, pt, at) in pfields.iter() {
                    let sub = binds.iter().find(|(bn, _, _)| bn == n);
                    let st = sf.iter().find(|(sn, _, _)| sn == n);
                    match (sub, st) {
                        (Some((_, p, _)), Some((_, st, _))) => {
                            match p.complete_type_predicate_inner(
                                env,
                                pt,
                                st,
                                depth + 1,
                            )? {
                                Some(t) => {
                                    changed = true;
                                    fields.push((n.clone(), t, *at))
                                }
                                None => fields.push((n.clone(), pt.clone(), *at)),
                            }
                        }
                        _ => fields.push((n.clone(), pt.clone(), *at)),
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
                let (pts, at) = match ptype {
                    Type::Variant(_, pts, at) if pts.len() == binds.len() => (pts, *at),
                    _ => return Ok(None),
                };
                let mut ms: SmallVec<[Type; 8]> = SmallVec::new();
                members(env, scrutinee, depth, &mut ms);
                let sts = match ms.iter().find(
                    |m| matches!(m, Type::Variant(t, s, _) if t == tag && s.len() == binds.len()),
                ) {
                    Some(Type::Variant(_, sts, _)) => sts.clone(),
                    _ => return Ok(None),
                };
                let (changed, out) = complete_elems!(binds, pts, sts);
                Ok(changed.then(|| {
                    Type::Variant(tag.clone(), Arc::from_iter(out.into_iter()), at)
                }))
            }
            Self::Slice { list: true, all: _, binds }
            | Self::SlicePrefix { list: true, all: _, prefix: binds, tail: _ } => {
                let pt = match ptype {
                    Type::List(t) => t,
                    _ => return Ok(None),
                };
                let mut ms: SmallVec<[Type; 8]> = SmallVec::new();
                members(env, scrutinee, depth, &mut ms);
                let st = match ms.iter().find(|m| matches!(m, Type::List(_))) {
                    Some(Type::List(st)) => st.clone(),
                    _ => return Ok(None),
                };
                let mut changed = false;
                let mut ts: SmallVec<[Type; 8]> = smallvec![Type::Bottom];
                for p in binds.iter() {
                    let sub = p
                        .complete_type_predicate_inner(env, pt, &st, depth + 1)?
                        .inspect(|_| changed = true)
                        .unwrap_or_else(|| (**pt).clone());
                    ts.push(sub);
                }
                if !changed {
                    return Ok(None);
                }
                return Ok(Some(Type::List(Arc::new(Type::union(
                    env,
                    &ts.iter().collect::<SmallVec<[_; 8]>>(),
                )?))));
            }
            Self::Slice { list: false, all: _, binds }
            | Self::SlicePrefix { list: false, all: _, prefix: binds, tail: _ }
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
                let mut ts: SmallVec<[Type; 8]> = smallvec![Type::Bottom];
                for p in binds.iter() {
                    let sub = p
                        .complete_type_predicate_inner(env, pt, &st, depth + 1)?
                        .inspect(|_| changed = true)
                        .unwrap_or_else(|| (**pt).clone());
                    ts.push(sub);
                }
                if !changed {
                    return Ok(None);
                }
                Ok(Some(Type::Array(Arc::new(Type::union(
                    env,
                    &ts.iter().collect::<SmallVec<[_; 8]>>(),
                )?))))
            }
            Self::Or(alts) => {
                let pts = match ptype {
                    Type::Set(pts) if pts.len() == alts.len() => pts,
                    _ => return Ok(None),
                };
                let mut changed = false;
                let mut out: SmallVec<[Type; 8]> = SmallVec::new();
                for (p, pt) in alts.iter().zip(pts.iter()) {
                    match p.complete_type_predicate_inner(
                        env,
                        pt,
                        scrutinee,
                        depth + 1,
                    )? {
                        Some(t) => {
                            changed = true;
                            out.push(t)
                        }
                        None => out.push(pt.clone()),
                    }
                }
                Ok(changed.then(|| Type::Set(Arc::from_iter(out.into_iter()))))
            }
            Self::Ignore | Self::Bind(_) | Self::Literal(_) => Ok(None),
        }
    }
}

// CR claude for eric: [structure] Patterns have no `PrettyDisplay`, so a long
// destructuring or type predicate never breaks and pushes its value under the
// head. probe: a seven-field `let { alpha_field, .. } = s` prints a 97-column
// line, then `s;` on its own.
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
            StructurePattern::Literal(v) => write!(f, "{}", Literal(v)),
            StructurePattern::Bind(n) => write!(f, "{n}"),
            StructurePattern::Slice { list, all, binds } => {
                if let Some(all) = all {
                    write!(f, "{all}@ ")?
                }
                write!(f, "{}", if *list { "[<" } else { "[" })?;
                with_sep!(binds);
                write!(f, "{}", if *list { ">]" } else { "]" })
            }
            StructurePattern::SlicePrefix { list, all, prefix, tail } => {
                if let Some(all) = all {
                    write!(f, "{all}@ ")?
                }
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
            // CR claude for eric: [style] `x@` takes a space before a slice, tuple
            // or struct pattern but not before a variant or abstract one: `kk@ `Up`
            // (the book's spelling) formats to `kk@`Up`, `c@ T(x)` to `c@T(x)`.
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
            StructurePattern::Or(alts) => {
                for (i, p) in alts.iter().enumerate() {
                    write!(f, "{p}")?;
                    if i < alts.len() - 1 {
                        write!(f, " | ")?
                    }
                }
                Ok(())
            }
            StructurePattern::Struct { exhaustive, all, binds } => {
                if let Some(all) = all {
                    write!(f, "{all}@ ")?
                }
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
