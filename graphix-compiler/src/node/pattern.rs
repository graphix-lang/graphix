use crate::SourcePosition;
use crate::image::ImageBuf;
use crate::{
    BindId, CFlag, CompileCtx, ExecCtx, PrintFlag, Rt, Scope, Tag, TagValue, UserEvent,
    env::Env,
    expr::{ExprId, Name, Origin, Pattern, StructurePattern, WrittenAt},
    format_with_flags,
    node::{Held, compiler, list},
    typ::{AbstractId, IsAFlags, Type},
};
use ahash::AHashMap;
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, encode_varint, varint_len};
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
/// adds nothing to the env. A name the alternatives share types as the
/// union of their types: under a written predicate here, under an
/// inferred one once the select narrows them.
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

/// What stays fixed across one pattern's compile.
#[derive(Clone, Copy)]
struct PatCx<'a> {
    scope: &'a Scope,
    pos: SourcePosition,
    ori: &'a Arc<Origin>,
    /// The predicate was inferred from this pattern, so an or-pattern's
    /// Set holds one member per alternative, in order.
    inferred: bool,
}

fn leaf_bind<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    cx: PatCx,
    name: &Name,
    typ: &Type,
    mode: &mut BindMode,
) -> Result<BindId> {
    let pos = name.pos_or(cx.pos);
    let name = &name.name;
    let fresh = |ctx: &mut CompileCtx<R, E>| {
        ctx.env
            .bind_variable(&cx.scope.lexical, name, typ.clone(), pos, cx.ori.clone())
            .id
    };
    match mode {
        BindMode::Fresh => Ok(fresh(ctx)),
        BindMode::Record(map) => {
            let id = fresh(ctx);
            map.insert(name.clone(), (id, typ.clone()));
            Ok(id)
        }
        BindMode::Reuse(map) => match map.get(name) {
            // XCR claude for claude: [bug] Under an inferred predicate, this two-way
            // contains binds the open cells inside both alternatives' inferred
            // types. For a reused capture that binding is the only lasting effect,
            // because PatternNode::compile retypes captures and bind_captures types
            // them later. So `p@ (0, y) | p@ (y, 0) => y + 1` over `([i64, null],
            // [i64, null])` binds both y cells to i64, taken from the other
            // alternative's literal. The arm is accepted (without `p@` it is
            // refused, and the correct `y$ + 1` is refused), and f((0, null))
            // panics the JIT at fusion/kernel.rs:243 while the node-walk logs
            // "can't add null". The same probe gives a name that is a payload in
            // one alternative and a capture in the other the capture's type alone:
            // `` `A(x) | x@ `B `` over `` [`A([i64, `B]), `B] `` makes x `` `B ``,
            // so f(`A(5)) bottoms. A rest is compared here as a payload
            // (capture=false at :417) but typed as a capture, so `[1, r..] | [r..,
            // "a"]` over Array<[i64, string]> is refused; probe:
            // design/review-2026-10-05/repro/c-pattern-04.gx (c-pattern-04)
            // 2026-10-08 claude: under an inferred predicate a reused name only shares
            // its id here; StructPatternNode::leaves judges the alternatives after the
            // select narrows each over the scrutinee: plain binds must agree exactly, and
            // a name some alternative captures (a rest included) takes the union. Pins:
            // lang::select::or_capture_and_payload, pattern_typing_refusals.
            // 2026-10-08 claude: Eric ruled: a name or-alternatives share is always the
            // union of their narrowed types, plain binds included (no exact-equality
            // rule). Pin: lang::select::or_binds_union.
            // under an inferred predicate the alternatives' types are
            // judged once the select narrows them (`leaves`)
            Some((id, _)) if cx.inferred => Ok(*id),
            Some((id, t0)) => {
                let id = *id;
                let probe = BitFlags::empty();
                if !(t0.contains_with_flags(probe, &ctx.env, typ)?
                    && typ.contains_with_flags(probe, &ctx.env, t0)?)
                {
                    let u = Type::union(&ctx.env, &[t0, typ])?;
                    map.insert(name.clone(), (id, u.clone()));
                    ctx.env.retype(id, u);
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

/// Bind a `name@` capture: the whole value at this position.
fn bind_all<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    cx: PatCx,
    all: &Option<Name>,
    typ: &Type,
    mode: &mut BindMode,
) -> Result<Option<BindId>> {
    all.as_ref().map(|n| leaf_bind(ctx, cx, n, typ, mode)).transpose()
}

/// The members of an inferred Set, one per child: an or-pattern's
/// alternatives, or a slice's elements followed by its rest.
pub(crate) fn set_members(typ: &Type, n: usize) -> Option<Arc<[Type]>> {
    typ.with_deref(|t| match t {
        Some(Type::Set(ts)) if ts.len() == n => Some(ts.clone()),
        _ => None,
    })
}

/// `typ` through its bindings and aliases: the constructor at its head.
/// The chase fills no alias's cell: a name the body names may yet be
/// declared by a later statement.
fn expand(env: &Env, typ: &Type) -> Option<Type> {
    let mut t = typ.deref_cloned()?;
    while let Type::Ref(_) = t {
        t = t.lookup_ref_peek(env).ok()?.deref_cloned()?;
    }
    Some(t)
}

fn struct_fields(env: &Env, typ: &Type) -> Option<Arc<[(ArcStr, Type, WrittenAt)]>> {
    typ.with_deref(|t| match t {
        Some(t @ Type::Ref(_)) => match &t.lookup_ref(env) {
            Ok(Type::Struct(elts)) => Some(elts.clone()),
            Ok(_) | Err(_) => None,
        },
        Some(Type::Struct(elts)) => Some(elts.clone()),
        _ => None,
    })
}

/// One leaf name of a pattern with the part of a type it stands over.
/// `fresh`: the name's own cell is fresh and takes `typ` once the select
/// narrows the arm: a capture (`name@`, a slice's rest), which stands
/// over a whole position, or a name or-alternatives share, which is the
/// union of theirs. Any other plain bind is its position's own cell.
pub(super) struct Leaf {
    pub(super) id: BindId,
    pub(super) typ: Type,
    pub(super) fresh: bool,
}

impl StructPatternNode {
    /// This pattern's children, in the order [`Self::child_types`]
    /// gives their types.
    fn children(&self) -> SmallVec<[&Self; 8]> {
        match self {
            Self::Ignore | Self::Literal(_) | Self::Bind(_) => SmallVec::new(),
            Self::Or { alts: ps }
            | Self::Variant { binds: ps, .. }
            | Self::Slice { binds: ps, .. }
            | Self::SlicePrefix { prefix: ps, .. }
            | Self::SliceSuffix { suffix: ps, .. } => ps.iter().collect(),
            Self::Struct { binds, .. } => binds.iter().map(|(_, _, p)| p).collect(),
            Self::Abstract { bind, .. } => smallvec![&**bind],
        }
    }

    // XCR claude for claude: [structure] captures_inner and realign_inner (:284) derive
    // the type at each child position with the same code: alt_types for an or,
    // struct_fields for a struct, with_deref for variant and tuple payloads and slice
    // elements, rep for an abstract. They differ only in what they do with each child
    // (collect captures, or reset struct field indexes) and on a missing struct field
    // (skipped here, an error there). Any change to how a position is typed must be
    // made twice: the pairing of or-alternatives with Set members by count, which
    // mistypes or-patterns inside slices, is already in both, and alt_types is inlined
    // a third time at fusion/emit/select.rs:1798. One helper that yields each child's
    // type, with a struct child's field index, would give both walks a single
    // derivation. (c-pattern-12)
    // 2026-10-08 claude: child_types is the one derivation: leaves, realign and footprint
    // walk it, and fusion's or-chain calls set_members.
    /// The part of `typ`, a type of this pattern's shape, each child
    /// stands over, with a struct child's field index; `None` when `typ`
    /// is not of the shape, and a `None` part for a field it lacks. Under
    /// an inferred predicate an or-pattern's alternatives and a slice's
    /// elements each stand over their own member of a Set; under a
    /// written one, over the whole type or its element type.
    fn child_types(
        &self,
        env: &Env,
        typ: &Type,
        inferred: bool,
    ) -> Option<SmallVec<[Option<(Type, usize)>; 8]>> {
        let expanded = expand(env, typ);
        let typ = expanded.as_ref().unwrap_or(typ);
        fn at(ts: &[Type]) -> SmallVec<[Option<(Type, usize)>; 8]> {
            ts.iter().enumerate().map(|(i, t)| Some((t.clone(), i))).collect()
        }
        let payload = |n: usize| {
            typ.with_deref(|t| match t {
                Some(Type::Variant(_, ts, _) | Type::Tuple(ts)) if ts.len() == n => {
                    Some(ts.clone())
                }
                _ => None,
            })
        };
        let elems = |n: usize, rest: bool| {
            let et = typ.with_deref(|t| match t {
                Some(Type::Array(et) | Type::List(et)) => Some((**et).clone()),
                _ => None,
            })?;
            match inferred {
                true => set_members(&et, n + rest as usize),
                false => Some(Arc::from_iter((0..n).map(|_| et.clone()))),
            }
        };
        match self {
            Self::Ignore | Self::Literal(_) | Self::Bind(_) => Some(SmallVec::new()),
            Self::Or { alts } => match inferred {
                true => set_members(typ, alts.len()).map(|ts| at(&ts)),
                false => Some(
                    alts.iter()
                        .enumerate()
                        .map(|(i, _)| Some((typ.clone(), i)))
                        .collect(),
                ),
            },
            Self::Variant { binds, .. }
            | Self::Slice { kind: SliceKind::Tuple, binds, .. } => {
                payload(binds.len()).map(|ts| at(&ts))
            }
            Self::Slice { kind: SliceKind::Array | SliceKind::List, binds, .. } => {
                elems(binds.len(), false).map(|ts| at(&ts[..binds.len()]))
            }
            Self::SlicePrefix { prefix: ps, .. }
            | Self::SliceSuffix { suffix: ps, .. } => {
                elems(ps.len(), true).map(|ts| at(&ts[..ps.len()]))
            }
            Self::Struct { binds, .. } => {
                let fields = struct_fields(env, typ)?;
                Some(
                    binds
                        .iter()
                        .map(|(name, _, _)| {
                            let i = fields.iter().position(|(n, _, _)| n == name)?;
                            Some((fields[i].1.clone(), i))
                        })
                        .collect(),
                )
            }
            Self::Abstract { rep, .. } => Some(smallvec![Some((rep.clone(), 0))]),
        }
    }

    /// Whether this pattern's children stand over an inferred predicate,
    /// given that it does: an abstract's payload checks against its
    /// declared representation.
    fn children_inferred(&self, inferred: bool) -> bool {
        inferred && !matches!(self, Self::Abstract { .. })
    }

    /// The `all@` capture and the slice rest, each standing over the
    /// whole of this pattern's type.
    fn whole_binds(&self) -> impl Iterator<Item = BindId> {
        let (all, rest) = match self {
            Self::Ignore | Self::Literal(_) | Self::Bind(_) | Self::Or { .. } => {
                (None, None)
            }
            Self::SlicePrefix { all, tail: rest, .. }
            | Self::SliceSuffix { all, head: rest, .. } => (*all, *rest),
            Self::Slice { all, .. }
            | Self::Struct { all, .. }
            | Self::Variant { all, .. }
            | Self::Abstract { all, .. } => (*all, None),
        };
        all.into_iter().chain(rest)
    }

    /// Every leaf name with the part of `typ` it stands over, `typ`
    /// being a type of this pattern's shape. An or-pattern's
    /// alternatives share their names: each is reported once, at the
    /// union of the alternatives' parts; with `checking`, alternatives a
    /// run-time test cannot tell apart are refused, as separate arms
    /// would be.
    pub(super) fn leaves(
        &self,
        env: &Env,
        typ: &Type,
        inferred: bool,
        checking: bool,
        out: &mut SmallVec<[Leaf; 4]>,
    ) -> Result<()> {
        crate::stack::ensure_sufficient(|| {
            self.leaves_inner(env, typ, inferred, checking, out)
        })
    }

    fn leaves_inner(
        &self,
        env: &Env,
        typ: &Type,
        inferred: bool,
        checking: bool,
        out: &mut SmallVec<[Leaf; 4]>,
    ) -> Result<()> {
        if let Self::Bind(id) = self {
            out.push(Leaf { id: *id, typ: typ.clone(), fresh: false });
            return Ok(());
        }
        for id in self.whole_binds() {
            out.push(Leaf { id, typ: typ.normalize(), fresh: true })
        }
        let Some(ts) = self.child_types(env, typ, inferred) else { return Ok(()) };
        let sub = self.children_inferred(inferred);
        let Self::Or { alts } = self else {
            for (c, t) in self.children().into_iter().zip(ts) {
                if let Some((t, _)) = t {
                    c.leaves(env, &t, sub, checking, out)?
                }
            }
            return Ok(());
        };
        let ts: SmallVec<[Type; 4]> = ts.into_iter().flatten().map(|(t, _)| t).collect();
        if checking {
            for (i, a) in alts.iter().enumerate() {
                let fp = a.footprint(env, &ts[i], sub);
                for (j, b) in ts.iter().enumerate() {
                    if i != j && fp.rep_collision(env, b).is_some() {
                        let (a, b) = (ts[i].resolve_tvars(), b.resolve_tvars());
                        return format_with_flags(PrintFlag::DerefTVars, || {
                            bail!(
                                "these or-pattern alternatives can't tell {a} from {b}: \
                                 an alternative is taken by its structure, which a value \
                                 of either type passes; write them as separate arms"
                            )
                        });
                    }
                }
            }
        }
        let mut per: SmallVec<[SmallVec<[Leaf; 4]>; 4]> = SmallVec::new();
        for (a, t) in alts.iter().zip(ts.iter()) {
            let mut v = SmallVec::new();
            a.leaves(env, t, sub, checking, &mut v)?;
            per.push(v)
        }
        let Some((first, rest)) = per.split_first() else { return Ok(()) };
        for l in first.iter() {
            let others = rest.iter().flat_map(|v| v.iter().filter(|o| o.id == l.id));
            let ts: SmallVec<[&Type; 4]> =
                std::iter::once(&l.typ).chain(others.map(|o| &o.typ)).collect();
            out.push(Leaf { id: l.id, typ: Type::union(env, &ts)?, fresh: true });
        }
        Ok(())
    }

    /// What this pattern's structure test admits over `typ`: `typ` with
    /// every position the structure does not test as `Any`. An
    /// or-alternative is taken by its structure alone, so a value of
    /// another alternative's type this admits would bind through it.
    fn footprint(&self, env: &Env, typ: &Type, inferred: bool) -> Type {
        crate::stack::ensure_sufficient(|| {
            let sub = self.children_inferred(inferred);
            let parts = |ts: Option<SmallVec<[Option<(Type, usize)>; 8]>>| {
                let ts = ts?;
                let fps = self.children().into_iter().zip(ts).map(|(c, t)| match t {
                    Some((t, _)) => c.footprint(env, &t, sub),
                    None => Type::Any,
                });
                Some(fps.collect::<SmallVec<[Type; 8]>>())
            };
            match self {
                Self::Ignore | Self::Bind(_) => Type::Any,
                Self::Literal(_) | Self::Abstract { .. } => typ.clone(),
                Self::Slice { kind: SliceKind::List, .. }
                | Self::SlicePrefix { list: true, .. } => Type::List(Arc::new(Type::Any)),
                Self::Slice { kind: SliceKind::Array, .. }
                | Self::SlicePrefix { .. }
                | Self::SliceSuffix { .. } => Type::Array(Arc::new(Type::Any)),
                Self::Slice { kind: SliceKind::Tuple, .. } => {
                    match parts(self.child_types(env, typ, inferred)) {
                        Some(ps) => Type::Tuple(Arc::from_iter(ps)),
                        None => typ.clone(),
                    }
                }
                Self::Variant { tag, .. } => {
                    match parts(self.child_types(env, typ, inferred)) {
                        Some(ps) => Type::Variant(
                            tag.clone(),
                            Arc::from_iter(ps),
                            WrittenAt::NOWHERE,
                        ),
                        None => typ.clone(),
                    }
                }
                Self::Or { .. } => match parts(self.child_types(env, typ, inferred)) {
                    Some(ps) => Type::Set(Arc::from_iter(ps)),
                    None => typ.clone(),
                },
                Self::Struct { binds, .. } => {
                    let Some(fields) = struct_fields(env, typ) else {
                        return typ.clone();
                    };
                    let fields = fields.iter().map(|(n, t, at)| {
                        let t = match binds.iter().find(|(bn, _, _)| bn == n) {
                            Some((_, _, p)) => p.footprint(env, t, sub),
                            None => Type::Any,
                        };
                        (n.clone(), t, *at)
                    });
                    Type::Struct(Arc::from_iter(fields))
                }
            }
        })
    }

    /// The id of every [`Leaf::fresh`] name: each capture and every
    /// name an or-pattern binds.
    fn fresh_ids(&self, f: &mut impl FnMut(BindId)) {
        crate::stack::ensure_sufficient(|| match self {
            Self::Or { .. } => self.ids(&mut *f),
            _ => {
                self.whole_binds().for_each(&mut *f);
                for c in self.children() {
                    c.fresh_ids(f)
                }
            }
        })
    }

    /// Re-derive the struct binders' field indexes from a completed
    /// type predicate: a partial pattern compiles against the fields it
    /// names, so its indexes are wrong once the select typecheck
    /// completes the predicate from the scrutinee.
    pub(super) fn realign(&mut self, env: &Env, typ: &Type) -> Result<()> {
        self.realign_with(env, typ, true)
    }

    fn realign_with(&mut self, env: &Env, typ: &Type, inferred: bool) -> Result<()> {
        crate::stack::ensure_sufficient(|| {
            let Some(ts) = self.child_types(env, typ, inferred) else { return Ok(()) };
            let sub = self.children_inferred(inferred);
            match self {
                Self::Struct { binds, all: _ } => {
                    for ((name, index, p), t) in binds.iter_mut().zip(ts) {
                        let Some((t, i)) = t else {
                            bail!("no such struct field {name} in {typ}")
                        };
                        *index = i;
                        p.realign_with(env, &t, sub)?
                    }
                    Ok(())
                }
                Self::Ignore | Self::Literal(_) | Self::Bind(_) => Ok(()),
                Self::Or { alts: ps }
                | Self::Variant { binds: ps, .. }
                | Self::Slice { binds: ps, .. }
                | Self::SlicePrefix { prefix: ps, .. }
                | Self::SliceSuffix { suffix: ps, .. } => {
                    for (p, t) in ps.iter_mut().zip(ts) {
                        if let Some((t, _)) = t {
                            p.realign_with(env, &t, sub)?
                        }
                    }
                    Ok(())
                }
                Self::Abstract { bind, .. } => match ts.into_iter().next().flatten() {
                    Some((t, _)) => bind.realign_with(env, &t, sub),
                    None => Ok(()),
                },
            }
        })
    }

    /// Compile `spec` against the explicit type `type_predicate` (a
    /// `let` or lambda parameter's type).
    pub fn compile<R: Rt, E: UserEvent>(
        ctx: &mut CompileCtx<R, E>,
        type_predicate: &Type,
        spec: &StructurePattern,
        scope: &Scope,
        pos: SourcePosition,
        ori: Arc<Origin>,
    ) -> Result<Self> {
        let cx = PatCx { scope, pos, ori: &ori, inferred: false };
        Self::compile_with(ctx, cx, type_predicate, spec)
    }

    fn compile_with<R: Rt, E: UserEvent>(
        ctx: &mut CompileCtx<R, E>,
        cx: PatCx,
        type_predicate: &Type,
        spec: &StructurePattern,
    ) -> Result<Self> {
        if !spec.binds_uniq() {
            bail!("bound variables must have unique names")
        }
        Self::compile_int(ctx, cx, type_predicate, spec, BindMode::Fresh)
    }

    fn compile_int<R: Rt, E: UserEvent>(
        ctx: &mut CompileCtx<R, E>,
        cx: PatCx,
        type_predicate: &Type,
        spec: &StructurePattern,
        mode: BindMode,
    ) -> Result<Self> {
        crate::stack::ensure_sufficient(|| {
            Self::compile_int_inner(ctx, cx, type_predicate, spec, mode)
        })
    }

    /// A slice pattern's parts against an `Array`/`List` type: the
    /// `all@` capture, the `rest..` bind (array-typed) and the elements,
    /// each against its own member of an inferred element Set.
    fn compile_slice<R: Rt, E: UserEvent>(
        ctx: &mut CompileCtx<R, E>,
        cx: PatCx,
        typ: &Type,
        list: bool,
        open: bool,
        all: &Option<Name>,
        rest: &Option<Name>,
        elems: &[StructurePattern],
        mut mode: BindMode,
    ) -> Result<(Option<BindId>, Option<BindId>, Box<[Self]>)> {
        let want = if list {
            Type::List(Arc::new(Type::empty_tvar()))
        } else {
            Type::Array(Arc::new(Type::empty_tvar()))
        };
        typ.check_contains(&ctx.env, &want)?;
        let et = typ.with_deref(|t| match t {
            Some(Type::Array(et)) if !list => Some((**et).clone()),
            Some(Type::List(et)) if list => Some((**et).clone()),
            _ => None,
        });
        let Some(et) = et else {
            return format_with_flags(PrintFlag::DerefTVars, || {
                bail!("slice patterns can't match {typ}")
            });
        };
        // XCR claude for claude: [bug] Every element compiles against one element type.
        // Under an inferred predicate, that type is infer_slice's union of all the
        // element patterns (graphix-types/src/expr/pattern.rs:201). A `_` element makes
        // it Any, so every bind beside it is Any: `[x, _] => x + 1` over Array<i64> is
        // refused with "cannot compute Any + i64", and `[_, (x, y)]` with "tuple
        // patterns can't match Any". Elements of different shapes make it a union, and
        // the Tuple, Variant and Struct arms below accept only their exact constructor.
        // So `[`A, `B]` over Array<[`A, `B]> is refused, even written `Array<[`A, `B]>
        // as [`A, `B]`, and so are `[(1, x), (y, 2)]` and `[{b, ..}, x]`. The same
        // exact-constructor rule refuses `Box(`A(x))` for `type Box =
        // Abstract<[`A(i64), `B(i64)]>`. probe:
        // design/review-2026-10-05/repro/c-pattern-08.sh (c-pattern-08)
        // 2026-10-08 claude: under an inferred predicate each element compiles against
        // its own member, so `[x, _]`, `[`A, `B]`, `[(1, x), (y, 2)]` and `[{b, ..}, x]`
        // pass (lang::select::slice_elements_typed_apart). Open: a constructor pattern at
        // a union position under a WRITTEN type, `Array<[`A, `B]> as [`A, `B]` and
        // `Box(`A(x))` over a union representation, is still refused. Narrowing it to its
        // member needs the node to carry its own refutable test: Variant/Tuple/Struct
        // claim irrefutability because a type test decided their constructor, so `let
        // `A(x) = v` over [`A(i64), `B] would be accepted and bind nothing.
        // 2026-10-08 claude: ruled (Eric): the written-type cases too. Refutability is
        // judged against a type now (StructPatternNode::covers: a constructor covers only
        // a position whose type is that constructor), so a constructor at a union
        // position under a written type narrows to its member at compile with its own
        // test live; a slice ladder under a written type needs its elements to cover the
        // written element type; Reach credits a written-union atom with each member it
        // covers; the literal pool reads an abstract's payload. `Array<[`A, `B]> as [`A,
        // `B]` and `Box(`A(x)) / Box(`B(y))` are accepted, the latter exhaustive; `let
        // `A(x) = v` over a union is refused as refutable. Pins:
        // lang::select::written_type_constructor_at_union,
        // written_type_constructor_refusals.
        let members = match cx.inferred {
            true => set_members(&et, elems.len() + open as usize),
            false => None,
        };
        let all = bind_all(ctx, cx, all, typ, &mut mode)?;
        let rest = rest.as_ref().map(|n| leaf_bind(ctx, cx, n, typ, &mut mode));
        let rest = rest.transpose()?;
        let elems = elems
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let et = members.as_ref().map_or(&et, |ts| &ts[i]);
                Self::compile_int(ctx, cx, et, p, mode.reborrow())
            })
            .collect::<Result<Box<[Self]>>>()?;
        Ok((all, rest, elems))
    }

    fn compile_int_inner<R: Rt, E: UserEvent>(
        ctx: &mut CompileCtx<R, E>,
        cx: PatCx,
        type_predicate: &Type,
        spec: &StructurePattern,
        mut mode: BindMode,
    ) -> Result<Self> {
        // an alias of an alias expands to its body: typedefs are
        // contractive, so the chain ends. The chase fills no cell: a name
        // the body names may yet be declared by a later statement
        let mut type_predicate = type_predicate.clone();
        while let Type::Ref(_) = type_predicate {
            type_predicate = type_predicate.lookup_ref_peek(&ctx.env)?;
        }
        let type_predicate = &type_predicate;
        // under a written type, a constructor pattern at a union narrows to
        // the members it can match, as an arm narrows over its scrutinee;
        // its own test then decides them (`covers`)
        let constructor = matches!(
            spec,
            StructurePattern::Tuple { .. }
                | StructurePattern::Variant { .. }
                | StructurePattern::Struct { .. }
        );
        if constructor
            && !cx.inferred
            && type_predicate.with_deref(|t| matches!(t, Some(Type::Set(_))))
        {
            let env = &ctx.env;
            let shape = spec.infer_type_predicate(env, &cx.scope.lexical)?;
            let shape = match spec.complete_type_predicate(env, &shape, type_predicate)? {
                Some(t) => t,
                None => shape,
            };
            let shape = shape.any_as_tvar();
            type_predicate.check_contains(env, &shape)?;
            let node = Self::compile_int_inner(ctx, cx, &shape, spec, mode)?;
            let env = &ctx.env;
            if let Ok(rest) = type_predicate.diff(env, &shape)
                && node.footprint(env, &shape, false).rep_collision(env, &rest).is_some()
            {
                let (a, b) = (shape.resolve_tvars(), rest.resolve_tvars());
                return format_with_flags(PrintFlag::DerefTVars, || {
                    bail!(
                        "this pattern can't tell {a} from {b}: its structure test \
                         passes a value of either; test the type first"
                    )
                });
            }
            return Ok(node);
        }
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
                // Each alternative compiles against its own member of an
                // inferred predicate; under an explicit `T as p1 | p2`
                // every alternative checks against T.
                // XCR claude for claude: [bug] A slice's element type is the union of
                // every element's inferred type (infer_slice). Through compile_slice
                // (:421), this pairs an or-pattern's alternatives with that union's
                // members whenever the counts happen to agree, and captures (:215) and
                // realign (:288) pair the same way. `[x, 1 | 2]` over Array<[i64,
                // null]> checks `2` against x's cell, so x is typed i64 and the fused
                // run panics at fusion/kernel.rs:243 on the null. Because the union is
                // sorted, `[`B | `A]` over Array<[`A, `B]> is refused while `[`A | `B]`
                // is accepted. Each alternative needs its own inferred type, or slice
                // elements must compile with inferred false. probe:
                // design/review-2026-10-05/repro/c-pattern-05.gx (c-pattern-05)
                // 2026-10-08 claude: a slice's element type is a Set with one member per
                // element (infer_slice) and each element compiles against its own, so an
                // or-pattern in an element pairs with its own member. Pin:
                // lang::select::or_in_slice_element.
                let alt_types = match cx.inferred {
                    true => set_members(type_predicate, alts.len()),
                    false => None,
                };
                let mut local = AHashMap::default();
                let (map, reuse_first) = match &mut mode {
                    BindMode::Fresh => (&mut local, false),
                    BindMode::Record(m) => (&mut **m, false),
                    BindMode::Reuse(m) => (&mut **m, true),
                };
                let compiled = alts
                    .iter()
                    .enumerate()
                    .map(|(i, alt)| {
                        let typ = alt_types.as_ref().map_or(type_predicate, |ts| &ts[i]);
                        let mode = if i == 0 && !reuse_first {
                            BindMode::Record(&mut *map)
                        } else {
                            BindMode::Reuse(&mut *map)
                        };
                        Self::compile_int(ctx, cx, typ, alt, mode)
                    })
                    .collect::<Result<Box<[Self]>>>()?;
                // a bare name or `_` matches every value of every member; any
                // other alternative only its own member's, judged by the
                // select arm by arm
                let wild = |p: &Self| match cx.inferred {
                    true => matches!(p, Self::Bind(_) | Self::Ignore),
                    false => p.covers(&ctx.env, type_predicate, false),
                };
                for i in 1..compiled.len() {
                    // XCR claude for claude: [bug] An alternative that matches_anything()
                    // only covers its own member of the inferred Set, but this refuses
                    // every later alternative whatever its member. So `(x, _) | (x, _,
                    // _)` over `[(i64, i64), (i64, i64, i64)]` is refused, while the
                    // same select written as two arms is accepted and prints [5, 7].
                    // matches_anything(Or) (:1052) makes the same mistake through
                    // matches_every (:1150): `(x, 0, _) | (x, _)` over that union
                    // counts as a wildcard, so this non-exhaustive select is accepted
                    // and f((7, 8, 9)) produces nothing. Treat an alternative as a
                    // wildcard only over its own member, as check_dead_arms does per
                    // atom. Do not just drop this check: Or's is_match/bind (:906,
                    // :787) pick an alternative by structure alone, so `(x, _)` would
                    // bind x to ["x", 1] for {x: 1, y: 2} in `(x, _) | {x, ..}`. probe:
                    // design/review-2026-10-05/repro/c-pattern-10.sh (c-pattern-10)
                    // 2026-10-08 claude: under an inferred predicate only a bare name or
                    // `_` kills the alternatives after it here; Reach::arm judges the rest
                    // per alternative against what reaches the arm. matches_anything(Or) is
                    // false, so an or-arm is never a wildcard. Alternatives with one runtime
                    // form are refused (StructPatternNode::leaves), so `(x, _) | {x, ..}`
                    // is refused as its two arms are. Pins: lang::select::or_wild_*.
                    if compiled[..i].iter().any(wild) {
                        bail!(
                            "unreachable or-pattern alternative: an earlier \
                             alternative already matches anything"
                        )
                    }
                }
                Self::Or { alts: compiled }
            }
            StructurePattern::Ignore => Self::Ignore,
            StructurePattern::Literal(v, _) => {
                type_predicate
                    .check_contains(&ctx.env, &Type::Primitive(Typ::get(v).into()))?;
                Self::Literal(v.clone())
            }
            StructurePattern::Bind(name) => {
                Self::Bind(leaf_bind(ctx, cx, name, type_predicate, &mut mode)?)
            }
            StructurePattern::SlicePrefix { list, all, prefix, tail } => {
                let (all, tail, prefix) = Self::compile_slice(
                    ctx,
                    cx,
                    type_predicate,
                    *list,
                    true,
                    all,
                    tail,
                    prefix,
                    mode,
                )?;
                Self::SlicePrefix { list: *list, all, prefix, tail }
            }
            StructurePattern::SliceSuffix { all, head, suffix } => {
                let (all, head, suffix) = Self::compile_slice(
                    ctx,
                    cx,
                    type_predicate,
                    false,
                    true,
                    all,
                    head,
                    suffix,
                    mode,
                )?;
                Self::SliceSuffix { all, head, suffix }
            }
            StructurePattern::Slice { list, all, binds } => {
                let (all, _, binds) = Self::compile_slice(
                    ctx,
                    cx,
                    type_predicate,
                    *list,
                    false,
                    all,
                    &None,
                    binds,
                    mode,
                )?;
                let kind = if *list { SliceKind::List } else { SliceKind::Array };
                Self::Slice { kind, all, binds }
            }
            StructurePattern::Tuple { all, binds } => {
                // an open predicate takes the tuple's shape; a tuple has it,
                // and a check would expand the names its elements hold
                if !matches!(type_predicate.deref_cloned(), Some(Type::Tuple(_))) {
                    type_predicate.check_contains(
                        &ctx.env,
                        &Type::Tuple(Arc::from_iter(
                            binds.iter().map(|_| Type::empty_tvar()),
                        )),
                    )?;
                }
                let Some(Type::Tuple(elts)) = &type_predicate.deref_cloned() else {
                    return format_with_flags(PrintFlag::DerefTVars, || {
                        bail!("tuple patterns can't match {type_predicate}")
                    });
                };
                if binds.len() != elts.len() {
                    bail!("expected a tuple of length {}", elts.len())
                }
                let all = bind_all(ctx, cx, all, type_predicate, &mut mode)?;
                let binds = elts
                    .iter()
                    .zip(binds.iter())
                    .map(|(t, b)| Self::compile_int(ctx, cx, t, b, mode.reborrow()))
                    .collect::<Result<Box<[Self]>>>()?;
                Self::Slice { kind: SliceKind::Tuple, all, binds }
            }
            StructurePattern::Variant { all, tag, binds } => {
                if !matches!(type_predicate.deref_cloned(), Some(Type::Variant(..))) {
                    type_predicate.check_contains(
                        &ctx.env,
                        &Type::Variant(
                            tag.clone(),
                            Arc::from_iter(binds.iter().map(|_| Type::empty_tvar())),
                            WrittenAt::NOWHERE,
                        ),
                    )?;
                }
                let Some(Type::Variant(ttag, elts, _)) = &type_predicate.deref_cloned()
                else {
                    return format_with_flags(PrintFlag::DerefTVars, || {
                        bail!("variant patterns can't match {type_predicate}")
                    });
                };
                if *ttag != *tag {
                    bail!("pattern cannot match type, tag mismatch {ttag} vs {tag}")
                }
                if binds.len() != elts.len() {
                    bail!("expected a variant with {} args", elts.len())
                }
                let all = bind_all(ctx, cx, all, type_predicate, &mut mode)?;
                let binds = elts
                    .iter()
                    .zip(binds.iter())
                    .map(|(t, b)| Self::compile_int(ctx, cx, t, b, mode.reborrow()))
                    .collect::<Result<Box<[Self]>>>()?;
                Self::Variant { tag: tag.clone(), all, binds }
            }
            StructurePattern::Abstract { all, name, bind } => {
                let td = ctx
                    .env
                    .lookup_typedef(&cx.scope.lexical, name)?
                    .ok_or_else(|| anyhow!("unknown type {name}"))?;
                let Type::Abstract { id, .. } = td.typ() else {
                    bail!("{name} is not an abstract type, so it has no constructor")
                };
                let id = *id;
                let Some(r) = ctx.env.abstract_rep(id, &cx.scope.lexical) else {
                    bail!(
                        "the definition of {name} is not visible here, so its values \
                         cannot be destructured"
                    )
                };
                let (atyp, rep) = r.instantiate(id);
                type_predicate.check_contains(&ctx.env, &atyp)?;
                // the predicate is this abstract type, as a variant pattern's is
                // its variant: a union around it would make the arm refutable by
                // a tag test nothing performs
                if !matches!(type_predicate.deref_cloned(), Some(Type::Abstract { id: t, .. }) if t == id)
                {
                    return format_with_flags(PrintFlag::DerefTVars, || {
                        bail!(
                            "{name}(..) patterns can't match {type_predicate}: test the tag first, `{name} as x`"
                        )
                    });
                }
                let all = bind_all(ctx, cx, all, type_predicate, &mut mode)?;
                // the payload checks against the declared representation
                let rep_cx = PatCx { inferred: false, ..cx };
                let bind = Box::new(Self::compile_int(ctx, rep_cx, &rep, bind, mode)?);
                Self::Abstract { id, all, rep, bind }
            }
            StructurePattern::Struct { exhaustive, all, binds } => {
                match &type_predicate {
                    Type::Struct(_) => (),
                    _ if *exhaustive => type_predicate.check_contains(
                        &ctx.env,
                        &Type::Struct(Arc::from_iter(binds.iter().map(
                            |(name, _, _)| {
                                (name.clone(), Type::empty_tvar(), WrittenAt::NOWHERE)
                            },
                        ))),
                    )?,
                    _ => bail!("non exhaustive struct matches require type annotations"),
                }
                let Some(Type::Struct(elts)) = &type_predicate.deref_cloned() else {
                    return format_with_flags(PrintFlag::DerefTVars, || {
                        bail!("struct patterns can't match {type_predicate}")
                    });
                };
                let fields = binds
                    .iter()
                    .map(|(field, pat, _)| {
                        elts.iter()
                            .position(|(name, _, _)| field == name)
                            .map(|i| (i, pat))
                            .ok_or_else(|| anyhow!("no such struct field {field}"))
                    })
                    .collect::<Result<SmallVec<[(usize, &StructurePattern); 8]>>>()?;
                if *exhaustive && fields.len() < elts.len() {
                    bail!("missing bindings for struct fields")
                }
                let all = bind_all(ctx, cx, all, type_predicate, &mut mode)?;
                let binds = fields
                    .into_iter()
                    .map(|(i, pat)| {
                        let (name, typ, _) = &elts[i];
                        let p = Self::compile_int(ctx, cx, typ, pat, mode.reborrow())?;
                        Ok((name.clone(), i, p))
                    })
                    .collect::<Result<Box<[(ArcStr, usize, Self)]>>>()?;
                Self::Struct { all, binds }
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
                    // XCR claude for claude: [bug] An or-arm picks its alternative by
                    // structure alone, here and in is_match; emit_or_chain in
                    // fusion/emit/select.rs does the same. But the check types each
                    // alternative's binds against its own member of the union. A value
                    // of a later member that an earlier alternative's structure accepts
                    // binds through the earlier alternative: same-tag variants, tuples,
                    // arrays, structs and payload variants are all Value::Array. So a
                    // bind typed i64 receives a string: the node-walk returns it, the
                    // fused select returns 0, and a kernel fed the bind panics at
                    // fusion/kernel.rs:243. The same alternatives written as separate
                    // arms are typed soundly (x is [i64, string] there). Either test
                    // each alternative's member before its structure in both engines,
                    // or type each alternative's binds over the whole scrutinee as
                    // separate arms are. probe:
                    // design/review-2026-10-05/repro/c-pattern-06.gx (c-pattern-06)
                    // 2026-10-08 claude: ruled (Eric): type each alternative's binds over
                    // the whole scrutinee, as separate arms. leaves does: a value an
                    // earlier alternative's structure takes is within that alternative's
                    // narrowed types, which the alternatives must agree on; alternatives
                    // a structure test cannot tell apart (the footprint check) are
                    // refused. The probe is refused: x is [i64, string] in one
                    // alternative and i64 in the other. Pins:
                    // lang::select::pattern_typing_refusals.
                    // 2026-10-08 claude: with the union rule (Eric) x is [i64, string]
                    // over the probe, so `-> i64` refuses it, and without the annotation
                    // `A("s", 1) binds x = "s" soundly (lang::select::or_binds_union).
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
                if let Some(id) = all {
                    f(*id, v.clone());
                }
                list::zip_prefix(v, binds, |n, h| {
                    n.bind(h, f);
                    true
                });
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
                if let Some(id) = all {
                    f(*id, v.clone())
                }
                let rest = list::zip_prefix(v, prefix, |n, h| {
                    n.bind(h, f);
                    true
                });
                if let (Some(rest), Some(id)) = (rest, tail) {
                    f(*id, rest.clone())
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
                list::zip_prefix(v, binds, |b, h| b.is_match(h)).is_some_and(list::is_nil)
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
                list::zip_prefix(v, prefix, |b, h| b.is_match(h)).is_some()
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

    /// Whether every value of `typ` passes this pattern's structure:
    /// the pattern is irrefutable over `typ`. A constructor pattern
    /// covers only a position whose type is that constructor, so its own
    /// tag, arity or field test decides nothing there; `inferred` pairs
    /// an or-pattern's alternatives and a slice's elements with their
    /// members, as [`Self::child_types`] does.
    pub fn covers(&self, env: &Env, typ: &Type, inferred: bool) -> bool {
        crate::stack::ensure_sufficient(|| self.covers_inner(env, typ, inferred))
    }

    fn covers_inner(&self, env: &Env, typ: &Type, inferred: bool) -> bool {
        if let Self::Bind(_) | Self::Ignore = self {
            return true;
        }
        let shape = expand(env, typ);
        let shaped = match (self, &shape) {
            (Self::Bind(_) | Self::Ignore, _) => return true,
            (
                Self::Or { .. }
                | Self::Literal(_)
                | Self::Slice { kind: SliceKind::Array | SliceKind::List, .. }
                | Self::SlicePrefix { .. }
                | Self::SliceSuffix { .. },
                _,
            ) => return false,
            (
                Self::Slice { kind: SliceKind::Tuple, binds, .. },
                Some(Type::Tuple(ts)),
            ) => ts.len() == binds.len(),
            (Self::Variant { tag, binds, .. }, Some(Type::Variant(t, ts, _))) => {
                t == tag && ts.len() == binds.len()
            }
            (Self::Struct { .. }, Some(Type::Struct(_))) => true,
            (Self::Abstract { id, .. }, Some(Type::Abstract { id: t, .. })) => id == t,
            _ => false,
        };
        let Some(ts) = shaped.then(|| self.child_types(env, typ, inferred)).flatten()
        else {
            return false;
        };
        let sub = self.children_inferred(inferred);
        self.children()
            .into_iter()
            .zip(ts)
            .all(|(c, t)| t.is_some_and(|(t, _)| c.covers(env, &t, sub)))
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
    /// Under a written type test, `explicit`, the elements must also
    /// cover its element type: nothing else tests them.
    pub fn array_len_coverage(
        &self,
        env: &Env,
        explicit: Option<&Type>,
    ) -> Option<(usize, bool)> {
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
        let typed = match explicit {
            None => true,
            Some(t) => self.child_types(env, t, false).is_some_and(|ts| {
                self.children()
                    .into_iter()
                    .zip(ts)
                    .all(|(c, t)| t.is_some_and(|(t, _)| c.covers(env, &t, false)))
            }),
        };
        if all_cover && typed { self.array_len_range() } else { None }
    }

    fn matches_anything_inner(&self) -> bool {
        match &self {
            Self::Bind(_) | Self::Ignore => true,
            // an alternative matches anything of its own member only
            Self::Or { .. }
            | Self::Literal(_)
            | Self::Variant { .. }
            | Self::Abstract { .. } => false,
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

    pub fn delete<R: Rt, E: UserEvent>(&self, ctx: &mut ExecCtx<'_, R, E>) {
        self.ids(&mut |id| {
            ctx.rt.store_remove(&id);
            ctx.env.unbind_variable(id);
        })
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
    pub structure_predicate: StructPatternNode,
    pub guard: Option<Held<R, E>>,
}

impl<R: Rt, E: UserEvent> PatternNode<R, E> {
    pub(crate) fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.explicit_type_predicate.encode(buf)?;
        self.type_predicate.encode(buf)?;
        self.structure_predicate.encode(buf)?;
        self.guard.is_some().encode(buf)?;
        match &self.guard {
            Some(g) => g.image_encode(buf),
            None => Ok(()),
        }
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let explicit_type_predicate = bool::decode(buf)?;
        let type_predicate = Type::decode(buf)?;
        let structure_predicate = StructPatternNode::decode(buf)?;
        let guard = match bool::decode(buf)? {
            true => Some(Held::image_decode(ctx, buf)?),
            false => None,
        };
        Ok(PatternNode {
            explicit_type_predicate,
            type_predicate,
            structure_predicate,
            guard,
        })
    }

    /// The arm's coverage atoms: each or-alternative paired with its
    /// member of an inferred predicate (under an explicit `T as p1 |
    /// p2`, with T); any other arm is one atom.
    pub(super) fn atoms(&self) -> SmallVec<[(&StructPatternNode, Type); 4]> {
        match &self.structure_predicate {
            StructPatternNode::Or { alts } => {
                let ts = match self.explicit_type_predicate {
                    true => None,
                    false => set_members(&self.type_predicate, alts.len()),
                };
                let typ = |i: usize| match &ts {
                    Some(ts) => ts[i].clone(),
                    None => self.type_predicate.clone(),
                };
                alts.iter().enumerate().map(|(i, a)| (a, typ(i))).collect()
            }
            sp => smallvec![(sp, self.type_predicate.clone())],
        }
    }

    /// Does the arm match every value of `scrut` by its structure alone?
    /// Its predicate is inferred, its structure matches anything, and
    /// the predicate's shape covers the scrutinee's.
    pub(super) fn matches_every(&self, env: &Env, scrut: &Type) -> Result<bool> {
        Ok(!self.explicit_type_predicate
            && self.structure_predicate.matches_anything()
            && self.type_predicate.contains_with_flags(BitFlags::empty(), env, scrut)?)
    }

    /// Type the arm's binds from `narrowed`, the arm's predicate as the
    /// select narrowed it against what reaches the arm: each capture
    /// takes the part it stands over (a `_` slot and a field a partial
    /// pattern leaves out have the scrutinee's type), and a name
    /// or-alternatives share the union of theirs
    /// ([`StructPatternNode::leaves`]).
    pub(super) fn bind_narrowed(
        &self,
        env: &Env,
        narrowed: &Type,
        checking: bool,
    ) -> Result<()> {
        if self.explicit_type_predicate {
            return Ok(());
        }
        let mut leaves = SmallVec::new();
        self.structure_predicate.leaves(env, narrowed, true, checking, &mut leaves)?;
        for l in leaves.iter().filter(|l| l.fresh) {
            if let Some(b) = env.by_id.get(&l.id) {
                b.typ.check_contains(env, &l.typ)?;
            }
        }
        Ok(())
    }

    pub(super) fn compile(
        ctx: &mut CompileCtx<R, E>,
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
        // test that compares the parameters a Graphix-minted value
        // carries; a Rust-backed value carries its id alone, and the
        // select refuses a test over two of its instantiations
        match &type_predicate {
            Type::Fn(_) => bail!("can't match on Fn type"),
            t if explicit && t.holds_fn(&ctx.env) => {
                bail!(
                    "can't match on a type holding a function: a test tells a function from a value, not one signature from another"
                )
            }
            Type::App(..) | Type::Hole => bail!("can't match on a type constructor"),
            Type::Concrete
            | Type::Function
            | Type::Singleton
            | Type::OneNumber
            | Type::Ordered
            | Type::Discernible => {
                bail!("can't match on a constraint")
            }
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
            | Type::ByRef(..)
            | Type::Tuple(_)
            | Type::Variant(_, _, _)
            | Type::Struct(_)
            | Type::Ref(_) => (),
        }
        let cx = PatCx { scope, pos, ori: &ori, inferred: !explicit };
        let structure_predicate = StructPatternNode::compile_with(
            ctx,
            cx,
            &type_predicate,
            &spec.structure_predicate,
        )?;
        // Under an inferred predicate a capture's type is unknown until
        // the select narrows the arm against its scrutinee
        // (`bind_narrowed`); the guard and the arm body compile over the
        // cell.
        if !explicit {
            structure_predicate
                .fresh_ids(&mut |id| ctx.env.retype(id, Type::empty_tvar()));
        }
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
        })
    }

    /// Deliver the scrutinee's destructured leaves to this arm's binds,
    /// carrying the scrutinee's production tag: a stale scrutinee binds
    /// stale leaves, never fired ones.
    pub(super) fn bind_event(
        &self,
        ctx: &mut ExecCtx<'_, R, E>,
        v: &Value,
        tag: crate::Tag,
    ) {
        self.structure_predicate.bind(v, &mut |id, v| {
            ctx.event.variables.insert(id, TagValue::tagged(v.clone(), tag));
            ctx.rt.store_insert(id, TagValue::tagged(v, tag));
        })
    }

    /// [`Self::bind_event`] for a guard's tick, returning the store
    /// entries it replaced for [`Self::retract`].
    pub(super) fn bind_tentative(
        &self,
        ctx: &mut ExecCtx<'_, R, E>,
        v: &Value,
        tag: crate::Tag,
    ) -> SmallVec<[(BindId, Option<(TagValue, u64)>); 4]> {
        let mut saved = SmallVec::new();
        self.structure_predicate
            .ids(&mut |id| saved.push((id, ctx.rt.store_get(&id).cloned())));
        self.bind_event(ctx, v, tag);
        saved
    }

    /// Undo a [`Self::bind_tentative`]: the overlay and the store as
    /// they were, so an arm not taken leaves no value behind.
    pub(super) fn retract(
        &self,
        ctx: &mut ExecCtx<'_, R, E>,
        saved: SmallVec<[(BindId, Option<(TagValue, u64)>); 4]>,
    ) {
        let cycle = ctx.rt.cycle();
        for (id, prev) in saved {
            ctx.event.variables.remove(&id);
            match prev {
                None => ctx.rt.store_remove(&id),
                Some((tv, at)) if at == cycle => ctx.rt.store_insert(id, tv),
                Some((tv, _)) => ctx.rt.store_insert_standing(id, tv),
            }
        }
    }

    /// Tick the guard (it must see every cycle) and return its
    /// production tag (`None` = no guard). The caller reads channel
    /// bottomness off `guard.tag`.
    pub(super) fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> Option<Tag> {
        match &mut self.guard {
            None => None,
            Some(g) => Some(g.update(ctx)),
        }
    }

    /// The O(1) shallow discriminator for this arm's inferred predicate
    /// against the select's scrutinee type ([`Type::shallow_discriminant`]);
    /// `None` = run the full `is_a` walk. Read once every tvar in the
    /// predicate is settled, at the select's first consult.
    pub(super) fn shallow_discriminant(
        &self,
        env: &Env,
        scrutinee: &Type,
    ) -> Option<Type> {
        if self.explicit_type_predicate {
            return None;
        }
        let shallow = self.type_predicate.shallow_discriminant(env, scrutinee);
        if crate::dbgenv::gxdbg_shallow() {
            match &shallow {
                Some(t) => eprintln!("SHALLOW {} => {t}", self.type_predicate),
                None => eprintln!("SHALLOW {} => deep", self.type_predicate),
            }
        }
        shallow
    }

    /// Whether the arm's type and structure predicates admit `v`, given
    /// the arm's shallow discriminator. The checker narrows the arm's
    /// binds by exactly this, so a value that fails it is never
    /// delivered to them.
    // XCR claude for eric: [bug] This test cannot tell apart union members that share a
    // runtime representation: a nullary variant `A is the string "A", and tuples,
    // structs, payload variants and List cells are all arrays. The checker keeps those
    // members apart and requires an arm for each. At run time the first arm whose shape
    // fits wins: over [string, `A] an `A value takes a `string as s` arm and the string
    // "A" takes an `A arm; a (i64, i64) takes an Array<i64> arm; an empty List takes an
    // Array arm. Union trait dispatch lowers to this select, so Show::show over
    // [string, `A] runs the string impl for `A, and == over that union says "A" == `A.
    // Both engines agree, so the fuzzer cannot see it; either refuse a type-tested or
    // compared union whose members overlap in runtime footprint, or give those members
    // distinct representations. probe:
    // design/review-2026-10-05/repro/x-typecheck-patterns-04.gx
    // (x-typecheck-patterns-04)
    // 2026-10-06 claude: selects and union trait dispatch are done: an arm that would
    // tell apart two types with one runtime form is refused (Type::rep_collision in
    // Select::typecheck0_with; pins lang::select::same_form_*, must-reject family 10).
    // `==` and map keys over such a union are not: "A" == `A is still true. The probe's
    // f, g, h, l and d are refused.
    // 2026-10-06 claude: `==`, the other comparisons and map keys are refused too
    // (PendingSettle::SameForm: Type::rep_ambiguity over a compared type,
    // Type::map_key_ambiguity over a map literal's type and every call's return type).
    // Pinned by lang::select::same_form_compare_refused and same_form_map_key_refused.
    // 2026-10-06 claude: the rule is now the `Discernible` bound
    // (PendingSettle::SameForm became PendingSettle::Discernible): comparisons, map
    // literals and the stdlib functions that compare or hash carry it, a generic
    // definition's variable takes it from its body and each call checks it, and a
    // call's member left open is judged at the settle. Pins:
    // lang::types::discernible_refuses_one_runtime_form, discernible_accepts.
    pub(super) fn shape_matches(
        &self,
        env: &Env,
        shallow: Option<&Type>,
        v: &Value,
    ) -> bool {
        // the type predicate is checked whether written or inferred: a
        // tuple and an array are the same `Value::Array` at runtime. An
        // inferred predicate is checked permissively (an abstract's
        // hidden rep cannot be verified); an explicit one strictly.
        let typed = if self.explicit_type_predicate {
            self.type_predicate.is_a(env, v)
        } else {
            let t = shallow.unwrap_or(&self.type_predicate);
            t.is_a_with(env, IsAFlags::MatchAbstract.into(), v)
        };
        typed && self.structure_predicate.is_match(v)
    }

    pub(super) fn arm_match(
        &self,
        env: &Env,
        shallow: Option<&Type>,
        v: &Value,
    ) -> ArmMatch {
        if !self.shape_matches(env, shallow, v) {
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

    pub(super) fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Some(n) = &mut self.guard {
            n.node.delete(ctx)
        }
        self.structure_predicate.delete(ctx)
    }
}

fn boxed_len(items: &[StructPatternNode]) -> usize {
    varint_len(items.len() as u64) + items.iter().map(|p| p.encoded_len()).sum::<usize>()
}

fn boxed_encode(
    items: &[StructPatternNode],
    buf: &mut impl bytes::BufMut,
) -> Result<(), PackError> {
    encode_varint(items.len() as u64, buf);
    for p in items {
        p.encode(buf)?;
    }
    Ok(())
}

/// A decoded count, capped by the bytes left: every element takes at
/// least one, so a corrupt count fails the decode instead of sizing an
/// allocation.
fn boxed_decode(
    buf: &mut impl bytes::Buf,
) -> Result<Box<[StructPatternNode]>, PackError> {
    let n = crate::image::count_decode(buf)?;
    (0..n).map(|_| StructPatternNode::decode(buf)).collect()
}

/// The compiled pattern is data: bind ids, literals and shapes.
impl Pack for StructPatternNode {
    fn encoded_len(&self) -> usize {
        crate::stack::ensure_sufficient(|| self.encoded_len_inner())
    }

    fn encode(&self, buf: &mut impl bytes::BufMut) -> Result<(), PackError> {
        crate::stack::ensure_sufficient(|| self.encode_inner(buf))
    }

    fn decode(buf: &mut impl bytes::Buf) -> Result<Self, PackError> {
        crate::stack::ensure_sufficient(|| Self::decode_inner(buf))
    }
}

impl StructPatternNode {
    fn encoded_len_inner(&self) -> usize {
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
                    + varint_len(binds.len() as u64)
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

    fn encode_inner(&self, buf: &mut impl bytes::BufMut) -> Result<(), PackError> {
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
                encode_varint(binds.len() as u64, buf);
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

    fn decode_inner(buf: &mut impl bytes::Buf) -> Result<Self, PackError> {
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
                let n = crate::image::count_decode(buf)?;
                let binds = (0..n)
                    .map(|_| {
                        Ok((Pack::decode(buf)?, Pack::decode(buf)?, Pack::decode(buf)?))
                    })
                    .collect::<Result<_, PackError>>()?;
                Self::Struct { all, binds }
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
