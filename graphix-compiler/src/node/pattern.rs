use crate::SourcePosition;
use crate::image::ImageBuf;
use crate::{
    BindId, CFlag, CompileCtx, ExecCtx, PrintFlag, Rt, Scope, Tag, TagValue, UserEvent,
    env::Env,
    expr::{ExprId, Name, Origin, Pattern, StructurePattern, WrittenAt, union_members},
    format_with_flags,
    node::{Held, compiler, list},
    rep,
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

/// How many fields a struct a struct pattern matches has.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StructWidth {
    /// The field count of the struct type the pattern was compiled or
    /// realigned against.
    Exact(usize),
    /// A partial pattern over a predicate inferred from the fields it
    /// names, which nothing completed.
    AtLeast(usize),
}

impl StructWidth {
    pub fn admits(self, len: usize) -> bool {
        match self {
            Self::Exact(n) => len == n,
            Self::AtLeast(n) => len >= n,
        }
    }

    fn parts(self) -> (bool, usize) {
        match self {
            Self::Exact(n) => (false, n),
            Self::AtLeast(n) => (true, n),
        }
    }
}

impl Pack for StructWidth {
    fn encoded_len(&self) -> usize {
        self.parts().encoded_len()
    }

    fn encode(&self, buf: &mut impl bytes::BufMut) -> Result<(), PackError> {
        self.parts().encode(buf)
    }

    fn decode(buf: &mut impl bytes::Buf) -> Result<Self, PackError> {
        Ok(match <(bool, usize)>::decode(buf)? {
            (false, n) => Self::Exact(n),
            (true, n) => Self::AtLeast(n),
        })
    }
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
        width: StructWidth,
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
    /// (`ids`, `fresh_ids`, `delete`) visit alternative 0 only; `is_match`
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

/// The members of the union `t`, an open cell as itself.
fn members_or_open(env: &Env, t: &Type, out: &mut SmallVec<[Type; 8]>) -> Result<()> {
    crate::stack::ensure_sufficient(|| match &t.deref_cloned() {
        None => {
            out.push(t.clone());
            Ok(())
        }
        Some(Type::Set(ts)) => ts.iter().try_for_each(|t| members_or_open(env, t, out)),
        Some(_) => union_members(env, t, out),
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

/// An abstract pattern with its representation at each instance that
/// reaches it, and whether `Any` does.
struct PayloadReps<'a> {
    pat: &'a StructPatternNode,
    reps: SmallVec<[Type; 2]>,
    over_any: bool,
}

/// `leaves` gathered over several members or alternatives, each name
/// once, at the union of its types.
fn merge_leaves(
    env: &Env,
    all: SmallVec<[Leaf; 4]>,
    out: &mut SmallVec<[Leaf; 4]>,
) -> Result<()> {
    let mut by_id: SmallVec<[(BindId, bool, SmallVec<[Type; 2]>); 4]> = SmallVec::new();
    for l in all {
        match by_id.iter_mut().find(|(id, _, _)| *id == l.id) {
            Some((_, _, ts)) => ts.push(l.typ),
            None => by_id.push((l.id, l.fresh, smallvec![l.typ])),
        }
    }
    for (id, fresh, ts) in by_id {
        let typ = match &ts[..] {
            [t] => t.clone(),
            ts => Type::union(env, &ts.iter().collect::<SmallVec<[&Type; 2]>>())?,
        };
        out.push(Leaf { id, typ, fresh })
    }
    Ok(())
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
        let parts = |n: usize, variant: Option<&ArcStr>| {
            typ.with_deref(|t| match (t, variant) {
                (Some(Type::Tuple(ts)), None) if ts.len() == n => Some(at(ts)),
                (Some(Type::Variant(tag, ts, _)), Some(want))
                    if tag == want && ts.len() == n =>
                {
                    Some(at(ts))
                }
                _ => None,
            })
        };
        let elems = |n: usize, rest: bool, list: bool| {
            let et = typ.with_deref(|t| match t {
                Some(Type::Array(et)) if !list => Some((**et).clone()),
                Some(Type::List(et)) if list => Some((**et).clone()),
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
            Self::Variant { tag, binds, .. } => parts(binds.len(), Some(tag)),
            Self::Slice { kind: SliceKind::Tuple, binds, .. } => parts(binds.len(), None),
            Self::Slice { kind, binds, .. } => {
                elems(binds.len(), false, *kind == SliceKind::List)
                    .map(|ts| at(&ts[..binds.len()]))
            }
            Self::SlicePrefix { prefix: ps, list, .. } => {
                elems(ps.len(), true, *list).map(|ts| at(&ts[..ps.len()]))
            }
            Self::SliceSuffix { suffix: ps, .. } => {
                elems(ps.len(), true, false).map(|ts| at(&ts[..ps.len()]))
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
            // XCR claude for claude: [bug/high] An abstract pattern nested in a tuple,
            // struct field or slice, over a position typed by a union of instantiations
            // ([Box<bool>, Box<i64>]), binds its payload at one member's type. This arm
            // reads the payload only for a lone Type::Abstract and otherwise falls back
            // to the pattern's own rep, which narrow_payloads then narrows against
            // ts.first(). `(Box(b), n) => b + 1` over ([Box<bool>, Box<i64>], i64)
            // checks with b: i64. The node-walk gives 1 for Box(false), and the JIT
            // panics "kernel param `b`: runtime Bool(false) does not match the compiled
            // Scalar(I64) slot". Without a `_` arm the select is refused as
            // non-exhaustive, though the top-level `Box(b)` form is exhaustive. Fix:
            // type the payload over every member of the union that instantiates this
            // id, as the top-level select does, and pin the tuple, struct and slice
            // forms. This is a variant of x-typecheck-patterns-02. probe:
            // design/review-2026-10-26/repro/x-rerun-2-nested-abstract-bind-one-member.gx
            // (x-rerun-2-01)
            // 2026-10-26 claude: Fixed: a payload's own representation narrows to the
            // union of its representation at every instance the scrutinee's members hold
            // at its position, walking the raw members with each union at a constructor
            // distributed (StructPatternNode::narrow_payloads, payload_reps,
            // member_parts), and an abstract pattern covers a union of its type's
            // instances whose every representation its payload covers (covers_inner;
            // select coverage walks the members for any pattern holding an abstract).
            // Pins: lang::select::nested_abstract_payload_over_a_union_of_instances
            // (tuple, struct field and slice forms, exhaustive without a wildcard),
            // nested_abstract_payload_is_no_one_instance.
            Self::Abstract { rep, .. } => {
                let rep = match typ.deref_cloned() {
                    None | Some(Type::Set(_)) => rep.clone(),
                    Some(t) => self.member_rep(env, &t)?,
                };
                Some(smallvec![Some((rep, 0))])
            }
        }
    }

    /// An abstract pattern's representation at `m`, one member of the
    /// type at its position: at `m`'s parameters when `m` is an instance of
    /// its type, at `Any` for each parameter when `m` is `Any`; `None` for
    /// a member it cannot match.
    fn member_rep(&self, env: &Env, m: &Type) -> Option<Type> {
        let Self::Abstract { id, rep, .. } = self else { return None };
        let at = |params: &[Type]| {
            env.abstract_reps
                .get(id)
                .map_or_else(|| rep.clone(), |r| r.instantiate_with(params))
        };
        match &expand(env, m)? {
            Type::Abstract { id: t, params } if t == id => Some(at(params)),
            Type::Any => {
                let n = env.abstract_reps.get(id).map_or(0, |r| r.params.len());
                let anys: SmallVec<[Type; 4]> = (0..n).map(|_| Type::Any).collect();
                Some(at(&anys))
            }
            _ => None,
        }
    }

    /// [`Self::child_types`] of each member of `typ` this pattern's head
    /// can match: a union at a constructor pattern's position stands for
    /// each of its members in turn, and a struct pattern matches only a
    /// member with every field it names, at a width it admits.
    fn member_parts(
        &self,
        env: &Env,
        typ: &Type,
        inferred: bool,
    ) -> SmallVec<[(Type, SmallVec<[Option<(Type, usize)>; 8]>); 2]> {
        let union = expand(env, typ).is_some_and(|t| matches!(t, Type::Set(_)));
        let distributes = !matches!(
            self,
            Self::Ignore | Self::Literal(_) | Self::Bind(_) | Self::Or { .. }
        );
        if !(union && distributes) {
            let ts = self.child_types(env, typ, inferred);
            return ts.map(|ts| (typ.clone(), ts)).into_iter().collect();
        }
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        if union_members(env, typ, &mut members).is_err() {
            return SmallVec::new();
        }
        members
            .iter()
            .filter_map(|m| {
                let ts = self.child_types(env, m, inferred)?;
                let fits = match self {
                    Self::Struct { width, .. } => {
                        ts.iter().all(Option::is_some)
                            && struct_fields(env, m)
                                .is_some_and(|fs| width.admits(fs.len()))
                    }
                    _ => true,
                };
                fits.then(|| (m.clone(), ts))
            })
            .collect()
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

    /// Narrow each abstract payload's own representation, which its binds
    /// and captures are typed by, to the union of its representation at
    /// every instance `members`, the scrutinee's members the arm can match,
    /// hold at its position ([`Self::member_rep`]): an inferred abstract
    /// predicate takes its parameters from the scrutinee, never from its
    /// payload.
    // XCR claude for claude: [bug/high] Over an Any scrutinee child_types gives nothing,
    // so an inferred `Box(p)` keeps its own fresh parameters, and the arm body's uses
    // decide them. shape_matches then passes any Box (IsAFlags::MatchAbstract), so `let
    // f = |a: Any| select a { Box(p) => str::len(p), _ => 0 }; f(Box(5))` delivers 5 as
    // a string, and the runtime panics in the kernel param check. Refuse an inferred
    // abstract pattern where no representation reaches it, with the `Box<T> as Box(p)`
    // hint the partial-struct refusal gives, or bind its parameters to Any. Pin the
    // top-level and nested forms. probe:
    // design/review-2026-10-26/repro/x-typecheck-patterns-abstract-payload-over-any.gx
    // (x-typecheck-patterns-03)
    // 2026-10-26 claude: Fixed by the second option: over an Any member an abstract
    // payload's representation is the type at Any for each parameter (member_rep), and
    // what narrowing leaves open binds to Any (narrow_payloads, over_any), so p is Any
    // and str::len(p) is refused; the refusal option was not taken. Pins:
    // lang::select::abstract_payload_over_any_is_any,
    // abstract_payload_over_any_decides_nothing,
    // nested_abstract_payload_over_any_decides_nothing.
    fn narrow_payloads(&self, env: &Env, members: &[Type]) -> Result<()> {
        let mut reps: SmallVec<[PayloadReps; 2]> = SmallVec::new();
        for m in members {
            self.payload_reps(env, m, &mut reps);
        }
        for PayloadReps { pat, reps, over_any } in reps.iter() {
            if let Self::Abstract { rep, .. } = pat {
                let rs: SmallVec<[&Type; 2]> = reps.iter().collect();
                let at = Type::union(env, &rs)?;
                at.narrow_pattern(env, rep)?;
                // narrowing against `Any` decides nothing: each part the
                // payload leaves open is `Any`
                if *over_any {
                    rep.contains(env, &at)?;
                }
            }
        }
        Ok(())
    }

    /// Each abstract pattern in this one with its representation at every
    /// instance `typ` holds at its position.
    fn payload_reps<'a>(
        &'a self,
        env: &Env,
        typ: &Type,
        out: &mut SmallVec<[PayloadReps<'a>; 2]>,
    ) {
        crate::stack::ensure_sufficient(|| {
            if let Self::Abstract { .. } = self {
                let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                if union_members(env, typ, &mut members).is_err() {
                    return;
                }
                for m in members.iter() {
                    let Some(r) = self.member_rep(env, m) else { continue };
                    let any = matches!(m, Type::Any);
                    match out.iter_mut().find(|e| std::ptr::eq(e.pat, self)) {
                        Some(e) => {
                            e.reps.push(r);
                            e.over_any |= any
                        }
                        None => out.push(PayloadReps {
                            pat: self,
                            reps: smallvec![r],
                            over_any: any,
                        }),
                    }
                }
                return;
            }
            for (_, ts) in self.member_parts(env, typ, false) {
                for (c, t) in self.children().into_iter().zip(ts) {
                    if let Some((t, _)) = t {
                        c.payload_reps(env, &t, out)
                    }
                }
            }
        })
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
        let sub = self.children_inferred(inferred);
        // XCR claude for claude: [bug/high] Returning here drops an abstract payload's
        // binds from the leaves. But fresh_ids, through `ids`, gives every name an
        // or-pattern binds a fresh cell, payload binds included, which erases the
        // rep-typed cell the compile gave them. A name bound only under `T(..)` in
        // every alternative is never typed: `` `A(T(n)) | `B(T(n)) => str::len(n) ``
        // passes and the JIT panics on an i64 in a String slot. A name another
        // alternative binds outside a payload loses the payload's part of its union:
        // `(x, 1) | (T(x), 2)` types x as [string, T], and a select on x hits a
        // coverage hole on 4. Fix: descend into the payload here using child_types'
        // representation (narrowed by narrow_payloads), with inferred = false, so the
        // or-union sees it. probe:
        // design/review-2026-10-26/repro/c-pattern-or-abstract-payload-untyped.gx
        // (c-pattern-04)
        // 2026-10-26 claude: Fixed: leaves descends into an abstract payload over the
        // pattern's own representation (narrowed by narrow_payloads) with inferred =
        // false, so an or-union sees a payload bind, and the or-merge unions every
        // alternative's leaves (merge_leaves). Pins:
        // lang::select::or_names_under_an_abstract_payload (x is [string, T, i64]),
        // or_name_under_an_abstract_payload_is_typed.
        if let Self::Abstract { rep, bind, .. } = self {
            for id in self.whole_binds() {
                out.push(Leaf { id, typ: typ.normalize(), fresh: true })
            }
            return bind.leaves(env, rep, false, checking, out);
        }
        let Self::Or { alts } = self else {
            let members = self.member_parts(env, typ, inferred);
            let mut all: SmallVec<[Leaf; 4]> = SmallVec::new();
            for (m, ts) in members.iter() {
                for id in self.whole_binds() {
                    all.push(Leaf { id, typ: m.normalize(), fresh: true })
                }
                for (c, t) in self.children().into_iter().zip(ts.iter()) {
                    if let Some((t, _)) = t {
                        c.leaves(env, t, sub, checking, &mut all)?
                    }
                }
            }
            // over a raw member, a head of another shape matches none of it
            if members.is_empty() && (inferred || expand(env, typ).is_none()) {
                for id in self.whole_binds() {
                    all.push(Leaf { id, typ: typ.normalize(), fresh: true })
                }
            }
            return merge_leaves(env, all, out);
        };
        let Some(ts) = self.child_types(env, typ, inferred) else { return Ok(()) };
        let ts: SmallVec<[Type; 4]> = ts.into_iter().flatten().map(|(t, _)| t).collect();
        if checking {
            for (i, a) in alts.iter().enumerate() {
                let fp = a.footprint(env, &ts[i], sub);
                for (j, b) in ts.iter().enumerate() {
                    // XCR claude for claude: [bug/high] This judges alternatives with
                    // Open::Benign, so open cells collide with nothing, and `(x, _) |
                    // (_, x)` over an unannotated parameter or Any is accepted. At run
                    // time the arm passes on one alternative's types (shape_matches
                    // tests the Set of alternative types) and binds by the first
                    // alternative whose structure matches (StructPatternNode::bind),
                    // which may be the other one. `let f = |a| select a { (x, _) | (_,
                    // x) => x + 1 }; f(("s", 5))` binds "s" as an i64 and the runtime
                    // panics in the kernel param check. Take an alternative only when
                    // its own types and structure both pass, and bind from that one, or
                    // judge here with open cells read as any type, as the arms' Told
                    // deferral does. Pin both forms. probe:
                    // design/review-2026-10-26/repro/x-typecheck-patterns-or-alt-type-unpaired.gx
                    // (x-typecheck-patterns-02)
                    // 2026-10-26 claude: Fixed by typing, not by the run-time binding: a name
                    // or-alternatives share is the union of its part, through every alternative
                    // whose structure admits a member, over each member that reaches the arm
                    // (wholes_by_member's raw walk, which now runs over an open scrutinee's
                    // predicate too), and an Any member stands for each member of the arm's
                    // predicate, whose `_` slots are Any. So in both forms x is Any and
                    // str::len(x) or x + 1 is refused; or-names over a real union, (x, 1) | (1,
                    // x) and `A(x) | `B(x) still check. Bind still takes the first alternative
                    // whose structure matches, which the typing now covers. Pins:
                    // lang::select::or_name_over_an_open_scrutinee_is_every_position,
                    // or_name_over_any_is_every_position, or_names_over_unions_keep_working.
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
        let mut all: SmallVec<[Leaf; 4]> = SmallVec::new();
        for (a, t) in alts.iter().zip(ts.iter()) {
            a.leaves(env, t, sub, checking, &mut all)?;
        }
        for l in all.iter_mut() {
            l.fresh = true
        }
        merge_leaves(env, all, out)
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
                Self::Literal(v) => Type::of_literal(v).unwrap_or_else(|_| typ.clone()),
                Self::Abstract { .. } => typ.clone(),
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

    /// Under an inferred predicate each element of an array or list slice
    /// stands over its own member, but the run-time test admits a value
    /// of any member at every position: every name an element binds holds
    /// its part of each member the element's structure admits.
    fn hold_slice_members(&self, env: &Env, typ: &Type) -> Result<()> {
        crate::stack::ensure_sufficient(|| {
            let Some(ts) = self.child_types(env, typ, true) else { return Ok(()) };
            if let Self::Slice { kind: SliceKind::Array | SliceKind::List, .. }
            | Self::SlicePrefix { .. }
            | Self::SliceSuffix { .. } = self
            {
                let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                if let Some(Type::Array(et) | Type::List(et)) = &expand(env, typ) {
                    members_or_open(env, &et, &mut members)?
                }
                let mut leaves: SmallVec<[Leaf; 4]> = SmallVec::new();
                for c in self.children() {
                    for m in members.iter() {
                        if !c.footprint(env, m, false).could_match(env, m)? {
                            continue;
                        }
                        leaves.clear();
                        c.leaves(env, m, false, false, &mut leaves)?;
                        for l in leaves.iter() {
                            if let Some(b) = env.by_id.get(&l.id) {
                                b.typ.check_contains(env, &l.typ)?
                            }
                        }
                    }
                }
            }
            if !self.children_inferred(true) {
                return Ok(());
            }
            for (c, t) in self.children().into_iter().zip(ts) {
                if let Some((t, _)) = t {
                    c.hold_slice_members(env, &t)?
                }
            }
            Ok(())
        })
    }

    /// The id of every [`Leaf::fresh`] name: each capture and every
    /// name an or-pattern binds.
    fn fresh_ids(&self, f: &mut impl FnMut(BindId)) {
        crate::stack::ensure_sufficient(|| match self {
            Self::Or { .. } => self.ids(&mut *f),
            // a payload compiled against its written rep: its binds are typed
            Self::Abstract { .. } => self.whole_binds().for_each(&mut *f),
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
                Self::Struct { binds, width, all: _ } => {
                    if let Some(fields) = struct_fields(env, typ) {
                        *width = StructWidth::Exact(fields.len())
                    }
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
        // contractive, so the chain ends. The chase fills no cell, and stops
        // at a name a later statement declares, which means that type: a
        // name binds at it, and a pattern that tests or takes it apart is
        // refused
        let mut type_predicate = type_predicate.clone();
        // XCR claude for claude: [bug/critical] This loop expands a let annotation at
        // compile time with lookup_ref_peek, and the leaf binding takes the expanded
        // type. A `type Foo` declared later in the same block is not registered yet, so
        // with an outer `Foo` in scope the binding gets the outer type.
        // Bind::check_value checks the initializer against the unexpanded Ref after the
        // block's Foo is registered, so the check uses the inner type. Result: `type
        // Foo = string; let r = { let a: Foo = 42; type Foo = i64; a }; str::len(r)`
        // passes the check, and the JIT reads 42 as a string pointer and aborts
        // (misaligned pointer 0x2a). Fix: give a statement list's type names one
        // meaning throughout. Either pre-register a block's typedef names before
        // compiling its statements, as compile_block_children does for `mod` names, or
        // bind the leaf with the unexpanded Ref and use the expansion only for the
        // structural decisions here. probe:
        // design/review-2026-10-26/repro/t-env-forward-typedef-confusion.gx (t-env-01)
        // 2026-10-26 claude: Fixed: a block declaring types records their names while
        // its statements compile (CompileCtx::block_typedefs, node::later_typedef). A
        // pattern's expansion stops at a name a later statement of an enclosing block
        // declares and no nearer scope already does: a bare name binds at the written
        // ref, which means the block's final type (the repro's r is i64, str::len(r)
        // refused, r + 1 is 43), and a pattern that tests or takes it apart is refused
        // ("P names the type a later statement of this block declares"); before, a
        // forward name with no outer one was an undefined type. Pins:
        // lang::types::a_pattern_binds_at_the_blocks_final_type,
        // a_pattern_binds_at_no_type_a_later_statement_shadows,
        // a_destructure_names_no_type_a_later_statement_declares,
        // a_pattern_names_the_types_declared_before_it;
        // alias_body_names_the_blocks_type still holds. The destructure refusal is a
        // new rule: say if CLAUDE.md should state it.
        while let Type::Ref(tr) = &type_predicate {
            if super::later_typedef(ctx, tr) {
                if let StructurePattern::Bind(_) | StructurePattern::Ignore = spec {
                    break;
                }
                bail!(
                    "{} names the type a later statement of this block declares: \
                     declare it before this pattern",
                    tr.name
                )
            }
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
                | StructurePattern::Slice { .. }
                | StructurePattern::SlicePrefix { .. }
                | StructurePattern::SliceSuffix { .. }
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
            if !type_predicate.narrow_pattern(env, &shape)? {
                type_predicate.check_contains(env, &shape)?;
            }
            // XCR claude for claude: [bug/medium] Compiled against the inferred `shape`,
            // a `name@` capture and a slice rest are bound at the shape: a per-position
            // union such as ([i64, string], [i64, string]), or the shape's private
            // per-element Set. They should be bound at the written member(s) the
            // pattern matches. Because cx.inferred is false here, compile_slice also
            // pairs every element with that whole Set. `let a@ (_, _) = v` over [(i64,
            // string), (string, i64)] cannot pass `a` on as that type, and `A as [_,
            // r..]` names Array<[[i64, string], [i64, string]]>, while an unannotated
            // arm types the same captures by member (wholes_by_member). Fix: bind each
            // whole capture and rest at the union of its part of each written member
            // the shape can match, and pair the children with the shape's Set members.
            // probe: design/review-2026-10-26/repro/c-pattern-written-union-capture.gx
            // (c-pattern-05)
            // 2026-10-26 claude: Fixed: a pattern compiled against the shape under a
            // written union, and a let's pattern compiled against its per-member
            // narrowed predicate (node/bind.rs), retype each name, captures, rests and
            // element binds alike, to the union of its part of each member the shape
            // can match (StructPatternNode::names_by_member, retype_by_member); an
            // or-alternative unions it with the earlier alternatives' type. Pin:
            // lang::select::written_union_names_take_each_member (let, U as, A as [_,
            // r..], A as [x, ..]). The lambda-argument form |a@ (_, _): U| stays
            // refused as refutable (a tuple pattern does not cover a union of tuples),
            // before and after.
            let prior: SmallVec<[(BindId, Type); 4]> = match &mode {
                BindMode::Reuse(map) => map.values().cloned().collect(),
                BindMode::Fresh | BindMode::Record(_) => SmallVec::new(),
            };
            let node = Self::compile_int_inner(ctx, cx, &shape, spec, mode.reborrow())?;
            for Leaf { id, typ, .. } in
                node.names_by_member(&ctx.env, type_predicate, &shape)?
            {
                let typ = match prior.iter().find(|(p, _)| *p == id) {
                    Some((_, t0)) => Type::union(&ctx.env, &[t0, &typ])?,
                    None => typ,
                };
                if let BindMode::Record(map) | BindMode::Reuse(map) = &mut mode
                    && let Some((_, t)) = map.values_mut().find(|(p, _)| *p == id)
                {
                    *t = typ.clone()
                }
                ctx.env.retype(id, typ)
            }
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
                type_predicate.check_contains(&ctx.env, &Type::of_literal(v)?)?;
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
                let own = cx.inferred.then(|| r.instantiate(id).1);
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
                // the payload checks against the declared representation; under
                // an inferred predicate, at parameters of its own, the
                // predicate's being the scrutinee's (`leaves` types its binds)
                let rep = own.unwrap_or(rep);
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
                let width = if !*exhaustive && cx.inferred {
                    StructWidth::AtLeast(elts.len())
                } else {
                    StructWidth::Exact(elts.len())
                };
                let all = bind_all(ctx, cx, all, type_predicate, &mut mode)?;
                let binds = fields
                    .into_iter()
                    .map(|(i, pat)| {
                        let (name, typ, _) = &elts[i];
                        let p = Self::compile_int(ctx, cx, typ, pat, mode.reborrow())?;
                        Ok((name.clone(), i, p))
                    })
                    .collect::<Result<Box<[(ArcStr, usize, Self)]>>>()?;
                Self::Struct { all, width, binds }
            }
        };
        Ok(t)
    }

    /// Each name this pattern binds with the union of its part of each
    /// member of `typ` that `shape`, the one view of them all it was
    /// compiled against, can match.
    fn names_by_member(
        &self,
        env: &Env,
        typ: &Type,
        shape: &Type,
    ) -> Result<SmallVec<[Leaf; 4]>> {
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        union_members(env, typ, &mut members)?;
        let mut parts: SmallVec<[Leaf; 4]> = SmallVec::new();
        for m in members.iter() {
            if shape.could_match(env, m)? {
                self.leaves(env, m, false, false, &mut parts)?
            }
        }
        let mut names = SmallVec::new();
        merge_leaves(env, parts, &mut names)?;
        Ok(names)
    }

    /// Retype each name a `let`'s pattern, compiled against `shape`, the
    /// value's type `typ` narrowed per member, binds by
    /// [`Self::names_by_member`].
    pub(super) fn retype_by_member<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut CompileCtx<R, E>,
        typ: &Type,
        shape: &Type,
    ) -> Result<()> {
        for Leaf { id, typ, .. } in self.names_by_member(&ctx.env, typ, shape)? {
            ctx.env.retype(id, typ)
        }
        Ok(())
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
        match self {
            Self::Bind(id) => f(*id),
            Self::Or { alts } => alts[0].ids(f),
            _ => {
                self.whole_binds().for_each(&mut *f);
                for c in self.children() {
                    c.ids(f)
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
            Self::Slice {
                kind: kind @ (SliceKind::Tuple | SliceKind::Array),
                all,
                binds,
            } => match slice_elems(*kind, v) {
                Some(a) if a.len() == binds.len() => {
                    if let Some(id) = all {
                        f(*id, v.clone());
                    }
                    for (j, n) in binds.iter().enumerate() {
                        n.bind(&a[j], f)
                    }
                }
                _ => (),
            },
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
                if let Some((_, payload)) = rep::variant(v)
                    && payload.len() == binds.len()
                {
                    for (n, v) in binds.iter().zip(payload) {
                        n.bind(v, f)
                    }
                }
            }
            Self::SlicePrefix { list: false, all, prefix, tail } => {
                match rep::array_elems(v) {
                    Some(a) if a.len() >= prefix.len() => {
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
                }
            }
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
            Self::SliceSuffix { all, head, suffix } => match rep::array_elems(v) {
                Some(a) if a.len() >= suffix.len() => {
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
            Self::Struct { all, width, binds } => match rep::struct_fields(v) {
                Some(a) if width.admits(a.len()) => {
                    if let Some(id) = all {
                        f(*id, v.clone())
                    }
                    for (name, i, n) in binds.iter() {
                        if let Some((field, v)) = rep::field_at(a, *i)
                            && field == name
                        {
                            n.bind(v, f)
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
            Self::Slice {
                kind: kind @ (SliceKind::Tuple | SliceKind::Array),
                all: _,
                binds,
            } => match slice_elems(*kind, v) {
                Some(a) => {
                    a.len() == binds.len()
                        && binds.iter().zip(a.iter()).all(|(b, v)| b.is_match(v))
                }
                None => false,
            },
            // exactly `binds.len()` cells, each head matching, ending at nil
            Self::Slice { kind: SliceKind::List, all: _, binds } => {
                list::zip_prefix(v, binds, |b, h| b.is_match(h)).is_some_and(list::is_nil)
            }
            Self::Variant { tag, all: _, binds } => match rep::variant(v) {
                Some((t, payload)) => {
                    t == tag
                        && payload.len() == binds.len()
                        && binds.iter().zip(payload).all(|(b, v)| b.is_match(v))
                }
                None => false,
            },
            Self::SlicePrefix { list: false, all: _, prefix, tail: _ } => {
                match rep::array_elems(v) {
                    Some(a) => {
                        a.len() >= prefix.len()
                            && prefix.iter().zip(a.iter()).all(|(b, v)| b.is_match(v))
                    }
                    _ => false,
                }
            }
            Self::SlicePrefix { list: true, all: _, prefix, tail: _ } => {
                list::zip_prefix(v, prefix, |b, h| b.is_match(h)).is_some()
            }
            Self::SliceSuffix { all: _, head: _, suffix } => match rep::array_elems(v) {
                Some(a) => {
                    a.len() >= suffix.len()
                        && suffix
                            .iter()
                            .zip(a.iter().skip(a.len() - suffix.len()))
                            .all(|(b, v)| b.is_match(v))
                }
                _ => false,
            },
            // a field is the pair at its index naming it: a struct of
            // another shape that has the index fails
            Self::Struct { all: _, width, binds } => match rep::struct_fields(v) {
                Some(a) => {
                    width.admits(a.len())
                        && binds.iter().all(|(name, i, p)| match rep::field_at(a, *i) {
                            Some((field, v)) if field == name => p.is_match(v),
                            _ => false,
                        })
                }
                _ => false,
            },
        }
    }

    /// The part of `typ` this pattern covers when its only refutable parts
    /// are `null` literals: `typ` with `null` at each of them, so `{a: null}`
    /// over `{a: [i64, null]}` covers `{a: null}`. `None` for any other
    /// pattern, or a position whose type holds no `null`.
    pub(super) fn null_narrowed(&self, env: &Env, typ: &Type) -> Option<Type> {
        let parts = |binds: &[Self], ts: &[Type]| {
            (ts.len() == binds.len()).then_some(())?;
            let parts = binds
                .iter()
                .zip(ts.iter())
                .map(|(b, t)| b.null_narrowed(env, t))
                .collect::<Option<SmallVec<[Type; 8]>>>()?;
            Some(Arc::from_iter(parts))
        };
        crate::stack::ensure_sufficient(|| match self {
            Self::Bind(_) | Self::Ignore => Some(typ.clone()),
            Self::Literal(Value::Null) => {
                let null = Type::Primitive(Typ::Null.into());
                null.could_match(env, typ).unwrap_or(false).then_some(null)
            }
            Self::Slice { kind: SliceKind::Tuple, binds, all: _ } => {
                let Some(Type::Tuple(ref ts)) = expand(env, typ) else { return None };
                Some(Type::Tuple(parts(binds, ts)?))
            }
            Self::Variant { tag, binds, all: _ } => {
                let Some(Type::Variant(ref t, ref ts, at)) = expand(env, typ) else {
                    return None;
                };
                (t == tag).then_some(())?;
                Some(Type::Variant(t.clone(), parts(binds, ts)?, at))
            }
            Self::Struct { binds, width, all: _ } => {
                let Some(Type::Struct(ref fs)) = expand(env, typ) else { return None };
                width.admits(fs.len()).then_some(())?;
                let fields = fs
                    .iter()
                    .map(|(n, t, at)| {
                        let t = match binds.iter().find(|(m, _, _)| m == n) {
                            Some((_, _, p)) => p.null_narrowed(env, t)?,
                            None => t.clone(),
                        };
                        Some((n.clone(), t, *at))
                    })
                    .collect::<Option<SmallVec<[_; 8]>>>()?;
                Some(Type::Struct(Arc::from_iter(fields)))
            }
            _ => None,
        })
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
        // alternatives cover a type when each of its members is covered by
        // one of them
        if let Self::Or { alts } = self {
            let mut members: SmallVec<[Type; 8]> = SmallVec::new();
            return union_members(env, typ, &mut members).is_ok()
                && !members.is_empty()
                && members.iter().all(|m| alts.iter().any(|a| a.covers(env, m, false)));
        }
        // an abstract pattern's run-time test reads the tag alone: it covers
        // a union of its type's instances whose every representation its
        // payload covers
        if let Self::Abstract { id, bind, .. } = self {
            let mut members: SmallVec<[Type; 8]> = SmallVec::new();
            return union_members(env, typ, &mut members).is_ok()
                && !members.is_empty()
                && members.iter().all(|m| {
                    matches!(expand(env, m), Some(Type::Abstract { id: t, .. }) if t == *id)
                        && self.member_rep(env, m).is_some_and(|r| bind.covers(env, &r, false))
                });
        }
        let shape = expand(env, typ);
        let shaped = match (self, &shape) {
            (
                Self::Literal(_)
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
            (Self::Struct { width, .. }, Some(Type::Struct(fs))) => {
                width.admits(fs.len())
            }
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

    /// Whether an abstract pattern is in this one.
    pub(super) fn holds_abstract(&self) -> bool {
        crate::stack::ensure_sufficient(|| {
            matches!(self, Self::Abstract { .. })
                || self.children().into_iter().any(|c| c.holds_abstract())
        })
    }

    /// True when the pattern matches any value of the scrutinee's type:
    /// `Select`'s wildcard test. A variant pattern with an all-bind
    /// payload is irrefutable given its tag matched, but its inferred
    /// predicate still carries the tag test.
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
            Some(t) => {
                let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                let listed = union_members(env, t, &mut members).is_ok();
                let mut shaped = members
                    .iter()
                    .filter_map(|m| self.child_types(env, m, false))
                    .peekable();
                listed
                    && shaped.peek().is_some()
                    && shaped.all(|ts| {
                        self.children().into_iter().zip(ts).all(|(c, t)| {
                            t.is_some_and(|(t, _)| c.covers(env, &t, false))
                        })
                    })
            }
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
            Self::Struct { all: _, width: _, binds } => {
                binds.iter().all(|(_, _, p)| p.matches_anything())
            }
            Self::Slice { kind: SliceKind::Array | SliceKind::List, .. }
            | Self::SlicePrefix { .. }
            | Self::SliceSuffix { .. } => false,
        }
    }

    // CR claude for eric: [bug/high] Deleting a collection slot unbinds its parameters
    // here even when a closure the slot made escaped and still captures them. A call
    // site that binds that closure after the deletion fails with 'binding the callee
    // failed: k not defined', and no value is produced; a site bound earlier keeps
    // working. References have a rule to outlive their site (reference::Lives);
    // function values need one. Proposed rule: a function value keeps its captures
    // alive, enforced by pinning a closure's captured ids as Lives pins referenced
    // ones. probe: design/review-2026-10-26/repro/core-aux-closure-outlives-slot.gx
    // (core-aux-02)
    pub fn delete<R: Rt, E: UserEvent>(&self, ctx: &mut ExecCtx<'_, R, E>) {
        self.ids(&mut |id| {
            // a reference that names it keeps its value
            if !ctx.control.lives().orphan_if_pinned(id) {
                ctx.rt.store_release(&id);
            }
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

    /// A written `error` test, whose capture is typed by the errors that
    /// reach the arm ([`Self::error_capture`]), not by the bare predicate.
    fn tests_error(&self) -> bool {
        matches!(&self.type_predicate, Type::Primitive(p) if *p == Typ::Error)
    }

    /// The type this arm's binds are typed from: an `error` test's is the
    /// `Error<..>` members of `reaching`, else the predicate.
    // CR claude for eric: [bug/medium] Only an error test's bind is narrowed to what
    // reaches the arm. Every other written test binds the full written type, contrary
    // to CLAUDE.md's rule that an arm's binds narrow to what reaches it. So `|x: [i64,
    // string]| select x { Number as n => n + 1, string as s => 0 }` is refused (n is
    // [Int, Real]), and an inner select over `[i64, string, f64] as n` with x: [i64,
    // string, bool] must cover f64. Needs a ruling: whether the bind should be the
    // written type intersected with the reaching members it can match (the error
    // filter, generalized), or stay the written type with CLAUDE.md saying so. probe:
    // design/review-2026-10-26/repro/x-typecheck-patterns-typetest-bind-not-narrowed.gx
    // (x-typecheck-patterns-06)
    pub(super) fn capture_type(&self, env: &Env, reaching: &Type) -> Result<Type> {
        if !self.explicit_type_predicate {
            return Ok(self.type_predicate.any_as_tvar());
        }
        if !self.tests_error() {
            return Ok(self.type_predicate.clone());
        }
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        union_members(env, reaching, &mut members)?;
        members.retain(|m| m.with_deref(|m| matches!(m, Some(Type::Error(_)))));
        Ok(match members.len() {
            0 => self.type_predicate.clone(),
            1 => members.pop().unwrap(),
            _ => Type::Set(Arc::from_iter(members)).normalize(),
        })
    }

    /// Type the arm's binds from `narrowed`, the arm's predicate as the
    /// select narrowed it against what reaches the arm: each capture
    /// takes the part it stands over (a `_` slot and a field a partial
    /// pattern leaves out have the scrutinee's type), and a name
    /// or-alternatives share the union of theirs
    /// ([`StructPatternNode::leaves`]). A whole capture (`a@ (_, _)`)
    /// stands over one of `reaching`, the members of what reaches the arm
    /// as they were before the narrowing, at a time, so it takes the
    /// union of its part of each member the arm can match, where a plain
    /// bind takes the union of its position's parts.
    pub(super) fn bind_narrowed(
        &self,
        env: &Env,
        narrowed: &Type,
        reaching: &[Type],
        checking: bool,
    ) -> Result<()> {
        let tests_error = self.explicit_type_predicate && self.tests_error();
        if self.explicit_type_predicate && !tests_error {
            return Ok(());
        }
        // the members of what reaches the arm it can match; for its whole
        // captures, `Any` is each member of the predicate a value passing
        // the arm's test is of
        let mut matched: SmallVec<[Type; 8]> = SmallVec::new();
        let mut wholes: SmallVec<[Type; 8]> = SmallVec::new();
        if !self.explicit_type_predicate {
            for m in reaching {
                if self.type_predicate.could_match(env, m)? {
                    matched.push(m.clone());
                    match m {
                        Type::Any => {
                            union_members(env, &self.type_predicate, &mut wholes)?
                        }
                        m => wholes.push(m.clone()),
                    }
                }
            }
            self.structure_predicate.narrow_payloads(env, &matched)?;
            self.structure_predicate.hold_slice_members(env, narrowed)?;
        }
        let mut leaves = SmallVec::new();
        self.structure_predicate.leaves(env, narrowed, true, checking, &mut leaves)?;
        if reaching.len() > 1 || wholes.len() > 1 {
            self.wholes_by_member(env, &wholes, &mut leaves)?;
        }
        for l in leaves.iter().filter(|l| l.fresh || tests_error) {
            if let Some(b) = env.by_id.get(&l.id) {
                b.typ.check_contains(env, &l.typ)?;
            }
        }
        Ok(())
    }

    /// Each whole capture in `leaves` typed as the union of its part of
    /// each of `members`, the members of a union reaching the arm that its
    /// predicate can match: each member stands at the pattern's position
    /// whole, its unions distributed at each constructor the pattern tests.
    fn wholes_by_member(
        &self,
        env: &Env,
        members: &[Type],
        leaves: &mut SmallVec<[Leaf; 4]>,
    ) -> Result<()> {
        if !leaves.iter().any(|l| l.fresh) {
            return Ok(());
        }
        let mut parts: AHashMap<BindId, SmallVec<[Type; 4]>> = AHashMap::default();
        for m in members.iter() {
            let mut ls: SmallVec<[Leaf; 4]> = SmallVec::new();
            // XCR claude for claude: [bug/high] `m` is a raw scrutinee member, but
            // `inferred = true` makes child_types read its element union as the slice's
            // per-element Set whenever the member counts line up (elements plus the
            // rest), so element 0 is paired with the union's first member. child_types'
            // Variant case also ignores the tag. Over [Array<[`A(i64), `B(string)]>,
            // null], `[y@ `B(_), ..]` types y as `A(i64): a select on y passes the
            // check and hits a coverage hole at run time. Fix: walk raw members with
            // `inferred = false` (each element stands over the whole element type), and
            // make child_types give no parts for a Variant pattern over another tag.
            // probe:
            // design/review-2026-10-26/repro/c-pattern-whole-capture-set-pairing.gx
            // (c-pattern-02)
            // 2026-10-26 claude: Fixed: wholes_by_member walks the raw members with
            // inferred = false; leaves distributes a union at a constructor pattern's
            // position over the members its head matches (member_parts); child_types
            // gives no parts for a variant pattern over another tag or a slice over the
            // other collection kind. bind_narrowed now takes the reaching members as
            // they were before the narrowing bound an open scrutinee. Pins:
            // lang::select::whole_capture_in_a_slice_element_takes_its_member,
            // whole_capture_in_a_slice_element_is_not_another_tag.
            self.structure_predicate.leaves(env, m, false, false, &mut ls)?;
            for l in ls.into_iter().filter(|l| l.fresh) {
                parts.entry(l.id).or_default().push(l.typ)
            }
        }
        for l in leaves.iter_mut().filter(|l| l.fresh) {
            if let Some(ts) = parts.get(&l.id) {
                let ts: SmallVec<[&Type; 4]> = ts.iter().collect();
                l.typ = Type::union_exact(env, &ts)?;
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
            | Type::Data
            | Type::Function
            | Type::Singleton
            | Type::OneNumber
            | Type::NoError
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
        let mut retype = |id| ctx.env.retype(id, Type::empty_tvar());
        match &type_predicate {
            Type::Primitive(p) if explicit && *p == Typ::Error => {
                structure_predicate.ids(&mut retype)
            }
            _ if !explicit => structure_predicate.fresh_ids(&mut retype),
            _ => (),
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
        for (id, prev) in saved {
            ctx.event.variables.remove(&id);
            ctx.rt.store_put(id, prev);
        }
    }

    /// Update the guard, which the chain reaches, and return its
    /// production tag (`None` = no guard). The caller reads channel
    /// bottomness off `guard.tag`.
    pub(super) fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> Option<Tag> {
        match &mut self.guard {
            None => None,
            Some(g) => Some(g.update(ctx)),
        }
    }

    /// The chain no longer reaches the guard: it sleeps.
    pub(super) fn sleep_guard(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Some(g) = &mut self.guard {
            g.sleep(ctx)
        }
    }

    /// The O(1) shallow discriminator for this arm's predicate
    /// against the select's scrutinee type ([`Type::shallow_discriminant`]);
    /// `None` = run the full `is_a` walk. Read once every tvar in the
    /// predicate is settled, at the select's first consult.
    pub(super) fn shallow_discriminant(
        &self,
        env: &Env,
        scrutinee: &Type,
    ) -> Option<Type> {
        if self.explicit_type_predicate && !self.covers_what_it_meets(env, scrutinee) {
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

    /// Whether a written predicate holds the whole of every member of
    /// `scrutinee` it could match, so telling the members apart decides
    /// it: `Array<i64> as a` over `Array<[i64, string]>` tests elements.
    fn covers_what_it_meets(&self, env: &Env, scrutinee: &Type) -> bool {
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        if union_members(env, scrutinee, &mut members).is_err() {
            return false;
        }
        let pred = &self.type_predicate;
        !members.is_empty()
            && members.iter().all(|m| match pred.could_match(env, m) {
                Ok(false) => true,
                Ok(true) => {
                    !m.has_unbound()
                        && pred
                            .contains_with_flags(BitFlags::empty(), env, m)
                            .unwrap_or(false)
                }
                Err(_) => false,
            })
    }

    /// Whether the arm's type and structure predicates admit `v`, given
    /// the arm's shallow discriminator. The checker narrows the arm's
    /// binds by exactly this, so a value that fails it is never
    /// delivered to them.
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
        let t = shallow.unwrap_or(&self.type_predicate);
        let typed = if self.explicit_type_predicate {
            t.is_a(env, v)
        } else {
            t.is_a_with(env, IsAFlags::MatchAbstract.into(), v)
        };
        typed && self.structure_predicate.is_match(v)
    }

    /// The arm's verdict over a value whose shape test gave `shaped`
    /// ([`Self::shape_matches`]).
    pub(super) fn arm_match(&self, shaped: bool) -> ArmMatch {
        if !shaped {
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

/// The elements a tuple or array slice pattern reads from `v`.
fn slice_elems(kind: SliceKind, v: &Value) -> Option<&[Value]> {
    match kind {
        SliceKind::Tuple => rep::tuple_elems(v),
        _ => rep::array_elems(v).map(|a| &a[..]),
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
            Self::Struct { all, width, binds } => {
                all.encoded_len()
                    + width.encoded_len()
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
            Self::Struct { all, width, binds } => {
                buf.put_u8(6);
                all.encode(buf)?;
                width.encode(buf)?;
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
                let width = Pack::decode(buf)?;
                let n = crate::image::count_decode(buf)?;
                let binds = (0..n)
                    .map(|_| {
                        Ok((Pack::decode(buf)?, Pack::decode(buf)?, Pack::decode(buf)?))
                    })
                    .collect::<Result<_, PackError>>()?;
                Self::Struct { all, width, binds }
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
