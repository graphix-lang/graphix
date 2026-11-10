use super::{
    Held, Holders, WakeBit,
    compiler::compile,
    pattern::{
        ArmMatch, CompiledAbstracts, Holder, PatternNode, SliceKind, StructPatternNode,
        rigid_refusal, written_refusal,
    },
    wake::{ArmMask, TrackedFires},
};
use crate::{
    BindId, CFlag, CompileCtx, ExecCtx, Node, NodeView, PrintFlag, Refs, Rt, Scope, Tag,
    TagValue, Update, View, bailat,
    env::Env,
    expr::{
        At, Decided, Expr, ExprId, ExprKind, Origin, Pattern, StructurePattern,
        UndecidedMembers, union_members, union_members_open, union_members_with,
    },
    format_with_flags,
    fusion::emit::{BodyCx, CompiledExpr, emit_select_node},
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, put_tag},
    },
    prune::BoundaryRef,
    typ::{
        AbstractId, ContainsFlags, InfiniteType, NodeKey, ShallowScrutinee, TVar, Type,
        TypeMismatch,
    },
    wrap,
};
use ahash::AHashMap;
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, encode_varint};
use netidx_value::{Typ, Value};
use nohash::IntSet;
use poolshark::local::LPooled;
use smallvec::{SmallVec, smallvec};
use std::{mem, sync::OnceLock};
use triomphe::Arc;

/// Where an error in a pattern is placed: the pattern's text when it was
/// parsed, else its arm's body.
fn pattern_site(pat: &Pattern, body: &Expr) -> Expr {
    match (pat.pos.get(), pat.end.get()) {
        (Some(pos), Some(end)) => {
            let mut site = ExprKind::NoOp.to_expr(pos).ending(end);
            site.ori = body.ori.clone();
            site
        }
        _ => body.clone(),
    }
}

#[derive(Debug)]
pub struct Select<R: Rt> {
    /// The selected arm. Semantic state: survives sleep.
    selected: Option<usize>,
    pub arg: Held<R>,
    pub arms: Vec<(PatternNode<R>, Node<R>)>,
    pub typ: Type,
    pub(crate) spec: Arc<Expr>,
    /// Bit i = arm i's guard was consulted by the last re-match. Quiet
    /// cycles read it so a standing bottom on a consulted guard keeps
    /// the select bottom.
    consulted_guard_mask: ArmMask,
    resident: TagValue,
    /// Set by `sleep()`; the next update re-matches against the present
    /// scrutinee, which may have moved while no reader was awake.
    slept: WakeBit,
    arm_facts: Option<LazyArmFacts>,
    discriminants: Option<Arc<ArmDiscriminants>>,
    /// The compile's flags, for the definition check's warnings.
    flags: BitFlags<CFlag>,
}

/// Facts about the arms that need the sealed scrutinee type or the
/// analysis facts, so they are built on the first update.
#[derive(Debug)]
struct LazyArmFacts {
    tracked: TrackedFires,
    /// Per arm: sleep on deselect? False only for a pure non-recursive
    /// body, which is neither slept nor updated while untaken.
    sleep_on_deselect: ArmMask,
    /// Per arm: entered before. A first entry is the arm's birth, every
    /// later one a resume.
    entered: ArmMask,
    discriminants: Arc<ArmDiscriminants>,
    /// A value no arm matched was logged: once per select.
    logged_no_match: bool,
    /// The arms with a guard, in order.
    guarded: SmallVec<[usize; 4]>,
    /// Per arm: a scrutinee fire the chain passed its guard by; the guard
    /// is given the value fired when the chain next reaches it.
    missed: ArmMask,
    /// A select with guards: the ones the chain does not reach sleep.
    guards: Option<Vec<GuardRun>>,
    /// The arms whose deselect kept a recursive callee asleep.
    kept: ArmMask,
}

#[derive(Debug)]
pub(crate) struct ArmDiscriminants {
    shallow: Box<[OnceLock<Option<Type>>]>,
    scrutinee: ShallowScrutinee,
}

impl ArmDiscriminants {
    pub(crate) fn new(scrutinee: Type, arms: usize) -> Self {
        Self {
            shallow: (0..arms).map(|_| OnceLock::new()).collect(),
            scrutinee: ShallowScrutinee::new(scrutinee),
        }
    }
}

/// A guard's run so far, as the chain reached it.
#[derive(Debug, Clone, Copy)]
enum GuardRun {
    /// Neither run nor slept: the select's first update has not come.
    Unborn,
    /// Slept before its first run: its first reach is its birth.
    Unreached,
    /// Ran when the chain last came by.
    Awake,
    /// Ran, then went unreached: its next reach is a resume.
    Asleep,
}

impl GuardRun {
    /// The chain passes the guard by: whether it falls asleep now.
    fn unreached(&mut self) -> bool {
        let (next, falls) = match *self {
            GuardRun::Unborn => (GuardRun::Unreached, true),
            GuardRun::Awake => (GuardRun::Asleep, true),
            run => (run, false),
        };
        *self = next;
        falls
    }

    /// The chain reaches the guard: the view it runs under.
    fn reached(&mut self) -> View {
        match mem::replace(self, GuardRun::Awake) {
            GuardRun::Unborn | GuardRun::Awake => View::Cycle,
            GuardRun::Unreached => View::Step,
            GuardRun::Asleep => View::Wake,
        }
    }
}

fn guard_refs<R: Rt>(pat: &PatternNode<R>, r: &mut Refs) {
    if let Some(g) = &pat.guard {
        g.node.refs(r)
    }
}

impl LazyArmFacts {
    fn build<R: Rt>(
        ctx: &ExecCtx<'_, R>,
        arms: &[(PatternNode<R>, Node<R>)],
        discriminants: Arc<ArmDiscriminants>,
    ) -> Self {
        let has_guards = arms.iter().any(|(pat, _)| pat.guard.is_some());
        let consumers = arms.len() * (1 + usize::from(has_guards));
        LazyArmFacts {
            tracked: TrackedFires::new(&ctx.env, ctx.rt.cycle(), consumers, |i, r| {
                match arms.get(i) {
                    Some(arm) => arm_refs(arm, r),
                    None => guard_refs(&arms[i - arms.len()].0, r),
                }
            }),
            sleep_on_deselect: ArmMask::of(
                arms.iter().map(|(_, n)| crate::analysis::arm_sleeps_on_deselect(ctx, n)),
            ),
            entered: ArmMask::new(arms.len()),
            discriminants,
            logged_no_match: false,
            guarded: arms
                .iter()
                .enumerate()
                .filter(|(_, (pat, _))| pat.guard.is_some())
                .map(|(i, _)| i)
                .collect(),
            missed: ArmMask::new(arms.len()),
            guards: has_guards.then(|| vec![GuardRun::Unborn; arms.len()]),
            kept: ArmMask::new(arms.len()),
        }
    }
}

/// What this cycle's consumed inputs say about the emission: the
/// scrutinee's production and the guards the last re-match consulted.
struct EmissionPlanes {
    /// A consulted guard's production fired.
    guard_fire: bool,
    /// A non-bottom fire was consumed.
    sound: bool,
    /// Any consumed input fired, bottom or not.
    anyfire: bool,
    /// A consulted guard's current channel is bottom.
    consulted_bottom: bool,
}

impl EmissionPlanes {
    /// `arg_prod` is a non-bottom scrutinee production.
    fn new(guard_tags: &[(usize, Tag)], arg_prod: Tag, mask: &ArmMask) -> Self {
        let mut planes = EmissionPlanes {
            guard_fire: false,
            sound: arg_prod.triggers(),
            anyfire: arg_prod.triggers(),
            consulted_bottom: false,
        };
        let consulted = guard_tags.iter().filter(|(i, _)| mask.get(*i));
        for t in consulted.map(|(_, t)| *t) {
            if t.triggers() {
                planes.guard_fire = true;
                planes.anyfire = true;
                planes.sound |= !t.is_bottom();
            }
            planes.consulted_bottom |= t.is_bottom();
        }
        planes
    }

    /// The taken arm's production `(t, v)` as the select's: a value
    /// fires when the arm or a sound consumed input fired; a bottom arm
    /// sets the select bottom, fresh on the same condition.
    fn emit(&self, t: Tag, v: Option<Value>) -> TagValue {
        match v {
            Some(v) if t.is_fired() || self.sound => TagValue::fired(v),
            Some(v) => TagValue::stale(v),
            None if t.triggers() || self.sound => {
                TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM)
            }
            None => TagValue::tagged(Value::Null, Tag::STALE_BOTTOM),
        }
    }
}

/// Evaluate the taken arm, consuming its tracked fire bits with the
/// catch-up deliveries scoped to it, and the ones its own guard took.
fn evaluate_arm<R: Rt>(
    tracked: &mut TrackedFires,
    arm: &mut (PatternNode<R>, Node<R>),
    i: usize,
    guard_took: &[(usize, SmallVec<[BindId; 4]>)],
    ctx: &mut ExecCtx<'_, R>,
) -> (Tag, Option<Value>) {
    let mut injected = tracked.deliver(ctx, i);
    for (_, ids) in guard_took.iter().filter(|(j, _)| *j == i) {
        for id in ids {
            super::wake::inject(ctx, *id, &mut injected);
        }
    }
    let scope = tracked.open(ctx, i);
    let tv = arm.1.update(ctx);
    let t = tv.tag();
    let v = if t.is_bottom() { None } else { Some(tv.value_cloned()) };
    tracked.ran(ctx, i, scope);
    TrackedFires::restore(ctx.event, injected);
    tracked.evaluated(&ctx.env, i, |r| arm_refs(arm, r));
    (t, v)
}

impl<R: Rt> Select<R> {
    fn new(
        arg: Held<R>,
        arms: Vec<(PatternNode<R>, Node<R>)>,
        typ: Type,
        spec: Arc<Expr>,
        flags: BitFlags<CFlag>,
    ) -> Node<R> {
        Node::new(Self {
            selected: None,
            arg,
            arms,
            typ,
            spec,
            consulted_guard_mask: ArmMask::default(),
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
            arm_facts: None,
            discriminants: None,
            flags,
        })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let arg = Held::image_decode(ctx, buf)?;
        let n = crate::image::count_decode(buf)?;
        let mut arms = Vec::with_capacity(n);
        for _ in 0..n {
            let pat = PatternNode::image_decode(ctx, buf)?;
            let body = decode_node(ctx, buf)?;
            arms.push((pat, body));
        }
        let typ = Type::decode(buf)?;
        let spec = Arc::new(Expr::decode(buf)?);
        let flags = match bool::decode(buf)? {
            true => CFlag::RunEvent.into(),
            false => BitFlags::empty(),
        };
        Ok(Self::new(arg, arms, typ, spec, flags))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        arg: &Expr,
        arms: &[(Pattern, Expr)],
    ) -> Result<Node<R>> {
        let own = flags & CFlag::RunEvent;
        let flags = flags & !CFlag::RunEvent;
        let arg = Held::new(compile(ctx, flags, arg.clone(), scope, top_id)?);
        let inputs = scrutinee_inputs(&ctx.env, &arg.node);
        // every instance holds its arms: exactly as many as written
        let mut built = Vec::with_capacity(arms.len());
        for (i, (pat, body)) in arms.iter().enumerate() {
            let scope = scope.append_place("sel", &spec, Some(i));
            let pat = PatternNode::compile(
                ctx,
                flags,
                pat,
                &scope,
                top_id,
                body.pos,
                body.ori.clone(),
            )
            .at(&pattern_site(pat, body))
            .at(&spec)?;
            pat.structure_predicate
                .ids(&mut |id| ctx.env.mark_pattern_bind(id, inputs.clone()));
            let n = super::prune::Prune::compile(ctx, |ctx| {
                compile(ctx, flags, body.clone(), &scope, top_id)
            })
            .at(&spec)?;
            built.push((pat, n));
        }
        Ok(Self::new(arg, built, Type::empty_tvar(), spec, flags | own))
    }
}

/// Warn where an arm acts on constants alone: a `<-` whose right side
/// reads no variable, a raise to a handler whose operand reads none, or an
/// effectful call whose arguments read none (a function's name is no
/// variable read). Constants fire at an arm's birth, its first entry, and
/// stand at every later entry, which resumes the arm, so these act once.
/// Nested lambdas, selects and seq machines are left to their own checks.
fn warn_birth_only<R: Rt>(
    ctx: &CompileCtx<R>,
    flags: BitFlags<CFlag>,
    n: &Node<R>,
) -> Result<()> {
    let reads_nothing = |n: &Node<R>| {
        let mut refs = Refs::default();
        n.refs(&mut refs);
        let mut any = false;
        refs.with_refs(|id| any |= !ctx.bind_to_lambda.contains_key(&id));
        !any
    };
    let warn = |spec: &Expr, what: &str| {
        ctx.env.warn(
            flags,
            spec,
            spec.pos,
            spec.end.0,
            format_args!(
                "{what} reads no variable, so it acts only at this arm's first entry: \
                 a later entry resumes the arm and its constants do not fire again. \
                 Sample the event that selects the arm (`k ~ v`), or use a seq"
            ),
        )
    };
    crate::stack::ensure_sufficient(|| match n.view() {
        NodeView::Lambda(_) | NodeView::Select(_) | NodeView::SeqMachine(_) => Ok(()),
        // a handler's action runs on its errors, not at an entry
        NodeView::Catch(c) => warn_birth_only(ctx, flags, &c.handler),
        NodeView::Qop(q) if q.handler().is_some() && reads_nothing(&q.n) => {
            warn(n.spec(), "this raise")
        }
        NodeView::Connect(c) if reads_nothing(&c.node) => warn(&c.spec, "this write"),
        NodeView::CallSite(cs)
            if cs.arg_nodes().all(|a| reads_nothing(a)) && acts_on_the_world(ctx, cs) =>
        {
            warn(n.spec(), "this call")
        }
        _ => {
            let mut r = Ok(());
            n.for_each_child(&mut |c| {
                if r.is_ok() {
                    r = warn_birth_only(ctx, flags, c)
                }
            });
            r
        }
    })
}

/// Whether the call acts: an async or Io builtin given an argument (one
/// given none produces at its birth and stands), or a function whose body
/// performs Io, raises or writes a binding it does not own.
fn acts_on_the_world<R: Rt>(
    ctx: &CompileCtx<R>,
    cs: &super::callsite::CallSite<R>,
) -> bool {
    if let Some(b) = super::callsite::static_builtin(ctx, cs) {
        return cs.arg_nodes().next().is_some()
            && matches!(
                ctx.builtin_effect(b),
                crate::Effect::Async | crate::Effect::Io(_)
            )
            && !ctx.builtin_resumes(b);
    }
    let NodeView::Ref(r) = cs.fnode().view() else { return false };
    ctx.bind_to_lambda
        .get(&r.id)
        .and_then(|v| v.downcast_ref::<super::lambda::LambdaDef<R>>())
        .is_some_and(|d| {
            let facts = d.facts.lock();
            facts.io || facts.event
        })
}

/// Warn where an arm holds a `seq` or `seqq` with no trigger, or one that
/// names no variable: it runs at its birth, the arm's first entry, and not
/// at a later one.
fn warn_triggerless_seq<R: Rt>(
    ctx: &CompileCtx<R>,
    flags: BitFlags<CFlag>,
    e: &Expr,
) -> Result<()> {
    crate::stack::ensure_sufficient(|| match &e.kind {
        ExprKind::Lambda(_) | ExprKind::Select(_) => Ok(()),
        ExprKind::Seq(s) => {
            let what = match &s.trigger {
                None => "this seq has no trigger",
                Some(t) if names_nothing(t.expr()) => {
                    "this seq's trigger reads no variable"
                }
                Some(_) => return Ok(()),
            };
            ctx.env.warn(
                flags,
                e,
                e.pos,
                e.end.0,
                format_args!(
                    "{what}, so it runs only at this arm's first entry: a later entry \
                     resumes the arm. Trigger it on the event that selects the arm"
                ),
            )
        }
        _ => {
            let mut r = Ok(());
            e.for_each_child(&mut |c| {
                if r.is_ok() {
                    r = warn_triggerless_seq(ctx, flags, c)
                }
            });
            r
        }
    })
}

/// Whether `e` names no binding: a constant, or built of constants.
fn names_nothing(e: &Expr) -> bool {
    crate::stack::ensure_sufficient(|| {
        let mut none = !matches!(e.kind, ExprKind::Ref { .. });
        e.for_each_child(&mut |c| none = none && names_nothing(c));
        none
    })
}

/// Do two atoms test the same thing: equal literals and heads at every
/// position, a bind and `_` alike?
fn same_test(a: &StructPatternNode, b: &StructPatternNode) -> bool {
    use StructPatternNode as P;
    let all = |x: &[P], y: &[P]| {
        x.len() == y.len() && x.iter().zip(y).all(|(x, y)| same_test(x, y))
    };
    crate::stack::ensure_sufficient(|| match (a, b) {
        (P::Ignore | P::Bind(_), P::Ignore | P::Bind(_)) => true,
        (P::Literal(x), P::Literal(y)) => x == y,
        (P::Slice { kind: k0, binds: b0, .. }, P::Slice { kind: k1, binds: b1, .. }) => {
            k0 == k1 && all(b0, b1)
        }
        (
            P::SlicePrefix { list: l0, prefix: p0, .. },
            P::SlicePrefix { list: l1, prefix: p1, .. },
        ) => l0 == l1 && all(p0, p1),
        (P::SliceSuffix { suffix: s0, .. }, P::SliceSuffix { suffix: s1, .. }) => {
            all(s0, s1)
        }
        (P::Struct { binds: b0, .. }, P::Struct { binds: b1, .. }) => {
            b0.len() == b1.len()
                && b0
                    .iter()
                    .zip(b1.iter())
                    .all(|((n0, _, p0), (n1, _, p1))| n0 == n1 && same_test(p0, p1))
        }
        (
            P::Variant { tag: t0, binds: b0, .. },
            P::Variant { tag: t1, binds: b1, .. },
        ) => t0 == t1 && all(b0, b1),
        (P::Abstract { id: i0, bind: x0, .. }, P::Abstract { id: i1, bind: x1, .. }) => {
            i0 == i1 && same_test(x0, x1)
        }
        (P::Or { alts: a0 }, P::Or { alts: a1 }) => all(a0, a1),
        _ => false,
    })
}

fn type_test<R: Rt>(p: &PatternNode<R>) -> Option<&Type> {
    p.explicit_type_predicate.then_some(&p.type_predicate)
}

/// Refuse a refutable atom that tests what an earlier unguarded one does,
/// under the same type test: it matches nothing new.
fn check_repeats<R: Rt>(env: &Env, arms: &[(PatternNode<R>, Node<R>)]) -> Result<()> {
    for (i, (pat, body)) in arms.iter().enumerate() {
        for (sp, at) in pat.atoms() {
            if sp.covers(env, &at, !pat.explicit_type_predicate) {
                continue;
            }
            let repeat =
                arms[..i].iter().filter(|(p, _)| p.guard.is_none()).any(|(p, _)| {
                    type_test(p) == type_test(pat)
                        && p.atoms().iter().any(|(e, _)| same_test(e, sp))
                });
            if repeat {
                bailat!(
                    body.spec(),
                    "unreachable arm: an earlier arm matches the same values, unused match \
                     cases"
                )
            }
        }
    }
    Ok(())
}

const SLICE_LENGTHS_COVERED: &str =
    "every array length this slice pattern can match is covered by earlier arms";

/// An arm nothing reaches: its refusal.
struct Dead(anyhow::Error);

/// What reaches each arm of a select, walked arm by arm: the scrutinee
/// less what the unguarded arms before it cover (irrefutable atoms,
/// `null`, a true/false pair, a completed literal pool or part, a
/// completed slice ladder). It narrows an arm's binds, an arm that can match none
/// of it is dead, and the select is exhaustive when nothing reaches past
/// the last arm. Quadratic in the arm count (each arm re-walks what still
/// reaches; 4000 tag arms check in about 2 s): member sets indexed by
/// head would make it linear.
struct Reach {
    /// What still reaches the next arm.
    atype: Type,
    /// Whose members the slice ladders and the literal pool count: an
    /// open cell's one typed bound, whose values it holds, else the
    /// scrutinee.
    members_of: Type,
    ladders: SmallVec<[Ladder; 4]>,
    nested: SmallVec<[NestedLadder; 2]>,
    pool: LiteralPool,
    parts: PartPool,
    /// The members of `members_of` the pools do not see: its open cells,
    /// each as its one typed bound where it has one, and `Any`.
    unpooled: SmallVec<[Type; 2]>,
    bools: (bool, bool),
    /// What the unguarded arms cover, as containment reads it.
    covered: SmallVec<[Type; 8]>,
    /// An unguarded arm matches every value.
    wildcard: bool,
    guarded_slice: bool,
    refutable_slice: bool,
    /// What was built into a scrutinee cell the arms decided.
    built: Option<Built>,
}

/// The union of the lower bounds of a scrutinee cell the arms decide, as
/// written before the arms bound its cells: each cell open then is a
/// placeholder named as the cell beside it. `None` where it has no
/// written form.
struct Built {
    typ: Option<Type>,
    cells: SmallVec<[(ArcStr, TVar); 2]>,
}

impl Reach {
    /// The walk over `scrut`. An under-constrained scrutinee first
    /// settles against the arms' informative predicates, except a union
    /// scrutinee's open members, which are what the arms discriminate.
    /// An open cell takes the inferred predicates' cells as its parts;
    /// any other scrutinee settles against copies of them, so a name is
    /// typed only where its arm narrows: its part of every member the arm
    /// can match. A copy the settle leaves open, the part an open cell
    /// inside the scrutinee took, is the arm's own cell again.
    fn new<R: Rt>(
        env: &Env,
        scrut: &Type,
        arms: &[(PatternNode<R>, Node<R>)],
    ) -> Result<Self> {
        let mut preds: LPooled<Vec<&PatternNode<R>>> = LPooled::take();
        // an arm that matches anything before narrowing steers only the
        // settle: narrowing may bind its cells to one member
        let mut wildcard = false;
        for (pat, _) in arms.iter() {
            match pat.matches_every(env, scrut)? {
                true => wildcard |= pat.guard.is_none(),
                false => preds.push(pat),
            }
        }
        let mut itypes: LPooled<Vec<&Type>> = LPooled::take();
        itypes.extend(preds.iter().map(|p| &p.type_predicate));
        let itype = Type::union_exact(env, &itypes)?;
        drop(itypes);
        let union_scrut = match scrut.deref_cloned() {
            Some(Type::Set(_)) => true,
            Some(t @ Type::Ref(_)) => matches!(t.lookup_ref(env)?, Type::Set(_)),
            _ => false,
        };
        let informative = itype != Type::Primitive(BitFlags::empty());
        let mut built = None;
        if informative && !(wildcard && union_scrut) && scrut.has_unbound() {
            if let Type::TVar(tv) = scrut
                && let Some(cell) = tv.open_cell()
            {
                let lower = cell.cell_lower();
                if !lower.is_empty() {
                    let mut cells: SmallVec<[(ArcStr, TVar); 2]> = SmallVec::new();
                    let typ = Type::Set(Arc::from_iter(lower)).written_form(&mut |tv| {
                        if tv.is_bound() || !tv.cell_lower().is_empty() {
                            return None;
                        }
                        let name = match cells.iter().find(|(_, c)| c.same_cell(tv)) {
                            Some((n, _)) => n.clone(),
                            None => {
                                cells.push((tv.name.clone(), tv.clone()));
                                tv.name.clone()
                            }
                        };
                        Some(Type::TVar(TVar::empty_named(name)))
                    });
                    built = Some(Built { typ, cells });
                }
            }
            match matches!(scrut, Type::TVar(tv) if tv.open_cell().is_some()) {
                true => {
                    let _ = itype.contains(env, scrut);
                }
                false => {
                    let mut written: LPooled<AHashMap<usize, TVar>> = LPooled::take();
                    for p in preds.iter().filter(|p| p.explicit_type_predicate) {
                        p.type_predicate.reached_cells(&mut written)
                    }
                    if !itype.contains_through_copy(env, &mut written, scrut)? {
                        bail!(
                            "the arms decide this scrutinee's type from their patterns, \
                             and cannot: annotate its type where it is built"
                        )
                    }
                }
            }
        }
        let atype = scrut.normalize();
        let members_of = atype.open_bound(env).unwrap_or_else(|| atype.clone());
        let ladders = Ladder::of(env, &members_of)?;
        let parts = PartPool::new(env, &members_of, arms)?;
        let mut unpooled: SmallVec<[Type; 2]> = SmallVec::new();
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        union_members_with(env, &members_of, &mut members, &mut |tv| {
            unpooled.push(match &tv.typed_bounds(env)[..] {
                [b] => b.clone(),
                _ => Type::TVar(tv.clone()),
            })
        })?;
        unpooled.extend(members.into_iter().filter(|m| matches!(m, Type::Any)));
        Ok(Self {
            atype,
            members_of,
            ladders,
            nested: SmallVec::new(),
            pool: LiteralPool::default(),
            parts,
            unpooled,
            bools: (false, false),
            covered: SmallVec::new(),
            wildcard: false,
            guarded_slice: false,
            refutable_slice: false,
            built,
        })
    }

    /// Whether an arm whose written type test is `explicit` can match a
    /// value of a member the pools do not see, so the pools never find it
    /// dead.
    fn reaches_unpooled(&self, env: &Env, explicit: Option<&Type>) -> Result<bool> {
        for m in self.unpooled.iter() {
            match explicit {
                Some(t) if !t.could_match(env, m)? => (),
                _ => return Ok(true),
            }
        }
        Ok(false)
    }

    /// A refusal of an arm's binds over a scrutinee the arms decided,
    /// told as the annotation that fixes it.
    fn untyped_binds(&self, env: &Env, at: &Origin, e: anyhow::Error) -> anyhow::Error {
        let Some(built) = &self.built else { return e };
        if e.downcast_ref::<TypeMismatch>().is_none()
            && e.downcast_ref::<InfiniteType>().is_none()
        {
            return e;
        }
        let mut holders = Holders::new(env, None);
        let known: Option<AHashMap<ArcStr, Type>> = built
            .cells
            .iter()
            .map(|(n, tv)| {
                Some((n.clone(), holders.name_cell(tv).or_else(|| tv.written_open())?))
            })
            .collect();
        match (&built.typ, known) {
            (Some(typ), Some(known)) => {
                let typ = typ.replace_tvars(&known);
                let meanings = holders.meanings(at);
                anyhow!(
                    "the arms decide this scrutinee's type from their patterns, and their \
                     binds cannot hold what was built into it, {typ}: annotate that type \
                     where it is built{meanings}"
                )
            }
            _ => anyhow!(
                "the arms decide this scrutinee's type from their patterns, and their \
                 binds cannot hold what was built into it: annotate its type where it is \
                 built"
            ),
        }
    }

    /// Take away what `t` covers from what reaches the arms after.
    fn cover(&mut self, env: &Env, t: Type) -> Result<()> {
        self.atype = self.atype.diff(env, &t)?;
        self.covered.push(t);
        Ok(())
    }

    /// The arm `pat`, whose body is `site`, against what reaches it
    /// (`Err(Dead)` when it is dead and `checking`), then less what it
    /// covers.
    fn arm<R: Rt>(
        &mut self,
        env: &Env,
        scrut: &Type,
        pat: &PatternNode<R>,
        site: &Expr,
        checking: bool,
    ) -> Result<Result<(), Dead>> {
        let dead_at =
            |e: anyhow::Error| -> Result<Result<(), Dead>> { Ok(Err(Dead(e.at(site)))) };
        let empty = Type::Primitive(BitFlags::empty());
        if checking && (self.wildcard || self.atype == empty) {
            return dead_at(anyhow!(
                "unreachable arm: the earlier arms already cover the whole scrutinee, \
                 unused match cases"
            ));
        }
        if checking && !pat.type_predicate.could_match(env, &self.atype)? {
            let (tp, atype) = (&pat.type_predicate, &self.atype);
            return dead_at(format_with_flags(PrintFlag::DerefTVars, || {
                if pat.explicit_type_predicate && tp.has_bottom() {
                    anyhow!(
                        "pattern {tp} will never match {atype}, unused match cases (`_` in \
                         a type is bottom, the type of never(); a type test that admits any \
                         parameter is spelled with Any, e.g. Error<Any>)"
                    )
                } else {
                    anyhow!("pattern {tp} will never match {atype}, unused match cases")
                }
            }));
        }
        let wild = pat.matches_every(env, scrut)?;
        let unguarded = pat.guard.is_none();
        self.wildcard |= wild && unguarded;
        if !unguarded {
            self.guarded_slice |= pat.structure_predicate.is_array_slice();
        }
        let atoms = pat.atoms();
        let or_arm = atoms.len() > 1;
        let dead = |what: &str| {
            dead_at(match or_arm {
                true => anyhow!(
                    "unreachable or-pattern alternative: {what}, unused match cases"
                ),
                false => anyhow!("unreachable arm: {what}, unused match cases"),
            })
        };
        let probe = BitFlags::empty();
        for (sp, at) in atoms.iter() {
            if checking && or_arm && !at.could_match(env, &self.atype)? {
                let atype = &self.atype;
                return dead_at(format_with_flags(PrintFlag::DerefTVars, || {
                    if *atype == empty {
                        anyhow!(
                            "unreachable or-pattern alternative: the earlier arms and \
                             alternatives already cover everything {at} matches, unused \
                             match cases"
                        )
                    } else {
                        anyhow!(
                            "unreachable or-pattern alternative: {at} will never match \
                             {atype}, unused match cases"
                        )
                    }
                }));
            }
            if let Some((k, exact)) = sp.array_len_range() {
                let (mut any, mut all) = (false, true);
                for l in self.ladders.iter() {
                    if at.could_match(env, &l.member)? {
                        any = true;
                        all &= l.covers(k, exact);
                    }
                }
                if checking && any && all {
                    return dead(SLICE_LENGTHS_COVERED);
                }
                let explicit = pat.explicit_type_predicate.then_some(at);
                if unguarded {
                    let any_member = sp.array_len_coverage(env, explicit).is_some();
                    let mut done: SmallVec<[Type; 2]> = SmallVec::new();
                    for l in self.ladders.iter_mut() {
                        if !l.complete()
                            && (any_member || sp.elements_cover(env, &l.member))
                            && at.contains_with_flags(probe, env, &l.member)?
                            && l.claim(k, exact)
                        {
                            done.push(l.member.clone());
                        }
                    }
                    for m in done {
                        self.cover(env, m)?
                    }
                }
            } else if self.nested_slices(env, sp, at, checking, unguarded)? {
                return dead(SLICE_LENGTHS_COVERED);
            }
            if !unguarded {
                continue;
            }
            if let StructPatternNode::Literal(v) = sp {
                match v {
                    Value::Bool(b) => {
                        self.bools.0 |= *b;
                        self.bools.1 |= !*b;
                        if self.bools == (true, true) {
                            self.cover(env, Type::Primitive(Typ::Bool.into()))?
                        }
                    }
                    Value::Null => self.cover(env, Type::Primitive(Typ::Null.into()))?,
                    _ => (),
                }
            }
            let explicit = pat.explicit_type_predicate.then_some(&pat.type_predicate);
            match self.pool.claim(env, &self.members_of, sp, explicit)? {
                Claim::Dead if checking && !self.reaches_unpooled(env, explicit)? => {
                    return dead(
                        "every combination of heads it matches is covered by earlier arms",
                    );
                }
                // a slice group covers its length of the member, on its ladder
                Claim::Completes(m) => match sp.array_len_range() {
                    Some((k, exact)) => {
                        let mut done = false;
                        for l in self.ladders.iter_mut() {
                            if l.member == m && !l.complete() {
                                done |= l.claim(k, exact);
                            }
                        }
                        if done {
                            self.cover(env, m)?
                        }
                    }
                    None => self.cover(env, m)?,
                },
                Claim::Dead | Claim::Open | Claim::Unpooled => (),
            }
            match self.parts.claim(env, sp, explicit)? {
                PartClaim::Dead
                    if checking && !self.reaches_unpooled(env, explicit)? =>
                {
                    return dead(
                        "every combination of heads it matches is covered by earlier arms",
                    );
                }
                PartClaim::Dead => (),
                PartClaim::Covers(ts) => {
                    for t in ts {
                        self.cover(env, t)?
                    }
                }
            }
            if !pat.explicit_type_predicate && sp.holds_abstract() {
                // an inferred abstract predicate tests the tag at run time: the
                // atom covers each member its pattern covers
                let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                union_members(env, scrut, &mut members)?;
                for m in members {
                    if sp.covers(env, &m, false) {
                        self.cover(env, m)?
                    }
                }
            } else if sp.covers(env, at, !pat.explicit_type_predicate) {
                match wild {
                    true => self.atype = self.atype.diff(env, at)?,
                    false => self.cover(env, at.clone())?,
                }
            } else {
                // a null literal covers its position's null, a constructor
                // its members of a union; under a written type only a closed one
                if (!pat.explicit_type_predicate || !at.has_unbound())
                    && let Some(t) = sp.matched_part(env, at)
                {
                    self.cover(env, t)?
                }
                // under a written union, each member the atom covers
                if pat.explicit_type_predicate {
                    let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                    union_members(env, at, &mut members)?;
                    if members.len() > 1 {
                        for m in members {
                            if sp.covers(env, &m, false) {
                                self.cover(env, m)?
                            }
                        }
                    }
                }
                if sp.is_array_slice() && sp.array_len_coverage(env, explicit).is_none() {
                    self.refutable_slice = true;
                }
            }
        }
        Ok(Ok(()))
    }

    /// The composite atom `sp`, whose predicate is `at`, against the
    /// nested ladders: true (`checking`) when at each member it can match
    /// one of its slices matches only lengths earlier arms claimed there;
    /// else, `unguarded`, it claims its lengths of each member it holds
    /// where one slice is all it refutes by, covering a member it completes.
    fn nested_slices(
        &mut self,
        env: &Env,
        sp: &StructPatternNode,
        at: &Type,
        checking: bool,
        unguarded: bool,
    ) -> Result<bool> {
        let dead_check = checking && !self.nested.is_empty();
        if !dead_check && !unguarded || !nests_slice(sp) {
            return Ok(false);
        }
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        union_members(env, &self.members_of, &mut members)?;
        let (mut any, mut all) = (false, true);
        let mut claims: SmallVec<[(Type, InnerSlice); 2]> = SmallVec::new();
        for m in members {
            let mut slices: SmallVec<[InnerSlice; 2]> = SmallVec::new();
            let only = inner_slices(env, sp, &m, &mut SmallVec::new(), &mut slices)?;
            if dead_check && at.could_match(env, &m)? {
                any = true;
                all &= slices.iter().any(|s| {
                    self.nested.iter().any(|l| {
                        l.ladder.member == m
                            && l.path == s.path
                            && l.ladder.covers(s.len.0, s.len.1)
                    })
                });
            }
            if unguarded
                && only
                && slices.len() == 1
                && at.contains_with_flags(BitFlags::empty(), env, &m)?
                && let Some(s) = slices.pop()
            {
                claims.push((m, s))
            }
        }
        if dead_check && any && all {
            return Ok(true);
        }
        for (m, s) in claims {
            let i = match self
                .nested
                .iter()
                .position(|l| l.ladder.member == m && l.path == s.path)
            {
                Some(i) => i,
                None => {
                    let ladder =
                        Ladder { member: m.clone(), exacts: SmallVec::new(), rest: None };
                    self.nested.push(NestedLadder { path: s.path, ladder });
                    self.nested.len() - 1
                }
            };
            if self.nested[i].ladder.claim(s.len.0, s.len.1) {
                self.cover(env, m)?
            }
        }
        Ok(false)
    }

    /// Nothing that reaches `pat`'s arm can pass its test.
    fn passes_nothing<R: Rt>(&self, env: &Env, pat: &PatternNode<R>) -> Result<bool> {
        Ok(self.wildcard
            || self.atype == Type::Primitive(BitFlags::empty())
            || !pat.type_predicate.could_match(env, &self.atype)?)
    }

    /// Nothing reaches past the last arm: an unguarded arm matches
    /// anything, or what the arms cover leaves no case, each of
    /// `undecided` one type whatever it becomes, else where the cover
    /// decides them.
    fn exhausted(&self, env: &Env, scrut: &Type, undecided: &[TVar]) -> Result<()> {
        let empty = Type::Primitive(BitFlags::empty());
        if self.wildcard || self.atype == empty {
            return Ok(());
        }
        let covered: LPooled<Vec<&Type>> = self.covered.iter().collect();
        let covered = Type::union_exact(env, &covered)?;
        if !undecided.is_empty() && covered.contains_holding(env, scrut, undecided)? {
            return Ok(());
        }
        if covered.contains(env, scrut)? {
            return Ok(());
        }
        // the hint names a ladder slice arms claimed, whose member still
        // reaches past the last arm
        let mut open = None;
        for l in self.ladders.iter() {
            let claimed = !l.exacts.is_empty() || l.rest.is_some();
            if claimed && !l.complete() && l.member.could_match(env, &self.atype)? {
                open = Some(l);
                break;
            }
        }
        format_with_flags(PrintFlag::DerefTVars, || {
            let gap = match open {
                None => format_compact!(""),
                Some(l) if l.rest.is_none() => format_compact!(
                    " (the slice arms cover finitely many lengths of {} — it also needs \
                     a rest pattern or a wildcard)",
                    l.member
                ),
                Some(l) => format_compact!(
                    " (the slice arms leave length {} of {} uncovered)",
                    l.gap().unwrap_or(0),
                    l.member
                ),
            };
            let refutable = match self.refutable_slice {
                true => {
                    " (a slice arm with refutable element patterns covers its length only \
                     with exact-length siblings whose bool literals and variant heads \
                     admit every combination; other literals, a rest and nested slices \
                     never do)"
                }
                false => "",
            };
            let guarded = match self.guarded_slice {
                true => " (a guarded slice arm cannot establish length coverage)",
                false => "",
            };
            let settled = match crate::typ::tvar::settled_note(env, &[scrut]) {
                Some(n) => format_compact!(": {n}"),
                None => format_compact!(""),
            };
            Err(anyhow!(
                "missing match cases: no arm covers {}{gap}{refutable}{guarded}{settled}",
                self.atype
            ))
        })
    }
}

/// The inputs whose fires reach a scrutinee, for the pattern binds it
/// delivers: its triggering free refs (a level under a sample's right
/// side is banked, never fired through), with a pattern bind among
/// them standing for its own inputs.
fn scrutinee_inputs<R: Rt>(env: &Env, arg: &Node<R>) -> Arc<[BindId]> {
    let mut r = Refs::default();
    arg.refs(&mut r);
    let mut out: LPooled<IntSet<BindId>> = LPooled::take();
    for id in r.triggering.difference(&r.bound) {
        match env.pattern_inputs(*id) {
            Some(inputs) => out.extend(inputs.iter().copied()),
            None => {
                out.insert(*id);
            }
        }
    }
    Arc::from_iter(out.drain())
}

/// The slice-length ladder over one array or list member of a
/// scrutinee: the exact lengths and the least `rest..` bound the slice
/// arms claim for it.
struct Ladder {
    member: Type,
    exacts: SmallVec<[usize; 8]>,
    rest: Option<usize>,
}

/// The slice-length ladder at one array or list position inside a
/// tuple, struct or variant member of a scrutinee, which `ladder.member`
/// names: the lengths there that arms testing nothing else claim.
struct NestedLadder {
    path: SmallVec<[u32; 4]>,
    ladder: Ladder,
}

impl Ladder {
    /// One ladder per array or list member of `scrut`.
    fn of(env: &Env, scrut: &Type) -> Result<SmallVec<[Ladder; 4]>> {
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        union_members(env, scrut, &mut members)?;
        Ok(members
            .into_iter()
            .filter(|m| matches!(m, Type::Array(_) | Type::List(_)))
            .map(|member| Ladder { member, exacts: SmallVec::new(), rest: None })
            .collect())
    }

    /// Do the claimed lengths cover every length? That needs a rest
    /// bound with every length below it exact.
    fn complete(&self) -> bool {
        self.gap().is_none()
    }

    /// The least length no claim covers, `None` when they cover all;
    /// with no rest bound every length past the exacts is a gap.
    fn gap(&self) -> Option<usize> {
        let r = self.rest.unwrap_or(usize::MAX);
        (0..r).find(|n| !self.exacts.contains(n))
    }

    /// Is the length range `== k` (`exact`) / `>= k` already claimed?
    fn covers(&self, k: usize, exact: bool) -> bool {
        let covered =
            |n: usize| self.rest.is_some_and(|r| n >= r) || self.exacts.contains(&n);
        match (exact, self.rest) {
            (true, _) => covered(k),
            (false, None) => false,
            (false, Some(r)) => (k..r.max(k)).all(covered),
        }
    }

    /// Claim `== k` (`exact`) or `>= k`; true when this claim completes
    /// the ladder.
    fn claim(&mut self, k: usize, exact: bool) -> bool {
        if self.complete() {
            return false;
        }
        if !exact {
            self.rest = Some(self.rest.map_or(k, |r| r.min(k)));
        } else if !self.exacts.contains(&k) {
            self.exacts.push(k)
        }
        self.complete()
    }
}

/// Deselect arm `j`: sleep it unless it is pure, then refresh its
/// tracked refs. Under [`super::deselecting_arm`] a recursive-edge callee
/// inside the arm is deleted, or kept asleep when pure (`CallSite::sleep`),
/// which marks the arm `kept`.
fn deselect<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    tracked: &mut TrackedFires,
    kept: &mut ArmMask,
    j: usize,
    arm: &mut (PatternNode<R>, Node<R>),
    sleep: bool,
) {
    if sleep && super::deselecting_arm(true, || arm.1.sleep(ctx)) {
        kept.set(j)
    }
    tracked.fell_asleep(j, ctx.rt.cycle());
    tracked.refresh(&ctx.env, j, |r| arm_refs(arm, r));
    tracked
        .observe_late(ctx, |i, b| i == j && BoundaryRef::held_by(arm_boundary(arm), b));
}

/// The walk-pruning boundary around an arm's body.
fn arm_boundary<R: Rt>(arm: &(PatternNode<R>, Node<R>)) -> Option<&BoundaryRef> {
    arm.1.as_prune().map(|p| p.boundary_ref())
}

/// The chain ran and took `taken` (`None`: no arm, or a bottom scrutinee),
/// an exit of a recursion through this select: each other kept arm sheds
/// part of what it keeps (`callsite::shed_kept`), and the arm taken
/// rebirths what it kept.
fn exit_kept<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    kept: &mut ArmMask,
    arms: &mut [(PatternNode<R>, Node<R>)],
    taken: Option<usize>,
) {
    if kept.is_empty() {
        return;
    }
    for (j, (_, body)) in arms.iter_mut().enumerate() {
        if kept.get(j) && (taken == Some(j) || !super::callsite::shed_kept(ctx, body)) {
            kept.unset(j)
        }
    }
}

/// An arm's free reads: its body's refs less its pattern's binds.
fn arm_refs<R: Rt>((pat, body): &(PatternNode<R>, Node<R>), r: &mut Refs) {
    body.refs(r);
    bound_by(pat, r)
}

fn bound_by<R: Rt>(pat: &PatternNode<R>, r: &mut Refs) {
    pat.structure_predicate.ids(&mut |id| {
        r.bound.insert(id);
    });
}

/// A refutable head the literal pool reads at one position of a
/// composite pattern.
#[derive(Clone, PartialEq, Eq)]
enum Head {
    Bool(bool),
    Null,
    /// A variant's tag and arity; its payload matches anything.
    Tag(ArcStr, usize),
    /// A union's one tuple or struct member, whose own positions count
    /// only under this head.
    Composite,
    /// A value of an open cell the position holds, which only a name or
    /// `_` there matches.
    Open,
    /// A value of a primitive member but `bool` and `null`, which only a
    /// name or `_` there matches.
    Rest,
}

/// One position of a composite pattern, keyed by its path into the
/// scrutinee member: the heads the arm admits there (`None` = any
/// value, which also covers every position under the path) and the
/// heads the member admits there (`None` = not a finite set of heads).
struct Leaf {
    path: SmallVec<[u32; 4]>,
    heads: Option<SmallVec<[Head; 2]>>,
    domain: Option<SmallVec<[Head; 4]>>,
}

/// Why the pool takes no leaves from a composite pattern against a member.
#[derive(Clone, Copy)]
enum Miss {
    /// The pattern has a refutable part the pool does not read.
    Unread,
    /// The pattern cannot match the member.
    Never,
}

/// The verdict over several positions: a `Never` decides, else an
/// `Unread`.
fn worst(misses: impl Iterator<Item = Result<Option<Miss>>>) -> Result<Option<Miss>> {
    let mut out = None;
    for m in misses {
        match m? {
            Some(Miss::Never) => return Ok(Some(Miss::Never)),
            Some(Miss::Unread) => out = Some(Miss::Unread),
            None => (),
        }
    }
    Ok(out)
}

/// `t` is `Any` or a union holding it.
fn may_be_any(env: &Env, t: &Type) -> Result<bool> {
    let mut members: SmallVec<[Type; 8]> = SmallVec::new();
    union_members(env, t, &mut members)?;
    Ok(members.iter().any(|m| matches!(m, Type::Any)))
}

/// Whether a value of `t` may have head `h`, one [`head_of`] gives: a
/// position holding an open cell may.
fn may_hold(env: &Env, t: &Type, h: &Head) -> Result<bool> {
    let mut members: SmallVec<[Type; 8]> = SmallVec::new();
    Ok(union_members_open(env, t, &mut members)?
        || members.is_empty()
        || members.iter().any(|m| match (h, m) {
            (Head::Bool(_), Type::Primitive(p)) => p.contains(Typ::Bool),
            (Head::Null, Type::Primitive(p)) => p.contains(Typ::Null),
            (Head::Tag(tag, n), Type::Variant(t, ts, _)) => tag == t && *n == ts.len(),
            (
                _,
                Type::Primitive(_)
                | Type::Variant(..)
                | Type::Tuple(_)
                | Type::Struct(_)
                | Type::Array(_)
                | Type::List(_)
                | Type::Map { .. }
                | Type::Abstract { .. }
                | Type::Error(_)
                | Type::Fn(_)
                | Type::ByRef(..),
            ) => false,
            _ => true,
        }))
}

/// The heads `t` admits when it is a finite set of them: a union of
/// `bool`, `null`, variants, at most one tuple or struct holding no open
/// cell, [`Head::Rest`] for its other primitives and [`Head::Open`] for
/// the open cells among them.
fn heads(env: &Env, t: &Type) -> Result<Option<SmallVec<[Head; 4]>>> {
    let mut members: SmallVec<[Type; 8]> = SmallVec::new();
    let open = union_members_open(env, t, &mut members)?;
    if members.is_empty() {
        return Ok(None);
    }
    let mut out: SmallVec<[Head; 4]> = SmallVec::new();
    let mut composite = false;
    for m in members.iter() {
        match m {
            Type::Variant(tag, ts, _) => out.push(Head::Tag(tag.clone(), ts.len())),
            Type::Tuple(_) | Type::Struct(_) if !composite && !m.has_unbound() => {
                composite = true;
                out.push(Head::Composite)
            }
            Type::Primitive(p) => {
                for typ in p.iter() {
                    match typ {
                        Typ::Bool => out.extend([Head::Bool(true), Head::Bool(false)]),
                        Typ::Null => out.push(Head::Null),
                        _ if out.contains(&Head::Rest) => (),
                        _ => out.push(Head::Rest),
                    }
                }
            }
            _ => return Ok(None),
        }
    }
    if open {
        out.push(Head::Open)
    }
    Ok(Some(out))
}

/// The head a pattern tests at one position, when it tests only a head:
/// a bool literal or a variant whose payload matches anything.
fn head_of(sp: &StructPatternNode) -> Option<Head> {
    match sp {
        StructPatternNode::Literal(Value::Bool(b)) => Some(Head::Bool(*b)),
        StructPatternNode::Literal(Value::Null) => Some(Head::Null),
        StructPatternNode::Variant { tag, all: _, binds }
            if binds.iter().all(|p| p.matches_anything()) =>
        {
            Some(Head::Tag(tag.clone(), binds.len()))
        }
        _ => None,
    }
}

/// The pooled positions of composite pattern `sp` against `m`, a
/// scrutinee member of its shape; `Unread` when the pattern has a
/// refutable part the pool does not read (another literal, a slice, a
/// type test, a variant with a refutable payload) or tests no head,
/// `Never` when it cannot match `m`.
fn pooled_leaves(
    env: &Env,
    sp: &StructPatternNode,
    m: &Type,
) -> Result<Result<SmallVec<[Leaf; 8]>, Miss>> {
    fn leaf(
        env: &Env,
        sp: &StructPatternNode,
        t: &Type,
        path: &mut SmallVec<[u32; 4]>,
        out: &mut SmallVec<[Leaf; 8]>,
    ) -> Result<Option<Miss>> {
        let admitted = match sp {
            StructPatternNode::Bind(_) | StructPatternNode::Ignore => None,
            StructPatternNode::Slice { kind: SliceKind::Tuple, .. }
            | StructPatternNode::Struct { .. }
            | StructPatternNode::Abstract { .. } => {
                return crate::stack::ensure_sufficient(|| {
                    children(env, sp, t, path, out)
                });
            }
            StructPatternNode::Or { alts } => {
                match alts.iter().map(head_of).collect::<Option<SmallVec<[Head; 2]>>>() {
                    Some(hs) => Some(hs),
                    None => return Ok(Some(Miss::Unread)),
                }
            }
            sp => match head_of(sp) {
                Some(h) => Some(SmallVec::from_iter([h])),
                None => return Ok(Some(Miss::Unread)),
            },
        };
        let domain = heads(env, t)?;
        if let Some(hs) = &admitted {
            let held = match &domain {
                Some(d)
                    if d.contains(&Head::Open) && !hs.iter().any(|h| d.contains(h)) =>
                {
                    return Ok(Some(Miss::Unread));
                }
                Some(d) => hs.iter().any(|h| d.contains(h)),
                None => {
                    let mut held = false;
                    for h in hs.iter() {
                        held = held || may_hold(env, t, h)?;
                    }
                    held
                }
            };
            if !held {
                return Ok(Some(Miss::Never));
            }
        }
        out.push(Leaf { path: path.clone(), heads: admitted, domain });
        Ok(None)
    }
    fn children(
        env: &Env,
        sp: &StructPatternNode,
        t: &Type,
        path: &mut SmallVec<[u32; 4]>,
        out: &mut SmallVec<[Leaf; 8]>,
    ) -> Result<Option<Miss>> {
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        let open = union_members_open(env, t, &mut members)?;
        let m = match &members[..] {
            [m] if !open => m,
            [] if open => return Ok(Some(Miss::Unread)),
            ms => {
                let Some(shape) = Shape::of(sp) else { return Ok(Some(Miss::Unread)) };
                let mut fit = ms.iter().filter(|m| shape.fits(m));
                let m = match (fit.next(), fit.next()) {
                    (None, _) => return Ok(Some(Miss::Never)),
                    (Some(m), None) => m,
                    (Some(_), Some(_)) => return Ok(Some(Miss::Unread)),
                };
                if !matches!(m, Type::Tuple(_) | Type::Struct(_)) {
                    return Ok(Some(Miss::Unread));
                }
                match heads(env, t)? {
                    Some(domain) if domain.contains(&Head::Composite) => out.push(Leaf {
                        path: path.clone(),
                        heads: Some(smallvec![Head::Composite]),
                        domain: Some(domain),
                    }),
                    _ => return Ok(Some(Miss::Unread)),
                }
                m
            }
        };
        let mut at =
            |i: usize, p: &StructPatternNode, t: &Type| -> Result<Option<Miss>> {
                path.push(i as u32);
                let r = leaf(env, p, t, path, out);
                path.pop();
                r
            };
        match (sp, m) {
            (
                StructPatternNode::Slice { kind: SliceKind::Tuple, all: _, binds },
                Type::Tuple(ts),
            )
            | (
                StructPatternNode::Variant { tag: _, all: _, binds },
                Type::Variant(_, ts, _),
            ) if ts.len() == binds.len() => worst(
                binds.iter().zip(ts.iter()).enumerate().map(|(i, (p, t))| at(i, p, t)),
            ),
            (StructPatternNode::Slice { kind: SliceKind::Tuple, .. }, Type::Tuple(_)) => {
                Ok(Some(Miss::Never))
            }
            (
                StructPatternNode::Slice { kind: SliceKind::Array, all: _, binds },
                Type::Array(e),
            )
            | (
                StructPatternNode::Slice { kind: SliceKind::List, all: _, binds },
                Type::List(e),
            ) => worst(binds.iter().enumerate().map(|(i, p)| at(i, p, e))),
            (
                StructPatternNode::Abstract { id, bind, .. },
                Type::Abstract { id: t, params },
            ) => match env.abstract_reps.get(id) {
                _ if id != t => Ok(Some(Miss::Never)),
                Some(r) => at(0, bind, &r.instantiate_with(params)),
                None => Ok(Some(Miss::Unread)),
            },
            (StructPatternNode::Struct { all: _, width, binds }, Type::Struct(fs)) => {
                if !width.admits(fs.len())
                    || !binds.iter().all(|(n, _, _)| fs.iter().any(|(f, _, _)| n == f))
                {
                    return Ok(Some(Miss::Never));
                }
                worst(fs.iter().enumerate().map(|(i, (name, t, _))| {
                    let p = binds.iter().find(|(n, _, _)| n == name);
                    at(i, p.map_or(&StructPatternNode::Ignore, |(_, _, p)| p), t)
                }))
            }
            _ => Ok(Some(Miss::Unread)),
        }
    }
    let mut out = SmallVec::new();
    Ok(match children(env, sp, m, &mut SmallVec::new(), &mut out)? {
        Some(miss) => Err(miss),
        None if out.iter().any(|l| l.heads.is_some()) => Ok(out),
        None => Err(Miss::Unread),
    })
}

/// An array or list slice a composite pattern tests inside a scrutinee
/// member: its path into the member, keyed as a [`Leaf`]'s, and the
/// lengths it matches, `(k, exact)` as `array_len_range` gives them.
struct InnerSlice {
    path: SmallVec<[u32; 4]>,
    len: (usize, bool),
}

/// Whether an array or list slice stands at a tuple, struct or variant
/// position of `sp`, where [`inner_slices`] looks for one.
fn nests_slice(sp: &StructPatternNode) -> bool {
    let holds = |p: &StructPatternNode| p.is_array_slice() || nests_slice(p);
    crate::stack::ensure_sufficient(|| match sp {
        StructPatternNode::Slice { kind: SliceKind::Tuple, all: _, binds }
        | StructPatternNode::Variant { tag: _, all: _, binds } => binds.iter().any(holds),
        StructPatternNode::Struct { all: _, width: _, binds } => {
            binds.iter().any(|(_, _, p)| holds(p))
        }
        _ => false,
    })
}

/// The slices composite pattern `sp` tests inside `t`, found along
/// positions of one member that fits the pattern there; true when they
/// are all it refutes a value of `t` by: everything else is a name or
/// `_`, and every slice's elements are too.
fn inner_slices(
    env: &Env,
    sp: &StructPatternNode,
    t: &Type,
    path: &mut SmallVec<[u32; 4]>,
    out: &mut SmallVec<[InnerSlice; 2]>,
) -> Result<bool> {
    if sp.matches_anything() {
        return Ok(true);
    }
    let mut members: SmallVec<[Type; 8]> = SmallVec::new();
    if union_members_open(env, t, &mut members)? {
        return Ok(false);
    }
    let [m] = &members[..] else { return Ok(false) };
    let mut children: SmallVec<[(&StructPatternNode, &Type); 8]> = SmallVec::new();
    match (sp, m) {
        (
            StructPatternNode::Slice { kind: SliceKind::Array, .. }
            | StructPatternNode::SlicePrefix { list: false, .. }
            | StructPatternNode::SliceSuffix { .. },
            Type::Array(_),
        )
        | (
            StructPatternNode::Slice { kind: SliceKind::List, .. }
            | StructPatternNode::SlicePrefix { list: true, .. },
            Type::List(_),
        ) if !path.is_empty() => {
            let Some(len) = sp.array_len_range() else { return Ok(false) };
            out.push(InnerSlice { path: path.clone(), len });
            return Ok(sp.array_len_coverage(env, None).is_some());
        }
        (
            StructPatternNode::Slice { kind: SliceKind::Tuple, all: _, binds },
            Type::Tuple(ts),
        ) if ts.len() == binds.len() => children.extend(binds.iter().zip(ts.iter())),
        (StructPatternNode::Variant { tag, all: _, binds }, Type::Variant(t, ts, _))
            if tag == t && ts.len() == binds.len() =>
        {
            children.extend(binds.iter().zip(ts.iter()))
        }
        (StructPatternNode::Struct { all: _, width, binds }, Type::Struct(fs))
            if width.admits(fs.len())
                && binds.iter().all(|(n, _, _)| fs.iter().any(|(f, _, _)| n == f)) =>
        {
            children.extend(fs.iter().map(|(name, t, _)| {
                let p = binds.iter().find(|(n, _, _)| n == name);
                (p.map_or(&StructPatternNode::Ignore, |(_, _, p)| p), t)
            }))
        }
        _ => return Ok(false),
    }
    let mut all = true;
    for (i, (p, t)) in children.into_iter().enumerate() {
        path.push(i as u32);
        all &= crate::stack::ensure_sufficient(|| inner_slices(env, p, t, path, out))?;
        path.pop();
    }
    Ok(all)
}

/// The head a composite pattern tests: what groups arms in the
/// literal pool and selects the scrutinee member they cover.
#[derive(PartialEq, Eq)]
enum Shape {
    Tuple(usize),
    /// An exact-length array or list slice: its group covers that length.
    Slice(SliceKind, usize),
    Variant(ArcStr, usize),
    Struct(SmallVec<[ArcStr; 8]>),
    Abstract(AbstractId),
}

impl Shape {
    fn of(sp: &StructPatternNode) -> Option<Shape> {
        match sp {
            StructPatternNode::Slice { kind: SliceKind::Tuple, all: _, binds } => {
                Some(Shape::Tuple(binds.len()))
            }
            StructPatternNode::Slice { kind, all: _, binds } => {
                Some(Shape::Slice(*kind, binds.len()))
            }
            StructPatternNode::Variant { tag, all: _, binds } => {
                Some(Shape::Variant(tag.clone(), binds.len()))
            }
            StructPatternNode::Struct { all: _, width: _, binds } => {
                let mut names: SmallVec<[ArcStr; 8]> =
                    binds.iter().map(|(n, _, _)| n.clone()).collect();
                names.sort();
                Some(Shape::Struct(names))
            }
            StructPatternNode::Abstract { id, .. } => Some(Shape::Abstract(*id)),
            _ => None,
        }
    }

    fn fits(&self, t: &Type) -> bool {
        match (self, t) {
            (Shape::Tuple(n), Type::Tuple(ts)) => *n == ts.len(),
            (Shape::Slice(SliceKind::Array, _), Type::Array(_))
            | (Shape::Slice(SliceKind::List, _), Type::List(_)) => true,
            (Shape::Variant(tag, n), Type::Variant(t, ts, _)) => {
                tag == t && *n == ts.len()
            }
            (Shape::Struct(names), Type::Struct(fs)) => {
                names.iter().all(|n| fs.iter().any(|(f, _, _)| n == f))
            }
            (Shape::Abstract(id), Type::Abstract { id: t, .. }) => id == t,
            _ => false,
        }
    }

    /// The scrutinee's members of this shape: the types a complete pool
    /// group may cover.
    fn members(&self, env: &Env, scrut: &Type) -> Result<SmallVec<[Type; 4]>> {
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        union_members(env, scrut, &mut members)?;
        Ok(members.into_iter().filter(|m| self.fits(m)).collect())
    }

    /// The shape that keys `m`'s pool group: a struct's by all of its
    /// fields, so arms naming different ones pool together.
    fn of_member(self, m: &Type) -> Shape {
        match (self, m) {
            (Shape::Struct(_), Type::Struct(fs)) => {
                Shape::Struct(fs.iter().map(|(n, _, _)| n.clone()).collect())
            }
            (shape, _) => shape,
        }
    }
}

/// A pool group's combinations are enumerated up to this many; a group
/// over more positions never completes.
const MAX_POOL_COMBINATIONS: usize = 1024;

/// Composite arms whose only refutable parts are bool literals and
/// variant heads pool coverage per position: same-shaped arms cover the
/// scrutinee's member of that shape once some arm matches every
/// combination of heads the member admits (`(true, `A) | (true, `B) |
/// (false, _)` covers `(bool, [`A, `B])`).
#[derive(Default)]
struct LiteralPool {
    groups: Vec<PoolGroup>,
}

struct PoolGroup {
    shape: Shape,
    member: Type,
    arms: Vec<SmallVec<[Leaf; 8]>>,
    done: bool,
    /// The combinations `arms` match, kept from the last count.
    matched: Option<Matched>,
}

/// The combinations a group's arms match, indexed as
/// [`PoolGroup::combinations`] numbers them over its first `positions`
/// positions. An arm adds its new positions after the group's, so a
/// group with as many positions numbers its combinations alike.
struct Matched {
    positions: usize,
    count: usize,
    bits: [u64; MAX_POOL_COMBINATIONS / 64],
}

impl Matched {
    fn get(&self, c: usize) -> bool {
        self.bits[c / 64] & (1 << (c % 64)) != 0
    }

    fn set(&mut self, c: usize) {
        if !self.get(c) {
            self.bits[c / 64] |= 1 << (c % 64);
            self.count += 1;
        }
    }
}

/// What pooling an atom decided.
#[derive(Debug, PartialEq)]
enum Claim {
    /// The pool does not read it.
    Unpooled,
    /// Its group does not cover its member yet.
    Open,
    /// It completes its group: the member it covers.
    Completes(Type),
    /// Every combination of heads it matches, earlier arms match.
    Dead,
}

impl LiteralPool {
    /// Pool the unguarded atom `sp`; `explicit`, its arm's written type
    /// test.
    fn claim(
        &mut self,
        env: &Env,
        scrut: &Type,
        sp: &StructPatternNode,
        explicit: Option<&Type>,
    ) -> Result<Claim> {
        let Some(shape) = Shape::of(sp) else { return Ok(Claim::Unpooled) };
        // the atom pools against the one member of its shape it can match:
        // under its type test, with a head each position admits
        let mut reached: Option<(Type, SmallVec<[Leaf; 8]>)> = None;
        for m in shape.members(env, scrut)? {
            if let Some(t) = explicit
                && !t.contains_with_flags(ContainsFlags::RigidCheck.into(), env, &m)?
            {
                continue;
            }
            let leaves = match pooled_leaves(env, sp, &m)? {
                Ok(leaves) => leaves,
                Err(Miss::Never) => continue,
                Err(Miss::Unread) => return Ok(Claim::Unpooled),
            };
            if reached.is_some() {
                return Ok(Claim::Unpooled);
            }
            reached = Some((m, leaves));
        }
        let Some((m, leaves)) = reached else { return Ok(Claim::Unpooled) };
        let shape = shape.of_member(&m);
        let i = match self.groups.iter().position(|g| g.shape == shape && g.member == m) {
            Some(i) => i,
            None => {
                let member = m.clone();
                self.groups.push(PoolGroup {
                    shape,
                    member,
                    arms: Vec::new(),
                    done: false,
                    matched: None,
                });
                self.groups.len() - 1
            }
        };
        Ok(self.groups[i].add(leaves))
    }
}

/// A member the parts cut is cut into at most this many; one that would
/// take more stays whole.
const MAX_POOL_PARTS: usize = 64;

/// What one position of a part holds: one member of the position's
/// union, each primitive its own, or one member of the typed bound of a
/// declared variable standing there.
struct Choice {
    typ: Type,
    /// The declared variable it is a member of, else `typ`.
    held: Type,
}

/// The choices at a position of type `t`; `None` where it reaches an
/// open cell no declaration holds rigid.
fn choices(env: &Env, t: &Type) -> Result<Option<SmallVec<[Choice; 4]>>> {
    let mut members: SmallVec<[Type; 8]> = SmallVec::new();
    let mut cells: SmallVec<[TVar; 2]> = SmallVec::new();
    union_members_with(env, t, &mut members, &mut |tv| cells.push(tv.clone()))?;
    let mut out: SmallVec<[Choice; 4]> = SmallVec::new();
    if !split_members(&members, None, &mut out) {
        return Ok(None);
    }
    for tv in cells.iter() {
        match tv.open_cell() {
            Some(cell) if cell.is_rigid() => (),
            _ => return Ok(None),
        }
        let held = Type::TVar(tv.clone());
        match &tv.typed_bounds(env)[..] {
            [b] => {
                let mut bound: SmallVec<[Type; 8]> = SmallVec::new();
                if union_members_open(env, b, &mut bound)?
                    || !split_members(&bound, Some(&held), &mut out)
                {
                    return Ok(None);
                }
            }
            _ => out.push(Choice { typ: held.clone(), held }),
        }
    }
    Ok(Some(out))
}

/// `members` as choices, a primitive set one per primitive, each a
/// member of `held` where given; false where one reaches an open cell no
/// declaration holds rigid.
fn split_members(
    members: &[Type],
    held: Option<&Type>,
    out: &mut SmallVec<[Choice; 4]>,
) -> bool {
    let choice =
        |typ: Type| Choice { held: held.cloned().unwrap_or_else(|| typ.clone()), typ };
    for m in members {
        if m.has_free() {
            return false;
        }
        match m {
            Type::Primitive(p) => {
                out.extend(p.iter().map(|typ| choice(Type::Primitive(typ.into()))))
            }
            m => out.push(choice(m.clone())),
        }
    }
    true
}

/// `m`, a tuple or struct, with `ts` at its positions.
fn with_positions(m: &Type, ts: impl Iterator<Item = Type>) -> Type {
    match m {
        Type::Struct(fs) => Type::Struct(Arc::from_iter(
            fs.iter().zip(ts).map(|((n, _, at), t)| (n.clone(), t, *at)),
        )),
        _ => Type::Tuple(Arc::from_iter(ts)),
    }
}

/// The literal pool over the parts the written tests cut the scrutinee's
/// tuple and struct members into, so a written test that holds only part
/// of a member covers that part with the arms beside it: `(true, _)`,
/// `(bool, i64) as (false, n)` and `(bool, null) as (false, _)` cover
/// `(bool, [i64, null])`. A member is cut where a written test can match
/// it and does not hold it: at each position into the choices there.
#[derive(Default)]
struct PartPool {
    parts: Vec<Part>,
    /// The tuple and struct members left whole.
    whole: SmallVec<[Type; 2]>,
}

struct Part {
    /// One choice at each position.
    typ: Type,
    /// `typ` with the declared variable each choice is a member of: what
    /// the part covers once every part of it is covered.
    held: Type,
    group: PoolGroup,
}

/// What pooling an atom into the parts decided.
enum PartClaim {
    /// Every combination of heads it matches in each part it reaches,
    /// earlier arms match.
    Dead,
    /// What its arm completes.
    Covers(SmallVec<[Type; 2]>),
}

impl PartPool {
    fn new<R: Rt>(
        env: &Env,
        members_of: &Type,
        arms: &[(PatternNode<R>, Node<R>)],
    ) -> Result<Self> {
        let mut pool = Self::default();
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        union_members(env, members_of, &mut members)?;
        let rigid = ContainsFlags::RigidCheck.into();
        for m in members {
            if !matches!(m, Type::Tuple(_) | Type::Struct(_)) {
                continue;
            }
            let mut narrowed = false;
            for (pat, _) in arms.iter().filter(|(p, _)| p.explicit_type_predicate) {
                let t = &pat.type_predicate;
                if t.could_match(env, &m)? && !t.contains_with_flags(rigid, env, &m)? {
                    narrowed = true;
                    break;
                }
            }
            if !narrowed || !pool.cut(env, &m)? {
                pool.whole.push(m)
            }
        }
        Ok(pool)
    }

    /// Cut `m` into its parts; false where it stays whole.
    fn cut(&mut self, env: &Env, m: &Type) -> Result<bool> {
        let (shape, positions): (Shape, SmallVec<[&Type; 8]>) = match m {
            Type::Tuple(ts) => (Shape::Tuple(ts.len()), ts.iter().collect()),
            Type::Struct(fs) => (
                Shape::Struct(fs.iter().map(|(n, _, _)| n.clone()).collect()),
                fs.iter().map(|(_, t, _)| t).collect(),
            ),
            _ => return Ok(false),
        };
        let mut at: SmallVec<[SmallVec<[Choice; 4]>; 8]> = SmallVec::new();
        let mut total = 1usize;
        for t in positions {
            match choices(env, t)? {
                Some(cs) if !cs.is_empty() => {
                    total = total.saturating_mul(cs.len());
                    at.push(cs)
                }
                _ => return Ok(false),
            }
        }
        if total > MAX_POOL_PARTS {
            return Ok(false);
        }
        for c in 0..total {
            let pick = |c: usize, f: fn(&Choice) -> &Type| {
                let mut c = c;
                with_positions(
                    m,
                    at.iter().map(move |cs| {
                        let t = f(&cs[c % cs.len()]).clone();
                        c /= cs.len();
                        t
                    }),
                )
            };
            let typ = pick(c, |ch| &ch.typ);
            let held = pick(c, |ch| &ch.held);
            if self.parts.iter().any(|p| p.typ == typ && p.held == held) {
                continue;
            }
            let shape = match &shape {
                Shape::Tuple(n) => Shape::Tuple(*n),
                Shape::Struct(names) => Shape::Struct(names.clone()),
                _ => return Ok(false),
            };
            let group = PoolGroup {
                shape,
                member: typ.clone(),
                arms: Vec::new(),
                done: false,
                matched: None,
            };
            self.parts.push(Part { typ, held, group });
        }
        Ok(true)
    }

    /// Pool the unguarded atom `sp`, whose arm's written type test is
    /// `explicit`, into each part it reaches: one its test holds, part
    /// or its held form, with a head each position admits.
    fn claim(
        &mut self,
        env: &Env,
        sp: &StructPatternNode,
        explicit: Option<&Type>,
    ) -> Result<PartClaim> {
        let mut covers: SmallVec<[Type; 2]> = SmallVec::new();
        let Some(shape) = Shape::of(sp).filter(|_| !self.parts.is_empty()) else {
            return Ok(PartClaim::Covers(covers));
        };
        let rigid = ContainsFlags::RigidCheck.into();
        let mut dead = !self.whole.iter().any(|m| shape.fits(m));
        let mut reached = false;
        for i in 0..self.parts.len() {
            let part = &mut self.parts[i];
            if !shape.fits(&part.typ) {
                continue;
            }
            if let Some(t) = explicit {
                if !t.could_match(env, &part.typ)? {
                    continue;
                }
                if !t.contains_with_flags(rigid, env, &part.held)?
                    && !t.contains_with_flags(rigid, env, &part.typ)?
                {
                    dead = false;
                    continue;
                }
            }
            let leaves = match pooled_leaves(env, sp, &part.typ)? {
                Ok(leaves) => leaves,
                Err(Miss::Never) => continue,
                Err(Miss::Unread) => {
                    dead = false;
                    continue;
                }
            };
            reached = true;
            if part.group.done {
                continue;
            }
            match part.group.add(leaves) {
                Claim::Dead => (),
                Claim::Completes(_) => {
                    dead = false;
                    let held = &self.parts[i].held;
                    if self.parts.iter().all(|p| p.held != *held || p.group.done) {
                        covers.push(held.clone())
                    }
                }
                Claim::Open | Claim::Unpooled => dead = false,
            }
        }
        Ok(match dead && reached {
            true => PartClaim::Dead,
            false => PartClaim::Covers(covers),
        })
    }
}

/// Does `arm` match head `h` at `path`: a leaf there admitting it, or
/// any-value leaf at a path above it.
fn admits(arm: &[Leaf], path: &[u32], h: &Head) -> bool {
    arm.iter().any(|l| match &l.heads {
        None => path.starts_with(&l.path),
        Some(hs) => l.path[..] == *path && hs.contains(h),
    })
}

#[cfg(test)]
thread_local! {
    static MATCHES: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

impl PoolGroup {
    /// Pool an arm's leaves: dead where earlier arms match every
    /// combination it matches, else whether the group now covers its
    /// member.
    fn add(&mut self, leaves: SmallVec<[Leaf; 8]>) -> Claim {
        if self.done {
            return Claim::Open;
        }
        if !self.arms.is_empty() && self.covers(&leaves) {
            return Claim::Dead;
        }
        self.arms.push(leaves);
        self.done = self.covers_all();
        match self.done {
            true => Claim::Completes(self.member.clone()),
            false => Claim::Open,
        }
    }

    /// The positions some arm (or `extra`) tests a head at, with the
    /// heads the member admits there; `None` when one is not a finite
    /// set, or there are too many combinations.
    fn positions<'a>(
        arms: &'a [SmallVec<[Leaf; 8]>],
        extra: Option<&'a [Leaf]>,
    ) -> Option<SmallVec<[(&'a [u32], &'a [Head]); 8]>> {
        let mut out: SmallVec<[(&[u32], &[Head]); 8]> = SmallVec::new();
        let mut total = 1usize;
        let arms = arms.iter().map(|a| &a[..]).chain(extra);
        for l in arms.flat_map(|a| a.iter()).filter(|l| l.heads.is_some()) {
            if out.iter().any(|(p, _)| *p == &l.path[..]) {
                continue;
            }
            let domain = l.domain.as_deref()?;
            total = total.saturating_mul(domain.len());
            out.push((&l.path[..], domain));
        }
        (total <= MAX_POOL_COMBINATIONS).then_some(out)
    }

    /// Every combination of heads `positions` names, as head references.
    fn combinations<'a>(
        positions: &'a [(&'a [u32], &'a [Head])],
    ) -> impl Iterator<Item = SmallVec<[&'a Head; 8]>> + 'a {
        let total = positions.iter().map(|(_, d)| d.len()).product::<usize>();
        (0..total).map(move |mut c| {
            positions
                .iter()
                .map(|(_, domain)| {
                    let h = &domain[c % domain.len()];
                    c /= domain.len();
                    h
                })
                .collect()
        })
    }

    /// Does `arm` match `combo` at every position the combination
    /// reaches: one under a position whose head is not the composite
    /// member is not there. Only a composite head has positions under it
    /// (`nested`).
    fn matches(
        arm: &[Leaf],
        positions: &[(&[u32], &[Head])],
        nested: bool,
        combo: &[&Head],
    ) -> bool {
        #[cfg(test)]
        MATCHES.with(|n| n.set(n.get() + 1));
        positions.iter().zip(combo.iter()).all(|((p, _), h)| {
            admits(arm, p, h)
                || nested
                    && positions.iter().zip(combo.iter()).any(|((q, _), g)| {
                        q.len() < p.len()
                            && p.starts_with(q)
                            && !matches!(g, Head::Composite)
                    })
        })
    }

    fn nested(positions: &[(&[u32], &[Head])]) -> bool {
        positions.iter().any(|(_, d)| d.iter().any(|h| matches!(h, Head::Composite)))
    }

    /// Some arm matches every combination the member admits, the last
    /// arm pushed counted into `matched`.
    fn covers_all(&mut self) -> bool {
        let Some(positions) = Self::positions(&self.arms, None) else {
            self.matched = None;
            return false;
        };
        let nested = Self::nested(&positions);
        let (m, from) = match &mut self.matched {
            Some(m) if m.positions == positions.len() => (m, self.arms.len() - 1),
            slot => {
                let fresh = Matched {
                    positions: positions.len(),
                    count: 0,
                    bits: [0; MAX_POOL_COMBINATIONS / 64],
                };
                (slot.insert(fresh), 0)
            }
        };
        for (i, c) in Self::combinations(&positions).enumerate() {
            if self.arms[from..].iter().any(|a| Self::matches(a, &positions, nested, &c))
            {
                m.set(i)
            }
        }
        m.count == positions.iter().map(|(_, d)| d.len()).product::<usize>()
    }

    /// Earlier arms match every combination `arm` matches.
    fn covers(&self, arm: &[Leaf]) -> bool {
        let Some(positions) = Self::positions(&self.arms, Some(arm)) else {
            return false;
        };
        let nested = Self::nested(&positions);
        let mut combos = Self::combinations(&positions)
            .enumerate()
            .filter(|(_, c)| Self::matches(arm, &positions, nested, c));
        match &self.matched {
            Some(m) if m.positions == positions.len() => combos.all(|(i, _)| m.get(i)),
            _ => combos.all(|(_, c)| {
                self.arms.iter().any(|a| Self::matches(a, &positions, nested, &c))
            }),
        }
    }
}

impl<R: Rt> Update<R> for Select<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.arg.node);
        for (pat, body) in self.arms.iter() {
            if let Some(g) = &pat.guard {
                f(&g.node)
            }
            f(body)
        }
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.arg.node);
        for (pat, body) in self.arms.iter_mut() {
            if let Some(g) = &mut pat.guard {
                f(&mut g.node)
            }
            f(body)
        }
    }

    /// The selection, the consulted mask and the tracker exist only once
    /// a cycle has run.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.selected.is_some() || self.arm_facts.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        put_tag(NodeTag::Select, buf);
        self.arg.image_encode(buf)?;
        encode_varint(self.arms.len() as u64, buf);
        for (p, n) in &self.arms {
            p.image_encode(buf)?;
            n.image_encode(buf)?;
        }
        self.typ.encode(buf)?;
        self.spec.encode(buf)?;
        self.flags.contains(CFlag::RunEvent).encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        if self.arm_facts.is_none() {
            let discriminants = self.discriminants.take().unwrap_or_else(|| {
                Arc::new(ArmDiscriminants::new(
                    self.arg.node.typ().clone(),
                    self.arms.len(),
                ))
            });
            self.arm_facts = Some(LazyArmFacts::build(ctx, &self.arms, discriminants));
        }
        let Self {
            selected,
            arg,
            arms,
            typ: _,
            spec,
            consulted_guard_mask,
            resident,
            slept,
            arm_facts,
            discriminants: _,
            flags,
        } = self;
        let LazyArmFacts {
            tracked,
            sleep_on_deselect,
            entered,
            discriminants,
            logged_no_match,
            guarded,
            missed,
            guards,
            kept,
        } = arm_facts.as_mut().expect("arm facts built above");
        let ArmDiscriminants { shallow, scrutinee } = &**discriminants;
        let woke = slept.take();
        // A reborn select starts with no arm; the arm it held is left as a
        // deselect leaves one unless the chain takes it again.
        let held = match woke && ctx.event.born() {
            false => None,
            true => {
                entered.clear(arms.len());
                missed.clear(arms.len());
                if let Some(runs) = guards.as_mut() {
                    runs.fill(GuardRun::Unborn);
                }
                selected.take()
            }
        };
        // The guarded arms' production tags. Only guards the chain
        // consults contribute fires or bottomness.
        let mut guard_tags: SmallVec<[(usize, Tag); 4]> = SmallVec::new();
        // the missed fires each guard took: its own arm's body, one
        // evaluation with it, sees them too
        let mut guard_took: SmallVec<[(usize, SmallVec<[BindId; 4]>); 2]> =
            SmallVec::new();
        let arg_prod = arg.update(ctx);
        tracked.observe(ctx);
        let awake = *selected;
        tracked.observe_late(ctx, |i, b| match i.checked_sub(arms.len()) {
            None => Some(i) != awake && BoundaryRef::held_by(arm_boundary(&arms[i]), b),
            Some(g) => {
                guards.as_ref().is_some_and(|runs| !matches!(runs[g], GuardRun::Awake))
                    && BoundaryRef::held_by(arms[g].0.guard_boundary(), b)
            }
        });
        // A guard runs where the chain reaches it, an arm whose shape
        // admits the value with no earlier arm claiming it (`open` is the
        // chain still running at arm `i`): the checker narrowed the binds
        // by that shape. Any other guard sleeps, as an unselected arm
        // does, and wakes caught up when the chain next reaches it.
        // An arm's shape test, which the chain below reuses.
        let guards_end = guarded.last().map_or(0, |l| l + 1);
        let mut shaped: SmallVec<[Option<bool>; 16]> = smallvec![None; guards_end];
        let live = arg.value.as_ref().filter(|_| !arg.tag.is_bottom());
        let mut open = live.is_some();
        for i in 0..guards_end {
            let consumer = arms.len() + i;
            let pat = &mut arms[i].0;
            let reach = live.filter(|_| open).filter(|v| {
                let shallow = shallow[i]
                    .get_or_init(|| pat.shallow_discriminant(&ctx.env, scrutinee));
                let s = pat.shape_matches(&ctx.env, shallow.as_ref(), v);
                shaped[i] = Some(s);
                s
            });
            if pat.guard.is_none() {
                open &= reach.is_none();
                continue;
            }
            let tag = if missed.get(i) { arg_prod.fresh() } else { arg_prod };
            match reach {
                Some(_) => missed.unset(i),
                None if arg_prod.triggers() => missed.set(i),
                None => (),
            }
            let runs = guards.as_mut().expect("a guarded arm builds the guard sleep");
            let Some(v) = reach else {
                if runs[i].unreached() {
                    pat.sleep_guard(ctx);
                    tracked.fell_asleep(consumer, ctx.rt.cycle());
                    tracked.refresh(&ctx.env, consumer, |r| guard_refs(pat, r));
                    tracked.observe_late(ctx, |k, b| {
                        k == consumer && BoundaryRef::held_by(pat.guard_boundary(), b)
                    });
                }
                continue;
            };
            let saved = pat.bind_tentative(ctx, v, tag);
            let view = runs[i].reached();
            let caught_up = tracked.deliver(ctx, consumer);
            if !caught_up.is_empty() {
                guard_took.push((i, caught_up.iter().map(|(id, _)| *id).collect()));
            }
            let t = ctx.under(view, |ctx| {
                let scope = tracked.open(ctx, consumer);
                let t = pat.update(ctx);
                tracked.ran(ctx, consumer, scope);
                t
            });
            TrackedFires::restore(ctx.event, caught_up);
            tracked.evaluated(&ctx.env, consumer, |r| guard_refs(pat, r));
            if let Some(t) = t {
                guard_tags.push((i, t));
            }
            pat.retract(ctx, saved);
            open &= matches!(pat.arm_match(true), ArmMatch::GuardFalse);
        }
        // Any guard fire drives a re-match; whether it affects the
        // emission is decided by the consulted set below.
        let pat_up = guard_tags.iter().any(|(_, t)| t.triggers());
        // A bottom scrutinee bottoms the select; it consults no guards.
        // A window with no arm (a bottom scrutinee, an undecidable guard)
        // pauses the selected arm as a deselect does, so it resumes with
        // catch-up; a wake that lands in it waits for the next arm.
        if arg.tag.is_bottom() {
            if woke {
                slept.set();
            }
            if arg_prod.triggers() || woke {
                exit_kept(ctx, kept, arms, None)
            }
            if let Some(j) = selected.take().or(held) {
                deselect(ctx, tracked, kept, j, &mut arms[j], sleep_on_deselect.get(j));
            }
            return resident.set_bottom(arg_prod.triggers());
        }
        let v = arg.value.as_ref().expect("Held keeps every non-bottom production");
        if crate::dbgenv::graphix_dbg_select() {
            eprintln!(
                "SELECT[{}] upd init={} pat_up={pat_up} sel={selected:?} argc={v:?} vars={}",
                spec.pos,
                ctx.event.init(),
                ctx.event.variables.len()
            );
        }
        enum ChainOut {
            Quiet(usize),
            Taken(Option<usize>),
            Undet,
        }
        let chain = match *selected {
            Some(i) if !(arg_prod.triggers() || pat_up || woke) => ChainOut::Quiet(i),
            _ => {
                consulted_guard_mask.clear(arms.len());
                let mut out = ChainOut::Taken(None);
                for (i, ((pat, _), shallow)) in
                    arms.iter().zip(shallow.iter()).enumerate()
                {
                    let s = shaped.get(i).copied().flatten().unwrap_or_else(|| {
                        let shallow = shallow.get_or_init(|| {
                            pat.shallow_discriminant(&ctx.env, scrutinee)
                        });
                        pat.shape_matches(&ctx.env, shallow.as_ref(), v)
                    });
                    match pat.arm_match(s) {
                        ArmMatch::NoStruct => (),
                        ArmMatch::GuardFalse => consulted_guard_mask.set(i),
                        ArmMatch::GuardBottom => {
                            consulted_guard_mask.set(i);
                            out = ChainOut::Undet;
                            break;
                        }
                        ArmMatch::Matched => {
                            if pat.guard.is_some() {
                                consulted_guard_mask.set(i);
                            }
                            out = ChainOut::Taken(Some(i));
                            break;
                        }
                    }
                }
                out
            }
        };
        match chain {
            ChainOut::Quiet(_) => (),
            ChainOut::Taken(i) => exit_kept(ctx, kept, arms, i),
            ChainOut::Undet => exit_kept(ctx, kept, arms, None),
        }
        let planes = EmissionPlanes::new(&guard_tags, arg_prod, consulted_guard_mask);
        // A selection stays undecidable while a consulted guard stands
        // bottom: no arm is evaluated.
        let chain = match chain {
            ChainOut::Quiet(_) if planes.consulted_bottom => ChainOut::Undet,
            chain => chain,
        };
        let tv = match chain {
            ChainOut::Undet => {
                if woke {
                    slept.set();
                }
                if let Some(j) = selected.take().or(held) {
                    deselect(
                        ctx,
                        tracked,
                        kept,
                        j,
                        &mut arms[j],
                        sleep_on_deselect.get(j),
                    );
                }
                return resident.set_bottom(planes.anyfire);
            }
            ChainOut::Quiet(i) => {
                let (t, v) = evaluate_arm(tracked, &mut arms[i], i, &guard_took, ctx);
                planes.emit(t, v)
            }
            ChainOut::Taken(Some(i)) if *selected == Some(i) => {
                if crate::dbgenv::graphix_dbg_select() {
                    eprintln!("SELECT[{}] same-arm i={i} arg={v:?}", spec.pos);
                }
                arms[i].0.bind_event(ctx, v, arg_prod);
                let (t, v) = evaluate_arm(tracked, &mut arms[i], i, &guard_took, ctx);
                planes.emit(t, v)
            }
            ChainOut::Taken(Some(i)) => {
                if crate::dbgenv::graphix_dbg_select() {
                    eprintln!(
                        "SELECT[{}] BECOMING-SELECTED {selected:?} -> {i} init={}",
                        spec.pos,
                        ctx.event.init()
                    );
                }
                if let Some(j) = selected.replace(i).or(held.filter(|j| *j != i)) {
                    deselect(
                        ctx,
                        tracked,
                        kept,
                        j,
                        &mut arms[j],
                        sleep_on_deselect.get(j),
                    );
                }
                // A bind is a facet of its scrutinee's delivery: a stale
                // scrutinee binds stale, at a first consult, a wake or a
                // guard's flip alike.
                let bind_tag = match arg_prod.triggers() {
                    true => arg_prod,
                    false => Tag::STALE,
                };
                arms[i].0.bind_event(ctx, v, bind_tag);
                // An arm is born at its first entry, into a world it reads
                // stale, and resumes at every later one, a pure arm too: its
                // constants stand, as a kernel's do. A pure arm, not slept
                // when deselected since nothing in it runs, is slept now so
                // its nodes recompute from what stands, as a slept arm's do.
                // CR claude for claude: [bug/high] A select inside a woken arm that
                // re-routes by a stale scrutinee answers differently in the two
                // engines. The node-walk enters the new arm as below: a first entry is
                // a Step (its constants fire) and a re-entry catches up the fires no
                // selected reader saw. A fused kernel does neither. On main 3d73e2e1c,
                // node-walk vs fused: w_newarm count 2 vs 1, w_pend2 3 vs 2, w_default
                // 2 vs 1, reent_w `w` 1007 vs stuck at 4, io_ir_x prints "IO 3" vs
                // nothing. The node-walk follows the first-entry and catch-up rules
                // (design/wake_catchup.md), so the kernel is wrong; fusion must lose
                // fusion here, never answer otherwise. D20 (b) (bound-bind-policy-01)
                // fuses every run-time bind and spreads this, so this lands first. The
                // fuzz corpus has no wake that re-routes a select inside the woken arm.
                // probes: ~/ptmp/cr29/d20x/probes/design-B/{w_newarm,w_pend2}.gx,
                // verify-B-engines/w_default.gx, verify-D-events/reent_w.gx,
                // engines-impl/io_ir_x.gx (wake-entry-split-01)
                let view = match entered.replace(i) {
                    true => {
                        if !sleep_on_deselect.get(i) {
                            arms[i].1.sleep(ctx);
                        }
                        match flags.contains(CFlag::RunEvent) {
                            true => View::Step,
                            false => View::Wake,
                        }
                    }
                    false if ctx.event.view == View::Birth => View::Birth,
                    false => View::Step,
                };
                let (t, v) = ctx.under(view, |ctx| {
                    evaluate_arm(tracked, &mut arms[i], i, &guard_took, ctx)
                });
                planes.emit(t, v)
            }
            // No arm matches: the select has no value.
            ChainOut::Taken(None) => {
                // exhaustiveness makes this a checker bug: say so, once
                if !mem::replace(logged_no_match, true) {
                    crate::node::error::report_coverage_hole(&format_args!(
                        "{spec}: no arm matches {v}"
                    ));
                }
                if let Some(j) = selected.take().or(held) {
                    deselect(
                        ctx,
                        tracked,
                        kept,
                        j,
                        &mut arms[j],
                        sleep_on_deselect.get(j),
                    );
                }
                return resident.set_bottom(false);
            }
        };
        resident.set(tv)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        let Self {
            selected: _,
            arg,
            arms,
            typ: _,
            spec: _,
            consulted_guard_mask: _,
            resident: _,
            slept: _,
            arm_facts: _,
            discriminants: _,
            flags: _,
        } = self;
        arg.node.delete(ctx);
        for (pat, arm) in arms {
            arm.delete(ctx);
            pat.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        let Self {
            selected: _,
            arg,
            arms,
            typ: _,
            spec: _,
            consulted_guard_mask: _,
            resident: _,
            slept,
            arm_facts,
            discriminants: _,
            flags: _,
        } = self;
        slept.set();
        if let Some(facts) = arm_facts {
            let cycle = ctx.rt.cycle();
            facts.tracked.all_fell_asleep(cycle);
        }
        arg.sleep(ctx);
        for (pat, body) in arms {
            body.sleep(ctx);
            if let Some(n) = &mut pat.guard {
                n.sleep(ctx)
            }
        }
    }

    fn refs(&self, refs: &mut Refs) {
        let Self {
            selected: _,
            arg,
            arms,
            typ: _,
            spec: _,
            consulted_guard_mask: _,
            resident: _,
            slept: _,
            arm_facts: _,
            discriminants: _,
            flags: _,
        } = self;
        arg.node.refs(refs);
        for arm in arms {
            arm_refs(arm, refs);
            if let Some(n) = &arm.0.guard {
                n.node.refs(refs);
            }
        }
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        let checking = !super::instance_rechecked();
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx), checking)?;
        if self.flags.intersects(CFlag::SeqStep | CFlag::RunEvent) {
            return Ok(());
        }
        for (_, n) in &self.arms {
            warn_birth_only(ctx, self.flags, n)?;
        }
        if let ExprKind::Select(se) = &self.spec.kind {
            for (_, body) in se.arms.iter() {
                warn_triggerless_seq(ctx, self.flags, body)?;
            }
        }
        Ok(())
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        match types.aux(self.spec.id) {
            Some(rows) => self.typecheck0_rows(ctx, types, &rows),
            None => self.typecheck0_with(
                ctx,
                &mut |n, ctx| n.typecheck0_instance(ctx, types),
                false,
            ),
        }
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        self.arg.node.typecheck1(ctx)?;
        for (pat, n) in self.arms.iter_mut() {
            if let Some(guard) = &mut pat.guard {
                guard.node.typecheck1(ctx)?;
            }
            wrap!(n, n.typecheck1(ctx))?;
        }
        Ok(())
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn view(&self) -> NodeView<'_, R> {
        NodeView::Select(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_select_node(cx, self)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        // Reached only when no enclosing region fused this select whole.
        // The scrutinee, each guard and each arm body get their own
        // region passes; constant bodies are skipped.
        crate::fusion::fuse(&mut self.arg.node, ctx)?;
        for (pat, body) in self.arms.iter_mut() {
            if let Some(g) = &mut pat.guard {
                if !matches!(g.node.view(), NodeView::Constant(_)) {
                    crate::fusion::fuse(&mut g.node, ctx)?;
                }
            }
            if !matches!(body.view(), NodeView::Constant(_)) {
                crate::fusion::fuse(body, ctx)?;
            }
        }
        Ok(None)
    }
}

impl<R: Rt> Select<R> {
    /// What the check derived for the arms ([`super::lambda::DefTable`]):
    /// each arm's completed predicate, then the types of its pattern's
    /// binds in [`StructPatternNode::ids`] order.
    pub(crate) fn aux_types(&self, env: &Env) -> Box<[Type]> {
        let mut out = Vec::new();
        for (pat, _) in self.arms.iter() {
            out.push(pat.type_predicate.clone());
            pat.structure_predicate.ids(&mut |id| {
                out.push(env.by_id.get(&id).map_or(Type::Bottom, |b| b.typ.clone()))
            });
        }
        out.into_boxed_slice()
    }

    /// An instance's typecheck from the rows its definition's check
    /// recorded: the arms' predicates and binds are read, not narrowed.
    fn typecheck0_rows(
        &mut self,
        ctx: &mut CompileCtx<R>,
        types: &mut super::lambda::InstanceTypes,
        rows: &[Type],
    ) -> Result<()> {
        wrap!(self.arg.node, self.arg.node.typecheck0_instance(ctx, types))?;
        let mut rows = rows.iter();
        for (pat, n) in self.arms.iter_mut() {
            let tp = rows.next().ok_or_else(|| anyhow!("BUG: a select row short"))?;
            // the predicate's cells are its binds' (an inferred predicate
            // is built from them), bound by position before it is replaced
            pat.type_predicate.take_row(tp);
            match pat.explicit_type_predicate {
                // the type test reads the names it writes at run time, where
                // the body's typedefs are gone: resolve them here
                true => {
                    pat.type_predicate.seed_refs(&ctx.env);
                }
                false => {
                    pat.structure_predicate.realign(&ctx.env, tp).at(n.spec())?;
                    pat.type_predicate = tp.clone();
                }
            }
            let mut ids: SmallVec<[BindId; 4]> = SmallVec::new();
            pat.structure_predicate.ids(&mut |id| ids.push(id));
            for id in ids {
                let row =
                    rows.next().ok_or_else(|| anyhow!("BUG: a select row short"))?;
                if let Some(b) = ctx.env.by_id.get(&id) {
                    b.typ.take_row(row);
                }
            }
            if let Some(guard) = &mut pat.guard {
                wrap!(guard.node, guard.node.typecheck0_instance(ctx, types))?;
            }
            wrap!(n, n.typecheck0_instance(ctx, types))?;
        }
        self.discriminants = types.arm_discriminants(
            self.spec.id,
            self.arg.node.spec().id,
            self.arms.len(),
        );
        let rtypes: LPooled<Vec<&Type>> =
            self.arms.iter().map(|(_, n)| n.typ()).collect();
        self.typ = Type::union_of_alternatives(&ctx.env, &rtypes)?;
        Ok(())
    }

    /// `checking`: the definition's check, which also judges coverage,
    /// dead arms and guards; an instance does only what they decide.
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut super::Child<'_, R>,
        checking: bool,
    ) -> Result<()> {
        wrap!(self.arg.node, child(&mut self.arg.node, ctx))?;
        // the cells the scrutinee holds before the arms decide any
        let mut outer: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        let mut below: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        let lower = match checking {
            true => {
                let scrut = self.arg.node.typ();
                crate::typ::settle::position_cells(scrut, &mut outer);
                let lower = lower_view(scrut);
                if let Some(l) = &lower {
                    crate::typ::settle::position_cells(l, &mut below);
                    below.retain(|addr, _| !outer.contains_key(addr));
                }
                lower
            }
            false => None,
        };
        // the cells the arms' patterns decide, each by the cell it stands for
        let mut decided = Decisions::default();
        let deciding = match &self.spec.kind {
            ExprKind::Select(se) if checking => Deciding::new(
                &outer,
                &below,
                self.arg.node.typ(),
                lower.as_ref(),
                &se.arms,
            ),
            _ => Deciding::default(),
        };
        let mut written = Deciding::default();
        if checking && let ExprKind::Select(se) = &self.spec.kind {
            let scrut = self.arg.node.typ();
            for ((pat, n), (spec_pat, _)) in self.arms.iter().zip(se.arms.iter()) {
                if pat.explicit_type_predicate {
                    written.hold_written(&pat.type_predicate)
                }
                let sp = &spec_pat.structure_predicate;
                let ids = CompiledAbstracts::new(&pat.structure_predicate, sp);
                let ids = |p: &StructurePattern| ids.get(p);
                let site = || pattern_site(spec_pat, n.spec());
                let over = match pat.explicit_type_predicate {
                    true => &pat.type_predicate,
                    false => scrut,
                };
                if let Some(tv) = sp.meets_rigid(&ctx.env, &ids, over)? {
                    let e = rigid_refusal(&ctx.env, &ids, sp, over, &tv, Holder::Arm);
                    return Err(e).at(&site());
                }
                if pat.explicit_type_predicate
                    && scrut.has_rigid()
                    && let Some((tv, part)) =
                        sp.meets_rigid_written(&ctx.env, &ids, scrut, over)?
                {
                    let e = written_refusal(&ctx.env, &ids, sp, scrut, &tv, &part);
                    return Err(e).at(&site());
                }
            }
        }
        // A partial struct pattern infers only its named fields;
        // complete each inferred predicate against the typed scrutinee
        // before coverage or dispatch reads it. One the scrutinee leaves
        // ambiguous is completed again against what reaches its arm.
        let mut deferred: SmallVec<[bool; 16]> = smallvec![false; self.arms.len()];
        if let ExprKind::Select(se) = &self.spec.kind {
            let scrut = self.arg.node.typ().clone();
            for (((pat, _), (spec_pat, _)), d) in
                self.arms.iter_mut().zip(se.arms.iter()).zip(deferred.iter_mut())
            {
                if !pat.explicit_type_predicate {
                    match spec_pat.structure_predicate.complete_type_predicate(
                        &ctx.env,
                        &pat.type_predicate,
                        &scrut,
                    ) {
                        Err(_) => *d = true,
                        // a partial struct, here or nested, over a scrutinee
                        // that may be Any has no struct type to complete it,
                        // so its test would be the exact struct of the fields
                        // it names
                        Ok(None)
                            if spec_pat.structure_predicate.has_partial_struct()
                                && may_be_any(&ctx.env, &scrut)? =>
                        {
                            bailat!(
                                self.spec,
                                "the partial struct pattern {} over {scrut} has no \
                                 struct type to complete it: test the struct's type \
                                 first (`{{a: i64, b: string}} as {{a, ..}}`)",
                                spec_pat.structure_predicate
                            )
                        }
                        Ok(None) => (),
                        Ok(Some(t)) => {
                            pat.structure_predicate.realign(&ctx.env, &t)?;
                            pat.type_predicate = t;
                        }
                    }
                }
            }
        }
        let scrut = self.arg.node.typ().clone();
        let mut reach = Reach::new(&ctx.env, &scrut, &self.arms)?;
        deciding.decided(&mut decided);
        let mut reached: SmallVec<[Type; 16]> = SmallVec::new();
        let spec_pats: &[_] = match &self.spec.kind {
            ExprKind::Select(se) => &se.arms,
            _ => &[],
        };
        for i in 0..self.arms.len() {
            let (pat, n) = &mut self.arms[i];
            if checking && pat.explicit_type_predicate {
                super::defer_written_map_keys(ctx, &pat.type_predicate, &self.spec)
            }
            let deciding = deciding.again();
            // a refusal about the pattern points at the pattern
            let site = match spec_pats.get(i) {
                Some((p, _)) => pattern_site(p, n.spec()),
                None => n.spec().clone(),
            };
            if deferred[i]
                && let Some((spec_pat, _)) = spec_pats.get(i)
            {
                let t = spec_pat
                    .structure_predicate
                    .complete_type_predicate(&ctx.env, &pat.type_predicate, &reach.atype)
                    .map_err(|e| undecided_members(&ctx.env, e, &self.spec.ori))
                    .at(&site)?;
                if let Some(t) = t {
                    pat.structure_predicate.realign(&ctx.env, &t)?;
                    pat.type_predicate = t;
                }
            }
            // The arm's binds alias against what reaches it. The
            // `any_as_tvar` view keeps a `_` slot from short-circuiting the
            // walk.
            let reaching = match !super::instance_rechecked()
                || !reach.passes_nothing(&ctx.env, pat)?
            {
                true => reach.atype.clone(),
                // an arm its definition's check kept that nothing reaches
                // at the instance's types never runs
                false => Type::Bottom,
            };
            // a written test types its binds itself (Type::matching_type)
            let reaching = spec_pats
                .get(i)
                .filter(|_| !pat.explicit_type_predicate)
                .and_then(|(p, _)| {
                    p.structure_predicate.collection_view(&ctx.env, &reaching)
                })
                .unwrap_or(reaching);
            if checking {
                reached.push(reaching.clone());
            }
            let mut members: SmallVec<[Type; 8]> = SmallVec::new();
            union_members(&ctx.env, &reaching, &mut members)?;
            let narrowed = pat.capture_type(&ctx.env, &reaching).at(&site)?;
            reaching.narrow_pattern(&ctx.env, &narrowed)?;
            pat.bind_narrowed(&ctx.env, &narrowed, &reaching, &members, checking)
                .map_err(|e| reach.untyped_binds(&ctx.env, &self.spec.ori, e))
                .at(&site)?;
            deciding.decided(&mut decided);
            // a runtime test can't tell apart two types with one runtime form
            if checking
                && pat.explicit_type_predicate
                && pat.type_predicate.ref_over_any(&ctx.env, &reaching)
            {
                let t = &pat.type_predicate;
                return Err(format_with_flags(PrintFlag::DerefTVars, || {
                    anyhow!(
                        "this pattern can't tell what the reference in {t} refers to \
                         where the scrutinee may hold Any: every reference has one runtime \
                         form"
                    )
                }))
                .at(&site);
            }
            if checking {
                let mut tests: SmallVec<[(Type, Type); 4]> =
                    smallvec![(pat.type_predicate.clone(), reaching.clone())];
                if !pat.explicit_type_predicate {
                    pat.structure_predicate.element_tests(
                        &ctx.env,
                        &pat.type_predicate,
                        &reaching,
                        &mut tests,
                    );
                }
                let spec = Arc::new(site.clone());
                for (test, among) in tests {
                    let Ok(rest) = among.diff(&ctx.env, &test) else { continue };
                    if let Some(why) = test.rep_collision(&ctx.env, &rest) {
                        return Err(format_with_flags(PrintFlag::DerefTVars, || {
                            why.untold("this pattern")
                        }))
                        .at(&site);
                    }
                    let spec = spec.clone();
                    super::defer_judgment(
                        ctx,
                        crate::PendingSettle::Told { test, rest, spec },
                    );
                }
            }
            // The guard typechecks after the narrowing so it sees the
            // arm's binds at their settled type; it must be bool.
            if let Some(guard) = &mut pat.guard {
                wrap!(guard.node, child(&mut guard.node, ctx))?;
                if checking {
                    let bt = Type::Primitive(Typ::Bool.into());
                    wrap!(guard.node, bt.check_contains(&ctx.env, guard.node.typ()))?;
                }
            }
            wrap!(n, child(n, ctx))?;
            let deciding = deciding.again();
            let covered = reach.arm(&ctx.env, &scrut, pat, &site, checking)?;
            deciding.decided(&mut decided);
            // an arm dead for what an earlier arm's pattern decided is refused
            // for that decision
            if let Err(Dead(e)) = covered {
                let open = still_open(&outer);
                self.check_undecided(
                    &ctx.env,
                    &open,
                    &decided,
                    &written,
                    &reached,
                    lower.as_ref(),
                )?;
                return Err(e);
            }
        }
        let rtypes: LPooled<Vec<&Type>> =
            self.arms.iter().map(|(_, n)| n.typ()).collect();
        self.typ = Type::union_of_alternatives(&ctx.env, &rtypes)?;
        drop(rtypes);
        match checking {
            true => {
                let mut undecided = still_open(&outer);
                self.check_undecided(
                    &ctx.env,
                    &undecided,
                    &decided,
                    &written,
                    &reached,
                    lower.as_ref(),
                )?;
                check_repeats(&ctx.env, &self.arms)?;
                let undecided: LPooled<Vec<TVar>> =
                    undecided.drain().map(|(_, c)| c).collect();
                reach.exhausted(&ctx.env, &scrut, &undecided).at(&self.spec)
            }
            false => Ok(()),
        }
    }

    /// Refuse an arm whose pattern tests a part whose type is not decided
    /// before the arms, a later decision could make the test match what its
    /// coverage and its binds assumed it would not: one of `undecided`, the
    /// cells the scrutinee held before the arms ran (never one the arms' own
    /// patterns brought) still open, or of `decided`, those the arms'
    /// patterns decided, unless the arm's pattern there only takes apart
    /// every value of the shape decided, in the part of the scrutinee that
    /// reaches the arm (`reached`, one per arm judged: every arm, or up to
    /// the one the coverage found dead), or any cell of a written test open
    /// before the arms (`written`), its own variable, still open or decided
    /// since, as one of `decided` is met. A cell decided through the
    /// scrutinee's lower bounds is met in `lower`, the scrutinee as those
    /// bounds held it ([`lower_view`]).
    fn check_undecided(
        &self,
        env: &Env,
        undecided: &AHashMap<usize, TVar>,
        decided: &Decisions,
        written: &Deciding,
        reached: &[Type],
        lower: Option<&Type>,
    ) -> Result<()> {
        let ExprKind::Select(se) = &self.spec.kind else { return Ok(()) };
        let open = |tv: &TVar| undecided.contains_key(&tv.cell_addr());
        let was_open = |tv: &TVar| decided.cells.contains_key(&tv.stands_for());
        let held = |tv: &TVar| decided.held.get(&tv.stands_for()).cloned();
        let was = Decided { cell: &was_open, held: &held };
        let mut own = Decisions::default();
        written.decided(&mut own);
        let own_open = |tv: &TVar| own.cells.contains_key(&tv.stands_for());
        let own_was = Decided { cell: &own_open, held: &|_| None };
        for (((pat, n), (spec_pat, _)), reaching) in
            self.arms.iter().zip(se.arms.iter()).zip(reached.iter())
        {
            let sp = &spec_pat.structure_predicate;
            let ids = CompiledAbstracts::new(&pat.structure_predicate, sp);
            let ids = |p: &StructurePattern| ids.get(p);
            let e = match pat.explicit_type_predicate {
                true => {
                    let t = &pat.type_predicate;
                    let own_met = match sp.meets_undecided(env, &ids, t, &|_| true)? {
                        Some(tv) => Some(tv),
                        None if own.cells.is_empty() => None,
                        None => sp.meets_decided(env, &ids, t, &own_was)?,
                    };
                    match own_met {
                        Some(tv) => {
                            let w = spec_pat.type_predicate.as_ref().unwrap_or(t);
                            Some(anyhow!(
                                "{sp} tests a value of '{}, a type variable its type test \
                                 {w} leaves open: write the type it tests",
                                tv.name
                            ))
                        }
                        None => {
                            match written_tests_undecided(env, t, reaching, undecided)? {
                                Some(tv) => Some(tv),
                                None if decided.cells.is_empty() => None,
                                None => met_in(reaching, lower, |over| {
                                    written_tests_decided(env, t, over, decided)
                                })?,
                            }
                            .map(|tv| {
                                let pat = format_compact!("{t} as {sp}");
                                undecided_refusal(env, &pat, &tv, &self.spec)
                            })
                        }
                    }
                }
                false => match sp.meets_undecided(env, &ids, reaching, &open)? {
                    Some(tv) => Some(tv),
                    None if decided.cells.is_empty() => None,
                    None => met_in(reaching, lower, |over| {
                        sp.meets_decided(env, &ids, over, &was)
                    })?,
                }
                .map(|tv| undecided_refusal(env, sp, &tv, &self.spec)),
            };
            if let Some(e) = e {
                return Err(e).at(&pattern_site(spec_pat, n.spec()));
            }
        }
        Ok(())
    }
}

/// The scrutinee's cells open before a step of a select's check that may
/// decide them, the arms' settle or an arm's pattern, and the bound cells
/// whose bindings hold open cells, each with its binding: deciding a cell
/// may rewrite a binding that held it (`['x, `J]` to `` `J ``), so a holder
/// whose binding the step changed was decided too. A whole scrutinee open
/// before the arms is theirs to decide together, unless an arm holds a
/// partial struct pattern, whose completion has no one answer.
#[derive(Default)]
struct Deciding(SmallVec<[(TVar, Option<Type>); 4]>);

impl Deciding {
    /// `below`: the cells the scrutinee holds only through lower bounds,
    /// `lower` the scrutinee as those bounds hold it ([`lower_view`]), whole
    /// there as the scrutinee is.
    fn new(
        outer: &AHashMap<usize, TVar>,
        below: &AHashMap<usize, TVar>,
        scrut: &Type,
        lower: Option<&Type>,
        arms: &[(Pattern, Expr)],
    ) -> Self {
        let partial = arms.iter().any(|(p, _)| {
            p.type_predicate.is_none() && p.structure_predicate.has_partial_struct()
        });
        let whole = |t: &Type| match t {
            Type::TVar(tv) if !partial => tv.open_cell().map(|c| c.cell_addr()),
            _ => None,
        };
        let wholes = [whole(scrut), lower.and_then(whole)];
        Self(
            outer
                .values()
                .chain(below.values())
                .filter_map(|tv| match tv.open_cell() {
                    Some(c) => (!c.is_rigid() && !wholes.contains(&Some(c.cell_addr())))
                        .then(|| (tv.clone(), None)),
                    None => Self::holding(tv),
                })
                .collect(),
        )
    }

    /// Add the open cells of `t`, a written test: its own variables, which
    /// nothing declares.
    fn hold_written(&mut self, t: &Type) {
        let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        crate::typ::settle::position_cells(t, &mut cells);
        self.0.extend(cells.drain().filter_map(|(_, tv)| {
            let c = tv.open_cell()?;
            (!c.is_rigid()).then(|| (tv, None))
        }))
    }

    fn holding(tv: &TVar) -> Option<(TVar, Option<Type>)> {
        let b = tv.binding()?;
        b.has_free().then(|| (tv.clone(), Some(b)))
    }

    /// The cells still open, and the holders still holding one, for the
    /// next step.
    fn again(&self) -> Self {
        Self(
            self.0
                .iter()
                .filter_map(|(tv, held)| match held {
                    None => tv.open_cell().is_some().then(|| (tv.clone(), None)),
                    Some(_) => Self::holding(tv),
                })
                .collect(),
        )
    }

    /// Add to `decided` each cell the step decided, and each binding it
    /// rewrote as it was.
    fn decided(&self, decided: &mut Decisions) {
        for (tv, held) in self.0.iter() {
            match held {
                None if tv.open_cell().is_none() => {
                    decided.cells.insert(tv.stands_for(), tv.clone());
                }
                Some(b) if tv.binding().as_ref() != Some(b) => {
                    decided.held.entry(tv.stands_for()).or_insert_with(|| b.clone());
                }
                None | Some(_) => (),
            }
        }
    }
}

/// `t` with each open cell that has lower bounds read as their union: the
/// parts an arms' settle that decides such a cell decides through its
/// bounds. `None` where `t` reaches no such cell.
fn lower_view(t: &Type) -> Option<Type> {
    /// Each shared node's view, by its key: `None` while it is walked.
    type Memo = AHashMap<NodeKey, Option<Type>>;
    fn go(t: &Type, memo: &mut Memo) -> Option<Type> {
        let id = t.node_key();
        if let Some(v) = id.and_then(|id| memo.get(&id)) {
            return v.clone();
        }
        if let Some(id) = id {
            memo.insert(id, None);
        }
        let v = crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.open_cell() {
                None => go(&tv.binding()?, memo),
                Some(c) => {
                    let lower = c.cell_lower();
                    if lower.is_empty() {
                        return None;
                    }
                    let mut ts: SmallVec<[Type; 1]> =
                        lower.into_iter().map(|l| go(&l, memo).unwrap_or(l)).collect();
                    Some(match ts.len() {
                        1 => ts.pop()?,
                        _ => Type::Set(Arc::from_iter(ts)),
                    })
                }
            },
            t => t.cow_children(&mut |c| go(c, memo)),
        });
        if let Some(id) = id {
            memo.insert(id, v.clone());
        }
        v
    }
    go(t, &mut LPooled::take())
}

// CR claude for claude: [bug/low] A cell the scrutinee holds only through a lower
// bound that the arms leave open is not judged: `below` feeds only Deciding, and
// still_open reads `outer` alone, so meets_undecided never sees it. `|x| { let r =
// &mut [x, 0]; select *r { [`J, _] => 1, _ => 3 } }` runs ([3, 1] for `M and `J),
// though its plain twin `|x| select [x, 0] { [`J, _] => 1, _ => 3 }` is refused
// "[`J, _] tests a value of 'x". Same on base-r19. Judging it needs the D5 follow-up
// 2 exemption for irrefutable destructures to hold for open below-bound cells (so
// `select *r { ((a, b), _) => a }` over `&mut (x, 0)` still checks), and the whole
// exemption kept for a cell the bounds hold whole (`select W(x).0 { `J => 1, `M =>
// 3 }`). probe: ~/ptmp/cr29/scratch/r19/19a/w6.gx (undecided-lower-bound-03)
/// What `meets` finds over `reaching`, else over `lower`.
fn met_in(
    reaching: &Type,
    lower: Option<&Type>,
    meets: impl Fn(&Type) -> Result<Option<TVar>>,
) -> Result<Option<TVar>> {
    match (meets(reaching)?, lower) {
        (Some(tv), _) => Ok(Some(tv)),
        (None, Some(l)) => meets(l),
        (None, None) => Ok(None),
    }
}

/// The open cells, none rigid, of the scrutinee's cells `outer`, each by
/// its address.
fn still_open(outer: &AHashMap<usize, TVar>) -> LPooled<AHashMap<usize, TVar>> {
    let mut open: LPooled<AHashMap<usize, TVar>> = LPooled::take();
    for tv in outer.values() {
        if let Some(c) = tv.open_cell().filter(|c| !c.is_rigid()) {
            open.insert(c.cell_addr(), c);
        }
    }
    open
}

/// What a select's arms decided: the cells open before them, and the
/// bindings their decisions rewrote, as they were, each by the cell it
/// stands for.
#[derive(Default)]
struct Decisions {
    cells: AHashMap<usize, TVar>,
    held: AHashMap<usize, Type>,
}

// CR claude for claude: [bug/low] A member's undecided cell that no binding names is
// printed raw. With `let rx = &never(); let ry = &never()`, `{a, ..}` over `{a: [1],
// c: *rx}` and `{a: ["s"], c: *ry}` is refused "the pattern { a, .. } matches two
// members, { a: Array<i64>, c: '_8664: unbound } and { a: Array<string>, c: '_8666:
// unbound }, told apart by types not decided at this select; ...": `written` finds
// no name for the cell a read through a read-only reference to an unnamed expression
// has, so undecided_members returns the UndecidedMembers error as it came, whose
// Display prints the cells. Binding the read (`let x = *rx`), naming the referent
// (`&n`) or a writable reference each gets a name. The refusal should name the
// reference read there too, or write the members with no name for such a cell.
// Same on base-r20. probe: ~/ptmp/cr29/scratch/r20/20e/ulb02/d1.gx
// (undecided-member-raw-cell-01)
/// An [`UndecidedMembers`] refusal told with the bindings whose types
/// the undecided cells are.
fn undecided_members(env: &Env, e: anyhow::Error, at: &Origin) -> anyhow::Error {
    let Some(u) = e.downcast_ref::<UndecidedMembers>() else { return e };
    let mut holders = Holders::new(env, None);
    let mut written = |t: &Type| {
        t.written_form(&mut |tv| match tv.is_bound() {
            true => None,
            false => holders.name_cell(tv).or_else(|| tv.written_open()),
        })
    };
    let (Some(a), Some(b)) = (written(&u.members[0]), written(&u.members[1])) else {
        return e;
    };
    let pat = &u.pattern;
    let meanings = holders.meanings(at);
    anyhow!(
        "the pattern {pat} matches two members, {a} and {b}, told apart by types not \
         decided at this select; annotate those types, or the member you mean, e.g. `T \
         as {pat}`, or name every field{meanings}"
    )
}

/// An undecided cell the written test `t` tests in a member of `reaching`:
/// the member passes the test whatever its cells become, but not with its
/// undecided cells held together, each one type of its own. The cell named
/// is the first whose hold alone fails, else the first held.
fn written_tests_undecided(
    env: &Env,
    t: &Type,
    reaching: &Type,
    undecided: &AHashMap<usize, TVar>,
) -> Result<Option<TVar>> {
    if undecided.is_empty() {
        return Ok(None);
    }
    let mut members: SmallVec<[Type; 8]> = SmallVec::new();
    union_members(env, reaching, &mut members)?;
    for m in members.iter() {
        let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        crate::typ::settle::position_cells(m, &mut cells);
        let mut held: SmallVec<[TVar; 2]> = SmallVec::new();
        for tv in cells.values().filter_map(|tv| tv.open_cell()) {
            let addr = tv.cell_addr();
            if undecided.contains_key(&addr)
                && !held.iter().any(|h| h.cell_addr() == addr)
            {
                held.push(tv)
            }
        }
        if held.is_empty() || !t.contains_holding(env, m, &[])? {
            continue;
        }
        if t.contains_holding(env, m, &held)? {
            continue;
        }
        held.sort_by(|a, b| a.name.cmp(&b.name));
        for tv in held.iter() {
            if !t.contains_holding(env, m, std::slice::from_ref(tv))? {
                return Ok(Some(tv.clone()));
            }
        }
        return Ok(held.into_iter().next());
    }
    Ok(None)
}

/// [`written_tests_undecided`] over `reaching` with each of `decided`, cells
/// the arms' patterns decided, open again on a copy: the cell of `decided`
/// the written test `t` tests.
fn written_tests_decided(
    env: &Env,
    t: &Type,
    reaching: &Type,
    decided: &Decisions,
) -> Result<Option<TVar>> {
    fn reopen(
        t: &Type,
        decided: &Decisions,
        fresh: &mut AHashMap<usize, (TVar, TVar)>,
    ) -> Option<Type> {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => match decided.cells.get(&tv.stands_for()) {
                Some(d) => {
                    let (c, _) = fresh.entry(tv.stands_for()).or_insert_with(|| {
                        (TVar::empty_named(d.name.clone()), d.clone())
                    });
                    Some(Type::TVar(c.clone()))
                }
                None => {
                    let b = decided
                        .held
                        .get(&tv.stands_for())
                        .cloned()
                        .or_else(|| tv.binding())?;
                    Some(reopen(&b, decided, fresh).unwrap_or(b))
                }
            },
            t => t.cow_children(&mut |c| reopen(c, decided, fresh)),
        })
    }
    let mut fresh: LPooled<AHashMap<usize, (TVar, TVar)>> = LPooled::take();
    let Some(copy) = reopen(reaching, decided, &mut fresh) else { return Ok(None) };
    let open: AHashMap<usize, TVar> =
        fresh.values().map(|(c, _)| (c.cell_addr(), c.clone())).collect();
    Ok(written_tests_undecided(env, t, &copy, &open)?.and_then(|c| {
        fresh.values().find(|(f, _)| f.same_cell(&c)).map(|(_, d)| d.clone())
    }))
}

// XCR claude for claude: [bug/low] A reference's referent cell is named after the
// expression that made it, so the refusal tells the user to write a type for a name
// no one can write: `|x| { let r = &mut (x, 0); select (*r, 1) { ((`J, _), _) => 1,
// ((`M, _), _) => 3 } }` is refused "((`J, _), _) tests a value of '&mut (x, 0) ...:
// annotate it, writing for '&mut (x, 0) the type of `&mut (x, 0)`". Annotating `x`
// (or `r`) is the fix the message should name; `lower_view` meets `x` there, after
// the arm's reaching type met the referent. Same on base-r19. probe:
// ~/ptmp/cr29/scratch/r19/19a/w2.gx (undecided-lower-bound-02)
// 2026-11-10 claude: A reference to an expression no binding names now marks its
// referent's binding env::Binder::Referent, and Holders::nameable (Holders::new,
// name_part) skips it as it skips a lowering's, so the refusal names a binding that
// holds the reference: w2 is refused "... tests a value whose type is a part of 'r
// ...: annotate it, writing for 'r the type of `r`". Annotating x does not fix w2
// (the referent is a cell of its own: |x: [`J, `M]| is still refused), annotating r
// does, so the hint names r, not x. Pins in lang::select_undecided fail without it
// (12 runs): tag_deciding_a_referent_names_the_reference_refused,
// tag_deciding_a_referent_of_an_annotated_part_names_the_reference_refused,
// tag_deciding_a_referent_through_a_call_names_the_reference_refused; twin
// tag_over_an_annotated_reference; skill select.md cases. The other two hints over
// such a cell are referent-holder-name-01.
/// The refusal of `pat`, which tests a value of `tv`, a cell not decided
/// at its select, naming the binding whose type it is or holds it.
fn undecided_refusal(
    env: &Env,
    pat: &dyn std::fmt::Display,
    tv: &TVar,
    select: &Expr,
) -> anyhow::Error {
    let at = &*select.ori;
    let mut holders = Holders::new(env, None);
    match holders.name_cell(tv) {
        Some(name) => {
            let meanings = holders.meanings(at);
            anyhow!(
                "{pat} tests a value of {name}, a type not decided at this select, which \
                 a later use could decide apart from what the arms test: annotate it\
                 {meanings}"
            )
        }
        None => match holders.name_part(env, tv, select) {
            Some(name) => {
                let meanings = holders.meanings(at);
                anyhow!(
                    "{pat} tests a value whose type is a part of {name}, a type not \
                     decided at this select, which a later use could decide apart from \
                     what the arms test: annotate it{meanings}"
                )
            }
            None => anyhow!(
                "{pat} tests a value whose type is not decided at this select, which a \
                 later use could decide apart from what the arms test: annotate the \
                 scrutinee's type where it is built"
            ),
        },
    }
}

#[cfg(test)]
mod test {
    use super::{Claim, Head, Leaf, MATCHES, PoolGroup, Shape};
    use crate::typ::Type;
    use arcstr::ArcStr;
    use enumflags2::BitFlags;
    use smallvec::{SmallVec, smallvec};

    fn group() -> PoolGroup {
        PoolGroup {
            shape: Shape::Tuple(2),
            member: Type::Primitive(BitFlags::empty()),
            arms: Vec::new(),
            done: false,
            matched: None,
        }
    }

    /// `(`Ki, _)` over a tuple whose first position holds the tags `tags`.
    fn arm(tags: &SmallVec<[Head; 4]>, i: usize) -> SmallVec<[Leaf; 8]> {
        smallvec![
            Leaf {
                path: smallvec![0],
                heads: Some(smallvec![tags[i].clone()]),
                domain: Some(tags.clone()),
            },
            Leaf { path: smallvec![1], heads: None, domain: None },
        ]
    }

    // n tag arms over n tags check n combinations per arm, a dead copy
    // of it included: the group keeps what its earlier arms matched (n^3 / 6
    // matches when every arm walks the combinations again).
    #[test]
    fn pool_counts_each_arm_once() {
        let n = 300;
        let tags: SmallVec<[Head; 4]> =
            (0..n).map(|i| Head::Tag(ArcStr::from(format!("K{i}")), 0)).collect();
        let mut g = group();
        MATCHES.with(|m| m.set(0));
        for i in 0..n - 1 {
            assert_eq!(g.add(arm(&tags, i)), Claim::Open, "arm {i}");
            assert_eq!(g.add(arm(&tags, i)), Claim::Dead, "arm {i} again");
        }
        assert_eq!(g.add(arm(&tags, n - 1)), Claim::Completes(g.member.clone()));
        let steps = MATCHES.with(|m| m.get());
        assert!(steps <= 3 * n * n, "{steps} matches for {n} arms");
    }
}
