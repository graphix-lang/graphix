use super::{
    Held, Holders, WakeBit,
    compiler::compile,
    pattern::{
        ArmMatch, CompiledAbstracts, Holder, PatternNode, SliceKind, StructPatternNode,
        Tested, rigid_refusal, undecided_members, written_named, written_refusal,
    },
    wake::{self, ArmMask, TrackedFires},
};
use crate::{
    BindId, CFlag, CompileCtx, ExecCtx, Node, NodeView, PrintFlag, Refs, Rt, Scope, Tag,
    TagValue, Update, View, bailat,
    env::Env,
    expr::{
        At, Decided, Expr, ExprId, ExprKind, Origin, Pattern, StructurePattern,
        union_members, union_members_open, union_members_with,
    },
    format_with_flags,
    fusion::emit::{BodyCx, CompiledExpr, emit_select_node},
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, put_tag},
    },
    prune::BoundaryRef,
    typ::{
        AbstractId, Brief, ContainsFlags, InfiniteType, NodeKey, Open, ShallowScrutinee,
        TVar, Type, TypeMismatch,
    },
    wrap,
};
use ahash::AHashMap;
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, encode_varint};
use netidx_value::{Typ, Value};
use nohash::IntSet;
use poolshark::local::LPooled;
use smallvec::{SmallVec, smallvec};
use std::{borrow::Cow, iter, mem, sync::OnceLock};
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
    /// The seq machine's tracker, for a select of its world
    /// ([`seq_world`]); a body's select holds none.
    tracked: Option<TrackedFires>,
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
    /// Per arm: its guard ran at the select's last run. One whose shape
    /// did not admit the value is paused and recomputes from what stands
    /// when it next runs.
    guard_ran: ArmMask,
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

/// Whether a select is the seq machine's world, which stays as built: a
/// run event's select, and one written in a seq step (not in a lambda
/// there), whose arms are born at their first entry in each run and
/// wake caught up.
fn seq_world(flags: BitFlags<CFlag>) -> bool {
    flags.intersects(CFlag::RunEvent | CFlag::SeqStep)
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
        run_event: bool,
    ) -> Self {
        let has_guards = arms.iter().any(|(pat, _)| pat.guard.is_some());
        let consumers = arms.len() * (1 + usize::from(has_guards));
        let tracked = run_event.then(|| {
            TrackedFires::new(&ctx.env, ctx.rt.cycle(), consumers, |i, r| {
                match arms.get(i) {
                    Some(arm) => arm_refs(arm, r),
                    None => guard_refs(&arms[i - arms.len()].0, r),
                }
            })
        });
        LazyArmFacts {
            tracked,
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
            guard_ran: ArmMask::new(arms.len()),
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
    /// A consumed input moved at a wake ([`Tag::moved`]).
    moved: bool,
}

impl EmissionPlanes {
    /// `arg_prod` is a non-bottom scrutinee production.
    fn new(guard_tags: &[(usize, Tag)], arg_prod: Tag, mask: &ArmMask) -> Self {
        let mut planes = EmissionPlanes {
            guard_fire: false,
            sound: arg_prod.triggers(),
            anyfire: arg_prod.triggers(),
            consulted_bottom: false,
            moved: arg_prod.moved(),
        };
        let consulted = guard_tags.iter().filter(|(i, _)| mask.get(*i));
        for t in consulted.map(|(_, t)| *t) {
            if t.triggers() {
                planes.guard_fire = true;
                planes.anyfire = true;
                planes.sound |= !t.is_bottom();
            }
            planes.consulted_bottom |= t.is_bottom();
            planes.moved |= t.moved();
        }
        planes
    }

    /// The taken arm's production `(t, v)` as the select's: a value
    /// fires when the arm or a sound consumed input fired; a bottom arm
    /// sets the select bottom, fresh on the same condition; a quiet one
    /// moved when the arm or a consumed input did.
    fn emit(&self, t: Tag, v: Option<Value>) -> TagValue {
        let moved = t.moved() || self.moved;
        match v {
            Some(v) if t.is_fired() || self.sound => TagValue::fired(v),
            Some(v) => TagValue::tagged(v, Tag::STALE.moved_if(moved)),
            None if t.triggers() || self.sound => {
                TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM)
            }
            None => TagValue::tagged(Value::Null, Tag::STALE_BOTTOM.moved_if(moved)),
        }
    }
}

/// Evaluate the taken arm. Only a select of the seq machine's world
/// delivers anything to it.
fn evaluate_arm<R: Rt>(
    tracked: &mut Option<TrackedFires>,
    arm: &mut (PatternNode<R>, Node<R>),
    i: usize,
    ctx: &mut ExecCtx<'_, R>,
) -> (Tag, Option<Value>) {
    let produce = |ctx: &mut ExecCtx<'_, R>, body: &mut Node<R>| {
        let tv = body.update(ctx);
        let t = tv.tag();
        (t, if t.is_bottom() { None } else { Some(tv.value_cloned()) })
    };
    let Some(tracked) = tracked else {
        return wake::in_body(ctx, |ctx| produce(ctx, &mut arm.1));
    };
    let injected = tracked.deliver(ctx, i);
    let scope = tracked.open(ctx, i);
    let r = produce(ctx, &mut arm.1);
    let bound = tracked.ran(ctx, i, scope);
    TrackedFires::restore(ctx.event, injected);
    tracked.evaluated(ctx, i, bound, |r| arm_refs(arm, r));
    r
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
        let mut flags = BitFlags::empty();
        if bool::decode(buf)? {
            flags |= CFlag::RunEvent
        }
        if bool::decode(buf)? {
            flags |= CFlag::SeqStep
        }
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
/// variable read). Constants fire only in a birth, so these act only where
/// the arm is taken in its root's first cycle
/// (`design/bind_driven_bodies.md`). Nested lambdas, selects and seq
/// machines are left to their own checks.
fn warn_constant_action<R: Rt>(
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
                "{what} reads no variable, so it acts only if this arm is taken in the \
                 program's first cycle, which an arm over an async value never is: \
                 constants in an arm do not fire. Sample the event that selects the \
                 arm (`k ~ v`)"
            ),
        )
    };
    crate::stack::ensure_sufficient(|| match n.view() {
        NodeView::Lambda(_) | NodeView::Select(_) | NodeView::SeqMachine(_) => Ok(()),
        // a handler's action runs on its errors, not at an entry
        NodeView::Catch(c) => warn_constant_action(ctx, flags, &c.handler),
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
                    r = warn_constant_action(ctx, flags, c)
                }
            });
            r
        }
    })
}

/// Whether the call acts: an async or Io builtin given an argument (one
/// given none has a value that stands), or a function whose body performs
/// Io, raises or writes a binding it does not own.
fn acts_on_the_world<R: Rt>(
    ctx: &CompileCtx<R>,
    cs: &super::callsite::CallSite<R>,
) -> bool {
    if let Some(b) = super::callsite::static_builtin(ctx, cs) {
        return cs.arg_nodes().next().is_some()
            && matches!(
                ctx.builtin_effect(b),
                crate::Effect::Async | crate::Effect::Io(_)
            );
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
/// names no variable: it runs only where the arm is taken in its root's
/// first cycle.
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
                    "{what}, so it runs only if this arm is taken in the program's \
                     first cycle: constants in an arm do not fire. Trigger it on the \
                     event that selects the arm"
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

// CR claude for claude: [bug/low] An arm whose every value an earlier unguarded arm
// takes is not found dead where the earlier arm has a name or `_` at a position the
// later one tests by a literal the pools do not read (a number, a string): `|x: (i64,
// i64)| select x { (_, 1) => 0, (3, 1) => 1, _ => 2 }` checks and the second arm never
// runs, and so do `(_, true)` before `(3, true)`, `[_, 1]` before `[3, 1]`, `` `A(_, 1)
// `` before `` `A(3, 1) ``, `{b: _, c: true}` before `{b: "s", c: true}` and its written
// form (`E as {b: _, c: true}` before `E as {b: "s", c: true}`). `(_, true)` before
// `(true, true)` is refused (the pool reads both), and so is the same test written
// twice (check_repeats). base-r24 alike. What would likely fix it: same_test taken
// one way, an earlier name or `_` standing for any test the later atom makes at its
// position; census both repos. probes: ~/ptmp/cr29/scratch/r24/24a/wsaf/p6.txt, p5.txt
// lines 1 to 3 (run.sh beside them runs each line: check, fused, node-walk)
// (literal-arm-subsumed-dead-01)
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
    /// One of the arms is such an arm: no value goes uncovered.
    catch_all: bool,
    guarded_slice: bool,
    refutable_slice: bool,
    /// What was built into a scrutinee cell the arms decided.
    built: Option<Built>,
    /// The refusal of a part built into that cell that two of the arms'
    /// tests could take ([`built_standing`]), for a select no arm of which
    /// is refused for testing it.
    apart: Option<anyhow::Error>,
}

/// The union of the lower bounds of a scrutinee cell the arms decide, as
/// written before the arms bound its cells: each cell open then is a
/// placeholder named by the cell's place in `cells`. `None` where it has
/// no written form.
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
    /// inside the scrutinee took, is the arm's own cell again. A part
    /// built into an open cell that one of the arms' tests holds whatever
    /// its cells become keeps them open ([`built_standing`]), and one
    /// two tests could take that neither holds so is refused, for a
    /// definition's check (`checking`, where it is).
    fn new<R: Rt>(
        env: &Env,
        scrut: &Type,
        arms: &[(PatternNode<R>, Node<R>)],
        checking: Option<&Origin>,
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
        let (mut standing, mut apart) = (OpenCells::new(), None);
        if informative && !(wildcard && union_scrut) && scrut.has_unbound() {
            if let Type::TVar(tv) = scrut
                && let Some(cell) = tv.open_cell()
            {
                let lower = cell.cell_lower();
                (standing, apart) = built_standing(env, &itype, &lower, checking)?;
                if !lower.is_empty() {
                    let mut cells: SmallVec<[(ArcStr, TVar); 2]> = SmallVec::new();
                    let typ = Type::Set(Arc::from_iter(lower)).written_form(&mut |tv| {
                        if tv.is_bound() || !tv.cell_lower().is_empty() {
                            return None;
                        }
                        let name = match cells.iter().find(|(_, c)| c.same_cell(tv)) {
                            Some((n, _)) => n.clone(),
                            None => {
                                let n: ArcStr =
                                    format_compact!("{}", cells.len()).as_str().into();
                                cells.push((n.clone(), tv.clone()));
                                n
                            }
                        };
                        Some(Type::TVar(TVar::empty_named(name)))
                    });
                    built = Some(Built { typ, cells });
                }
            }
            // CR claude for claude: [bug/low] A slice in each alternative of an or-pattern
            // over an unannotated parameter is refused at the call, where the same slices
            // in arms of their own and the written parameter run: `let f = |x| select x {
            // `A([h, ..]) | `B([h, ..]) => h, _ => 0 }; f(`A([1, 2]))` is refused, "i64
            // could go to '_N or '_N of the union ['_N, '_N], which nothing has decided",
            // and with `` `A([]) => 0, `B([]) => 0 `` for the wildcard "`A(Array<i64>)
            // could go to any of 2 members of the union [`A(Array<['_a, '_b]>),
            // `B(Array<['_a, '_b]>)]". `` `A([h, ..]) => h, `B([h, ..]) => h, `A([]) => 0,
            // `B([]) => 0 `` prints (1, 0), and so does the or-arm under `|x: [`A(Array<i64>),
            // `B(Array<i64>)]|`. base-r23 and lane 23d alike. Not diagnosed: the printed
            // union keeps an alternative's element cell and its rest's cell apart, which a
            // slice arm of its own does not. probes: ~/ptmp/cr29/scratch/r23/23d/vsc/
            // {q01,q03,q04,q05}.gx (slice-or-arm-cells-01)
            // 2026-11-12 claude: slice-tuple-bind-cells-01's fix (Type::narrow_per_member: an
            // open cell among the other parts of a cell that is a member's own part is merged
            // with it) makes q01.gx and q05.gx print (1, 0), as q04.gx does; pin
            // lang::types::slice_in_each_alternative_of_an_or_arm_over_an_unannotated_parameter
            // fails in every mode with that fix undone. Not looked at beyond the four probes.
            // 2026-11-13 claude: Stale on base-r24 (main be2c16a05): q01, q03, q04 and q05
            // print (1, 0) there and on the lane, fused, --no-fusion, GRAPHIX_PAR=force,
            // GRAPHIX_NO_SUBST=1, under GRAPHIX_ELAB_AUDIT and from a cached image. Closed by
            // 97d653516 (slice-tuple-bind-cells-01): with the merge in
            // Type::narrow_per_member_in undone (`own` forced false) q01 and q05 are refused
            // again by the two messages quoted above. Looked one step over, 27 probes, base-r24
            // and the lane alike: a rest bound in each alternative, three alternatives, slices
            // of different lengths, a slice element against a whole payload, a second part
            // beside the slice, a struct field, the or inside a slice, nested slices,
            // literal-tagged tuples, one instance (a function a call returns) and a scrutinee a
            // later `<-` writes all run as their written twins; one instance called at two
            // element types and `h + 1` then a string written are refused. Pins beside the
            // first,
            // lang::types::slices_of_other_shapes_in_the_alternatives_of_an_or_arm_over_an_unannotated_parameter,
            // slice_in_each_alternative_of_an_or_arm_binds_one_cell_in_one_instance,
            // .._as_two_types_in_one_instance_refused, .._over_a_binding_written_later (20 of
            // the 24 runs of the five fail with the merge undone;
            // .._used_as_a_number_then_written_a_string_refused holds either way). probes:
            // ~/ptmp/cr29/scratch/r24/24b/soa/
            // 2026-11-13 claude: Review: the stale claim holds (the four probes on base-r24 and the lane; q01 and
            // q05 refused again and 20 of the 24 pin runs failing with the merge undone). What is left of this
            // shape is another defect, or-slice-name-union-01: over a written parameter the or-arm runs only
            // where the alternatives' elements are one type, so the one cell an unannotated parameter gets has no
            // annotation that passes for two.
            // CR claude for claude: [bug/low] A `_` in one arm at a part another arm
            // binds types that part `Any` over an unannotated parameter, and the bind's
            // uses are refused by a message that names no annotation: `let f = |s| select
            // s { (1, _) => 0, (_, b) => b + 1 }; f((2, 5))` says "cannot compute Any +
            // i64", `([], _) => 0, ([x, ..], b) => str::len(b)` "type mismatch string
            // does not contain Any", and `([], _) => 0, ([x, ..], b) => b` checks as
            // `fn(s: (Array<'e>, Any)) -> Any`. With `b` for the `_`, or the parameter
            // written (`|s: (i64, i64)|`), each runs. The settle below takes the arms'
            // predicates as written, where a `_` slot is `Any`, so the parameter's part
            // is `Any` before the arm that binds it is narrowed. Of 1200 generated
            // selects whose arms test one part and bind another, the 111 that are
            // refused where the written twin runs each hold such a `_`. base-r23 alike.
            // probes: ~/ptmp/cr29/scratch/r23/23d/stb/{s4.txt,s5.txt} (run with rb.sh),
            // gen.py (seeds 4201 and 977), rungen.sh (wildcard-slot-any-part-01)
            // 2026-11-13 claude: slice-arms-check-cost-01's fix (a cell a member holds
            // alone, a union of it included, is not copied) changes how two generated
            // selects of this shape are refused, not that they are: `let f = |s| select s
            // { ([x], b, c) => x + b, ([], _, _) => 1, ([x, y, t..], _, _) => 2 }; f(([],
            // 5, "r"))` said "Any could go to '_N or '_N or '_N or '_N of the union ['_N,
            // '_N, '_N, '_N], which nothing has decided" and now says "type mismatch
            // Number does not contain Any", as this CR's other shapes do. probes:
            // ~/ptmp/cr29/scratch/r23/23d/sacc/{msg_p0085u,msg_p0199u}.gx
            // 2026-11-13 claude: Review, lane 24b: a fix (e4a119389, reverted) read every
            // `_` at a position of a tuple, struct or variant as a cell of its own in the
            // settle, over a scrutinee cell with no lower bounds. It ran this CR's shapes
            // and closed bind-inside-wildcard-part-01, but the `_`'s cell is then one
            // more undecided cell, which takes what another arm tests at its part or
            // stays open beside it, and selects base-r24 runs right were refused. Where
            // they are written: `|s| select s { (`Key, `Char(c)) => c + 1, (`Mouse, _) =>
            // 0 }` ("this pattern tests for (`Key, `Char(i64)) among (`Mouse, '_N), but a
            // type variable there isn't known here"; a struct, a tuple or a slice for the
            // `Char alike), `(`Key, `Char(c)) => c + 1, (`Key, _) => 0` and `(1, _) => 0,
            // (_, null) => 1, (_, _) => 2` ("unreachable arm"), `` `S(`C(1)) => 1, `S(_)
            // => 0 `` ("may share `C(i64)'s runtime form"). At the use: `(1, _) => 0, (_,
            // "a") => 1, (_, _) => 2` called with `(2, 5)`; one instance called at two
            // types of a part only `_` meets (`let h = mk(0)`, a `let rec`, a function a
            // `<-` writes or a `&` holds, a later REPL input); `str::parse` selected by
            // `(1, _) => 0, (_, _) => 1` ("must be fully known here"); a second select
            // over one never() let. Of 900 generated selects (gen2.py, seeds 9102 and
            // 3317) 78 that base-r24 runs were refused and 51 it refuses ran. D5
            // (cr-discuss, Ruled) says a `_`'s own cell is not to count as undecided.
            // Tried on top of it, none landed (tried-scoped-cells.patch holds the last):
            // a cell only where another arm binds a name at, inside or over the `_`'s
            // position (two arms then differ by `Any` against a type at another part and
            // a call "could go to" both members); every `_` a cell only in a select where
            // one stands beside a bind, and `Any` for a cell the arms leave open that no
            // arm's predicate holds (that runs the `Mouse, `S, instance, parse and
            // second-select programs above, but `(`Key, `Char(c)) => c, (`Key, _) => 0`
            // is still "unreachable arm": narrowing binds the `_`'s cell to the other
            // arm's shape). What a fix has to hold at once: this CR's shapes (a name at
            // the `_`'s own position), a `_` wider than the shape another arm of the same
            // first part takes apart, and bind-inside-wildcard-part-01, which is a
            // question of where a call's argument goes among a decided `(i64, Any)`
            // member and an undecided one, not of the `_`'s type. Pins that hold the
            // programs above as they run (each fails on e4a119389):
            // lang::select_undecided::wildcard_beside_a_shape_under_another_tag,
            // .._under_the_same_tag, .._in_a_struct, .._in_a_variants_payload,
            // wildcard_beside_a_literal_of_another_type,
            // wildcard_arm_behind_a_null_test_is_live,
            // wildcard_part_of_one_instance_called_at_two_types,
            // wildcard_part_of_a_let_written_at_two_types,
            // wildcard_part_of_a_parsed_value. probes:
            // ~/ptmp/cr29/scratch/r24/24b/wsa-rev/ (README)
            // XCR claude for claude: [bug/high] A bind inside a part that another arm leaves
            // to a `_` is a variable no argument decides, over an unannotated parameter,
            // and the fused run dies: `let f = |x| select x { (1, _) => 0, (2, (c, _)) => c,
            // (_, _) => 1 }; let r = f((2, ("s", 4))); println(r)` checks, `f` being `fn(x:
            // (i64, Any)) -> [i64, '_N]`; fused it panics, "kernel param `r`: runtime
            // String("s") does not match the compiled Scalar(I64) slot", and --no-fusion
            // prints s. `f((2, ("s", 4))) + 1` checks and prints 2 on both engines: the
            // use makes the arm's test `(i64, (i64, Any))`, and the value goes to the `(_,
            // _)` arm. As REPL inputs, `r + 1` after `let r = f((2, ("s", 4)))` kills the
            // runtime. The settle below joins `(i64, Any)` and `(i64, ('c, Any))`, the
            // `_`'s `Any` holds the other arm's part, the call's argument is held by the
            // first, and nothing decides `'c`. With the parameter written `(i64, Any)` the
            // same (narrow-verdict-dropped-01's note). A `_` read as a cell of its own
            // closed it and was reverted (wildcard-slot-any-part-01's note). What would
            // close it: an argument that a decided member holds and an undecided member
            // could take decides that one too, or the bind is refused where the select is
            // written, naming the parameter to annotate. base-r24 alike. probes:
            // ~/ptmp/cr29/scratch/r24/24b/wsa-rev/{hole1.gx,hole2.gx,repl.in}
            // (bind-inside-wildcard-part-01)
            // 2026-11-13 claude: Fixer of bind-beside-shapes-01: Type::take now gives an
            // argument to the undecided member that holds the others (contains::widest), but
            // only where that leaves each of the others holding the argument as it stands
            // (contains::decides_each), so the three-part form of this hole, `(1, _, z) =>
            // z, (2, (c, _), z) => c, (_, _, z) => z` called with (2, ("s", 4), 0), stays
            // refused ("could go to (i64, Any, '_N) or (i64, ('_N, Any), '_N)"), pinned by
            // lang::select_undecided::wildcard_member_holding_a_bind_inside_takes_no_argument.
            // The same test at the decided member that takes this CR's argument (the others
            // that could take it must hold it as it stands, else refuse) would close the
            // two-part form; not tried.
            // 2026-11-14 claude: Fixer of narrow-verdict-dropped-01, lane 24b: closed by that
            // fix (PendingSettle::InsideAny): `c` is `Any` at the end of the definition of
            // `f`, so hole1.gx prints s fused and --no-fusion, hole2.gx (`f((2, ("s", 4))) +
            // 1`) is refused, "cannot compute [i64, Any] + i64", and repl.in refuses `r + 1`
            // where it killed the runtime. Pin:
            // lang::select::name_inside_an_any_part_nothing_decides_is_any (its `u`), which
            // fails without the fix. The three-part form stays refused as pinned (its `c` is a
            // cell of a member of the parameter, which an argument decides).
            // 2026-11-14 claude: Finisher, lane 24b, rebased onto r24-staging: the deferred
            // close the note above names (PendingSettle::InsideAny) is not in the tree. Lane
            // 24a's any-cell-bound-undecided-01 closes this instead: a name inside a part that
            // is Any is fixed at its arm's end (Type::close_over_any), so c is Any. hole1.gx
            // prints s fused, --no-fusion, forked and under GRAPHIX_NO_SUBST=1; hole2.gx is
            // refused, cannot compute Any + i64; as REPL inputs r + 1 is refused and the
            // session goes on. Pins
            // lang::select::name_inside_an_any_part_nothing_decides_is_any (its u) and
            // name_inside_a_part_another_arms_wildcard_makes_any_is_not_decided_by_a_caller_refused
            // fail with the close disabled. The three-part form no longer stays refused: with
            // c fixed, the member holding the others takes the argument (Type::take) and it
            // prints s in every mode, pinned by
            // lang::select_undecided::wildcard_member_holding_a_bind_inside_takes_a_decided_argument;
            // an argument still undecided is refused as before
            // (.._takes_no_undecided_argument). Probes: ~/ptmp/cr29/scratch/r24/24b/fin/
            // (README).
            // CR claude for claude: [bug/high] A bind beside two other arms' literals of two
            // types at one part of a tuple or struct is typed without one of them where its
            // arm comes after theirs, and the fused arm reads a value of that type as the
            // other: `let f = |s| select s { (1, 1) => 0, (3, "x") => 2, (4, v) => v + 1,
            // (_, _) => 9 }; f((4, "y"))` checks, `f` being `fn(s: [(i64, [i64, string]),
            // (i64, i64)]) -> i64`; fused it prints an address plus one (140031906144801),
            // --no-fusion 9, and GRAPHIX_NO_SUBST=1 refuses it, "cannot compute [i64,
            // string] + i64" (GRAPHIX_ELAB_AUDIT reports it, for `f((4, 7))` too). With
            // `"[v]"` for the body fused prints an address, --no-fusion w and NO_SUBST y;
            // with `null` for the `"x"` and `f((4, null))` fused prints 0; with `"a"` for
            // the first arm's second `1` as well the fused run dumps core. The struct form
            // (`{k: 4, v} => v + 1`) alike. With the bind's arm first the check refuses it
            // ("cannot compute [i64, string] + i64"), and the variant form (`` `S(4, v) ``)
            // is refused too ("cannot compute Any + i64"). The tuple twin of
            // payload-bind-beside-literal-01, whose text has the tuple forms right: with
            // one literal arm they are. Not diagnosed. base-r24 alike. probes:
            // ~/ptmp/cr29/scratch/r24/24b/wsa-rev/{r10.txt,r11.txt,r17.txt} (run with
            // rb.sh), fs1.gx (bind-beside-literal-types-01)
            // 2026-11-13 claude: Fixer of bind-beside-shapes-01: literals are not needed,
            // two arms that take the part apart in two shapes do the same to a bind behind
            // them, and both engines then agree on the wrong arm (only GRAPHIX_NO_SUBST=1
            // takes the bind's): `let f = |s| select s { (1, `B(v1)) => 100, (2, (v1, v2))
            // => v1 + 200, (3, v1) => 300, (_, v1) => 9 }; f((3, (1, 7)))` checks, `f` being
            // `fn(s: [(i64, `B('_N)), (i64, (i64, '_N))]) -> i64`, and prints 9 where the
            // third arm takes every value (300, which `|s: (i64, [`B('b), (i64, i64)])|`
            // prints); `f((3, `B(1)))` prints 300. One literal and one shape alike (`(1,
            // [v1]) => v1 + 100, (2, 2) => 200, (3, v1) => 300, (_, v1) => 9` called with
            // (3, 3) prints 9), and a variant scrutinee (``S(2, `B(v0)) => 200, `S(4,
            // `A(v1)) => 400, `S(v1, v2) => v1 + 500, _ => 9` called with `S(2, `A(1))
            // prints 9; with `str::len(v0)` for the first body the fused run prints 502 and
            // --no-fusion 9; without the `_` arm it is refused, "missing match cases: no arm
            // covers `S(i64, [`A('_N), `B('_N)])"). With the bind's arm before theirs the
            // calls were refused ("could go to") until bind-beside-shapes-01's fix and now
            // run right; that fix also lets more calls reach this: `(f(`S(2, `A(1))),
            // f(`S(2, `B("x"))))` over the variant select was refused at its second call and
            // prints (9, 200). Of 1400 generated selects (gen.py, which computes what the
            // arms should print) 15 print another arm's value this way, fused and
            // --no-fusion alike, and 3 more only --no-fusion; base-r24 alike. probes:
            // ~/ptmp/cr29/scratch/r24/24b/bbs/{e1.txt,e3.txt} (run with rb.sh), gen.py,
            // rungen.sh, wrong.txt
            // 2026-11-14 claude: Reviewer of bind-beside-shapes-01: a call that fix newly
            // accepts can itself print another arm's value through this, in both engines:
            // `let f = |s| select s { (1, _, "bc") => 100, (2, v1, "a") => 200, (3, "a",
            // (v1, 2, v2)) => str::len(v2) + 300, (v1, "bc", v2) => v1 + 400, (5, _, (v1,
            // v2, "bc")) => v2 + 500, (_, v1, v2) => 9 }; f((4, "bc", (2, 1, "bc")))` was
            // refused on base-r24 ("could go to (i64, Any, ..) or (i64, string, ..)") and
            // prints 9 where the fourth arm gives 404 (GRAPHIX_NO_SUBST=1 prints 404). The
            // defect is this CR's, not the take's: base-r24 accepts the same call over four
            // of those arms and over `(1, "bc") => 100, (3, (v1, v2)) => v1 + 300, (4, v2)
            // => 400, (_, v2) => 9` called with (4, (2, 1)), and prints 9 for both. Of 900
            // more generated selects (gen.py seeds 5151, 808, 99173) 13 print another arm's
            // value on the lane, 12 of them on base-r24 too. probes:
            // ~/ptmp/cr29/scratch/r24/24b/bbs-rev/q3.txt (run with ../bbs/rb.sh), p0047.gx,
            // g5151.sum
            // CR claude for claude: [perf/low] The check of a select over an unannotated
            // parameter whose arms hold a literal at one part and a name at another grows
            // with the square of the arms: `|s| select s { (1, a) => 1, .., (N, a) => N, (_,
            // _) => 0 }` takes 3.73 G user instructions of `--no-cache --check` in a debug
            // build at 200 arms, beside the 0.74 G of an empty program and the 0.80 G of
            // the same arms with `_` for the name; a struct of three fields takes 4.65 G
            // at 200. The variant form (`` `A(k, a) ``) takes 1.11 G: its arms' cells are
            // one before the settle (Type::binds_beside), where each tuple or struct arm
            // is a member of its own that every arm narrows against. What would likely fix
            // it: the same for the members of one tuple or struct shape whose other parts
            // are equal. The fixer of wildcard-slot-any-part-01 measured 0.94 G at 50 arms
            // and 1.50 G at 100. base-r24. probes:
            // ~/ptmp/cr29/scratch/r24/24b/wsa/{w200,b200w,b200s,b200v}.gx, m.sh
            // (tuple-arms-settle-cost-01)
            // CR claude for claude: [bug/low] The check accepts and the build refuses a
            // select of three literal arms and a wildcard that bind a part of an abstract's
            // payload built at two shapes, one holding the other's cell: `type B<'p> =
            // Abstract<'p>; let x = never(); let b = B((1, [x])); b <- B((2, x)); let v =
            // select b.0 { (1, h) => h, (2, h) => h, (3, h) => h, (_, h) => h }; x <- t ~
            // 1; println(v)` passes `--check`, and a run is refused at `println(v)`, "i64
            // could go to any of 2 members of the union ['_N, i64, Array<'_N>, Array<i64>]
            // .. write the type of what holds that union". GRAPHIX_ELAB_AUDIT=1 reports
            // nothing. With one literal arm (`(1, h) => h, (_, h) => h`) or a bind alone
            // (`(n, h) => h`) it prints [1] and then 1. base-r23 alike (its union reads
            // "[['_N, i64, Array<'_N>, Array<i64>], ['_N, Array<'_N>]]"). Not diagnosed.
            // probes: ~/ptmp/cr29/scratch/r23/23d/stb-rev/{sp1,sp2,sp3}.gx
            // (payload-bound-arms-split-01)
            // 2026-11-13 claude: Stale on base-r24 (main be2c16a05): sp1 checks and runs to
            // [1] then 1, fused, --no-fusion, PAR=force, NO_SUBST, ELAB_AUDIT and from a
            // cached image, and so do five and one literal arms and the shapes swapped. A
            // bisect of sp1 over the 98 commits since this CR was written gives 0ec300127
            // (construct-lower-merge-01) as the first that runs it. Its two parts each
            // close it now: with TVar::weld_lower a no-op and FnType::settle_terminal's
            // walk of the cells a lower bound holds both undone on the lane, sp1 is refused
            // again with this CR's message, and with either one alone it runs. The arms'
            // binds were never apart from the cell of x: a use in any one arm that decides
            // its bind (let q: [i64, Array<i64>] = h) makes a later x <- "s" refused, at
            // the check and the build alike, for each of the four arms over five tag pairs.
            // Pins:
            // lang::select_undecided::payload_part_bound_by_literal_arms_over_two_shapes_builds_at_a_call
            // (all four modes fail with both parts undone, by the message above) and
            // payload_part_an_arm_decides_over_two_shapes_then_written_apart_refused (holds
            // either way). 468 generated programs of this family (holder, shapes, 0 to 5
            // literal arms, the read, the write): no check/build difference, verdicts and
            // outputs equal on base-r24 and the lane. probes:
            // ~/ptmp/cr29/scratch/r24/24b/pbas/ (README)
            // CR claude for claude: [bug/low] A part built into a scrutinee cell the arms
            // decide still goes by the order of the arms' union where the arm that takes
            // its member whole binds a name: `let f = |x, y, b: bool| { let r = &mut
            // (select b { true => (`K, x, y), false => (`L, 0, "a") }); select *r { (`K,
            // n, _) => "0:[n]", (_, _, "a") => "1", _ => "w" } }` is `fn(x: '_N, y:
            // string, b: bool) -> string`, so `f(5, 7, true)` is refused, "type mismatch
            // string does not contain i64", and with `y` first (`(y, `K, x)` beside `("a",
            // `L, 0)`, arms `(_, `K, n)`, `("a", _, _)`, `_`) it is `fn(x: '_N, y: '_N, b:
            // bool) -> string` and prints ["0:5", "1"]. built_standing takes only a test
            // that holds no cell as holding a part as it stands, so `(`K, 'n, Any)`, which
            // holds `(`K, 'x, 'y)` binding only its own cell, holds nothing back, `(Any,
            // Any, string)` takes the part where it sorts first, and one such test is not
            // two for the refusal. `|x, y: i64, b: bool|` prints in both orders. Of 70
            // generated families over two undecided parts (gen2.py, 1680 selects, each
            // family in the six orders of its parts) this is the one whose verdict follows
            // the order. What would close it: a test with cells of its own that holds the
            // part binding only those counts as holding it, where the settle can still
            // place every other part (the test's cells are shared between the parts: `(`K,
            // 'n)` over `(`K, 1)` and `(`K, x)`); or the part is refused as one two tests
            // could take, which also refuses the selects whose other test decides only the
            // cell the name binds (`(`K, n, _)`, `(_, 0, _)`: `x` is i64 in every order
            // today). base-r24 alike. probes: ~/ptmp/cr29/scratch/r24/24b/lbfm-rev/a2.txt
            // lines 6 to 8 (run with rb.sh), t6.txt (ty.sh), gen2.py, g7711.sum (f0046)
            // (cover-bind-part-order-01)
            // CR claude for claude: [bug/high] A bind in an arm over a referent the arms
            // decide is typed by its arm's body alone where another arm's member holds
            // what was built, and the arm then takes a value of another type: `let b =
            // true; let r = &mut (select b { true => (`K, "s", "t"), false => (`L, 0, "a")
            // }); select *r { (`K, n, _) => n + 1, (_, _, m) => str::len(m) }` checks,
            // with no type undecided; fused it panics, "kernel param `n`: runtime
            // String("s") does not match the compiled Scalar(I64) slot", and --no-fusion
            // logs an arith error and prints nothing. The struct form and an abstract's
            // parameter (`select W(..).0`) alike. Inside a function over unannotated `x`
            // and `y` (`(`K, x, y)` beside `(`L, 0, "a")`) it is `fn(x: '_N, y: string, b:
            // bool) -> i64` in every order of the parts, `f("s", "s", true)` panics, and
            // the abstract form prints an address plus one ([139885276136705, 1]). Refused
            // as it should be: the referent written (`let r: &mut [(`K, string, string),
            // (`L, i64, string)]`, "cannot compute string + i64"), the plain twin (`select
            // (select b { .. }) { .. }`) and the variant form ("cannot compute Any +
            // i64"). With both binds at one part (`(`K, n, _)`, `(_, m, _)`) the first
            // arm's test is run and the second arm takes the value. The settle below joins
            // `(`K, 'n, Any)` and `(Any, Any, 'm)`, both built parts go to the second, and
            // nothing gives `'n` the part of `(`K, string, string)` that its arm takes;
            // why the arm's run-time test then passes the string is not traced. What would
            // likely close it: a built part goes to every member that could take it, so
            // each arm's bind holds the part, or the select is refused where a built part
            // goes to a member whose arm is not the first that matches its values.
            // base-r24 alike. probes: ~/ptmp/cr29/scratch/r24/24b/lbfm-rev/h1.txt (run
            // with rb.sh), p1.txt, p3.txt, p6.txt, q2.gx (referent-bind-other-member-01)
            // CR claude for claude: [bug/low] Two undecided values that meet in one
            // part of a variant scrutinee are made one type by arms that bind that
            // part beside literals, and the refusal names no annotation: `let f = |x,
            // y, c| { let u = select c { true => x, false => y }; select `A(1, u) {
            // `A(1, a) => "[a]", `A(2, a) => "[a]", _ => "" } }; f(1, "s", true)` says
            // "type mismatch i64 does not contain string", `f` being `fn(x: 'a, y: 'a,
            // c: bool) -> string`. The tuple and struct forms (`select (1, u) { (1, a)
            // => .., (2, a) => .. }`) check as `fn(x: 'a, y: 'b, c: bool)` and print
            // "1", and so do `|x: 'a, y: 'b, c: bool|` and the select with `` `A(_, a)
            // `` for its second arm. Two lets over `never()` written into one binding
            // (`v <- `A(1, x); v <- `A(2, y)`, then `x <- t ~ 1; y <- t ~ "s"`) say "y
            // is i64, inferred from an earlier use, and cannot hold string; declare y:
            // [i64, string]", which runs when written. base-r24 refused each too, by
            // another message ("i64 could go to '_N or '_N of the union ['_N, '_N],
            // which nothing has decided .. write the type of the parameter"), `f`
            // being `fn(x: ['_N, '_N], y: ['_N, '_N], c: bool) -> string`: the settle
            // below gave each of the scrutinee's two cells the union of the arms' two,
            // where a tuple's members each take the scrutinee's union whole, and since
            // variant-payload-bind-cells-01 the arms' cells are one (Type::binds_beside),
            // so the scrutinee's are. Not traced beyond the types printed. What would
            // likely close it: a part of the arms' joined predicate that is only their
            // binds decides nothing of the scrutinee's part here, and each arm's bind
            // takes that part as it stands when the arm narrows. probes:
            // ~/ptmp/cr29/scratch/r24/24b/vpb-rev/{r2.txt,r6.txt} (run with rb.sh)
            // (outer-cells-take-arm-cells-01)
            match matches!(scrut, Type::TVar(tv) if tv.open_cell().is_some()) {
                true => {
                    let gates: SmallVec<[_; 2]> =
                        standing.iter().map(|c| c.open_rigid()).collect();
                    let _ = itype.contains_ordered(env, scrut);
                    drop(gates)
                }
                false => {
                    // CR claude for claude: [bug/low] A written test's own variable
                    // takes the first member's part wherever this settle runs (no
                    // catch-all arm, or a scrutinee that is no union) over a scrutinee
                    // holding an open cell, which a lambda's function type does: the
                    // written predicates' cells are kept out of the copy, so `itype ⊇
                    // scrut` binds the variable itself, where an inferred predicate's
                    // cell is left to its arm's narrowing. Over `u = select v > 0 { true
                    // => (|x: i64| x, 1), false => (|y: string| y, 2) }`, `select u {
                    // ('q, i64) as (r, n) => n }` says "this pattern can't tell fn(x:
                    // i64) -> i64 from fn(y: string) -> string" (`'q` is the first
                    // function alone), where the same arm beside `_ => 0` checks and
                    // gives `r` both; `{f: 'q, n: i64} as {f, n}` and, beside a `_` arm,
                    // `Array<('q, i64)> as [(r, n), ..]` over `[u, u]` alike; and with a
                    // third member `(1, 2)` the arm is `(i64, i64)`: "missing match
                    // cases: no arm covers [(fn(x: i64) -> i64, i64), (fn(y: string) ->
                    // string, i64)]". base-r24 alike. Tried: `written.retain(|_, tv|
                    // tv.is_bound() || tv.is_rigid())` before the settle (the test's
                    // open variables copied as an inferred predicate's cells are, a
                    // declared variable itself): the four probes run right, graphix-tests
                    // lib passes, census 0/0, regress and typemorph-scan clean, and of
                    // 600 generated selects 206 more check, none differing from an
                    // oracle. Not landed: it moves a select whose variable is written
                    // twice from the first member's binding, which refuses it ("will
                    // never match"), to the open variable of
                    // (written-variable-twice-open-01); fix that one first. probes:
                    // ~/ptmp/cr29/scratch/r24/24a/wvt/{p6c,s1,a1b,d2,x1}.gx,
                    // settle.patch, gen/ (written-variable-settle-first-01)
                    // 2026-11-14 claude: (finisher, lane 24a rebased onto r24-staging
                    // e8c333097) With variant-tuple-payload-dead-01 and
                    // diff-product-held-01 on staging the catch-all behind a written test
                    // that took every member is found dead, so this settle is now all
                    // that refuses the two-member forms of (written-variable-told-01):
                    // over [(fn_a, i64), (fn_b, i64)], `('q, i64) as (r, n) => n` is
                    // refused 'can't tell' here and `.. , _ => 0` 'unreachable arm' (pin
                    // lang::select::written_variable_over_every_member_leaves_a_catch_all_dead),
                    // and only a scrutinee holding a member the catch-all takes (a null)
                    // checks: lang::select::written_variable_binds_two_function_types
                    // holds a null member for that reason, and drops it when this is
                    // fixed. The tuple, literal, field and twice forms without a
                    // catch-all are the probes:
                    // ~/ptmp/cr29/scratch/r24/24a/fin/wv{2,4,6,10}.gx (pin.gx is the
                    // fixture as it stands, dead.gx the refused twin).
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
            catch_all: wildcard,
            guarded_slice: false,
            refutable_slice: false,
            built,
            apart,
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
        match &built.typ {
            Some(typ) => {
                let known: AHashMap<ArcStr, Type> = built
                    .cells
                    .iter()
                    .map(|(n, tv)| {
                        let name = holders.name_cell(tv).or_else(|| tv.written_own());
                        (n.clone(), name.unwrap_or_else(|| holders.number(tv)))
                    })
                    .collect();
                let typ = typ.replace_tvars(&known);
                let meanings = holders.meanings(at);
                anyhow!(
                    "the arms decide this scrutinee's type from their patterns, and their \
                     binds cannot hold what was built into it, {typ}: annotate that type \
                     where it is built{meanings}"
                )
            }
            None => anyhow!(
                "the arms decide this scrutinee's type from their patterns, and their \
                 binds cannot hold what was built into it: annotate its type where it is \
                 built"
            ),
        }
    }

    // CR claude for claude: [bug/low] A cover that takes one member of a union at a
    // product's position leaves the product whole in what reaches the next arm, so an
    // arm after arms that cover such a union member by member is never found dead:
    // `|x: [`A([i64, string])]| select x { `A(i64) as `A(n) => n, `A(string) as `A(s)
    // => 0, _ => 3 }` checks and runs, as do the tuple form (`(null, _)`, `(i64, i64)
    // as (n, _)`, `_` over `([i64, null], i64)`) and slice ladders beside a null
    // (`` `A([h, ..]), `A([]), `A(null), `A(_) `` over `` `A([Array<i64>, null]) ``,
    // whose covers since slice-position-other-member-01 are `` `A(Array<i64>) `` and
    // `` `A(null) ``). Exhaustiveness reads `covered` by containment, so no select is
    // refused that should check; only the dead arm is missed. Type::diff takes a
    // product out whole or not at all (setops::held_by_shape); a residual narrowed at
    // the position was tried for diff-product-held-01 and cost the recursive-union
    // budget, so a fix wants a residual taken only where the subtrahend differs from
    // the minuend at one position and takes whole members of the union there.
    // base-r24 alike. probes: ~/ptmp/cr29/scratch/r24/24h/spom/residual/{g01,g02,g03,
    // d04,d05,n03}.gx (product-member-residual-01)
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
        // CR claude for claude: [bug/low] A catch-all arm behind a written test that
        // takes every member is not found dead where the test's variable stands over
        // a function in a tuple or a struct: over `u = select v { 0 => (7, 1), _ =>
        // (fa, 2) }` (`fa = |x: i64| x + 100`), `select u { ('q, i64) as (r, n) => n,
        // _ => 0 }` checks and prints (1, 2) for 0 and 1, and so does `(i64, 'q1, 'q2)
        // as (a0, a1, a2) => 1, _ => 0` over `[(i64, fn(y: string) -> i64, i64), (i64,
        // i64, fn(x: i64) -> i64)]` (base-r24 alike), and now the two-function forms
        // (written-variable-told-01) admits (`('q, i64)` over `[(fa, i64), (fb,
        // i64)]`, `{f: 'q, n: i64}`). The names arm `(r, n) => n, _ => 0`, the variant
        // `` `K('q, i64) as `K(r, n) ``, and `('q, i64)` over `[(i64, i64), (string,
        // i64)]` are refused, "unreachable arm". Of 600 generated selects 133 run with
        // such an arm. Not diagnosed: what the arm covers is the written test, a
        // product whose part is a union of the members' parts, and a difference
        // subtracts a product only by the same type (diff-product-held-01). A fix here
        // makes the accepted forms of (written-variable-told-01) need no catch-all,
        // which (written-variable-settle-first-01) refuses today: fix that first.
        // probes: ~/ptmp/cr29/scratch/r24/24a/wvt/{dead1,dead2,dead3,dead4,dead5,dead6,
        // dead7}.gx, gen/g1-summary.txt (written-variable-dead-arm-01)
        // 2026-11-13 claude: written-variable-told-01's reviewer: a union the test
        // writes over references does the same since that review's correction (the
        // held-member rule at the settle too): over `u = select v { 0 => (&mut a, 1), 1
        // => (&mut b, 2), _ => (null, 3) }` with `type Y = [[&mut i64, null], &mut
        // string]`, `select u { (Y, i64) as (r, n) => n, (x, n) => n + 10 }` checks and
        // prints (1, 2, 3), the second arm never taken (base-r24 refuses the first arm,
        // "can't tell"); the written test alone (`(Y, i64) as (r, n) => n`) checks too,
        // so here no catch-all is needed. probe:
        // ~/ptmp/cr29/scratch/r24/24a/wvt/review/h07.gx
        // 2026-11-14 claude: (review of written-struct-absent-field-cover-01)
        // r24-staging finds this arm dead: in a trial merge of lane 24a with it
        // (variant-tuple-payload-dead-01 and diff-product-held-01 are there),
        // lang::select::written_variable_binds_two_function_types fails in all four
        // modes, 'unreachable arm: the earlier arms already cover the whole scrutinee'
        // at the catch-all of its tuple, literal, field and twice selects, and the same
        // selects without the catch-all are still refused 'can't tell fn(x: i64) -> i64
        // from fn(y: string) -> string' (written-variable-settle-first-01), so only the
        // payload form checks there. The pin holds at the landing only with
        // settle-first fixed and its catch-alls dropped. probe:
        // ~/ptmp/cr29/scratch/r24/24a/wsaf-rev/wv.txt (lines 1 to 10, lane against the
        // merge)
        if checking && (self.wildcard || self.atype == empty) {
            return dead_at(anyhow!(
                "unreachable arm: the earlier arms already cover the whole scrutinee, \
                 unused match cases"
            ));
        }
        if checking && !pat.type_predicate.could_match(env, &self.atype)? {
            let (tp, atype) = (&pat.type_predicate, Brief(&self.atype));
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
        if checking && !self.structure_admits(env, pat, &pat.structure_predicate)? {
            let atype = &self.atype;
            return dead_at(format_with_flags(PrintFlag::DerefTVars, || {
                anyhow!(
                    "unreachable arm: no value of {atype} has the parts this pattern \
                     tests, unused match cases"
                )
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
                             {}, unused match cases",
                            Brief(atype)
                        )
                    }
                }));
            }
            if checking && or_arm && !self.structure_admits(env, pat, sp)? {
                let atype = &self.atype;
                return dead_at(format_with_flags(PrintFlag::DerefTVars, || {
                    anyhow!(
                        "unreachable or-pattern alternative: no value of {atype} has the \
                         parts it tests, unused match cases"
                    )
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
                    // CR claude for claude: [bug/high] A slice whose element is a tuple or
                    // struct of names claims its lengths of an array member whose element
                    // type is a union holding other members, so the check accepts a select
                    // that has no arm for a value: `let f = |x: Array<[(i64, i64), i64]>|
                    // select x { [(a, b), ..] => 1, [] => 0 }; f([3])` passes --check and
                    // the run logs "no arm matches [i64:3]: a coverage hole in the check",
                    // and `Array<[(i64, i64), (i64, i64, i64)]>` with `f([(1, 2, 3)])`
                    // alike; `|x: Array<(i64, i64)>|` is the twin that runs. base-r24
                    // alike. array_len_coverage reads an element pattern by
                    // matches_anything, which takes a tuple of names as irrefutable
                    // whatever the element type is, and the inferred predicate holds each
                    // element over its own member, so `at` contains the member. The same
                    // hole inside a tuple, struct or variant position is closed (the
                    // nested ladder reads elements by elements_cover since
                    // slice-position-other-member-01). What would close it: without a
                    // written test, claim a member only where sp.elements_cover holds of
                    // it (any_member only under `explicit`); tried, h12/h15 are then
                    // refused and five twins run, but the refusal's hint then says the
                    // ladder "needs a rest pattern" where it has one: refutable_slice
                    // wants setting where a member's elements are not covered. probes:
                    // ~/ptmp/cr29/scratch/r24/24h/spom/hole/{h12,h15,k02}.gx
                    // (slice-element-union-hole-01)
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
    /// one of its slices matches only lengths earlier arms claimed there,
    /// on a ladder whose member holds it;
    /// else, `unguarded`, it claims its lengths of each member it holds
    /// where one slice is all it refutes by, covering a member it completes.
    /// A member stands as `sp`'s shape narrows it: less what a position
    /// holds that `sp` admits no value of (`` `A(null) `` of `` `A([Array<i64>,
    /// null]) `` for `` `A([h, ..]) ``).
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
            let inner = inner_slices(env, sp, &m, &mut SmallVec::new(), &mut slices)?;
            let m = inner.narrowed.unwrap_or(m);
            if dead_check && at.could_match(env, &m)? {
                any = true;
                let mut claimed = false;
                'slices: for s in slices.iter() {
                    for l in self.nested.iter() {
                        if l.path == s.path
                            && l.ladder.covers(s.len.0, s.len.1)
                            && l.ladder.member.contains_with_flags(
                                BitFlags::empty(),
                                env,
                                &m,
                            )?
                        {
                            claimed = true;
                            break 'slices;
                        }
                    }
                }
                all &= claimed;
            }
            if unguarded
                && inner.only
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
            || !pat.type_predicate.could_match(env, &self.atype)?
            || !self.structure_admits(env, pat, &pat.structure_predicate)?)
    }

    /// Whether a value of what reaches may pass `sp`, the structure of
    /// `pat` or of one of its or-alternatives, where `pat`'s type predicate
    /// could match it. An inferred predicate holds every test of its
    /// structure but those [`StructPatternNode::tests_past_inferred`] names.
    fn structure_admits<R: Rt>(
        &self,
        env: &Env,
        pat: &PatternNode<R>,
        sp: &StructPatternNode,
    ) -> Result<bool> {
        match pat.explicit_type_predicate || sp.tests_past_inferred() {
            true => sp.admits(env, &self.atype, false),
            false => Ok(true),
        }
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
        if covered.contains_ordered(env, scrut)? {
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
                    Brief(&l.member)
                ),
                Some(l) => format_compact!(
                    " (the slice arms leave length {} of {} uncovered)",
                    l.gap().unwrap_or(0),
                    Brief(&l.member)
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
                Brief(&self.atype.normalize())
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
/// names as the arms' shape narrows it: the lengths there that arms
/// testing nothing else claim.
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

/// Deselect arm `j`: sleep it unless it is pure. Under
/// [`super::deselecting_arm`] a recursive-edge callee inside the arm is
/// deleted (`CallSite::sleep`). The arm did not run this cycle, so the
/// cycle's fires are observed again once the tracker has spent what
/// joined the arm.
fn deselect<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    tracked: &mut Option<TrackedFires>,
    j: usize,
    arm: &mut (PatternNode<R>, Node<R>),
    sleep: bool,
) {
    if sleep {
        super::deselecting_arm(true, || arm.1.sleep(ctx))
    }
    let Some(tracked) = tracked else { return };
    tracked.deselect(ctx, j, |r| arm_refs(arm, r));
    tracked.observe(ctx);
    tracked
        .observe_late(ctx, |i, b| i == j && BoundaryRef::held_by(arm_boundary(arm), b));
}

/// The walk-pruning boundary around an arm's body.
fn arm_boundary<R: Rt>(arm: &(PatternNode<R>, Node<R>)) -> Option<&BoundaryRef> {
    arm.1.as_prune().map(|p| p.boundary_ref())
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

// CR claude for claude: [bug/low] `true` and `false` arms at a variant's payload
// position whose union holds an array or a list never cover the bool member, so
// the select is refused though an arm takes every value: `|x: [`A([bool,
// Array<i64>])]| select x { `A(true) => 0, `A(false) => 1, `A(Array<i64>) as `A(a)
// => 2 }` and the slice form (`` `A([a, ..]) => 2, `A([]) => 3 `` for the last arm)
// say "missing match cases: no arm covers `A([bool, Array<i64>])". The tuple form
// (`(true, _)`, `(false, _)`, `(Array<i64>, i64) as (a, _)` over `([bool,
// Array<i64>], i64)`) runs, since a written test cuts a tuple into parts
// (PartPool), and so does `[bool, (i64, i64)]` at the position, whose tuple is a
// head (Head::Composite). An array or list member has no head here, so the
// position's heads are None and the pool never completes; a `null` arm covers its
// member by StructPatternNode::matched_part and a bool pair has no such path.
// base-r24 alike. probes: ~/ptmp/cr29/scratch/r24/24h/spom/bool/{b05,b06,b08}.gx,
// b07.gx (the tuple twin) (bool-beside-collection-cover-01)
// CR claude for claude: [bug/low] An arm is not found dead behind arms that take
// every value it matches where a position inside the scrutinee holds two tuple or
// struct members: `type E = [{a: i64, d: i64}, {c: i64}]; |x: (i64, E)| select x { (n,
// {a: _, d: _}) => 2, (n, {a: h, d: _}) => h, (n, {c: _}) => 1 }` checks, its second
// arm never taken, and so do the arms `(n, {a: _, d: _}) => 2, (n, {c: _}) => 1`
// followed by `_ => 3` or `(n, _) => 3`, the same in a variant's payload (`` `A({a: _,
// d: _}) `` twice over `` [`A(E), `B] ``) and in a struct's field (`{s: {a: _, d: _},
// z}` twice), and `(n, (a, b))` twice over `(i64, [(i64, i64), (i64, i64, i64)])`. The
// same arms over `E` itself are refused, and so are `(n, (a, b))` twice over `(i64,
// [(i64, i64), i64])` and the written form (`T as (n, {a: _, d: _})` twice, "an earlier
// arm matches the same values"). base-r24, lane 24a and a trial merge of it with
// r24-staging alike; of 900 generated selects of struct patterns over a union of structs
// (gen2.py: at the top, in a tuple, a payload, a field), the merge checks 106 with a dead
// arm at a nested position and 6 at the top (those by a literal or a guard written
// twice). Not traced past this: a position holding a second tuple or struct has no
// finite set of heads here, so the pool does not read it. probes:
// ~/ptmp/cr29/scratch/r24/24a/wsaf-rev/r4.txt, r5.txt (run.sh in ../wsaf runs each
// line), gen2.py and c11.*.txt (the generated programs' verdicts)
// (two-composite-position-dead-01)
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
#[derive(PartialEq)]
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

/// What [`inner_slices`] found of a composite pattern inside a type.
struct Inner {
    /// The slices found are all the pattern refutes a value of the type
    /// by: every other part of it, and every slice's elements, take any
    /// value of their type.
    only: bool,
    /// The type less the members the pattern admits no value of, at this
    /// position or below; `None` where it leaves none out.
    narrowed: Option<Type>,
}

impl Inner {
    const NONE: Self = Self { only: false, narrowed: None };
}

/// The slices composite pattern `sp` tests inside `t`, found along
/// positions whose every member `sp` admits a value of fits the pattern
/// there with the same slices.
fn inner_slices(
    env: &Env,
    sp: &StructPatternNode,
    t: &Type,
    path: &mut SmallVec<[u32; 4]>,
    out: &mut SmallVec<[InnerSlice; 2]>,
) -> Result<Inner> {
    if sp.covers(env, t, false) {
        return Ok(Inner { only: true, narrowed: None });
    }
    let mut members: SmallVec<[Type; 8]> = SmallVec::new();
    if union_members_open(env, t, &mut members)? {
        return Ok(Inner::NONE);
    }
    let found = out.len();
    let mut only = true;
    let mut left_out = false;
    let mut kept: SmallVec<[Type; 4]> = SmallVec::new();
    let mut other: SmallVec<[InnerSlice; 2]> = SmallVec::new();
    for m in members.iter() {
        if !sp.admits(env, m, false)? {
            left_out = true;
            continue;
        }
        let first = kept.is_empty();
        let inner = match first {
            true => member_slices(env, sp, m, path, out)?,
            false => {
                other.clear();
                member_slices(env, sp, m, path, &mut other)?
            }
        };
        if !first && other[..] != out[found..] {
            out.truncate(found);
            return Ok(Inner::NONE);
        }
        only &= inner.only;
        left_out |= inner.narrowed.is_some();
        kept.push(inner.narrowed.unwrap_or_else(|| m.clone()));
    }
    let narrowed = match kept.len() {
        0 => return Ok(Inner::NONE),
        _ if !left_out => None,
        1 => kept.pop(),
        _ => Some(Type::Set(Arc::from_iter(kept)).normalize()),
    };
    Ok(Inner { only, narrowed })
}

/// [`inner_slices`] inside `m`, one member of a position's type.
fn member_slices(
    env: &Env,
    sp: &StructPatternNode,
    m: &Type,
    path: &mut SmallVec<[u32; 4]>,
    out: &mut SmallVec<[InnerSlice; 2]>,
) -> Result<Inner> {
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
            let Some(len) = sp.array_len_range() else { return Ok(Inner::NONE) };
            out.push(InnerSlice { path: path.clone(), len });
            return Ok(Inner { only: sp.elements_cover(env, m), narrowed: None });
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
        _ => return Ok(Inner::NONE),
    }
    let mut only = true;
    let mut parts: SmallVec<[Option<Type>; 8]> = SmallVec::new();
    for (i, (p, t)) in children.into_iter().enumerate() {
        path.push(i as u32);
        let inner =
            crate::stack::ensure_sufficient(|| inner_slices(env, p, t, path, out))?;
        path.pop();
        only &= inner.only;
        parts.push(inner.narrowed);
    }
    if parts.iter().all(Option::is_none) {
        return Ok(Inner { only, narrowed: None });
    }
    let part = |p: Option<Type>, t: &Type| p.unwrap_or_else(|| t.clone());
    let parts = parts.into_iter();
    let narrowed = match m {
        Type::Tuple(ts) => {
            Type::Tuple(Arc::from_iter(parts.zip(ts.iter()).map(|(p, t)| part(p, t))))
        }
        Type::Variant(tag, ts, at) => Type::Variant(
            tag.clone(),
            Arc::from_iter(parts.zip(ts.iter()).map(|(p, t)| part(p, t))),
            *at,
        ),
        Type::Struct(fs) => Type::Struct(Arc::from_iter(
            parts.zip(fs.iter()).map(|(p, (n, t, at))| (n.clone(), part(p, t), *at)),
        )),
        _ => return Ok(Inner::NONE),
    };
    Ok(Inner { only, narrowed: Some(narrowed) })
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
    // CR claude for claude: [bug/low] Arms that pool over a referent they decide do not
    // exhaust it, with nothing undecided: `|b: bool, c: bool| { let r = &mut (select c {
    // true => (b, "s"), false => (`L, `Z) }); select *r { (true, _) => 1, (false, _) =>
    // 2, (_, `Z) => 3 } }` is refused, "missing match cases: no arm covers (bool, Any)".
    // The plain twin `select (select c { .. }) { .. }` and the referent annotated
    // `[(bool, string), (`L, `Z)]` check and print [1, 2, 3] for the three values; a
    // cover in place of the pool (`(`K, _)`, `(_, `Z)` over `(`K, "s")`) and a catch-all
    // added check too. The arms decide the referent `[(Any, `Z), (bool, Any)]`, whose
    // members overlap: `(true, _)` has leaves against both, so it pools against neither
    // (the second member reached in claim_among), no group completes and
    // Reach::exhausted finds `(bool, Any)` uncovered. Uncovered::pool counts the pair
    // over the lower view's members for the undecided check alone
    // (lower-view-pool-cover-01's fix), so that select needs the catch-all it has.
    // base-r24 and lane 24b alike. A fix to try: an atom two members admit pools against
    // each of them, since it takes those heads of both, where it pools against none;
    // census and the generated selects of lower-view-pool-cover-01 (its gen.py without
    // the last arm) first. probes: ~/ptmp/cr29/scratch/r24/24b/lvp-rev/r6.txt (run with
    // ../lvp/rb.sh) (referent-pool-exhaust-01)
    /// Pool the unguarded atom `sp` over the scrutinee's members;
    /// `explicit`, its arm's written type test.
    fn claim(
        &mut self,
        env: &Env,
        scrut: &Type,
        sp: &StructPatternNode,
        explicit: Option<&Type>,
    ) -> Result<Claim> {
        let Some(shape) = Shape::of(sp) else { return Ok(Claim::Unpooled) };
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        union_members(env, scrut, &mut members)?;
        self.claim_among(env, sp, shape, members.iter(), |i| match explicit {
            Some(t) => {
                t.contains_with_flags(ContainsFlags::RigidCheck.into(), env, &members[i])
            }
            None => Ok(true),
        })
    }

    /// Pool the unguarded atom `sp`, of `shape`, against the one of
    /// `members` of that shape it can match: one its arm's test `holds`,
    /// asked by the member's place, with a head each position admits.
    fn claim_among<'a>(
        &mut self,
        env: &Env,
        sp: &StructPatternNode,
        shape: Shape,
        members: impl Iterator<Item = &'a Type>,
        mut holds: impl FnMut(usize) -> Result<bool>,
    ) -> Result<Claim> {
        let mut reached: Option<(&Type, SmallVec<[Leaf; 8]>)> = None;
        for (i, m) in members.enumerate().filter(|(_, m)| shape.fits(m)) {
            if !holds(i)? {
                continue;
            }
            let leaves = match pooled_leaves(env, sp, m)? {
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
        let shape = shape.of_member(m);
        let i = match self.groups.iter().position(|g| g.shape == shape && g.member == *m)
        {
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
        self.flags.contains(CFlag::RunEvent).encode(buf)?;
        self.flags.contains(CFlag::SeqStep).encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        if self.arm_facts.is_none() {
            let discriminants = self.discriminants.take().unwrap_or_else(|| {
                Arc::new(ArmDiscriminants::new(
                    self.arg.node.typ().clone(),
                    self.arms.len(),
                ))
            });
            self.arm_facts = Some(LazyArmFacts::build(
                ctx,
                &self.arms,
                discriminants,
                seq_world(self.flags),
            ));
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
            guard_ran,
        } = arm_facts.as_mut().expect("arm facts built above");
        let ArmDiscriminants { shallow, scrutinee } = &**discriminants;
        let woke = slept.take();
        // A reborn select starts with no arm; the arm it held is left as a
        // deselect leaves one unless the chain takes it again.
        let held = match woke && ctx.event.born() {
            false => None,
            true => {
                entered.clear(arms.len());
                guard_ran.clear(arms.len());
                selected.take()
            }
        };
        // The guarded arms' production tags. Only guards the chain
        // consults contribute fires or bottomness.
        let mut guard_tags: SmallVec<[(usize, Tag); 4]> = SmallVec::new();
        let arg_prod = arg.update(ctx);
        if let Some(tracked) = tracked {
            tracked.observe(ctx);
            let awake = *selected;
            tracked.observe_late(ctx, |i, b| {
                i < arms.len()
                    && Some(i) != awake
                    && BoundaryRef::held_by(arm_boundary(&arms[i]), b)
            });
        }
        // Every guard whose arm's shape admits the value runs, on every
        // run of the select, wherever the chain stops: a guard keeps no
        // memory and is given nothing but its binds. One whose shape does
        // not admit the value has no binds to run on: it is paused, and
        // recomputes from what stands when it next runs.
        // An arm's shape test, which the chain below reuses.
        let guards_end = guarded.last().map_or(0, |l| l + 1);
        let mut shaped: SmallVec<[Option<bool>; 16]> = smallvec![None; guards_end];
        let live = arg.value.as_ref().filter(|_| !arg.tag.is_bottom());
        for i in 0..guards_end {
            let consumer = arms.len() + i;
            let pat = &mut arms[i].0;
            if pat.guard.is_none() {
                continue;
            }
            let reach = live.filter(|v| {
                let shallow = shallow[i]
                    .get_or_init(|| pat.shallow_discriminant(&ctx.env, scrutinee));
                let s = pat.shape_matches(&ctx.env, shallow.as_ref(), v);
                shaped[i] = Some(s);
                s
            });
            let ran = guard_ran.get(i);
            let Some(v) = reach else {
                if ran {
                    guard_ran.unset(i);
                    pat.sleep_guard(ctx);
                    if let Some(tracked) = tracked {
                        tracked.fell_asleep(consumer, ctx.rt.cycle())
                    }
                }
                continue;
            };
            guard_ran.set(i);
            let held = ran && !woke && !arg_prod.triggers() && !arg_prod.moved();
            let saved = pat.bind_tentative(ctx, v, arg_prod, held);
            let mut run = |ctx: &mut ExecCtx<'_, R>| match tracked {
                None => wake::in_body(ctx, |ctx| pat.update(ctx)),
                Some(tracked) => {
                    let scope = tracked.open(ctx, consumer);
                    let t = pat.update(ctx);
                    tracked.ran(ctx, consumer, scope);
                    t
                }
            };
            let t = if ran { run(ctx) } else { ctx.enter_body(run) };
            if let Some(t) = t {
                guard_tags.push((i, t));
            }
            pat.retract(ctx, saved);
        }
        // Any guard fire, and any guard a wake recomputed, drives a
        // re-match; whether it affects the emission is decided by the
        // consulted set below.
        let pat_up = guard_tags.iter().any(|(_, t)| t.triggers() || t.moved());
        // A bottom scrutinee bottoms the select; it consults no guards.
        // A window with no arm (a bottom scrutinee, an undecidable guard)
        // pauses the selected arm as a deselect does, so it resumes with
        // catch-up; a wake that lands in it waits for the next arm.
        if arg.tag.is_bottom() {
            if woke {
                slept.set();
            }
            if let Some(j) = selected.take().or(held) {
                deselect(ctx, tracked, j, &mut arms[j], sleep_on_deselect.get(j));
            }
            return resident.set_bottom_as(arg_prod);
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
            Some(i) if !(arg_prod.triggers() || arg_prod.moved() || pat_up || woke) => {
                ChainOut::Quiet(i)
            }
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
                    deselect(ctx, tracked, j, &mut arms[j], sleep_on_deselect.get(j));
                }
                let tag = match planes.anyfire {
                    true => Tag::FRESH_BOTTOM,
                    false => Tag::STALE_BOTTOM.moved_if(planes.moved),
                };
                return resident.set_bottom_as(tag);
            }
            ChainOut::Quiet(i) => {
                let (t, v) = evaluate_arm(tracked, &mut arms[i], i, ctx);
                planes.emit(t, v)
            }
            ChainOut::Taken(Some(i)) if *selected == Some(i) => {
                if crate::dbgenv::graphix_dbg_select() {
                    eprintln!("SELECT[{}] same-arm i={i} arg={v:?}", spec.pos);
                }
                arms[i].0.bind_event(ctx, v, arg_prod);
                let (t, v) = evaluate_arm(tracked, &mut arms[i], i, ctx);
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
                    deselect(ctx, tracked, j, &mut arms[j], sleep_on_deselect.get(j));
                }
                // A bind is a facet of its scrutinee's delivery: a stale
                // scrutinee binds stale, at a first consult, a wake or a
                // guard's flip alike.
                let bind_tag = match arg_prod.triggers() {
                    true => arg_prod,
                    false => Tag::MOVED,
                };
                arms[i].0.bind_event(ctx, v, bind_tag);
                // An arm is a body: nothing in it is born. Its first entry
                // and every later one read what stands stale, its constants
                // stand, and only its binds fire (`ExecCtx::enter_body`). A
                // pure arm, not slept when deselected since nothing in it
                // runs, is slept at a re-entry so its nodes recompute from
                // what stands. A run event's select is the seq lowering's:
                // its arm is born at every entry, as built.
                let reentry = entered.replace(i);
                if reentry && !sleep_on_deselect.get(i) {
                    let since = match tracked {
                        Some(tracked) => tracked.unseen(i),
                        None => wake::stopped(ctx),
                    };
                    wake::asleep_since(since, || arms[i].1.sleep(ctx));
                }
                let view = match (reentry, seq_world(*flags)) {
                    (true, _) if flags.contains(CFlag::RunEvent) => Some(View::Step),
                    (true, _) => Some(View::Wake),
                    (false, true) if ctx.event.view == View::Birth => Some(View::Birth),
                    (false, true) => Some(View::Step),
                    (false, false) => None,
                };
                let (t, v) = match view {
                    Some(view) => {
                        ctx.under(view, |ctx| evaluate_arm(tracked, &mut arms[i], i, ctx))
                    }
                    None => {
                        ctx.enter_body(|ctx| evaluate_arm(tracked, &mut arms[i], i, ctx))
                    }
                };
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
                    deselect(ctx, tracked, j, &mut arms[j], sleep_on_deselect.get(j));
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
        let stopped = wake::stopped(ctx);
        let mut tracked = arm_facts.as_mut().and_then(|f| f.tracked.as_mut());
        if let Some(tracked) = &mut tracked {
            tracked.all_fell_asleep(stopped);
        }
        arg.sleep(ctx);
        for (i, (pat, body)) in arms.iter_mut().enumerate() {
            let since = tracked.as_ref().map_or(stopped, |t| t.unseen(i).min(stopped));
            wake::asleep_since(since, || body.sleep(ctx));
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
            warn_constant_action(ctx, self.flags, n)?;
        }
        if let ExprKind::Select(se) = &self.spec.kind {
            for (_, body) in se.arms.iter() {
                warn_triggerless_seq(ctx, self.flags, body)?;
            }
        }
        Ok(())
    }

    // CR claude for claude: [bug/low] An instance checked again (GRAPHIX_NO_SUBST=1)
    // refuses a select whose arm the instance's type rules out where that arm reads a
    // part of its bind: `let c = 'c: Collection, 'b: (i64, i64) |x: ('c<'b>, i64)| ->
    // i64 select x { ([h, ..], n) => h.0 + n, ([<h, ..>], n) => h.0 + n, _ => 0 };
    // c(([(3, 4)], 1))` checks and prints 4, and under GRAPHIX_NO_SUBST=1 the build
    // refuses `h.0` in the list arm, "reading a tuple needs its type known here:
    // annotate the binding"; the list call alike in the array arm, and either arm alone
    // builds. The pin lang::select::nested_slice_element_bind_typed fails this way in
    // all four modes under GRAPHIX_NO_SUBST=1. base-r24 alike. Not traced: the arm the
    // instance's 'c rules out leaves its bind untyped in the second check, where the
    // definition's check typed it from 'b. An instance takes its definition's types by
    // default, so only the debug A/B splits. probes:
    // ~/ptmp/cr29/scratch/r24/24h/spom/nosubst/{ns5,ns6,ns7}.gx, ns2.gx (one arm, builds)
    // (nosubst-ruled-out-arm-bind-01)
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
            if pat.explicit_type_predicate
                && let Some(t) = pat.type_predicate.blind_named_args()
            {
                pat.type_predicate = t
            }
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
                // the scrutinee as it stands, and as its open cells' lower
                // bounds hold it: no arm decides a declared variable
                let mut views = iter::once(scrut).chain(lower.as_ref());
                let rigid = |over: &Type| -> Result<()> {
                    match sp.meets_rigid(&ctx.env, &ids, over)? {
                        None => Ok(()),
                        Some(tv) => {
                            let e =
                                rigid_refusal(&ctx.env, &ids, sp, over, &tv, Holder::Arm);
                            Err(e).at(&site())
                        }
                    }
                };
                match pat.explicit_type_predicate {
                    false => views.try_for_each(rigid)?,
                    true => {
                        let over = &pat.type_predicate;
                        rigid(over)?;
                        for view in views.filter(|v| v.has_rigid()) {
                            if let Some((tv, part)) =
                                sp.meets_rigid_written(&ctx.env, &ids, view, over)?
                            {
                                let e =
                                    written_refusal(&ctx.env, &ids, sp, view, &tv, &part);
                                return Err(e).at(&site());
                            }
                        }
                    }
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
                            let e = spec_pat.structure_predicate.no_struct_type(&scrut);
                            return Err(e).at(&self.spec);
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
        let at = checking.then_some(&*self.spec.ori);
        let mut reach = Reach::new(&ctx.env, &scrut, &self.arms, at)?;
        deciding.decided(&mut decided);
        let mut reached: SmallVec<[Reached; 16]> = SmallVec::new();
        // told after a missing case, which leaves the arms' settle short
        let mut by_order: Result<()> = Ok(());
        let spec_pats: &[_] = match &self.spec.kind {
            ExprKind::Select(se) => &se.arms,
            _ => &[],
        };
        for i in 0..self.arms.len() {
            let (pat, n) = &mut self.arms[i];
            if checking && pat.explicit_type_predicate {
                super::defer_written_map_keys(ctx, &pat.type_predicate, &self.spec)
            }
            let step = deciding.again();
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
                    .map_err(|e| {
                        let whole = &spec_pat.structure_predicate;
                        undecided_members(&ctx.env, e, &self.spec.ori, Holder::Arm, whole)
                    })
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
                reached.push(Reached {
                    atype: reaching.clone(),
                    covered: reach.covered.len(),
                });
            }
            let mut members: SmallVec<[Type; 8]> = SmallVec::new();
            let tested = checking && pat.explicit_type_predicate;
            let mut bare: SmallVec<[TVar; 2]> = SmallVec::new();
            union_members_with(&ctx.env, &reaching, &mut members, &mut |tv| {
                if tested {
                    bare.push(tv.clone())
                }
            })?;
            let takers = written_takers(&ctx.env, &outer, &bare, &pat.type_predicate)?;
            let narrowed = pat.capture_type(&ctx.env, &reaching).at(&site)?;
            reaching.narrow_pattern(&ctx.env, &narrowed)?;
            if !pat.explicit_type_predicate && pat.structure_predicate.holds_abstract() {
                reaching
                    .widen_abstracts_over_any(&ctx.env, &pat.type_predicate)
                    .at(&site)?;
            }
            // CR claude for claude: [bug/low] A refusal over an unannotated parameter
            // prints a different union from run to run, with one eval thread too: `let f =
            // |x| select x { [v13.., ([v2.., v1], _), ([v4.., v3], _), (_, [v7.., v5, v6]),
            // (v8, v9), (v10, [v11, v12])] => array::concat(v7, v7), [v1, v2] => [v1, v2],
            // [v1] => v1, v1 => [v1] }` is refused at its first arm's pattern, "type
            // mismatch Array<['_N, '_N, '_N]> does not contain Any", and about half the
            // runs print two cells, `Array<['_N, '_N]>`, under `--check`, a run and
            // `--no-fusion` alike, base-r24 alike. The verdict holds. Not looked into: what
            // decides how many cells the union keeps (a map's order, the kind
            // settle::settle_order fixed for settles, is likely). probe:
            // ~/ptmp/cr29/scratch/r24/24f/sacm/p0112.gx (select-refusal-cells-nondeterministic-01)
            pat.bind_narrowed(&ctx.env, &narrowed, &reaching, &members, checking)
                .map_err(|e| reach.untyped_binds(&ctx.env, &self.spec.ori, e))
                .at(&site)?;
            step.decided(&mut decided);
            if takers.len() > 1 && takers.iter().any(|c| c.open_cell().is_none()) {
                let t = &pat.type_predicate;
                let pat = match spec_pats.get(i) {
                    Some((p, _)) => {
                        let w = p.type_predicate.as_ref().unwrap_or(t);
                        format_compact!("{w} as {}", p.structure_predicate)
                    }
                    None => format_compact!("the test {t}"),
                };
                let e = Err(taken_by_order(&ctx.env, &pat, &takers, &self.spec));
                if reach.catch_all {
                    return e.at(&site);
                }
                if by_order.is_ok() {
                    by_order = e.at(&site)
                }
            }
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
                let mut tests: SmallVec<[Tested; 4]> = smallvec![Tested {
                    test: pat.type_predicate.clone(),
                    among: reaching.clone(),
                    open_shape: None,
                }];
                if !pat.explicit_type_predicate {
                    pat.structure_predicate.element_tests(
                        &ctx.env,
                        &pat.type_predicate,
                        &reaching,
                        &mut tests,
                    );
                }
                let spec = Arc::new(site.clone());
                let mut element_binds: SmallVec<[crate::PendingSettle; 2]> =
                    SmallVec::new();
                for Tested { test, among, open_shape } in tests {
                    element_binds.extend(open_shape.map(|shape| {
                        crate::PendingSettle::ElementBinds {
                            test: test.clone(),
                            shape,
                            elems: among.clone(),
                            spec: spec.clone(),
                        }
                    }));
                    let Ok(rest) = among.diff(&ctx.env, &test) else { continue };
                    let collision = match pat.explicit_type_predicate {
                        true => test.rep_collision_written(&ctx.env, &rest),
                        false => test.rep_collision_tested(&ctx.env, Open::Benign, &rest),
                    };
                    if let Some(why) = collision {
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
                for judgment in element_binds {
                    super::defer_judgment(ctx, judgment)
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
            if pat.type_predicate.has_unbound() {
                reaching
                    .close_over_any(&ctx.env, &pat.type_predicate)
                    .map_err(|e| {
                        let written =
                            spec_pats.get(i).map(|(p, _)| &p.structure_predicate);
                        pat.undecided_over_any(&ctx.env, written, e)
                    })
                    .at(&site)?;
            }
            let step = deciding.again();
            let covered = reach.arm(&ctx.env, &scrut, pat, &site, checking)?;
            step.decided(&mut decided);
            // an arm dead for what an earlier arm's pattern decided is refused
            // for that decision
            if let Err(Dead(e)) = covered {
                by_order?;
                deciding.left_open(&mut decided);
                deciding.rewritten(&mut decided);
                let open = still_open(&outer);
                self.check_undecided(
                    &ctx.env,
                    &open,
                    &decided,
                    &written,
                    &reached,
                    &reach.covered,
                    lower.as_ref(),
                    true,
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
                deciding.left_open(&mut decided);
                deciding.rewritten(&mut decided);
                let mut undecided = still_open(&outer);
                self.check_undecided(
                    &ctx.env,
                    &undecided,
                    &decided,
                    &written,
                    &reached,
                    &reach.covered,
                    lower.as_ref(),
                    false,
                )?;
                if let Some(e) = reach.apart.take() {
                    return Err(e);
                }
                check_repeats(&ctx.env, &self.arms)?;
                let undecided: LPooled<Vec<TVar>> =
                    undecided.drain().map(|(_, c)| c).collect();
                reach.exhausted(&ctx.env, &scrut, &undecided).at(&self.spec)?;
                by_order
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
    /// bounds held it ([`lower_view`]), less the members the unguarded arms
    /// before the arm cover whole (`covered`, [`Uncovered`]), except by the
    /// last arm where it is `dead`: nothing reaches it.
    #[allow(clippy::too_many_arguments)]
    fn check_undecided(
        &self,
        env: &Env,
        undecided: &AHashMap<usize, TVar>,
        decided: &Decisions,
        written: &Deciding,
        reached: &[Reached],
        covered: &[Type],
        lower: Option<&Type>,
        dead: bool,
    ) -> Result<()> {
        let ExprKind::Select(se) = &self.spec.kind else { return Ok(()) };
        let open = |tv: &TVar| undecided.contains_key(&tv.cell_addr());
        let was_open = |tv: &TVar| decided.cells.contains_key(&tv.stands_for());
        let held = |tv: &TVar| decided.held(tv);
        let was = Decided { cell: &was_open, held: &held };
        let mut own = Decisions::default();
        written.decided(&mut own);
        let own_open = |tv: &TVar| own.cells.contains_key(&tv.stands_for());
        let own_was = Decided { cell: &own_open, held: &|_| None };
        let unguarded = |i: usize| {
            let (pat, _) = &self.arms[i];
            pat.guard.is_none().then(|| pat.atoms())
        };
        let mut below =
            Below { lower, covered, decided, unguarded: &unguarded, left: None };
        for (i, (((pat, n), (spec_pat, _)), arm)) in
            self.arms.iter().zip(se.arms.iter()).zip(reached.iter()).enumerate()
        {
            let reaching = &arm.atype;
            let view = !dead || i + 1 < reached.len();
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
                                None => {
                                    let meets = |over: &Type| {
                                        written_tests_decided(env, t, over, decided)
                                    };
                                    below.met_beside(
                                        env,
                                        i,
                                        arm.covered,
                                        view,
                                        reaching,
                                        meets,
                                    )?
                                }
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
                    None => {
                        let meets = |over: &Type| sp.meets_decided(env, &ids, over, &was);
                        below.met_beside(env, i, arm.covered, view, reaching, meets)?
                    }
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
/// whose binding changed under the arms was decided too. A whole scrutinee open
/// before the arms is theirs to decide together, unless an arm holds a
/// partial struct pattern, whose completion has no one answer.
#[derive(Default)]
struct Deciding(SmallVec<[(TVar, Kept); 4]>);

/// How a [`Deciding`] holds a cell.
enum Kept {
    /// Open, in the scrutinee's type or a written test's.
    Open,
    /// Open, in the scrutinee's lower bounds alone.
    Below,
    /// Bound to this type, which holds open cells.
    Holder(Type),
}

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
        let held = |tv: &TVar, open: Kept| match tv.open_cell() {
            Some(c) => (!c.is_rigid() && !wholes.contains(&Some(c.cell_addr())))
                .then(|| (tv.clone(), open)),
            None => Self::holding(tv),
        };
        Self(
            outer
                .values()
                .filter_map(|tv| held(tv, Kept::Open))
                .chain(below.values().filter_map(|tv| held(tv, Kept::Below)))
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
            (!c.is_rigid()).then(|| (tv, Kept::Open))
        }))
    }

    fn holding(tv: &TVar) -> Option<(TVar, Kept)> {
        let b = tv.binding()?;
        b.has_free().then(|| (tv.clone(), Kept::Holder(b)))
    }

    /// The cells still open, for the next step.
    fn again(&self) -> Self {
        Self(
            self.0
                .iter()
                .filter_map(|(tv, held)| match held {
                    Kept::Open => tv.open_cell().map(|_| (tv.clone(), Kept::Open)),
                    Kept::Below => tv.open_cell().map(|_| (tv.clone(), Kept::Below)),
                    Kept::Holder(_) => None,
                })
                .collect(),
        )
    }

    /// Add to `decided` each binding rewritten since the arms began, as it
    /// was then, whatever rewrote it: a union an arm's body or the select's
    /// result builds may drop the member that held a cell a pattern decided.
    fn rewritten(&self, decided: &mut Decisions) {
        for (tv, held) in self.0.iter() {
            if let Kept::Holder(b) = held
                && tv.binding().as_ref() != Some(b)
            {
                decided.held.insert(tv.cell_addr(), b.clone());
            }
        }
    }

    /// Add to `decided` each cell the step decided.
    fn decided(&self, decided: &mut Decisions) {
        for (tv, held) in self.0.iter() {
            if let Kept::Open | Kept::Below = held
                && tv.open_cell().is_none()
            {
                decided.cells.insert(tv.stands_for(), tv.clone());
            }
        }
    }

    /// Add to `decided` each cell of the lower bounds the arms left open:
    /// the scrutinee's type holds its values without it, so an arm tests
    /// them as it tests those of a cell the arms decided.
    fn left_open(&self, decided: &mut Decisions) {
        for (tv, held) in self.0.iter() {
            if let Kept::Below = held
                && tv.open_cell().is_some()
            {
                decided.cells.insert(tv.stands_for(), tv.clone());
            }
        }
    }
}

/// `t` with each open cell that has lower bounds read as their union: the
/// parts an arms' settle that decides such a cell decides through its
/// bounds. `None` where `t` reaches no such cell.
pub(super) fn lower_view(t: &Type) -> Option<Type> {
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

/// The open cells of the parts built into a scrutinee cell the arms
/// decide (`lower`, its lower bounds) that the arms' settle holds as they
/// stand: those of a part one of the arms' `tests` that hold no cell holds
/// whatever its cells become, so no other test decides them by its place
/// in the arms' union, but a cell another part needs decided. With them,
/// when `checking`, the refusal of the first part no test holds so that
/// two of them could take.
fn built_standing(
    env: &Env,
    tests: &Type,
    lower: &[Type],
    checking: Option<&Origin>,
) -> Result<(OpenCells, Option<anyhow::Error>)> {
    let (mut held, mut apart) = (OpenCells::new(), None);
    if lower.is_empty() {
        return Ok((held, apart));
    }
    let mut members: SmallVec<[Type; 8]> = SmallVec::new();
    union_members(env, tests, &mut members)?;
    members.retain(|m| !m.has_unbound());
    if members.is_empty() {
        return Ok((held, apart));
    }
    let mut built: SmallVec<[Type; 8]> = SmallVec::new();
    let mut decided = OpenCells::new();
    for l in lower {
        union_members_with(env, l, &mut built, &mut |tv| decided.push(tv.clone()))?
    }
    for l in built.iter() {
        let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        crate::typ::settle::position_cells(l, &mut cells);
        let (rigid, open): (OpenCells, OpenCells) =
            cells.values().filter_map(|tv| tv.open_cell()).partition(|c| c.is_rigid());
        if open.is_empty() {
            continue;
        }
        let all: OpenCells = rigid.iter().chain(open.iter()).cloned().collect();
        // CR claude for claude: [bug/low] Arms whose tests hold a built member only
        // together do not hold its cells open, so the arm behind them is refused where
        // the plain twin runs: `|x, t: [`A, `B], c: bool| { let r = &mut (select c { true
        // => (t, x), false => (`L, 0) }); select *r { (`A, _) => 1, (`B, _) => 2, (_, 0)
        // => 3, _ => 4 } }` says "(_, 0) tests a value of 'x", and so does `(`A, true, _)`,
        // `(`A, false, _)`, `(`B, _, _)` over `(t, b, x)`; `select (select c { .. }) { ..
        // }` and the or form `(`A | `B, _) => 1` check and take any `x`. Each test is
        // asked alone below, `(`A, Any)` and `(`B, Any)` each hold half of `([`A, `B],
        // 'x)`, so `x` does not stand, the settle gives it to `(Any, i64)`, the test of
        // the arm behind, and Uncovered::pool leaves a member holding a cell the arms
        // decided alone (Member::decided), as it must: that test is what decided it.
        // Tried: the tests asked together here (their union's `contains_holding`, which
        // splits the part by its heads) runs both selects right in every mode and every
        // lang:: pin passes; not censused, not run over generated families, and not
        // looked at for a part two tests hold together where a third could take a half.
        // probes: ~/ptmp/cr29/scratch/r24/24b/lvp/x1.txt, a1.txt programs 7 and 8 (run
        // with rb.sh), tried-tests-together.patch (standing-by-tests-together-01)
        let mut stands = false;
        for m in members.iter() {
            stands = stands || m.contains_holding(env, l, &all)?
        }
        if stands {
            held.extend(open);
            continue;
        }
        decided.extend(open);
        let Some(at) = checking.filter(|_| apart.is_none()) else { continue };
        let mut takers: SmallVec<[&Type; 2]> = SmallVec::new();
        for m in members.iter() {
            if takers.len() < 2 && m.contains_holding(env, l, &rigid)? {
                takers.push(m)
            }
        }
        if let [a, b] = takers[..] {
            apart = Some(apart_refusal(env, l, [a, b], at))
        }
    }
    held.retain(|c| !decided.iter().any(|d| d.stands_for() == c.stands_for()));
    Ok((held, apart))
}

/// The refusal of `built`, which either of `takers` could take and
/// neither holds as it stands, naming the binding whose type each of its
/// open cells is.
fn apart_refusal(
    env: &Env,
    built: &Type,
    takers: [&Type; 2],
    at: &Origin,
) -> anyhow::Error {
    let mut holders = Holders::new(env, None);
    let mut written =
        |t: &Type| written_named(&mut holders, t).unwrap_or_else(|| t.clone());
    let (built, [a, b]) = (written(built), takers.map(&mut written));
    let meanings = holders.meanings(at);
    let annotate = match meanings.is_empty() {
        true => format_compact!("annotate the scrutinee's type where it is built"),
        false => format_compact!("annotate it{meanings}"),
    };
    anyhow!(
        "the arms decide this scrutinee's type from their patterns, and {built}, built \
         into it, could go to {a} or {b} of the types they test, neither of which holds \
         it as it stands: {annotate}"
    )
}

/// What reaches an arm of a select, and how many of [`Reach::covered`]
/// the arms before it cover.
struct Reached {
    atype: Type,
    covered: usize,
}

/// What reaches the arms of a select through the lower bounds of the
/// scrutinee's open cells: `lower`, the scrutinee as those bounds held it
/// ([`lower_view`]), less what the unguarded arms before an arm cover
/// (`covered`), taken out arm by arm in the arms' order.
struct Below<'a> {
    lower: Option<&'a Type>,
    covered: &'a [Type],
    decided: &'a Decisions,
    unguarded: &'a ArmAtoms<'a>,
    left: Option<Uncovered>,
}

/// The coverage atoms ([`PatternNode::atoms`]) of the arm at a place among
/// a select's arms, `None` for a guarded arm.
type ArmAtoms<'a> =
    dyn Fn(usize) -> Option<SmallVec<[(&'a StructPatternNode, Type); 4]>> + 'a;

impl<'a> Below<'a> {
    /// What `meets` finds over `reaching`, else, where the arm, the one
    /// at `arm`, is judged in the lower `view`, over that view less what
    /// the first `before` of the covers hold of it and what the arms
    /// before it pool.
    fn met_beside(
        &mut self,
        env: &Env,
        arm: usize,
        before: usize,
        view: bool,
        reaching: &Type,
        meets: impl Fn(&Type) -> Result<Option<TVar>>,
    ) -> Result<Option<TVar>> {
        if let Some(tv) = meets(reaching)? {
            return Ok(Some(tv));
        }
        let Some(lower) = self.lower.filter(|_| view) else { return Ok(None) };
        if arm == 0 {
            return meets(lower);
        }
        let (covered, decided) = (self.covered, self.decided);
        let left = self.left.get_or_insert_with(|| Uncovered::new(lower, decided));
        left.take(env, covered, before)?;
        left.pool(env, self.unguarded, arm)?;
        match left.view(lower) {
            Some(left) => meets(&left),
            None => Ok(None),
        }
    }
}

#[cfg(test)]
thread_local! {
    static HOLDS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// The members of a lower view's union that none of the first `seen` of
/// the covers holds whole, and no pool of the first `pooled` arms covers
/// (`pool`, the literal pool over these members: `(true, _)` with
/// `(false, _)` over `(bool, 'x)`), each with its cells still open, which
/// a cover and a pooled arm's test meet as one type equal only to itself:
/// no value of a member taken out reaches an arm behind the arms that
/// cover it. A cell of the decisions among the members stands whole, where
/// the walks that read it as open meet it.
struct Uncovered {
    members: SmallVec<[Member; 8]>,
    whole: bool,
    seen: usize,
    pooled: usize,
    pool: LiteralPool,
}

type OpenCells = SmallVec<[TVar; 2]>;

/// A member of a lower view that no arm has covered.
struct Member {
    typ: Type,
    /// The open cells it holds, each once.
    open: OpenCells,
    /// It holds a cell the arms decided, or a binding their decisions
    /// rewrote: no pool counts it as the type that cell took.
    decided: bool,
}

impl Member {
    fn new(typ: Type, decided: &Decisions) -> Self {
        let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        crate::typ::settle::position_cells(&typ, &mut cells);
        let mut open = OpenCells::new();
        open_among(&cells, &mut open);
        let decided = cells.iter().any(|(addr, tv)| {
            decided.held.contains_key(addr)
                || tv.open_cell().is_none()
                    && decided.cells.contains_key(&tv.stands_for())
        });
        Self { typ, open, decided }
    }
}

/// Add to `out` the open cells among `cells`, each once.
fn open_among(cells: &AHashMap<usize, TVar>, out: &mut OpenCells) {
    for tv in cells.values().filter_map(|tv| tv.open_cell()) {
        if !out.iter().any(|h| h.same_cell(&tv)) {
            out.push(tv)
        }
    }
}

/// Add to `out` the open cells `t` holds, each once.
fn open_cells(t: &Type, out: &mut OpenCells) {
    let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
    crate::typ::settle::position_cells(t, &mut cells);
    open_among(&cells, out)
}

impl Uncovered {
    fn new(lower: &Type, decided: &Decisions) -> Self {
        fn members(t: &Type, decided: &Decisions, out: &mut SmallVec<[Type; 8]>) {
            crate::stack::ensure_sufficient(|| match t {
                Type::Set(ms) => ms.iter().for_each(|m| members(m, decided, out)),
                Type::TVar(tv)
                    if !decided.cells.contains_key(&tv.stands_for())
                        && !decided.held.contains_key(&tv.stands_for())
                        && let Some(b) = tv.binding() =>
                {
                    members(&b, decided, out)
                }
                t => out.push(t.clone()),
            })
        }
        let mut ms: SmallVec<[Type; 8]> = SmallVec::new();
        members(lower, decided, &mut ms);
        let members = ms.into_iter().map(|m| Member::new(m, decided)).collect();
        Self { members, whole: true, seen: 0, pooled: 0, pool: LiteralPool::default() }
    }

    /// Whether `test`, whose open cells are `of_test`, holds `part`, the
    /// member `m` or a part of it, whatever their open cells become.
    fn holds(
        env: &Env,
        test: &Type,
        of_test: &OpenCells,
        m: &Member,
        part: &Type,
    ) -> Result<bool> {
        let mut held = of_test.clone();
        for tv in m.open.iter() {
            if !held.iter().any(|h| h.same_cell(tv)) {
                held.push(tv.clone())
            }
        }
        #[cfg(test)]
        HOLDS.with(|n| n.set(n.get() + 1));
        test.contains_holding(env, part, &held)
    }

    /// Take out the members the covers from `seen` up to `before` hold.
    fn take(&mut self, env: &Env, covered: &[Type], before: usize) -> Result<()> {
        for c in covered.iter().take(before).skip(self.seen) {
            let mut of_cover = OpenCells::new();
            open_cells(c, &mut of_cover);
            let mut i = 0;
            while i < self.members.len() {
                let m = &self.members[i];
                match Self::holds(env, c, &of_cover, m, &m.typ)? {
                    true => {
                        self.members.remove(i);
                        self.whole = false;
                    }
                    false => i += 1,
                }
            }
        }
        self.seen = self.seen.max(before);
        Ok(())
    }

    /// Take out the members the unguarded arms from `pooled` up to
    /// `before` complete a pool of. An arm pools against the one member
    /// left, none of whose cells the arms decided, that its test holds
    /// where its pattern takes it ([`StructPatternNode::headed_part`]).
    fn pool(&mut self, env: &Env, unguarded: &ArmAtoms, before: usize) -> Result<()> {
        for arm in self.pooled..before {
            for (sp, test) in unguarded(arm).into_iter().flatten() {
                let Some(shape) =
                    Shape::of(sp).filter(|_| sp.array_len_range().is_none())
                else {
                    continue;
                };
                let mut of_test = OpenCells::new();
                open_cells(&test, &mut of_test);
                let members = &self.members;
                let claim = self.pool.claim_among(
                    env,
                    sp,
                    shape,
                    members.iter().map(|m| &m.typ),
                    |i| {
                        let m = &members[i];
                        let part = sp.headed_part(env, &m.typ);
                        Ok(!m.decided
                            && Self::holds(
                                env,
                                &test,
                                &of_test,
                                m,
                                part.as_ref().unwrap_or(&m.typ),
                            )?)
                    },
                )?;
                if let Claim::Completes(m) = claim
                    && let Some(i) = self.members.iter().position(|t| t.typ == m)
                {
                    self.members.remove(i);
                    self.whole = false;
                }
            }
        }
        self.pooled = self.pooled.max(before);
        Ok(())
    }

    /// What is left of `lower`, `None` where nothing is.
    fn view<'a>(&self, lower: &'a Type) -> Option<Cow<'a, Type>> {
        match &self.members[..] {
            [] => None,
            _ if self.whole => Some(Cow::Borrowed(lower)),
            [m] => Some(Cow::Owned(m.typ.clone())),
            ms => {
                let ms = ms.iter().map(|m| m.typ.clone());
                Some(Cow::Owned(Type::Set(Arc::from_iter(ms))))
            }
        }
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

/// What a select's arms decided: the cells open before them, each by the
/// cell it stands for, and the bindings rewritten since, as they were
/// before the arms, each by its own cell: a rewrite may leave a holder
/// standing for a cell its binding held.
#[derive(Default)]
struct Decisions {
    cells: AHashMap<usize, TVar>,
    held: AHashMap<usize, Type>,
}

impl Decisions {
    /// The binding `tv` read before the arms, where a decision rewrote it:
    /// that of the first rewritten cell along its forward links.
    fn held(&self, tv: &TVar) -> Option<Type> {
        let mut tv = tv.clone();
        loop {
            if let Some(b) = self.held.get(&tv.cell_addr()) {
                return Some(b.clone());
            }
            tv = match &tv.binding() {
                Some(Type::TVar(next)) => next.clone(),
                _ => return None,
            };
        }
    }
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
                    let b = decided.held(tv).or_else(|| tv.binding())?;
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

/// The scrutinee's cells open before the arms (`outer`) and still, none
/// rigid, among `bare`, the open cells that are members of what reaches an
/// arm, that could each take the arm's written test `t` whole: none where
/// fewer than two could.
fn written_takers(
    env: &Env,
    outer: &AHashMap<usize, TVar>,
    bare: &[TVar],
    t: &Type,
) -> Result<SmallVec<[TVar; 2]>> {
    let mut takers: SmallVec<[TVar; 2]> = SmallVec::new();
    if bare.len() < 2 {
        return Ok(takers);
    }
    let open = still_open(outer);
    for c in bare.iter().filter_map(|tv| tv.open_cell()) {
        if open.contains_key(&c.cell_addr())
            && !takers.iter().any(|k| k.same_cell(&c))
            && Type::TVar(c.clone()).contains_holding(env, t, &[])?
        {
            takers.push(c)
        }
    }
    if takers.len() < 2 {
        takers.clear()
    }
    Ok(takers)
}

/// The refusal of `pat`, whose written test the narrowing gave to the
/// first of `takers` ([`written_takers`]) in the scrutinee's union,
/// naming the bindings whose types they are, each once: cells no binding
/// of their own names go by the one binding that holds their union.
fn taken_by_order(
    env: &Env,
    pat: &dyn std::fmt::Display,
    takers: &[TVar],
    select: &Expr,
) -> anyhow::Error {
    let name = |holders: &mut Holders, tv: &TVar| {
        let name = holders.name_cell(tv).unwrap_or_else(|| holders.number(tv));
        format_compact!("{name}")
    };
    // named in one order, whatever the union's
    let mut sorted: SmallVec<[&TVar; 2]> = takers.iter().collect();
    sorted.sort_by_cached_key(|tv| name(&mut Holders::new(env, None), tv));
    let mut holders = Holders::new(env, None);
    let mut names: SmallVec<[CompactString; 2]> = SmallVec::new();
    for tv in sorted {
        let name = name(&mut holders, tv);
        if !names.contains(&name) {
            names.push(name)
        }
    }
    let meanings = holders.meanings(&select.ori);
    match &names[..] {
        [name] => anyhow!(
            "{pat} tests a value of {name}, a type not decided at this select, members \
             of which could each take the test, and the check picks none by their \
             order: annotate it{meanings}"
        ),
        each => {
            let mut names = CompactString::new("");
            for (i, name) in each.iter().enumerate() {
                names.push_str(if i == 0 { "" } else { " or " });
                names.push_str(name);
            }
            anyhow!(
                "{pat} tests a value of {names}, types not decided at this select that \
                 could each take the test, and the check picks none by their order: \
                 annotate all but one of them{meanings}"
            )
        }
    }
}

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
    use super::{Below, Claim, Decisions, HOLDS, Head, Leaf, MATCHES, PoolGroup, Shape};
    use crate::{
        env::Env,
        expr::WrittenAt,
        typ::{TVar, Type},
    };
    use arcstr::{ArcStr, literal};
    use enumflags2::BitFlags;
    use netidx_value::Typ;
    use smallvec::{SmallVec, smallvec};
    use std::cell::RefCell;
    use triomphe::Arc;

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

    // n arms, each behind one more cover, over a lower view of n + 1
    // members test a member against a cover once, n^2 / 2 containments:
    // the view keeps what the covers before took out.
    #[test]
    fn lower_view_takes_each_cover_out_once() {
        let n = 200;
        let env = Env::default();
        let x = Type::TVar(TVar::empty_named(literal!("x")));
        let pair = |i: usize, t: &Type| {
            let tag = ArcStr::from(format!("T{i}"));
            let tag = Type::Variant(tag, Arc::from_iter([]), WrittenAt::NOWHERE);
            Type::Tuple(Arc::from_iter([tag, t.clone()]))
        };
        let rest = pair(n, &Type::Primitive(Typ::I64.into()));
        let lower = (0..n).map(|i| pair(i, &x)).chain([rest.clone()]);
        let lower = Type::Set(Arc::from_iter(lower));
        let covered: Vec<Type> = (0..n).map(|i| pair(i, &Type::Any)).collect();
        let decided = Decisions::default();
        let mut below = Below {
            lower: Some(&lower),
            covered: &covered,
            decided: &decided,
            unguarded: &|_| None,
            left: None,
        };
        let reaching = Type::Primitive(BitFlags::empty());
        let met: RefCell<Option<Type>> = RefCell::new(None);
        let meets = |over: &Type| {
            *met.borrow_mut() = Some(over.clone());
            Ok(None)
        };
        HOLDS.with(|h| h.set(0));
        for before in 0..=n {
            below.met_beside(&env, before, before, true, &reaching, meets).unwrap();
            let left = match met.borrow().as_ref() {
                Some(Type::Set(ms)) => ms.len(),
                _ => 1,
            };
            assert_eq!(left, n + 1 - before, "behind {before} covers");
        }
        assert_eq!(met.borrow().as_ref(), Some(&rest));
        let holds = HOLDS.with(|h| h.get());
        assert!(holds <= n * (n + 3) / 2, "{holds} containments for {n} arms");
    }
}
