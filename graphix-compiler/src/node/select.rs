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
        UndecidedMembers, union_members, union_members_open,
    },
    format_with_flags,
    fusion::emit::{BodyCx, CompiledExpr, emit_select_node},
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, put_tag},
    },
    typ::{AbstractId, InfiniteType, ShallowScrutinee, TVar, Type, TypeMismatch},
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

/// An arm nothing reaches: its refusal.
struct Dead(anyhow::Error);

// XCR claude for claude: [bug/low] A written test over an abstract parameter that is a
// declared variable covers nothing of it: `'b: (i64, string) |x: W<'b>| select x {
// W<(i64, string)> as W((n, s)) => n }` over `type W<'a> = Abstract<'a>` is refused
// "missing match cases: no arm covers W<'b>", as is `'b: [`A(i64), `B(i64)]` with the
// two tests `W<[`A(i64), `B(i64)]> as W(`A(n))` and `.. as W(`B(n))`; a catch-all
// passes and runs. D6 tells a pattern inside W<'b> to write such a test, and a written
// test covers a declared variable where it covers every member of its bound, but not
// under an abstract type's (invariant) parameter. Main eb0989d33 alike. probes:
// ~/ptmp/cr29/scratch/review-r14/B-r2-1/n/{y2,y3,y6,y7}.gx (abstract-written-cover-01)
// 2026-11-05 claude: Not taken as asked: a type test of an abstract compares its
// parameters (design/nominal_abstract_types.md item 4, Type::is_a), so W<bound> as ..
// matches only a W built at exactly the bound; with a catch-all, g(W(`A(3))) at 'b =
// `A(i64) takes the catch-all (probe ~/ptmp/cr29/scratch/r17-hk/a1.gx prints 99), so
// letting the test cover W<'b> would leave values no arm matches. The bug was the
// advice: rigid_refusal told an arm testing inside W<'b> to write W<bound> as .., and
// the skill said the same. Now an arm meeting the variable inside an abstract's
// parameter is told to bind the payload by a name and test that (`bound as ..`) or
// declare no 'b; the skill says so. Pins (lang::select):
// abstract_payload_inside_a_declared_variable_refused (fails without the fix),
// abstract_payload_inside_a_declared_variable_tested_by_name,
// abstract_written_test_at_the_bound_covers_no_instance_refused,
// abstract_written_test_at_the_bound_misses_a_narrower_instance.
/// What reaches each arm of a select, walked arm by arm: the scrutinee
/// less what the unguarded arms before it cover (irrefutable atoms,
/// `null`, a true/false pair, a completed literal pool, a completed
/// slice ladder). It narrows an arm's binds, an arm that can match none
/// of it is dead, and the select is exhaustive when nothing reaches past
/// the last arm. Quadratic in the arm count (each arm re-walks what still
/// reaches; 4000 tag arms check in about 2 s): member sets indexed by
/// head would make it linear.
// CR claude for claude: [perf/low] A select over an open part checks worse than
// quadratically in its arms, on main eb0989d33 as on cr50-P: `--no-cache --check`
// instructions over the empty program's 1.45G, a tuple scrutinee `(k, z)` with z an
// unannotated parameter and arms `(`Ki, x)`: 50 arms +0.07G, 100 +0.23G, 200 +1.0G,
// 400 +5.6G (x5.6 for x2); a never() member and `_` arms alike, a union of N structs
// holding the open cell milder (200 arms +0.55G). Not located: the Reach walk
// (`Reach::arm`'s diff of what still reaches) is the suspect the doc above names.
// probes: ~/ptmp/cr29/scratch/review-r15/P-r1-1/scale/{tup,cat,mem,un}_*.gx, results
// in scale/results.txt there (select-arm-cost-01)
struct Reach {
    /// What still reaches the next arm.
    atype: Type,
    /// Whose members the slice ladders and the literal pool count: an
    /// open cell's one typed bound, whose values it holds, else the
    /// scrutinee.
    members_of: Type,
    ladders: SmallVec<[Ladder; 4]>,
    pool: LiteralPool,
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
        Ok(Self {
            atype,
            members_of,
            ladders,
            pool: LiteralPool::default(),
            bools: (false, false),
            covered: SmallVec::new(),
            wildcard: false,
            guarded_slice: false,
            refutable_slice: false,
            built,
        })
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
            // CR claude for claude: [bug/low] A slice arm whose lengths earlier
            // arms cover is found dead only where the slice is the whole pattern:
            // inside a tuple it is not, so `|x: Array<i64>| select (x, 0) { ([a,
            // ..], _) => a, ([a, b, ..], _) => a + b, _ => 0 }` runs (5 for [5,
            // 1]) with arm 2 dead, where the same arms over x alone are refused
            // "every array length this slice pattern can match is covered by
            // earlier arms". Same on main (base-r16). Found by review r16
            // (U-r2-1). probes: ~/ptmp/cr29/scratch/review-r16/U-r2-1/q/a12v.gx,
            // a12t.gx, a12.gx (slice-ladder-nested-01)
            if let Some((k, exact)) = sp.array_len_range() {
                let (mut any, mut all) = (false, true);
                for l in self.ladders.iter() {
                    if at.could_match(env, &l.member)? {
                        any = true;
                        all &= l.covers(k, exact);
                    }
                }
                if checking && any && all {
                    return dead(
                        "every array length this slice pattern can match is covered by earlier arms",
                    );
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
                Claim::Dead if checking => {
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
}

/// What pooling an atom decided.
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
                && !t.contains_with_flags(BitFlags::empty(), env, &m)?
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
                });
                self.groups.len() - 1
            }
        };
        let g = &mut self.groups[i];
        if g.done {
            return Ok(Claim::Open);
        }
        if !g.arms.is_empty() && g.covers(&leaves) {
            return Ok(Claim::Dead);
        }
        g.arms.push(leaves);
        g.done = g.covers_all();
        Ok(match g.done {
            true => Claim::Completes(m),
            false => Claim::Open,
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

impl PoolGroup {
    /// The positions some arm (or `extra`) tests a head at, with the
    /// heads the member admits there; `None` when one is not a finite
    /// set, or there are too many combinations.
    fn positions<'a>(
        &'a self,
        extra: Option<&'a [Leaf]>,
    ) -> Option<SmallVec<[(&'a [u32], &'a [Head]); 8]>> {
        let mut out: SmallVec<[(&[u32], &[Head]); 8]> = SmallVec::new();
        let mut total = 1usize;
        let arms = self.arms.iter().map(|a| &a[..]).chain(extra);
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

    /// Some arm matches every combination the member admits.
    fn covers_all(&self) -> bool {
        let Some(positions) = self.positions(None) else { return false };
        let nested = Self::nested(&positions);
        Self::combinations(&positions)
            .all(|c| self.arms.iter().any(|a| Self::matches(a, &positions, nested, &c)))
    }

    /// Earlier arms match every combination `arm` matches.
    fn covers(&self, arm: &[Leaf]) -> bool {
        let Some(positions) = self.positions(Some(arm)) else { return false };
        let nested = Self::nested(&positions);
        Self::combinations(&positions)
            .filter(|c| Self::matches(arm, &positions, nested, c))
            .all(|c| self.arms.iter().any(|a| Self::matches(a, &positions, nested, &c)))
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
        if checking {
            crate::typ::settle::position_cells(self.arg.node.typ(), &mut outer)
        }
        // the cells the arms' patterns decide, each by the cell it stands for
        let mut decided = Decisions::default();
        let deciding = match &self.spec.kind {
            ExprKind::Select(se) if checking => {
                Deciding::new(&outer, self.arg.node.typ(), &se.arms)
            }
            _ => Deciding::default(),
        };
        if checking && let ExprKind::Select(se) = &self.spec.kind {
            let scrut = self.arg.node.typ();
            for ((pat, n), (spec_pat, _)) in self.arms.iter().zip(se.arms.iter()) {
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
                            why.untold("this pattern", &test, &rest)
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
                self.check_undecided(&ctx.env, &still_open(&outer), &decided, &reached)?;
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
                self.check_undecided(&ctx.env, &undecided, &decided, &reached)?;
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
    /// the one the coverage found dead), or any open cell of a written test.
    fn check_undecided(
        &self,
        env: &Env,
        undecided: &AHashMap<usize, TVar>,
        decided: &Decisions,
        reached: &[Type],
    ) -> Result<()> {
        let ExprKind::Select(se) = &self.spec.kind else { return Ok(()) };
        let open = |tv: &TVar| undecided.contains_key(&tv.cell_addr());
        let was_open = |tv: &TVar| decided.cells.contains_key(&tv.stands_for());
        let held = |tv: &TVar| decided.held.get(&tv.stands_for()).cloned();
        let was = Decided { cell: &was_open, held: &held };
        for (((pat, n), (spec_pat, _)), reaching) in
            self.arms.iter().zip(se.arms.iter()).zip(reached.iter())
        {
            let sp = &spec_pat.structure_predicate;
            let ids = CompiledAbstracts::new(&pat.structure_predicate, sp);
            let ids = |p: &StructurePattern| ids.get(p);
            let e = match pat.explicit_type_predicate {
                true => {
                    let t = &pat.type_predicate;
                    match sp.meets_undecided(env, &ids, t, &|_| true)? {
                        Some(tv) => {
                            Some(format_with_flags(PrintFlag::DerefTVars, || {
                                anyhow!(
                                    "{sp} tests a value of '{}, a type variable its type test \
                                 {t} leaves open: write the type it tests",
                                    tv.name
                                )
                            }))
                        }
                        None => {
                            match written_tests_undecided(env, t, reaching, undecided)? {
                                Some(tv) => Some(tv),
                                None if decided.cells.is_empty() => None,
                                None => written_tests_decided(env, t, reaching, decided)?,
                            }
                            .map(|tv| {
                                let pat = format_compact!("{t} as {sp}");
                                undecided_refusal(env, &pat, &tv, &self.spec.ori)
                            })
                        }
                    }
                }
                false => match sp.meets_undecided(env, &ids, reaching, &open)? {
                    Some(tv) => Some(tv),
                    None if decided.cells.is_empty() => None,
                    None => sp.meets_decided(env, &ids, reaching, &was)?,
                }
                .map(|tv| undecided_refusal(env, sp, &tv, &self.spec.ori)),
            };
            if let Some(e) = e {
                return Err(e).at(&pattern_site(spec_pat, n.spec()));
            }
        }
        Ok(())
    }
}

// CR claude for claude: [bug/low] A cell the scrutinee holds only through a lower
// bound is not one Deciding notes (settle::position_cells walks no bounds), so the
// first arm decides it with no D5 refusal: `|x| { let r = &mut (x, 0); select *r {
// (`J, _) => 1, (`M, _) => 3 } }` is accepted with arm 2 dead and `f(`M)` refused
// "type mismatch `J does not contain `M"; with a `_` arm, "unreachable arm". The same
// through an abstract constructor's parameter, `select W((x, 0)).0`. The annotated
// twin runs. Same on main (base-r16): D5's undecided cells come from the same walk.
// Found by review r16 (U-r1-1). probes: ~/ptmp/cr29/scratch/r16-U/review-fix/p/
// lb4.gx, lb5.gx, lb1.gx, lb2.gx; twin lb7.gx (undecided-lower-bound-01)
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
    fn new(
        outer: &AHashMap<usize, TVar>,
        scrut: &Type,
        arms: &[(Pattern, Expr)],
    ) -> Self {
        let whole = match scrut {
            Type::TVar(tv)
                if !arms.iter().any(|(p, _)| {
                    p.type_predicate.is_none()
                        && p.structure_predicate.has_partial_struct()
                }) =>
            {
                tv.open_cell().map(|c| c.cell_addr())
            }
            _ => None,
        };
        Self(
            outer
                .values()
                .filter_map(|tv| match tv.open_cell() {
                    Some(c) => (!c.is_rigid() && Some(c.cell_addr()) != whole)
                        .then(|| (tv.clone(), None)),
                    None => Self::holding(tv),
                })
                .collect(),
        )
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

// XCR claude for claude: [question/low] D5 (b) refuses an arm that tests a part still
// undecided at the select's end, but an arm may decide the part first, and then nothing
// is undecided to refuse: the arms decide what they will. So `(_, {a, ..})` over
// [(`K, {a: Array<i64>, c: i64}), (`J, 'z)] completes against the K member's struct and
// decides z := {a: _, c: i64}, even where an earlier arm took the K member whole, and a
// later `z <- {a: ["s"], c: "w"}` is refused at the write ("z is { a: Array<string>, c:
// i64 }, inferred from an earlier use"), a type the program never gave z; a lone open
// cell under `{a, ..}` becomes {a: '_} ("does not contain {..}" over a parameter, naming
// nothing); `(`J, _) | (_, 1)` decides z := i64 where the arms written apart pass; and a
// tag test `(`J, _)` over (('w, ..)) decides w := `J, so the next arm `(`M, _)` is
// "will never match", naming neither w nor `let w: [`J, `M, `L]`. All sound (refused,
// no wrong value) and main eb0989d33 alike. Should an arm that decides an undecided part
// (a completion, a tag, a literal in an or-alternative) be refused as testing it,
// naming the binding, rather than decide it? probes:
// ~/ptmp/cr29/scratch/review-r15/P-r2-1/{d3,t3,g1,n2,n3,or5,w6,w8}.gx (arm-decides-undecided-01)
// 2026-11-05 claude: Eric ruled (2026-11-05): (b). An arm whose pattern would decide an
// undecided part of the scrutinee (a completion, a tag, a literal, an or-alternative) is refused
// as testing it, with the D5 message naming the binding to annotate; a whole open scrutinee the
// arms decide together stays as it is. Census first. Analysis and probes: ~/ptmp/cr-discuss.md
// (Ruled 11-05, D5 follow-up); ~/tmp/d5/.
// 2026-11-05 claude: Built (lane U, round 16): the cells the scrutinee held open are noted
// before each step that runs a pattern (Reach::new's settle and the completions before the arms,
// each arm's completion and narrowing; never a guard or a body), and a cell such a step bound
// counts as undecided (Deciding; StructurePattern::meets_decided walks it as the open cell it
// was; written_tests_decided judges a written test over a copy with those cells open again). A
// refusal the arms' check makes is first offered to this rule, so t1, t6, t9, w6 and e1 name the
// binding instead of 'unreachable arm'. A whole open scrutinee stays the arms' to decide
// together unless an arm holds a partial struct pattern (n2, n3 refused). Pins (each fails
// without the fix): lang::select_undecided::{tag_deciding_*, element_tag_*, payload_tag_*,
// written_test_deciding_*, partial_struct_completing_*, partial_struct_over_a_lone_*,
// partial_struct_over_an_undecided_parameter_refused, or_alternative_deciding_*,
// slice_over_an_undecided_part_refused, undecided_member_beside_a_guarded_tag_refused,
// widened_open_element_refused}, lang::select::{an_open_part_a_slice_arm_would_shape_refused,
// an_open_field_*, a_later_written_part_*,
// partial_struct_after_a_type_test_deciding_the_first_member_refused}; annotated twins and kept
// cases beside them; must-reject family 14's decide shape (graphix-fuzz
// must_reject_families_are_refused_where_their_rules_say: LEAK without the fix). Census of both
// repos: no change.
// 2026-11-05 claude: Review r16 (U-r1-1) fix: a refusal in the arms' loop is now offered to the
// rule over every arm, an arm not checked yet judged against the whole scrutinee, since the
// settle (Reach::new) runs every pattern before any arm's guard or body: g7 (`(a, _) if b =>
// select a { `J => 1, `M => 2 }, (`M, _) => 3, _ => 4` over (x, 0)) is refused naming x for arm
// 2 instead of "pattern `J will never match `M". Pins
// lang::select_undecided::later_arm_deciding_a_part_an_earlier_body_tests_refused (fails x4
// without the fix) and its annotated twin
// later_arm_over_an_annotated_part_an_earlier_body_tests. Left by judgment: a guard or body
// deciding a part a pattern tests keeps main's message (the ruling names patterns); a cell held
// only through a lower bound (undecided-lower-bound-01) and a decided cell inside a binding's
// type (undecided-part-name-01) are new CRs, main sharing both.
// 2026-11-05 claude: For Eric (rule question, not changed): the ruling's letter also refuses,
// over an unannotated parameter, working programs main ran: exhaustive bool literals (true, _) /
// (false, _); a literal beside a catch-all (a, 0) => a, (a, _) => a; irrefutable destructuring
// ((a, b), _), ({a, b}, _), ([], _), a list head; an or-pattern (a, 0) | (_, a); an irrefutable
// abstract pattern (W((p, q)), _) over (x, 0) with type W = Abstract<(i64, i64)> (names x); and
// a cell no binding owns, an empty array literal's element in select (x, []) { (0, [`J]) => 1, _
// => 2 } (the nameless message, annotate the scrutinee where it is built). A guard deciding the
// same part passes. None is in the census of both repos. probes:
// ~/ptmp/cr29/scratch/r16-U/review-fix/p/,
// ~/ptmp/cr29/scratch/review-r16/U-r2-1/p/{e1,e8,e9}.gx.
// 2026-11-05 claude: Eric ruled (11-05, D5 follow-up 2): (b), irrefutable structure is exempt.
// Built (lane U, round 16 part 2): an arm whose pattern at a decided part only takes apart every
// value of the shape decided (a tuple, a struct naming every field, a slice or list with a rest,
// an abstract payload, with only names, _ and such patterns:
// StructurePattern::only_destructures, destructures) decides the shape as an annotation would
// and is not refused; literals, tags, null, [], a fixed-length slice, an or-pattern, a partial
// struct and a written test stay refused. A later arm of another shape than the one decided is
// refused naming the binding (may_hold_met reads decided cells as open; the arm a refusal stops
// at is judged against the whole scrutinee). Also the literal at a union position main and lane
// missed ((null, _) over y = [x, null]): deciding x rewrote y's binding to the literal's member,
// hiding x; Deciding now notes the scrutinee's bound cells whose bindings hold open cells and
// Decisions::held walks a rewritten one as it was, and Reach::arm (coverage) counts as a pattern
// step. Pins: lang::select_undecided::{destructuring_tuple_*, destructuring_struct_*,
// destructuring_slice_with_a_rest_*, destructuring_slice_suffix_*, destructuring_list_*,
// destructuring_abstract_payload_*, destructuring_nested_capture_*} (run as on main; fail x4
// without the exemption), {bool_literals_*, literal_*, empty_slice_*, fixed_length_slice_*,
// list_head_*, or_pattern_*, partial_struct_deciding_*,
// other_shape_than_an_earlier_arm_decided_refused} with annotated twins,
// {tag,null}_at_a_union_position_deciding_an_undecided_member_refused (fail x4 without the
// holder snapshot) and its annotated twin; main's lang::select::settle_shapes_* restored.
// Must-reject family 14 emits its decide shape only where an arm chooses a value. Census of both
// repos: no change. Judged: a slice of fixed length (d10, [a, b]) stays refused as a length
// choice; a partial struct stays refused (its completion is the struct of the fields it names).
// probes: ~/ptmp/cr29/scratch/r16-U2/p/
// 2026-11-05 claude: Review r16 (U-r1-1 part 2) medium: a destructure at a union position beside
// a member of its shape, ((a, b), _) over y = [x, (i64, i64)], then _ => 0, said unreachable arm
// (as main). Fixed where x stays open (t1, t1n, t2 struct, t4 whole holder): the step a refusal
// stops at now judges the arms before it against the cells still open as well as those decided
// (select.rs still_open), so these get D5's refusal naming x, the one main and lane gave without
// the later arm (b1). Pins
// lang::select_undecided::{destructure_beside_an_undecided_member_before_a_dead_arm_refused,
// struct_beside_an_undecided_member_before_a_dead_arm_refused,
// whole_holder_destructured_before_a_dead_arm_refused} (fail x4 without), twin
// destructure_beside_an_annotated_member. Still open: where the coverage decides x by merging it
// into the member (t3 abstract W(a); t5 the ladder ((a, b), 0), ((a, b), _), _), the exemption
// holds and the later _ is unreachable, as it is beside no member (a1: |x| select (x, 0) { ((a,
// b), _) => .., _ => 0 }) and as under the annotation. Refusing a destructure beside a member of
// its shape (tried: StructurePattern::meets, the rigid loop) also refuses t7, ([a, ..], _) => a,
// _ => 0 over [x, Array<i64>], which main and the lane run (5) and the ruling's letter exempts:
// a rule the ruling does not state, left for Eric. probes:
// ~/ptmp/cr29/scratch/review-r16/U-r1-1/part2/p5/; the tried diff
// ~/ptmp/cr29/scratch/r16-U3/pattern-beside.diff
// 2026-11-05 claude: Review r16 (U-r2-1) found that judging every arm against the still-open
// cells when a step fails made an ordinary body error a false D5 refusal of a later arm (e4:
// (`K, n) => str::len(n) reported as '(_, 3) tests a value of 'z'). Now the failing step judges
// the arms before it, by what reaches each, against the still-open cells, and every arm (from
// the failing one on as reached by the whole scrutinee) against the decided cells only, as
// before. Pins lang::select_undecided::a_body_error_before_a_later_arm_is_the_error (fails x4
// without) and its twin a_later_arm_after_a_whole_member_taken; t1/t1n/t2/t4 keep D5's refusal.
// Both reviewers again report t3 (W(a) at a tuple position) and t5 (the ladder) giving
// 'unreachable arm', as main does: still waiting on Eric's ruling on a destructure beside a
// member of its shape (second review in a row; not lane work).
// 2026-11-05 claude: Round 16 part 3 (lane U), the failure path: a step of the arms' check that
// fails with its own error (a pattern's, a guard's, a body's) returns that error and makes no D5
// judgement; the judgement runs once every arm has checked, or where the coverage finds an arm
// dead (Reach::arm), which is the refusal D5 explains, and then judges the arms up to it against
// the cells still open and those decided, as at the end. Gone: the arm loop's closure,
// reached.truncate, the separate still-open judgement on failure, and padding later arms with
// the whole scrutinee. k2, k6, e5 (review r16 U-r3) report their own errors as main does; t1,
// t1n, t2, t4 keep naming x. Main's messages are back for g7 (a body's nested select: pattern `J
// will never match `M), another-shape arm after a destructure (unreachable arm), a body-decided
// element under a later literal (type mismatch), and a partial struct after a written test (two
// members). Pins: lang::select_undecided::{a_body_error_after_a_literal_arm_is_the_error,
// a_guard_error_after_a_literal_arm_is_the_error,
// a_body_error_after_a_destructuring_arm_is_the_error} (each fails x4 with 338e2916f's loop),
// twins arms_after_a_literal_arm_run, arms_after_a_destructuring_arm_run; rewritten:
// an_earlier_body_over_a_part_a_later_arm_decides_is_its_error,
// other_shape_than_an_earlier_arm_decided_is_dead, widened_open_element_refused,
// lang::select::partial_struct_after_a_type_test_taking_a_decided_member.
// 2026-11-05 claude: Round 16 part 3 (lane U), t3 and t5 (Eric's D5 follow-up 2 (b), per the
// round's brief): a destructure is exempt only where it takes apart every value of the shape it
// decides; at a position where the decided cell stands beside a member of that shape it chooses
// between them, and is refused with D5's message naming the cell (StructurePattern::meets, the
// decided-cell loop), whether the cell stays open (t1, t2, t4, as before) or the coverage merges
// it into the member: t3 (W(a) beside W), t5 (the ladder ((a, b), 0), ((a, b), _)), and so t7
// ([a, ..] beside Array<i64>, which main runs), qt3n, qt5n, qt5w. d10 ([a, b]) stays refused. In
// the same walk (destructures' fits), an open cell inside the decided shape, the arm's own,
// takes the shape a destructure there gives it: v38/v40 ([(a, b), ..] over x, element type ['r,
// ('a, 'b)]) are no D5 test, and are refused by the runtime-form rule as on main. Pins:
// lang::select_undecided::{abstract_payload_beside_an_undecided_member_refused,
// destructure_ladder_beside_an_undecided_member_refused,
// slice_with_a_rest_beside_an_undecided_member_refused,
// destructure_under_a_slice_element_is_no_undecided_test} (16/16 fail without), annotated twins
// abstract_payload_beside_an_annotated_member, destructure_ladder_beside_an_annotated_member,
// slice_with_a_rest_beside_an_annotated_member, destructure_under_an_annotated_slice_element;
// must-reject family 14's beside shape (graphix-fuzz
// must_reject_families_are_refused_where_their_rules_say, the W(_) subject, fails without).
// 2026-11-05 claude: Review r16 part 3 (U-r1-1) low: the beside-shape rule follows where the
// union sits: y = select c { true => (x, 1), false => ((1, 2), 1) } with select y { ((a, b), _)
// => a + b } (i4c) and i4b over (y, 0) are accepted, since x has no member of its shape beside
// it at its own position inside the tuple member, while y = x or (1, 2) with select (y, 0) {
// ((a, b), _) => .. } (i4f) is refused naming x. Not unsound (the arm decides x as a pair and no
// later use decides it apart); main (base-r16) has the same split. probes:
// ~/ptmp/cr29/scratch/review-r16/U-r1-1/r16U3/p/i4b.gx, i4c.gx, i4f.gx
// 2026-11-05 claude: Review r16 part 3 (U-r2-1 low): Reach::arm now returns a dead arm as
// Ok(Err(Dead)) and any other failure (a type walk's ?) as Err, so only a dead arm gets the D5
// judgement; an internal error is the select's own. No program found reaches the internal
// errors, so no pin; lang:: (11536) passes unchanged.
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

// CR claude for claude: [bug/low] The D5 refusal names no binding when the
// undecided cell is a part of a binding's type rather than all of it (Holders names
// a binding only by the cell its whole type is): `|x| select (x[0]$, 0) { (`J, _) =>
// 1, _ => 2 }` and `|x| select (W(x), 0) { (W(`J), _) => 1, _ => 2 }` are refused
// "tests a value whose type is not decided at this select ... annotate the
// scrutinee's type where it is built", where the ruling asks to name the binding to
// annotate (x). On main these were "unreachable arm"; main's D5 message for a part
// still open at the select's end names through the same Holders. Found by review r16
// (U-r1-1).
// probes: ~/ptmp/cr29/scratch/r16-U/review-fix/p/idx1.gx, abs1.gx
// (undecided-part-name-01)
// 2026-11-05 claude: Review r16 (U-r1-1 part 2) found the same root through pattern
// binds: after an exempt destructure decides x as ('p, 'q), a nested select in the
// arm's body that tests one of those part cells by value is refused with the
// nameless message, where the annotation that passes is on x: ((a, b), 0) => .., (c,
// _) => select c { (1, d) => d, _ => 0 } (n01), (c@ (a, b), _) => select c { (1, d)
// => .. } (n02), ((a, b), _) => select (a, 0) { (`J, _) => 1, _ => 0 } (n04). main
// ran n01 (6) and n02 (0) and said unreachable arm for n04; the let-destructuring
// analog n05 names a. Holders would have to reach a cell through the pattern binds
// that took it apart. None is in the census. probes:
// ~/ptmp/cr29/scratch/review-r16/U-r1-1/part2/p4/n01.gx, n02.gx, n04.gx, n05.gx;
// p4.out
// 2026-11-05 claude: Review r16 part 3 (U-r1-1): two more instances: nm1 (the t5
// ladder over a cell that is part of y's type, y = x[0]$ or (1, 2), arms ((a, b),
// 0), ((a, b), _), _), refused by the beside-shape rule with the message that names
// no binding, where the annotation goes on x; and x8 (a part a pattern bind took
// apart, ((a, b), _) => select (a, 0) { (1, _) => .. }), refused the same nameless
// way, where main runs it and prints 2. probes:
// ~/ptmp/cr29/scratch/review-r16/U-r1-1/r16U3/p/nm1.gx, x8.gx
/// The refusal of `pat`, which tests a value of `tv`, a cell not decided
/// at its select, naming the binding whose type it is.
fn undecided_refusal(
    env: &Env,
    pat: &dyn std::fmt::Display,
    tv: &TVar,
    at: &Origin,
) -> anyhow::Error {
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
        None => anyhow!(
            "{pat} tests a value whose type is not decided at this select, which a \
             later use could decide apart from what the arms test: annotate the \
             scrutinee's type where it is built"
        ),
    }
}
