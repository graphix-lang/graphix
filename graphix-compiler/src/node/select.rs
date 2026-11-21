use super::{
    Held, WakeBit,
    compiler::compile,
    pattern::{ArmMatch, PatternNode},
    solving,
    wake::{self, ArmMask, TrackedFires},
};
// CR claude for claude: [style/low] `crate::typ::settle::position_cells` is written by
// path ten times in this file and `crate::stack::ensure_sufficient` fourteen: import
// both. Five of the `position_cells` calls are one walk, a pooled map filled and
// filtered to its open cells (`Select::typecheck0_with` for an arm's own cells,
// `Deciding::hold_written`, `built_standing`, `later_cells`,
// `written_tests_undecided`), which `open_cells` and `open_among` below already are:
// call them. Found by reading; not run. (c-select-seq-09)
use crate::{
    BindId, CFlag, CompileCtx, ExecCtx, Node, NodeView, Refs, Rt, Scope, Tag, TagValue,
    Update, View,
    env::Env,
    expr::{At, Expr, ExprId, ExprKind, Pattern},
    fusion::emit::{BodyCx, CompiledExpr, emit_select_node},
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, put_tag},
    },
    prune::BoundaryRef,
    typ::{
        ShallowScrutinee, TVar, Type,
        bounds::{self, Arm, Use, unsplit},
    },
    wrap,
};
use anyhow::{Result, anyhow};
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
    /// The seq machine's tracker, for a select of its world
    /// ([`seq_world`]); a body's select holds none.
    tracked: Option<TrackedFires>,
    /// Per arm: sleep on deselect? False only for a pure non-recursive
    /// body, which is neither slept nor updated while untaken.
    sleep_on_deselect: ArmMask,
    /// Per arm: entered before. A first entry is the arm's birth, every
    /// later one a resume.
    // CR claude for claude: [readability/low] This doc says a first entry is the arm's
    // birth; `Select::update` says, and does, the opposite for a body's select (nothing
    // in an arm is born; only a select of the seq machine's world births an arm), so
    // the bit decides the late sleep of a pure arm and a step's select's view and
    // nothing else. The same stale model is in `Select::update`'s note on a window with
    // no arm ("so it resumes with catch-up": a body's arm is owed nothing) and in
    // seq_machine.rs's module doc ("with `Select`'s catch-up": the catch-up is
    // `TrackedFires`', which a body's select does not hold), and `seq_world`'s "stays
    // as built" is history. Say what the bit decides and drop the rest. Found by
    // reading; not run. (c-select-seq-05)
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
// CR claude for claude: [structure/low] The tracker's bracket around one evaluation
// (`deliver`, `open`, the update, `ran`, `restore`, `evaluated`) is written out here,
// in seq_machine.rs's `evaluate` and in callsite.rs's `wake` over an absent call's
// captures, and a guard's run in `Select::update` opens and closes without the rest. A
// copy that drops `restore` leaves catch-up fires in the event for the rest of the
// cycle, and nothing but reading the copies shows they agree. Make it one method of
// `TrackedFires` that takes the consumer, its refs and the update as a closure and
// returns the update's result. Found by reading; not run. (c-select-seq-06)
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
    let changed = tracked.ran(ctx, i, scope);
    TrackedFires::restore(ctx.event, injected);
    tracked.evaluated(ctx, i, changed, |r| arm_refs(arm, r));
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
// CR claude for eric: [bug/low] This warning runs only over select arms
// (SelectNode::typecheck0). A collection callback's slot is the same kind of body
// (design/bind_driven_bodies.md): a raise of a constant, a `<-` of a constant or an
// effectful call with constant arguments in an array::map callback acts only in the
// slots born in the program's first cycle. It silently never acts in a slot added
// later, and the check says nothing. In the probe, slots 1..3 raise and print nothing,
// and --check is silent. Should a collection whose source is not a constant run this
// and warn_triggerless_seq over its callback body, with a message about slots added
// after the first cycle? A constant source has every slot born then, so warning there
// would be noise. probe:
// design/review-2026-11-16/repro/x-rerun-4-slot-constant-no-warning.gx (x-rerun-4-04)
// CR claude for eric: [structure/high] The check warns of a constant action in an arm
// but not of the commonest handler mistake: a lambda that writes or performs Io and
// never reads a named parameter, so nothing in it is driven by the call. In the probe
// `let save = |c| select valid { .. }` shows its error text in the first cycle and
// writes the file as soon as the form is valid, with no press at all. Warn where a
// lambda whose `LambdaDef` facts say it writes or performs reads none of its named
// parameters, and let `|_|` silence it; census both repos first. probe:
// design/review-2026-11-16/repro/a-language-handler-ignores-event.gx (a-language-02)
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
    // CR claude for claude: [readability/low] This warning says a write reading no
    // variable "acts only if this arm is taken in the program's first cycle". A call a
    // seq step makes is born again every run (SKILL.md), so an arm it takes writes each
    // run: in the probe, `select m { `A => x <- 0, `B => x <- 1 }` in a function called
    // from a seq step writes at n = 3 and n = 6 after two warnings saying it cannot.
    // Word it for the rule as built ("taken as its body is born: the program's first
    // cycle, or a call a seq step makes"), here and in warn_triggerless_seq. probe:
    // design/review-2026-11-16/repro/x-diagnostics-arm-warning-seq-call.gx
    // (x-diagnostics-10)
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
        // CR claude for claude: [bug/low] The birth warning misses a write through a
        // reference. An inline `*mr <- 6` gets no warning, because there is no
        // ConnectDeref arm beside this one. A call to a function doing `*mr <- v` gets
        // none either: analysis::node_facts gives a ConnectDeref STATEFUL, never EVENT,
        // so acts_on_the_world takes it as acting on nothing. Both writes act only in
        // the program's first cycle (count(m) stays 2 while the arm is entered at n =
        // 0, 2, 4), where `m <- 6` and a callee doing `m <- v` warn. Add a ConnectDeref
        // arm that warns when its right side reads nothing, and make a ConnectDeref
        // EVENT under OwnTargets::Bound, since it may write any binding. probe:
        // design/review-2026-11-16/repro/x-rerun-5-birth-warn-ref-write.gx
        // (x-rerun-5-06)
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
            // CR claude for claude: [perf/low] Every update of the select runs
            // `shape_matches` for each guarded arm, also when the scrutinee stands, and
            // a test that needs the deep `is_a` walk costs the size of the value each
            // time: a guarded arm with a written test over a standing 300000-element
            // array takes 1.8 ms per update of the select in the node-walk (6.9 s
            // against 1.7 s for the twin without the guard, 3000 updates). Where the
            // scrutinee neither fired nor moved and the select did not wake, the
            // verdict is `guard_ran.get(i)`: read it instead of testing again. probe:
            // design/review-2026-11-16/repro/c-select-seq-guard-shape-test-every-update.sh
            // (c-select-seq-04)
            // 2026-11-19 claude: Moved here unchanged with the old select check
            // (Select::typecheck0_with, its coverage walk Reach and ladders, and the
            // pattern helpers only it used), deleted on bounds-spike once every definition
            // is checked by the solver; re-probe under check_by_bounds before closing.
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
        self.check_by_bounds(ctx)?;
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

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        match types.aux(self.spec.id) {
            Some(rows) => self.typecheck0_rows(ctx, types, &rows),
            // a select its definition's check left no row (an id two nodes
            // share) is checked as a program's are, in a check of its own
            None => {
                let check = bounds::Check::begin();
                self.check_by_bounds(ctx).and_then(|()| check.end(&ctx.env))
            }
        }
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        self.arg.node.typecheck1(ctx)?;
        // the check decided what reaches each arm after the patterns were
        // built: they take their places from what the arm tests for now
        for (pat, n) in self.arms.iter_mut() {
            let tested = match pat.explicit_type_predicate {
                true => bounds::written_test(&pat.type_predicate),
                false => pat.type_predicate.resolve_tvars(),
            };
            if !tested.is_uninhabited() {
                wrap!(n, pat.structure_predicate.realign_decided(&ctx.env, &tested))?
            }
            pat.type_predicate = tested
        }
        for (pat, n) in self.arms.iter_mut() {
            if pat.explicit_type_predicate
                && let Some(t) = pat.type_predicate.blind_bottoms()
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
    /// The check by bounds: the select is one use of its scrutinee. Each
    /// arm's guard and body are checked with the pattern's names as cells,
    /// and the solver routes each type that reaches the scrutinee down the
    /// arms, to those cells.
    // CR claude for claude: [bug/medium] [bug/medium] For a declared variable this
    // branch says it "isn't known here ... annotate it", but the author already wrote
    // and bounded it. `'b: &string |x: [&i64, 'b]| select x { &i64 as r => 1, _ => 2 }`
    // is told to annotate (and the variable prints as "'b: unbound within &string"),
    // while `|x: [&i64, &string]|` gets the right advice, "wrap them in distinct
    // variants". Read a rigid declared variable as its bound, so a collision with the
    // bound is the ordinary untold refusal. For an unbounded one, say "bound 'b by a
    // type of another runtime form", as the "tests inside a value of 'b" refusals do.
    // The same test and message are copied in `discernible` and in
    // `Type::bound_refusal` (contains.rs); one method on `Indiscernible` would carry
    // the fix. Six lang::select fixtures (the open_member_* tests and
    // select_over_a_bounded_quantifier_still_tells_forms) pin the bare "annotate it",
    // and only the error case has a passing twin. probe:
    // design/review-2026-11-16/repro/tests-select-b-declared-variable-form-says-annotate.gx
    // (tests-select-b-02)
    // 2026-11-19 claude: On bounds-spike the old settle drain's told(), where this judgment
    // was made, is deleted with the old check (every module ran the same with the drain
    // off); a select's tests are the solver's (check_by_bounds), where this moved. Re-probe
    // under the new check before closing.
    // 2026-11-19 claude: Moved here unchanged with the old select check
    // (Select::typecheck0_with, its coverage walk Reach and ladders, and the pattern
    // helpers only it used), deleted on bounds-spike once every definition is checked by
    // the solver; re-probe under check_by_bounds before closing.

    // CR claude for claude: [bug/medium] This refusal ends with "annotate it"
    // and prints only generated cells, so it names nothing the program wrote.
    // Over `|a| select a { [`Keep(r), ..] => r, _ => null }` it reads "tests
    // for `Keep('_N) among '_M ... annotate it", but the fix is to annotate the
    // parameter `a`. Over `|v: 'a| select v { Array<'b> as xs => xs, _ =>
    // never() }` the fix is a bound on 'a, which no annotation gives. The
    // pattern.rs refusals of this family name the binding or the bound ("bound
    // 'a by the type it tests, `'a: T`"). Name the binding that holds the open
    // cell, or the declared variable to bound. Then give
    // lang::select::unannotated_slice_element_telling_rest_from_keep_refused
    // and told_refusal_names_written_variable_once their passing twins (`a:
    // Array<`Keep(i64)>`; `'a: [Array<i64>, i64]` with `Array<i64> as xs`).
    // probe:
    // design/review-2026-11-16/repro/tests-select-c-told-refusal-annotate-it.sh
    // (tests-select-c-02)
    // 2026-11-19 claude: On bounds-spike the old settle drain's told(), where
    // this judgment was made, is deleted with the old check (every module ran
    // the same with the drain off); a select's tests are the solver's
    // (check_by_bounds), where this moved. Re-probe under the new check before
    // closing.
    // 2026-11-19 claude: Moved here unchanged with the old select check
    // (Select::typecheck0_with, its coverage walk Reach and ladders, and the
    // pattern helpers only it used), deleted on bounds-spike once every
    // definition is checked by the solver; re-probe under check_by_bounds
    // before closing.

    // CR claude for claude: [structure/low] This function is 415 lines, and its arm
    // loop does every per-arm judgment in sequence over a dozen shared locals:
    // completion, the view the binds narrow over, the narrowing, the order of takers,
    // the rep-collision judgments, the guard, the body, the Any closure and the
    // coverage step. `check_undecided` takes eight parameters because of it. Take out
    // the two blocks that only compute: the view an arm's binds narrow over (`reaching`
    // through `uncovered_parts`, returning the view, its members and the copies) and
    // the tests an arm defers (`Tested` through `defer_judgment`). What is left reads
    // as the arm loop. Found by reading; not run. (c-select-seq-08)
    // 2026-11-19 claude: Moved here unchanged with the old select check
    // (Select::typecheck0_with, its coverage walk Reach and ladders, and the pattern
    // helpers only it used), deleted on bounds-spike once every definition is checked
    // by the solver; re-probe under check_by_bounds before closing.

    // CR claude for claude: [bug/low] The arm's body is checked before
    // `Reach::arm` judges the arm dead, so where nothing reaches the arm its
    // binds are typed `[]` and the body's error is told in place of the
    // unreachable arm: `|x| select x { null as _ => 0, v => v + 1 }` says
    // "cannot compute [] + i64", where `v => v` says "unreachable arm". Fix:
    // before the guard and body are checked, refuse the arm when checking and
    // nothing reaches it (the wildcard is taken, or `reach.atype` is empty),
    // with the message `Reach::arm` gives. probe:
    // design/review-2026-11-16/repro/x-typecheck-patterns-dead-arm-body-error.gx
    // (x-typecheck-patterns-08)
    // 2026-11-19 claude: Moved here unchanged with the old select check
    // (Select::typecheck0_with), deleted on bounds-spike. Under check_by_bounds
    // the probe is accepted (--check exit 0): the new check infers x from the
    // arms ([null, Number]), so the second arm is reachable and nothing is
    // refused; the expectation above comes from the old rule that a type test
    // over an untyped parameter binds it. Likely obsolete with that rule (step
    // 4).

    // CR claude for claude: [bug/critical] Once the arm's guard and body are
    // checked, only a function type inside an `Any` part is refused.
    // `ref_over_any` ran before them, and only on a written test, so a part the
    // arm decides a reference stands, and every reference passes the run-time
    // test. `|x: Any| select x { (r, n) => { *r <- n + 1; 0 }, _ => 0 }` checks
    // and writes an i64 into a string variable, and the next kernel that reads
    // it panics the runtime thread; `let q: &i64 = r`, a guard and a written
    // test's own variable pass the check the same way. Fix: refuse here a
    // predicate, written or inferred, that holds a reference at or inside an
    // `Any` part of what reaches the arm, as the written test `(&mut i64, i64)
    // as (r, n)` is refused, and pin it. probe:
    // design/review-2026-11-16/repro/x-typecheck-patterns-any-arm-decides-ref.gx
    // (x-typecheck-patterns-02)
    // 2026-11-19 claude: Moved here unchanged with the old select check
    // (Select::typecheck0_with), deleted on bounds-spike. Under check_by_bounds
    // the probe is refused: 'expected a reference not Any' (--check exit 1), so
    // the write of an i64 into a string variable is closed, though not in the
    // words proposed above. Pin:
    // lang::select::arm_over_any_deciding_a_reference_is_refused.

    // CR claude for claude: [bug/critical] A name a written test types at `_` is
    // refused only here, where the scrutinee holds a declared variable. Elsewhere
    // the arm is refused only when the whole test can match nothing, so a `_`
    // beside a live member of a written union, or over `Any`, stands and its bind
    // is bottom, which every type holds, while the run-time test passes every value
    // there (`blind_bottoms`). `|x: [`A(i64), `B]| select x { [`A(_), `B] as `A(a)
    // => str::len(a), _ => 1 }` checks, and f(`A(7)) hands 7 to str::len and panics
    // the runtime thread at the kernel boundary; `(_, i64) as (a, b)` over `Any`
    // does the same. Fix: refuse every bind whose type under a written test holds
    // the test's bottom, whatever the scrutinee, with the message
    // `type_inside_rigid` has, and pin both cases. probe:
    // design/review-2026-11-16/repro/x-typecheck-patterns-bottom-test-bind.gx
    // (x-typecheck-patterns-01)
    // 2026-11-19 claude: Moved here unchanged with the old select check
    // (Select::typecheck0_with, its coverage walk Reach and ladders, and the
    // pattern helpers only it used), deleted on bounds-spike once every definition
    // is checked by the solver; re-probe under check_by_bounds before closing.

    // CR claude for claude: [readability/low] This function does three jobs in 150
    // lines and rebinds its names as it goes: `own` is a bool, then the predicate's
    // element type, then a slice of `members`, then one name's parts, then an
    // alternative's map; `typed` is a count and then a slice; `members` is the element
    // members and, inside the loop over them, a bool. Move the typing of the slice's
    // own elements (the `if let Self::Slice ..` block) into a method of its own and
    // give each of these a name of its own. Found by reading; not run. (c-pattern-08)
    // 2026-11-19 claude: Moved here unchanged with the old select check
    // (Select::typecheck0_with, its coverage walk Reach and ladders, and the pattern
    // helpers only it used), deleted on bounds-spike once every definition is checked
    // by the solver; re-probe under check_by_bounds before closing.

    // CR claude for eric: [bug/medium] [bug/medium] When the arms between them test 11 bool
    // positions, this cap stops pooling, and an exhaustive decision list (`(true, _, ..)`,
    // `(false, true, _, ..)`, ..., all `false`) is refused "missing match cases: no arm
    // covers (bool, ..., bool)". Over 10 positions the same ladder is accepted and a
    // trailing `_` is refused as unreachable, so adding one flag turns one refusal into the
    // other. Neither message names the cap or the `_` that fixes it. Variant heads escape
    // this because `Type::diff` splits them; bool literals and other literal pools do not.
    // Either decide coverage without enumerating the product, or make the refusal name the
    // cap and the `_` arm, and pin both sides in lang::select. probe:
    // design/review-2026-11-16/repro/tests-select-b-pool-cap-11-bools.gx
    // (tests-select-b-01)
    // 2026-11-19 claude: Moved here unchanged with the old select check
    // (Select::typecheck0_with, its coverage walk Reach and ladders, and the pattern
    // helpers only it used), deleted on bounds-spike once every definition is checked by
    // the solver; re-probe under check_by_bounds before closing.

    // CR claude for claude: [bug/medium] A variant at a position inside a
    // tuple, struct or variant is pooled only when its payload matches
    // anything; one with a refutable payload is `Miss::Unread` here, though a
    // tuple or struct member at the same position is pooled under
    // `Head::Composite`. So `|x: (`A(bool), i64)| select x { (`A(true), _) =>
    // 0, (`A(false), _) => 1 }` is refused, "no arm covers (`A(bool), i64)",
    // while `A(true), `A(false) at the top and ((true, _), _), ((false, _), _)
    // check. Fix: give a variant member with a refutable payload a head of its
    // own and count its payload positions under it, as `children` does for a
    // composite member, and pin it. probe:
    // design/review-2026-11-16/repro/x-typecheck-patterns-nested-variant-payload-pool.gx
    // (x-typecheck-patterns-06)
    // 2026-11-19 claude: Moved here unchanged with the old select check
    // (Select::typecheck0_with, its coverage walk Reach and ladders, and the pattern
    // helpers only it used), deleted on bounds-spike once every definition is checked by
    // the solver; re-probe under check_by_bounds before closing.

    // CR claude for claude: [bug/medium] Exhaustiveness over a recursive typedef
    // depends on the order of the arms. Over `type L = [`S(L), `Z]`, the arms `Z,
    // `S(`Z), `S(`S(`Z)), `S(`S(`S(_))) are refused, "no arm covers `S(L)", and the
    // same arms with `Z last check; the list form (`Nil, `Cons(a, `Nil), ..) is
    // refused alike, while the tuple form and the type written out three links deep
    // check. This containment fails on its own: `[`Z, `S(`Z), `S(`S(`Z)),
    // `S(`S(`S(L)))]` does not contain `L` in a `let` annotation, though the
    // two-deep union does. Fix: have the containment distribute over the `S head
    // through the recursive name as deep as the union's members go, or have the
    // diff take `S(`Z) out of `S(L), and pin both orders. probe:
    // design/review-2026-11-16/repro/x-typecheck-patterns-recursive-typedef-arm-order.gx
    // (x-typecheck-patterns-05)
    // 2026-11-19 claude: Moved here unchanged with the old select check
    // (Select::typecheck0_with, its coverage walk Reach and ladders, and the pattern
    // helpers only it used), deleted on bounds-spike once every definition is checked by
    // the solver; re-probe under check_by_bounds before closing.

    // CR claude for claude: [bug/low] An arm is found dead here only when
    // `Type::diff` has emptied `atype`, but `Reach::exhausted` also accepts a
    // select whose `covered` union holds the scrutinee while `atype` is not empty
    // (written tests that split a union two positions deep). So a catch-all behind
    // such arms is accepted, though the same select without it is judged
    // exhaustive: `((bool, i64), bool) as _ => 0, ((i64, i64), bool) as _ => 1, _
    // => 2` over `(([i64, bool], i64), bool)`. Before an arm that matches every
    // value, ask what `exhausted` asks (the union of `covered` holds the scrutinee,
    // its open cells held) and refuse the arm when it does. probe:
    // design/review-2026-11-16/repro/c-select-seq-dead-arms-accepted.sh
    // (c-select-seq-03)
    // 2026-11-19 claude: Moved here unchanged with the old select check
    // (Select::typecheck0_with, its coverage walk Reach and ladders, and the
    // pattern helpers only it used), deleted on bounds-spike once every definition
    // is checked by the solver; re-probe under check_by_bounds before closing.

    // CR claude for claude: [readability/medium] Over an unannotated parameter,
    // `|x| select x { i64 as n => n, _ => 0 }` (or the skill's `null as _ => 0,
    // v => v`), the test decides x's type, and this refusal then says the
    // earlier arms "cover the whole scrutinee". It names neither x nor the test
    // that decided it, nor the fix the skill gives (`|x: [i64, string]|`, which
    // passes). Where a written test bound a scrutinee cell that was open before
    // the arms, say so and name the binding to annotate through Holders, as the
    // other select refusals do. probe:
    // design/review-2026-11-16/repro/x-diagnostics-dead-arm-decided-param.gx
    // (x-diagnostics-06)
    // 2026-11-19 claude: Moved here unchanged with the old select check
    // (Select::typecheck0_with, its coverage walk Reach and ladders, and the
    // pattern helpers only it used), deleted on bounds-spike once every
    // definition is checked by the solver; re-probe under check_by_bounds
    // before closing.

    // CR claude for claude: [structure/low] select.rs is 4700 lines holding three things
    // that share only `Select`: the node and its update, the coverage walk (`Reach`, the
    // ladders, `LiteralPool`, `PartPool`, `PoolGroup`: 2100 lines) and what the arms decide
    // of an undecided scrutinee (`Deciding`, `Decisions`, `Below`, `Uncovered`, the
    // `written_*` walks: 900 lines). A reader of the runtime must page past the checker and
    // back. Move the coverage walk and the undecided-type walk into files of their own
    // beside this one (node/select/coverage.rs, node/select/undecided.rs), a move with no
    // change of interface. Found by reading; not run. (c-select-seq-07)
    // 2026-11-19 claude: Moved here unchanged with the old select check
    // (Select::typecheck0_with, its coverage walk Reach and ladders, and the pattern
    // helpers only it used), deleted on bounds-spike once every definition is checked by
    // the solver; re-probe under check_by_bounds before closing.

    // CR claude for claude: [perf/low] This compares each arm's atoms with every earlier
    // unguarded arm, and Reach re-walks what still reaches at every arm (its doc says so),
    // so a select's check is quadratic in its arms. An 800-tag union with one arm per tag
    // takes 427 ms of Typecheck0 (109 ms at 400), and 1600 literal arms take 54 ms (14 at
    // 800). Key the earlier unguarded atoms by type test and head (literal value, tag,
    // shape) and compare a new atom only within its key and against earlier binds and
    // wildcards; index Reach's members by head as its doc describes. probe:
    // design/review-2026-11-16/repro/x-perf-compile-select-arms-quadratic.py
    // (x-perf-compile-06)
    // 2026-11-19 claude: Moved here unchanged with the old select check
    // (Select::typecheck0_with, its coverage walk Reach and ladders, and the pattern
    // helpers only it used), deleted on bounds-spike once every definition is checked by
    // the solver; re-probe under check_by_bounds before closing.

    // CR claude for claude: [bug/low] This finds a repeat only between arms whose type
    // tests are equal (an inferred test never equals a written one), and skips every atom
    // that covers its own test. So a written test repeated with a name or `_` (`Array<i64>
    // as _ => 0, Array<i64> as b => 1` over `Array<[i64, bool]>`) and a pattern repeated
    // across a written and an inferred test (`true => 0, bool as true => 1`) are accepted
    // as live arms wherever `Type::diff` cannot take the first arm out of what reaches.
    // Compare the tests by containment (the earlier arm's predicate holds the later atom's)
    // and drop the skip for a written test: `subsumes` already answers a name or `_`.
    // probe: design/review-2026-11-16/repro/c-select-seq-dead-arms-accepted.sh
    // (c-select-seq-02)
    // 2026-11-19 claude: Moved here unchanged with the old select check
    // (Select::typecheck0_with, its coverage walk Reach and ladders, and the pattern
    // helpers only it used), deleted on bounds-spike once every definition is checked by
    // the solver; re-probe under check_by_bounds before closing.

    // CR claude for claude: [bug/low] This skips every atom that covers its own
    // predicate, as a constructor pattern over its inferred predicate does, so
    // only repeated literals are found here and a subsumed constructor arm is
    // left to the diff, which loses a variant only whole or one position down.
    // `S(`S(`S(_))) => 0, `S(`S(`S(`Z))) => 1, _ => 2` over three nested
    // variants checks, and so does `S(`S(_)), `S(`S(`Z))` over `type L =
    // [`S(L), `Z]`; one level shallower the second arm is refused. Fix: where
    // an earlier unguarded arm `subsumes` the atom and its predicate holds the
    // atom's, refuse the arm whether or not the atom covers its own predicate.
    // probe:
    // design/review-2026-11-16/repro/x-typecheck-patterns-nested-variant-dead-arm.gx
    // (x-typecheck-patterns-07)
    // 2026-11-19 claude: Moved here unchanged with the old select check
    // (Select::typecheck0_with, its coverage walk Reach and ladders, and the pattern
    // helpers only it used), deleted on bounds-spike once every definition is
    // checked by the solver; re-probe under check_by_bounds before closing.

    fn check_by_bounds(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        wrap!(self.arg.node, self.arg.node.typecheck0(ctx))?;
        let bool = Type::Primitive(Typ::Bool.into());
        let mut arms = Vec::with_capacity(self.arms.len());
        for (pat, n) in self.arms.iter_mut() {
            let inner = wrap!(n, pat.structure_predicate.by_bounds(ctx))?;
            let arm = match pat.explicit_type_predicate {
                true => {
                    let whole = TVar::default();
                    let test = &pat.type_predicate;
                    wrap!(n, solving(|s| s.tested(&ctx.env, test, inner, whole)))?
                }
                false => inner,
            };
            // what reaches the arm is what its pattern is run against: an
            // arm with no test of its own tests for it
            let arm = match pat.explicit_type_predicate {
                true => arm,
                false => {
                    let reach = TVar::default();
                    pat.type_predicate = Type::TVar(reach.clone());
                    bounds::Pat::Whole(reach, Box::new(arm))
                }
            };
            if let Some(guard) = &mut pat.guard {
                wrap!(guard.node, guard.node.typecheck0(ctx))?;
                wrap!(guard.node, bool.check_contains(&ctx.env, guard.node.typ()))?
            }
            wrap!(n, n.typecheck0(ctx))?;
            wrap!(n, self.typ.check_contains(&ctx.env, n.typ()))?;
            arms.push(Arm { pat: arm, guarded: pat.guard.is_some() })
        }
        // with no arm that admits everything, the arms' tests bound a
        // scrutinee nothing has decided from above
        let scrut = self.arg.node.typ().clone();
        let admitted: Option<SmallVec<[Type; 8]>> =
            arms.iter().map(|a| a.pat.admits()).collect();
        if scrut.with_deref(|t| t.is_none())
            && arms.iter().all(|a| !a.guarded)
            && let Some(ts) = admitted
        {
            let most = unsplit(&bounds::union_of(ts));
            wrap!(self.arg.node, most.check_contains(&ctx.env, &scrut))?
        }
        let at = self.arg.node.spec().pos;
        let at = format_compact!("{}:{}", at.line, at.column);
        // an arm is named by the type it tests for, an abstract one by its
        // name, or by its pattern
        let written: Arc<[ArcStr]> = match &self.spec.kind {
            ExprKind::Select(se) => {
                let named = |(p, _): &(Pattern, Expr)| -> ArcStr {
                    let text = match &p.type_predicate {
                        Some(t @ Type::Ref(_)) => match t.lookup_ref(&ctx.env) {
                            Ok(a @ Type::Abstract { .. }) => format_compact!("{a}"),
                            _ => format_compact!("{t}"),
                        },
                        Some(t) => format_compact!("{t}"),
                        None => format_compact!("{}", p.structure_predicate),
                    };
                    text.as_str().into()
                };
                Arc::from_iter(se.arms.iter().map(named))
            }
            _ => Arc::from_iter([]),
        };
        let sites: Arc<[Expr]> = match &self.spec.kind {
            ExprKind::Select(se) => {
                Arc::from_iter(se.arms.iter().map(|(p, body)| pattern_site(p, body)))
            }
            _ => Arc::from_iter([]),
        };
        wrap!(
            self,
            solving(|s| {
                let id = s.select(arms, at.as_str().into());
                s.name_arms(id, written, sites);
                s.watch(&ctx.env, &scrut, Use::Select(id))
            })
        )
    }

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
            match pat.explicit_type_predicate {
                // the type test reads the names it writes at run time, where
                // the body's typedefs are gone: resolve them here
                true => {
                    pat.type_predicate.take_row(tp);
                    pat.type_predicate.seed_refs(&ctx.env);
                }
                // the row is what reached the arm, which says nothing of
                // positions: each bind takes its own row, and the pattern
                // its places from the decided type, in typecheck1
                false => pat.type_predicate = tp.clone(),
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
        self.typ = Type::union(&ctx.env, &rtypes)?;
        Ok(())
    }
}
