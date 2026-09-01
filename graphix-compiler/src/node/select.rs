use super::{Held, compiler::compile, pattern::StructPatternNode};
use crate::{
    BindId, CFlag, Event, ExecCtx, Node, NodeView, PrintFlag, Refs, Rt, Scope, Tag,
    TagValue, Update, UserEvent,
    expr::{Expr, ExprId, ExprKind, Pattern},
    format_with_flags,
    fusion::emit::{BodyCx, CompiledExpr, emit_select_node},
    node::pattern::PatternNode,
    typ::Type,
    wrap,
};
use anyhow::{Context, Result, anyhow, bail};
use enumflags2::BitFlags;
use netidx_value::Typ;
use netidx_value::Value;
use std::sync::atomic::Ordering;

atomic_id!(SelectId);

/// The persistent selection (`Option<usize>` — which arm is selected),
/// stored as an `AtomicUsize` (`usize::MAX` = none) so it can be
/// written through `&self` (the `Sync` bound on `Update`; same pattern
/// as `tail_position`). Semantic state, not a replay cache: it
/// survives sleep and `reset_replay`. Under the strict select rule
/// the selection IS observable (becoming-selected fires), so any
/// machinery that rebuilds a node tree must preserve it.
#[derive(Debug)]
pub(crate) struct SelCell(std::sync::atomic::AtomicUsize);

impl SelCell {
    fn new() -> Self {
        Self(std::sync::atomic::AtomicUsize::new(usize::MAX))
    }

    pub(crate) fn get(&self) -> Option<usize> {
        match self.0.load(Ordering::Relaxed) {
            usize::MAX => None,
            i => Some(i),
        }
    }

    pub(crate) fn set(&self, v: Option<usize>) {
        self.0.store(v.unwrap_or(usize::MAX), Ordering::Relaxed)
    }
}

#[derive(Debug)]
pub struct Select<R: Rt, E: UserEvent> {
    pub(crate) selected: SelCell,
    pub arg: Held<R, E>,
    pub arms: Vec<(PatternNode<R, E>, Node<R, E>)>,
    pub typ: Type,
    pub(crate) spec: Expr,
    /// `true` iff this select sits on a tail-recursive lambda's TAIL
    /// SPINE (the dispatch select whose arms terminate the loop or
    /// jump — marked by `analysis::mark_tail_sites`, written through
    /// `&self` hence the atomic). A tail select's emit rides the
    /// ARM's organic tag alone: the scrutinee is the loop variable,
    /// and its per-iteration firing (jump rebinds deliver FIRED) is
    /// loop plumbing, not an observable event — the interp twin of
    /// the kernel's `emit_body_tail` no-scrutinee-fold rule.
    /// Value-position selects follow the same organic-tag emission
    /// since the strict select rule (Eric's ruling 2026-08-06: emit
    /// iff the selection changes or the taken arm's body produces);
    /// what remains tail-specific is the becoming-selected path — a
    /// tail re-selection is loop mechanics and rides the arm's tag,
    /// a value-position re-selection fires — and the dispatch-level
    /// `tail_scrut_fired` fold.
    pub(crate) tail_position: std::sync::atomic::AtomicBool,
    /// The CONSULTED-GUARD mask from the last completed re-match
    /// (bit i = arm i's guard was consulted — structure-matching, at
    /// or above the chain's stop point; arms ≥ 64 are conservatively
    /// always-consulted). Quiet cycles read it so a STANDING bottom
    /// on a consulted guard keeps the select bottom until the guard
    /// recovers (design/activation_state.md, Eric 2026-08-20).
    consulted: u64,
    resident: TagValue,
    /// Set on the first update, when each arm's shallow type
    /// discriminator is sealed against the settled scrutinee type
    /// ([`PatternNode::seal_shallow`]).
    shallow_sealed: bool,
    /// wake catch-up: set by `sleep()`, taken by the next update — the
    /// first update after sleep RE-MATCHES against the present
    /// scrutinee (a selection retained across the sleep was made
    /// against a value that may have moved while no reader was
    /// awake: an arm-local `<-` target keeps counting).
    slept: bool,
    /// Wake-catch-up fire tracking (design/wake_catchup.md). `None`
    /// until the first update (arm ref sets need the compiled tree).
    tracked: Option<TrackedFires>,
}

impl<R: Rt, E: UserEvent> Select<R, E> {
    /// Build a `Select` node from an already-compiled scrutinee
    /// expression and a vector of (pattern, arm body) pairs.
    #[allow(dead_code)]
    pub fn new(
        arg: Node<R, E>,
        arms: Vec<(PatternNode<R, E>, Node<R, E>)>,
        typ: Type,
        spec: Expr,
    ) -> Node<R, E> {
        Node::new(Self {
            spec,
            typ,
            arg: Held::new(arg),
            arms,
            selected: SelCell::new(),
            tail_position: std::sync::atomic::AtomicBool::new(false),
            consulted: 0,
            resident: TagValue::phantom(),
            shallow_sealed: false,
            slept: false,
            tracked: None,
        })
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        arg: &Expr,
        arms: &[(Pattern, Expr)],
    ) -> Result<Node<R, E>> {
        let arg = Held::new(compile(ctx, flags, arg.clone(), scope, top_id)?);
        let arms = arms
            .iter()
            .map(|(pat, spec)| {
                let scope = scope.append_block("sel", SelectId::new().0);
                let pat = PatternNode::compile(
                    ctx,
                    flags,
                    pat,
                    &scope,
                    top_id,
                    spec.pos,
                    spec.ori.clone(),
                )
                .with_context(|| format!("in select at {}", spec.pos))?;
                let n = compile(ctx, flags, spec.clone(), &scope, top_id)?;
                Ok((pat, n))
            })
            .collect::<Result<Vec<_>>>()
            .with_context(|| format!("in select at {}", spec.pos))?;
        let typ = Type::empty_tvar();
        Ok(Node::new(Self {
            spec,
            typ,
            arg,
            arms,
            selected: SelCell::new(),
            tail_position: std::sync::atomic::AtomicBool::new(false),
            consulted: 0,
            resident: TagValue::phantom(),
            shallow_sealed: false,
            slept: false,
            tracked: None,
        }))
    }
}

/// Collect every ARRAY member of `t` (through refs, bound tvars, and
/// set nesting) — the types slice-pattern length coverage can claim.
/// `depth` caps pathological ref chains; hitting the cap just stops
/// collecting (missing members can only make exhaustiveness stricter).
fn array_members(
    env: &crate::env::Env,
    t: &Type,
    out: &mut smallvec::SmallVec<[Type; 4]>,
    depth: usize,
) -> Result<()> {
    if depth > 64 {
        return Ok(());
    }
    match t {
        Type::Array(_) | Type::List(_) => out.push(t.clone()),
        Type::Set(ts) => {
            for t in ts.iter() {
                array_members(env, t, out, depth + 1)?
            }
        }
        Type::Ref(_) => {
            let t = t.lookup_ref(env)?;
            array_members(env, &t, out, depth + 1)?
        }
        Type::TVar(_) => {
            if let Some(t) = t.with_deref(|t| t.cloned()) {
                array_members(env, &t, out, depth + 1)?
            }
        }
        _ => (),
    }
    Ok(())
}

/// Do lengths `exacts ∪ [rest, ∞)` cover every array length? Complete
/// coverage needs a rest bound with every length below it exact.
fn lens_complete(exacts: &[usize], rest: Option<usize>) -> bool {
    match rest {
        None => false,
        Some(r) => (0..r).all(|n| exacts.contains(&n)),
    }
}

/// Is the length range `== k` (`exact`) / `>= k` (rest form) entirely
/// inside `exacts ∪ [rest, ∞)`?
fn range_covered(k: usize, exact: bool, exacts: &[usize], rest: Option<usize>) -> bool {
    let covered = |n: usize| rest.is_some_and(|r| n >= r) || exacts.contains(&n);
    if exact {
        covered(k)
    } else {
        match rest {
            None => false,
            Some(r) => (k..r.max(k)).all(covered),
        }
    }
}

/// Sleep an arm the select is ACTIVELY deselecting (a re-match during
/// `update`, never a whole-select pause via `sleep`). Under
/// `shrink_unwind` a recursive-edge callee inside the arm is DELETED
/// rather than retained (`CallSite::sleep`): a recursion that reached a
/// shallower depth this cycle sheds the deeper activations, and
/// re-reaching them binds fresh — MapQ's delete-on-shrink for recursion.
fn deselect_sleep<R: Rt, E: UserEvent>(arm: &mut Node<R, E>, ctx: &mut ExecCtx<R, E>) {
    let saved = ctx.shrink_unwind;
    ctx.shrink_unwind = true;
    arm.sleep(ctx);
    ctx.shrink_unwind = saved;
}

/// Wake-catch-up fire tracking (design/wake_catchup.md, Eric's rule
/// 2026-09-01): the select keeps one fire bit per ARM-BODY input —
/// set when the input fires, consumed by whichever arm evaluation
/// reads that input (the live selected arm consuming same-cycle as
/// the degenerate case) — so a woken arm receives, as genuine FIRED
/// deliveries conflated to the current standing value, exactly the
/// fires no selected reader saw, once, and everything else
/// present-but-stale. Guards, the scrutinee, and the select's own
/// pattern binds are OUTSIDE the mechanism (they have their own
/// consult/wake rules). Semantic state: survives sleep and
/// `reset_replay`, cleared only by consumption. Frames are excluded
/// entirely (framed passes run against private variable maps — loop
/// plumbing, not the reactive world).
#[derive(Debug, Default)]
struct TrackedFires {
    /// Per arm: the arm BODY's free refs (referenced minus bound
    /// within the arm, minus the arm's own pattern binds). Computed
    /// from compile-time refs at first update and REFRESHED at each
    /// deselect — the moment the arm's subtree (dynamically bound
    /// callees included) is fully materialized and its sleep begins.
    per_arm: Vec<nohash::IntSet<BindId>>,
    /// The union of `per_arm` — the observed set.
    all: nohash::IntSet<BindId>,
    /// Sound fires no arm evaluation has consumed yet.
    pending: nohash::IntSet<BindId>,
}

impl TrackedFires {
    fn arm_refs<R: Rt, E: UserEvent>(
        pat: &PatternNode<R, E>,
        arm: &Node<R, E>,
    ) -> nohash::IntSet<BindId> {
        let mut r = Refs::default();
        arm.refs(&mut r);
        pat.structure_predicate.ids(&mut |id| {
            r.bound.insert(id);
        });
        r.refed.difference(&r.bound).copied().collect()
    }

    fn init<R: Rt, E: UserEvent>(arms: &[(PatternNode<R, E>, Node<R, E>)]) -> Self {
        let per_arm: Vec<_> =
            arms.iter().map(|(pat, n)| Self::arm_refs(pat, n)).collect();
        let all = per_arm.iter().flatten().copied().collect();
        TrackedFires { per_arm, all, pending: nohash::IntSet::default() }
    }

    fn refresh_arm<R: Rt, E: UserEvent>(
        &mut self,
        i: usize,
        pat: &PatternNode<R, E>,
        arm: &Node<R, E>,
    ) {
        self.per_arm[i] = Self::arm_refs(pat, arm);
        self.all = self.per_arm.iter().flatten().copied().collect();
        self.pending.retain(|id| self.all.contains(id));
    }

    /// Record this cycle's sound fires of tracked inputs. Runs before
    /// routing, so the taken arm's evaluation consumes same-cycle
    /// fires immediately (set-then-consume); with no arm selected —
    /// bottom scrutinee, undecidable guards — the bits simply
    /// accumulate for a future waker.
    fn observe<R: Rt, E: UserEvent>(&mut self, ctx: &ExecCtx<R, E>, event: &Event<E>) {
        if ctx.frame_depth > 0 {
            return;
        }
        let mut newly: smallvec::SmallVec<[BindId; 8]> = smallvec::SmallVec::new();
        for id in self.all.iter() {
            if self.pending.contains(id) {
                continue;
            }
            if let Some(super::VarRead::Delivered(tv)) = super::read_var(ctx, event, id) {
                let t = tv.tag();
                if t.is_fired() && !t.is_bottom() {
                    newly.push(*id);
                }
            }
        }
        for id in newly {
            self.pending.insert(id);
        }
    }

    /// Consume the bits arm `i`'s evaluation reads, injecting a
    /// catch-up FIRED delivery (the current standing value — N fires
    /// during a sleep conflate to one) for each consumed input that
    /// was not already delivered live this cycle. Returns the
    /// injected entries for [`Self::restore`] — the deliveries are
    /// scoped to this arm's evaluation, never visible to siblings. A
    /// consumed input whose standing state has since bottomed (or
    /// vanished) injects nothing — the bit is still spent; the arm
    /// reads the bottom through its ordinary paths.
    fn deliver<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<R, E>,
        event: &mut Event<E>,
        i: usize,
    ) -> smallvec::SmallVec<[(BindId, Option<TagValue>); 4]> {
        let mut injected: smallvec::SmallVec<[(BindId, Option<TagValue>); 4]> =
            smallvec::SmallVec::new();
        if ctx.frame_depth > 0 || self.pending.is_empty() {
            return injected;
        }
        let Some(set) = self.per_arm.get(i) else { return injected };
        let ids: smallvec::SmallVec<[BindId; 8]> =
            self.pending.iter().filter(|id| set.contains(id)).copied().collect();
        for id in ids {
            self.pending.remove(&id);
            let standing = match super::read_var(ctx, event, &id) {
                // delivered live this cycle: the live delivery IS the
                // catch-up
                Some(super::VarRead::Delivered(_)) => None,
                Some(super::VarRead::Standing(tv)) if !tv.tag().is_bottom() => {
                    Some(tv.value_cloned())
                }
                _ => None,
            };
            if let Some(v) = standing {
                let prev = event.variables.insert(id, TagValue::fired(v));
                injected.push((id, prev));
            }
        }
        injected
    }

    fn restore<E: UserEvent>(
        event: &mut Event<E>,
        injected: smallvec::SmallVec<[(BindId, Option<TagValue>); 4]>,
    ) {
        for (id, prev) in injected {
            match prev {
                Some(tv) => {
                    event.variables.insert(id, tv);
                }
                None => {
                    event.variables.remove(&id);
                }
            }
        }
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Select<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        if !self.shallow_sealed {
            self.shallow_sealed = true;
            let scrut = self.arg.node.typ().clone();
            for (pat, _) in self.arms.iter_mut() {
                pat.seal_shallow(&ctx.env, &scrut);
            }
        }
        if self.tracked.is_none() {
            self.tracked = Some(TrackedFires::init(&self.arms));
        }
        let Self {
            selected,
            arg,
            arms,
            typ: _,
            spec: _,
            tail_position,
            consulted,
            resident,
            shallow_sealed: _,
            slept,
            tracked,
        } = self;
        let tracked = tracked.as_mut().expect("tracked initialized above");
        // WAKE CATCH-UP (design/wake_catchup.md): the first update
        // after this select's sleep re-matches from the present
        // scrutinee — a reselected arm recomputes from the world as it
        // stands, and a selection retained across the sleep was made
        // against a value that may have moved meanwhile. Depth 0 only
        // (frames re-derive value-driven).
        let woke = std::mem::take(slept) && ctx.frame_depth == 0;
        // Per-arm guard production tags for THE CONSULTED-GUARD RULE
        // (design/activation_state.md, Eric 2026-08-20): only guards
        // the chain CONSULTS — structure-matching arms at or above the
        // stop point — contribute fires or bottomness to the select;
        // a structure-failed or below-the-taken-arm guard is
        // irrelevant. `None` = unguarded arm.
        let mut guard_tags: smallvec::SmallVec<[Option<Tag>; 8]> =
            smallvec::SmallVec::with_capacity(arms.len());
        let arg_prod = arg.update(ctx, event);
        // WAKE CATCH-UP OBSERVATION (design/wake_catchup.md): record
        // this cycle's sound fires of the arm-body inputs BEFORE any
        // routing or early return, so no-arm windows (bottom
        // scrutinee, undecidable guards) still accumulate, and the
        // taken arm's evaluation below consumes same-cycle fires
        // immediately.
        tracked.observe(ctx, event);
        let bottomed = arg.tag.is_tainted();
        // BOTTOM SCRUTINEE ⇒ BOTTOM SELECT (Eric's ruling 2026-08-29):
        // full stop, no ride. A select whose scrutinee bottoms produces
        // nothing this cycle even if the currently-selected arm is an
        // active async producer — the user writes `hold` on the scrutinee
        // to persist the last value if they care. The retained selection
        // still routes the taken arm's OWN fires on a stale-PRESENT
        // scrutinee (the `ChainOut::Quiet` path below); that is organic
        // own-firing, not the deleted bottom ride. The old unified ride
        // (hold the arm index, re-run it over ⊥-poisoned binds, consult
        // the held guard) is gone.
        // A bottom scrutinee has no value view to bind from.
        let arg_up = !bottomed;
        // Arm binds carry the SCRUTINEE's production tag (the kernel's
        // arm-bind disc carry): a stale scrutinee production — a framed
        // re-derivation from a quiet entry — binds STALE leaves. Firing
        // comes from the selection/emission rules, never from the binds
        // themselves (Eric's ruling 2026-07-18, tail_jump_fired_plumbing).
        let bind_tag = arg_prod;
        macro_rules! bind {
            ($i:expr) => {
                bind!($i, bind_tag)
            };
            ($i:expr, $tag:expr) => {{
                if let Some(arg) = arg.value.as_ref() {
                    arms[$i].0.bind_event(ctx, event, arg, $tag);
                }
            }};
        }
        // The pattern/guard tick runs even for a tainted scrutinee
        // (binds skipped — a placeholder can't be bound): guards are
        // live nodes that must see every cycle, ESPECIALLY init — a
        // tainted init delivery that skipped this loop left const
        // guards unfired forever, so when the first real value
        // arrived no guarded arm could match and selection fell
        // through to the first unguarded arm (jul19b survivor,
        // select-guard-taint-jul2026).
        for (pat, _) in arms.iter_mut() {
            // `arg_up` gates the BIND, not the tick — `pat.update`
            // below is unconditional, so the guard runs every cycle
            // regardless. Binding only on a value view is correct and
            // deliberate: `bind_event` writes the store as well as the
            // transient event entry, and the guard subtree's residents
            // hold the bound leaves, so on a quiet cycle the guard
            // still evaluates against the arg it saw when the
            // scrutinee last fired — combineLatest, not starvation.
            // Re-binding every cycle instead would be a phantom event
            // to anything that reads presence as firing; it is safe
            // today only because `bind_tag` is STALE.
            let bind_guard = arg_up && pat.guard.is_some();
            if bind_guard {
                if let Some(arg) = arg.value.as_ref() {
                    pat.bind_event(ctx, event, arg, bind_tag);
                }
            }
            guard_tags.push(pat.update(ctx, event));
            if bind_guard {
                pat.unbind_event(event);
            }
        }
        // Any guard fire drives a RE-MATCH (routing is harmless and
        // idempotent); whether it fires or bottoms the EMISSION is
        // decided by the consulted set below.
        let pat_up = guard_tags.iter().any(|t| t.is_some_and(|t| t.triggers()));
        // ORGANIC FIRING (Eric's ruling 2026-08-14,
        // design/organic_firing.md): the select's own fired inputs — a
        // triggering VALUE delivery of the scrutinee or a triggering
        // guard production — fire the emission regardless of whether
        // the selection or the taken arm's value changed. `uniq` is
        // the explicit cadence tool; the compiler never gates firing
        // on value or selection identity. The bottom/ride axis is
        // untouched: a bottomed scrutinee delivery rides (selection
        // continuity) and is not an own-fire.
        // The consulted mask is refreshed by each re-match; quiet
        // cycles read the stored one. Bit i set = arm i has a guard
        // the chain consulted. The emission planes over a mask:
        // - sound: a sound scrutinee delivery or a consulted guard's
        //   sound fire;
        // - bottom-fire: a fresh-bottom scrutinee delivery (the
        //   scrutinee axis — class 5);
        // - consulted_bottom: any consulted guard whose CURRENT
        //   channel is bottom — the selection is undecidable and the
        //   select bottoms, whatever else fired (Eric's ruling
        //   2026-08-20; there is no held-verdict ride).
        let scoped = |mask: u64| -> (bool, bool, bool) {
            let mut sound = !bottomed && arg_prod.triggers();
            let mut anyfire = arg_prod.triggers();
            let mut cbot = false;
            for (i, t) in guard_tags.iter().enumerate() {
                if i < 64 && mask & (1 << i) == 0 {
                    continue;
                }
                if let Some(t) = t {
                    if t.triggers() {
                        anyfire = true;
                        if !t.is_bottom() {
                            sound = true;
                        }
                    }
                    if t.is_bottom() {
                        cbot = true;
                    }
                }
            }
            (sound, anyfire, cbot)
        };
        // A bottom scrutinee bottoms the select, full stop (no ride).
        // The guards ticked above (their productions are not CONSUMED — a
        // bottom select consults no guards, so it stays quiet on
        // unrelated guard fires; the aug13l select-miss-standing-fresh
        // ruling). A triggering delivery is a fresh bottom; a standing
        // one rides the resident (the value channel is unchanged — this
        // is not the deleted selection ride).
        if bottomed {
            return if arg_prod.triggers() {
                resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
            } else {
                resident.ride()
            };
        }
        if crate::dbgenv::graphix_dbg_select() {
            eprintln!(
                "SELECT[{}] upd init={} fd={} arg_up={arg_up} pat_up={pat_up} sel={:?} argc={:?} vars={}",
                self.spec.pos,
                event.init,
                ctx.frame_depth,
                selected.get(),
                arg.value.as_ref(),
                event.variables.len()
            );
        }
        // In-frame wake binds ride the loop plumbing's honest tag
        // (per-jump re-selections arrive STALE by ruling,
        // tail_jump_fired_plumbing) — used by `wake_tag` below.
        let tail = tail_position.load(Ordering::Relaxed) && ctx.frame_depth > 0;
        // Read the taken arm's production: its tag plus the value
        // (None for a bottom — the placeholder is never usable). The
        // evaluation CONSUMES the arm's tracked fire bits
        // (design/wake_catchup.md): unconsumed fires of inputs this
        // arm reads are injected as catch-up FIRED deliveries scoped
        // to exactly this evaluation, and the bits clear — once per
        // select, whether the delivery was live or caught up.
        macro_rules! arm_prod {
            ($i:expr) => {{
                let injected = tracked.deliver(ctx, event, $i);
                let tv = arms[$i].1.update(ctx, event);
                let t = tv.tag();
                let v = if t.is_bottom() { None } else { Some(tv.value_cloned()) };
                TrackedFires::restore(event, injected);
                (t, v)
            }};
        }
        // THE FLOW DRIVER runs FIRST under the consulted-guard rule:
        // the chain's consulted set decides which guard productions
        // the emission consumes, so re-match precedes the plane
        // computation. A re-match runs on a triggering scrutinee
        // delivery or ANY guard fire (routing is idempotent — an
        // unconsulted guard's fire re-derives the same selection and
        // the scoped planes keep the emission quiet). Inside frames
        // selection stays VALUE-DRIVEN (R1; see the once_tainted
        // note below).
        let arg_trig = arg_prod.triggers() || (ctx.frame_depth > 0 && arg_up);
        enum ChainOut {
            Quiet,
            Taken(Option<usize>),
            Undet,
        }
        // A PRESENT scrutinee with NO retained selection must still
        // route: selection is a VALUE question (which arm holds the
        // value channel), not a firing one. Under the wake-forced init
        // view a standing scrutinee reads present-but-stale
        // (2026-08-31), so a select consulted for the first time
        // inside a woken subtree would otherwise dead-end (Quiet with
        // nothing retained) and the arm chain below it never
        // materializes — the pump's identity modal ate every key.
        // Depth 0 only: framed re-matching stays value-driven (R1).
        let first_consult = ctx.frame_depth == 0
            && selected.get().is_none()
            && arg.value.is_some()
            && !arg.tag.is_tainted();
        // A non-bottom scrutinee (bottom returned above): a quiet cycle
        // (no scrutinee trigger, no guard fire) rides the retained
        // selection through `ChainOut::Quiet`; otherwise re-match.
        let chain = if !arg_trig && !pat_up && !first_consult && !woke {
            ChainOut::Quiet
        } else {
            match arg.value.as_ref() {
                None => ChainOut::Taken(None),
                Some(v) => {
                    let mut mask = 0u64;
                    let mut out = ChainOut::Taken(None);
                    for (i, (pat, _)) in arms.iter().enumerate() {
                        use super::pattern::ArmMatch;
                        match pat.arm_match(&ctx.env, v) {
                            // Structure failed: the guard was NOT
                            // consulted — irrelevant to this select.
                            ArmMatch::NoStruct => (),
                            ArmMatch::GuardFalse => {
                                if i < 64 {
                                    mask |= 1 << i;
                                }
                            }
                            // A consulted guard's channel is bottom:
                            // the selection is UNDECIDABLE — the
                            // chain stops, arms below are never
                            // reached, selection state holds.
                            ArmMatch::GuardBottom => {
                                if i < 64 {
                                    mask |= 1 << i;
                                }
                                out = ChainOut::Undet;
                                break;
                            }
                            ArmMatch::Matched => {
                                if pat.guard.is_some() && i < 64 {
                                    mask |= 1 << i;
                                }
                                out = ChainOut::Taken(Some(i));
                                break;
                            }
                        }
                    }
                    *consulted = mask;
                    out
                }
            }
        };
        let (own_sound, own_anyfire, consulted_bottom) = scoped(*consulted);
        // Fold a tail-spine select's SOUND own-fires into the
        // dispatch-wide accumulator (the kernel's `tail_scrut_stale`,
        // applied at every `emit_kernel_return`) — the only channel
        // that carries an own-fire to the final base-arm emission
        // (jul21g). Sound only: a bottom must never upgrade a stale
        // result to Fired.
        if own_sound && tail_position.load(Ordering::Relaxed) {
            ctx.tail_scrut_fired = true;
        }
        // THE ORGANIC EMISSION under THE CONSULTED-GUARD RULE
        // (design/activation_state.md): a consulted guard whose
        // CURRENT channel is bottom makes the selection undecidable —
        // the emission is bottom whatever else fired (fresh iff a
        // consumed input fired; `hold` is the explicit tool).
        // Otherwise: a sound consumed fire (scrutinee delivery,
        // consulted guard, or the arm's own body) emits the taken
        // arm's current value; a fresh-bottom scrutinee alone emits
        // FreshBottom; quiet rides.
        macro_rules! emit {
            ($t:expr, $v:expr) => {{
                let t: Tag = $t;
                if consulted_bottom {
                    if t.triggers() || own_anyfire {
                        Some(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
                    } else {
                        None
                    }
                } else if t.is_bottom() {
                    if t.triggers() || own_sound {
                        Some(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
                    } else {
                        None
                    }
                } else if t.is_fired() || own_sound {
                    $v.map(|v| TagValue::tagged(v, Tag::FIRED))
                } else {
                    $v.map(|v| TagValue::tagged(v, Tag::STALE))
                }
            }};
        }
        // The quiet fast path keeps the once_tainted rule: a framed
        // descent's leaked SelCell selection must not be "discovered"
        // by a quiet poll's re-match and fire a phantom
        // becoming-selected. Inside frames re-matching is
        // value-driven (arg_up) — a jump-rebound loop variable
        // arrives STALE by ruling (tail_jump_fired_plumbing), so a
        // triggers()-only driver retained the previous pass's arm and
        // spun the loop forever.
        let out = match chain {
            ChainOut::Quiet => selected.get().and_then(|i| {
                let (t, v) = arm_prod!(i);
                emit!(t, v)
            }),
            // A consulted guard's channel is bottom: the selection is
            // UNDECIDABLE (Eric's ruling 2026-08-20). No flip, no
            // wake, no sleep — selection state holds; the emission is
            // bottom, fresh iff a consumed input fired.
            ChainOut::Undet => {
                let t = if own_anyfire { Tag::FRESH_BOTTOM } else { Tag::STALE_BOTTOM };
                return resident.set(TagValue::tagged(Value::Null, t));
            }
            ChainOut::Taken(sel) => match (sel, selected.get()) {
                (Some(i), Some(j)) if i == j => {
                    if crate::dbgenv::graphix_dbg_select() {
                        eprintln!(
                            "SELECT[{}] same-arm i={i} arg={:?}",
                            self.spec.pos,
                            arg.value.as_ref()
                        );
                    }
                    if arg_up {
                        bind!(i);
                    }
                    let (t, v) = arm_prod!(i);
                    emit!(t, v)
                }
                (Some(i), Some(_) | None) => {
                    if crate::dbgenv::graphix_dbg_select() {
                        eprintln!(
                            "SELECT[{}] BECOMING-SELECTED {:?} -> {i} fd={} init={}",
                            self.spec.pos,
                            selected.get(),
                            ctx.frame_depth,
                            event.init
                        );
                    }
                    if let Some(j) = selected.get() {
                        deselect_sleep(&mut arms[j].1, ctx);
                        // refresh the sleeper's tracked read set now,
                        // while its subtree (dynamically bound callees
                        // included) is fully materialized — the
                        // compile-time refs walk cannot see through a
                        // lambda literal into an instantiated body
                        tracked.refresh_arm(j, &arms[j].0, &arms[j].1);
                    }
                    selected.set(Some(i));
                    // The wake bind is part of the arm's INIT VIEW: on
                    // a guard-flip re-selection the scrutinee produced
                    // nothing, but a STALE pattern bind leaves interior
                    // builtin CallSites undispatched (any-arg-fired
                    // gate) and the woken body can't evaluate — the
                    // select then emits nothing where the kernel
                    // produces the arm value (aug03 reactive/000000).
                    // The observable firing
                    // still comes from the emission rules alone. The
                    // in-frame tail spine keeps the scrutinee's honest
                    // tag (per-jump re-selections are loop plumbing).
                    // The old FIRED external seeding is gone: the arm's
                    // refs read the store under the forced init view (R2)
                    // — wake WITHOUT refill.
                    let wake_tag = if tail {
                        bind_tag
                    } else if arg_prod.triggers() {
                        // a genuinely-triggering scrutinee delivery
                        // carries its own tag into the binds
                        arg_prod
                    } else if first_consult && !pat_up {
                        // the stale first consult (2026-08-31): the
                        // binds carry a PAST value — deliver them
                        // present-but-stale; the arm still evaluates
                        // through the forced init view's
                        // materialization rules, and firing comes
                        // only from inputs that fired this cycle.
                        Tag::STALE
                    } else {
                        // guard-flip wake: the scrutinee produced only
                        // its quiet ride — a STALE wake bind left the
                        // woken arm's interior call sites undispatched
                        // (aug03's exact symptom). The wake bind is the
                        // arm's INIT VIEW (R2's fresh reader): bind
                        // FIRED.
                        Tag::FIRED
                    };
                    bind!(i, wake_tag);
                    // The init view is REAL (kernels force their input
                    // view, call sites prime, refs read standing entries
                    // as Fired), but flagged as a WAKE: the arm is
                    // RESUMED, not created, so a `<-` target that
                    // already holds a value is not reseeded by its own
                    // initializer — sleep is PAUSE. A first-ever
                    // selection needs no special case: nothing has
                    // published yet, so the seed applies normally.
                    let (init, wake) = (event.init, event.wake_init);
                    event.init = true;
                    event.wake_init = true;
                    let (t, v) = arm_prod!(i);
                    event.init = init;
                    event.wake_init = wake;
                    // The wake emission is the SAME organic rule: a
                    // genuine wake always has an own-fire set (the
                    // depth-0 flow driver only re-matches on a
                    // triggering delivery or a guard fire), so
                    // becoming-selected emits FIRED — while a framed
                    // value-driven re-selection (loop mechanics, no
                    // trigger) rides the arm's organic tag, with the
                    // in-frame own-fires carried to the final emission
                    // by `ctx.tail_scrut_fired` above.
                    emit!(t, v)
                }
                (None, Some(j)) => {
                    deselect_sleep(&mut arms[j].1, ctx);
                    tracked.refresh_arm(j, &arms[j].0, &arms[j].1);
                    selected.set(None);
                    None
                }
                (None, None) => None,
            },
        };
        match out {
            Some(tv) => resident.set(tv),
            // quiet / deselected-to-nothing: the select's value
            // channel re-surfaces its last emission
            None => resident.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        let Self {
            selected: _,
            arg,
            arms,
            typ: _,
            spec: _,
            tail_position: _,
            consulted: _,
            resident: _,
            shallow_sealed: _,
            slept: _,
            tracked: _,
        } = self;
        arg.node.delete(ctx);
        for (pat, arm) in arms {
            arm.delete(ctx);
            pat.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        let Self {
            selected: _,
            arg,
            arms,
            typ: _,
            spec: _,
            tail_position: _,
            consulted: _,
            resident: _,
            shallow_sealed: _,
            slept,
            tracked: _,
        } = self;
        *slept = true;
        arg.sleep(ctx);
        for (pat, arg) in arms {
            arg.sleep(ctx);
            if let Some(n) = &mut pat.guard {
                n.sleep(ctx)
            }
        }
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        // The SELECTION is semantic state, not a replay cache (Eric's
        // ruling 2026-07-16): a frame pass that re-derives a DIFFERENT
        // selection fires through the ordinary selection-change path
        // (arm wake, init view); re-deriving the SAME selection is
        // quiet — an arm fires once when it BECOMES selected, and a
        // reset that didn't change the selection didn't make it become
        // selected again. Re-derivation is guaranteed because every
        // frame pass seeds ALL external refs (lambda.rs frame
        // discipline) — the scrutinee/guards re-produce and the
        // same-selection path re-binds the pattern binds. The former
        // `*selected = None` forced the full arm-wake per pass — a
        // value-channel redelivery the frame's forced init view
        // already provides — re-seeding arm-local binds on UNCHANGED
        // selections.
        let Self {
            selected: _,
            arg,
            arms,
            typ: _,
            spec: _,
            tail_position: _,
            consulted: _,
            resident: _,
            shallow_sealed: _,
            slept: _,
            tracked: _,
        } = self;
        arg.reset_replay(ctx);
        for (pat, arg) in arms {
            arg.reset_replay(ctx);
            if let Some(n) = &mut pat.guard {
                n.reset_replay(ctx)
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
            tail_position: _,
            consulted: _,
            resident: _,
            shallow_sealed: _,
            slept: _,
            tracked: _,
        } = self;
        arg.node.refs(refs);
        for (pat, arm) in arms {
            arm.refs(refs);
            pat.structure_predicate.ids(&mut |id| {
                refs.bound.insert(id);
            });
            if let Some(n) = &pat.guard {
                n.node.refs(refs);
            }
        }
    }

    /// An arm's coverage ATOMS: for an or-pattern arm, each alternative
    /// paired with its member of the arm's predicate (the inferred Set
    /// is built one member per alternative, in order, and completion/
    /// realign preserve the alignment; under an explicit predicate
    /// every alternative pairs with the whole claim). Any other arm is
    /// its own single atom.
    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        fn arm_atoms<'a>(
            sp: &'a StructPatternNode,
            typ: &'a Type,
            out: &mut smallvec::SmallVec<[(&'a StructPatternNode, Type); 4]>,
        ) {
            match sp {
                StructPatternNode::Or { alts } => {
                    let ts = typ.with_deref(|t| match t {
                        Some(Type::Set(ts)) if ts.len() == alts.len() => Some(ts.clone()),
                        _ => None,
                    });
                    for (i, a) in alts.iter().enumerate() {
                        let t = match &ts {
                            Some(ts) => ts[i].clone(),
                            None => typ.clone(),
                        };
                        out.push((a, t));
                    }
                }
                _ => out.push((sp, typ.clone())),
            }
        }
        self.arg.node.typecheck0(ctx)?;
        // A PARTIAL struct pattern (`{x, ..}`) infers a type carrying
        // only its named fields — an exact struct that could never
        // match the real member. Now that the scrutinee is typed, the
        // rest of the fields are known: complete each inferred
        // predicate against the scrutinee (at any nesting depth)
        // before any coverage math or runtime dispatch reads it.
        // Explicit predicates are the user's claim and stay untouched.
        if let ExprKind::Select(se) = &self.spec.kind {
            let scrut = self.arg.node.typ().clone();
            for ((pat, _), (spec_pat, _)) in self.arms.iter_mut().zip(se.arms.iter()) {
                if !pat.explicit_type_predicate {
                    if let Some(t) = spec_pat
                        .structure_predicate
                        .complete_type_predicate(&ctx.env, &pat.type_predicate, &scrut)?
                    {
                        pat.structure_predicate.realign(&ctx.env, &t)?;
                        pat.type_predicate = t;
                    }
                }
            }
        }
        let mut rtype = Type::Primitive(BitFlags::empty());
        let mut mtype = Type::Primitive(BitFlags::empty());
        let mut itype = Type::Primitive(BitFlags::empty());
        let mut saw_true = false;
        let mut saw_false = false;
        // An UNGUARDED wildcard (an arm whose pattern is irrefutable
        // with an INFERRED type predicate — a bind-all / destructure of
        // binds) matches anything, so its presence makes the select
        // exhaustive by construction. Its raw predicate is a fresh TVar
        // (or a composite of them) and must stay OUT of the coverage
        // unions: `check_contains` is a greedy unifying walk, and fed
        // the wildcard's tvar it bound it to the FIRST scrutinee union
        // member it met and reported the rest missing (a guarded arm
        // followed by a bind-all final was rejected as non-exhaustive);
        // fed the scrutinee's own not-yet-bound cell it burned that
        // instead.
        let mut wildcard = false;
        // Unguarded array-slice arms whose element sub-patterns match
        // anything claim LENGTH coverage — `(k, exact, predicate)`:
        // the arm matches every array (of its predicate's type) of
        // length == k (`exact`) or >= k (a rest form). Pooled after
        // the loop: if the claimed lengths cover ℕ, the pool covers
        // each scrutinee array member that EVERY pool arm's type
        // predicate contains (dispatch is type-gated per arm, so one
        // differently-typed arm is a runtime hole, not coverage).
        let mut slice_pool: smallvec::SmallVec<[(usize, bool, Type); 8]> =
            smallvec::SmallVec::new();
        let (mut guarded_slice, mut refutable_slice) = (false, false);
        for (pat, _) in self.arms.iter_mut() {
            let inferred_irrefutable = !pat.explicit_type_predicate
                && pat.structure_predicate.matches_anything();
            match &mut pat.guard {
                // The guard's OWN typecheck0 runs in the second loop,
                // after the bind narrowing — see the note there.
                Some(_) => {
                    guarded_slice |= pat.structure_predicate.is_array_slice();
                }
                None => {
                    if inferred_irrefutable {
                        wildcard = true;
                    } else {
                        // Per coverage ATOM: an or-pattern arm claims
                        // once per alternative, each against its own
                        // member of the arm's predicate.
                        let mut atoms: smallvec::SmallVec<
                            [(&StructPatternNode, Type); 4],
                        > = smallvec::SmallVec::new();
                        arm_atoms(
                            &pat.structure_predicate,
                            &pat.type_predicate,
                            &mut atoms,
                        );
                        for (sp, at) in atoms.iter() {
                            if !sp.is_refutable() {
                                mtype = mtype.union(&ctx.env, at)?
                            } else if let StructPatternNode::Literal(Value::Bool(b)) = sp
                            {
                                saw_true |= *b;
                                saw_false |= !*b;
                                if saw_true && saw_false {
                                    mtype = mtype.union(
                                        &ctx.env,
                                        &Type::Primitive(Typ::Bool.into()),
                                    )?;
                                }
                            } else if let Some((k, exact)) = sp.array_len_coverage() {
                                slice_pool.push((k, exact, at.clone()));
                            } else if sp.is_array_slice() {
                                refutable_slice = true;
                            }
                        }
                    }
                }
            }
            if !inferred_irrefutable {
                itype = itype.union(&ctx.env, &pat.type_predicate)?;
            }
            // NOTE: rtype (the arm-result union) is built in the
            // SECOND loop, after each arm's typecheck0 — an arm whose
            // node REPLACES its typ field there (a nested select sets
            // `self.typ = rtype`) would otherwise contribute its
            // pre-typecheck EMPTY tvar to the union: an orphaned cell
            // the enclosing annotation check greedily bound (an
            // `-> i64` lambda/let accepted a nested select yielding
            // STRUCTS; the fused return slot then leaked the struct
            // pointer as a scalar — soak jul08o).
        }
        if wildcard {
            // Exhaustive by construction — but still narrow an
            // under-constrained scrutinee against the union of the
            // informative arm predicates (a side-effect-only walk, the
            // bool is discarded). This is where
            // `|n, acc| select n { 0 => .., _ => .. }` learns n: i64;
            // without a wildcard the coverage checks below perform the
            // same narrowing.
            // ... but not a UNION scrutinee: its open members are what
            // the arms discriminate, and this walk would bind one to an
            // arm predicate (`select acc { null as _ => .., found => .. }`
            // over `[e, null]` with `e` open bound `e := null`, and the
            // wildcard arm died). A free union member stays free.
            let union_scrut = match self.arg.node.typ().with_deref(|t| t.cloned()) {
                Some(Type::Set(_)) => true,
                Some(t @ Type::Ref(_)) => matches!(t.lookup_ref(&ctx.env)?, Type::Set(_)),
                _ => false,
            };
            if itype != Type::Primitive(BitFlags::empty()) && !union_scrut {
                let _ = itype.contains(&ctx.env, &self.arg.node.typ())?;
            }
        } else {
            itype.check_contains(&ctx.env, &self.arg.node.typ()).map_err(|e| {
                format_with_flags(PrintFlag::DerefTVars, || {
                    anyhow!("missing match cases {e}")
                })
            })?;
            // Resolve the slice pool AFTER the itype check (it narrows
            // an under-constrained scrutinee) and BEFORE the mtype
            // check (coverage joins that union). A pool whose lengths
            // fall short doesn't contribute; the note says which
            // length is the hole, since the mtype error alone can't.
            let mut slice_note = String::new();
            if !slice_pool.is_empty() {
                let rest =
                    slice_pool.iter().filter(|(_, e, _)| !e).map(|(k, _, _)| *k).min();
                match rest {
                    None => {
                        slice_note = " (the slice arms cover finitely many \
                                      lengths — an array or list scrutinee also needs a \
                                      rest pattern or a wildcard)"
                            .into()
                    }
                    Some(rest) => {
                        let hole = (0..rest)
                            .find(|n| !slice_pool.iter().any(|(k, e, _)| *e && k == n));
                        match hole {
                            Some(n) => {
                                slice_note = format!(
                                    " (the slice arms leave array length {n} \
                                     uncovered)"
                                )
                            }
                            None => {
                                let mut members: smallvec::SmallVec<[Type; 4]> =
                                    smallvec::SmallVec::new();
                                array_members(
                                    &ctx.env,
                                    self.arg.node.typ(),
                                    &mut members,
                                    0,
                                )?;
                                for m in members {
                                    let mut all = true;
                                    for (_, _, p) in slice_pool.iter() {
                                        if !p.contains(&ctx.env, &m)? {
                                            all = false;
                                            break;
                                        }
                                    }
                                    if all {
                                        mtype = mtype.union(&ctx.env, &m)?;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if refutable_slice {
                slice_note.push_str(
                    " (a slice arm with refutable element patterns — literals, \
                     variants, nested slices — cannot establish length coverage)",
                );
            }
            if guarded_slice {
                slice_note
                    .push_str(" (a guarded slice arm cannot establish length coverage)");
            }
            let scrut = self.arg.node.typ().clone();
            mtype.check_contains(&ctx.env, &scrut).map_err(|e| {
                format_with_flags(PrintFlag::DerefTVars, || {
                    if mtype == Type::Primitive(BitFlags::empty()) {
                        anyhow!(
                            "missing match cases: no unguarded arm irrefutably \
                             covers {scrut}{slice_note}"
                        )
                    } else {
                        anyhow!("missing match cases {e}{slice_note}")
                    }
                })
            })?;
        }
        let mut ntype = self.arg.node.typ().clone().normalize();
        for (pat, n) in self.arms.iter_mut() {
            // make sure tvars are aliased properly even if itype was Any.
            // Alias against the NARROWED scrutinee type — the scrutinee
            // minus every EARLIER unguarded irrefutable arm's coverage
            // (the same subtraction the dead-arm walk below performs):
            // in `select opt { null as _ => "", s => s }` the value
            // reaching `s` cannot be null, so `s` is `string`, not
            // `[string, null]`. Aliasing against the full scrutinee
            // widened every post-narrowing bind (this used to come out
            // right only when the coverage walk happened to greedily
            // bind the tvar to the union's first member).
            //
            // Unify through the `any_as_tvar` VIEW (same TVar cells, `Any`
            // leaves swapped for throwaway fresh TVars): the contains walk
            // short-circuits composite pairs on the first false, and
            // `T.contains(Any)` is false — a `_` slot would otherwise stop
            // the walk and leave every LATER slot's bind TVars un-narrowed.
            ntype.contains(&ctx.env, &pat.type_predicate.any_as_tvar())?;
            // A guard decides whether its arm matches, so it must be
            // `bool`. Unchecked, ANY type was accepted and the arm
            // simply never matched — `select n { v if n => a, _ => b }`
            // (someone reaching for truthiness) compiled to a silently
            // dead arm, which the differential fuzzer cannot see
            // because both engines agree on it. Checked HERE, after the
            // narrowing above, so that a guard which IS the arm's own
            // bind reports against the bind's settled type; run in the
            // first loop it bound that still-open TVar to bool instead,
            // and the failure surfaced as an unrelated "pattern will
            // never match" from the coverage walk.
            // The guard is typechecked HERE, after the narrowing above,
            // for the same reason its bool check is: a guard reads the
            // arm's own binds, so it must see them at their SETTLED
            // type. Run in the first loop (where it used to be) the
            // binds are still open TVars, and the first use inside the
            // guard binds them — `select u { v if p(v) => .. }` with
            // `u: [i64, f64]` and `p: fn(i64) -> bool` bound `v := i64`
            // and compiled, where the same call in the arm BODY is
            // correctly rejected. The node-walk then compared
            // dynamically while the kernel froze the param to
            // Scalar(I64) and bottomed the f64 at the ABI boundary
            // (soak aug14e, katana divergence_000001 / hz1
            // divergence_000000 — the same hole reached through a
            // struct+slice pattern and through `any`).
            if let Some(guard) = &mut pat.guard {
                guard.node.typecheck0(ctx)?;
                let bt = Type::Primitive(Typ::Bool.into());
                wrap!(guard.node, bt.check_contains(&ctx.env, guard.node.typ()))?;
            }
            wrap!(n, n.typecheck0(ctx))?;
            rtype = rtype.union(&ctx.env, n.typ())?;
            if !pat.structure_predicate.is_refutable() && pat.guard.is_none() {
                ntype = ntype.diff(&ctx.env, &pat.type_predicate)?;
            }
        }
        // The dead-arm walk: `atype` is the residual scrutinee — what
        // can still reach each arm — and an arm that could match none
        // of it is refused. Coverage subtracts LENGTH-precisely for
        // array members (the mirror of the exhaustiveness pool above)
        // and for the bool literal pair, so a wildcard behind a
        // complete slice ladder or behind `true`+`false` is dead, the
        // same as behind a full variant set. Slice-shaped arms also
        // die by RANGE: an arm whose every matchable length is already
        // matched by earlier covering arms can never run, whatever its
        // guard or element patterns say.
        let mut atype = self.arg.node.typ().clone().normalize();
        let mut members: smallvec::SmallVec<[Type; 4]> = smallvec::SmallVec::new();
        array_members(&ctx.env, &atype, &mut members, 0)?;
        struct Cov {
            m: Type,
            exacts: smallvec::SmallVec<[usize; 8]>,
            rest: Option<usize>,
            done: bool,
        }
        let mut covered: smallvec::SmallVec<[Cov; 4]> = members
            .drain(..)
            .map(|m| Cov {
                m,
                exacts: smallvec::SmallVec::new(),
                rest: None,
                done: false,
            })
            .collect();
        let (mut saw_t, mut saw_f) = (false, false);
        for (pat, _) in self.arms.iter() {
            if atype == Type::Primitive(BitFlags::empty()) {
                bail!(
                    "unreachable arm: the earlier arms already cover the whole \
                     scrutinee, unused match cases"
                )
            }
            if !&pat.type_predicate.could_match(&ctx.env, &atype)? {
                format_with_flags(PrintFlag::DerefTVars, || {
                    bail!(
                        "pattern {} will never match {}, unused match cases",
                        pat.type_predicate,
                        atype
                    )
                })?
            }
            let mut atoms: smallvec::SmallVec<[(&StructPatternNode, Type); 4]> =
                smallvec::SmallVec::new();
            arm_atoms(&pat.structure_predicate, &pat.type_predicate, &mut atoms);
            let or_arm = atoms.len() > 1;
            for (sp, at) in atoms.iter() {
                // A type-dead ALTERNATIVE is an error like a type-dead
                // arm (the house dead-arm rule applied within the arm).
                if or_arm && !at.could_match(&ctx.env, &atype)? {
                    format_with_flags(PrintFlag::DerefTVars, || {
                        bail!(
                            "unreachable or-pattern alternative: {at} will never \
                             match {atype}, unused match cases"
                        )
                    })?
                }
                if let Some((k, exact)) = sp.array_len_range() {
                    let mut any = false;
                    let mut all = true;
                    for c in covered.iter() {
                        if at.could_match(&ctx.env, &c.m)? {
                            any = true;
                            all &= range_covered(k, exact, &c.exacts, c.rest);
                        }
                    }
                    if any && all {
                        // For an or-pattern arm this is one DEAD
                        // ALTERNATIVE (the house dead-arm rule applied
                        // within the arm), not necessarily a dead arm.
                        if or_arm {
                            bail!(
                                "unreachable or-pattern alternative: every array \
                                 length it can match is covered by earlier arms, \
                                 unused match cases"
                            )
                        }
                        bail!(
                            "unreachable arm: every array length this slice pattern \
                             can match is covered by earlier arms, unused match cases"
                        )
                    }
                    if pat.guard.is_none() && sp.array_len_coverage().is_some() {
                        for c in covered.iter_mut() {
                            if !c.done && at.contains(&ctx.env, &c.m)? {
                                if exact {
                                    if !c.exacts.contains(&k) {
                                        c.exacts.push(k)
                                    }
                                } else {
                                    c.rest = Some(c.rest.map_or(k, |r| r.min(k)))
                                }
                                if lens_complete(&c.exacts, c.rest) {
                                    c.done = true;
                                    atype = atype.diff(&ctx.env, &c.m)?;
                                }
                            }
                        }
                    }
                }
                if pat.guard.is_none()
                    && let StructPatternNode::Literal(Value::Bool(b)) = sp
                {
                    saw_t |= *b;
                    saw_f |= !*b;
                    if saw_t && saw_f {
                        atype =
                            atype.diff(&ctx.env, &Type::Primitive(Typ::Bool.into()))?;
                    }
                }
                if !sp.is_refutable() && pat.guard.is_none() {
                    atype = atype.diff(&ctx.env, at)?;
                }
            }
        }
        self.typ = rtype;
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
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

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Select(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_select_node(cx, self)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // Reached only when no enclosing region fused this select whole
        // (a region-interior select emits via emit_select_node instead).
        // The scrutinee, each guard, and each arm BODY get their own
        // region passes: an arm body is an ordinary node, so a fused arm
        // is just a FusedKernel in arm position — sleep/wake already has
        // kernel semantics (Kernel::sleep keeps the input slots for the
        // arm-wake replay), the select's wake-forced event.init flows
        // into the kernel per invocation (wire slot 0), and pattern
        // binds are store-backed BindIds the region collector treats as
        // external inputs with in-band tags. This is also what routes
        // attribute dispatch into arms — `#[native]` on an arm body
        // errors honestly instead of sitting inert (Eric, 2026-08-14).
        // Constant bodies are skipped: a 0-input kernel wrapping a
        // literal is pure dispatch overhead (the CallSite::fuse rule).
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
