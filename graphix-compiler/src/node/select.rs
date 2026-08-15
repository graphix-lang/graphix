use super::{Held, compiler::compile, pattern::StructPatternNode};
use crate::{
    CFlag, Event, ExecCtx, Node, NodeView, PrintFlag, Refs, Rt, Scope, Tag, TagValue,
    Update, UserEvent,
    expr::{Expr, ExprId, Pattern},
    format_with_flags,
    fusion::emit::{BodyCx, CompiledExpr, emit_select_node},
    node::pattern::PatternNode,
    typ::Type,
    wrap,
};
use anyhow::{Context, Result, anyhow, bail};
use compact_str::format_compact;
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
    resident: TagValue,
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
            resident: TagValue::phantom(),
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
                let scope = scope.append(&format_compact!("sel{}", SelectId::new().0));
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
            resident: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Select<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let Self { selected, arg, arms, typ: _, spec: _, tail_position, resident } = self;
        let mut pat_up = false;
        let arg_prod = arg.update(ctx, event);
        let bottomed = arg.tag.is_tainted();
        // THE SCRUTINEE RIDE (Eric's ruling 2026-08-07, aug06ghz0):
        // a bottomed scrutinee WITH history rides — the standing
        // selection lives on against the CACHED value (pattern binds
        // ride it stale; a guard-dep fire re-matches against it; a
        // flip re-selects and fires becoming-selected). Only a
        // no-history bottom (the aug04b phantom rule) bottoms the
        // whole select — the early return after the guard tick below.
        // EXCEPT during a depth-trip unwind (Eric's whole-derivation
        // ruling 2026-08-14): the tripped derivation must bottom at
        // its root, so no ride may launder a stale fragment out of it
        // — the kernel's abort-to-root is the model.
        let ride = bottomed && !ctx.depth_tripped && arg.value.is_some();
        // "The scrutinee has a bindable value view this cycle": a
        // value-bearing production, or the ride.
        let arg_up = !bottomed || ride;
        // Arm binds carry the SCRUTINEE's production tag (the kernel's
        // arm-bind disc carry): a stale scrutinee production — a
        // framed re-derivation from a quiet entry — binds STALE
        // leaves; a wake with NO production this update (a guard-flip
        // re-selection) binds the value channel. Firing comes from the
        // selection/emission rules, never from the binds themselves
        // (Eric's ruling 2026-07-18, tail_jump_fired_plumbing).
        let bind_tag = if ride { Tag::STALE } else { arg_prod };
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
            pat_up |= pat.update(ctx, event);
            if bind_guard {
                pat.unbind_event(event);
            }
        }
        // ORGANIC FIRING (Eric's ruling 2026-08-14,
        // design/organic_firing.md): the select's own fired inputs — a
        // triggering VALUE delivery of the scrutinee or a triggering
        // guard production — fire the emission regardless of whether
        // the selection or the taken arm's value changed. `uniq` is
        // the explicit cadence tool; the compiler never gates firing
        // on value or selection identity. The bottom/ride axis is
        // untouched: a bottomed scrutinee delivery rides (selection
        // continuity) and is not an own-fire.
        let own_fired = (!bottomed && arg_prod.triggers()) || pat_up;
        // Fold a tail-spine select's own fires into the dispatch-wide
        // accumulator (the kernel's `tail_scrut_stale`, applied at
        // every `emit_kernel_return`). The arms terminate individually
        // and a jump arm's emission is swallowed by the tail-call
        // stash, so this is the only channel that carries an own-fire
        // to the final base-arm emission — a const base arm
        // re-selected by a later cycle's loop read stale here while
        // the kernel's return fold fired (jul21g divergence). Depth-0
        // passes fold too: a dispatch whose previous cycle didn't loop
        // runs its first pass unframed, and that pass's entry delivery
        // is iteration 1's disc.
        if own_fired && tail_position.load(Ordering::Relaxed) {
            ctx.tail_scrut_fired = true;
        }
        // A NO-HISTORY bottomed scrutinee can't be matched (nothing to
        // ride — the aug04b phantom rule): the whole select bottoms. A
        // triggering delivery is a fresh bottom; a standing one rides
        // the resident.
        if bottomed && !ride {
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
        // (None for a bottom — the placeholder is never usable).
        macro_rules! arm_prod {
            ($i:expr) => {{
                let tv = arms[$i].1.update(ctx, event);
                let t = tv.tag();
                let v = if t.is_bottom() { None } else { Some(tv.value_cloned()) };
                (t, v)
            }};
        }
        // THE ORGANIC EMISSION: the arm's production tag joined with
        // the select's own fires. A fired input (scrutinee delivery,
        // guard production, or the arm's own body) fires the emission
        // of the taken arm's current value; a bottom arm is FreshBottom
        // when anything fired (op-consistency: an op with a
        // standing-bottom operand also mints FreshBottom when
        // triggered) and rides otherwise.
        macro_rules! emit {
            ($t:expr, $v:expr) => {{
                let t: Tag = $t;
                if t.is_bottom() {
                    if t.triggers() || own_fired {
                        Some(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
                    } else {
                        None
                    }
                } else {
                    let tag = if t.is_fired() || own_fired {
                        Tag::FIRED
                    } else {
                        Tag::STALE
                    };
                    $v.map(|v| TagValue::tagged(v, tag))
                }
            }};
        }
        // THE FLOW DRIVER: a re-match runs on a TRIGGERING scrutinee
        // delivery or a guard-dep fire — a merely-stale ride is the
        // value channel and stays on the fast path (a framed descent's
        // leaked SelCell selection must not be "discovered" by a quiet
        // poll's re-match and fire a phantom becoming-selected — the
        // once_tainted re-descent).
        // The scrutinee RIDE re-matches only via pat_up, per the
        // aug06ghz0 ruling ("a guard-dep fire re-matches against the
        // cached value").
        //
        // INSIDE FRAMES selection is VALUE-DRIVEN (R1: a framed pass
        // is the kernel's per-invocation re-derivation, whose tail
        // if-chain re-matches on the scrutinee VALUE every pass): a
        // jump-rebound loop variable arrives STALE by ruling
        // (tail_jump_fired_plumbing — loop plumbing, not an event),
        // so a triggers()-only driver retained the previous pass's
        // arm and spun the loop forever. Firing stays with the
        // emission rules — a value-driven re-match to the same arm
        // with a stale production emits stale. The once_tainted rule
        // is a DEPTH-0 rule (the leaked selection is discovered by a
        // quiet poll AFTER the frames end) and is untouched.
        let arg_trig =
            !bottomed && (arg_prod.triggers() || (ctx.frame_depth > 0 && arg_up));
        let out = if !arg_trig && !pat_up {
            selected.get().and_then(|i| {
                let (t, v) = arm_prod!(i);
                emit!(t, v)
            })
        } else {
            let sel = match arg.value.as_ref() {
                None => None,
                Some(v) => arms.iter().enumerate().find_map(|(i, (pat, _))| {
                    if pat.is_match(&ctx.env, v) { Some(i) } else { None }
                }),
            };
            match (sel, selected.get()) {
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
                        arms[j].1.sleep(ctx);
                    }
                    selected.set(Some(i));
                    // The wake bind is part of the arm's INIT VIEW: on
                    // a guard-flip re-selection the scrutinee produced
                    // nothing, but a STALE pattern bind leaves interior
                    // builtin CallSites undispatched (any-arg-fired
                    // gate) and the woken body can't evaluate — the
                    // select then emits nothing where the kernel's
                    // selection-memory fire produces the arm value
                    // (aug03 reactive/000000). The observable firing
                    // still comes from the emission rules alone. The
                    // in-frame tail spine keeps the scrutinee's honest
                    // tag (per-jump re-selections are loop plumbing),
                    // and a RIDE wake binds the value channel. The old
                    // FIRED external seeding is gone: the arm's refs
                    // read the store under the forced init view (R2) —
                    // wake WITHOUT refill.
                    let wake_tag = if tail || ride {
                        bind_tag
                    } else if arg_prod.triggers() {
                        // a genuinely-triggering scrutinee delivery
                        // carries its own tag into the binds
                        arg_prod
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
                    // genuine wake always has `own_fired` set (the
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
                    arms[j].1.sleep(ctx);
                    selected.set(None);
                    None
                }
                (None, None) => None,
            }
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
            resident: _,
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
            resident: _,
        } = self;
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
        // already provides — re-seeding arm-local lifted targets on
        // UNCHANGED selections, and contradicting fused region
        // kernels, whose selection words are semantic and survive
        // `Kernel::reset_replay`.
        let Self {
            selected: _,
            arg,
            arms,
            typ: _,
            spec: _,
            tail_position: _,
            resident: _,
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
            resident: _,
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

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.arg.node.typecheck0(ctx)?;
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
        for (pat, _) in self.arms.iter_mut() {
            let inferred_irrefutable = !pat.explicit_type_predicate
                && pat.structure_predicate.matches_anything();
            match &mut pat.guard {
                // The guard's OWN typecheck0 runs in the second loop,
                // after the bind narrowing — see the note there.
                Some(_) => (),
                None => {
                    if inferred_irrefutable {
                        wildcard = true;
                    } else if !pat.structure_predicate.is_refutable() {
                        mtype = mtype.union(&ctx.env, &pat.type_predicate)?
                    } else if let StructPatternNode::Literal(Value::Bool(b)) =
                        &pat.structure_predicate
                    {
                        saw_true |= b;
                        saw_false |= !b;
                        if saw_true && saw_false {
                            mtype = mtype
                                .union(&ctx.env, &Type::Primitive(Typ::Bool.into()))?;
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
            if itype != Type::Primitive(BitFlags::empty()) {
                let _ = itype.contains(&ctx.env, &self.arg.node.typ())?;
            }
        } else {
            itype.check_contains(&ctx.env, &self.arg.node.typ()).map_err(|e| {
                format_with_flags(PrintFlag::DerefTVars, || {
                    anyhow!("missing match cases {e}")
                })
            })?;
            mtype.check_contains(&ctx.env, &self.arg.node.typ()).map_err(|e| {
                format_with_flags(PrintFlag::DerefTVars, || {
                    anyhow!("missing match cases {e}")
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
        let mut atype = self.arg.node.typ().clone().normalize();
        for (pat, _) in self.arms.iter() {
            if !&pat.type_predicate.could_match(&ctx.env, &atype)? {
                format_with_flags(PrintFlag::DerefTVars, || {
                    bail!(
                        "pattern {} will never match {}, unused match cases",
                        pat.type_predicate,
                        atype
                    )
                })?
            }
            if !pat.structure_predicate.is_refutable() && pat.guard.is_none() {
                atype = atype.diff(&ctx.env, &pat.type_predicate)?;
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
