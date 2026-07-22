use super::{Cached, compiler::compile, pattern::StructPatternNode};
use crate::{
    BindId, CFlag, Event, ExecCtx, Node, NodeView, PrintFlag, Refs, Rt, Scope, Tag,
    TagValue, Update, UserEvent,
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
use netidx::subscriber::Value;
use netidx_value::Typ;
use poolshark::local::LPooled;
use std::{collections::hash_map::Entry, sync::atomic::Ordering};

atomic_id!(SelectId);

#[derive(Debug)]
pub struct Select<R: Rt, E: UserEvent> {
    pub(super) selected: Option<usize>,
    pub arg: Cached<R, E>,
    pub arms: Vec<(PatternNode<R, E>, Cached<R, E>)>,
    pub typ: Type,
    pub(crate) spec: Expr,
    /// `true` iff this select sits on a tail-recursive lambda's TAIL
    /// SPINE (the dispatch select whose arms terminate the loop or
    /// jump — marked by `analysis::mark_tail_sites`, written through
    /// `&self` hence the atomic). A tail select's emit rides the
    /// ARM's organic tag alone: the scrutinee is the loop variable,
    /// and its per-iteration firing (jump rebinds deliver FIRED) is
    /// loop plumbing, not an observable event — the interp twin of
    /// the kernel's `emit_body_tail` no-scrutinee-fold rule. Value-
    /// position selects keep the normal fold (result fires iff the
    /// arm production or the consumed scrutinee fired — the #178
    /// disc algebra).
    pub(crate) tail_position: std::sync::atomic::AtomicBool,
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
        let arms = arms.into_iter().map(|(p, n)| (p, Cached::new(n))).collect::<Vec<_>>();
        Box::new(Self {
            spec,
            typ,
            arg: Cached::new(arg),
            arms,
            selected: None,
            tail_position: std::sync::atomic::AtomicBool::new(false),
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
        let arg = Cached::new(compile(ctx, flags, arg.clone(), scope, top_id)?);
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
                let n = Cached::new(compile(ctx, flags, spec.clone(), &scope, top_id)?);
                Ok((pat, n))
            })
            .collect::<Result<Vec<_>>>()
            .with_context(|| format!("in select at {}", spec.pos))?;
        let typ = Type::empty_tvar();
        Ok(Box::new(Self {
            spec,
            typ,
            arg,
            arms,
            selected: None,
            tail_position: std::sync::atomic::AtomicBool::new(false),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Select<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<TagValue> {
        let Self { selected, arg, arms, typ: _, spec: _, tail_position } = self;
        let mut pat_up = false;
        let arg_prod = arg.update(ctx, event);
        let tainted = arg.tag.is_tainted();
        let arg_up = arg_prod.is_some();
        let arg_fired = arg_prod.is_some_and(|t| t.is_fired());
        // Fold a tail-spine scrutinee's firing into the dispatch-wide
        // accumulator (the kernel's `tail_scrut_stale`, folded by
        // `emit_select_node_tail` per pass and applied at every
        // `emit_kernel_return`). The arms terminate individually and a
        // jump arm's emission is swallowed by the tail-call stash, so
        // this is the only channel that carries the scrutinee's
        // control-dependence firing to the final base-arm emission — a
        // const base arm re-selected by a later cycle's loop read
        // stale here while the kernel's return fold fired (jul21g
        // divergence). Depth-0 passes fold too: a dispatch whose
        // previous cycle didn't loop runs its first pass unframed, and
        // that pass's entry delivery is iteration 1's disc.
        if arg_fired && tail_position.load(Ordering::Relaxed) {
            ctx.tail_scrut_fired = true;
        }
        // Arm binds carry the SCRUTINEE's production tag (the kernel's
        // arm-bind disc carry): a stale scrutinee production — a
        // framed re-derivation from a quiet entry — binds STALE
        // leaves; a wake with NO production this update (a guard-flip
        // re-selection) binds the value channel. Firing comes from the
        // selection/emission rules, never from the binds themselves
        // (Eric's ruling 2026-07-18, tail_jump_fired_plumbing).
        let bind_tag = arg_prod.unwrap_or(Tag::STALE);
        macro_rules! bind {
            ($i:expr) => {{
                if let Some(arg) = arg.cached.as_ref() {
                    arms[$i].0.bind_event(ctx, event, arg, bind_tag);
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
            let bind_guard = arg_up && !tainted && pat.guard.is_some();
            if bind_guard {
                if let Some(arg) = arg.cached.as_ref() {
                    pat.bind_event(ctx, event, arg, bind_tag);
                }
            }
            pat_up |= pat.update(ctx, event);
            if bind_guard {
                pat.unbind_event(event);
            }
        }
        // Destructuring-consumer force (the kernel's is_tainted gate at
        // dispatch): a tainted scrutinee production can't be matched —
        // the whole select produces the taint placeholder.
        if tainted {
            return if arg_up { Some(TagValue::tainted(Value::Null)) } else { None };
        }
        if std::env::var_os("GRAPHIX_DBG_SELECT").is_some() {
            eprintln!(
                "SELECT upd init={} arg_up={arg_up} pat_up={pat_up} sel={:?} vars={}",
                event.init,
                selected,
                event.variables.len()
            );
        }
        // TAIL-SPINE selects inside evaluation FRAMES ride the ARM's
        // organic tag alone: the scrutinee is the loop variable, whose
        // per-jump FIRED deliveries are loop plumbing, not observable
        // events (the kernel's `emit_body_tail` no-scrutinee-fold
        // rule; replay-frames v3, 2026-07-16 — a call's result is an
        // event iff an input the result depends on fired). At frame
        // depth 0 the scrutinee firing is a genuine entry event and
        // the normal fold applies.
        let tail = tail_position.load(Ordering::Relaxed) && ctx.frame_depth > 0;
        // An arm result's tag: tainted if the arm's resident value is a
        // placeholder; else fired iff the arm production fired, or the
        // scrutinee production fired and was consumed (the #178 disc
        // algebra fold — suppressed on the in-frame tail spine); else
        // stale (the value channel).
        macro_rules! emit {
            ($i:expr, $prod:expr) => {{
                let i = $i;
                if arms[i].1.tag.is_tainted() {
                    Some(TagValue::tainted(Value::Null))
                } else {
                    let fired =
                        $prod.is_some_and(|t: Tag| t.is_fired()) || (arg_fired && !tail);
                    let tag = if fired { Tag::FIRED } else { Tag::STALE };
                    arms[i].1.cached.clone().map(|v| TagValue::tagged(v, tag))
                }
            }};
        }
        if !arg_up && !pat_up {
            self.selected.and_then(|i| match arms[i].1.update(ctx, event) {
                None => None,
                prod => emit!(i, prod),
            })
        } else {
            let sel = match arg.cached.as_ref() {
                None => None,
                Some(v) => arms.iter().enumerate().find_map(|(i, (pat, _))| {
                    if pat.is_match(&ctx.env, v) { Some(i) } else { None }
                }),
            };
            match (sel, *selected) {
                (Some(i), Some(j)) if i == j => {
                    if arg_up {
                        bind!(i);
                    }
                    let prod = arms[i].1.update(ctx, event);
                    if prod.is_some() || arg_up { emit!(i, prod) } else { None }
                }
                (Some(i), Some(_) | None) => {
                    let mut set: LPooled<Vec<BindId>> = LPooled::take();
                    if let Some(j) = *selected {
                        arms[j].1.node.sleep(ctx);
                    }
                    *selected = Some(i);
                    bind!(i);
                    // Seed the woken arm's quiet externals from the
                    // runtime cache so it can evaluate (the arm's init
                    // view); already-delivered ids keep their honest
                    // event entries.
                    let mut refs = Refs::default();
                    arms[i].1.node.refs(&mut refs);
                    refs.with_external_refs(|id| {
                        if let Entry::Vacant(e) = event.variables.entry(id) {
                            if let Some(v) = ctx.rt.cached().get(&id) {
                                // FIRED: an arm wake is the arm's init view
                                e.insert(TagValue::fired(v.clone()));
                                set.push(id);
                            }
                        }
                    });
                    let init = event.init;
                    event.init = true;
                    let prod = arms[i].1.update(ctx, event);
                    event.init = init;
                    for id in set.drain(..) {
                        event.variables.remove(&id);
                    }
                    if arms[i].1.tag.is_tainted() {
                        Some(TagValue::tainted(Value::Null))
                    } else if tail {
                        // A selection change on the in-frame tail
                        // spine is loop mechanics (the dispatch arm
                        // oscillates between the jump and base arms
                        // every framed evaluation) — the result is an
                        // event iff the arm's own production fired.
                        // The scrutinee's control-dependence firing is
                        // NOT lost by the stale read here: it rides
                        // `ctx.tail_scrut_fired` (folded above) and
                        // upgrades the final result tag at the
                        // dispatch, the kernel's return fold (jul21g
                        // divergence: a const base arm re-selected by
                        // a later cycle's loop). Residual: a FEEDER-
                        // guard-driven re-selection of a const arm
                        // (scrutinee quiet, a capture flips the guard)
                        // reads stale here where the kernel's
                        // `tail_sel_path` selection word fires on
                        // final-selection change — no known witness;
                        // revisit if the fuzzer finds one.
                        let fired = prod.is_some_and(|t| t.is_fired());
                        let tag = if fired { Tag::FIRED } else { Tag::STALE };
                        arms[i].1.cached.clone().map(|v| TagValue::tagged(v, tag))
                    } else {
                        // BECOMING selected is the fire (Eric's ruled
                        // select semantics; the kernel's selection-
                        // memory word compare). `selected` is semantic
                        // state that persists across frames and cycles
                        // (5e246d2f), so reaching this path at all
                        // means the selection genuinely changed — the
                        // old trigger-derivation (and its
                        // body_input_fired approximation, which
                        // over-counted deliveries a quiet inner select
                        // had suppressed) is subsumed.
                        arms[i].1.cached.clone().map(|v| TagValue::fired(v))
                    }
                }
                (None, Some(j)) => {
                    arms[j].1.node.sleep(ctx);
                    *selected = None;
                    None
                }
                (None, None) => None,
            }
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        let Self { selected: _, arg, arms, typ: _, spec: _, tail_position: _ } = self;
        arg.node.delete(ctx);
        for (pat, arg) in arms {
            arg.node.delete(ctx);
            pat.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        let Self { selected: _, arg, arms, typ: _, spec: _, tail_position: _ } = self;
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
        let Self { selected: _, arg, arms, typ: _, spec: _, tail_position: _ } = self;
        arg.reset_replay(ctx);
        for (pat, arg) in arms {
            arg.reset_replay(ctx);
            if let Some(n) = &mut pat.guard {
                n.reset_replay(ctx)
            }
        }
    }

    fn refs(&self, refs: &mut Refs) {
        let Self { selected: _, arg, arms, typ: _, spec: _, tail_position: _ } = self;
        arg.node.refs(refs);
        for (pat, arg) in arms {
            arg.node.refs(refs);
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
                Some(guard) => guard.node.typecheck0(ctx)?,
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
            wrap!(n.node, n.node.typecheck0(ctx))?;
            rtype = rtype.union(&ctx.env, n.node.typ())?;
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
            wrap!(n.node, n.node.typecheck1(ctx))?;
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
}
