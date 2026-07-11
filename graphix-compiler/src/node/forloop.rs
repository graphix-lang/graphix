//! The `For` node — the sync subset's loop (design/sync_subset.md,
//! "P4 final scope"). Compiled from the desugar-internal
//! `ExprKind::ForFold`: a fold whose accumulator is the tuple of the
//! mut locals the loop body assigns.
//!
//! Emission contract: NEVER-UNTIL-COMPLETE — the loop emits only when
//! every element produced a value this evaluation; a bottomed body (or,
//! for an async body, a not-yet-arrived value) means no emission this
//! cycle. This matches the old fold contract and the JIT's #219
//! loop-carried taint model.
//!
//! Sequential semantics ARE the semantics: the sync body is ONE node
//! tree re-run in place per element (the `GXLambda` tail-loop
//! precedent) — a stateful Sync builtin in the body (`count`) is shared
//! across iterations, exactly what a loop means in a sequential
//! language. The per-slot-counter behavior of the old HOF machinery
//! dies with it.
//!
//! Async bodies (per-index instantiation + re-evaluation under taint)
//! are the second phase of this construction — see `design/
//! sync_subset.md`. Until that lands, an async call in a loop body has
//! ONE shared instance and its cross-iteration semantics are wrong;
//! the differential fixtures gate the phase.

use super::{Cached, compiler::compile, pattern::StructPatternNode};
use crate::{
    BindId, CFlag, Event, ExecCtx, ExprId, Node, NodeView, PrintFlag, Refs, Rt, Scope,
    TagValue, Update, UserEvent,
    expr::{Expr, StructurePattern},
    fusion::emit::{BodyCx, CompiledExpr},
    typ::Type,
    wrap,
};
use anyhow::Result;
use enumflags2::BitFlags;
use netidx::publisher::Value;
use nohash::IntMap;
use poolshark::local::LPooled;
use std::{
    mem,
    sync::atomic::{AtomicBool, Ordering},
};

#[derive(Debug)]
pub struct For<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub iter: Cached<R, E>,
    pub init: Cached<R, E>,
    pub acc_pattern: StructPatternNode,
    pub elem_pattern: StructPatternNode,
    pub body: Node<R, E>,
    /// The element type cell the elem pattern was compiled against —
    /// the iter's Array element unifies into it at typecheck.
    elem_t: Type,
    /// The body's EXTERNAL references (its refs minus the ids the
    /// acc/elem patterns bind) — the loop re-runs when any of these
    /// fire, in addition to iter/init updates and the init view.
    /// Filled LAZILY on first use: a compile-time walk ran before
    /// `typecheck1`'s static resolution, so a callee instance's
    /// CAPTURES (`CallSite::refs` → the resolved apply's refs) were
    /// invisible and a capture-only event never re-ran the loop.
    ext_refs: std::sync::OnceLock<Vec<BindId>>,
    /// Set by analysis pass 4 when the body has an ASYNC effect: the
    /// loop switches to per-index instantiation + re-evaluation (each
    /// element gets its OWN body instance so element-distinct async
    /// state — a subscription per element — is possible; the design's
    /// "per-element lambda instantiation", minus any cloning).
    async_body: AtomicBool,
    /// The sync loop's first ITERATING run (≥1 element) hasn't happened
    /// yet: that run is forced to an init view so the body's constants
    /// and defaults materialize — the loop-level mirror of a call
    /// site's first-dispatch priming. Later runs get no init view; a
    /// body evaluation fires only from what it consumes.
    primed: bool,
    /// Per-index body instances (async path only), compiled on demand
    /// from `body_spec` in the captured `env` — the NORMAL compile
    /// path, never clone_rebind.
    instances: IntMap<usize, Node<R, E>>,
    body_spec: Expr,
    env: crate::env::Env,
    flags: BitFlags<CFlag>,
    scope: Scope,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> For<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        iter: &Expr,
        init: &Expr,
        acc_pattern: &StructurePattern,
        elem_pattern: &StructurePattern,
        body: &Expr,
    ) -> Result<Node<R, E>> {
        let iter = Cached::new(compile(ctx, flags, iter.clone(), scope, top_id)?);
        let init = Cached::new(compile(ctx, flags, init.clone(), scope, top_id)?);
        // The acc's type is the loop-carried unification knot: init
        // seeds it, the body result must stay within it.
        let acc_t = Type::empty_tvar();
        let elem_t = Type::empty_tvar();
        let acc_pattern = StructPatternNode::compile(
            ctx,
            &acc_t,
            acc_pattern,
            scope,
            spec.pos,
            spec.ori.clone(),
        )?;
        let elem_pattern = StructPatternNode::compile(
            ctx,
            &elem_t,
            elem_pattern,
            scope,
            spec.pos,
            spec.ori.clone(),
        )?;
        // Snapshot the env BEFORE the shared body compiles: instance
        // re-compiles must resolve names exactly as the shared body's
        // first compile did — the shared body's own interior binds (the
        // desugar's shadow `let res`) land in ctx.env and would
        // otherwise shadow the acc pattern's binding for every
        // instance.
        let env = ctx.env.clone();
        let body = compile(ctx, flags, body.clone(), scope, top_id)?;
        let body_spec = body.spec().clone();
        Ok(Box::new(Self {
            spec,
            typ: acc_t,
            iter,
            init,
            acc_pattern,
            elem_pattern,
            body,
            elem_t,
            ext_refs: std::sync::OnceLock::new(),
            async_body: AtomicBool::new(
                std::env::var_os("GXDBG_FOR_FORCE_ASYNC").is_some(),
            ),
            primed: false,
            instances: IntMap::default(),
            body_spec,
            env,
            flags,
            scope: scope.clone(),
            top_id,
        }))
    }
}

impl<R: Rt, E: UserEvent> For<R, E> {
    /// The element type cell (the iter's Array element unified into
    /// it at typecheck) — the emitter's element-shape authority.
    pub fn elem_typ(&self) -> &Type {
        &self.elem_t
    }

    /// The body's external references — the loop re-runs when any of
    /// these fire, and the emitter folds their discs into the loop's
    /// firing (coarse sequential firing: any consumed input fires the
    /// loop). Computed on first use — after `typecheck1`'s static
    /// resolution, so resolved callee instances contribute their
    /// captures.
    pub fn ext_refs(&self) -> &[BindId] {
        self.ext_refs.get_or_init(|| {
            let mut refs = Refs::default();
            self.body.refs(&mut refs);
            self.acc_pattern.ids(&mut |id| refs.mark_bound(id));
            self.elem_pattern.ids(&mut |id| refs.mark_bound(id));
            let mut ext: Vec<BindId> = Vec::new();
            refs.with_external_refs(|id| {
                if !ext.contains(&id) {
                    ext.push(id);
                }
            });
            ext
        })
    }

    /// Analysis pass 4's marking hook — `&self` (the pass walks shared
    /// nodes), hence the atomic.
    pub fn set_async_body(&self, v: bool) {
        let v = v || std::env::var_os("GXDBG_FOR_FORCE_ASYNC").is_some();
        self.async_body.store(v, Ordering::Relaxed)
    }

    /// The ASYNC evaluation: per-index instances pump their own async
    /// state every cycle; the acc CHAIN threads through as far as the
    /// arrived values allow (an instance's Cached interiors let its
    /// sync part re-fire against a fresh acc binding), and the loop
    /// emits only when a full pass completes — never-until-complete.
    fn update_async(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
        arr: &netidx_value::ValArray,
        init: Value,
        ext_fired: bool,
        iter_up: bool,
        init_up: bool,
    ) -> Option<TagValue> {
        let n = arr.len();
        // The array shrank: excess instances' async state dies with
        // their elements (re-evaluation semantics).
        let excess: Vec<usize> =
            self.instances.keys().copied().filter(|i| *i >= n).collect();
        for i in excess {
            if let Some(mut inst) = self.instances.remove(&i) {
                inst.delete(ctx);
            }
        }
        let mut acc = init;
        let mut complete = true;
        let mut inst_fired = false;
        for (i, v) in arr.iter().enumerate() {
            if ctx.control.interrupted() {
                return None;
            }
            let mut fresh = false;
            if !self.instances.contains_key(&i) {
                let inst = {
                    let body_spec = self.body_spec.clone();
                    let scope = self.scope.clone();
                    let flags = self.flags;
                    let top_id = self.top_id;
                    ctx.with_restored(self.env.clone(), |ctx| {
                        let mut inst = compile(ctx, flags, body_spec, &scope, top_id)?;
                        inst.typecheck0(ctx)?;
                        Ok::<_, anyhow::Error>(inst)
                    })
                };
                match inst {
                    Ok(inst) => {
                        // A runtime-compiled instance missed analysis
                        // pass 4 — mark its own nested For loops (the
                        // inner loop of a nested async for must also
                        // instantiate per index).
                        crate::analysis::mark_for_bodies_standalone(&inst, ctx);
                        // Prime the fresh instance's EXTERNAL refs from
                        // the runtime cache so its first (init-forced)
                        // update sees outer bindings — the same
                        // priming a lazy CallSite bind does. FIRED:
                        // a fresh instance's first dispatch is an init
                        // view, everything it sees is new to it (only
                        // RE-RUN frames of the same tree seed stale).
                        let mut refs = Refs::default();
                        inst.refs(&mut refs);
                        refs.with_external_refs(|id| {
                            if let Some(v) = ctx.rt.cached().get(&id) {
                                event
                                    .variables
                                    .entry(id)
                                    .or_insert_with(|| TagValue::fired(v.clone()));
                            }
                        });
                        self.instances.insert(i, inst);
                        fresh = true;
                    }
                    Err(e) => {
                        // An instance that can't compile is a bug (the
                        // shared body compiled) — log, never complete.
                        log::error!("for-loop instance {i} failed to compile: {e:#}");
                        return None;
                    }
                }
            }
            if complete {
                self.acc_pattern.bind(&acc, &mut |id, v| {
                    ctx.rt.cached_mut().insert(id, v.clone());
                    event.variables.insert(id, TagValue::fired(v));
                });
            }
            self.elem_pattern.bind(v, &mut |id, v| {
                ctx.rt.cached_mut().insert(id, v.clone());
                event.variables.insert(id, TagValue::fired(v));
            });
            let inst = self.instances.get_mut(&i).unwrap();
            // A fresh instance's FIRST update runs under a forced init
            // view (its Refs and lazy callsite binds fire from cached
            // state) — the flag is restored immediately after, so only
            // this subtree sees it.
            let saved_init = event.init;
            if fresh {
                event.init = true;
            }
            let r = inst.update(ctx, event);
            event.init = saved_init;
            if std::env::var_os("GXDBG_FOR").is_some() {
                let mut ids: Vec<(BindId, bool)> = Vec::new();
                self.acc_pattern
                    .ids(&mut |id| ids.push((id, event.variables.contains_key(&id))));
                self.elem_pattern
                    .ids(&mut |id| ids.push((id, event.variables.contains_key(&id))));
                eprintln!(
                    "FOR-ASYNC i={i} fresh={fresh} complete={complete} r={r:?} \
                     ids={ids:?} spec={}",
                    self.body_spec
                );
            }
            match r {
                Some(tv) if tv.is_tainted() => {
                    // a tainted evaluation is a bottom: never complete
                    // (sticky, the kernel's loop-carried taint)
                    complete = false;
                }
                Some(tv) => {
                    let tag = tv.tag();
                    if tag.is_fired() {
                        inst_fired = true;
                    }
                    if complete {
                        acc = tv.value();
                    }
                }
                // Not arrived yet (or bottom): the chain halts here,
                // but LATER instances still pump their async state.
                None => complete = false,
            }
        }
        let fired = iter_up || init_up || ext_fired || inst_fired || event.init;
        if complete && fired { Some(TagValue::fired(acc)) } else { None }
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for For<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<TagValue> {
        let iter_up = self.iter.update_triggers(ctx, event);
        let init_up = self.init.update_triggers(ctx, event);
        let arr = match self.iter.cached.as_ref() {
            Some(Value::Array(a)) => a.clone(),
            Some(_) | None => return None,
        };
        // A merely-stale re-delivery of an external is NOT a firing;
        // only fired (or tainted) entries re-run the loop.
        let ext_fired = self
            .ext_refs()
            .iter()
            .any(|id| event.variables.get(id).is_some_and(|tv| tv.tag().triggers()));
        if self.async_body.load(Ordering::Relaxed) {
            // The async walk runs EVERY cycle (instances must pump
            // their own async arrivals, whose ids the For can't know);
            // emission is gated inside on something having fired.
            let init = self.init.cached.as_ref()?.clone();
            return self
                .update_async(ctx, event, &arr, init, ext_fired, iter_up, init_up);
        }
        if std::env::var_os("GXDBG_FOR").is_some() {
            eprintln!(
                "FOR-SYNC-GATE spec={} iter_up={iter_up} init_up={init_up} ext={ext_fired} evinit={} iter_cached={} init_cached={}",
                self.spec,
                event.init,
                self.iter.cached.is_some(),
                self.init.cached.is_some()
            );
        }
        if !(iter_up || init_up || ext_fired || event.init) {
            return None;
        }
        // STRICT sequential semantics: a TAINTED iter or init bottoms
        // the whole loop (sticky, the kernel's loop-carried taint).
        if self.iter.tag.is_tainted() || self.init.tag.is_tainted() {
            return Some(TagValue::tainted(Value::Null));
        }
        // One call-depth unit covers the whole loop (the JIT scaffold's
        // `emit_depth_unit` twin): sequential element dispatches all
        // sit at the same level. A trip means the loop must not run —
        // bottom (never-until-complete).
        if !ctx.control.depth_enter() {
            ctx.control.depth_pop();
            return None;
        }
        // A BOTTOM init (`None` cached — a depth-tripped or errored
        // init expression) bottoms the whole loop; `let mut acc = ⊥`
        // can't proceed.
        let Some(init) = self.init.cached.clone() else {
            ctx.control.depth_pop();
            return None;
        };
        let mut acc: Option<Value> = Some(init);
        let mut out = true;
        // Body-driven firing (two-channel): the loop's result fires iff
        // any body evaluation's RESULT fired — a body that consumes
        // only stale values computes the acc chain on the value channel
        // and stays quiet, exactly the kernel's AND-reduced STALE disc.
        // The first ITERATING run's forced init view counts as fired
        // (constants materialize = the loop's own init view), and an
        // empty source falls back to "an input fired" (the top gate).
        let mut fired_any = false;
        // FRAME DISCIPLINE (reset_replay): each iteration is a fresh
        // evaluation frame, delivered exactly like a select-arm wake —
        // replay caches cleared, then a PRIVATE variables map under a
        // (first-run) forced init view. Swapping the map instead of
        // enumerating ids to remove is what makes the frame leak-proof
        // by construction: a body publish (a shadow bind, a callsite
        // arg, a callee formal) dies with the frame, so iteration i−1's
        // sub-results cannot resurrect when iteration i's producer
        // bottoms. The kernel needs none of this — its temps are SSA,
        // recomputed per iteration by construction.
        //
        // Seeds are the kernel's params: an external that FIRED this
        // cycle keeps its real entry (fired for the whole pass, every
        // iteration — the kernel's per-invocation param disc); a quiet
        // external is seeded STALE from the runtime cache (the value
        // channel, delivery-fresh per cached-into-Rt).
        let mut seeds: LPooled<IntMap<BindId, TagValue>> = LPooled::take();
        for id in self.ext_refs() {
            if let Some(tv) = event.variables.get(id) {
                seeds.insert(*id, tv.clone());
            } else if let Some(v) = ctx.rt.cached().get(id) {
                seeds.insert(*id, TagValue::stale(v.clone()));
            }
        }
        let mut frame: LPooled<IntMap<BindId, TagValue>> = LPooled::take();
        // First ITERATING run: force the init view so the body's
        // constants/defaults materialize into their (frame-surviving)
        // caches — see the `primed` field. Later runs run un-forced: a
        // body evaluation fires only from what it consumes, which is
        // what keeps a const-callback fold quiet after its first-ever
        // evaluation (the hof-lift-firing pin; the kernel's slots-word
        // agrees).
        let force_init = !self.primed && !arr.is_empty();
        if force_init {
            self.primed = true;
            fired_any = true;
        }
        for v in arr.iter() {
            // Cooperative interrupt poll — a wedged huge loop aborts to
            // bottom, mirroring the scaffold loop head's poll.
            if ctx.control.interrupted() {
                out = false;
                break;
            }
            self.body.reset_replay(ctx);
            frame.clear();
            frame.extend(seeds.iter().map(|(k, v)| (*k, v.clone())));
            if let Some(a) = &acc {
                self.acc_pattern.bind(a, &mut |id, v| {
                    ctx.rt.cached_mut().insert(id, v.clone());
                    frame.insert(id, TagValue::fired(v));
                });
            }
            self.elem_pattern.bind(v, &mut |id, v| {
                ctx.rt.cached_mut().insert(id, v.clone());
                frame.insert(id, TagValue::fired(v));
            });
            mem::swap(&mut event.variables, &mut *frame);
            let saved_init = event.init;
            event.init = saved_init || force_init;
            ctx.frame_depth += 1;
            let r = self.body.update(ctx, event);
            ctx.frame_depth -= 1;
            event.init = saved_init;
            mem::swap(&mut event.variables, &mut *frame);
            match r {
                // A tainted body result is a GENUINE bottom — sticky
                // (never-until-complete = the sequential break), like
                // the kernel's loop-carried taint.
                Some(tv) if tv.is_tainted() => {
                    out = false;
                    break;
                }
                Some(tv) => {
                    let tag = tv.tag();
                    fired_any |= tag.is_fired();
                    acc = Some(tv.value());
                }
                // NEVER-UNTIL-COMPLETE: a bottomed element (nothing in
                // the body produced, not even the value channel)
                // suppresses the whole loop's emission this cycle.
                None => {
                    if std::env::var_os("GXDBG_FOR").is_some() {
                        eprintln!("FOR-SYNC-BODYNONE spec={} elem={v}", self.spec);
                    }
                    out = false;
                    break;
                }
            }
        }
        ctx.control.depth_pop();
        if !out {
            return None;
        }
        if arr.is_empty() {
            // Source empty: the result is the init, fired iff an input
            // fired — the top gate already established that.
            return acc.map(TagValue::fired);
        }
        if fired_any {
            acc.map(TagValue::fired)
        } else {
            // Complete pass, nothing fired: the acc chain advanced on
            // the value channel only — a stale production (the parent
            // caches the value; nothing downstream fires).
            acc.map(TagValue::stale)
        }
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        self.iter.node.refs(refs);
        self.init.node.refs(refs);
        self.acc_pattern.ids(&mut |id| refs.mark_bound(id));
        self.elem_pattern.ids(&mut |id| refs.mark_bound(id));
        self.body.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.iter.node.delete(ctx);
        self.init.node.delete(ctx);
        self.body.delete(ctx);
        for (_, mut inst) in std::mem::take(&mut self.instances) {
            inst.delete(ctx);
        }
        self.acc_pattern.ids(&mut |id| ctx.env.unbind_variable(id));
        self.elem_pattern.ids(&mut |id| ctx.env.unbind_variable(id));
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.iter.sleep(ctx);
        self.init.sleep(ctx);
        self.body.sleep(ctx);
        for inst in self.instances.values_mut() {
            inst.sleep(ctx);
        }
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        // A nested For inside an enclosing frame: the iter/init caches
        // hold the PREVIOUS enclosing-frame's values (an init that
        // reads the outer element must re-evaluate), and the published
        // acc/elem binds are this loop's own frame state.
        self.iter.reset_replay(ctx);
        self.init.reset_replay(ctx);
        self.acc_pattern.ids(&mut |id| {
            ctx.rt.cached_mut().remove(&id);
        });
        self.elem_pattern.ids(&mut |id| {
            ctx.rt.cached_mut().remove(&id);
        });
        self.body.reset_replay(ctx);
        for inst in self.instances.values_mut() {
            inst.reset_replay(ctx);
        }
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.iter.node, self.iter.node.typecheck0(ctx))?;
        wrap!(self.init.node, self.init.node.typecheck0(ctx))?;
        // iter must be an Array; its element type feeds the elem
        // pattern's cells (bound at pattern compile).
        let elem_t = crate::deref_typ!("array", ctx, self.iter.node.typ(),
            Some(Type::Array(et)) => Ok((**et).clone())
        );
        let elem_t = wrap!(self.iter.node, elem_t)?;
        wrap!(self.iter.node, self.elem_t.check_contains(&ctx.env, &elem_t))?;
        // init seeds the acc knot; the body result must stay within it.
        wrap!(self.init.node, self.typ.check_contains(&ctx.env, &self.init.node.typ()))?;
        wrap!(self.body, self.body.typecheck0(ctx))?;
        // The loop-carry check runs against a SNAPSHOT of the body's
        // type (`reset_tvars` — bindings carried, cells fresh): the
        // body type is a select-arm UNION whose members' interiors can
        // reference FOREIGN live cells (a stdlib callee's def-signature
        // tvar reached through an arm's callsite type), and a live
        // containment walk alias/copy-contaminated `array::push`'s def
        // 'a. Snapshot cells are throwaway, so the walk can only bind
        // the acc's OWN cells.
        let body_t = self.body.typ().reset_tvars();
        wrap!(self.body, self.typ.check_contains(&ctx.env, &body_t))?;
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.iter.node, self.iter.node.typecheck1(ctx))?;
        wrap!(self.init.node, self.init.node.typecheck1(ctx))?;
        wrap!(self.body, self.body.typecheck1(ctx))
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::For(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        crate::fusion::emit::emit_for_node(cx, self)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // Statement spine: descend (the loop itself fuses only as part
        // of a parent region via emit_clif above).
        crate::fusion::fuse(&mut self.iter.node, ctx)?;
        crate::fusion::fuse(&mut self.init.node, ctx)?;
        crate::fusion::fuse(&mut self.body, ctx)?;
        Ok(None)
    }
}
