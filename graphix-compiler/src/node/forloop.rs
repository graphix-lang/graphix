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
    BindId, CFlag, Event, ExecCtx, ExprId, Node, NodeView, PrintFlag, Refs,
    Rt, Scope, Update, UserEvent,
    expr::{Expr, StructurePattern},
    fusion::emit::{BodyCx, CompiledExpr},
    typ::Type,
    wrap,
};
use anyhow::Result;
use enumflags2::BitFlags;
use netidx::publisher::Value;
use nohash::IntMap;
use std::sync::atomic::{AtomicBool, Ordering};

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
    /// The ids the body tree BINDS internally (the desugar's shadow
    /// `let`s etc.), computed lazily like `ext_refs`. The sync loop
    /// removes them from `event.variables` before each iteration: a
    /// shadow bind published by iteration i would otherwise keep its
    /// value visible to iteration i+1's reads, so a BOTTOMED body
    /// evaluation "fired" with the previous element's value (soak
    /// jul10c generate 000000 — `fold(.., 1, |v0, _| v0 % v0)` emitted
    /// 0 in the interp where 0 % 0 must bottom the loop; the kernel
    /// was right).
    body_bound: std::sync::OnceLock<Vec<BindId>>,
    /// Set by analysis pass 4 when the body has an ASYNC effect: the
    /// loop switches to per-index instantiation + re-evaluation (each
    /// element gets its OWN body instance so element-distinct async
    /// state — a subscription per element — is possible; the design's
    /// "per-element lambda instantiation", minus any cloning).
    async_body: AtomicBool,
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
            body_bound: std::sync::OnceLock::new(),
            async_body: AtomicBool::new(
                std::env::var_os("GXDBG_FOR_FORCE_ASYNC").is_some(),
            ),
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

    /// The ids the body tree binds internally — see the field docs.
    /// Collected via the fusion walker's `Bind` nodes only: the full
    /// `refs().bound` set also contains call-site arg ids, connect
    /// targets, and resolved-instance interiors, and clearing THOSE
    /// per iteration broke cross-node delivery (the nested-HOF tests
    /// hung under FusionDisabled).
    fn body_bound(&self) -> &[BindId] {
        self.body_bound.get_or_init(|| {
            let mut ids: Vec<BindId> = Vec::new();
            crate::fusion::for_each_node(&self.body, &mut |n| {
                if let NodeView::Bind(b) = n.view() {
                    b.pattern.ids(&mut |id| {
                        if !ids.contains(&id) {
                            ids.push(id);
                        }
                    });
                }
            });
            let mut skip: Vec<BindId> = Vec::new();
            self.acc_pattern.ids(&mut |id| skip.push(id));
            self.elem_pattern.ids(&mut |id| skip.push(id));
            ids.retain(|id| !skip.contains(id));
            ids
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
    ) -> Option<Value> {
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
                        // priming a lazy CallSite bind does.
                        let mut refs = Refs::default();
                        inst.refs(&mut refs);
                        refs.with_external_refs(|id| {
                            if let Some(v) = ctx.rt.cached().get(&id) {
                                event.variables.entry(id).or_insert_with(|| v.clone());
                            }
                        });
                        self.instances.insert(i, inst);
                        fresh = true;
                    }
                    Err(e) => {
                        // An instance that can't compile is a bug (the
                        // shared body compiled) — log, never complete.
                        log::error!(
                            "for-loop instance {i} failed to compile: {e:#}"
                        );
                        return None;
                    }
                }
            }
            if complete {
                self.acc_pattern.bind(&acc, &mut |id, v| {
                    ctx.rt.cached_mut().insert(id, v.clone());
                    event.variables.insert(id, v);
                });
            }
            self.elem_pattern.bind(v, &mut |id, v| {
                ctx.rt.cached_mut().insert(id, v.clone());
                event.variables.insert(id, v);
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
                self.acc_pattern.ids(&mut |id| {
                    ids.push((id, event.variables.contains_key(&id)))
                });
                self.elem_pattern.ids(&mut |id| {
                    ids.push((id, event.variables.contains_key(&id)))
                });
                eprintln!(
                    "FOR-ASYNC i={i} fresh={fresh} complete={complete} r={r:?} \
                     ids={ids:?} spec={}",
                    self.body_spec
                );
            }
            match r {
                Some(v) => {
                    inst_fired = true;
                    if complete {
                        acc = v;
                    }
                }
                // Not arrived yet (or bottom): the chain halts here,
                // but LATER instances still pump their async state.
                None => complete = false,
            }
        }
        let fired = iter_up || init_up || ext_fired || inst_fired || event.init;
        if complete && fired { Some(acc) } else { None }
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for For<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        let iter_up = self.iter.update(ctx, event);
        let init_up = self.init.update(ctx, event);
        let arr = match self.iter.cached.as_ref() {
            Some(Value::Array(a)) => a.clone(),
            Some(_) | None => return None,
        };
        let ext_fired =
            self.ext_refs().iter().any(|id| event.variables.contains_key(id));
        if self.async_body.load(Ordering::Relaxed) {
            // The async walk runs EVERY cycle (instances must pump
            // their own async arrivals, whose ids the For can't know);
            // emission is gated inside on something having fired.
            let init = self.init.cached.as_ref()?.clone();
            return self.update_async(
                ctx, event, &arr, init, ext_fired, iter_up, init_up,
            );
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
        // One call-depth unit covers the whole loop (the JIT scaffold's
        // `emit_depth_unit` twin): sequential element dispatches all
        // sit at the same level. A trip means the loop must not run —
        // bottom (never-until-complete).
        if !ctx.control.depth_enter() {
            ctx.control.depth_pop();
            return None;
        }
        // STRICT sequential semantics: a BOTTOM init (`None` cached —
        // a depth-tripped or errored init expression) bottoms the
        // whole loop; `let mut acc = ⊥` can't proceed. The kernel
        // agrees (the init's taint is sticky in the fold scaffold).
        let Some(init) = self.init.cached.clone() else {
            ctx.control.depth_pop();
            return None;
        };
        let mut acc: Option<Value> = Some(init);
        let mut out = true;
        for v in arr.iter() {
            // Cooperative interrupt poll — a wedged huge loop aborts to
            // bottom, mirroring the scaffold loop head's poll.
            if ctx.control.interrupted() {
                out = false;
                break;
            }
            if std::env::var_os("GXDBG_NOCLEAR").is_none() {
                for id in self.body_bound() {
                    event.variables.remove(id);
                }
            }
            if let Some(a) = &acc {
                self.acc_pattern.bind(a, &mut |id, v| {
                    ctx.rt.cached_mut().insert(id, v.clone());
                    event.variables.insert(id, v);
                });
            }
            self.elem_pattern.bind(v, &mut |id, v| {
                ctx.rt.cached_mut().insert(id, v.clone());
                event.variables.insert(id, v);
            });
            match self.body.update(ctx, event) {
                Some(v) => acc = Some(v),
                // NEVER-UNTIL-COMPLETE: a bottomed element (or a
                // not-yet-arrived async value) suppresses the whole
                // loop's emission this cycle. The kernel agrees — a
                // body evaluation's taint is STICKY in the fold
                // scaffold.
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
        if out { acc } else { None }
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
