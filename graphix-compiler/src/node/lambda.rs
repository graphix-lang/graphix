use super::{Nop, WakeBit, compiler::compile};
use crate::{
    Apply, ApplyView, ApplyViewMut, BindId, BindMode, CFlag, Event, ExecCtx, InitFn,
    LambdaId, LambdaInstanceId, Node, NodeView, Refs, Rt, Scope, Tag, TagValue, Update,
    UserEvent,
    effects::{EffectKind, RecursionKind},
    env::{Bind, Env},
    expr::{self, Arg, ErrorContext, Expr, ExprId, Origin},
    fusion::emit::{BodyCx, CompiledExpr},
    node::{
        callsite::CallSite, collection::CollectionIntrinsic, pattern::StructPatternNode,
    },
    profile::{self, Phase},
    typ::{FnArgKind, FnArgType, FnType, TVar, Type, fntyp::LambdaIds},
    wrap,
};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use combine::stream::position::SourcePosition;
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_core::pack::Pack;
use netidx_core::utils::Either;
use netidx_value::Value;
use nohash::IntMap;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use std::{
    collections::hash_map::Entry as MapEntry,
    fmt,
    hash::Hash,
    mem,
    sync::{
        Arc as SArc,
        atomic::{AtomicBool, Ordering},
    },
};
use triomphe::Arc;

pub struct LambdaDef<R: Rt, E: UserEvent> {
    pub id: LambdaId,
    /// The pretty-printed source: an identity stable across compiles.
    /// Not used for equality — `PartialEq` is id-based so same-source
    /// closures over different captures stay distinct.
    pub src: ArcStr,
    pub env: Env,
    pub scope: Scope,
    pub argspec: Arc<[Arg]>,
    pub typ: Arc<FnType>,
    pub init: InitFn<R, E>,
    pub check: Mutex<Option<Box<dyn Apply<R, E>>>>,
    /// Intrinsic sync/async effect, computed by `analysis::infer_effects`
    /// after all lambdas are compiled. Calls through fn-typed parameters
    /// do not contribute; the call site joins the resolved arg's effect.
    pub intrinsic_effect: Mutex<EffectKind>,
    /// The body holds no per-activation state: every builtin it reaches
    /// is `Effect::Stateless`, no `<-` targets its own binding, every
    /// callee is stateless. A tail loop reuses one activation only then.
    pub stateless: AtomicBool,
    /// How this lambda recurses, computed by `analysis::analyze`. The
    /// operational tail-loop gate is `GXLambda::tail_loop`, not this.
    pub recursion: Mutex<RecursionKind>,
    /// The lambda expression this def was compiled from; stable across
    /// instance re-compiles, which is what [`crate::FnArgIdentity`] keys on.
    pub source: ExprId,
}

impl<R: Rt, E: UserEvent> fmt::Debug for LambdaDef<R, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "lambda#{}", self.id.inner())
    }
}

impl<R: Rt, E: UserEvent> PartialEq for LambdaDef<R, E> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<R: Rt, E: UserEvent> Eq for LambdaDef<R, E> {}

impl<R: Rt, E: UserEvent> PartialOrd for LambdaDef<R, E> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.id.cmp(&other.id))
    }
}

impl<R: Rt, E: UserEvent> Ord for LambdaDef<R, E> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl<R: Rt, E: UserEvent> Hash for LambdaDef<R, E> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<R: Rt, E: UserEvent> Pack for LambdaDef<R, E> {
    fn encoded_len(&self) -> usize {
        0
    }

    fn encode(
        &self,
        _buf: &mut impl bytes::BufMut,
    ) -> std::result::Result<(), netidx_core::pack::PackError> {
        Err(netidx_core::pack::PackError::Application(0))
    }

    fn decode(
        _buf: &mut impl bytes::Buf,
    ) -> std::result::Result<Self, netidx_core::pack::PackError> {
        Err(netidx_core::pack::PackError::Application(0))
    }
}

/// A call-site instance of a Graphix lambda, produced by
/// [`LambdaDef::init`] when a `CallSite` resolves to it (lazily at
/// runtime or statically in `typecheck1`). Fusion reaches the body
/// through [`ApplyView::Lambda`].
#[derive(Debug)]
pub struct GXLambda<R: Rt, E: UserEvent> {
    /// wake catch-up: set by `sleep()`, taken by the next update
    slept: WakeBit,
    id: LambdaId,
    instance_id: LambdaInstanceId,
    args: Box<[StructPatternNode]>,
    body: Node<R, E>,
    typ: Arc<FnType>,
    /// `true` iff this lambda is sync, self-tail-recursive and has
    /// loop-able formals; set by `analysis::analyze`, read by both
    /// engines (`update` loops in place, the JIT emits a native loop).
    tail_loop: AtomicBool,
    self_recursive: AtomicBool,
    self_bind: Mutex<Option<BindId>>,
    /// The dispatch's return slot, lent to the owning `CallSite`.
    resident: TagValue,
    resumes_mid_recursion: bool,
    /// `true` until the first dispatch, which seeds the fresh formal
    /// ids' value channel from the args' quiet productions.
    first_dispatch: bool,
    /// The def-side lexical env the body was compiled under. The body
    /// typechecks under it too: the caller's env, which drives the
    /// checks, may lack the defining module's private typedefs.
    env: Env,
}

fn body_reads_triggered<R: Rt, E: UserEvent>(
    body: &Node<R, E>,
    ctx: &ExecCtx<R, E>,
    event: &Event<E>,
) -> bool {
    let mut refs = Refs::default();
    body.refs(&mut refs);
    let mut hit = false;
    refs.with_refs(|id| {
        hit |= matches!(
            super::read_var(ctx, event, &id),
            Some(super::VarRead::Delivered(tv)) if tv.tag().triggers()
        );
    });
    hit
}

impl<R: Rt, E: UserEvent> GXLambda<R, E> {
    /// The definition's id, shared by every instance of it.
    pub fn id(&self) -> LambdaId {
        self.id
    }

    fn run_tail_loop(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
        entry_fired: bool,
    ) -> TagValue {
        let mut frame: LPooled<IntMap<BindId, TagValue>> = LPooled::take();
        let mut reentered = false;
        let framed = self.resumes_mid_recursion
            && !event.init
            && (entry_fired || body_reads_triggered(&self.body, ctx, event));
        if framed {
            self.body.reset_replay(ctx);
            // a delivered formal keeps its cycle tag, a standing one
            // reads quiet
            for pat in self.args.iter() {
                pat.ids(&mut |id| {
                    if let Some(vr) = super::read_var(ctx, event, &id) {
                        let tv = match vr {
                            super::VarRead::Delivered(tv) => tv.clone(),
                            super::VarRead::Standing(tv) => {
                                let mut c = tv.clone();
                                let t = c.tag().quiet();
                                c.retag(t);
                                c
                            }
                        };
                        frame.insert(id, tv);
                    }
                });
            }
        }
        let prev_tsf = mem::replace(&mut ctx.tail_scrut_fired, false);
        let res = loop {
            if ctx.interrupted() {
                break self.resident.ride().clone();
            }
            let res = if !reentered && !framed {
                self.body.update(ctx, event).clone()
            } else {
                event.enter_frame(mem::take(&mut *frame));
                let prev = mem::replace(&mut event.init, true);
                // `dispatch_init` carries the dispatch's real init beside
                // the forced one; a nested dispatch inherits the outer's
                let real = if ctx.frame_depth > 0 { ctx.dispatch_init } else { prev };
                let prev_fi = mem::replace(&mut ctx.dispatch_init, real);
                ctx.frame_depth += 1;
                let res = self.body.update(ctx, event).clone();
                ctx.frame_depth -= 1;
                ctx.dispatch_init = prev_fi;
                event.init = prev;
                *frame = event.exit_frame();
                // deliver handler errors parked in `frame_outbox` once
                // `event.variables` is the real event again
                if ctx.frame_depth == 0 && !ctx.frame_outbox.is_empty() {
                    for (id, v) in mem::take(&mut ctx.frame_outbox) {
                        match event.variables.entry(id) {
                            MapEntry::Vacant(slot) => {
                                slot.insert(TagValue::fired(v));
                            }
                            MapEntry::Occupied(_) => ctx.rt.set_var(id, v),
                        }
                    }
                }
                res
            };
            if crate::dbgenv::gxdbg_tail() {
                eprintln!(
                    "TAILDBG id={:?} pass reentered={reentered} framed={framed} init={} fi={} res={:?} pending={:?}",
                    self.id,
                    event.init,
                    ctx.dispatch_init,
                    res,
                    ctx.pending_tail_call.as_ref().map(|p| (&p.lambda, &p.args))
                );
            }
            let mine = matches!(
                &ctx.pending_tail_call,
                Some(p) if p.lambda == self.id
            );
            if !mine {
                break res;
            }
            reentered = true;
            let p = ctx.pending_tail_call.take().unwrap();
            self.body.reset_replay(ctx);
            // A `None` arg rides the formal's previous entry, value and
            // tag: the last rebind in this evaluation, else the ordinary
            // read. Rebinds are frame-private.
            let prev: LPooled<IntMap<BindId, TagValue>> =
                mem::replace(&mut frame, LPooled::take());
            for (v, pat) in p.args.iter().zip(self.args.iter()) {
                match v {
                    Some(tv) => {
                        let (v, tag) = tv.clone().into_parts();
                        pat.bind(&v, &mut |id, v| {
                            frame.insert(id, TagValue::tagged(v, tag));
                        })
                    }
                    None => pat.ids(&mut |id| {
                        let tv =
                            prev.get(&id).cloned().or_else(|| {
                                match super::read_var(ctx, event, &id) {
                                    Some(super::VarRead::Delivered(tv)) => {
                                        Some(tv.clone())
                                    }
                                    Some(super::VarRead::Standing(tv)) => {
                                        let mut c = tv.clone();
                                        let t = c.tag().quiet();
                                        c.retag(t);
                                        Some(c)
                                    }
                                    None => None,
                                }
                            });
                        if let Some(tv) = tv {
                            frame.insert(id, tv);
                        }
                    }),
                }
            }
        };
        // a quiet poll cleans no frame state, so it must not clear the flag
        if reentered
            || framed
            || event.init
            || entry_fired
            || body_reads_triggered(&self.body, ctx, event)
        {
            self.resumes_mid_recursion = reentered;
        }
        // A framed run's tag: stale unless something at the entry
        // triggered; fired if any tail-select scrutinee on the executed
        // path fired; otherwise the body's own tag.
        let res = if (reentered || framed) && !res.is_bottom() {
            let entry = entry_fired || body_reads_triggered(&self.body, ctx, event);
            if !entry {
                TagValue::stale(res.value())
            } else if ctx.tail_scrut_fired {
                TagValue::fired(res.value())
            } else {
                res
            }
        } else {
            res
        };
        ctx.tail_scrut_fired = prev_tsf;
        res
    }

    pub fn instance_id(&self) -> LambdaInstanceId {
        self.instance_id
    }

    /// The compiled body.
    pub fn body(&self) -> &Node<R, E> {
        &self.body
    }

    /// The compiled body, for fusion to splice kernels into.
    pub fn body_mut(&mut self) -> &mut Node<R, E> {
        &mut self.body
    }

    pub(crate) fn inline_callback_body(&self) -> Option<&Node<R, E>> {
        match self.body.view() {
            NodeView::MapQ(map) => map.callback_body(),
            NodeView::FoldQ(fold) => fold.callback_body(),
            _ => None,
        }
    }

    /// Argument-binding patterns, parallel to `self.typ().args`.
    pub fn args(&self) -> &[StructPatternNode] {
        &self.args
    }

    /// This instance's resolved `FnType` (same as `Apply::typ()`).
    pub fn typ(&self) -> &Arc<FnType> {
        &self.typ
    }

    /// The tail-loop gate (see the `tail_loop` field).
    pub fn tail_loop(&self) -> bool {
        self.tail_loop.load(Ordering::Relaxed)
    }

    /// Set the tail-loop gate; `&self` so analysis can mark through a shared `&Node`.
    pub fn set_tail_loop(&self, v: bool) {
        self.tail_loop.store(v, Ordering::Relaxed)
    }

    pub fn self_recursive(&self) -> bool {
        self.self_recursive.load(Ordering::Relaxed)
    }

    pub fn set_self_recursive(&self, recursive: bool) {
        self.self_recursive.store(recursive, Ordering::Relaxed)
    }

    pub fn self_bind(&self) -> Option<BindId> {
        *self.self_bind.lock()
    }

    pub fn set_self_bind(&self, bind: Option<BindId>) {
        *self.self_bind.lock() = bind;
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for GXLambda<R, E> {
    fn view(&self) -> ApplyView<'_, R, E> {
        ApplyView::Lambda(self)
    }

    fn view_mut(&mut self) -> ApplyViewMut<'_, R, E> {
        ApplyViewMut::Lambda(self)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        let woke = self.slept.take() && ctx.frame_depth == 0;
        let mut entry_fired = event.init;
        let first = mem::replace(&mut self.first_dispatch, false);
        for (arg, pat) in from.iter_mut().zip(&self.args) {
            let tv = arg.update(ctx, event);
            let tag = tv.tag();
            entry_fired |= tag.triggers();
            // Seed the formals' value channel from a quiet arg production
            // on the first dispatch, after a wake, and on every framed
            // dispatch (a frame's seed dies with the pass; frames never
            // write the store).
            if (first || ctx.frame_depth > 0 || woke)
                && !tag.triggers()
                && !tag.is_bottom()
            {
                let v = tv.value_cloned();
                let store = ctx.frame_depth == 0;
                pat.bind(&v, &mut |id, v| {
                    if store {
                        // store only: an overlay entry would shadow the
                        // store's init-view upgrade
                        ctx.rt.store_insert_standing(id, TagValue::stale(v.clone()));
                    } else {
                        event.variables.insert(id, TagValue::stale(v.clone()));
                    }
                });
            }
            // Publish triggering deliveries only. A fresh bottom persists
            // in the store so a later quiet read sees the standing bottom,
            // not the pre-bottom value. Frames never write the store.
            if tag.triggers() {
                if tag.is_bottom() {
                    let store = ctx.frame_depth == 0;
                    pat.ids(&mut |id| {
                        if store {
                            ctx.rt.store_insert(
                                id,
                                TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM),
                            );
                        }
                        event
                            .variables
                            .insert(id, TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM));
                    });
                } else {
                    let v = tv.value_cloned();
                    let store = ctx.frame_depth == 0;
                    pat.bind(&v, &mut |id, v| {
                        if store {
                            ctx.rt.store_insert(id, TagValue::fired(v.clone()));
                        }
                        event.variables.insert(id, TagValue::tagged(v.clone(), tag));
                    })
                }
            }
        }
        // an interrupted dispatch is not a bottom: it rides its last result
        if ctx.control.interrupted() {
            return self.resident.ride();
        }
        // A quiet poll of a previously looped tail body rides the resident:
        // an unframed pass would re-read the entry formals and derive the
        // pre-loop value. Sound because a tail loop is sync.
        if self.tail_loop.load(Ordering::Relaxed)
            && self.resumes_mid_recursion
            && !entry_fired
            && !body_reads_triggered(&self.body, ctx, event)
        {
            return self.resident.ride();
        }
        *ctx.active_lambdas.entry(self.id).or_insert(0) += 1;
        let res = if !self.tail_loop.load(Ordering::Relaxed) {
            crate::stack::ensure_sufficient(|| self.body.update(ctx, event).clone())
        } else {
            self.run_tail_loop(ctx, event, entry_fired)
        };
        match ctx.active_lambdas.entry(self.id) {
            MapEntry::Occupied(mut e) => {
                let n = e.get_mut();
                *n -= 1;
                if *n == 0 {
                    e.remove();
                }
            }
            MapEntry::Vacant(_) => unreachable!("active_lambdas underflow"),
        }
        self.resident.set(res)
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        args: &mut [Node<R, E>],
    ) -> Result<()> {
        let _profile = profile::phase(Phase::InstanceCheck);
        for (arg, FnArgType { typ, .. }) in args.iter_mut().zip(self.typ.args.iter()) {
            wrap!(arg, arg.typecheck0(ctx))?;
            wrap!(arg, typ.check_contains_rigid(&ctx.env, &arg.typ()))?;
        }
        let env = self.env.clone();
        ctx.with_restored(env, |ctx| {
            wrap!(self.body, self.body.typecheck0(ctx))?;
            wrap!(
                self.body,
                self.typ.rtype.check_contains_rigid(&ctx.env, &self.body.typ())
            )
        })?;
        Ok(())
    }

    /// Drives the body's `typecheck1`; the driving `CallSite::typecheck1`
    /// already walked the args.
    fn typecheck1(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _resolved: &FnType,
    ) -> Result<()> {
        let env = self.env.clone();
        ctx.with_restored(env, |ctx| wrap!(self.body, self.body.typecheck1(ctx)))
    }

    fn emit_clif(
        &self,
        callsite: &CallSite<R, E>,
        cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        let res = match self.body.view() {
            NodeView::MapQ(map) => map.emit_clif_call(callsite, cx)?,
            NodeView::FoldQ(fold) => fold.emit_clif_call(callsite, cx)?,
            _ => None,
        };
        // the loop emits the lambda's own return shape; a callsite widened
        // to a union must hand its consumers a Value pair
        match res {
            Some(cv)
                if crate::fusion::emit::call_result_needs_value_widening(
                    callsite.typ(),
                    &self.typ.rtype,
                ) =>
            {
                Ok(Some(crate::fusion::emit::widen_result_to_value(
                    cx,
                    &self.typ.rtype,
                    cv,
                )?))
            }
            res => Ok(res),
        }
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        if crate::dbgenv::gxdbg_instance_fusion() {
            let before = ctx.fusion.stats.failed.len();
            let fused_before = ctx.fusion.stats.fused;
            let r = crate::fusion::fuse(&mut self.body, ctx);
            eprintln!(
                "INSTANCE-FUSION GXLambda::fuse id={:?} fused_delta={} new_failures:",
                self.id,
                ctx.fusion.stats.fused - fused_before
            );
            for failure in &ctx.fusion.stats.failed[before..] {
                eprintln!("  INSTANCE-FUSION-FAIL {:?}: {}", failure.id, failure.reason);
            }
            return r;
        }
        crate::fusion::fuse(&mut self.body, ctx)
    }

    fn typ(&self) -> Arc<FnType> {
        Arc::clone(&self.typ)
    }

    fn refs(&self, refs: &mut Refs) {
        for pat in &self.args {
            pat.ids(&mut |id| {
                refs.bound.insert(id);
            })
        }
        self.body.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.body.delete(ctx);
        for n in &self.args {
            n.delete(ctx)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.slept.set();
        // a callee body is outside the shrink scope: a recursion shrinking
        // one level does not shrink the external calls it made
        let saved = ctx.deselecting_arm;
        ctx.deselecting_arm = false;
        self.body.sleep(ctx);
        ctx.deselecting_arm = saved;
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.body.reset_replay(ctx);
    }
}

impl<R: Rt, E: UserEvent> GXLambda<R, E> {
    pub(super) fn new(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        id: LambdaId,
        typ: Arc<FnType>,
        argspec: Arc<[Arg]>,
        args: &[Node<R, E>],
        scope: &Scope,
        tid: ExprId,
        body: Expr,
    ) -> Result<Self> {
        let origin = body.ori.clone();
        Self::new_with_body(ctx, id, typ, argspec, args, scope, origin, |ctx, _| {
            compile(ctx, flags, body, scope, tid)
        })
    }

    pub(super) fn new_collection(
        ctx: &mut ExecCtx<R, E>,
        id: LambdaId,
        typ: Arc<FnType>,
        argspec: Arc<[Arg]>,
        args: &[Node<R, E>],
        scope: &Scope,
        tid: ExprId,
        spec: Expr,
        intrinsic: CollectionIntrinsic,
    ) -> Result<Self> {
        let origin = spec.ori.clone();
        Self::new_with_body(
            ctx,
            id,
            typ.clone(),
            argspec,
            args,
            scope,
            origin,
            |ctx, argpats| intrinsic.build(ctx, spec, scope, tid, &typ, argpats),
        )
    }

    fn new_with_body(
        ctx: &mut ExecCtx<R, E>,
        id: LambdaId,
        typ: Arc<FnType>,
        argspec: Arc<[Arg]>,
        args: &[Node<R, E>],
        scope: &Scope,
        origin: Arc<Origin>,
        build_body: impl FnOnce(
            &mut ExecCtx<R, E>,
            &[StructPatternNode],
        ) -> Result<Node<R, E>>,
    ) -> Result<Self> {
        if args.len() != argspec.len() {
            bail!("arity mismatch, expected {} arguments", argspec.len())
        }
        // a narrower `typ` would truncate the zip below and silently drop
        // parameters; bailing lets a Dynamic dispatch retry with `def_typ`
        if argspec.len() != typ.args.len() {
            bail!(
                "instance signature has {} parameters, the definition has {}",
                typ.args.len(),
                argspec.len()
            )
        }
        let mut argpats: LPooled<Vec<StructPatternNode>> = LPooled::take();
        for (a, atyp) in argspec.iter().zip(typ.args.iter()) {
            let pattern = StructPatternNode::compile(
                ctx,
                &atyp.typ,
                &a.pattern,
                scope,
                a.pos,
                origin.clone(),
            )?;
            if pattern.is_refutable() {
                bail!(
                    "refutable patterns are not allowed in lambda arguments {}",
                    a.pattern
                )
            }
            argpats.push(pattern);
        }
        let p = profile::phase(Phase::InstanceGraph);
        let body = build_body(ctx, &argpats)?;
        drop(p);
        Ok(Self {
            slept: WakeBit::default(),
            id,
            instance_id: LambdaInstanceId::new(),
            args: Box::from_iter(argpats.drain(..)),
            typ,
            body,
            tail_loop: AtomicBool::new(false),
            self_recursive: AtomicBool::new(false),
            self_bind: Mutex::new(None),
            resident: TagValue::phantom(),
            resumes_mid_recursion: false,
            first_dispatch: true,
            env: ctx.env.clone(),
        })
    }
}

#[derive(Debug)]
struct BuiltInLambda<R: Rt, E: UserEvent> {
    typ: Arc<FnType>,
    apply: Box<dyn Apply<R, E> + Send + Sync + 'static>,
}

impl<R: Rt, E: UserEvent> Apply<R, E> for BuiltInLambda<R, E> {
    /// Fusion sees the wrapped builtin's own view.
    fn view(&self) -> ApplyView<'_, R, E> {
        self.apply.view()
    }

    fn view_mut(&mut self) -> ApplyViewMut<'_, R, E> {
        self.apply.view_mut()
    }

    fn emit_clif(
        &self,
        callsite: &CallSite<R, E>,
        cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        // the trait default's `Ok(None)` would silently de-fuse every builtin
        self.apply.emit_clif(callsite, cx)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.apply.fuse(ctx)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        self.apply.update(ctx, from, event)
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        args: &mut [Node<R, E>],
    ) -> Result<()> {
        if args.len() < self.typ.args.len()
            || (args.len() > self.typ.args.len() && self.typ.vargs.is_none())
        {
            let vargs = if self.typ.vargs.is_some() { "at least " } else { "" };
            bail!(
                "expected {}{} arguments got {}",
                vargs,
                self.typ.args.len(),
                args.len()
            )
        }
        for i in 0..args.len() {
            wrap!(args[i], args[i].typecheck0(ctx))?;
            let atyp = if i < self.typ.args.len() {
                &self.typ.args[i].typ
            } else {
                self.typ.vargs.as_ref().unwrap()
            };
            wrap!(args[i], atyp.check_contains_rigid(&ctx.env, &args[i].typ()))?
        }
        self.apply.typecheck0(ctx, args)
    }

    fn typecheck1(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        args: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.apply.typecheck1(ctx, args, resolved)
    }

    fn typ(&self) -> Arc<FnType> {
        Arc::clone(&self.typ)
    }

    fn refs(&self, refs: &mut Refs) {
        self.apply.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.apply.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.apply.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        // a no-op here would leave the builtin's arg caches replaying across frames
        self.apply.reset_replay(ctx);
    }
}

#[derive(Debug)]
pub struct Lambda {
    spec: Expr,
    def: Value,
    typ: Type,
    resident: TagValue,
}

impl Lambda {
    /// The definition's `LambdaId`.
    pub fn lambda_id<R: Rt, E: UserEvent>(&self) -> Option<LambdaId> {
        self.def.downcast_ref::<LambdaDef<R, E>>().map(|d| d.id)
    }

    /// Borrow the underlying `LambdaDef`.
    pub fn def<R: Rt, E: UserEvent>(&self) -> Option<&LambdaDef<R, E>> {
        self.def.downcast_ref::<LambdaDef<R, E>>()
    }

    /// The wrapped `LambdaDef` `Value`, which this node emits at init.
    pub fn def_value(&self) -> &Value {
        &self.def
    }

    /// The literal's source identity (`LambdaDef::source`).
    pub fn source_id(&self) -> ExprId {
        self.spec.id
    }
}

impl Lambda {
    pub(crate) fn compile<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        l: &expr::LambdaExpr,
        _top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let mut s: LPooled<Vec<&ArcStr>> = LPooled::take();
        for a in l.args.iter() {
            a.pattern.with_names(&mut |n| s.push(n));
        }
        let len = s.len();
        s.sort();
        s.dedup();
        if len != s.len() {
            bail!("arguments must have unique names");
        }
        let id = LambdaId::new();
        let vargs = match l.vargs.as_ref() {
            None => None,
            Some(None) => Some(None),
            Some(Some(typ)) => Some(Some(typ.scope_refs(&scope.lexical))),
        };
        let rtype = match l.rtype.as_ref() {
            None => None,
            Some(t) => Some(t.scope_refs(&scope.lexical).rewrite_trait_args(&ctx.env)?),
        };
        let throws = match l.throws.as_ref() {
            None => None,
            Some(t) => Some(t.scope_refs(&scope.lexical).rewrite_trait_args(&ctx.env)?),
        };
        // a trait as a parameter's type is a fresh bounded quantifier
        // (`|s: Read|` ≡ `'s: Read |s: 's|`), joined to the declared ones
        // so the def gate holds it rigid
        let mut trait_quantifiers: LPooled<Vec<(TVar, Type)>> = LPooled::take();
        let mut argspec: LPooled<Vec<Arg>> = LPooled::take();
        for (i, a) in l.args.iter().enumerate() {
            let constraint = match &a.constraint {
                None => None,
                Some(typ) => {
                    let typ = typ.scope_refs(&scope.lexical);
                    match &typ {
                        Type::Ref(tr) if ctx.env.trait_of_ref(tr).is_some() => {
                            let name: ArcStr = match a.pattern.single_bind() {
                                Some(n) => format_compact!("#{n}").as_str().into(),
                                None => format_compact!("#arg{i}").as_str().into(),
                            };
                            let tv = TVar::empty_named(name);
                            trait_quantifiers.push((tv.clone(), typ.clone()));
                            Some(Type::trait_param(&ctx.env, tv, tr))
                        }
                        _ => Some(typ.rewrite_trait_args(&ctx.env)?),
                    }
                }
            };
            argspec.push(Arg {
                labeled: a.labeled.clone(),
                pattern: a.pattern.clone(),
                constraint,
                pos: a.pos,
            });
        }
        let argspec = Arc::from_iter(argspec.drain(..));
        let mut constraints = l
            .constraints
            .iter()
            .map(|(tv, tc)| {
                let tv = tv.scope_refs(&scope.lexical);
                let tc = tc.scope_refs(&scope.lexical);
                Ok((tv, tc))
            })
            .collect::<Result<LPooled<Vec<_>>>>()?;
        constraints.extend(trait_quantifiers.drain(..));
        let original_scope = scope.clone();
        let scope = scope.append_block("fn", id.0);
        let def_scope = scope.clone();
        let env = ctx.env.clone();
        let def_env = ctx.env.clone();
        if let Either::Right(builtin) = &l.body {
            if CollectionIntrinsic::from_name(builtin).is_none()
                && ctx.builtins.get(builtin.as_str()).is_none()
            {
                bail!("unknown builtin function {builtin}")
            }
            if !ctx.builtins_allowed {
                bail!("defining builtins is not allowed in this context")
            }
            for a in argspec.iter() {
                if a.constraint.is_none() {
                    bail!(
                        "builtin function {builtin} requires all arguments to have type annotations"
                    )
                }
            }
            if rtype.is_none() {
                bail!("builtin function {builtin} requires a return type annotation")
            }
        }
        let typ = {
            let args = Arc::from_iter(argspec.iter().map(|a| {
                let kind = match (a.labeled.as_ref(), a.pattern.single_bind()) {
                    (Some(default), Some(name)) => FnArgKind::Labeled {
                        name: name.clone(),
                        has_default: default.is_some(),
                    },
                    (Some(_), None) => FnArgKind::Positional { name: None },
                    (None, name) => FnArgKind::Positional { name: name.cloned() },
                };
                let typ = match a.constraint.as_ref() {
                    Some(t) => t.clone(),
                    None => Type::empty_tvar(),
                };
                FnArgType { kind, typ }
            }));
            let vargs = match vargs {
                Some(Some(t)) => Some(t.clone()),
                Some(None) => Some(Type::empty_tvar()),
                None => None,
            };
            let rtype = rtype.clone().unwrap_or_else(|| Type::empty_tvar());
            let explicit_throws = throws.is_some();
            let throws = throws.clone().unwrap_or_else(|| Type::empty_tvar());
            Arc::new(FnType {
                args,
                vargs,
                rtype,
                throws,
                explicit_throws,
                quantifiers: crate::expr::parser::quantifier_names(
                    constraints.iter().map(|(tv, _)| tv),
                ),
                lambda_ids: LambdaIds::default(),
            })
        };
        // alias same-named leaves onto the declared quantifier tvars first
        // so each constraint lands in the one cell every occurrence shares
        {
            let mut known: LPooled<ahash::AHashMap<ArcStr, TVar>> = LPooled::take();
            for (tv, _) in constraints.iter() {
                known.insert(tv.name.clone(), tv.clone());
            }
            typ.alias_tvars(&mut known);
            for (tv, tc) in constraints.iter() {
                tc.alias_tvars(&mut known);
                tv.add_cell_constraint(tc.clone());
            }
        }
        typ.lambda_ids.set_id(id);
        let def_typ = typ.clone();
        let def_argspec = argspec.clone();
        let def_spec = spec.clone();
        let body = l.body.clone();
        let init: InitFn<R, E> = SArc::new(move |scope, ctx, args, mode, tid| {
            ctx.with_restored(def_env.clone(), |ctx| match body.clone() {
                Either::Left(body) => {
                    let scope = Scope {
                        dynamic: scope.dynamic.clone(),
                        lexical: def_scope.lexical.clone(),
                    };
                    // a dynamic bind retries with the definition signature:
                    // the runtime callee can differ from the site's prior view
                    let build = |ctx: &mut ExecCtx<R, E>, typ: Arc<FnType>| {
                        GXLambda::new(
                            ctx,
                            flags,
                            id,
                            typ,
                            def_argspec.clone(),
                            args,
                            &scope,
                            tid,
                            body.clone(),
                        )
                    };
                    match mode {
                        BindMode::Static { instance, .. } => {
                            build(ctx, Arc::new(instance.clone()))
                        }
                        BindMode::Dynamic(r) => build(ctx, Arc::new(r.clone()))
                            .or_else(|_| build(ctx, def_typ.clone())),
                        BindMode::Definition => build(ctx, def_typ.clone()),
                    }
                    .map(|a| -> Box<dyn Apply<R, E>> { Box::new(a) })
                }
                Either::Right(builtin) => {
                    if let Some(intrinsic) = CollectionIntrinsic::from_name(&builtin) {
                        let scope = Scope {
                            dynamic: scope.dynamic.clone(),
                            lexical: def_scope.lexical.clone(),
                        };
                        let build = |ctx: &mut ExecCtx<R, E>, typ: Arc<FnType>| {
                            GXLambda::new_collection(
                                ctx,
                                id,
                                typ,
                                def_argspec.clone(),
                                args,
                                &scope,
                                tid,
                                def_spec.clone(),
                                intrinsic,
                            )
                        };
                        let result = match mode {
                            BindMode::Static { instance, .. } => {
                                build(ctx, Arc::new(instance.clone()))
                            }
                            BindMode::Dynamic(r) => build(ctx, Arc::new(r.clone()))
                                .or_else(|_| build(ctx, def_typ.clone())),
                            BindMode::Definition => build(ctx, def_typ.clone()),
                        };
                        result.map(|a| -> Box<dyn Apply<R, E>> { Box::new(a) })
                    } else {
                        match ctx.builtins.get(&*builtin) {
                            None => bail!("unknown builtin function {builtin}"),
                            Some(init) => {
                                let typ = match mode.resolved() {
                                    Some(r) => Arc::new(r.clone()),
                                    None => def_typ.clone(),
                                };
                                let resolved = mode.resolved();
                                init(ctx, &def_typ, resolved, &def_scope, args, tid).map(
                                    |apply| {
                                        let f: Box<dyn Apply<R, E>> =
                                            Box::new(BuiltInLambda { typ, apply });
                                        f
                                    },
                                )
                            }
                        }
                    }
                }
            })
        });
        // No signature ref seeding here: the module tree is mid-registration
        // and a name's final target may not be registered yet. Cells fill
        // at typecheck.
        let def = ctx.lambdawrap.wrap(LambdaDef {
            id,
            src: ArcStr::from(spec.to_string()),
            typ: typ.clone(),
            env,
            argspec,
            init,
            scope: original_scope,
            check: Mutex::new(None),
            intrinsic_effect: Mutex::new(match &l.body {
                Either::Right(name) if CollectionIntrinsic::from_name(name).is_some() => {
                    EffectKind::Sync
                }
                Either::Right(name) => ctx.builtin_effect(name),
                Either::Left(_) => EffectKind::Sync,
            }),
            stateless: AtomicBool::new(match &l.body {
                Either::Right(name) if CollectionIntrinsic::from_name(name).is_some() => {
                    true
                }
                Either::Right(name) => ctx.builtin_stateless(name),
                Either::Left(_) => true,
            }),
            recursion: Mutex::new(RecursionKind::NotRecursive),
            source: spec.id,
        });
        ctx.lambda_defs.insert(id, def.clone());
        Ok(Node::new(Self {
            spec,
            def: def.clone(),
            typ: Type::Fn(typ),
            // a lambda literal is a constant: present from birth (see Constant)
            resident: TagValue::stale(def),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Lambda {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // same production rule as `Constant`: FIRED at init, STALE inside
        // frames, which force init
        if ctx.frame_depth > 0 {
            if ctx.dispatch_init {
                self.resident.set(TagValue::fired(self.def.clone()))
            } else {
                self.resident.set(TagValue::stale(self.def.clone()))
            }
        } else if event.init {
            self.resident.set(TagValue::fired(self.def.clone()))
        } else {
            self.resident.ride()
        }
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn refs(&self, _refs: &mut Refs) {}

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        // a retained def keeps its `LambdaIds` link-graph nodes alive, and
        // `typecheck1`'s `ids()` walks grow with them
        if let Some(def) = self.def.downcast_ref::<LambdaDef<R, E>>() {
            ctx.lambda_defs.remove(&def.id);
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        let def = self
            .def
            .downcast_ref::<LambdaDef<R, E>>()
            .ok_or_else(|| anyhow!("failed to unwrap lambda"))?;
        // Every arg, defaulted labeled ones included, checks as a Nop of
        // its declared type: a default is compiled and checked per
        // omitting call site (`setup_bind`), where it may narrow that
        // site's cells.
        let mut faux_args: LPooled<Vec<Node<R, E>>> = def
            .typ
            .args
            .iter()
            .map(|at| {
                let n: Node<R, E> = Node::new(Nop { typ: at.typ.clone() });
                Ok(n)
            })
            .collect::<Result<_>>()?;
        let faux_id = BindId::new();
        ctx.env.by_id.insert_cow(
            faux_id,
            Bind {
                doc: None,
                export: false,
                id: faux_id,
                name: "faux".into(),
                scope: def.scope.lexical.clone(),
                typ: Type::empty_tvar(),
                pos: SourcePosition::default(),
                ori: Arc::new(Origin::default()),
                pattern: false,
                facet: None,
            },
        );
        let gate_scope = def.scope.with_catch((faux_id, ExprId::new()));
        // Declared (named) signature tvars are rigid for the duration of
        // the def gate: the body must be well-typed for arbitrary 'a.
        // Anonymous '_N inference cells stay bindable.
        let mut named_tvs: LPooled<ahash::AHashMap<ArcStr, TVar>> = LPooled::take();
        def.typ.collect_tvars(&mut named_tvs);
        named_tvs.retain(|name, _| !name.starts_with('_'));
        for tv in named_tvs.values() {
            tv.set_rigid();
        }
        // a self-call site knots to the def's own cells (`ExecCtx::rec_defs`)
        ctx.rec_defs.insert(def.id);
        ctx.def_gate_depth += 1;
        let res = (def.init)(
            &gate_scope,
            ctx,
            &mut faux_args,
            BindMode::Definition,
            ExprId::new(),
        )
        .with_context(|| ErrorContext(Update::<R, E>::spec(self).clone()));
        let res = res.and_then(|mut f| {
            let ftyp = f.typ().clone();
            // fn-typed params knot like self-calls: a call to `f` unifies
            // against the param's own declared cells (`ExecCtx::def_gate_params`)
            let mut param_knot: LPooled<Vec<BindId>> = LPooled::take();
            if let ApplyView::Lambda(g) = f.view() {
                for (pat, at) in g.args().iter().zip(ftyp.args.iter()) {
                    if at.typ.with_deref(|t| matches!(t, Some(Type::Fn(_))))
                        && let Some(id) = pat.single_bind_id()
                    {
                        ctx.def_gate_params.insert(id);
                        param_knot.push(id);
                    }
                }
            }
            let res = f
                .typecheck0(ctx, &mut faux_args)
                .with_context(|| ErrorContext(Update::<R, E>::spec(self).clone()));
            for id in param_knot.drain(..) {
                ctx.def_gate_params.remove(&id);
            }
            // a builtin's check `Apply` is retained for `CallSite::typecheck1`;
            // a user body is not re-checked per call site
            if matches!(f.view(), ApplyView::Lambda(_)) {
                f.delete(ctx)
            } else {
                let def = self
                    .def
                    .downcast_ref::<LambdaDef<R, E>>()
                    .expect("failed to unwrap lambda");
                *def.check.lock() = Some(f);
            }
            res?;
            let inferred_throws = ctx.env.by_id[&faux_id]
                .typ
                .deref_cloned()
                .unwrap_or(Type::Bottom)
                .scope_refs(&def.scope.lexical)
                .normalize();
            ftyp.throws
                .check_contains(&ctx.env, &inferred_throws)
                .with_context(|| ErrorContext(Update::<R, E>::spec(self).clone()))?;
            // record the gate's inferred facts as cell conjuncts; a nested
            // gate records closed facts only (`FnType::constrain_known`)
            ftyp.constrain_known(ctx.def_gate_depth > 1);
            Ok(())
        });
        ctx.def_gate_depth -= 1;
        ctx.rec_defs.remove(&def.id);
        ctx.env.by_id.remove_cow(&faux_id);
        for tv in named_tvs.values() {
            tv.clear_rigid();
        }
        // closed inferred bindings survive the gate: a solved fact must not
        // degrade to an upper bound a consumer can narrow first
        self.typ.unbind_open_tvars();
        // GRAPHIX_RIGID_AUDIT=1 is a cataloging tool: a rejected def that
        // continues may compile to a different shape, so never trust its
        // value output
        if res.is_err() && crate::dbgenv::graphix_rigid_audit() {
            if let Err(e) = &res {
                eprintln!("RIGID-AUDIT reject: {} — {e:#}", Update::<R, E>::spec(self));
            }
            return Ok(());
        }
        res
    }

    /// A definition has no children here; the body is checked per call
    /// site through `GXLambda::typecheck1`.
    fn typecheck1(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Lambda(self)
    }
}
