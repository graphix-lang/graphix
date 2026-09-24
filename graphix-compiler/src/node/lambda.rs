use super::{
    Nop, VarRead, WakeBit,
    callsite::{CallSite, Feeds, QuietAtRoot, publish_production},
    collection::CollectionIntrinsic,
    compiler::compile,
    pattern::StructPatternNode,
    produce_constant, read_quiet, read_var,
};
use crate::{
    Apply, ApplyView, BindId, BindMode, CFlag, Event, ExecCtx, InitFn, LambdaId,
    LambdaInstanceId, Node, NodeView, Refs, Rt, Scope, TagValue, Update, UserEvent,
    dbgenv,
    effects::{EffectKind, RecursionKind},
    env::{Bind, Env},
    expr::{self, Arg, At, Expr, ExprId, Origin},
    fusion::{
        self,
        emit::{
            BodyCx, CompiledExpr, call_result_needs_value_widening, widen_result_to_value,
        },
    },
    image::{
        self, ImageBuf,
        env::{lexical_decode, lexical_encode, lexical_len},
        nodes::{NodeTag, decode_node, put_tag, tag_len},
    },
    profile::{self, Phase},
    typ::{FnArgKind, FnArgType, FnType, TVar, Type, fntyp::LambdaIds, tvar::RigidGate},
    wrap,
};
use ahash::AHashMap;
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use combine::stream::position::SourcePosition;
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_core::{
    pack::{Pack, PackError},
    utils::Either,
};
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
    pub env: Env,
    pub scope: Scope,
    pub argspec: Arc<[Arg]>,
    pub typ: Arc<FnType>,
    pub init: InitFn<R, E>,
    // XCR claude for eric: the body kind is decided once (`DefBody`); the check
    // stays a def field: `DefBody` is Clone data make_init and the image carry,
    // a Mutex'd Apply is neither, and `builtin_check` answers it for a builtin only.
    /// A builtin definition's check `Apply`, built by the definition gate
    /// ([`Self::builtin_check`]).
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
    pub origin: DefOrigin,
}

/// Where a definition came from: a lambda expression, whose `init` is
/// a function of these and which an image carries as data, or Rust
/// code building an `Apply` at runtime, which no image can carry.
pub enum DefOrigin {
    Source { body: DefBody, flags: BitFlags<CFlag>, spec: Expr },
    Runtime,
}

/// What a source definition's instances run.
#[derive(Debug, Clone)]
pub enum DefBody {
    Expr(Expr),
    /// A traversal the compiler builds (`'array_map`, ..).
    Collection(CollectionIntrinsic),
    /// A Rust builtin, by its registered name.
    BuiltIn(ArcStr),
}

impl DefBody {
    pub(crate) fn of(body: &Either<Expr, ArcStr>) -> Self {
        match body {
            Either::Left(e) => DefBody::Expr(e.clone()),
            Either::Right(name) => match CollectionIntrinsic::from_name(name) {
                Some(intrinsic) => DefBody::Collection(intrinsic),
                None => DefBody::BuiltIn(name.clone()),
            },
        }
    }
}

impl<R: Rt, E: UserEvent> LambdaDef<R, E> {
    /// The check `Apply` of a builtin definition; `None` for any other.
    /// It holds `None` until the definition gate builds it, and for a
    /// definition restored from an image until its first call site
    /// rebuilds it.
    pub(crate) fn builtin_check(&self) -> Option<&Mutex<Option<Box<dyn Apply<R, E>>>>> {
        match &self.origin {
            DefOrigin::Source { body: DefBody::BuiltIn(_), .. } => Some(&self.check),
            DefOrigin::Source { .. } | DefOrigin::Runtime => None,
        }
    }
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

    fn encode(&self, _buf: &mut impl bytes::BufMut) -> Result<(), PackError> {
        Err(PackError::Application(0))
    }

    fn decode(_buf: &mut impl bytes::Buf) -> Result<Self, PackError> {
        Err(PackError::Application(0))
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
    /// A tail loop ended mid-recursion: the next framed pass resumes it.
    /// Cycle state, false before any cycle (and so in an image).
    resumes_mid_recursion: bool,
    /// `true` until the first dispatch, which seeds the fresh formal
    /// ids' value channel from the args' quiet productions; true in an
    /// image, which is written before any cycle.
    first_dispatch: bool,
    /// The def-side lexical env the body was compiled under. The body
    /// typechecks under it too: the caller's env, which drives the
    /// checks, may lack the defining module's private typedefs.
    env: Env,
}

// XCR claude for eric: a dispatch now walks the body's refs at most once
// before the body runs and once after (it walked up to four times). Caching
// on the instance is not done: the set changes when any nested dynamic site
// binds, which the instance cannot observe without a ctx-wide bind counter.
/// Did the body read a variable delivered with a triggering tag?
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
            read_var(ctx, event, &id),
            Some(VarRead::Delivered(tv)) if tv.tag().triggers()
        );
    });
    hit
}

impl<R: Rt, E: UserEvent> GXLambda<R, E> {
    /// The definition's id, shared by every instance of it.
    pub fn id(&self) -> LambdaId {
        self.id
    }

    /// `reads` answers [`body_reads_triggered`] before the body runs.
    fn run_tail_loop(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
        entry_fired: bool,
        reads: &mut impl FnMut(&Node<R, E>, &ExecCtx<R, E>, &Event<E>) -> bool,
    ) -> TagValue {
        let mut frame: LPooled<IntMap<BindId, TagValue>> = LPooled::take();
        let mut reentered = false;
        let framed = self.resumes_mid_recursion
            && !event.init
            && (entry_fired || reads(&self.body, ctx, event));
        if framed {
            self.body.reset_replay(ctx);
            // a delivered formal keeps its cycle tag, a standing one
            // reads quiet
            for pat in self.args.iter() {
                pat.ids(&mut |id| {
                    if let Some(tv) = read_quiet(ctx, event, &id) {
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
            if dbgenv::gxdbg_tail() {
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
                    Some(tv) if tv.tag().is_bottom() => {
                        let tag = tv.tag();
                        pat.ids(&mut |id| {
                            frame.insert(id, TagValue::tagged(Value::Null, tag));
                        })
                    }
                    Some(tv) => {
                        let (v, tag) = tv.clone().into_parts();
                        pat.bind(&v, &mut |id, v| {
                            frame.insert(id, TagValue::tagged(v, tag));
                        })
                    }
                    None => pat.ids(&mut |id| {
                        let tv = prev
                            .get(&id)
                            .cloned()
                            .or_else(|| read_quiet(ctx, event, &id));
                        if let Some(tv) = tv {
                            frame.insert(id, tv);
                        }
                    }),
                }
            }
        };
        let mut after: Option<bool> = None;
        let mut reads_after =
            |body: &Node<R, E>, ctx: &ExecCtx<R, E>, event: &Event<E>| {
                *after.get_or_insert_with(|| body_reads_triggered(body, ctx, event))
            };
        // a quiet poll cleans no frame state, so it must not clear the flag
        if reentered
            || framed
            || event.init
            || entry_fired
            || reads_after(&self.body, ctx, event)
        {
            self.resumes_mid_recursion = reentered;
        }
        // A framed run's tag: stale unless something at the entry
        // triggered; fired if any tail-select scrutinee on the executed
        // path fired; otherwise the body's own tag.
        let res = if (reentered || framed) && !res.is_bottom() {
            if !(entry_fired || reads_after(&self.body, ctx, event)) {
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
    fn image_len(&self) -> usize {
        self.id.encoded_len()
            + self.instance_id.encoded_len()
            + image::slice_len(&self.args)
            + self.typ.encoded_len()
            + self.body.image_len()
            + 2
            + self.self_bind.lock().encoded_len()
            + lexical_len(&self.env)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.resumes_mid_recursion {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.id.encode(buf)?;
        self.instance_id.encode(buf)?;
        image::slice_encode(&self.args, buf)?;
        self.typ.encode(buf)?;
        self.body.image_encode(buf)?;
        self.tail_loop.load(Ordering::Relaxed).encode(buf)?;
        self.self_recursive.load(Ordering::Relaxed).encode(buf)?;
        self.self_bind.lock().encode(buf)?;
        lexical_encode(&self.env, buf)
    }

    fn view(&self) -> ApplyView<'_, R, E> {
        ApplyView::Lambda(self)
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
        // the formals' value channel seeds from a quiet arg production on
        // the first dispatch and after a wake; in a frame it always does
        // (the seed dies with the pass: frames never write the store)
        let root = if first || woke { QuietAtRoot::Stand } else { QuietAtRoot::Skip };
        for (arg, pat) in from.iter_mut().zip(&self.args) {
            let tv = arg.update(ctx, event);
            entry_fired |= tv.tag().triggers();
            publish_production(ctx, event, Feeds::Pattern(pat), tv, false, root);
        }
        // an interrupted dispatch is not a bottom: it rides its last result
        if ctx.control.interrupted() {
            return self.resident.ride();
        }
        let mut before: Option<bool> = None;
        let mut reads = |body: &Node<R, E>, ctx: &ExecCtx<R, E>, event: &Event<E>| {
            *before.get_or_insert_with(|| body_reads_triggered(body, ctx, event))
        };
        let tail_loop = self.tail_loop.load(Ordering::Relaxed);
        // A quiet poll of a previously looped tail body rides the resident:
        // an unframed pass would re-read the entry formals and derive the
        // pre-loop value. Sound because a tail loop is sync.
        if tail_loop
            && self.resumes_mid_recursion
            && !entry_fired
            && !reads(&self.body, ctx, event)
        {
            return self.resident.ride();
        }
        let res = if tail_loop {
            self.run_tail_loop(ctx, event, entry_fired, &mut reads)
        } else {
            self.body.update(ctx, event).clone()
        };
        self.resident.set(res)
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        args: &mut [Node<R, E>],
    ) -> Result<()> {
        let mut p = profile::phase(Phase::InstanceCheck);
        profile::instance(&mut p, self.instance_id, self.id, self.body.spec());
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
        profile::instance_signature(self.instance_id, &self.typ, None);
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
                if call_result_needs_value_widening(callsite.typ(), &self.typ.rtype) =>
            {
                Ok(Some(widen_result_to_value(cx, &self.typ.rtype, cv)?))
            }
            res => Ok(res),
        }
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        if dbgenv::gxdbg_instance_fusion() {
            let before = ctx.fusion.stats.failed.len();
            let fused_before = ctx.fusion.stats.fused;
            let r = fusion::fuse(&mut self.body, ctx);
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
        fusion::fuse(&mut self.body, ctx)
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
            n.ids(&mut |id| {
                ctx.fn_forward_resolutions.remove(&id);
            });
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
        let mut p = profile::phase(Phase::InstanceGraph);
        let body = build_body(ctx, &argpats)?;
        let instance_id = LambdaInstanceId::new();
        profile::instance(&mut p, instance_id, id, body.spec());
        drop(p);
        Ok(Self {
            slept: WakeBit::default(),
            id,
            instance_id,
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

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let id = LambdaId::decode(buf)?;
        let instance_id = LambdaInstanceId::decode(buf)?;
        let args = Vec::<StructPatternNode>::decode(buf)?.into_boxed_slice();
        let typ: Arc<FnType> = Pack::decode(buf)?;
        let body = decode_node(ctx, buf)?;
        let tail_loop = bool::decode(buf)?;
        let self_recursive = bool::decode(buf)?;
        let self_bind = Option::<BindId>::decode(buf)?;
        let env = lexical_decode(buf)?;
        Ok(Self {
            slept: WakeBit::default(),
            id,
            instance_id,
            args,
            body,
            typ,
            tail_loop: AtomicBool::new(tail_loop),
            self_recursive: AtomicBool::new(self_recursive),
            self_bind: Mutex::new(self_bind),
            resident: TagValue::phantom(),
            resumes_mid_recursion: false,
            first_dispatch: true,
            env,
        })
    }
}

#[derive(Debug)]
pub(crate) struct BuiltInLambda<R: Rt, E: UserEvent> {
    typ: Arc<FnType>,
    name: ArcStr,
    apply: Box<dyn Apply<R, E> + Send + Sync + 'static>,
}

impl<R: Rt, E: UserEvent> BuiltInLambda<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let typ = Arc::new(FnType::decode(buf)?);
        let name = ArcStr::decode(buf)?;
        let decode = ctx.builtin_decoder(&name).ok_or_else(|| {
            log::warn!("the image names an unregistered builtin {name}");
            PackError::InvalidFormat
        })?;
        let apply = decode(ctx, from, buf)?;
        Ok(Self { typ, name, apply })
    }
}

/// Stands in for a builtin this binary does not have, under an IDE
/// check: a package under development declares builtins only its own
/// build registers. Like any builtin it is typed by its declared
/// signature alone; it never produces, and the check never runs it.
#[derive(Debug)]
struct UnknownBuiltIn(TagValue);

impl UnknownBuiltIn {
    fn init<R: Rt, E: UserEvent>(
        _: &mut ExecCtx<R, E>,
        _: &FnType,
        _: Option<&FnType>,
        _: &Scope,
        _: &[Node<R, E>],
        _: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self(TagValue::phantom())))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for UnknownBuiltIn {
    fn update(
        &mut self,
        _: &mut ExecCtx<R, E>,
        _: &mut [Node<R, E>],
        _: &mut Event<E>,
    ) -> &TagValue {
        &self.0
    }

    fn image_len(&self) -> usize {
        0
    }

    fn image_encode(&self, _: &mut ImageBuf) -> Result<(), PackError> {
        Err(PackError::Application(crate::image::NOT_IMAGED))
    }

    fn sleep(&mut self, _: &mut ExecCtx<R, E>) {}

    fn reset_replay(&mut self, _: &mut ExecCtx<R, E>) {}
}

impl<R: Rt, E: UserEvent> Apply<R, E> for BuiltInLambda<R, E> {
    fn image_len(&self) -> usize {
        self.typ.encoded_len() + self.name.encoded_len() + self.apply.image_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.typ.encode(buf)?;
        self.name.encode(buf)?;
        self.apply.image_encode(buf)
    }

    /// Fusion sees the wrapped builtin's own view.
    fn view(&self) -> ApplyView<'_, R, E> {
        self.apply.view()
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

    /// The wrapped `LambdaDef` `Value`, which this node emits at init.
    pub fn def_value(&self) -> &Value {
        &self.def
    }

    /// The literal's source identity (`LambdaDef::source`).
    pub fn source_id(&self) -> ExprId {
        self.spec.id
    }
}

/// The `init` of a definition: how a call site builds an instance from
/// the source body (or a builtin) in the definition's environment and
/// scope. A function of its data, so an image can rebuild it.
pub(crate) fn make_init<R: Rt, E: UserEvent>(
    id: LambdaId,
    flags: BitFlags<CFlag>,
    def_env: Env,
    def_scope: Scope,
    def_typ: Arc<FnType>,
    def_argspec: Arc<[Arg]>,
    def_spec: Expr,
    body: DefBody,
) -> InitFn<R, E> {
    SArc::new(move |scope, ctx, args, mode, tid| {
        // the definition's names, the call site's handlers
        let scope =
            Scope { dynamic: scope.dynamic.clone(), lexical: def_scope.lexical.clone() };
        ctx.with_restored(def_env.clone(), |ctx| match &body {
            DefBody::Expr(body) => instantiate(ctx, mode, &def_typ, |ctx, typ| {
                let argspec = def_argspec.clone();
                GXLambda::new(
                    ctx,
                    flags,
                    id,
                    typ,
                    argspec,
                    args,
                    &scope,
                    tid,
                    body.clone(),
                )
            }),
            DefBody::Collection(intrinsic) => {
                instantiate(ctx, mode, &def_typ, |ctx, typ| {
                    GXLambda::new_collection(
                        ctx,
                        id,
                        typ,
                        def_argspec.clone(),
                        args,
                        &scope,
                        tid,
                        def_spec.clone(),
                        *intrinsic,
                    )
                })
            }
            DefBody::BuiltIn(name) => {
                let init = match ctx.builtins.get(&**name).copied() {
                    Some(init) => init,
                    None if ctx.env.lsp_mode => UnknownBuiltIn::init as _,
                    None => bail!("unknown builtin function {name}"),
                };
                let resolved = mode.resolved();
                let typ =
                    resolved.map_or_else(|| def_typ.clone(), |r| Arc::new(r.clone()));
                let apply = init(ctx, &def_typ, resolved, &scope, args, tid)?;
                Ok(Box::new(BuiltInLambda { typ, name: name.clone(), apply }) as Box<_>)
            }
        })
    })
}

/// Do `a` and `b` list the same parameters, kind for kind?
pub(crate) fn same_parameters(a: &FnType, b: &FnType) -> bool {
    a.args.len() == b.args.len()
        && a.args.iter().zip(b.args.iter()).all(|(a, b)| a.kind == b.kind)
}

/// Build an instance at the signature `mode` names: the site's, unless a
/// dynamic bind's runtime callee has another parameter list than the
/// site's view, which takes the definition's own.
fn instantiate<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    mode: BindMode<'_>,
    def_typ: &Arc<FnType>,
    build: impl FnOnce(&mut ExecCtx<R, E>, Arc<FnType>) -> Result<GXLambda<R, E>>,
) -> Result<Box<dyn Apply<R, E>>> {
    let typ = match mode {
        BindMode::Static { instance, .. } => Arc::new(instance.clone()),
        BindMode::Dynamic(r) if same_parameters(r, def_typ) => Arc::new(r.clone()),
        BindMode::Dynamic(_) | BindMode::Definition => def_typ.clone(),
    };
    Ok(Box::new(build(ctx, typ)?))
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
        let mut constraints: LPooled<Vec<_>> = l
            .constraints
            .iter()
            .map(|(tv, tc)| {
                (tv.scope_refs(&scope.lexical), tc.scope_refs(&scope.lexical))
            })
            .collect();
        constraints.extend(trait_quantifiers.drain(..));
        let body = DefBody::of(&l.body);
        let builtin = match &l.body {
            Either::Left(_) => None,
            Either::Right(name) => Some(name),
        };
        if let DefBody::BuiltIn(builtin) = &body
            && ctx.builtins.get(builtin.as_str()).is_none()
        {
            if !ctx.env.lsp_mode {
                bail!("unknown builtin function {builtin}")
            }
            // the `'name` that ends the lambda's text
            let end = spec.end.0;
            let len = builtin.chars().count() as i32 + 1;
            let pos = SourcePosition { column: (end.column - len).max(1), ..end };
            let pos = if end == expr::WrittenAt::NOWHERE.0 { spec.pos } else { pos };
            let msg = format_args!(
                "unknown builtin function {builtin}: this graphix was not built \
                 with it, so calls are checked against its signature only"
            );
            ctx.env.warn(&spec.ori, pos, end, msg);
        }
        if let Some(builtin) = builtin {
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
            let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
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
        let init = make_init(
            id,
            flags,
            ctx.env.clone(),
            scope.append_block("fn", id.0),
            typ.clone(),
            argspec.clone(),
            spec.clone(),
            body.clone(),
        );
        let (intrinsic_effect, stateless) = match &body {
            DefBody::Expr(_) | DefBody::Collection(_) => (EffectKind::Sync, true),
            DefBody::BuiltIn(name) => {
                (ctx.builtin_effect(name), ctx.builtin_stateless(name))
            }
        };
        // No signature ref seeding here: the module tree is mid-registration
        // and a name's final target may not be registered yet. Cells fill
        // at typecheck.
        let def = ctx.lambdawrap.wrap(LambdaDef {
            id,
            typ: typ.clone(),
            env: ctx.env.clone(),
            argspec,
            init,
            scope: scope.clone(),
            check: Mutex::new(None),
            intrinsic_effect: Mutex::new(intrinsic_effect),
            stateless: AtomicBool::new(stateless),
            recursion: Mutex::new(RecursionKind::NotRecursive),
            source: spec.id,
            origin: DefOrigin::Source { body, flags, spec: spec.clone() },
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

impl Lambda {
    pub(crate) fn image_decode<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let id = LambdaId::decode(buf)?;
        let typ = Type::decode(buf)?;
        let def = ctx.lambda_defs.get(&id).ok_or(PackError::InvalidFormat)?.clone();
        Ok(Node::new(Self { spec, typ, resident: TagValue::stale(def.clone()), def }))
    }
}

/// A definition's gate: its body checks once, over a `Nop` per declared
/// argument, raising to a faux catch that collects its throws. Its
/// declared tvars are rigid (the body must be well-typed for any 'a;
/// anonymous '_N inference cells stay bindable) and a self-call knots to
/// its own cells (`ExecCtx::rec_defs`). Every path leaves by `close`.
// XCR claude for eric: shared by the def gate and a restored builtin's check;
// it leaves by an explicit `close(ctx)`, not on drop: a Drop cannot reach the
// context, and a guard holding `&mut ExecCtx` would lock it for the body check.
struct DefGate<R: Rt, E: UserEvent> {
    def: LambdaId,
    faux_id: BindId,
    args: LPooled<Vec<Node<R, E>>>,
    scope: Scope,
    rigid: LPooled<Vec<RigidGate>>,
}

impl<R: Rt, E: UserEvent> DefGate<R, E> {
    fn open(ctx: &mut ExecCtx<R, E>, def: &LambdaDef<R, E>) -> Self {
        let args = def.typ.args.iter().map(|at| Nop::new(at.typ.clone())).collect();
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
                pattern: None,
                facet: None,
            },
        );
        let scope = def.scope.with_catch((faux_id, ExprId::new()), false);
        let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        def.typ.collect_tvars(&mut named);
        named.retain(|name, _| !name.starts_with('_'));
        let rigid = named.values().map(|tv| tv.open_rigid()).collect();
        ctx.rec_defs.insert(def.id);
        ctx.def_gate_depth += 1;
        Self { def: def.id, faux_id, args, scope, rigid }
    }

    /// The error type the body raised to the gate's catch.
    fn thrown(&self, ctx: &ExecCtx<R, E>) -> Type {
        ctx.env.by_id[&self.faux_id].typ.deref_cloned().unwrap_or(Type::Bottom)
    }

    fn close(mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.def_gate_depth -= 1;
        ctx.rec_defs.remove(&self.def);
        ctx.env.by_id.remove_cow(&self.faux_id);
        for gate in self.rigid.drain(..) {
            gate.close();
        }
    }
}

/// A builtin's check `Apply`, as the definition gate builds it: the
/// builtin over the gate's arguments, checked once.
pub(crate) fn build_builtin_check<R: Rt, E: UserEvent>(
    def: &LambdaDef<R, E>,
    ctx: &mut ExecCtx<R, E>,
) -> Result<Box<dyn Apply<R, E>>> {
    let mut gate = DefGate::open(ctx, def);
    let res =
        (def.init)(&gate.scope, ctx, &mut gate.args, BindMode::Definition, ExprId::new())
            .and_then(|mut f| f.typecheck0(ctx, &mut gate.args).map(|()| f));
    gate.close(ctx);
    res
}

/// The definition's check of its labeled defaults, under the gate:
/// each default compiles in the def's scope and must fit its parameter.
/// Against a declared tvar it must fit the tvar's constraints, not the
/// variable, since a default is allowed to instantiate the variable at
/// a site that omits the argument (`CallSite::prepare_bind`).
fn check_defaults<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    def: &LambdaDef<R, E>,
    scope: &Scope,
) -> Result<()> {
    let flags = match &def.origin {
        DefOrigin::Source { flags, .. } => {
            let mut flags = *flags;
            flags.remove(CFlag::WarnUnhandled);
            flags
        }
        DefOrigin::Runtime => return Ok(()),
    };
    for (arg, at) in def.argspec.iter().zip(def.typ.args.iter()) {
        let Some(Some(expr)) = arg.labeled.as_ref() else { continue };
        let mut node = ctx.with_restored(def.env.clone(), |ctx| {
            compile(ctx, flags, expr.clone(), scope, ExprId::new())
        })?;
        let res = node.typecheck0(ctx).and_then(|()| {
            let typ = node.typ().clone();
            match &at.typ {
                Type::TVar(tv) if tv.read().typ.read().typ.is_none() => tv
                    .cell_constraints()
                    .iter()
                    .try_for_each(|c| c.check_contains(&ctx.env, &typ)),
                t => t.check_contains(&ctx.env, &typ),
            }
        });
        let res = res.at(&node.spec());
        node.delete(ctx);
        res?;
    }
    Ok(())
}

impl<R: Rt, E: UserEvent> Update<R, E> for Lambda {
    fn image_len(&self) -> usize {
        let id = self.lambda_id::<R, E>().map_or(0, |id| id.encoded_len());
        tag_len() + self.spec.encoded_len() + id + self.typ.encoded_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Lambda, buf);
        self.spec.encode(buf)?;
        self.lambda_id::<R, E>().ok_or(PackError::InvalidFormat)?.encode(buf)?;
        self.typ.encode(buf)
    }

    /// A lambda literal is a constant.
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        produce_constant(ctx, event, &mut self.resident, || self.def.clone())
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
        let spec = &self.spec;
        // Every arg, defaulted labeled ones included, checks as a Nop of
        // its declared type; the defaults themselves are checked after
        // the body (`check_defaults`), and again per omitting call site
        // (`CallSite::prepare_bind`), where one may narrow that site's cells.
        let mut gate = DefGate::open(ctx, def);
        let res = (def.init)(
            &gate.scope,
            ctx,
            &mut gate.args,
            BindMode::Definition,
            ExprId::new(),
        )
        .at(spec);
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
            let res = f.typecheck0(ctx, &mut gate.args).at(spec);
            for id in param_knot.drain(..) {
                ctx.def_gate_params.remove(&id);
            }
            // a builtin's check `Apply` is retained for `CallSite::typecheck1`;
            // a user body is not re-checked per call site
            match def.builtin_check() {
                None => f.delete(ctx),
                Some(check) => *check.lock() = Some(f),
            }
            res?;
            let inferred_throws =
                gate.thrown(ctx).scope_refs(&def.scope.lexical).normalize();
            ftyp.throws.check_contains(&ctx.env, &inferred_throws).at(spec)?;
            // record the gate's inferred facts as cell conjuncts; a nested
            // gate records closed facts only (`FnType::constrain_known`)
            ftyp.constrain_known(ctx.def_gate_depth > 1);
            Ok(())
        });
        let res = res.and_then(|()| check_defaults(ctx, def, &gate.scope));
        gate.close(ctx);
        // closed inferred bindings survive the gate: a solved fact must not
        // degrade to an upper bound a consumer can narrow first
        self.typ.unbind_open_tvars();
        // GRAPHIX_RIGID_AUDIT=1 is a cataloging tool: a rejected def that
        // continues may compile to a different shape, so never trust its
        // value output
        if let Err(e) = &res
            && dbgenv::graphix_rigid_audit()
        {
            eprintln!("RIGID-AUDIT reject: {spec} — {e:#}");
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
