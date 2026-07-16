use super::{Nop, compiler::compile};
use crate::{
    Apply, ApplyView, ApplyViewMut, BindId, BindMode, CFlag, Event, ExecCtx, InitFn,
    LambdaId, LambdaInstanceId, Node, NodeView, Refs, Rt, Scope, Tag, TagValue, Update,
    UserEvent,
    effects::{EffectKind, RecursionKind},
    env::{Bind, Env},
    expr::{self, Arg, ErrorContext, Expr, ExprId, ModPath, Origin},
    fusion::emit::{BodyCx, CompiledExpr},
    node::{
        callsite::CallSite, collection::CollectionIntrinsic, pattern::StructPatternNode,
    },
    typ::{FnArgKind, FnArgType, FnType, TVar, Type, fntyp::LambdaIds},
    wrap,
};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use combine::stream::position::SourcePosition;
use compact_str::format_compact;
use enumflags2::BitFlags;
use log::error;
use netidx::{pack::Pack, subscriber::Value, utils::Either};
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
    /// The lambda's pretty-printed SOURCE — a compile-stable identity
    /// for contexts where the minted `id` can't compare (two compiles
    /// of the same program mint different ids; the differential
    /// oracle normalizes fn values to this). Deliberately NOT used by
    /// the compiler's own equality: `PartialEq` below stays id-based
    /// because callsite rebind detection (`Callee::DynamicBound { def
    /// } if def == &v`) must distinguish same-source closures over
    /// different captured environments.
    pub src: ArcStr,
    pub env: Env,
    pub scope: Scope,
    pub argspec: Arc<[Arg]>,
    pub typ: Arc<FnType>,
    pub init: InitFn<R, E>,
    pub check: Mutex<Option<Box<dyn Apply<R, E>>>>,
    /// Intrinsic sync/async effect — see `effects::EffectKind` and
    /// `design/whole_graph_fusion.md`. Computed by the M6 effect
    /// inference pass after all lambdas have been compiled. Defaults
    /// to `Sync`; the pass walks each lambda body and flips to
    /// `Async` if it finds an async-effect builtin call or a call to
    /// another async user lambda. Function-typed parameter calls do
    /// NOT contribute here — those are handled at the call site via
    /// the lattice join with the resolved fn-arg's effect.
    pub intrinsic_effect: Mutex<EffectKind>,
    /// How this lambda recurses (none / non-tail / tail). Summary
    /// computed by `analysis::analyze`; see [`RecursionKind`]. Defaults
    /// to `NotRecursive` until the pass runs. The operational tail-loop
    /// gate lives on `GXLambda::tail_loop`, not here.
    pub recursion: Mutex<RecursionKind>,
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
    ) -> std::result::Result<(), netidx::pack::PackError> {
        Err(netidx::pack::PackError::Application(0))
    }

    fn decode(
        _buf: &mut impl bytes::Buf,
    ) -> std::result::Result<Self, netidx::pack::PackError> {
        Err(netidx::pack::PackError::Application(0))
    }
}

/// Runtime representation of a graphix-language lambda (i.e. a user
/// `fn` defined in `.gx` source). Produced by [`LambdaDef::init`]
/// when a `CallSite` resolves to this lambda — either lazily on
/// first runtime use, or eagerly at compile time by
/// `CallSite::try_static_resolve` (in `typecheck1`).
///
/// Public surface for fusion: `Apply::view()` on `GXLambda` returns
/// [`ApplyView::Lambda(&self)`], letting fusion's walker
/// reach `self.body()` and inline the lambda body into the kernel
/// being built.
#[derive(Debug)]
pub struct GXLambda<R: Rt, E: UserEvent> {
    id: LambdaId,
    instance_id: LambdaInstanceId,
    dispatch: LambdaDispatch,
    scope: ModPath,
    args: Box<[StructPatternNode]>,
    body: Node<R, E>,
    typ: Arc<FnType>,
    /// The operational tail-loop gate: `true` iff this lambda is sync,
    /// self-tail-recursive, and has loop-able formals. Set by
    /// `analysis::analyze` (through `&self`, hence the atomic), read by
    /// both the interpreter (`Apply::update` loops in place instead of
    /// recursing) and the JIT (`build_lambda_kernel` emits a native
    /// loop). `false` until the analysis runs / for non-tail lambdas.
    tail_loop: AtomicBool,
    self_recursive: AtomicBool,
    self_bind: Mutex<Option<BindId>>,
    /// The tag of the last value `update` returned — surfaced through
    /// `Apply::out_tag` so the owning `CallSite` can reconstitute the
    /// body result's two-channel tag across the clean-`Value` Apply
    /// boundary.
    last_out: Tag,
}

#[derive(Debug, Clone, Copy)]
enum LambdaDispatch {
    Graphix,
    Collection,
}

impl<R: Rt, E: UserEvent> GXLambda<R, E> {
    /// The lambda definition's stable id. All `GXLambda` instances
    /// produced from the same `LambdaDef::init` carry the same id;
    /// fusion uses this as part of the `(LambdaId, FnType)` cache
    /// key for on-demand kernel monomorphization.
    pub fn id(&self) -> LambdaId {
        self.id
    }

    pub fn instance_id(&self) -> LambdaInstanceId {
        self.instance_id
    }

    /// The compiled body Node — the lambda's expression tree.
    /// Fusion walks this via [`Update::view`] /
    /// [`NodeView`].
    pub fn body(&self) -> &Node<R, E> {
        &self.body
    }

    /// Mutable body — for fusion's splicing of inner sub-kernels.
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

    /// Argument-binding patterns, in signature order. Parallel to
    /// `self.typ().args`. Each pattern binds one positional or
    /// labeled arg from the call-site `arg_refs` into the body's
    /// scope.
    pub fn args(&self) -> &[StructPatternNode] {
        &self.args
    }

    /// The fully-resolved `FnType` of this lambda. Same as what
    /// `Apply::typ()` returns; provided as a direct accessor for
    /// consumers that have a `&GXLambda` without going through the
    /// trait.
    pub fn typ(&self) -> &Arc<FnType> {
        &self.typ
    }

    /// The operational tail-loop gate — see the `tail_loop` field. Read
    /// by both backends; both must agree, so this is the single source.
    pub fn tail_loop(&self) -> bool {
        self.tail_loop.load(Ordering::Relaxed)
    }

    /// Set the tail-loop gate. Takes `&self` (the field is atomic) so the
    /// analysis pass can mark a lambda it reaches through a shared `&Node`.
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

fn check_instance_type<R: Rt, E: UserEvent>(
    ctx: &ExecCtx<R, E>,
    scope: &ModPath,
    expected: &Type,
    actual: &Type,
) -> Result<()> {
    let probe = expected.check_contains_rigid(&ctx.env, actual);
    if let Err(e) = &probe
        && std::env::var_os("GXDBG_INSTCHECK").is_some()
    {
        crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
            eprintln!(
                "INSTCHECK-PROBE-FAIL scope={scope} opaque={}\n  expected={expected}\n  actual={actual}\n  err={e:#}",
                e.downcast_ref::<crate::typ::AbstractOpaque>().is_some()
            );
            Ok::<_, std::fmt::Error>(())
        })
        .ok();
    }
    match probe {
        Err(e) if e.downcast_ref::<crate::typ::AbstractOpaque>().is_some() => {
            let expected = crate::fusion::lowering::privatize_type(
                &ctx.fusion.abstract_registry,
                expected,
                &ctx.env,
                scope,
            );
            let actual = crate::fusion::lowering::privatize_type(
                &ctx.fusion.abstract_registry,
                actual,
                &ctx.env,
                scope,
            );
            let r = expected.check_contains_rigid(&ctx.env, &actual);
            if r.is_err() && std::env::var_os("GXDBG_INSTCHECK").is_some() {
                crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
                    eprintln!(
                        "INSTCHECK-RETRY-FAIL scope={scope}\n  expected={expected}\n  actual={actual}\n  err={:#}",
                        r.as_ref().unwrap_err()
                    );
                    Ok::<_, std::fmt::Error>(())
                })
                .ok();
            }
            r
        }
        result => result,
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
    ) -> Option<Value> {
        // Did anything TRIGGER this dispatch (a fired/tainted formal
        // delivery, or a real init view)? The tail loop below needs
        // this to derive its result tag — see the override at its end.
        let mut entry_fired = event.init;
        for (arg, pat) in from.iter_mut().zip(&self.args) {
            if let Some(tv) = arg.update(ctx, event) {
                let (v, tag) = tv.into_parts();
                entry_fired |= tag.triggers();
                if tag.is_tainted() {
                    // never destructure a taint placeholder; poison the
                    // formals and keep it out of the cross-cycle store
                    pat.ids(&mut |id| {
                        event.variables.insert(id, TagValue::tainted(Value::Null));
                    });
                } else {
                    pat.bind(&v, &mut |id, v| {
                        ctx.rt.cached_mut().insert(id, v.clone());
                        event.variables.insert(id, TagValue::tagged(v.clone(), tag));
                    })
                }
            }
        }
        // The call-depth guard: each nested (non-tail) lambda dispatch
        // counts one against the shared limit; at the limit the
        // dispatch produces BOTTOM (logged) instead of recursing the
        // Rust stack to death. One entry = one unit — the tail loop
        // below iterates inside this frame, so tail recursion is exempt
        // in exactly the way it is in a JIT'd rebind-and-jump loop.
        // Bottom (not a catchable error value): the callsite's static
        // type has no error branch to carry one — the same reasoning as
        // unchecked-arith overflow and div-by-zero (hot-path failures
        // log and bottom; see `Control::max_call_depth`).
        //
        // Cooperative interrupt first: exponential-BREADTH recursion is
        // depth-bounded but time-unbounded (2^n calls each under the
        // depth limit — the fib-mutant wedge class), and only loop
        // backedges polled the flag. One atomic load per dispatch makes
        // a runaway call TREE abortable (Eric approved 2026-07-04; the
        // JIT twin lives in graphix_depth_push).
        if ctx.control.interrupted() {
            return None;
        }
        let depth_pushed = match self.dispatch {
            LambdaDispatch::Graphix => {
                if !ctx.control.depth_push() {
                    let limit = ctx.control.max_call_depth();
                    if std::env::var("GRAPHIX_DBG_DEPTH").is_ok() {
                        eprintln!(
                            "DEPTH TRIP lambda id={:?} tail_loop={}",
                            self.id,
                            self.tail_loop.load(Ordering::Relaxed)
                        );
                    }
                    error!(
                        "call depth limit ({limit}) exceeded in {} — deep non-tail \
                         recursion produces no value (raise via \
                         Control::set_max_call_depth)",
                        self.body.spec()
                    );
                    ctx.diagnostics.push(crate::RtDiagnostic::CallDepthLimit {
                        limit,
                        spec: self.body.spec().clone(),
                    });
                    return None;
                }
                true
            }
            LambdaDispatch::Collection => false,
        };
        *ctx.active_lambdas.entry(self.id).or_insert(0) += 1;
        let res = if !self.tail_loop.load(Ordering::Relaxed) {
            self.body.update(ctx, event)
        } else {
            // Sync self-tail-recursion: loop in place instead of recursing on
            // the Rust stack (which overflows; the JIT compiles this to a
            // native loop). A tail-position self-call in the body stashes its
            // rebind args in `ctx.pending_tail_call` and returns without
            // dispatching (`CallSite::update`); we take them, rebind the
            // formals, and re-run the body. `event.init = true` on each
            // RE-ENTERED pass makes it a "fresh call" — init-gated nodes
            // (Constants) re-fire, matching both the old fresh-body-per-level
            // node-walk and the JIT's per-iteration re-execution. The FIRST
            // pass honors the event's real init flag: it is an ordinary
            // call/poll, exactly like the non-tail-loop path above, and
            // forcing init there re-fired the body's constants on every
            // passive re-poll — one spurious result emit per cycle whenever
            // any unrelated event flowed (#8, soak jul04; the JIT, which
            // gates the kernel on its inputs' fired bits, was right).
            // The loop's per-iteration rebinds below are INTERNAL loop
            // state, but `pat.bind` writes them into `ctx.cached`, which
            // outlives the call. Snapshot the formals' cached entries and
            // restore them after the loop, so a later call whose arg is
            // quiet re-reads the ARG's value — not the previous loop's
            // final iteration (a `g(in0, i64:0)` accumulated across calls:
            // the const seed never re-fires, so the formal kept the last
            // loop's leftover acc; the JIT, whose formals live in
            // registers seeded fresh from the args each invocation, was
            // right — soak jul04 follow-up).
            let mut saved: LPooled<Vec<(BindId, Option<Value>)>> = LPooled::take();
            for pat in self.args.iter() {
                pat.ids(&mut |id| {
                    saved.push((id, ctx.rt.cached().get(&id).cloned()));
                });
            }
            // FRAME DISCIPLINE (reset_replay): every RE-ENTERED pass is
            // a fresh evaluation frame — replay caches cleared, and the
            // body run against a PRIVATE variables map (externals seeded
            // from the runtime cache + the jump's rebound formals) under
            // a forced init view. This is
            // what retires the tail-arg stale-cache class: a jump whose
            // arg expression bottoms no longer dispatches with the
            // previous pass's published value. The first pass stays an
            // ordinary poll on the real event (#8, soak jul04).
            let mut seeds: LPooled<IntMap<BindId, TagValue>> = LPooled::take();
            let mut frame: LPooled<IntMap<BindId, TagValue>> = LPooled::take();
            let mut reentered = false;
            let res = loop {
                // Cooperative interrupt: a wedged tail loop aborts to bottom
                // when `interrupt()`/`abort()` is requested (`do_cycle` clears
                // the one-shot Interrupt; Abort additionally shuts down).
                if ctx.interrupted() {
                    break None;
                }
                let res = if !reentered {
                    self.body.update(ctx, event)
                } else {
                    mem::swap(&mut event.variables, &mut *frame);
                    let prev = mem::replace(&mut event.init, true);
                    ctx.frame_depth += 1;
                    let res = self.body.update(ctx, event);
                    ctx.frame_depth -= 1;
                    event.init = prev;
                    mem::swap(&mut event.variables, &mut *frame);
                    res
                };
                let mine = matches!(
                    &ctx.pending_tail_call,
                    Some(p) if p.lambda == self.id
                );
                if !mine {
                    break res;
                }
                if !reentered {
                    reentered = true;
                    // Seeds are the kernel's params: an external that
                    // FIRED this cycle keeps its real entry; a quiet
                    // one is seeded STALE from the runtime cache (the
                    // value channel).
                    let mut refs = Refs::default();
                    self.body.refs(&mut refs);
                    refs.with_external_refs(|id| {
                        if let Some(tv) = event.variables.get(&id) {
                            seeds.insert(id, tv.clone());
                        } else if let Some(v) = ctx.rt.cached().get(&id) {
                            seeds.insert(id, TagValue::stale(v.clone()));
                        }
                    });
                }
                let p = ctx.pending_tail_call.take().unwrap();
                self.body.reset_replay(ctx);
                frame.clear();
                frame.extend(seeds.iter().map(|(k, v)| (*k, v.clone())));
                for (v, pat) in p.args.iter().zip(self.args.iter()) {
                    pat.bind(v, &mut |id, v| {
                        ctx.rt.cached_mut().insert(id, v.clone());
                        frame.insert(id, TagValue::fired(v));
                    })
                }
            };
            if reentered {
                for (id, v) in saved.drain(..) {
                    match v {
                        Some(v) => {
                            ctx.rt.cached_mut().insert(id, v);
                        }
                        None => {
                            ctx.rt.cached_mut().remove(&id);
                        }
                    }
                }
            }
            // Result-tag derivation for a loop that actually RE-ENTERED:
            // every re-entered pass runs under a forced init view (body
            // constants must re-fire per jump), which poisons the body's
            // own tag FIRED regardless of what triggered the call.
            // Derive from the ENTRY instead — the kernel's
            // rebind-and-jump derives its result disc from the call
            // site's input discs: fired iff a formal delivery triggered,
            // the dispatch ran under a real init view, or a captured
            // input triggered this cycle. A first pass that never jumped
            // ran on the real event and keeps its organic tag.
            match res {
                Some(tv) if reentered && !tv.is_tainted() => {
                    let entry = entry_fired || {
                        let mut refs = Refs::default();
                        self.body.refs(&mut refs);
                        let mut hit = false;
                        refs.with_external_refs(|id| {
                            hit |= event
                                .variables
                                .get(&id)
                                .is_some_and(|tv| tv.tag().triggers());
                        });
                        hit
                    };
                    let v = tv.value();
                    Some(if entry { TagValue::fired(v) } else { TagValue::stale(v) })
                }
                res => res,
            }
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
        if depth_pushed {
            ctx.control.depth_pop();
        }
        // Surface the body result's tag across the clean-Value Apply
        // boundary (the CallSite reconstitutes via `out_tag`).
        match res {
            Some(tv) => {
                let (v, tag) = tv.into_parts();
                self.last_out = tag;
                Some(v)
            }
            None => None,
        }
    }

    fn out_tag(&self) -> Tag {
        self.last_out
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        args: &mut [Node<R, E>],
    ) -> Result<()> {
        for (arg, FnArgType { typ, .. }) in args.iter_mut().zip(self.typ.args.iter()) {
            wrap!(arg, arg.typecheck0(ctx))?;
            wrap!(arg, check_instance_type(ctx, &self.scope, typ, &arg.typ()))?;
        }
        wrap!(self.body, self.body.typecheck0(ctx))?;
        wrap!(
            self.body,
            check_instance_type(ctx, &self.scope, &self.typ.rtype, &self.body.typ())
        )?;
        Ok(())
    }

    /// CallSite phase: drive the body's `typecheck1` so nested call
    /// sites in the body finalize against their resolved types (the
    /// cascade). The caller's args are walked by the driving
    /// `CallSite::typecheck1`, so we don't re-walk them here.
    fn typecheck1(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _resolved: &FnType,
    ) -> Result<()> {
        wrap!(self.body, self.body.typecheck1(ctx))
    }

    fn emit_clif(
        &self,
        callsite: &CallSite<R, E>,
        cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        match self.body.view() {
            NodeView::MapQ(map) => map.emit_clif_call(callsite, cx),
            NodeView::FoldQ(fold) => fold.emit_clif_call(callsite, cx),
            _ => Ok(None),
        }
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        // This call-site instance owns a monomorphic body, so give it
        // the same region-fusion pass as the top-level graph. Collection
        // callbacks then compile inside the instance when their calls
        // resolved statically.
        if std::env::var_os("GXDBG_INSTANCE_FUSION").is_some() {
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
        self.body.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        // The formal binds this instance published into `rt.cached`
        // (`update`'s `pat.bind` closure) are replay memory — a frame
        // whose arg didn't arrive must not run the body against the
        // previous frame's formals. Per-instance ids, read only through
        // this instance's body.
        for pat in self.args.iter() {
            pat.ids(&mut |id| {
                ctx.rt.cached_mut().remove(&id);
            });
        }
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
        Self::new_with_body(
            ctx,
            id,
            LambdaDispatch::Graphix,
            typ,
            argspec,
            args,
            scope,
            origin,
            |ctx, _| compile(ctx, flags, body, scope, tid),
        )
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
            LambdaDispatch::Collection,
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
        dispatch: LambdaDispatch,
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
        let body = build_body(ctx, &argpats)?;
        Ok(Self {
            id,
            instance_id: LambdaInstanceId::new(),
            dispatch,
            scope: scope.lexical.clone(),
            args: Box::from_iter(argpats.drain(..)),
            typ,
            body,
            tail_loop: AtomicBool::new(false),
            self_recursive: AtomicBool::new(false),
            self_bind: Mutex::new(None),
            last_out: Tag::FIRED,
        })
    }
}

#[derive(Debug)]
struct BuiltInLambda<R: Rt, E: UserEvent> {
    typ: Arc<FnType>,
    apply: Box<dyn Apply<R, E> + Send + Sync + 'static>,
    /// The DEF's fn scope — check_instance_type's privatize gate needs
    /// a scope inside the defining module so an abstract-typed arg
    /// bridges to its private form (the cons-building fold callback:
    /// the acc cell binds the caller's OPAQUE view at the top-level
    /// unification, and the builtin's declared formal is the private
    /// view — same seam GXLambda's per-arg checks already bridge).
    scope: ModPath,
}

impl<R: Rt, E: UserEvent> Apply<R, E> for BuiltInLambda<R, E> {
    /// Pass-through to the inner builtin. `BuiltInLambda` is a
    /// runtime-plumbing wrapper (typecheck/refs); fusion sees the
    /// wrapped builtin's own view as if the wrapper weren't here.
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
        // Without this delegation the trait default's `Ok(None)`
        // silently swallows every builtin's emission hook — the
        // call site falls to DynCall and the builtin "loses fusion"
        // with no error anywhere (it happened: Stage D2 landed
        // Builtin emission would otherwise be swallowed by the wrapper.
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
    ) -> Option<Value> {
        self.apply.update(ctx, from, event)
    }

    fn out_tag(&self) -> Tag {
        // MUST delegate (the reset_replay/emit_clif trap): the default
        // FIRED would erase a wrapped lambda's stale/taint result tag.
        self.apply.out_tag()
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
            // Through check_instance_type, not a raw check_contains:
            // an abstract-typed arg (an acc cell bound to the caller's
            // OPAQUE view) must bridge to the private form the
            // declared formal carries — the def-scope privatize retry.
            wrap!(args[i], check_instance_type(ctx, &self.scope, atyp, &args[i].typ()))?
        }
        // The old post-hoc constraint-list check is retired (phase C):
        // cell conjuncts are validated at every bind by
        // `cell_constraints_ok`, which reaches the same cells with a
        // better error site.
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
        // MUST delegate (same trap as emit_clif/fuse above): a no-op
        // here would silently leave the wrapped builtin's arg caches
        // replaying across frames.
        self.apply.reset_replay(ctx);
    }
}

#[derive(Debug)]
pub struct Lambda {
    top_id: ExprId,
    spec: Expr,
    def: Value,
    flags: BitFlags<CFlag>,
    typ: Type,
}

impl Lambda {
    /// LambdaId of this lambda's definition — pulled from the
    /// `LambdaDef` stored as a `Value`. Used by `Bind::compile`
    /// to thread the id into `BuiltinBindInfo` so the fusion
    /// pre-binding pass can later look up the lambda's env+scope
    /// for compiling labeled-default expressions.
    pub fn lambda_id<R: Rt, E: UserEvent>(&self) -> Option<LambdaId> {
        self.def.downcast_ref::<LambdaDef<R, E>>().map(|d| d.id)
    }

    /// Borrow the underlying `LambdaDef`. The static-call resolution
    /// pass uses this to call `InitFn` (or construct a `GXLambda`
    /// directly) at compile time when it can prove the call site's
    /// function expression always resolves to this Lambda.
    pub fn def<R: Rt, E: UserEvent>(&self) -> Option<&LambdaDef<R, E>> {
        self.def.downcast_ref::<LambdaDef<R, E>>()
    }

    /// The wrapped `LambdaDef` `Value`. Equivalent to the value the
    /// Lambda Node emits on its init event.
    pub fn def_value(&self) -> &Value {
        &self.def
    }
}

impl Lambda {
    pub(crate) fn compile<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        l: &expr::LambdaExpr,
        top_id: ExprId,
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
        let rtype = l.rtype.as_ref().map(|t| t.scope_refs(&scope.lexical));
        let throws = l.throws.as_ref().map(|t| t.scope_refs(&scope.lexical));
        let mut argspec = l
            .args
            .iter()
            .map(|a| match &a.constraint {
                None => Arg {
                    labeled: a.labeled.clone(),
                    pattern: a.pattern.clone(),
                    constraint: None,
                    pos: a.pos,
                },
                Some(typ) => Arg {
                    labeled: a.labeled.clone(),
                    pattern: a.pattern.clone(),
                    constraint: Some(typ.scope_refs(&scope.lexical)),
                    pos: a.pos,
                },
            })
            .collect::<LPooled<Vec<_>>>();
        let argspec = Arc::from_iter(argspec.drain(..));
        let constraints = l
            .constraints
            .iter()
            .map(|(tv, tc)| {
                let tv = tv.scope_refs(&scope.lexical);
                let tc = tc.scope_refs(&scope.lexical);
                Ok((tv, tc))
            })
            .collect::<Result<LPooled<Vec<_>>>>()?;
        let original_scope = scope.clone();
        let _original_scope = scope.clone();
        let scope = scope.append(&format_compact!("fn{}", id.0));
        let _scope = scope.clone();
        let env = ctx.env.clone();
        let _env = ctx.env.clone();
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
                quantifiers: Arc::from_iter(
                    constraints.iter().map(|(tv, _)| tv.name.clone()),
                ),
                lambda_ids: LambdaIds::default(),
            })
        };
        // Seed the CELL constraints — the explicit `'a: T |…|` form is
        // sugar for a constrained cell, and the cells are the ONLY
        // store (phase C). Alias same-named leaves onto the declared
        // quantifier tvars FIRST so the conjunct lands in the one
        // shared cell every occurrence points at.
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
        let _typ = typ.clone();
        let _argspec = argspec.clone();
        let _spec = spec.clone();
        let body = l.body.clone();
        let init: InitFn<R, E> = SArc::new(move |scope, ctx, args, mode, tid| {
            // restore the lexical environment to the state it was in
            // when the closure was created
            ctx.with_restored(_env.clone(), |ctx| match body.clone() {
                Either::Left(body) => {
                    // Always GXLambda for now. The new fusion pipeline
                    // (`fuse`) will splice a
                    // `FusedKernel` Update node into the graph
                    // *before* runtime, so by the time this InitFn
                    // fires we either have no kernel for this lambda
                    // (run via GXLambda) or the runtime is already
                    // calling into the FusedKernel directly via the
                    // splice. No InitFn cache lookup needed.
                    let scope = Scope {
                        dynamic: scope.dynamic.clone(),
                        lexical: _scope.lexical.clone(),
                    };
                    // Static user instances use the full definition-shaped
                    // signature after it has been refined by the call site in
                    // the definition's private type scope. Dynamic binding may
                    // retry the shared definition signature because the runtime
                    // callee can differ from the site's prior view.
                    let build = |ctx: &mut ExecCtx<R, E>, typ: Arc<FnType>| {
                        GXLambda::new(
                            ctx,
                            flags,
                            id,
                            typ,
                            _argspec.clone(),
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
                            .or_else(|_| build(ctx, _typ.clone())),
                        BindMode::Definition => build(ctx, _typ.clone()),
                    }
                    .map(|a| -> Box<dyn Apply<R, E>> { Box::new(a) })
                }
                Either::Right(builtin) => {
                    if let Some(intrinsic) = CollectionIntrinsic::from_name(&builtin) {
                        let scope = Scope {
                            dynamic: scope.dynamic.clone(),
                            lexical: _scope.lexical.clone(),
                        };
                        let build = |ctx: &mut ExecCtx<R, E>, typ: Arc<FnType>| {
                            GXLambda::new_collection(
                                ctx,
                                id,
                                typ,
                                _argspec.clone(),
                                args,
                                &scope,
                                tid,
                                _spec.clone(),
                                intrinsic,
                            )
                        };
                        let result = match mode {
                            BindMode::Static { instance, .. } => {
                                build(ctx, Arc::new(instance.clone()))
                            }
                            BindMode::Dynamic(r) => build(ctx, Arc::new(r.clone()))
                                .or_else(|_| build(ctx, _typ.clone())),
                            BindMode::Definition => build(ctx, _typ.clone()),
                        };
                        result.map(|a| -> Box<dyn Apply<R, E>> { Box::new(a) })
                    } else {
                        match ctx.builtins.get(&*builtin) {
                            None => bail!("unknown builtin function {builtin}"),
                            Some(init) => {
                                let typ = match mode.resolved() {
                                    Some(r) => Arc::new(r.clone()),
                                    None => _typ.clone(),
                                };
                                let resolved = mode.resolved();
                                init(ctx, &_typ, resolved, &_scope, args, tid).map(
                                    |apply| {
                                        let f: Box<dyn Apply<R, E>> =
                                            Box::new(BuiltInLambda {
                                                typ,
                                                apply,
                                                scope: _scope.lexical.clone(),
                                            });
                                        f
                                    },
                                )
                            }
                        }
                    }
                }
            })
        });
        // Deliberately NO signature seeding here: at lambda-compile
        // time the enclosing module tree is mid-registration, and an
        // early resolve can capture a DIFFERENT def than the name's
        // final meaning (tui's `list::List` means the tui::list
        // submodule's type, but resolves to the list PACKAGE's before
        // that submodule compiles). Cells fill at typecheck (after
        // all registrations) and the privatize walk makes instance
        // signatures env-independent at static-bind time.
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
            recursion: Mutex::new(RecursionKind::NotRecursive),
        });
        ctx.lambda_defs.insert(id, def.clone());
        Ok(Box::new(Self { spec, def, typ: Type::Fn(typ), top_id, flags }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Lambda {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<TagValue> {
        // A lambda literal is a constant of function type — same
        // production rule as `Constant`: FIRED at init, the STALE
        // value channel inside frames (a framed `let f = |..| ..`
        // re-binds quietly so the body's call sites stay computable).
        if event.init {
            Some(TagValue::fired(self.def.clone()))
        } else if ctx.frame_depth > 0 {
            Some(TagValue::stale(self.def.clone()))
        } else {
            None
        }
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn refs(&self, _refs: &mut Refs) {}

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

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
        // EVERY arg body-checks as a Nop of its DECLARED type —
        // including defaulted labeled args. Defaults are per-CALLSITE
        // constructs (Eric's ruling, 2026-07-09): they compile and
        // typecheck at each omitting site against that site's
        // instantiated signature (`setup_bind`), where a generic
        // default legitimately narrows the site's cells. Checking the
        // default here against the def's rigid cells rejected every
        // generic-typed default (rand's f64 seeds vs `'a: [Float,
        // Int]`), and the old faux-compile also BOUND def cells the
        // gate then had to unwind.
        let mut faux_args: LPooled<Vec<Node<R, E>>> = def
            .typ
            .args
            .iter()
            .map(|at| {
                let n: Node<R, E> = Box::new(Nop { typ: at.typ.clone() });
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
            },
        );
        let prev_catch = ctx.env.catch.insert_cow(def.scope.dynamic.clone(), faux_id);
        // DECLARED (named) signature tvars are RIGID for the DURATION
        // of this def gate: the body must be well-typed for ARBITRARY
        // 'a, so a concrete body type can't bind (and thereby escape)
        // the annotation — see `TCell::rigid`. Anonymous '_N inference
        // cells (unannotated args, inferred rtype) stay bindable.
        // Rigidity is CLEARED below with the gate's other cell state
        // (`unbind_tvars`): late/dynamic binds legitimately build
        // instances against the def's own cells (the #18 fallback in
        // the InitFn above), and a permanently-rigid def refused every
        // such bind.
        let mut named_tvs: LPooled<ahash::AHashMap<ArcStr, TVar>> = LPooled::take();
        def.typ.collect_tvars(&mut named_tvs);
        named_tvs.retain(|name, _| !name.starts_with('_'));
        for tv in named_tvs.values() {
            tv.set_rigid();
        }
        // While this def's body is checked, a self-call site must knot
        // to the def's own ftype cells (see `ExecCtx::rec_defs`).
        ctx.rec_defs.insert(def.id);
        ctx.def_gate_depth += 1;
        let res = (def.init)(
            &def.scope,
            ctx,
            &mut faux_args,
            BindMode::Definition,
            ExprId::new(),
        )
        .with_context(|| ErrorContext(Update::<R, E>::spec(self).clone()));
        let res = res.and_then(|mut f| {
            let ftyp = f.typ().clone();
            // Fn-typed params knot the same way self-calls do: a body
            // callsite calling `f` must unify against the param's OWN
            // declared cells or the body can't prove it delivers the
            // declared (rigid) rtype — see `ExecCtx::def_gate_params`.
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
            // Retain a check `Apply` for every BUILTIN lambda so
            // `CallSite::typecheck1` can run its resolved-`typecheck1`
            // (validation / type extraction). A user `GXLambda`
            // (`ApplyView::Lambda`) is discarded — its body is not
            // re-checked per call site. Structural test replacing the
            // old `needs_callsite` flag.
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
                .with_deref(|t| t.cloned())
                .unwrap_or(Type::Bottom)
                .scope_refs(&def.scope.lexical)
                .normalize();
            ftyp.throws
                .check_contains(&ctx.env, &inferred_throws)
                .with_context(|| ErrorContext(Update::<R, E>::spec(self).clone()))?;
            // Record the gate's inferred facts as cell conjuncts at
            // EVERY gate exit. Skipping nested gates entirely
            // (8630436f's depth-1 gate) was the typing-acceptance hole
            // (jul10h 000002): an inline lambda's own body fact
            // (`n == i64:3` ⟹ 'n ⊇ i64) was never recorded, so a
            // tuple-passing site validated against a fully-open formal
            // — no error, and the JIT froze a scalar slot the runtime
            // fed a tuple. Nested gates record CLOSED facts only —
            // the entanglement scoping that motivated the depth gate
            // (see `FnType::constrain_known`).
            ftyp.constrain_known(ctx.def_gate_depth > 1);
            Ok(())
        });
        ctx.def_gate_depth -= 1;
        ctx.rec_defs.remove(&def.id);
        ctx.env.by_id.remove_cow(&faux_id);
        match prev_catch {
            Some(id) => ctx.env.catch.insert_cow(def.scope.dynamic.clone(), id),
            None => ctx.env.catch.remove_cow(&def.scope.dynamic),
        };
        for tv in named_tvs.values() {
            tv.clear_rigid();
        }
        // CLOSED inferred bindings survive the gate — a solved fact
        // (the body DELIVERS that type) must not degrade to an
        // upper-bound constraint a consumer can narrow first. Only
        // open/partial cells re-open for per-site solving.
        self.typ.unbind_open_tvars();
        // GRAPHIX_RIGID_AUDIT=1: report a def-gate failure and
        // continue — a CATALOGING tool for surveying which defs the
        // rigid-tvar gate rejects, NOT a semantics-preserving escape:
        // a refused acceptance check bails the walk before the interior
        // bindings the old accept path used to make, so a rejected def
        // that continues under audit can compile to a DIFFERENT shape
        // (finding 37 diverged with a pointer-typed leak under audit
        // and AGREEs under enforcement). Never trust value output from
        // an audit-mode run of a program whose defs it rejected.
        if res.is_err() && std::env::var_os("GRAPHIX_RIGID_AUDIT").is_some() {
            if let Err(e) = &res {
                eprintln!("RIGID-AUDIT reject: {} — {e:#}", Update::<R, E>::spec(self));
            }
            return Ok(());
        }
        res
    }

    /// A lambda *definition* node has no children in the main node tree —
    /// its body is reached and `typecheck1`'d per call site through
    /// `GXLambda::Apply::typecheck1`. Nothing to do here.
    fn typecheck1(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Lambda(self)
    }
}
