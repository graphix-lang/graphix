use super::{Nop, compiler::compile};
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
    /// Whether the body holds no per-activation state — every builtin it
    /// reaches is `Effect::Stateless`, no `<-` targets a binding of its own, and
    /// every callee is stateless (`analysis::infer_effects`, the same
    /// fixpoint as `intrinsic_effect`). A tail loop reuses one activation
    /// only when this holds (`design/recursive_activations.md` §2).
    pub stateless: AtomicBool,
    /// How this lambda recurses (none / non-tail / tail). Summary
    /// computed by `analysis::analyze`; see [`RecursionKind`]. Defaults
    /// to `NotRecursive` until the pass runs. The operational tail-loop
    /// gate lives on `GXLambda::tail_loop`, not here.
    pub recursion: Mutex<RecursionKind>,
    /// The lambda EXPRESSION this def was compiled from. Stable across
    /// instance-body re-compiles (an `Expr` clone keeps its id), unlike
    /// the minted `id` — the identity call-site instantiation keys on
    /// ([`crate::FnArgIdentity`]).
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
    /// wake catch-up: set by `sleep()`, taken by the next update
    slept: bool,
    id: LambdaId,
    instance_id: LambdaInstanceId,
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
    /// The dispatch's return slot — `update` lends the body result
    /// (tag riding in the value) to the owning `CallSite` from here.
    resident: TagValue,
    /// `true` iff the previous dispatch's tail loop actually RE-ENTERED
    /// (jumped at least once). Its innermost frame left the body's node
    /// state mid-recursion (a select's `selected`, operator operand
    /// caches — the LAST iteration's values); the next genuinely-fired
    /// dispatch must not incrementally extend that state (Eric's ruling
    /// 2026-07-16: for a tail loop, frame state cannot survive across
    /// cycles — the user's model is a fresh call over the current
    /// formals, and the kernel, whose formals re-seed from the entry
    /// every invocation, is the reference). Read in `update` to run the
    /// first pass framed.
    prev_looped: bool,
    /// `true` until this instance's first dispatch has run. A fresh
    /// bind mints fresh formal pattern ids; the first dispatch seeds
    /// their VALUE CHANNEL from the args' quiet productions (the
    /// kernel delivers every param per invocation) — without it a
    /// rebound instance whose args are all stale (a parked transient
    /// woken by a capture) reads phantom formals and its body
    /// early-bottoms (transient-prime-park/01 under the flip).
    first_dispatch: bool,
    /// The DEF-side lexical env this instance's body was compiled
    /// under (snapshotted at build, inside the init's `with_restored`
    /// of the `LambdaDef` env). The body's typecheck must run under
    /// it too: the CallSite drives `typecheck0`/`typecheck1` from the
    /// CALLER's env, where the defining module's private typedefs are
    /// gone — a body annotation ref no def-time walk had cell-filled
    /// (a private type as a UNION MEMBER; the def gate's probe walks
    /// answer without expanding it) resolved against the caller's
    /// world and failed "undefined type" (the admin-TUI Toast
    /// recurrence of module-system finding 1, 2026-08-31). Args stay
    /// caller-side — only the body drive restores.
    env: Env,
}

/// True iff any of the body's external refs — formals or CAPTURES —
/// delivered a triggering event this cycle: the interp's read of the
/// kernel's "any param fired" entry condition (kernel params are
/// formals ++ captures). Reads through the dense seam: only a
/// DELIVERED production can trigger; the standing value channel never
/// does.
/// Did anything the body READS carry new information into this cycle?
///
/// Every id the body reads counts, not just the ones bound outside it.
/// A `<-` target declared inside the body (`x => { let s = 0; s <- e; s }`)
/// receives its update from the runtime across cycles exactly like a
/// capture does, so a poll that delivers one is not quiet — riding the
/// resident through it swallowed the connect's delivery outright, and
/// the loop never re-derived (aug15b katana fuzz 000000). Externality
/// was only ever a proxy for "can this change under us", and it stopped
/// being a sound one once a body could own the target.
fn inputs_triggered<R: Rt, E: UserEvent>(
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
        // Did anything TRIGGER this dispatch (a fired/tainted formal
        // delivery, or a real init view)? The tail loop below needs
        // this to derive its result tag — see the override at its end.
        let woke = std::mem::take(&mut self.slept) && ctx.frame_depth == 0;
        let mut entry_fired = event.init;
        let first = mem::replace(&mut self.first_dispatch, false);
        for (arg, pat) in from.iter_mut().zip(&self.args) {
            let tv = arg.update(ctx, event);
            let tag = tv.tag();
            entry_fired |= tag.triggers();
            // First dispatch of a fresh instance: seed the fresh
            // formal ids' VALUE CHANNEL from a quiet (stale) arg
            // production — the store is keyed by the PREVIOUS
            // instance's ids, so without this the body reads phantom
            // formals. Delivered both as a cycle-scoped overlay entry
            // (works at any frame depth) and, at depth 0, as a
            // standing store entry for later quiet cycles (R3: frames
            // never write the store). Triggering productions take the
            // normal publish below.
            //
            // Inside a frame the seed must run on EVERY dispatch, not
            // just the first: R3 means no framed publication ever
            // reaches the store, and the frame map is pass-scoped —
            // a first dispatch that happens inside a frame consumes
            // the one-shot seed into a map that dies with the pass,
            // and later passes rebind only triggering args. A formal
            // whose arg fires once ever (a callback lambda literal)
            // then reads phantom forever — the fold in a rec arm
            // reached through a tail step dispatched its slots with
            // kind=none for the life of the program (aug18a class 1).
            // ... and on the first dispatch after this site's SLEEP
            // (wake catch-up, design/wake_catchup.md): the arg
            // recomputed from present values, so its stale production
            // may carry a value the formals' store entries drifted
            // behind while the arm slept — re-seed the value channel.
            if (first || ctx.frame_depth > 0 || woke)
                && !tag.triggers()
                && !tag.is_bottom()
            {
                let v = tv.value_cloned();
                let store = ctx.frame_depth == 0;
                pat.bind(&v, &mut |id, v| {
                    if store {
                        // Store only — an overlay entry would shadow
                        // the store's R2 init-view upgrade (see the
                        // CallSite seed twin).
                        ctx.rt.store_insert_standing(id, TagValue::stale(v.clone()));
                    } else {
                        event.variables.insert(id, TagValue::stale(v.clone()));
                    }
                });
            }
            // Publish TRIGGERING deliveries only: a stale production is
            // the value channel, which the formal's store read already
            // serves (a standing entry reads Stale). A fresh bottom
            // poisons the formals AND persists in the store (the Bind
            // twin, ruled delta 7 / STRICT): a later quiet cycle's
            // Standing read must see the standing bottom, not
            // resurrect the pre-bottom value — the un-stored bottom
            // let a stdlib wrapper's formal serve the CLEAN list to a
            // fold whose real source had bottomed (aug13i
            // hz0-reactive-000001), where the kernel marshals
            // TAINT|STALE. Placeholder inserted per-id (never
            // destructured); frames never write the store (R3).
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
                    // R3: frames never write the store — a formal
                    // published from inside an enclosing loop's frame
                    // is loop plumbing, and storing it clobbered the
                    // cross-cycle channel with the LAST intra-loop
                    // rebind (the next dispatch's framed seed then
                    // read n=0 instead of the entry value —
                    // tail-arg-bottom/02 under the flip).
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
        // Cooperative interrupt: a runaway call TREE is time-unbounded
        // and only loop backedges polled the flag, so one atomic load
        // per dispatch makes it abortable (Eric approved 2026-07-04;
        // the JIT twin is `graphix_stack_check` at self-call sites).
        if ctx.control.interrupted() {
            // abort ≠ bottom: an interrupted dispatch re-surfaces its
            // last result on the value channel
            return self.resident.ride();
        }
        // A quiet poll of a previously-LOOPED tail body rides the
        // resident instead of re-deriving: the loop's rebinds were
        // frame-private and died with the frames, so an unframed pass
        // re-reads the ENTRY formals and derives the PRE-loop value —
        // a Stale production whose payload differs from the settled
        // result (`lp(500, 0) + x` rode a stale 0 over the settled
        // 125250 — aug13i hz1-fuzz). The kernel never invokes on a
        // quiet cycle, so riding is the exact twin. Sound to skip the
        // body entirely: tail_loop requires `lambda_is_sync`, so a
        // quiet pass can produce nothing new.
        if self.tail_loop.load(Ordering::Relaxed)
            && self.prev_looped
            && !entry_fired
            && !inputs_triggered(&self.body, ctx, event)
        {
            return self.resident.ride();
        }
        *ctx.active_lambdas.entry(self.id).or_insert(0) += 1;
        let res = if !self.tail_loop.load(Ordering::Relaxed) {
            // Non-tail recursion nests the Rust stack one level per
            // dispatch, on heap segments: depth is bounded by memory,
            // not a counter (design/recursive_activations.md §4b).
            crate::stack::ensure_sufficient(|| self.body.update(ctx, event).clone())
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
            // The loop's per-iteration rebinds are FRAME-PRIVATE under
            // dense delivery: they live in the frame overlay and die
            // with it — the old cached-write + snapshot/restore
            // machinery (the jul04 leftover-acc class) is structurally
            // gone because frames never touch the store.
            //
            // FRAME DISCIPLINE (reset_replay): every RE-ENTERED pass is
            // a fresh evaluation frame — replay caches cleared, and the
            // body run against a private overlay (reads fall through
            // the frame stack to the store) under a forced init view.
            // This is what retires the tail-arg stale-cache class: a
            // jump whose arg expression bottoms no longer dispatches
            // with the previous pass's published value. The first pass
            // stays an ordinary poll on the real event (#8, soak jul04)
            // — UNLESS the previous dispatch actually looped: its
            // innermost frame left the body's node state mid-recursion,
            // and an ordinary incremental pass would observably RESUME
            // the recursion (jul16a fuzz class D). Frame state cannot
            // survive across cycles (Eric's ruling 2026-07-16) — run
            // that first pass framed too: a full re-derivation from the
            // current formals, which is what the kernel does on every
            // invocation. Quiet polls stay ordinary passes: they read
            // the leftover state but nothing fires, so nothing escapes
            // (#8 stays fixed).
            let mut frame: LPooled<IntMap<BindId, TagValue>> = LPooled::take();
            let mut reentered = false;
            // "Genuinely fired" includes CAPTURES, not just formal
            // deliveries: the kernel's params are formals ++ captures,
            // so a capture fire re-runs the whole body there. A
            // capture-fed jump arg (`countdown(n - 1, x / 3)`) left
            // the interp's recursive arm ASLEEP on capture-only
            // cycles (the retained tail-select arm gated it) — fired
            // once where the kernel fired per capture event (jul16g
            // fuzz divergence 000001). Safe now that framed
            // re-derivation carries the honest tag algebra (the
            // tail-spine no-fold + becoming-selected rules in
            // node/select.rs — replay-frames v3): re-deriving is not
            // re-firing.
            let framed = self.prev_looped
                && !event.init
                && (entry_fired || inputs_triggered(&self.body, ctx, event));
            if framed {
                self.body.reset_replay(ctx);
                // Seed the framed first pass's frame with the FORMALS'
                // current per-cycle truth: a delivered formal keeps
                // its cycle tag, a standing one reads QUIET (stale) —
                // exactly the kernel's param staging for a retained
                // input (an un-redelivered formal must NOT read fresh:
                // the freshness over-fired an output whose value
                // ignores the changed capture, tail-jump-honest-tags/
                // 00, Eric ruled kernel-right 2026-07-18). The framed
                // re-derivation's RE-MATCH comes from the select's
                // in-frame value-driven flow driver, not from minted
                // firing.
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
            // Fresh tail-scrutinee accumulator for this dispatch (the
            // kernel initializes `tail_scrut_stale` per invocation);
            // the previous value is restored below so nested
            // dispatches can't bleed into each other.
            let prev_tsf = mem::replace(&mut ctx.tail_scrut_fired, false);
            let res = loop {
                // Cooperative interrupt: a wedged tail loop aborts when
                // `interrupt()`/`abort()` is requested (`do_cycle`
                // clears the one-shot Interrupt; Abort additionally
                // shuts down). Abort ≠ bottom: ride the last result.
                if ctx.interrupted() {
                    break self.resident.ride().clone();
                }
                let res = if !reentered && !framed {
                    self.body.update(ctx, event).clone()
                } else {
                    event.enter_frame(mem::take(&mut *frame));
                    let prev = mem::replace(&mut event.init, true);
                    // The dispatch's REAL init rides beside the forced
                    // one: literal nodes inside frames produce FIRED
                    // iff the dispatch itself was a genuine init — the
                    // kernel's `init_flag`, uniform across all of an
                    // invocation's iterations (`const_stale_gate`). A
                    // dispatch already INSIDE another loop's frames
                    // saw the outer's forced flag, so the real init
                    // INHERITS through nesting (the kernel threads the
                    // parent's flag into callee invocations).
                    let real = if ctx.frame_depth > 0 { ctx.frame_init } else { prev };
                    let prev_fi = mem::replace(&mut ctx.frame_init, real);
                    ctx.frame_depth += 1;
                    let res = self.body.update(ctx, event).clone();
                    ctx.frame_depth -= 1;
                    ctx.frame_init = prev_fi;
                    event.init = prev;
                    *frame = event.exit_frame();
                    // Deliver anything the pass raised that must escape
                    // the frame (a `catch` handler's error — see
                    // `ExecCtx::frame_outbox`). `event.variables` is
                    // the outer map again here; at depth 0 that is the
                    // real event, so nested frames bubble outward one
                    // level per unwind instead of landing in an
                    // intermediate private map that would swallow them
                    // just the same.
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
                        ctx.frame_init,
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
                // A `None` arg rides the formal's previous ENTRY —
                // value AND tag — exactly the kernel's taint-gated
                // rebind, which keeps both the old payload and the old
                // disc in the loop slot. Riding the value alone with a
                // forced STALE tag under-fired: a formal delivered
                // FIRED at entry (or rebound FIRED earlier in this
                // same evaluation) stays fired when a later jump rides
                // it (tailalt3's final-jump ride read the base arm
                // stale; countdown's init emission was suppressed the
                // same way). Lookup order: the previous frame (last
                // rebind in this evaluation), then the ordinary dense
                // read (overlays, then the store's value channel).
                // Rebinds are FRAME-PRIVATE — nothing touches the
                // store, which is what retired the old cached
                // snapshot/restore.
                let prev: LPooled<IntMap<BindId, TagValue>> =
                    mem::replace(&mut frame, LPooled::take());
                for (v, pat) in p.args.iter().zip(self.args.iter()) {
                    match v {
                        // The jump's production tag rides into the
                        // formal delivery (Eric's ruling 2026-07-18,
                        // tail_jump_fired_plumbing): an `n - 1` chain
                        // from a quiet entry stays STALE through the
                        // loop instead of being minted FIRED — the
                        // kernel's rebind disc carry.
                        Some(tv) => {
                            let (v, tag) = tv.clone().into_parts();
                            pat.bind(&v, &mut |id, v| {
                                frame.insert(id, TagValue::tagged(v, tag));
                            })
                        }
                        None => pat.ids(&mut |id| {
                            let tv =
                                prev.get(&id).cloned().or_else(|| match super::read_var(
                                    ctx, event, &id,
                                ) {
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
                                });
                            if let Some(tv) = tv {
                                frame.insert(id, tv);
                            }
                        }),
                    }
                }
            };
            // Update the looped flag on GENUINE dispatches only. Under
            // dense delivery the callsite polls its callee EVERY cycle
            // — a quiet poll (no triggering formal, no external, not
            // init, never reentered) evaluates nothing that could
            // clean the body's mid-recursion frame state, so it must
            // not clear the flag either: the pre-flip sparse gate
            // never dispatched quiet cycles at all, and clearing here
            // let the NEXT genuine dispatch run unframed and
            // observably RESUME the recursion (the jul16 class D
            // artifact returned — frame-state-cross-cycle/
            // 00_resumed_recursion under the flip).
            if reentered
                || framed
                || event.init
                || entry_fired
                || inputs_triggered(&self.body, ctx, event)
            {
                self.prev_looped = reentered;
            }
            // Result-tag derivation for a loop that actually RE-ENTERED
            // (or ran its first pass framed): every framed pass runs
            // under a forced init view (body constants must re-fire per
            // jump), which poisons the body's own tag FIRED regardless
            // of what triggered the call. Derive from the ENTRY instead
            // — the kernel's rebind-and-jump derives its result disc
            // from the call site's input discs: fired iff a formal
            // delivery triggered, the dispatch ran under a real init
            // view, or a captured input triggered this cycle. A first
            // pass that never jumped and wasn't framed ran on the real
            // event and keeps its organic tag.
            let res = if (reentered || framed) && !res.is_tainted() {
                // DOWNGRADE-only on the value chain (replay-frames
                // v3): with constants stale inside frames,
                // frame-forced init never counting as a fire, and
                // the tail-spine no-scrutinee-fold, the body's
                // organic tag IS the kernel's value-chain disc —
                // ride it. Two folds on top, both kernel twins:
                // the stale force protects the #8 class (a
                // dispatch nothing genuinely triggered must not
                // emit; the old unconditional FIRED-upgrade
                // re-fired results whose fired inputs a quiet
                // select had suppressed), and the tail-scrutinee
                // accumulator upgrades — `emit_kernel_return`'s
                // `fold_stale`: the result fires if any tail-
                // select scrutinee on the executed path fired,
                // even when the taken arm's own production is
                // stale (a const base arm re-selected by a later
                // cycle's loop — jul21g divergence).
                let entry = entry_fired || inputs_triggered(&self.body, ctx, event);
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
        // Lend the body result — tag riding in the value — to the
        // owning CallSite through the resident. Bottom is a DELIVERED
        // production under dense delivery: the old depth-0
        // taint→absence conversion is gone (its jul23e protection — a
        // `~` consuming its debt on a bottoming callee — moved into
        // Sample itself, which takes debt on Fired only).
        self.resident.set(res)
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        args: &mut [Node<R, E>],
    ) -> Result<()> {
        for (arg, FnArgType { typ, .. }) in args.iter_mut().zip(self.typ.args.iter()) {
            wrap!(arg, arg.typecheck0(ctx))?;
            wrap!(arg, typ.check_contains_rigid(&ctx.env, &arg.typ()))?;
        }
        // The body typechecks under the DEF-side env it was compiled
        // under (see the `env` field) — the caller's env may lack the
        // defining module's private typedefs.
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
        // The inline loop emits per the collection lambda's OWN return
        // shape; a callsite node whose type inference widened to a
        // union must hand its consumers a genuine Value pair (jul18d
        // crash — see `widen_result_to_value`).
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
        // This call-site instance owns a monomorphic body, so give it
        // the same region-fusion pass as the top-level graph. Collection
        // callbacks then compile inside the instance when their calls
        // resolved statically.
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
        self.slept = true;
        // Crossing into a callee body ends the shrink scope: a recursion
        // shrinking one level does not shrink an external call it made,
        // and a whole-recursion pause is not a shrink at all (both
        // retain — sleep-is-pause). `Select::update` re-arms the flag for
        // each arm it actively deselects.
        let saved = ctx.shrink_unwind;
        ctx.shrink_unwind = false;
        self.body.sleep(ctx);
        ctx.shrink_unwind = saved;
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        // The formals' STORE entries survive: under dense delivery the
        // store is the cross-cycle value channel (a framed pass's
        // rebinds are frame-private overlays, so the old previous-
        // frame-formal leak is structurally gone — the store holds the
        // genuine entry delivery, which is what the kernel's formal
        // re-seed reads).
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
        // `argspec` and `typ.args` are parallel — one pattern per
        // signature parameter. A narrower `typ` (fewer params than the
        // lambda actually declares) makes the zip below TRUNCATE the
        // param patterns, silently dropping the tail parameters: a
        // collection callback typed by the HOF's declared
        // `fn(x: 'a) -> 'b` against a user lambda `|#foo = 42, x|`
        // kept only `foo` (bound to its default) and dropped the
        // positional `x`, so the element was never delivered and the
        // body's `x` fell through to an outer binding (aug27a katana).
        // Bail so a Dynamic-mode dispatch retries with the full
        // definition signature (`def_typ`), which has the right arity.
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
        let body = build_body(ctx, &argpats)?;
        Ok(Self {
            slept: false,
            id,
            instance_id: LambdaInstanceId::new(),
            args: Box::from_iter(argpats.drain(..)),
            typ,
            body,
            tail_loop: AtomicBool::new(false),
            self_recursive: AtomicBool::new(false),
            self_bind: Mutex::new(None),
            resident: TagValue::phantom(),
            prev_looped: false,
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
        // MUST delegate: the trait default's `Ok(None)` would silently
        // swallow every builtin's emission hook and the builtin
        // "loses fusion" with no error anywhere.
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
    spec: Expr,
    def: Value,
    typ: Type,
    resident: TagValue,
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
        // (`|s: Read|` ≡ `'s: Read |s: 's|`) — see
        // `Type::rewrite_trait_args`; it joins the declared quantifiers
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
        let def_typ = typ.clone();
        let def_argspec = argspec.clone();
        let def_spec = spec.clone();
        let body = l.body.clone();
        let init: InitFn<R, E> = SArc::new(move |scope, ctx, args, mode, tid| {
            // restore the lexical environment to the state it was in
            // when the closure was created
            ctx.with_restored(def_env.clone(), |ctx| match body.clone() {
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
                        lexical: def_scope.lexical.clone(),
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
        // Deliberately NO signature seeding here: at lambda-compile
        // time the enclosing module tree is mid-registration, and an
        // early resolve can capture a DIFFERENT def than the name's
        // final meaning (tui's `list::List` means the tui::list
        // submodule's type, but resolves to the list PACKAGE's before
        // that submodule compiles). Cells fill at typecheck (after
        // all registrations), which makes instance signatures
        // env-independent at static-bind time.
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
            // a lambda literal is a constant of function type: present
            // on the value channel from birth (see Constant)
            resident: TagValue::stale(def),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Lambda {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // A lambda literal is a constant of function type — same
        // production rule as `Constant`: FIRED at init, the STALE
        // value channel inside frames (a framed `let f = |..| ..`
        // re-binds quietly so the body's call sites stay computable).
        // Frame depth first — frames force init (see Constant).
        if ctx.frame_depth > 0 {
            if ctx.frame_init {
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
        // The compile inserted this node's def into `ctx.lambda_defs`;
        // without this removal the map retained every lazily-compiled
        // def forever (each transient-recursion re-bind mints a few),
        // which kept the defs' `LambdaIds` link-graph nodes alive — so
        // `typecheck1`'s `ids()` walks grew without bound for the life
        // of the process (25µs → 1.1ms per bind over 90k binds, the
        // jul22b class). Dropping the map entry drops the def, the
        // dead links prune on the next walk, and `finalize_lambda`
        // correctly skips ids with no live def.
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
            },
        );
        let gate_scope = def.scope.with_catch((faux_id, ExprId::new()));
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
            &gate_scope,
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
        if res.is_err() && crate::dbgenv::graphix_rigid_audit() {
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
