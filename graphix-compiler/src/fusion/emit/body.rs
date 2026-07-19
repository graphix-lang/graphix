//! Kernel body emission: [`BodyCx`] (the context every
//! `emit_clif` receives), the [`BodyEmitter`] node relay,
//! forcing/bottom/interrupt machinery, tail-rebind jumps, and
//! the kernel return protocol.

use crate::{
    BindId, Node, NodeView, Rt, Update, UserEvent,
    env::Env,
    expr::{Expr, ExprId, ExprKind},
    fusion::{
        LambdaCallInfo, intern,
        kernel_abi::{self, AbiKind, AbstractRegistry},
        lowering::BuiltinCallSiteInfo,
    },
    typ::Type,
};
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use cranelift_codegen::ir::{
    Block, BlockArg, FuncRef, InstBuilder, MemFlags, Value as ClifValue,
    condcodes::IntCC, types,
};
use cranelift_frontend::{FunctionBuilder, Variable};
use netidx_value::Value;

use super::{
    abi::{
        CompiledExpr, JitEnv, LocalKind, STALE, ValueVar, emit_untainted_i64,
        is_not_fresh, propagate_flags, scalar_disc, value_disc,
    },
    call::{CompositeSource, drop_owned_composites, emit_pending_cleanup},
    flow::emit_body_tail,
    lower::{LowerCtx, SelWord, SiteLayout, SlotTableFrame},
    nodes::emit_owned_value_operand_node,
    scalar::scalar_to_payload_i64,
};

// ─── Body / statement compilation ────────────────────────────────

/// The rebind-and-jump core of a self tail-call
/// ([`emit_self_tail_call`]).
/// `new_vals` are the already-evaluated replacement values for the
/// leading `new_vals.len()` tail-call slots — the FORMALS; trailing
/// capture slots stay bound (loop-invariant within one invocation).
/// Each rebind writes BOTH registers of the slot's ValueVar — the
/// payload AND the disc. The disc carry is load-bearing for firing:
/// the loop's terminating arm returns a formal, and its disc at that
/// point must be the LAST iteration's computed fired-ness (the kernel
/// mirror of the interp loop's per-iteration `pat.bind`). Rebinding
/// payloads only left every formal's disc at its ENTRY value, so a
/// constant-seeded accumulator (`g(in0, i64:0)`) returned the seed's
/// non-init STALE on every later invocation — the callee flagged
/// not-fresh, the caller forced bottom, and a live-input-driven tail
/// loop fired exactly once (soak jul04 follow-up; the same
/// carry-the-disc lesson as fold/#9).
/// `sources[i]` classifies each arg's composite provenance so a
/// Borrowed pointer is refcount-bumped before the old slot value
/// drops. Drops every owned non-slot local above the param mark
/// (per-iteration lets would leak otherwise), truncates the
/// compile-time env back to the params, and jumps to the loop head.
pub(super) fn emit_tail_rebind_jump(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
    new_vals: Vec<CompiledExpr>,
    sources: &[CompositeSource],
    taints: &[ClifValue],
) -> Result<()> {
    let head = ctx.loop_head.ok_or_else(|| {
        anyhow!("kernel malformed: TailCall in kernel without has_tail_loop")
    })?;
    // A TAINTED new value keeps the slot's loop-carried previous value
    // instead of rebinding — the kernel twin of the node-walk dispatch
    // backfilling a quiet-or-bottomed arg from its cache (combineLatest;
    // Eric's ruling 2026-07-15). The whole-kernel abort this replaces
    // made a bottoming jump arg kill the invocation where the node-walk
    // kept looping on the last good value (soak-jul14b 000004).
    // Back-compat: hand-built test kernels leave `tail_call_slots`
    // empty and assume all params are scalar in declaration order.
    // Drive the rebind positionally in that case.
    if ctx.tail_call_slots.is_none() {
        debug_assert_eq!(new_vals.len(), ctx.param_mark);
        for (i, v) in new_vals.iter().enumerate() {
            let vv = env.locals[i].vv;
            let old_p = b.use_var(vv.payload);
            let old_d = b.use_var(vv.disc);
            let p = b.ins().select(taints[i], old_p, v.payload);
            let d = b.ins().select(taints[i], old_d, v.disc);
            b.def_var(vv.payload, p);
            b.def_var(vv.disc, d);
        }
        env.truncate(ctx.param_mark);
        b.ins().jump(head, &[]);
        return Ok(());
    }
    let slots = ctx.tail_call_slots.unwrap();
    // Slots cover EVERY kernel value param (they double as the
    // runtime arg layout — `arg_layout`, kernel.rs); a tail call
    // rebinds only the leading FORMALS.
    debug_assert!(new_vals.len() <= slots.len());
    use kernel_abi::AbiParamKind;
    let drop_helper = ctx
        .helper_refs
        .get("graphix_valarray_drop")
        .ok_or_else(|| anyhow!("missing graphix_valarray_drop"))?;
    let clone_helper = ctx
        .helper_refs
        .get("graphix_valarray_clone")
        .ok_or_else(|| anyhow!("missing graphix_valarray_clone"))?;
    for ((i, slot), v) in slots.iter().enumerate().zip(new_vals.iter()) {
        match slot.kind.abi() {
            AbiParamKind::Scalar(_) => {
                let vv = env
                    .lookup_name(&slot.name)
                    .ok_or_else(|| {
                        anyhow!("TailCall: scalar slot `{}` not in env", slot.name)
                    })?
                    .vv;
                let old_p = b.use_var(vv.payload);
                let old_d = b.use_var(vv.disc);
                let p = b.ins().select(taints[i], old_p, v.payload);
                let d = b.ins().select(taints[i], old_d, v.disc);
                b.def_var(vv.payload, p);
                b.def_var(vv.disc, d);
            }
            AbiParamKind::Array | AbiParamKind::Tuple | AbiParamKind::Struct => {
                // Composite rebind: drop the previously-owned pointer in
                // the slot and store the new one — branched on the taint
                // bit. REPLACE clones a Borrowed new value first (the
                // next iteration must hold its own reference) and drops
                // the old slot pointer; KEEP leaves the slot untouched
                // and drops an Owned new value instead (the unconsumed
                // production — a Borrowed one is someone else's).
                let vv = env
                    .lookup_name(&slot.name)
                    .ok_or_else(|| {
                        anyhow!("TailCall: composite slot `{}` not in env", slot.name)
                    })?
                    .vv;
                let keep_bl = b.create_block();
                let replace_bl = b.create_block();
                let cont_bl = b.create_block();
                b.ins().brif(taints[i], keep_bl, &[], replace_bl, &[]);
                b.seal_block(keep_bl);
                b.seal_block(replace_bl);
                b.switch_to_block(replace_bl);
                let newp = if sources[i] == CompositeSource::Borrowed {
                    let call = b.ins().call(clone_helper, &[v.payload]);
                    b.inst_results(call)[0]
                } else {
                    v.payload
                };
                let old = b.use_var(vv.payload);
                b.ins().call(drop_helper, &[old]);
                b.def_var(vv.payload, newp);
                b.def_var(vv.disc, v.disc);
                b.ins().jump(cont_bl, &[]);
                b.switch_to_block(keep_bl);
                if sources[i] == CompositeSource::Owned {
                    b.ins().call(drop_helper, &[v.payload]);
                }
                b.ins().jump(cont_bl, &[]);
                b.seal_block(cont_bl);
                b.switch_to_block(cont_bl);
            }
            AbiParamKind::Variant => {
                return Err(anyhow!("JIT: variant tail-call rebind not yet supported"));
            }
            AbiParamKind::Nullable => {
                // The tail-loop gate (`build_lambda_kernel`'s
                // all-formals-Prim/Array/Tuple/Struct check) keeps
                // Nullable formals out of tail loops, so this is
                // unreachable in practice. Err keeps the codegen safe
                // if the gating ever drifts.
                return Err(anyhow!(
                    "JIT: nullable tail-call rebind not supported — \
                     the tail-loop gate should have refused this kernel"
                ));
            }
            AbiParamKind::String | AbiParamKind::Value => {
                // A recursive lambda whose tail-call rebinds a String
                // / value-shape param. The JIT doesn't lower the
                // owned-ArcStr / two-word Value rebind yet; bail so
                // the kernel falls back to the node-walk.
                return Err(anyhow!(
                    "JIT: string/value tail-call rebind not yet \
                     supported — falling back to interp"
                ));
            }
        }
    }
    // Drop any owned composite/variant/nullable/string locals
    // introduced between `ctx.param_mark` and now that ISN'T in
    // `tail_call_slots` (slot rebinds drop the old slot value above;
    // non-slot lets above the tail-call would leak per iteration
    // otherwise). Block / select-arm locals were already dropped at
    // runtime by their scope-exit code (block emission, terminating
    // statements), so iterating the env's tail catches only the
    // non-Block top-level lets that didn't get a rebind slot.
    let arr_drop = ctx
        .helper_refs
        .get("graphix_valarray_drop")
        .ok_or_else(|| anyhow!("missing graphix_valarray_drop"))?;
    let val_drop = ctx
        .helper_refs
        .get("graphix_value_drop")
        .ok_or_else(|| anyhow!("missing graphix_value_drop"))?;
    let str_drop = ctx
        .helper_refs
        .get("graphix_arcstr_drop")
        .ok_or_else(|| anyhow!("missing graphix_arcstr_drop"))?;
    // Composite slot rebinds already drop their old value (above);
    // skip any entry whose name matches a rebind slot. Drop every other
    // owned non-slot local above the param mark, by kind.
    let slot_names: std::collections::HashSet<&str> =
        slots.iter().map(|s| s.name.as_str()).collect();
    let drops: smallvec::SmallVec<[(LocalKind, ValueVar); 8]> = env.locals
        [ctx.param_mark..]
        .iter()
        .filter(|l| !slot_names.contains(l.name.as_str()))
        .map(|l| (l.kind, l.vv))
        .collect();
    for (kind, vv) in drops {
        match kind {
            LocalKind::Scalar(_) => {}
            LocalKind::Composite => {
                let ptr = b.use_var(vv.payload);
                b.ins().call(arr_drop, &[ptr]);
            }
            LocalKind::String => {
                let ptr = b.use_var(vv.payload);
                b.ins().call(str_drop, &[ptr]);
            }
            LocalKind::Variant | LocalKind::Nullable | LocalKind::Value => {
                let disc = b.use_var(vv.disc);
                let payload = b.use_var(vv.payload);
                b.ins().call(val_drop, &[disc, payload]);
            }
        }
    }
    // Compile-time env-Vec hygiene: pop everything above the param
    // mark so the next iteration starts with a clean lexical state.
    env.truncate(ctx.param_mark);
    b.ins().jump(head, &[]);
    Ok(())
}

/// Emit a value-bottom abort: when the I8 `valid` bit is 0, set the
/// pending flag, run `emit_pending_cleanup`, and jump to `pending_exit`
/// (so `Kernel::update` returns `None`). Falls through to a fresh
/// `continue_block` when valid. Used where a tainted scalar is consumed
/// by a site that has no per-value validity channel (e.g. a composite
/// producer field) — the bottom must propagate to the kernel OUTPUT.
pub(super) fn emit_bottom_abort(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
    valid: ClifValue,
) -> Result<()> {
    let pending_set = ctx
        .helper_refs
        .get("graphix_dyncall_set_pending")
        .ok_or_else(|| anyhow!("missing graphix_dyncall_set_pending"))?;
    let pre_pending = b.create_block();
    let continue_block = b.create_block();
    let pending_exit = {
        let mut slot = ctx.pending_exit.borrow_mut();
        match *slot {
            Some(blk) => blk,
            None => {
                let blk = b.create_block();
                *slot = Some(blk);
                blk
            }
        }
    };
    b.ins().brif(valid, continue_block, &[], pre_pending, &[]);
    b.switch_to_block(pre_pending);
    b.seal_block(pre_pending);
    b.ins().call(pending_set, &[]);
    emit_pending_cleanup(b, env, ctx)?;
    b.ins().jump(pending_exit, &[]);
    b.switch_to_block(continue_block);
    b.seal_block(continue_block);
    Ok(())
}

/// Emit a cooperative-interrupt poll at a loop head: call
/// `graphix_interrupted`, and if it returns nonzero take the kernel's
/// abort path — set the pending flag, drop in-flight owned
/// buffers/composites (`emit_pending_cleanup`, which drains the HOF
/// result buffer off `dyncall_buf_stack` and drops owned params), and
/// jump to `pending_exit` so `Kernel::update` yields `None`. Falls
/// through to a fresh `continue_block` otherwise. Reused at the tail-loop
/// head and every HOF scaffold loop head so a wedged native loop honours
/// `interrupt()`/`abort()`. This is [`emit_bottom_abort`] with the branch
/// reversed — abort on the interrupted bit instead of on an invalid bit.
///
/// First cut polls every iteration; amortizing the helper call to every K
/// iterations is a future optimization (the poll roughly doubles the
/// tightest native loops).
pub(super) fn emit_interrupt_check(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
    let interrupted = ctx
        .helper_refs
        .get("graphix_interrupted")
        .ok_or_else(|| anyhow!("missing graphix_interrupted"))?;
    let pending_set = ctx
        .helper_refs
        .get("graphix_dyncall_set_pending")
        .ok_or_else(|| anyhow!("missing graphix_dyncall_set_pending"))?;
    let call = b.ins().call(interrupted, &[]);
    let intr = b.inst_results(call)[0];
    let pre_pending = b.create_block();
    let continue_block = b.create_block();
    let pending_exit = {
        let mut slot = ctx.pending_exit.borrow_mut();
        match *slot {
            Some(blk) => blk,
            None => {
                let blk = b.create_block();
                *slot = Some(blk);
                blk
            }
        }
    };
    // nonzero (interrupted) ⇒ abort; zero ⇒ continue the loop body.
    b.ins().brif(intr, pre_pending, &[], continue_block, &[]);
    b.switch_to_block(pre_pending);
    b.seal_block(pre_pending);
    b.ins().call(pending_set, &[]);
    emit_pending_cleanup(b, env, ctx)?;
    b.ins().jump(pending_exit, &[]);
    b.switch_to_block(continue_block);
    b.seal_block(continue_block);
    Ok(())
}

// ─── Node → CLIF body emission ──────────────────────────────────
//
// Bodies are emitted by walking the region-root [`Node`] graph:
// `Update::emit_clif` / `Apply::emit_clif` impls recurse into children
// and delegate to the `emit_*_node` helpers below, which share the
// scalar codegen primitives
// (`compile_bin`/`compile_cmp`/`compile_cast`/`JitEnv`/`CompiledExpr`)
// so there is exactly one home for the CLIF logic.
//
// A node without an `emit_clif` impl (any async op, an unsupported
// shape) returns `Err` → the kernel build fails → `try_fuse` leaves
// the region un-spliced → it node-walks (the universal fallback).
// Bottom/taint: a may-bottom scalar (div/mod-by-zero) produces a
// `CompiledExpr` whose disc carries `TAINT`, resolved where it's
// consumed (`emit_bottom_abort` / the kernel boundary).

/// A type-erased producer of a kernel function BODY, given the
/// already-set-up entry block, bound params, and lowering context. The
/// sole implementor is [`NodeBodyEmitter`], which walks the
/// region-root `Node` via `emit_clif` recursion. Erasing the `R`/`E`
/// of the Node lets the (monomorphic) JIT-compile pipeline thread a
/// generic `&Node<R, E>` through without itself becoming generic.
pub(super) trait BodyEmitter {
    fn emit(
        &self,
        b: &mut FunctionBuilder,
        env: &mut JitEnv,
        ctx: &LowerCtx,
    ) -> Result<()>;

    /// Discovered sync-builtin Apply sites for the region being
    /// emitted (`CallSite::emit_clif` lowers a registered site to a
    /// DynCall via [`BodyCx::builtin_site`]). `None` for callee
    /// bodies (their inner sites are #203-unresolved).
    fn builtin_apply_sites(
        &self,
    ) -> Option<&nohash::IntMap<ExprId, BuiltinCallSiteInfo>> {
        None
    }

    /// Discovered statically-resolved lambda call sites for the region
    /// being emitted (`CallSite::emit_clif` lowers a registered site to
    /// a CLIF `call` via [`BodyCx::lambda_site`]). `None` for callee
    /// bodies (a callee's only cross-kernel reference is itself).
    fn lambda_call_sites(&self) -> Option<&nohash::IntMap<ExprId, LambdaCallInfo>> {
        None
    }

    /// `Some` when the kernel being emitted is a self-recursive lambda
    /// body: the binding its self-references carry + the kernel's own
    /// call descriptor. Drives tail-position rebind-and-jump
    /// (`emit_body_tail`), value-position self-calls
    /// (`CallSite::emit_clif` → the kernel's own FuncRef), and the
    /// self-FuncRef import in `define_kernel_body`.
    fn self_call(&self) -> Option<&(BindId, LambdaCallInfo)> {
        None
    }

    /// The environment snapshot for TYPE RESOLUTION ONLY (#218): node
    /// `typ` cells can carry `Type::Ref`s to abstract type names whose
    /// concrete rep needs `env.lookup_ref` + the abstract registry
    /// (`resolve_abstract`) before `abi_kind`/freeze can classify
    /// them. Defaults to `None`; [`NodeBodyEmitter`] supplies it. NOT
    /// for binding lookups; those stay in the analysis phase, per the
    /// BodyCx design.
    fn type_env(&self) -> Option<&Env> {
        None
    }

    /// The compiling context's abstract-type registry — the fusion
    /// classifiers (`abi_kind`/freeze/`resolve_abstract`) consult it to
    /// peek through abstract types to their wire shape. Threaded from the
    /// `ExecCtx` into [`LowerCtx`] so emit-time type classification has
    /// the same view as the analysis phase.
    fn registry(&self) -> &AbstractRegistry;

    /// Base offset added to every DynCall `fn_index` this body bakes, so
    /// the body indexes its slots in the REGION-WIDE combined `dyn_slots`
    /// table (parent slots first, then each callee's). `0` for the parent
    /// (its slots lead the table) and for a callee with no DynCalls;
    /// nonzero only for a callee body whose `fn_params` sit at an offset.
    /// Doubles as the `base` half of the `(ptr, base)` JIT cache key —
    /// see `compile_kernel_with_callees_inner`. Default `0`.
    fn fn_index_offset(&self) -> u32 {
        0
    }

    /// Whether this body's kernel return FORCES on not-fresh
    /// (TAINT | STALE). `true` for a PUBLISHED body — the region root /
    /// an inline collection callback whose result flows through the wrapper
    /// to `Kernel::update`: a not-fresh result must yield `None`,
    /// matching the node-walk's "publish only on Some". `false` for a
    /// cross-kernel CALLEE body, which never forces: its result's
    /// not-fresh bits ride back to the caller as DATA, in-band in the
    /// returned disc (the unified Value ABI) — so a bottomed or
    /// unfired callee result bottoms only the caller's consumers that
    /// read it (#219), like a node-walk callsite that produced no
    /// value. Default `true`.
    fn gate_stale_at_return(&self) -> bool {
        true
    }

    /// The region's LIFTED connect-target bind ids — let-bound scalar
    /// counters/accumulators routed in as kernel inputs (feeders).
    /// `emit_let_node` reads this to emit the seed-select for a lifted
    /// binding instead of a plain let; `emit_connect_node` reads it to
    /// allow the variable write. Empty for callees and for any region
    /// with no lifts. Default: a shared empty set.
    fn lifted(&self) -> &ahash::AHashSet<BindId> {
        static EMPTY: std::sync::LazyLock<ahash::AHashSet<BindId>> =
            std::sync::LazyLock::new(ahash::AHashSet::default);
        &EMPTY
    }

    /// Whether this body may claim per-instance state words (see
    /// [`BodyCx::claim_state_word`]). `true` only for the region
    /// parent's root body: a CALLEE is reached from arbitrarily many
    /// call sites, and one static word offset can't hold per-call-site
    /// memory — its stateful constructs keep the stateless
    /// approximation. Default `false`.
    fn allow_state(&self) -> bool {
        false
    }

    /// Whether this body may claim REPLAY state words (see
    /// [`BodyCx::claim_state_word_replay`]). `true` only for a REGION
    /// parent: replay words are zeroed by `Kernel::reset_replay`, which
    /// evaluation frames reach through the `FusedKernel` NODE — but a
    /// LAMBDA kernel is also entered by native cross-kernel calls that
    /// bypass the node layer entirely, so a replay word claimed there
    /// can never honor its per-iteration reset contract (jul10h 000009:
    /// element 1's success bridged element 2's div0 across the caller's
    /// native map loop). Such bodies keep the stateless approximation.
    /// Default `false`.
    fn allow_replay_state(&self) -> bool {
        false
    }
}

/// The body emitter — walks the region-root `Node` via `emit_clif`
/// recursion and emits the kernel return. `return_type` comes from
/// the `KernelSig` so the boundary marshalling agrees with the kernel
/// signature / wrapper / runtime arg-pack.
pub(super) struct NodeBodyEmitter<'a, R: Rt, E: UserEvent> {
    pub(super) root: &'a Node<R, E>,
    pub(super) return_type: &'a Type,
    pub(super) apply_sites: &'a nohash::IntMap<ExprId, BuiltinCallSiteInfo>,
    pub(super) lambda_sites: &'a nohash::IntMap<ExprId, LambdaCallInfo>,
    /// `Some` for a self-recursive callee body — see
    /// [`BodyEmitter::self_call`].
    pub(super) self_call: Option<&'a (BindId, LambdaCallInfo)>,
    /// Type-resolution env snapshot — see [`BodyEmitter::type_env`].
    pub(super) type_env: &'a Env,
    /// The compiling `ExecCtx`'s abstract-type registry — see
    /// [`BodyEmitter::registry`].
    pub(super) registry: &'a AbstractRegistry,
    /// DynCall `fn_index` base — see [`BodyEmitter::fn_index_offset`].
    /// `0` for region parents and per-slot callbacks; a callee's
    /// combined-table offset for cross-kernel callee bodies.
    pub(super) fn_index_offset: u32,
    /// STALE-gate the return — see [`BodyEmitter::gate_stale_at_return`].
    /// `true` for the published parent body, `false` for callee bodies.
    pub(super) gate_stale: bool,
    /// Lifted connect-target bind ids — see [`BodyEmitter::lifted`].
    pub(super) lifted: &'a ahash::AHashSet<BindId>,
    /// May this body claim per-instance state words — see
    /// [`BodyEmitter::allow_state`]. `true` for the region parent's
    /// root body, `false` for callee bodies.
    pub(super) allow_state: bool,
    /// May this body claim REPLAY state words — see
    /// [`BodyEmitter::allow_replay_state`]. `true` for region parents
    /// only; `false` for lambda kernels (cross-kernel entry bypasses
    /// the node-level `reset_replay`) and callee bodies.
    pub(super) allow_replay: bool,
}

impl<R: Rt, E: UserEvent> BodyEmitter for NodeBodyEmitter<'_, R, E> {
    fn emit(
        &self,
        b: &mut FunctionBuilder,
        env: &mut JitEnv,
        ctx: &LowerCtx,
    ) -> Result<()> {
        let mut cx = BodyCx { b: &mut *b, env: &mut *env, ctx };
        match self.self_call {
            // Recursive body: tail-position walk — self tail-calls
            // become the rebind-and-jump loop, every other path
            // returns directly. (Non-recursive bodies keep the
            // value-position emission below: per-arm returns would be
            // equivalent codegen but churn every existing kernel.)
            Some(_) => emit_body_tail(&mut cx, self.root, self.return_type),
            None => emit_return_from_node(&mut cx, self.return_type, self.root),
        }
    }

    fn builtin_apply_sites(
        &self,
    ) -> Option<&nohash::IntMap<ExprId, BuiltinCallSiteInfo>> {
        Some(self.apply_sites)
    }

    fn lambda_call_sites(&self) -> Option<&nohash::IntMap<ExprId, LambdaCallInfo>> {
        Some(self.lambda_sites)
    }

    fn self_call(&self) -> Option<&(BindId, LambdaCallInfo)> {
        self.self_call
    }

    fn type_env(&self) -> Option<&Env> {
        Some(self.type_env)
    }

    fn registry(&self) -> &AbstractRegistry {
        self.registry
    }

    fn fn_index_offset(&self) -> u32 {
        self.fn_index_offset
    }

    fn gate_stale_at_return(&self) -> bool {
        self.gate_stale
    }

    fn lifted(&self) -> &ahash::AHashSet<BindId> {
        self.lifted
    }

    fn allow_state(&self) -> bool {
        self.allow_state
    }

    fn allow_replay_state(&self) -> bool {
        self.allow_replay
    }
}

/// The open-kernel emission context handed to
/// [`Update::emit_clif`] / [`crate::Apply::emit_clif`] impls:
/// everything needed to add CLIF to the function currently being
/// defined. Constructed by the kernel-body emitter right after the
/// entry binder installs the params; emission recursion is
/// `child.emit_clif(cx)`. `b` is the raw cranelift builder — full
/// power for novel emitters; the graphix-specific surface (env binds,
/// helper FuncRefs, element reads, taint/pending) stays behind
/// crate-internal relays so `JitEnv`/`LowerCtx` remain private.
pub struct BodyCx<'a, 'f, 'c> {
    pub b: &'a mut FunctionBuilder<'f>,
    pub(crate) env: &'a mut JitEnv,
    pub(crate) ctx: &'a LowerCtx<'c>,
}

impl<'a, 'f, 'c> BodyCx<'a, 'f, 'c> {
    /// The compiling context's abstract-type registry, for the emit-time
    /// type classifiers (`kernel_abi::abi_kind`/`freeze_for_abi`/…). Borrowed
    /// from the [`LowerCtx`], which got it from [`BodyEmitter::registry`].
    /// Returns the `'c`-lifetime borrow (the registry lives in the
    /// `LowerCtx`, not in `self`), so reading it does NOT hold `cx`
    /// borrowed — a reader call can coexist with `&mut cx` in the same
    /// expression.
    pub fn registry(&self) -> &'c AbstractRegistry {
        self.ctx.registry
    }

    /// FuncRef for a registered `emit_helpers` runtime helper.
    pub fn helper(&self, name: &str) -> Result<FuncRef> {
        self.ctx.helper_refs.get(name).ok_or_else(|| anyhow!("missing helper {name}"))
    }

    /// The EFFECTIVE `event.init` flag (`I64`, nonzero on an init
    /// view): the kernel-init word from wire slot 0 (see
    /// [`kernel_abi::CTX_WIRE_SLOTS`]), overridden inside select ARM
    /// bodies with `kernel_init | selection-changed` (see
    /// `LowerCtx::init_override` — the node-walk gives a newly-taken
    /// arm an init view). `emit_const_node` reads it to STALE-gate a
    /// constant: a constant fires only on an init view, then carries a
    /// cached (stale) value.
    pub fn init_flag(&self) -> ClifValue {
        self.ctx.init_override.get().unwrap_or(self.ctx.init_flag)
    }

    /// The per-instance state-buffer pointer (`I64`), loaded from wire
    /// slot 1. Only meaningful at offsets returned by
    /// [`claim_state_word`](Self::claim_state_word); 0 when the kernel
    /// claimed nothing (no claimed offset exists to read through it).
    pub fn state_ptr(&self) -> ClifValue {
        self.ctx.state_ptr
    }

    /// Claim one `u64` of per-kernel-INSTANCE cross-invocation memory,
    /// returning its byte offset from [`state_ptr`](Self::state_ptr) —
    /// or `None` when state is unavailable here: inside a scaffold
    /// loop (one static word can't hold per-slot memory) or in a
    /// callee body (one word can't hold per-call-site memory). Callers
    /// MUST emit their stateless approximation on `None`.
    ///
    /// The buffer is zero-initialized per instance, so consumers store
    /// `value + 1` and read 0 as "no previous observation" — init
    /// semantics fall out of the zeroing. See
    /// `design/kernel_instance_state.md`.
    /// State-buffer byte offset of a LIFTED connect target's
    /// per-instance `BindId` word, if `id` is lifted in this kernel.
    pub fn lifted_state_off(&self, id: BindId) -> Option<i32> {
        self.ctx.lifted_ord.binary_search(&id).ok().map(|i| (i as i32) * 8)
    }

    pub fn claim_state_word(&self) -> Option<i32> {
        if !self.ctx.state_enabled || self.ctx.loop_depth.get() > 0 {
            return None;
        }
        let idx = self.ctx.state_next.get();
        self.ctx.state_next.set(idx + 1);
        Some((idx * 8) as i32)
    }

    /// [`claim_state_word`](Self::claim_state_word) for REPLAY memory —
    /// a cross-invocation cache whose interp twin `reset_replay`
    /// clears. The claimed word is registered on
    /// [`WrappedKernel::replay_state_words`] and zeroed by
    /// `Kernel::reset_replay`, so an evaluation frame's per-iteration
    /// reset severs the cache exactly like the node-walk's.
    pub fn claim_state_word_replay(&self) -> Option<i32> {
        // Replay words are refused wherever their reset contract can't
        // be honored — lambda kernels and callee bodies (see
        // `LowerCtx::replay_enabled`). Claimants fall back stateless.
        if !self.ctx.replay_enabled {
            return None;
        }
        let off = self.claim_state_word()?;
        self.ctx.replay_words.borrow_mut().push((off / 8) as u32);
        Some(off)
    }

    /// [`claim_state_word`](Self::claim_state_word) for a claimant
    /// whose word is exact ACROSS loop iterations, waiving the
    /// in-loop refusal: either the observed quantity is
    /// LOOP-INVARIANT — identical on every iteration of every open
    /// scaffold loop (see [`node_loop_invariant_ref`]) — or the word
    /// is a per-INSTANCE anchor whose per-slot content lives in the
    /// heap structure it owns (a nested slot-table chain,
    /// [`open_slot_tables`](Self::open_slot_tables)). Callee bodies
    /// still refuse (one word can't hold per-call-site memory).
    pub fn claim_state_word_loop_invariant(&self) -> Option<i32> {
        if !self.ctx.state_enabled {
            return None;
        }
        let idx = self.ctx.state_next.get();
        self.ctx.state_next.set(idx + 1);
        Some((idx * 8) as i32)
    }

    /// Open a scaffold loop's per-slot state-table frame. Emitted in
    /// the loop PREHEADER (`len` and `idx_var` in hand, before the
    /// jump into the loop): for each guarded-select site the caller
    /// prewalked out of the loop body ([`guarded_select_sites`]),
    /// anchor a chain of owning tables that mirrors the loop nesting
    /// and ends in a leaf table with one word per slot ordinal:
    ///
    /// - At nesting depth 1 the anchor is a plain
    ///   [`claim_state_word`](Self::claim_state_word) claim and the
    ///   chain is just the leaf.
    /// - Inside enclosing loops the anchor still gets ONE static word
    ///   (a directory word is per-INSTANCE — the per-slot-ness lives
    ///   in the heap structure — so the in-loop static refusal
    ///   doesn't apply; `claim_state_word_loop_invariant` carries
    ///   that exemption), and each enclosing frame contributes one
    ///   DIRECTORY level: `graphix_slot_state_table` ensures that
    ///   level's table (sized by the frame's `len`, resize gated by
    ///   its source taint) and the frame's current ordinal indexes
    ///   into it, yielding the word that owns the next level. The
    ///   node-walk twin is the interpreted slot tree — outer slot i
    ///   owns an inner MapQ instance which owns per-slot selects —
    ///   with u64s in place of subgraphs. Truncation at any level
    ///   frees the dropped subtrees (`own_levels`); regrow re-creates
    ///   fresh — the MapQ prefix-retention lifecycle applied per
    ///   level. This code runs once per enclosing-iteration (it IS
    ///   the nested preheader), so ensure calls follow the loop
    ///   structure's natural cost.
    ///
    /// A TAINTED source at any level skips that level's logical
    /// resize — the node-walk saw no event — growing only as an
    /// in-bounds guard, mirroring [`scaffold::SlotFlags::apply`]'s
    /// prev-len word.
    ///
    /// A frame is ALWAYS pushed (possibly with no tables — claims are
    /// refused in callee bodies, and most loops have no guarded
    /// selects); [`close_slot_tables`](Self::close_slot_tables) MUST
    /// pop it after body emission.
    pub(crate) fn open_slot_tables(
        &mut self,
        sites: &[ExprId],
        len: ClifValue,
        src_disc: ClifValue,
        idx_var: Variable,
    ) -> Result<()> {
        let depth = self.ctx.loop_depth.get() + 1;
        // (len, src_disc, idx_var) of each enclosing loop,
        // outermost first. Every open loop pushed a frame, so the
        // stack IS the enclosing-loop chain.
        let enclosing: Vec<(ClifValue, ClifValue, Variable)> = {
            let frames = self.ctx.slot_tables.borrow();
            debug_assert_eq!(
                frames.len(),
                self.ctx.loop_depth.get() as usize,
                "slot-table frames out of sync with loop depth"
            );
            frames.iter().map(|f| (f.len, f.src_disc, f.idx_var)).collect()
        };
        let n_dirs = enclosing.len();
        let mut tables = Vec::new();
        for id in sites {
            let anchor = if n_dirs == 0 {
                self.claim_state_word()
            } else {
                self.claim_state_word_loop_invariant()
            };
            let entry = match anchor {
                Some(off) => {
                    self.ctx.slot_table_words.borrow_mut().push(kernel_abi::SiteAnchor {
                        rel: (off / 8) as u32,
                        own_levels: n_dirs as u32,
                        leaf: None,
                        reset: false,
                    });
                    let sp = self.state_ptr();
                    let word_addr = self.b.ins().iadd_imm(sp, off as i64);
                    let table =
                        self.emit_slot_chain(word_addr, &enclosing, len, src_disc)?;
                    Some((table, false))
                }
                // A CALLEE body's loop: anchor in the per-call-site
                // block. The base is 0 on a recursive back-edge —
                // branch around the chain (dereferencing the anchor
                // word would fault) and hand the selects a 0 table
                // (the no-memory semantics, [`SelWord::Guarded`]).
                None => match self.claim_site_anchor(n_dirs as u32, None) {
                    Some(off) => {
                        let base = self.site_ptr();
                        let word_addr = self.b.ins().iadd_imm(base, off as i64);
                        let has = self.b.ins().icmp_imm(IntCC::NotEqual, base, 0);
                        let chain_bl = self.b.create_block();
                        let merge = self.b.create_block();
                        self.b.append_block_param(merge, types::I64);
                        let zero = self.b.ins().iconst(types::I64, 0);
                        self.b.ins().brif(
                            has,
                            chain_bl,
                            &[],
                            merge,
                            &[BlockArg::Value(zero)],
                        );
                        self.b.switch_to_block(chain_bl);
                        self.b.seal_block(chain_bl);
                        let table =
                            self.emit_slot_chain(word_addr, &enclosing, len, src_disc)?;
                        self.b.ins().jump(merge, &[BlockArg::Value(table)]);
                        self.b.switch_to_block(merge);
                        self.b.seal_block(merge);
                        let table = self.b.block_params(merge)[0];
                        Some((table, true))
                    }
                    None => None,
                },
            };
            match entry {
                Some((table, guarded)) => tables.push((*id, table, guarded)),
                None => break,
            }
        }
        self.ctx.slot_tables.borrow_mut().push(SlotTableFrame {
            depth,
            idx_var,
            len,
            src_disc,
            tables,
        });
        Ok(())
    }

    /// Emit the owning-table chain from `word_addr` (an anchor word's
    /// address) through one directory level per enclosing frame down
    /// to this loop's LEAF selection table (sized `len`, resize gated
    /// by `src_disc`'s taint). Returns the leaf table base.
    fn emit_slot_chain(
        &mut self,
        word_addr: ClifValue,
        enclosing: &[(ClifValue, ClifValue, Variable)],
        len: ClifValue,
        src_disc: ClifValue,
    ) -> Result<ClifValue> {
        let helper = self.helper("graphix_slot_state_table")?;
        let n_dirs = enclosing.len();
        let no_leaf = self.b.ins().iconst(types::I64, 0);
        let mut word_addr = word_addr;
        for (k, (flen, fdisc, fidx)) in enclosing.iter().enumerate() {
            let fvalid = emit_untainted_i64(self.b, *fdisc);
            let own = self.b.ins().iconst(types::I64, (n_dirs - k) as i64);
            let call =
                self.b.ins().call(helper, &[word_addr, *flen, fvalid, own, no_leaf]);
            let dir = self.b.inst_results(call)[0];
            let i = self.b.use_var(*fidx);
            let o = self.b.ins().ishl_imm(i, 3);
            word_addr = self.b.ins().iadd(dir, o);
        }
        let valid = emit_untainted_i64(self.b, src_disc);
        let own0 = self.b.ins().iconst(types::I64, 0);
        let call = self.b.ins().call(helper, &[word_addr, len, valid, own0, no_leaf]);
        Ok(self.b.inst_results(call)[0])
    }

    /// Pop the frame [`open_slot_tables`](Self::open_slot_tables)
    /// pushed. Every scaffold loop emitter closes right after its
    /// body emission (before `?`-propagating a body error — the
    /// kernel is discarded on `Err`, but the pop keeps the frame
    /// stack honest for sibling loops in the same kernel).
    pub(crate) fn close_slot_tables(&mut self) {
        let popped = self.ctx.slot_tables.borrow_mut().pop();
        debug_assert!(popped.is_some(), "close_slot_tables without an open frame");
    }

    /// The address of THIS slot's per-slot state word for the site at
    /// `id` — a guarded select's selection memory, or a nested
    /// collection loop's prev-length word ([`slot_state_sites`]) —
    /// when the innermost open scaffold loop carries a table for it:
    /// `table + idx * 8`. `None` (fall back to the stateless
    /// approximation) when there is no open loop, the site is emitted
    /// at a different depth than the frame's body (a further nesting
    /// level — the frame's ordinal can't identify its slots), or the
    /// frame claimed no table (no state available).
    pub(crate) fn slot_select_word(&mut self, id: ExprId) -> Option<SelWord> {
        let (idx_var, table, guarded) = {
            let frames = self.ctx.slot_tables.borrow();
            let f = frames.last()?;
            if f.depth != self.ctx.loop_depth.get() {
                return None;
            }
            let (_, table, guarded) = *f.tables.iter().find(|(eid, _, _)| *eid == id)?;
            (f.idx_var, table, guarded)
        };
        let i = self.b.use_var(idx_var);
        let off = self.b.ins().ishl_imm(i, 3);
        let addr = self.b.ins().iadd(table, off);
        Some(if guarded {
            // A callee loop's chain is anchored in the (possibly null)
            // site block: `table` is 0 on a recursive back-edge.
            SelWord::Guarded { base: table, addr }
        } else {
            SelWord::Sure(addr)
        })
    }

    /// PER-SLOT interior-bottom cache storage for an in-loop
    /// [`emit_scalar_taint_cache`] site: a two-word (value, ok) pair
    /// per slot ordinal, in a chain claimed ON DEMAND at the site's
    /// emission (the leaf sized `len * 2`, pair-addressed, prefix
    /// retention at pair granularity — exactly the interp's retained
    /// per-slot node caches across resizes). The anchor is registered
    /// `reset: true`: `Kernel::reset_replay`/`sleep` FREE the chain
    /// (fresh = zero), so a slot's cached success can't bridge an
    /// evaluation frame — the chain twin of `claim_state_word_replay`,
    /// per-slot so it can't alias slots (the jul10h rule that forced
    /// the old in-loop stateless degrade — jul17c katana divergence
    /// 000002, the filter `10/x` witness). Returns the slot's VALUE
    /// word address (the ok word is at +8). `None` in callee bodies
    /// (per-call-site chains would need caller-side reset authority —
    /// the honor problem; documented degrade) and when the innermost
    /// frame isn't this emission's loop.
    pub(crate) fn claim_slot_cache_words(&mut self) -> Option<ClifValue> {
        let (idx_var, len, src_disc, enclosing) = {
            let frames = self.ctx.slot_tables.borrow();
            let f = frames.last()?;
            if f.depth != self.ctx.loop_depth.get() {
                return None;
            }
            let enclosing: Vec<(ClifValue, ClifValue, Variable)> = frames
                [..frames.len() - 1]
                .iter()
                .map(|g| (g.len, g.src_disc, g.idx_var))
                .collect();
            (f.idx_var, f.len, f.src_disc, enclosing)
        };
        let off = self.claim_state_word_loop_invariant()?;
        self.ctx.slot_table_words.borrow_mut().push(kernel_abi::SiteAnchor {
            rel: (off / 8) as u32,
            own_levels: enclosing.len() as u32,
            leaf: None,
            reset: true,
        });
        let sp = self.state_ptr();
        let word_addr = self.b.ins().iadd_imm(sp, off as i64);
        let len2 = self.b.ins().ishl_imm(len, 1);
        let table = self.emit_slot_chain(word_addr, &enclosing, len2, src_disc).ok()?;
        let i = self.b.use_var(idx_var);
        let o = self.b.ins().ishl_imm(i, 4);
        Some(self.b.ins().iadd(table, o))
    }

    /// Claim one word of PER-CALL-SITE block memory (wire slot 2) —
    /// the CALLEE-body twin of [`claim_state_word`]
    /// (Self::claim_state_word): one compiled callee body, per-call-
    /// site storage supplied by each caller ([`SiteLayout`]). Returns
    /// the word's byte offset; `None` outside callee bodies. The
    /// block BASE may be 0 at runtime (recursive back-edge — a fresh
    /// transient activation), so every consumer null-guards
    /// ([`SelWord::Guarded`]).
    pub(crate) fn claim_site_word(&self) -> Option<i32> {
        if !self.ctx.site_enabled {
            return None;
        }
        let idx = self.ctx.site_next.get();
        self.ctx.site_next.set(idx + 1);
        Some((idx * 8) as i32)
    }

    /// [`claim_site_word`](Self::claim_site_word) for REPLAY memory in
    /// a CALLEE body — the site twin of
    /// [`claim_state_word_replay`](Self::claim_state_word_replay).
    /// Returns `(word, header)` byte offsets: the word is recorded on
    /// the [`SiteLayout`] (`replay`) for the caller's reset
    /// registration, and the block-shared HONOR header (claimed with
    /// the first replay word) gates the cache — see
    /// [`LowerCtx::site_replay_hdr`]. Refused inside scaffold loops
    /// (one per-call-site word would alias slots — the jul10h rule).
    pub(crate) fn claim_site_word_replay(&self) -> Option<(i32, i32)> {
        // Also refused in TAIL-LOOP bodies: the caller's cross-cycle
        // reset can't sever iteration i−1's success from iteration i's
        // bottom the way the interp's per-frame `reset_replay` does —
        // the jul10h rule applied across the loop this body IS.
        if self.ctx.loop_depth.get() > 0 || self.ctx.loop_head.is_some() {
            return None;
        }
        let hdr = match self.ctx.site_replay_hdr.get() {
            Some(h) => h,
            None => {
                let h = self.claim_site_word()?;
                self.ctx.site_replay_hdr.set(Some(h));
                h
            }
        };
        let off = self.claim_site_word()?;
        self.ctx.site_replay.borrow_mut().push((off / 8) as u32);
        Some((off, hdr))
    }

    /// [`claim_site_word`](Self::claim_site_word) for a word that
    /// ANCHORS a slot-table chain: registered on the kernel's
    /// [`SiteLayout`] so the block's OWNER (the caller's runtime
    /// `Kernel`) frees the chain.
    pub(crate) fn claim_site_anchor(
        &self,
        own_levels: u32,
        leaf: Option<std::sync::Arc<kernel_abi::SiteLeaf>>,
    ) -> Option<i32> {
        let off = self.claim_site_word()?;
        self.ctx.site_anchors.borrow_mut().push(kernel_abi::SiteAnchor {
            rel: (off / 8) as u32,
            own_levels,
            leaf,
            reset: false,
        });
        Some(off)
    }

    /// The per-call-site block base (`I64`, possibly 0 — see
    /// [`claim_site_word`](Self::claim_site_word)).
    pub(crate) fn site_ptr(&self) -> ClifValue {
        self.ctx.site_ptr
    }

    /// Set the collection-HOF callsite being inline-emitted (see
    /// [`LowerCtx::collection_site`]), returning the previous value
    /// for save/restore around the op emission.
    pub(crate) fn swap_collection_site(&self, id: Option<ExprId>) -> Option<ExprId> {
        self.ctx.collection_site.replace(id)
    }

    /// The collection-HOF callsite currently being inline-emitted.
    pub(crate) fn collection_site(&self) -> Option<ExprId> {
        self.ctx.collection_site.get()
    }

    /// The [`SiteLayout`] of an already-DEFINED callee, by kernel
    /// identity ([`kernel_abi::kernel_key`]). `None` = recursive
    /// back-edge (self-calls, mutual-recursion cycles): the call site
    /// passes 0.
    pub(crate) fn callee_site_layout(&self, key: usize) -> Option<&'c SiteLayout> {
        self.ctx.callee_layouts.get(&key)
    }

    /// Bracket scaffold-loop body emission (INCLUDING the loop's own
    /// element/index/acc binds — their `Local::depth` stamp is what
    /// [`node_loop_invariant_ref`] keys on): state claims are refused
    /// while any loop is open (see [`claim_state_word`](Self::claim_state_word)).
    pub fn enter_loop(&mut self) {
        self.ctx.loop_depth.set(self.ctx.loop_depth.get() + 1);
        self.env.loop_depth += 1;
    }

    pub fn exit_loop(&mut self) {
        let d = self.ctx.loop_depth.get();
        debug_assert!(d > 0, "exit_loop without a matching enter_loop");
        self.ctx.loop_depth.set(d.saturating_sub(1));
        self.env.loop_depth = self.env.loop_depth.saturating_sub(1);
    }

    /// True iff `bind_id` is a LIFTED connect target in this region — a
    /// let-bound scalar counter routed in as a feeder. `emit_let_node`
    /// emits a seed-select for it; `emit_connect_node` allows its write.
    pub(crate) fn is_lifted(&self, bind_id: BindId) -> bool {
        self.ctx.lifted.contains(&bind_id)
    }

    /// Stable `*const ArcStr` for `s` as an `iconst`, interned lazily
    /// at emission (no body prewalk on the direct path — coverage is
    /// exact by construction). The arena entry is individually boxed
    /// so its address survives growth; `define_kernel_body` merges the
    /// arena into the per-kernel [`KernelStrings`] so it outlives the
    /// compiled code that baked the pointer.
    pub fn interned_str(&mut self, s: &ArcStr) -> ClifValue {
        let mut lazy = self.ctx.lazy_strings.borrow_mut();
        let ptr = match lazy.iter().find(|b| b.as_ref() == s) {
            Some(b) => b.as_ref() as *const ArcStr,
            None => {
                lazy.push(Box::new(intern::intern(s)));
                lazy.last().unwrap().as_ref() as *const ArcStr
            }
        };
        self.b.ins().iconst(types::I64, ptr as i64)
    }

    /// The DynCall `fn_index` base for the body being emitted — added to
    /// every DynCall site's `info.fn_index` so a callee body indexes the
    /// region-wide combined `dyn_slots` table. `0` for parents / per-slot
    /// callbacks. See [`BodyEmitter::fn_index_offset`].
    pub(crate) fn fn_index_offset(&self) -> u32 {
        self.ctx.fn_index_offset
    }

    /// The discovered builtin Apply-site info for `id`, if the
    /// region's pre-build discovery pass registered one (direct path
    /// only — `None` whenever no [`BodyEmitter`] supplied the map).
    pub(crate) fn builtin_site(&self, id: ExprId) -> Option<&BuiltinCallSiteInfo> {
        self.ctx.builtin_apply_sites.and_then(|m| m.get(&id))
    }

    /// The discovered lambda call-site info for `id`, if `try_fuse`'s
    /// analysis registered one (direct path only). A `Some` means the
    /// callee kernel is declared in this function's `callee_refs` under
    /// its kernel identity and ready to `call`.
    pub(crate) fn lambda_site(&self, id: ExprId) -> Option<&LambdaCallInfo> {
        self.ctx.lambda_call_sites.and_then(|m| m.get(&id))
    }

    /// The kernel's own self-call descriptor when emitting a
    /// self-recursive lambda body: `(the self binding, the kernel's
    /// own LambdaCallInfo)`. A value-position call site whose fnode
    /// Ref carries the binding calls the kernel's own FuncRef.
    pub(crate) fn self_call_info(&self) -> Option<&(BindId, LambdaCallInfo)> {
        self.ctx.self_call
    }

    /// Stable `*const Value` for a value-shape constant — see
    /// [`Self::interned_str`].
    pub fn interned_value(&mut self, v: &Value) -> ClifValue {
        let mut lazy = self.ctx.lazy_values.borrow_mut();
        let ptr = match lazy.iter().find(|b| b.as_ref() == v) {
            Some(b) => b.as_ref() as *const Value,
            None => {
                lazy.push(Box::new(v.clone()));
                lazy.last().unwrap().as_ref() as *const Value
            }
        };
        self.b.ins().iconst(types::I64, ptr as i64)
    }
}

/// Ownership classification of a Node-rooted result. A
/// binding read is BORROWED (the env slot keeps the ref); grouping and
/// blocks are transparent to their tail; everything else (literals,
/// producers, calls) hands out an owned ref. Used by the direct path's
/// let-bind / block-tail / kernel-return sites to decide whether a
/// clone is needed before the source's scope drops.
pub fn node_composite_source<R: Rt, E: UserEvent>(node: &Node<R, E>) -> CompositeSource {
    use NodeView;
    let mut n: &dyn Update<R, E> = &**node;
    loop {
        match n.view() {
            NodeView::Ref(_) => return CompositeSource::Borrowed,
            NodeView::ExplicitParens(p) => n = &*p.n,
            // A Block's composite/value/string result is OWNED by
            // construction: `emit_block_node`'s tail handling clones a
            // borrowed tail before the scope drops (the tail may alias
            // a dying block local). Walking through to the tail here
            // (the old behavior) contradicted that — every consumer of
            // a block whose tail was a Ref cloned the already-owned
            // result AGAIN and leaked the first clone.
            NodeView::Block(_) => return CompositeSource::Owned,
            _ => return CompositeSource::Owned,
        }
    }
}

/// True iff the node's own type derefs to `Type::Bottom` — a `<-`
/// connect in value position, or a block/select whose value is one. A
/// Bottom node never produces a value and its emission is the shapeless
/// tainted-null placeholder `(NULL|TAINT, 0)`. That is safe wherever
/// the consumer reads the payload by the node's OWN shape (scalar and
/// 2-word Value operands, discarded statements — those classify Bottom
/// as `AbiKind::Unit` and refuse or discard), but a consumer that
/// interprets the payload by an EXTERNAL type (HOF source array, fold
/// init, lambda-call arg — all typed from a resolved signature) would
/// read the 0 payload as a pointer. Bottom unifies with every
/// signature type, so the signature-side shape-agreement checks can't
/// catch it — gate on the node itself and de-fuse; the node-walk runs
/// the subtree and the value simply never fires, which IS the
/// semantics (soak jul08h `fuzz/crash_000000`).
pub fn node_is_bottom<R: Rt, E: UserEvent>(node: &Node<R, E>) -> bool {
    node.typ().with_deref(|t| matches!(t, Some(Type::Bottom)))
}

/// True iff `node` is a plain `Ref` whose binding is LOOP-INVARIANT at
/// this emission point: a kernel input or a local bound outside every
/// open scaffold loop (`Local::depth` 0) — its value is identical on
/// every iteration of every open loop. A nested HOF whose SOURCE
/// passes this test qualifies for the exact (state-word) firing rule
/// via [`SlotFlags::src_invariant`]: one word shared across the
/// enclosing iterations observes one length, so "resized" stays real.
/// Everything else (loop elements, in-loop lets, computed sources) is
/// conservatively variant. Transparent through parens/blocks like
/// [`node_composite_source`].
pub fn node_loop_invariant_ref<R: Rt, E: UserEvent>(
    cx: &BodyCx,
    node: &Node<R, E>,
) -> bool {
    let mut n: &dyn Update<R, E> = &**node;
    loop {
        match n.view() {
            NodeView::Ref(r) => {
                let Some(name) = ref_local_name(n.spec()) else {
                    return false;
                };
                return cx.env.lookup(r.id, name).is_some_and(|l| l.depth == 0);
            }
            NodeView::ExplicitParens(p) => n = &*p.n,
            NodeView::Block(blk) => match blk.children.last() {
                Some(tail) => n = &**tail,
                None => return false,
            },
            _ => return false,
        }
    }
}

/// Clone a borrowed composite pointer so the result is owned; pass an
/// owned one through. `pub` for package crates' `Apply::emit_clif`
/// impls (e.g.
/// `array::flat_map`'s body hand-off).
pub fn ensure_owned_composite_src(
    cx: &mut BodyCx,
    src: CompositeSource,
    v: ClifValue,
) -> Result<ClifValue> {
    match src {
        CompositeSource::Owned => Ok(v),
        CompositeSource::Borrowed => {
            let clone = cx.helper("graphix_valarray_clone")?;
            let call = cx.b.ins().call(clone, &[v]);
            Ok(cx.b.inst_results(call)[0])
        }
    }
}

/// Clone a borrowed two-word Value so the result is owned; pass an
/// owned one through. By-source variant of `ensure_owned_value`.
/// `pub` for package crates' `Apply::emit_clif` impls (e.g.
/// `array::find_map`'s body pair, which escapes its loop as the
/// kernel result).
pub fn ensure_owned_value_src(
    cx: &mut BodyCx,
    src: CompositeSource,
    disc: ClifValue,
    payload: ClifValue,
) -> Result<(ClifValue, ClifValue)> {
    match src {
        CompositeSource::Owned => Ok((disc, payload)),
        CompositeSource::Borrowed => {
            // `graphix_value_clone` takes a `TagValue`: it masks the disc
            // to refcount correctly but PRESERVES the tag, so #219 taint
            // rides straight through the clone — no clean-before /
            // re-propagate-after needed.
            let clone = cx.helper("graphix_value_clone")?;
            let call = cx.b.ins().call(clone, &[disc, payload]);
            let r = cx.b.inst_results(call);
            Ok((r[0], r[1]))
        }
    }
}

/// Get (or lazily create) the kernel's single `pending_exit` block —
/// where every forced-bottom path jumps after dropping the owned set.
/// Its body (sentinel + `return`) is emitted at the end of
/// `compile_into_function`.
pub(super) fn pending_exit_block(cx: &mut BodyCx) -> Block {
    let mut slot = cx.ctx.pending_exit.borrow_mut();
    match *slot {
        Some(blk) => blk,
        None => {
            let blk = cx.b.create_block();
            *slot = Some(blk);
            blk
        }
    }
}

/// Return-path FORCE for a PUBLISHED body (`gate_stale_at_return`): if
/// `disc` is NOT fresh (tainted bottom OR STALE — didn't fire this
/// cycle), drop the owned result (`drop_fn`), set pending, run the
/// owned-set cleanup, and jump `pending_exit`; otherwise fall through —
/// a stale root means "didn't fire — don't publish." On return the
/// builder is positioned in the fresh (fired-with-value) block. Folds
/// to no branch for a fresh const disc. A cross-kernel CALLEE body
/// never forces: its result's not-fresh bits ride back to the caller
/// IN-BAND in the returned disc (unified Value ABI) — a bottomed or
/// unfired callee result rides back as DATA (#219), bottoming the
/// caller only if its taken output path consumes it, matching the
/// node-walk. Genuine aborts (depth trip, interrupt, async pend) keep
/// the pending path.
fn emit_force(
    cx: &mut BodyCx,
    disc: ClifValue,
    drop_fn: impl FnOnce(&mut BodyCx) -> Result<()>,
) -> Result<()> {
    // The return-path firing gate: a not-fresh result (bottom OR
    // didn't-fire-this-cycle) drops the owned result and bottoms the
    // kernel (returns None), matching the node-walk's "publish only on
    // Some". Folds to no branch for a fresh const disc.
    let not_fresh = is_not_fresh(cx.b, disc);
    let pending_set = cx.helper("graphix_dyncall_set_pending")?;
    let pre = cx.b.create_block();
    let ret = cx.b.create_block();
    let exit = pending_exit_block(cx);
    cx.b.ins().brif(not_fresh, pre, &[], ret, &[]);
    cx.b.switch_to_block(pre);
    cx.b.seal_block(pre);
    drop_fn(cx)?;
    cx.b.ins().call(pending_set, &[]);
    emit_pending_cleanup(cx.b, cx.env, cx.ctx)?;
    cx.b.ins().jump(exit, &[]);
    cx.b.switch_to_block(ret);
    cx.b.seal_block(ret);
    Ok(())
}

/// Unconditionally bottom the kernel from the current block: set the
/// pending flag, drop the in-flight owned set, and jump to
/// `pending_exit` (so `Kernel::update` returns `None`). Terminates the
/// block.
pub(super) fn emit_kernel_bottom(cx: &mut BodyCx) -> Result<()> {
    let pending_set = cx.helper("graphix_dyncall_set_pending")?;
    let exit = pending_exit_block(cx);
    cx.b.ins().call(pending_set, &[]);
    emit_pending_cleanup(cx.b, cx.env, cx.ctx)?;
    cx.b.ins().jump(exit, &[]);
    Ok(())
}

/// Emit `node` as the kernel's result and return it under
/// `return_type`'s convention. When the return is VALUE-SHAPE
/// (Variant/Nullable/Value — the in-band 2-word `(disc, payload)`
/// pair) the body node's own shape may differ: a callback declared
/// `fn(x: 'a) -> ['b, null]` whose body is a tuple literal emits the
/// COMPOSITE convention (`payload` = raw ValArray bits, no Value disc),
/// and returning that pair raw hands `TagValue::from_raw` a box
/// pointer as the in-band `ValArray` word — the runtime decode then
/// dereferences garbage (soak jul05 items 5/10/13, SIGSEGV/SIGABRT).
/// Route through `emit_owned_value_operand_node` — the same widening
/// select arms and value operands use — which converts each body
/// shape to a genuine owned Value pair (unified Value ABI: composite
/// and string pairs are already genuine, scalars widen by inline
/// packing, value-shape passes through).
pub(super) fn emit_return_from_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    return_type: &Type,
    node: &Node<R, E>,
) -> Result<()> {
    match kernel_abi::abi_kind(cx.registry(), return_type) {
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let cv = emit_owned_value_operand_node(cx, node)?;
            emit_kernel_return(cx, return_type, cv, CompositeSource::Owned)
        }
        _ => {
            let cv = node.emit_clif(cx)?;
            let src = node_composite_source(node);
            emit_kernel_return(cx, return_type, cv, src)
        }
    }
}

pub(super) fn emit_kernel_return(
    cx: &mut BodyCx,
    return_type: &Type,
    mut cv: CompiledExpr,
    src: CompositeSource,
) -> Result<()> {
    // Apply the tail-selects' control-dependence firing (see
    // `LowerCtx::tail_scrut_stale`): the result fires if its value
    // chain fired OR any tail-select scrutinee on the executed path
    // did. With no tail select the accumulator is the STALE-set
    // identity and this folds to the value's own bit.
    {
        let acc = cx.b.use_var(cx.ctx.tail_scrut_stale);
        let vs = cx.b.ins().band_imm(cv.disc, STALE);
        let folded = cx.b.ins().band(vs, acc);
        let cleaned = cx.b.ins().band_imm(cv.disc, !STALE);
        cv.disc = cx.b.ins().bor(cleaned, folded);
    }
    // Becoming-selected firing for the enclosing GUARDED tail selects
    // (`LowerCtx::tail_sel_path`): this return IS the invocation's
    // final selection for each select on the path — compare each
    // selection word against the taken arm index, fire on change, and
    // record. The scrutinee was forced valid before the arms ran, so
    // the record is unconditional here (the node-walk updates
    // `selected` before the arm body too).
    {
        let path: smallvec::SmallVec<[(SelWord, usize); 4]> =
            cx.ctx.tail_sel_path.borrow().iter().copied().collect();
        for (word, idx) in path {
            let record = |cx: &mut BodyCx, addr: ClifValue| -> ClifValue {
                let stored = cx.b.ins().load(types::I64, MemFlags::trusted(), addr, 0);
                let tag = cx.b.ins().iconst(types::I64, idx as i64 + 1);
                let changed = cx.b.ins().icmp(IntCC::NotEqual, stored, tag);
                cx.b.ins().store(MemFlags::trusted(), tag, addr, 0);
                // STALE when unchanged (the fold identity), 0 on a
                // selection change (forces FIRED).
                let stale_c = cx.b.ins().iconst(types::I64, STALE);
                let zero = cx.b.ins().iconst(types::I64, 0);
                cx.b.ins().select(changed, zero, stale_c)
            };
            let mask = match word {
                SelWord::Sure(addr) => record(cx, addr),
                // Null site block (recursive back-edge): fresh
                // transient semantics — no memory ≡ becoming selected,
                // matching the node-walk's fresh instance
                // (`selected: None` → first evaluation fires).
                SelWord::Guarded { base, addr } => {
                    let has = cx.b.ins().icmp_imm(IntCC::NotEqual, base, 0);
                    let mem_bl = cx.b.create_block();
                    let nomem_bl = cx.b.create_block();
                    let merge = cx.b.create_block();
                    cx.b.append_block_param(merge, types::I64);
                    cx.b.ins().brif(has, mem_bl, &[], nomem_bl, &[]);
                    cx.b.switch_to_block(mem_bl);
                    cx.b.seal_block(mem_bl);
                    let mask = record(cx, addr);
                    cx.b.ins().jump(merge, &[BlockArg::Value(mask)]);
                    cx.b.switch_to_block(nomem_bl);
                    cx.b.seal_block(nomem_bl);
                    let zero = cx.b.ins().iconst(types::I64, 0);
                    cx.b.ins().jump(merge, &[BlockArg::Value(zero)]);
                    cx.b.switch_to_block(merge);
                    cx.b.seal_block(merge);
                    cx.b.block_params(merge)[0]
                }
            };
            let vs = cx.b.ins().band_imm(cv.disc, STALE);
            let folded = cx.b.ins().band(vs, mask);
            let cleaned = cx.b.ins().band_imm(cv.disc, !STALE);
            cv.disc = cx.b.ins().bor(cleaned, folded);
        }
    }
    // Unified Value ABI: every kernel returns the two-word genuine
    // Value pair; TAINT/STALE ride the disc in-band (the old
    // CALLEE_RESULT_FLAGS side channel is gone). The disc is REBASED
    // on the static return shape's Value discriminant before the
    // return — the runtime's single `TagValue::from_raw` decode
    // materializes a `Value` from these exact bits, so the disc must
    // be a valid one-hot discriminant + tag bits by construction.
    match kernel_abi::abi_kind(cx.registry(), return_type) {
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let (disc, payload) = ensure_owned_value_src(cx, src, cv.disc, cv.payload)?;
            if cx.ctx.gate_stale_at_return {
                // FORCE at the output — a not-fresh result (bottom OR
                // didn't fire this cycle) drops the owned value + bottoms
                // the kernel; else fall through. Folds to no branch for a
                // fresh const disc.
                emit_force(cx, disc, |cx| {
                    let val_drop = cx.helper("graphix_value_drop")?;
                    cx.b.ins().call(val_drop, &[disc, payload]);
                    Ok(())
                })?;
            }
            drop_owned_composites(cx.b, cx.env, cx.ctx)?;
            cx.b.ins().return_(&[disc, payload]);
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let bits = ensure_owned_composite_src(cx, src, cv.payload)?;
            if cx.ctx.gate_stale_at_return {
                // #219: a tainted composite output bottoms — drop the
                // owned array on the tainted path (folds when untainted).
                emit_force(cx, cv.disc, |cx| {
                    let d = cx.helper("graphix_valarray_drop")?;
                    cx.b.ins().call(d, &[bits]);
                    Ok(())
                })?;
            }
            drop_owned_composites(cx.b, cx.env, cx.ctx)?;
            let base = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            cx.b.ins().return_(&[disc, bits]);
        }
        Some(AbiKind::String) => {
            // String results are owned at production (reads clone);
            // no ensure needed.
            if cx.ctx.gate_stale_at_return {
                emit_force(cx, cv.disc, |cx| {
                    let d = cx.helper("graphix_arcstr_drop")?;
                    cx.b.ins().call(d, &[cv.payload]);
                    Ok(())
                })?;
            }
            drop_owned_composites(cx.b, cx.env, cx.ctx)?;
            let base = cx.b.ins().iconst(types::I64, value_disc::STRING);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            cx.b.ins().return_(&[disc, cv.payload]);
        }
        Some(AbiKind::Scalar(p)) => {
            if cx.ctx.gate_stale_at_return {
                // FORCE — a not-fresh scalar (bottom OR didn't fire this
                // cycle) bottoms; else return. A scalar owns nothing, so
                // the drop is a no-op. Folds to an unconditional return
                // when the disc is a fresh const.
                emit_force(cx, cv.disc, |_| Ok(()))?;
            }
            drop_owned_composites(cx.b, cx.env, cx.ctx)?;
            // Widen the payload to the Value-encoded word (sign/zero
            // extension, float bitcast — `pack_value_to_u64`'s rules).
            let payload = scalar_to_payload_i64(cx.b, p, cv.payload);
            let base = scalar_disc(cx.b, p);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            cx.b.ins().return_(&[disc, payload]);
        }
        other => {
            return Err(anyhow!(
                "emit_kernel_return: kernel return shape {other:?} not \
                 representable — falls back to node-walk"
            ));
        }
    }
    Ok(())
}

/// The kernel-local name a scalar `Ref` resolves to in [`JitEnv`]. The
/// region's free-var inputs are bound as kernel params under the
/// binding's unqualified name (see `collect_region_inputs` →
/// `FreeVarInput.name` and `compile_into_function`'s entry binder), and
/// block-lets bind under their let name; both are the last component of
/// the Ref's `ModPath`. Mirrors `emit_node`'s Ref arm.
pub(super) fn ref_local_name(spec: &Expr) -> Option<&str> {
    let name = match &spec.kind {
        ExprKind::Ref { name } => name,
        _ => return None,
    };
    let s: &str = name.0.as_ref();
    Some(netidx::path::Path::basename(s).unwrap_or(s))
}
