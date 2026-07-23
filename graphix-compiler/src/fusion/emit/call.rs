//! Call emission: cross-kernel lambda calls (site blocks, arg
//! marshalling, drops, pending cleanup) and the generic builtin
//! `DynCall` path.

use crate::{
    Node, Rt, Update, UserEvent,
    fusion::{
        LambdaCallInfo,
        kernel_abi::{self, AbiKind},
        lowering::{BuiltinCallSiteInfo, CaptureSlot},
    },
    node::callsite::CallSite,
    typ::{FnArgKind, Type},
};
use anyhow::{Result, anyhow};
use cranelift_codegen::ir::{
    BlockArg, InstBuilder, MemFlags, Value as ClifValue, condcodes::IntCC, types,
};
use cranelift_frontend::{FunctionBuilder, Variable};

use super::{
    abi::{
        CompiledExpr, JitEnv, LocalKind, STALE, ValueVar, clean_disc,
        emit_scalar_taint_cache, emit_untainted_i64, is_tainted, propagate_flags,
        scalar_disc, taint_if, value_disc,
    },
    body::{BodyCx, node_composite_source, node_is_bottom, pending_exit_block},
    lower::{LowerCtx, SelWord},
    nodes::{call_result_needs_value_widening, emit_elem_placeholder},
    scalar::{
        cast_u64_to_prim, prim_to_clif, scalar_to_payload_i64, value_buf_push_helper,
    },
};

/// Builtin DynCall — marshal the (marshal-ordered) `args` into a
/// fresh `LPooled<Vec<Value>>` buf, dispatch via `graphix_dyncall`
/// against the `FnSource::Builtin` slot at `info.fn_index`, then
/// decode the return per shape: scalar / unit / string / composite
/// returns the unwrapped value, a Value-shape return passes the
/// (disc, payload) pair through. A dispatch that PENDS (the builtin
/// returned no value this cycle) is converted at this site to a #219
/// tainted shape-safe placeholder that continues — never a
/// whole-kernel abort (item 28).
pub(crate) fn emit_dyncall_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    info: &BuiltinCallSiteInfo,
    args: &[&Node<R, E>],
) -> Result<CompiledExpr> {
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let cap = cx.b.ins().iconst(types::I64, args.len() as i64);
    let call = cx.b.ins().call(buf_new, &[cap]);
    let buf = cx.b.inst_results(call)[0];
    let buf_var = cx.b.declare_var(types::I64);
    cx.b.def_var(buf_var, buf);
    cx.ctx.dyncall_buf_stack.borrow_mut().push(buf_var);
    // #219: each arg's disc — its TAINT bit propagates into a scalar
    // result and force-bottoms a non-scalar result (folds away when no
    // arg is tainted, the fast path).
    let mut arg_taint_discs: Vec<ClifValue> = Vec::with_capacity(args.len());
    for (arg_node, t) in args.iter().zip(info.arg_types.iter()) {
        // Compare by runtime SHAPE (`AbiKind`), not exact `Type` —
        // the direct twin of the lowering-side agreement check. The
        // DynCall marshals by `info.arg_types`, so only the shape
        // needs to agree; a mismatch aborts the kernel (the classic
        // path refuses at lowering — same net effect, the subtree
        // node-walks).
        let Some(frozen) =
            kernel_abi::freeze_for_abi_normalized(cx.registry(), arg_node.typ())
        else {
            return Err(anyhow!(
                "emit_clif: DynCall arg type {:?} doesn't freeze concrete",
                arg_node.typ()
            ));
        };
        if kernel_abi::abi_kind(cx.registry(), &frozen)
            != kernel_abi::abi_kind(cx.registry(), t)
        {
            return Err(anyhow!(
                "emit_clif: DynCall arg shape {:?} disagrees with the \
                 discovered arg type {t:?}",
                frozen
            ));
        }
        // For composite/value args the push helper depends on where the
        // SSA value came from. A `Borrowed` source (a Ref read — the
        // caller still owns it) refcount-bumps; an `Owned` source (a
        // producer op, or a composite/Value-return DynCall result not
        // bound to a local) transfers ownership into the buf. Using the
        // borrowed helper on an Owned source leaks the original; the
        // move helper on a Borrowed source double-frees it.
        let helper_name: &str = match kernel_abi::abi_kind(cx.registry(), t) {
            Some(AbiKind::Scalar(p)) => value_buf_push_helper(p)?,
            Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                match node_composite_source(arg_node) {
                    CompositeSource::Owned => "graphix_value_buf_push_array",
                    CompositeSource::Borrowed => "graphix_value_buf_push_array_borrowed",
                }
            }
            // Variant / Nullable / datetime / duration / bytes / map /
            // error all ride the two-word `(disc, payload)` Value wire
            // shape.
            Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
                match node_composite_source(arg_node) {
                    CompositeSource::Owned => "graphix_value_buf_push_value",
                    CompositeSource::Borrowed => "graphix_value_buf_push_value_borrowed",
                }
            }
            Some(AbiKind::String) => "graphix_value_buf_push_string",
            Some(AbiKind::Unit) => {
                return Err(anyhow!("emit_clif: DynCall arg has Unit type"));
            }
            Some(AbiKind::Null) | None => {
                return Err(anyhow!(
                    "DynCall arg with bare Null / non-fusable type — should \
                     have widened to Nullable<T> at construction"
                ));
            }
        };
        let push = cx.helper(helper_name)?;
        // #219: the helper is pure, so a garbage operand (from a missing
        // input / div0) is harmless — push the (possibly-garbage) value
        // and carry the arg's disc; its taint guards the result at the
        // dyncall site. The pushed disc must be CLEAN (a tainted disc is
        // an invalid tag — storing it builds a corrupt Value the builtin
        // would clone/drop, UB).
        let cv = arg_node.emit_clif(cx)?;
        arg_taint_discs.push(cv.disc);
        if kernel_abi::is_value_shape(cx.registry(), t) {
            cx.b.ins().call(push, &[buf, cv.disc, cv.payload]);
        } else {
            cx.b.ins().call(push, &[buf, cv.payload]);
        }
    }
    let dyncall = cx.helper("graphix_dyncall")?;
    if matches!(
        kernel_abi::abi_kind(cx.registry(), &info.return_type),
        Some(AbiKind::Null) | None
    ) {
        return Err(anyhow!(
            "DynCall with bare Null / non-fusable return — \
             should have widened to Nullable<T> at construction"
        ));
    }
    // Region-wide slot index: local fn_index + this body's base offset
    // into the combined `dyn_slots` table.
    let region_idx = info.fn_index + cx.fn_index_offset();
    let fn_idx_val = cx.b.ins().iconst(types::I32, region_idx as i64);
    // Interior-bottom v3 (Eric's ruling 2026-07-20,
    // dyncall-partial-args-jul2026): a tainted arg is delivered as
    // ABSENCE, not a reason to skip the call — the node-walk's seam is
    // per-slot (a bottomed arg is silence; the builtin's cached slot
    // keeps its previous state) and EVAL decides what a missing arg
    // means (window(#n:0, t, bottomed) legitimately produced [] before
    // the stdlib fix; the interior-bottom-v2 whole-call skip silenced
    // it). Build the per-arg taint MASK the dispatcher gates slot
    // delivery on. Effects stay safe: an all-absent delivery reaches
    // eval only through the builtin's own production rule (CachedArgs
    // runs eval only when a production arrived), so placeholder
    // garbage is never observed. Each `is_tainted` folds to
    // const-false for proven-untainted args, so the mask is const-0 on
    // the hot path.
    if arg_taint_discs.len() > 64 {
        return Err(anyhow!(
            "emit_clif: DynCall with more than 64 args — the taint \
             mask is one word"
        ));
    }
    let mut taint_mask = cx.b.ins().iconst(types::I64, 0);
    for (i, d) in arg_taint_discs.iter().enumerate() {
        let t = is_tainted(cx.b, *d);
        let t64 = cx.b.ins().uextend(types::I64, t);
        let bit = cx.b.ins().ishl_imm(t64, i as i64);
        taint_mask = cx.b.ins().bor(taint_mask, bit);
    }
    // IN-LOOP init-run exactness (katana jul21a, fold-acc-taint-jul2026):
    // one scaffold-loop DynCall site serves EVERY collection position,
    // so its cached slot state crosses position boundaries the
    // node-walk's per-position CallSites never cross — riding it can
    // resurrect a value whose per-position cache would be EMPTY (a
    // fold's broken acc chain re-fired off another position's acc).
    // On an INIT view the per-position caches are provably empty (the
    // positions were just created; a newly-taken select arm's override
    // counts — its interp sites are fresh too), so a masked delivery
    // must not ride ANYTHING: force the mask all-ones — no production
    // reaches eval, the dispatcher pends, and the pend paths below
    // produce the tainted placeholder. Non-init rides keep the ruled
    // mask semantics (a position-invariant arg's ride is correct and
    // load-bearing; cross-position rides on non-init runs remain a
    // known approximation — see design/collection_intrinsics.md).
    if cx.ctx.loop_depth.get() > 0 {
        let any = cx.b.ins().icmp_imm(IntCC::NotEqual, taint_mask, 0);
        let init = cx.init_flag();
        let init_b = cx.b.ins().icmp_imm(IntCC::NotEqual, init, 0);
        let bottom_now = cx.b.ins().band(any, init_b);
        let all_masked = cx.b.ins().iconst(types::I64, -1);
        taint_mask = cx.b.ins().select(bottom_now, all_masked, taint_mask);
    }
    // The result-disc fold must treat a masked (absent) arg as
    // NEUTRAL — it did not fire (STALE) and its bottom must NOT taint
    // a result eval computed over the remaining slots (the interp's
    // gated delivery contributes nothing). Neutralize each tainted
    // disc to a bare STALE for the folds below.
    let neutral_discs: Vec<ClifValue> = arg_taint_discs
        .iter()
        .map(|d| {
            let t = is_tainted(cx.b, *d);
            let stale = cx.b.ins().iconst(types::I64, STALE);
            cx.b.ins().select(t, stale, *d)
        })
        .collect();
    let dmerge = cx.b.create_block();
    let pay_ty = match kernel_abi::abi_kind(cx.registry(), &info.return_type) {
        Some(AbiKind::Scalar(p)) => prim_to_clif(p),
        _ => types::I64,
    };
    cx.b.append_block_param(dmerge, types::I64);
    cx.b.append_block_param(dmerge, pay_ty);
    // The buf's in-flight cover ends here: the dispatcher consumes it.
    cx.ctx.dyncall_buf_stack.borrow_mut().pop();
    let call = cx.b.ins().call(dyncall, &[fn_idx_val, buf, taint_mask]);
    // Unified Value ABI: `graphix_dyncall` returns the result's
    // genuine (disc, payload) Value pair for every return type; the
    // decode below adapts per the static shape.
    let (raw0, raw1) = {
        let r = cx.b.inst_results(call);
        (r[0], r[1])
    };
    // Take AND clear the pending flag: set means THIS dispatch
    // returned no value ("bottom this cycle" — e.g. `buffer::encode`'s
    // Pad guard). Converted HERE to a #219 tainted result that
    // CONTINUES — the node-walk's builtin `None` silences only its
    // consumers, so the whole-kernel pending abort it used to trigger
    // killed unrelated outputs (soak jul05 item 28). Clearing keeps
    // the flag meaning "genuine abort" for `Kernel::update`.
    let pend = {
        let take = cx.helper("graphix_dyncall_pending_take_clear")?;
        let c = cx.b.ins().call(take, &[]);
        cx.b.inst_results(c)[0]
    };
    // Results per return shape. Tainted args were delivered as
    // ABSENCE via the mask (their discs fold as neutral STALE), so a
    // result computed over the remaining slots keeps its honest tag.
    match kernel_abi::abi_kind(cx.registry(), &info.return_type) {
        Some(AbiKind::Scalar(p)) => {
            // Scalar return: the payload word carries the Value-encoded
            // scalar bits — narrow to the prim. The 0 sentinel on
            // pending is a harmless payload for downstream scalar
            // arithmetic — no branch, just fold the pend bit into TAINT.
            let value = cast_u64_to_prim(cx.b, raw1, p);
            let base = scalar_disc(cx.b, p);
            let disc = propagate_flags(cx.b, base, &neutral_discs);
            let disc = taint_if(cx.b, disc, pend);
            cx.b.ins().jump(dmerge, &[BlockArg::Value(disc), BlockArg::Value(value)]);
        }
        Some(AbiKind::Unit) => {
            // Unit return: dispatcher returned the Null pair. The
            // result is discarded by the statement position; the pend
            // bit still rides so a bound unit local reads as bottom.
            let disc = cx.b.ins().iconst(types::I64, value_disc::NULL);
            let disc = taint_if(cx.b, disc, pend);
            cx.b.ins().jump(dmerge, &[BlockArg::Value(disc), BlockArg::Value(raw1)]);
        }
        Some(
            AbiKind::String
            | AbiKind::Array
            | AbiKind::Tuple
            | AbiKind::Struct
            | AbiKind::Variant
            | AbiKind::Nullable
            | AbiKind::Value,
        ) => {
            // Pointer-carrying / two-word returns: the pending
            // sentinel `(0, 0)` is NOT inert — every String position
            // assumes a valid owned ArcStr (#214), zero ValArray bits
            // can't be touched, and a zero Value disc is not a real
            // tag. Branch: on pend, produce a tainted shape-safe
            // placeholder and continue to the merge. For String/composite
            // returns the returned DISC is additionally checked
            // against the expected shape — a dispatcher that returned
            // the wrong Value shape is a compiler bug (the old
            // ret_kind protocol logged + pended in the dispatcher);
            // adopting its payload as ArcStr/ValArray bits would be
            // UB, so a mismatch takes the placeholder path too.
            let pend_bl = cx.b.create_block();
            let ok_bl = cx.b.create_block();
            let ret_abi = kernel_abi::abi_kind(cx.registry(), &info.return_type);
            let expected_disc: Option<i64> = match ret_abi {
                Some(AbiKind::String) => Some(value_disc::STRING),
                Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                    Some(value_disc::ARRAY)
                }
                _ => None,
            };
            match expected_disc {
                Some(exp) => {
                    let clean0 = clean_disc(cx.b, raw0);
                    let mismatch = cx.b.ins().icmp_imm(IntCC::NotEqual, clean0, exp);
                    let bad = cx.b.ins().bor(pend, mismatch);
                    cx.b.ins().brif(bad, pend_bl, &[], ok_bl, &[]);
                }
                None => {
                    cx.b.ins().brif(pend, pend_bl, &[], ok_bl, &[]);
                }
            }
            cx.b.switch_to_block(pend_bl);
            cx.b.seal_block(pend_bl);
            // A shape-mismatched (non-pend) result still OWNS the
            // returned Value — drop it before the placeholder so the
            // compiler-bug path leaks nothing. On the pend path the
            // pair is the inert (0, 0) sentinel, which
            // `graphix_value_drop` rejects — guard on a nonzero disc.
            {
                let nz = cx.b.ins().icmp_imm(IntCC::NotEqual, raw0, 0);
                let drop_bl = cx.b.create_block();
                let cont_bl = cx.b.create_block();
                cx.b.ins().brif(nz, drop_bl, &[], cont_bl, &[]);
                cx.b.switch_to_block(drop_bl);
                cx.b.seal_block(drop_bl);
                let val_drop = cx.helper("graphix_value_drop")?;
                cx.b.ins().call(val_drop, &[raw0, raw1]);
                cx.b.ins().jump(cont_bl, &[]);
                cx.b.switch_to_block(cont_bl);
                cx.b.seal_block(cont_bl);
            }
            let ph = emit_elem_placeholder(cx, &info.return_type)?;
            let ph_disc = propagate_flags(cx.b, ph.disc, &neutral_discs);
            cx.b.ins()
                .jump(dmerge, &[BlockArg::Value(ph_disc), BlockArg::Value(ph.payload)]);
            cx.b.switch_to_block(ok_bl);
            cx.b.seal_block(ok_bl);
            let (disc, pay) = match ret_abi {
                Some(AbiKind::String) => {
                    // `raw1` is the ArcStr's bits (ownership
                    // transferred from the dispatcher).
                    let base = cx.b.ins().iconst(types::I64, value_disc::STRING);
                    (propagate_flags(cx.b, base, &neutral_discs), raw1)
                }
                Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                    // `raw1` is owned ValArray bits.
                    let base = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
                    (propagate_flags(cx.b, base, &neutral_discs), raw1)
                }
                _ => {
                    // Value-shape: both register-words. `raw0` is a
                    // REAL (flag-free) Value disc from the dispatcher —
                    // fold the args' firing in like every other arm,
                    // else the result reads as always-fired: a const
                    // `buffer::from_string(...)`/`cast<T>(x)$` re-fired
                    // on every kernel wake, and a consume-always
                    // connect of one SELF-WOKE its own kernel forever
                    // (soak jul05 items 1/4/25 — the ExtraFire/Timeout
                    // family).
                    (propagate_flags(cx.b, raw0, &neutral_discs), raw1)
                }
            };
            cx.b.ins().jump(dmerge, &[BlockArg::Value(disc), BlockArg::Value(pay)]);
        }
        Some(AbiKind::Null) | None => {
            return Err(anyhow!(
                "DynCall with bare Null / non-fusable return — \
                 should have widened to Nullable<T> at construction"
            ));
        }
    }
    cx.b.switch_to_block(dmerge);
    cx.b.seal_block(dmerge);
    let params = cx.b.block_params(dmerge);
    let merged = CompiledExpr::new(params[0], params[1]);
    // Interior-bottom exactness (scalar returns): a pended dispatch, a
    // tainted-arg skip, with PRIOR success degrades to STALE + the
    // cached value — matching the node-walk, where consumers sample
    // the builtin node's cached last output when it doesn't fire.
    match kernel_abi::abi_kind(cx.registry(), &info.return_type) {
        Some(AbiKind::Scalar(p)) => Ok(emit_scalar_taint_cache(cx, p, merged)),
        _ => Ok(merged),
    }
}

/// Where a composite expression's pointer came from. Drives whether
/// a tail-call rebind needs a refcount bump (`Borrowed`) or can
/// transfer ownership directly (`Owned`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompositeSource {
    /// Expression produces a fresh owned pointer — TupleNew,
    /// StructNew, ArrayInit, etc. Transfer to the slot as-is.
    Owned,
    /// Expression reads from an existing binding that already owns
    /// the pointer (typically `Local(name)`). If we move it into a
    /// slot whose old contents we then drop, the drop frees the
    /// shared underlying buffer. Caller must clone before transfer.
    Borrowed,
}

/// One owned composite/value cross-kernel-call arg that must be
/// dropped after the call returns. A cross-kernel call passes its
/// args borrowed — the callee clones every composite/value param on entry
/// (`compile_into_function`). An Owned-source arg (a producer like
/// `TupleNew`, or a composite-return `Call`) therefore leaves the
/// caller still holding the original after the callee took its own
/// clone; without this drop the original leaks.
enum CallArgDrop {
    Composite(ClifValue),
    String(ClifValue),
    Value { disc: ClifValue, payload: ClifValue },
}

/// One entry in the flat formals+captures list
/// [`emit_lambda_call_node`] marshals — either a call-site arg Node or
/// a closure-converted capture resolved from the calling kernel's env.
enum LambdaCallSlot<'a, R: Rt, E: UserEvent> {
    Arg(&'a Node<R, E>, Type),
    Cap(&'a CaptureSlot),
}

impl<R: Rt, E: UserEvent> LambdaCallSlot<'_, R, E> {
    fn typ(&self) -> &Type {
        match self {
            LambdaCallSlot::Arg(_, t) => t,
            LambdaCallSlot::Cap(c) => &c.typ,
        }
    }
}

/// Cross-kernel lambda call. The flat formals+captures list is
/// assembled in the callee's signature order — formal args in FnType
/// parameter order, then captures in `CaptureSlot` order — which IS
/// the ABI order (see [`KernelSig::abi_params`]); each slot marshals
/// as a two-word `(disc, payload)` Value pair, so a may-bottom arg
/// forwards its `TAINT` to the callee (which bottoms if it consumes
/// it) — no de-fuse. Owned composite/string/value ARGS are dropped
/// after the call (the callee clones every param on entry); captures
/// are env READS (borrowed) and never drop. Captures resolve
/// BindId-first (a capture whose id misses Errs and the region
/// de-fuses — never a silent wrong binding). The result is the
/// callee's two-word return pair, TAINT/STALE in-band in the disc — a
/// bottomed or unfired callee RESULT rides back as data (#219),
/// bottoming only consumers that read it, exactly like a node-walk
/// callsite whose output didn't fire. Genuine aborts (depth trip,
/// interrupt, async pend) still ride `DYNCALL_PENDING` and bottom the
/// whole caller kernel.
/// The PER-CALL-SITE state block argument for a cross-kernel call
/// (wire slot 2): storage for the callee's instance memory (select
/// selection memory, loop-table anchors), owned by THIS caller and
/// sized by the callee's recorded [`SiteLayout`]. Four shapes:
///
/// - Callee not yet defined (recursive back-edge — self-calls,
///   mutual-recursion cycles) or claims nothing → `0`; the callee's
///   null-guards degrade to the no-memory semantics, which for a
///   single-shot transient activation is exactly the node-walk's
///   fresh per-activation instance.
/// - Root call site → a contiguous run of words in this body's own
///   space (instance words in a parent — the callee's anchors
///   translate into `slot_table_words` so the existing Drop frees its
///   chains; site words in a callee, base null-guarded).
/// - In-loop call site → one block per slot coordinate: the leaf of
///   an owning chain over ALL open frames, `words` stride per slot —
///   the node-walk twin of each slot's CallSite owning its own Apply.
///   Plain leaf when the callee has no anchors; a
///   [`kernel_abi::SiteLeaf`]-described block leaf otherwise (the
///   resize helper and Drop free through it, recursively).
fn emit_site_block(cx: &mut BodyCx, info: &LambdaCallInfo) -> Result<ClifValue> {
    let key = kernel_abi::kernel_key(&info.kernel);
    let layout = match cx.callee_site_layout(key) {
        None => return Ok(cx.b.ins().iconst(types::I64, 0)),
        Some(l) => l.clone(),
    };
    if layout.words == 0 {
        return Ok(cx.b.ins().iconst(types::I64, 0));
    }
    if cx.ctx.loop_depth.get() == 0 {
        if let Some(first) = cx.claim_state_word() {
            for _ in 1..layout.words {
                cx.claim_state_word()
                    .expect("contiguous instance claims can't fail mid-run");
            }
            let base_idx = (first / 8) as u32;
            for a in layout.anchors.iter() {
                cx.ctx.state.anchors.borrow_mut().push(kernel_abi::SiteAnchor {
                    rel: base_idx + a.rel,
                    own_levels: a.own_levels,
                    leaf: a.leaf.clone(),
                    reset: a.reset,
                });
            }
            let sp = cx.state_ptr();
            // Activate the callee's interior-bottom taint caches iff
            // THIS body's replay reset contract holds (a region —
            // `Kernel::reset_replay` zeroes `replay_words`): register
            // the block's replay-kind words and store 1 to the honor
            // header. A parent whose resets don't run (a per-slot
            // collection-callback kernel: `allow_replay` false) leaves
            // the header 0 — the caches stay inert.
            if cx.ctx.replay_enabled
                && let Some(h) = layout.replay_hdr
            {
                for o in layout.replay.iter() {
                    cx.ctx.state.replay.borrow_mut().push(base_idx + o);
                }
                let one = cx.b.ins().iconst(types::I64, 1);
                cx.b.ins().store(
                    MemFlags::trusted(),
                    one,
                    sp,
                    (first as i64 + (h as i64) * 8) as i32,
                );
            }
            return Ok(cx.b.ins().iadd_imm(sp, first as i64));
        }
        if let Some(first) = cx.claim_site_word() {
            for _ in 1..layout.words {
                cx.claim_site_word().expect("contiguous site claims can't fail mid-run");
            }
            let base_idx = (first / 8) as u32;
            for a in layout.anchors.iter() {
                cx.ctx.site.anchors.borrow_mut().push(kernel_abi::SiteAnchor {
                    rel: base_idx + a.rel,
                    own_levels: a.own_levels,
                    leaf: a.leaf.clone(),
                    reset: a.reset,
                });
            }
            // Our own block may be 0 (a back-edge activation of THIS
            // callee) — forward 0, not a garbage offset.
            let base = cx.site_ptr();
            let addr = cx.b.ins().iadd_imm(base, first as i64);
            let has = cx.b.ins().icmp_imm(IntCC::NotEqual, base, 0);
            let zero = cx.b.ins().iconst(types::I64, 0);
            return Ok(cx.b.ins().select(has, addr, zero));
        }
        return Ok(cx.b.ins().iconst(types::I64, 0));
    }
    // In-loop call site. The chain runs per innermost iteration (this
    // IS the loop body) — the ensures are idempotent after the first.
    let frames: Vec<(ClifValue, ClifValue, Variable)> = {
        let fs = cx.ctx.slot_tables.borrow();
        debug_assert_eq!(
            fs.len(),
            cx.ctx.loop_depth.get() as usize,
            "slot-table frames out of sync with loop depth"
        );
        fs.iter().map(|f| (f.len, f.src_disc, f.idx_var)).collect()
    };
    let n_dirs = frames.len() - 1;
    let (dirs, leaf_frame) = (&frames[..n_dirs], frames[n_dirs]);
    let leaf_rt = if layout.anchors.is_empty() {
        None
    } else {
        let l = std::sync::Arc::new(kernel_abi::SiteLeaf {
            stride: layout.words,
            anchors: layout.anchors.clone(),
        });
        cx.ctx.lazy_site_leaves.borrow_mut().push(l.clone());
        Some(l)
    };
    let anchor = match cx.claim_state_word_loop_invariant() {
        Some(off) => {
            cx.ctx.state.anchors.borrow_mut().push(kernel_abi::SiteAnchor {
                rel: (off / 8) as u32,
                own_levels: n_dirs as u32,
                leaf: leaf_rt.clone(),
                reset: false,
            });
            let sp = cx.state_ptr();
            SelWord::Sure(cx.b.ins().iadd_imm(sp, off as i64))
        }
        None => match cx.claim_site_anchor(n_dirs as u32, leaf_rt.clone()) {
            Some(off) => {
                let base = cx.site_ptr();
                let addr = cx.b.ins().iadd_imm(base, off as i64);
                SelWord::Guarded { base, addr }
            }
            None => return Ok(cx.b.ins().iconst(types::I64, 0)),
        },
    };
    let emit_chain = |cx: &mut BodyCx, word_addr: ClifValue| -> Result<ClifValue> {
        let leaf_ptr = match &leaf_rt {
            None => cx.b.ins().iconst(types::I64, 0),
            Some(l) => {
                cx.b.ins()
                    .iconst(types::I64, std::sync::Arc::as_ptr(l) as *const u8 as i64)
            }
        };
        let table_helper = cx.helper("graphix_slot_state_table")?;
        let mut word_addr = word_addr;
        for (k, (flen, fdisc, fidx)) in dirs.iter().enumerate() {
            let fvalid = emit_untainted_i64(cx.b, *fdisc);
            let own = cx.b.ins().iconst(types::I64, (n_dirs - k) as i64);
            let call =
                cx.b.ins().call(table_helper, &[word_addr, *flen, fvalid, own, leaf_ptr]);
            let dir = cx.b.inst_results(call)[0];
            let i = cx.b.use_var(*fidx);
            let o = cx.b.ins().ishl_imm(i, 3);
            word_addr = cx.b.ins().iadd(dir, o);
        }
        let (llen, ldisc, lidx) = leaf_frame;
        let lvalid = emit_untainted_i64(cx.b, ldisc);
        let table = match &leaf_rt {
            // No in-block anchors: a plain table of slots*stride words.
            None => {
                let stride = cx.b.ins().iconst(types::I64, layout.words as i64);
                let words = cx.b.ins().imul(llen, stride);
                let own0 = cx.b.ins().iconst(types::I64, 0);
                let call =
                    cx.b.ins()
                        .call(table_helper, &[word_addr, words, lvalid, own0, leaf_ptr]);
                cx.b.inst_results(call)[0]
            }
            Some(_) => {
                let blocks_helper = cx.helper("graphix_slot_state_blocks")?;
                let call =
                    cx.b.ins().call(blocks_helper, &[word_addr, llen, lvalid, leaf_ptr]);
                cx.b.inst_results(call)[0]
            }
        };
        let i = cx.b.use_var(lidx);
        let stride_bytes = cx.b.ins().imul_imm(i, (layout.words as i64) * 8);
        Ok(cx.b.ins().iadd(table, stride_bytes))
    };
    match anchor {
        SelWord::Sure(word_addr) => emit_chain(cx, word_addr),
        SelWord::Guarded { base, addr } => {
            let has = cx.b.ins().icmp_imm(IntCC::NotEqual, base, 0);
            let chain_bl = cx.b.create_block();
            let merge = cx.b.create_block();
            cx.b.append_block_param(merge, types::I64);
            let zero = cx.b.ins().iconst(types::I64, 0);
            cx.b.ins().brif(has, chain_bl, &[], merge, &[BlockArg::Value(zero)]);
            cx.b.switch_to_block(chain_bl);
            cx.b.seal_block(chain_bl);
            let block = emit_chain(cx, addr)?;
            cx.b.ins().jump(merge, &[BlockArg::Value(block)]);
            cx.b.switch_to_block(merge);
            cx.b.seal_block(merge);
            Ok(cx.b.block_params(merge)[0])
        }
    }
}

pub(crate) fn emit_lambda_call_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    cs: &CallSite<R, E>,
    info: &LambdaCallInfo,
) -> Result<CompiledExpr> {
    let fn_name = &info.fn_name;
    // Hoist the registry borrow (a `'c` ref independent of `cx`) so the
    // slot-grouping closures below capture IT, not `cx` — otherwise the
    // closures would hold `cx` shared while the per-slot emit needs
    // `&mut cx`.
    let reg = cx.registry();
    let ftype = cs
        .resolved_ftype()
        .or_else(|| cs.ftype())
        .ok_or_else(|| anyhow!("lambda call `{fn_name}`: no resolved FnType"))?;
    // Formal args in FnType parameter order — the order
    // `build_lambda_kernel` translated them into kernel inputs. Each
    // slot is typed from the CALLEE's signature (`info.arg_types`,
    // formals first, captures appended): those types were resolved
    // (`resolve_abstract` — Refs to hidden-rep abstract type names
    // expanded to the registered concrete rep) and frozen at build
    // time. Freezing the caller-side node type here instead would
    // re-reject exactly those abstract Refs (#218), and env isn't
    // available at emit time to resolve them — the classic caller
    // (`emit_lambda_call`) types args from the signature the same
    // way. A node whose actual emission shape disagrees with the
    // signature type Errs in the per-slot extractors below (build
    // time, de-fuse).
    let n_formal =
        info.arg_types.len().checked_sub(info.captures.len()).ok_or_else(|| {
            anyhow!(
                "lambda call `{fn_name}`: signature has fewer inputs than \
                 captures — discovery drift"
            )
        })?;
    if ftype.args.len() != n_formal {
        return Err(anyhow!(
            "lambda call `{fn_name}`: call-site FnType has {} formals, \
             kernel signature has {n_formal} — de-fuse",
            ftype.args.len()
        ));
    }
    let mut slots: Vec<LambdaCallSlot<R, E>> =
        Vec::with_capacity(ftype.args.len() + info.captures.len());
    let mut pos = 0usize;
    for (i, fa) in ftype.args.iter().enumerate() {
        let node = match &fa.kind {
            FnArgKind::Positional { .. } => {
                let n = cs.arg_positional(pos);
                pos += 1;
                n
            }
            FnArgKind::Labeled { name, .. } => cs.arg_named(name),
        }
        .ok_or_else(|| anyhow!("lambda call `{fn_name}`: missing call-site arg node"))?;
        slots.push(LambdaCallSlot::Arg(node, info.arg_types[i].clone()));
    }
    for cap in &info.captures {
        slots.push(LambdaCallSlot::Cap(cap));
    }
    // Shape gate. Under the unified Value ABI every data shape
    // marshals (String and bare-Value args included — the old
    // asymmetry gate is gone); only Unit / bare-Null / unfrozen
    // shapes refuse. The slot TYPES come from the callee's signature
    // (see above), so a Bottom-typed arg NODE slips through them
    // (Bottom unifies with any signature type) — gate on the node
    // itself, the caller-side twin of the DynCall shape-agreement
    // check ([`node_is_bottom`]).
    for s in &slots {
        if let LambdaCallSlot::Arg(n, _) = s {
            if node_is_bottom(n) {
                return Err(anyhow!(
                    "lambda call `{fn_name}`: Bottom-typed arg in value \
                     position — subtree node-walks"
                ));
            }
        }
        match kernel_abi::abi_kind(reg, s.typ()) {
            Some(
                AbiKind::Scalar(_)
                | AbiKind::Array
                | AbiKind::Tuple
                | AbiKind::Struct
                | AbiKind::String
                | AbiKind::Variant
                | AbiKind::Nullable
                | AbiKind::Value,
            ) => {}
            _ => {
                return Err(anyhow!(
                    "lambda call `{fn_name}`: arg/capture type {:?} not \
                     lowered on the calling side — subtree node-walks",
                    s.typ()
                ));
            }
        }
    }
    // Emit one slot to a two-register Value. An `Arg` compiles its node;
    // a `Cap` reads the capture from the calling kernel's env (disc
    // carries any #219 taint, forwarded to the callee — which bottoms if
    // it consumes it).
    let emit_slot = |cx: &mut BodyCx, s: &LambdaCallSlot<R, E>| -> Result<CompiledExpr> {
        match s {
            LambdaCallSlot::Arg(n, _) => n.emit_clif(cx),
            LambdaCallSlot::Cap(c) => {
                let vv = {
                    let l =
                        cx.env.lookup(c.bind_id, c.name.as_str()).ok_or_else(|| {
                            anyhow!(
                                "lambda call `{fn_name}`: capture `{}` not in the \
                             calling kernel's env",
                                c.name
                            )
                        })?;
                    l.vv
                };
                Ok(CompiledExpr::new(cx.b.use_var(vv.disc), cx.b.use_var(vv.payload)))
            }
        }
    };
    let mut clif_args: Vec<ClifValue> = Vec::with_capacity(slots.len() * 2 + 1);
    let mut drops: Vec<CallArgDrop> = Vec::new();
    let ret = &info.kernel.return_type;
    // The callsite NODE's type may promise a 2-word Value where the
    // callee ABI returns its own narrower shape (see
    // `widen_result_to_value`) — the call path widens its result, and
    // the trip placeholder is emitted per the NODE type so both merge
    // edges carry the Value pairing.
    let node_typ = cs.typ();
    let widen = call_result_needs_value_widening(reg, node_typ, ret);
    let ret_pay_ty = match kernel_abi::abi_kind(reg, ret) {
        Some(AbiKind::Scalar(p)) if !widen => prim_to_clif(p),
        _ => types::I64,
    };
    // Leading cycle-context words: forward THIS kernel's `event.init`
    // (the callee's constants fire when this region inits) and state
    // pointer — every kernel signature carries the leading context
    // slots (`push_abi_params`), so a cross-kernel call must pass them
    // or the call mismatches the sig. The state pointer is forwarded
    // for uniformity only: a callee body never CLAIMS words
    // (`BodyEmitter::allow_state` is false for callees), so it never
    // reads through it.
    // The callee's init view: the node-walk's `Callee::Static`
    // primes an instance's FIRST dispatch with a forced init view
    // (`first_update`), so a late first call — e.g. a fold callback
    // whose loop had zero elements until a lifted source grew — still
    // fires its consts/cached reads once. Mirror it with a per-call-
    // site state word: force the callee's init flag on the first call
    // ever, then never again (the word is call-site-shared across
    // loop iterations, exactly like the shared instance). No word
    // available (a callee body) → the plain kernel init flag.
    let callee_init = match cx.claim_state_word_loop_invariant() {
        Some(off) => {
            let sp = cx.state_ptr();
            let stored = cx.b.ins().load(types::I64, MemFlags::trusted(), sp, off);
            let first = cx.b.ins().icmp_imm(IntCC::Equal, stored, 0);
            let one = cx.b.ins().iconst(types::I64, 1);
            cx.b.ins().store(MemFlags::trusted(), one, sp, off);
            let init = cx.init_flag();
            let first_i = cx.b.ins().uextend(types::I64, first);
            cx.b.ins().bor(init, first_i)
        }
        None => cx.init_flag(),
    };
    clif_args.push(callee_init);
    clif_args.push(cx.state_ptr());
    // Wire slot 2: the callee's per-call-site state block, from THIS
    // caller's storage (see `emit_site_block`).
    let site_block = emit_site_block(cx, info)?;
    clif_args.push(site_block);
    // Marshal in `abi_params` order — which IS the callee signature's
    // SOURCE order (formals in ftype order, then captures — exactly
    // how `build_lambda_kernel` constructed `sig.params`), two words
    // (disc, payload) each. An Owned composite/string/value Arg is
    // dropped after the call (the callee refcount-bumps on entry).
    for s in slots.iter() {
        // Under the unified Value ABI a composite/string arg's pair IS
        // a genuine Value, so a value-shaped slot (typed from the
        // SIGNATURE) fed a narrower union member needs no wrapping —
        // the jul17a normalization survives only for SCALAR args,
        // whose payload word must WIDEN to the Value encoding
        // (sign/zero extension, float bitcast).
        let scalar_widen = match s {
            LambdaCallSlot::Arg(n, _)
                if matches!(
                    kernel_abi::abi_kind(reg, s.typ()),
                    Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value)
                ) =>
            {
                match kernel_abi::abi_kind(reg, n.typ()) {
                    Some(AbiKind::Scalar(p)) => Some(p),
                    _ => None,
                }
            }
            _ => None,
        };
        let cv = {
            let cv = emit_slot(cx, s)?;
            match scalar_widen {
                Some(p) => {
                    let payload = scalar_to_payload_i64(cx.b, p, cv.payload);
                    CompiledExpr::new(cv.disc, payload)
                }
                None => cv,
            }
        };
        if let LambdaCallSlot::Arg(n, _) = s {
            match kernel_abi::abi_kind(reg, s.typ()) {
                // String ARG emissions are ALWAYS owned (String local
                // reads clone at the read; producers are owned) — drop
                // unconditionally. Cap string reads stay borrowed
                // views of the env slot and are never dropped.
                Some(AbiKind::String) => {
                    drops.push(CallArgDrop::String(cv.payload));
                }
                _ if node_composite_source(n) == CompositeSource::Owned => {
                    match kernel_abi::abi_kind(reg, s.typ()) {
                        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                            drops.push(CallArgDrop::Composite(cv.payload));
                        }
                        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
                            // Scalar-widened args own nothing — the drop
                            // is only for genuinely refcounted pairs.
                            if scalar_widen.is_none() {
                                drops.push(CallArgDrop::Value {
                                    disc: cv.disc,
                                    payload: cv.payload,
                                });
                            }
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }
        clif_args.push(cv.disc);
        clif_args.push(cv.payload);
    }
    // The call-depth guard (Phase 4): every non-tail lambda dispatch —
    // cross-kernel calls AND value-position self-calls (native
    // recursion) — enters one unit against the SHARED
    // `Control::depth_push` counter the node-walk's `GXLambda::update`
    // pushes, so both evaluators bottom at the same logical depth and
    // an impure program's interleaved frames are bounded together. At
    // the limit the CALL bottoms LOCALLY: skip the dispatch and
    // continue with a #219 tainted, shape-safe placeholder — the same
    // observable as the node-walk's guarded dispatch yielding nothing
    // (its bottom silences only the call's consumers; a fold whose
    // callback ignores the tripped value RECOVERS — the former
    // whole-kernel abort here bottomed unrelated outputs, soak jul07h).
    // The check runs AFTER argument marshalling: the unit must not be
    // held while the args evaluate, because an arg containing a
    // further self-call (`f(n - g(f(n - 1)))`) then charges TWO units
    // per recursion level where the node-walk — whose CallSite
    // evaluates args before `GXLambda::update` pushes — charges one,
    // tripping the kernel at HALF the interp's depth (jul22a
    // divergence, trip at n=128 of a 256 limit). The trip path
    // therefore drops the already-marshalled owned args, exactly like
    // the pending-abort path below. A failed push does not increment,
    // so the trip path must NOT pop. Tail self-calls are exempt on
    // both sides (rebind-and-jump here, the in-place loop there).
    let call_bl = cx.b.create_block();
    let trip_bl = cx.b.create_block();
    let dmerge = cx.b.create_block();
    cx.b.append_block_param(dmerge, types::I64);
    cx.b.append_block_param(dmerge, ret_pay_ty);
    {
        let push = cx.helper("graphix_depth_push")?;
        let call = cx.b.ins().call(push, &[]);
        let ok = cx.b.inst_results(call)[0];
        let valid = cx.b.ins().icmp_imm(IntCC::NotEqual, ok, 0);
        cx.b.ins().brif(valid, call_bl, &[], trip_bl, &[]);
    }
    cx.b.switch_to_block(trip_bl);
    cx.b.seal_block(trip_bl);
    {
        emit_call_arg_drops(cx.b, cx.ctx, &drops)?;
        let ph = emit_elem_placeholder(cx, if widen { node_typ } else { ret })?;
        cx.b.ins().jump(dmerge, &[BlockArg::Value(ph.disc), BlockArg::Value(ph.payload)]);
    }
    cx.b.switch_to_block(call_bl);
    cx.b.seal_block(call_bl);
    let func_ref =
        cx.ctx.callee_refs.get(&kernel_abi::kernel_key(&info.kernel)).ok_or_else(
            || {
                anyhow!(
                    "lambda call `{fn_name}`: callee_refs has no entry — \
                     discovery/declare drift"
                )
            },
        )?;
    let inst = cx.b.ins().call(*func_ref, &clif_args);
    // Exit the depth-guard unit entered above. A callee that ABORTED
    // (interrupt / its own deeper depth trip) still returns here
    // normally (the abort path returns the pending sentinel), so the
    // pop is unconditional.
    {
        let pop = cx.helper("graphix_depth_pop")?;
        cx.b.ins().call(pop, &[]);
    }
    // A callee that genuinely ABORTED (interrupt, depth trip) left
    // `DYNCALL_PENDING` set and returned the pending sentinel — the
    // zero pair, NOT a real value. Propagate the abort at the call
    // site: drop the owned call args, drop this kernel's owned set,
    // and jump to `pending_exit` with the flag still set (peek, not
    // clear) so `Kernel::update` discards. Without this the sentinel
    // would flow into downstream derefs and drops. Value-level
    // bottoms never take this path — a callee's pended DynCall
    // converts to a #219 tainted result at its own site and rides
    // back IN-BAND in the returned disc.
    {
        let peek = cx.helper("graphix_dyncall_pending_take")?;
        let call = cx.b.ins().call(peek, &[]);
        let pending = cx.b.inst_results(call)[0];
        let abort_bl = cx.b.create_block();
        let cont_bl = cx.b.create_block();
        cx.b.ins().brif(pending, abort_bl, &[], cont_bl, &[]);
        cx.b.switch_to_block(abort_bl);
        cx.b.seal_block(abort_bl);
        emit_call_arg_drops(cx.b, cx.ctx, &drops)?;
        let exit = pending_exit_block(cx.b, cx.ctx);
        emit_pending_cleanup(cx.b, cx.env, cx.ctx)?;
        cx.b.ins().jump(exit, &[]);
        cx.b.switch_to_block(cont_bl);
        cx.b.seal_block(cont_bl);
    }
    // Unified Value ABI: every callee returns the genuine two-word
    // `(disc, payload)` Value pair, TAINT/STALE in-band in the disc.
    // A value-typed consumer (`widen`) keeps the pair verbatim; a
    // scalar-typed one narrows the payload to the prim's CLIF type
    // for interior register use.
    let (r0, r1) = {
        let results = cx.b.inst_results(inst);
        if results.len() != 2 {
            return Err(anyhow!(
                "lambda call `{fn_name}`: callee returned {} values, expected 2",
                results.len()
            ));
        }
        (results[0], results[1])
    };
    let result = match kernel_abi::abi_kind(reg, ret) {
        Some(AbiKind::Scalar(p)) if !widen => {
            CompiledExpr::new(r0, cast_u64_to_prim(cx.b, r1, p))
        }
        Some(
            AbiKind::Scalar(_)
            | AbiKind::Array
            | AbiKind::Tuple
            | AbiKind::Struct
            | AbiKind::String
            | AbiKind::Variant
            | AbiKind::Nullable
            | AbiKind::Value,
        ) => CompiledExpr::new(r0, r1),
        other => {
            return Err(anyhow!(
                "lambda call `{fn_name}`: return shape {other:?} not \
                 lowered — subtree node-walks"
            ));
        }
    };
    // Owned-arg drops for the CALL path (the trip path dropped its own
    // marshalled copies before its placeholder), then meet the trip
    // placeholder at the merge.
    emit_call_arg_drops(cx.b, cx.ctx, &drops)?;
    cx.b.ins()
        .jump(dmerge, &[BlockArg::Value(result.disc), BlockArg::Value(result.payload)]);
    cx.b.switch_to_block(dmerge);
    cx.b.seal_block(dmerge);
    let disc = cx.b.block_params(dmerge)[0];
    let payload = cx.b.block_params(dmerge)[1];
    Ok(CompiledExpr::new(disc, payload))
}

/// Emit the post-call drops for owned composite/value call args. The
/// drops run after the call returns (the result SSA value is already
/// read out) — dropping an arg doesn't touch the return value.
fn emit_call_arg_drops(
    b: &mut FunctionBuilder,
    ctx: &LowerCtx,
    drops: &[CallArgDrop],
) -> Result<()> {
    if drops.is_empty() {
        return Ok(());
    }
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
    for d in drops {
        match d {
            CallArgDrop::Composite(bits) => {
                b.ins().call(arr_drop, &[*bits]);
            }
            CallArgDrop::String(bits) => {
                b.ins().call(str_drop, &[*bits]);
            }
            CallArgDrop::Value { disc, payload } => {
                b.ins().call(val_drop, &[*disc, *payload]);
            }
        }
    }
    Ok(())
}

/// Emit a `graphix_valarray_drop` call for every owned composite
/// local currently in scope. Called at every Return point so we
/// don't leak refcount-bumped ValArrays past kernel exit.
///
/// A composite/string/value RETURN is safe against these drops
/// because `emit_kernel_return` makes the result independently owned
/// (`ensure_owned_*_src` clones a borrowed local read) BEFORE calling
/// this — the returned pointer never aliases a dropped slot.
pub(super) fn drop_owned_composites(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
    // Every owned local is dropped by `kind`. Composite params/locals
    // are refcount-cloned on kernel entry; Variant/Nullable/Value locals
    // come from entry clones / `VariantNew` / composite-return DynCall;
    // String locals carry an owned refcount. Scalars own nothing.
    let drops: smallvec::SmallVec<[(LocalKind, ValueVar); 8]> =
        env.locals.iter().map(|l| (l.kind, l.vv)).collect();
    for (kind, vv) in drops {
        emit_drop_local(b, ctx, kind, vv)?;
    }
    Ok(())
}

/// Emit the runtime drop for ONE owned local of `kind` held in `vv` —
/// the single per-kind drop dispatch (scalars own nothing). Shared by
/// every bulk-drop site: `drop_owned_composites`, block scope exits
/// (`emit_scope_drops`), and the tail-rebind residual drops. (The
/// select merge's scrutinee drop keeps its own match — a String
/// scrutinee there is a classify bug it must error on, not drop.)
pub(super) fn emit_drop_local(
    b: &mut FunctionBuilder,
    ctx: &LowerCtx,
    kind: LocalKind,
    vv: ValueVar,
) -> Result<()> {
    let helper =
        |name: &str| ctx.helper_refs.get(name).ok_or_else(|| anyhow!("missing {name}"));
    match kind {
        LocalKind::Scalar(_) => {}
        LocalKind::Composite => {
            let f = helper("graphix_valarray_drop")?;
            let ptr = b.use_var(vv.payload);
            b.ins().call(f, &[ptr]);
        }
        LocalKind::String => {
            let f = helper("graphix_arcstr_drop")?;
            let ptr = b.use_var(vv.payload);
            b.ins().call(f, &[ptr]);
        }
        LocalKind::Variant | LocalKind::Nullable | LocalKind::Value => {
            let f = helper("graphix_value_drop")?;
            let disc = b.use_var(vv.disc);
            let payload = b.use_var(vv.payload);
            b.ins().call(f, &[disc, payload]);
        }
    }
    Ok(())
}

/// Emit drops for everything the JIT'd kernel currently owns, for
/// the `pre_pending` block of a composite-return DynCall that
/// pended. This is `drop_owned_composites` plus the in-flight
/// outer DynCall args bufs from `dyncall_buf_stack`.
///
/// Ordering doesn't matter — every entry is an independent owned
/// allocation. `use_var` at this CFG point resolves each Variable
/// to its value along the edge from the DynCall block.
pub(super) fn emit_pending_cleanup(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
    let buf_drop = ctx
        .helper_refs
        .get("graphix_value_buf_drop")
        .ok_or_else(|| anyhow!("missing graphix_value_buf_drop"))?;
    // Outer in-flight DynCall args bufs. The current DynCall's own
    // buf was already popped before this is called.
    for buf_var in ctx.dyncall_buf_stack.borrow().iter() {
        let ptr = b.use_var(*buf_var);
        b.ins().call(buf_drop, &[ptr]);
    }
    // Owned HOF input arrays in flight (fresh producers being
    // consumed by a loop scaffold) — finished ValArrays, dropped via
    // `graphix_valarray_drop` (NOT the buf destructor).
    let arr_drop = ctx
        .helper_refs
        .get("graphix_valarray_drop")
        .ok_or_else(|| anyhow!("missing graphix_valarray_drop"))?;
    for arr_var in ctx.owned_input_stack.borrow().iter() {
        let ptr = b.use_var(*arr_var);
        b.ins().call(arr_drop, &[ptr]);
    }
    // Owned composite + variant locals (and entry-cloned params).
    drop_owned_composites(b, env, ctx)
}

// (The old `emit_dyncall_pending_branch` / `emit_return_pending_check`
// pair — the site-level and return-level whole-kernel aborts for a
// pended DynCall — is gone: every dyncall site now take-and-CLEARS the
// pending flag and converts a pend to a #219 tainted placeholder that
// continues, and a cross-kernel callee's genuine abort is checked and
// propagated at the call site in `emit_lambda_call_node`. No path can
// leave the flag set and keep executing, so a return-time peek would
// always read false.)
