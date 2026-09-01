//! Call emission: cross-kernel lambda calls (site blocks, arg
//! marshalling, drops, pending cleanup) and the direct fastcall /
//! cast path.

use crate::{
    Node, Rt, Update, UserEvent,
    fusion::{
        LambdaCallInfo,
        kernel_abi::{self, AbiKind, PrimType},
        lowering::{BuiltinCallSiteInfo, CaptureSlot, SiteDispatch},
    },
    node::callsite::CallSite,
    typ::{FnArgKind, Type},
};
use anyhow::{Result, anyhow};
use cranelift_codegen::ir::{
    BlockArg, Inst, InstBuilder, MemFlags, StackSlotData, StackSlotKind,
    Value as ClifValue, condcodes::IntCC, types,
};
use cranelift_frontend::{FunctionBuilder, Variable};
use netidx_value::Value;

use super::{
    abi::{
        CompiledExpr, JitEnv, LocalKind, STALE, TAINT, ValueVar, clean_disc,
        emit_untainted_i64, is_tainted, scalar_disc, value_disc,
    },
    body::{BodyCx, node_composite_source, node_is_bottom, pending_exit_block},
    lower::{LowerCtx, SelWord},
    nodes::{call_result_needs_value_widening, emit_elem_placeholder},
    scalar::{cast_u64_to_prim, prim_to_clif, scalar_to_payload_i64},
};

/// The `Value` discriminant word of each register scalar's variant —
/// what a FASTCALL site stores beside a scalar arg's bits so the
/// trampoline's `&[Value]` view reads a genuine `Value::I64(..)` etc.
fn prim_value_disc(p: PrimType) -> u64 {
    let sample = match p {
        PrimType::I8 => Value::I8(0),
        PrimType::I16 => Value::I16(0),
        PrimType::I32 => Value::I32(0),
        PrimType::I64 => Value::I64(0),
        PrimType::U8 => Value::U8(0),
        PrimType::U16 => Value::U16(0),
        PrimType::U32 => Value::U32(0),
        PrimType::U64 => Value::U64(0),
        PrimType::F32 => Value::F32(0.0),
        PrimType::F64 => Value::F64(0.0),
        PrimType::Bool => Value::Bool(false),
    };
    crate::tval::value_words(&sample)[0]
}

/// `Value::Array`'s / `Value::String`'s discriminant words (the
/// composite and string wire words are the payloads of those variants).
static ARRAY_VALUE_DISC: std::sync::LazyLock<(u64,)> = std::sync::LazyLock::new(|| {
    let v = Value::Array(netidx_value::ValArray::from_iter_exact(std::iter::empty()));
    (crate::tval::value_words(&v)[0],)
});
static STRING_VALUE_DISC: std::sync::LazyLock<(u64,)> = std::sync::LazyLock::new(|| {
    (crate::tval::value_words(&Value::String(arcstr::ArcStr::new()))[0],)
});

/// A fusable call — a `FastFn` builtin or the Cast pseudo-site
/// ([`SiteDispatch`]): marshal the (marshal-ordered) `args` as (disc,
/// payload) pairs into a STACK buffer the trampoline views as
/// `&[Value]` (a scalar with its variant's discriminant word,
/// composite/string bits borrowed, a value shape with its disc
/// cleaned), dispatch, release what this site owned, then decode the
/// returned pair per the static return shape: scalar / unit / string
/// / composite returns the unwrapped value, a Value-shape return
/// passes the (disc, payload) pair through. The trampoline computes
/// the production's tag from the arg masks (a tainted arg bottoms the
/// call without invoking the fn; all-stale args make the result
/// stale; `None` is this cycle's bottom) and returns it in-band on
/// the disc, so a bottomed or quiet result rides to its consumers as
/// data, never as a whole-kernel abort.
pub(crate) fn emit_builtin_call_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    info: &BuiltinCallSiteInfo,
    args: &[&Node<R, E>],
) -> Result<CompiledExpr> {
    let ret_abi = kernel_abi::abi_kind(&info.return_type);
    if matches!(ret_abi, Some(AbiKind::Null) | None) {
        return Err(anyhow!(
            "emit_clif: call with bare Null / non-fusable return — \
             should have widened to Nullable<T> at construction"
        ));
    }
    if args.len() > 64 {
        return Err(anyhow!(
            "emit_clif: call with more than 64 args — the taint mask is one word"
        ));
    }
    let slot = cx.b.create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (16 * args.len().max(1)) as u32,
        3,
    ));
    let mut drops: smallvec::SmallVec<[(&str, ClifValue, Option<ClifValue>); 8]> =
        smallvec::SmallVec::new();
    // #219: each arg's disc — its TAINT bit bottoms the call, its STALE
    // bit feeds the stale mask.
    let mut arg_discs: smallvec::SmallVec<[ClifValue; 8]> = smallvec::SmallVec::new();
    for (i, (arg_node, t)) in args.iter().zip(info.arg_types.iter()).enumerate() {
        // Compare by runtime SHAPE (`AbiKind`), not exact `Type` — the
        // buffer is laid out by `info.arg_types`, so only the shape
        // needs to agree.
        let Some(frozen) = kernel_abi::freeze_for_abi_normalized(arg_node.typ()) else {
            return Err(anyhow!(
                "emit_clif: call arg type {:?} doesn't freeze concrete",
                arg_node.typ()
            ));
        };
        let kind = kernel_abi::abi_kind(t);
        if kernel_abi::abi_kind(&frozen) != kind {
            return Err(anyhow!(
                "emit_clif: call arg shape {frozen:?} disagrees with the \
                 discovered arg type {t:?}"
            ));
        }
        let cv = arg_node.emit_clif(cx)?;
        arg_discs.push(cv.disc);
        // The stored disc must be CLEAN (a tainted disc is an invalid
        // tag — the fn would see a corrupt Value); the arg's own disc
        // carries its taint to the masks. The fn borrows the buffer, so
        // an OWNED producer's value is released by this site after the
        // call; strings are always owned at production.
        let (disc, payload) = match kind {
            Some(AbiKind::Scalar(p)) => {
                (cx.b.ins().iconst(types::I64, prim_value_disc(p) as i64), cv.payload)
            }
            Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                if node_composite_source(arg_node) == CompositeSource::Owned {
                    drops.push(("graphix_valarray_drop", cv.payload, None));
                }
                (cx.b.ins().iconst(types::I64, ARRAY_VALUE_DISC.0 as i64), cv.payload)
            }
            Some(AbiKind::String) => {
                drops.push(("graphix_arcstr_drop", cv.payload, None));
                (cx.b.ins().iconst(types::I64, STRING_VALUE_DISC.0 as i64), cv.payload)
            }
            Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
                let disc = clean_disc(cx.b, cv.disc);
                if node_composite_source(arg_node) == CompositeSource::Owned {
                    drops.push(("graphix_value_drop", disc, Some(cv.payload)));
                }
                (disc, cv.payload)
            }
            Some(AbiKind::Unit) => {
                return Err(anyhow!("emit_clif: call arg has Unit type"));
            }
            Some(AbiKind::Null) | None => {
                return Err(anyhow!(
                    "emit_clif: call arg with bare Null / non-fusable type — \
                     should have widened to Nullable<T> at construction"
                ));
            }
        };
        cx.b.ins().stack_store(disc, slot, (16 * i) as i32);
        cx.b.ins().stack_store(payload, slot, (16 * i + 8) as i32);
    }
    // Interior-bottom v3 (Eric's ruling 2026-07-20): a tainted arg is
    // a mask bit, not a reason to skip emission — the trampoline
    // bottoms the call. Each `is_tainted` folds to const-false for a
    // proven-untainted arg, so the mask is const-0 on the hot path.
    let mut taint_mask = cx.b.ins().iconst(types::I64, 0);
    for (i, d) in arg_discs.iter().enumerate() {
        let t = is_tainted(cx.b, *d);
        let t64 = cx.b.ins().uextend(types::I64, t);
        let bit = cx.b.ins().ishl_imm(t64, i as i64);
        taint_mask = cx.b.ins().bor(taint_mask, bit);
    }
    // The STALE twin: bit `i` set = the arg is present but did not
    // fire this cycle. Suppressed under GENUINE init only — the raw
    // kernel init (wire bit 0) minus the wake bit (wire bit 2): at
    // genuine init every input is born and the production fires; a
    // wake invocation delivers its standing inputs with honest stale
    // bits (design/wake_catchup.md — the interp's stateless eval
    // re-runs from the present stale slots at wake).
    let stale_mask = {
        let mut stale_mask = cx.b.ins().iconst(types::I64, 0);
        for (i, d) in arg_discs.iter().enumerate() {
            let s = cx.b.ins().band_imm(*d, STALE);
            let sb = cx.b.ins().icmp_imm(IntCC::NotEqual, s, 0);
            let s64 = cx.b.ins().uextend(types::I64, sb);
            let bit = cx.b.ins().ishl_imm(s64, i as i64);
            stale_mask = cx.b.ins().bor(stale_mask, bit);
        }
        let (init, wake) = (cx.ctx.init_flag, cx.ctx.wake_flag);
        let init_b = cx.b.ins().icmp_imm(IntCC::NotEqual, init, 0);
        let no_wake = cx.b.ins().icmp_imm(IntCC::Equal, wake, 0);
        let genuine = cx.b.ins().band(init_b, no_wake);
        let zero = cx.b.ins().iconst(types::I64, 0);
        cx.b.ins().select(genuine, zero, stale_mask)
    };
    let base = cx.b.ins().stack_addr(types::I64, slot, 0);
    let n = cx.b.ins().iconst(types::I64, args.len() as i64);
    let call = match &info.dispatch {
        SiteDispatch::Fast(f) => {
            let fast = cx.helper("graphix_fastcall")?;
            let fp = cx.b.ins().iconst(types::I64, *f as usize as i64);
            cx.b.ins().call(fast, &[fp, base, n, taint_mask, stale_mask])
        }
        SiteDispatch::Cast(target) => {
            let castcall = cx.helper("graphix_castcall")?;
            let tp = cx.interned_type(target);
            cx.b.ins().call(castcall, &[tp, base, n, taint_mask, stale_mask])
        }
    };
    for (helper, w0, w1) in drops.drain(..) {
        let h = cx.helper(helper)?;
        match w1 {
            Some(w1) => {
                cx.b.ins().call(h, &[w0, w1]);
            }
            None => {
                cx.b.ins().call(h, &[w0]);
            }
        }
    }
    let (raw0, raw1) = {
        let r = cx.b.inst_results(call);
        (r[0], r[1])
    };
    let dmerge = cx.b.create_block();
    let pay_ty = match ret_abi {
        Some(AbiKind::Scalar(p)) => prim_to_clif(p),
        _ => types::I64,
    };
    cx.b.append_block_param(dmerge, types::I64);
    cx.b.append_block_param(dmerge, pay_ty);
    // The returned disc's TAINT/STALE bits ARE the production's tag.
    let tagbits = cx.b.ins().band_imm(raw0, TAINT | STALE);
    match ret_abi {
        Some(AbiKind::Scalar(p)) => {
            // The payload word carries the Value-encoded scalar bits —
            // narrow to the prim (a bottom's placeholder payload is
            // harmless garbage for downstream scalar arithmetic,
            // guarded by its TAINT bit).
            let value = cast_u64_to_prim(cx.b, raw1, p);
            let base = scalar_disc(cx.b, p);
            let disc = cx.b.ins().bor(base, tagbits);
            cx.b.ins().jump(dmerge, &[BlockArg::Value(disc), BlockArg::Value(value)]);
        }
        Some(AbiKind::Unit) => {
            // The result is discarded by the statement position; the
            // tag still rides so a bound unit local reads honestly.
            let base = cx.b.ins().iconst(types::I64, value_disc::NULL);
            let disc = cx.b.ins().bor(base, tagbits);
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
            // Pointer-carrying / two-word returns: a BOTTOM return
            // (in-band TAINT) carries only its helper-safe placeholder
            // — never adopt its payload. Branch: on taint, produce a
            // tainted shape-safe placeholder (preserving the return's
            // STALE bit — a standing bottom must stay quiet) and
            // continue to the merge. For String/composite returns the
            // returned DISC is additionally checked against the
            // expected shape — a fn that returned the wrong Value
            // shape violated its declared type; adopting its payload
            // as ArcStr/ValArray bits would be UB, so a mismatch takes
            // the placeholder path too, loudly.
            let bad_bl = cx.b.create_block();
            let ok_bl = cx.b.create_block();
            let t = is_tainted(cx.b, raw0);
            let expected_disc: Option<i64> = match ret_abi {
                Some(AbiKind::String) => Some(value_disc::STRING),
                Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                    Some(value_disc::ARRAY)
                }
                _ => None,
            };
            let bad = match expected_disc {
                Some(exp) => {
                    let clean0 = clean_disc(cx.b, raw0);
                    let mismatch = cx.b.ins().icmp_imm(IntCC::NotEqual, clean0, exp);
                    cx.b.ins().bor(t, mismatch)
                }
                None => t,
            };
            cx.b.ins().brif(bad, bad_bl, &[], ok_bl, &[]);
            cx.b.switch_to_block(bad_bl);
            cx.b.seal_block(bad_bl);
            // A shape-mismatched (untainted) result still OWNS the
            // returned Value — warn and drop it before the placeholder
            // so the declared-type violation leaks nothing
            // (sprintf-error-return-shape-aug2026). A bottom's
            // placeholder pair owns nothing.
            {
                let untainted = cx.b.ins().icmp_imm(IntCC::Equal, t, 0);
                let drop_bl = cx.b.create_block();
                let cont_bl = cx.b.create_block();
                cx.b.ins().brif(untainted, drop_bl, &[], cont_bl, &[]);
                cx.b.switch_to_block(drop_bl);
                cx.b.seal_block(drop_bl);
                let warn_h = cx.helper("graphix_shape_mismatch_warn")?;
                cx.b.ins().call(warn_h, &[raw0]);
                let val_drop = cx.helper("graphix_value_drop")?;
                cx.b.ins().call(val_drop, &[raw0, raw1]);
                cx.b.ins().jump(cont_bl, &[]);
                cx.b.switch_to_block(cont_bl);
                cx.b.seal_block(cont_bl);
            }
            let ph = emit_elem_placeholder(cx, &info.return_type)?;
            let taint_c = cx.b.ins().iconst(types::I64, TAINT);
            let ph_disc = cx.b.ins().bor(ph.disc, tagbits);
            let ph_disc = cx.b.ins().bor(ph_disc, taint_c);
            cx.b.ins()
                .jump(dmerge, &[BlockArg::Value(ph_disc), BlockArg::Value(ph.payload)]);
            cx.b.switch_to_block(ok_bl);
            cx.b.seal_block(ok_bl);
            let (disc, pay) = match ret_abi {
                // `raw1` is the owned ArcStr / ValArray bits; the
                // return's tag rides.
                Some(AbiKind::String) => {
                    let base = cx.b.ins().iconst(types::I64, value_disc::STRING);
                    (cx.b.ins().bor(base, tagbits), raw1)
                }
                Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                    let base = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
                    (cx.b.ins().bor(base, tagbits), raw1)
                }
                // Value-shape: `raw0` is the real Value disc with the
                // production's tag already in-band.
                _ => (raw0, raw1),
            };
            cx.b.ins().jump(dmerge, &[BlockArg::Value(disc), BlockArg::Value(pay)]);
        }
        Some(AbiKind::Null) | None => unreachable!("refused above"),
    }
    cx.b.switch_to_block(dmerge);
    cx.b.seal_block(dmerge);
    let params = cx.b.block_params(dmerge);
    // STRICT (Eric's ruling 2026-08-13): BOTTOM PROPAGATES at the
    // call's merge, everywhere.
    Ok(CompiledExpr::new(params[0], params[1]))
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
/// callsite whose output didn't fire. Aborts (depth trip, interrupt)
/// ride `KERNEL_ABORT` and bottom the whole caller kernel.
/// The PER-CALL-SITE state block argument for a cross-kernel call
/// (wire slot 2): storage for the callee's instance memory
/// (prev-length words, first-call words, loop-table anchors), owned
/// by THIS caller and
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
fn emit_site_block(
    cx: &mut BodyCx,
    info: &LambdaCallInfo,
    is_self: bool,
) -> Result<ClifValue> {
    let key = kernel_abi::kernel_key(&info.kernel);
    let layout = match cx.callee_site_layout(key) {
        // No recorded layout means a back-edge: the callee is still
        // being emitted, so it is either US (a self-call) or a mutual
        // cycle — and mutual cycles de-fuse at the static call edge, so
        // in practice it is US.
        //
        // A self-call cannot carve its callee's block out of ours the
        // way every other call site does: it would have to nest one
        // level per activation, and how deep the recursion runs is a
        // run-time fact. Root a lazily-grown TREE of per-activation
        // blocks instead (`graphix_site_child_block`) — the node-walk's
        // tree of retained instances, at a few words per activation
        // instead of ~10KB. Passing 0 here, as this did, left every
        // recursive activation with no interior memory at all, so its
        // rides missed where the interp's rode
        // (fuzz/open/01_recursive_activation_cache.gx).
        None => {
            // Non-self edges without a layout are mutual-recursion
            // cycles, refused upstream (`is_recursive_edge` bails at
            // the CallSite before this path) — reaching here is a
            // routing bug, and passing 0 would run the callee with no
            // interior memory (a silent Ruling-2 divergence, the
            // wrong failure direction). De-fuse loudly instead; same
            // for a self-call whose per-activation block root can't
            // be claimed (in-loop scaffold contexts — today those
            // shapes are unreachable: a callback-mediated self-call
            // is a mutual edge and self-as-callback is an occurs-
            // check error; if a future shape gets here it LOSES
            // FUSION, never correctness — the 2026-08-20 audit,
            // design/activation_state.md).
            if !is_self {
                return Err(anyhow!(
                    "emit_clif: non-self recursive edge reached site-block                      emission — mutual cycles refuse at the call site"
                ));
            }
            let Some(off) = cx.claim_self_block_word() else {
                return Err(anyhow!(
                    "emit_clif: no per-activation block root for a self-call                      (in-loop context) — de-fuse"
                ));
            };
            let base = cx.site_ptr();
            let word = cx.b.ins().iadd_imm(base, off as i64);
            // Our own block can be null (we are ourselves a back-edge
            // activation that got no block); the helper takes that as
            // "no memory" and hands back null, which every consumer
            // already guards.
            let live = cx.b.ins().icmp_imm(IntCC::NotEqual, base, 0);
            let zero = cx.b.ins().iconst(types::I64, 0);
            let word = cx.b.ins().select(live, word, zero);
            // The size lives in the callee's `site_desc` cell, read at
            // run time: our own layout is not final while we are still
            // emitting into it. The cell's address is stable for the
            // JIT's lifetime (the kernel cache holds the `Arc`).
            let desc = cx.b.ins().iconst(
                types::I64,
                (&info.kernel.site_desc as *const std::sync::atomic::AtomicU64) as i64,
            );
            let f = cx.helper("graphix_site_child_block")?;
            let call = cx.b.ins().call(f, &[word, desc]);
            return Ok(cx.b.inst_results(call)[0]);
        }
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
            for b in layout.self_blocks.iter() {
                cx.ctx.state.self_blocks.borrow_mut().push(kernel_abi::SelfBlock {
                    rel: base_idx + b.rel,
                    words: b.words,
                    slots: b.slots.clone(),
                });
            }
            for a in layout.anchors.iter() {
                cx.ctx.state.anchors.borrow_mut().push(kernel_abi::SiteAnchor {
                    rel: base_idx + a.rel,
                    own_levels: a.own_levels,
                    leaf: a.leaf.clone(),
                });
            }
            let sp = cx.state_ptr();
            return Ok(cx.b.ins().iadd_imm(sp, first as i64));
        }
        if let Some(first) = cx.claim_site_word() {
            for _ in 1..layout.words {
                cx.claim_site_word().expect("contiguous site claims can't fail mid-run");
            }
            let base_idx = (first / 8) as u32;
            for b in layout.self_blocks.iter() {
                cx.ctx.site.self_blocks.borrow_mut().push(kernel_abi::SelfBlock {
                    rel: base_idx + b.rel,
                    words: b.words,
                    slots: b.slots.clone(),
                });
            }
            for a in layout.anchors.iter() {
                cx.ctx.site.anchors.borrow_mut().push(kernel_abi::SiteAnchor {
                    rel: base_idx + a.rel,
                    own_levels: a.own_levels,
                    leaf: a.leaf.clone(),
                });
            }
            let base = cx.site_ptr();
            // Our own block may be 0 (a back-edge activation of THIS
            // callee) — forward 0, not a garbage offset.
            let addr = cx.b.ins().iadd_imm(base, first as i64);
            let has = cx.b.ins().icmp_imm(IntCC::NotEqual, base, 0);
            let zero = cx.b.ins().iconst(types::I64, 0);
            return Ok(cx.b.ins().select(has, addr, zero));
        }
        return Ok(cx.b.ins().iconst(types::I64, 0));
    }
    // In-loop call site. The chain runs per innermost iteration (this
    // IS the loop body) — the ensures are idempotent after the first.
    let frames: smallvec::SmallVec<[(ClifValue, ClifValue, Variable); 4]> = {
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
    // Exit-block re-ensure record (THE SHRINK-TO-ZERO RULE, aug18a
    // class 4): this chain's per-iteration ensure never runs on a
    // len-0 epoch, so every enclosing loop's exit re-ensures it at
    // its level (`BodyCx::emit_slot_truncates`) — a shrink truncates
    // the per-slot call-site blocks exactly when the interp deletes
    // the slot instances.
    let trunc_rec = |anchor| {
        use crate::fusion::emit::lower::{TruncLeaf, TruncRec};
        TruncRec {
            anchor,
            n_dirs: n_dirs as u32,
            leaf: match &leaf_rt {
                None => TruncLeaf::Table { stride: layout.words },
                Some(_) => TruncLeaf::Blocks,
            },
            leaf_ptr: leaf_rt
                .as_ref()
                .map(|l| std::sync::Arc::as_ptr(l) as *const u8 as i64)
                .unwrap_or(0),
        }
    };
    let anchor = match cx.claim_state_word_loop_invariant() {
        Some(off) => {
            cx.ctx.state.anchors.borrow_mut().push(kernel_abi::SiteAnchor {
                rel: (off / 8) as u32,
                own_levels: n_dirs as u32,
                leaf: leaf_rt.clone(),
            });
            if let Some(f) = cx.ctx.slot_tables.borrow_mut().last_mut() {
                f.pending
                    .push(trunc_rec(crate::fusion::emit::lower::TruncAnchor::State(off)));
            }
            let sp = cx.state_ptr();
            SelWord::Sure(cx.b.ins().iadd_imm(sp, off as i64))
        }
        None => match cx.claim_site_anchor(n_dirs as u32, leaf_rt.clone()) {
            Some(off) => {
                if let Some(f) = cx.ctx.slot_tables.borrow_mut().last_mut() {
                    f.pending.push(trunc_rec(
                        crate::fusion::emit::lower::TruncAnchor::Site(off),
                    ));
                }
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

fn callee_results(
    cx: &mut BodyCx,
    inst: Inst,
    fn_name: &str,
) -> Result<(ClifValue, ClifValue)> {
    let results = cx.b.inst_results(inst);
    if results.len() != 2 {
        return Err(anyhow!(
            "lambda call `{fn_name}`: callee returned {} values, expected 2",
            results.len()
        ));
    }
    Ok((results[0], results[1]))
}

pub(crate) fn emit_lambda_call_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    cs: &CallSite<R, E>,
    info: &LambdaCallInfo,
    is_self: bool,
) -> Result<CompiledExpr> {
    let fn_name = &info.fn_name;
    // Hoist the registry borrow (a `'c` ref independent of `cx`) so the
    // slot-grouping closures below capture IT, not `cx` — otherwise the
    // closures would hold `cx` shared while the per-slot emit needs
    // `&mut cx`.
    let ftype = cs
        .resolved_ftype()
        .or_else(|| cs.ftype())
        .ok_or_else(|| anyhow!("lambda call `{fn_name}`: no resolved FnType"))?;
    // Formal args in FnType parameter order — the order
    // `build_lambda_kernel` translated them into kernel inputs. Each
    // slot is typed from the CALLEE's signature (`info.arg_types`,
    // formals first, captures appended): those types were resolved
    // (`expand_refs` — named types expanded through the env) and
    // frozen at build time. Freezing the caller-side node type here
    // instead would re-reject exactly those Refs (#218), and env isn't
    // available at emit time to resolve them — the classic caller
    // (`emit_lambda_call`) types args from the signature the same
    // way. A node whose actual emission shape disagrees with the
    // signature type Errs in the per-slot extractors below (build
    // time, de-fuse).
    let skipped = &info.kernel.skipped_args;
    let n_formal =
        info.arg_types.len().checked_sub(info.captures.len()).ok_or_else(|| {
            anyhow!(
                "lambda call `{fn_name}`: signature has fewer inputs than \
                 captures — discovery drift"
            )
        })?;
    if ftype.args.len() != n_formal + skipped.len() {
        return Err(anyhow!(
            "lambda call `{fn_name}`: call-site FnType has {} formals, \
             kernel signature has {n_formal} (+{} skipped fn) — de-fuse",
            ftype.args.len(),
            skipped.len()
        ));
    }
    let mut slots: poolshark::local::LPooled<Vec<LambdaCallSlot<R, E>>> =
        poolshark::local::LPooled::take();
    let mut pos = 0usize;
    let mut sig_idx = 0usize;
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
        // A SKIPPED fn formal: the callee kernel has no slot for it —
        // the callee's uses are statically-resolved calls baked at
        // build time (keyed by the resolution fingerprint), so the
        // value is never consumed. The arg node is USUALLY pure (a
        // lambda literal or a Ref), so don't emit it — but it may be a
        // block carrying an EFFECT the node-walk performs (`{z <- z+1;
        // g}`), and skipping that drops the effect (aug28b: a `<-`
        // spinner in a discarded fn arg quiesced the kernel where the
        // node-walk spun forever). De-fuse so the node-walk runs it —
        // effects de-fuse, never silently skip.
        if skipped.contains(&(i as u32)) {
            if !super::flow::stmt_subtree_effect_free(node) {
                return Err(anyhow!(
                    "lambda call `{fn_name}`: skipped fn-formal arg carries \
                     an effect — de-fuse so the node-walk runs it"
                ));
            }
            continue;
        }
        slots.push(LambdaCallSlot::Arg(node, info.arg_types[sig_idx].clone()));
        sig_idx += 1;
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
    // itself ([`node_is_bottom`]).
    for s in &*slots {
        if let LambdaCallSlot::Arg(n, _) = s {
            if node_is_bottom(n) {
                return Err(anyhow!(
                    "lambda call `{fn_name}`: Bottom-typed arg in value \
                     position — subtree node-walks"
                ));
            }
        }
        match kernel_abi::abi_kind(s.typ()) {
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
    let mut clif_args: smallvec::SmallVec<[ClifValue; 24]> =
        smallvec::SmallVec::with_capacity(slots.len() * 2 + 1);
    let mut drops: smallvec::SmallVec<[CallArgDrop; 8]> = smallvec::SmallVec::new();
    let ret = &info.kernel.return_type;
    // The callsite NODE's type may promise a 2-word Value where the
    // callee ABI returns its own narrower shape (see
    // `widen_result_to_value`) — the call path widens its result, and
    // the trip placeholder is emitted per the NODE type so both merge
    // edges carry the Value pairing.
    let node_typ = cs.typ();
    let widen = call_result_needs_value_widening(node_typ, ret);
    let ret_pay_ty = match kernel_abi::abi_kind(ret) {
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
    // whose loop had zero elements until its source grew — still
    // fires its consts/cached reads once. Mirror it with a per-call-
    // site state word: force the callee's init flag on the first call
    // ever, then never again (the word is call-site-shared across
    // loop iterations, exactly like the shared instance). No word
    // available (a callee body) → the plain kernel init flag.
    // Emit every argument slot FIRST — the derivation-changed memo
    // below needs the marshaled scalar pairs — then push the leading
    // context words, then the (disc, payload) pairs. Drops record
    // exactly as before; only the push order moved.
    let mut slot_cvs: smallvec::SmallVec<[CompiledExpr; 12]> = smallvec::SmallVec::new();
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
                    kernel_abi::abi_kind(s.typ()),
                    Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value)
                ) =>
            {
                match kernel_abi::abi_kind(n.typ()) {
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
            match kernel_abi::abi_kind(s.typ()) {
                // String ARG emissions are ALWAYS owned (String local
                // reads clone at the read; producers are owned) — drop
                // unconditionally. Cap string reads stay borrowed
                // views of the env slot and are never dropped.
                Some(AbiKind::String) => {
                    drops.push(CallArgDrop::String(cv.payload));
                }
                _ if node_composite_source(n) == CompositeSource::Owned => {
                    match kernel_abi::abi_kind(s.typ()) {
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
        slot_cvs.push(cv);
    }
    // The callee's context word: its init view — ours, or a forced
    // one on this site's first call ever (the `Callee::Static`
    // priming: a `bound` dispatch seeds its formals FIRED at any
    // frame depth, callsite.rs) — and the QUIET bit, inherited.
    let quiet = cx.quiet_flag();
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
    let quiet_bit = cx.b.ins().ishl_imm(quiet, 1);
    let callee_init = cx.b.ins().bor(callee_init, quiet_bit);
    clif_args.push(callee_init);
    clif_args.push(cx.state_ptr());
    let site_block = emit_site_block(cx, info, is_self)?;
    clif_args.push(site_block);
    for cv in slot_cvs.iter() {
        clif_args.push(cv.disc);
        clif_args.push(cv.payload);
    }
    let func_ref =
        cx.ctx.callee_refs.get(&kernel_abi::kernel_key(&info.kernel)).ok_or_else(
            || {
                anyhow!(
                    "lambda call `{fn_name}`: callee_refs has no entry — \
                     discovery/declare drift"
                )
            },
        )?;
    let dmerge = cx.b.create_block();
    cx.b.append_block_param(dmerge, types::I64);
    cx.b.append_block_param(dmerge, ret_pay_ty);
    // The raw (disc, payload) pair from whichever entry ran the callee.
    let rmerge = cx.b.create_block();
    cx.b.append_block_param(rmerge, types::I64);
    cx.b.append_block_param(rmerge, types::I64);
    if is_self {
        // Depth is bounded by memory, not a counter
        // (design/recursive_activations.md §4b): a self-call whose
        // remaining stack is inside the red zone re-enters the callee
        // on a fresh segment through the kernel's spill thunk — the
        // kernel twin of `stack::ensure_sufficient`. The same check
        // carries the cooperative interrupt: an interrupted call skips
        // the dispatch and continues with a tainted, shape-safe
        // placeholder. Only self-calls need it: cross-kernel edges are
        // acyclic (mutual recursion de-fuses), so their nesting is
        // bounded by the program. The check runs AFTER argument
        // marshalling, so an arg containing a further self-call is
        // checked at its own site, and the abort path drops the
        // already-marshalled owned args like the pending-abort path.
        let abort_bl = cx.b.create_block();
        let direct_bl = cx.b.create_block();
        let call_bl = cx.b.create_block();
        let grow_bl = cx.b.create_block();
        let check = cx.helper("graphix_stack_check")?;
        let call = cx.b.ins().call(check, &[]);
        let flag = cx.b.inst_results(call)[0];
        let interrupted = cx.b.ins().icmp_imm(IntCC::Equal, flag, 0);
        cx.b.ins().brif(interrupted, abort_bl, &[], direct_bl, &[]);
        cx.b.switch_to_block(direct_bl);
        cx.b.seal_block(direct_bl);
        let direct = cx.b.ins().icmp_imm(IntCC::Equal, flag, 1);
        cx.b.ins().brif(direct, call_bl, &[], grow_bl, &[]);
        cx.b.switch_to_block(abort_bl);
        cx.b.seal_block(abort_bl);
        emit_call_arg_drops(cx.b, cx.ctx, &drops)?;
        let ph = emit_elem_placeholder(cx, if widen { node_typ } else { ret })?;
        cx.b.ins().jump(dmerge, &[BlockArg::Value(ph.disc), BlockArg::Value(ph.payload)]);
        cx.b.switch_to_block(call_bl);
        cx.b.seal_block(call_bl);
        let inst = cx.b.ins().call(*func_ref, &clif_args);
        let (r0, r1) = callee_results(cx, inst, fn_name)?;
        cx.b.ins().jump(rmerge, &[BlockArg::Value(r0), BlockArg::Value(r1)]);
        cx.b.switch_to_block(grow_bl);
        cx.b.seal_block(grow_bl);
        let thunk = cx.ctx.self_thunk.ok_or_else(|| {
            anyhow!("lambda call `{fn_name}`: self-call in a kernel with no spill thunk")
        })?;
        let n = clif_args.len();
        let slot = cx.b.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            (8 * (n + 2)) as u32,
            3,
        ));
        let base = cx.b.ins().stack_addr(types::I64, slot, 0);
        for (i, v) in clif_args.iter().enumerate() {
            cx.b.ins().store(MemFlags::trusted(), *v, base, (8 * i) as i32);
        }
        let out = cx.b.ins().iadd_imm(base, (8 * n) as i64);
        let thunk = cx.b.ins().func_addr(types::I64, thunk);
        let grow = cx.helper("graphix_grow_stack")?;
        cx.b.ins().call(grow, &[thunk, base, out]);
        let r0 = cx.b.ins().load(types::I64, MemFlags::trusted(), out, 0);
        let r1 = cx.b.ins().load(types::I64, MemFlags::trusted(), out, 8);
        cx.b.ins().jump(rmerge, &[BlockArg::Value(r0), BlockArg::Value(r1)]);
    } else {
        let inst = cx.b.ins().call(*func_ref, &clif_args);
        let (r0, r1) = callee_results(cx, inst, fn_name)?;
        cx.b.ins().jump(rmerge, &[BlockArg::Value(r0), BlockArg::Value(r1)]);
    }
    cx.b.switch_to_block(rmerge);
    cx.b.seal_block(rmerge);
    let r0 = cx.b.block_params(rmerge)[0];
    let r1 = cx.b.block_params(rmerge)[1];
    // A callee that ABORTED (interrupt, depth trip) left `KERNEL_ABORT`
    // set and returned the abort sentinel — the zero pair, NOT a real
    // value. Propagate the abort at the call site: drop the owned call
    // args, drop this kernel's owned set, and jump to `pending_exit`
    // with the flag still set (peek, not clear) so `Kernel::update`
    // discards. Without this the sentinel would flow into downstream
    // derefs and drops. Value-level bottoms never take this path — they
    // ride back IN-BAND in the returned disc.
    {
        let peek = cx.helper("graphix_abort_peek")?;
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
    let result = match kernel_abi::abi_kind(ret) {
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
    #[cfg(debug_assertions)]
    if std::env::var_os("GXDBG_CALLRET").is_some() {
        let f = cx.helper("graphix_dbg_disc")?;
        let t = cx.b.ins().iconst(types::I64, 1);
        cx.b.ins().call(f, &[t, disc]);
    }
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
    // come from entry clones / `VariantNew` / composite-return calls;
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

/// Emit drops for everything the JIT'd kernel currently owns, for a
/// whole-kernel abort path: `drop_owned_composites` plus the in-flight
/// value bufs from `value_buf_stack` and the owned HOF inputs.
///
/// Ordering doesn't matter — every entry is an independent owned
/// allocation. `use_var` at this CFG point resolves each Variable
/// to its value along the edge from the aborting block.
pub(super) fn emit_pending_cleanup(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
    let buf_drop = ctx
        .helper_refs
        .get("graphix_value_buf_drop")
        .ok_or_else(|| anyhow!("missing graphix_value_buf_drop"))?;
    for buf_var in ctx.value_buf_stack.borrow().iter() {
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
