//! Call emission: cross-kernel lambda calls (site blocks, arg
//! marshalling, drops, pending cleanup) and the direct fastcall /
//! typed-fastcall path.

use super::lower::OwnCall;
use crate::{
    Node, Rt, UserEvent,
    expr::ExprId,
    fusion::{
        LambdaCallInfo,
        kernel_abi::{self, AbiKind},
        lowering::{BuiltinCallSiteInfo, CaptureSlot, SiteDispatch, formal_arg},
    },
    node::callsite::CallSite,
    typ::Type,
};
use anyhow::{Result, anyhow};
use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::{
    Block, BlockArg, FuncRef, Inst, InstBuilder, MemFlags, StackSlotData, StackSlotKind,
    Value as ClifValue, condcodes::IntCC, types,
};
use cranelift_frontend::{FunctionBuilder, Variable};
use poolshark::local::LPooled;
use smallvec::SmallVec;

use super::{
    abi::{
        CompiledExpr, FIRE_TEST, JitEnv, LocalKind, STALE, TAINT, ValueVar, clean_disc,
        emit_untainted_i64, is_tainted, prim_to_value_disc, scalar_disc, value_disc,
    },
    body::{BodyCx, emit_into_slot, node_composite_source, node_is_bottom},
    lower::{CtxWord, KernelFns, LowerCtx, SiteLayout, TruncLeaf, TruncRec},
    nodes::{call_result_needs_value_widening, emit_bottom_placeholder},
    record::KernelConst,
    scalar::{cast_u64_to_prim, prim_to_clif},
};

/// Emit a fusable call ([`SiteDispatch`]): marshal `args` as (disc,
/// payload) pairs into a stack buffer the trampoline views as
/// `&[Value]`, dispatch, release what this site owned, then decode the
/// returned pair per the static return shape. The trampoline derives
/// the production's tag from the arg taint/stale masks and returns it
/// in-band on the disc: a bottomed or quiet result rides to its
/// consumers as data, never as a whole-kernel abort.
pub(crate) fn emit_builtin_call_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    info: &BuiltinCallSiteInfo,
    args: &[&Node<R, E>],
    order: &[usize],
) -> Result<CompiledExpr> {
    let ret_abi = kernel_abi::abi_kind(&info.return_type);
    if ret_abi.is_none() {
        return Err(anyhow!("emit_clif: call with a non-fusable return"));
    }
    // An argument-less call fires like a constant, which the fast
    // fn's arg-derived tag cannot express.
    if args.is_empty() {
        return Err(anyhow!("emit_clif: a fast fn called with no arguments"));
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
    let mut drops: SmallVec<[OwnedDrop; 8]> = SmallVec::new();
    let mut arg_discs: SmallVec<[Option<ClifValue>; 8]> =
        smallvec::smallvec![None; args.len()];
    // in the call's update order; an owned arg is held while the later
    // args emit (an emission error abandons the body, its holds with it)
    for &i in order {
        let (arg_node, t) = (args[i], &info.arg_types[i]);
        // The buffer is laid out by `info.arg_types`, so only the
        // `AbiKind` needs to agree, not the exact `Type`.
        let Some(frozen) = kernel_abi::freeze_for_abi_normalized(arg_node.typ()) else {
            return Err(anyhow!(
                "emit_clif: call arg type {} doesn't freeze concrete",
                arg_node.typ()
            ));
        };
        let kind = kernel_abi::abi_kind(t);
        if kernel_abi::abi_kind(&frozen) != kind {
            return Err(anyhow!(
                "emit_clif: call arg shape {frozen:?} disagrees with the \
                 discovered arg type {t}"
            ));
        }
        let cv = arg_node.emit_clif(cx)?;
        arg_discs[i] = Some(cv.disc);
        // The stored disc must be clean or the fn sees a corrupt Value;
        // the arg's own disc carries its taint to the masks. The fn
        // borrows the buffer, so owned args are released after the call.
        let (disc, payload) = match kind {
            Some(AbiKind::Scalar(p)) => {
                (cx.b.ins().iconst(types::I64, prim_to_value_disc(p)), cv.payload)
            }
            Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                (cx.b.ins().iconst(types::I64, value_disc::ARRAY), cv.payload)
            }
            Some(AbiKind::String) => {
                (cx.b.ins().iconst(types::I64, value_disc::STRING), cv.payload)
            }
            // A bare-null arg is a value-shape pair with the Null disc.
            Some(
                AbiKind::Variant | AbiKind::Nullable | AbiKind::Value | AbiKind::Null,
            ) => (clean_disc(cx.b, cv.disc), cv.payload),
            Some(AbiKind::Unit) => {
                return Err(anyhow!("emit_clif: call arg has Unit type"));
            }
            None => {
                return Err(anyhow!("emit_clif: call arg with non-fusable type"));
            }
        };
        if let Some(k) =
            kind.and_then(|k| owned_drop_kind(k, node_composite_source(arg_node)))
        {
            hold_owned(cx, &mut drops, (k, cv));
        }
        cx.b.ins().stack_store(disc, slot, (16 * i) as i32);
        cx.b.ins().stack_store(payload, slot, (16 * i + 8) as i32);
    }
    release_owned(cx, drops.len());
    let arg_discs =
        arg_discs.into_iter().collect::<Option<SmallVec<[ClifValue; 8]>>>().ok_or_else(
            || anyhow!("emit_clif: a call's update order misses an argument"),
        )?;
    // A tainted arg is a mask bit, not a reason to skip emission; the
    // trampoline bottoms the call.
    let mut taint_mask = cx.b.ins().iconst(types::I64, 0);
    for (i, d) in arg_discs.iter().enumerate() {
        let t = is_tainted(cx.b, *d);
        let t64 = cx.b.ins().uextend(types::I64, t);
        let bit = cx.b.ins().ishl_imm(t64, i as i64);
        taint_mask = cx.b.ins().bor(taint_mask, bit);
    }
    // Bit `i` set = the arg is present but did not fire this cycle.
    // Suppressed under genuine init only (init minus wake): at init
    // every input is born; a wake delivers standing inputs stale.
    let genuine = {
        let (init, wake) = (cx.born_flag(), cx.ctx.wake_flag);
        let init_b = cx.b.ins().icmp_imm(IntCC::NotEqual, init, 0);
        let no_wake = cx.b.ins().icmp_imm(IntCC::Equal, wake, 0);
        cx.b.ins().band(init_b, no_wake)
    };
    // an `Io` call answers its trigger alone: every other argument reads
    // as stale
    let trigger = match &info.dispatch {
        SiteDispatch::Io { trigger, .. } => Some(*trigger),
        _ => None,
    };
    let stale_mask = {
        let mut stale_mask = cx.b.ins().iconst(types::I64, 0);
        for (i, d) in arg_discs.iter().enumerate() {
            let s = cx.b.ins().band_imm(*d, FIRE_TEST);
            let sb = cx.b.ins().icmp_imm(IntCC::NotEqual, s, 0);
            let s64 = cx.b.ins().uextend(types::I64, sb);
            let bit = cx.b.ins().ishl_imm(s64, i as i64);
            stale_mask = cx.b.ins().bor(stale_mask, bit);
        }
        let zero = cx.b.ins().iconst(types::I64, 0);
        let stale_mask = cx.b.ins().select(genuine, zero, stale_mask);
        match trigger {
            Some(t) => {
                let others = (u64::MAX >> (64 - args.len())) & !(1u64 << t);
                cx.b.ins().bor_imm(stale_mask, others as i64)
            }
            None => stale_mask,
        }
    };
    let base = cx.b.ins().stack_addr(types::I64, slot, 0);
    let n = cx.b.ins().iconst(types::I64, args.len() as i64);
    let call = match &info.dispatch {
        SiteDispatch::Fast { name, f } => {
            let fp = cx.const_ptr(KernelConst::FastFn { name: name.clone(), f: *f })?;
            cx.call_helper("graphix_fastcall", &[fp, base, n, taint_mask, stale_mask])?
        }
        SiteDispatch::Io { name, f, trigger, arg, typ } => {
            let fp = cx.const_ptr(KernelConst::FastFn { name: name.clone(), f: *f })?;
            let site = crate::IoSite::new(name.clone(), arg.clone(), typ.clone());
            let site = cx.const_ptr(KernelConst::IoSite(Box::new(site)))?;
            let bit = cx.b.ins().iconst(types::I64, 1i64 << *trigger);
            cx.call_helper(
                "graphix_iocall",
                &[fp, site, base, n, taint_mask, stale_mask, bit],
            )?
        }
        SiteDispatch::Typed { name, f, typ } => {
            let fp = cx.const_ptr(KernelConst::TypedFn { name: name.clone(), f: *f })?;
            let tp = cx.interned_type(&Type::Fn(typ.clone()))?;
            cx.call_helper(
                "graphix_typedcall",
                &[fp, tp, base, n, taint_mask, stale_mask],
            )?
        }
        SiteDispatch::Cast { target, source } => {
            let fp = cx.const_ptr(KernelConst::Cast)?;
            let pair =
                Type::Tuple(triomphe::Arc::from_iter([target.clone(), source.clone()]));
            let tp = cx.interned_type(&pair)?;
            cx.call_helper(
                "graphix_castcall",
                &[fp, tp, base, n, taint_mask, stale_mask],
            )?
        }
    };
    emit_owned_drops(cx.b, cx.ctx, &drops)?;
    let (raw0, raw1) = (call[0], call[1]);
    let dmerge = cx.b.create_block();
    let pay_ty = match ret_abi {
        Some(AbiKind::Scalar(p)) => prim_to_clif(p),
        _ => types::I64,
    };
    cx.b.append_block_param(dmerge, types::I64);
    cx.b.append_block_param(dmerge, pay_ty);
    // The returned disc's TAINT/STALE bits are the production's tag.
    let tagbits = cx.b.ins().band_imm(raw0, TAINT | STALE);
    match ret_abi {
        Some(AbiKind::Scalar(p)) => {
            // A bottom's placeholder payload is harmless garbage, guarded
            // by its TAINT bit.
            let value = cast_u64_to_prim(cx.b, raw1, p);
            let base = scalar_disc(cx.b, p);
            let disc = cx.b.ins().bor(base, tagbits);
            cx.b.ins().jump(dmerge, &[BlockArg::Value(disc), BlockArg::Value(value)]);
        }
        Some(AbiKind::Unit | AbiKind::Null) => {
            // The tag still rides so a bound unit local reads honestly.
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
            // A bottom return carries only a placeholder payload: never
            // adopt it. A String/composite return whose disc is the wrong
            // shape violated its declared type; adopting its payload as
            // ArcStr/ValArray bits would be UB, so it takes the same path.
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
            // An untainted result of the wrong shape is an invariant
            // violation; the helper does not return. A bottom's
            // placeholder pair owns nothing and passes.
            {
                let untainted = cx.b.ins().icmp_imm(IntCC::Equal, t, 0);
                let bad_shape_bl = cx.b.create_block();
                let cont_bl = cx.b.create_block();
                cx.b.ins().brif(untainted, bad_shape_bl, &[], cont_bl, &[]);
                cx.b.switch_to_block(bad_shape_bl);
                cx.b.seal_block(bad_shape_bl);
                cx.call_helper("graphix_shape_mismatch", &[raw0])?;
                cx.b.ins().jump(cont_bl, &[]);
                cx.b.switch_to_block(cont_bl);
                cx.b.seal_block(cont_bl);
            }
            let ph = emit_bottom_placeholder(cx, &info.return_type, &[tagbits])?;
            cx.b.ins()
                .jump(dmerge, &[BlockArg::Value(ph.disc), BlockArg::Value(ph.payload)]);
            cx.b.switch_to_block(ok_bl);
            cx.b.seal_block(ok_bl);
            let (disc, pay) = match ret_abi {
                Some(AbiKind::String) => {
                    let base = cx.b.ins().iconst(types::I64, value_disc::STRING);
                    (cx.b.ins().bor(base, tagbits), raw1)
                }
                Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                    let base = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
                    (cx.b.ins().bor(base, tagbits), raw1)
                }
                // `raw0` is the real Value disc with the tag already in-band
                _ => (raw0, raw1),
            };
            cx.b.ins().jump(dmerge, &[BlockArg::Value(disc), BlockArg::Value(pay)]);
        }
        None => unreachable!("refused above"),
    }
    cx.b.switch_to_block(dmerge);
    cx.b.seal_block(dmerge);
    let params = cx.b.block_params(dmerge);
    Ok(CompiledExpr::new(params[0], params[1]))
}

/// Where a composite expression's pointer came from: whether a transfer
/// into a slot needs a refcount bump (`Borrowed`) or not (`Owned`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompositeSource {
    /// A fresh owned pointer; transfer as-is.
    Owned,
    /// A read of a binding that still owns the pointer; clone before
    /// transferring or a later drop frees the shared buffer.
    Borrowed,
}

/// An owned value the emitter drops after its consumer: its kind and
/// its words. A call's argument is one: args pass borrowed (the callee
/// clones every composite/value param on entry).
pub(super) type OwnedDrop = (LocalKind, CompiledExpr);

/// Record an owned argument for the drop after its call, and hold it
/// while the arguments after it emit, so a pending exit they take frees
/// it ([`release_owned`] ends the holds once the last one emitted).
pub(super) fn hold_owned(
    cx: &mut BodyCx,
    drops: &mut SmallVec<[OwnedDrop; 8]>,
    d: OwnedDrop,
) {
    drops.push(d);
    cx.hold(d.0, d.1);
}

/// End `n` holds [`hold_owned`] made.
pub(super) fn release_owned(cx: &mut BodyCx, n: usize) {
    for _ in 0..n {
        cx.release();
    }
}

/// One entry in the flat formals+captures list [`emit_lambda_call_node`]
/// marshals: a call-site arg Node or a capture read from the calling
/// kernel's env.
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

/// The callee's context word: born as we are, and on this site's
/// first call ever (the node-walk primes an instance's first dispatch
/// the same way). In a loop each slot is an instance with a first call
/// of its own.
fn emit_callee_context_word(cx: &mut BodyCx, site: ExprId) -> ClifValue {
    let word = match cx.env.loop_depth {
        0 => Some(cx.claim_word()),
        _ => cx.slot_word(site),
    };
    let born = cx.first_use(word);
    CtxWord { born, wake: cx.ctx.wake_flag }.encode(cx.b)
}

/// Claim a contiguous run from this body's channel and rebase the callee's
/// anchors and activation roots onto it.
fn claim_block_run(cx: &mut BodyCx, layout: &SiteLayout) -> ClifValue {
    assert!(layout.words > 0);
    assert_eq!(cx.env.loop_depth, 0);
    let chan = cx.ctx.claims_channel();
    let first = chan.claim();
    for _ in 1..layout.words {
        chan.claim();
    }
    let rel0 = (first / 8) as u32;
    chan.self_blocks.borrow_mut().extend(
        layout.self_blocks.iter().map(|b| kernel_abi::SelfBlock {
            rel: rel0 + b.rel,
            layout: b.layout.clone(),
        }),
    );
    chan.anchors.borrow_mut().extend(layout.anchors.iter().map(|a| {
        kernel_abi::SiteAnchor {
            rel: rel0 + a.rel,
            own_levels: a.own_levels,
            leaf: a.leaf.clone(),
        }
    }));
    cx.claimed_word(first).0
}

/// Emit the per-call-site state block argument for a cross-kernel call
/// (wire slot 2): storage for the callee's interior memory, owned by
/// this caller and sized by the callee's recorded `SiteLayout`.
///
/// - A self-call → a node in a lazily grown per-activation block tree.
/// - Callee claims nothing → `0`; its null-guards give no-memory semantics.
/// - Root call site → a contiguous run of words in this body's own space.
/// - In-loop call site → one block per slot coordinate, the leaf of an
///   owning chain over all open frames, `words` stride per slot.
fn emit_site_block(
    cx: &mut BodyCx,
    info: &LambdaCallInfo,
    is_self: bool,
    site: ExprId,
) -> Result<(ClifValue, Option<ClifValue>)> {
    let key = kernel_abi::kernel_key(&info.kernel);
    let layout = match cx.callee_site_layout(key) {
        // No recorded layout means the callee is still being emitted: a
        // self-call, since mutual cycles de-fuse at the static call
        // edge. Recursion depth is a run-time fact, so a self-call roots
        // a lazily grown tree of per-activation blocks instead of
        // carving a block out of ours.
        None => {
            // Passing 0 would run the callee with no interior memory, a
            // silent divergence; de-fuse loudly instead.
            if !is_self {
                return Err(anyhow!(
                    "emit_clif: non-self recursive edge reached site-block \
                     emission — mutual cycles refuse at the call site"
                ));
            }
            let Some(off) = cx.claim_self_block_word() else {
                return Err(anyhow!(
                    "emit_clif: no per-activation block root for a self-call \
                     (in-loop context) — de-fuse"
                ));
            };
            let base = cx.site_ptr();
            let word = cx.b.ins().iadd_imm(base, off as i64);
            // Our own block can be null; the helper maps null to null.
            let live = cx.b.ins().icmp_imm(IntCC::NotEqual, base, 0);
            let zero = cx.b.ins().iconst(types::I64, 0);
            let word = cx.b.ins().select(live, word, zero);
            // The size is read at run time from `site_block_words`: our own
            // layout is not final while we are still emitting into it.
            if !std::ptr::eq(&*info.kernel, cx.ctx.kernel) {
                return Err(anyhow!(
                    "emit_clif: a self-call site names another kernel — de-fuse"
                ));
            }
            let desc = cx.const_ptr(KernelConst::SiteBlockWords)?;
            let call = cx.call_helper("graphix_site_child_block", &[word, desc])?;
            let rs = call;
            return Ok((rs[0], Some(rs[1])));
        }
        Some(l) => l.clone(),
    };
    claim_callee_block(cx, &layout, site).map(|block| (block, None))
}

/// A static callee's block, carved out of this body's memory: in a loop,
/// its slot's in the table the loop's preheader sized, else (a site the
/// preheader did not see) in the table the chain reaches this iteration.
fn claim_callee_block(
    cx: &mut BodyCx,
    layout: &SiteLayout,
    site: ExprId,
) -> Result<ClifValue> {
    if layout.words == 0 {
        return Ok(cx.b.ins().iconst(types::I64, 0));
    }
    if cx.env.loop_depth == 0 {
        return Ok(claim_block_run(cx, layout));
    }
    let (idx, table) = match cx.callee_block_table(site) {
        Some(t) => t,
        None => {
            let frames: smallvec::SmallVec<[(ClifValue, ClifValue, Variable); 4]> = {
                let fs = cx.ctx.slot_tables.borrow();
                debug_assert_eq!(
                    fs.len(),
                    cx.env.loop_depth as usize,
                    "slot-table frames out of sync with loop depth"
                );
                fs.iter().map(|f| (f.len, f.src_disc, f.idx_var)).collect()
            };
            let n_dirs = frames.len() - 1;
            let (llen, ldisc, lidx) = frames[n_dirs];
            let (table, rec) =
                callee_table(cx, layout, &frames[..n_dirs], (llen, ldisc))?;
            if let Some(f) = cx.ctx.slot_tables.borrow_mut().last_mut() {
                f.pending.push(rec);
            }
            (lidx, table)
        }
    };
    let i = cx.b.use_var(idx);
    let off = cx.b.ins().imul_imm(i, (layout.words as i64) * 8);
    let addr = cx.b.ins().iadd(table, off);
    // a null table is a body with no memory, whose callees have none
    let live = cx.b.ins().icmp_imm(IntCC::NotEqual, table, 0);
    let zero = cx.b.ins().iconst(types::I64, 0);
    Ok(cx.b.ins().select(live, addr, zero))
}

/// A callee's blocks for every slot of the innermost loop `leaf` (its
/// length and source disc), `layout.words` a slot, in a table reached
/// from an anchor of this body through the enclosing loops `dirs`; null
/// when this body has no memory. The truncate record goes with the loop
/// the table is the leaf of: its len-0 epochs never reach this ensure, so
/// every enclosing loop's exit re-ensures it to truncate on a shrink.
pub(super) fn callee_table(
    cx: &mut BodyCx,
    layout: &SiteLayout,
    dirs: &[(ClifValue, ClifValue, Variable)],
    leaf: (ClifValue, ClifValue),
) -> Result<(ClifValue, TruncRec)> {
    let n_dirs = dirs.len();
    let leaf_rt = if layout.anchors.is_empty() && layout.self_blocks.is_empty() {
        None
    } else {
        Some(triomphe::Arc::new(kernel_abi::SiteLeaf {
            stride: layout.words,
            anchors: layout.anchors.clone(),
            self_blocks: layout.self_blocks.clone(),
        }))
    };
    let (trunc, anchor) = cx.claim_anchor(n_dirs as u32, leaf_rt.clone());
    let rec = TruncRec {
        anchor: trunc,
        n_dirs: n_dirs as u32,
        leaf: match &leaf_rt {
            None => TruncLeaf::Table { stride: layout.words },
            Some(l) => TruncLeaf::Blocks(l.clone()),
        },
    };
    let (llen, ldisc) = leaf;
    let emit_chain = |cx: &mut BodyCx, word_addr: ClifValue| -> Result<ClifValue> {
        let leaf_ptr = match &leaf_rt {
            None => cx.b.ins().iconst(types::I64, 0),
            Some(l) => cx.const_ptr(KernelConst::SiteLeaf(l.clone()))?,
        };
        let word_addr = cx.emit_dir_walk(word_addr, dirs, n_dirs, leaf_ptr)?;
        let lvalid = emit_untainted_i64(cx.b, ldisc);
        Ok(match &leaf_rt {
            // the leaf a chunk's own loop sizes is shared by the chunks
            _ if n_dirs == 0 && cx.ctx.chunk => {
                let stride = cx.b.ins().iconst(types::I64, layout.words as i64);
                let words = cx.b.ins().imul(llen, stride);
                cx.call_helper(
                    "graphix_slot_state_table_shared",
                    &[word_addr, words, lvalid],
                )?[0]
            }
            None => {
                let stride = cx.b.ins().iconst(types::I64, layout.words as i64);
                let words = cx.b.ins().imul(llen, stride);
                let own0 = cx.b.ins().iconst(types::I64, 0);
                cx.call_helper(
                    "graphix_slot_state_table",
                    &[word_addr, words, lvalid, own0, leaf_ptr],
                )?[0]
            }
            Some(_) => cx.call_helper(
                "graphix_slot_state_blocks",
                &[word_addr, llen, lvalid, leaf_ptr],
            )?[0],
        })
    };
    let table = emit_chain(cx, anchor.0)?;
    Ok((table, rec))
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

/// The flat formals-then-captures list a cross-kernel call marshals,
/// validated against the callee's signature. Slots are typed from the
/// callee (`info.arg_types`): those types were resolved and frozen at
/// build time, and env is unavailable at emit time to resolve the
/// caller-side node type.
fn call_slots<'a, R: Rt, E: UserEvent>(
    cs: &'a CallSite<R, E>,
    info: &'a LambdaCallInfo,
) -> Result<LPooled<Vec<LambdaCallSlot<'a, R, E>>>> {
    let fn_name = &info.kernel.fn_name;
    let ftype = cs
        .resolved_ftype()
        .or_else(|| cs.ftype())
        .ok_or_else(|| anyhow!("lambda call `{fn_name}`: no resolved FnType"))?;
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
    let mut slots: LPooled<Vec<LambdaCallSlot<R, E>>> = LPooled::take();
    let mut sig_idx = 0usize;
    for i in 0..ftype.args.len() {
        let node = formal_arg(cs, &ftype, i).ok_or_else(|| {
            anyhow!("lambda call `{fn_name}`: missing call-site arg node")
        })?;
        // A skipped fn formal has no callee slot (its uses are
        // statically-resolved calls baked at build time). Its arg node
        // is not emitted, so one carrying an effect must de-fuse.
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
    for s in &*slots {
        // Bottom unifies with any signature type, so a Bottom-typed arg
        // node is gated on the node itself.
        if let LambdaCallSlot::Arg(n, _) = s
            && node_is_bottom(n)
        {
            return Err(anyhow!(
                "lambda call `{fn_name}`: Bottom-typed arg in value \
                 position — subtree node-walks"
            ));
        }
        if matches!(
            kernel_abi::abi_kind(s.typ()),
            Some(AbiKind::Unit | AbiKind::Null) | None
        ) {
            return Err(anyhow!(
                "lambda call `{fn_name}`: arg/capture type {} not \
                 lowered on the calling side — subtree node-walks",
                s.typ()
            ));
        }
    }
    Ok(slots)
}

/// Emit each slot as the pair the callee's param expects
/// ([`emit_into_slot`]). Args pass borrowed, so the owned ones are
/// returned to drop after the call.
fn marshal_args<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    cs: &CallSite<R, E>,
    slots: &[LambdaCallSlot<R, E>],
    fn_name: &str,
) -> Result<(SmallVec<[CompiledExpr; 12]>, SmallVec<[OwnedDrop; 8]>)> {
    let mut cvs: SmallVec<[Option<CompiledExpr>; 12]> =
        smallvec::smallvec![None; slots.len()];
    let mut drops: SmallVec<[OwnedDrop; 8]> = SmallVec::new();
    let nodes: SmallVec<[Option<&Node<R, E>>; 12]> = slots
        .iter()
        .map(|s| match s {
            LambdaCallSlot::Arg(n, _) => Some(*n),
            LambdaCallSlot::Cap(_) => None,
        })
        .collect();
    for i in cs.update_order(&nodes) {
        let cv = match &slots[i] {
            LambdaCallSlot::Arg(n, t) => {
                let slot = kernel_abi::abi_kind(t).and_then(LocalKind::of).ok_or_else(|| {
                    anyhow!("lambda call `{fn_name}`: an arg slot of type {t} has no local")
                })?;
                let s = emit_into_slot(cx, n, slot)?;
                if let Some(k) = s.owns {
                    hold_owned(cx, &mut drops, (k, s.cv));
                }
                s.cv
            }
            // Capture reads are borrowed.
            LambdaCallSlot::Cap(c) => {
                let index = cx
                    .env
                    .position(c.bind_id, Some(c.name.as_str()))
                    .ok_or_else(|| {
                        anyhow!(
                            "lambda call `{fn_name}`: capture `{}` not in the calling \
                             kernel's env",
                            c.name
                        )
                    })?;
                let payload = cx.env.locals[index].words.payload;
                CompiledExpr::new(cx.read_disc(index), cx.b.use_var(payload))
            }
        };
        cvs[i] = Some(cv);
    }
    release_owned(cx, drops.len());
    let cvs = cvs.into_iter().collect::<Option<_>>().ok_or_else(|| {
        anyhow!("lambda call `{fn_name}`: the update order misses a slot")
    })?;
    Ok((cvs, drops))
}

/// A checked call of another kernel or of itself: inside the stack red
/// zone it enters the callee on a fresh segment through the callee's
/// spill thunk, since a chain of calls is as deep as the program is long;
/// the same check carries the cooperative interrupt and the stack budget,
/// which abort the run. The call's result pair goes to `rmerge`.
fn emit_kernel_dispatch(
    cx: &mut BodyCx,
    callee: KernelFns<FuncRef>,
    clif_args: &[ClifValue],
    rmerge: Block,
    fn_name: &str,
) -> Result<()> {
    let abort_bl = cx.b.create_block();
    let direct_bl = cx.b.create_block();
    let call_bl = cx.b.create_block();
    let grow_bl = cx.b.create_block();
    let call = cx.call_helper("graphix_stack_check", &[])?;
    let flag = call[0];
    let interrupted = cx.b.ins().icmp_imm(IntCC::Equal, flag, 0);
    cx.b.ins().brif(interrupted, abort_bl, &[], direct_bl, &[]);
    cx.b.switch_to_block(direct_bl);
    cx.b.seal_block(direct_bl);
    let direct = cx.b.ins().icmp_imm(IntCC::Equal, flag, 1);
    cx.b.ins().brif(direct, call_bl, &[], grow_bl, &[]);
    cx.b.switch_to_block(abort_bl);
    cx.b.seal_block(abort_bl);
    // an interrupt or the stack budget aborts the run, which rides its
    // last result as the node-walk's interrupted dispatch does: the zero
    // pair takes the pending path after `rmerge`, which drops the args
    cx.call_helper("graphix_abort_set", &[])?;
    let zero = cx.b.ins().iconst(types::I64, 0);
    cx.b.ins().jump(rmerge, &[BlockArg::Value(zero), BlockArg::Value(zero)]);
    cx.b.switch_to_block(call_bl);
    cx.b.seal_block(call_bl);
    let inst = cx.b.ins().call(callee.body, clif_args);
    let (r0, r1) = callee_results(cx, inst, fn_name)?;
    cx.b.ins().jump(rmerge, &[BlockArg::Value(r0), BlockArg::Value(r1)]);
    cx.b.switch_to_block(grow_bl);
    cx.b.seal_block(grow_bl);
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
    let thunk = cx.b.ins().func_addr(types::I64, callee.thunk);
    cx.call_helper("graphix_grow_stack", &[thunk, base, out])?;
    let r0 = cx.b.ins().load(types::I64, MemFlags::trusted(), out, 0);
    let r1 = cx.b.ins().load(types::I64, MemFlags::trusted(), out, 8);
    cx.b.ins().jump(rmerge, &[BlockArg::Value(r0), BlockArg::Value(r1)]);
    Ok(())
}

/// The kernel a lambda call site calls: another function's, or this
/// self-recursive body's own.
pub(crate) enum Called<'a> {
    Other(&'a LambdaCallInfo),
    Own(OwnCall<'a>),
}

pub(crate) fn emit_lambda_call_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    cs: &CallSite<R, E>,
    called: Called<'_>,
) -> Result<CompiledExpr> {
    let (info, own) = match called {
        Called::Other(info) => (info, None),
        Called::Own(own) => (own.info, Some(own)),
    };
    let is_self = own.is_some();
    let fn_name = &info.kernel.fn_name;
    let slots = call_slots(cs, info)?;
    let ret = &info.kernel.return_type;
    // the callsite node's type may promise a 2-word Value where the
    // callee ABI returns a narrower shape
    let widen = call_result_needs_value_widening(cx, cs.typ(), ret)?;
    let (slot_cvs, drops) = marshal_args(cx, cs, &slots, fn_name)?;
    let mut clif_args: SmallVec<[ClifValue; 24]> =
        SmallVec::with_capacity(slot_cvs.len() * 2 + 3);
    let context = emit_callee_context_word(cx, cs.spec().id);
    let (site_block, fresh) = emit_site_block(cx, info, is_self, cs.spec().id)?;
    // a self-call's activation made just now is born with the init view
    let context = match fresh {
        None => context,
        Some(fresh) => cx.b.ins().bor(context, fresh),
    };
    clif_args.push(context);
    clif_args.push(cx.state_ptr());
    clif_args.push(site_block);
    for cv in slot_cvs.iter() {
        clif_args.push(cv.disc);
        clif_args.push(cv.payload);
    }
    let callee =
        *cx.ctx.callee_refs.get(&kernel_abi::kernel_key(&info.kernel)).ok_or_else(|| {
            anyhow!("lambda call `{fn_name}`: callee_refs has no entry — discovery/declare drift")
        })?;
    let rmerge = cx.b.create_block();
    cx.b.append_block_param(rmerge, types::I64);
    cx.b.append_block_param(rmerge, types::I64);
    if callee.checked {
        emit_kernel_dispatch(cx, callee, &clif_args, rmerge, fn_name)?;
    } else {
        let inst = cx.b.ins().call(callee.body, &clif_args);
        let (r0, r1) = callee_results(cx, inst, fn_name)?;
        cx.b.ins().jump(rmerge, &[BlockArg::Value(r0), BlockArg::Value(r1)]);
    }
    cx.b.switch_to_block(rmerge);
    cx.b.seal_block(rmerge);
    let r0 = cx.b.block_params(rmerge)[0];
    let r1 = cx.b.block_params(rmerge)[1];
    // An aborted callee left `KERNEL_ABORT` set and returned the zero
    // pair (disc 0 is never a real value): drop what we own and jump to
    // `pending_exit` with the flag still set so `FusedKernel::update`
    // discards.
    {
        let pending = cx.b.ins().icmp_imm(IntCC::Equal, r0, 0);
        let abort_bl = cx.b.create_block();
        let cont_bl = cx.b.create_block();
        cx.b.ins().brif(pending, abort_bl, &[], cont_bl, &[]);
        cx.b.switch_to_block(abort_bl);
        cx.b.seal_block(abort_bl);
        emit_owned_drops(cx.b, cx.ctx, &drops)?;
        emit_pending_abort(cx.b, cx.env, cx.ctx)?;
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
    emit_owned_drops(cx.b, cx.ctx, &drops)?;
    #[cfg(debug_assertions)]
    if crate::dbgenv::gxdbg_callret() {
        use crate::fusion::emit_helpers::CallRetTag;
        let t = cx.b.ins().iconst(types::I64, CallRetTag::CallResult as i64);
        cx.call_helper("graphix_dbg_disc", &[t, result.disc])?;
    }
    Ok(result)
}

/// What a production of `kind` from a node of `source` leaves its
/// consumer to drop: a string read is always owned; a composite or a
/// value only when the node made it; a scalar owns nothing.
pub(super) fn owned_drop_kind(
    kind: AbiKind,
    source: CompositeSource,
) -> Option<LocalKind> {
    match kind {
        AbiKind::String => Some(LocalKind::String),
        _ if source != CompositeSource::Owned => None,
        AbiKind::Array | AbiKind::Tuple | AbiKind::Struct => Some(LocalKind::Composite),
        AbiKind::Variant | AbiKind::Nullable | AbiKind::Value => Some(LocalKind::Value),
        AbiKind::Scalar(_) | AbiKind::Unit | AbiKind::Null => None,
    }
}

/// Drop an owned value of `kind` (`cv` its words): the one per-kind
/// drop dispatch.
pub(super) fn emit_owned_drop(
    b: &mut FunctionBuilder,
    ctx: &LowerCtx,
    kind: LocalKind,
    cv: CompiledExpr,
) -> Result<()> {
    let (helper, args): (&str, SmallVec<[ClifValue; 2]>) = match kind {
        LocalKind::Scalar(_) => return Ok(()),
        LocalKind::Composite => {
            ("graphix_valarray_drop", smallvec::smallvec![cv.payload])
        }
        LocalKind::String => ("graphix_arcstr_drop", smallvec::smallvec![cv.payload]),
        LocalKind::Value => {
            ("graphix_value_drop", smallvec::smallvec![cv.disc, cv.payload])
        }
    };
    ctx.call_helper(b, helper, &args)?;
    Ok(())
}

pub(super) fn emit_owned_drops(
    b: &mut FunctionBuilder,
    ctx: &LowerCtx,
    drops: &[OwnedDrop],
) -> Result<()> {
    drops.iter().try_for_each(|(kind, cv)| emit_owned_drop(b, ctx, *kind, *cv))
}

/// [`emit_owned_drop`] of an owned local of `kind` held in `vv`.
pub(super) fn emit_drop_local(
    b: &mut FunctionBuilder,
    ctx: &LowerCtx,
    kind: LocalKind,
    vv: ValueVar,
) -> Result<()> {
    let cv = CompiledExpr::new(b.use_var(vv.disc), b.use_var(vv.payload));
    emit_owned_drop(b, ctx, kind, cv)
}

/// Which helper family a buffer between its `_new` and its finalize
/// belongs to.
#[derive(Debug, Clone, Copy)]
pub(super) enum BufKind {
    /// A `graphix_value_buf`.
    Value,
    /// A `graphix_string_buf`.
    String,
}

/// Register the fresh buf `buf` so an abort before its finalize drops
/// it; [`close_buf`] unregisters the innermost one.
pub(super) fn open_buf(cx: &mut BodyCx, kind: BufKind, buf: ClifValue) {
    let var = cx.b.declare_var(types::I64);
    cx.b.def_var(var, buf);
    cx.ctx.in_flight_bufs.borrow_mut().push((kind, var));
}

/// Unregister the innermost in-flight buf; emit right before the call
/// that consumes it.
pub(super) fn close_buf(cx: &mut BodyCx) {
    cx.ctx.in_flight_bufs.borrow_mut().pop();
}

/// A fresh registered value buf of capacity `cap`.
pub(super) fn open_value_buf(cx: &mut BodyCx, cap: ClifValue) -> Result<ClifValue> {
    let call = cx.call_helper("graphix_value_buf_new", &[cap])?;
    let buf = call[0];
    open_buf(cx, BufKind::Value, buf);
    Ok(buf)
}

/// Finalize the innermost in-flight value buf into owned ValArray bits.
pub(super) fn finalize_valarray(cx: &mut BodyCx, buf: ClifValue) -> Result<ClifValue> {
    close_buf(cx);
    let call = cx.call_helper("graphix_valarray_finalize", &[buf])?;
    Ok(call[0])
}

/// Leave the kernel from the current block for its pending exit,
/// dropping everything it owns: the in-flight bufs and the held inputs
/// here, then each owned local, newest first, down a ladder of blocks
/// every abort shares, so the code is linear in the locals and the exits.
pub(super) fn emit_pending_abort(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
    for (kind, var) in ctx.in_flight_bufs.borrow().iter() {
        let helper = match kind {
            BufKind::Value => "graphix_value_buf_drop",
            BufKind::String => "graphix_string_buf_drop",
        };
        let ptr = b.use_var(*var);
        ctx.call_helper(b, helper, &[ptr])?;
    }
    for (kind, vv) in ctx.owned_input_stack.borrow().iter() {
        emit_drop_local(b, ctx, *kind, *vv)?;
    }
    let owned: SmallVec<[(usize, LocalKind, ValueVar); 8]> = env
        .locals_above(ctx.owned_floor)
        .enumerate()
        .filter(|(_, (kind, _))| !matches!(kind, LocalKind::Scalar(_)))
        .map(|(i, (kind, vv))| (ctx.owned_floor + i, kind, vv))
        .collect();
    let exit = super::body::pending_exit_block(b, ctx);
    // the rung of each owned local, newest first, each going on to the
    // next; a rung made before is filled already, and so is all below it
    let mut fill: SmallVec<[(Block, LocalKind, ValueVar, Block); 8]> = SmallVec::new();
    let mut next = exit;
    for (i, kind, vv) in owned.iter() {
        let key = (*i, vv.payload.index());
        let rung = ctx.ladder.borrow().get(&key).copied();
        next = match rung {
            Some(r) => r,
            None => {
                let r = b.create_block();
                ctx.ladder.borrow_mut().insert(key, r);
                fill.push((r, *kind, *vv, next));
                r
            }
        };
    }
    b.ins().jump(next, &[]);
    for (rung, kind, vv, below) in fill.drain(..) {
        b.switch_to_block(rung);
        emit_drop_local(b, ctx, kind, vv)?;
        b.ins().jump(below, &[]);
    }
    Ok(())
}
