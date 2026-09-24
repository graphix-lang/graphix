//! Call emission: cross-kernel lambda calls (site blocks, arg
//! marshalling, drops, pending cleanup) and the direct fastcall /
//! typed-fastcall path.

use crate::{
    Node, Rt, Update, UserEvent,
    fusion::{
        LambdaCallInfo,
        kernel_abi::{self, AbiKind},
        lowering::{BuiltinCallSiteInfo, CaptureSlot, SiteDispatch, cast_typed},
    },
    node::callsite::CallSite,
    typ::{FnArgKind, Type},
};
use anyhow::{Result, anyhow};
use cranelift_codegen::ir::{
    Block, BlockArg, FuncRef, Inst, InstBuilder, MemFlags, StackSlotData, StackSlotKind,
    Value as ClifValue, condcodes::IntCC, types,
};
use cranelift_frontend::{FunctionBuilder, Variable};
use poolshark::local::LPooled;
use smallvec::SmallVec;

use super::{
    abi::{
        CompiledExpr, JitEnv, LocalKind, STALE, TAINT, ValueVar, clean_disc,
        emit_untainted_i64, is_tainted, prim_to_value_disc, scalar_disc, value_disc,
    },
    body::{BodyCx, node_composite_source, node_is_bottom, pending_exit_block},
    lower::{Channel, LowerCtx, SelWord, SiteLayout, TruncAnchor, TruncLeaf, TruncRec},
    nodes::{call_result_needs_value_widening, emit_bottom_placeholder},
    record::KernelConst,
    scalar::{cast_u64_to_prim, prim_to_clif, scalar_to_payload_i64},
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
    let mut arg_discs: smallvec::SmallVec<[ClifValue; 8]> = smallvec::SmallVec::new();
    for (i, (arg_node, t)) in args.iter().zip(info.arg_types.iter()).enumerate() {
        // The buffer is laid out by `info.arg_types`, so only the
        // `AbiKind` needs to agree, not the exact `Type`.
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
        // The stored disc must be clean or the fn sees a corrupt Value;
        // the arg's own disc carries its taint to the masks. The fn
        // borrows the buffer, so owned args are released after the call.
        let (disc, payload) = match kind {
            Some(AbiKind::Scalar(p)) => {
                (cx.b.ins().iconst(types::I64, prim_to_value_disc(p)), cv.payload)
            }
            Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                if node_composite_source(arg_node) == CompositeSource::Owned {
                    drops.push(("graphix_valarray_drop", cv.payload, None));
                }
                (cx.b.ins().iconst(types::I64, value_disc::ARRAY), cv.payload)
            }
            Some(AbiKind::String) => {
                drops.push(("graphix_arcstr_drop", cv.payload, None));
                (cx.b.ins().iconst(types::I64, value_disc::STRING), cv.payload)
            }
            // A bare-null arg is a value-shape pair with the Null disc.
            Some(
                AbiKind::Variant | AbiKind::Nullable | AbiKind::Value | AbiKind::Null,
            ) => {
                let disc = clean_disc(cx.b, cv.disc);
                if node_composite_source(arg_node) == CompositeSource::Owned {
                    drops.push(("graphix_value_drop", disc, Some(cv.payload)));
                }
                (disc, cv.payload)
            }
            Some(AbiKind::Unit) => {
                return Err(anyhow!("emit_clif: call arg has Unit type"));
            }
            None => {
                return Err(anyhow!("emit_clif: call arg with non-fusable type"));
            }
        };
        cx.b.ins().stack_store(disc, slot, (16 * i) as i32);
        cx.b.ins().stack_store(payload, slot, (16 * i + 8) as i32);
    }
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
        SiteDispatch::Fast { name, f } => {
            let fast = cx.helper("graphix_fastcall")?;
            let fp = cx.const_ptr(KernelConst::FastFn { name: name.clone(), f: *f })?;
            cx.b.ins().call(fast, &[fp, base, n, taint_mask, stale_mask])
        }
        SiteDispatch::Typed { name, f, typ } => {
            let typed = cx.helper("graphix_typedcall")?;
            let fp = cx.const_ptr(KernelConst::TypedFn { name: name.clone(), f: *f })?;
            let tp = cx.interned_type(typ)?;
            cx.b.ins().call(typed, &[fp, tp, base, n, taint_mask, stale_mask])
        }
        SiteDispatch::Cast(typ) => {
            let typed = cx.helper("graphix_typedcall")?;
            let fp = cx.const_ptr(KernelConst::Cast(cast_typed))?;
            let tp = cx.interned_type(typ)?;
            cx.b.ins().call(typed, &[fp, tp, base, n, taint_mask, stale_mask])
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
        Some(AbiKind::Unit) => {
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
                let mismatch_h = cx.helper("graphix_shape_mismatch")?;
                cx.b.ins().call(mismatch_h, &[raw0]);
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
                // `raw0` is the real Value disc with the tag already in-band.
                _ => (raw0, raw1),
            };
            cx.b.ins().jump(dmerge, &[BlockArg::Value(disc), BlockArg::Value(pay)]);
        }
        Some(AbiKind::Null) | None => unreachable!("refused above"),
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

/// One owned cross-kernel-call arg to drop after the call returns: args
/// pass borrowed (the callee clones every composite/value param on
/// entry), so an owned-source arg's original would otherwise leak.
enum CallArgDrop {
    Composite(ClifValue),
    String(ClifValue),
    Value { disc: ClifValue, payload: ClifValue },
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

/// The callee's context word: our init view, forced on this site's
/// first call ever (the node-walk primes an instance's first dispatch
/// the same way), plus the inherited quiet bit.
fn emit_callee_context_word(cx: &mut BodyCx) -> ClifValue {
    let quiet = cx.quiet_flag();
    // XCR claude for eric: kept as built (kernel_instance_state.md; the contract 3
    // text was the stale one, fixed). An init view only sets constants' FIRED bits,
    // and a late first call rides a fire that already reaches the output (the
    // select's scrutinee or guard, the loop's resize); probes p_first_call*.gx agree.
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
    cx.b.ins().bor(callee_init, quiet_bit)
}

/// Claim a contiguous run of `layout.words` words from this body's own
/// channel for a callee's block and rebase the callee's anchors and
/// activation roots onto it; the run's address, `None` when this body
/// claims nothing.
fn claim_block_run(cx: &mut BodyCx, layout: &SiteLayout) -> Option<ClifValue> {
    let claim = |cx: &BodyCx| match cx.ctx.claims {
        Channel::State => cx.claim_state_word(),
        Channel::Site => cx.claim_site_word(),
    };
    let first = claim(cx)?;
    for _ in 1..layout.words {
        claim(cx).expect("contiguous claims can't fail mid-run");
    }
    let rel0 = (first / 8) as u32;
    let chan = cx.ctx.claims_channel();
    chan.self_blocks.borrow_mut().extend(layout.self_blocks.iter().map(|b| {
        kernel_abi::SelfBlock {
            rel: rel0 + b.rel,
            words: b.words,
            slots: b.slots.clone(),
        }
    }));
    chan.anchors.borrow_mut().extend(layout.anchors.iter().map(|a| {
        kernel_abi::SiteAnchor {
            rel: rel0 + a.rel,
            own_levels: a.own_levels,
            leaf: a.leaf.clone(),
        }
    }));
    let base = chan.ptr;
    let addr = cx.b.ins().iadd_imm(base, first as i64);
    Some(match cx.ctx.claims {
        Channel::State => addr,
        // Our own block may be 0; forward 0, not a garbage offset.
        Channel::Site => {
            let has = cx.b.ins().icmp_imm(IntCC::NotEqual, base, 0);
            let zero = cx.b.ins().iconst(types::I64, 0);
            cx.b.ins().select(has, addr, zero)
        }
    })
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
) -> Result<ClifValue> {
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
            let f = cx.helper("graphix_site_child_block")?;
            let call = cx.b.ins().call(f, &[word, desc]);
            return Ok(cx.b.inst_results(call)[0]);
        }
        Some(l) => l.clone(),
    };
    if layout.words == 0 {
        return Ok(cx.b.ins().iconst(types::I64, 0));
    }
    if cx.env.loop_depth == 0 {
        return Ok(match claim_block_run(cx, &layout) {
            Some(addr) => addr,
            None => cx.b.ins().iconst(types::I64, 0),
        });
    }
    // In-loop call site: the chain runs per innermost iteration; the
    // ensures are idempotent after the first.
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
    let (dirs, leaf_frame) = (&frames[..n_dirs], frames[n_dirs]);
    let leaf_rt = if layout.anchors.is_empty() {
        None
    } else {
        Some(triomphe::Arc::new(kernel_abi::SiteLeaf {
            stride: layout.words,
            anchors: layout.anchors.clone(),
        }))
    };
    // This chain's per-iteration ensure never runs on a len-0 epoch, so
    // every enclosing loop's exit re-ensures it at its level
    // (`BodyCx::emit_slot_truncates`) to truncate on shrink.
    let trunc_rec = |anchor| TruncRec {
        anchor,
        n_dirs: n_dirs as u32,
        leaf: match &leaf_rt {
            None => TruncLeaf::Table { stride: layout.words },
            Some(l) => TruncLeaf::Blocks(l.clone()),
        },
    };
    let anchor = match cx.claim_state_word_loop_invariant() {
        Some(off) => {
            cx.ctx.state.anchors.borrow_mut().push(kernel_abi::SiteAnchor {
                rel: (off / 8) as u32,
                own_levels: n_dirs as u32,
                leaf: leaf_rt.clone(),
            });
            if let Some(f) = cx.ctx.slot_tables.borrow_mut().last_mut() {
                f.pending.push(trunc_rec(TruncAnchor::State(off)));
            }
            let sp = cx.state_ptr();
            SelWord::Sure(cx.b.ins().iadd_imm(sp, off as i64))
        }
        None => match cx.claim_site_anchor(n_dirs as u32, leaf_rt.clone()) {
            Some(off) => {
                if let Some(f) = cx.ctx.slot_tables.borrow_mut().last_mut() {
                    f.pending.push(trunc_rec(TruncAnchor::Site(off)));
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
            Some(l) => cx.const_ptr(KernelConst::SiteLeaf(l.clone()))?,
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
                "lambda call `{fn_name}`: arg/capture type {:?} not \
                 lowered on the calling side — subtree node-walks",
                s.typ()
            ));
        }
    }
    Ok(slots)
}

/// Emit each slot as the pair the callee's param expects: a scalar fed
/// to a value-shaped slot widens its payload word to the Value encoding
/// (a composite/string pair already is one). Args pass borrowed, so the
/// owned ones are returned to drop after the call.
fn marshal_args<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    slots: &[LambdaCallSlot<R, E>],
    fn_name: &str,
) -> Result<(SmallVec<[CompiledExpr; 12]>, SmallVec<[CallArgDrop; 8]>)> {
    let mut cvs: SmallVec<[CompiledExpr; 12]> = SmallVec::new();
    let mut drops: SmallVec<[CallArgDrop; 8]> = SmallVec::new();
    for s in slots.iter() {
        let slot_kind = kernel_abi::abi_kind(s.typ());
        let value_slot = matches!(
            slot_kind,
            Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value)
        );
        let cv = match s {
            LambdaCallSlot::Arg(n, _) => {
                let cv = n.emit_clif(cx)?;
                match kernel_abi::abi_kind(n.typ()) {
                    Some(AbiKind::Scalar(p)) if value_slot => CompiledExpr::new(
                        cv.disc,
                        scalar_to_payload_i64(cx.b, p, cv.payload),
                    ),
                    // String arg emissions are always owned (local reads
                    // clone at the read); a scalar-widened arg owns nothing.
                    _ => {
                        match slot_kind {
                            Some(AbiKind::String) => {
                                drops.push(CallArgDrop::String(cv.payload))
                            }
                            _ if node_composite_source(n) != CompositeSource::Owned => (),
                            Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                                drops.push(CallArgDrop::Composite(cv.payload))
                            }
                            Some(
                                AbiKind::Variant | AbiKind::Nullable | AbiKind::Value,
                            ) => drops.push(CallArgDrop::Value {
                                disc: cv.disc,
                                payload: cv.payload,
                            }),
                            _ => (),
                        }
                        cv
                    }
                }
            }
            // Capture reads are borrowed.
            LambdaCallSlot::Cap(c) => {
                let vv = cx
                    .env
                    .lookup(c.bind_id, c.name.as_str())
                    .ok_or_else(|| {
                        anyhow!(
                            "lambda call `{fn_name}`: capture `{}` not in the calling \
                             kernel's env",
                            c.name
                        )
                    })?
                    .words;
                CompiledExpr::new(cx.b.use_var(vv.disc), cx.b.use_var(vv.payload))
            }
        };
        cvs.push(cv);
    }
    Ok((cvs, drops))
}

/// A self-call inside the stack red zone re-enters on a fresh segment
/// through the spill thunk; the same check carries the cooperative
/// interrupt, which skips the dispatch with a tainted placeholder of
/// `placeholder` to `dmerge`. The call's result pair goes to `rmerge`.
/// Only self-calls need it: cross-kernel edges are acyclic.
fn emit_self_dispatch(
    cx: &mut BodyCx,
    func_ref: FuncRef,
    clif_args: &[ClifValue],
    drops: &[CallArgDrop],
    placeholder: &Type,
    (dmerge, rmerge): (Block, Block),
    fn_name: &str,
) -> Result<()> {
    let abort_bl = cx.b.create_block();
    let direct_bl = cx.b.create_block();
    let call_bl = cx.b.create_block();
    let grow_bl = cx.b.create_block();
    let call = cx.call_helper("graphix_stack_check", &[])?;
    let flag = cx.b.inst_results(call)[0];
    let interrupted = cx.b.ins().icmp_imm(IntCC::Equal, flag, 0);
    cx.b.ins().brif(interrupted, abort_bl, &[], direct_bl, &[]);
    cx.b.switch_to_block(direct_bl);
    cx.b.seal_block(direct_bl);
    let direct = cx.b.ins().icmp_imm(IntCC::Equal, flag, 1);
    cx.b.ins().brif(direct, call_bl, &[], grow_bl, &[]);
    cx.b.switch_to_block(abort_bl);
    cx.b.seal_block(abort_bl);
    emit_call_arg_drops(cx.b, cx.ctx, drops)?;
    // The abort discards the whole run, so no trigger fold.
    let ph = emit_bottom_placeholder(cx, placeholder, &[])?;
    cx.b.ins().jump(dmerge, &[BlockArg::Value(ph.disc), BlockArg::Value(ph.payload)]);
    cx.b.switch_to_block(call_bl);
    cx.b.seal_block(call_bl);
    let inst = cx.b.ins().call(func_ref, clif_args);
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
    cx.call_helper("graphix_grow_stack", &[thunk, base, out])?;
    let r0 = cx.b.ins().load(types::I64, MemFlags::trusted(), out, 0);
    let r1 = cx.b.ins().load(types::I64, MemFlags::trusted(), out, 8);
    cx.b.ins().jump(rmerge, &[BlockArg::Value(r0), BlockArg::Value(r1)]);
    Ok(())
}

pub(crate) fn emit_lambda_call_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    cs: &CallSite<R, E>,
    info: &LambdaCallInfo,
    is_self: bool,
) -> Result<CompiledExpr> {
    let fn_name = &info.kernel.fn_name;
    let slots = call_slots(cs, info)?;
    let ret = &info.kernel.return_type;
    // The callsite node's type may promise a 2-word Value where the
    // callee ABI returns a narrower shape; both merge edges must carry
    // the widened pairing.
    let node_typ = cs.typ();
    let widen = call_result_needs_value_widening(node_typ, ret);
    let ret_pay_ty = match kernel_abi::abi_kind(ret) {
        Some(AbiKind::Scalar(p)) if !widen => prim_to_clif(p),
        _ => types::I64,
    };
    let (slot_cvs, drops) = marshal_args(cx, &slots, fn_name)?;
    let mut clif_args: SmallVec<[ClifValue; 24]> =
        SmallVec::with_capacity(slot_cvs.len() * 2 + 3);
    clif_args.push(emit_callee_context_word(cx));
    clif_args.push(cx.state_ptr());
    let site_block = emit_site_block(cx, info, is_self)?;
    clif_args.push(site_block);
    for cv in slot_cvs.iter() {
        clif_args.push(cv.disc);
        clif_args.push(cv.payload);
    }
    let func_ref =
        *cx.ctx.callee_refs.get(&kernel_abi::kernel_key(&info.kernel)).ok_or_else(|| {
            anyhow!("lambda call `{fn_name}`: callee_refs has no entry — discovery/declare drift")
        })?;
    let dmerge = cx.b.create_block();
    cx.b.append_block_param(dmerge, types::I64);
    cx.b.append_block_param(dmerge, ret_pay_ty);
    let rmerge = cx.b.create_block();
    cx.b.append_block_param(rmerge, types::I64);
    cx.b.append_block_param(rmerge, types::I64);
    if is_self {
        let placeholder = if widen { node_typ } else { ret };
        emit_self_dispatch(
            cx,
            func_ref,
            &clif_args,
            &drops,
            placeholder,
            (dmerge, rmerge),
            fn_name,
        )?;
    } else {
        let inst = cx.b.ins().call(func_ref, &clif_args);
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
    emit_call_arg_drops(cx.b, cx.ctx, &drops)?;
    cx.b.ins()
        .jump(dmerge, &[BlockArg::Value(result.disc), BlockArg::Value(result.payload)]);
    cx.b.switch_to_block(dmerge);
    cx.b.seal_block(dmerge);
    let disc = cx.b.block_params(dmerge)[0];
    let payload = cx.b.block_params(dmerge)[1];
    #[cfg(debug_assertions)]
    if crate::dbgenv::gxdbg_callret() {
        use crate::fusion::emit_helpers::CallRetTag;
        let t = cx.b.ins().iconst(types::I64, CallRetTag::CallResult as i64);
        cx.call_helper("graphix_dbg_disc", &[t, disc])?;
    }
    Ok(CompiledExpr::new(disc, payload))
}

/// Emit the post-call drops for owned call args.
fn emit_call_arg_drops(
    b: &mut FunctionBuilder,
    ctx: &LowerCtx,
    drops: &[CallArgDrop],
) -> Result<()> {
    if drops.is_empty() {
        return Ok(());
    }
    for d in drops {
        match d {
            CallArgDrop::Composite(bits) => {
                let f = ctx.helper(b, "graphix_valarray_drop")?;
                b.ins().call(f, &[*bits]);
            }
            CallArgDrop::String(bits) => {
                let f = ctx.helper(b, "graphix_arcstr_drop")?;
                b.ins().call(f, &[*bits]);
            }
            CallArgDrop::Value { disc, payload } => {
                let f = ctx.helper(b, "graphix_value_drop")?;
                b.ins().call(f, &[*disc, *payload]);
            }
        }
    }
    Ok(())
}

/// Drop every owned local currently in scope; called at every return
/// point. `emit_kernel_return` makes the result independently owned
/// before calling this, so the returned pointer never aliases a dropped slot.
pub(super) fn drop_owned_composites(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
    let drops: smallvec::SmallVec<[(LocalKind, ValueVar); 8]> =
        env.locals_above(0).collect();
    for (kind, vv) in drops {
        emit_drop_local(b, ctx, kind, vv)?;
    }
    Ok(())
}

/// Emit the runtime drop for one owned local of `kind` held in `vv`;
/// the single per-kind drop dispatch (scalars own nothing).
pub(super) fn emit_drop_local(
    b: &mut FunctionBuilder,
    ctx: &LowerCtx,
    kind: LocalKind,
    vv: ValueVar,
) -> Result<()> {
    match kind {
        LocalKind::Scalar(_) => {}
        LocalKind::Composite => {
            let f = ctx.helper(b, "graphix_valarray_drop")?;
            let ptr = b.use_var(vv.payload);
            b.ins().call(f, &[ptr]);
        }
        LocalKind::String => {
            let f = ctx.helper(b, "graphix_arcstr_drop")?;
            let ptr = b.use_var(vv.payload);
            b.ins().call(f, &[ptr]);
        }
        LocalKind::Value => {
            let f = ctx.helper(b, "graphix_value_drop")?;
            let disc = b.use_var(vv.disc);
            let payload = b.use_var(vv.payload);
            b.ins().call(f, &[disc, payload]);
        }
    }
    Ok(())
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
    let buf = cx.b.inst_results(call)[0];
    open_buf(cx, BufKind::Value, buf);
    Ok(buf)
}

/// Finalize the innermost in-flight value buf into owned ValArray bits.
pub(super) fn finalize_valarray(cx: &mut BodyCx, buf: ClifValue) -> Result<ClifValue> {
    close_buf(cx);
    let call = cx.call_helper("graphix_valarray_finalize", &[buf])?;
    Ok(cx.b.inst_results(call)[0])
}

/// Emit drops for everything the kernel currently owns, for a
/// whole-kernel abort path: the in-flight bufs, the owned HOF inputs,
/// then `drop_owned_composites`.
pub(super) fn emit_pending_cleanup(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
    for (kind, var) in ctx.in_flight_bufs.borrow().iter() {
        let f = match kind {
            BufKind::Value => ctx.helper(b, "graphix_value_buf_drop")?,
            BufKind::String => ctx.helper(b, "graphix_string_buf_drop")?,
        };
        let ptr = b.use_var(*var);
        b.ins().call(f, &[ptr]);
    }
    // In-flight HOF inputs are finished ValArrays, not value bufs.
    for arr_var in ctx.owned_input_stack.borrow().iter() {
        let f = ctx.helper(b, "graphix_valarray_drop")?;
        let ptr = b.use_var(*arr_var);
        b.ins().call(f, &[ptr]);
    }
    drop_owned_composites(b, env, ctx)
}
