//! Call emission: cross-kernel lambda calls (site blocks, arg
//! marshalling, drops, pending cleanup) and the direct fastcall /
//! typed-fastcall path.

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
    nodes::{call_result_needs_value_widening, emit_bottom_placeholder},
    scalar::{cast_u64_to_prim, prim_to_clif, scalar_to_payload_i64},
};

/// The `Value` discriminant word of a register scalar's variant, stored
/// beside a scalar arg's bits so the trampoline's `&[Value]` view reads
/// a genuine `Value::I64(..)` etc.
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

/// `Value::Array`'s / `Value::String`'s discriminant words.
static ARRAY_VALUE_DISC: std::sync::LazyLock<(u64,)> = std::sync::LazyLock::new(|| {
    let v = Value::Array(netidx_value::ValArray::from_iter_exact(std::iter::empty()));
    (crate::tval::value_words(&v)[0],)
});
static STRING_VALUE_DISC: std::sync::LazyLock<(u64,)> = std::sync::LazyLock::new(|| {
    (crate::tval::value_words(&Value::String(arcstr::ArcStr::new()))[0],)
});

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
        SiteDispatch::Fast(f) => {
            let fast = cx.helper("graphix_fastcall")?;
            let fp = cx.b.ins().iconst(types::I64, *f as usize as i64);
            cx.b.ins().call(fast, &[fp, base, n, taint_mask, stale_mask])
        }
        SiteDispatch::Typed(f, typ) => {
            let typed = cx.helper("graphix_typedcall")?;
            let fp = cx.b.ins().iconst(types::I64, *f as usize as i64);
            let tp = cx.interned_type(typ);
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
            // A shape-mismatched (untainted) result still owns the
            // returned Value: warn and drop it. A bottom's placeholder
            // pair owns nothing.
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
            // Our own block can be null; the helper maps null to null.
            let live = cx.b.ins().icmp_imm(IntCC::NotEqual, base, 0);
            let zero = cx.b.ins().iconst(types::I64, 0);
            let word = cx.b.ins().select(live, word, zero);
            // The size is read at run time from `site_desc`: our own
            // layout is not final while we are still emitting into it.
            // The cell's address is stable (the kernel cache holds the Arc).
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
            // Our own block may be 0; forward 0, not a garbage offset.
            let addr = cx.b.ins().iadd_imm(base, first as i64);
            let has = cx.b.ins().icmp_imm(IntCC::NotEqual, base, 0);
            let zero = cx.b.ins().iconst(types::I64, 0);
            return Ok(cx.b.ins().select(has, addr, zero));
        }
        return Ok(cx.b.ins().iconst(types::I64, 0));
    }
    // In-loop call site: the chain runs per innermost iteration; the
    // ensures are idempotent after the first.
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
    // This chain's per-iteration ensure never runs on a len-0 epoch, so
    // every enclosing loop's exit re-ensures it at its level
    // (`BodyCx::emit_slot_truncates`) to truncate on shrink.
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
    let ftype = cs
        .resolved_ftype()
        .or_else(|| cs.ftype())
        .ok_or_else(|| anyhow!("lambda call `{fn_name}`: no resolved FnType"))?;
    // Slots are typed from the callee's signature (`info.arg_types`):
    // those types were resolved and frozen at build time, and env is
    // unavailable at emit time to resolve the caller-side node type.
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
    // Slot types come from the callee's signature, and Bottom unifies
    // with any signature type, so a Bottom-typed arg node is gated on
    // the node itself.
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
    // The callsite node's type may promise a 2-word Value where the
    // callee ABI returns a narrower shape; both merge edges must carry
    // the widened pairing.
    let node_typ = cs.typ();
    let widen = call_result_needs_value_widening(node_typ, ret);
    let ret_pay_ty = match kernel_abi::abi_kind(ret) {
        Some(AbiKind::Scalar(p)) if !widen => prim_to_clif(p),
        _ => types::I64,
    };
    let mut slot_cvs: smallvec::SmallVec<[CompiledExpr; 12]> = smallvec::SmallVec::new();
    for s in slots.iter() {
        // A composite/string pair is already a genuine Value; only a
        // scalar fed to a value-shaped slot must widen its payload word
        // to the Value encoding.
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
                // String arg emissions are always owned (local reads
                // clone at the read); capture string reads are borrowed.
                Some(AbiKind::String) => {
                    drops.push(CallArgDrop::String(cv.payload));
                }
                _ if node_composite_source(n) == CompositeSource::Owned => {
                    match kernel_abi::abi_kind(s.typ()) {
                        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                            drops.push(CallArgDrop::Composite(cv.payload));
                        }
                        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
                            // Scalar-widened args own nothing.
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
    // The callee's context word: our init view, forced on this site's
    // first call ever (the node-walk primes an instance's first
    // dispatch the same way), plus the inherited quiet bit.
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
    let rmerge = cx.b.create_block();
    cx.b.append_block_param(rmerge, types::I64);
    cx.b.append_block_param(rmerge, types::I64);
    if is_self {
        // A self-call inside the stack red zone re-enters on a fresh
        // segment through the spill thunk; the same check carries the
        // cooperative interrupt, which skips the dispatch with a
        // tainted placeholder. Only self-calls need it: cross-kernel
        // edges are acyclic.
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
        // The abort discards the whole run, so no trigger fold.
        let ph = emit_bottom_placeholder(cx, if widen { node_typ } else { ret }, &[])?;
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
    // An aborted callee left `KERNEL_ABORT` set and returned the zero
    // pair, not a real value: drop what we own and jump to
    // `pending_exit` with the flag still set so `Kernel::update` discards.
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

/// Emit the post-call drops for owned call args.
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

/// Drop every owned local currently in scope; called at every return
/// point. `emit_kernel_return` makes the result independently owned
/// before calling this, so the returned pointer never aliases a dropped slot.
pub(super) fn drop_owned_composites(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
    let drops: smallvec::SmallVec<[(LocalKind, ValueVar); 8]> =
        env.locals.iter().map(|l| (l.kind, l.vv)).collect();
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

/// Emit drops for everything the kernel currently owns, for a
/// whole-kernel abort path: the in-flight value bufs, the owned HOF
/// inputs, then `drop_owned_composites`.
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
    // In-flight HOF inputs are finished ValArrays, not value bufs.
    let arr_drop = ctx
        .helper_refs
        .get("graphix_valarray_drop")
        .ok_or_else(|| anyhow!("missing graphix_valarray_drop"))?;
    for arr_var in ctx.owned_input_stack.borrow().iter() {
        let ptr = b.use_var(*arr_var);
        b.ins().call(arr_drop, &[ptr]);
    }
    drop_owned_composites(b, env, ctx)
}
