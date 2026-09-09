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
        kernel_abi::{self, AbiKind},
        lowering::BuiltinCallSiteInfo,
    },
    typ::Type,
};
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use cranelift_codegen::ir::{
    Block, BlockArg, FuncRef, Inst, InstBuilder, Value as ClifValue, condcodes::IntCC,
    types,
};
use cranelift_frontend::{FunctionBuilder, Variable};
use netidx_value::Value;

use super::{
    abi::{
        CompiledExpr, JitEnv, LocalKind, STALE, TAINT, ValueVar, emit_untainted_i64,
        propagate_flags, scalar_disc, value_disc,
    },
    call::{
        CompositeSource, drop_owned_composites, emit_drop_local, emit_pending_cleanup,
    },
    flow::emit_body_tail,
    lower::{
        LowerCtx, SelWord, SiteLayout, SlotTableFrame, TruncAnchor, TruncLeaf, TruncRec,
    },
    nodes::emit_owned_value_operand_node,
    scalar::scalar_to_payload_i64,
};

/// One tail-call rebind: the kernel param slot index (among
/// `KernelSig::params` — skipped and invariant formals leave holes,
/// so the pairing is explicit), the new value, its composite
/// provenance, and its taint bit.
pub(super) struct TailRebind {
    pub(super) slot: usize,
    pub(super) val: CompiledExpr,
    pub(super) source: CompositeSource,
    pub(super) taint: ClifValue,
}

/// Resolve a tail-rebind target slot's `ValueVar`, by BindId first:
/// a carried formal and a capture can share a basename, and a
/// name-only lookup finds the later-bound capture.
fn lookup_slot(env: &JitEnv, slot: &kernel_abi::KernelParam) -> Option<ValueVar> {
    let l = match slot.bind_id {
        Some(id) => env.lookup(id, &slot.name),
        None => env.lookup_name(&slot.name),
    }?;
    Some(l.vv)
}

/// The rebind-and-jump core of a self tail-call. Rebinds the leading
/// formal slots, writing both the payload and the disc (the
/// terminating arm returns a formal, whose disc must be the last
/// iteration's fired-ness), drops every owned non-slot local above
/// the param mark, truncates the env to the params, and jumps to the
/// loop head.
pub(super) fn emit_tail_rebind_jump(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
    rebinds: smallvec::SmallVec<[TailRebind; 8]>,
) -> Result<()> {
    let head = ctx.tail.loop_head.ok_or_else(|| {
        anyhow!("kernel malformed: TailCall in kernel without has_tail_loop")
    })?;
    // A tainted new value keeps the slot's previous value, as the
    // node-walk backfills a bottomed arg from its cache. Hand-built
    // test kernels leave `call_slots` empty: rebind positionally.
    if ctx.tail.call_slots.is_none() {
        debug_assert!(rebinds.len() <= ctx.tail.param_mark);
        for r in rebinds.iter() {
            let vv = env.locals[r.slot].vv;
            let old_p = b.use_var(vv.payload);
            let old_d = b.use_var(vv.disc);
            let p = b.ins().select(r.taint, old_p, r.val.payload);
            let d = b.ins().select(r.taint, old_d, r.val.disc);
            b.def_var(vv.payload, p);
            b.def_var(vv.disc, d);
        }
        env.truncate(ctx.tail.param_mark);
        b.ins().jump(head, &[]);
        return Ok(());
    }
    let slots = ctx.tail.call_slots.unwrap();
    // Slots cover every kernel value param; a tail call rebinds only
    // the loop-carried formals.
    debug_assert!(rebinds.len() <= slots.len());
    use kernel_abi::AbiParamKind;
    let drop_helper = ctx
        .helper_refs
        .get("graphix_valarray_drop")
        .ok_or_else(|| anyhow!("missing graphix_valarray_drop"))?;
    let clone_helper = ctx
        .helper_refs
        .get("graphix_valarray_clone")
        .ok_or_else(|| anyhow!("missing graphix_valarray_clone"))?;
    for r in rebinds.iter() {
        let slot = &slots[r.slot];
        match slot.kind.abi() {
            AbiParamKind::Scalar(_) => {
                let vv = lookup_slot(env, slot).ok_or_else(|| {
                    anyhow!("TailCall: scalar slot `{}` not in env", slot.name)
                })?;
                let old_p = b.use_var(vv.payload);
                let old_d = b.use_var(vv.disc);
                let p = b.ins().select(r.taint, old_p, r.val.payload);
                let d = b.ins().select(r.taint, old_d, r.val.disc);
                b.def_var(vv.payload, p);
                b.def_var(vv.disc, d);
            }
            AbiParamKind::Array | AbiParamKind::Tuple | AbiParamKind::Struct => {
                // REPLACE clones a Borrowed new value and drops the old
                // slot pointer; KEEP leaves the slot and drops an Owned
                // new value.
                let vv = lookup_slot(env, slot).ok_or_else(|| {
                    anyhow!("TailCall: composite slot `{}` not in env", slot.name)
                })?;
                let keep_bl = b.create_block();
                let replace_bl = b.create_block();
                let cont_bl = b.create_block();
                b.ins().brif(r.taint, keep_bl, &[], replace_bl, &[]);
                b.seal_block(keep_bl);
                b.seal_block(replace_bl);
                b.switch_to_block(replace_bl);
                let newp = if r.source == CompositeSource::Borrowed {
                    let call = b.ins().call(clone_helper, &[r.val.payload]);
                    b.inst_results(call)[0]
                } else {
                    r.val.payload
                };
                let old = b.use_var(vv.payload);
                b.ins().call(drop_helper, &[old]);
                b.def_var(vv.payload, newp);
                b.def_var(vv.disc, r.val.disc);
                b.ins().jump(cont_bl, &[]);
                b.switch_to_block(keep_bl);
                if r.source == CompositeSource::Owned {
                    b.ins().call(drop_helper, &[r.val.payload]);
                }
                b.ins().jump(cont_bl, &[]);
                b.seal_block(cont_bl);
                b.switch_to_block(cont_bl);
            }
            AbiParamKind::Variant | AbiParamKind::Nullable | AbiParamKind::Value => {
                // The composite protocol over the (disc, payload) pair
                // helpers.
                let vv = lookup_slot(env, slot).ok_or_else(|| {
                    anyhow!("TailCall: value slot `{}` not in env", slot.name)
                })?;
                let vclone = ctx
                    .helper_refs
                    .get("graphix_value_clone")
                    .ok_or_else(|| anyhow!("missing graphix_value_clone"))?;
                let vdrop = ctx
                    .helper_refs
                    .get("graphix_value_drop")
                    .ok_or_else(|| anyhow!("missing graphix_value_drop"))?;
                let keep_bl = b.create_block();
                let replace_bl = b.create_block();
                let cont_bl = b.create_block();
                b.ins().brif(r.taint, keep_bl, &[], replace_bl, &[]);
                b.seal_block(keep_bl);
                b.seal_block(replace_bl);
                b.switch_to_block(replace_bl);
                let (newd, newp) = if r.source == CompositeSource::Borrowed {
                    let call = b.ins().call(vclone, &[r.val.disc, r.val.payload]);
                    let rs = b.inst_results(call);
                    (rs[0], rs[1])
                } else {
                    (r.val.disc, r.val.payload)
                };
                let old_d = b.use_var(vv.disc);
                let old_p = b.use_var(vv.payload);
                b.ins().call(vdrop, &[old_d, old_p]);
                b.def_var(vv.payload, newp);
                b.def_var(vv.disc, newd);
                b.ins().jump(cont_bl, &[]);
                b.switch_to_block(keep_bl);
                if r.source == CompositeSource::Owned {
                    b.ins().call(vdrop, &[r.val.disc, r.val.payload]);
                }
                b.ins().jump(cont_bl, &[]);
                b.seal_block(cont_bl);
                b.switch_to_block(cont_bl);
            }
            AbiParamKind::String => {
                // A String production is always owned (a local read
                // refcount-bumps), so there is no source branch.
                let vv = lookup_slot(env, slot).ok_or_else(|| {
                    anyhow!("TailCall: string slot `{}` not in env", slot.name)
                })?;
                let sdrop = ctx
                    .helper_refs
                    .get("graphix_arcstr_drop")
                    .ok_or_else(|| anyhow!("missing graphix_arcstr_drop"))?;
                let keep_bl = b.create_block();
                let replace_bl = b.create_block();
                let cont_bl = b.create_block();
                b.ins().brif(r.taint, keep_bl, &[], replace_bl, &[]);
                b.seal_block(keep_bl);
                b.seal_block(replace_bl);
                b.switch_to_block(replace_bl);
                let old_p = b.use_var(vv.payload);
                b.ins().call(sdrop, &[old_p]);
                b.def_var(vv.payload, r.val.payload);
                b.def_var(vv.disc, r.val.disc);
                b.ins().jump(cont_bl, &[]);
                b.switch_to_block(keep_bl);
                b.ins().call(sdrop, &[r.val.payload]);
                b.ins().jump(cont_bl, &[]);
                b.seal_block(cont_bl);
                b.switch_to_block(cont_bl);
            }
        }
    }
    // Block and select-arm locals were dropped at their scope exits,
    // so the env's tail holds only top-level lets without a rebind
    // slot; those would leak per iteration.
    let drops: smallvec::SmallVec<[(LocalKind, ValueVar); 8]> = env.locals
        [ctx.tail.param_mark..]
        .iter()
        .filter(|l| !slots.iter().any(|s| s.name == l.name))
        .map(|l| (l.kind, l.vv))
        .collect();
    for (kind, vv) in drops {
        emit_drop_local(b, ctx, kind, vv)?;
    }
    env.truncate(ctx.tail.param_mark);
    b.ins().jump(head, &[]);
    Ok(())
}

/// When the I8 `valid` bit is 0, set the pending flag, run
/// `emit_pending_cleanup` and jump to `pending_exit`; otherwise fall
/// through to a fresh block. For a tainted scalar consumed by a site
/// with no per-value validity channel.
pub(super) fn emit_bottom_abort(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
    valid: ClifValue,
) -> Result<()> {
    let pending_set = ctx
        .helper_refs
        .get("graphix_abort_set")
        .ok_or_else(|| anyhow!("missing graphix_abort_set"))?;
    let pre_pending = b.create_block();
    let continue_block = b.create_block();
    let pending_exit = pending_exit_block(b, ctx);
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

/// Poll `graphix_interrupted` at a loop head: nonzero takes the
/// kernel's abort path (pending flag, `emit_pending_cleanup`,
/// `pending_exit`), zero falls through to a fresh block. Emitted at
/// the tail-loop head and every HOF scaffold loop head.
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
        .get("graphix_abort_set")
        .ok_or_else(|| anyhow!("missing graphix_abort_set"))?;
    let call = b.ins().call(interrupted, &[]);
    let intr = b.inst_results(call)[0];
    let pre_pending = b.create_block();
    let continue_block = b.create_block();
    let pending_exit = pending_exit_block(b, ctx);
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

/// A type-erased producer of a kernel function body, given the entry
/// block, bound params and lowering context. Erasing the Node's
/// `R`/`E` keeps the JIT pipeline monomorphic; the per-body data
/// rides alongside as a [`BodySpec`].
pub(super) trait BodyEmitter {
    fn emit(
        &self,
        b: &mut FunctionBuilder,
        env: &mut JitEnv,
        ctx: &LowerCtx,
    ) -> Result<()>;
}

/// The data facts a kernel build needs about one body;
/// `compile_into_function` copies them onto the [`LowerCtx`].
#[derive(Clone, Copy)]
pub(super) struct BodySpec<'a> {
    /// Fastcall/cast sites of the region being emitted;
    /// `CallSite::emit_clif` lowers a registered site to a direct call.
    pub(super) builtin_apply_sites:
        Option<&'a nohash::IntMap<ExprId, BuiltinCallSiteInfo>>,
    /// Statically-resolved lambda call sites of the region being
    /// emitted; `None` for callee bodies (a callee's only cross-kernel
    /// reference is itself).
    pub(super) lambda_call_sites: Option<&'a nohash::IntMap<ExprId, LambdaCallInfo>>,
    /// `Some` when the kernel being emitted is a self-recursive lambda
    /// body: the binding its self-references carry and the kernel's
    /// own call descriptor.
    pub(super) self_call: Option<&'a (BindId, LambdaCallInfo)>,
    /// The environment for type resolution only: node `typ` cells can
    /// carry `Type::Ref`s that need `env.lookup_ref` before
    /// `abi_kind`/freeze can classify them. Never for binding lookups.
    pub(super) type_env: Option<&'a Env>,
    /// Whether this body may claim per-instance state words — `true`
    /// only for the region parent's root body. See
    /// [`StateChannel::enabled`].
    pub(super) allow_state: bool,
}

/// One body to build: the data spec + the type-erased emission hook.
pub(super) struct BodySource<'a> {
    pub(super) spec: BodySpec<'a>,
    pub(super) hook: &'a dyn BodyEmitter,
}

/// Walks the region-root `Node` via `emit_clif` recursion and emits
/// the kernel return. `return_type` comes from the `KernelSig` so the
/// boundary marshalling agrees with the wrapper.
pub(super) struct NodeBodyEmitter<'a, R: Rt, E: UserEvent> {
    pub(super) root: &'a Node<R, E>,
    pub(super) return_type: &'a Type,
}

impl<R: Rt, E: UserEvent> BodyEmitter for NodeBodyEmitter<'_, R, E> {
    fn emit(
        &self,
        b: &mut FunctionBuilder,
        env: &mut JitEnv,
        ctx: &LowerCtx,
    ) -> Result<()> {
        let mut cx = BodyCx { b: &mut *b, env: &mut *env, ctx };
        match ctx.self_call {
            // A recursive body is emitted in tail position: self
            // tail-calls become the rebind-and-jump loop.
            Some(_) => emit_body_tail(&mut cx, self.root, self.return_type),
            None => emit_return_from_node(&mut cx, self.return_type, self.root),
        }
    }
}

/// The emission context handed to [`Update::emit_clif`] /
/// [`crate::Apply::emit_clif`] impls; recursion is
/// `child.emit_clif(cx)`. `b` is the raw cranelift builder; the
/// graphix-specific surface (env binds, helpers, element reads,
/// taint/pending) goes through this type's methods so `JitEnv` and
/// `LowerCtx` stay private.
pub struct BodyCx<'a, 'f, 'c> {
    pub b: &'a mut FunctionBuilder<'f>,
    pub(crate) env: &'a mut JitEnv,
    pub(crate) ctx: &'a LowerCtx<'c>,
}

impl<'a, 'f, 'c> BodyCx<'a, 'f, 'c> {
    /// FuncRef for a registered `emit_helpers` runtime helper.
    pub fn helper(&self, name: &str) -> Result<FuncRef> {
        self.ctx.helper_refs.get(name).ok_or_else(|| anyhow!("missing helper {name}"))
    }

    /// Look up a helper and call it, asserting the argument count
    /// against its registered wire signature.
    ///
    /// Prefer this over `helper()` + `ins().call()`: cranelift's
    /// verifier rejects a mismatched call as a whole-function failure,
    /// so the region would silently node-walk instead of failing a test.
    pub fn call_helper(&mut self, name: &str, args: &[ClifValue]) -> Result<Inst> {
        let f = self.helper(name)?;
        debug_assert_eq!(
            self.ctx.helper_refs.arity.get(name).copied(),
            Some(args.len()),
            "helper `{name}` called with {} args",
            args.len()
        );
        Ok(self.b.ins().call(f, args))
    }

    /// The `event.init` word from wire slot 0 (`I64`, nonzero on an
    /// init view; see [`kernel_abi::CTX_WIRE_SLOTS`]).
    pub fn init_flag(&self) -> ClifValue {
        self.ctx.init_flag
    }

    /// THE QUIET FLAG (`I64`, 0/1) — see [`LowerCtx::quiet_flag`].
    pub fn quiet_flag(&self) -> ClifValue {
        self.ctx.quiet_flag
    }

    /// The per-instance state-buffer pointer (`I64`), loaded from wire
    /// slot 1. Only meaningful at offsets returned by
    /// [`claim_state_word`](Self::claim_state_word); 0 when the kernel
    /// claimed nothing (no claimed offset exists to read through it).
    pub fn state_ptr(&self) -> ClifValue {
        self.ctx.state.ptr
    }

    /// Claim one `u64` of per-instance cross-invocation memory,
    /// returning its byte offset from [`state_ptr`](Self::state_ptr);
    /// `None` inside a scaffold loop or in a callee body, where the
    /// caller must emit its stateless approximation. The buffer is
    /// zero-initialized per instance, so store `value + 1` and read 0
    /// as "no previous observation".
    pub fn claim_state_word(&self) -> Option<i32> {
        if !self.ctx.state.enabled || self.ctx.loop_depth.get() > 0 {
            return None;
        }
        let idx = self.ctx.state.next.get();
        self.ctx.state.next.set(idx + 1);
        Some((idx * 8) as i32)
    }

    /// [`claim_state_word`](Self::claim_state_word) without the
    /// in-loop refusal, for a word that is exact across iterations:
    /// the observed quantity is loop-invariant
    /// ([`node_loop_invariant_ref`]) or the word anchors a per-slot
    /// heap chain ([`open_slot_tables`](Self::open_slot_tables)).
    /// Callee bodies still refuse.
    pub fn claim_state_word_loop_invariant(&self) -> Option<i32> {
        if !self.ctx.state.enabled {
            return None;
        }
        let idx = self.ctx.state.next.get();
        self.ctx.state.next.set(idx + 1);
        Some((idx * 8) as i32)
    }

    /// Open a scaffold loop's per-slot state-table frame; emit in the
    /// loop preheader. For each guarded-select site in `sites` it
    /// anchors a chain of owning tables mirroring the loop nesting
    /// (one directory level per enclosing frame) that ends in a leaf
    /// table with one word per slot ordinal. A tainted source at any
    /// level grows that level only as an in-bounds guard, never as a
    /// logical resize. A frame is always pushed, possibly with no
    /// tables; [`close_slot_tables`](Self::close_slot_tables) must
    /// pop it after body emission.
    pub(crate) fn open_slot_tables(
        &mut self,
        sites: &[ExprId],
        len: ClifValue,
        src_disc: ClifValue,
        idx_var: Variable,
    ) -> Result<()> {
        debug_assert!(
            self.ctx.closed_frame.borrow().is_none(),
            "a closed frame's slot truncates were never emitted"
        );
        let depth = self.ctx.loop_depth.get() + 1;
        // Every open loop pushed a frame, so the stack is the
        // enclosing-loop chain, outermost first.
        let enclosing: smallvec::SmallVec<[(ClifValue, ClifValue, Variable); 4]> = {
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
        // Records for the exit re-ensures: they propagate outward so
        // every enclosing level truncates on its exit, since an outer
        // len-0 epoch skips this preheader entirely.
        let mut pending: Vec<TruncRec> = Vec::new();
        for id in sites {
            let anchor = if n_dirs == 0 {
                self.claim_state_word()
            } else {
                self.claim_state_word_loop_invariant()
            };
            let entry = match anchor {
                Some(off) => {
                    pending.push(TruncRec {
                        anchor: TruncAnchor::State(off),
                        n_dirs: n_dirs as u32,
                        leaf: TruncLeaf::Table { stride: 1 },
                        leaf_ptr: 0,
                    });
                    self.ctx.state.anchors.borrow_mut().push(kernel_abi::SiteAnchor {
                        rel: (off / 8) as u32,
                        own_levels: n_dirs as u32,
                        leaf: None,
                    });
                    let sp = self.state_ptr();
                    let word_addr = self.b.ins().iadd_imm(sp, off as i64);
                    let table =
                        self.emit_slot_chain(word_addr, &enclosing, len, src_disc)?;
                    Some((table, false))
                }
                // A callee body's loop anchors in the per-call-site
                // block, whose base is 0 on a recursive back-edge:
                // branch around the chain and hand the selects a 0 table.
                None => match self.claim_site_anchor(n_dirs as u32, None) {
                    Some(off) => {
                        pending.push(TruncRec {
                            anchor: TruncAnchor::Site(off),
                            n_dirs: n_dirs as u32,
                            leaf: TruncLeaf::Table { stride: 1 },
                            leaf_ptr: 0,
                        });
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
            pending,
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
    /// pushed. Every scaffold loop emitter closes right after its body
    /// emission, before propagating a body error.
    pub(crate) fn close_slot_tables(&mut self) {
        let popped = self.ctx.slot_tables.borrow_mut().pop();
        debug_assert!(popped.is_some(), "close_slot_tables without an open frame");
        // Stashed for `emit_slot_truncates` in the exit block; an
        // overwrite happens only on error paths, where the kernel is
        // discarded.
        if let Some(f) = popped {
            *self.ctx.closed_frame.borrow_mut() =
                Some((f.depth, f.len, f.src_disc, f.pending));
        }
    }

    /// Emit the closed frame's chain re-ensures in the current block;
    /// every scaffold loop emitter calls this right after switching to
    /// its always-executed `loop_exit`. An in-body ensure never runs
    /// on a zero-length epoch, so the exit re-ensures level `depth`
    /// with this frame's len, truncating on a shrink; the records then
    /// propagate to the enclosing frame.
    pub(crate) fn emit_slot_truncates(&mut self) -> Result<()> {
        let Some((depth, len, src_disc, recs)) =
            self.ctx.closed_frame.borrow_mut().take()
        else {
            debug_assert!(false, "emit_slot_truncates without a closed frame");
            return Ok(());
        };
        if recs.is_empty() {
            return Ok(());
        }
        let k = depth as usize;
        let dirs: smallvec::SmallVec<[(ClifValue, ClifValue, Variable); 4]> = {
            let frames = self.ctx.slot_tables.borrow();
            debug_assert_eq!(frames.len(), k - 1, "closed frame depth out of sync");
            frames.iter().map(|f| (f.len, f.src_disc, f.idx_var)).collect()
        };
        let table_helper = self.helper("graphix_slot_state_table")?;
        let valid = emit_untainted_i64(self.b, src_disc);
        for r in recs.iter() {
            let leaf_ptr = self.b.ins().iconst(types::I64, r.leaf_ptr);
            // The anchor's word address; a site anchor's base may be 0
            // (a back-edge activation) — branch around the walk.
            let (word0, guard) = match r.anchor {
                TruncAnchor::State(off) => {
                    let sp = self.state_ptr();
                    (self.b.ins().iadd_imm(sp, off as i64), None)
                }
                TruncAnchor::Site(off) => {
                    let base = self.site_ptr();
                    (self.b.ins().iadd_imm(base, off as i64), Some(base))
                }
            };
            let (walk_bl, done_bl) = match guard {
                Some(base) => {
                    let has = self.b.ins().icmp_imm(IntCC::NotEqual, base, 0);
                    let walk = self.b.create_block();
                    let done = self.b.create_block();
                    self.b.ins().brif(has, walk, &[], done, &[]);
                    self.b.switch_to_block(walk);
                    self.b.seal_block(walk);
                    (Some(walk), Some(done))
                }
                None => (None, None),
            };
            let n_dirs = r.n_dirs as usize;
            // Walk the still-open directory levels by their current
            // ordinals, then ensure level k: a directory when
            // k <= n_dirs, the leaf when k == n_dirs + 1.
            let mut word = word0;
            for (j, (flen, fdisc, fidx)) in dirs.iter().enumerate() {
                let fvalid = emit_untainted_i64(self.b, *fdisc);
                let own = self.b.ins().iconst(types::I64, (n_dirs - j) as i64);
                let call = self
                    .b
                    .ins()
                    .call(table_helper, &[word, *flen, fvalid, own, leaf_ptr]);
                let dir = self.b.inst_results(call)[0];
                let i = self.b.use_var(*fidx);
                let o = self.b.ins().ishl_imm(i, 3);
                word = self.b.ins().iadd(dir, o);
            }
            if k <= n_dirs {
                let own = self.b.ins().iconst(types::I64, (n_dirs - (k - 1)) as i64);
                self.b.ins().call(table_helper, &[word, len, valid, own, leaf_ptr]);
            } else {
                debug_assert_eq!(k, n_dirs + 1, "trunc record deeper than its claim");
                match r.leaf {
                    TruncLeaf::Table { stride } => {
                        let words = self.b.ins().imul_imm(len, stride as i64);
                        let own0 = self.b.ins().iconst(types::I64, 0);
                        self.b
                            .ins()
                            .call(table_helper, &[word, words, valid, own0, leaf_ptr]);
                    }
                    TruncLeaf::Blocks => {
                        let blocks_helper = self.helper("graphix_slot_state_blocks")?;
                        self.b.ins().call(blocks_helper, &[word, len, valid, leaf_ptr]);
                    }
                }
            }
            if let (Some(_), Some(done)) = (walk_bl, done_bl) {
                self.b.ins().jump(done, &[]);
                self.b.switch_to_block(done);
                self.b.seal_block(done);
            }
        }
        if k > 1 {
            let mut frames = self.ctx.slot_tables.borrow_mut();
            if let Some(f) = frames.last_mut() {
                f.pending.extend(recs);
            }
        }
        Ok(())
    }

    /// The address of this slot's state word for the site at `id`
    /// (`table + idx * 8`) when the innermost open scaffold loop
    /// carries a table for it; `None` when there is no open loop, the
    /// site is at a different depth than the frame's body, or the
    /// frame claimed no table.
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

    /// Claim one word of per-call-site block memory (wire slot 2),
    /// the callee-body twin of [`claim_state_word`](Self::claim_state_word).
    /// Returns the byte offset; `None` outside callee bodies. The block
    /// base may be 0 at runtime (a recursive back-edge), so every
    /// consumer null-guards ([`SelWord::Guarded`]).
    pub(crate) fn claim_site_word(&self) -> Option<i32> {
        if !self.ctx.site.enabled {
            return None;
        }
        let idx = self.ctx.site.next.get();
        self.ctx.site.next.set(idx + 1);
        Some((idx * 8) as i32)
    }

    /// Claim the word that roots a self-call's per-activation block
    /// tree ([`kernel_abi::SelfBlock`]); one per self-call site, so
    /// sibling calls at the same depth get separate trees. Refused
    /// inside scaffold loops, where the root would alias every slot.
    pub(crate) fn claim_self_block_word(&self) -> Option<i32> {
        if self.ctx.loop_depth.get() > 0 {
            return None;
        }
        let off = self.claim_site_word()?;
        self.ctx.self_call_roots.borrow_mut().push(off);
        Some(off)
    }

    /// [`claim_site_word`](Self::claim_site_word) for a word that
    /// anchors a slot-table chain: registered on the kernel's
    /// [`SiteLayout`] so the block's owner frees the chain.
    pub(crate) fn claim_site_anchor(
        &self,
        own_levels: u32,
        leaf: Option<std::sync::Arc<kernel_abi::SiteLeaf>>,
    ) -> Option<i32> {
        let off = self.claim_site_word()?;
        self.ctx.site.anchors.borrow_mut().push(kernel_abi::SiteAnchor {
            rel: (off / 8) as u32,
            own_levels,
            leaf,
        });
        Some(off)
    }

    /// The per-call-site block base (`I64`, possibly 0 — see
    /// [`claim_site_word`](Self::claim_site_word)).
    pub(crate) fn site_ptr(&self) -> ClifValue {
        self.ctx.site.ptr
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

    /// Bracket scaffold-loop body emission, including the loop's own
    /// element/index/acc binds (their `Local::depth` stamp is what
    /// [`node_loop_invariant_ref`] keys on).
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

    /// Stable `*const ArcStr` for `s` as an `iconst`. The boxed arena
    /// entry is merged into the kernel's [`KernelStrings`] so it
    /// outlives the compiled code that baked the pointer.
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

    /// The builtin Apply-site info for `id`, if the region's discovery
    /// pass registered one.
    pub(crate) fn builtin_site(&self, id: ExprId) -> Option<&BuiltinCallSiteInfo> {
        self.ctx.builtin_apply_sites.and_then(|m| m.get(&id))
    }

    /// The lambda call-site info for `id`, if `try_fuse`'s analysis
    /// registered one; `Some` means the callee kernel is declared in
    /// this function's `callee_refs` and ready to `call`.
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

    /// Stable `*const Type` for a Cast site's destination type — see
    /// [`Self::interned_str`]; the kernel's [`KernelValues`] keeps it
    /// alive as long as the compiled code that baked the pointer.
    pub fn interned_type(&mut self, t: &Type) -> ClifValue {
        let b = Box::new(t.clone());
        let ptr = b.as_ref() as *const Type;
        self.ctx.lazy_keep.borrow_mut().push(b);
        self.b.ins().iconst(types::I64, ptr as i64)
    }

    /// Stable `*const QopSite` for a handler-ful `?` site — the
    /// delivery drain's key (`graphix_qop_raise`); kept alive like
    /// [`Self::interned_type`].
    pub fn interned_qop_site(&mut self, site: crate::node::error::QopSite) -> ClifValue {
        let b = Box::new(site);
        let ptr = b.as_ref() as *const crate::node::error::QopSite;
        self.ctx.lazy_keep.borrow_mut().push(b);
        self.b.ins().iconst(types::I64, ptr as i64)
    }
}

/// Ownership classification of a Node-rooted result: a binding read
/// is borrowed (the env slot keeps the ref), grouping is transparent
/// to its tail, everything else hands out an owned ref. Decides
/// whether a clone is needed before the source's scope drops.
pub fn node_composite_source<R: Rt, E: UserEvent>(node: &Node<R, E>) -> CompositeSource {
    use NodeView;
    let mut n: &dyn Update<R, E> = &**node;
    loop {
        match n.view() {
            NodeView::Ref(_) => return CompositeSource::Borrowed,
            NodeView::ExplicitParens(p) => n = &*p.n,
            // A Block's result is owned by construction:
            // `emit_block_node` clones a borrowed tail before the
            // scope drops.
            NodeView::Block(_) => return CompositeSource::Owned,
            _ => return CompositeSource::Owned,
        }
    }
}

/// True iff the node's own type derefs to `Type::Bottom`. A Bottom
/// node emits the shapeless placeholder `(NULL|TAINT, 0)`, which a
/// consumer typed from an external signature (HOF source, fold init,
/// lambda-call arg) would read as a pointer; Bottom unifies with every
/// signature type, so gate on the node itself and de-fuse.
pub fn node_is_bottom<R: Rt, E: UserEvent>(node: &Node<R, E>) -> bool {
    node.typ().with_deref(|t| matches!(t, Some(Type::Bottom)))
}

/// True iff `node` is a plain `Ref` whose binding is loop-invariant
/// at this emission point: a kernel input or a local bound outside
/// every open scaffold loop (`Local::depth` 0). Everything else is
/// conservatively variant. Transparent through parens and blocks.
pub fn node_loop_invariant_ref<R: Rt, E: UserEvent>(
    cx: &BodyCx,
    node: &Node<R, E>,
) -> bool {
    let mut n: &dyn Update<R, E> = &**node;
    loop {
        match n.view() {
            NodeView::Ref(r) => {
                let l = match ref_local_name(n.spec()) {
                    Some(name) => cx.env.lookup(r.id, name),
                    None => cx.env.lookup_id(r.id),
                };
                return l.is_some_and(|l| l.depth == 0);
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
/// owned one through. `pub` for package crates' `Apply::emit_clif` impls.
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
/// owned one through. `pub` for package crates' `Apply::emit_clif` impls.
pub fn ensure_owned_value_src(
    cx: &mut BodyCx,
    src: CompositeSource,
    disc: ClifValue,
    payload: ClifValue,
) -> Result<(ClifValue, ClifValue)> {
    match src {
        CompositeSource::Owned => Ok((disc, payload)),
        CompositeSource::Borrowed => {
            // `graphix_value_clone` preserves the tag bits, so taint
            // rides through the clone.
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
pub(super) fn pending_exit_block(b: &mut FunctionBuilder, ctx: &LowerCtx) -> Block {
    let mut slot = ctx.pending_exit.borrow_mut();
    match *slot {
        Some(blk) => blk,
        None => {
            let blk = b.create_block();
            *slot = Some(blk);
            blk
        }
    }
}

/// Unconditionally bottom the kernel from the current block: set the
/// pending flag, drop the in-flight owned set, and jump to
/// `pending_exit` (so `Kernel::update` returns `None`). Terminates the
/// block.
pub(super) fn emit_kernel_bottom(cx: &mut BodyCx) -> Result<()> {
    let pending_set = cx.helper("graphix_abort_set")?;
    let exit = pending_exit_block(cx.b, cx.ctx);
    cx.b.ins().call(pending_set, &[]);
    emit_pending_cleanup(cx.b, cx.env, cx.ctx)?;
    cx.b.ins().jump(exit, &[]);
    Ok(())
}

/// Emit `node` as the kernel's result under `return_type`'s
/// convention. A value-shape return (Variant/Nullable/Value) routes
/// through `emit_owned_value_operand_node`, since the body node's own
/// shape may differ (a tuple-literal body under a `['b, null]`
/// signature emits the composite convention).
pub(super) fn emit_return_from_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    return_type: &Type,
    node: &Node<R, E>,
) -> Result<()> {
    match kernel_abi::abi_kind(return_type) {
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

/// AND `mask`'s STALE bit into `disc`'s — the firing fold (STALE
/// survives only when BOTH sides are stale); every other bit is
/// unchanged.
pub(super) fn fold_stale(
    b: &mut FunctionBuilder,
    disc: ClifValue,
    mask: ClifValue,
) -> ClifValue {
    let vs = b.ins().band_imm(disc, STALE);
    let folded = b.ins().band(vs, mask);
    let cleaned = b.ins().band_imm(disc, !STALE);
    b.ins().bor(cleaned, folded)
}

pub(super) fn emit_kernel_return(
    cx: &mut BodyCx,
    return_type: &Type,
    mut cv: CompiledExpr,
    src: CompositeSource,
) -> Result<()> {
    // The result fires if its value chain fired or any tail-select
    // scrutinee on the executed path did (`LowerCtx::tail_scrut_stale`).
    #[cfg(debug_assertions)]
    if std::env::var_os("GXDBG_CALLRET").is_some() {
        let f = cx.helper("graphix_dbg_disc")?;
        let t = cx.b.ins().iconst(types::I64, 2);
        cx.b.ins().call(f, &[t, cv.disc]);
        let acc = cx.b.use_var(cx.ctx.tail.scrut_stale);
        let t3 = cx.b.ins().iconst(types::I64, 3);
        cx.b.ins().call(f, &[t3, acc]);
    }
    {
        // The bottom-out rule (design/activation_state.md): fold the
        // enclosing tail-select scopes innermost-first; a still-stale
        // result meeting a level's fresh-bottom fire becomes TAINT fresh.
        let levels: smallvec::SmallVec<[(ClifValue, Option<ClifValue>); 4]> =
            cx.ctx.sel_fires.borrow().iter().rev().copied().collect();
        for (sound, bf) in levels {
            cv.disc = fold_stale(cx.b, cv.disc, sound);
            if let Some(bf) = bf {
                let sbit = cx.b.ins().band_imm(cv.disc, STALE);
                let quiet = cx.b.ins().icmp_imm(IntCC::NotEqual, sbit, 0);
                let ov = cx.b.ins().band(quiet, bf);
                let d_bot = cx.b.ins().band_imm(cv.disc, !STALE);
                let d_bot = cx.b.ins().bor_imm(d_bot, TAINT);
                cv.disc = cx.b.ins().select(ov, d_bot, cv.disc);
            }
        }
        // The loop-carried accumulator (cross-ITERATION sound fires —
        // a fired loop-head scrutinee in any pass upgrades a stale
        // final result) folds last, outermost.
        let acc = cx.b.use_var(cx.ctx.tail.scrut_stale);
        cv.disc = fold_stale(cx.b, cv.disc, acc);
    }
    // The disc is rebased on the static return shape's Value
    // discriminant: `TagValue::from_raw` decodes these exact bits, so
    // they must be a valid one-hot discriminant plus tag bits.
    match kernel_abi::abi_kind(return_type) {
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let (disc, payload) = ensure_owned_value_src(cx, src, cv.disc, cv.payload)?;
            drop_owned_composites(cx.b, cx.env, cx.ctx)?;
            cx.b.ins().return_(&[disc, payload]);
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let bits = ensure_owned_composite_src(cx, src, cv.payload)?;
            drop_owned_composites(cx.b, cx.env, cx.ctx)?;
            let base = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            cx.b.ins().return_(&[disc, bits]);
        }
        Some(AbiKind::String) => {
            // String results are owned at production.
            drop_owned_composites(cx.b, cx.env, cx.ctx)?;
            let base = cx.b.ins().iconst(types::I64, value_disc::STRING);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            cx.b.ins().return_(&[disc, cv.payload]);
        }
        Some(AbiKind::Scalar(p)) => {
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

/// The kernel-local name a `Ref` resolves to in [`JitEnv`]: kernel
/// params and block-lets both bind under the last component of the
/// Ref's `ModPath`.
pub(super) fn ref_local_name(spec: &Expr) -> Option<&str> {
    let name = match &spec.kind {
        ExprKind::Ref { name } => name,
        _ => return None,
    };
    let s: &str = name.0.as_ref();
    Some(netidx_core::path::Path::basename(s).unwrap_or(s))
}
