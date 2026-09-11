//! Function-shape lowering: `compile_into_function`, the
//! [`LowerCtx`] handed to body emission, per-call-site slot
//! layout ([`SiteLayout`]/[`SlotTableFrame`]), and runtime
//! helper declaration from the `emit_helpers` registry.

use crate::{
    BindId,
    env::Env,
    expr::ExprId,
    fusion::{
        LambdaCallInfo,
        emit_helpers::{AbiTy, HelperSpec, all_helpers},
        kernel_abi::{self, AbiParamKind, KernelSig},
        lowering::{self, BuiltinCallSiteInfo},
    },
    typ::Type,
};
use anyhow::{Context as AnyContext, Result};
use cranelift_codegen::ir::{
    AbiParam, Block, FuncRef, InstBuilder, Signature, Value as ClifValue,
    condcodes::IntCC, types,
};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Linkage, Module};
use std::collections::BTreeMap;

use super::{
    abi::{JitEnv, LocalKind, STALE, ValueVar, local_payload_ty},
    body::{BodySource, emit_interrupt_check},
    record::{EmitConst, SymbolTable},
};

pub(super) fn compile_into_function<'a>(
    b: &mut FunctionBuilder,
    kernel: &'a KernelSig,
    callee_refs: &'a BTreeMap<usize, FuncRef>,
    self_thunk: Option<FuncRef>,
    helper_refs: &'a HelperRefs,
    consts: &'a std::cell::RefCell<Vec<EmitConst>>,
    module: &'a std::cell::RefCell<&'a mut JITModule>,
    symbols: &'a SymbolTable,
    symbol: &'a str,
    body: &'a BodySource<'a>,
    callee_layouts: &'a BTreeMap<usize, SiteLayout>,
) -> Result<(usize, Vec<kernel_abi::SiteAnchor>, Vec<kernel_abi::SelfBlock>, SiteLayout)>
{
    let spec = &body.spec;
    let entry = b.create_block();
    b.append_block_params_for_function_params(entry);
    b.switch_to_block(entry);

    let mut env = JitEnv::new();
    // Params are declared first: tail-call dispatch relies on
    // `env.locals[0..param_count]` being the params in order.
    let mut initial_vals: poolshark::local::LPooled<Vec<ClifValue>> =
        poolshark::local::LPooled::take();
    initial_vals.extend_from_slice(b.block_params(entry));
    // Wire slot 0 is the context word: bit 0 init, bit 1 quiet, bit 2
    // wake. Under a wake view init is not genuine: consumers read
    // `init & !wake`.
    let ctx_word = initial_vals[0];
    let init_flag = b.ins().band_imm(ctx_word, 1);
    let wake_flag = {
        let w = b.ins().band_imm(ctx_word, 4);
        b.ins().ushr_imm(w, 2)
    };
    let quiet_flag = {
        let q = b.ins().band_imm(ctx_word, 2);
        let q = b.ins().ushr_imm(q, 1);
        if kernel.has_tail_loop {
            // Every non-init pass of a tail loop is a quiet pass.
            let not_init = b.ins().icmp_imm(IntCC::Equal, init_flag, 0);
            let not_init = b.ins().uextend(types::I64, not_init);
            b.ins().bor(q, not_init)
        } else {
            q
        }
    };
    let state_ptr = initial_vals[1];
    #[cfg(debug_assertions)]
    if std::env::var_os("GXDBG_CALLRET").is_some() {
        if let Some(f) = helper_refs.get("graphix_dbg_disc") {
            let t = b.ins().iconst(types::I64, 4);
            b.ins().call(f, &[t, init_flag]);
        }
    }
    // Wire slot 2: the callee's per-call-site block; 0 for parents and
    // recursive back-edges, so consumers null-guard.
    let site_ptr = initial_vals[2];
    // Non-scalar params are cloned at entry so the body owns every
    // slot and drops them unconditionally.
    let clone_helper = helper_refs
        .get("graphix_valarray_clone")
        .expect("graphix_valarray_clone helper must be registered");
    let value_clone_helper = helper_refs
        .get("graphix_value_clone")
        .expect("graphix_value_clone helper must be registered");
    let arcstr_clone_helper = helper_refs
        .get("graphix_arcstr_clone")
        .expect("graphix_arcstr_clone helper must be registered");
    for d in kernel.abi_params() {
        // A missing input arrives as a helper-safe placeholder; the disc's
        // TAINT guards it.
        let disc = initial_vals[d.wire_slot];
        let payload_in = initial_vals[d.wire_slot + 1];
        let (payload, kind) = match d.kind {
            AbiParamKind::Scalar(p) => (payload_in, LocalKind::Scalar(p)),
            AbiParamKind::Array | AbiParamKind::Tuple | AbiParamKind::Struct => {
                let call = b.ins().call(clone_helper, &[payload_in]);
                (b.inst_results(call)[0], LocalKind::Composite)
            }
            AbiParamKind::String => {
                let call = b.ins().call(arcstr_clone_helper, &[payload_in]);
                (b.inst_results(call)[0], LocalKind::String)
            }
            AbiParamKind::Variant | AbiParamKind::Nullable | AbiParamKind::Value => {
                let call = b.ins().call(value_clone_helper, &[disc, payload_in]);
                let owned_payload = b.inst_results(call)[1];
                let kind = match d.kind {
                    AbiParamKind::Variant => LocalKind::Variant,
                    AbiParamKind::Nullable => LocalKind::Nullable,
                    _ => LocalKind::Value,
                };
                (owned_payload, kind)
            }
        };
        let disc_var = b.declare_var(types::I64);
        b.def_var(disc_var, disc);
        let payload_var = b.declare_var(local_payload_ty(kind));
        b.def_var(payload_var, payload);
        env.bind(
            d.name.clone(),
            ValueVar { disc: disc_var, payload: payload_var },
            kind,
            d.bind_id,
        );
    }
    b.seal_block(entry);
    // A tail-call rebind truncates the env back to this mark.
    let param_mark = env.mark();

    // AND of every tail-position select scrutinee's STALE bit;
    // `emit_kernel_return` folds it into the returned disc. Defined in
    // the entry block so it dominates loop-carried uses.
    let tail_scrut_stale_acc = {
        let v = b.declare_var(types::I64);
        let init = b.ins().iconst(types::I64, STALE);
        b.def_var(v, init);
        v
    };

    let loop_head = if kernel.has_tail_loop {
        // Sealed after the body: each TailCall adds a predecessor.
        let head = b.create_block();
        b.ins().jump(head, &[]);
        b.switch_to_block(head);
        Some(head)
    } else {
        None
    };

    let call_slots = if kernel.params.is_empty() {
        TailSlots::Positional
    } else {
        TailSlots::Named(kernel.params.as_slice())
    };
    let lower = LowerCtx {
        tail: TailCtx { loop_head, param_mark, call_slots, tail_scrut_stale_acc },
        init_flag,
        quiet_flag,
        wake_flag,
        callee_refs,
        self_thunk,
        helper_refs,
        sel_fires: std::cell::RefCell::new(Vec::new()),
        value_buf_stack: std::cell::RefCell::new(Vec::new()),
        owned_input_stack: std::cell::RefCell::new(Vec::new()),
        collection_site: std::cell::Cell::new(None),
        self_call_roots: std::cell::RefCell::new(Vec::new()),
        pending_exit: std::cell::RefCell::new(None),
        consts,
        module,
        symbols,
        symbol,
        kernel,
        builtin_apply_sites: spec.builtin_apply_sites,
        lambda_call_sites: spec.lambda_call_sites,
        self_call: spec.self_call,
        type_env: spec.type_env,
        state: StateChannel {
            ptr: state_ptr,
            enabled: spec.allow_state,
            next: std::cell::Cell::new(0),
            anchors: std::cell::RefCell::new(Vec::new()),
            self_blocks: std::cell::RefCell::new(Vec::new()),
        },
        site: StateChannel {
            ptr: site_ptr,
            enabled: !spec.allow_state,
            next: std::cell::Cell::new(0),
            anchors: std::cell::RefCell::new(Vec::new()),
            self_blocks: std::cell::RefCell::new(Vec::new()),
        },
        slot_tables: std::cell::RefCell::new(Vec::new()),
        closed_frame: std::cell::RefCell::new(None),
        callee_layouts,
        loop_depth: std::cell::Cell::new(0),
    };
    // A wedged native loop aborts to bottom on interrupt.
    if loop_head.is_some() {
        emit_interrupt_check(b, &mut env, &lower)?;
    }
    body.hook.emit(b, &mut env, &lower)?;

    if let Some(head) = loop_head {
        b.seal_block(head);
    }

    // Every abort path drops the owned set before jumping here; the
    // sentinel is discarded by `Kernel::update` via `KERNEL_ABORT`.
    let pending_exit_block = *lower.pending_exit.borrow();
    if let Some(pe) = pending_exit_block {
        b.switch_to_block(pe);
        let s0 = b.ins().iconst(types::I64, 0);
        let s1 = b.ins().iconst(types::I64, 0);
        b.ins().return_(&[s0, s1]);
    }

    b.seal_all_blocks();
    let slot_table_words = lower.state.anchors.borrow().clone();
    let words = lower.site.next.get() as u32;
    // A self-call's child block has this body's layout, which is only
    // known once emission ends.
    let self_roots: std::sync::Arc<[u32]> = {
        let mut v: Vec<u32> =
            lower.self_call_roots.borrow().iter().map(|off| (*off / 8) as u32).collect();
        v.sort_unstable();
        v.into()
    };
    kernel.site_block_words.store(words as u64, std::sync::atomic::Ordering::Relaxed);
    let mut self_blocks: Vec<kernel_abi::SelfBlock> = self_roots
        .iter()
        .map(|rel| kernel_abi::SelfBlock { rel: *rel, words, slots: self_roots.clone() })
        .collect();
    self_blocks.extend(lower.site.self_blocks.borrow().iter().cloned());
    let site_layout = SiteLayout {
        words,
        anchors: lower.site.anchors.borrow().clone().into(),
        self_blocks: self_blocks.into(),
    };
    Ok((
        lower.state.next.get(),
        slot_table_words,
        lower.state.self_blocks.borrow().clone(),
        site_layout,
    ))
}

/// A callee kernel's per-call-site state-block layout, recorded when
/// the callee body is defined and read by every caller to size the
/// block it supplies. A caller with no layout is on a recursive
/// back-edge and passes 0.
#[derive(Debug, Clone)]
pub(crate) struct SiteLayout {
    pub(crate) words: u32,
    pub(crate) anchors: std::sync::Arc<[kernel_abi::SiteAnchor]>,
    /// Words rooting per-activation block trees; the block's owner
    /// frees and resets them.
    pub(crate) self_blocks: std::sync::Arc<[kernel_abi::SelfBlock]>,
}

/// A per-slot state word's address. `Guarded` words ride a base
/// that is 0 on recursive back-edges; the consumer takes the
/// no-memory path when the base is null.
#[derive(Clone, Copy)]
pub(crate) enum SelWord {
    Sure(ClifValue),
    Guarded { base: ClifValue, addr: ClifValue },
}

/// An in-loop state-chain claim re-ensured in every enclosing loop's
/// exit block, so a zero-length epoch still truncates the chain and
/// frees the dropped subtrees.
#[derive(Clone)]
pub(crate) struct TruncRec {
    pub(super) anchor: TruncAnchor,
    /// Directory levels above the claim's own loop.
    pub(super) n_dirs: u32,
    pub(super) leaf: TruncLeaf,
    /// The leaf passed at every level, when the entries are blocks.
    pub(super) leaf_rt: Option<std::sync::Arc<kernel_abi::SiteLeaf>>,
}

#[derive(Clone, Copy)]
pub(crate) enum TruncAnchor {
    /// Instance-state word at this byte offset.
    State(i32),
    /// Per-call-site block word (base may be 0 — null-guarded).
    Site(i32),
}

#[derive(Clone, Copy)]
pub(crate) enum TruncLeaf {
    /// `graphix_slot_state_table` leaf of `len * stride` words.
    Table { stride: u32 },
    /// `graphix_slot_state_blocks` leaf (SiteLeaf-described blocks).
    Blocks,
}

/// A guarded-select site's per-slot state table in an open scaffold
/// loop.
#[derive(Clone, Copy)]
pub(crate) struct SlotTable {
    pub(super) site: ExprId,
    pub(super) base: ClifValue,
    /// The table pointer may be null and must be guarded.
    pub(super) guarded: bool,
}

/// One open scaffold loop's per-slot state tables. A select consults
/// the frame only when emitted at exactly `depth`; `len`/`src_disc`
/// dominate the loop body so a nested loop can chain its own tables.
pub(crate) struct SlotTableFrame {
    pub(super) depth: u32,
    /// The loop's slot-ordinal induction variable.
    pub(super) idx_var: Variable,
    /// The loop's slot count (post-clamp, preheader-defined).
    pub(super) len: ClifValue,
    /// The loop source's disc — its TAINT bit gates this level's
    /// logical resize in a nested chain.
    pub(super) src_disc: ClifValue,
    pub(super) tables: Vec<SlotTable>,
    /// In-loop chain claims made in this frame's body ([`TruncRec`]).
    pub(super) pending: Vec<TruncRec>,
}

/// One state-word channel: base pointer, claim counter, claim
/// registries. `state` is the per-instance channel (wire slot 1),
/// `site` the per-call-site channel (wire slot 2); only one is
/// enabled for a body.
pub(super) struct StateChannel {
    /// Base pointer (`I64`); possibly 0, so consumers null-guard where
    /// it can be absent ([`SelWord::Guarded`]).
    pub(super) ptr: ClifValue,
    /// Whether this body may claim words here: `state` only for the
    /// region root, `site` only for callees (`BodySpec::allow_state`).
    pub(super) enabled: bool,
    /// Next unclaimed word index.
    pub(super) next: std::cell::Cell<usize>,
    /// Words anchoring per-slot state-table chains, freed by the
    /// chain's owner.
    pub(super) anchors: std::cell::RefCell<Vec<kernel_abi::SiteAnchor>>,
    /// Words holding per-activation block trees owned by this channel,
    /// including callee-owned ones rebased into blocks this body carves.
    pub(super) self_blocks: std::cell::RefCell<Vec<kernel_abi::SelfBlock>>,
}

/// Tail-loop machinery for a self-recursive kernel body; empty
/// otherwise.
pub(super) struct TailCtx<'a> {
    /// The block a tail-call rebind jumps to.
    pub(super) loop_head: Option<Block>,
    /// Env mark right after the params are bound; a rebind truncates
    /// to it.
    pub(super) param_mark: usize,
    pub(super) call_slots: TailSlots<'a>,
    /// AND over every tail-position select scrutinee's STALE bit on
    /// the executed path; `emit_kernel_return` folds it into the
    /// returned disc.
    pub(super) tail_scrut_stale_acc: Variable,
}

/// How a tail-call rebind maps its args onto the kernel's params.
#[derive(Clone, Copy)]
pub(super) enum TailSlots<'a> {
    /// Hand-built test kernels: rebind by position.
    Positional,
    /// Per-source-position tail-call slot map (`KernelSig::tail_call_slots`).
    Named(&'a [kernel_abi::KernelParam]),
}

/// A closed scaffold-loop frame awaiting its exit's slot truncates.
pub(crate) struct ClosedFrame {
    pub(super) depth: u32,
    pub(super) len: ClifValue,
    pub(super) src_disc: ClifValue,
    pub(super) pending: Vec<TruncRec>,
}

/// A tail-position select's own-fire summary; a still-stale result
/// meeting `bfired` becomes TAINT fresh.
#[derive(Clone, Copy)]
pub(super) struct SelFire {
    pub(super) sound_stale: ClifValue,
    pub(super) bfired: Option<ClifValue>,
}

pub(crate) struct LowerCtx<'a> {
    /// See [`TailCtx`].
    pub(super) tail: TailCtx<'a>,
    /// Wire slot 0 bit 0: 1 on the kernel's init cycle.
    pub(super) init_flag: ClifValue,
    /// Wire slot 0 bit 1: the invocation re-derives inside a frame or
    /// tail loop that is not its own init; grants no init view.
    pub(super) quiet_flag: ClifValue,
    /// Wire slot 0 bit 2: a wake view, under which init is not genuine
    /// (`init & !wake`).
    pub(super) wake_flag: ClifValue,
    /// Per-instance state channel (wire slot 1).
    pub(super) state: StateChannel,
    /// Per-call-site state channel (wire slot 2).
    pub(super) site: StateChannel,
    /// Layouts of already-defined callees; a missing entry is a
    /// recursive back-edge (the call passes 0).
    pub(super) callee_layouts: &'a BTreeMap<usize, SiteLayout>,
    /// Open scaffold-loop frames, innermost last.
    pub(super) slot_tables: std::cell::RefCell<Vec<SlotTableFrame>>,
    /// The frame `close_slot_tables` just popped, for the loop exit's
    /// `emit_slot_truncates`.
    pub(super) closed_frame: std::cell::RefCell<Option<ClosedFrame>>,
    /// Enclosing scaffold-loop depth. State claims are refused inside
    /// loops: one static word cannot hold per-slot memory.
    pub(super) loop_depth: std::cell::Cell<u32>,
    /// Callee kernel identity (`kernel_key`) → `FuncRef`, declared in
    /// the current function before the FunctionBuilder is built.
    pub(super) callee_refs: &'a BTreeMap<usize, FuncRef>,
    /// The spill thunk a self-call takes when the remaining stack is
    /// inside the red zone.
    pub(super) self_thunk: Option<FuncRef>,
    /// `FuncRef`s for the runtime helpers, by helper name.
    pub(super) helper_refs: &'a HelperRefs,
    /// Enclosing tail-position selects' own-fire summaries, innermost
    /// last. `emit_kernel_return` folds them innermost-first.
    pub(super) sel_fires: std::cell::RefCell<Vec<SelFire>>,
    /// In-flight value bufs between `buf_new` and finalize; a
    /// whole-kernel abort drops them ([`emit_pending_cleanup`]).
    pub(super) value_buf_stack: std::cell::RefCell<Vec<Variable>>,
    /// Owned HOF input arrays in flight, freed by a pending exit inside
    /// the loop body. Finished ValArrays, not bufs.
    pub(super) owned_input_stack: std::cell::RefCell<Vec<Variable>>,
    /// The collection HOF callsite whose loop scaffold is under
    /// construction; keys a nested loop's prev-length word.
    pub(super) collection_site: std::cell::Cell<Option<ExprId>>,
    /// Site-word byte offsets rooting self-call activation trees; the
    /// block size they describe is final only after emission.
    pub(super) self_call_roots: std::cell::RefCell<Vec<i32>>,
    /// The constants this body refers to by address, in symbol order;
    /// harvested into the body's record.
    pub(super) consts: &'a std::cell::RefCell<Vec<EmitConst>>,
    /// The module, for declaring a constant's data symbol.
    pub(super) module: &'a std::cell::RefCell<&'a mut JITModule>,
    /// Where a constant symbol's address is entered for the loader.
    pub(super) symbols: &'a SymbolTable,
    /// This body's symbol; constant symbols are named under it.
    pub(super) symbol: &'a str,
    /// This body's kernel, whose cells a constant may name.
    pub(super) kernel: &'a KernelSig,
    /// The single abort block; its body is emitted at the end of
    /// `compile_into_function`. A bottomed call does not come here.
    pub(super) pending_exit: std::cell::RefCell<Option<Block>>,
    /// Sync-builtin Apply sites by spec id (`None` for callee bodies).
    pub(super) builtin_apply_sites:
        Option<&'a nohash::IntMap<ExprId, BuiltinCallSiteInfo>>,
    /// Statically-resolved lambda call sites (`None` for callee bodies).
    pub(super) lambda_call_sites: Option<&'a nohash::IntMap<ExprId, LambdaCallInfo>>,
    /// Set when this kernel is a self-recursive lambda body: the self
    /// binding and the kernel's own call descriptor.
    pub(super) self_call: Option<&'a (BindId, LambdaCallInfo)>,
    /// Type-resolution env snapshot ([`resolve_node_typ`]).
    pub(super) type_env: Option<&'a Env>,
}

/// Expand named/abstract type refs in a node's `Type` through the
/// region's env snapshot; unchanged when there is none.
pub(super) fn resolve_node_typ(ctx: &LowerCtx, t: &Type) -> Type {
    match ctx.type_env {
        Some(env) => lowering::expand_refs(t, env),
        None => t.clone(),
    }
}

/// [`kernel_abi::freeze_for_abi_normalized`], retrying through
/// [`resolve_node_typ`] when the plain freeze fails.
pub(super) fn freeze_node_typ(ctx: &LowerCtx, t: &Type) -> Option<Type> {
    kernel_abi::freeze_for_abi_normalized(t)
        .or_else(|| kernel_abi::freeze_for_abi_normalized(&resolve_node_typ(ctx, t)))
}

/// Runtime-helper `FuncRef`s, valid within one function body.
#[derive(Default)]
pub(super) struct HelperRefs {
    pub(super) refs: BTreeMap<&'static str, FuncRef>,
    /// Wire-slot count per helper, for [`BodyCx::call_helper`]'s debug
    /// assert (cranelift reports an arity mismatch only as a whole-
    /// function failure, a silent de-fuse).
    pub(super) arity: BTreeMap<&'static str, usize>,
}

impl HelperRefs {
    pub(super) fn get(&self, name: &str) -> Option<FuncRef> {
        self.refs.get(name).copied()
    }
}

/// Runtime-helper `FuncId`s, declared once per JIT module;
/// [`declare_helpers`] materializes them per function.
pub(super) struct HelperFuncIds {
    pub(super) ids: BTreeMap<&'static str, FuncId>,
    /// See [`HelperRefs::arity`].
    pub(super) arity: BTreeMap<&'static str, usize>,
}

impl HelperFuncIds {
    pub(super) fn new(module: &mut JITModule) -> Result<Self> {
        let mut ids = BTreeMap::new();
        let mut arity = BTreeMap::new();
        for h in all_helpers() {
            let sig = helper_signature(module, &h);
            let fid = module
                .declare_function(h.name, Linkage::Import, &sig)
                .with_context(|| format!("declare_function for helper `{}`", h.name))?;
            ids.insert(h.name, fid);
            arity.insert(h.name, h.params.iter().map(|p| p.len()).sum());
        }
        Ok(Self { ids, arity })
    }
}

/// The u/s extension flags apply to parameters only: the C ABI
/// requires the caller to extend narrow integer arguments, while
/// returns are read at their narrow type.
fn helper_abi_param(t: AbiTy, is_param: bool) -> AbiParam {
    match t {
        AbiTy::I64 => AbiParam::new(types::I64),
        AbiTy::I32 => AbiParam::new(types::I32),
        AbiTy::F64 => AbiParam::new(types::F64),
        AbiTy::F32 => AbiParam::new(types::F32),
        AbiTy::I16u => {
            let p = AbiParam::new(types::I16);
            if is_param { p.uext() } else { p }
        }
        AbiTy::I16s => {
            let p = AbiParam::new(types::I16);
            if is_param { p.sext() } else { p }
        }
        AbiTy::I8u => {
            let p = AbiParam::new(types::I8);
            if is_param { p.uext() } else { p }
        }
        AbiTy::I8s => {
            let p = AbiParam::new(types::I8);
            if is_param { p.sext() } else { p }
        }
    }
}

/// A helper's cranelift `Signature` from its registered [`HelperSpec`].
fn helper_signature(module: &JITModule, spec: &HelperSpec) -> Signature {
    let mut sig = Signature::new(module.isa().default_call_conv());
    for slots in spec.params {
        for t in *slots {
            sig.params.push(helper_abi_param(*t, true));
        }
    }
    for t in spec.ret {
        sig.returns.push(helper_abi_param(*t, false));
    }
    sig
}

/// Declare each helper as a `FuncRef` in `func`; call before
/// constructing the FunctionBuilder.
pub(super) fn declare_helpers(
    module: &mut JITModule,
    func: &mut cranelift_codegen::ir::Function,
    ids: &HelperFuncIds,
) -> HelperRefs {
    let mut refs = BTreeMap::new();
    for (name, fid) in ids.ids.iter() {
        let fref = module.declare_func_in_func(*fid, func);
        refs.insert(*name, fref);
    }
    HelperRefs { refs, arity: ids.arity.clone() }
}

impl netidx_core::pack::Pack for SiteLayout {
    fn encoded_len(&self) -> usize {
        let SiteLayout { words, anchors, self_blocks } = self;
        words.encoded_len()
            + crate::image::slice_len(anchors)
            + crate::image::slice_len(self_blocks)
    }

    fn encode(
        &self,
        buf: &mut impl bytes::BufMut,
    ) -> Result<(), netidx_core::pack::PackError> {
        let SiteLayout { words, anchors, self_blocks } = self;
        words.encode(buf)?;
        crate::image::slice_encode(anchors, buf)?;
        crate::image::slice_encode(self_blocks, buf)
    }

    fn decode(buf: &mut impl bytes::Buf) -> Result<Self, netidx_core::pack::PackError> {
        use netidx_core::pack::Pack;
        let words = u32::decode(buf)?;
        let anchors: Vec<kernel_abi::SiteAnchor> = Pack::decode(buf)?;
        let self_blocks: Vec<kernel_abi::SelfBlock> = Pack::decode(buf)?;
        Ok(SiteLayout { words, anchors: anchors.into(), self_blocks: self_blocks.into() })
    }
}
