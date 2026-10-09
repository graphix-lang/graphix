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
        kernel_abi::{self, AbiParamKind, KernelKey, KernelSig},
        lowering::{self, BuiltinCallSiteInfo},
    },
    typ::Type,
};
use anyhow::{Result, anyhow, bail};
use cranelift_codegen::{
    ir::{
        AbiParam, Block, FuncRef, Function, InstBuilder, Signature, Value as ClifValue,
        types,
    },
    isa::CallConv,
};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_module::FuncId;
use poolshark::local::LPooled;
use std::{
    cell::{Cell, RefCell},
    collections::BTreeMap,
};
use triomphe::Arc;

use super::{
    abi::{JitEnv, LocalKind, STALE, ValueVar, local_payload_ty},
    body::{BodyRole, BodySource, emit_interrupt_check, param_id},
    call::BufKind,
    jit::{ChunkFn, Names},
    record::EmitConst,
};

/// The kernels a body calls, as the function being built names them,
/// and by their ids, for another function of the body to import.
pub(super) struct Callees<'a> {
    pub(super) refs: &'a BTreeMap<KernelKey, FuncRef>,
    pub(super) ids: &'a BTreeMap<KernelKey, FuncId>,
    /// The spill thunk of a self-recursive body.
    pub(super) thunk: Option<FuncRef>,
    pub(super) thunk_id: Option<FuncId>,
}

pub(super) fn compile_into_function<'a>(
    b: &mut FunctionBuilder,
    kernel: &'a KernelSig,
    callees: Callees<'a>,
    helper_ids: &'a HelperFuncIds,
    consts: &'a RefCell<Vec<EmitConst>>,
    chunks: &'a RefCell<Vec<ChunkFn>>,
    names: &'a RefCell<&'a mut Names>,
    body: &'a BodySource<'a>,
    callee_layouts: &'a ahash::AHashMap<KernelKey, SiteLayout>,
) -> Result<EmittedBody> {
    let spec = &body.spec;
    let entry = b.create_block();
    b.append_block_params_for_function_params(entry);
    b.switch_to_block(entry);

    let mut env = JitEnv::new();
    let mut initial_vals: LPooled<Vec<ClifValue>> = LPooled::take();
    initial_vals.extend_from_slice(b.block_params(entry));
    // Wire slot 0 is the context word: bit 0 init, bit 1 wake. Under a
    // wake view init is not genuine: consumers read `init & !wake`.
    let ctx_word = initial_vals[0];
    let init_flag = b.ins().band_imm(ctx_word, 1);
    let wake_flag = {
        let w = b.ins().band_imm(ctx_word, 2);
        b.ins().ushr_imm(w, 1)
    };
    let helper_refs = HelperRefs::new(helper_ids, names);
    let helper = |b: &mut FunctionBuilder, name: &str| {
        helper_refs.get(b.func, name).ok_or_else(|| anyhow!("missing helper {name}"))
    };
    let state_ptr = initial_vals[1];
    #[cfg(debug_assertions)]
    if crate::dbgenv::gxdbg_callret()
        && let Some(f) = helper_refs.get(b.func, "graphix_dbg_disc")
    {
        use crate::fusion::emit_helpers::CallRetTag;
        let t = b.ins().iconst(types::I64, CallRetTag::InitFlag as i64);
        b.ins().call(f, &[t, init_flag]);
    }
    // Wire slot 2: the callee's per-call-site block; 0 for parents and
    // bodies that claim no site words.
    let site_ptr = initial_vals[2];
    // Non-scalar params are cloned at entry so the body owns every
    // slot and drops them unconditionally.
    for d in kernel.abi_params() {
        // A missing input arrives as a helper-safe placeholder; the disc's
        // TAINT guards it.
        let disc = initial_vals[d.wire_slot];
        let payload_in = initial_vals[d.wire_slot + 1];
        let (payload, kind) = match d.kind {
            AbiParamKind::Scalar(p) => (payload_in, LocalKind::Scalar(p)),
            AbiParamKind::Array | AbiParamKind::Tuple | AbiParamKind::Struct => {
                let clone = helper(b, "graphix_valarray_clone")?;
                let call = b.ins().call(clone, &[payload_in]);
                (b.inst_results(call)[0], LocalKind::Composite)
            }
            AbiParamKind::String => {
                let clone = helper(b, "graphix_arcstr_clone")?;
                let call = b.ins().call(clone, &[payload_in]);
                (b.inst_results(call)[0], LocalKind::String)
            }
            AbiParamKind::Variant | AbiParamKind::Nullable | AbiParamKind::Value => {
                let clone = helper(b, "graphix_value_clone")?;
                let call = b.ins().call(clone, &[disc, payload_in]);
                (b.inst_results(call)[1], LocalKind::Value)
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
            param_id(spec.params, d.bind_id),
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

    let lower = LowerCtx {
        tail: TailCtx {
            loop_head,
            param_mark,
            call_slots: &kernel.params,
            params: spec.params,
            tail_scrut_stale_acc,
        },
        init_flag,
        wake_flag,
        callee_refs: callees.refs,
        callee_ids: callees.ids,
        self_thunk: callees.thunk,
        self_thunk_id: callees.thunk_id,
        helper_ids,
        helper_refs,
        chunks,
        chunk: false,
        owned_floor: 0,
        in_flight_bufs: RefCell::new(Vec::new()),
        owned_input_stack: RefCell::new(Vec::new()),
        collection_site: Cell::new(None),
        self_call_roots: RefCell::new(Vec::new()),
        pending_exit: RefCell::new(None),
        consts,
        names,
        kernel,
        builtin_apply_sites: spec.builtin_apply_sites,
        lambda_call_sites: spec.lambda_call_sites,
        self_call: spec.self_call(),
        type_env: spec.type_env,
        claims: match spec.role {
            BodyRole::Parent => Channel::State,
            BodyRole::Callee(_) => Channel::Site,
        },
        state: StateChannel::new(state_ptr),
        site: StateChannel::new(site_ptr),
        slot_tables: RefCell::new(Vec::new()),
        closed_frame: RefCell::new(None),
        callee_layouts,
    };
    // A wedged native loop aborts on interrupt; the kernel keeps its last result.
    if loop_head.is_some() {
        emit_interrupt_check(b, &mut env, &lower)?;
    }
    body.hook.emit(b, &mut env, &lower)?;

    if let Some(head) = loop_head {
        b.seal_block(head);
    }

    // Every abort path drops the owned set before jumping here; the
    // sentinel is discarded by `FusedKernel::update` via `KERNEL_ABORT`.
    let pending_exit_block = *lower.pending_exit.borrow();
    if let Some(pe) = pending_exit_block {
        b.switch_to_block(pe);
        let s0 = b.ins().iconst(types::I64, 0);
        let s1 = b.ins().iconst(types::I64, 0);
        b.ins().return_(&[s0, s1]);
    }

    b.seal_all_blocks();
    let LowerCtx { state, site, self_call_roots, .. } = lower;
    let words = site.next.get() as u32;
    // A self-call's child block has this body's layout, which is only
    // known once emission ends.
    let mut slots: Vec<u32> =
        self_call_roots.into_inner().into_iter().map(|off| (off / 8) as u32).collect();
    slots.sort_unstable();
    publish_site_block_words(kernel, words)?;
    let anchors = site.anchors.into_inner();
    let nested = site.self_blocks.into_inner();
    let activation = Arc::new(kernel_abi::ActivationLayout {
        words,
        slots: slots.iter().copied().collect(),
        anchors: anchors.iter().cloned().collect(),
        nested: nested.iter().cloned().collect(),
    });
    let self_blocks: Vec<kernel_abi::SelfBlock> = slots
        .iter()
        .map(|rel| kernel_abi::SelfBlock { rel: *rel, layout: activation.clone() })
        .chain(nested)
        .collect();
    Ok(EmittedBody {
        state_words: state.next.get(),
        slot_table_words: state.anchors.into_inner(),
        state_self_blocks: state.self_blocks.into_inner(),
        site_layout: SiteLayout {
            words,
            anchors: anchors.into(),
            self_blocks: self_blocks.into(),
        },
    })
}

/// Record the body's site-block size on its kernel, where a self-call
/// reads it at run time. Every build of one body lays its block out
/// alike; a build that would not is refused rather than let a child
/// block be sized for another layout.
fn publish_site_block_words(kernel: &KernelSig, words: u32) -> Result<()> {
    use std::sync::atomic::Ordering::Relaxed;
    match kernel.site_block_words.compare_exchange(0, words as u64, Relaxed, Relaxed) {
        Ok(_) => Ok(()),
        Err(prev) if prev == words as u64 => Ok(()),
        Err(prev) => bail!(
            "kernel `{}`: a site block of {words} words where another build laid out \
             {prev} — de-fuse",
            kernel.fn_name
        ),
    }
}

/// What emitting one body produced beside its CLIF.
pub(super) struct EmittedBody {
    /// Per-instance state words the body claimed (wire slot 1).
    pub(super) state_words: usize,
    /// The instance-state words anchoring per-slot chains.
    pub(super) slot_table_words: Vec<kernel_abi::SiteAnchor>,
    /// The instance-state words rooting per-activation block trees.
    pub(super) state_self_blocks: Vec<kernel_abi::SelfBlock>,
    /// The per-call-site block its callers supply (wire slot 2).
    pub(super) site_layout: SiteLayout,
}

/// A callee kernel's per-call-site state-block layout, recorded when
/// the callee body is defined and read by every caller to size the
/// block it supplies. A self-call has no layout yet and roots a
/// per-activation child block (`graphix_site_child_block`); any other
/// caller without one is refused.
#[derive(Debug, Clone)]
pub(crate) struct SiteLayout {
    pub(crate) words: u32,
    pub(crate) anchors: Arc<[kernel_abi::SiteAnchor]>,
    /// Words rooting per-activation block trees; the block's owner
    /// frees and resets them.
    pub(crate) self_blocks: Arc<[kernel_abi::SelfBlock]>,
}

/// A state word's address: an instance's or a call site's word, or a
/// loop slot's (a prev-length word, a first-call word, an in-loop call
/// site's block anchor). `Guarded` words ride a site block base, null
/// only for a body that claims no site words; the consumer takes the
/// no-memory path when it is.
#[derive(Clone, Copy)]
pub(crate) enum StateWord {
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
}

#[derive(Clone, Copy)]
pub(crate) enum TruncAnchor {
    /// Instance-state word at this byte offset.
    State(i32),
    /// Per-call-site block word (base may be 0 — null-guarded).
    Site(i32),
}

#[derive(Clone)]
pub(crate) enum TruncLeaf {
    /// `graphix_slot_state_table` leaf of `len * stride` words.
    Table { stride: u32 },
    /// `graphix_slot_state_blocks` leaf of blocks the `SiteLeaf`
    /// describes, passed at every level.
    Blocks(Arc<kernel_abi::SiteLeaf>),
}

impl TruncLeaf {
    /// The leaf descriptor every level of the chain is passed.
    pub(super) fn site_leaf(&self) -> Option<&Arc<kernel_abi::SiteLeaf>> {
        match self {
            TruncLeaf::Table { .. } => None,
            TruncLeaf::Blocks(l) => Some(l),
        }
    }
}

/// A state site's per-slot table in an open scaffold loop.
#[derive(Clone, Copy)]
pub(crate) struct SlotTable {
    pub(super) site: ExprId,
    pub(super) base: ClifValue,
    /// The table pointer may be null and must be guarded.
    pub(super) guarded: bool,
}

/// One open scaffold loop's per-slot state tables. A site consults the
/// frame only when emitted at exactly `depth`; `len`/`src_disc`
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

/// Which state channel a body claims its words from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Channel {
    /// The per-instance state (wire slot 1): the region parent's root body.
    State,
    /// The per-call-site block (wire slot 2): a callee body.
    Site,
}

/// One state-word channel: base pointer, claim counter, claim
/// registries. `state` is the per-instance channel (wire slot 1),
/// `site` the per-call-site channel (wire slot 2); a body claims from
/// its [`Channel`] only.
pub(super) struct StateChannel {
    /// Base pointer (`I64`); possibly 0, so consumers null-guard where
    /// it can be absent ([`StateWord::Guarded`]).
    pub(super) ptr: ClifValue,
    /// Next unclaimed word index.
    pub(super) next: Cell<usize>,
    /// Words anchoring per-slot state-table chains, freed by the
    /// chain's owner.
    pub(super) anchors: RefCell<Vec<kernel_abi::SiteAnchor>>,
    /// Words holding per-activation block trees owned by this channel,
    /// including callee-owned ones rebased into blocks this body carves.
    pub(super) self_blocks: RefCell<Vec<kernel_abi::SelfBlock>>,
}

impl StateChannel {
    pub(super) fn new(ptr: ClifValue) -> Self {
        Self {
            ptr,
            next: Cell::new(0),
            anchors: RefCell::new(Vec::new()),
            self_blocks: RefCell::new(Vec::new()),
        }
    }
}

/// Tail-loop machinery for a self-recursive kernel body; empty
/// otherwise.
pub(super) struct TailCtx<'a> {
    /// The block a tail-call rebind jumps to.
    pub(super) loop_head: Option<Block>,
    /// Env mark right after the params are bound; a rebind truncates
    /// to it.
    pub(super) param_mark: usize,
    /// The params a tail-call rebinds, by slot index.
    pub(super) call_slots: &'a [kernel_abi::KernelParam],
    /// The body's own ids for the params' ([`BodySpec::params`]).
    pub(super) params: &'a [(BindId, BindId)],
    /// AND over every tail-position select scrutinee's STALE bit on
    /// the executed path; `emit_kernel_return` folds it into the
    /// returned disc.
    pub(super) tail_scrut_stale_acc: Variable,
}

/// A closed scaffold-loop frame awaiting its exit's slot truncates.
pub(crate) struct ClosedFrame {
    pub(super) depth: u32,
    pub(super) len: ClifValue,
    pub(super) src_disc: ClifValue,
    pub(super) pending: Vec<TruncRec>,
}

pub(crate) struct LowerCtx<'a> {
    /// See [`TailCtx`].
    pub(super) tail: TailCtx<'a>,
    /// Wire slot 0 bit 0: 1 on the kernel's init cycle.
    pub(super) init_flag: ClifValue,
    /// Wire slot 0 bit 1: a wake view, under which init is not genuine
    /// (`init & !wake`).
    pub(super) wake_flag: ClifValue,
    /// The channel this body claims from.
    pub(super) claims: Channel,
    /// Per-instance state channel (wire slot 1).
    pub(super) state: StateChannel,
    /// Per-call-site state channel (wire slot 2).
    pub(super) site: StateChannel,
    /// Layouts of already-defined callees; a missing entry is a
    /// self-call, which roots a per-activation child block.
    pub(super) callee_layouts: &'a ahash::AHashMap<KernelKey, SiteLayout>,
    /// Open scaffold-loop frames, innermost last.
    pub(super) slot_tables: RefCell<Vec<SlotTableFrame>>,
    /// The frame `close_slot_tables` just popped, for the loop exit's
    /// `emit_slot_truncates`.
    pub(super) closed_frame: RefCell<Option<ClosedFrame>>,
    /// Callee kernel identity (`kernel_key`) → `FuncRef`, declared in
    /// the current function before the FunctionBuilder is built.
    pub(super) callee_refs: &'a BTreeMap<KernelKey, FuncRef>,
    /// The callees by id, for a chunk to import.
    pub(super) callee_ids: &'a BTreeMap<KernelKey, FuncId>,
    /// The spill thunk a self-call takes when the remaining stack is
    /// inside the red zone.
    pub(super) self_thunk: Option<FuncRef>,
    pub(super) self_thunk_id: Option<FuncId>,
    pub(super) helper_ids: &'a HelperFuncIds,
    /// `FuncRef`s for the runtime helpers, by helper name.
    pub(super) helper_refs: HelperRefs<'a>,
    /// The body's outlined loops, emitted so far.
    pub(super) chunks: &'a RefCell<Vec<ChunkFn>>,
    /// Emitting an outlined loop's chunk: the chain level its loop sizes
    /// is shared by forked chunks, sized before the fork, read-only here.
    pub(super) chunk: bool,
    /// The env's first locals, below this mark, are borrowed from the
    /// body a chunk was outlined from: an abort drops only those above.
    pub(super) owned_floor: usize,
    /// In-flight bufs between their `_new` and finalize, innermost
    /// last; a whole-kernel abort drops them ([`emit_pending_cleanup`]).
    pub(super) in_flight_bufs: RefCell<Vec<(BufKind, Variable)>>,
    /// Owned values held while a sibling emits (a loop's input array,
    /// an operand awaiting the other), freed by a pending exit there
    /// ([`super::body::BodyCx::hold`]).
    pub(super) owned_input_stack: RefCell<Vec<(LocalKind, ValueVar)>>,
    /// The collection HOF callsite whose loop scaffold is under
    /// construction; keys a nested loop's prev-length word.
    pub(super) collection_site: Cell<Option<ExprId>>,
    /// Site-word byte offsets rooting self-call activation trees; the
    /// block size they describe is final only after emission.
    pub(super) self_call_roots: RefCell<Vec<i32>>,
    /// The constants this body refers to by address, in symbol order;
    /// harvested into the body's record.
    pub(super) consts: &'a RefCell<Vec<EmitConst>>,
    /// The ids the body names, for declaring a constant.
    pub(super) names: &'a RefCell<&'a mut Names>,
    /// This body's kernel, whose cells a constant may name.
    pub(super) kernel: &'a KernelSig,
    /// The single abort block; its body is emitted at the end of
    /// `compile_into_function`. A bottomed call does not come here.
    pub(super) pending_exit: RefCell<Option<Block>>,
    /// Sync-builtin Apply sites by spec id.
    pub(super) builtin_apply_sites: &'a nohash::IntMap<ExprId, BuiltinCallSiteInfo>,
    /// Statically-resolved lambda call sites.
    pub(super) lambda_call_sites: &'a nohash::IntMap<ExprId, LambdaCallInfo>,
    /// Set when this kernel is a self-recursive lambda body: the self
    /// binding and the kernel's own call descriptor.
    pub(super) self_call: Option<&'a (BindId, LambdaCallInfo)>,
    /// Type-resolution env snapshot ([`resolve_node_typ`]).
    pub(super) type_env: &'a Env,
}

impl LowerCtx<'_> {
    /// The `FuncRef` of a registered `emit_helpers` runtime helper.
    pub(super) fn helper(&self, b: &mut FunctionBuilder, name: &str) -> Result<FuncRef> {
        self.helper_refs.get(b.func, name).ok_or_else(|| anyhow!("missing helper {name}"))
    }

    /// The channel this body claims from.
    pub(super) fn claims_channel(&self) -> &StateChannel {
        match self.claims {
            Channel::State => &self.state,
            Channel::Site => &self.site,
        }
    }
}

/// Expand named/abstract type refs in a node's `Type` through the
/// region's env snapshot.
pub(crate) fn resolve_node_typ(ctx: &LowerCtx, t: &Type) -> Type {
    lowering::expand_refs(t, ctx.type_env)
}

/// [`kernel_abi::freeze_for_abi_normalized`], retrying through
/// [`resolve_node_typ`] when the plain freeze fails.
pub(super) fn freeze_node_typ(ctx: &LowerCtx, t: &Type) -> Option<Type> {
    kernel_abi::freeze_for_abi_normalized(t)
        .or_else(|| kernel_abi::freeze_for_abi_normalized(&resolve_node_typ(ctx, t)))
}

/// The runtime helpers a function body calls, each imported into the
/// function on its first use.
pub(super) struct HelperRefs<'a> {
    ids: &'a HelperFuncIds,
    names: &'a RefCell<&'a mut Names>,
    refs: RefCell<BTreeMap<&'static str, FuncRef>>,
}

impl<'a> HelperRefs<'a> {
    pub(super) fn new(ids: &'a HelperFuncIds, names: &'a RefCell<&'a mut Names>) -> Self {
        Self { ids, names, refs: RefCell::new(BTreeMap::new()) }
    }

    /// The helper's `FuncRef` in `func`, the body being built.
    pub(super) fn get(&self, func: &mut Function, name: &str) -> Option<FuncRef> {
        if let Some(f) = self.refs.borrow().get(name) {
            return Some(*f);
        }
        let (name, id) = self.ids.ids.get_key_value(name)?;
        let f = self.names.borrow().import_func(*id, func);
        self.refs.borrow_mut().insert(name, f);
        Some(f)
    }

    /// The helper's wire-slot count, for [`BodyCx::call_helper`]'s debug
    /// assert (cranelift reports an arity mismatch only as a whole-
    /// function failure, a silent de-fuse).
    pub(super) fn arity(&self, name: &str) -> Option<usize> {
        self.ids.arity.get(name).copied()
    }
}

/// Runtime-helper `FuncId`s, declared once per id table by `declare`;
/// [`HelperRefs`] imports them into a function as it uses them.
pub(super) struct HelperFuncIds {
    pub(super) ids: BTreeMap<&'static str, FuncId>,
    /// See [`HelperRefs::arity`].
    pub(super) arity: BTreeMap<&'static str, usize>,
}

impl HelperFuncIds {
    pub(super) fn new(
        call_conv: CallConv,
        mut declare: impl FnMut(&'static str, &Signature) -> Result<FuncId>,
    ) -> Result<Self> {
        let mut ids = BTreeMap::new();
        let mut arity = BTreeMap::new();
        for h in all_helpers() {
            let sig = helper_signature(call_conv, &h);
            ids.insert(h.name, declare(h.name, &sig)?);
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
fn helper_signature(call_conv: CallConv, spec: &HelperSpec) -> Signature {
    let mut sig = Signature::new(call_conv);
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
