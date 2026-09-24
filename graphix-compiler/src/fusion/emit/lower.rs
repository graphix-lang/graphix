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
use anyhow::{Context as AnyContext, Result, anyhow, bail};
use cranelift_codegen::ir::{
    AbiParam, Block, FuncRef, Function, InstBuilder, Signature, Value as ClifValue,
    condcodes::IntCC, types,
};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Linkage, Module};
use poolshark::local::LPooled;
use std::{
    cell::{Cell, RefCell},
    collections::BTreeMap,
};
use triomphe::Arc;

use super::{
    abi::{JitEnv, LocalKind, STALE, ValueVar, local_payload_ty},
    body::{BodySource, emit_interrupt_check},
    call::BufKind,
    record::{EmitConst, SymbolTable},
};

// XCR claude for eric: returns a named struct now; the params are distinct inputs.
// The words stay published here: `publish_site_block_words` is write-once and
// refuses a second layout, so a discarded build leaves only the value every build
// of the body computes, and nothing reads the cell before a define succeeds.
pub(super) fn compile_into_function<'a>(
    b: &mut FunctionBuilder,
    kernel: &'a KernelSig,
    callee_refs: &'a BTreeMap<usize, FuncRef>,
    self_thunk: Option<FuncRef>,
    helper_ids: &'a HelperFuncIds,
    consts: &'a RefCell<Vec<EmitConst>>,
    module: &'a RefCell<&'a mut JITModule>,
    symbols: &'a SymbolTable,
    symbol: &'a str,
    body: &'a BodySource<'a>,
    callee_layouts: &'a BTreeMap<usize, SiteLayout>,
) -> Result<EmittedBody> {
    let spec = &body.spec;
    let entry = b.create_block();
    b.append_block_params_for_function_params(entry);
    b.switch_to_block(entry);

    let mut env = JitEnv::new();
    let mut initial_vals: LPooled<Vec<ClifValue>> = LPooled::take();
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
    let helper_refs = HelperRefs::new(helper_ids, module);
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
    // recursive back-edges, so consumers null-guard.
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

    let lower = LowerCtx {
        tail: TailCtx {
            loop_head,
            param_mark,
            call_slots: &kernel.params,
            tail_scrut_stale_acc,
        },
        init_flag,
        quiet_flag,
        wake_flag,
        callee_refs,
        self_thunk,
        helper_refs,
        in_flight_bufs: RefCell::new(Vec::new()),
        owned_input_stack: RefCell::new(Vec::new()),
        collection_site: Cell::new(None),
        self_call_roots: RefCell::new(Vec::new()),
        pending_exit: RefCell::new(None),
        consts,
        module,
        symbols,
        symbol,
        kernel,
        builtin_apply_sites: spec.builtin_apply_sites,
        lambda_call_sites: spec.lambda_call_sites,
        self_call: spec.self_call,
        type_env: spec.type_env,
        claims: if spec.allow_state { Channel::State } else { Channel::Site },
        state: StateChannel::new(state_ptr),
        site: StateChannel::new(site_ptr),
        slot_tables: RefCell::new(Vec::new()),
        closed_frame: RefCell::new(None),
        callee_layouts,
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
    let mut roots: Vec<u32> =
        self_call_roots.into_inner().into_iter().map(|off| (off / 8) as u32).collect();
    roots.sort_unstable();
    let roots: Arc<[u32]> = roots.into();
    publish_site_block_words(kernel, words)?;
    let self_blocks: Vec<kernel_abi::SelfBlock> = roots
        .iter()
        .map(|rel| kernel_abi::SelfBlock { rel: *rel, words, slots: roots.clone() })
        .chain(site.self_blocks.into_inner())
        .collect();
    Ok(EmittedBody {
        state_words: state.next.get(),
        slot_table_words: state.anchors.into_inner(),
        state_self_blocks: state.self_blocks.into_inner(),
        site_layout: SiteLayout {
            words,
            anchors: site.anchors.into_inner().into(),
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
/// block it supplies. A caller with no layout is on a recursive
/// back-edge and passes 0.
#[derive(Debug, Clone)]
pub(crate) struct SiteLayout {
    pub(crate) words: u32,
    pub(crate) anchors: Arc<[kernel_abi::SiteAnchor]>,
    /// Words rooting per-activation block trees; the block's owner
    /// frees and resets them.
    pub(crate) self_blocks: Arc<[kernel_abi::SelfBlock]>,
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

// XCR claude for eric: one `claims: Channel` now says which channel a body
// claims from, and the loop depth lives on JitEnv alone. The emission state stays
// in RefCells behind `&LowerCtx`: moving it to the `&mut` side rewrites every
// emitter's access (select.rs, flow.rs, body.rs), a follow-up once those settle.
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
    /// it can be absent ([`SelWord::Guarded`]).
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
    fn new(ptr: ClifValue) -> Self {
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
    /// Wire slot 0 bit 1: the invocation re-derives inside a frame or
    /// tail loop that is not its own init; grants no init view.
    pub(super) quiet_flag: ClifValue,
    /// Wire slot 0 bit 2: a wake view, under which init is not genuine
    /// (`init & !wake`).
    pub(super) wake_flag: ClifValue,
    /// The channel this body claims from.
    pub(super) claims: Channel,
    /// Per-instance state channel (wire slot 1).
    pub(super) state: StateChannel,
    /// Per-call-site state channel (wire slot 2).
    pub(super) site: StateChannel,
    /// Layouts of already-defined callees; a missing entry is a
    /// recursive back-edge (the call passes 0).
    pub(super) callee_layouts: &'a BTreeMap<usize, SiteLayout>,
    /// Open scaffold-loop frames, innermost last.
    pub(super) slot_tables: RefCell<Vec<SlotTableFrame>>,
    /// The frame `close_slot_tables` just popped, for the loop exit's
    /// `emit_slot_truncates`.
    pub(super) closed_frame: RefCell<Option<ClosedFrame>>,
    /// Callee kernel identity (`kernel_key`) → `FuncRef`, declared in
    /// the current function before the FunctionBuilder is built.
    pub(super) callee_refs: &'a BTreeMap<usize, FuncRef>,
    /// The spill thunk a self-call takes when the remaining stack is
    /// inside the red zone.
    pub(super) self_thunk: Option<FuncRef>,
    /// `FuncRef`s for the runtime helpers, by helper name.
    pub(super) helper_refs: HelperRefs<'a>,
    /// In-flight bufs between their `_new` and finalize, innermost
    /// last; a whole-kernel abort drops them ([`emit_pending_cleanup`]).
    pub(super) in_flight_bufs: RefCell<Vec<(BufKind, Variable)>>,
    /// Owned HOF input arrays in flight, freed by a pending exit inside
    /// the loop body. Finished ValArrays, not bufs.
    pub(super) owned_input_stack: RefCell<Vec<Variable>>,
    /// The collection HOF callsite whose loop scaffold is under
    /// construction; keys a nested loop's prev-length word.
    pub(super) collection_site: Cell<Option<ExprId>>,
    /// Site-word byte offsets rooting self-call activation trees; the
    /// block size they describe is final only after emission.
    pub(super) self_call_roots: RefCell<Vec<i32>>,
    /// The constants this body refers to by address, in symbol order;
    /// harvested into the body's record.
    pub(super) consts: &'a RefCell<Vec<EmitConst>>,
    /// The module, for declaring a constant's data symbol.
    pub(super) module: &'a RefCell<&'a mut JITModule>,
    /// Where a constant symbol's address is entered for the loader.
    pub(super) symbols: &'a SymbolTable,
    /// This body's symbol; constant symbols are named under it.
    pub(super) symbol: &'a str,
    /// This body's kernel, whose cells a constant may name.
    pub(super) kernel: &'a KernelSig,
    /// The single abort block; its body is emitted at the end of
    /// `compile_into_function`. A bottomed call does not come here.
    pub(super) pending_exit: RefCell<Option<Block>>,
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

/// The runtime helpers a function body calls, each imported into the
/// function on its first use.
pub(super) struct HelperRefs<'a> {
    ids: &'a HelperFuncIds,
    module: &'a RefCell<&'a mut JITModule>,
    refs: RefCell<BTreeMap<&'static str, FuncRef>>,
}

impl<'a> HelperRefs<'a> {
    fn new(ids: &'a HelperFuncIds, module: &'a RefCell<&'a mut JITModule>) -> Self {
        Self { ids, module, refs: RefCell::new(BTreeMap::new()) }
    }

    /// The helper's `FuncRef` in `func`, the body being built.
    pub(super) fn get(&self, func: &mut Function, name: &str) -> Option<FuncRef> {
        if let Some(f) = self.refs.borrow().get(name) {
            return Some(*f);
        }
        let (name, id) = self.ids.ids.get_key_value(name)?;
        let f = self.module.borrow_mut().declare_func_in_func(*id, func);
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

/// Runtime-helper `FuncId`s, declared once per JIT module;
/// [`HelperRefs`] imports them into a function as it uses them.
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
