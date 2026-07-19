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
        kernel_abi::{self, AbiParamKind, AbstractRegistry, KernelSig},
        lowering::{self, BuiltinCallSiteInfo},
    },
    typ::Type,
};
use anyhow::{Context as AnyContext, Result};
use arcstr::ArcStr;
use cranelift_codegen::ir::{
    AbiParam, Block, FuncRef, InstBuilder, Signature, Value as ClifValue, types,
};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Linkage, Module};
use netidx_value::Value;
use std::collections::BTreeMap;

use super::{
    abi::{JitEnv, LocalKind, STALE, ValueVar, local_payload_ty},
    body::{BodyEmitter, emit_interrupt_check},
};

// ─── Function shape ──────────────────────────────────────────────

pub(super) fn compile_into_function(
    b: &mut FunctionBuilder,
    kernel: &KernelSig,
    callee_refs: &BTreeMap<usize, FuncRef>,
    helper_refs: &HelperRefs,
    lazy_strings: &std::cell::RefCell<Vec<Box<ArcStr>>>,
    lazy_values: &std::cell::RefCell<Vec<Box<Value>>>,
    body_emitter: &dyn BodyEmitter,
    callee_layouts: &BTreeMap<usize, SiteLayout>,
    lazy_site_leaves: &std::cell::RefCell<Vec<std::sync::Arc<kernel_abi::SiteLeaf>>>,
) -> Result<(usize, Vec<u32>, Vec<kernel_abi::SiteAnchor>, SiteLayout)> {
    let entry = b.create_block();
    b.append_block_params_for_function_params(entry);
    b.switch_to_block(entry);

    let mut env = JitEnv::new();
    // Bind each parameter to a fresh Variable, taking its initial
    // value from the entry block params. We declare params first so
    // tail-call dispatch can rely on `env.locals[0..param_count]`
    // being the params in order.
    //
    // Snapshot block params before declaring Variables: declare_var
    // takes &mut self on the FunctionBuilder, which would conflict
    // with the &[Value] returned from block_params.
    let initial_vals: Vec<ClifValue> = b.block_params(entry).to_vec();
    // The leading cycle-context words (`CTX_WIRE_SLOTS`), BEFORE the
    // params (`abi_params` wire slots start past them): the
    // `event.init` flag (1 on the kernel's init cycle — read by
    // `emit_const_node` to gate each constant's STALE bit) and the
    // per-instance state-buffer pointer (see
    // [`BodyCx::claim_state_word`]).
    let init_flag = initial_vals[0];
    let state_ptr = initial_vals[1];
    // Slot 2: the per-call-site state block base — a CALLEE body's
    // instance memory, supplied by each caller; 0 for parents and on
    // recursive back-edges (see [`kernel_abi::CTX_WIRE_SLOTS`]), so
    // every consumer null-guards ([`BodyCx::site_word`]).
    let site_ptr = initial_vals[2];
    // Bind every kernel param into the env in ABI (= source) order
    // (see `KernelSig::abi_params`), reading entry-block params from
    // `initial_vals[d.wire_slot..]`. Composite (array/tuple/struct) and
    // value-shape (variant/nullable) params are refcount-cloned at
    // entry so the body owns them outright: going-owned uniformly lets
    // tail-call rebind, `drop_owned_composites`, and function exit drop
    // every slot unconditionally without distinguishing a borrowed
    // param from a produced local. The entry clone is one relaxed
    // atomic increment per composite/value param per invocation
    // (`triomphe::Arc` clone, ~ns-scale).
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
        // Two block params per kernel param: disc (carrying #219 taint
        // from the dispatch) then payload. Composite / String / Value
        // params clone (refcount-bump) on entry so the body owns them
        // outright — a missing input arrives as a helper-safe placeholder
        // (empty ValArray / empty ArcStr / `Value::Null`) so the clone
        // runs harmlessly; the disc's TAINT guards it.
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
                // Clean the disc for the clone helper (a tainted disc is
                // an invalid tag); keep the tainted disc for the slot.
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
    // Snapshot the env now that every param is bound. A TailCall rebind
    // truncates back to exactly this — per-iteration block / select-arm
    // locals are dropped, the params stay.
    let param_mark = env.mark();

    // Control-dependence firing accumulator for TAIL-position selects
    // (`emit_select_node_tail` ANDs each scrutinee's STALE bit in;
    // `emit_kernel_return` folds it into the returned disc). A tail
    // select's arms terminate individually — there is no merge point
    // where the value-position emitter would fold the scrutinee's
    // firing — so without this a result whose VALUE chain is all-stale
    // (const-seeded acc + const capture) read quiet even when the
    // scrutinee (the loop bound) fired: `g(in0, i64:0)` with
    // `|n, acc| select n {0 => acc, _ => g(n-1, acc+cap)}` fired only
    // at init. Initialized to STALE-set (the AND identity): a kernel
    // with no tail select folds as a no-op. Defined in the ENTRY block
    // so it dominates every use (including loop-carried ones — once a
    // fired scrutinee clears it, band keeps it cleared).
    let tail_scrut_stale = {
        let v = b.declare_var(types::I64);
        let init = b.ins().iconst(types::I64, STALE);
        b.def_var(v, init);
        v
    };

    let loop_head = if kernel.has_tail_loop {
        // A separate loop_head lets multiple TailCall arms branch back
        // to a single point. We can't seal it until the body's been
        // compiled (each TailCall adds a predecessor).
        let head = b.create_block();
        b.ins().jump(head, &[]);
        b.switch_to_block(head);
        Some(head)
    } else {
        None
    };

    let tail_call_slots =
        if kernel.params.is_empty() { None } else { Some(kernel.params.as_slice()) };
    let lower = LowerCtx {
        loop_head,
        param_mark,
        init_flag,
        callee_refs,
        helper_refs,
        tail_call_slots,
        tail_scrut_stale,
        tail_sel_path: std::cell::RefCell::new(Vec::new()),
        init_override: std::cell::Cell::new(None),
        dyncall_buf_stack: std::cell::RefCell::new(Vec::new()),
        owned_input_stack: std::cell::RefCell::new(Vec::new()),
        collection_site: std::cell::Cell::new(None),
        site_replay: std::cell::RefCell::new(Vec::new()),
        site_replay_hdr: std::cell::Cell::new(None),
        pending_exit: std::cell::RefCell::new(None),
        lazy_strings,
        lazy_values,
        builtin_apply_sites: body_emitter.builtin_apply_sites(),
        lambda_call_sites: body_emitter.lambda_call_sites(),
        self_call: body_emitter.self_call(),
        type_env: body_emitter.type_env(),
        registry: body_emitter.registry(),
        fn_index_offset: body_emitter.fn_index_offset(),
        gate_stale_at_return: body_emitter.gate_stale_at_return(),
        lifted: body_emitter.lifted(),
        lifted_ord: &kernel.lifted,
        state_ptr,
        state_enabled: body_emitter.allow_state(),
        replay_enabled: body_emitter.allow_replay_state(),
        state_next: std::cell::Cell::new(kernel.lifted.len()),
        replay_words: std::cell::RefCell::new(Vec::new()),
        slot_table_words: std::cell::RefCell::new(Vec::new()),
        slot_tables: std::cell::RefCell::new(Vec::new()),
        site_ptr,
        site_enabled: !body_emitter.allow_state(),
        site_next: std::cell::Cell::new(0),
        site_anchors: std::cell::RefCell::new(Vec::new()),
        callee_layouts,
        lazy_site_leaves,
        loop_depth: std::cell::Cell::new(0),
    };
    // Cooperative-interrupt poll at the tail-loop head, before the body:
    // a wedged native rebind-and-jump loop aborts to bottom on
    // `interrupt()`/`abort()` (env holds only the owned params here, all
    // freed by the abort path's cleanup).
    if loop_head.is_some() {
        emit_interrupt_check(b, &mut env, &lower)?;
    }
    // Body codegen: the `NodeBodyEmitter` walks the region-root Node
    // via `emit_clif` recursion.
    body_emitter.emit(b, &mut env, &lower)?;

    if let Some(head) = loop_head {
        b.seal_block(head);
    }

    // If any forced-bottom path (the return-gate force, an interrupt
    // poll, a bottom abort, a callee genuine-abort check) created the
    // lazy `pending_exit` block, emit its body now: a sentinel of the
    // kernel's return type plus `return`. All abort paths already
    // dropped the owned set before jumping here, so `pending_exit`
    // itself owns nothing.
    //
    // The kernel result on the pending path is discarded by
    // `Kernel::update` (which checks `DYNCALL_PENDING` after the
    // wrapper returns), so the sentinel value is never observed —
    // it just has to be a well-typed CLIF value of the right width.
    let pending_exit_block = *lower.pending_exit.borrow();
    if let Some(pe) = pending_exit_block {
        b.switch_to_block(pe);
        // Unified Value ABI: the pending sentinel is the `(0, 0)` pair
        // for every return shape (disc 0 is never a real one-hot
        // discriminant). The caller's pending check fires from
        // `DYNCALL_PENDING` before decoding, so the bits are never
        // observed.
        let s0 = b.ins().iconst(types::I64, 0);
        let s1 = b.ins().iconst(types::I64, 0);
        b.ins().return_(&[s0, s1]);
    }

    // After body compilation, every block has been sealed except
    // possibly some auxiliary blocks (select arms, if-chain merges,
    // pending_exit). FunctionBuilder requires all blocks be sealed
    // before finalize; seal_all_blocks catches the stragglers.
    b.seal_all_blocks();
    let replay_words = lower.replay_words.borrow().clone();
    let slot_table_words = lower.slot_table_words.borrow().clone();
    let site_layout = SiteLayout {
        words: lower.site_next.get() as u32,
        anchors: lower.site_anchors.borrow().clone().into(),
        replay: lower.site_replay.borrow().clone().into(),
        replay_hdr: lower.site_replay_hdr.get().map(|h| (h / 8) as u32),
    };
    Ok((lower.state_next.get(), replay_words, slot_table_words, site_layout))
}

/// Per-kernel storage of the ArcStrs the JIT'd code references via
/// stable `*const ArcStr` pointers — the lazily-interned entries from
/// emission ([`BodyCx::interned_str`]). Each unique string is interned
/// through the global [`intern`] table (which gives back a
/// refcount-shared canonical `ArcStr`) and individually boxed so its
/// address survives Vec growth; the baked pointers point INTO the
/// boxes, valid for as long as this table (and hence the owning
/// kernel cache entry / `WrappedKernel`) is alive. There is no
/// pre-walk (a Node prewalk mirroring emission coverage would be a
/// silent-drift dangling-pointer hazard); interning happens AT
/// emission, so coverage is exact by construction.
///
/// On drop, every `ArcStr` drops, decrementing the shared `Arc<str>`
/// refcounts, so the global interner's GC pass can reclaim entries
/// whose last consumer is gone.
pub struct KernelStrings {
    lazy: Vec<Box<ArcStr>>,
}

impl KernelStrings {
    /// An empty string table — for kernels that reference no strings,
    /// or as a placeholder before the real table is built.
    pub fn empty() -> Self {
        Self { lazy: Vec::new() }
    }

    /// Attach the emission arena's entries so they live exactly as
    /// long as this table — i.e. as long as the compiled code that
    /// baked their addresses.
    pub fn with_lazy(mut self, lazy: Vec<Box<ArcStr>>) -> Self {
        self.lazy = lazy;
        self
    }
}

/// Per-kernel value-shape constants table — stable-address `Value`
/// entries whose `*const Value` the codegen bakes for value-shape
/// constants (datetime/duration/bytes/map). Mirrors
/// [`KernelStrings`]. (Scalar `Const`s aren't interned — they lower
/// inline.)
pub struct KernelValues {
    lazy: Vec<Box<Value>>,
}

impl KernelValues {
    pub fn empty() -> Self {
        Self { lazy: Vec::new() }
    }

    /// See [`KernelStrings::with_lazy`].
    pub fn with_lazy(mut self, lazy: Vec<Box<Value>>) -> Self {
        self.lazy = lazy;
        self
    }
}

/// Per-function lowering context: things that don't change across
/// statements within a single body.
/// A callee kernel's PER-CALL-SITE state-block layout: how many words
/// its body claimed from the site channel (wire slot 2), and which of
/// those words ANCHOR slot-table chains (`(rel word idx, own_levels)`
/// — heap Vecs the block's OWNER must free). Recorded when the callee
/// body is DEFINED (callees define before parents — reverse
/// `to_define` order) and read by every CALLER to size and register
/// the block it supplies; a caller that can't find a layout is
/// looking at a recursive back-edge (the callee isn't defined yet)
/// and passes 0.
#[derive(Debug, Clone)]
pub(crate) struct SiteLayout {
    pub(crate) words: u32,
    pub(crate) anchors: std::sync::Arc<[kernel_abi::SiteAnchor]>,
    /// REPLAY-KIND block words (rel word indices) — the callee's
    /// interior-bottom taint caches. A caller that can honor the reset
    /// contract (a region parent's root call site, whose
    /// `Kernel::reset_replay` zeroes its registered words) translates
    /// these into its own replay set AND stores 1 to `replay_hdr`,
    /// activating the caches; every other caller leaves the header 0
    /// and the caches stay inert. See [`LowerCtx::site_replay_hdr`].
    pub(crate) replay: std::sync::Arc<[u32]>,
    /// The block's honor header (rel word index) — `Some` iff `replay`
    /// is non-empty.
    pub(crate) replay_hdr: Option<u32>,
}

/// A selection-memory word's address, with its null-guard obligation.
/// Instance-channel and parent-chain words are backed by storage that
/// always exists (`Sure`); a CALLEE's site-block words ride a base
/// that is 0 on recursive back-edges (`Guarded`) — the consumer
/// branches to the no-memory semantics (unrefined guard term, no arm
/// init view) when the base is null, which is exactly the node-walk's
/// fresh transient activation (fresh memory ≡ no memory for a
/// single-shot activation).
#[derive(Clone, Copy)]
pub(crate) enum SelWord {
    Sure(ClifValue),
    Guarded { base: ClifValue, addr: ClifValue },
}

/// One open scaffold loop's per-slot state tables (see
/// [`BodyCx::open_slot_tables`]). `depth` is the [`LowerCtx::loop_depth`]
/// at which the loop BODY runs — a select consults the frame only when
/// emitted at exactly that depth (its own table lives in the top
/// frame; an enclosing frame's ordinal can't identify its slots).
/// `len`/`src_disc` are the loop's preheader-defined slot count and
/// source disc — they dominate the loop body, so a NESTED loop's
/// `open_slot_tables` reads them to emit the directory chain that
/// anchors its own per-slot tables (one owning level per enclosing
/// frame).
pub(crate) struct SlotTableFrame {
    pub(super) depth: u32,
    /// The loop's slot-ordinal induction variable.
    pub(super) idx_var: Variable,
    /// The loop's slot count (post-clamp, preheader-defined).
    pub(super) len: ClifValue,
    /// The loop source's disc — its TAINT bit gates this level's
    /// logical resize in a nested chain.
    pub(super) src_disc: ClifValue,
    /// Guarded-select site (`Select::spec.id`) → per-slot table base
    /// pointer (the `graphix_slot_state_table` result, valid for this
    /// kernel invocation) and whether the table is null-GUARDED (a
    /// callee loop's chain rides the possibly-0 site block).
    pub(super) tables: Vec<(ExprId, ClifValue, bool)>,
}

// CR claude for eric: ~35 fields; the per-instance state channel and
// the per-call-site channel are structural twins (ptr/enabled/next/
// replay/anchors) that want to be two instances of one sub-struct,
// with the tail machinery a third. Also proposing splitting emit.rs
// along its existing section markers (10.2k lines = 18% of the
// compiler in one file). See design/code_review_2026_07_19.md A2/A6.
pub(crate) struct LowerCtx<'a> {
    /// `Some(block)` when the kernel has a tail loop; TailCall jumps
    /// here. `None` for non-tail-recursive kernels.
    pub(super) loop_head: Option<Block>,
    /// Env snapshot taken right after all params are bound. A
    /// tail-call rebind truncates `env` back to this so per-iteration
    /// block / select-arm locals (scalar, composite, and variant)
    /// don't leak across iterations.
    pub(super) param_mark: usize,
    /// The `event.init` flag (`I64`, 1 on the kernel's init cycle),
    /// loaded from wire slot 0. Read by [`emit_const_node`] (via
    /// [`BodyCx::init_flag`]) so a constant carries [`STALE`] on every
    /// non-init cycle — it fires only at init, like the node-walk.
    pub(super) init_flag: ClifValue,
    /// The per-instance state-buffer pointer (`I64`), loaded from wire
    /// slot 1 (0 when the kernel claimed no words). See
    /// [`BodyCx::claim_state_word`].
    pub(super) state_ptr: ClifValue,
    /// Whether THIS function's body may claim state words — true only
    /// for the region parent's root body (`BodyEmitter::allow_state`).
    /// A callee is reached from arbitrarily many call sites whose
    /// claims would alias one buffer offset, so callees never claim
    /// (their stateful constructs keep the stateless approximation).
    pub(super) state_enabled: bool,
    /// Whether THIS function's body may claim REPLAY state words —
    /// true only for REGION parents (`BodyEmitter::allow_replay_state`).
    /// A lambda kernel is also entered by native cross-kernel calls
    /// that bypass the node-level `reset_replay`, so a replay word
    /// claimed there could never honor its per-iteration reset
    /// contract (jul10h 000009: a caller's native map loop bridged
    /// element 1's success into element 2's div0).
    pub(super) replay_enabled: bool,
    /// Next unclaimed state word index. The final count becomes
    /// [`WrappedKernel::state_words`] — the runtime buffer size.
    /// STARTS at `lifted_ord.len()`: the first words are reserved for
    /// the per-instance lifted-target `BindId`s (see
    /// [`KernelSig::lifted`]).
    pub(super) state_next: std::cell::Cell<usize>,
    /// Word indices of claims that are REPLAY memory (cross-invocation
    /// caches whose interp twins `reset_replay` clears — the
    /// interior-bottom taint cache), as opposed to semantic/config
    /// state (lifted ids, first-call-ever flags, select memory).
    /// `Kernel::reset_replay` zeroes exactly these, so a value cached
    /// on one evaluation-frame iteration cannot bridge a bottom on the
    /// next (see [`emit_scalar_taint_cache`]).
    pub(super) replay_words: std::cell::RefCell<Vec<u32>>,
    /// `(word index, own_levels)` of claims that ANCHOR per-slot
    /// state-table chains — a `Box<Vec<u64>>` raw pointer managed by
    /// the `graphix_slot_state_table` helper, with `own_levels`
    /// directory levels below it (one per enclosing loop), freed
    /// recursively by the runtime `Kernel`'s `Drop` (see
    /// [`BodyCx::open_slot_tables`]).
    pub(super) slot_table_words: std::cell::RefCell<Vec<kernel_abi::SiteAnchor>>,
    /// The per-call-site state block base (`I64`, wire slot 2) — a
    /// CALLEE body's instance memory, supplied by each caller.
    /// Possibly 0 (region parents, recursive back-edges): every
    /// consumer null-guards ([`SelWord::Guarded`]).
    pub(super) site_ptr: ClifValue,
    /// Whether THIS body may claim site-block words — true only for
    /// CALLEE bodies (a region parent's root body uses the instance
    /// channel; `!BodyEmitter::allow_state`).
    pub(super) site_enabled: bool,
    /// Next unclaimed site-block word. Count + anchors become the
    /// kernel's [`SiteLayout`], read by every caller.
    pub(super) site_next: std::cell::Cell<usize>,
    /// Site-block words that anchor slot-table chains
    /// (`(rel word idx, own_levels)`) — freed by the block's OWNER.
    pub(super) site_anchors: std::cell::RefCell<Vec<kernel_abi::SiteAnchor>>,
    /// Already-DEFINED callee site layouts (kernel key →
    /// [`SiteLayout`]) for the call sites this body emits; a missing
    /// entry is a recursive back-edge (the call passes 0).
    pub(super) callee_layouts: &'a BTreeMap<usize, SiteLayout>,
    /// Arena of [`kernel_abi::SiteLeaf`]s whose ADDRESSES this body's
    /// code baked into `graphix_slot_state_table`/`_blocks` calls
    /// (in-loop call-site blocks) — merged into the per-kernel cache
    /// entry so they outlive the compiled code, like `lazy_strings`.
    pub(super) lazy_site_leaves:
        &'a std::cell::RefCell<Vec<std::sync::Arc<kernel_abi::SiteLeaf>>>,
    /// Open scaffold-loop per-slot state-table frames, innermost
    /// last. Pushed/popped by [`BodyCx::open_slot_tables`] /
    /// [`BodyCx::close_slot_tables`] around every scaffold loop; a
    /// loop-body guarded select whose static state claim is refused
    /// consults the top frame for its per-slot word (see
    /// [`BodyCx::slot_select_word`]).
    pub(super) slot_tables: std::cell::RefCell<Vec<SlotTableFrame>>,
    /// The kernel's lifted connect targets, sorted — slot `i`'s id
    /// lives in state word `i`. `emit_connect_node` loads its write
    /// target from there instead of an `iconst` (per-instance
    /// identity must never be baked into the shared code).
    pub(super) lifted_ord: &'a [crate::BindId],
    /// Depth of enclosing scaffold loops at the current emission
    /// point. State claims are refused inside loops: a loop-body
    /// construct evaluates once PER SLOT per invocation, so one static
    /// word can't hold per-slot memory (the node-walk gives each slot
    /// its own state, but an inline loop body is one function).
    pub(super) loop_depth: std::cell::Cell<u32>,
    /// Cross-kernel call sites resolve their callee's kernel IDENTITY
    /// (`kernel_key` of the site's `info.kernel` Arc) through this map
    /// to a CLIF `FuncRef` — never by name (names shadow, and
    /// monomorphizations share a name). The caller must
    /// `declare_func_in_func` each callee's `FuncId` against the
    /// current function before constructing the FunctionBuilder, then
    /// pass the resulting refs in here. Empty for kernels with no
    /// lambda call sites.
    pub(super) callee_refs: &'a BTreeMap<usize, FuncRef>,
    /// `FuncRef`s for the `emit_helpers::*` runtime helpers.
    /// Declared in the current function before the FunctionBuilder
    /// is constructed (same constraint as `callee_refs`). Lookups
    /// are by helper name (e.g. `"graphix_valarray_get_i64"`).
    pub(super) helper_refs: &'a HelperRefs,
    /// Per-source-position tail-call slot map (from
    /// `KernelSig::tail_call_slots`). Drives which Variable each
    /// tail-call arg rebinds into — scalar slots hit `env.locals`,
    /// composite slots hit `env.composites`. `None` for kernels
    /// without a tail loop (or that hand-built fixtures leave
    /// empty).
    pub(super) tail_call_slots: Option<&'a [kernel_abi::KernelParam]>,
    /// Control-dependence firing accumulator: the AND over every
    /// TAIL-position select scrutinee's STALE bit on the executed path
    /// (`emit_select_node_tail` folds each in; STALE-set = no tail
    /// select fired). `emit_kernel_return` ANDs it into the returned
    /// disc's STALE so a result whose value chain is all-stale still
    /// fires when the arm-selecting scrutinee fired — the tail-arm
    /// mirror of the value-position select's merge-point fold.
    pub(super) tail_scrut_stale: Variable,
    /// The enclosing GUARDED tail selects' (selection word, taken arm
    /// index) on the CURRENT arm-emission path. `emit_select_node_tail`
    /// pushes one entry per guarded select around each arm's body
    /// emission; `emit_kernel_return` — reached only on TERMINATING
    /// paths — compares each word against the taken index and folds a
    /// selection CHANGE into the returned disc (becoming-selected
    /// fires, Eric's ruled select semantics), then records the index.
    /// Jump arms never reach a return, so intra-invocation loop
    /// mechanics (the dispatch arm oscillating per iteration) never
    /// touch the word — the compare is final-selection vs the previous
    /// invocation's final selection, which is exactly the node-walk's
    /// cross-cycle `selected` memory as seen from its depth-0 retained
    /// -arm pass (jul17c capture-dispatch pin: a guard reading a
    /// CAPTURE flips selection while the entry stays quiet — the old
    /// entry-derived seam tag could never fire it).
    pub(super) tail_sel_path: std::cell::RefCell<Vec<(SelWord, usize)>>,
    /// Effective-init override for select ARM bodies. The node-walk
    /// updates a newly-taken arm with `event.init = true`
    /// (node/select.rs — an arm WAKE is an init view: seeds re-fire,
    /// constants re-deliver, connects re-write), so
    /// `emit_select_value_arm` emits each arm body with this set to
    /// `kernel_init | selection-changed-into-this-arm` (from the
    /// select's state word) and [`BodyCx::init_flag`] returns it.
    /// Nested selects compose naturally: the inner arm ORs its own
    /// changed bit into the outer effective init it reads. `None`
    /// outside arm bodies (and inside arms of stateless-context
    /// selects, which refuse arm-lifted binds instead).
    pub(super) init_override: std::cell::Cell<Option<ClifValue>>,
    /// Stack of in-flight DynCall args bufs (`*mut LPooled<Vec<Value>>`
    /// Variables). Each DynCall pushes its args buf at `buf_new` and
    /// pops it once `graphix_dyncall` has consumed it. A forced-bottom
    /// abort (interrupt poll, return-gate force, bottom abort, callee
    /// genuine-abort check) drops whatever is still on this stack
    /// (= the args bufs of OUTER, not-yet-dispatched DynCalls) plus
    /// every owned composite/variant local via
    /// [`emit_pending_cleanup`].
    pub(super) dyncall_buf_stack: std::cell::RefCell<Vec<Variable>>,
    /// Owned HOF input arrays in flight (fresh producers — a literal,
    /// slice, or inlined-HOF result consumed by a loop scaffold).
    /// Registered by `scaffold::adopt_owned_src` at loop entry and
    /// popped by `scaffold::drop_owned_src` right after the loop's
    /// normal-path drop, so a pending exit INSIDE the loop body frees
    /// the input via [`emit_pending_cleanup`] (`graphix_valarray_drop`
    /// — these are finished owned ValArray bits, NOT bufs, hence a
    /// separate stack from `dyncall_buf_stack`). Variables here are
    /// always defined on the paths that can pend (the registering
    /// loop dominates its body) — unlike a JitEnv binding, an entry
    /// never outlives its defining region, so select arms stay safe.
    pub(super) owned_input_stack: std::cell::RefCell<Vec<Variable>>,
    /// The collection HOF callsite currently being inline-emitted
    /// (its loop scaffold is under construction). Set/restored by the
    /// MapQ/FoldQ `emit_clif_call` wrappers around each op emission
    /// ([`BodyCx::swap_collection_site`]); read by
    /// `scaffold::SlotFlags::new` so a NESTED loop's `apply` can look
    /// up its per-enclosing-slot prev-length word in the enclosing
    /// frame's chain ([`slot_state_sites`] keyed it by this id).
    pub(super) collection_site: std::cell::Cell<Option<ExprId>>,
    /// REPLAY-KIND site words claimed by this (callee) body — rel word
    /// indices into its per-call-site block, recorded on the
    /// [`SiteLayout`] so a caller that can honor the reset contract
    /// registers them for zeroing ([`emit_site_block`]'s region root
    /// path → the parent's `replay_words`). See
    /// [`BodyCx::claim_site_word_replay`].
    pub(super) site_replay: std::cell::RefCell<Vec<u32>>,
    /// The block's replay HONOR header word (rel word index), claimed
    /// lazily with the first replay-kind site claim. The cache sites
    /// set their `ok` flag FROM this word, so history only accumulates
    /// when the caller stored 1 here — a caller that can't honor the
    /// reset contract (a lambda-kernel parent, an in-loop per-slot
    /// block, a tail-loop body) leaves it 0 and the cache is INERT
    /// (today's taint-through behavior), never wrong.
    pub(super) site_replay_hdr: std::cell::Cell<Option<i32>>,
    /// Direct-path lazy interning arenas (see
    /// [`KernelStrings::lazy`]) — entries appended during emission via
    /// [`BodyCx::interned_str`] / [`BodyCx::interned_value`], harvested
    /// by `define_kernel_body` into the per-kernel tables. Each entry
    /// is individually boxed so its address survives Vec growth.
    pub(super) lazy_strings: &'a std::cell::RefCell<Vec<Box<ArcStr>>>,
    pub(super) lazy_values: &'a std::cell::RefCell<Vec<Box<Value>>>,
    /// Lazily-created single `pending_exit` block — the target of
    /// every genuine whole-kernel abort (interrupt poll, return-gate
    /// force, bottom abort, callee genuine-abort check). Its body
    /// (sentinel + `return`) is emitted at the end of
    /// `compile_into_function`. All abort paths jump here after
    /// dropping the owned set. A pended DynCall does NOT come here —
    /// it converts to a #219 tainted placeholder at its own site and
    /// continues.
    pub(super) pending_exit: std::cell::RefCell<Option<Block>>,
    /// Discovered sync-builtin Apply sites for the direct path
    /// (`Some` only when a [`BodyEmitter`] supplies them) — keyed by
    /// the Apply's spec id, consumed by [`BodyCx::builtin_site`] so
    /// `CallSite::emit_clif` can lower a registered site to a DynCall.
    pub(super) builtin_apply_sites:
        Option<&'a nohash::IntMap<ExprId, BuiltinCallSiteInfo>>,
    /// Discovered statically-resolved lambda call sites — the direct
    /// path's `ExprId → LambdaCallInfo` map (`None` for callee bodies).
    /// `CallSite::emit_clif` resolves a registered site to a CLIF
    /// `call` against `callee_refs[kernel_key(&info.kernel)]`.
    pub(super) lambda_call_sites: Option<&'a nohash::IntMap<ExprId, LambdaCallInfo>>,
    /// `Some` when this kernel is a self-recursive lambda body being
    /// Node-emitted: the self binding + the kernel's own call
    /// descriptor. Tail-position self-calls rebind-and-jump
    /// (`emit_body_tail`); value-position ones call the kernel's own
    /// FuncRef (`CallSite::emit_clif`).
    pub(super) self_call: Option<&'a (BindId, LambdaCallInfo)>,
    /// Type-resolution env snapshot for the direct path (`None` when
    /// no [`BodyEmitter`] supplies it). See [`BodyEmitter::type_env`]
    /// and [`resolve_node_typ`].
    pub(super) type_env: Option<&'a Env>,
    /// The compiling `ExecCtx`'s abstract-type registry, borrowed from
    /// [`BodyEmitter::registry`]. Read by the emit-time type
    /// classifiers (`abi_kind`/freeze/`resolve_abstract`) via
    /// [`BodyCx::registry`].
    pub(super) registry: &'a AbstractRegistry,
    /// Whether [`emit_kernel_return`] gates this body's return on STALE —
    /// `true` for a published body, `false` for a cross-kernel callee.
    /// See [`BodyEmitter::gate_stale_at_return`].
    pub(super) gate_stale_at_return: bool,
    /// Lifted connect-target bind ids (let-bound scalar counters routed
    /// in as feeders). Read by [`emit_let_node`] (seed-select) and
    /// [`emit_connect_node`] (write gate). See [`BodyEmitter::lifted`].
    pub(super) lifted: &'a ahash::AHashSet<BindId>,
    /// DynCall `fn_index` base for the body being emitted (see
    /// [`BodyEmitter::fn_index_offset`]). Added to every DynCall's
    /// `info.fn_index` at emit so a callee body indexes the region-wide
    /// combined `dyn_slots`. `0` for parents / per-slot callbacks.
    pub(super) fn_index_offset: u32,
}

/// Resolve named/abstract type refs in a node-carried `Type` through
/// the region's env snapshot (#218): node `typ` cells can hold
/// `Type::Ref`s to abstract type names (e.g. an interface's
/// `type Elem`) whose concrete rep `abi_kind`/freeze can't see —
/// `resolve_abstract` expands them (env `lookup_ref` + the abstract
/// registry). When `type_env` is `None` the type returns unchanged.
pub(super) fn resolve_node_typ(ctx: &LowerCtx, t: &Type) -> Type {
    match ctx.type_env {
        Some(env) => lowering::resolve_abstract(ctx.registry, t, env),
        None => t.clone(),
    }
}

/// [`kernel_abi::freeze_for_abi_normalized`] with an abstract-Ref resolution RETRY
/// (#218): on failure, resolve through the region's env snapshot and
/// freeze again. The retry only runs when the plain freeze fails, so
/// the common (concrete-typed) path pays nothing; a freeze that
/// already succeeds can't be changed by resolution (it was fully
/// concrete).
pub(super) fn freeze_node_typ(ctx: &LowerCtx, t: &Type) -> Option<Type> {
    kernel_abi::freeze_for_abi_normalized(ctx.registry, t).or_else(|| {
        kernel_abi::freeze_for_abi_normalized(ctx.registry, &resolve_node_typ(ctx, t))
    })
}

/// FuncRefs into the JIT module for each runtime helper, valid
/// within a single function's body. Populated by [`declare_helpers`]
/// just before constructing the FunctionBuilder.
#[derive(Default)]
pub(super) struct HelperRefs {
    pub(super) refs: BTreeMap<&'static str, FuncRef>,
}

impl HelperRefs {
    pub(super) fn get(&self, name: &str) -> Option<FuncRef> {
        self.refs.get(name).copied()
    }
}

/// Stable FuncIds for the runtime helpers, declared once per JIT
/// module at [`JitCtx::new`]. Per-function compilation calls
/// [`declare_helpers`] to materialize these as FuncRefs in the
/// current function.
pub(super) struct HelperFuncIds {
    pub(super) ids: BTreeMap<&'static str, FuncId>,
}

impl HelperFuncIds {
    pub(super) fn new(module: &mut JITModule) -> Result<Self> {
        let mut ids = BTreeMap::new();
        for h in all_helpers() {
            let sig = helper_signature(module, &h);
            let fid = module
                .declare_function(h.name, Linkage::Import, &sig)
                .with_context(|| format!("declare_function for helper `{}`", h.name))?;
            ids.insert(h.name, fid);
        }
        Ok(Self { ids })
    }
}

/// Translate one [`AbiTy`] slot of a helper's registered wire
/// signature to a cranelift `AbiParam`. The u/s extension flags apply
/// to PARAMETERS only — the C ABI requires the CALLER to extend
/// integer arguments narrower than the register (an x86 `setcc`
/// leaves the upper bits dirty; a `false` comparison pushed into a
/// composite once arrived as `true`), while returns are read at
/// their narrow type. The extension choice itself lives with the
/// helper's Rust parameter TYPE (`emit_helpers::HelperArg`), not
/// here.
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

/// Build a helper's cranelift `Signature` from its registered
/// [`HelperSpec`] — the wire shape derived from the helper's Rust
/// types by `emit_helpers::jit_helpers!`, so definition and
/// signature cannot disagree.
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

/// Declare each runtime helper FuncId as a FuncRef in the current
/// function being built. Call this with `&mut jit.func_ctx.func`
/// before constructing the FunctionBuilder (same borrowing
/// constraint as the existing `callee_refs` setup).
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
    HelperRefs { refs }
}
