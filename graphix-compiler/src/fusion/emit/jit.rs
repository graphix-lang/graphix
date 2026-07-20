//! The per-context JIT pipeline: [`JitCtx`], the kernel
//! declare/define/wrap entry points, [`WrappedKernel`]/[`Jit`],
//! and the wrapper-seam value packing
//! (`pack_value_to_u64`/`unpack_u64_to_value`).

use crate::{
    BindId, Node, Rt, UserEvent,
    env::Env,
    expr::ExprId,
    fusion::{
        CalleeBody, LambdaCallInfo,
        emit_helpers::all_helpers,
        kernel_abi::{
            self, AbiParamKind, AbiReturn, AbstractRegistry, KernelSig, PrimType,
        },
        lowering::BuiltinCallSiteInfo,
    },
};
use anyhow::{Context as AnyContext, Result, anyhow};
use arcstr::ArcStr;
use cranelift_codegen::{
    Context,
    ir::{AbiParam, FuncRef, InstBuilder, MemFlags, Signature, types},
    settings::{self, Configurable},
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module, default_libcall_names};
use netidx_value::Value;
use std::collections::BTreeMap;

use super::{
    body::{BodySource, BodySpec, NodeBodyEmitter},
    lower::{
        HelperFuncIds, KernelStrings, KernelValues, SiteLayout, compile_into_function,
        declare_helpers,
    },
    scalar::prim_to_clif,
};

// ─── JIT context ─────────────────────────────────────────────────

/// Owns the Cranelift JIT module plus reusable per-function builder
/// contexts. One `JitCtx` can compile many kernels; the compiled
/// function pointers live on it and stay valid until the ctx is
/// dropped.
pub struct JitCtx {
    module: JITModule,
    builder_ctx: FunctionBuilderContext,
    func_ctx: Context,
    /// Increments per call to [`Self::compile_kernel`] so each
    /// declared function gets a unique symbol — useful when a single
    /// graphix-level name (e.g. `iterate`) shows up across multiple
    /// fused lambdas in a single program.
    counter: u32,
    /// Pre-declared FuncIds for the `emit_helpers::*` runtime
    /// helpers — registered once at construction so per-function
    /// codegen can materialize FuncRefs to them without re-declaring.
    helper_ids: HelperFuncIds,
}

impl JitCtx {
    pub fn new() -> Result<Self> {
        let mut flag_builder = settings::builder();
        // Speed > size on the assumption that a JIT'd kernel is hot
        // by definition. PIC off (cranelift-jit requires it), no
        // colocated libcalls (we don't need any libm helpers for the
        // v1 op set).
        flag_builder.set("opt_level", "speed").context("set opt_level")?;
        flag_builder
            .set("use_colocated_libcalls", "false")
            .context("set use_colocated_libcalls")?;
        flag_builder.set("is_pic", "false").context("set is_pic")?;
        let isa_builder = cranelift_native::builder()
            .map_err(|e| anyhow!("cranelift_native::builder failed: {e}"))?;
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .context("isa_builder.finish")?;
        let mut builder = JITBuilder::with_isa(isa, default_libcall_names());
        // One contiguous up-front reservation for ALL of this module's
        // code and data, instead of the default provider's scattered
        // per-chunk mmaps. This is CORRECTNESS, not tuning: intra-module
        // calls (wrapper→kernel, cross-kernel) are colocated
        // (Linkage::Local), which lowers to a ±2GiB PC-relative
        // relocation that cranelift-jit applies with
        // `i32::try_from(dist).unwrap()` — with scattered chunks, two
        // functions can land >2GiB apart and finalize PANICS, killing
        // the runtime (the fuzzer's in-process selfcheck, ~100
        // concurrent JIT contexts, hit this reliably; an unlucky
        // single-runtime mmap layout can too). Host-helper calls were
        // never at risk (Linkage::Import lowers to movabs/Abs8). The
        // arena is a PROT_NONE reservation — real memory is committed
        // page-by-page as kernels are emitted — and exhausting it is a
        // clean error ("jit memory region exhausted") that de-fuses the
        // region onto the node-walk rather than a crash.
        const JIT_ARENA_RESERVE: usize = 256 * 1024 * 1024;
        builder.memory_provider(Box::new(
            cranelift_jit::ArenaMemoryProvider::new_with_size(JIT_ARENA_RESERVE)
                .map_err(|e| anyhow!("jit arena reservation failed: {e}"))?,
        ));
        // Make the emit_helpers entry points resolvable from JIT'd
        // code — registered BY POINTER under the registry's symbol
        // name (the same name `declare_function` uses); nothing
        // resolves helpers through the process symbol table.
        for h in all_helpers() {
            builder.symbol(h.name, h.ptr);
        }
        let mut module = JITModule::new(builder);
        let helper_ids = HelperFuncIds::new(&mut module)?;
        Ok(Self {
            module,
            builder_ctx: FunctionBuilderContext::new(),
            func_ctx: Context::new(),
            counter: 0,
            helper_ids,
        })
    }

    fn next_symbol(&mut self, fn_name: &str) -> String {
        self.counter += 1;
        format!("{fn_name}__kir_{}", self.counter)
    }
}

// ─── Public entry point ──────────────────────────────────────────

/// Define the typed kernel function in the JIT module without
/// finalizing. Returns the FuncId and Signature; the caller is
/// responsible for finalizing before extracting fn pointers.
/// Splitting this out from `compile_kernel` lets us emit a typed
/// kernel and a wrapper that calls it in the same module before a
/// single `finalize_definitions` call.
///
/// Push the kernel's parameter `AbiParam`s onto `sig` in ABI (=
/// source) order. Derives the order + wire footprint from
/// [`KernelSig::abi_params`] — the single source of truth — so this
/// and [`ensure_declared`] can't drift.
fn push_abi_params(sig: &mut Signature, kernel: &KernelSig) {
    // The leading cycle-context word(s) — currently the `event.init`
    // flag (see `CTX_WIRE_SLOTS`). Read by every constant's STALE gate.
    for _ in 0..kernel_abi::CTX_WIRE_SLOTS {
        sig.params.push(AbiParam::new(types::I64));
    }
    // Every param is two words: a disc (`I64`, carrying #219 taint) and
    // a payload at its natural CLIF type (a scalar's prim, else `I64`
    // for a pointer / value word).
    for d in kernel.abi_params() {
        sig.params.push(AbiParam::new(types::I64)); // disc
        let payload_ty = match d.kind {
            AbiParamKind::Scalar(p) => prim_to_clif(p),
            _ => types::I64,
        };
        sig.params.push(AbiParam::new(payload_ty));
    }
}

/// Push the kernel's return `AbiParam`s onto `sig` — the unified
/// Value ABI: every kernel returns two `I64` words, the genuine
/// `(disc, payload)` Value pair. Errors on the invalid bare-`Null`
/// return shape.
fn push_abi_returns(
    sig: &mut Signature,
    kernel: &KernelSig,
    registry: &AbstractRegistry,
) -> Result<()> {
    match kernel.abi_return(registry) {
        Some(AbiReturn::Pair) => {
            sig.returns.push(AbiParam::new(types::I64)); // disc
            sig.returns.push(AbiParam::new(types::I64)); // payload
        }
        None => {
            return Err(anyhow!(
                "kernel returns the bare Null type; should have \
                 widened to Nullable<T> at construction"
            ));
        }
    }
    Ok(())
}

/// When the `GRAPHIX_DUMP_CLIF` env var is set, print the just-built
/// CLIF function to stderr with a `;; clif <label>` header. The dump
/// runs after `FunctionBuilder::finalize` but before
/// `define_function`, so it shows exactly what emission produced.
/// Baked pointer constants (interned strings/values) vary run-to-run;
/// normalize large `iconst` immediates before diffing two captures.
fn maybe_dump_clif(func: &cranelift_codegen::ir::Function, label: &str) {
    static DUMP: std::sync::LazyLock<bool> =
        std::sync::LazyLock::new(|| std::env::var_os("GRAPHIX_DUMP_CLIF").is_some());
    if *DUMP {
        eprintln!(";; clif {label}\n{}", func.display());
    }
}

/// A compiled kernel exposed through a uniform calling convention,
/// suitable for dispatch from runtime code that doesn't know the
/// kernel's specific signature.
///
/// The `wrapper` field is `extern "C" fn(args: *const u64, out: *mut u64)`:
/// - `args` is a pointer to a slice of u64 slots — the context words
///   then a (disc, payload) pair per kernel parameter.
/// - `out` is a pointer to two u64 slots receiving the result's
///   genuine (disc, payload) Value pair (unified Value ABI).
///
/// The wrapper itself is JIT-compiled cranelift code that loads each
/// arg from the slot at the correct CLIF type, calls the typed
/// kernel, and stores the result into `*out` as raw bits. Pack and
/// unpack helpers ([`pack_value_to_u64`], [`unpack_u64_to_value`])
/// handle the Rust-side bit-fiddling.
///
/// Owns its `JitCtx` (for the local-module path) or holds `None` (for
/// the per-context cross-kernel-call path, where the owning `ExecCtx`'s
/// [`Jit`] module keeps the mmap'd code alive for the program's lifetime).
pub struct WrappedKernel {
    /// Type-erased entry point. Cast via transmute to the
    /// canonical `WrapperFn` signature for invocation.
    pub wrapper_fn_ptr: *const u8,
    /// `Some(_)` for kernels compiled into a private JIT module (no
    /// cross-kernel calls); the ctx keeps the mmap'd code alive. `None`
    /// for kernels compiled into the per-context [`Jit`] module — that
    /// module (owned by the `ExecCtx`) keeps them mapped for the
    /// program's lifetime, so no per-kernel ownership is needed.
    _ctx: Option<JitCtx>,
    /// The REGION-WIDE DynCall slot table this kernel's runtime
    /// [`crate::fusion::kernel::Kernel`] builds its `dyn_slots` from: the
    /// parent kernel's `fn_params` followed by each transitively-called
    /// callee's `fn_params`, in callee-declaration order. A callee body's
    /// builtin/cast/qop DynCalls bake `fn_index = base + local` where
    /// `base` is the callee's offset into THIS list — so the one combined
    /// `dyn_slots` array services the parent and every callee. Equal to
    /// the parent's `fn_params` when there are no callee DynCalls (the
    /// common case). Carried here (not on `KernelSig`) because it's
    /// region-specific — the same callee `KernelSig` lands at different
    /// `base`s in different regions — and rides into every `Kernel`.
    pub dyn_fn_params: std::sync::Arc<[kernel_abi::FnParam]>,
    /// Number of `u64` cross-invocation state words the parent's ROOT
    /// body claimed during emission (0 = stateless — the common case).
    /// The runtime `Kernel` allocates a zeroed per-INSTANCE buffer of
    /// this size and passes its pointer in wire slot 1 (see
    /// [`kernel_abi::CTX_WIRE_SLOTS`]).
    pub state_words: usize,
    /// Word indices (into the per-instance state buffer) of the ROOT
    /// body's REPLAY-memory claims (the interior-bottom taint caches —
    /// see `emit_scalar_taint_cache`). `Kernel::reset_replay` zeroes
    /// exactly these so a value cached on one evaluation-frame
    /// iteration cannot bridge a bottom on the next; semantic/config
    /// words (lifted ids, first-call flags, select memory) survive.
    pub replay_state_words: Vec<u32>,
    /// `(word index, own_levels)` of the ROOT body's per-slot
    /// state-table ANCHORS: each word holds (0 or) a `Box<Vec<u64>>`
    /// raw pointer managed by the `graphix_slot_state_table` helper,
    /// the root of a chain of owning tables mirroring the select
    /// site's loop nesting (`own_levels` directory levels, then a
    /// leaf with one selection word per slot ordinal — see
    /// [`BodyCx::open_slot_tables`]). The runtime `Kernel`'s `Drop`
    /// frees the chains (`free_slot_chain`); `reset_replay` never
    /// touches them (semantic state, like the static select memory
    /// word).
    pub slot_table_words: Vec<kernel_abi::SiteAnchor>,
    /// Per-kernel ArcStr slots that the JIT'd code references via
    /// stable `*const ArcStr` pointers. Held here so the slots live
    /// as long as the compiled function does. When this struct
    /// drops, the strings drop, decrementing the global intern
    /// table's `Arc<str>` refcounts — letting the GC pass reclaim
    /// entries whose last consumer is gone.
    _strings: KernelStrings,
    /// Per-kernel datetime/duration `Value` constants the JIT'd code
    /// references via stable `*const Value` pointers. Same lifetime
    /// discipline as `_strings`.
    _values: KernelValues,
}

unsafe impl Send for WrappedKernel {}
unsafe impl Sync for WrappedKernel {}

/// The uniform Rust-side signature the wrapper presents.
pub type WrapperFn = unsafe extern "C" fn(args: *const u64, out: *mut u64);

impl WrappedKernel {
    /// Cast the raw fn pointer to the canonical [`WrapperFn`]. Marked
    /// unsafe at the call site because `wrapper_fn_ptr` must come from
    /// a successful `compile_kernel_with_wrapper` invocation; passing
    /// a bogus pointer produces UB.
    pub unsafe fn fn_ptr(&self) -> WrapperFn {
        unsafe { std::mem::transmute(self.wrapper_fn_ptr) }
    }
}

// ─── Per-context JIT module: cross-kernel CLIF calls ─────────────
//
// All kernels from a given `ExecCtx` go into a
// single JIT module owned by that ExecCtx, so that one kernel's
// compiled code can `call` another's directly via a CLIF `call`
// instruction. The module lives as long as the ExecCtx; when the
// ExecCtx drops, the module drops and the mapped code goes with it.
//
// `by_kernel` keys by `(Arc<KernelSig> raw-pointer identity, DynCall
// base offset)` so the same `Arc<KernelSig>` referenced from multiple
// parent kernels reuses one compilation within a single ExecCtx. Names
// alone aren't unique enough — two distinct programs can both have a
// binding `foo` with a different fused kernel.
//
// The `base` is a SOUNDNESS half of the key (Stage 2 — transitive
// callee builtins). A callee body bakes `iconst(base + local_fn_index)`
// for each of its DynCalls, where `base` is the callee's offset into the
// COMBINED region-wide `dyn_slots` table (parent `fn_params` ++ each
// callee's). That offset depends on the calling REGION (parent
// `fn_params` length + callee order), so the same callee `KernelSig`
// fused into region A (base 0) and region B (base 1) needs TWO compiled
// bodies with different baked constants — keying on the pointer alone
// would hand region B region A's body and silently mis-dispatch.
// Callees that bake nothing (empty `fn_params`) are pinned at base 0 so
// they still share one compilation across regions; the parent is always
// base 0 and unique per region.
//
// The `layout` id is the OTHER soundness half (soak jul07c
// generate/crash_000000). Since the #203 cascade resolves a callee's
// inner call sites, a callee body may bake CLIF calls to SIBLING
// kernels' FuncIds — and which FuncId a sibling resolves to depends on
// the whole region's slot layout (`f0` with no DynCalls of its own,
// pinned at base 0 and shared, called `h0@baseA` from region A's
// compilation; region B's table put `h0` at a different base, and the
// shared `f0` body dispatched region A's index into region B's table —
// OOB panic across the JIT FFI boundary, or a silent wrong-slot
// dispatch when the stale index happens to be in range). The region's
// interned layout — the ordered `(kernel ptr, base)` list, parent
// first — is therefore part of the key: identical layouts resolve every
// sibling to the same FuncId and may share; different layouts compile
// fresh. Kernels that bake NEITHER fn_indices nor sibling FuncIds
// (empty `fn_params`, no non-self lambda sites) are layout-INDEPENDENT
// and use layout 0 — a true leaf shares one compilation everywhere.
//
// (Was a process-global `SHARED_JIT` static before May 2026; moved
// to per-context to align with the runtime's documented "multiple
// ExecCtxes can hold different modes without racing on shared
// global state" guarantee.)

pub struct Jit {
    ctx: JitCtx,
    /// Per-kernel cache: `(Arc<KernelSig> raw pointer, DynCall base
    /// offset, interned region layout)` → cached entry. We keep the Arc
    /// alive in the entry so the raw pointer key stays valid for the
    /// lifetime of the ExecCtx. Without it, Arc-allocator reuse could
    /// land a different KernelSig at the same address and we'd return a
    /// stale FuncId pointing at code with the wrong signature. See the
    /// module comment above for why `base` and the layout are part of
    /// the key.
    by_kernel: BTreeMap<(usize, u32, u32), CachedKernel>,
    /// Interned region layouts (the ordered `(kernel ptr, base)` list,
    /// parent first) → layout id. Ids start at 1; 0 is reserved for
    /// layout-INDEPENDENT kernels (see the module comment). The keyed
    /// pointers stay valid because every layout's kernels are pinned by
    /// their `by_kernel` entries' `_kernel` Arcs.
    layouts: BTreeMap<Vec<(usize, u32)>, u32>,
}

impl Jit {
    /// Construct a fresh per-context JIT. Initializes the cranelift
    /// module + ISA. Returns `Err` if cranelift initialization fails
    /// (rare; only happens if the target ISA isn't supported).
    pub fn new() -> Result<Self> {
        Ok(Self {
            ctx: JitCtx::new()?,
            by_kernel: BTreeMap::new(),
            layouts: BTreeMap::new(),
        })
    }

    fn intern_layout(&mut self, layout: Vec<(usize, u32)>) -> u32 {
        let next = self.layouts.len() as u32 + 1;
        *self.layouts.entry(layout).or_insert(next)
    }
}

struct CachedKernel {
    func_id: FuncId,
    signature: Signature,
    /// See [`WrappedKernel::replay_state_words`]; filled in phase 2.
    replay_state_words: Vec<u32>,
    /// See [`WrappedKernel::slot_table_words`]; filled in phase 2.
    slot_table_words: Vec<kernel_abi::SiteAnchor>,
    /// The kernel's per-call-site state-block layout ([`SiteLayout`]),
    /// filled when its body is DEFINED (phase 2, callees before
    /// parents). `None` = not yet defined — a caller emitting against
    /// a `None` layout is a recursive back-edge and passes 0.
    site_layout: Option<SiteLayout>,
    /// [`kernel_abi::SiteLeaf`]s whose addresses the compiled code
    /// baked in — kept alive with the code, like `_strings`.
    _site_leaves: Vec<std::sync::Arc<kernel_abi::SiteLeaf>>,
    /// Holds the Arc alive so its raw pointer can't be reused by a
    /// later allocation.
    _kernel: std::sync::Arc<KernelSig>,
    /// Per-kernel string table. The JIT'd code for this kernel bakes
    /// in `*const ArcStr` pointers into this table's `Box<[ArcStr]>`
    /// (struct field names, variant tags). The module's code lives
    /// for the lifetime of the owning `Jit` (which lives for the
    /// lifetime of the `ExecCtx`), so the table must too — kept
    /// here, alongside the kernel's `FuncId`. Populated empty in
    /// phase 1 (`ensure_declared`); the real table is moved in
    /// during phase 2 (`define_kernel_body`). Moving the
    /// `KernelStrings` struct around the `BTreeMap` is fine — only
    /// the `Box` pointer moves, not the heap allocation the baked-in
    /// pointers reference.
    _strings: KernelStrings,
    /// Per-kernel datetime/duration `Value` constants table — same
    /// role and lifetime as `_strings`, baked `*const Value` pointers.
    _values: KernelValues,
    /// Cross-invocation state words this kernel's body claimed when it
    /// was defined (see [`WrappedKernel::state_words`]). Recorded here
    /// so a region whose parent comes back from the cache still learns
    /// the buffer size its runtime `Kernel` must allocate.
    state_words: usize,
}

unsafe impl Send for Jit {}

/// Compile `kernel`, emitting BODIES by walking Nodes via
/// `emit_clif` recursion.
/// The kernel ABI (params / return / wrapper) still comes from each
/// `KernelSig`; only body codegen changes. The parent emits from
/// `root`; each callee with an entry in `callee_bodies` (keyed by
/// `Arc::as_ptr`, recorded by `discover_lambda_calls`) emits from its
/// body Node with its OWN discovered lambda sites (`CalleeBody.sites` —
/// #203 Phase C, so a callee's body emits ITS nested cross-kernel calls)
/// AND its own discovered builtin/cast/qop sites (`CalleeBody.apply_sites`),
/// each DynCall offset by the callee's base into the region-wide combined
/// `dyn_slots` table (assembled here onto `WrappedKernel.dyn_fn_params`).
/// A callee WITHOUT a recorded body bails (the whole region de-fuses);
/// discovery records a body for every callee it returns.
pub fn compile_kernel_with_callees_direct<R: Rt, E: UserEvent>(
    jit: &mut Jit,
    kernel: &std::sync::Arc<KernelSig>,
    callees: &[(usize, std::sync::Arc<KernelSig>)],
    root: &Node<R, E>,
    apply_sites: &nohash::IntMap<ExprId, BuiltinCallSiteInfo>,
    lambda_sites: &nohash::IntMap<ExprId, LambdaCallInfo>,
    callee_bodies: &BTreeMap<usize, CalleeBody<'_, R, E>>,
    parent_self_call: Option<&(BindId, LambdaCallInfo)>,
    type_env: &Env,
    registry: &AbstractRegistry,
    lifted: &ahash::AHashSet<BindId>,
    // REPLAY-word eligibility for the parent body: `true` for region
    // parents (frames reach their reset through the `FusedKernel`
    // node), `false` for lambda kernels (native cross-kernel entry
    // bypasses the reset — see `BodyEmitter::allow_replay_state`).
    parent_allow_replay: bool,
) -> Result<WrappedKernel> {
    // Callee bodies never lift (lifts are region-level let-bound
    // counters); only the parent emitter carries the lifted set.
    let no_lift: ahash::AHashSet<BindId> = ahash::AHashSet::default();
    let parent = NodeBodyEmitter { root, return_type: &kernel.return_type };
    let parent_spec = BodySpec {
        builtin_apply_sites: Some(apply_sites),
        lambda_call_sites: Some(lambda_sites),
        // None for region parents (a region can't self-call); the
        // collection callback path's "parent" IS a lambda kernel and passes
        // its own self info here.
        self_call: parent_self_call,
        type_env: Some(type_env),
        registry,
        // Parent slots lead the combined `dyn_slots` table.
        fn_index_offset: 0,
        // A non-recursive published body gates STALE at its return (the
        // over-fire fix). A RECURSIVE parent (a self-recursive collection
        // callback) emits its returns in tail position
        // (`emit_body_tail`), which does NOT fold the select scrutinee's
        // STALE into a constant tail arm — and a recursive kernel produces
        // a value on EVERY dispatch (it's a function call, not a reactive
        // sample), so its return must gate TAINT-only, else a stale-
        // constant base-case arm wrong-bottoms on a non-init dispatch.
        gate_stale_at_return: parent_self_call.is_none(),
        lifted,
        allow_state: true,
        allow_replay_state: parent_allow_replay,
    };
    // Assemble the region-wide DynCall slot table: parent `fn_params`
    // first (base 0), then each callee's `fn_params`, in callee
    // DISCOVERY order (`callees` is discovery-ordered — pointer order
    // was ASLR-dependent, #19). Each callee's `base` (its offset here)
    // is stamped on its emitter as `fn_index_offset` — what its body
    // bakes into DynCalls AND the cache key's `base` half. A callee
    // with no DynCalls (empty `fn_params`) bakes nothing, so it's
    // pinned at base 0 to share one compilation across regions. A
    // callee that IS the parent (a self-recursive per-slot callback)
    // shares the parent's FuncId/body (its slots ARE the parent's,
    // already at base 0) — skip it, matching the phase-1 declare loop.
    let parent_ptr = kernel_abi::kernel_key(kernel);
    let mut combined: Vec<kernel_abi::FnParam> = kernel.fn_params.to_vec();
    let callee_emitters: Vec<(usize, NodeBodyEmitter<R, E>, BodySpec)> = callees
        .iter()
        .filter_map(|(key, k)| {
            let key = *key;
            if key == parent_ptr {
                return None;
            }
            let cb = callee_bodies.get(&key)?;
            let raw_base = combined.len() as u32;
            let base = if k.fn_params.is_empty() { 0 } else { raw_base };
            combined.extend(k.fn_params.iter().cloned());
            Some((
                key,
                NodeBodyEmitter { root: cb.body, return_type: &k.return_type },
                BodySpec {
                    builtin_apply_sites: Some(&cb.apply_sites),
                    lambda_call_sites: Some(&cb.sites),
                    self_call: cb.self_call.as_ref(),
                    type_env: Some(type_env),
                    registry,
                    fn_index_offset: base,
                    // A callee body's result is CONSUMED via a cross-kernel
                    // call (its disc lost in the scalar return ABI), so it
                    // gates TAINT only — gating STALE would wrong-bottom.
                    gate_stale_at_return: false,
                    lifted: &no_lift,
                    allow_state: false,
                    allow_replay_state: false,
                },
            ))
        })
        .collect();
    let mut emitters: BTreeMap<usize, BodySource> = callee_emitters
        .iter()
        .map(|(key, em, spec)| (*key, BodySource { spec: *spec, hook: em }))
        .collect();
    emitters.insert(parent_ptr, BodySource { spec: parent_spec, hook: &parent });
    let mut wrapped =
        compile_kernel_with_callees_impl(jit, kernel, callees, &emitters, registry)?;
    // Override the parent-only default with the combined table; the
    // runtime `Kernel` builds its `dyn_slots` from this.
    wrapped.dyn_fn_params = combined.into();
    Ok(wrapped)
}

fn compile_kernel_with_callees_impl(
    jit: &mut Jit,
    kernel: &std::sync::Arc<KernelSig>,
    callees: &[(usize, std::sync::Arc<KernelSig>)],
    emitters: &BTreeMap<usize, BodySource>,
    registry: &AbstractRegistry,
) -> Result<WrappedKernel> {
    let mut to_define: Vec<(std::sync::Arc<KernelSig>, u32, u32)> = Vec::new();
    let mut defined: Vec<(usize, u32, u32)> = Vec::new();
    let r = compile_kernel_with_callees_inner(
        jit,
        kernel,
        callees,
        emitters,
        &mut to_define,
        &mut defined,
        registry,
    );
    if r.is_err() {
        // Evict every freshly-declared cache entry. On the direct path
        // a failed compile is the COMMON "doesn't fuse" signal (any
        // node without an emit_clif impl), and a kernel `Arc` can be
        // re-submitted later (lambda kernels are cached and shared
        // across call sites) — a stale entry would hand out a `FuncId`
        // whose body was never defined. It would also pin the kernel
        // `Arc` (and its declared symbol) forever.
        //
        // Entries whose bodies were never defined (not in `defined` —
        // recorded per-key, so this can't drift when the phase-2
        // definition order changes; the count-plus-forward-order
        // inference it replaces broke silently when definition
        // reversed for SiteLayout availability) also get a TRAP STUB:
        // the module is shared across every fusion attempt in this
        // ExecCtx, and cranelift's next `finalize_definitions` — from
        // any LATER successful compile — panics on a declared-but-
        // undefined Local symbol ("can't resolve symbol …"), killing
        // the runtime. An already-defined sibling body from this
        // abandoned attempt may carry a call relocation to the
        // undefined symbol, so the stub is load-bearing even though
        // nothing ever calls it (the region wasn't spliced and the
        // cache entry is gone).
        for (k, base, layout) in to_define.iter() {
            let key = (std::sync::Arc::as_ptr(k) as usize, *base, *layout);
            if let Some(entry) = jit.by_kernel.remove(&key)
                && !defined.contains(&key)
                && let Err(se) =
                    define_stub_body(&mut jit.ctx, entry.func_id, &entry.signature)
            {
                log::warn!(
                    "stub definition for abandoned kernel `{}` failed: {se:?}",
                    k.fn_name
                );
            }
        }
    }
    r
}

/// Define `fid` as a signature-conformant body that immediately traps.
/// Used when a kernel-closure build is ABANDONED after declaration: the
/// shared module must not carry declared-but-undefined Local symbols
/// into the next `finalize_definitions` (a panic), and a defined
/// sibling body may hold a call relocation to this symbol. The stub is
/// never executed — the abandoned region node-walks and its cache
/// entries are evicted.
fn define_stub_body(jit: &mut JitCtx, fid: FuncId, sig: &Signature) -> Result<()> {
    use cranelift_codegen::ir::TrapCode;
    // Same hygiene as `define_kernel_body`: the failed build that got
    // us here may have left `func_ctx`/`builder_ctx` mid-function.
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    jit.func_ctx.func.signature = sig.clone();
    jit.func_ctx.func.name = cranelift_codegen::ir::UserFuncName::user(0, fid.as_u32());
    {
        let mut b = FunctionBuilder::new(&mut jit.func_ctx.func, &mut jit.builder_ctx);
        let entry = b.create_block();
        b.append_block_params_for_function_params(entry);
        b.switch_to_block(entry);
        b.seal_block(entry);
        b.ins().trap(TrapCode::user(1).expect("valid user trap code"));
        b.finalize();
    }
    jit.module
        .define_function(fid, &mut jit.func_ctx)
        .context("define_function (abandon stub)")?;
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    Ok(())
}

fn compile_kernel_with_callees_inner(
    jit: &mut Jit,
    kernel: &std::sync::Arc<KernelSig>,
    callees: &[(usize, std::sync::Arc<KernelSig>)],
    emitters: &BTreeMap<usize, BodySource>,
    to_define: &mut Vec<(std::sync::Arc<KernelSig>, u32, u32)>,
    defined: &mut Vec<(usize, u32, u32)>,
    registry: &AbstractRegistry,
) -> Result<WrappedKernel> {
    // Phase 1 — declare every kernel in the closure (parent + all
    // transitively-reachable callees). Cached entries reuse their
    // `FuncId`; fresh ones get a freshly-declared FuncId and queue
    // for phase-2 body definition.
    //
    // `funcids` is keyed by kernel IDENTITY (`kernel_key`) — a callee
    // that IS the parent (a self-recursive per-slot callback) lands on
    // the parent's entry by pointer equality, which is how
    // self-recursion resolves to a CLIF call back to the parent.
    //
    // Each kernel's DynCall `base` offset (0 for the parent, its
    // combined-list offset for a callee) is the body emitter's
    // `fn_index_offset()` — the SAME value baked into its DynCalls — so
    // the `(ptr, base)` cache key and the compiled body agree by
    // construction.
    let parent_ptr = kernel_abi::kernel_key(kernel);
    // The region's layout — the ordered `(ptr, base)` list, parent
    // first — interned once; each kernel keys on it UNLESS its body is
    // layout-independent (bakes neither fn_indices nor sibling
    // FuncIds), which keys on the shared layout 0. See the module
    // comment on `by_kernel`.
    let layout_id = {
        let mut layout: Vec<(usize, u32)> = Vec::with_capacity(callees.len() + 1);
        layout.push((parent_ptr, 0));
        for (ptr, _) in callees {
            if *ptr == parent_ptr {
                continue;
            }
            layout.push((*ptr, emitters.get(ptr).map_or(0, |e| e.spec.fn_index_offset)));
        }
        jit.intern_layout(layout)
    };
    let layout_of = |k: &std::sync::Arc<KernelSig>| -> u32 {
        let self_ptr = kernel_abi::kernel_key(k);
        let ext_sites = emitters.get(&self_ptr).is_some_and(|e| {
            e.spec.lambda_call_sites.is_some_and(|m| {
                m.values().any(|info| kernel_abi::kernel_key(&info.kernel) != self_ptr)
            })
        });
        if k.fn_params.is_empty() && !ext_sites { 0 } else { layout_id }
    };
    let parent_layout = layout_of(kernel);
    // Insertion-ordered (parent first, then callee discovery order):
    // `define_kernel_body` imports FuncRefs by walking this list, and
    // funcref indices (fn0, fn1, …) are assigned in import order — a
    // pointer-ordered map here made the numbering ASLR-dependent (#19).
    let mut funcids: Vec<(usize, (FuncId, Signature))> = Vec::new();
    let parent_entry =
        ensure_declared(jit, kernel, 0, parent_layout, to_define, registry)?;
    funcids.push((parent_ptr, parent_entry.clone()));
    for (ptr, k) in callees {
        if *ptr == parent_ptr {
            continue;
        }
        let base = emitters.get(ptr).map_or(0, |e| e.spec.fn_index_offset);
        let layout = layout_of(k);
        let entry = ensure_declared(jit, k, base, layout, to_define, registry)?;
        funcids.push((*ptr, entry));
    }
    // Phase 2 — define each freshly-declared body. Bodies that came
    // back from the cache already had `define_function` called for
    // them on a prior compile and need no re-definition. New bodies
    // can reference any other declared FuncId (including each other,
    // for mutual recursion).
    //
    // Each body's `KernelStrings` carries the stable `*const ArcStr`
    // addresses its code baked in; store it back into the per-kernel
    // cache entry so it lives as long as the per-context module's
    // code.
    //
    // `emitters` keys body emitters by kernel identity (the parent +
    // every callee whose body discovery recorded). A kernel WITHOUT
    // an entry cannot compile — there is no other body source — so
    // bail (the whole region de-fuses). This can't fire today:
    // `discover_lambda_calls` records a body for every callee it
    // returns; the check guards the invariant rather than a known
    // path.
    // Definition order is REVERSED (deepest-discovered callees first,
    // the parent last): a caller's body emission reads its callees'
    // per-call-site state-block layouts ([`SiteLayout`], recorded at
    // definition) to size the blocks it supplies. A callee whose
    // layout is still missing at the caller's definition is a
    // recursive back-edge (self-calls, mutual-recursion cycles) — the
    // call site passes 0 and the callee's null-guards degrade to the
    // no-memory semantics (fresh transient activation).
    for (k, base, layout) in to_define.iter().rev() {
        let ptr = std::sync::Arc::as_ptr(k) as usize;
        // The emitter is keyed by pointer identity (it's the same body
        // regardless of base); the `fn_index_offset` it carries (== base)
        // is what `compile_into_function` bakes into the DynCalls.
        let body: &BodySource = emitters.get(&ptr).ok_or_else(|| {
            anyhow!(
                "no body emitter recorded for kernel `{}` — \
                     discovery must record every callee body",
                k.fn_name
            )
        })?;
        // Already-defined callee layouts, keyed by kernel identity.
        // Base/layout variants of the same body share one SiteLayout
        // (the claims are a body property), so first-found wins.
        let callee_layouts: BTreeMap<usize, SiteLayout> = jit
            .by_kernel
            .iter()
            .filter_map(|((p, _, _), e)| e.site_layout.as_ref().map(|l| (*p, l.clone())))
            .collect();
        let db = define_kernel_body(&mut jit.ctx, k, &funcids, body, &callee_layouts)?;
        defined.push((ptr, *base, *layout));
        if let Some(cached) = jit.by_kernel.get_mut(&(ptr, *base, *layout)) {
            cached._strings = db.strings;
            cached._values = db.values;
            cached.state_words = db.state_words;
            cached.replay_state_words = db.replay_words;
            cached.slot_table_words = db.slot_table_words;
            cached.site_layout = Some(db.site_layout);
            cached._site_leaves = db.site_leaves;
        }
    }
    // Phase 3 — compile the uniform wrapper for the parent and
    // finalize the module so the new code is mapped read-execute.
    let wrapper_id = define_wrapper(&mut jit.ctx, kernel, parent_entry.0, registry)?;
    jit.ctx
        .module
        .finalize_definitions()
        .context("finalize_definitions (per-context jit)")?;
    let wrapper_fn_ptr = jit.ctx.module.get_finalized_function(wrapper_id);
    // _ctx: None — the per-context module is kept alive by the Jit
    // field on ExecCtx.
    // _strings: empty here — for the cross-kernel-call path each
    // kernel's string table lives in `Jit::by_kernel[key]._strings`
    // (stored in phase 2), since the compiled code outlives this
    // `WrappedKernel` and is owned by the ExecCtx's `Jit`.
    // The parent's claimed state footprint — from its cache entry, so
    // a parent that came back from the cache (its body defined by a
    // prior compile) still sizes the runtime buffer correctly. Only
    // the parent's ROOT body may claim (callee claims would alias
    // across call sites), so callees' entries stay 0 by construction.
    let (state_words, replay_state_words, slot_table_words) = jit
        .by_kernel
        .get(&(parent_ptr, 0, parent_layout))
        .map(|e| {
            (e.state_words, e.replay_state_words.clone(), e.slot_table_words.clone())
        })
        .ok_or_else(|| anyhow!("parent kernel missing from the by_kernel cache"))?;
    Ok(WrappedKernel {
        wrapper_fn_ptr,
        _ctx: None,
        _strings: KernelStrings::empty(),
        _values: KernelValues::empty(),
        // Parent-only default (correct when there are no callee DynCalls).
        // `compile_kernel_with_callees_direct` overwrites this with the
        // combined parent++callees list when callees contribute slots.
        dyn_fn_params: kernel.fn_params.iter().cloned().collect(),
        state_words,
        replay_state_words,
        slot_table_words,
    })
}

/// Phase-1 helper: ensure `k` has a `FuncId` declared in the shared
/// module FOR THIS DynCall `base` offset. Cached kernels (by
/// `(Arc::as_ptr, base)` identity) reuse their existing entry. Freshly-
/// declared kernels are pushed onto `to_define` (with their base) so
/// phase 2 compiles their body with the matching `fn_index` offset.
fn ensure_declared(
    jit: &mut Jit,
    k: &std::sync::Arc<KernelSig>,
    base: u32,
    layout: u32,
    to_define: &mut Vec<(std::sync::Arc<KernelSig>, u32, u32)>,
    registry: &AbstractRegistry,
) -> Result<(FuncId, Signature)> {
    let key = (std::sync::Arc::as_ptr(k) as usize, base, layout);
    if let Some(e) = jit.by_kernel.get(&key) {
        return Ok((e.func_id, e.signature.clone()));
    }
    let symbol = jit.ctx.next_symbol(&k.fn_name);
    let mut sig = Signature::new(jit.ctx.module.isa().default_call_conv());
    push_abi_params(&mut sig, k);
    push_abi_returns(&mut sig, k, registry)?;
    let fid = jit
        .ctx
        .module
        .declare_function(&symbol, Linkage::Local, &sig)
        .context("declare_function (per-context jit)")?;
    jit.by_kernel.insert(
        key,
        CachedKernel {
            func_id: fid,
            signature: sig.clone(),
            _kernel: k.clone(),
            // Filled in during phase 2 by `define_kernel_body`.
            _strings: KernelStrings::empty(),
            _values: KernelValues::empty(),
            state_words: 0,
            replay_state_words: Vec::new(),
            slot_table_words: Vec::new(),
            site_layout: None,
            _site_leaves: Vec::new(),
        },
    );
    to_define.push((k.clone(), base, layout));
    Ok((fid, sig))
}

/// What defining one kernel body produced — stored onto the kernel's
/// `by_kernel` cache entry (the fields mirror the entry's).
struct DefinedBody {
    strings: KernelStrings,
    values: KernelValues,
    state_words: usize,
    replay_words: Vec<u32>,
    slot_table_words: Vec<kernel_abi::SiteAnchor>,
    site_layout: SiteLayout,
    site_leaves: Vec<std::sync::Arc<kernel_abi::SiteLeaf>>,
}

/// Phase-2 helper: compile `kernel`'s body and call `define_function`
/// on its pre-declared `FuncId`. `funcids` must contain entries for
/// the kernel itself and every callee its body's discovered lambda
/// call sites reference.
fn define_kernel_body(
    jit: &mut JitCtx,
    kernel: &std::sync::Arc<KernelSig>,
    funcids: &[(usize, (FuncId, Signature))],
    body_emitter: &BodySource,
    callee_layouts: &BTreeMap<usize, SiteLayout>,
) -> Result<DefinedBody> {
    let self_ptr = kernel_abi::kernel_key(kernel);
    let (func_id, sig) =
        funcids.iter().find(|(p, _)| *p == self_ptr).map(|(_, e)| e.clone()).ok_or_else(
            || {
                anyhow!(
                    "define_kernel_body: missing FuncId for kernel `{}` \
                 (phase-1 declare must have populated `funcids` first)",
                    kernel.fn_name
                )
            },
        )?;
    // Defensive: a *prior* kernel compile that returned `Err` before
    // its end-of-function `clear_context` leaves `func_ctx` dirty,
    // which makes the next `FunctionBuilder::new` panic
    // (`assertion failed: func_ctx.is_empty()`). Clear here so one
    // failed compile can't poison every subsequent kernel in the
    // same per-context module.
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    jit.func_ctx.func.signature = sig;
    jit.func_ctx.func.name =
        cranelift_codegen::ir::UserFuncName::user(0, func_id.as_u32());
    let (
        strings,
        values,
        state_words,
        replay_words,
        slot_table_words,
        site_layout,
        site_leaves,
    ) = {
        // Declare each lambda call site's callee as a FuncRef in this
        // function. Done before constructing the FunctionBuilder
        // because both `declare_func_in_func` and `FunctionBuilder::new`
        // borrow `jit.func_ctx.func` mutably.
        //
        // Exactly the kernels the body's discovered lambda call sites
        // reference (the parent carries the region's direct callee
        // set; a CALLEE's map is empty — its inner sites are
        // #203-unresolved, so its only cross-kernel reference is
        // itself). Keyed by kernel identity, so shadowed same-name
        // targets and sibling monomorphizations each resolve to their
        // OWN FuncId. The kernel itself is excluded from sites; a
        // self-recursive body imports its own FuncRef via `self_call`.
        let needed: ahash::AHashSet<usize> = {
            let mut s: ahash::AHashSet<usize> = body_emitter
                .spec
                .lambda_call_sites
                .map(|m| {
                    m.values()
                        .map(|info| kernel_abi::kernel_key(&info.kernel))
                        .filter(|ptr| *ptr != self_ptr)
                        .collect()
                })
                .unwrap_or_default();
            if let Some((_, info)) = body_emitter.spec.self_call {
                s.insert(kernel_abi::kernel_key(&info.kernel));
            }
            s
        };
        // Import in `funcids` order (parent first, then callee
        // discovery order): funcref indices (fn0, fn1, …) are assigned
        // by import order, and iterating `needed` directly — a set of
        // POINTERS — made the numbering ASLR-dependent (#19).
        let mut callee_refs: BTreeMap<usize, FuncRef> = BTreeMap::new();
        for (ptr, (fid, _)) in funcids {
            if !needed.contains(ptr) {
                continue;
            }
            let fref = jit.module.declare_func_in_func(*fid, &mut jit.func_ctx.func);
            callee_refs.insert(*ptr, fref);
        }
        if callee_refs.len() != needed.len() {
            return Err(anyhow!(
                "define_kernel_body: kernel `{}` calls a kernel with \
                     no entry in funcids",
                kernel.fn_name
            ));
        }
        // Lazy interning arenas — filled during emission via
        // `BodyCx::interned_str` / `interned_value`, merged into the
        // returned tables below so the baked addresses live as long
        // as the compiled code.
        let lazy_strings: std::cell::RefCell<Vec<Box<ArcStr>>> =
            std::cell::RefCell::new(Vec::new());
        let lazy_values: std::cell::RefCell<Vec<Box<Value>>> =
            std::cell::RefCell::new(Vec::new());
        let lazy_site_leaves: std::cell::RefCell<
            Vec<std::sync::Arc<kernel_abi::SiteLeaf>>,
        > = std::cell::RefCell::new(Vec::new());
        let helper_refs =
            declare_helpers(&mut jit.module, &mut jit.func_ctx.func, &jit.helper_ids);
        let mut builder =
            FunctionBuilder::new(&mut jit.func_ctx.func, &mut jit.builder_ctx);
        let (state_words, replay_words, slot_table_words, site_layout) =
            compile_into_function(
                &mut builder,
                kernel,
                &callee_refs,
                &helper_refs,
                &lazy_strings,
                &lazy_values,
                body_emitter,
                callee_layouts,
                &lazy_site_leaves,
            )?;
        builder.finalize();
        maybe_dump_clif(&jit.func_ctx.func, &kernel.fn_name);
        // Hand `strings`/`values` back to the caller, which stores
        // them in the shared module's per-kernel cache entry — the
        // JIT'd code baked in `*const ArcStr` / `*const Value`
        // pointers into these tables and the shared module's code
        // outlives this function.
        (
            KernelStrings::empty().with_lazy(lazy_strings.into_inner()),
            KernelValues::empty().with_lazy(lazy_values.into_inner()),
            state_words,
            replay_words,
            slot_table_words,
            site_layout,
            lazy_site_leaves.into_inner(),
        )
    };
    jit.module
        .define_function(func_id, &mut jit.func_ctx)
        .context("define_function (shared body)")?;
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    Ok(DefinedBody {
        strings,
        values,
        state_words,
        replay_words,
        slot_table_words,
        site_layout,
        site_leaves,
    })
}

/// Define the (args*, out*) wrapper that adapts the typed kernel to a
/// uniform Rust-side calling convention. The wrapper:
/// 1. Loads each arg from the raw u64 slot at the correct CLIF type.
/// 2. Calls the typed kernel.
/// 3. Stores the result into the out slot as raw bits.
fn define_wrapper(
    jit: &mut JitCtx,
    kernel: &KernelSig,
    typed_func_id: FuncId,
    registry: &AbstractRegistry,
) -> Result<FuncId> {
    let symbol = jit.next_symbol(&format!("{}_wrap", kernel.fn_name));
    let ptr_ty = jit.module.target_config().pointer_type();

    let mut sig = Signature::new(jit.module.isa().default_call_conv());
    sig.params.push(AbiParam::new(ptr_ty)); // args
    sig.params.push(AbiParam::new(ptr_ty)); // out

    let wrapper_id = jit
        .module
        .declare_function(&symbol, Linkage::Local, &sig)
        .context("declare_function (wrapper)")?;
    // A failure past this point leaves `wrapper_id` declared-but-
    // undefined in the shared module — stub it so the next
    // `finalize_definitions` doesn't panic (see `define_stub_body`).
    match define_wrapper_body(
        jit,
        kernel,
        typed_func_id,
        registry,
        wrapper_id,
        &sig,
        &symbol,
    ) {
        Ok(()) => Ok(wrapper_id),
        Err(e) => {
            if let Err(se) = define_stub_body(jit, wrapper_id, &sig) {
                log::warn!(
                    "stub definition for abandoned wrapper `{symbol}` failed: {se:?}"
                );
            }
            Err(e)
        }
    }
}

fn define_wrapper_body(
    jit: &mut JitCtx,
    kernel: &KernelSig,
    typed_func_id: FuncId,
    registry: &AbstractRegistry,
    wrapper_id: FuncId,
    sig: &Signature,
    symbol: &str,
) -> Result<()> {
    // Defensive clear — see `define_kernel_body`. A prior failed
    // compile must not poison the wrapper build.
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    jit.func_ctx.func.signature = sig.clone();
    jit.func_ctx.func.name =
        cranelift_codegen::ir::UserFuncName::user(0, wrapper_id.as_u32());

    {
        let typed_ref =
            jit.module.declare_func_in_func(typed_func_id, &mut jit.func_ctx.func);
        // Debug-build instrumentation: declare a FuncRef for the
        // JIT invocation counter helper so we can emit a call at
        // the start of the wrapper. Production release builds skip
        // both the FuncRef declaration and the call.
        #[cfg(debug_assertions)]
        let record_ref = {
            let fid = jit
                .helper_ids
                .ids
                .get("graphix_record_jit_invocation")
                .copied()
                .ok_or_else(|| anyhow!("missing graphix_record_jit_invocation FuncId"))?;
            jit.module.declare_func_in_func(fid, &mut jit.func_ctx.func)
        };
        let mut b = FunctionBuilder::new(&mut jit.func_ctx.func, &mut jit.builder_ctx);
        let entry = b.create_block();
        b.append_block_params_for_function_params(entry);
        b.switch_to_block(entry);
        b.seal_block(entry);

        // Bump the per-thread JIT invocation counter (debug builds
        // only). The test harness's `jit` mode reads this after
        // running a fixture to verify the JIT actually executed.
        #[cfg(debug_assertions)]
        {
            b.ins().call(record_ref, &[]);
        }

        let args_ptr = b.block_params(entry)[0];
        let out_ptr = b.block_params(entry)[1];

        // Load each kernel param from the `args` slot buffer in ABI
        // (= source) order (see `KernelSig::abi_params`): two slots
        // per param — the disc at `I64`, then the payload at its
        // natural CLIF type (a scalar's prim — sound because the
        // packer stores the sign/zero-extended Value form — else
        // `I64`). `d.wire_slot` is the param's starting 8-byte slot
        // offset.
        let mut typed_args = Vec::with_capacity(kernel.abi_param_wire_slots());
        // The leading cycle-context word(s) (the `event.init` flag) sit
        // at the front of the args buffer, BEFORE the params — load and
        // forward them first so `d.wire_slot` (already offset past them
        // in `abi_params`) indexes the params correctly.
        for i in 0..kernel_abi::CTX_WIRE_SLOTS {
            let v =
                b.ins().load(types::I64, MemFlags::trusted(), args_ptr, (i as i32) * 8);
            typed_args.push(v);
        }
        for d in kernel.abi_params() {
            // Every param is two slots: disc (`I64`) then payload at its
            // narrow CLIF type (a scalar's prim, else `I64`). #219 taint
            // rides in the disc word.
            let base = (d.wire_slot as i32) * 8;
            let disc = b.ins().load(types::I64, MemFlags::trusted(), args_ptr, base);
            let payload_ty = match d.kind {
                AbiParamKind::Scalar(p) => prim_to_clif(p),
                _ => types::I64,
            };
            let payload =
                b.ins().load(payload_ty, MemFlags::trusted(), args_ptr, base + 8);
            typed_args.push(disc);
            typed_args.push(payload);
        }

        let call = b.ins().call(typed_ref, &typed_args);
        // Unified Value ABI: every kernel returns the two-word
        // `(disc, payload)` Value pair — store both slots. (`registry`
        // stays a parameter for the signature build's abi_return
        // error path.)
        let _ = registry;
        let (r0, r1) = {
            let results = b.inst_results(call);
            (results[0], results[1])
        };
        b.ins().store(MemFlags::trusted(), r0, out_ptr, 0);
        b.ins().store(MemFlags::trusted(), r1, out_ptr, 8);
        b.ins().return_(&[]);

        b.seal_all_blocks();
        b.finalize();
    }
    maybe_dump_clif(&jit.func_ctx.func, symbol);

    jit.module
        .define_function(wrapper_id, &mut jit.func_ctx)
        .context("define_function (wrapper)")?;
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    Ok(())
}

/// Pack a scalar [`Value`] into a u64 slot for passing into a JIT'd
/// wrapper, extracting the scalar according to the declared `prim`.
/// The bits represent the primitive's value; for narrower primitives
/// the upper bits are unused (the wrapper loads at the CLIF type and
/// ignores them). `None` if `v` isn't a scalar of `prim`'s shape — a
/// kernel built against a typechecker decision the runtime disagrees
/// with (the never-tvar / obs-4 unsoundness class,
/// fuzz/triage-fuzzer-v2/typecheck_observations.md); the caller
/// substitutes the tainted missing-input placeholder so the runtime
/// SURVIVES those programs until the typechecker is sound.
/// (`Z32`/`Z64`/`V32`/`V64` accepted for the matching fixed-width prim.)
pub fn pack_value_to_u64(v: &Value, prim: PrimType) -> Option<u64> {
    macro_rules! bad {
        () => {
            return None
        };
    }
    Some(match prim {
        PrimType::I8 => match v {
            Value::I8(x) => *x as i64 as u64,
            _ => bad!(),
        },
        PrimType::I16 => match v {
            Value::I16(x) => *x as i64 as u64,
            _ => bad!(),
        },
        PrimType::I32 => match v {
            Value::I32(x) | Value::Z32(x) => *x as i64 as u64,
            _ => bad!(),
        },
        PrimType::I64 => match v {
            Value::I64(x) | Value::Z64(x) => *x as u64,
            _ => bad!(),
        },
        PrimType::U8 => match v {
            Value::U8(x) => *x as u64,
            _ => bad!(),
        },
        PrimType::U16 => match v {
            Value::U16(x) => *x as u64,
            _ => bad!(),
        },
        PrimType::U32 => match v {
            Value::U32(x) | Value::V32(x) => *x as u64,
            _ => bad!(),
        },
        PrimType::U64 => match v {
            Value::U64(x) | Value::V64(x) => *x,
            _ => bad!(),
        },
        PrimType::F32 => match v {
            Value::F32(x) => x.to_bits() as u64,
            _ => bad!(),
        },
        PrimType::F64 => match v {
            Value::F64(x) => x.to_bits(),
            _ => bad!(),
        },
        PrimType::Bool => match v {
            Value::Bool(b) => *b as u64,
            _ => bad!(),
        },
    })
}

/// Unpack a u64 slot from a JIT'd wrapper's `out` parameter into the
/// scalar [`Value`] of the declared `prim`.
pub fn unpack_u64_to_value(bits: u64, prim: PrimType) -> Value {
    match prim {
        PrimType::I8 => Value::I8(bits as i8),
        PrimType::I16 => Value::I16(bits as i16),
        PrimType::I32 => Value::I32(bits as i32),
        PrimType::I64 => Value::I64(bits as i64),
        PrimType::U8 => Value::U8(bits as u8),
        PrimType::U16 => Value::U16(bits as u16),
        PrimType::U32 => Value::U32(bits as u32),
        PrimType::U64 => Value::U64(bits),
        PrimType::F32 => Value::F32(f32::from_bits(bits as u32)),
        PrimType::F64 => Value::F64(f64::from_bits(bits)),
        PrimType::Bool => Value::Bool(bits != 0),
    }
}
