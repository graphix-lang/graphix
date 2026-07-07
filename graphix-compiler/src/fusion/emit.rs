//! Cranelift JIT backend: compile a fused region's node graph to
//! native machine code.
//!
//! Body code generation is DISTRIBUTED (`design/distributed_jit.md`):
//! each node's `Update::emit_clif` (and each builtin's
//! `Apply::emit_clif`) emits its own computation into the open kernel,
//! recursing into children via `child.emit_clif(cx)`. This module is
//! the shared machinery those impls call: the per-context [`Jit`]
//! (declare/define/wrap pipeline + `by_kernel` cache), [`BodyCx`] /
//! [`JitEnv`] / [`LowerCtx`] (the emission context), [`CompiledExpr`]
//! (the SSA result shape), the `emit_*_node` helpers the node impls
//! delegate to, the kernel entry/return/pending machinery, and the
//! scalar codegen primitives (`compile_bin`/`compile_cmp`/
//! `compile_cast`). The loop scaffolds HOF builtins reuse live in
//! [`scaffold`].
//!
//! ## Calling convention
//!
//! The compiled function uses the host platform's default C calling
//! convention (SystemV on Linux, Windows-fastcall on Windows).
//! Parameters are laid out in "kind-grouped" order — all scalars
//! first, then composite pointers, then two-word `Value`s — defined
//! once by [`KernelSig::abi_params`] and consumed by every ABI site
//! (the signature builder, the wrapper unpacker, the entry binder,
//! and the runtime arg packer in `kernel`). Per-kind wire shape:
//!
//! - scalar `i8/i16/i32/i64/u8/u16/u32/u64` → CLIF `I8`/`I16`/`I32`/`I64`;
//!   `f32/f64` → `F32`/`F64`; `bool` → `I8` (0 = false, non-zero = true)
//! - array/tuple/struct → one `I64` (a `*ValArray`)
//! - string → one `I64` (an `ArcStr` thin pointer)
//! - variant/nullable (`[T, null]`) → two `I64`s, a `repr(u64)`
//!   `Value`'s (disc, payload), matching the SysV two-register
//!   16-byte aggregate ABI
//!
//! The runtime calls through the uniform-slot [`WrappedKernel`]
//! (args*, out* — see `define_wrapper`).

use crate::{
    // `Update` is imported by name (not `as _`) for both its trait
    // methods (`typ`/`view`/`emit_clif`, called on region-root Nodes)
    // and the `Update::emit_clif` path form.
    BindId,
    Node,
    NodeView,
    Refs,
    Rt,
    Update,
    UserEvent,
    env::Env,
    expr::{Expr, ExprId, ExprKind},
    fusion::{
        self, CalleeBody, LambdaCallInfo,
        emit_helpers::all_symbols,
        intern,
        kernel_abi::{
            self, AbiKind, AbiParamKind, AbiReturn, AbstractRegistry, KernelSig, PrimType,
        },
        lowering::{self, BuiltinCallSiteInfo, CaptureSlot},
    },
    node::{
        Cached,
        callsite::CallSite,
        op::{BinOp, BoolOp, CmpOp},
        pattern::StructPatternNode,
        select::Select,
    },
    typ::{FnArgKind, Type},
};
use anyhow::{Context as AnyContext, Result, anyhow};
use arcstr::ArcStr;
use cranelift_codegen::{
    Context,
    ir::{
        AbiParam, Block, BlockArg, FuncRef, InstBuilder, MemFlags, Signature,
        Type as ClifType, Value as ClifValue,
        condcodes::{FloatCC, IntCC},
        types,
    },
    settings::{self, Configurable},
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module, default_libcall_names};
use netidx_value::Value;
use std::collections::BTreeMap;

/// The HOF loop scaffolds (`emit_map_loop` & co.) shared by the
/// direct node path's HOF emitters (Stage D2 of
/// `design/distributed_jit.md`).
#[path = "scaffold.rs"]
pub mod scaffold;

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
        // code. Each one is `#[no_mangle] extern "C"`, registered
        // here under the same symbol name we use in `declare_function`.
        for (name, ptr) in all_symbols() {
            builder.symbol(name, ptr);
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
/// Push the kernel's parameter `AbiParam`s onto `sig` in kind-grouped
/// ABI order. Derives the order + wire footprint from
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

/// Push the kernel's return `AbiParam`s onto `sig` (one value for
/// scalar/composite/string/unit returns, two for variant/nullable).
/// Errors on the invalid bare-`Null` return shape.
fn push_abi_returns(
    sig: &mut Signature,
    kernel: &KernelSig,
    registry: &AbstractRegistry,
) -> Result<()> {
    match kernel.abi_return(registry) {
        Some(AbiReturn::One { prim: Some(p) }) => {
            sig.returns.push(AbiParam::new(prim_to_clif(p)))
        }
        Some(AbiReturn::One { prim: None }) => {
            sig.returns.push(AbiParam::new(types::I64))
        }
        Some(AbiReturn::Two) => {
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
/// - `args` is a pointer to a slice of u64 slots, one per kernel
///   parameter, holding the raw bits of each primitive.
/// - `out` is a pointer to a single u64 slot for the return value.
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
    /// `base`s in different regions — and rides into every `Kernel`
    /// (incl. per-slot `clone_rebind`s, which clone this `Arc`).
    pub dyn_fn_params: std::sync::Arc<[kernel_abi::FnParam]>,
    /// Number of `u64` cross-invocation state words the parent's ROOT
    /// body claimed during emission (0 = stateless — the common case).
    /// The runtime `Kernel` allocates a zeroed per-INSTANCE buffer of
    /// this size and passes its pointer in wire slot 1 (see
    /// [`kernel_abi::CTX_WIRE_SLOTS`]).
    pub state_words: usize,
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
        Ok(Self { ctx: JitCtx::new()?, by_kernel: BTreeMap::new(), layouts: BTreeMap::new() })
    }

    fn intern_layout(&mut self, layout: Vec<(usize, u32)>) -> u32 {
        let next = self.layouts.len() as u32 + 1;
        *self.layouts.entry(layout).or_insert(next)
    }
}

struct CachedKernel {
    func_id: FuncId,
    signature: Signature,
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
    callees: &BTreeMap<usize, std::sync::Arc<KernelSig>>,
    root: &Node<R, E>,
    apply_sites: &nohash::IntMap<ExprId, BuiltinCallSiteInfo>,
    lambda_sites: &nohash::IntMap<ExprId, LambdaCallInfo>,
    callee_bodies: &BTreeMap<usize, CalleeBody<'_, R, E>>,
    parent_self_call: Option<&(BindId, LambdaCallInfo)>,
    type_env: &Env,
    registry: &AbstractRegistry,
    lifted: &ahash::AHashSet<BindId>,
) -> Result<WrappedKernel> {
    // Callee bodies never lift (lifts are region-level let-bound
    // counters); only the parent emitter carries the lifted set.
    let no_lift: ahash::AHashSet<BindId> = ahash::AHashSet::default();
    let parent = NodeBodyEmitter {
        root,
        return_type: &kernel.return_type,
        apply_sites,
        lambda_sites,
        // None for region parents (a region can't self-call); the
        // per-slot HOF path's "parent" IS a lambda kernel and passes
        // its own self info here.
        self_call: parent_self_call,
        type_env,
        registry,
        // Parent slots lead the combined `dyn_slots` table.
        fn_index_offset: 0,
        // A non-recursive published body gates STALE at its return (the
        // over-fire fix). A RECURSIVE parent (a self-recursive per-slot
        // HOF callback) emits its returns in tail position
        // (`emit_body_tail`), which does NOT fold the select scrutinee's
        // STALE into a constant tail arm — and a recursive kernel produces
        // a value on EVERY dispatch (it's a function call, not a reactive
        // sample), so its return must gate TAINT-only, else a stale-
        // constant base-case arm wrong-bottoms on a non-init dispatch.
        gate_stale: parent_self_call.is_none(),
        lifted,
        allow_state: true,
    };
    // Assemble the region-wide DynCall slot table: parent `fn_params`
    // first (base 0), then each callee's `fn_params`, in callee order.
    // Each callee's `base` (its offset here) is stamped on its emitter as
    // `fn_index_offset` — what its body bakes into DynCalls AND the cache
    // key's `base` half. A callee with no DynCalls (empty `fn_params`)
    // bakes nothing, so it's pinned at base 0 to share one compilation
    // across regions. A callee that IS the parent (a self-recursive
    // per-slot callback) shares the parent's FuncId/body (its slots ARE
    // the parent's, already at base 0) — skip it, matching the phase-1
    // declare loop.
    let parent_ptr = kernel_abi::kernel_key(kernel);
    let mut combined: Vec<kernel_abi::FnParam> = kernel.fn_params.to_vec();
    let callee_emitters: Vec<(usize, NodeBodyEmitter<R, E>)> = callees
        .values()
        .filter_map(|k| {
            let key = kernel_abi::kernel_key(k);
            if key == parent_ptr {
                return None;
            }
            let cb = callee_bodies.get(&key)?;
            let raw_base = combined.len() as u32;
            let base = if k.fn_params.is_empty() { 0 } else { raw_base };
            combined.extend(k.fn_params.iter().cloned());
            Some((
                key,
                NodeBodyEmitter {
                    root: cb.body,
                    return_type: &k.return_type,
                    apply_sites: &cb.apply_sites,
                    lambda_sites: &cb.sites,
                    self_call: cb.self_call.as_ref(),
                    type_env,
                    registry,
                    fn_index_offset: base,
                    // A callee body's result is CONSUMED via a cross-kernel
                    // call (its disc lost in the scalar return ABI), so it
                    // gates TAINT only — gating STALE would wrong-bottom.
                    gate_stale: false,
                    lifted: &no_lift,
                    allow_state: false,
                },
            ))
        })
        .collect();
    let mut emitters: BTreeMap<usize, &dyn BodyEmitter> =
        callee_emitters.iter().map(|(key, em)| (*key, em as &dyn BodyEmitter)).collect();
    emitters.insert(parent_ptr, &parent);
    let mut wrapped =
        compile_kernel_with_callees_impl(jit, kernel, callees, &emitters, registry)?;
    // Override the parent-only default with the combined table; the
    // runtime `Kernel` builds its `dyn_slots` from this (and per-slot
    // `clone_rebind`s clone the `Arc`).
    wrapped.dyn_fn_params = combined.into();
    Ok(wrapped)
}

fn compile_kernel_with_callees_impl(
    jit: &mut Jit,
    kernel: &std::sync::Arc<KernelSig>,
    callees: &BTreeMap<usize, std::sync::Arc<KernelSig>>,
    emitters: &BTreeMap<usize, &dyn BodyEmitter>,
    registry: &AbstractRegistry,
) -> Result<WrappedKernel> {
    let mut to_define: Vec<(std::sync::Arc<KernelSig>, u32, u32)> = Vec::new();
    let mut defined = 0usize;
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
        // Entries whose bodies were never defined (`i >= defined`) also
        // get a TRAP STUB: the module is shared across every fusion
        // attempt in this ExecCtx, and cranelift's next
        // `finalize_definitions` — from any LATER successful compile —
        // panics on a declared-but-undefined Local symbol ("can't
        // resolve symbol …"), killing the runtime. An already-defined
        // sibling body from this abandoned attempt may carry a call
        // relocation to the undefined symbol, so the stub is
        // load-bearing even though nothing ever calls it (the region
        // wasn't spliced and the cache entry is gone).
        for (i, (k, base, layout)) in to_define.iter().enumerate() {
            let key = (std::sync::Arc::as_ptr(k) as usize, *base, *layout);
            if let Some(entry) = jit.by_kernel.remove(&key)
                && i >= defined
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
    callees: &BTreeMap<usize, std::sync::Arc<KernelSig>>,
    emitters: &BTreeMap<usize, &dyn BodyEmitter>,
    to_define: &mut Vec<(std::sync::Arc<KernelSig>, u32, u32)>,
    defined: &mut usize,
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
            layout.push((*ptr, emitters.get(ptr).map_or(0, |e| e.fn_index_offset())));
        }
        jit.intern_layout(layout)
    };
    let layout_of = |k: &std::sync::Arc<KernelSig>| -> u32 {
        let self_ptr = kernel_abi::kernel_key(k);
        let ext_sites = emitters.get(&self_ptr).is_some_and(|e| {
            e.lambda_call_sites().is_some_and(|m| {
                m.values().any(|info| kernel_abi::kernel_key(&info.kernel) != self_ptr)
            })
        });
        if k.fn_params.is_empty() && !ext_sites { 0 } else { layout_id }
    };
    let parent_layout = layout_of(kernel);
    let mut funcids: BTreeMap<usize, (FuncId, Signature)> = BTreeMap::new();
    let parent_entry = ensure_declared(jit, kernel, 0, parent_layout, to_define, registry)?;
    funcids.insert(parent_ptr, parent_entry.clone());
    for (ptr, k) in callees {
        if *ptr == parent_ptr {
            continue;
        }
        let base = emitters.get(ptr).map_or(0, |e| e.fn_index_offset());
        let layout = layout_of(k);
        let entry = ensure_declared(jit, k, base, layout, to_define, registry)?;
        funcids.insert(*ptr, entry);
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
    for (k, base, layout) in to_define.iter() {
        let ptr = std::sync::Arc::as_ptr(k) as usize;
        // The emitter is keyed by pointer identity (it's the same body
        // regardless of base); the `fn_index_offset` it carries (== base)
        // is what `compile_into_function` bakes into the DynCalls.
        let body: &dyn BodyEmitter = *emitters.get(&ptr).ok_or_else(|| {
            anyhow!(
                "no body emitter recorded for kernel `{}` — \
                     discovery must record every callee body",
                k.fn_name
            )
        })?;
        let (strings, values, state_words) =
            define_kernel_body(&mut jit.ctx, k, &funcids, body)?;
        *defined += 1;
        if let Some(cached) = jit.by_kernel.get_mut(&(ptr, *base, *layout)) {
            cached._strings = strings;
            cached._values = values;
            cached.state_words = state_words;
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
    let state_words = jit
        .by_kernel
        .get(&(parent_ptr, 0, parent_layout))
        .map(|e| e.state_words)
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
        },
    );
    to_define.push((k.clone(), base, layout));
    Ok((fid, sig))
}

/// Phase-2 helper: compile `kernel`'s body and call `define_function`
/// on its pre-declared `FuncId`. `funcids` must contain entries for
/// the kernel itself and every callee its body's discovered lambda
/// call sites reference.
fn define_kernel_body(
    jit: &mut JitCtx,
    kernel: &std::sync::Arc<KernelSig>,
    funcids: &BTreeMap<usize, (FuncId, Signature)>,
    body_emitter: &dyn BodyEmitter,
) -> Result<(KernelStrings, KernelValues, usize)> {
    let self_ptr = kernel_abi::kernel_key(kernel);
    let (func_id, sig) = funcids.get(&self_ptr).cloned().ok_or_else(|| {
        anyhow!(
            "define_kernel_body: missing FuncId for kernel `{}` \
                 (phase-1 declare must have populated `funcids` first)",
            kernel.fn_name
        )
    })?;
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
    let (strings, values, state_words) = {
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
        let needed: std::collections::BTreeSet<usize> = {
            let mut s: std::collections::BTreeSet<usize> = body_emitter
                .lambda_call_sites()
                .map(|m| {
                    m.values()
                        .map(|info| kernel_abi::kernel_key(&info.kernel))
                        .filter(|ptr| *ptr != self_ptr)
                        .collect()
                })
                .unwrap_or_default();
            if let Some((_, info)) = body_emitter.self_call() {
                s.insert(kernel_abi::kernel_key(&info.kernel));
            }
            s
        };
        let mut callee_refs: BTreeMap<usize, FuncRef> = BTreeMap::new();
        for ptr in needed {
            let target_fid = funcids.get(&ptr).map(|(f, _)| *f).ok_or_else(|| {
                anyhow!(
                    "define_kernel_body: kernel `{}` calls a kernel with \
                         no entry in funcids",
                    kernel.fn_name
                )
            })?;
            let fref =
                jit.module.declare_func_in_func(target_fid, &mut jit.func_ctx.func);
            callee_refs.insert(ptr, fref);
        }
        // Lazy interning arenas — filled during emission via
        // `BodyCx::interned_str` / `interned_value`, merged into the
        // returned tables below so the baked addresses live as long
        // as the compiled code.
        let lazy_strings: std::cell::RefCell<Vec<Box<ArcStr>>> =
            std::cell::RefCell::new(Vec::new());
        let lazy_values: std::cell::RefCell<Vec<Box<Value>>> =
            std::cell::RefCell::new(Vec::new());
        let helper_refs =
            declare_helpers(&mut jit.module, &mut jit.func_ctx.func, &jit.helper_ids);
        let mut builder =
            FunctionBuilder::new(&mut jit.func_ctx.func, &mut jit.builder_ctx);
        let state_words = compile_into_function(
            &mut builder,
            kernel,
            &callee_refs,
            &helper_refs,
            &lazy_strings,
            &lazy_values,
            body_emitter,
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
        )
    };
    jit.module
        .define_function(func_id, &mut jit.func_ctx)
        .context("define_function (shared body)")?;
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    Ok((strings, values, state_words))
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

        // Load each kernel param from the `args` slot buffer in
        // kind-grouped ABI order (see `KernelSig::abi_params`). Scalar
        // params load at their narrow CLIF type; composite params load
        // one `I64` (a `*ValArray` the caller stored as u64); variant /
        // nullable params load two `I64`s (disc, payload).
        // `d.wire_slot` is the param's starting 8-byte slot offset.
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
        // Two-word return shape comes from the same single source as
        // the signature (`abi_return`). `None` (bare Null) can't reach
        // here — the kernel's signature build would have errored first.
        let returns_two_words =
            matches!(kernel.abi_return(registry), Some(AbiReturn::Two));
        let (r0, r1_opt) = {
            let results = b.inst_results(call);
            (results[0], if returns_two_words { Some(results[1]) } else { None })
        };

        // Store result into *out. Scalar/composite kernels write one
        // slot (out[0]); Value-shaped returns write two slots (out[0]
        // = disc, out[1] = payload). Width matches the kernel's
        // return type — the upper bytes of a scalar slot stay
        // whatever they were (the caller knows the return type and
        // reads the right width).
        b.ins().store(MemFlags::trusted(), r0, out_ptr, 0);
        if let Some(r1) = r1_opt {
            b.ins().store(MemFlags::trusted(), r1, out_ptr, 8);
        }
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

// ─── Function shape ──────────────────────────────────────────────

fn compile_into_function(
    b: &mut FunctionBuilder,
    kernel: &KernelSig,
    callee_refs: &BTreeMap<usize, FuncRef>,
    helper_refs: &HelperRefs,
    lazy_strings: &std::cell::RefCell<Vec<Box<ArcStr>>>,
    lazy_values: &std::cell::RefCell<Vec<Box<Value>>>,
    body_emitter: &dyn BodyEmitter,
) -> Result<usize> {
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
    // Bind every kernel param into the env in kind-grouped ABI order
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

    let tail_call_slots = if kernel.tail_call_slots.is_empty() {
        None
    } else {
        Some(kernel.tail_call_slots.as_slice())
    };
    let lower = LowerCtx {
        loop_head,
        param_mark,
        init_flag,
        callee_refs,
        helper_refs,
        tail_call_slots,
        tail_scrut_stale,
        dyncall_buf_stack: std::cell::RefCell::new(Vec::new()),
        owned_input_stack: std::cell::RefCell::new(Vec::new()),
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
        state_ptr,
        state_enabled: body_emitter.allow_state(),
        state_next: std::cell::Cell::new(0),
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
        match kernel_abi::abi_kind(lower.registry, &kernel.return_type) {
            Some(AbiKind::Scalar(p)) => {
                let s = zero_const(b, p);
                b.ins().return_(&[s]);
            }
            // Composite returns: the kernel's typed signature returns
            // a single `I64` pointer. The pending sentinel is null.
            // Unit returns the I64 ABI slot too (the caller discards).
            // String returns travel as a single `i64` (ArcStr's thin
            // pointer); the sentinel is `0` (null pointer) — the caller
            // checks the pending flag before decoding.
            Some(
                AbiKind::Array
                | AbiKind::Tuple
                | AbiKind::Struct
                | AbiKind::Unit
                | AbiKind::String,
            ) => {
                let s = b.ins().iconst(types::I64, 0);
                b.ins().return_(&[s]);
            }
            // Value-shape returns: two `I64`s (disc, payload). The
            // pending sentinel is `(0, 0)` — the caller's pending
            // check fires from `DYNCALL_PENDING` before decoding,
            // so the bits are never observed.
            Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
                let s0 = b.ins().iconst(types::I64, 0);
                let s1 = b.ins().iconst(types::I64, 0);
                b.ins().return_(&[s0, s1]);
            }
            // Bare Null / non-fusable returns aren't a real shape —
            // fusion widens to Nullable<T> before producing.
            Some(AbiKind::Null) | None => unreachable!(
                "kernel returns bare Null / non-fusable but reached JIT \
                 pending-exit emission — should have widened earlier"
            ),
        }
    }

    // After body compilation, every block has been sealed except
    // possibly some auxiliary blocks (select arms, if-chain merges,
    // pending_exit). FunctionBuilder requires all blocks be sealed
    // before finalize; seal_all_blocks catches the stragglers.
    b.seal_all_blocks();
    Ok(lower.state_next.get())
}

/// Per-kernel storage of the ArcStrs the JIT'd code references via
/// stable `*const ArcStr` pointers.
///
/// Each unique string used by the kernel is interned through the
/// global [`intern`] table (which gives back a
/// refcount-shared canonical `ArcStr`), and the canonical clone is
/// stored at a fixed index in `slots`. Codegen emits an `iconst`
/// of `&slots[index]` for each reference; the pointer is valid for
/// as long as the boxed slice (and hence the owning `WrappedKernel`)
/// is alive.
///
/// On drop, every `ArcStr` in `slots` drops, decrementing the
/// shared `Arc<str>` refcounts. When the last live kernel that
/// uses a particular string drops, the global interner's GC pass
/// can reclaim that entry.
///
/// The `index` map is only used at compile time (codegen lookup);
/// once compile is done, only `slots` matters at runtime.
pub struct KernelStrings {
    /// Stable-address ArcStr slots. Codegen-emitted pointers point
    /// into this slice. `Box<[ArcStr]>` is heap-allocated and the
    /// Box never moves its allocation — moving the Box value only
    /// moves a 16-byte pointer, not the data.
    slots: Box<[ArcStr]>,
    /// String → index lookup, populated during the pre-walk.
    /// Dropped after codegen would be fine, but we keep it for
    /// debug-print symmetry; cost is small.
    index: BTreeMap<ArcStr, usize>,
    /// Lazily-interned entries from direct-path emission
    /// ([`BodyCx::interned_str`]) — each individually boxed so its
    /// address survives Vec growth; the baked pointers point INTO the
    /// boxes. There is no pre-walk on the direct path (a Node prewalk
    /// mirroring emission coverage would be a silent-drift dangling-
    /// pointer hazard); interning happens AT emission, so coverage is
    /// exact by construction.
    lazy: Vec<Box<ArcStr>>,
}

impl KernelStrings {
    /// An empty string table — for kernels that reference no strings,
    /// or as a placeholder before the real table is built.
    pub fn empty() -> Self {
        Self { slots: Box::new([]), index: BTreeMap::new(), lazy: Vec::new() }
    }

    /// Attach direct-path lazily-interned entries (see the `lazy`
    /// field) so they live exactly as long as this table — i.e. as
    /// long as the compiled code that baked their addresses.
    pub fn with_lazy(mut self, lazy: Vec<Box<ArcStr>>) -> Self {
        self.lazy = lazy;
        self
    }

    /// Stable `*const ArcStr` for `s`, or `None` on a lookup miss
    /// (de-fuses rather than panicking). Unused on the direct path,
    /// which interns at emission via `interned_str`; retained for the
    /// pre-walk pointer-baking path.
    pub fn get(&self, s: &ArcStr) -> Option<*const ArcStr> {
        let i = *self.index.get(s)?;
        Some(&self.slots[i] as *const ArcStr)
    }
}

/// Per-kernel value-shape constants table — stable-address `Value`
/// slots whose `*const Value` the codegen bakes for value-shape
/// constants (datetime/duration/bytes/map). Mirrors
/// [`KernelStrings`]: the `Box<[Value]>` never moves its heap
/// allocation, so the baked pointers stay valid as long as the table
/// (held on the kernel) lives. (Scalar `Const`s aren't interned — they
/// lower inline.)
pub struct KernelValues {
    slots: Box<[Value]>,
    /// Direct-path lazily-interned value-shape constants — see
    /// [`KernelStrings::lazy`].
    lazy: Vec<Box<Value>>,
}

impl KernelValues {
    pub fn empty() -> Self {
        Self { slots: Box::new([]), lazy: Vec::new() }
    }

    /// See [`KernelStrings::with_lazy`].
    pub fn with_lazy(mut self, lazy: Vec<Box<Value>>) -> Self {
        self.lazy = lazy;
        self
    }

    /// Stable `*const Value` for `v`, or `None` on a miss (de-fuses
    /// rather than panicking). See [`KernelStrings::get`].
    pub fn get(&self, v: &Value) -> Option<*const Value> {
        self.slots.iter().find(|s| *s == v).map(|s| s as *const Value)
    }
}

/// Per-function lowering context: things that don't change across
/// statements within a single body.
pub(crate) struct LowerCtx<'a> {
    /// `Some(block)` when the kernel has a tail loop; TailCall jumps
    /// here. `None` for non-tail-recursive kernels.
    loop_head: Option<Block>,
    /// Env snapshot taken right after all params are bound. A
    /// tail-call rebind truncates `env` back to this so per-iteration
    /// block / select-arm locals (scalar, composite, and variant)
    /// don't leak across iterations.
    param_mark: usize,
    /// The `event.init` flag (`I64`, 1 on the kernel's init cycle),
    /// loaded from wire slot 0. Read by [`emit_const_node`] (via
    /// [`BodyCx::init_flag`]) so a constant carries [`STALE`] on every
    /// non-init cycle — it fires only at init, like the node-walk.
    init_flag: ClifValue,
    /// The per-instance state-buffer pointer (`I64`), loaded from wire
    /// slot 1 (0 when the kernel claimed no words). See
    /// [`BodyCx::claim_state_word`].
    state_ptr: ClifValue,
    /// Whether THIS function's body may claim state words — true only
    /// for the region parent's root body (`BodyEmitter::allow_state`).
    /// A callee is reached from arbitrarily many call sites whose
    /// claims would alias one buffer offset, so callees never claim
    /// (their stateful constructs keep the stateless approximation).
    state_enabled: bool,
    /// Next unclaimed state word index. The final count becomes
    /// [`WrappedKernel::state_words`] — the runtime buffer size.
    state_next: std::cell::Cell<usize>,
    /// Depth of enclosing scaffold loops at the current emission
    /// point. State claims are refused inside loops: a loop-body
    /// construct evaluates once PER SLOT per invocation, so one static
    /// word can't hold per-slot memory (the node-walk gives each slot
    /// clone its own state; per-slot kernels get theirs via fresh
    /// buffers per `clone_rebind`, but an inline loop body is one
    /// function).
    loop_depth: std::cell::Cell<u32>,
    /// Cross-kernel call sites resolve their callee's kernel IDENTITY
    /// (`kernel_key` of the site's `info.kernel` Arc) through this map
    /// to a CLIF `FuncRef` — never by name (names shadow, and
    /// monomorphizations share a name). The caller must
    /// `declare_func_in_func` each callee's `FuncId` against the
    /// current function before constructing the FunctionBuilder, then
    /// pass the resulting refs in here. Empty for kernels with no
    /// lambda call sites.
    callee_refs: &'a BTreeMap<usize, FuncRef>,
    /// `FuncRef`s for the `emit_helpers::*` runtime helpers.
    /// Declared in the current function before the FunctionBuilder
    /// is constructed (same constraint as `callee_refs`). Lookups
    /// are by helper name (e.g. `"graphix_valarray_get_i64"`).
    helper_refs: &'a HelperRefs,
    /// Per-source-position tail-call slot map (from
    /// `KernelSig::tail_call_slots`). Drives which Variable each
    /// tail-call arg rebinds into — scalar slots hit `env.locals`,
    /// composite slots hit `env.composites`. `None` for kernels
    /// without a tail loop (or that hand-built fixtures leave
    /// empty).
    tail_call_slots: Option<&'a [kernel_abi::TailCallSlot]>,
    /// Control-dependence firing accumulator: the AND over every
    /// TAIL-position select scrutinee's STALE bit on the executed path
    /// (`emit_select_node_tail` folds each in; STALE-set = no tail
    /// select fired). `emit_kernel_return` ANDs it into the returned
    /// disc's STALE so a result whose value chain is all-stale still
    /// fires when the arm-selecting scrutinee fired — the tail-arm
    /// mirror of the value-position select's merge-point fold.
    tail_scrut_stale: Variable,
    /// Stack of in-flight DynCall args bufs (`*mut LPooled<Vec<Value>>`
    /// Variables). Each DynCall pushes its args buf at `buf_new` and
    /// pops it once `graphix_dyncall` has consumed it. A forced-bottom
    /// abort (interrupt poll, return-gate force, bottom abort, callee
    /// genuine-abort check) drops whatever is still on this stack
    /// (= the args bufs of OUTER, not-yet-dispatched DynCalls) plus
    /// every owned composite/variant local via
    /// [`emit_pending_cleanup`].
    dyncall_buf_stack: std::cell::RefCell<Vec<Variable>>,
    /// Owned HOF input arrays in flight (fresh producers — a literal,
    /// slice, or inlined-HOF result consumed by a loop scaffold).
    /// Registered by `scaffold::adopt_owned_src` at loop entry and
    /// popped by `scaffold::drop_owned_src` right after the loop's
    /// normal-path drop, so a pending exit INSIDE the loop body frees
    /// the input via [`emit_pending_cleanup`] (`graphix_valarray_drop`
    /// — these are finished `*mut ValArray`s, NOT bufs, hence a
    /// separate stack from `dyncall_buf_stack`). Variables here are
    /// always defined on the paths that can pend (the registering
    /// loop dominates its body) — unlike a JitEnv binding, an entry
    /// never outlives its defining region, so select arms stay safe.
    owned_input_stack: std::cell::RefCell<Vec<Variable>>,
    /// Direct-path lazy interning arenas (see
    /// [`KernelStrings::lazy`]) — entries appended during emission via
    /// [`BodyCx::interned_str`] / [`BodyCx::interned_value`], harvested
    /// by `define_kernel_body` into the per-kernel tables. Each entry
    /// is individually boxed so its address survives Vec growth.
    lazy_strings: &'a std::cell::RefCell<Vec<Box<ArcStr>>>,
    lazy_values: &'a std::cell::RefCell<Vec<Box<Value>>>,
    /// Lazily-created single `pending_exit` block — the target of
    /// every genuine whole-kernel abort (interrupt poll, return-gate
    /// force, bottom abort, callee genuine-abort check). Its body
    /// (sentinel + `return`) is emitted at the end of
    /// `compile_into_function`. All abort paths jump here after
    /// dropping the owned set. A pended DynCall does NOT come here —
    /// it converts to a #219 tainted placeholder at its own site and
    /// continues.
    pending_exit: std::cell::RefCell<Option<Block>>,
    /// Discovered sync-builtin Apply sites for the direct path
    /// (`Some` only when a [`BodyEmitter`] supplies them) — keyed by
    /// the Apply's spec id, consumed by [`BodyCx::builtin_site`] so
    /// `CallSite::emit_clif` can lower a registered site to a DynCall.
    builtin_apply_sites: Option<&'a nohash::IntMap<ExprId, BuiltinCallSiteInfo>>,
    /// Discovered statically-resolved lambda call sites — the direct
    /// path's `ExprId → LambdaCallInfo` map (`None` for callee bodies).
    /// `CallSite::emit_clif` resolves a registered site to a CLIF
    /// `call` against `callee_refs[kernel_key(&info.kernel)]`.
    lambda_call_sites: Option<&'a nohash::IntMap<ExprId, LambdaCallInfo>>,
    /// `Some` when this kernel is a self-recursive lambda body being
    /// Node-emitted: the self binding + the kernel's own call
    /// descriptor. Tail-position self-calls rebind-and-jump
    /// (`emit_body_tail`); value-position ones call the kernel's own
    /// FuncRef (`CallSite::emit_clif`).
    self_call: Option<&'a (BindId, LambdaCallInfo)>,
    /// Type-resolution env snapshot for the direct path (`None` when
    /// no [`BodyEmitter`] supplies it). See [`BodyEmitter::type_env`]
    /// and [`resolve_node_typ`].
    type_env: Option<&'a Env>,
    /// The compiling `ExecCtx`'s abstract-type registry, borrowed from
    /// [`BodyEmitter::registry`]. Read by the emit-time type
    /// classifiers (`abi_kind`/freeze/`resolve_abstract`) via
    /// [`BodyCx::registry`].
    registry: &'a AbstractRegistry,
    /// Whether [`emit_kernel_return`] gates this body's return on STALE —
    /// `true` for a published body, `false` for a cross-kernel callee.
    /// See [`BodyEmitter::gate_stale_at_return`].
    gate_stale_at_return: bool,
    /// Lifted connect-target bind ids (let-bound scalar counters routed
    /// in as feeders). Read by [`emit_let_node`] (seed-select) and
    /// [`emit_connect_node`] (write gate). See [`BodyEmitter::lifted`].
    lifted: &'a ahash::AHashSet<BindId>,
    /// DynCall `fn_index` base for the body being emitted (see
    /// [`BodyEmitter::fn_index_offset`]). Added to every DynCall's
    /// `info.fn_index` at emit so a callee body indexes the region-wide
    /// combined `dyn_slots`. `0` for parents / per-slot callbacks.
    fn_index_offset: u32,
}

/// Resolve named/abstract type refs in a node-carried `Type` through
/// the region's env snapshot (#218): node `typ` cells can hold
/// `Type::Ref`s to abstract type names (e.g. an interface's
/// `type Elem`) whose concrete rep `abi_kind`/freeze can't see —
/// `resolve_abstract` expands them (env `lookup_ref` + the abstract
/// registry). When `type_env` is `None` the type returns unchanged.
fn resolve_node_typ(ctx: &LowerCtx, t: &Type) -> Type {
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
fn freeze_node_typ(ctx: &LowerCtx, t: &Type) -> Option<Type> {
    kernel_abi::freeze_for_abi_normalized(ctx.registry, t).or_else(|| {
        kernel_abi::freeze_for_abi_normalized(ctx.registry, &resolve_node_typ(ctx, t))
    })
}

/// FuncRefs into the JIT module for each runtime helper, valid
/// within a single function's body. Populated by [`declare_helpers`]
/// just before constructing the FunctionBuilder.
#[derive(Default)]
struct HelperRefs {
    refs: BTreeMap<&'static str, FuncRef>,
}

impl HelperRefs {
    fn get(&self, name: &str) -> Option<FuncRef> {
        self.refs.get(name).copied()
    }
}

/// Stable FuncIds for the runtime helpers, declared once per JIT
/// module at [`JitCtx::new`]. Per-function compilation calls
/// [`declare_helpers`] to materialize these as FuncRefs in the
/// current function.
struct HelperFuncIds {
    ids: BTreeMap<&'static str, FuncId>,
}

impl HelperFuncIds {
    fn new(module: &mut JITModule) -> Result<Self> {
        let mut ids = BTreeMap::new();
        // Each helper's signature. The naming convention encodes the
        // return type — we match on that to pick the right CLIF sig.
        for (name, _ptr) in all_symbols() {
            let sig = helper_signature(module, name)?;
            let fid = module
                .declare_function(name, Linkage::Import, &sig)
                .with_context(|| format!("declare_function for helper `{name}`"))?;
            ids.insert(name, fid);
        }
        Ok(Self { ids })
    }
}

fn helper_signature(module: &JITModule, name: &str) -> Result<Signature> {
    let mut sig = Signature::new(module.isa().default_call_conv());
    // The C ABI requires an integer argument narrower than the register
    // width (i8/i16) to be zero-/sign-extended by the CALLER; the
    // `extern "C"` helper is compiled to read the full register under that
    // contract. Cranelift only emits the extension when the AbiParam
    // records it, so a bool/int the kernel COMPUTES (e.g. a comparison,
    // whose x86 `setcc` leaves the upper register bits dirty) reaches the
    // helper unextended and is misread — a `false` comparison pushed into a
    // composite comes back `true`. Match the Rust param's signedness:
    // bool/unsigned → uext, signed → sext. (i32/u32 need no extension — a
    // 32-bit register write clears the upper bits.)
    let uext = |ty| AbiParam::new(ty).uext();
    let sext = |ty| AbiParam::new(ty).sext();
    // Manual per-helper signature wiring — explicit is clearer than
    // a clever scheme when adding new helpers means revisiting this
    // anyway.
    match name {
        // Read helpers: (ptr: i64, idx: i64) -> <prim>
        "graphix_valarray_get_i64" | "graphix_struct_get_i64" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "graphix_valarray_get_f64" | "graphix_struct_get_f64" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::F64));
        }
        "graphix_valarray_get_i32"
        | "graphix_valarray_get_u32"
        | "graphix_struct_get_i32"
        | "graphix_struct_get_u32" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I32));
        }
        "graphix_valarray_get_f32" | "graphix_struct_get_f32" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::F32));
        }
        "graphix_valarray_get_bool"
        | "graphix_struct_get_bool"
        | "graphix_valarray_get_i8"
        | "graphix_struct_get_i8"
        | "graphix_valarray_get_u8"
        | "graphix_struct_get_u8" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I8));
        }
        "graphix_valarray_get_i16"
        | "graphix_struct_get_i16"
        | "graphix_valarray_get_u16"
        | "graphix_struct_get_u16" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I16));
        }
        "graphix_valarray_get_u64" | "graphix_struct_get_u64" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        // Non-primitive element reads: (ptr, idx) -> single i64 word
        // (a `*mut ValArray` for composite elems, or an `ArcStr` thin
        // pointer for string elems).
        "graphix_valarray_get_array"
        | "graphix_valarray_get_array_borrowed"
        | "graphix_valarray_get_arcstr"
        | "graphix_struct_get_array"
        | "graphix_struct_get_array_borrowed"
        | "graphix_struct_get_arcstr" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        // Value-shape element reads: (ptr, idx) -> Value (two words).
        // `graphix_valarray_index` is the bounds-checked `array[i]`
        // (signed idx, elem-or-error); the `_get_value` pair are the
        // unchecked struct-field / tuple-element value-shape reads.
        "graphix_valarray_index"
        | "graphix_valarray_get_value"
        | "graphix_struct_get_value" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64)); // ret.disc
            sig.returns.push(AbiParam::new(types::I64)); // ret.payload
        }
        "graphix_valarray_len" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        // Producer-op builder.
        "graphix_value_buf_new" => {
            // (cap: usize) -> *mut LPooled<Vec<Value>>
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "graphix_value_buf_push_i64" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
        }
        "graphix_value_buf_push_f64" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::F64));
        }
        "graphix_value_buf_push_i32" | "graphix_value_buf_push_u32" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I32));
        }
        "graphix_value_buf_push_f32" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::F32));
        }
        "graphix_value_buf_push_bool" | "graphix_value_buf_push_u8" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(uext(types::I8));
        }
        "graphix_value_buf_push_i8" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(sext(types::I8));
        }
        "graphix_value_buf_push_u16" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(uext(types::I16));
        }
        "graphix_value_buf_push_i16" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(sext(types::I16));
        }
        "graphix_value_buf_push_u64" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
        }
        // Push helpers split by argument shape:
        //   _push_array   : (buf, inner: *mut ValArray) — pointer arg
        //   _push_arcstr  : (buf, ptr: *const ArcStr) — pointer arg
        //   _push_value   : (buf, v: Value)            — Value by value
        "graphix_value_buf_push_array"
        // (buf, inner: *mut ValArray) — flattens inner's elements into
        // buf and drops inner; same two-pointer shape as push_array.
        | "graphix_value_buf_extend_from_array"
        | "graphix_value_buf_push_arcstr"
        // ArcStr is `repr(transparent)` over a thin pointer — passes
        // as a single I64 register, same shape as the others above.
        | "graphix_value_buf_push_string" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
        }
        // Value-by-value push helpers: buf ptr + two-word Value (disc,
        // payload). The Value is consumed (owned-mode push) or
        // refcount-bumped + forgotten (_borrowed mode); both share
        // the same CLIF signature.
        "graphix_value_buf_push_value"
        | "graphix_value_buf_push_value_borrowed" => {
            sig.params.push(AbiParam::new(types::I64)); // buf
            sig.params.push(AbiParam::new(types::I64)); // v.disc
            sig.params.push(AbiParam::new(types::I64)); // v.payload
        }
        "graphix_valarray_finalize" => {
            // consumes buf, returns *mut ValArray
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "graphix_valarray_clone" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "graphix_valarray_drop" => {
            sig.params.push(AbiParam::new(types::I64));
        }
        // Value-by-value lifecycle helpers — every Value arg is two
        // `I64` registers (disc, payload), every Value return is two
        // `I64` registers (RAX, RDX on SysV AMD64).
        "graphix_value_drop" => {
            sig.params.push(AbiParam::new(types::I64)); // v.disc
            sig.params.push(AbiParam::new(types::I64)); // v.payload
        }
        "graphix_value_clone" => {
            sig.params.push(AbiParam::new(types::I64)); // v.disc
            sig.params.push(AbiParam::new(types::I64)); // v.payload
            sig.returns.push(AbiParam::new(types::I64)); // ret.disc
            sig.returns.push(AbiParam::new(types::I64)); // ret.payload
        }
        "graphix_value_new_from_array"
        | "graphix_value_new_string_from_arcstr"
        // Clone a `*const Value` static (value-shape Const):
        // one pointer arg, a Value (two words) returned.
        | "graphix_value_clone_from_static" => {
            sig.params.push(AbiParam::new(types::I64)); // *mut ValArray / *const ArcStr / *const Value
            sig.returns.push(AbiParam::new(types::I64)); // ret.disc
            sig.returns.push(AbiParam::new(types::I64)); // ret.payload
        }
        // Unwrap a `Value::Array` into the composite ABI's owned
        // `*mut ValArray` (consuming / borrowed-read variants).
        "graphix_value_into_array" | "graphix_value_into_array_borrowed" => {
            sig.params.push(AbiParam::new(types::I64)); // v.disc
            sig.params.push(AbiParam::new(types::I64)); // v.payload
            sig.returns.push(AbiParam::new(types::I64)); // *mut ValArray
        }
        // Value arithmetic (datetime/duration `ValueArith`): two Value
        // args (four words) in, one Value (two words) out.
        "graphix_value_add"
        | "graphix_value_sub"
        | "graphix_value_mul"
        | "graphix_value_div"
        | "graphix_value_rem"
        // Checked arithmetic (`+?` family): same four-words-in /
        // Value-out shape; the result is the success Value or the
        // `ArithError` error value.
        | "graphix_value_checked_add"
        | "graphix_value_checked_sub"
        | "graphix_value_checked_mul"
        | "graphix_value_checked_div"
        | "graphix_value_checked_rem"
        // Map access (`MapRef`): map Value + key Value (four words) in,
        // a Value (Nullable<V>, two words) out — same shape.
        | "graphix_map_ref" => {
            sig.params.push(AbiParam::new(types::I64)); // l.disc
            sig.params.push(AbiParam::new(types::I64)); // l.payload
            sig.params.push(AbiParam::new(types::I64)); // r.disc
            sig.params.push(AbiParam::new(types::I64)); // r.payload
            sig.returns.push(AbiParam::new(types::I64)); // ret.disc
            sig.returns.push(AbiParam::new(types::I64)); // ret.payload
        }
        // Value equality (`ValueEq`): two Value args (four words) in,
        // a bool (I8) out.
        "graphix_value_eq" => {
            sig.params.push(AbiParam::new(types::I64)); // l.disc
            sig.params.push(AbiParam::new(types::I64)); // l.payload
            sig.params.push(AbiParam::new(types::I64)); // r.disc
            sig.params.push(AbiParam::new(types::I64)); // r.payload
            sig.returns.push(AbiParam::new(types::I8)); // bool
        }
        // `bytes[i]` (`BytesIndex`): a Value (two words) + i64 index in,
        // a Value (Nullable<u8>, two words) out.
        "graphix_bytes_index" => {
            sig.params.push(AbiParam::new(types::I64)); // bytes.disc
            sig.params.push(AbiParam::new(types::I64)); // bytes.payload
            sig.params.push(AbiParam::new(types::I64)); // index
            sig.returns.push(AbiParam::new(types::I64)); // ret.disc
            sig.returns.push(AbiParam::new(types::I64)); // ret.payload
        }
        // `a[i..j]` (`ArraySlice`): source Value (two words) + i64 start
        // + i64 end + i64 flags in, a Value (Nullable<source>) out.
        "graphix_array_slice" => {
            sig.params.push(AbiParam::new(types::I64)); // src.disc
            sig.params.push(AbiParam::new(types::I64)); // src.payload
            sig.params.push(AbiParam::new(types::I64)); // start
            sig.params.push(AbiParam::new(types::I64)); // end
            sig.params.push(AbiParam::new(types::I64)); // flags
            sig.returns.push(AbiParam::new(types::I64)); // ret.disc
            sig.returns.push(AbiParam::new(types::I64)); // ret.payload
        }
        // Variant consumer ops: Value arg by value + extra arg.
        "graphix_variant_tag_eq" => {
            sig.params.push(AbiParam::new(types::I64)); // v.disc
            sig.params.push(AbiParam::new(types::I64)); // v.payload
            sig.params.push(AbiParam::new(types::I64)); // expected ArcStr ptr
            sig.returns.push(AbiParam::new(types::I8));
        }
        "graphix_variant_payload_i64" | "graphix_variant_payload_u64" => {
            sig.params.push(AbiParam::new(types::I64)); // v.disc
            sig.params.push(AbiParam::new(types::I64)); // v.payload
            sig.params.push(AbiParam::new(types::I64)); // idx
            sig.returns.push(AbiParam::new(types::I64));
        }
        "graphix_variant_payload_f64" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::F64));
        }
        "graphix_variant_payload_i32"
        | "graphix_variant_payload_u32" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I32));
        }
        "graphix_variant_payload_f32" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::F32));
        }
        "graphix_variant_payload_bool"
        | "graphix_variant_payload_i8"
        | "graphix_variant_payload_u8" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I8));
        }
        "graphix_variant_payload_i16" | "graphix_variant_payload_u16" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I16));
        }
        // DynCall dispatch:
        //   (fn_index: u32, args: *mut LPooled<Vec<Value>>, ret_kind: u8)
        //     -> ret.disc: u64
        //     -> ret.payload: u64
        // ret_kind=2 (Value-shaped return) populates BOTH return regs;
        // for scalar (ret_kind=0) the high reg is undefined and the
        // caller ignores it. For ret_kind=3 (Unit) both are 0.
        "graphix_dyncall" => {
            sig.params.push(AbiParam::new(types::I32));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(uext(types::I8)); // ret_kind: u8
            sig.returns.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "graphix_set_var" => {
            // (bind_id, disc, payload) → void
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
        }
        "graphix_dyncall_pending_take" => {
            sig.returns.push(AbiParam::new(types::I8));
        }
        "graphix_dyncall_pending_take_clear" => {
            sig.returns.push(AbiParam::new(types::I8));
        }
        "graphix_dyncall_set_pending" => {
            // no args, no return — just flips the thread-local flag
        }
        "graphix_callee_flags_set" => {
            // (bits: u64) — record a callee result's not-fresh disc bits
            sig.params.push(AbiParam::new(types::I64));
        }
        "graphix_callee_flags_take" => {
            // () -> u64 — read + clear the callee-result flag bits
            sig.returns.push(AbiParam::new(types::I64));
        }
        "graphix_interrupted" => {
            // no args; returns i8 (1 = abort the loop, 0 = continue)
            sig.returns.push(AbiParam::new(types::I8));
        }
        "graphix_depth_push" => {
            // no args; returns i8 (1 = dispatch may proceed, 0 = at
            // the call-depth limit — skip the call, abort to bottom)
            sig.returns.push(AbiParam::new(types::I8));
        }
        "graphix_depth_enter" => {
            // no args; returns i8 (1 = the HOF loop may run, 0 = at
            // the call-depth limit — skip the loop, taint its result)
            sig.returns.push(AbiParam::new(types::I8));
        }
        "graphix_depth_pop" => {
            // no args, no return
        }
        "graphix_value_buf_push_array_borrowed" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
        }
        "graphix_value_buf_drop" => {
            sig.params.push(AbiParam::new(types::I64));
        }
        // `is_null(v: Value) -> u8` retained for completeness, though
        // the JIT inlines the disc compare (`icmp_imm Equal disc,
        // NULL_DISC`) rather than calling this.
        "graphix_value_is_null" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I8));
        }
        // ─── String helpers ──────────────────────────────────────
        // ArcStr is repr(transparent) over a thin pointer — wire
        // shape is a single i64.
        // (ptr: i64) -> ArcStr
        "graphix_arcstr_clone_from_static" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        // (s: ArcStr) -> ArcStr — same ABI shape as
        // `clone_from_static`, but semantically different: takes the
        // ArcStr by value (consumes the bits via `mem::forget`) and
        // returns a fresh refcount-bumped clone. Used by `Local`
        // reads of String slots.
        "graphix_arcstr_clone" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        // (s: ArcStr) -> ()
        "graphix_arcstr_drop" => {
            sig.params.push(AbiParam::new(types::I64));
        }
        // (s: ArcStr) -> Value (two-register)
        "graphix_value_new_string" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        // () -> *mut String
        "graphix_string_buf_new"
        | "graphix_arcstr_empty"
        | "graphix_valarray_empty_boxed" => {
            sig.returns.push(AbiParam::new(types::I64));
        }
        // (buf: i64) -> ()
        "graphix_string_buf_drop" => {
            sig.params.push(AbiParam::new(types::I64));
        }
        // (buf: i64) -> ArcStr
        "graphix_string_buf_finalize" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        // (buf: i64, s: ArcStr) -> ()
        "graphix_string_buf_push_arcstr" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
        }
        // (buf: i64, v: i64) -> ()
        "graphix_string_buf_push_i64" | "graphix_string_buf_push_u64" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
        }
        // (buf: i64, v: i32) -> ()
        "graphix_string_buf_push_i32" | "graphix_string_buf_push_u32" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I32));
        }
        // (buf: i64, v: i16) -> ()
        "graphix_string_buf_push_u16" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(uext(types::I16));
        }
        "graphix_string_buf_push_i16" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(sext(types::I16));
        }
        // (buf: i64, v: i8) -> ()
        "graphix_string_buf_push_u8" | "graphix_string_buf_push_bool" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(uext(types::I8));
        }
        "graphix_string_buf_push_i8" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(sext(types::I8));
        }
        // (buf: i64, v: f64) -> ()
        "graphix_string_buf_push_f64" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::F64));
        }
        // (buf: i64, v: f32) -> ()
        "graphix_string_buf_push_f32" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::F32));
        }
        // () -> () — debug-build JIT invocation counter bump.
        // Registered only when `cfg(debug_assertions)` is set; the
        // codegen call site is gated the same way.
        #[cfg(debug_assertions)]
        "graphix_record_jit_invocation" => {}
        other => {
            return Err(anyhow!("unknown JIT helper symbol `{other}`"))
        }
    }
    Ok(sig)
}

/// Declare each runtime helper FuncId as a FuncRef in the current
/// function being built. Call this with `&mut jit.func_ctx.func`
/// before constructing the FunctionBuilder (same borrowing
/// constraint as the existing `callee_refs` setup).
fn declare_helpers(
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

// ─── Value-shape ABI ─────────────────────────────────────────────
//
// `Value` is `#[repr(u64)]` with explicit discriminant values
// (`Value::Null = 0x0000_8000`, `Value::I64(_) = 0x0000_0400`, etc.)
// and a fixed 16-byte layout — `(u64 disc, u64 payload)`. The JIT
// represents Value-shaped expressions as a pair of CLIF `I64` SSA
// values; the SysV AMD64 ABI passes them in two integer registers
// when crossing the helper boundary. Discriminant constants below
// mirror the values in `netidx_value::Value`'s definition; the
// const block at the bottom of `emit_helpers.rs` keeps Value's
// layout pinned at 16 bytes, so these stay coherent.
mod value_disc {
    pub const U8: i64 = 0x0000_0001;
    pub const I8: i64 = 0x0000_0002;
    pub const U16: i64 = 0x0000_0004;
    pub const I16: i64 = 0x0000_0008;
    pub const U32: i64 = 0x0000_0010;
    pub const I32: i64 = 0x0000_0040;
    pub const U64: i64 = 0x0000_0100;
    pub const I64: i64 = 0x0000_0400;
    pub const F32: i64 = 0x0000_1000;
    pub const F64: i64 = 0x0000_2000;
    pub const BOOL: i64 = 0x0000_4000;
    pub const NULL: i64 = 0x0000_8000;
    #[allow(dead_code)]
    pub const STRING: i64 = 0x8000_0000;
    #[allow(dead_code)]
    pub const ARRAY: i64 = 0x1000_0000;
}

/// Map a [`PrimType`] to the `Value` discriminant for boxing a
/// scalar into a `Value::T(x)`. Used by `IfChain` widening to
/// `Nullable<T>` — the arm packs `(prim_to_value_disc(T), scalar)`
/// inline rather than calling a helper.
pub(crate) fn prim_to_value_disc(p: PrimType) -> i64 {
    match p {
        PrimType::I8 => value_disc::I8,
        PrimType::I16 => value_disc::I16,
        PrimType::I32 => value_disc::I32,
        PrimType::I64 => value_disc::I64,
        PrimType::U8 => value_disc::U8,
        PrimType::U16 => value_disc::U16,
        PrimType::U32 => value_disc::U32,
        PrimType::U64 => value_disc::U64,
        PrimType::F32 => value_disc::F32,
        PrimType::F64 => value_disc::F64,
        PrimType::Bool => value_disc::BOOL,
    }
}

/// A Variant or Nullable local in the JIT: two `Variable`s holding
/// the (disc, payload) words of a `repr(u64)` Value. Reads return
/// both via `b.use_var`; writes (tail-call rebind, IfChain merge
/// via phi) update both.
#[derive(Debug, Clone, Copy)]
struct ValueVar {
    disc: Variable,
    payload: Variable,
}

/// Result of emitting one expression node: a `Value`-shaped
/// `(disc, payload)` register pair, mirroring netidx's `#[repr(u64)]`
/// `Value` layout. `disc` is always an `I64` discriminant word;
/// `payload` keeps its NATURAL cranelift type within a kernel (`F64`
/// for floats, `I8` for bools, `I64` for ints / pointers / the value
/// word of a two-word Value).
///
/// `disc` doubles as the **taint channel** (#219). A real discriminant
/// only occupies bits 0..31 (a bitmask — see [`value_disc`]), so bit 62
/// ([`TAINT`]) is reserved to mark a value that MAY be a bottom (a div0,
/// a `?`-error, a missing region input the taken path never consumes).
/// Pure ops OR their operands' taint into the result disc
/// ([`propagate_taint`]); the kernel FORCES at the output (and at
/// destructuring consumers) via [`is_tainted`] — a tainted output makes
/// `Kernel::update` return `None`, moving the abort from the producing
/// site to the output so an intermediate bottom an un-taken arm never
/// consumes can't abort the whole kernel (`design/representable_bottom.md`).
///
/// For a non-tainted value the disc is a compile-time constant cranelift
/// folds out, so the uniform two-register shape costs nothing on the hot
/// path while letting every consumer treat every value identically.
#[derive(Debug, Clone, Copy)]
pub struct CompiledExpr {
    pub disc: ClifValue,
    pub payload: ClifValue,
}

/// The reserved taint bit of a [`CompiledExpr`]'s `disc`. Outside the
/// [`value_disc`] bitmask range (bits 0..31) so it never collides with a
/// real discriminant; set = "this value MAY be a bottom" (#219). Shared
/// with the runtime dispatch (`kernel.rs`), which sets it for a missing
/// input's disc word.
pub(crate) const TAINT: i64 = 0x4000_0000_0000_0000;

/// [`TAINT`] as a [`emit_helpers::TagValue`] tag byte, for helpers that
/// mint a tainted result themselves.
pub(crate) const TAINT_TAG: u8 = (TAINT >> 56) as u8;

/// The reserved "did not fire this cycle" bit of a [`CompiledExpr`]'s
/// `disc` (bit 61, inside the JIT tag region [`emit_helpers::TagValue`]
/// reserves, below [`TAINT`]). Set = "this value carries a CACHED value
/// from a prior cycle — it did not update this cycle." It is the JIT
/// twin of the node-walk's `Cached` reporting `false` from `update`
/// (`node/mod.rs`): a node fires (publishes / writes a variable) only
/// when at least one of its inputs fired this cycle. Leaves set it
/// (a non-firing feeder, a constant after init); ops AND-reduce it
/// ([`propagate_stale`] — a result fires iff ANY operand fired); and the
/// kernel FORCES freshness at exactly two consumers — the output
/// ([`emit_kernel_return`], via [`is_not_fresh`]) and a `connect`/`?`
/// variable write (`set_var_typed`, the runtime twin). Mid-expression a stale
/// value is USED (its cached payload), never aborted — that is what
/// makes combineLatest agree with the node-walk. Invariant `TAINT ⟹
/// STALE` (a value that bottoms this cycle reads as not-fired
/// downstream) is maintained by [`propagate_flags`].
pub(crate) const STALE: i64 = 0x2000_0000_0000_0000;

impl CompiledExpr {
    pub fn new(disc: ClifValue, payload: ClifValue) -> Self {
        Self { disc, payload }
    }
}

/// An `I64` discriminant constant for a scalar of `prim` (taint clear).
fn scalar_disc(b: &mut FunctionBuilder, prim: PrimType) -> ClifValue {
    b.ins().iconst(types::I64, prim_to_value_disc(prim))
}

/// OR the [`TAINT`] bit of each operand disc into `base`, yielding the
/// result disc. The fast path (every operand a non-tainted const disc)
/// folds back to `base`. Mirrors the interp's `?`-absorb: any consumed
/// bottom taints the result.
fn propagate_taint(
    b: &mut FunctionBuilder,
    base: ClifValue,
    operands: &[ClifValue],
) -> ClifValue {
    let mut disc = base;
    for op in operands {
        let t = b.ins().band_imm(*op, TAINT);
        disc = b.ins().bor(disc, t);
    }
    disc
}

/// AND-reduce the [`STALE`] bit of `operands` into `base`. The dual of
/// [`propagate_taint`]: a node FIRES this cycle iff at least one of its
/// inputs fired, so its result is stale (not-fired) ONLY when EVERY
/// operand is stale. Folds to `base` for a single operand and, when all
/// operands carry compile-time-fresh discs, cranelift constant-folds the
/// whole chain away.
fn propagate_stale(
    b: &mut FunctionBuilder,
    base: ClifValue,
    operands: &[ClifValue],
) -> ClifValue {
    let Some((first, rest)) = operands.split_first() else { return base };
    let mut all = b.ins().band_imm(*first, STALE);
    for op in rest {
        let s = b.ins().band_imm(*op, STALE);
        all = b.ins().band(all, s);
    }
    b.ins().bor(base, all)
}

/// The result-disc flag combinator for an op that consumes `operands`:
/// [`TAINT`] is OR-reduced (any consumed bottom taints the result) and
/// [`STALE`] is AND-reduced (the result fired iff any operand fired).
/// The single call every binary/unary op uses to build its result disc.
fn propagate_flags(
    b: &mut FunctionBuilder,
    base: ClifValue,
    operands: &[ClifValue],
) -> ClifValue {
    let d = propagate_taint(b, base, operands);
    propagate_stale(b, d, operands)
}

/// Fold `base` with a conditional taint: when `cond` (an I8 0/1) is
/// true, OR [`TAINT`] into the disc. Used where a computed condition
/// signals bottom (a div0, a `?`-error).
fn taint_if(b: &mut FunctionBuilder, base: ClifValue, cond: ClifValue) -> ClifValue {
    let tainted = b.ins().bor_imm(base, TAINT);
    b.ins().select(cond, tainted, base)
}

/// The interior-bottom cache-substitute (Eric-approved 2026-07-04):
/// [`TAINT`] must mean "no value EVER seen at this point" — but in the
/// node-walk every node caches its last value, so a taint ORIGIN (a
/// div0, a `$`-dropped error) with a PRIOR success doesn't poison its
/// consumers: they fire with the origin's cached value (node/op.rs
/// `Cached` operands; the interior-bottom soak family). Kernel INPUTS
/// already honor this (the runtime hands STALE + the last value after
/// the first fire); this wraps interior scalar origins the same way.
///
/// Wraps an already-computed scalar result: on every UNTAINTED compute
/// the value + a valid flag are stored to two claimed state words; on a
/// TAINTED disc with stored history the result becomes (cached value,
/// scalar base | [`STALE`]) — didn't-fire-this-cycle, exactly the
/// node-walk — while a genuine no-history taint passes through and
/// gates at the output as before. Stateless fallback (scaffold loops,
/// callee bodies): the unwrapped result — the pre-existing conflation,
/// a documented residual.
fn emit_scalar_taint_cache(
    cx: &mut BodyCx,
    prim: PrimType,
    cv: CompiledExpr,
) -> CompiledExpr {
    let (Some(off_val), Some(off_ok)) = (cx.claim_state_word(), cx.claim_state_word())
    else {
        return cv;
    };
    let sp = cx.state_ptr();
    let tainted = is_tainted(cx.b, cv.disc);
    // Loads FIRST: `have` must reflect history from BEFORE this fire.
    let cur_val = cx.b.ins().load(types::I64, MemFlags::trusted(), sp, off_val);
    let cur_ok = cx.b.ins().load(types::I64, MemFlags::trusted(), sp, off_ok);
    // Branchless store: keep the old contents when tainted, else the
    // fresh value + valid=1.
    let widened = scalar_to_payload_i64(cx.b, prim, cv.payload);
    let new_val = cx.b.ins().select(tainted, cur_val, widened);
    let one = cx.b.ins().iconst(types::I64, 1);
    let new_ok = cx.b.ins().select(tainted, cur_ok, one);
    cx.b.ins().store(MemFlags::trusted(), new_val, sp, off_val);
    cx.b.ins().store(MemFlags::trusted(), new_ok, sp, off_ok);
    // Substitute on tainted-with-history.
    let have = cx.b.ins().icmp_imm(IntCC::NotEqual, cur_ok, 0);
    let use_cache = cx.b.ins().band(tainted, have);
    let cached = cast_u64_to_prim(cx.b, cur_val, prim);
    let value = cx.b.ins().select(use_cache, cached, cv.payload);
    let base = scalar_disc(cx.b, prim);
    let stale = cx.b.ins().bor_imm(base, STALE);
    let disc = cx.b.ins().select(use_cache, stale, cv.disc);
    CompiledExpr::new(disc, value)
}

/// True (I8 bool) iff the disc's [`TAINT`] bit is set.
fn is_tainted(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    let t = b.ins().band_imm(disc, TAINT);
    b.ins().icmp_imm(IntCC::NotEqual, t, 0)
}

/// True (I8 bool) iff the disc's [`TAINT`] bit is CLEAR — the
/// "continue / has-a-value" bit consumed by [`emit_bottom_abort`].
fn is_untainted(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    let t = b.ins().band_imm(disc, TAINT);
    b.ins().icmp_imm(IntCC::Equal, t, 0)
}

/// True (I8 bool) iff the disc is NOT fresh — [`TAINT`] (bottom) OR
/// [`STALE`] (did not fire this cycle) set, i.e. this value did NOT fire
/// with a value. The firing gate consumed at the kernel-return seam
/// (`emit_kernel_return` / `emit_force`): the node-walk publishes only
/// when its output node returned `Some`, so a not-fresh root routes to
/// the pending path → `Kernel::update` returns `None`. (The `set_var`
/// write gate is the runtime twin, `set_var_typed` in `kernel.rs`.)
/// Folds to const-false for a value the emitter proved fresh (an
/// untainted, non-stale const disc) — no branch on the hot path.
fn is_not_fresh(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    let m = b.ins().band_imm(disc, TAINT | STALE);
    b.ins().icmp_imm(IntCC::NotEqual, m, 0)
}

/// OR [`STALE`] into a constant's `disc` on every NON-init cycle: a
/// constant node fires only at init (`event.init`, wire slot 0), then
/// reports not-fired (a cached value) — the node-walk's `Constant`
/// `update` returning `Some` once then `None`. `init_flag` is the
/// kernel's [`LowerCtx::init_flag`] (1 at init).
fn const_stale_gate(
    b: &mut FunctionBuilder,
    init_flag: ClifValue,
    disc: ClifValue,
) -> ClifValue {
    let not_init = b.ins().icmp_imm(IntCC::Equal, init_flag, 0);
    let staled = b.ins().bor_imm(disc, STALE);
    b.ins().select(not_init, staled, disc)
}

/// Strip the [`TAINT`] and [`STALE`] flag bits, yielding the real netidx
/// discriminant. Used before a disc crosses into a netidx-`Value` helper
/// (a flagged disc is an invalid tag) and before a structural disc
/// compare (we compare on the underlying tag, stale or not).
fn clean_disc(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    b.ins().band_imm(disc, !(TAINT | STALE))
}

// ─── Env: name → Variable lookup ─────────────────────────────────

/// What a [`Local`]'s `payload` word means and how it is owned/dropped
/// at scope exit. The `disc` word always carries the netidx
/// discriminant (and #219 taint); `kind` says how to read and free the
/// `payload`.
#[derive(Debug, Clone, Copy)]
enum LocalKind {
    /// `payload` is the scalar value at its natural CLIF type; nothing
    /// to drop.
    Scalar(PrimType),
    /// `payload` is an owned `*mut ValArray` (array / tuple / struct);
    /// dropped via `graphix_valarray_drop`. Reads are BORROWED (the env
    /// keeps owning); consumers clone when they need ownership.
    Composite,
    /// `payload` is an owned `ArcStr` thin pointer; dropped via
    /// `graphix_arcstr_drop`. Reads CLONE (refcount bump) so each
    /// consumer gets an independently-owned ArcStr.
    String,
    /// `payload` is the value word of a two-word `repr(u64)` Value;
    /// dropped via `graphix_value_drop(disc, payload)`; reads BORROWED.
    /// Variant / Nullable / bare value-shape stay distinct for
    /// consumer-op well-typedness (`IsNull` vs `VariantTagEq`).
    Variant,
    Nullable,
    Value,
}

/// One in-scope kernel local. Every local is a two-register Value
/// (`vv` = disc + payload Variables) tagged by `kind`; the disc carries
/// #219 taint uniformly, so there is no separate validity slot. The
/// `payload` Variable's CLIF type follows `kind` (a scalar's natural
/// type, else `I64` for a pointer / value word).
struct Local {
    name: ArcStr,
    vv: ValueVar,
    kind: LocalKind,
    /// `Some` for params and lets carrying a source BindId, so a `Ref`
    /// resolves BindId-first (exact under shadowing — the #162/#167 bug
    /// class). `None` for synthetic locals (e.g. an HOF loop element
    /// bound only by name).
    bind_id: Option<BindId>,
}

pub(crate) struct JitEnv {
    /// All in-scope locals in binding order. Lookups walk back-to-front
    /// so an inner binding shadows an outer one. `mark`/`truncate` track
    /// the single Vec length — a block / select-arm / loop-body scope
    /// pops back to its entry length on exit.
    locals: Vec<Local>,
}

impl JitEnv {
    fn new() -> Self {
        Self { locals: Vec::with_capacity(8) }
    }

    fn bind(
        &mut self,
        name: ArcStr,
        vv: ValueVar,
        kind: LocalKind,
        bind_id: Option<BindId>,
    ) {
        self.locals.push(Local { name, vv, kind, bind_id });
    }

    /// Resolve a local BindId-first (exact under shadowing), then by
    /// name — but ONLY to id-LESS (synthetic) locals. A same-named
    /// local carrying a DIFFERENT real id is a distinct binding: a
    /// capture whose id missed must fail here (the site Errs and the
    /// region de-fuses — a perf loss, never a wrong answer), not
    /// silently read an unrelated variable. The unrestricted fallback
    /// let a fold callback's captured `x` resolve to the fold ELEMENT
    /// (which FoldQ name-binds as "x") whenever the capture's
    /// instantiation id drifted — a nondeterministic wrong VALUE
    /// (interp 19 / jit 17, soak jul07h fuzz/divergence_000001; the
    /// same never-by-name discipline as audit-jul2026/03 in
    /// clone_rebind). Walks back-to-front.
    fn lookup(&self, id: BindId, name: &str) -> Option<&Local> {
        if let Some(l) = self.locals.iter().rev().find(|l| l.bind_id == Some(id)) {
            return Some(l);
        }
        self.locals
            .iter()
            .rev()
            .find(|l| l.bind_id.is_none() && l.name.as_str() == name)
    }

    /// Resolve a local by name only — for sites with no BindId (string /
    /// variant / nullable / value reads via `ref_local_name`).
    fn lookup_name(&self, name: &str) -> Option<&Local> {
        self.locals.iter().rev().find(|l| l.name.as_str() == name)
    }

    /// Resolve a local by BindId only (no name fallback). Used to read a
    /// captured feeder's disc for HOF firing propagation
    /// ([`inherit_hof_firing`]).
    fn lookup_by_id(&self, id: BindId) -> Option<&Local> {
        self.locals.iter().rev().find(|l| l.bind_id == Some(id))
    }

    /// Snapshot the binding list length. Pair with [`Self::truncate`] to
    /// pop every binding introduced since the mark, so a block /
    /// select-arm / loop-body scope doesn't leak names into its
    /// enclosing scope.
    ///
    /// `truncate` does NOT emit any runtime drops. Dropping owned
    /// composite / string / value locals is the scope-exit code's job
    /// (`emit_scope_drops`) or the terminating return
    /// (`drop_owned_composites`); `truncate` is purely compile-time env
    /// hygiene.
    fn mark(&self) -> usize {
        self.locals.len()
    }

    fn truncate(&mut self, mark: usize) {
        self.locals.truncate(mark);
    }
}

/// The CLIF payload type for a local of `kind` — a scalar's natural
/// type, else `I64` (pointer / value word).
fn local_payload_ty(kind: LocalKind) -> ClifType {
    match kind {
        LocalKind::Scalar(p) => prim_to_clif(p),
        _ => types::I64,
    }
}

/// Declare fresh disc + payload Variables holding `disc`/`payload`, then
/// bind them as a [`Local`] of `kind`. The single end-state bind path —
/// every local is a two-register Value carrying its own taint in the
/// disc.
fn bind_local(
    cx: &mut BodyCx,
    name: ArcStr,
    disc: ClifValue,
    payload: ClifValue,
    kind: LocalKind,
    bind_id: Option<BindId>,
) -> ValueVar {
    let dv = cx.b.declare_var(types::I64);
    cx.b.def_var(dv, disc);
    let pv = cx.b.declare_var(local_payload_ty(kind));
    cx.b.def_var(pv, payload);
    let vv = ValueVar { disc: dv, payload: pv };
    cx.env.bind(name, vv, kind, bind_id);
    vv
}

/// Emit an HOF operand and FORCE its #219 taint (a tainted operand
/// bottoms the whole HOF), returning the payload word. Used by the
/// stdlib array HOF `emit_clif` impls for operands whose scaffold has no
/// per-value taint channel — the source array, a scalar predicate /
/// body, a composite flat_map body, the `init` count. Folds to no branch
/// for an untainted operand.
pub fn emit_forced<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    node: &Node<R, E>,
) -> Result<ClifValue> {
    let cv = node.emit_clif(cx)?;
    let valid = is_untainted(cx.b, cv.disc);
    emit_bottom_abort(cx.b, cx.env, cx.ctx, valid)?;
    Ok(cv.payload)
}

/// Wrap an owned `*mut ValArray` pointer as a composite [`CompiledExpr`]
/// (const `ARRAY` disc). The result of an HOF that produces an array.
pub fn array_result(cx: &mut BodyCx, ptr: ClifValue) -> CompiledExpr {
    let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    CompiledExpr::new(disc, ptr)
}

/// Wrap a register-scalar payload as a [`CompiledExpr`] with the prim's
/// natural (taint-clear) disc. The result of an HOF that produces a
/// scalar (`array::fold` over a scalar accumulator). Unlike
/// [`array_result`], the disc's tag matches the payload's shape, so the
/// value survives a `connect`'s `set_var` (which reconstructs a `Value`
/// from the disc) — an ARRAY disc over a scalar payload would deref the
/// scalar as a `*ValArray`.
pub fn scalar_result(
    cx: &mut BodyCx,
    prim: PrimType,
    payload: ClifValue,
) -> CompiledExpr {
    CompiledExpr::new(scalar_disc(cx.b, prim), payload)
}

/// Like [`emit_forced`] but returns the whole [`CompiledExpr`] (disc +
/// payload) rather than just the payload. The abort guarantees the
/// continue path is reached only when [`TAINT`] is clear, so the returned
/// disc carries just the operand's [`STALE`] bit.
pub fn emit_forced_keep<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    node: &Node<R, E>,
) -> Result<CompiledExpr> {
    let cv = node.emit_clif(cx)?;
    let valid = is_untainted(cx.b, cv.disc);
    emit_bottom_abort(cx.b, cx.env, cx.ctx, valid)?;
    Ok(cv)
}

/// Bind an EXISTING scalar Variable as a local (a loop counter /
/// accumulator the scaffold rebinds in place — `bind_local` would
/// declare a fresh one), with a caller-owned DISC Variable: reads of
/// the local see whatever taint/staleness the caller carries into it
/// (a fold accumulator's loop-carried taint, an element's
/// source-inherited STALE bit).
pub(crate) fn bind_scalar_var_with_disc(
    cx: &mut BodyCx,
    name: ArcStr,
    prim: PrimType,
    payload_var: Variable,
    disc_var: Variable,
    bind_id: Option<BindId>,
) {
    cx.env.bind(
        name,
        ValueVar { disc: disc_var, payload: payload_var },
        LocalKind::Scalar(prim),
        bind_id,
    );
}

// ─── Body / statement compilation ────────────────────────────────

/// The rebind-and-jump core of a self tail-call
/// ([`emit_self_tail_call`]).
/// `new_vals` are the already-evaluated replacement values for the
/// leading `new_vals.len()` tail-call slots — the FORMALS; trailing
/// capture slots stay bound (loop-invariant within one invocation).
/// Each rebind writes BOTH registers of the slot's ValueVar — the
/// payload AND the disc. The disc carry is load-bearing for firing:
/// the loop's terminating arm returns a formal, and its disc at that
/// point must be the LAST iteration's computed fired-ness (the kernel
/// mirror of the interp loop's per-iteration `pat.bind`). Rebinding
/// payloads only left every formal's disc at its ENTRY value, so a
/// constant-seeded accumulator (`g(in0, i64:0)`) returned the seed's
/// non-init STALE on every later invocation — the callee flagged
/// not-fresh, the caller forced bottom, and a live-input-driven tail
/// loop fired exactly once (soak jul04 follow-up; the same
/// carry-the-disc lesson as fold/#9).
/// `sources[i]` classifies each arg's composite provenance so a
/// Borrowed pointer is refcount-bumped before the old slot value
/// drops. Drops every owned non-slot local above the param mark
/// (per-iteration lets would leak otherwise), truncates the
/// compile-time env back to the params, and jumps to the loop head.
fn emit_tail_rebind_jump(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
    new_vals: Vec<CompiledExpr>,
    sources: &[CompositeSource],
) -> Result<()> {
    let head = ctx.loop_head.ok_or_else(|| {
        anyhow!("kernel malformed: TailCall in kernel without has_tail_loop")
    })?;
    // Back-compat: hand-built test kernels leave `tail_call_slots`
    // empty and assume all params are scalar in declaration order.
    // Drive the rebind positionally in that case.
    if ctx.tail_call_slots.is_none() {
        debug_assert_eq!(new_vals.len(), ctx.param_mark);
        for (i, v) in new_vals.iter().enumerate() {
            b.def_var(env.locals[i].vv.payload, v.payload);
            b.def_var(env.locals[i].vv.disc, v.disc);
        }
        env.truncate(ctx.param_mark);
        b.ins().jump(head, &[]);
        return Ok(());
    }
    let slots = ctx.tail_call_slots.unwrap();
    // Slots cover EVERY kernel value param (they double as the
    // runtime arg layout — `arg_layout`, kernel.rs); a tail call
    // rebinds only the leading FORMALS.
    debug_assert!(new_vals.len() <= slots.len());
    use kernel_abi::TailCallSlotKind;
    let drop_helper = ctx
        .helper_refs
        .get("graphix_valarray_drop")
        .ok_or_else(|| anyhow!("missing graphix_valarray_drop"))?;
    let clone_helper = ctx
        .helper_refs
        .get("graphix_valarray_clone")
        .ok_or_else(|| anyhow!("missing graphix_valarray_clone"))?;
    let mut new_vals: Vec<CompiledExpr> = new_vals;
    for (i, slot) in slots.iter().take(new_vals.len()).enumerate() {
        if matches!(slot.kind, TailCallSlotKind::ValArray)
            && sources[i] == CompositeSource::Borrowed
        {
            // Clone (refcount bump) so the next iteration holds an
            // owned reference, separate from any other live alias.
            let call = b.ins().call(clone_helper, &[new_vals[i].payload]);
            new_vals[i].payload = b.inst_results(call)[0];
        }
    }
    for (slot, v) in slots.iter().zip(new_vals.iter()) {
        match slot.kind {
            TailCallSlotKind::Scalar(_) => {
                let vv = env
                    .lookup_name(&slot.name)
                    .ok_or_else(|| {
                        anyhow!("TailCall: scalar slot `{}` not in env", slot.name)
                    })?
                    .vv;
                b.def_var(vv.payload, v.payload);
                b.def_var(vv.disc, v.disc);
            }
            TailCallSlotKind::ValArray => {
                // Composite rebind: drop the previously-owned pointer
                // in the slot, then store the new owned
                // `*mut ValArray`. This closes the leak we had in
                // Phase 2.
                let vv = env
                    .lookup_name(&slot.name)
                    .ok_or_else(|| {
                        anyhow!("TailCall: composite slot `{}` not in env", slot.name)
                    })?
                    .vv;
                let old = b.use_var(vv.payload);
                b.ins().call(drop_helper, &[old]);
                b.def_var(vv.payload, v.payload);
                b.def_var(vv.disc, v.disc);
            }
            TailCallSlotKind::Variant => {
                return Err(anyhow!("JIT: variant tail-call rebind not yet supported"));
            }
            TailCallSlotKind::Nullable => {
                // The tail-loop gate (`build_lambda_kernel`'s
                // all-formals-Prim/Array/Tuple/Struct check) keeps
                // Nullable formals out of tail loops, so this is
                // unreachable in practice. Err keeps the codegen safe
                // if the gating ever drifts.
                return Err(anyhow!(
                    "JIT: nullable tail-call rebind not supported — \
                     the tail-loop gate should have refused this kernel"
                ));
            }
            TailCallSlotKind::String | TailCallSlotKind::Value => {
                // A recursive lambda whose tail-call rebinds a String
                // / value-shape param. The JIT doesn't lower the
                // owned-ArcStr / two-word Value rebind yet; bail so
                // the kernel falls back to the node-walk.
                return Err(anyhow!(
                    "JIT: string/value tail-call rebind not yet \
                     supported — falling back to interp"
                ));
            }
        }
    }
    // Drop any owned composite/variant/nullable/string locals
    // introduced between `ctx.param_mark` and now that ISN'T in
    // `tail_call_slots` (slot rebinds drop the old slot value above;
    // non-slot lets above the tail-call would leak per iteration
    // otherwise). Block / select-arm locals were already dropped at
    // runtime by their scope-exit code (block emission, terminating
    // statements), so iterating the env's tail catches only the
    // non-Block top-level lets that didn't get a rebind slot.
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
    // Composite slot rebinds already drop their old value (above);
    // skip any entry whose name matches a rebind slot. Drop every other
    // owned non-slot local above the param mark, by kind.
    let slot_names: std::collections::HashSet<&str> =
        slots.iter().map(|s| s.name.as_str()).collect();
    let drops: smallvec::SmallVec<[(LocalKind, ValueVar); 8]> = env.locals
        [ctx.param_mark..]
        .iter()
        .filter(|l| !slot_names.contains(l.name.as_str()))
        .map(|l| (l.kind, l.vv))
        .collect();
    for (kind, vv) in drops {
        match kind {
            LocalKind::Scalar(_) => {}
            LocalKind::Composite => {
                let ptr = b.use_var(vv.payload);
                b.ins().call(arr_drop, &[ptr]);
            }
            LocalKind::String => {
                let ptr = b.use_var(vv.payload);
                b.ins().call(str_drop, &[ptr]);
            }
            LocalKind::Variant | LocalKind::Nullable | LocalKind::Value => {
                let disc = b.use_var(vv.disc);
                let payload = b.use_var(vv.payload);
                b.ins().call(val_drop, &[disc, payload]);
            }
        }
    }
    // Compile-time env-Vec hygiene: pop everything above the param
    // mark so the next iteration starts with a clean lexical state.
    env.truncate(ctx.param_mark);
    b.ins().jump(head, &[]);
    Ok(())
}

/// Emit a value-bottom abort: when the I8 `valid` bit is 0, set the
/// pending flag, run `emit_pending_cleanup`, and jump to `pending_exit`
/// (so `Kernel::update` returns `None`). Falls through to a fresh
/// `continue_block` when valid. Used where a tainted scalar is consumed
/// by a site that has no per-value validity channel (e.g. a composite
/// producer field) — the bottom must propagate to the kernel OUTPUT.
fn emit_bottom_abort(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
    valid: ClifValue,
) -> Result<()> {
    let pending_set = ctx
        .helper_refs
        .get("graphix_dyncall_set_pending")
        .ok_or_else(|| anyhow!("missing graphix_dyncall_set_pending"))?;
    let pre_pending = b.create_block();
    let continue_block = b.create_block();
    let pending_exit = {
        let mut slot = ctx.pending_exit.borrow_mut();
        match *slot {
            Some(blk) => blk,
            None => {
                let blk = b.create_block();
                *slot = Some(blk);
                blk
            }
        }
    };
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

/// Emit a cooperative-interrupt poll at a loop head: call
/// `graphix_interrupted`, and if it returns nonzero take the kernel's
/// abort path — set the pending flag, drop in-flight owned
/// buffers/composites (`emit_pending_cleanup`, which drains the HOF
/// result buffer off `dyncall_buf_stack` and drops owned params), and
/// jump to `pending_exit` so `Kernel::update` yields `None`. Falls
/// through to a fresh `continue_block` otherwise. Reused at the tail-loop
/// head and every HOF scaffold loop head so a wedged native loop honours
/// `interrupt()`/`abort()`. This is [`emit_bottom_abort`] with the branch
/// reversed — abort on the interrupted bit instead of on an invalid bit.
///
/// First cut polls every iteration; amortizing the helper call to every K
/// iterations is a future optimization (the poll roughly doubles the
/// tightest native loops).
fn emit_interrupt_check(
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
        .get("graphix_dyncall_set_pending")
        .ok_or_else(|| anyhow!("missing graphix_dyncall_set_pending"))?;
    let call = b.ins().call(interrupted, &[]);
    let intr = b.inst_results(call)[0];
    let pre_pending = b.create_block();
    let continue_block = b.create_block();
    let pending_exit = {
        let mut slot = ctx.pending_exit.borrow_mut();
        match *slot {
            Some(blk) => blk,
            None => {
                let blk = b.create_block();
                *slot = Some(blk);
                blk
            }
        }
    };
    // nonzero (interrupted) ⇒ abort; zero ⇒ continue the loop body.
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

// ─── Node → CLIF body emission ──────────────────────────────────
//
// Bodies are emitted by walking the region-root [`Node`] graph:
// `Update::emit_clif` / `Apply::emit_clif` impls recurse into children
// and delegate to the `emit_*_node` helpers below, which share the
// scalar codegen primitives
// (`compile_bin`/`compile_cmp`/`compile_cast`/`JitEnv`/`CompiledExpr`)
// so there is exactly one home for the CLIF logic.
//
// A node without an `emit_clif` impl (any async op, an unsupported
// shape) returns `Err` → the kernel build fails → `try_fuse` leaves
// the region un-spliced → it node-walks (the universal fallback).
// Bottom/taint: a may-bottom scalar (div/mod-by-zero) produces a
// `CompiledExpr` whose disc carries `TAINT`, resolved where it's
// consumed (`emit_bottom_abort` / the kernel boundary).

/// A type-erased producer of a kernel function BODY, given the
/// already-set-up entry block, bound params, and lowering context. The
/// sole implementor is [`NodeBodyEmitter`], which walks the
/// region-root `Node` via `emit_clif` recursion. Erasing the `R`/`E`
/// of the Node lets the (monomorphic) JIT-compile pipeline thread a
/// generic `&Node<R, E>` through without itself becoming generic.
trait BodyEmitter {
    fn emit(
        &self,
        b: &mut FunctionBuilder,
        env: &mut JitEnv,
        ctx: &LowerCtx,
    ) -> Result<()>;

    /// Discovered sync-builtin Apply sites for the region being
    /// emitted (`CallSite::emit_clif` lowers a registered site to a
    /// DynCall via [`BodyCx::builtin_site`]). `None` for callee
    /// bodies (their inner sites are #203-unresolved).
    fn builtin_apply_sites(
        &self,
    ) -> Option<&nohash::IntMap<ExprId, BuiltinCallSiteInfo>> {
        None
    }

    /// Discovered statically-resolved lambda call sites for the region
    /// being emitted (`CallSite::emit_clif` lowers a registered site to
    /// a CLIF `call` via [`BodyCx::lambda_site`]). `None` for callee
    /// bodies (a callee's only cross-kernel reference is itself).
    fn lambda_call_sites(&self) -> Option<&nohash::IntMap<ExprId, LambdaCallInfo>> {
        None
    }

    /// `Some` when the kernel being emitted is a self-recursive lambda
    /// body: the binding its self-references carry + the kernel's own
    /// call descriptor. Drives tail-position rebind-and-jump
    /// (`emit_body_tail`), value-position self-calls
    /// (`CallSite::emit_clif` → the kernel's own FuncRef), and the
    /// self-FuncRef import in `define_kernel_body`.
    fn self_call(&self) -> Option<&(BindId, LambdaCallInfo)> {
        None
    }

    /// The environment snapshot for TYPE RESOLUTION ONLY (#218): node
    /// `typ` cells can carry `Type::Ref`s to abstract type names whose
    /// concrete rep needs `env.lookup_ref` + the abstract registry
    /// (`resolve_abstract`) before `abi_kind`/freeze can classify
    /// them. Defaults to `None`; [`NodeBodyEmitter`] supplies it. NOT
    /// for binding lookups; those stay in the analysis phase, per the
    /// BodyCx design.
    fn type_env(&self) -> Option<&Env> {
        None
    }

    /// The compiling context's abstract-type registry — the fusion
    /// classifiers (`abi_kind`/freeze/`resolve_abstract`) consult it to
    /// peek through abstract types to their wire shape. Threaded from the
    /// `ExecCtx` into [`LowerCtx`] so emit-time type classification has
    /// the same view as the analysis phase.
    fn registry(&self) -> &AbstractRegistry;

    /// Base offset added to every DynCall `fn_index` this body bakes, so
    /// the body indexes its slots in the REGION-WIDE combined `dyn_slots`
    /// table (parent slots first, then each callee's). `0` for the parent
    /// (its slots lead the table) and for a callee with no DynCalls;
    /// nonzero only for a callee body whose `fn_params` sit at an offset.
    /// Doubles as the `base` half of the `(ptr, base)` JIT cache key —
    /// see `compile_kernel_with_callees_inner`. Default `0`.
    fn fn_index_offset(&self) -> u32 {
        0
    }

    /// Whether this body's kernel return FORCES on not-fresh
    /// (TAINT | STALE). `true` for a PUBLISHED body — the region root /
    /// a per-slot HOF callback whose result flows through the wrapper
    /// to `Kernel::update`: a not-fresh result must yield `None`,
    /// matching the node-walk's "publish only on Some". `false` for a
    /// cross-kernel CALLEE body, which never forces: its result's
    /// not-fresh bits ride back to the caller as DATA — in-band for
    /// two-word (value-shape) returns, via `CALLEE_RESULT_FLAGS` for
    /// one-word returns (`emit_flag_not_fresh`) — so a bottomed or
    /// unfired callee result bottoms only the caller's consumers that
    /// read it (#219), like a node-walk callsite that produced no
    /// value. Default `true`.
    fn gate_stale_at_return(&self) -> bool {
        true
    }

    /// The region's LIFTED connect-target bind ids — let-bound scalar
    /// counters/accumulators routed in as kernel inputs (feeders).
    /// `emit_let_node` reads this to emit the seed-select for a lifted
    /// binding instead of a plain let; `emit_connect_node` reads it to
    /// allow the variable write. Empty for callees and for any region
    /// with no lifts. Default: a shared empty set.
    fn lifted(&self) -> &ahash::AHashSet<BindId> {
        static EMPTY: std::sync::LazyLock<ahash::AHashSet<BindId>> =
            std::sync::LazyLock::new(ahash::AHashSet::default);
        &EMPTY
    }

    /// Whether this body may claim per-instance state words (see
    /// [`BodyCx::claim_state_word`]). `true` only for the region
    /// parent's root body: a CALLEE is reached from arbitrarily many
    /// call sites, and one static word offset can't hold per-call-site
    /// memory — its stateful constructs keep the stateless
    /// approximation. Default `false`.
    fn allow_state(&self) -> bool {
        false
    }
}

/// The body emitter — walks the region-root `Node` via `emit_clif`
/// recursion and emits the kernel return. `return_type` comes from
/// the `KernelSig` so the boundary marshalling agrees with the kernel
/// signature / wrapper / runtime arg-pack.
struct NodeBodyEmitter<'a, R: Rt, E: UserEvent> {
    root: &'a Node<R, E>,
    return_type: &'a Type,
    apply_sites: &'a nohash::IntMap<ExprId, BuiltinCallSiteInfo>,
    lambda_sites: &'a nohash::IntMap<ExprId, LambdaCallInfo>,
    /// `Some` for a self-recursive callee body — see
    /// [`BodyEmitter::self_call`].
    self_call: Option<&'a (BindId, LambdaCallInfo)>,
    /// Type-resolution env snapshot — see [`BodyEmitter::type_env`].
    type_env: &'a Env,
    /// The compiling `ExecCtx`'s abstract-type registry — see
    /// [`BodyEmitter::registry`].
    registry: &'a AbstractRegistry,
    /// DynCall `fn_index` base — see [`BodyEmitter::fn_index_offset`].
    /// `0` for region parents and per-slot callbacks; a callee's
    /// combined-table offset for cross-kernel callee bodies.
    fn_index_offset: u32,
    /// STALE-gate the return — see [`BodyEmitter::gate_stale_at_return`].
    /// `true` for the published parent body, `false` for callee bodies.
    gate_stale: bool,
    /// Lifted connect-target bind ids — see [`BodyEmitter::lifted`].
    lifted: &'a ahash::AHashSet<BindId>,
    /// May this body claim per-instance state words — see
    /// [`BodyEmitter::allow_state`]. `true` for the region parent's
    /// root body, `false` for callee bodies.
    allow_state: bool,
}

impl<R: Rt, E: UserEvent> BodyEmitter for NodeBodyEmitter<'_, R, E> {
    fn emit(
        &self,
        b: &mut FunctionBuilder,
        env: &mut JitEnv,
        ctx: &LowerCtx,
    ) -> Result<()> {
        let mut cx = BodyCx { b: &mut *b, env: &mut *env, ctx };
        match self.self_call {
            // Recursive body: tail-position walk — self tail-calls
            // become the rebind-and-jump loop, every other path
            // returns directly. (Non-recursive bodies keep the
            // value-position emission below: per-arm returns would be
            // equivalent codegen but churn every existing kernel.)
            Some(_) => emit_body_tail(&mut cx, self.root, self.return_type),
            None => emit_return_from_node(&mut cx, self.return_type, self.root),
        }
    }

    fn builtin_apply_sites(
        &self,
    ) -> Option<&nohash::IntMap<ExprId, BuiltinCallSiteInfo>> {
        Some(self.apply_sites)
    }

    fn lambda_call_sites(&self) -> Option<&nohash::IntMap<ExprId, LambdaCallInfo>> {
        Some(self.lambda_sites)
    }

    fn self_call(&self) -> Option<&(BindId, LambdaCallInfo)> {
        self.self_call
    }

    fn type_env(&self) -> Option<&Env> {
        Some(self.type_env)
    }

    fn registry(&self) -> &AbstractRegistry {
        self.registry
    }

    fn fn_index_offset(&self) -> u32 {
        self.fn_index_offset
    }

    fn gate_stale_at_return(&self) -> bool {
        self.gate_stale
    }

    fn lifted(&self) -> &ahash::AHashSet<BindId> {
        self.lifted
    }

    fn allow_state(&self) -> bool {
        self.allow_state
    }
}

/// The open-kernel emission context handed to
/// [`Update::emit_clif`] / [`crate::Apply::emit_clif`] impls:
/// everything needed to add CLIF to the function currently being
/// defined. Constructed by the kernel-body emitter right after the
/// entry binder installs the params; emission recursion is
/// `child.emit_clif(cx)`. `b` is the raw cranelift builder — full
/// power for novel emitters; the graphix-specific surface (env binds,
/// helper FuncRefs, element reads, taint/pending) stays behind
/// crate-internal relays so `JitEnv`/`LowerCtx` remain private.
pub struct BodyCx<'a, 'f, 'c> {
    pub b: &'a mut FunctionBuilder<'f>,
    pub(crate) env: &'a mut JitEnv,
    pub(crate) ctx: &'a LowerCtx<'c>,
}

impl<'a, 'f, 'c> BodyCx<'a, 'f, 'c> {
    /// The compiling context's abstract-type registry, for the emit-time
    /// type classifiers (`kernel_abi::abi_kind`/`freeze_for_abi`/…). Borrowed
    /// from the [`LowerCtx`], which got it from [`BodyEmitter::registry`].
    /// Returns the `'c`-lifetime borrow (the registry lives in the
    /// `LowerCtx`, not in `self`), so reading it does NOT hold `cx`
    /// borrowed — a reader call can coexist with `&mut cx` in the same
    /// expression.
    pub fn registry(&self) -> &'c AbstractRegistry {
        self.ctx.registry
    }

    /// FuncRef for a registered `emit_helpers` runtime helper.
    pub fn helper(&self, name: &str) -> Result<FuncRef> {
        self.ctx.helper_refs.get(name).ok_or_else(|| anyhow!("missing helper {name}"))
    }

    /// The `event.init` flag (`I64`, 1 on the kernel's init cycle),
    /// loaded from wire slot 0 (see [`kernel_abi::CTX_WIRE_SLOTS`]).
    /// `emit_const_node` reads it to STALE-gate a constant: a constant
    /// fires only at init, then carries a cached (stale) value.
    pub fn init_flag(&self) -> ClifValue {
        self.ctx.init_flag
    }

    /// The per-instance state-buffer pointer (`I64`), loaded from wire
    /// slot 1. Only meaningful at offsets returned by
    /// [`claim_state_word`](Self::claim_state_word); 0 when the kernel
    /// claimed nothing (no claimed offset exists to read through it).
    pub fn state_ptr(&self) -> ClifValue {
        self.ctx.state_ptr
    }

    /// Claim one `u64` of per-kernel-INSTANCE cross-invocation memory,
    /// returning its byte offset from [`state_ptr`](Self::state_ptr) —
    /// or `None` when state is unavailable here: inside a scaffold
    /// loop (one static word can't hold per-slot memory) or in a
    /// callee body (one word can't hold per-call-site memory). Callers
    /// MUST emit their stateless approximation on `None`.
    ///
    /// The buffer is zero-initialized per instance (fresh per
    /// `clone_rebind`), so consumers store `value + 1` and read 0 as
    /// "no previous observation" — init semantics fall out of the
    /// zeroing. See `design/kernel_instance_state.md`.
    pub fn claim_state_word(&self) -> Option<i32> {
        if !self.ctx.state_enabled || self.ctx.loop_depth.get() > 0 {
            return None;
        }
        let idx = self.ctx.state_next.get();
        self.ctx.state_next.set(idx + 1);
        Some((idx * 8) as i32)
    }

    /// Bracket scaffold-loop body emission: state claims are refused
    /// while any loop is open (see [`claim_state_word`](Self::claim_state_word)).
    pub fn enter_loop(&self) {
        self.ctx.loop_depth.set(self.ctx.loop_depth.get() + 1);
    }

    pub fn exit_loop(&self) {
        let d = self.ctx.loop_depth.get();
        debug_assert!(d > 0, "exit_loop without a matching enter_loop");
        self.ctx.loop_depth.set(d.saturating_sub(1));
    }

    /// True iff `bind_id` is a LIFTED connect target in this region — a
    /// let-bound scalar counter routed in as a feeder. `emit_let_node`
    /// emits a seed-select for it; `emit_connect_node` allows its write.
    pub(crate) fn is_lifted(&self, bind_id: BindId) -> bool {
        self.ctx.lifted.contains(&bind_id)
    }

    /// Stable `*const ArcStr` for `s` as an `iconst`, interned lazily
    /// at emission (no body prewalk on the direct path — coverage is
    /// exact by construction). The arena entry is individually boxed
    /// so its address survives growth; `define_kernel_body` merges the
    /// arena into the per-kernel [`KernelStrings`] so it outlives the
    /// compiled code that baked the pointer.
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

    /// The DynCall `fn_index` base for the body being emitted — added to
    /// every DynCall site's `info.fn_index` so a callee body indexes the
    /// region-wide combined `dyn_slots` table. `0` for parents / per-slot
    /// callbacks. See [`BodyEmitter::fn_index_offset`].
    pub(crate) fn fn_index_offset(&self) -> u32 {
        self.ctx.fn_index_offset
    }

    /// The discovered builtin Apply-site info for `id`, if the
    /// region's pre-build discovery pass registered one (direct path
    /// only — `None` whenever no [`BodyEmitter`] supplied the map).
    pub(crate) fn builtin_site(&self, id: ExprId) -> Option<&BuiltinCallSiteInfo> {
        self.ctx.builtin_apply_sites.and_then(|m| m.get(&id))
    }

    /// The discovered lambda call-site info for `id`, if `try_fuse`'s
    /// analysis registered one (direct path only). A `Some` means the
    /// callee kernel is declared in this function's `callee_refs` under
    /// its kernel identity and ready to `call`.
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
}

/// Ownership classification of a Node-rooted result. A
/// binding read is BORROWED (the env slot keeps the ref); grouping and
/// blocks are transparent to their tail; everything else (literals,
/// producers, calls) hands out an owned ref. Used by the direct path's
/// let-bind / block-tail / kernel-return sites to decide whether a
/// clone is needed before the source's scope drops.
pub fn node_composite_source<R: Rt, E: UserEvent>(node: &Node<R, E>) -> CompositeSource {
    use NodeView;
    let mut n: &dyn Update<R, E> = &**node;
    loop {
        match n.view() {
            NodeView::Ref(_) => return CompositeSource::Borrowed,
            NodeView::ExplicitParens(p) => n = &*p.n,
            NodeView::Block(blk) => match blk.children.last() {
                Some(tail) => n = &**tail,
                None => return CompositeSource::Owned,
            },
            _ => return CompositeSource::Owned,
        }
    }
}

/// Clone a borrowed composite pointer so the result is owned; pass an
/// owned one through. `pub` for package crates' `Apply::emit_clif`
/// impls (e.g.
/// `array::flat_map`'s body hand-off).
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
/// owned one through. By-source variant of `ensure_owned_value`.
/// `pub` for package crates' `Apply::emit_clif` impls (e.g.
/// `array::find_map`'s body pair, which escapes its loop as the
/// kernel result).
pub fn ensure_owned_value_src(
    cx: &mut BodyCx,
    src: CompositeSource,
    disc: ClifValue,
    payload: ClifValue,
) -> Result<(ClifValue, ClifValue)> {
    match src {
        CompositeSource::Owned => Ok((disc, payload)),
        CompositeSource::Borrowed => {
            // `graphix_value_clone` takes a `TagValue`: it masks the disc
            // to refcount correctly but PRESERVES the tag, so #219 taint
            // rides straight through the clone — no clean-before /
            // re-propagate-after needed.
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
fn pending_exit_block(cx: &mut BodyCx) -> Block {
    let mut slot = cx.ctx.pending_exit.borrow_mut();
    match *slot {
        Some(blk) => blk,
        None => {
            let blk = cx.b.create_block();
            *slot = Some(blk);
            blk
        }
    }
}

/// Return-path FORCE for a PUBLISHED body (`gate_stale_at_return`): if
/// `disc` is NOT fresh (tainted bottom OR STALE — didn't fire this
/// cycle), drop the owned result (`drop_fn`), set pending, run the
/// owned-set cleanup, and jump `pending_exit`; otherwise fall through —
/// a stale root means "didn't fire — don't publish." On return the
/// builder is positioned in the fresh (fired-with-value) block. Folds
/// to no branch for a fresh const disc. A cross-kernel CALLEE body
/// never forces: its result's not-fresh bits ride back to the caller
/// as data ([`emit_flag_not_fresh`], or in-band for two-word returns).

/// Callee-mode return-path companion to [`emit_force`]: instead of
/// bottoming the kernel on a not-fresh RESULT, record its `TAINT`/
/// `STALE` disc bits in [`CALLEE_RESULT_FLAGS`] — the scalar/composite
/// return ABI has no disc word — and return the value as-is (a tainted
/// value is already the helper-safe placeholder; a stale value is the
/// real cached value). The caller takes the bits right after the call
/// and ORs them into its synthesized result disc, so a bottomed or
/// unfired callee result rides back as DATA (#219): the caller bottoms
/// only if its taken output path consumes it — matching the node-walk,
/// where a callsite arg that never fires silences only the consumers
/// that read it. Genuine aborts (depth trip, interrupt, async pend)
/// keep the pending path. Folds to nothing for a fresh const disc.
fn emit_flag_not_fresh(cx: &mut BodyCx, disc: ClifValue) -> Result<()> {
    let bits = cx.b.ins().band_imm(disc, TAINT | STALE);
    let set = cx.helper("graphix_callee_flags_set")?;
    let do_set = cx.b.create_block();
    let cont = cx.b.create_block();
    let nz = cx.b.ins().icmp_imm(IntCC::NotEqual, bits, 0);
    cx.b.ins().brif(nz, do_set, &[], cont, &[]);
    cx.b.switch_to_block(do_set);
    cx.b.seal_block(do_set);
    cx.b.ins().call(set, &[bits]);
    cx.b.ins().jump(cont, &[]);
    cx.b.switch_to_block(cont);
    cx.b.seal_block(cont);
    Ok(())
}

fn emit_force(
    cx: &mut BodyCx,
    disc: ClifValue,
    drop_fn: impl FnOnce(&mut BodyCx) -> Result<()>,
) -> Result<()> {
    // The return-path firing gate: a not-fresh result (bottom OR
    // didn't-fire-this-cycle) drops the owned result and bottoms the
    // kernel (returns None), matching the node-walk's "publish only on
    // Some". Folds to no branch for a fresh const disc.
    let not_fresh = is_not_fresh(cx.b, disc);
    let pending_set = cx.helper("graphix_dyncall_set_pending")?;
    let pre = cx.b.create_block();
    let ret = cx.b.create_block();
    let exit = pending_exit_block(cx);
    cx.b.ins().brif(not_fresh, pre, &[], ret, &[]);
    cx.b.switch_to_block(pre);
    cx.b.seal_block(pre);
    drop_fn(cx)?;
    cx.b.ins().call(pending_set, &[]);
    emit_pending_cleanup(cx.b, cx.env, cx.ctx)?;
    cx.b.ins().jump(exit, &[]);
    cx.b.switch_to_block(ret);
    cx.b.seal_block(ret);
    Ok(())
}

/// Unconditionally bottom the kernel from the current block: set the
/// pending flag, drop the in-flight owned set, and jump to
/// `pending_exit` (so `Kernel::update` returns `None`). Terminates the
/// block.
fn emit_kernel_bottom(cx: &mut BodyCx) -> Result<()> {
    let pending_set = cx.helper("graphix_dyncall_set_pending")?;
    let exit = pending_exit_block(cx);
    cx.b.ins().call(pending_set, &[]);
    emit_pending_cleanup(cx.b, cx.env, cx.ctx)?;
    cx.b.ins().jump(exit, &[]);
    Ok(())
}

/// Emit `node` as the kernel's result and return it under
/// `return_type`'s convention. When the return is VALUE-SHAPE
/// (Variant/Nullable/Value — the in-band 2-word `(disc, payload)`
/// pair) the body node's own shape may differ: a callback declared
/// `fn(x: 'a) -> ['b, null]` whose body is a tuple literal emits the
/// COMPOSITE convention (`payload = *mut ValArray`, a box pointer),
/// and returning that pair raw hands `TagValue::from_raw` a box
/// pointer as the in-band `ValArray` word — the runtime decode then
/// dereferences garbage (soak jul05 items 5/10/13, SIGSEGV/SIGABRT).
/// Route through `emit_owned_value_operand_node` — the same widening
/// select arms and value operands use — which converts each body
/// shape to a genuine owned Value pair (composites via
/// `graphix_value_new_from_array`, strings via
/// `graphix_value_new_string`, scalars by inline packing, value-shape
/// pass-through).
fn emit_return_from_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    return_type: &Type,
    node: &Node<R, E>,
) -> Result<()> {
    match kernel_abi::abi_kind(cx.registry(), return_type) {
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

fn emit_kernel_return(
    cx: &mut BodyCx,
    return_type: &Type,
    mut cv: CompiledExpr,
    src: CompositeSource,
) -> Result<()> {
    // Apply the tail-selects' control-dependence firing (see
    // `LowerCtx::tail_scrut_stale`): the result fires if its value
    // chain fired OR any tail-select scrutinee on the executed path
    // did. With no tail select the accumulator is the STALE-set
    // identity and this folds to the value's own bit.
    {
        let acc = cx.b.use_var(cx.ctx.tail_scrut_stale);
        let vs = cx.b.ins().band_imm(cv.disc, STALE);
        let folded = cx.b.ins().band(vs, acc);
        let cleaned = cx.b.ins().band_imm(cv.disc, !STALE);
        cv.disc = cx.b.ins().bor(cleaned, folded);
    }
    match kernel_abi::abi_kind(cx.registry(), return_type) {
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let (disc, payload) = ensure_owned_value_src(cx, src, cv.disc, cv.payload)?;
            if cx.ctx.gate_stale_at_return {
                // FORCE at the output — a not-fresh result (bottom OR
                // didn't fire this cycle) drops the owned value + bottoms
                // the kernel; else fall through. Folds to no branch for a
                // fresh const disc.
                emit_force(cx, disc, |cx| {
                    let val_drop = cx.helper("graphix_value_drop")?;
                    cx.b.ins().call(val_drop, &[disc, payload]);
                    Ok(())
                })?;
            }
            // Callee mode: the two-word return carries the disc in-band,
            // so TAINT/STALE ride back as data — no force, no flags.
            drop_owned_composites(cx.b, cx.env, cx.ctx)?;
            cx.b.ins().return_(&[disc, payload]);
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let v = ensure_owned_composite_src(cx, src, cv.payload)?;
            if cx.ctx.gate_stale_at_return {
                // #219: a tainted composite output bottoms — drop the
                // owned array on the tainted path (folds when untainted).
                emit_force(cx, cv.disc, |cx| {
                    let d = cx.helper("graphix_valarray_drop")?;
                    cx.b.ins().call(d, &[v]);
                    Ok(())
                })?;
            } else {
                emit_flag_not_fresh(cx, cv.disc)?;
            }
            drop_owned_composites(cx.b, cx.env, cx.ctx)?;
            cx.b.ins().return_(&[v]);
        }
        Some(AbiKind::String) => {
            // String results are owned at production (reads clone);
            // no ensure needed.
            if cx.ctx.gate_stale_at_return {
                emit_force(cx, cv.disc, |cx| {
                    let d = cx.helper("graphix_arcstr_drop")?;
                    cx.b.ins().call(d, &[cv.payload]);
                    Ok(())
                })?;
            } else {
                emit_flag_not_fresh(cx, cv.disc)?;
            }
            drop_owned_composites(cx.b, cx.env, cx.ctx)?;
            cx.b.ins().return_(&[cv.payload]);
        }
        Some(AbiKind::Scalar(_)) => {
            if cx.ctx.gate_stale_at_return {
                // FORCE — a not-fresh scalar (bottom OR didn't fire this
                // cycle) bottoms; else return. A scalar owns nothing, so
                // the drop is a no-op. Folds to an unconditional return
                // when the disc is a fresh const.
                emit_force(cx, cv.disc, |_| Ok(()))?;
            } else {
                emit_flag_not_fresh(cx, cv.disc)?;
            }
            drop_owned_composites(cx.b, cx.env, cx.ctx)?;
            cx.b.ins().return_(&[cv.payload]);
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

/// The kernel-local name a scalar `Ref` resolves to in [`JitEnv`]. The
/// region's free-var inputs are bound as kernel params under the
/// binding's unqualified name (see `collect_region_inputs` →
/// `FreeVarInput.name` and `compile_into_function`'s entry binder), and
/// block-lets bind under their let name; both are the last component of
/// the Ref's `ModPath`. Mirrors `emit_node`'s Ref arm.
fn ref_local_name(spec: &Expr) -> Option<&str> {
    let name = match &spec.kind {
        ExprKind::Ref { name } => name,
        _ => return None,
    };
    let s: &str = name.0.as_ref();
    Some(netidx::path::Path::basename(s).unwrap_or(s))
}

// ─── Distributed node-emission relays ─────────────────────────────
//
// The per-node `Update::emit_clif` impls (node/*.rs) are thin shims
// over these crate-internal helpers, which own the CLIF mechanics
// (`JitEnv`/`LowerCtx` stay private to this module).

/// Constant literal, dispatched on its runtime shape:
///
/// - Scalar: inline `iconst`/`f64const`.
/// - String: stable interned `*const ArcStr` + refcount bump via
///   `graphix_arcstr_clone_from_static` → an OWNED ArcStr word.
/// - Value-shape (datetime/duration/bytes/map literals): stable
///   interned `*const Value` + `graphix_value_clone_from_static` →
///   an OWNED two-word Value.
pub(crate) fn emit_const_node(
    cx: &mut BodyCx,
    value: &Value,
    typ: &Type,
) -> Result<CompiledExpr> {
    // A literal fires only at init, then carries a cached (stale) value
    // (see `const_stale_gate`). The disc gate is the only flag change —
    // a constant is never tainted.
    let init = cx.init_flag();
    match kernel_abi::abi_kind(cx.registry(), typ) {
        Some(AbiKind::Scalar(prim)) => {
            let disc = scalar_disc(cx.b, prim);
            let disc = const_stale_gate(cx.b, init, disc);
            Ok(CompiledExpr::new(disc, compile_const(cx.b, value, prim)?))
        }
        Some(AbiKind::String) => {
            let s = match value {
                Value::String(s) => s,
                v => {
                    return Err(anyhow!("emit_clif: String-typed Constant holds {v:?}"));
                }
            };
            let ptr = cx.interned_str(s);
            let clone = cx.helper("graphix_arcstr_clone_from_static")?;
            let call = cx.b.ins().call(clone, &[ptr]);
            let payload = cx.b.inst_results(call)[0];
            let disc = cx.b.ins().iconst(types::I64, value_disc::STRING);
            let disc = const_stale_gate(cx.b, init, disc);
            Ok(CompiledExpr::new(disc, payload))
        }
        Some(AbiKind::Value) => {
            let ptr = cx.interned_value(value);
            let clone = cx.helper("graphix_value_clone_from_static")?;
            let call = cx.b.ins().call(clone, &[ptr]);
            let (r0, r1) = {
                let r = cx.b.inst_results(call);
                (r[0], r[1])
            };
            let disc = const_stale_gate(cx.b, init, r0);
            Ok(CompiledExpr::new(disc, r1))
        }
        other => {
            Err(anyhow!("emit_clif: Constant of shape {other:?} — not yet supported"))
        }
    }
}

/// A `{k => v, ...}` map literal. Mirrors the classic path's
/// `emit_map_new`: fuses ONLY when every key and value is a
/// compile-time constant — the `CMap` is built at compile time
/// (`insert_cow` in entry order, exactly as `Map::update` does at
/// runtime) and emitted as an interned Value constant. A dynamic
/// entry de-fuses; the runtime map producer isn't lowered on either
/// path.
pub(crate) fn emit_map_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    keys: &[Cached<R, E>],
    vals: &[Cached<R, E>],
    typ: &Type,
) -> Result<CompiledExpr> {
    let v = lowering::const_map(keys, vals).ok_or_else(|| {
        anyhow!(
            "emit_clif: map literal with non-constant entries — \
             subtree node-walks"
        )
    })?;
    let typ = kernel_abi::freeze_for_abi(cx.registry(), typ)
        .unwrap_or_else(kernel_abi::map_type);
    emit_const_node(cx, &v, &typ)
}

/// A binding read. Resolve the Ref's source name to the kernel param
/// / block-let slot in the env (same name the params were bound under
/// — see `compile_into_function`'s entry binder). Surfaces the local's
/// disc (carrying any `TAINT`/`STALE`) alongside its payload.
pub(crate) fn emit_ref_node(
    cx: &mut BodyCx,
    spec: &Expr,
    typ: &Type,
    id: BindId,
) -> Result<CompiledExpr> {
    // Resolve BindId-first (exact under shadowing — an outer capture
    // and an inner let sharing a basename resolve to different slots,
    // the #162/#167 bug class), then by name (id-less synthetic
    // locals). The disc already carries the binding's #219 taint (a
    // tainted param disc from the ABI, or a let's computed disc).
    let _ = typ;
    let name = ref_local_name(spec)
        .ok_or_else(|| anyhow!("emit_clif: Ref spec isn't an ExprKind::Ref"))?;
    let (vv, kind) = {
        let l = cx
            .env
            .lookup(id, name)
            .ok_or_else(|| anyhow!("emit_clif: undefined local `{name}`"))?;
        (l.vv, l.kind)
    };
    let disc = cx.b.use_var(vv.disc);
    match kind {
        // String: read the slot and refcount-bump — each consumer gets
        // an independently-owned ArcStr; the slot keeps its own ref
        // until scope exit.
        LocalKind::String => {
            let s = cx.b.use_var(vv.payload);
            let clone = cx.helper("graphix_arcstr_clone")?;
            let call = cx.b.ins().call(clone, &[s]);
            Ok(CompiledExpr::new(disc, cx.b.inst_results(call)[0]))
        }
        // Scalar: read both words. Composite / Variant / Nullable /
        // Value: BORROWED read — the env still owns the slot; consumers
        // clone via `ensure_owned_*_src` when they need ownership.
        LocalKind::Scalar(_)
        | LocalKind::Composite
        | LocalKind::Variant
        | LocalKind::Nullable
        | LocalKind::Value => Ok(CompiledExpr::new(disc, cx.b.use_var(vv.payload))),
    }
}

/// Arithmetic — compile both operands, then the shared `compile_bin`
/// helper (including the div/mod taint/guard), propagating operand
/// validity. A datetime/duration operand
/// routes to the `ValueArith` mirror first (netidx `Value` arithmetic
/// via the `graphix_value_<op>` helpers, both operands OWNED since the
/// helpers consume).
pub(crate) fn emit_arith_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    op: BinOp,
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> Result<CompiledExpr> {
    if lowering::is_datetime_or_duration(lhs.typ())
        || lowering::is_datetime_or_duration(rhs.typ())
    {
        let lcv = emit_owned_value_operand_node(cx, lhs)?;
        let rcv = emit_owned_value_operand_node(cx, rhs)?;
        let helper = match op {
            BinOp::Add => "graphix_value_add",
            BinOp::Sub => "graphix_value_sub",
            BinOp::Mul => "graphix_value_mul",
            BinOp::Div => "graphix_value_div",
            BinOp::Mod => "graphix_value_rem",
        };
        let fref = cx.helper(helper)?;
        // Clean the operand discs before the netidx-Value helper (a
        // tainted disc is an invalid tag); the helper ran on the value
        // bits regardless. #219: the result disc re-absorbs the operands'
        // taint, guarding a garbage result a missing input produced.

        let call = cx.b.ins().call(fref, &[lcv.disc, lcv.payload, rcv.disc, rcv.payload]);
        let (rdisc, rpay) = {
            let r = cx.b.inst_results(call);
            (r[0], r[1])
        };
        let disc = propagate_flags(cx.b, rdisc, &[lcv.disc, rcv.disc]);
        return Ok(CompiledExpr::new(disc, rpay));
    }
    let lcv = lhs.emit_clif(cx)?;
    let rcv = rhs.emit_clif(cx)?;
    let l = lcv.payload;
    let r = rcv.payload;
    // Fallible prim derivation (not `prim_of`, which panics): an
    // operand's type may be typecheck's un-normalized union (a select
    // result) — scalar after `freeze_for_abi_normalized` (with the abstract-
    // Ref resolution retry, #218), or Err → no fusion.
    let prim = freeze_node_typ(cx.ctx, lhs.typ())
        .as_ref()
        .and_then(|t| kernel_abi::scalar_prim(cx.registry(), t))
        .ok_or_else(|| {
            anyhow!("emit_clif: arith operand of non-scalar type {:?}", lhs.typ())
        })?;
    let base = scalar_disc(cx.b, prim);
    // Integer div/mod taint/guard.
    if matches!(op, BinOp::Div | BinOp::Mod)
        && prim.is_integer()
        && node_int_div_may_bottom(lhs, rhs)
    {
        let is_zero = cx.b.ins().icmp_imm(IntCC::Equal, r, 0);
        let bad = if prim.is_signed() {
            let min: i64 = match prim {
                PrimType::I8 => i8::MIN as i64,
                PrimType::I16 => i16::MIN as i64,
                PrimType::I32 => i32::MIN as i64,
                _ => i64::MIN,
            };
            let is_min = cx.b.ins().icmp_imm(IntCC::Equal, l, min);
            let is_neg1 = cx.b.ins().icmp_imm(IntCC::Equal, r, -1);
            let overflow = cx.b.ins().band(is_min, is_neg1);
            cx.b.ins().bor(is_zero, overflow)
        } else {
            is_zero
        };
        let one = cx.b.ins().iconst(prim_to_clif(prim), 1);
        let safe_r = cx.b.ins().select(bad, one, r);
        let value = compile_bin(cx.b, op, prim, l, safe_r)?;
        // #219: a div0 / signed-MIN÷-1 taints the result; so does a
        // tainted operand. `is_tainted` resolves at the output.
        let disc = propagate_flags(cx.b, base, &[lcv.disc, rcv.disc]);
        let taint_word = cx.b.ins().iconst(types::I64, TAINT);
        let zero = cx.b.ins().iconst(types::I64, 0);
        let bad_taint = cx.b.ins().select(bad, taint_word, zero);
        let disc = cx.b.ins().bor(disc, bad_taint);
        // Interior-bottom exactness: with prior history this degrades a
        // tainted result to STALE + the cached value, matching the
        // node-walk's cached div node.
        return Ok(emit_scalar_taint_cache(cx, prim, CompiledExpr::new(disc, value)));
    }
    let value = compile_bin(cx.b, op, prim, l, r)?;
    let disc = propagate_flags(cx.b, base, &[lcv.disc, rcv.disc]);
    Ok(CompiledExpr::new(disc, value))
}

/// Checked arithmetic (`+?` / `-?` / `*?` / `/?` / `%?`). Both
/// operands are compiled as OWNED `(disc, payload)` Values (the same
/// route as `emit_arith_node`'s ValueArith dispatch — the helpers
/// consume), then the `graphix_value_checked_<op>` helper computes via
/// the SAME netidx `Value::checked_*` + [`op::
/// wrap_arith_error`] core the node-walk's update uses. The result is
/// a Value: the success scalar, or the catchable `ArithError` error
/// VALUE (`[T, Error<`ArithError(string)>]` freezes to the Nullable
/// wire shape) — never bottom, unlike unchecked div0.
pub(crate) fn emit_checked_arith_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    op: BinOp,
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> Result<CompiledExpr> {
    let lcv = emit_owned_value_operand_node(cx, lhs)?;
    let rcv = emit_owned_value_operand_node(cx, rhs)?;
    let helper = match op {
        BinOp::Add => "graphix_value_checked_add",
        BinOp::Sub => "graphix_value_checked_sub",
        BinOp::Mul => "graphix_value_checked_mul",
        BinOp::Div => "graphix_value_checked_div",
        BinOp::Mod => "graphix_value_checked_rem",
    };
    let fref = cx.helper(helper)?;

    let call = cx.b.ins().call(fref, &[lcv.disc, lcv.payload, rcv.disc, rcv.payload]);
    let (rdisc, rpay) = {
        let r = cx.b.inst_results(call);
        (r[0], r[1])
    };
    // #219: propagate operand taint into the checked-arith result Value.
    let disc = propagate_flags(cx.b, rdisc, &[lcv.disc, rcv.disc]);
    Ok(CompiledExpr::new(disc, rpay))
}

/// Comparison — `compile_cmp` (total-order floats) on scalar operands,
/// propagating operand validity. Non-scalar `==`/`!=` (String,
/// composite, value-shape) compiles both operands as OWNED
/// `(disc, payload)` Values (the helper consumes them),
/// compared via netidx `Value` PartialEq. Ordering operators on
/// non-scalar operands aren't lowered (mirrors `kernel_abi::cmp`) — Err, the
/// region node-walks.
pub(crate) fn emit_cmp_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    op: CmpOp,
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> Result<CompiledExpr> {
    let lprim = kernel_abi::freeze_for_abi_normalized(cx.registry(), lhs.typ())
        .as_ref()
        .and_then(|t| kernel_abi::scalar_prim(cx.registry(), t));
    let rprim = kernel_abi::freeze_for_abi_normalized(cx.registry(), rhs.typ())
        .as_ref()
        .and_then(|t| kernel_abi::scalar_prim(cx.registry(), t));
    if let (Some(lp), Some(_)) = (lprim, rprim) {
        let lcv = lhs.emit_clif(cx)?;
        let rcv = rhs.emit_clif(cx)?;
        let value = compile_cmp(cx.b, op, lp, lcv.payload, rcv.payload);
        let base = scalar_disc(cx.b, PrimType::Bool);
        let disc = propagate_flags(cx.b, base, &[lcv.disc, rcv.disc]);
        return Ok(CompiledExpr::new(disc, value));
    }
    let ne = match op {
        CmpOp::Eq => false,
        CmpOp::Ne => true,
        other => {
            return Err(anyhow!(
                "emit_clif: ordering cmp {other:?} on non-scalar operands \
                 — not lowered (mirrors kernel_abi::cmp)"
            ));
        }
    };
    for t in [lhs.typ(), rhs.typ()] {
        if matches!(
            kernel_abi::abi_kind(cx.registry(), t),
            Some(AbiKind::Unit | AbiKind::Null) | None
        ) {
            return Err(anyhow!(
                "emit_clif: ==/!= operand of type {t:?} has no comparable \
                 runtime form (mirrors kernel_abi::cmp)"
            ));
        }
    }
    let lcv = emit_owned_value_operand_node(cx, lhs)?;
    let rcv = emit_owned_value_operand_node(cx, rhs)?;
    let helper = cx.helper("graphix_value_eq")?;

    let call = cx.b.ins().call(helper, &[lcv.disc, lcv.payload, rcv.disc, rcv.payload]);
    let eq = cx.b.inst_results(call)[0]; // I8 bool
    let result = if ne {
        let one = cx.b.ins().iconst(types::I8, 1);
        cx.b.ins().bxor(eq, one)
    } else {
        eq
    };
    // #219: a tainted operand taints the bool result.
    let base = scalar_disc(cx.b, PrimType::Bool);
    let disc = propagate_flags(cx.b, base, &[lcv.disc, rcv.disc]);
    Ok(CompiledExpr::new(disc, result))
}

/// Logical — STRICT `band`/`bor` (both operands always compiled),
/// taint-propagating, matching the node-walk's `bool_op!`.
pub(crate) fn emit_bool_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    op: BoolOp,
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> Result<CompiledExpr> {
    let lcv = lhs.emit_clif(cx)?;
    let rcv = rhs.emit_clif(cx)?;
    let value = match op {
        BoolOp::And => cx.b.ins().band(lcv.payload, rcv.payload),
        BoolOp::Or => cx.b.ins().bor(lcv.payload, rcv.payload),
    };
    let base = scalar_disc(cx.b, PrimType::Bool);
    let disc = propagate_flags(cx.b, base, &[lcv.disc, rcv.disc]);
    Ok(CompiledExpr::new(disc, value))
}

/// Logical NOT — taint-propagating.
pub(crate) fn emit_not_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    inner: &Node<R, E>,
) -> Result<CompiledExpr> {
    let cv = inner.emit_clif(cx)?;
    let one = cx.b.ins().iconst(types::I8, 1);
    let value = cx.b.ins().bxor(cv.payload, one);
    let base = scalar_disc(cx.b, PrimType::Bool);
    let disc = propagate_flags(cx.b, base, &[cv.disc]);
    Ok(CompiledExpr::new(disc, value))
}

/// `-x` — integer `ineg` / float `fneg`, taint-propagating. The operand's
/// PrimType drives int-vs-float; a non-register-scalar operand (e.g.
/// `decimal`) has no `scalar_prim` and Errs, de-fusing to the node-walk.
pub(crate) fn emit_neg_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    inner: &Node<R, E>,
) -> Result<CompiledExpr> {
    let cv = inner.emit_clif(cx)?;
    let prim = freeze_node_typ(cx.ctx, inner.typ())
        .as_ref()
        .and_then(|t| kernel_abi::scalar_prim(cx.registry(), t))
        .ok_or_else(|| {
            anyhow!("emit_neg: operand of non-scalar type {:?}", inner.typ())
        })?;
    let value = if prim.is_integer() {
        cx.b.ins().ineg(cv.payload)
    } else {
        cx.b.ins().fneg(cv.payload)
    };
    let base = scalar_disc(cx.b, prim);
    let disc = propagate_flags(cx.b, base, &[cv.disc]);
    Ok(CompiledExpr::new(disc, value))
}

/// `cast<T>(x)` — `compile_cast`, taint-propagating.
pub(crate) fn emit_cast_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    inner: &Node<R, E>,
    target: &Type,
    expr_id: ExprId,
) -> Result<CompiledExpr> {
    // Scalar→scalar fast path: pure register arithmetic, branchless,
    // infallible — a cast in a hot loop (e.g. `cast<f64>(2*n-1)`) must
    // stay inline, not pay a runtime call per iteration. Restricted to
    // NUMERIC prims: `compile_cast` can't lower a `bool` cast (it
    // `unreachable!`s), so a bool source/target falls through to the
    // machinery DynCall below, which casts via `Value::cast`.
    if let (Some(src), Some(tgt)) =
        (kernel_abi::scalar_prim(cx.registry(), inner.typ()), PrimType::from_type(target))
    {
        if src.is_numeric() && tgt.is_numeric() {
            let cv = inner.emit_clif(cx)?;
            let value = compile_cast(cx.b, cv.payload, src, tgt);
            let base = scalar_disc(cx.b, tgt);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            return Ok(CompiledExpr::new(disc, value));
        }
    }
    // Any other cast (non-scalar source like `datetime`, or non-scalar
    // target): the discovery pass registered a `FnSource::Cast` slot —
    // lower it as a one-argument DynCall to the cast machinery
    // (`CastApply` → `target.cast_value`, the SAME fn the node-walk
    // uses). The fallible `[T, Error]` result rides the 2-word Value
    // wire shape; a surrounding `$`/`?` unwraps it via `emit_qop_node`.
    let info = match cx.builtin_site(expr_id) {
        Some(i) => i.clone(),
        None => {
            return Err(anyhow!(
                "emit_clif: cast site {expr_id:?} not discovered — doesn't fuse"
            ));
        }
    };
    emit_dyncall_node(cx, &info, &[inner])
}

/// `connect` (`x <- expr`) fused: compute the RHS and write the reactive
/// variable mid-kernel via `graphix_set_var`. The write is a side
/// effect (the node returns bottom — `Connect::update` returns `None`),
/// and the read side is unchanged: a downstream region reading the
/// written variable already sees it next cycle through its feeder Ref
/// (keyed to `fusion.top_id`). A `#219`-tainted RHS is skipped by the
/// helper (no value this cycle = no write), mirroring the node-walk's
/// `if let Some(v) = ..` guard.
///
/// The RHS is marshaled to an OWNED `(disc, payload)` Value of any shape
/// (`emit_owned_value_operand_node`) and handed to `graphix_set_var`, which
/// CONSUMES it — or drops it on a tainted/stale skip. So a composite/string
/// RHS (`data <- array::push(data, x)`) fuses without a leak, uniform with the
/// scalar case.
pub(crate) fn emit_connect_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    rhs: &Node<R, E>,
    bind_id: BindId,
) -> Result<CompiledExpr> {
    // Read-after-write-same-var guard: if the connect TARGET is a
    // kernel-local (let-bound inside this region) that was NOT lifted, a
    // read of it in the same kernel resolves to the stale local value,
    // not the written one — de-fuse (the block node-walks, correct). A
    // LIFTED target is the local-counter case: it was routed in as a
    // feeder and `emit_let_node` bound it to a seed-select reading that
    // feeder, so a read DOES see the variable's value. The fired-bit
    // makes the write correct (`set_var_typed` skips a non-fired RHS), so
    // the lifted connect fuses. A connect to an EXTERNAL variable (not in
    // the env at all) also fuses.
    if cx.env.lookup(bind_id, "").is_some() && !cx.is_lifted(bind_id) {
        return Err(anyhow!(
            "emit_clif: connect target is a non-lifted kernel-local — \
             read-after-write unsafe, node-walks"
        ));
    }
    // Marshal the RHS to an OWNED `(disc, payload)` Value of any shape; a
    // scalar's `(disc, payload)` is already its Value wire form (payload
    // widened to i64). `set_var` consumes the payload (or drops it on the
    // tainted/stale skip), so a composite/string RHS transfers ownership
    // without leaking; #219 taint / STALE ride `cv.disc` and the helper honors
    // them (skip the write).
    let cv = emit_owned_value_operand_node(cx, rhs)?;
    let id_const = cx.b.ins().iconst(types::I64, bind_id.inner() as i64);
    let set_var = cx.helper("graphix_set_var")?;
    cx.b.ins().call(set_var, &[id_const, cv.disc, cv.payload]);
    // `connect` produces no value — a tainted-null bottom. Discarded as a
    // block statement (a connect is never a kernel's published result;
    // its `typ()` is `Bottom`).
    let disc = cx.b.ins().iconst(types::I64, value_disc::NULL | TAINT);
    let zero = cx.b.ins().iconst(types::I64, 0);
    Ok(CompiledExpr::new(disc, zero))
}

/// String interpolation `"x is [x]"` — build a heap-owned
/// `*mut String`,
/// push each part (append-as-str for string parts — reads are already
/// owned clones, the push consumes; Display-rendered for scalars via
/// the shared [`string_buf_push_helper`]), finalize into an OWNED
/// ArcStr. Keeps the restriction on part shapes: a non-scalar /
/// non-string part (a Nullable from `a[i]`, a composite, a value-shape
/// — see findings "StringInterpolate non-scalar part") is Err, the
/// subtree node-walks.
pub(crate) fn emit_string_interpolate_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    args: &[Cached<R, E>],
) -> Result<CompiledExpr> {
    let new_buf = cx.helper("graphix_string_buf_new")?;
    let call = cx.b.ins().call(new_buf, &[]);
    let buf = cx.b.inst_results(call)[0];
    // #219: a tainted part (a div0 inside `[..]`) renders harmlessly into
    // the buffer; its taint accumulates and forces the result at the
    // output. Collect part discs and fold them into the result disc.
    let mut part_discs: Vec<ClifValue> = Vec::with_capacity(args.len());
    for a in args {
        let part = &a.node;
        // `freeze_for_abi_normalized` so a select-valued part (whose type is
        // the un-normalized arm union) still classifies.
        let frozen = kernel_abi::freeze_for_abi_normalized(cx.registry(), part.typ());
        match frozen.as_ref().and_then(|t| kernel_abi::abi_kind(cx.registry(), t)) {
            Some(AbiKind::String) => {
                let cv = part.emit_clif(cx)?;
                part_discs.push(cv.disc);
                let push = cx.helper("graphix_string_buf_push_arcstr")?;
                cx.b.ins().call(push, &[buf, cv.payload]);
            }
            Some(AbiKind::Scalar(p)) => {
                let cv = part.emit_clif(cx)?;
                part_discs.push(cv.disc);
                let push = cx.helper(string_buf_push_helper(p))?;
                cx.b.ins().call(push, &[buf, cv.payload]);
            }
            other => {
                return Err(anyhow!(
                    "emit_clif: string-interpolate part of shape {other:?} \
                     — only String and scalar parts are lowered"
                ));
            }
        }
    }
    let finalize = cx.helper("graphix_string_buf_finalize")?;
    let call = cx.b.ins().call(finalize, &[buf]);
    let payload = cx.b.inst_results(call)[0];
    let base = cx.b.ins().iconst(types::I64, value_disc::STRING);
    let disc = propagate_flags(cx.b, base, &part_discs);
    Ok(CompiledExpr::new(disc, payload))
}

/// A `do` / `{ ... }` block — let prefix then a tail expression.
/// Mirrors `compile_block_scalar`/`_value`: bind each let by its
/// runtime shape (cloning borrowed composite/value sources so the
/// block exclusively owns its locals), compile the tail, clone the
/// tail out if it borrows a local we're about to drop, emit the
/// scope-exit drops for this block's owned locals, then pop the
/// block-scoped env entries.
/// Conservative effect-freedom for dead-statement elimination: TRUE
/// only when no node in the subtree could carry an effect the
/// node-walk would have performed. Connect/ConnectDeref write
/// variables; a `?` WITH a catch handler writes the handler's
/// variable; a CallSite may target an async/effectful builtin (we
/// can't consult `builtin_effects` at emit time, so ALL call sites
/// are conservatively effectful). Handler-less `?` and `$` only emit
/// the swallowed-error diagnostics, which are node-walk-only by
/// design (a fused kernel drops them even when live) — and treating
/// them as effects is actively WRONG here: their non-scalar error
/// paths abort the whole kernel, so emitting a dead `m{k}$` bind
/// wrong-bottoms the region (soak finding
/// corpus-generate/divergence_000000, 2026-07-03). Everything else
/// the direct emitter can encounter is value-only.
fn stmt_subtree_effect_free<R: Rt, E: UserEvent>(node: &Node<R, E>) -> bool {
    let mut ok = true;
    fusion::for_each_node(node, &mut |n| match n.view() {
        NodeView::Connect(_) | NodeView::ConnectDeref(_) | NodeView::CallSite(_) => {
            ok = false
        }
        NodeView::Qop(q) => {
            if q.id.is_some() {
                ok = false
            }
        }
        NodeView::OrNever(_) => {}
        _ => {}
    });
    ok
}

pub(crate) fn emit_block_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    children: &[Node<R, E>],
) -> Result<CompiledExpr> {
    if children.is_empty() {
        return Err(anyhow!("emit_clif: empty block"));
    }
    let mark = cx.env.mark();
    let last = children.len() - 1;
    for (i, child) in children.iter().enumerate() {
        if i == last {
            let tail_cv = child.emit_clif(cx)?;
            // The tail may alias a block-scoped local we're about to
            // drop — clone borrowed results so they outlive the block.
            // Taint rides in the disc through the clone.
            let src = node_composite_source(child);
            let result = match kernel_abi::abi_kind(cx.registry(), child.typ()) {
                Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                    let v = ensure_owned_composite_src(cx, src, tail_cv.payload)?;
                    CompiledExpr::new(tail_cv.disc, v)
                }
                Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
                    let (disc, payload) =
                        ensure_owned_value_src(cx, src, tail_cv.disc, tail_cv.payload)?;
                    CompiledExpr::new(disc, payload)
                }
                // Scalars need no clone; a String read is already an
                // owned clone (the Ref/Const arms bump the refcount).
                _ => tail_cv,
            };
            emit_scope_drops(cx, mark)?;
            cx.env.truncate(mark);
            return Ok(result);
        }
        // Dead-statement elimination — the classic prune pass's
        // semantics at the emission seam. A non-tail statement whose
        // value nobody reads AND whose subtree is provably effect-free
        // contributes NOTHING canonical: its node-walk value (or
        // bottom) flows to no consumer. Emitting it anyway is worse
        // than useless — a dead bottom (div0 inside a discarded
        // tuple, an unused let holding an aborting array literal)
        // poisons the WHOLE kernel via the composite producers'
        // bottom-abort, a value divergence vs the node-walk
        // (findings/flip-jun2026). A Bind is dead iff no LATER
        // sibling or the tail references a bound id; a bare
        // expression statement's value is always unread.
        //
        // The effect-free gate is LOAD-BEARING: a statement whose
        // subtree contains a Connect (`<-`), handler-ful `?`, `$`
        // (logs), or ANY CallSite (a builtin may be async/effectful)
        // must NOT be skipped — skipping converts "emission fails →
        // region de-fuses → node-walk runs the effect" into silently
        // DROPPING the effect (the env-accounting probe caught
        // exactly that with a skipped `counter <- v`). Those emit
        // normally and de-fuse via their emit Errs as before.
        let dead = if matches!(child.view(), NodeView::Bind(_)) {
            let mut bound = Refs::default();
            child.refs(&mut bound);
            let mut suffix = Refs::default();
            for later in &children[i + 1..] {
                later.refs(&mut suffix);
            }
            let mut alive = false;
            bound.with_bound(|id| {
                if suffix.is_refed(id) {
                    alive = true;
                }
            });
            !alive
        } else {
            true
        };
        if dead && stmt_subtree_effect_free(child) {
            continue;
        }
        emit_block_stmt(cx, child)?;
    }
    // The loop always returns on the last child.
    unreachable!("emit_block_node: last child not handled")
}

/// Emit one non-last block child as a statement: a `let` binds into
/// the env, compile-time-only declarations are skipped, anything else
/// evaluates and discards. Shared by the value-position block emitter
/// and the tail-position walk (`emit_body_tail`).
fn emit_block_stmt<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    child: &Node<R, E>,
) -> Result<()> {
    use NodeView;
    match child.view() {
        NodeView::Bind(bind) => {
            let bspec = match &bind.spec.kind {
                ExprKind::Bind(be) => be,
                _ => {
                    return Err(anyhow!(
                        "emit_clif: Bind node spec isn't ExprKind::Bind"
                    ));
                }
            };
            // A rec FN binding is just a function-valued let (the
            // binding node-walks, call sites fuse as cross-kernel
            // calls — recursion included); `emit_let_node`'s
            // fallthrough produces that message. A rec NON-fn let
            // (`let rec x = x + 1` — a reactive feedback loop) is
            // genuinely unfusable: its value depends on its own
            // previous cycle.
            if bspec.rec && !matches!(bind.node.typ(), Type::Fn(_)) {
                return Err(anyhow!(
                    "emit_clif: recursive non-function let not supported"
                ));
            }
            let name = bspec.pattern.single_bind().ok_or_else(|| {
                anyhow!("emit_clif: non-single-bind let pattern not supported")
            })?;
            let bind_id = bind.single_bind_id();
            emit_let_node(cx, name, bind_id, &bind.node)?;
        }
        // Compile-time-only declarations — skip (mirrors emit_do).
        NodeView::Nop(_) | NodeView::TypeDef(_) | NodeView::Use(_) => {}
        // Expression statement — evaluate, discard the result. A
        // discarded may-bottom scalar is fine — the bottom is never
        // consumed. Owned non-scalar results are dropped: discarding is
        // consuming. This includes a CALL in statement position: a
        // no-value fire is a #219 tainted placeholder now, so
        // discarding it discards the bottom exactly like the
        // node-walk (`{println("hi"); 100}` used to pending-abort the
        // kernel and lose the 100 — soak finding 2026-07-04, item 28).
        // Effects that can't emit (println's bare-Null return) still
        // Err out of `emit_dyncall_node` and de-fuse the region —
        // effects de-fuse, never silently skip.
        _ => {
            let cv = child.emit_clif(cx)?;
            emit_discard_result(cx, child, cv)?;
        }
    }
    Ok(())
}

/// Tail-position body emission for a self-recursive kernel — the Node
/// twin of lowering's `emit_body_into`/`emit_tail`. Tail positions are
/// the body root, a Block's LAST child, Select arm bodies, and
/// ExplicitParens; a self-call in one becomes the rebind-and-jump loop
/// (`emit_tail_rebind_jump`), every other expression returns directly
/// (`emit_kernel_return`, which drops ALL owned locals at any depth —
/// nested-scope returns can't leak). Every path through this function
/// leaves the current block TERMINATED.
fn emit_body_tail<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    node: &Node<R, E>,
    ret: &Type,
) -> Result<()> {
    use NodeView;
    // Self tail-call — checked BEFORE value emission, mirroring
    // lowering's `emit_tail` → `try_emit_tail_call` order. Matching is
    // by the self BindId (names shadow, ids don't — #206).
    if let Some((sb, _)) = cx.ctx.self_call {
        if let NodeView::CallSite(cs) = node.view() {
            if matches!(cs.fnode().view(), NodeView::Ref(r) if r.id == *sb) {
                return emit_self_tail_call(cx, cs);
            }
        }
    }
    match node.view() {
        NodeView::Block(blk) => {
            let mark = cx.env.mark();
            let (last, init) = blk
                .children
                .split_last()
                .ok_or_else(|| anyhow!("emit_clif: empty block in tail position"))?;
            for child in init {
                emit_block_stmt(cx, child)?;
            }
            emit_body_tail(cx, last, ret)?;
            // Every path through the tail terminated — returns dropped
            // all owned locals, tail-jumps dropped the non-slot locals
            // (and already truncated to the param mark, making this a
            // no-op). Pop the compile-time scope.
            cx.env.truncate(mark);
            Ok(())
        }
        NodeView::ExplicitParens(ep) => emit_body_tail(cx, &ep.n, ret),
        NodeView::Select(s) => emit_select_node_tail(cx, s, ret),
        _ => emit_return_from_node(cx, ret, node),
    }
}

/// Tail-position select: the shared pattern chain with arms that
/// TERMINATE (return or self tail-call jump) instead of widening to a
/// merge block.
fn emit_select_node_tail<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    sel: &Select<R, E>,
    ret: &Type,
) -> Result<()> {
    if sel.arms.is_empty() {
        return Err(anyhow!("emit_clif: select with no arms"));
    }
    let (scrut, scrut_kind, scrut_typ, _none) =
        classify_select_scrutinee(cx, sel, false)?;
    // Tail position: FORCE the scrutinee up front — a missing scrutinee
    // bottoms the whole kernel (the tail select IS the kernel result, so
    // there's no dead-select hazard, unlike value position). The arms
    // then run on a known-valid scrutinee, so the final-arm miss is
    // unreachable. `is_untainted` folds to const-true for an untainted
    // scrutinee, emitting no branch (#219).
    let valid = is_untainted(cx.b, scrut.disc());
    emit_bottom_abort(cx.b, cx.env, cx.ctx, valid)?;
    // Fold this scrutinee's firing into the kernel's tail-firing
    // accumulator (see `LowerCtx::tail_scrut_stale`): the arms
    // terminate individually, so the return path is where the
    // scrutinee's control-dependence firing gets applied. band keeps
    // a cleared (fired) bit cleared across loop iterations.
    {
        let cur = cx.b.use_var(cx.ctx.tail_scrut_stale);
        let ss = cx.b.ins().band_imm(scrut.disc(), STALE);
        let n = cx.b.ins().band(cur, ss);
        cx.b.def_var(cx.ctx.tail_scrut_stale, n);
    }
    emit_select_arms(
        cx,
        sel,
        scrut,
        scrut_kind,
        &scrut_typ,
        &mut |cx, body, mark| {
            emit_body_tail(cx, &body.node, ret)?;
            // The arm terminated; pop its binds for the next arm's
            // compile-time scope. (Arm binds are scalars — no drops.)
            cx.env.truncate(mark);
            Ok(())
        },
        // Unreachable (scrutinee forced valid → exhaustive matches); a
        // terminator is still required.
        &mut |cx| emit_kernel_bottom(cx),
    )
}

/// A self-call in tail position: evaluate the new formal values,
/// rebind the leading tail-call slots via `emit_tail_rebind_jump`,
/// and jump to the loop head.
fn emit_self_tail_call<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    cs: &CallSite<R, E>,
) -> Result<()> {
    let spec_apply = match &cs.spec().kind {
        ExprKind::Apply(a) => a,
        _ => {
            return Err(anyhow!("emit_clif: self tail-call spec isn't an Apply"));
        }
    };
    // Labeled args would need default materialization in source order;
    // de-fuse.
    if spec_apply.args.iter().any(|(label, _)| label.is_some()) {
        return Err(anyhow!("emit_clif: labeled args on a self tail-call"));
    }
    let n = spec_apply.args.len();
    let mut new_vals = Vec::with_capacity(n);
    let mut sources = Vec::with_capacity(n);
    for i in 0..n {
        let arg = cs
            .arg_positional(i)
            .ok_or_else(|| anyhow!("emit_clif: self tail-call arg {i} missing"))?;
        let cv = arg.emit_clif(cx)?;
        // A possibly-bottom arg: no value this cycle means no call —
        // kernel-wide bottom (the node-walk's CallSite wouldn't fire).
        // `is_untainted` folds to const-true for an untainted disc, so a
        // normal arg emits no branch. The rebind protocol covers
        // scalar/composite slots — the full (disc, payload) pair; a
        // value-shape formal is refused by the tail-loop gate at build.
        let valid = is_untainted(cx.b, cv.disc);
        emit_bottom_abort(cx.b, cx.env, cx.ctx, valid)?;
        new_vals.push(cv);
        sources.push(node_composite_source(arg));
    }
    emit_tail_rebind_jump(cx.b, cx.env, cx.ctx, new_vals, &sources)
}

/// Bind one `let local = value` into the env by the value's runtime
/// shape — the direct-path mirror of `compile_block_scalar`'s let
/// arms (composite/value lets clone borrowed sources so this scope
/// exclusively owns them; the scope-exit drop would otherwise free a
/// buffer the enclosing scope still holds).
fn emit_let_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    name: &ArcStr,
    bind_id: Option<BindId>,
    value: &Node<R, E>,
) -> Result<()> {
    // LIFTED connect target (a let-bound scalar counter routed in as a
    // feeder): bind it to a SEED-SELECT — the feeder (entry param) when it
    // has a value (untainted, i.e. ever fired), else the constant SEED
    // (this let's value). Reproduces the node-walk exactly: the variable's
    // value is the last `set_var`'d value, or the one-shot `Bind` seed
    // until the first write. The seed is a STALE-gated constant
    // (`emit_const_node` — fresh at init, stale after), so reads see the
    // node-walk's firing; the feeder carries its own fresh/stale bit. The
    // entry param is the binding the kernel-entry binder installed under
    // this same `bind_id` (the lift added it to `inputs`); this let then
    // SHADOWS it, so post-let reads resolve to the seed-select result.
    if let Some(id) = bind_id {
        if cx.is_lifted(id) {
            let vv = {
                let l = cx.env.lookup(id, name).ok_or_else(|| {
                    anyhow!("emit_clif: lifted target `{name}` has no feeder param")
                })?;
                l.vv
            };
            let pdisc = cx.b.use_var(vv.disc);
            let ppay = cx.b.use_var(vv.payload);
            let valid = is_untainted(cx.b, pdisc);
            let frozen =
                kernel_abi::freeze_for_abi_normalized(cx.registry(), value.typ());
            let ak = frozen.as_ref().and_then(|t| kernel_abi::abi_kind(cx.registry(), t));
            match ak {
                Some(AbiKind::Scalar(p)) => {
                    // Register scalars are branch-free: both sides are
                    // plain values, select the feeder or the seed.
                    let seed = value.emit_clif(cx)?;
                    let disc = cx.b.ins().select(valid, pdisc, seed.disc);
                    let payload = cx.b.ins().select(valid, ppay, seed.payload);
                    bind_local(
                        cx,
                        name.clone(),
                        disc,
                        payload,
                        LocalKind::Scalar(p),
                        Some(id),
                    );
                }
                // A composite / string accumulator (`let data = []; data <-
                // array::push(data, x)`) needs OWNERSHIP on both sides:
                // the feeder path CLONES the entry param (the param local
                // keeps its own allocation — both drop at the return via
                // `drop_owned_composites`), the seed path emits the fresh
                // literal only when taken (no allocation to discard on the
                // other side).
                Some(k @ (AbiKind::Array | AbiKind::Tuple | AbiKind::Struct))
                | Some(k @ AbiKind::String) => {
                    let is_string = matches!(k, AbiKind::String);
                    let use_param = cx.b.create_block();
                    let use_seed = cx.b.create_block();
                    let merge = cx.b.create_block();
                    cx.b.append_block_param(merge, types::I64); // disc
                    cx.b.append_block_param(merge, types::I64); // ptr/bits
                    cx.b.ins().brif(valid, use_param, &[], use_seed, &[]);
                    cx.b.switch_to_block(use_param);
                    cx.b.seal_block(use_param);
                    let clone_helper = if is_string {
                        cx.helper("graphix_arcstr_clone")?
                    } else {
                        cx.helper("graphix_valarray_clone")?
                    };
                    let call = cx.b.ins().call(clone_helper, &[ppay]);
                    let cloned = cx.b.inst_results(call)[0];
                    cx.b.ins()
                        .jump(merge, &[BlockArg::Value(pdisc), BlockArg::Value(cloned)]);
                    cx.b.switch_to_block(use_seed);
                    cx.b.seal_block(use_seed);
                    let seed = value.emit_clif(cx)?;
                    let seed_owned = if is_string {
                        // Strings are owned at production.
                        seed.payload
                    } else {
                        ensure_owned_composite_src(
                            cx,
                            node_composite_source(value),
                            seed.payload,
                        )?
                    };
                    cx.b.ins().jump(
                        merge,
                        &[BlockArg::Value(seed.disc), BlockArg::Value(seed_owned)],
                    );
                    cx.b.switch_to_block(merge);
                    cx.b.seal_block(merge);
                    let (disc, payload) = {
                        let params = cx.b.block_params(merge);
                        (params[0], params[1])
                    };
                    let kind =
                        if is_string { LocalKind::String } else { LocalKind::Composite };
                    bind_local(cx, name.clone(), disc, payload, kind, Some(id));
                }
                other => {
                    return Err(anyhow!(
                        "emit_clif: lifted connect target `{name}` of shape \
                         {other:?} — not yet supported"
                    ));
                }
            }
            return Ok(());
        }
    }
    // `freeze_for_abi_normalized` so a select-valued let (whose type is the
    // un-normalized arm union) still classifies.
    let frozen = kernel_abi::freeze_for_abi_normalized(cx.registry(), value.typ());
    let ak = frozen.as_ref().and_then(|t| kernel_abi::abi_kind(cx.registry(), t));
    match ak {
        Some(AbiKind::Scalar(p)) => {
            // The disc carries the binding's taint — a `let`-bound bottom
            // flows to its uses; an unconsumed one is dropped, never
            // bottoming (#219).
            let cv = value.emit_clif(cx)?;
            bind_local(
                cx,
                name.clone(),
                cv.disc,
                cv.payload,
                LocalKind::Scalar(p),
                bind_id,
            );
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let cv = value.emit_clif(cx)?;
            let owned =
                ensure_owned_composite_src(cx, node_composite_source(value), cv.payload)?;
            bind_local(cx, name.clone(), cv.disc, owned, LocalKind::Composite, bind_id);
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let cv = value.emit_clif(cx)?;
            let (disc, payload) = ensure_owned_value_src(
                cx,
                node_composite_source(value),
                cv.disc,
                cv.payload,
            )?;
            let kind = match ak {
                Some(AbiKind::Variant) => LocalKind::Variant,
                Some(AbiKind::Nullable) => LocalKind::Nullable,
                _ => LocalKind::Value,
            };
            bind_local(cx, name.clone(), disc, payload, kind, bind_id);
        }
        Some(AbiKind::String) => {
            // String reads/consts are already owned clones.
            let cv = value.emit_clif(cx)?;
            bind_local(cx, name.clone(), cv.disc, cv.payload, LocalKind::String, bind_id);
        }
        other => {
            // A function-valued let can NEVER emit by design — a
            // lambda isn't a kernel value; its call sites fuse as
            // cross-kernel calls while the binding itself node-walks
            // (publishing the LambdaDef). Distinct message so probe
            // assertions can treat it as structural recurse noise,
            // not a coverage gap.
            if matches!(value.typ(), Type::Fn(_)) {
                return Err(anyhow!(
                    "emit_clif: function-valued let — the binding \
                     node-walks, call sites fuse"
                ));
            }
            return Err(anyhow!(
                "emit_clif: let value of shape {other:?} — not yet supported"
            ));
        }
    }
    Ok(())
}

/// Drop a discarded statement's result if it owns an allocation.
/// Borrowed reads (a bare `Ref` statement) own nothing; strings are
/// always owned at production (reads clone).
fn emit_discard_result<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    node: &Node<R, E>,
    cv: CompiledExpr,
) -> Result<()> {
    let owned = matches!(node_composite_source(node), CompositeSource::Owned);
    match kernel_abi::abi_kind(cx.registry(), node.typ()) {
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) if owned => {
            let drop = cx.helper("graphix_valarray_drop")?;
            cx.b.ins().call(drop, &[cv.payload]);
        }
        Some(AbiKind::String) => {
            let drop = cx.helper("graphix_arcstr_drop")?;
            cx.b.ins().call(drop, &[cv.payload]);
        }
        // #219: a tainted 2-word value drops its (disc, payload) like an
        // untainted one — clean the disc so the helper sees a valid tag.
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) if owned => {
            let drop = cx.helper("graphix_value_drop")?;
            cx.b.ins().call(drop, &[cv.disc, cv.payload]);
        }
        _ => {}
    }
    Ok(())
}

/// Emit runtime drops for every owned composite / variant / nullable /
/// string local this scope introduced above `mark` — the direct-path
/// mirror of `compile_block_scalar`'s scope-exit drop block. Scalars
/// need no drop.
fn emit_scope_drops(cx: &mut BodyCx, mark: usize) -> Result<()> {
    let arr_drop = cx.helper("graphix_valarray_drop")?;
    let val_drop = cx.helper("graphix_value_drop")?;
    let str_drop = cx.helper("graphix_arcstr_drop")?;
    // Snapshot the (kind, vv) of every local above the mark so the
    // `cx.env` borrow ends before we drive `cx.b`.
    let drops: smallvec::SmallVec<[(LocalKind, ValueVar); 8]> =
        cx.env.locals[mark..].iter().map(|l| (l.kind, l.vv)).collect();
    for (kind, vv) in drops {
        match kind {
            LocalKind::Scalar(_) => {}
            LocalKind::Composite => {
                let ptr = cx.b.use_var(vv.payload);
                cx.b.ins().call(arr_drop, &[ptr]);
            }
            LocalKind::String => {
                let ptr = cx.b.use_var(vv.payload);
                cx.b.ins().call(str_drop, &[ptr]);
            }
            LocalKind::Variant | LocalKind::Nullable | LocalKind::Value => {
                let disc = cx.b.use_var(vv.disc);
                let payload = cx.b.use_var(vv.payload);
                cx.b.ins().call(val_drop, &[disc, payload]);
            }
        }
    }
    Ok(())
}

/// Compile a `ValueArith` / `ValueEq` / bytes / map / slice operand as
/// an OWNED `(disc, payload)` Value — the consuming helpers take both
/// operands by value. The Node twin of `compile_owned_value_operand`:
/// a value-shape operand clones a Borrowed read via
/// `ensure_owned_value_src`; a scalar is promoted to a fresh
/// `Value::<prim>` by inline packing; a String (already owned at
/// production) is wrapped via `graphix_value_new_string` (consumes); a
/// composite is owned-ensured then wrapped via
/// `graphix_value_new_from_array` (consumes). A possibly-bottom scalar
/// (its disc may carry `TAINT`) runtime-aborts via `emit_bottom_abort`.
fn emit_owned_value_operand_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    node: &Node<R, E>,
) -> Result<CompiledExpr> {
    match kernel_abi::abi_kind(cx.registry(), node.typ()) {
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            // #219: the disc carries the operand's taint through. A missing
            // 2-word input is a `Value::Null` placeholder (nonzero disc), so
            // clone and the value-arith helpers run harmlessly on it; the
            // taint guards the garbage result, which the consumer resolves.
            let cv = node.emit_clif(cx)?;
            let (disc, payload) = ensure_owned_value_src(
                cx,
                node_composite_source(node),
                cv.disc,
                cv.payload,
            )?;
            Ok(CompiledExpr::new(disc, payload))
        }
        Some(AbiKind::Scalar(p)) => {
            // A scalar's disc IS its value disc (`scalar_disc` ==
            // `prim_to_value_disc`), so taint carries forward unchanged;
            // only the payload is widened to the 8-byte Value word.
            let cv = node.emit_clif(cx)?;
            let payload = scalar_to_payload_i64(cx.b, p, cv.payload);
            Ok(CompiledExpr::new(cv.disc, payload))
        }
        Some(AbiKind::String) => {
            // Const/Ref/Concat reads all produce an owned ArcStr;
            // `graphix_value_new_string` consumes it into a Value. The
            // helper mints a flag-free disc — fold the source's
            // TAINT/STALE back on, else a placeholder input's garbage
            // ("" at init) computes an untainted result that escapes the
            // output gate.
            let cv = node.emit_clif(cx)?;
            let helper = cx.helper("graphix_value_new_string")?;
            let call = cx.b.ins().call(helper, &[cv.payload]);
            let r = cx.b.inst_results(call);
            let (rd, rp) = (r[0], r[1]);
            let disc = propagate_flags(cx.b, rd, &[cv.disc]);
            Ok(CompiledExpr::new(disc, rp))
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            // Same flag fold as the String arm: without it, slicing the
            // EMPTY placeholder of a not-yet-fired array input emitted a
            // real ArrayIndexError value on the init cycle (soak finding
            // divergence_000004, 2026-07-03).
            let cv = node.emit_clif(cx)?;
            let ptr =
                ensure_owned_composite_src(cx, node_composite_source(node), cv.payload)?;
            let helper = cx.helper("graphix_value_new_from_array")?;
            let call = cx.b.ins().call(helper, &[ptr]);
            let r = cx.b.inst_results(call);
            let (rd, rp) = (r[0], r[1]);
            let disc = propagate_flags(cx.b, rd, &[cv.disc]);
            Ok(CompiledExpr::new(disc, rp))
        }
        other => Err(anyhow!("emit_clif: value operand has unexpected type {other:?}")),
    }
}

/// Compile one producer-op field and emit the matching
/// `graphix_value_buf_push_*` call into `buf` — the Node twin of
/// `scaffold::push_field` (same helper choice per shape, same
/// owned/borrowed push variant via `node_composite_source`, same
/// bottom-abort for a may-bottom (tainted-disc) field).
fn emit_push_field_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    buf: ClifValue,
    field: &Node<R, E>,
) -> Result<ClifValue> {
    let helper_name: &str = match kernel_abi::abi_kind(cx.registry(), field.typ()) {
        Some(AbiKind::Scalar(p)) => value_buf_push_helper(p)?,
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            match node_composite_source(field) {
                CompositeSource::Owned => "graphix_value_buf_push_array",
                CompositeSource::Borrowed => "graphix_value_buf_push_array_borrowed",
            }
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            match node_composite_source(field) {
                CompositeSource::Owned => "graphix_value_buf_push_value",
                CompositeSource::Borrowed => "graphix_value_buf_push_value_borrowed",
            }
        }
        // String SSA is the ArcStr's raw thin-pointer bits (owned);
        // `_push_string` takes it by value (consumes). `_push_arcstr`
        // (which derefs a `*const ArcStr`) would be UB here.
        Some(AbiKind::String) => "graphix_value_buf_push_string",
        other => {
            return Err(anyhow!(
                "emit_clif: producer field of shape {other:?} — not \
                 representable"
            ));
        }
    };
    let push = cx.helper(helper_name)?;
    let cv = field.emit_clif(cx)?;
    // A tainted (bottom) field does NOT abort the kernel: the node-walk
    // only bottoms the PRODUCER node (nothing downstream that doesn't
    // read it is affected), so the composite must come out TAINTED, not
    // the whole kernel bottom — the caller's `propagate_flags` ORs the
    // field discs into the result disc, and the output path forces
    // bottom only if it CONSUMES the tainted composite (#219). Pushing
    // the tainted field is safe: its payload is the helper-safe
    // placeholder, and every push helper goes through the `TagValue`
    // gateway, which MASKS the tag byte before the value is cloned or
    // stored (a tainted disc can never materialize as a corrupt
    // `Value`). Previously this aborted to `pending_exit`, escalating a
    // locally-unconsumed bottom into whole-kernel bottom
    // (fuzz/triage-fuzzer-v2/divergence_000001: an UNUSED tuple binding
    // with a bottom element bottomed an unrelated const output).
    if kernel_abi::is_value_shape(cx.registry(), field.typ()) {
        cx.b.ins().call(push, &[buf, cv.disc, cv.payload]);
    } else {
        cx.b.ins().call(push, &[buf, cv.payload]);
    }
    // Return the field's disc so the composite result can OR-reduce
    // TAINT and AND-reduce STALE (a composite bottoms if any field
    // bottomed, and fires iff any field fired).
    Ok(cv.disc)
}

/// Tuple / array literal — build a `Vec<Value>` field-by-field via the
/// producer helpers, then finalize into an owned `*mut ValArray`.
/// Tuples and array literals share this emission — the runtime shape
/// is identical, only the static type differs.
pub(crate) fn emit_tuple_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    fields: &[Cached<R, E>],
) -> Result<CompiledExpr> {
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let finalize = cx.helper("graphix_valarray_finalize")?;
    let cap = cx.b.ins().iconst(types::I64, fields.len() as i64);
    let call = cx.b.ins().call(buf_new, &[cap]);
    let buf = cx.b.inst_results(call)[0];
    let mut field_discs = Vec::with_capacity(fields.len());
    for f in fields {
        field_discs.push(emit_push_field_node(cx, buf, &f.node)?);
    }
    let call = cx.b.ins().call(finalize, &[buf]);
    let payload = cx.b.inst_results(call)[0];
    // The composite fires iff any field fired → STALE = AND(field stales).
    // ZERO fields (`[]`) is a constant: fires at init only, like
    // `emit_const_node` — the bare ARRAY disc read as fired-every-
    // invocation, so an empty literal feeding a HOF re-fired the HOF's
    // `src_fired ∧ empty` rule on every unrelated kernel input event
    // (soak jul07d divergence_000000: a dead select arm's captured
    // async feeder re-fired `find([], …)` through the enclosing map —
    // interp 1, jit 5).
    let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    let disc = if field_discs.is_empty() {
        let init = cx.init_flag();
        const_stale_gate(cx.b, init, disc)
    } else {
        propagate_flags(cx.b, disc, &field_discs)
    };
    Ok(CompiledExpr::new(disc, payload))
}

/// Struct literal — an outer ValArray of inner `[name, value]` pairs,
/// fields sorted alphabetically by name (graphix's canonical struct
/// layout). Field names are interned lazily via
/// [`BodyCx::interned_str`].
pub(crate) fn emit_struct_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    names: &[ArcStr],
    fields: &[Cached<R, E>],
) -> Result<CompiledExpr> {
    if names.len() != fields.len() {
        return Err(anyhow!("emit_clif: struct literal name/field arity mismatch"));
    }
    let mut indexed: Vec<(&ArcStr, &Cached<R, E>)> =
        names.iter().zip(fields.iter()).collect();
    indexed.sort_by(|a, b| a.0.cmp(b.0));
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let push_arcstr = cx.helper("graphix_value_buf_push_arcstr")?;
    let push_array = cx.helper("graphix_value_buf_push_array")?;
    let finalize = cx.helper("graphix_valarray_finalize")?;
    let outer_cap = cx.b.ins().iconst(types::I64, indexed.len() as i64);
    let call = cx.b.ins().call(buf_new, &[outer_cap]);
    let outer = cx.b.inst_results(call)[0];
    let mut field_discs = Vec::with_capacity(indexed.len());
    for (name, field) in indexed {
        let inner_cap = cx.b.ins().iconst(types::I64, 2);
        let call = cx.b.ins().call(buf_new, &[inner_cap]);
        let inner = cx.b.inst_results(call)[0];
        let name_ptr = cx.interned_str(name);
        cx.b.ins().call(push_arcstr, &[inner, name_ptr]);
        // The struct fires iff any field VALUE fired — the names are
        // interned constants, so only the value discs gate freshness.
        field_discs.push(emit_push_field_node(cx, inner, &field.node)?);
        let call = cx.b.ins().call(finalize, &[inner]);
        let inner_arr = cx.b.inst_results(call)[0];
        cx.b.ins().call(push_array, &[outer, inner_arr]);
    }
    let call = cx.b.ins().call(finalize, &[outer]);
    let payload = cx.b.inst_results(call)[0];
    let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    let disc = propagate_flags(cx.b, disc, &field_discs);
    Ok(CompiledExpr::new(disc, payload))
}

/// `{ source with f: v, ... }` — build a NEW struct that copies the
/// source's sorted `[name, value]` pairs, overriding the replaced
/// fields. Mirrors [`emit_struct_new_node`], but each unchanged field's
/// value is READ from the source struct (one source read, N element
/// reads) while a replaced field emits its replacement node. `Replace.index`
/// is the field's sorted position (set by typecheck0). The outer/inner
/// bufs and an Owned source are registered for pending-exit cleanup so a
/// may-bottom replacement (`{s with x: a / b}`) frees them instead of
/// leaking.
pub(crate) fn emit_struct_with_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    replace: &[crate::node::data::Replace<R, E>],
) -> Result<CompiledExpr> {
    // Sorted (name, type) fields of the source struct — cloned out of the
    // deref before emitting (lock discipline).
    let fields: Vec<(ArcStr, Type)> = source.typ().with_deref(|t| match t {
        Some(Type::Struct(flds)) => {
            Ok(flds.iter().map(|(n, t)| (n.clone(), t.clone())).collect())
        }
        _ => Err(anyhow!("emit_clif: struct-with source isn't a struct")),
    })?;
    let (arr_ptr, src, src_disc) =
        emit_accessor_source_node(cx, source, AbiKind::Struct)?;
    // Register an Owned source: a replaced-field bottom-abort between here
    // and the finalize would otherwise leak it (a Borrowed source is
    // env-owned — nothing to drop).
    let src_var = match src {
        CompositeSource::Owned => {
            let v = cx.b.declare_var(types::I64);
            cx.b.def_var(v, arr_ptr);
            cx.ctx.owned_input_stack.borrow_mut().push(v);
            Some(v)
        }
        CompositeSource::Borrowed => None,
    };
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let push_arcstr = cx.helper("graphix_value_buf_push_arcstr")?;
    let push_array = cx.helper("graphix_value_buf_push_array")?;
    let finalize = cx.helper("graphix_valarray_finalize")?;
    let outer_cap = cx.b.ins().iconst(types::I64, fields.len() as i64);
    let call = cx.b.ins().call(buf_new, &[outer_cap]);
    let outer = cx.b.inst_results(call)[0];
    let outer_var = cx.b.declare_var(types::I64);
    cx.b.def_var(outer_var, outer);
    cx.ctx.dyncall_buf_stack.borrow_mut().push(outer_var);
    // The struct fires iff the source fired OR any replacement fired: fold
    // the source disc once (all unchanged fields share its freshness) and
    // each replacement's disc.
    let mut field_discs: Vec<ClifValue> = vec![src_disc];
    for (i, (name, field_typ)) in fields.iter().enumerate() {
        let inner_cap = cx.b.ins().iconst(types::I64, 2);
        let call = cx.b.ins().call(buf_new, &[inner_cap]);
        let inner = cx.b.inst_results(call)[0];
        let inner_var = cx.b.declare_var(types::I64);
        cx.b.def_var(inner_var, inner);
        cx.ctx.dyncall_buf_stack.borrow_mut().push(inner_var);
        let name_ptr = cx.interned_str(name);
        cx.b.ins().call(push_arcstr, &[inner, name_ptr]);
        match replace.iter().find(|r| r.index == Some(i)) {
            Some(r) => {
                // Replacement value — may bottom-abort (both bufs + Owned
                // source are registered, so `emit_pending_cleanup` frees them).
                field_discs.push(emit_push_field_node(cx, inner, &r.n.node)?);
            }
            None => {
                // Unchanged field — read the value from the source struct
                // (an owned clone, no abort) and push it.
                let ftyp = resolve_node_typ(cx.ctx, field_typ);
                let idx = cx.b.ins().iconst(types::I64, i as i64);
                let cv = compile_element_read(cx.b, arr_ptr, idx, &ftyp, true, cx.ctx)?;
                scaffold::push_field(cx, inner, cv, &ftyp, CompositeSource::Owned)?;
            }
        }
        let call = cx.b.ins().call(finalize, &[inner]);
        let inner_arr = cx.b.inst_results(call)[0];
        cx.ctx.dyncall_buf_stack.borrow_mut().pop(); // inner consumed by finalize
        cx.b.ins().call(push_array, &[outer, inner_arr]);
    }
    let call = cx.b.ins().call(finalize, &[outer]);
    let payload = cx.b.inst_results(call)[0];
    cx.ctx.dyncall_buf_stack.borrow_mut().pop(); // outer consumed by finalize
    // Drop the Owned source on the normal path (exactly once — the pending
    // path drops it via `owned_input_stack`).
    if src_var.is_some() {
        let drop = cx.helper("graphix_valarray_drop")?;
        cx.b.ins().call(drop, &[arr_ptr]);
        cx.ctx.owned_input_stack.borrow_mut().pop();
    }
    let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    let disc = propagate_flags(cx.b, disc, &field_discs);
    Ok(CompiledExpr::new(disc, payload))
}

/// Variant constructor. Nullary →
/// `Value::String(tag)` via `graphix_value_new_string_from_arcstr`
/// (clones the interned tag — the borrowed interned pointer makes the
/// clone mandatory). With payloads → `Value::Array([tag, p0, ...])` built
/// via the buf helpers and unwrapped into a two-register Value by
/// `graphix_value_new_from_array`. The tag is interned lazily via
/// [`BodyCx::interned_str`].
pub(crate) fn emit_variant_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    tag: &ArcStr,
    payloads: &[Cached<R, E>],
) -> Result<CompiledExpr> {
    let tag_ptr = cx.interned_str(tag);
    if payloads.is_empty() {
        let new_str = cx.helper("graphix_value_new_string_from_arcstr")?;
        let call = cx.b.ins().call(new_str, &[tag_ptr]);
        let (r0, r1) = {
            let r = cx.b.inst_results(call);
            (r[0], r[1])
        };
        // A nullary variant `` `Tag `` is a constant — fires once at init.
        let init = cx.init_flag();
        let disc = const_stale_gate(cx.b, init, r0);
        Ok(CompiledExpr::new(disc, r1))
    } else {
        let buf_new = cx.helper("graphix_value_buf_new")?;
        let push_arcstr = cx.helper("graphix_value_buf_push_arcstr")?;
        let finalize = cx.helper("graphix_valarray_finalize")?;
        let wrap_array = cx.helper("graphix_value_new_from_array")?;
        let cap = cx.b.ins().iconst(types::I64, (payloads.len() + 1) as i64);
        let call = cx.b.ins().call(buf_new, &[cap]);
        let buf = cx.b.inst_results(call)[0];
        cx.b.ins().call(push_arcstr, &[buf, tag_ptr]);
        let mut payload_discs = Vec::with_capacity(payloads.len());
        for p in payloads {
            payload_discs.push(emit_push_field_node(cx, buf, &p.node)?);
        }
        let call = cx.b.ins().call(finalize, &[buf]);
        let arr = cx.b.inst_results(call)[0];
        let call = cx.b.ins().call(wrap_array, &[arr]);
        let (r0, r1) = {
            let r = cx.b.inst_results(call);
            (r[0], r[1])
        };
        // A `Tag(a, b)` variant fires iff any payload fired.
        let disc = propagate_flags(cx.b, r0, &payload_discs);
        Ok(CompiledExpr::new(disc, r1))
    }
}

/// Compile an accessor's composite source to a `*const ValArray`,
/// returning the pointer plus its ownership classification. Shared by
/// the tuple/struct/array element-read relays; the caller drops an
/// Owned pointer after the read (the element helpers clone the slot
/// out, so the temporary producer would otherwise leak — a Borrowed
/// read stays owned by its env slot).
fn emit_accessor_source_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    want: AbiKind,
) -> Result<(ClifValue, CompositeSource, ClifValue)> {
    if kernel_abi::abi_kind(cx.registry(), source.typ()) != Some(want) {
        return Err(anyhow!(
            "emit_clif: accessor source of type {:?} isn't {want:?}",
            source.typ()
        ));
    }
    let src = node_composite_source(source);
    // #219: FORCE the source taint — `t.0` / `s.field` / `a[i]` over a
    // missing composite bottoms the kernel (folds away for an untainted
    // source; only a Borrowed missing input can be tainted, and it owns
    // nothing to leak on the abort path).
    let cv = source.emit_clif(cx)?;
    let valid = is_untainted(cx.b, cv.disc);
    emit_bottom_abort(cx.b, cx.env, cx.ctx, valid)?;
    // The source disc (TAINT now forced clear, STALE preserved) — the
    // caller AND-folds it: `t.0` / `s.field` / `a[i]` fires iff the source
    // (and, for `a[i]`, the index) fired.
    Ok((cv.payload, src, cv.disc))
}

/// Drop an accessor's temporary Owned source after the element read.
fn emit_accessor_source_drop(
    cx: &mut BodyCx,
    ptr: ClifValue,
    src: CompositeSource,
) -> Result<()> {
    if matches!(src, CompositeSource::Owned) {
        let drop = cx.helper("graphix_valarray_drop")?;
        cx.b.ins().call(drop, &[ptr]);
    }
    Ok(())
}

/// Like [`emit_accessor_source_node`] but WITHOUT the taint force — for
/// callers that guard the read themselves
/// ([`emit_guarded_element_read`]). The returned disc may carry TAINT.
fn emit_accessor_source_node_unforced<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    want: AbiKind,
) -> Result<(ClifValue, CompositeSource, ClifValue)> {
    if kernel_abi::abi_kind(cx.registry(), source.typ()) != Some(want) {
        return Err(anyhow!(
            "emit_clif: accessor source of type {:?} isn't {want:?}",
            source.typ()
        ));
    }
    let src = node_composite_source(source);
    let cv = source.emit_clif(cx)?;
    Ok((cv.payload, src, cv.disc))
}

/// A helper-safe placeholder (payload of `elem`'s CLIF type) plus the
/// matching TAINTED disc for a skipped read — what a tainted source's
/// consumer gets instead of an unchecked out-of-bounds read (#219: it
/// runs harmlessly downstream; the taint gates at the output).
fn emit_elem_placeholder(cx: &mut BodyCx, elem: &Type) -> Result<CompiledExpr> {
    match kernel_abi::abi_kind(cx.registry(), elem) {
        Some(AbiKind::Scalar(p)) => {
            let disc = cx.b.ins().iconst(types::I64, prim_to_value_disc(p) | TAINT);
            Ok(CompiledExpr::new(disc, zero_const(cx.b, p)))
        }
        Some(AbiKind::String) => {
            let helper = cx.helper("graphix_arcstr_empty")?;
            let call = cx.b.ins().call(helper, &[]);
            let s = cx.b.inst_results(call)[0];
            let disc = cx.b.ins().iconst(types::I64, value_disc::STRING | TAINT);
            Ok(CompiledExpr::new(disc, s))
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let helper = cx.helper("graphix_valarray_empty_boxed")?;
            let call = cx.b.ins().call(helper, &[]);
            let a = cx.b.inst_results(call)[0];
            let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY | TAINT);
            Ok(CompiledExpr::new(disc, a))
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value)
        | Some(AbiKind::Unit) => {
            let disc = cx.b.ins().iconst(types::I64, value_disc::NULL | TAINT);
            let zero = cx.b.ins().iconst(types::I64, 0);
            Ok(CompiledExpr::new(disc, zero))
        }
        other => Err(anyhow!("emit_clif: no placeholder for shape {other:?}")),
    }
}

/// Taint-guarded structural element read: a tainted source carries a
/// #219 PLACEHOLDER (e.g. the empty array a bottomed `$`/`?` now
/// produces), which the UNCHECKED read helpers cannot touch at fixed
/// offsets — so branch: skip the read and produce a tainted shape-safe
/// placeholder instead (the node-walk's accessor simply doesn't fire;
/// downstream consumers see taint and the output gates). `is_tainted`
/// folds to const-false for proven-untainted sources, so the branch
/// AND the placeholder path fold away entirely on the hot path.
fn emit_guarded_element_read(
    cx: &mut BodyCx,
    arr_ptr: ClifValue,
    src_disc: ClifValue,
    idx_val: ClifValue,
    elem: &Type,
    struct_access: bool,
) -> Result<CompiledExpr> {
    let tainted = is_tainted(cx.b, src_disc);
    let read_bl = cx.b.create_block();
    let skip_bl = cx.b.create_block();
    let merge = cx.b.create_block();
    let pay_ty = match kernel_abi::abi_kind(cx.registry(), elem) {
        Some(AbiKind::Scalar(p)) => prim_to_clif(p),
        _ => types::I64,
    };
    cx.b.append_block_param(merge, types::I64);
    cx.b.append_block_param(merge, pay_ty);
    cx.b.ins().brif(tainted, skip_bl, &[], read_bl, &[]);
    cx.b.switch_to_block(read_bl);
    cx.b.seal_block(read_bl);
    let rv = compile_element_read(cx.b, arr_ptr, idx_val, elem, struct_access, cx.ctx)?;
    cx.b.ins().jump(merge, &[BlockArg::Value(rv.disc), BlockArg::Value(rv.payload)]);
    cx.b.switch_to_block(skip_bl);
    cx.b.seal_block(skip_bl);
    let ph = emit_elem_placeholder(cx, elem)?;
    cx.b.ins().jump(merge, &[BlockArg::Value(ph.disc), BlockArg::Value(ph.payload)]);
    cx.b.switch_to_block(merge);
    cx.b.seal_block(merge);
    let params = cx.b.block_params(merge);
    Ok(CompiledExpr::new(params[0], params[1]))
}

/// `t.<idx>` — a statically-valid index, read through
/// `compile_element_read` (owned result; Value shape for a value-shape
/// element, Single otherwise).
pub(crate) fn emit_tuple_ref_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    idx: usize,
    elem_typ: &Type,
) -> Result<CompiledExpr> {
    // Node-carried elem types can be Refs to abstract type names
    // (#218) — resolve before the read classifies by abi_kind.
    let elem_typ = resolve_node_typ(cx.ctx, elem_typ);
    let (arr_ptr, src, src_disc) =
        emit_accessor_source_node_unforced(cx, source, AbiKind::Tuple)?;
    let idx_const = cx.b.ins().iconst(types::I64, idx as i64);
    let result =
        emit_guarded_element_read(cx, arr_ptr, src_disc, idx_const, &elem_typ, false)?;
    emit_accessor_source_drop(cx, arr_ptr, src)?;
    // `t.0` fires iff the source tuple fired (the index is a constant); the
    // element read synthesizes a fresh disc, so the source's STALE gates it.
    let disc = propagate_flags(cx.b, result.disc, &[src_disc]);
    Ok(CompiledExpr::new(disc, result.payload))
}

/// `s.field` — the two-level kv-pair read via the `struct_get_*`
/// helper family. `sorted_idx` is the
/// field's position in the struct type's canonical (sorted) layout —
/// resolved by the node's typecheck.
pub(crate) fn emit_struct_ref_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    sorted_idx: usize,
    elem_typ: &Type,
) -> Result<CompiledExpr> {
    // Same abstract-Ref resolution as the tuple read (#218).
    let elem_typ = resolve_node_typ(cx.ctx, elem_typ);
    let (arr_ptr, src, src_disc) =
        emit_accessor_source_node_unforced(cx, source, AbiKind::Struct)?;
    let idx_const = cx.b.ins().iconst(types::I64, sorted_idx as i64);
    let result =
        emit_guarded_element_read(cx, arr_ptr, src_disc, idx_const, &elem_typ, true)?;
    emit_accessor_source_drop(cx, arr_ptr, src)?;
    // `s.field` fires iff the source struct fired (the field index is
    // static); fold the source's STALE onto the fresh element read.
    let disc = propagate_flags(cx.b, result.disc, &[src_disc]);
    Ok(CompiledExpr::new(disc, result.payload))
}

/// `a[i]` / `bytes[i]` — the result type is always `Nullable<elem>`
/// (out-of-bounds →
/// the `ArrayIndexError` Value), produced by the shared bounds-checked
/// helpers (`graphix_valarray_index` routes through the node-walk's
/// own `node::array::array_index`, `graphix_bytes_index` through
/// `bytes_index` — all backends agree bit-for-bit).
pub(crate) fn emit_array_ref_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    idx: &Node<R, E>,
) -> Result<CompiledExpr> {
    let idx_prim = kernel_abi::scalar_prim(cx.registry(), idx.typ())
        .filter(|p| p.is_integer())
        .ok_or_else(|| anyhow!("emit_clif: index of non-integer type {:?}", idx.typ()))?;
    if matches!(kernel_abi::abi_kind(cx.registry(), source.typ()), Some(AbiKind::Array)) {
        // Unforced: `graphix_valarray_index` is bounds-checked (safe on
        // a placeholder), and the source's taint folds into the result
        // disc below — forcing here kernel-bottomed live chains through
        // a tainted literal (triage item 23).
        let (arr_ptr, src, src_disc) =
            emit_accessor_source_node_unforced(cx, source, AbiKind::Array)?;
        let idx_cv = idx.emit_clif(cx)?;
        let idx_i64 = widen_to_i64(cx.b, idx_cv.payload, idx_prim)?;
        let helper = cx.helper("graphix_valarray_index")?;
        let call = cx.b.ins().call(helper, &[arr_ptr, idx_i64]);
        let r = cx.b.inst_results(call);
        let (rdisc, rpay) = (r[0], r[1]);
        emit_accessor_source_drop(cx, arr_ptr, src)?;
        // #219: a tainted index taints the result. STALE folds the source
        // array AND the index (`a[i]` fires iff a OR i fired) — both
        // operands present now that the accessor returns the source disc.
        let disc = propagate_flags(cx.b, rdisc, &[src_disc, idx_cv.disc]);
        return Ok(CompiledExpr::new(disc, rpay));
    }
    if lowering::is_bytes(source.typ()) {
        // The helper consumes the bytes operand — owned.
        let bcv = emit_owned_value_operand_node(cx, source)?;
        let idx_cv = idx.emit_clif(cx)?;
        let helper = cx.helper("graphix_bytes_index")?;
        let call = cx.b.ins().call(helper, &[bcv.disc, bcv.payload, idx_cv.payload]);
        let (rdisc, rpay) = {
            let r = cx.b.inst_results(call);
            (r[0], r[1])
        };
        // A tainted bytes operand or index taints the result; STALE folds
        // too (the index fires iff bytes OR idx fired — both operands
        // present here, so the AND-reduce is complete).
        let disc = propagate_flags(cx.b, rdisc, &[bcv.disc, idx_cv.disc]);
        return Ok(CompiledExpr::new(disc, rpay));
    }
    Err(anyhow!(
        "emit_clif: index source of type {:?} isn't an array or bytes",
        source.typ()
    ))
}

/// `m{key}` — both operands as OWNED `(disc, payload)` Values (the
/// helper consumes them);
/// `graphix_map_ref` does the lookup (shared `node::map::map_get`
/// semantics), returning `Nullable<V>` as two words.
pub(crate) fn emit_map_ref_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    key: &Node<R, E>,
) -> Result<CompiledExpr> {
    if !lowering::is_map(source.typ()) {
        return Err(anyhow!(
            "emit_clif: map-ref source of type {:?} isn't a Map",
            source.typ()
        ));
    }
    let mcv = emit_owned_value_operand_node(cx, source)?;
    let kcv = emit_owned_value_operand_node(cx, key)?;
    let helper = cx.helper("graphix_map_ref")?;

    let call = cx.b.ins().call(helper, &[mcv.disc, mcv.payload, kcv.disc, kcv.payload]);
    let (rdisc, rpay) = {
        let r = cx.b.inst_results(call);
        (r[0], r[1])
    };
    // A tainted map or key taints the lookup result; STALE folds too (the
    // lookup fires iff map OR key fired — both operands present here).
    let disc = propagate_flags(cx.b, rdisc, &[mcv.disc, kcv.disc]);
    Ok(CompiledExpr::new(disc, rpay))
}

/// `a[i..j]` — the source as an OWNED Value (the helper consumes it;
/// a composite source is wrapped
/// via `graphix_value_new_from_array`), present bounds as integer
/// scalars with a flag bit each, absent bounds pass 0 with the bit
/// cleared. Result is `Nullable<source>` (shared
/// `node::array::array_slice` semantics).
pub(crate) fn emit_array_slice_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    start: Option<&Node<R, E>>,
    end: Option<&Node<R, E>>,
) -> Result<CompiledExpr> {
    if !(matches!(
        kernel_abi::abi_kind(cx.registry(), source.typ()),
        Some(AbiKind::Array)
    ) || lowering::is_bytes(source.typ()))
    {
        return Err(anyhow!(
            "emit_clif: slice source of type {:?} isn't an array or bytes",
            source.typ()
        ));
    }
    let scv = emit_owned_value_operand_node(cx, source)?;
    // #219: source + present-bound taint propagates into the slice
    // result (forced at the output).
    let mut taint_discs: Vec<ClifValue> = vec![scv.disc];
    let emit_bound = |cx: &mut BodyCx,
                      n: Option<&Node<R, E>>,
                      flag: i64,
                      flags: &mut i64,
                      taint: &mut Vec<ClifValue>|
     -> Result<ClifValue> {
        match n {
            None => Ok(cx.b.ins().iconst(types::I64, 0)),
            Some(n) => {
                if !kernel_abi::scalar_prim(cx.registry(), n.typ())
                    .is_some_and(|p| p.is_integer())
                {
                    return Err(anyhow!(
                        "emit_clif: slice bound of non-integer type {:?}",
                        n.typ()
                    ));
                }
                *flags |= flag;
                let cv = n.emit_clif(cx)?;
                taint.push(cv.disc);
                Ok(cv.payload)
            }
        }
    };
    let mut flags = 0i64;
    let start_v = emit_bound(cx, start, 1, &mut flags, &mut taint_discs)?;
    let end_v = emit_bound(cx, end, 2, &mut flags, &mut taint_discs)?;
    let flags_v = cx.b.ins().iconst(types::I64, flags);
    let helper = cx.helper("graphix_array_slice")?;
    let call = cx.b.ins().call(helper, &[scv.disc, scv.payload, start_v, end_v, flags_v]);
    let r = cx.b.inst_results(call);
    let (rdisc, rpay) = (r[0], r[1]);
    // The slice fires iff the source OR any present bound fired — and
    // `taint_discs` already holds the source disc plus each present bound,
    // so STALE folds completely (a const-bound slice fires iff the source
    // fired).
    let disc = propagate_flags(cx.b, rdisc, &taint_discs);
    Ok(CompiledExpr::new(disc, rpay))
}

/// Emit the error-delivery DynCall for a handler-ful `?` (a `?` caught
/// by an enclosing `try` — `FnSource::QopDeliver`). Marshals the error
/// value `cv` as the single Value-shape argument and dispatches the
/// pre-bound `QopDeliverApply`, which runs `wrap_error` + writes the
/// catch handler's variable (exactly `Qop::update`'s handler path). Unit
/// return, discarded — the caller's error branch bottoms the result
/// separately (the scalar taint). `inner_owned` selects owned vs
/// borrowed push: the error rides the inner's ownership, and the owned
/// push hands it to `QopDeliverApply` to drop (no separate drop here).
fn emit_qop_deliver(
    cx: &mut BodyCx,
    site_id: ExprId,
    cv: &CompiledExpr,
    inner_owned: bool,
) -> Result<()> {
    let info = cx
        .builtin_site(site_id)
        .ok_or_else(|| anyhow!("emit_clif: qop-deliver site {site_id:?} not discovered"))?
        .clone();
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let cap = cx.b.ins().iconst(types::I64, 1);
    let call = cx.b.ins().call(buf_new, &[cap]);
    let buf = cx.b.inst_results(call)[0];
    let buf_var = cx.b.declare_var(types::I64);
    cx.b.def_var(buf_var, buf);
    cx.ctx.dyncall_buf_stack.borrow_mut().push(buf_var);
    // The error rides the 2-word Value wire shape; the pushed disc must be
    // CLEAN (a tainted disc is an invalid tag).
    let clean = clean_disc(cx.b, cv.disc);
    let push_name = if inner_owned {
        "graphix_value_buf_push_value"
    } else {
        "graphix_value_buf_push_value_borrowed"
    };
    let push = cx.helper(push_name)?;
    cx.b.ins().call(push, &[buf, clean, cv.payload]);
    let dyncall = cx.helper("graphix_dyncall")?;
    // Region-wide slot index: the site's local fn_index plus this body's
    // base offset into the combined `dyn_slots` table.
    let region_idx = info.fn_index + cx.fn_index_offset();
    let fn_idx = cx.b.ins().iconst(types::I32, region_idx as i64);
    // ret_kind=3 (Unit): QopDeliverApply returns Value::Null; discarded.
    let ret_kind = cx.b.ins().iconst(types::I8, 3);
    cx.b.ins().call(dyncall, &[fn_idx, buf, ret_kind]);
    // `QopDeliverApply::update` structurally returns `Some(Null)` (the
    // marshalled arg is always present), but every dyncall site clears
    // the pending flag so it can only ever mean "genuine abort".
    let take = cx.helper("graphix_dyncall_pending_take_clear")?;
    cx.b.ins().call(take, &[]);
    cx.ctx.dyncall_buf_stack.borrow_mut().pop();
    Ok(())
}

/// `?` / `$` — both unwrap a Nullable<T> to T (else pass the value
/// through unchanged for a non-Nullable inner, mirroring `wrap_qop`'s
/// None branch). Scalar / string / composite success returns the
/// unwrapped element; Value-shape success returns the (disc, payload)
/// pair.
///
/// `result_typ` is the qop NODE's static type — the arm selection
/// keys on it, NOT on the inner's one-layer option success. The
/// typechecker (and the node-walk, which drops ANY error value)
/// strips EVERY error member of the flattened inner union — so for a
/// nested fallible union like `[[string, Error<E>], Error<AIE>]` the
/// node's type is `string` and every consumer (the kernel return
/// included) expects the STRING convention, while the inner's
/// one-layer success `[string, Error<E>]` freezes value-shape. Arm-
/// selecting on the latter minted the value-shape `(Null, 0)`
/// placeholder on the error path, and the String-conventioned
/// consumer dropped payload 0 — `graphix_arcstr_drop(NULL)` SIGABRT
/// (soak jul05 item 15, crash_000014).
pub(crate) fn emit_qop_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    inner: &Node<R, E>,
    result_typ: &Type,
    handler_site: Option<ExprId>,
) -> Result<CompiledExpr> {
    // Lockstep with the discovery-side freeze (lowering.rs
    // try_register_qop_deliver): both normalized, so a site the
    // discovery registered always emits.
    let Some(inner_typ) =
        kernel_abi::freeze_for_abi_normalized(cx.registry(), inner.typ())
    else {
        return Err(anyhow!(
            "emit_clif: `?` inner type {:?} doesn't freeze concrete",
            inner.typ()
        ));
    };
    if kernel_abi::nullable_inner(cx.registry(), &inner_typ).is_none() {
        // No error possible — passthrough; the handler (if any) never fires.
        return inner.emit_clif(cx);
    }
    let Some(success_typ) =
        kernel_abi::freeze_for_abi_normalized(cx.registry(), result_typ)
    else {
        return Err(anyhow!(
            "emit_clif: `?` result type {:?} doesn't freeze concrete",
            result_typ
        ));
    };
    let cv = inner.emit_clif(cx)?;
    let (disc, payload) = (cv.disc, cv.payload);
    // `clean(disc) == Typ::Error` (`0x2000_0000`) means bottom (mask
    // taint first — a tainted disc is not a structural Error).
    let clean = clean_disc(cx.b, disc);
    let is_err = cx.b.ins().icmp_imm(IntCC::Equal, clean, 0x2000_0000_i64);
    // A TAINTED error is a PHANTOM — computed from #219 placeholders
    // after an upstream bottom (e.g. `(a[BAD]? /? y)?`: the index raise
    // taints, the checked div then computes `0 /? 0` on placeholders
    // and mints a REAL ArithError value). The node-walk never runs the
    // div at all, so delivering it to a catch handler invents an error
    // the program never raised (soak finding corpus-fuzz/
    // divergence_000030: the wrong catch arm fired). Delivers below
    // gate on `deliverable`; the abort/taint paths still treat it as
    // no-value.
    let untainted = is_untainted(cx.b, disc);
    let deliverable = cx.b.ins().band(is_err, untainted);
    match kernel_abi::abi_kind(cx.registry(), &success_typ) {
        // Prim success — BRANCHLESS per-value taint. The payload word
        // holds the success bits when !is_err; on the error path the
        // bits are garbage but the disc's TAINT means they're never
        // used (forced at the output). The error Value isn't dropped
        // here: a scalar `Nullable` inner is a by-value scalar (no
        // heap). #219: the inner's own taint also flows through.
        Some(AbiKind::Scalar(p)) => {
            // Handler-ful `?` (a `?` caught by an enclosing `try`): on the
            // error path, deliver the error to the catch handler's
            // variable before bottoming (mirrors `Qop::update`'s handler
            // path). The catch handler reading that variable is a separate
            // kernel (next cycle), so no read-after-write hazard.
            if let Some(site) = handler_site {
                let deliver_block = cx.b.create_block();
                let after = cx.b.create_block();
                cx.b.ins().brif(deliverable, deliver_block, &[], after, &[]);
                cx.b.switch_to_block(deliver_block);
                cx.b.seal_block(deliver_block);
                let inner_owned = node_composite_source(inner) == CompositeSource::Owned;
                emit_qop_deliver(cx, site, &cv, inner_owned)?;
                cx.b.ins().jump(after, &[]);
                cx.b.switch_to_block(after);
                cx.b.seal_block(after);
            }
            let value = cast_u64_to_prim(cx.b, payload, p);
            let base = scalar_disc(cx.b, p);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            let disc = taint_if(cx.b, disc, is_err);
            // Interior-bottom exactness: a `$`/`?`-dropped error with
            // prior success degrades to STALE + the cached value,
            // matching the node-walk's cached qop node.
            Ok(emit_scalar_taint_cache(cx, p, CompiledExpr::new(disc, value)))
        }
        // String / composite success — branch: the bad path (error OR
        // tainted) produces a tainted shape-safe PLACEHOLDER and
        // CONTINUES (interior-bottom v2, 2026-07-04) instead of the old
        // whole-kernel pending-exit, which escalated a locally
        // unconsumed bottom into no-output-at-all (the live-chain
        // findings, triage item 11).
        Some(AbiKind::String | AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let is_string = matches!(
                kernel_abi::abi_kind(cx.registry(), &success_typ),
                Some(AbiKind::String)
            );
            let base_disc =
                if is_string { value_disc::STRING } else { value_disc::ARRAY };
            let value_drop = cx.helper("graphix_value_drop")?;
            let inner_owned = node_composite_source(inner) == CompositeSource::Owned;
            let pre_pending = cx.b.create_block();
            let continue_block = cx.b.create_block();
            let qmerge = cx.b.create_block();
            cx.b.append_block_param(qmerge, types::I64); // disc
            cx.b.append_block_param(qmerge, types::I64); // payload
            // #219: a TAINTED inner may carry the helper-safe Value::Null
            // placeholder, whose CLEAN disc isn't Error — without folding
            // taint into this branch the success path unboxes Null as an
            // Array/ArcStr (`unreachable_unchecked` UB in the unboxers;
            // soak crash 2026-07-04, `x$` of a tainted slice). Tainted =
            // "no value" = the same abort as the error path.
            let tainted = is_tainted(cx.b, disc);
            let bad = cx.b.ins().bor(is_err, tainted);
            cx.b.ins().brif(bad, pre_pending, &[], continue_block, &[]);
            cx.b.switch_to_block(pre_pending);
            cx.b.seal_block(pre_pending);
            // Abort path. Handler-ful `?`: deliver a REAL error to the
            // catch handler — the deliver CONSUMES the owned error (or
            // clones a borrowed one), so it replaces the `value_drop`
            // (dropping AND delivering an owned error would double-free).
            // A tainted non-error must NOT deliver (the handler would
            // receive placeholder garbage) but an owned one still drops.
            // Handler-less: drop the owned value (a Borrowed Local is
            // owned by its env slot, which `emit_pending_cleanup` drops —
            // dropping here too would double-free).
            if let Some(site) = handler_site {
                let deliver_block = cx.b.create_block();
                let no_deliver = cx.b.create_block();
                let merged = cx.b.create_block();
                cx.b.ins().brif(deliverable, deliver_block, &[], no_deliver, &[]);
                cx.b.switch_to_block(deliver_block);
                cx.b.seal_block(deliver_block);
                emit_qop_deliver(cx, site, &cv, inner_owned)?;
                cx.b.ins().jump(merged, &[]);
                cx.b.switch_to_block(no_deliver);
                cx.b.seal_block(no_deliver);
                if inner_owned {
                    cx.b.ins().call(value_drop, &[clean, payload]);
                }
                cx.b.ins().jump(merged, &[]);
                cx.b.switch_to_block(merged);
                cx.b.seal_block(merged);
            } else if inner_owned {
                cx.b.ins().call(value_drop, &[clean, payload]);
            }
            // Tainted shape-safe placeholder; the inner's STALE carries
            // (the qop fires iff its inner fired) and TAINT marks
            // no-value — downstream consumers run harmlessly on the
            // placeholder and the output gates only if it's consumed.
            let ph_helper = if is_string {
                cx.helper("graphix_arcstr_empty")?
            } else {
                cx.helper("graphix_valarray_empty_boxed")?
            };
            let call = cx.b.ins().call(ph_helper, &[]);
            let ph = cx.b.inst_results(call)[0];
            let tainted_base = cx.b.ins().iconst(types::I64, base_disc | TAINT);
            let stale_bit = cx.b.ins().band_imm(cv.disc, STALE);
            let ph_disc = cx.b.ins().bor(tainted_base, stale_bit);
            cx.b.ins().jump(qmerge, &[BlockArg::Value(ph_disc), BlockArg::Value(ph)]);
            cx.b.switch_to_block(continue_block);
            cx.b.seal_block(continue_block);
            // Extract success T (now known non-error). The unwrap
            // result is Owned, so a String/composite success from a
            // Borrowed inner must be cloned. String: the ArcStr bits
            // inside a Value ARE the string ABI's one-word
            // representation. Composite: the payload word is the
            // ValArray BITS, but the composite ABI is a boxed
            // `*mut ValArray` — re-box via `graphix_value_into_array`
            // (consumes) / `_borrowed` (clones inner) (#199).
            let v = match kernel_abi::abi_kind(cx.registry(), &success_typ) {
                Some(AbiKind::String) => {
                    if inner_owned {
                        payload
                    } else {
                        let clone = cx.helper("graphix_arcstr_clone")?;
                        let call = cx.b.ins().call(clone, &[payload]);
                        cx.b.inst_results(call)[0]
                    }
                }
                _ => {
                    let helper_name = if inner_owned {
                        "graphix_value_into_array"
                    } else {
                        "graphix_value_into_array_borrowed"
                    };
                    let unbox = cx.helper(helper_name)?;
                    let call = cx.b.ins().call(unbox, &[clean, payload]);
                    cx.b.inst_results(call)[0]
                }
            };
            let base = cx.b.ins().iconst(types::I64, base_disc);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            cx.b.ins().jump(qmerge, &[BlockArg::Value(disc), BlockArg::Value(v)]);
            cx.b.switch_to_block(qmerge);
            cx.b.seal_block(qmerge);
            let params = cx.b.block_params(qmerge);
            Ok(CompiledExpr::new(params[0], params[1]))
        }
        // Value-shape success. The bad path (error) produces a tainted
        // Value::Null placeholder and CONTINUES (interior-bottom v2);
        // otherwise the non-error Value IS the result T (its own
        // `(disc, payload)`, passed through to the consumer which takes
        // ownership).
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let value_drop = cx.helper("graphix_value_drop")?;
            let src = node_composite_source(inner);
            let inner_owned = src == CompositeSource::Owned;
            let pre_pending = cx.b.create_block();
            let continue_block = cx.b.create_block();
            let qmerge = cx.b.create_block();
            cx.b.append_block_param(qmerge, types::I64); // disc
            cx.b.append_block_param(qmerge, types::I64); // payload
            cx.b.ins().brif(is_err, pre_pending, &[], continue_block, &[]);
            cx.b.switch_to_block(pre_pending);
            cx.b.seal_block(pre_pending);
            // Error path. Handler-ful `?`: deliver a REAL (untainted)
            // error to the catch handler (consumes the owned error /
            // clones a borrowed one), so it replaces the `value_drop`;
            // a PHANTOM (tainted) error must not deliver — see
            // `deliverable` above — but an owned one still drops.
            // Handler-less: drop the owned error before aborting — but
            // ONLY when `inner` is an owned producer. A Borrowed (Ref)
            // inner is owned by its env slot, which
            // `emit_pending_cleanup` -> `drop_owned_composites` already
            // drops; dropping it here too would double-free.
            if let Some(site) = handler_site {
                let deliver_block = cx.b.create_block();
                let no_deliver = cx.b.create_block();
                let merged = cx.b.create_block();
                cx.b.ins().brif(deliverable, deliver_block, &[], no_deliver, &[]);
                cx.b.switch_to_block(deliver_block);
                cx.b.seal_block(deliver_block);
                emit_qop_deliver(cx, site, &cv, inner_owned)?;
                cx.b.ins().jump(merged, &[]);
                cx.b.switch_to_block(no_deliver);
                cx.b.seal_block(no_deliver);
                if inner_owned {
                    cx.b.ins().call(value_drop, &[clean, payload]);
                }
                cx.b.ins().jump(merged, &[]);
                cx.b.switch_to_block(merged);
                cx.b.seal_block(merged);
            } else if inner_owned {
                cx.b.ins().call(value_drop, &[clean, payload]);
            }
            // Tainted Value::Null placeholder (helper-safe by
            // construction); the inner's STALE carries.
            let tainted_base = cx.b.ins().iconst(types::I64, value_disc::NULL | TAINT);
            let stale_bit = cx.b.ins().band_imm(cv.disc, STALE);
            let ph_disc = cx.b.ins().bor(tainted_base, stale_bit);
            let zero = cx.b.ins().iconst(types::I64, 0);
            cx.b.ins().jump(qmerge, &[BlockArg::Value(ph_disc), BlockArg::Value(zero)]);
            cx.b.switch_to_block(continue_block);
            cx.b.seal_block(continue_block);
            // The non-error Value IS the result T. Ensure it's owned: a
            // Borrowed (Ref) inner aliases its env slot, which is also
            // dropped at scope exit — handing those bits to the consumer
            // (the unwrap result is Owned) would double-free. An Owned
            // inner passes through unchanged. Clean for the clone; the
            // inner's taint re-attaches to the result.
            let (od, op) = ensure_owned_value_src(cx, src, clean, payload)?;
            // `e?` fires iff its operand fired (single input); STALE folds.
            let disc = propagate_flags(cx.b, od, &[cv.disc]);
            cx.b.ins().jump(qmerge, &[BlockArg::Value(disc), BlockArg::Value(op)]);
            cx.b.switch_to_block(qmerge);
            cx.b.seal_block(qmerge);
            let params = cx.b.block_params(qmerge);
            Ok(CompiledExpr::new(params[0], params[1]))
        }
        Some(AbiKind::Unit | AbiKind::Null) | None => {
            Err(anyhow!("emit_clif: `?` with unsupported success type {:?}", success_typ))
        }
    }
}

// (The old `emit_dyncall_arg_taint_abort` — post-call whole-kernel
// bottom on a tainted arg — is gone: `emit_dyncall_node` now gates the
// CALL itself and produces a tainted placeholder, so the callee never
// observes placeholder garbage and live chains keep flowing. Its doc's
// "taint can't survive a composite/string let" caveat was stale:
// `bind_local` stores the RHS disc for every LocalKind.)

/// Builtin DynCall — marshal the (marshal-ordered) `args` into a
/// fresh `LPooled<Vec<Value>>` buf, dispatch via `graphix_dyncall`
/// against the `FnSource::Builtin` slot at `info.fn_index`, then
/// decode the return per shape: scalar / unit / string / composite
/// returns the unwrapped value, a Value-shape return passes the
/// (disc, payload) pair through. A dispatch that PENDS (the builtin
/// returned no value this cycle) is converted at this site to a #219
/// tainted shape-safe placeholder that continues — never a
/// whole-kernel abort (item 28).
pub(crate) fn emit_dyncall_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    info: &BuiltinCallSiteInfo,
    args: &[&Node<R, E>],
) -> Result<CompiledExpr> {
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let cap = cx.b.ins().iconst(types::I64, args.len() as i64);
    let call = cx.b.ins().call(buf_new, &[cap]);
    let buf = cx.b.inst_results(call)[0];
    let buf_var = cx.b.declare_var(types::I64);
    cx.b.def_var(buf_var, buf);
    cx.ctx.dyncall_buf_stack.borrow_mut().push(buf_var);
    // #219: each arg's disc — its TAINT bit propagates into a scalar
    // result and force-bottoms a non-scalar result (folds away when no
    // arg is tainted, the fast path).
    let mut arg_taint_discs: Vec<ClifValue> = Vec::with_capacity(args.len());
    for (arg_node, t) in args.iter().zip(info.arg_types.iter()) {
        // Compare by runtime SHAPE (`AbiKind`), not exact `Type` —
        // the direct twin of the lowering-side agreement check. The
        // DynCall marshals by `info.arg_types`, so only the shape
        // needs to agree; a mismatch aborts the kernel (the classic
        // path refuses at lowering — same net effect, the subtree
        // node-walks).
        let Some(frozen) =
            kernel_abi::freeze_for_abi_normalized(cx.registry(), arg_node.typ())
        else {
            return Err(anyhow!(
                "emit_clif: DynCall arg type {:?} doesn't freeze concrete",
                arg_node.typ()
            ));
        };
        if kernel_abi::abi_kind(cx.registry(), &frozen)
            != kernel_abi::abi_kind(cx.registry(), t)
        {
            return Err(anyhow!(
                "emit_clif: DynCall arg shape {:?} disagrees with the \
                 discovered arg type {t:?}",
                frozen
            ));
        }
        // For composite/value args the push helper depends on where the
        // SSA value came from. A `Borrowed` source (a Ref read — the
        // caller still owns it) refcount-bumps; an `Owned` source (a
        // producer op, or a composite/Value-return DynCall result not
        // bound to a local) transfers ownership into the buf. Using the
        // borrowed helper on an Owned source leaks the original; the
        // move helper on a Borrowed source double-frees it.
        let helper_name: &str = match kernel_abi::abi_kind(cx.registry(), t) {
            Some(AbiKind::Scalar(p)) => value_buf_push_helper(p)?,
            Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                match node_composite_source(arg_node) {
                    CompositeSource::Owned => "graphix_value_buf_push_array",
                    CompositeSource::Borrowed => "graphix_value_buf_push_array_borrowed",
                }
            }
            // Variant / Nullable / datetime / duration / bytes / map /
            // error all ride the two-word `(disc, payload)` Value wire
            // shape.
            Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
                match node_composite_source(arg_node) {
                    CompositeSource::Owned => "graphix_value_buf_push_value",
                    CompositeSource::Borrowed => "graphix_value_buf_push_value_borrowed",
                }
            }
            Some(AbiKind::String) => "graphix_value_buf_push_string",
            Some(AbiKind::Unit) => {
                return Err(anyhow!("emit_clif: DynCall arg has Unit type"));
            }
            Some(AbiKind::Null) | None => {
                return Err(anyhow!(
                    "DynCall arg with bare Null / non-fusable type — should \
                     have widened to Nullable<T> at construction"
                ));
            }
        };
        let push = cx.helper(helper_name)?;
        // #219: the helper is pure, so a garbage operand (from a missing
        // input / div0) is harmless — push the (possibly-garbage) value
        // and carry the arg's disc; its taint guards the result at the
        // dyncall site. The pushed disc must be CLEAN (a tainted disc is
        // an invalid tag — storing it builds a corrupt Value the builtin
        // would clone/drop, UB).
        let cv = arg_node.emit_clif(cx)?;
        arg_taint_discs.push(cv.disc);
        if kernel_abi::is_value_shape(cx.registry(), t) {
            cx.b.ins().call(push, &[buf, cv.disc, cv.payload]);
        } else {
            cx.b.ins().call(push, &[buf, cv.payload]);
        }
    }
    let dyncall = cx.helper("graphix_dyncall")?;
    let ret_kind: i64 = match kernel_abi::abi_kind(cx.registry(), &info.return_type) {
        Some(AbiKind::Scalar(_)) => 0,
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => 1,
        // Variant / Nullable / value-shape all come back as a
        // boxed `*mut Value` (dispatch_typed wraps the
        // dispatcher's returned `Value` in `Box::into_raw`
        // regardless of the outer enum tag), so they share the
        // ret_kind=2 path.
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => 2,
        // Unit return: dispatcher returns 0 (the slot is
        // discarded by the caller). ret_kind=3 tells
        // dispatch_typed not to box anything.
        Some(AbiKind::Unit) => 3,
        // String return: dispatcher extracts the ArcStr from
        // `Value::String`, transmutes to raw u64 bits, returns
        // in word0. Caller's SSA reads the bits directly as
        // an owned `arcstr::ArcStr` (`repr(transparent)`).
        Some(AbiKind::String) => 4,
        Some(AbiKind::Null) | None => {
            return Err(anyhow!(
                "DynCall with bare Null / non-fusable return — \
                 should have widened to Nullable<T> at construction"
            ));
        }
    };
    // Region-wide slot index: local fn_index + this body's base offset
    // into the combined `dyn_slots` table.
    let region_idx = info.fn_index + cx.fn_index_offset();
    let fn_idx_val = cx.b.ins().iconst(types::I32, region_idx as i64);
    let ret_kind_val = cx.b.ins().iconst(types::I8, ret_kind);
    // Interior-bottom v2: a tainted arg means the callee must NOT run —
    // the node-walk's builtin never fires without its inputs, and a
    // Sync EFFECT (println) must not observe placeholder garbage (the
    // old post-call abort ran the callee FIRST, then bottomed the whole
    // kernel — over-broad for live chains AND effect-leaking). Gate the
    // call: any tainted arg skips it, drops the marshalled buf, and
    // produces a tainted placeholder result. Each `is_tainted` folds to
    // const-false for proven-untainted args, so the gate vanishes on
    // the hot path.
    let mut any_tainted = cx.b.ins().iconst(types::I8, 0);
    for d in &arg_taint_discs {
        let t = is_tainted(cx.b, *d);
        any_tainted = cx.b.ins().bor(any_tainted, t);
    }
    let call_bl = cx.b.create_block();
    let skip_bl = cx.b.create_block();
    let dmerge = cx.b.create_block();
    let pay_ty = match kernel_abi::abi_kind(cx.registry(), &info.return_type) {
        Some(AbiKind::Scalar(p)) => prim_to_clif(p),
        _ => types::I64,
    };
    cx.b.append_block_param(dmerge, types::I64);
    cx.b.append_block_param(dmerge, pay_ty);
    // The buf's in-flight cover ends here: the call path CONSUMES it,
    // the skip path drops it explicitly.
    cx.ctx.dyncall_buf_stack.borrow_mut().pop();
    cx.b.ins().brif(any_tainted, skip_bl, &[], call_bl, &[]);
    cx.b.switch_to_block(skip_bl);
    cx.b.seal_block(skip_bl);
    let buf_drop = cx.helper("graphix_value_buf_drop")?;
    cx.b.ins().call(buf_drop, &[buf]);
    let ph = emit_elem_placeholder(cx, &info.return_type)?;
    // Fires iff any arg fired; TAINT is already set on the placeholder.
    let ph_disc = propagate_flags(cx.b, ph.disc, &arg_taint_discs);
    cx.b.ins().jump(dmerge, &[BlockArg::Value(ph_disc), BlockArg::Value(ph.payload)]);
    cx.b.switch_to_block(call_bl);
    cx.b.seal_block(call_bl);
    let call = cx.b.ins().call(dyncall, &[fn_idx_val, buf, ret_kind_val]);
    // `graphix_dyncall` has two `I64` returns (disc, payload) so that
    // ret_kind=2 (Value-shape) can deliver both words. For
    // scalar/composite/unit/string return kinds the first return
    // holds the value (or pointer / 0); the second is undefined.
    let (raw0, raw1) = {
        let r = cx.b.inst_results(call);
        (r[0], r[1])
    };
    // Take AND clear the pending flag: set means THIS dispatch
    // returned no value ("bottom this cycle" — e.g. `buffer::encode`'s
    // Pad guard). Converted HERE to a #219 tainted result that
    // CONTINUES — the node-walk's builtin `None` silences only its
    // consumers, so the whole-kernel pending abort it used to trigger
    // killed unrelated outputs (soak jul05 item 28). Clearing keeps
    // the flag meaning "genuine abort" for `Kernel::update`.
    let pend = {
        let take = cx.helper("graphix_dyncall_pending_take_clear")?;
        let c = cx.b.ins().call(take, &[]);
        cx.b.inst_results(c)[0]
    };
    // Call-path results per return shape (args are proven untainted on
    // this path — the old post-call arg-taint aborts are gone).
    match kernel_abi::abi_kind(cx.registry(), &info.return_type) {
        Some(AbiKind::Scalar(p)) => {
            // Scalar return: the 0 sentinel on pending is a harmless
            // payload for downstream scalar arithmetic — no branch,
            // just fold the pend bit into TAINT.
            let value = cast_u64_to_prim(cx.b, raw0, p);
            let base = scalar_disc(cx.b, p);
            let disc = propagate_flags(cx.b, base, &arg_taint_discs);
            let disc = taint_if(cx.b, disc, pend);
            cx.b.ins().jump(dmerge, &[BlockArg::Value(disc), BlockArg::Value(value)]);
        }
        Some(AbiKind::Unit) => {
            // Unit return: dispatcher returned (0, _). The result is
            // discarded by the statement position; the pend bit still
            // rides so a bound unit local reads as bottom.
            let disc = cx.b.ins().iconst(types::I64, value_disc::NULL);
            let disc = taint_if(cx.b, disc, pend);
            cx.b.ins().jump(dmerge, &[BlockArg::Value(disc), BlockArg::Value(raw0)]);
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
            // Pointer-carrying / two-word returns: the pending
            // sentinel (null / `(0, 0)`) is NOT inert — every String
            // position assumes a valid owned ArcStr (#214), a null
            // `*mut ValArray` can't be deref'd, and a zero Value disc
            // is not a real tag. Branch: on pend, produce the same
            // tainted shape-safe placeholder the tainted-ARG skip
            // path produces and continue to the merge.
            let pend_bl = cx.b.create_block();
            let ok_bl = cx.b.create_block();
            cx.b.ins().brif(pend, pend_bl, &[], ok_bl, &[]);
            cx.b.switch_to_block(pend_bl);
            cx.b.seal_block(pend_bl);
            let ph = emit_elem_placeholder(cx, &info.return_type)?;
            let ph_disc = propagate_flags(cx.b, ph.disc, &arg_taint_discs);
            cx.b.ins()
                .jump(dmerge, &[BlockArg::Value(ph_disc), BlockArg::Value(ph.payload)]);
            cx.b.switch_to_block(ok_bl);
            cx.b.seal_block(ok_bl);
            let (disc, pay) = match kernel_abi::abi_kind(cx.registry(), &info.return_type)
            {
                Some(AbiKind::String) => {
                    // `raw0` is the ArcStr's raw thin-pointer bits
                    // (transferred ownership).
                    let base = cx.b.ins().iconst(types::I64, value_disc::STRING);
                    (propagate_flags(cx.b, base, &arg_taint_discs), raw0)
                }
                Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                    // `raw0` is an owned `*mut ValArray`.
                    let base = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
                    (propagate_flags(cx.b, base, &arg_taint_discs), raw0)
                }
                _ => {
                    // Value-shape: both register-words. `raw0` is a
                    // REAL (flag-free) Value disc from the dispatcher —
                    // fold the args' firing in like every other arm,
                    // else the result reads as always-fired: a const
                    // `buffer::from_string(...)`/`cast<T>(x)$` re-fired
                    // on every kernel wake, and a consume-always
                    // connect of one SELF-WOKE its own kernel forever
                    // (soak jul05 items 1/4/25 — the ExtraFire/Timeout
                    // family).
                    (propagate_flags(cx.b, raw0, &arg_taint_discs), raw1)
                }
            };
            cx.b.ins().jump(dmerge, &[BlockArg::Value(disc), BlockArg::Value(pay)]);
        }
        Some(AbiKind::Null) | None => {
            return Err(anyhow!(
                "DynCall with bare Null / non-fusable return — \
                 should have widened to Nullable<T> at construction"
            ));
        }
    }
    cx.b.switch_to_block(dmerge);
    cx.b.seal_block(dmerge);
    let params = cx.b.block_params(dmerge);
    let merged = CompiledExpr::new(params[0], params[1]);
    // Interior-bottom exactness (scalar returns): a pended dispatch, a
    // tainted-arg skip, with PRIOR success degrades to STALE + the
    // cached value — matching the node-walk, where consumers sample
    // the builtin node's cached last output when it doesn't fire.
    match kernel_abi::abi_kind(cx.registry(), &info.return_type) {
        Some(AbiKind::Scalar(p)) => Ok(emit_scalar_taint_cache(cx, p, merged)),
        _ => Ok(merged),
    }
}

/// How a `select`'s arms merge into one result — derived from the
/// select node's frozen result type. Scalar merges thread the disc
/// (carrying `TAINT`/`STALE`) through the arm phi alongside the payload,
/// so a tainted arm value propagates its bottom to the merged result.
/// String/composite merges have no such per-arm channel — a possibly-
/// bottom scrutinee with one of those result shapes refuses to fuse
/// instead.
#[derive(Clone, Copy)]
enum SelectMerge {
    Scalar(PrimType),
    Value,
    Composite,
    String,
}

/// The select scrutinee, emitted exactly ONCE up front; every arm
/// condition and pattern bind reuses these SSA values (SSA reuse
/// gives eval-once for free). `Opaque` (string / composite) supports
/// only Ignore / guard arms, none of which can test the value. The
/// `disc` always carries the scrutinee's #219 taint — OR-ed into every
/// arm's result so a bottom (missing) scrutinee bottoms the select.
#[derive(Clone, Copy)]
enum SelectScrut {
    Scalar {
        disc: ClifValue,
        value: ClifValue,
        prim: PrimType,
    },
    Value {
        disc: ClifValue,
        payload: ClifValue,
    },
    /// A BORROWED array/tuple/struct scrutinee: the `*const ValArray`
    /// stays live across the whole arm chain with no drop (the env slot
    /// owns it — the owned-producer case de-fuses in
    /// [`classify_select_scrutinee`]). Structural patterns
    /// (tuple/struct/slice) test and read elements through it.
    Composite {
        disc: ClifValue,
        ptr: ClifValue,
    },
    Opaque {
        disc: ClifValue,
    },
}

impl SelectScrut {
    fn disc(&self) -> ClifValue {
        match self {
            SelectScrut::Scalar { disc, .. }
            | SelectScrut::Value { disc, .. }
            | SelectScrut::Composite { disc, .. }
            | SelectScrut::Opaque { disc } => *disc,
        }
    }
}

/// Where a scalar pattern leaf lives in the composite scrutinee.
#[derive(Clone, Copy)]
enum ElemIdx {
    /// `a[idx]` — tuple / slice / slice-prefix leaves.
    FromStart(usize),
    /// `a[len - back]` — slice-suffix leaves (`len` is the scrutinee
    /// length SSA value read by the arm's structure condition).
    FromEnd { back: usize, len: ClifValue },
    /// `a[idx][1]` — a struct field's value (idx is the canonically-
    /// sorted field index resolved by typecheck).
    StructField(usize),
}

/// Read one scalar pattern leaf off the borrowed composite scrutinee.
/// Callers MUST have proven the arm's length test first — under a
/// tainted (missing) scrutinee the placeholder is an EMPTY array, and
/// these reads are unchecked.
fn read_scrut_elem(
    cx: &mut BodyCx,
    ptr: ClifValue,
    idx: ElemIdx,
    prim: PrimType,
) -> Result<ClifValue> {
    let (helper_name, idx_v) = match idx {
        ElemIdx::FromStart(j) => {
            (valarray_get_helper(prim)?, cx.b.ins().iconst(types::I64, j as i64))
        }
        ElemIdx::FromEnd { back, len } => {
            let b = cx.b.ins().iconst(types::I64, back as i64);
            (valarray_get_helper(prim)?, cx.b.ins().isub(len, b))
        }
        ElemIdx::StructField(i) => {
            (struct_get_helper(prim)?, cx.b.ins().iconst(types::I64, i as i64))
        }
    };
    let helper = cx.helper(helper_name)?;
    let call = cx.b.ins().call(helper, &[ptr, idx_v]);
    Ok(cx.b.inst_results(call)[0])
}

/// A pattern binding to install in the arm's matched region, under the
/// pattern's real `BindId` (the arm body's `Ref`s resolve BindId-first,
/// so no shadow guard is needed).
enum SelectArmBind {
    /// `n => ...` — bind the scalar scrutinee itself.
    Scrut(BindId),
    /// `` `Tag(n) `` — bind one scalar variant payload. The read uses
    /// `unreachable_unchecked` on a wrong-tag value, so it MUST be
    /// emitted inside the matched region (after the tag-eq branch) —
    /// never in the fall-through chain. (The node-walk evaluates binds
    /// only after the pattern matches — we follow the node-walk.)
    Payload { id: BindId, idx: usize, prim: PrimType },
    /// `(x, y)` / `{f, ..}` / `[h, ..]` — bind one scalar leaf of a
    /// composite scrutinee. `ptr` is the (borrowed) composite the leaf
    /// reads from: the scrutinee itself, or — for a NESTED pattern — a
    /// borrowed interior pointer read during the arm's structure
    /// condition. Emitted inside the matched region: the length tests
    /// (the structure condition stages) are the memory-safety gate — a
    /// tainted scrutinee's empty placeholder fails them, so the
    /// unchecked element read never touches the placeholder.
    Elem { id: BindId, idx: ElemIdx, prim: PrimType, ptr: ClifValue },
}

/// `select` at expression position — the Node twin of
/// `emit_select_as_expr` (lowering) + `compile_ifchain` (codegen)
/// fused into one pass. Canonical semantics are `Select::update` /
/// `PatternNode::is_match` (node/select.rs, node/pattern.rs):
///
/// - the scrutinee is evaluated once; no scrutinee value → no select
///   value (the scrutinee's disc `TAINT`/`STALE` folds into every arm's
///   result disc — the #178 scrutinee gate);
/// - an explicit type predicate is TESTED (`null as _` → IsNull;
///   `i64 as _` over `[i64, null]` → NOT-null), so arm order is right
///   by construction;
/// - a guard runs only after the pattern matches, with the pattern's
///   binds in scope; a bottom guard means the arm does NOT match;
/// - the first matching arm wins; an arm with no condition and no
///   guard takes the chain unconditionally.
///
/// The final-arm miss trap mirrors `compile_ifchain`, but is emitted
/// only where typecheck's exhaustiveness makes it unreachable: a
/// guarded final arm, or a conditional final arm under a possibly-
/// bottom scrutinee (whose garbage cond bits could miss every arm),
/// refuse to fuse instead.
pub(crate) fn emit_select_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    sel: &Select<R, E>,
) -> Result<CompiledExpr> {
    if sel.arms.is_empty() {
        return Err(anyhow!("emit_clif: select with no arms"));
    }
    let result_typ = kernel_abi::freeze_for_abi_normalized(cx.registry(), sel.typ())
        .ok_or_else(|| {
            anyhow!(
                "emit_clif: select result type {:?} doesn't freeze concrete",
                sel.typ()
            )
        })?;
    let merge_shape = match kernel_abi::abi_kind(cx.registry(), &result_typ) {
        Some(AbiKind::Scalar(p)) => SelectMerge::Scalar(p),
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => SelectMerge::Value,
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => SelectMerge::Composite,
        Some(AbiKind::String) => SelectMerge::String,
        other @ (Some(AbiKind::Unit | AbiKind::Null) | None) => {
            return Err(anyhow!(
                "emit_clif: select result shape {other:?} not representable"
            ));
        }
    };
    let (scrut, scrut_kind, scrut_typ, scrut_drop) =
        classify_select_scrutinee(cx, sel, true)?;
    // Every merge shape phis (disc, payload) — the scrutinee's taint
    // rides the disc into every arm result, so there's no separate
    // validity phi and no possibly-bottom-scrutinee gate (#219).
    let merge = cx.b.create_block();
    cx.b.append_block_param(merge, types::I64); // disc
    let payload_ty = match merge_shape {
        SelectMerge::Scalar(p) => prim_to_clif(p),
        _ => types::I64,
    };
    cx.b.append_block_param(merge, payload_ty); // payload
    let scrut_disc = scrut.disc();
    // A select FIRES iff its scrutinee OR the taken arm OR a GUARD
    // fired. Guards are evaluated inside the pattern chain (only on the
    // paths that reach them), so "a guard fired" is computed HERE, path-
    // independently, from the guards' FEEDERS: AND-reduce the STALE bit
    // of every kernel-visible local any arm's guard reads — the word is
    // STALE only if no guard input fired. (The node-walk re-matches on
    // any pattern/guard update, so any guard feeder firing can re-fire
    // the select.) This replaces the old force-the-result-FRESH
    // treatment of guarded selects, which fired the select on EVERY
    // kernel invocation — a `count` over a guarded select with an
    // unrelated reactive input in the region counted every event
    // (interp 1, jit 5).
    //
    // The guard term is further refined by SELECTION MEMORY when a
    // per-instance state word is available (root body, outside loops —
    // see `BodyCx::claim_state_word`): the node-walk emits on a
    // guard-only event only when the SELECTION changes (verified on
    // the node-walk 2026-07-03: guard feeder fires, same arm re-taken,
    // const body → NO emission; every selection change → emission), so
    // each taken arm compares-and-records its index and the guard term
    // fires only on a change (fuzz/triage-fuzzer-v2/firing_000005).
    // Without a word (loop/callee context) the unrefined guard term
    // stands — a deliberate residual duplicate-fire, never a wrong
    // value.
    let has_guard = sel.arms.iter().any(|(pat, _)| pat.guard.is_some());
    let guard_stale: Option<ClifValue> = if has_guard {
        let mut ids: Vec<BindId> = Vec::new();
        for (pat, _) in sel.arms.iter() {
            if let Some(g) = &pat.guard {
                let mut refs = crate::Refs::default();
                g.node.refs(&mut refs);
                refs.with_external_refs(|id| {
                    if !ids.contains(&id) {
                        ids.push(id);
                    }
                });
            }
        }
        let mut word = cx.b.ins().iconst(types::I64, STALE);
        for id in ids {
            // Arm binds aren't in the env yet (the chain binds them per
            // arm) — their firing is the scrutinee's, already folded.
            if let Some(dv) = cx.env.lookup_by_id(id).map(|l| l.vv.disc) {
                let d = cx.b.use_var(dv);
                let sb = cx.b.ins().band_imm(d, STALE);
                word = cx.b.ins().band(word, sb);
            }
        }
        Some(word)
    } else {
        None
    };
    let sel_state_off = if has_guard { cx.claim_state_word() } else { None };
    let arm_index = std::cell::Cell::new(0usize);
    emit_select_arms(
        cx,
        sel,
        scrut,
        scrut_kind,
        &scrut_typ,
        &mut |cx, body, mark| {
            let idx = arm_index.get();
            arm_index.set(idx + 1);
            emit_select_value_arm(
                cx,
                body,
                mark,
                merge_shape,
                merge,
                scrut_disc,
                guard_stale,
                sel_state_off.map(|off| (off, idx)),
            )
        },
        &mut |cx| emit_select_miss_value(cx, merge_shape, merge),
    )?;
    cx.b.switch_to_block(merge);
    cx.b.seal_block(merge);
    let (rdisc, rpayload) = {
        let params = cx.b.block_params(merge);
        (params[0], params[1])
    };
    // Discharge an OWNED scrutinee: every normal path (each arm + the
    // miss trap) crosses the merge, so this drops exactly once; a
    // mid-arm pending exit dropped it as an env local instead. Unbind
    // (truncate) so a LATER pending exit elsewhere in the kernel can't
    // double-drop the already-freed value.
    if let Some(ScrutDrop { kind, vv, mark }) = scrut_drop {
        match kind {
            LocalKind::Composite => {
                let drop = cx.helper("graphix_valarray_drop")?;
                let p = cx.b.use_var(vv.payload);
                cx.b.ins().call(drop, &[p]);
            }
            LocalKind::Variant | LocalKind::Nullable | LocalKind::Value => {
                let drop = cx.helper("graphix_value_drop")?;
                let d = cx.b.use_var(vv.disc);
                let p = cx.b.use_var(vv.payload);
                cx.b.ins().call(drop, &[d, p]);
            }
            LocalKind::Scalar(_) | LocalKind::String => {
                return Err(anyhow!(
                    "emit_clif: scrutinee drop obligation of shape {kind:?} — \
                     classify bug"
                ));
            }
        }
        cx.env.truncate(mark);
    }
    Ok(CompiledExpr::new(rdisc, rpayload))
}

/// The final-arm fail block of a VALUE-position select: reached only
/// when a tainted (missing) scrutinee misses every conditional arm.
/// Jump to the merge with a drop-safe tainted bottom — the disc's TAINT
/// forces a bottom at the output, and the payload is a valid empty
/// allocation so a scope-exit drop is null-safe.
fn emit_select_miss_value(
    cx: &mut BodyCx,
    merge_shape: SelectMerge,
    merge: Block,
) -> Result<()> {
    let (disc, payload) = match merge_shape {
        SelectMerge::Scalar(p) => {
            let d = cx.b.ins().iconst(types::I64, prim_to_value_disc(p) | TAINT);
            // `zero_const`, NOT `iconst(prim_to_clif(p))`: an `iconst.f64`
            // is invalid CLIF (a verifier panic) — a float-result select
            // with a conditional final arm reaches this trap.
            let z = zero_const(cx.b, p);
            (d, z)
        }
        SelectMerge::Value => {
            let d = cx.b.ins().iconst(types::I64, value_disc::NULL | TAINT);
            let z = cx.b.ins().iconst(types::I64, 0);
            (d, z)
        }
        SelectMerge::Composite => {
            let buf_new = cx.helper("graphix_value_buf_new")?;
            let zero = cx.b.ins().iconst(types::I64, 0);
            let call = cx.b.ins().call(buf_new, &[zero]);
            let buf = cx.b.inst_results(call)[0];
            let fin = cx.helper("graphix_valarray_finalize")?;
            let call = cx.b.ins().call(fin, &[buf]);
            let arr = cx.b.inst_results(call)[0];
            let d = cx.b.ins().iconst(types::I64, value_disc::ARRAY | TAINT);
            (d, arr)
        }
        SelectMerge::String => {
            let buf_new = cx.helper("graphix_string_buf_new")?;
            let call = cx.b.ins().call(buf_new, &[]);
            let buf = cx.b.inst_results(call)[0];
            let fin = cx.helper("graphix_string_buf_finalize")?;
            let call = cx.b.ins().call(fin, &[buf]);
            let s = cx.b.inst_results(call)[0];
            let d = cx.b.ins().iconst(types::I64, value_disc::STRING | TAINT);
            (d, s)
        }
    };
    cx.b.ins().jump(merge, &[BlockArg::Value(disc), BlockArg::Value(payload)]);
    Ok(())
}

/// What the value-position select owes at its merge point for an OWNED
/// (fresh-producer) scrutinee: the scrutinee was bound as an env local
/// (so a mid-arm pending exit drops it via `drop_owned_composites`), and
/// the merge emits the normal-path drop then unbinds it (the env mark) —
/// exactly once on either path.
struct ScrutDrop {
    kind: LocalKind,
    vv: ValueVar,
    mark: usize,
}

/// Classify (and emit the read of) a select scrutinee: the shared
/// prologue of the value-position and tail-position select emitters.
/// `allow_owned`: the value-position caller has a single merge point
/// every path crosses, so it can accept an OWNED composite/Value
/// scrutinee and discharge the returned [`ScrutDrop`] there; the
/// tail-position caller's arms terminate individually (no merge), so it
/// passes `false` and owned scrutinees keep de-fusing.
fn classify_select_scrutinee<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    sel: &Select<R, E>,
    allow_owned: bool,
) -> Result<(SelectScrut, AbiKind, Type, Option<ScrutDrop>)> {
    let scrut_typ =
        kernel_abi::freeze_for_abi_normalized(cx.registry(), sel.arg.node.typ())
            .ok_or_else(|| {
                anyhow!(
                    "emit_clif: select scrutinee type {:?} doesn't freeze \
                 concrete",
                    sel.arg.node.typ()
                )
            })?;
    let scrut_kind = kernel_abi::abi_kind(cx.registry(), &scrut_typ)
        .ok_or_else(|| anyhow!("emit_clif: select scrutinee shape not classifiable"))?;
    let mut drop_ob: Option<ScrutDrop> = None;
    // Bind an OWNED scrutinee as an env local of its kind: a mid-arm
    // pending exit drops it via `drop_owned_composites`, and the caller
    // discharges the ScrutDrop (drop + unbind) at the merge on the
    // normal path — exactly once on either path.
    let adopt = |cx: &mut BodyCx,
                 kind: LocalKind,
                 disc: ClifValue,
                 payload: ClifValue|
     -> Option<ScrutDrop> {
        let mark = cx.env.mark();
        let name: ArcStr = compact_str::format_compact!("__scrut{}", sel.spec.id.inner())
            .as_str()
            .into();
        let vv = bind_local(cx, name, disc, payload, kind, None);
        Some(ScrutDrop { kind, vv, mark })
    };
    let scrut = match scrut_kind {
        AbiKind::Scalar(p) => {
            let cv = sel.arg.node.emit_clif(cx)?;
            SelectScrut::Scalar { disc: cv.disc, value: cv.payload, prim: p }
        }
        AbiKind::Variant | AbiKind::Nullable | AbiKind::Value => {
            // The (disc, payload) pair stays live across the whole arm
            // chain: a BORROWED env slot needs nothing; an OWNED
            // producer needs the merge-point drop (value position only).
            let owned = node_composite_source(&sel.arg.node) != CompositeSource::Borrowed;
            if owned && !allow_owned {
                return Err(anyhow!(
                    "emit_clif: owned value-shape select scrutinee in tail \
                     position — no merge point to drop at"
                ));
            }
            let cv = sel.arg.node.emit_clif(cx)?;
            if owned {
                let kind = match scrut_kind {
                    AbiKind::Variant => LocalKind::Variant,
                    AbiKind::Nullable => LocalKind::Nullable,
                    _ => LocalKind::Value,
                };
                drop_ob = adopt(cx, kind, cv.disc, cv.payload);
            }
            SelectScrut::Value { disc: cv.disc, payload: cv.payload }
        }
        // A composite scrutinee keeps its pointer live across the arm
        // chain: borrowed = env-owned; owned = merge-point drop.
        AbiKind::Array | AbiKind::Tuple | AbiKind::Struct => {
            let owned = node_composite_source(&sel.arg.node) != CompositeSource::Borrowed;
            if owned && !allow_owned {
                return Err(anyhow!(
                    "emit_clif: owned composite select scrutinee in tail \
                     position — no merge point to drop at"
                ));
            }
            let cv = sel.arg.node.emit_clif(cx)?;
            if owned {
                drop_ob = adopt(cx, LocalKind::Composite, cv.disc, cv.payload);
            }
            SelectScrut::Composite { disc: cv.disc, ptr: cv.payload }
        }
        // A String scrutinee supports only Ignore / guard arms (no
        // condition can test it); we read it only for its disc (#219
        // taint). The read is an owned ArcStr either way (a borrowed
        // slot read CLONES, an owned producer transfers) — drop it
        // immediately, keeping only the disc. No cross-arm retention,
        // so owned strings are fine in both positions.
        AbiKind::String => {
            let cv = sel.arg.node.emit_clif(cx)?;
            let drop = cx.helper("graphix_arcstr_drop")?;
            cx.b.ins().call(drop, &[cv.payload]);
            SelectScrut::Opaque { disc: cv.disc }
        }
        AbiKind::Unit | AbiKind::Null => {
            return Err(anyhow!("emit_clif: select scrutinee of shape {scrut_kind:?}"));
        }
    };
    Ok((scrut, scrut_kind, scrut_typ, drop_ob))
}

/// Structure condition + scalar leaf binds for a tuple/struct/slice
/// pattern over a BORROWED composite scrutinee. Mirrors
/// `StructPatternNode::is_match` / `bind` (node/pattern.rs — the
/// canonical semantics):
///
/// - Slice (tuple or array literal pattern): `len == N`, leaves at
///   `a[j]`;
/// - SlicePrefix: `len >= N`, leaves at `a[j]`;
/// - SliceSuffix: `len >= N`, leaves at `a[len - (N - j)]`;
/// - Struct: `len >= N`, leaf values at `a[i][1]` (canonically-sorted
///   field index from typecheck).
///
/// The LENGTH test is also the taint gate: a missing (#219) composite
/// input is an EMPTY placeholder array, so every length test with
/// `N > 0` fails under taint and the unchecked element reads (emitted
/// after the test) never touch the placeholder — the chain falls
/// through to the final-arm miss trap, which produces the tainted
/// bottom. Literal leaves are tested in a second block AFTER the
/// length branch (same reason); `Bind` leaves are recorded in `binds`
/// and read in the matched region.
///
/// Deferred (Err → the select de-fuses, node-walks): whole-composite
/// `@` bindings, named prefix/suffix rest bindings (both allocate an
/// owned composite local inside the arm — `JitEnv::truncate` emits no
/// drops, so they'd leak on the normal path), non-scalar leaves, and
/// nested structural leaves.
fn emit_composite_pattern_cond(
    cx: &mut BodyCx,
    ptr: ClifValue,
    scrut_typ: &Type,
    pat: &StructPatternNode,
    fail: Block,
    binds: &mut Vec<SelectArmBind>,
) -> Result<ClifValue> {
    // The length read up front (safe on the empty taint placeholder) —
    // suffix leaves index relative to it.
    let len_helper = cx.helper("graphix_valarray_len")?;
    let call = cx.b.ins().call(len_helper, &[ptr]);
    let len = cx.b.inst_results(call)[0];
    // (leaf position, sub-pattern, element type) + the length compare.
    let styp = resolve_node_typ(cx.ctx, scrut_typ);
    struct LeafSpec<'p> {
        idx: ElemIdx,
        sub: &'p StructPatternNode,
        typ: Type,
    }
    let (leaves, len_cc, n): (Vec<LeafSpec>, IntCC, usize) = match pat {
        StructPatternNode::Slice { tuple, all, binds: pbinds } => {
            if all.is_some() {
                return Err(anyhow!(
                    "emit_clif: whole-slice @ binding not lowerable (owned \
                     composite arm local)"
                ));
            }
            let elt = |j: usize| -> Result<Type> {
                if *tuple {
                    match &styp {
                        Type::Tuple(elts) if elts.len() == pbinds.len() => {
                            Ok(elts[j].clone())
                        }
                        t => Err(anyhow!(
                            "emit_clif: tuple pattern over non-tuple \
                             scrutinee {t:?}"
                        )),
                    }
                } else {
                    match &styp {
                        Type::Array(t) => Ok((**t).clone()),
                        t => Err(anyhow!(
                            "emit_clif: slice pattern over non-array \
                             scrutinee {t:?}"
                        )),
                    }
                }
            };
            let leaves = pbinds
                .iter()
                .enumerate()
                .map(|(j, sub)| {
                    Ok(LeafSpec { idx: ElemIdx::FromStart(j), sub, typ: elt(j)? })
                })
                .collect::<Result<Vec<_>>>()?;
            (leaves, IntCC::Equal, pbinds.len())
        }
        StructPatternNode::SlicePrefix { all, prefix, tail } => {
            if all.is_some() || tail.is_some() {
                return Err(anyhow!(
                    "emit_clif: slice-prefix @/rest binding not lowerable \
                     (owned subslice arm local)"
                ));
            }
            let t = match &styp {
                Type::Array(t) => (**t).clone(),
                t => {
                    return Err(anyhow!(
                        "emit_clif: slice pattern over non-array scrutinee {t:?}"
                    ));
                }
            };
            let leaves = prefix
                .iter()
                .enumerate()
                .map(|(j, sub)| LeafSpec {
                    idx: ElemIdx::FromStart(j),
                    sub,
                    typ: t.clone(),
                })
                .collect();
            (leaves, IntCC::SignedGreaterThanOrEqual, prefix.len())
        }
        StructPatternNode::SliceSuffix { all, head, suffix } => {
            if all.is_some() || head.is_some() {
                return Err(anyhow!(
                    "emit_clif: slice-suffix @/head binding not lowerable \
                     (owned subslice arm local)"
                ));
            }
            let t = match &styp {
                Type::Array(t) => (**t).clone(),
                t => {
                    return Err(anyhow!(
                        "emit_clif: slice pattern over non-array scrutinee {t:?}"
                    ));
                }
            };
            // suffix leaf j lives at a[len - (N - j)].
            let n = suffix.len();
            let leaves = suffix
                .iter()
                .enumerate()
                .map(|(j, sub)| LeafSpec {
                    idx: ElemIdx::FromEnd { back: n - j, len },
                    sub,
                    typ: t.clone(),
                })
                .collect();
            (leaves, IntCC::SignedGreaterThanOrEqual, n)
        }
        StructPatternNode::Struct { all, binds: sbinds } => {
            if all.is_some() {
                return Err(anyhow!(
                    "emit_clif: whole-struct @ binding not lowerable (owned \
                     composite arm local)"
                ));
            }
            let flds = match &styp {
                Type::Struct(flds) => flds,
                t => {
                    return Err(anyhow!(
                        "emit_clif: struct pattern over non-struct scrutinee {t:?}"
                    ));
                }
            };
            let leaves = sbinds
                .iter()
                .map(|(_, i, sub)| {
                    let typ = flds.get(*i).map(|(_, t)| t.clone()).ok_or_else(|| {
                        anyhow!(
                            "emit_clif: struct pattern field index {i} out \
                                 of range"
                        )
                    })?;
                    Ok(LeafSpec { idx: ElemIdx::StructField(*i), sub, typ })
                })
                .collect::<Result<Vec<_>>>()?;
            (leaves, IntCC::SignedGreaterThanOrEqual, sbinds.len())
        }
        _ => return Err(anyhow!("emit_clif: not a composite structural pattern")),
    };
    // Classify each leaf BEFORE emitting anything (an Err mid-emission
    // would abandon the kernel build — fine — but classify-first keeps
    // the failure cheap and the emission below straight-line).
    let mut lit_leaves: Vec<(ElemIdx, PrimType, &Value)> = Vec::new();
    let mut nested: Vec<(ElemIdx, &StructPatternNode, Type)> = Vec::new();
    for leaf in &leaves {
        match leaf.sub {
            StructPatternNode::Ignore => {}
            StructPatternNode::Bind(id) => {
                let prim = kernel_abi::scalar_prim(cx.registry(), &leaf.typ).ok_or_else(
                    || {
                        anyhow!(
                            "emit_clif: non-scalar select pattern leaf bind {:?}",
                            leaf.typ
                        )
                    },
                )?;
                binds.push(SelectArmBind::Elem { id: *id, idx: leaf.idx, prim, ptr });
            }
            StructPatternNode::Literal(v) => {
                let prim = kernel_abi::scalar_prim_of_value(v).ok_or_else(|| {
                    anyhow!("emit_clif: non-scalar literal pattern leaf {v:?}")
                })?;
                lit_leaves.push((leaf.idx, prim, v));
            }
            sub @ (StructPatternNode::Slice { .. }
            | StructPatternNode::SlicePrefix { .. }
            | StructPatternNode::SliceSuffix { .. }
            | StructPatternNode::Struct { .. }) => {
                // A NESTED structural pattern over a composite-shaped
                // leaf recurses through a BORROWED interior pointer —
                // read after this level's length test (below).
                match kernel_abi::abi_kind(cx.registry(), &leaf.typ) {
                    Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                        nested.push((leaf.idx, sub, leaf.typ.clone()));
                    }
                    other => {
                        return Err(anyhow!(
                            "emit_clif: nested pattern over a leaf of shape \
                             {other:?} not lowerable"
                        ));
                    }
                }
            }
            StructPatternNode::Variant { .. } => {
                return Err(anyhow!(
                    "emit_clif: nested variant pattern leaf not lowerable"
                ));
            }
        }
    }
    // The length test for THIS level.
    let n_c = cx.b.ins().iconst(types::I64, n as i64);
    let len_ok = cx.b.ins().icmp(len_cc, len, n_c);
    if lit_leaves.is_empty() && nested.is_empty() {
        // Nothing to read before the matched region — the length test
        // IS the condition (the caller's arm brif consumes it).
        return Ok(len_ok);
    }
    // Reads happen below, so the length must be proven FIRST: branch to
    // a staging block (the reads are unchecked — see the taint note
    // above), then test literal leaves and recurse into nested patterns.
    let stage = cx.b.create_block();
    cx.b.ins().brif(len_ok, stage, &[], fail, &[]);
    cx.b.switch_to_block(stage);
    cx.b.seal_block(stage);
    let mut cond: Option<ClifValue> = None;
    let mut fold = |cx: &mut BodyCx, c: ClifValue| {
        cond = Some(match cond {
            None => c,
            Some(p) => cx.b.ins().band(p, c),
        });
    };
    for (idx, prim, v) in lit_leaves {
        let elem = read_scrut_elem(cx, ptr, idx, prim)?;
        let lit = compile_const(cx.b, v, prim)?;
        let c = compile_cmp(cx.b, CmpOp::Eq, prim, elem, lit);
        fold(cx, c);
    }
    for (idx, sub, typ) in nested {
        // Borrowed interior pointer into this level's element slot —
        // stable for the whole arm chain (the root scrutinee is a
        // pinned borrowed env slot and values are immutable), so the
        // recursion's reads and the matched-region leaf binds need no
        // ownership or drops.
        let (helper_name, idx_v) = match idx {
            ElemIdx::FromStart(j) => (
                "graphix_valarray_get_array_borrowed",
                cx.b.ins().iconst(types::I64, j as i64),
            ),
            ElemIdx::FromEnd { back, len } => {
                let b = cx.b.ins().iconst(types::I64, back as i64);
                ("graphix_valarray_get_array_borrowed", cx.b.ins().isub(len, b))
            }
            ElemIdx::StructField(i) => (
                "graphix_struct_get_array_borrowed",
                cx.b.ins().iconst(types::I64, i as i64),
            ),
        };
        let helper = cx.helper(helper_name)?;
        let call = cx.b.ins().call(helper, &[ptr, idx_v]);
        let child_ptr = cx.b.inst_results(call)[0];
        let c = emit_composite_pattern_cond(cx, child_ptr, &typ, sub, fail, binds)?;
        fold(cx, c);
    }
    Ok(cond.expect("staged composite pattern with no conditions"))
}

/// The shared select arm chain: pattern conditions (type predicate /
/// structure / guard), per-arm binds, and the fail-block plumbing —
/// identical between value position (arms widen and jump to a merge
/// block) and tail position (arms terminate with a return or a self
/// tail-call jump). `emit_arm` supplies the position-specific arm-body
/// emission; it runs in the matched block with the arm's binds
/// installed and MUST leave the block terminated (jump or return).
/// `mark` is the env state to truncate back to after the body.
fn emit_select_arms<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    sel: &Select<R, E>,
    scrut: SelectScrut,
    scrut_kind: AbiKind,
    scrut_typ: &Type,
    emit_arm: &mut dyn FnMut(&mut BodyCx, &Cached<R, E>, usize) -> Result<()>,
    // The final-arm miss handler (reached only under a tainted
    // scrutinee): value position jumps to the merge with a tainted
    // bottom; tail position sets pending and exits.
    emit_miss: &mut dyn FnMut(&mut BodyCx) -> Result<()>,
) -> Result<()> {
    use StructPatternNode;
    let n = sel.arms.len();
    for (i, (pat, body)) in sel.arms.iter().enumerate() {
        let is_last = i == n - 1;
        // Type-predicate condition. The node-walk tests the predicate
        // only when it's explicit (`PatternNode::is_match`); an
        // inferred predicate imposes no runtime test.
        let tcond: Option<ClifValue> = if !pat.explicit_type_predicate {
            None
        } else {
            let pred = kernel_abi::freeze_for_abi(cx.registry(), &pat.type_predicate)
                .ok_or_else(|| {
                    anyhow!(
                        "emit_clif: select type predicate {:?} doesn't \
                         freeze concrete",
                        pat.type_predicate
                    )
                })?;
            match &pred {
                Type::Primitive(p)
                    if p.contains(netidx_value::Typ::Null) && p.iter().count() == 1 =>
                {
                    match scrut {
                        SelectScrut::Value { disc, .. }
                            if matches!(scrut_kind, AbiKind::Nullable) =>
                        {
                            // Mask taint before the structural compare —
                            // a tainted disc is not a clean tag (#219).
                            let cd = clean_disc(cx.b, disc);
                            Some(cx.b.ins().icmp_imm(IntCC::Equal, cd, value_disc::NULL))
                        }
                        _ => {
                            return Err(anyhow!(
                                "emit_clif: null predicate over non-\
                                 Nullable scrutinee {scrut_typ:?}"
                            ));
                        }
                    }
                }
                Type::Primitive(p)
                    if !p.contains(netidx_value::Typ::Null) && p.iter().count() == 1 =>
                {
                    let pt = p.iter().next().unwrap();
                    match scrut {
                        SelectScrut::Scalar { prim, .. }
                            if PrimType::from_typ(pt) == Some(prim) =>
                        {
                            None
                        }
                        SelectScrut::Value { disc, .. }
                            if matches!(scrut_kind, AbiKind::Nullable)
                                && kernel_abi::nullable_inner(
                                    cx.registry(),
                                    &scrut_typ,
                                )
                                .as_ref()
                                .and_then(|t| {
                                    kernel_abi::scalar_prim(cx.registry(), t)
                                }) == PrimType::from_typ(pt) =>
                        {
                            // `[T, null]` runtime value is T or null,
                            // so "is a T" ≡ "is not null" — tested,
                            // not assumed (order-sound). Mask taint
                            // before the structural compare (#219).
                            let cd = clean_disc(cx.b, disc);
                            Some(cx.b.ins().icmp_imm(
                                IntCC::NotEqual,
                                cd,
                                value_disc::NULL,
                            ))
                        }
                        _ => {
                            return Err(anyhow!(
                                "emit_clif: type predicate {pred:?} over \
                                 scrutinee {scrut_typ:?} not lowerable"
                            ));
                        }
                    }
                }
                _ => {
                    return Err(anyhow!(
                        "emit_clif: type predicate {pred:?} not lowerable"
                    ));
                }
            }
        };
        // A composite structural pattern (tuple/struct/slice) stages its
        // condition across blocks (length branch, then literal-leaf
        // tests), so its fail edge must exist BEFORE the condition is
        // emitted — pre-create it (block creation order is free).
        let composite_structural = matches!(
            &pat.structure_predicate,
            StructPatternNode::Slice { .. }
                | StructPatternNode::SlicePrefix { .. }
                | StructPatternNode::SliceSuffix { .. }
                | StructPatternNode::Struct { .. }
        ) && matches!(scrut, SelectScrut::Composite { .. });
        let early_fail =
            if composite_structural { Some(cx.b.create_block()) } else { None };
        // Structure condition + the binds to install once matched.
        let mut binds: Vec<SelectArmBind> = Vec::new();
        let scond: Option<ClifValue> = match &pat.structure_predicate {
            StructPatternNode::Ignore => None,
            StructPatternNode::Bind(id) => match scrut {
                SelectScrut::Scalar { .. } => {
                    binds.push(SelectArmBind::Scrut(*id));
                    None
                }
                SelectScrut::Value { .. }
                | SelectScrut::Composite { .. }
                | SelectScrut::Opaque { .. } => {
                    return Err(anyhow!(
                        "emit_clif: non-scalar scrutinee bind pattern not \
                         yet lowerable"
                    ));
                }
            },
            StructPatternNode::Literal(v) => {
                let lit_prim = kernel_abi::scalar_prim_of_value(v).ok_or_else(|| {
                    anyhow!("emit_clif: non-scalar literal pattern {v:?}")
                })?;
                match scrut {
                    SelectScrut::Scalar { value, prim, .. } if prim == lit_prim => {
                        let lit = compile_const(cx.b, v, lit_prim)?;
                        Some(compile_cmp(cx.b, CmpOp::Eq, lit_prim, value, lit))
                    }
                    _ => {
                        return Err(anyhow!(
                            "emit_clif: literal pattern prim {lit_prim:?} \
                             doesn't match scrutinee {scrut_typ:?}"
                        ));
                    }
                }
            }
            StructPatternNode::Variant { tag, all, binds: pbinds } => {
                if all.is_some() {
                    return Err(anyhow!(
                        "emit_clif: whole-variant @ binding not lowerable"
                    ));
                }
                let (disc, payload) = match scrut {
                    SelectScrut::Value { disc, payload }
                        if matches!(scrut_kind, AbiKind::Variant) =>
                    {
                        (disc, payload)
                    }
                    _ => {
                        return Err(anyhow!(
                            "emit_clif: variant pattern over non-variant \
                             scrutinee {scrut_typ:?}"
                        ));
                    }
                };
                // Payload types come from the arm's own (frozen)
                // type predicate — `Variant(tag, elts)` for exactly
                // this arm.
                let pred = kernel_abi::freeze_for_abi(cx.registry(), &pat.type_predicate)
                    .ok_or_else(|| {
                        anyhow!(
                            "emit_clif: variant pattern predicate {:?} \
                             doesn't freeze concrete",
                            pat.type_predicate
                        )
                    })?;
                let elts = match &pred {
                    Type::Variant(ptag, elts)
                        if ptag == tag && elts.len() == pbinds.len() =>
                    {
                        elts
                    }
                    _ => {
                        return Err(anyhow!(
                            "emit_clif: variant pattern `{tag}` doesn't \
                             match its predicate {pred:?}"
                        ));
                    }
                };
                for (idx, (sub, elt)) in pbinds.iter().zip(elts.iter()).enumerate() {
                    match sub {
                        StructPatternNode::Bind(id) => {
                            let prim = kernel_abi::scalar_prim(cx.registry(), elt)
                                .ok_or_else(|| {
                                    anyhow!(
                                        "emit_clif: non-scalar variant \
                                         payload {elt:?}"
                                    )
                                })?;
                            binds.push(SelectArmBind::Payload { id: *id, idx, prim });
                        }
                        StructPatternNode::Ignore => {}
                        StructPatternNode::Literal(_)
                        | StructPatternNode::Slice { .. }
                        | StructPatternNode::SlicePrefix { .. }
                        | StructPatternNode::SliceSuffix { .. }
                        | StructPatternNode::Struct { .. }
                        | StructPatternNode::Variant { .. } => {
                            return Err(anyhow!(
                                "emit_clif: nested variant payload \
                                 pattern not lowerable"
                            ));
                        }
                    }
                }
                let tag_ptr = cx.interned_str(tag);
                let helper = cx.helper("graphix_variant_tag_eq")?;
                // Mask taint — the helper reads the disc as a clean tag.
                let call = cx.b.ins().call(helper, &[disc, payload, tag_ptr]);
                Some(cx.b.inst_results(call)[0])
            }
            p @ (StructPatternNode::Slice { .. }
            | StructPatternNode::SlicePrefix { .. }
            | StructPatternNode::SliceSuffix { .. }
            | StructPatternNode::Struct { .. }) => match scrut {
                SelectScrut::Composite { ptr, .. } => {
                    if tcond.is_some() {
                        return Err(anyhow!(
                            "emit_clif: explicit type predicate on a \
                             structural composite pattern not lowerable"
                        ));
                    }
                    Some(emit_composite_pattern_cond(
                        cx,
                        ptr,
                        scrut_typ,
                        p,
                        early_fail.unwrap(),
                        &mut binds,
                    )?)
                }
                _ => {
                    return Err(anyhow!(
                        "emit_clif: slice/tuple/struct select pattern over a \
                         non-composite scrutinee not lowerable"
                    ));
                }
            },
        };
        let pcond = match (tcond, scond) {
            (None, None) => None,
            (Some(c), None) | (None, Some(c)) => Some(c),
            (Some(a), Some(b)) => Some(cx.b.ins().band(a, b)),
        };
        let has_guard = pat.guard.is_some();
        // The final-arm miss trap below is sound only when typecheck's
        // exhaustiveness makes a miss impossible. A guarded final arm
        // (typecheck forbids it today — defensive) or garbage cond
        // bits from a possibly-bottom scrutinee could miss every arm.
        if is_last && has_guard {
            return Err(anyhow!(
                "emit_clif: guard on the final select arm — the chain \
                 could miss every arm"
            ));
        }
        // #219: a conditional final arm CAN miss every arm under a
        // tainted (missing) scrutinee. That's no longer a refusal — the
        // final fail block runs `emit_miss` (a tainted bottom), which is
        // dead code for an exhaustive non-tainted scrutinee.
        let matched = cx.b.create_block();
        let fail: Option<Block> = match early_fail {
            Some(f) => Some(f),
            None if pcond.is_some() || has_guard => Some(cx.b.create_block()),
            None => None,
        };
        match pcond {
            Some(c) => {
                cx.b.ins().brif(c, matched, &[], fail.unwrap(), &[]);
            }
            None => {
                cx.b.ins().jump(matched, &[]);
            }
        }
        cx.b.switch_to_block(matched);
        cx.b.seal_block(matched);
        let mark = cx.env.mark();
        for bind in &binds {
            match bind {
                SelectArmBind::Scrut(id) => {
                    let SelectScrut::Scalar { disc, value, prim } = scrut else {
                        return Err(anyhow!(
                            "emit_clif: scrutinee bind without a scalar \
                             scrutinee"
                        ));
                    };
                    let name: ArcStr =
                        compact_str::format_compact!("__pat{}", id.inner())
                            .as_str()
                            .into();
                    // The bound local carries the scrutinee's taint in its
                    // disc (#219).
                    bind_local(cx, name, disc, value, LocalKind::Scalar(prim), Some(*id));
                }
                SelectArmBind::Payload { id, idx, prim } => {
                    let SelectScrut::Value { disc, payload } = scrut else {
                        return Err(anyhow!(
                            "emit_clif: payload bind without a variant \
                             scrutinee"
                        ));
                    };
                    let helper = cx.helper(variant_payload_helper(*prim)?)?;
                    let idx_c = cx.b.ins().iconst(types::I64, *idx as i64);
                    // Clean the scrutinee disc for the payload read; the
                    // payload inherits the variant's taint.
                    let call = cx.b.ins().call(helper, &[disc, payload, idx_c]);
                    let v = cx.b.inst_results(call)[0];
                    let name: ArcStr =
                        compact_str::format_compact!("__pat{}", id.inner())
                            .as_str()
                            .into();
                    // The bound payload fires iff its variant scrutinee
                    // fired — inherit the scrutinee's STALE (and taint), so
                    // an arm body reading it stays faithful.
                    let base = scalar_disc(cx.b, *prim);
                    let pdisc = propagate_flags(cx.b, base, &[disc]);
                    bind_local(cx, name, pdisc, v, LocalKind::Scalar(*prim), Some(*id));
                }
                SelectArmBind::Elem { id, idx, prim, ptr } => {
                    let SelectScrut::Composite { disc, .. } = scrut else {
                        return Err(anyhow!(
                            "emit_clif: element bind without a composite \
                             scrutinee"
                        ));
                    };
                    // Safe here: the arm's length tests (the structure
                    // condition stages guarding this matched region) proved
                    // the element exists — a tainted scrutinee's empty
                    // placeholder failed them.
                    let v = read_scrut_elem(cx, *ptr, *idx, *prim)?;
                    let name: ArcStr =
                        compact_str::format_compact!("__pat{}", id.inner())
                            .as_str()
                            .into();
                    // The bound leaf fires iff its composite scrutinee
                    // fired — inherit the scrutinee's STALE (and taint).
                    let base = scalar_disc(cx.b, *prim);
                    let pdisc = propagate_flags(cx.b, base, &[disc]);
                    bind_local(cx, name, pdisc, v, LocalKind::Scalar(*prim), Some(*id));
                }
            }
        }
        if let Some(g) = &pat.guard {
            // Canonical guard semantics: evaluated only after the
            // pattern matched, with the binds in scope; a bottom guard
            // (`valid` = 0) means the arm does NOT match.
            let gcv = g.node.emit_clif(cx)?;
            // A bottom guard (tainted disc) means the arm does NOT match.
            let valid = is_untainted(cx.b, gcv.disc);
            let eff = cx.b.ins().band(gcv.payload, valid);
            let body_blk = cx.b.create_block();
            cx.b.ins().brif(eff, body_blk, &[], fail.unwrap(), &[]);
            cx.b.switch_to_block(body_blk);
            cx.b.seal_block(body_blk);
        }
        emit_arm(cx, body, mark)?;
        match fail {
            Some(f) => {
                cx.b.switch_to_block(f);
                cx.b.seal_block(f);
                if is_last {
                    // Reached only under a tainted (missing) scrutinee —
                    // produce a tainted bottom (#219). Dead code for an
                    // exhaustive non-tainted scrutinee.
                    emit_miss(cx)?;
                }
            }
            // An unconditional arm consumed control flow; any
            // remaining arms are unreachable (typecheck's dead-arm
            // check forbids them anyway). Mirrors `compile_ifchain`.
            None => break,
        }
    }
    Ok(())
}

/// Value-position arm-body emission: widen the arm's result to the
/// select's merge shape and jump to the merge block. Extracted
/// verbatim from the pre-F0b `emit_select_node` arm loop.
fn emit_select_value_arm<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    body: &Cached<R, E>,
    mark: usize,
    merge_shape: SelectMerge,
    merge: Block,
    scrut_disc: ClifValue,
    guard_stale: Option<ClifValue>,
    sel_state: Option<(i32, usize)>,
) -> Result<()> {
    use NodeView;
    let body_frozen =
        kernel_abi::freeze_for_abi_normalized(cx.registry(), body.node.typ())
            .ok_or_else(|| {
                anyhow!(
                    "emit_clif: select arm type {:?} doesn't freeze concrete",
                    body.node.typ()
                )
            })?;
    let (disc, payload) = match merge_shape {
        SelectMerge::Scalar(rp) => {
            if kernel_abi::scalar_prim(cx.registry(), &body_frozen) != Some(rp) {
                return Err(anyhow!(
                    "emit_clif: select arm type {body_frozen:?} doesn't \
                     match the scalar merge {rp:?}"
                ));
            }
            let cv = body.node.emit_clif(cx)?;
            (cv.disc, cv.payload)
        }
        SelectMerge::Value => {
            // Node twin of `widen_arm_to_value`, keyed on the arm
            // BODY's frozen type.
            match kernel_abi::abi_kind(cx.registry(), &body_frozen) {
                Some(AbiKind::Null) => {
                    // A bare-null arm body has nothing to emit (and a
                    // Null-shaped node can't emit anyway); only the
                    // literal constant form is recognized.
                    match body.node.view() {
                        NodeView::Constant(c) if matches!(c.value, Value::Null) => {}
                        _ => {
                            return Err(anyhow!(
                                "emit_clif: null-typed select arm isn't \
                                 a null literal"
                            ));
                        }
                    }
                    // Same STALE gate as `emit_const_node`: a literal
                    // fires only at init. A raw (always-FRESH) disc here
                    // made the STALE AND-fold below unable to sleep the
                    // arm, so a guarded select taking a null arm re-fired
                    // on every kernel invocation (soak-jul06c B3).
                    let init = cx.init_flag();
                    let d = cx.b.ins().iconst(types::I64, value_disc::NULL);
                    let d = const_stale_gate(cx.b, init, d);
                    let p = cx.b.ins().iconst(types::I64, 0);
                    (d, p)
                }
                Some(AbiKind::Scalar(p)) => {
                    let cv = body.node.emit_clif(cx)?;
                    (cv.disc, scalar_to_payload_i64(cx.b, p, cv.payload))
                }
                Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
                    let cv = body.node.emit_clif(cx)?;
                    ensure_owned_value_src(
                        cx,
                        node_composite_source(&body.node),
                        cv.disc,
                        cv.payload,
                    )?
                }
                other => {
                    return Err(anyhow!(
                        "emit_clif: select arm of shape {other:?} can't \
                         widen to the Value merge"
                    ));
                }
            }
        }
        SelectMerge::Composite => {
            if !matches!(
                kernel_abi::abi_kind(cx.registry(), &body_frozen),
                Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct)
            ) {
                return Err(anyhow!(
                    "emit_clif: select arm type {body_frozen:?} doesn't \
                     match the composite merge"
                ));
            }
            let cv = body.node.emit_clif(cx)?;
            let v = ensure_owned_composite_src(
                cx,
                node_composite_source(&body.node),
                cv.payload,
            )?;
            (cv.disc, v)
        }
        SelectMerge::String => {
            if !matches!(
                kernel_abi::abi_kind(cx.registry(), &body_frozen),
                Some(AbiKind::String)
            ) {
                return Err(anyhow!(
                    "emit_clif: select arm type {body_frozen:?} doesn't \
                     match the string merge"
                ));
            }
            // String reads/produces are owned at production.
            let cv = body.node.emit_clif(cx)?;
            (cv.disc, cv.payload)
        }
    };
    // Fold the scrutinee's flags into the arm result. A select FIRES iff
    // its scrutinee OR its taken arm (OR a guard) fired. TAINT = OR(arm,
    // scrut): a missing scrutinee bottoms regardless of arm. For STALE,
    // off a cleaned base (so the arm's own STALE is recombined, not
    // blindly kept):
    //  - UNGUARDED: STALE = AND(arm, scrut) — fires iff arm or scrut fired.
    //    A stale-CONSTANT arm (`null => true`) must still fire when the
    //    scrutinee fired, else it wrong-bottoms.
    //  - GUARDED: additionally AND in the guard-feeder stale word
    //    computed before the chain — the select also fires when any
    //    guard input fired (see `emit_select_node`).
    let base = clean_disc(cx.b, disc);
    let disc = match guard_stale {
        None => propagate_flags(cx.b, base, &[disc, scrut_disc]),
        Some(gw) => {
            // Selection memory (see `emit_select_node`): with a state
            // word, the guard term fires only when the SELECTION
            // changed — compare-and-record this arm's index (stored
            // as idx+1; 0 = no previous selection, so a fresh
            // instance's first selection always reads as changed).
            // The record is skipped for a TAINTED scrutinee: the arm
            // is taken structurally but the node-walk made no
            // selection (its output is bottom), and recording it
            // would mask the next real selection change.
            let gw = match sel_state {
                None => gw,
                Some((off, idx)) => {
                    let sp = cx.state_ptr();
                    let stored =
                        cx.b.ins().load(types::I64, MemFlags::trusted(), sp, off);
                    let tag = cx.b.ins().iconst(types::I64, idx as i64 + 1);
                    let changed = cx.b.ins().icmp(IntCC::NotEqual, stored, tag);
                    let valid = is_untainted(cx.b, scrut_disc);
                    let recorded = cx.b.ins().select(valid, tag, stored);
                    cx.b.ins().store(MemFlags::trusted(), recorded, sp, off);
                    let quiet = cx.b.ins().iconst(types::I64, STALE);
                    cx.b.ins().select(changed, gw, quiet)
                }
            };
            let d = propagate_taint(cx.b, base, &[disc, scrut_disc]);
            propagate_stale(cx.b, d, &[disc, scrut_disc, gw])
        }
    };
    cx.env.truncate(mark);
    cx.b.ins().jump(merge, &[BlockArg::Value(disc), BlockArg::Value(payload)]);
    Ok(())
}

/// Node-graph analog of `kernel_abi::int_div_may_bottom` — true unless the
/// divisor is a non-zero constant (and, for signed, the dividend isn't
/// the MIN/-1 overflow pair). Conservative `true` keeps the runtime
/// guard; a provable non-bottom skips it. Sees through `ExplicitParens`.
fn node_int_div_may_bottom<R: Rt, E: UserEvent>(
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> bool {
    use NodeView;
    fn const_value<'a, R: Rt, E: UserEvent>(n: &'a Node<R, E>) -> Option<&'a Value> {
        match n.view() {
            NodeView::Constant(c) => Some(&c.value),
            NodeView::ExplicitParens(p) => const_value(&p.n),
            _ => None,
        }
    }
    let Some(rv) = const_value(rhs) else {
        return true;
    };
    // A zero divisor always bottoms.
    if value_is_zero(rv) {
        return true;
    }
    // A non-(-1) divisor can't hit the signed MIN/-1 overflow; safe.
    if !value_is_neg_one(rv) {
        return false;
    }
    // Divisor is -1: only the dividend == MIN bottoms. A non-MIN
    // constant dividend is safe; anything else is conservatively unsafe.
    match const_value(lhs) {
        Some(lv) => value_is_int_min(lv),
        None => true,
    }
}

fn value_is_zero(v: &Value) -> bool {
    matches!(
        v,
        Value::I8(0)
            | Value::I16(0)
            | Value::I32(0)
            | Value::I64(0)
            | Value::U8(0)
            | Value::U16(0)
            | Value::U32(0)
            | Value::U64(0)
            | Value::Z32(0)
            | Value::Z64(0)
            | Value::V32(0)
            | Value::V64(0)
    )
}

fn value_is_neg_one(v: &Value) -> bool {
    matches!(
        v,
        Value::I8(-1)
            | Value::I16(-1)
            | Value::I32(-1)
            | Value::I64(-1)
            | Value::Z32(-1)
            | Value::Z64(-1)
    )
}

fn value_is_int_min(v: &Value) -> bool {
    match v {
        Value::I8(x) => *x == i8::MIN,
        Value::I16(x) => *x == i16::MIN,
        Value::I32(x) | Value::Z32(x) => *x == i32::MIN,
        Value::I64(x) | Value::Z64(x) => *x == i64::MIN,
        _ => false,
    }
}

/// Cast a u64 (typically the raw bits of a scalar primitive packed
/// via [`pack_value_to_u64`] or returned from `graphix_dyncall`) to a
/// CLIF value of the target prim type. Integer truncations use
/// `ireduce`; floats route through a same-width integer then
/// `bitcast`.
fn cast_u64_to_prim(b: &mut FunctionBuilder, raw: ClifValue, p: PrimType) -> ClifValue {
    match p {
        PrimType::I64 | PrimType::U64 => raw,
        PrimType::I32 | PrimType::U32 => b.ins().ireduce(types::I32, raw),
        PrimType::I16 | PrimType::U16 => b.ins().ireduce(types::I16, raw),
        PrimType::I8 | PrimType::U8 | PrimType::Bool => b.ins().ireduce(types::I8, raw),
        PrimType::F32 => {
            let bits32 = b.ins().ireduce(types::I32, raw);
            b.ins().bitcast(
                types::F32,
                MemFlags::new()
                    .with_endianness(cranelift_codegen::ir::Endianness::Little),
                bits32,
            )
        }
        PrimType::F64 => b.ins().bitcast(
            types::F64,
            MemFlags::new().with_endianness(cranelift_codegen::ir::Endianness::Little),
            raw,
        ),
    }
}

fn variant_payload_helper(p: PrimType) -> Result<&'static str> {
    Ok(match p {
        PrimType::I8 => "graphix_variant_payload_i8",
        PrimType::I16 => "graphix_variant_payload_i16",
        PrimType::I32 => "graphix_variant_payload_i32",
        PrimType::I64 => "graphix_variant_payload_i64",
        PrimType::U8 => "graphix_variant_payload_u8",
        PrimType::U16 => "graphix_variant_payload_u16",
        PrimType::U32 => "graphix_variant_payload_u32",
        PrimType::U64 => "graphix_variant_payload_u64",
        PrimType::F32 => "graphix_variant_payload_f32",
        PrimType::F64 => "graphix_variant_payload_f64",
        PrimType::Bool => "graphix_variant_payload_bool",
    })
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
/// assembled in the same pre-group order the callee's input list uses
/// (formal args in FnType parameter order, then captures in
/// `CaptureSlot` order), then kind-grouped to the callee's ABI (see
/// [`KernelSig::abi_params`]): scalars, composite pointers
/// (array→tuple→struct), value-shape pairs (variant→nullable). Owned
/// composite/value ARGS are dropped after the call (the callee clones
/// every composite/value param on entry); captures are env READS
/// (borrowed) and never drop. Captures resolve BindId-first with a
/// name fallback; V1 supports scalar + composite captures — a
/// value-shape capture Errs (those env tables are still name-keyed)
/// and the subtree node-walks. Every arg/capture is passed as TWO words
/// (disc + payload), so a may-bottom scalar arg forwards its `TAINT` to
/// the callee (which bottoms if it consumes it) — no de-fuse. The
/// result is unpacked per the callee's return ABI: one CLIF result for
/// scalar / composite-pointer returns, a two-word `(disc, payload)` pair
/// for variant/nullable — owned. A one-word RETURN has no disc: the
/// callee records its result's not-fresh (TAINT/STALE) bits in
/// `CALLEE_RESULT_FLAGS` (`emit_flag_not_fresh`) and the caller takes
/// them here, OR-ing into the synthesized disc — a bottomed or unfired
/// callee RESULT rides back as data (#219), bottoming only consumers
/// that read it, exactly like a node-walk callsite whose output didn't
/// fire. Genuine aborts (depth trip, interrupt, async pend) still ride
/// `DYNCALL_PENDING` and bottom the whole caller kernel.
pub(crate) fn emit_lambda_call_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    cs: &CallSite<R, E>,
    info: &LambdaCallInfo,
) -> Result<CompiledExpr> {
    let fn_name = &info.fn_name;
    // Hoist the registry borrow (a `'c` ref independent of `cx`) so the
    // slot-grouping closures below capture IT, not `cx` — otherwise the
    // closures would hold `cx` shared while the per-slot emit needs
    // `&mut cx`.
    let reg = cx.registry();
    let ftype = cs
        .resolved_ftype()
        .or_else(|| cs.ftype())
        .ok_or_else(|| anyhow!("lambda call `{fn_name}`: no resolved FnType"))?;
    // Formal args in FnType parameter order — the order
    // `build_lambda_kernel` translated them into kernel inputs. Each
    // slot is typed from the CALLEE's signature (`info.arg_types`,
    // formals first, captures appended): those types were resolved
    // (`resolve_abstract` — Refs to hidden-rep abstract type names
    // expanded to the registered concrete rep) and frozen at build
    // time. Freezing the caller-side node type here instead would
    // re-reject exactly those abstract Refs (#218), and env isn't
    // available at emit time to resolve them — the classic caller
    // (`emit_lambda_call`) types args from the signature the same
    // way. A node whose actual emission shape disagrees with the
    // signature type Errs in the per-slot extractors below (build
    // time, de-fuse).
    let n_formal =
        info.arg_types.len().checked_sub(info.captures.len()).ok_or_else(|| {
            anyhow!(
                "lambda call `{fn_name}`: signature has fewer inputs than \
                 captures — discovery drift"
            )
        })?;
    if ftype.args.len() != n_formal {
        return Err(anyhow!(
            "lambda call `{fn_name}`: call-site FnType has {} formals, \
             kernel signature has {n_formal} — de-fuse",
            ftype.args.len()
        ));
    }
    let mut slots: Vec<LambdaCallSlot<R, E>> =
        Vec::with_capacity(ftype.args.len() + info.captures.len());
    let mut pos = 0usize;
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
        slots.push(LambdaCallSlot::Arg(node, info.arg_types[i].clone()));
    }
    for cap in &info.captures {
        slots.push(LambdaCallSlot::Cap(cap));
    }
    // Shape gate — scalar / composite / variant / nullable only.
    for s in &slots {
        match kernel_abi::abi_kind(reg, s.typ()) {
            Some(
                AbiKind::Scalar(_)
                | AbiKind::Array
                | AbiKind::Tuple
                | AbiKind::Struct
                | AbiKind::Variant
                | AbiKind::Nullable,
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
    let is_kind = |s: &&LambdaCallSlot<R, E>, k: AbiKind| {
        kernel_abi::abi_kind(reg, s.typ()) == Some(k)
    };
    let mut clif_args: Vec<ClifValue> = Vec::with_capacity(slots.len() * 2 + 1);
    let mut drops: Vec<CallArgDrop> = Vec::new();
    // The call-depth guard (Phase 4): every non-tail lambda dispatch —
    // cross-kernel calls AND value-position self-calls (native
    // recursion) — enters one unit against the SHARED
    // `Control::depth_push` counter the node-walk's `GXLambda::update`
    // pushes, so both evaluators bottom at the same logical depth and
    // an impure program's interleaved frames are bounded together. At
    // the limit the CALL bottoms LOCALLY: skip the dispatch and
    // continue with a #219 tainted, shape-safe placeholder — the same
    // observable as the node-walk's guarded dispatch yielding nothing
    // (its bottom silences only the call's consumers; a fold whose
    // callback ignores the tripped value RECOVERS — the former
    // whole-kernel abort here bottomed unrelated outputs, soak jul07h).
    // The check runs BEFORE argument marshalling so the trip path has
    // nothing of ours in flight (fused arg evaluation is pure, so
    // skipping it is unobservable — the node-walk evaluates args in
    // the caller's frame either way). A failed push does not
    // increment, so the trip path must NOT pop. Tail self-calls are
    // exempt on both sides (rebind-and-jump here, the in-place loop
    // there).
    let ret = &info.kernel.return_type;
    let ret_pay_ty = match kernel_abi::abi_kind(reg, ret) {
        Some(AbiKind::Scalar(p)) => prim_to_clif(p),
        _ => types::I64,
    };
    let call_bl = cx.b.create_block();
    let trip_bl = cx.b.create_block();
    let dmerge = cx.b.create_block();
    cx.b.append_block_param(dmerge, types::I64);
    cx.b.append_block_param(dmerge, ret_pay_ty);
    {
        let push = cx.helper("graphix_depth_push")?;
        let call = cx.b.ins().call(push, &[]);
        let ok = cx.b.inst_results(call)[0];
        let valid = cx.b.ins().icmp_imm(IntCC::NotEqual, ok, 0);
        cx.b.ins().brif(valid, call_bl, &[], trip_bl, &[]);
    }
    cx.b.switch_to_block(trip_bl);
    cx.b.seal_block(trip_bl);
    {
        let ph = emit_elem_placeholder(cx, ret)?;
        cx.b.ins().jump(dmerge, &[BlockArg::Value(ph.disc), BlockArg::Value(ph.payload)]);
    }
    cx.b.switch_to_block(call_bl);
    cx.b.seal_block(call_bl);
    // Leading cycle-context words: forward THIS kernel's `event.init`
    // (the callee's constants fire when this region inits) and state
    // pointer — every kernel signature carries the leading context
    // slots (`push_abi_params`), so a cross-kernel call must pass them
    // or the call mismatches the sig. The state pointer is forwarded
    // for uniformity only: a callee body never CLAIMS words
    // (`BodyEmitter::allow_state` is false for callees), so it never
    // reads through it.
    clif_args.push(cx.init_flag());
    clif_args.push(cx.state_ptr());
    // Marshal in canonical `abi_params` order — scalars, then composites
    // (array/tuple/struct), then value-shape (variant/nullable) — two
    // words (disc, payload) each. An Owned composite/value Arg is dropped
    // after the call (the callee refcount-bumps on entry).
    let order = slots
        .iter()
        .filter(|s| {
            matches!(kernel_abi::abi_kind(reg, s.typ()), Some(AbiKind::Scalar(_)))
        })
        .chain(slots.iter().filter(|s| is_kind(s, AbiKind::Array)))
        .chain(slots.iter().filter(|s| is_kind(s, AbiKind::Tuple)))
        .chain(slots.iter().filter(|s| is_kind(s, AbiKind::Struct)))
        .chain(slots.iter().filter(|s| is_kind(s, AbiKind::Variant)))
        .chain(slots.iter().filter(|s| is_kind(s, AbiKind::Nullable)));
    for s in order {
        let cv = emit_slot(cx, s)?;
        if let LambdaCallSlot::Arg(n, _) = s {
            if node_composite_source(n) == CompositeSource::Owned {
                match kernel_abi::abi_kind(reg, s.typ()) {
                    Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                        drops.push(CallArgDrop::Composite(cv.payload));
                    }
                    Some(AbiKind::Variant | AbiKind::Nullable) => {
                        drops.push(CallArgDrop::Value {
                            disc: cv.disc,
                            payload: cv.payload,
                        });
                    }
                    _ => {}
                }
            }
        }
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
    let inst = cx.b.ins().call(*func_ref, &clif_args);
    // Exit the depth-guard unit entered above. A callee that ABORTED
    // (interrupt / its own deeper depth trip) still returns here
    // normally (the abort path returns the pending sentinel), so the
    // pop is unconditional.
    {
        let pop = cx.helper("graphix_depth_pop")?;
        cx.b.ins().call(pop, &[]);
    }
    // The callee RETURN ABI is one word for scalar/composite, two for
    // value-shape. A one-word return has no disc, so the callee's
    // return path records its result's not-fresh (TAINT/STALE) bits in
    // the CALLEE_RESULT_FLAGS cell (`emit_flag_not_fresh`); take them
    // here — after EVERY call, keeping the cell clean call-to-call —
    // and OR them into the synthesized disc. A bottomed or unfired
    // callee result thus rides into this kernel as a tainted/stale
    // VALUE (#219) instead of aborting the whole region: it bottoms
    // only the consumers that read it, like the node-walk. Value-shape
    // returns carry the disc in-band (the callee never sets flags), so
    // the OR is a no-op there.
    let flags = {
        let take = cx.helper("graphix_callee_flags_take")?;
        let call = cx.b.ins().call(take, &[]);
        cx.b.inst_results(call)[0]
    };
    // A callee that genuinely ABORTED (interrupt, depth trip) left
    // `DYNCALL_PENDING` set and returned the pending sentinel — a null
    // pointer / zero pair, NOT a real value. Propagate the abort at
    // the call site: drop the owned call args, drop this kernel's
    // owned set, and jump to `pending_exit` with the flag still set
    // (peek, not clear) so `Kernel::update` discards. Without this the
    // sentinel would flow into downstream derefs and drops. Value-
    // level bottoms never take this path — a callee's pended DynCall
    // converts to a #219 tainted result at its own site and rides
    // back through `CALLEE_RESULT_FLAGS` / the in-band disc.
    {
        let peek = cx.helper("graphix_dyncall_pending_take")?;
        let call = cx.b.ins().call(peek, &[]);
        let pending = cx.b.inst_results(call)[0];
        let abort_bl = cx.b.create_block();
        let cont_bl = cx.b.create_block();
        cx.b.ins().brif(pending, abort_bl, &[], cont_bl, &[]);
        cx.b.switch_to_block(abort_bl);
        cx.b.seal_block(abort_bl);
        emit_call_arg_drops(cx.b, cx.ctx, &drops)?;
        let exit = pending_exit_block(cx);
        emit_pending_cleanup(cx.b, cx.env, cx.ctx)?;
        cx.b.ins().jump(exit, &[]);
        cx.b.switch_to_block(cont_bl);
        cx.b.seal_block(cont_bl);
    }
    let one_result = |cx: &mut BodyCx| -> Result<ClifValue> {
        let results = cx.b.inst_results(inst);
        if results.len() != 1 {
            return Err(anyhow!(
                "lambda call `{fn_name}`: callee returned {} values, expected 1",
                results.len()
            ));
        }
        Ok(results[0])
    };
    let result = match kernel_abi::abi_kind(reg, ret) {
        Some(AbiKind::Scalar(p)) => {
            let r0 = one_result(cx)?;
            let base = scalar_disc(cx.b, p);
            let disc = cx.b.ins().bor(base, flags);
            CompiledExpr::new(disc, r0)
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let r0 = one_result(cx)?;
            let base = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
            let disc = cx.b.ins().bor(base, flags);
            CompiledExpr::new(disc, r0)
        }
        Some(AbiKind::Variant | AbiKind::Nullable) => {
            let (r0, r1) = {
                let results = cx.b.inst_results(inst);
                if results.len() != 2 {
                    return Err(anyhow!(
                        "lambda call `{fn_name}`: value-shape callee returned \
                         {} values, expected 2",
                        results.len()
                    ));
                }
                (results[0], results[1])
            };
            let disc = cx.b.ins().bor(r0, flags);
            CompiledExpr::new(disc, r1)
        }
        other => {
            return Err(anyhow!(
                "lambda call `{fn_name}`: return shape {other:?} not \
                 lowered — subtree node-walks"
            ));
        }
    };
    // Owned-arg drops belong to the CALL path (the trip path never
    // marshalled), then meet the trip placeholder at the merge.
    emit_call_arg_drops(cx.b, cx.ctx, &drops)?;
    cx.b.ins().jump(
        dmerge,
        &[BlockArg::Value(result.disc), BlockArg::Value(result.payload)],
    );
    cx.b.switch_to_block(dmerge);
    cx.b.seal_block(dmerge);
    let disc = cx.b.block_params(dmerge)[0];
    let payload = cx.b.block_params(dmerge)[1];
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
    for d in drops {
        match d {
            CallArgDrop::Composite(ptr) => {
                b.ins().call(arr_drop, &[*ptr]);
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
/// Composite-return kernels (when we add them) would skip the
/// dropped value being returned; right now all composite return is
/// rejected at compile so it's safe to drop everything.
fn drop_owned_composites(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
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
    // Every owned local is dropped by `kind`. Composite params/locals
    // are refcount-cloned on kernel entry; Variant/Nullable/Value locals
    // come from entry clones / `VariantNew` / composite-return DynCall;
    // String locals carry an owned refcount. Scalars own nothing.
    let drops: smallvec::SmallVec<[(LocalKind, ValueVar); 8]> =
        env.locals.iter().map(|l| (l.kind, l.vv)).collect();
    for (kind, vv) in drops {
        match kind {
            LocalKind::Scalar(_) => {}
            LocalKind::Composite => {
                let ptr = b.use_var(vv.payload);
                b.ins().call(arr_drop, &[ptr]);
            }
            LocalKind::String => {
                let ptr = b.use_var(vv.payload);
                b.ins().call(str_drop, &[ptr]);
            }
            LocalKind::Variant | LocalKind::Nullable | LocalKind::Value => {
                let disc = b.use_var(vv.disc);
                let payload = b.use_var(vv.payload);
                b.ins().call(val_drop, &[disc, payload]);
            }
        }
    }
    Ok(())
}

/// Emit drops for everything the JIT'd kernel currently owns, for
/// the `pre_pending` block of a composite-return DynCall that
/// pended. This is `drop_owned_composites` plus the in-flight
/// outer DynCall args bufs from `dyncall_buf_stack`.
///
/// Ordering doesn't matter — every entry is an independent owned
/// allocation. `use_var` at this CFG point resolves each Variable
/// to its value along the edge from the DynCall block.
fn emit_pending_cleanup(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
    let buf_drop = ctx
        .helper_refs
        .get("graphix_value_buf_drop")
        .ok_or_else(|| anyhow!("missing graphix_value_buf_drop"))?;
    // Outer in-flight DynCall args bufs. The current DynCall's own
    // buf was already popped before this is called.
    for buf_var in ctx.dyncall_buf_stack.borrow().iter() {
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

// (The old `emit_dyncall_pending_branch` / `emit_return_pending_check`
// pair — the site-level and return-level whole-kernel aborts for a
// pended DynCall — is gone: every dyncall site now take-and-CLEARS the
// pending flag and converts a pend to a #219 tainted placeholder that
// continues, and a cross-kernel callee's genuine abort is checked and
// propagated at the call site in `emit_lambda_call_node`. No path can
// leave the flag set and keep executing, so a return-time peek would
// always read false.)

/// Map a [`PrimType`] to the `graphix_value_buf_push_<T>` helper.
fn value_buf_push_helper(p: PrimType) -> Result<&'static str> {
    Ok(match p {
        PrimType::I8 => "graphix_value_buf_push_i8",
        PrimType::I16 => "graphix_value_buf_push_i16",
        PrimType::I32 => "graphix_value_buf_push_i32",
        PrimType::I64 => "graphix_value_buf_push_i64",
        PrimType::U8 => "graphix_value_buf_push_u8",
        PrimType::U16 => "graphix_value_buf_push_u16",
        PrimType::U32 => "graphix_value_buf_push_u32",
        PrimType::U64 => "graphix_value_buf_push_u64",
        PrimType::F32 => "graphix_value_buf_push_f32",
        PrimType::F64 => "graphix_value_buf_push_f64",
        PrimType::Bool => "graphix_value_buf_push_bool",
    })
}

/// Map an element [`PrimType`] to the `graphix_valarray_get_<T>`
/// helper symbol name. Used by ArrayGet / TupleGet lowering.
fn valarray_get_helper(p: PrimType) -> Result<&'static str> {
    Ok(match p {
        PrimType::I8 => "graphix_valarray_get_i8",
        PrimType::I16 => "graphix_valarray_get_i16",
        PrimType::I32 => "graphix_valarray_get_i32",
        PrimType::I64 => "graphix_valarray_get_i64",
        PrimType::U8 => "graphix_valarray_get_u8",
        PrimType::U16 => "graphix_valarray_get_u16",
        PrimType::U32 => "graphix_valarray_get_u32",
        PrimType::U64 => "graphix_valarray_get_u64",
        PrimType::F32 => "graphix_valarray_get_f32",
        PrimType::F64 => "graphix_valarray_get_f64",
        PrimType::Bool => "graphix_valarray_get_bool",
    })
}

/// Map a struct field [`PrimType`] to the `graphix_struct_get_<T>`
/// helper symbol name.
fn struct_get_helper(p: PrimType) -> Result<&'static str> {
    Ok(match p {
        PrimType::I8 => "graphix_struct_get_i8",
        PrimType::I16 => "graphix_struct_get_i16",
        PrimType::I32 => "graphix_struct_get_i32",
        PrimType::I64 => "graphix_struct_get_i64",
        PrimType::U8 => "graphix_struct_get_u8",
        PrimType::U16 => "graphix_struct_get_u16",
        PrimType::U32 => "graphix_struct_get_u32",
        PrimType::U64 => "graphix_struct_get_u64",
        PrimType::F32 => "graphix_struct_get_f32",
        PrimType::F64 => "graphix_struct_get_f64",
        PrimType::Bool => "graphix_struct_get_bool",
    })
}

/// Map an element [`Type`] to its element-read helper symbol —
/// primitive (`get_<prim>`), String (`get_arcstr`), composite
/// (`get_array`, a `*mut ValArray`), or value-shape (`get_value`, a
/// two-word `Value`). `struct_access` picks the `struct_get_*` (two-
/// level kv-pair read) family over the flat `valarray_get_*` family.
fn element_read_helper(
    reg: &AbstractRegistry,
    elem: &Type,
    struct_access: bool,
) -> Result<&'static str> {
    Ok(match kernel_abi::abi_kind(reg, elem) {
        Some(AbiKind::Scalar(p)) => {
            if struct_access {
                struct_get_helper(p)?
            } else {
                valarray_get_helper(p)?
            }
        }
        Some(AbiKind::String) => {
            if struct_access {
                "graphix_struct_get_arcstr"
            } else {
                "graphix_valarray_get_arcstr"
            }
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            if struct_access {
                "graphix_struct_get_array"
            } else {
                "graphix_valarray_get_array"
            }
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            if struct_access {
                "graphix_struct_get_value"
            } else {
                "graphix_valarray_get_value"
            }
        }
        Some(AbiKind::Unit | AbiKind::Null) | None => {
            return Err(anyhow!(
                "element read of Unit/Null/non-fusable slot — emission is malformed"
            ));
        }
    })
}

/// Emit an element read: `arr_ptr[idx]` (or struct field) of the given
/// element `Type`, dispatching to the right `..._get_*` helper. The
/// result is OWNED (fresh box / refcount-bumped clone). Returns a
/// `CompiledExpr` (disc + payload) whose disc tag marks a value-shape
/// element (two-register Value) vs a scalar / string / composite-pointer
/// element — one routine serves both the scalar and value-shape reads.
fn compile_element_read(
    b: &mut FunctionBuilder,
    arr_ptr: ClifValue,
    idx_val: ClifValue,
    elem: &Type,
    struct_access: bool,
    ctx: &LowerCtx,
) -> Result<CompiledExpr> {
    let helper_name = element_read_helper(ctx.registry, elem, struct_access)?;
    let helper = ctx
        .helper_refs
        .get(helper_name)
        .ok_or_else(|| anyhow!("missing JIT helper `{helper_name}`"))?;
    let call = b.ins().call(helper, &[arr_ptr, idx_val]);
    if kernel_abi::is_value_shape(ctx.registry, elem) {
        // Value-shape element read returns two words (disc, payload).
        let (r0, r1) = {
            let r = b.inst_results(call);
            (r[0], r[1])
        };
        Ok(CompiledExpr::new(r0, r1))
    } else {
        // Scalar / string / composite element read returns one word.
        let r0 = b.inst_results(call)[0];
        // The element read is untainted (a valid array element); the disc
        // is a const taint-carrier matching the element kind.
        let disc = match kernel_abi::abi_kind(ctx.registry, elem) {
            Some(AbiKind::Scalar(p)) => scalar_disc(b, p),
            Some(AbiKind::String) => b.ins().iconst(types::I64, value_disc::STRING),
            _ => b.ins().iconst(types::I64, value_disc::ARRAY),
        };
        Ok(CompiledExpr::new(disc, r0))
    }
}

/// Widen a CLIF value to i64. Helpers expect a usize index; if the
/// caller's index expression was narrower (e.g. `i32` from a
/// `cast`), we zero/sign extend here.
fn widen_to_i64(b: &mut FunctionBuilder, v: ClifValue, p: PrimType) -> Result<ClifValue> {
    Ok(match p {
        PrimType::I64 | PrimType::U64 => v,
        PrimType::I8 | PrimType::I16 | PrimType::I32 => b.ins().sextend(types::I64, v),
        PrimType::U8 | PrimType::U16 | PrimType::U32 | PrimType::Bool => {
            b.ins().uextend(types::I64, v)
        }
        PrimType::F32 | PrimType::F64 => {
            return Err(anyhow::anyhow!(
                "widen_to_i64: float index — emission malformed"
            ));
        }
    })
}

/// Promote a scalar CLIF value to the 8-byte payload word of a
/// `repr(u64)` Value. Integers smaller than i64 get zero/sign-
/// extended (we use unsigned `uextend` because the payload's
/// interpretation is fixed by the discriminant; truncation back
/// preserves bits). f32/f64 bitcast through their integer mirror.
fn scalar_to_payload_i64(
    b: &mut FunctionBuilder,
    p: PrimType,
    v: ClifValue,
) -> ClifValue {
    match p {
        PrimType::I64 | PrimType::U64 => v,
        PrimType::I32 | PrimType::U32 => b.ins().uextend(types::I64, v),
        PrimType::I16 | PrimType::U16 => b.ins().uextend(types::I64, v),
        PrimType::I8 | PrimType::U8 | PrimType::Bool => b.ins().uextend(types::I64, v),
        PrimType::F32 => {
            let bits = b.ins().bitcast(
                types::I32,
                cranelift_codegen::ir::MemFlags::new()
                    .with_endianness(cranelift_codegen::ir::Endianness::Little),
                v,
            );
            b.ins().uextend(types::I64, bits)
        }
        PrimType::F64 => b.ins().bitcast(
            types::I64,
            cranelift_codegen::ir::MemFlags::new()
                .with_endianness(cranelift_codegen::ir::Endianness::Little),
            v,
        ),
    }
}

/// Kept for documentation; superseded by inline packing via
/// `prim_to_value_disc` + `scalar_to_payload_i64`. Will be deleted
/// once no callers remain.
#[allow(dead_code)]
fn value_new_prim_helper(p: PrimType) -> &'static str {
    match p {
        PrimType::I8 => "graphix_value_new_i8",
        PrimType::I16 => "graphix_value_new_i16",
        PrimType::I32 => "graphix_value_new_i32",
        PrimType::I64 => "graphix_value_new_i64",
        PrimType::U8 => "graphix_value_new_u8",
        PrimType::U16 => "graphix_value_new_u16",
        PrimType::U32 => "graphix_value_new_u32",
        PrimType::U64 => "graphix_value_new_u64",
        PrimType::F32 => "graphix_value_new_f32",
        PrimType::F64 => "graphix_value_new_f64",
        PrimType::Bool => "graphix_value_new_bool",
    }
}

/// The `graphix_string_buf_push_*` helper that Display-renders a
/// scalar of `p` into a Concat / string-interpolate buffer. Used by
/// [`emit_string_interpolate_node`].
fn string_buf_push_helper(p: PrimType) -> &'static str {
    match p {
        PrimType::I64 => "graphix_string_buf_push_i64",
        PrimType::U64 => "graphix_string_buf_push_u64",
        PrimType::I32 => "graphix_string_buf_push_i32",
        PrimType::U32 => "graphix_string_buf_push_u32",
        PrimType::I16 => "graphix_string_buf_push_i16",
        PrimType::U16 => "graphix_string_buf_push_u16",
        PrimType::I8 => "graphix_string_buf_push_i8",
        PrimType::U8 => "graphix_string_buf_push_u8",
        PrimType::F64 => "graphix_string_buf_push_f64",
        PrimType::F32 => "graphix_string_buf_push_f32",
        PrimType::Bool => "graphix_string_buf_push_bool",
    }
}

/// Lower a scalar [`Value`] constant of the given `prim` to a CLIF
/// `iconst`/`f32const`/`f64const`. `prim` comes from the constant's
/// frozen type; `v` must be the matching scalar (`Z*`/`V*`
/// accepted for their fixed-width prim). Returns `Err` otherwise (a
/// malformed kernel — de-fuses to the node-walk instead of panicking).
fn compile_const(
    b: &mut FunctionBuilder,
    v: &Value,
    prim: PrimType,
) -> Result<ClifValue> {
    macro_rules! bad {
        () => {
            return Err(anyhow::anyhow!("compile_const: {v:?} isn't a {prim:?} scalar"))
        };
    }
    Ok(match prim {
        PrimType::I8 => match v {
            Value::I8(x) => b.ins().iconst(types::I8, *x as i64),
            _ => bad!(),
        },
        PrimType::I16 => match v {
            Value::I16(x) => b.ins().iconst(types::I16, *x as i64),
            _ => bad!(),
        },
        PrimType::I32 => match v {
            Value::I32(x) | Value::Z32(x) => b.ins().iconst(types::I32, *x as i64),
            _ => bad!(),
        },
        PrimType::I64 => match v {
            Value::I64(x) | Value::Z64(x) => b.ins().iconst(types::I64, *x),
            _ => bad!(),
        },
        PrimType::U8 => match v {
            Value::U8(x) => b.ins().iconst(types::I8, *x as i64),
            _ => bad!(),
        },
        PrimType::U16 => match v {
            Value::U16(x) => b.ins().iconst(types::I16, *x as i64),
            _ => bad!(),
        },
        PrimType::U32 => match v {
            Value::U32(x) | Value::V32(x) => b.ins().iconst(types::I32, *x as i64),
            _ => bad!(),
        },
        PrimType::U64 => match v {
            Value::U64(x) | Value::V64(x) => b.ins().iconst(types::I64, *x as i64),
            _ => bad!(),
        },
        PrimType::F32 => match v {
            Value::F32(x) => b.ins().f32const(*x),
            _ => bad!(),
        },
        PrimType::F64 => match v {
            Value::F64(x) => b.ins().f64const(*x),
            _ => bad!(),
        },
        PrimType::Bool => match v {
            Value::Bool(true) => b.ins().iconst(types::I8, 1),
            Value::Bool(false) => b.ins().iconst(types::I8, 0),
            _ => bad!(),
        },
    })
}

/// A zero / false constant of the given prim type. Used for the
/// `pending_exit` block's sentinel return value (never observed —
/// `Kernel::update` discards the result on the pending path — but
/// CLIF needs a well-typed value of the right width).
fn zero_const(b: &mut FunctionBuilder, p: PrimType) -> ClifValue {
    match p {
        PrimType::I8 | PrimType::U8 | PrimType::Bool => b.ins().iconst(types::I8, 0),
        PrimType::I16 | PrimType::U16 => b.ins().iconst(types::I16, 0),
        PrimType::I32 | PrimType::U32 => b.ins().iconst(types::I32, 0),
        PrimType::I64 | PrimType::U64 => b.ins().iconst(types::I64, 0),
        PrimType::F32 => b.ins().f32const(0.0),
        PrimType::F64 => b.ins().f64const(0.0),
    }
}

fn compile_bin(
    b: &mut FunctionBuilder,
    op: BinOp,
    typ: PrimType,
    l: ClifValue,
    r: ClifValue,
) -> Result<ClifValue> {
    Ok(if typ.is_integer() {
        match op {
            BinOp::Add => b.ins().iadd(l, r),
            BinOp::Sub => b.ins().isub(l, r),
            BinOp::Mul => b.ins().imul(l, r),
            BinOp::Div => {
                if typ.is_signed() {
                    b.ins().sdiv(l, r)
                } else {
                    b.ins().udiv(l, r)
                }
            }
            BinOp::Mod => {
                if typ.is_signed() {
                    b.ins().srem(l, r)
                } else {
                    b.ins().urem(l, r)
                }
            }
        }
    } else {
        // float
        match op {
            BinOp::Add => b.ins().fadd(l, r),
            BinOp::Sub => b.ins().fsub(l, r),
            BinOp::Mul => b.ins().fmul(l, r),
            BinOp::Div => b.ins().fdiv(l, r),
            BinOp::Mod => {
                // Cranelift has no `frem` — float `%` would need an fmod
                // libcall, and `compile_bin` has no module handle to emit
                // one here. Bail so the kernel falls back to the
                // interpreter (which computes float `%` correctly),
                // instead of emitting a runtime trap that crashed the
                // whole runtime. (The trap was a latent crash found by
                // graphix-fuzz on `f64:7.0 % f64:3.0`; wiring the fmod
                // libcall so it JITs is a follow-up.)
                let _ = (l, r);
                return Err(anyhow!(
                    "JIT: float modulo unsupported (no cranelift frem); \
                     kernel runs on the interpreter"
                ));
            }
        }
    })
}

fn compile_cmp(
    b: &mut FunctionBuilder,
    op: CmpOp,
    operand_typ: PrimType,
    l: ClifValue,
    r: ClifValue,
) -> ClifValue {
    if operand_typ.is_float() {
        // Float comparison uses graphix's TOTAL order, matching
        // `Value::partial_cmp` / the node-walk: NaN == NaN, and NaN sorts
        // below every non-NaN value. `FloatCC::Equal`/`LessThan`/etc. are
        // the IEEE *ordered* predicates (any NaN operand → false); a NaN
        // is the only value unordered with itself, so `fcmp Unordered x x`
        // tests "x is NaN". fcmp yields an I8 0/1, so `bxor_imm(v, 1)` is
        // logical NOT. We build `eq` and `lt` under the total order and
        // derive the rest.
        let l_nan = b.ins().fcmp(FloatCC::Unordered, l, l);
        let r_nan = b.ins().fcmp(FloatCC::Unordered, r, r);
        let not_l_nan = b.ins().bxor_imm(l_nan, 1);
        let not_r_nan = b.ins().bxor_imm(r_nan, 1);
        // eq: ordered-equal, OR both NaN.
        let ord_eq = b.ins().fcmp(FloatCC::Equal, l, r);
        let both_nan = b.ins().band(l_nan, r_nan);
        let eq = b.ins().bor(ord_eq, both_nan);
        // lt: ordered IEEE l<r, OR (l is NaN and r is not) since NaN is least.
        let ord_lt = b.ins().fcmp(FloatCC::LessThan, l, r);
        let nan_lt = b.ins().band(l_nan, not_r_nan);
        let lt = b.ins().bor(ord_lt, nan_lt);
        // gt: ordered IEEE l>r, OR (r is NaN and l is not).
        let ord_gt = b.ins().fcmp(FloatCC::GreaterThan, l, r);
        let nan_gt = b.ins().band(r_nan, not_l_nan);
        let gt = b.ins().bor(ord_gt, nan_gt);
        match op {
            CmpOp::Eq => eq,
            CmpOp::Ne => b.ins().bxor_imm(eq, 1),
            CmpOp::Lt => lt,
            CmpOp::Gt => gt,
            CmpOp::Lte => b.ins().bxor_imm(gt, 1), // not gt
            CmpOp::Gte => b.ins().bxor_imm(lt, 1), // not lt
        }
    } else {
        let cc = if operand_typ.is_signed() || operand_typ == PrimType::Bool {
            // Bool comparisons are fine via signed (or unsigned) — but
            // signed eq/ne behaves identically on an I8 holding 0/1.
            match op {
                CmpOp::Eq => IntCC::Equal,
                CmpOp::Ne => IntCC::NotEqual,
                CmpOp::Lt => IntCC::SignedLessThan,
                CmpOp::Gt => IntCC::SignedGreaterThan,
                CmpOp::Lte => IntCC::SignedLessThanOrEqual,
                CmpOp::Gte => IntCC::SignedGreaterThanOrEqual,
            }
        } else {
            match op {
                CmpOp::Eq => IntCC::Equal,
                CmpOp::Ne => IntCC::NotEqual,
                CmpOp::Lt => IntCC::UnsignedLessThan,
                CmpOp::Gt => IntCC::UnsignedGreaterThan,
                CmpOp::Lte => IntCC::UnsignedLessThanOrEqual,
                CmpOp::Gte => IntCC::UnsignedGreaterThanOrEqual,
            }
        };
        b.ins().icmp(cc, l, r)
    }
}

fn compile_cast(
    b: &mut FunctionBuilder,
    v: ClifValue,
    src: PrimType,
    dst: PrimType,
) -> ClifValue {
    if prim_to_clif(src) == prim_to_clif(dst) && src.is_float() == dst.is_float() {
        // Same underlying CLIF type and same float/int family — no-op.
        return v;
    }
    let dst_ty = prim_to_clif(dst);
    let src_size = clif_size(src);
    let dst_size = clif_size(dst);
    if src.is_integer() && dst.is_integer() {
        if src_size < dst_size {
            if src.is_signed() {
                b.ins().sextend(dst_ty, v)
            } else {
                b.ins().uextend(dst_ty, v)
            }
        } else if src_size > dst_size {
            b.ins().ireduce(dst_ty, v)
        } else {
            // Same size — bit reinterpretation only.
            v
        }
    } else if src.is_integer() && dst.is_float() {
        if src.is_signed() {
            b.ins().fcvt_from_sint(dst_ty, v)
        } else {
            b.ins().fcvt_from_uint(dst_ty, v)
        }
    } else if src.is_float() && dst.is_integer() {
        // Saturating to match Rust `as` semantics on out-of-range.
        if dst.is_signed() {
            b.ins().fcvt_to_sint_sat(dst_ty, v)
        } else {
            b.ins().fcvt_to_uint_sat(dst_ty, v)
        }
    } else if src.is_float() && dst.is_float() {
        if src_size < dst_size {
            b.ins().fpromote(dst_ty, v)
        } else {
            b.ins().fdemote(dst_ty, v)
        }
    } else {
        // bool ↔ integer/float — `emit_cast_node` refuses these
        // before emitting; reaching this branch means a caller
        // bypassed that gate.
        unreachable!("compile_cast: bool casts should be rejected before emission");
    }
}

// ─── Type plumbing ───────────────────────────────────────────────

fn prim_to_clif(p: PrimType) -> ClifType {
    match p {
        PrimType::I8 | PrimType::U8 | PrimType::Bool => types::I8,
        PrimType::I16 | PrimType::U16 => types::I16,
        PrimType::I32 | PrimType::U32 => types::I32,
        PrimType::I64 | PrimType::U64 => types::I64,
        PrimType::F32 => types::F32,
        PrimType::F64 => types::F64,
    }
}

/// Width in bytes of the underlying CLIF type — used to pick between
/// extend / reduce / promote / demote in casts.
fn clif_size(p: PrimType) -> u32 {
    match p {
        PrimType::I8 | PrimType::U8 | PrimType::Bool => 1,
        PrimType::I16 | PrimType::U16 => 2,
        PrimType::I32 | PrimType::U32 | PrimType::F32 => 4,
        PrimType::I64 | PrimType::U64 | PrimType::F64 => 8,
    }
}
