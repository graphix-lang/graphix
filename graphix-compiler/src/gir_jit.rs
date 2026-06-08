//! Cranelift JIT backend for the kernel IR.
//!
//! Lowers a [`GirKernel`] to native machine code via Cranelift,
//! returning a function pointer that the runtime can call directly.
//! The companion of [`crate::gir_interp`] (the GIR interpreter) —
//! same fusion analysis, same IR, different backend.
//!
//! ## Calling convention
//!
//! The compiled function uses the host platform's default C calling
//! convention (SystemV on Linux, Windows-fastcall on Windows).
//! Parameters are laid out in "kind-grouped" order — all scalars
//! first, then composite pointers, then two-word `Value`s — defined
//! once by [`GirKernel::abi_params`] and consumed by every ABI site
//! (the two signature builders, the wrapper unpacker, the entry
//! binder, and the runtime arg packer in `gir_interp`). Per-kind wire
//! shape:
//!
//! - scalar `i8/i16/i32/i64/u8/u16/u32/u64` → CLIF `I8`/`I16`/`I32`/`I64`;
//!   `f32/f64` → `F32`/`F64`; `bool` → `I8` (0 = false, non-zero = true)
//! - array/tuple/struct → one `I64` (a `*ValArray`)
//! - variant/nullable (`[T, null]`) → two `I64`s, a `repr(u64)`
//!   `Value`'s (disc, payload), matching the SysV two-register
//!   16-byte aggregate ABI
//!
//! Call from Rust by transmuting the returned function pointer to
//! `extern "C" fn(...) -> ...` of the matching signature — or, more
//! commonly, through the uniform-slot [`WrappedKernel`].
//!
//! ## Body lowering
//!
//! Most of the IR maps cleanly onto Cranelift basic-block + SSA form:
//!
//! - Parameters become CLIF [`Variable`]s defined in the entry block.
//! - `let` becomes a fresh [`Variable`] storing the value.
//! - `Bin`/`Cmp`/`BoolBin`/`Not`/`Cast` become the corresponding
//!   [`InstBuilder`] instructions.
//! - Statement-form `Select` becomes a chain of conditional branches
//!   between per-arm basic blocks; each arm's body terminates via
//!   `return` or `jump` back to the loop head.
//! - Self-tail-call (when the kernel has `has_tail_loop`) reassigns
//!   the param Variables and jumps back to a designated loop head.
//! - Expression-form `IfChain` and `Block` use a merge block with a
//!   block parameter for the result value.
//!
//! ## Cross-kernel `Call`
//!
//! Not yet supported in the JIT path — the M4 wiring is what
//! provides a kernel registry. For now, fused kernels that call into
//! other fused kernels (cross-function fusion) fall back to the
//! interpreter on the boundary. The interpreter has the same
//! limitation; both lift in M4.

use crate::gir::{
    AbiParamKind, AbiReturn, BinOp, BoolOp, CmpOp, ConstVal, GirExpr, GirKernel,
    GirOp, GirStmt, GirType, PrimType, SelectArm,
};
use anyhow::{anyhow, Context as AnyContext, Result};
use arcstr::ArcStr;
use netidx_value::Value;
use cranelift_codegen::{
    ir::{
        condcodes::{FloatCC, IntCC},
        types, AbiParam, Block, BlockArg, FuncRef, InstBuilder, MemFlags, Signature,
        Type as ClifType, Value as ClifValue,
    },
    settings::{self, Configurable},
    Context,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{default_libcall_names, FuncId, Linkage, Module};
use std::collections::BTreeMap;

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
    /// Pre-declared FuncIds for the `gir_jit_helpers::*` runtime
    /// helpers — registered once at construction so per-function
    /// codegen can materialize FuncRefs to them without re-declaring.
    helper_ids: HelperFuncIds,
}

impl JitCtx {
    pub fn new() -> Result<Self> {
        let mut flag_builder = settings::builder();
        // Speed > size on the assumption that a JIT'd kernel is hot
        // by definition. PIC off (we stay in-process), no colocated
        // libcalls (we don't need any libm helpers for the v1
        // op set).
        flag_builder.set("opt_level", "speed").context("set opt_level")?;
        flag_builder
            .set("use_colocated_libcalls", "false")
            .context("set use_colocated_libcalls")?;
        flag_builder.set("is_pic", "false").context("set is_pic")?;
        let isa_builder = cranelift_native::builder().map_err(|e| {
            anyhow!("cranelift_native::builder failed: {e}")
        })?;
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .context("isa_builder.finish")?;
        let mut builder = JITBuilder::with_isa(isa, default_libcall_names());
        // Make the gir_jit_helpers entry points resolvable from JIT'd
        // code. Each one is `#[no_mangle] extern "C"`, registered
        // here under the same symbol name we use in `declare_function`.
        for (name, ptr) in crate::gir_jit_helpers::all_symbols() {
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

/// A successfully-JITed kernel. The `fn_ptr` is the entry point for
/// the compiled function; cast it to `extern "C" fn(...)` of the
/// matching signature to invoke. The pointer stays valid as long as
/// the [`JitCtx`] that produced it is alive.
pub struct CompiledKernel {
    pub func_id: FuncId,
    pub fn_ptr: *const u8,
    pub signature: Signature,
    /// Per-kernel ArcStr slots referenced by the JIT'd code via
    /// stable pointers. Held here so it lives as long as the
    /// compiled function; drops alongside the JIT module when this
    /// struct drops.
    _strings: KernelStrings,
    /// Per-kernel datetime/duration `Value` slots referenced by the
    /// JIT'd code via stable `*const Value` pointers. Same lifetime
    /// discipline as `_strings`.
    _values: KernelValues,
}

unsafe impl Send for CompiledKernel {}
unsafe impl Sync for CompiledKernel {}

// ─── Public entry point ──────────────────────────────────────────

/// JIT-compile `kernel` and return a function pointer to the entry
/// point. The pointer's call signature matches the kernel's params /
/// return type — see the module docstring for the calling convention.
pub fn compile_kernel(jit: &mut JitCtx, kernel: &GirKernel) -> Result<CompiledKernel> {
    // `compile_kernel` is the bare-entry-point path (no wrapper, no
    // string-table lifecycle). Strings would still need to live
    // somewhere for the JIT'd code to reach them — for now just
    // leak them by storing in the returned CompiledKernel, same
    // policy as the wrapped path.
    let (func_id, sig, strings, values) = define_typed_kernel(jit, kernel)?;
    jit.module
        .finalize_definitions()
        .context("finalize_definitions")?;
    let fn_ptr = jit.module.get_finalized_function(func_id);
    Ok(CompiledKernel {
        func_id,
        fn_ptr,
        signature: sig,
        _strings: strings,
        _values: values,
    })
}

/// Define the typed kernel function in the JIT module without
/// finalizing. Returns the FuncId and Signature; the caller is
/// responsible for finalizing before extracting fn pointers.
/// Splitting this out from `compile_kernel` lets us emit a typed
/// kernel and a wrapper that calls it in the same module before a
/// single `finalize_definitions` call.
///
/// Push the kernel's parameter `AbiParam`s onto `sig` in kind-grouped
/// ABI order. Derives the order + wire footprint from
/// [`GirKernel::abi_params`] — the single source of truth — so this
/// and [`ensure_declared`] can't drift.
fn push_abi_params(sig: &mut Signature, kernel: &GirKernel) {
    for d in kernel.abi_params() {
        match d.kind {
            AbiParamKind::Scalar(p) => {
                sig.params.push(AbiParam::new(prim_to_clif(p)))
            }
            AbiParamKind::Array
            | AbiParamKind::Tuple
            | AbiParamKind::Struct
            | AbiParamKind::String => {
                sig.params.push(AbiParam::new(types::I64))
            }
            AbiParamKind::Variant
            | AbiParamKind::Nullable
            | AbiParamKind::Value => {
                sig.params.push(AbiParam::new(types::I64)); // disc
                sig.params.push(AbiParam::new(types::I64)); // payload
            }
        }
    }
}

/// Push the kernel's return `AbiParam`s onto `sig` (one value for
/// scalar/composite/string/unit returns, two for variant/nullable).
/// Errors on the invalid bare-`Null` return shape.
fn push_abi_returns(sig: &mut Signature, kernel: &GirKernel) -> Result<()> {
    match kernel.abi_return() {
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
                "kernel returns bare GirType::Null; should have \
                 widened to Nullable<T> at construction"
            ))
        }
    }
    Ok(())
}

fn define_typed_kernel(
    jit: &mut JitCtx,
    kernel: &GirKernel,
) -> Result<(FuncId, Signature, KernelStrings, KernelValues)> {
    // Fn-typed params (HOFs) dispatch through `graphix_dyncall` at
    // runtime via the dispatcher handle set in `GirNode::update`.
    // Both scalar- and composite-return DynCalls are supported:
    // scalar returns are branchless (0 is a harmless pending
    // sentinel for downstream arithmetic); composite returns get
    // a per-DynCall `pre_pending` block that drops the owned set
    // and jumps to `pending_exit`.
    let symbol = jit.next_symbol(&kernel.fn_name);
    let mut sig = Signature::new(jit.module.isa().default_call_conv());
    push_abi_params(&mut sig, kernel);
    push_abi_returns(&mut sig, kernel)?;

    let func_id = jit
        .module
        .declare_function(&symbol, Linkage::Local, &sig)
        .context("declare_function (typed)")?;
    // Defensive clear — see `define_kernel_body`. Prevents a prior
    // failed compile's leftover `func_ctx` from poisoning this build.
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    jit.func_ctx.func.signature = sig.clone();
    jit.func_ctx.func.name =
        cranelift_codegen::ir::UserFuncName::user(0, func_id.as_u32());

    // Build the per-kernel string slot table. Each unique struct
    // field name / variant tag goes through the global intern table
    // (which dedupes across the whole process). The resulting
    // `KernelStrings` owns one canonical clone of each string; the
    // boxed slice has stable addresses for codegen to use.
    let strings = KernelStrings::build(kernel);
    let values = KernelValues::build(kernel);
    {
        let helper_refs = declare_helpers(
            &mut jit.module,
            &mut jit.func_ctx.func,
            &jit.helper_ids,
        );
        let mut builder =
            FunctionBuilder::new(&mut jit.func_ctx.func, &mut jit.builder_ctx);
        compile_into_function(
            &mut builder,
            kernel,
            &BTreeMap::new(),
            &helper_refs,
            &strings,
            &values,
        )?;
        builder.finalize();
    }

    jit.module
        .define_function(func_id, &mut jit.func_ctx)
        .context("define_function (typed)")?;
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    Ok((func_id, sig, strings, values))
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
/// unpack helpers ([`pack_reg_to_u64`], [`unpack_u64_to_reg`]) handle
/// the Rust-side bit-fiddling.
///
/// Owns its `JitCtx` (for the local-module path) or holds `None` (for
/// the shared-module path, where a static [`SHARED_JIT`] keeps the
/// mmap'd code alive forever).
pub struct WrappedKernel {
    /// Type-erased entry point. Cast via transmute to the
    /// canonical `WrapperFn` signature for invocation.
    pub wrapper_fn_ptr: *const u8,
    /// `Some(_)` for kernels compiled into a private JIT module (no
    /// cross-kernel calls); the ctx keeps the mmap'd code alive. `None`
    /// for kernels compiled into [`SHARED_JIT`] — that static keeps
    /// them mapped for the program's lifetime, so no per-kernel
    /// ownership is needed.
    _ctx: Option<JitCtx>,
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

/// JIT-compile a kernel together with a uniform-signature wrapper, so
/// the runtime can dispatch into native code without knowing the
/// kernel's specific param/return types. Returns a [`WrappedKernel`]
/// owning the JIT context (and thus the mapped code).
///
/// Doesn't support kernels containing `GirOp::Call` — use
/// [`compile_kernel_with_callees`] for those.
pub fn compile_kernel_with_wrapper(kernel: &GirKernel) -> Result<WrappedKernel> {
    let mut ctx = JitCtx::new()?;
    let (typed_id, _, strings, values) = define_typed_kernel(&mut ctx, kernel)?;
    let wrapper_id = define_wrapper(&mut ctx, kernel, typed_id)?;
    ctx.module
        .finalize_definitions()
        .context("finalize_definitions (wrapper)")?;
    let wrapper_fn_ptr = ctx.module.get_finalized_function(wrapper_id);
    Ok(WrappedKernel {
        wrapper_fn_ptr,
        _ctx: Some(ctx),
        _strings: strings,
        _values: values,
    })
}

// ─── Per-context JIT module: cross-kernel CLIF calls ─────────────
//
// All kernels with `GirOp::Call` from a given `ExecCtx` go into a
// single JIT module owned by that ExecCtx, so that one kernel's
// compiled code can `call` another's directly via a CLIF `call`
// instruction. The module lives as long as the ExecCtx; when the
// ExecCtx drops, the module drops and the mapped code goes with it.
//
// `by_kernel` keys by `Arc<GirKernel>` raw-pointer identity so the
// same `Arc<GirKernel>` referenced from multiple parent kernels
// reuses one compilation within a single ExecCtx. Names alone
// aren't unique enough — two distinct programs can both have a
// binding `foo` with different GIR.
//
// (Was a process-global `SHARED_JIT` static before May 2026; moved
// to per-context to align with the runtime's documented "multiple
// ExecCtxes can hold different modes without racing on shared
// global state" guarantee.)

pub struct Jit {
    ctx: JitCtx,
    /// Per-kernel cache: Arc<GirKernel> raw pointer → cached entry.
    /// We keep the Arc alive in the entry so the raw pointer key
    /// stays valid for the lifetime of the ExecCtx. Without it,
    /// Arc-allocator reuse could land a different GIR at the same
    /// address and we'd return a stale FuncId pointing at code with
    /// the wrong signature.
    by_kernel: BTreeMap<usize, CachedKernel>,
}

impl Jit {
    /// Construct a fresh per-context JIT. Initializes the cranelift
    /// module + ISA. Returns `Err` if cranelift initialization fails
    /// (rare; only happens if the target ISA isn't supported).
    pub fn new() -> Result<Self> {
        Ok(Self { ctx: JitCtx::new()?, by_kernel: BTreeMap::new() })
    }
}

struct CachedKernel {
    func_id: FuncId,
    signature: Signature,
    /// Holds the Arc alive so its raw pointer can't be reused by a
    /// later allocation.
    _kernel: std::sync::Arc<GirKernel>,
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
}

unsafe impl Send for Jit {}

/// JIT-compile `kernel` alongside any kernels it calls (`callees`),
/// returning a [`WrappedKernel`] whose code can directly CLIF-call
/// the callees via `call` instructions (no interpreter dispatch on
/// the cross-kernel boundary). Used by lazy fusion when a kernel
/// body contains `GirOp::Call`.
///
/// `callees` must be the *transitive* closure: every kernel reachable
/// from `kernel` through `GirOp::Call`, by name. Each value is the
/// `Arc<GirKernel>` for that name. Each callee's GirKernel is keyed
/// by Arc identity in the per-context cache (`Jit::by_kernel`); the
/// same Arc referenced from multiple parents compiles once within
/// the same ExecCtx.
///
/// Compile is two-phase: every kernel in the closure is *declared*
/// (gets a `FuncId`) before any are *defined* (body compiled). This
/// supports transitive fan-out — kernel A calls B calls C all in the
/// shared module — and trivially supports mutual recursion (each body
/// can reference any other body's pre-declared `FuncId`).
///
/// On failure, the caller should fall back to the interpreter — the
/// kernel still has correct semantics there, just slower at the
/// cross-kernel boundary.
pub fn compile_kernel_with_callees(
    jit: &mut Jit,
    kernel: &std::sync::Arc<GirKernel>,
    callees: &BTreeMap<ArcStr, std::sync::Arc<GirKernel>>,
) -> Result<WrappedKernel> {
    // Phase 1 — declare every kernel in the closure (parent + all
    // transitively-reachable callees). Cached entries reuse their
    // `FuncId`; fresh ones get a freshly-declared FuncId and queue
    // for phase-2 body definition.
    //
    // The parent and any callee with name == parent's fn_name share
    // the same FuncId; that's how self-recursion via `GirOp::Call`
    // resolves to a CLIF call back to the parent.
    let kernel_name = kernel.fn_name.clone();
    let mut funcids: BTreeMap<ArcStr, (FuncId, Signature)> = BTreeMap::new();
    let mut to_define: Vec<std::sync::Arc<GirKernel>> = Vec::new();
    let parent_entry = ensure_declared(jit, kernel, &mut to_define)?;
    funcids.insert(kernel_name.clone(), parent_entry.clone());
    for (name, k) in callees {
        if name.as_str() == kernel_name.as_str() {
            funcids.insert(name.clone(), parent_entry.clone());
            continue;
        }
        let entry = ensure_declared(jit, k, &mut to_define)?;
        funcids.insert(name.clone(), entry);
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
    for k in &to_define {
        let (strings, values) = define_kernel_body(&mut jit.ctx, k, &funcids)?;
        let key = std::sync::Arc::as_ptr(k) as usize;
        if let Some(cached) = jit.by_kernel.get_mut(&key) {
            cached._strings = strings;
            cached._values = values;
        }
    }
    // Phase 3 — compile the uniform wrapper for the parent and
    // finalize the module so the new code is mapped read-execute.
    let wrapper_id = define_wrapper(&mut jit.ctx, kernel, parent_entry.0)?;
    jit
        .ctx
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
    Ok(WrappedKernel {
        wrapper_fn_ptr,
        _ctx: None,
        _strings: KernelStrings::empty(),
        _values: KernelValues::empty(),
    })
}

/// Phase-1 helper: ensure `k` has a `FuncId` declared in the shared
/// module. Cached kernels (by `Arc::as_ptr` identity) reuse their
/// existing entry. Freshly-declared kernels are pushed onto
/// `to_define` so phase 2 compiles their body.
fn ensure_declared(
    jit: &mut Jit,
    k: &std::sync::Arc<GirKernel>,
    to_define: &mut Vec<std::sync::Arc<GirKernel>>,
) -> Result<(FuncId, Signature)> {
    let key = std::sync::Arc::as_ptr(k) as usize;
    if let Some(e) = jit.by_kernel.get(&key) {
        return Ok((e.func_id, e.signature.clone()));
    }
    let symbol = jit.ctx.next_symbol(&k.fn_name);
    let mut sig = Signature::new(jit.ctx.module.isa().default_call_conv());
    push_abi_params(&mut sig, k);
    push_abi_returns(&mut sig, k)?;
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
        },
    );
    to_define.push(k.clone());
    Ok((fid, sig))
}

/// Phase-2 helper: compile `kernel`'s body and call `define_function`
/// on its pre-declared `FuncId`. `funcids` must contain entries for
/// the kernel itself and every callee its body references via
/// `GirOp::Call`.
fn define_kernel_body(
    jit: &mut JitCtx,
    kernel: &GirKernel,
    funcids: &BTreeMap<ArcStr, (FuncId, Signature)>,
) -> Result<(KernelStrings, KernelValues)> {
    let (func_id, sig) =
        funcids.get(&kernel.fn_name).cloned().ok_or_else(|| {
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
    let (strings, values) = {
        // Declare each Call site's callee as a FuncRef in this
        // function. Done before constructing the FunctionBuilder
        // because both `declare_func_in_func` and `FunctionBuilder::new`
        // borrow `jit.func_ctx.func` mutably.
        let needed = crate::gir::collect_call_sites(kernel);
        let mut callee_refs: BTreeMap<ArcStr, FuncRef> = BTreeMap::new();
        for name in needed {
            let target_fid = funcids.get(&name).map(|(f, _)| *f).ok_or_else(
                || {
                    anyhow!(
                        "define_kernel_body: kernel `{}` calls `{name}` \
                         but no entry in funcids",
                        kernel.fn_name
                    )
                },
            )?;
            let fref = jit
                .module
                .declare_func_in_func(target_fid, &mut jit.func_ctx.func);
            callee_refs.insert(name, fref);
        }
        let strings = KernelStrings::build(kernel);
        let values = KernelValues::build(kernel);
        let helper_refs = declare_helpers(
            &mut jit.module,
            &mut jit.func_ctx.func,
            &jit.helper_ids,
        );
        let mut builder =
            FunctionBuilder::new(&mut jit.func_ctx.func, &mut jit.builder_ctx);
        compile_into_function(
            &mut builder,
            kernel,
            &callee_refs,
            &helper_refs,
            &strings,
            &values,
        )?;
        builder.finalize();
        // Hand `strings`/`values` back to the caller, which stores
        // them in the shared module's per-kernel cache entry — the
        // JIT'd code baked in `*const ArcStr` / `*const Value`
        // pointers into these tables and the shared module's code
        // outlives this function.
        (strings, values)
    };
    jit.module
        .define_function(func_id, &mut jit.func_ctx)
        .context("define_function (shared body)")?;
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    Ok((strings, values))
}

/// Define the (args*, out*) wrapper that adapts the typed kernel to a
/// uniform Rust-side calling convention. The wrapper:
/// 1. Loads each arg from the raw u64 slot at the correct CLIF type.
/// 2. Calls the typed kernel.
/// 3. Stores the result into the out slot as raw bits.
fn define_wrapper(
    jit: &mut JitCtx,
    kernel: &GirKernel,
    typed_func_id: FuncId,
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
    // Defensive clear — see `define_kernel_body`. A prior failed
    // compile must not poison the wrapper build.
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    jit.func_ctx.func.signature = sig.clone();
    jit.func_ctx.func.name =
        cranelift_codegen::ir::UserFuncName::user(0, wrapper_id.as_u32());

    {
        let typed_ref = jit.module.declare_func_in_func(
            typed_func_id,
            &mut jit.func_ctx.func,
        );
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
                .ok_or_else(|| {
                    anyhow!("missing graphix_record_jit_invocation FuncId")
                })?;
            jit.module.declare_func_in_func(fid, &mut jit.func_ctx.func)
        };
        let mut b =
            FunctionBuilder::new(&mut jit.func_ctx.func, &mut jit.builder_ctx);
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
        // kind-grouped ABI order (see `GirKernel::abi_params`). Scalar
        // params load at their narrow CLIF type; composite params load
        // one `I64` (a `*ValArray` the caller stored as u64); variant /
        // nullable params load two `I64`s (disc, payload).
        // `d.wire_slot` is the param's starting 8-byte slot offset.
        let mut typed_args = Vec::with_capacity(kernel.abi_param_wire_slots());
        for d in kernel.abi_params() {
            let base = (d.wire_slot as i32) * 8;
            match d.kind {
                AbiParamKind::Scalar(p) => {
                    let cty = prim_to_clif(p);
                    let v =
                        b.ins().load(cty, MemFlags::trusted(), args_ptr, base);
                    typed_args.push(v);
                }
                AbiParamKind::Array
                | AbiParamKind::Tuple
                | AbiParamKind::Struct
                | AbiParamKind::String => {
                    let v = b.ins().load(
                        types::I64,
                        MemFlags::trusted(),
                        args_ptr,
                        base,
                    );
                    typed_args.push(v);
                }
                AbiParamKind::Variant
                | AbiParamKind::Nullable
                | AbiParamKind::Value => {
                    let disc = b.ins().load(
                        types::I64,
                        MemFlags::trusted(),
                        args_ptr,
                        base,
                    );
                    let payload = b.ins().load(
                        types::I64,
                        MemFlags::trusted(),
                        args_ptr,
                        base + 8,
                    );
                    typed_args.push(disc);
                    typed_args.push(payload);
                }
            }
        }

        let call = b.ins().call(typed_ref, &typed_args);
        // Two-word return shape comes from the same single source as
        // the signature (`abi_return`). `None` (bare Null) can't reach
        // here — the kernel's signature build would have errored first.
        let returns_two_words =
            matches!(kernel.abi_return(), Some(AbiReturn::Two));
        let (r0, r1_opt) = {
            let results = b.inst_results(call);
            (
                results[0],
                if returns_two_words { Some(results[1]) } else { None },
            )
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

    jit.module
        .define_function(wrapper_id, &mut jit.func_ctx)
        .context("define_function (wrapper)")?;
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    Ok(wrapper_id)
}

/// Pack a [`crate::gir_interp::RegValue`] into a u64 slot for passing
/// into a JIT'd wrapper. The bits represent the primitive's value;
/// for narrower primitives the upper bits are unused (the wrapper
/// loads at the CLIF type and ignores them).
pub fn pack_reg_to_u64(r: &crate::gir_interp::RegValue) -> u64 {
    use crate::gir_interp::RegValue as R;
    match r {
        R::I8(x) => *x as i64 as u64,
        R::I16(x) => *x as i64 as u64,
        R::I32(x) => *x as i64 as u64,
        R::I64(x) => *x as u64,
        R::U8(x) => *x as u64,
        R::U16(x) => *x as u64,
        R::U32(x) => *x as u64,
        R::U64(x) => *x,
        R::F32(x) => x.to_bits() as u64,
        R::F64(x) => x.to_bits(),
        R::Bool(b) => *b as u64,
    }
}

/// Unpack a u64 slot from a JIT'd wrapper's `out` parameter into the
/// appropriate [`crate::gir_interp::RegValue`] variant.
pub fn unpack_u64_to_reg(
    bits: u64,
    prim: PrimType,
) -> crate::gir_interp::RegValue {
    use crate::gir_interp::RegValue as R;
    match prim {
        PrimType::I8 => R::I8(bits as i8),
        PrimType::I16 => R::I16(bits as i16),
        PrimType::I32 => R::I32(bits as i32),
        PrimType::I64 => R::I64(bits as i64),
        PrimType::U8 => R::U8(bits as u8),
        PrimType::U16 => R::U16(bits as u16),
        PrimType::U32 => R::U32(bits as u32),
        PrimType::U64 => R::U64(bits),
        PrimType::F32 => R::F32(f32::from_bits(bits as u32)),
        PrimType::F64 => R::F64(f64::from_bits(bits)),
        PrimType::Bool => R::Bool(bits != 0),
    }
}

// ─── Async JIT compile ───────────────────────────────────────────
//
// A single shared worker thread processes JIT compile requests off
// the runtime path. GirNode holds an `Arc<AsyncJitSlot>` whose
// internal `OnceLock` is empty until the worker fills it. The
// runtime thread's `update` pays one atomic load per call to check
// — once filled, it dispatches to native code; until then, it
// interprets. No atomic swap needed: `OnceLock` is set-once.
//
// Why not a thread pool: even one worker keeps codegen entirely off
// the runtime path, which is the win we want. A pool helps only if
// many compile requests pile up at startup; for typical programs
// (handful of fusable lambdas), a single worker drains in
// background well before any kernel is hot.

/// Set-once JIT slot. GirNode reads via `fetch()` on every update
/// (one atomic load); the worker calls `fill()` once when the kernel
/// finishes compiling. Failures (cranelift unsupported op, OOM, …)
/// leave the slot empty forever — the interpreter keeps running.
#[derive(Default)]
pub struct AsyncJitSlot {
    slot: std::sync::OnceLock<std::sync::Arc<WrappedKernel>>,
}

impl AsyncJitSlot {
    pub fn new() -> std::sync::Arc<Self> {
        std::sync::Arc::new(Self { slot: std::sync::OnceLock::new() })
    }

    /// Fast path checked on every GirNode update. Returns `None`
    /// while the worker is still compiling, `Some(_)` once the
    /// slot has been filled.
    pub fn fetch(&self) -> Option<std::sync::Arc<WrappedKernel>> {
        self.slot.get().cloned()
    }
}

struct CompileRequest {
    kernel: std::sync::Arc<crate::gir::GirKernel>,
    slot: std::sync::Arc<AsyncJitSlot>,
    name: arcstr::ArcStr,
}

static JIT_WORKER: std::sync::LazyLock<std::sync::mpsc::SyncSender<CompileRequest>> =
    std::sync::LazyLock::new(|| {
        // Bounded channel with generous buffer so a flurry of
        // Lambda::compiles at program startup don't block.
        let (tx, rx) = std::sync::mpsc::sync_channel::<CompileRequest>(1024);
        std::thread::Builder::new()
            .name("graphix-jit-compile".to_string())
            .spawn(move || {
                while let Ok(req) = rx.recv() {
                    match compile_kernel_with_wrapper(&req.kernel) {
                        Ok(wrapped) => {
                            // OnceLock::set returns Err if already
                            // set; we never re-fill, so the only way
                            // to hit that is two requests aliasing
                            // the same slot — shouldn't happen, but
                            // ignore if it does.
                            let _ = req.slot.slot.set(std::sync::Arc::new(wrapped));
                        }
                        Err(e) => {
                            log::warn!(
                                "gir_jit (async): compile failed for {}: \
                                 {e:#}; GirNode will stay on interpreter",
                                req.name
                            );
                            // Slot stays empty — GirNode keeps
                            // interpreting forever. Acceptable
                            // graceful degradation.
                        }
                    }
                }
            })
            .expect("spawn gir_jit worker thread");
        tx
    });

/// Queue an async JIT compile of `kernel`. Returns immediately with
/// an empty [`AsyncJitSlot`]; the worker fills it when codegen
/// completes (typically tens to a few hundred ms later, depending
/// on kernel size). GirNode polls the slot on each update and swaps
/// from interpreter to native dispatch transparently.
pub fn submit_async_compile(
    kernel: std::sync::Arc<crate::gir::GirKernel>,
    name: arcstr::ArcStr,
) -> std::sync::Arc<AsyncJitSlot> {
    let slot = AsyncJitSlot::new();
    let req = CompileRequest { kernel, slot: slot.clone(), name };
    if JIT_WORKER.try_send(req).is_err() {
        // Channel full or worker dead. We log once (TODO: throttle)
        // and return the empty slot — GirNode just keeps interpreting.
        log::warn!(
            "gir_jit (async): worker queue full or thread gone; \
             skipping JIT for this kernel"
        );
    }
    slot
}

// ─── Function shape ──────────────────────────────────────────────

fn compile_into_function(
    b: &mut FunctionBuilder,
    kernel: &GirKernel,
    callee_refs: &BTreeMap<ArcStr, FuncRef>,
    helper_refs: &HelperRefs,
    strings: &KernelStrings,
    values: &KernelValues,
) -> Result<()> {
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
    // Bind every kernel param into the env in kind-grouped ABI order
    // (see `GirKernel::abi_params`), reading entry-block params from
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
    for d in kernel.abi_params() {
        match d.kind {
            AbiParamKind::Scalar(prim) => {
                let cty = prim_to_clif(prim);
                let var = b.declare_var(cty);
                b.def_var(var, initial_vals[d.wire_slot]);
                env.bind(d.name.clone(), var, prim);
            }
            AbiParamKind::Array
            | AbiParamKind::Tuple
            | AbiParamKind::Struct => {
                let borrowed = initial_vals[d.wire_slot];
                let call = b.ins().call(clone_helper, &[borrowed]);
                let owned = b.inst_results(call)[0];
                let var = b.declare_var(types::I64);
                b.def_var(var, owned);
                env.bind_composite(d.name.clone(), var);
            }
            AbiParamKind::String => {
                // String param: borrowed ArcStr thin pointer at one
                // slot; clone (refcount-bump) into an owned local so
                // scope-exit drop discipline is uniform with locals.
                let borrowed = initial_vals[d.wire_slot];
                let clone = helper_refs
                    .get("graphix_arcstr_clone")
                    .expect("graphix_arcstr_clone helper must be registered");
                let call = b.ins().call(clone, &[borrowed]);
                let owned = b.inst_results(call)[0];
                let var = b.declare_var(types::I64);
                b.def_var(var, owned);
                env.bind_string(d.name.clone(), var);
            }
            AbiParamKind::Variant
            | AbiParamKind::Nullable
            | AbiParamKind::Value => {
                let borrowed_disc = initial_vals[d.wire_slot];
                let borrowed_payload = initial_vals[d.wire_slot + 1];
                let call = b.ins().call(
                    value_clone_helper,
                    &[borrowed_disc, borrowed_payload],
                );
                let (owned_disc, owned_payload) = {
                    let r = b.inst_results(call);
                    (r[0], r[1])
                };
                let disc_var = b.declare_var(types::I64);
                let payload_var = b.declare_var(types::I64);
                b.def_var(disc_var, owned_disc);
                b.def_var(payload_var, owned_payload);
                let vv = ValueVar { disc: disc_var, payload: payload_var };
                match d.kind {
                    AbiParamKind::Variant => env.bind_variant(d.name.clone(), vv),
                    // Nullable and bare value-shape (DateTime/Duration/
                    // Bytes) both ride the `nullables` ValueVar slot;
                    // a `Local` read re-wraps to the declared type.
                    AbiParamKind::Nullable | AbiParamKind::Value => {
                        env.bind_nullable(d.name.clone(), vv)
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
    b.seal_block(entry);
    // Snapshot the env now that every param (scalar, composite,
    // variant) is bound. A TailCall rebind truncates back to exactly
    // this — per-iteration block/select-arm locals are dropped, the
    // params stay.
    let param_mark = env.mark();

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
        callee_refs,
        helper_refs,
        tail_call_slots,
        strings,
        values,
        dyncall_buf_stack: std::cell::RefCell::new(Vec::new()),
        pending_exit: std::cell::RefCell::new(None),
    };
    compile_body(b, &kernel.body, &mut env, &lower)?;

    if let Some(head) = loop_head {
        b.seal_block(head);
    }

    // If any composite-return DynCall site created the lazy
    // `pending_exit` block, emit its body now: a sentinel of the
    // kernel's return type plus `return`. All `pre_pending_<n>`
    // blocks already dropped the owned set before jumping here, so
    // `pending_exit` itself owns nothing.
    //
    // The kernel result on the pending path is discarded by
    // `GirNode::update` (which checks `DYNCALL_PENDING` after the
    // wrapper returns), so the sentinel value is never observed —
    // it just has to be a well-typed CLIF value of the right width.
    let pending_exit_block = *lower.pending_exit.borrow();
    if let Some(pe) = pending_exit_block {
        b.switch_to_block(pe);
        match &kernel.return_type {
            GirType::Prim(p) => {
                let s = zero_const(b, *p);
                b.ins().return_(&[s]);
            }
            // Composite returns: the kernel's typed signature returns
            // a single `I64` pointer. The pending sentinel is null.
            GirType::Array(_)
            | GirType::Tuple(_)
            | GirType::Struct(_)
            // Unit returns the I64 ABI slot too (the caller
            // discards).
            | GirType::Unit => {
                let s = b.ins().iconst(types::I64, 0);
                b.ins().return_(&[s]);
            }
            // Value-shape returns: two `I64`s (disc, payload). The
            // pending sentinel is `(0, 0)` — the caller's pending
            // check fires from `DYNCALL_PENDING` before decoding,
            // so the bits are never observed.
            GirType::Variant(_)
            | GirType::Nullable(_)
            | GirType::DateTime
            | GirType::Duration
            | GirType::Bytes | GirType::Map | GirType::Error => {
                let s0 = b.ins().iconst(types::I64, 0);
                let s1 = b.ins().iconst(types::I64, 0);
                b.ins().return_(&[s0, s1]);
            }
            // String returns travel as a single `i64` (ArcStr's thin
            // pointer). Sentinel is `0` (null pointer) — the caller
            // checks the pending flag before decoding, so a null
            // ArcStr pointer never reaches `GirNode::update`'s
            // boundary marshaling.
            GirType::String => {
                let s = b.ins().iconst(types::I64, 0);
                b.ins().return_(&[s]);
            }
            // Bare Null returns aren't a real shape — fusion widens
            // to Nullable<T> before producing.
            GirType::Null => unreachable!(
                "kernel returns bare GirType::Null but reached JIT \
                 pending-exit emission — should have widened earlier"
            ),
        }
    }

    // After body compilation, every block has been sealed except
    // possibly some auxiliary blocks (select arms, if-chain merges,
    // pending_exit). FunctionBuilder requires all blocks be sealed
    // before finalize; seal_all_blocks catches the stragglers.
    b.seal_all_blocks();
    Ok(())
}

/// Per-kernel storage of the ArcStrs the JIT'd code references via
/// stable `*const ArcStr` pointers.
///
/// Each unique string used by the kernel is interned through the
/// global [`crate::gir_jit_intern`] table (which gives back a
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
}

impl KernelStrings {
    /// An empty string table — for kernels that reference no strings,
    /// or as a placeholder before the real table is built.
    pub fn empty() -> Self {
        Self { slots: Box::new([]), index: BTreeMap::new() }
    }

    /// Pre-walk `kernel`, intern every string it references, and
    /// store the resulting canonical clones at stable positions.
    pub fn build(kernel: &GirKernel) -> Self {
        let mut unique: std::collections::BTreeSet<ArcStr> =
            std::collections::BTreeSet::new();
        for stmt in &kernel.body {
            collect_strings_stmt(stmt, &mut unique);
        }
        let mut slots: Vec<ArcStr> = Vec::with_capacity(unique.len());
        let mut index: BTreeMap<ArcStr, usize> = BTreeMap::new();
        for s in unique {
            let canonical = crate::gir_jit_intern::intern(&s);
            index.insert(s.clone(), slots.len());
            slots.push(canonical);
        }
        Self { slots: slots.into_boxed_slice(), index }
    }

    /// Get the stable `*const ArcStr` for `s`. Panics if `s` wasn't
    /// in the pre-walk (i.e. a string-using op was missed).
    pub fn get(&self, s: &ArcStr) -> *const ArcStr {
        let i = *self.index.get(s).unwrap_or_else(|| {
            panic!(
                "KernelStrings: lookup miss for `{}` — pre-walk must \
                 have missed a string-using op",
                s
            )
        });
        &self.slots[i] as *const ArcStr
    }
}

/// Per-kernel datetime/duration constants table — stable-address
/// `Value` slots whose `*const Value` the codegen bakes for
/// [`GirOp::ConstValue`]. Mirrors [`KernelStrings`]: the `Box<[Value]>`
/// never moves its heap allocation, so the baked pointers stay valid
/// as long as the table (held on the kernel) lives.
pub struct KernelValues {
    slots: Box<[Value]>,
}

impl KernelValues {
    pub fn empty() -> Self {
        Self { slots: Box::new([]) }
    }

    /// Pre-walk `kernel`, collect each distinct `ConstValue` literal.
    pub fn build(kernel: &GirKernel) -> Self {
        let mut slots: Vec<Value> = Vec::new();
        for stmt in &kernel.body {
            collect_values_stmt(stmt, &mut slots);
        }
        Self { slots: slots.into_boxed_slice() }
    }

    /// Stable `*const Value` for `v`. Panics on a pre-walk miss (a
    /// `ConstValue`-using op the walk didn't cover).
    pub fn get(&self, v: &Value) -> *const Value {
        self.slots
            .iter()
            .find(|s| *s == v)
            .map(|s| s as *const Value)
            .unwrap_or_else(|| {
                panic!("KernelValues: lookup miss for a ConstValue literal")
            })
    }
}

fn collect_values_stmt(s: &GirStmt, out: &mut Vec<Value>) {
    match s {
        GirStmt::Let(l) => collect_values_expr(&l.value, out),
        GirStmt::Return(e) | GirStmt::Discard(e) => collect_values_expr(e, out),
        GirStmt::TailCall { args } => {
            args.iter().for_each(|a| collect_values_expr(a, out))
        }
        GirStmt::Select { arms } => {
            for arm in arms {
                if let Some(c) = &arm.cond {
                    collect_values_expr(c, out);
                }
                arm.body.iter().for_each(|s| collect_values_stmt(s, out));
            }
        }
    }
}

fn collect_values_expr(e: &GirExpr, out: &mut Vec<Value>) {
    if let GirOp::ConstValue(v) = &e.op {
        if !out.iter().any(|s| s == v) {
            out.push(v.clone());
        }
    }
    match &e.op {
        GirOp::ValueArith { lhs, rhs, .. }
        | GirOp::ValueEq { lhs, rhs, .. }
        | GirOp::Bin { lhs, rhs, .. }
        | GirOp::Cmp { lhs, rhs, .. }
        | GirOp::BoolBin { lhs, rhs, .. } => {
            collect_values_expr(lhs, out);
            collect_values_expr(rhs, out);
        }
        GirOp::IsNull(i)
        | GirOp::Not(i)
        | GirOp::Cast { inner: i, .. }
        | GirOp::QopUnwrap { inner: i, .. } => collect_values_expr(i, out),
        GirOp::Concat(parts) => {
            parts.iter().for_each(|p| collect_values_expr(p, out))
        }
        GirOp::Block { lets, tail } => {
            lets.iter().for_each(|l| collect_values_expr(&l.value, out));
            collect_values_expr(tail, out);
        }
        GirOp::IfChain { arms } => {
            for (c, v) in arms {
                if let Some(c) = c {
                    collect_values_expr(c, out);
                }
                collect_values_expr(v, out);
            }
        }
        GirOp::Call { args, .. } | GirOp::DynCall { args, .. } => {
            args.iter().for_each(|a| collect_values_expr(a, out))
        }
        GirOp::ArrayGet { idx, .. } => collect_values_expr(idx, out),
        GirOp::BytesIndex { bytes, idx } => {
            collect_values_expr(bytes, out);
            collect_values_expr(idx, out);
        }
        GirOp::MapRef { map, key } => {
            collect_values_expr(map, out);
            collect_values_expr(key, out);
        }
        GirOp::ArraySlice { source, start, end } => {
            collect_values_expr(source, out);
            if let Some(e) = start {
                collect_values_expr(e, out);
            }
            if let Some(e) = end {
                collect_values_expr(e, out);
            }
        }
        GirOp::ArrayFold { init, body, .. } => {
            collect_values_expr(init, out);
            collect_values_expr(body, out);
        }
        GirOp::ArrayInit { n, body, .. } => {
            collect_values_expr(n, out);
            collect_values_expr(body, out);
        }
        GirOp::ArrayMap { body, .. } => collect_values_expr(body, out),
        GirOp::ArrayFilterMap { body, .. } => collect_values_expr(body, out),
        GirOp::ArrayFindMap { body, .. } => collect_values_expr(body, out),
        GirOp::ArrayFlatMap { body, .. } => collect_values_expr(body, out),
        GirOp::ArrayFilter { predicate, .. } => {
            collect_values_expr(predicate, out)
        }
        GirOp::ArrayFind { predicate, .. } => {
            collect_values_expr(predicate, out)
        }
        GirOp::TupleNew { fields, .. } => {
            fields.iter().for_each(|f| collect_values_expr(f, out))
        }
        GirOp::StructNew { sorted_fields, .. } => {
            sorted_fields.iter().for_each(|(_, f)| collect_values_expr(f, out))
        }
        GirOp::VariantNew { payloads, .. } => {
            payloads.iter().for_each(|p| collect_values_expr(p, out))
        }
        GirOp::Const(_)
        | GirOp::ConstStr(_)
        | GirOp::ConstValue(_)
        | GirOp::ConstNull
        | GirOp::Local(_)
        | GirOp::ArrayLen { .. }
        | GirOp::TupleGet { .. }
        | GirOp::StructGet { .. }
        | GirOp::VariantTagEq { .. }
        | GirOp::VariantPayload { .. } => {}
    }
}

fn collect_strings_stmt(s: &GirStmt, out: &mut std::collections::BTreeSet<ArcStr>) {
    match s {
        GirStmt::Let(l) => collect_strings_expr(&l.value, out),
        GirStmt::Return(e) => collect_strings_expr(e, out),
        GirStmt::Discard(e) => collect_strings_expr(e, out),
        GirStmt::TailCall { args } => {
            for a in args {
                collect_strings_expr(a, out);
            }
        }
        GirStmt::Select { arms } => {
            for arm in arms {
                if let Some(c) = &arm.cond {
                    collect_strings_expr(c, out);
                }
                for s in &arm.body {
                    collect_strings_stmt(s, out);
                }
            }
        }
    }
}

fn collect_strings_expr(e: &GirExpr, out: &mut std::collections::BTreeSet<ArcStr>) {
    match &e.op {
        GirOp::Const(_) | GirOp::ConstValue(_) | GirOp::ConstNull
        | GirOp::Local(_) => {}
        GirOp::IsNull(inner) => collect_strings_expr(inner, out),
        GirOp::QopUnwrap { inner, .. } => collect_strings_expr(inner, out),
        GirOp::ValueArith { lhs, rhs, .. }
        | GirOp::ValueEq { lhs, rhs, .. } => {
            collect_strings_expr(lhs, out);
            collect_strings_expr(rhs, out);
        }
        GirOp::ConstStr(s) => {
            // String-containing kernels are routed to interp via
            // `kernel_contains_string`; this branch shouldn't run.
            // Intern defensively in case routing changes.
            out.insert(s.clone());
        }
        GirOp::Concat(parts) => {
            for p in parts {
                collect_strings_expr(p, out);
            }
        }
        GirOp::Bin { lhs, rhs, .. }
        | GirOp::Cmp { lhs, rhs, .. }
        | GirOp::BoolBin { lhs, rhs, .. } => {
            collect_strings_expr(lhs, out);
            collect_strings_expr(rhs, out);
        }
        GirOp::Not(e) | GirOp::Cast { inner: e, .. } => {
            collect_strings_expr(e, out)
        }
        GirOp::Call { args, .. } | GirOp::DynCall { args, .. } => {
            for a in args {
                collect_strings_expr(a, out);
            }
        }
        GirOp::Block { lets, tail } => {
            for l in lets {
                collect_strings_expr(&l.value, out);
            }
            collect_strings_expr(tail, out);
        }
        GirOp::IfChain { arms } => {
            for (cond, body) in arms {
                if let Some(c) = cond {
                    collect_strings_expr(c, out);
                }
                collect_strings_expr(body, out);
            }
        }
        GirOp::BytesIndex { bytes, idx } => {
            collect_strings_expr(bytes, out);
            collect_strings_expr(idx, out);
        }
        GirOp::MapRef { map, key } => {
            collect_strings_expr(map, out);
            collect_strings_expr(key, out);
        }
        GirOp::ArraySlice { source, start, end } => {
            collect_strings_expr(source, out);
            if let Some(e) = start {
                collect_strings_expr(e, out);
            }
            if let Some(e) = end {
                collect_strings_expr(e, out);
            }
        }
        GirOp::ArrayLen { .. }
        | GirOp::ArrayGet { .. }
        | GirOp::TupleGet { .. }
        | GirOp::StructGet { .. }
        | GirOp::VariantPayload { .. } => {}
        GirOp::VariantTagEq { expected_tag, .. } => {
            // The expected tag is emitted as an iconst in codegen,
            // so the per-kernel KernelStrings must have it interned.
            out.insert(expected_tag.clone());
        }
        GirOp::ArrayFold { init, body, .. } => {
            collect_strings_expr(init, out);
            collect_strings_expr(body, out);
        }
        GirOp::ArrayInit { n, body, .. } => {
            collect_strings_expr(n, out);
            collect_strings_expr(body, out);
        }
        GirOp::ArrayMap { body, .. } => collect_strings_expr(body, out),
        GirOp::ArrayFilterMap { body, .. } => collect_strings_expr(body, out),
        GirOp::ArrayFindMap { body, .. } => collect_strings_expr(body, out),
        GirOp::ArrayFlatMap { body, .. } => collect_strings_expr(body, out),
        GirOp::ArrayFilter { predicate, .. } => {
            collect_strings_expr(predicate, out)
        }
        GirOp::ArrayFind { predicate, .. } => {
            collect_strings_expr(predicate, out)
        }
        GirOp::TupleNew { fields, .. } => {
            for f in fields {
                collect_strings_expr(f, out);
            }
        }
        GirOp::StructNew { sorted_fields, .. } => {
            for (name, e) in sorted_fields {
                out.insert(name.clone());
                collect_strings_expr(e, out);
            }
        }
        GirOp::VariantNew { tag, payloads, .. } => {
            out.insert(tag.clone());
            for p in payloads {
                collect_strings_expr(p, out);
            }
        }
    }
}

/// Per-function lowering context: things that don't change across
/// statements within a single body.
struct LowerCtx<'a> {
    /// `Some(block)` when the kernel has a tail loop; TailCall jumps
    /// here. `None` for non-tail-recursive kernels.
    loop_head: Option<Block>,
    /// Env snapshot taken right after all params are bound. A
    /// tail-call rebind truncates `env` back to this so per-iteration
    /// block / select-arm locals (scalar, composite, and variant)
    /// don't leak across iterations.
    param_mark: EnvMark,
    /// `GirOp::Call { fn_name }` resolves through this map to a CLIF
    /// `FuncRef`. The caller must `declare_func_in_func` each callee's
    /// `FuncId` against the current function before constructing the
    /// FunctionBuilder, then pass the resulting refs in here. Empty
    /// for kernels with no `GirOp::Call` sites.
    callee_refs: &'a BTreeMap<ArcStr, FuncRef>,
    /// `FuncRef`s for the `gir_jit_helpers::*` runtime helpers.
    /// Declared in the current function before the FunctionBuilder
    /// is constructed (same constraint as `callee_refs`). Lookups
    /// are by helper name (e.g. `"graphix_valarray_get_i64"`).
    helper_refs: &'a HelperRefs,
    /// Per-source-position tail-call slot map (from
    /// `GirKernel::tail_call_slots`). Drives which Variable each
    /// tail-call arg rebinds into — scalar slots hit `env.locals`,
    /// composite slots hit `env.composites`. `None` for kernels
    /// without a tail loop (or that hand-built fixtures leave
    /// empty).
    tail_call_slots: Option<&'a [crate::gir::TailCallSlot]>,
    /// Per-kernel string table. StructNew / VariantNew codegen
    /// looks up field names / variant tags here to get stable
    /// `*const ArcStr` pointers to emit as iconst values. Lives
    /// on the resulting `WrappedKernel` so the pointers remain
    /// valid for as long as the compiled code does.
    strings: &'a KernelStrings,
    /// Per-kernel datetime/duration constants table. `GirOp::ConstValue`
    /// codegen looks up the literal here to get a stable `*const Value`
    /// to emit as an iconst, then clones it (`graphix_value_clone_from_static`).
    /// Lives on the resulting kernel so the pointers stay valid as long
    /// as the compiled code does (same discipline as `strings`).
    values: &'a KernelValues,
    /// Stack of in-flight DynCall args bufs (`*mut LPooled<Vec<Value>>`
    /// Variables). Each DynCall pushes its args buf at `buf_new` and
    /// pops it once `graphix_dyncall` has consumed it. A composite-
    /// return DynCall that pends emits a `pre_pending` block that
    /// drops whatever is still on this stack (= the args bufs of
    /// OUTER, not-yet-dispatched DynCalls) plus every owned
    /// composite/variant local. Producer-op bufs never appear here:
    /// a composite-return DynCall can't lexically sit inside a
    /// producer op's scalar fields/body, so no producer buf is ever
    /// in flight at a composite-DynCall pending point.
    dyncall_buf_stack: std::cell::RefCell<Vec<Variable>>,
    /// Lazily-created single `pending_exit` block. Created on the
    /// first composite-return DynCall site; its body (sentinel +
    /// `return`) is emitted at the end of `compile_into_function`.
    /// All `pre_pending_<n>` blocks jump here after dropping the
    /// owned set.
    pending_exit: std::cell::RefCell<Option<Block>>,
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
        for (name, _ptr) in crate::gir_jit_helpers::all_symbols() {
            let sig = helper_signature(module, name)?;
            let fid = module
                .declare_function(name, Linkage::Import, &sig)
                .with_context(|| {
                    format!("declare_function for helper `{name}`")
                })?;
            ids.insert(name, fid);
        }
        Ok(Self { ids })
    }
}

fn helper_signature(module: &JITModule, name: &str) -> Result<Signature> {
    let mut sig = Signature::new(module.isa().default_call_conv());
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
        | "graphix_valarray_get_arcstr"
        | "graphix_struct_get_array"
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
        "graphix_value_buf_push_bool"
        | "graphix_value_buf_push_i8"
        | "graphix_value_buf_push_u8" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I8));
        }
        "graphix_value_buf_push_i16" | "graphix_value_buf_push_u16" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I16));
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
        // Clone a `*const Value` static (datetime/duration ConstValue):
        // one pointer arg, a Value (two words) returned.
        | "graphix_value_clone_from_static" => {
            sig.params.push(AbiParam::new(types::I64)); // *mut ValArray / *const ArcStr / *const Value
            sig.returns.push(AbiParam::new(types::I64)); // ret.disc
            sig.returns.push(AbiParam::new(types::I64)); // ret.payload
        }
        // Value arithmetic (datetime/duration `ValueArith`): two Value
        // args (four words) in, one Value (two words) out.
        "graphix_value_add"
        | "graphix_value_sub"
        | "graphix_value_mul"
        | "graphix_value_div"
        | "graphix_value_rem"
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
            sig.params.push(AbiParam::new(types::I8));
            sig.returns.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "graphix_dyncall_pending_take" => {
            sig.returns.push(AbiParam::new(types::I8));
        }
        "graphix_dyncall_set_pending" => {
            // no args, no return — just flips the thread-local flag
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
        "graphix_string_buf_new" => {
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
        "graphix_string_buf_push_i16" | "graphix_string_buf_push_u16" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I16));
        }
        // (buf: i64, v: i8) -> ()
        "graphix_string_buf_push_i8"
        | "graphix_string_buf_push_u8"
        | "graphix_string_buf_push_bool" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I8));
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
// const block at the bottom of `gir_jit_helpers.rs` keeps Value's
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
fn prim_to_value_disc(p: PrimType) -> i64 {
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

/// Result of compiling a [`GirExpr`]: either a single CLIF value
/// (for scalar/composite/pointer-shaped exprs) or a Value-shaped
/// `(disc, payload)` pair (for variant/nullable exprs). Consumers
/// dispatch on the expression's `GirType`; the helpers `single()` /
/// `value()` assert the expected variant and bail loudly if a site
/// mishandles a Value-shaped result.
#[derive(Debug, Clone, Copy)]
enum CompiledExpr {
    Single(ClifValue),
    Value { disc: ClifValue, payload: ClifValue },
}

impl CompiledExpr {
    fn single(self) -> Result<ClifValue> {
        match self {
            CompiledExpr::Single(v) => Ok(v),
            CompiledExpr::Value { .. } => Err(anyhow!(
                "JIT: expected single CLIF value, got Value-shaped (disc, \
                 payload) pair — GIR is malformed or consumer is wrong"
            )),
        }
    }

    fn value(self) -> Result<(ClifValue, ClifValue)> {
        match self {
            CompiledExpr::Value { disc, payload } => Ok((disc, payload)),
            CompiledExpr::Single(_) => Err(anyhow!(
                "JIT: expected Value-shaped (disc, payload), got single \
                 — GIR is malformed or consumer is wrong"
            )),
        }
    }
}

// ─── Env: name → Variable lookup ─────────────────────────────────

struct JitEnv {
    /// Scalar locals: `(name, var, prim)`. Lookups walk back-to-front
    /// for proper shadowing.
    locals: Vec<(ArcStr, Variable, PrimType)>,
    /// Composite parameter pointer Variables: `(name, var)`. The var
    /// holds a `*const ValArray` (CLIF i64 on 64-bit targets). Array,
    /// tuple, and struct params share this table — they differ only
    /// in how the kernel reads slots, not in the pointer layout.
    composites: Vec<(ArcStr, Variable)>,
    /// Variant locals — each entry holds a [`ValueVar`] (`disc`,
    /// `payload`) representing the two-word `repr(u64)` Value. Reads
    /// return both Variables via `b.use_var`; consumer ops
    /// (`VariantTagEq`, `VariantPayload`, `IsNull`) take the pair.
    /// Drop / clone happen via the two-register helpers
    /// `graphix_value_drop` / `graphix_value_clone`.
    variants: Vec<(ArcStr, ValueVar)>,
    /// Nullable locals — identical storage to `variants` (the Value
    /// is either `Value::Null` or `T`'s runtime form), but kept
    /// semantically distinct so `IsNull` against an `env.nullables`
    /// entry is well-typed even though `VariantTagEq` against the
    /// same shape would also work.
    nullables: Vec<(ArcStr, ValueVar)>,
    /// String locals: `(name, var)` where `var` holds an owned
    /// `ArcStr`'s thin pointer as a single `i64`. `GirOp::Local`
    /// for a `GirType::String`-typed Ref reads the slot and
    /// `graphix_arcstr_clone`s it (refcount bump) so each consumer
    /// gets an independently-owned ArcStr — the slot keeps its
    /// own ref until scope exit where it drops via
    /// `graphix_arcstr_drop`. Mirrors the variant/nullable
    /// ownership discipline.
    strings: Vec<(ArcStr, Variable)>,
}

impl JitEnv {
    fn new() -> Self {
        Self {
            locals: Vec::with_capacity(8),
            composites: Vec::new(),
            variants: Vec::new(),
            nullables: Vec::new(),
            strings: Vec::new(),
        }
    }

    fn bind(&mut self, name: ArcStr, var: Variable, prim: PrimType) {
        self.locals.push((name, var, prim));
    }

    fn bind_composite(&mut self, name: ArcStr, var: Variable) {
        self.composites.push((name, var));
    }

    fn bind_variant(&mut self, name: ArcStr, vv: ValueVar) {
        self.variants.push((name, vv));
    }

    fn bind_nullable(&mut self, name: ArcStr, vv: ValueVar) {
        self.nullables.push((name, vv));
    }

    fn bind_string(&mut self, name: ArcStr, var: Variable) {
        self.strings.push((name, var));
    }

    fn lookup(&self, name: &str) -> Option<(Variable, PrimType)> {
        for (n, v, p) in self.locals.iter().rev() {
            if n.as_str() == name {
                return Some((*v, *p));
            }
        }
        None
    }

    fn lookup_composite(&self, name: &str) -> Option<Variable> {
        for (n, v) in self.composites.iter().rev() {
            if n.as_str() == name {
                return Some(*v);
            }
        }
        None
    }

    fn lookup_variant(&self, name: &str) -> Option<ValueVar> {
        for (n, vv) in self.variants.iter().rev() {
            if n.as_str() == name {
                return Some(*vv);
            }
        }
        None
    }

    fn lookup_nullable(&self, name: &str) -> Option<ValueVar> {
        for (n, vv) in self.nullables.iter().rev() {
            if n.as_str() == name {
                return Some(*vv);
            }
        }
        None
    }

    fn lookup_string(&self, name: &str) -> Option<Variable> {
        for (n, v) in self.strings.iter().rev() {
            if n.as_str() == name {
                return Some(*v);
            }
        }
        None
    }

    /// Snapshot the lengths of all three binding lists. Pair with
    /// `truncate` to pop every binding introduced since the mark —
    /// composite/variant bindings included, so a block / select-arm /
    /// loop-body scope doesn't leak names into its enclosing scope.
    ///
    /// `truncate` does NOT emit any runtime drops. Dropping owned
    /// composite/variant locals is the responsibility of the
    /// scope-exit code (`GirOp::Block`) or the terminating statement
    /// (`GirStmt::Return` via `drop_owned_composites`); `truncate` is
    /// purely compile-time `env`-Vec hygiene.
    fn mark(&self) -> EnvMark {
        EnvMark {
            locals: self.locals.len(),
            composites: self.composites.len(),
            variants: self.variants.len(),
            nullables: self.nullables.len(),
            strings: self.strings.len(),
        }
    }

    fn truncate(&mut self, m: EnvMark) {
        self.locals.truncate(m.locals);
        self.composites.truncate(m.composites);
        self.variants.truncate(m.variants);
        self.nullables.truncate(m.nullables);
        self.strings.truncate(m.strings);
    }
}

/// A snapshot of a [`JitEnv`]'s binding-list lengths — see
/// [`JitEnv::mark`].
#[derive(Debug, Clone, Copy)]
struct EnvMark {
    locals: usize,
    composites: usize,
    variants: usize,
    nullables: usize,
    strings: usize,
}

// ─── Body / statement compilation ────────────────────────────────

fn compile_body(
    b: &mut FunctionBuilder,
    stmts: &[GirStmt],
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
    for stmt in stmts {
        match stmt {
            GirStmt::Let(l) => {
                match &l.value.typ {
                    GirType::Prim(p) => {
                        let v = compile_scalar(b, &l.value, env, ctx)?;
                        let var = b.declare_var(prim_to_clif(*p));
                        b.def_var(var, v);
                        env.bind(l.local.clone(), var, *p);
                    }
                    GirType::Array(_)
                    | GirType::Tuple(_)
                    | GirType::Struct(_) => {
                        // Composite local: an owned `*mut ValArray`
                        // stored as I64. `ensure_owned_composite`
                        // clones a Borrowed source (e.g. `let a = b`
                        // aliasing another composite) so this local
                        // exclusively owns its buffer — otherwise
                        // `drop_owned_composites` at function exit
                        // would double-free. Dropped at function exit
                        // (and on pending paths by the per-DynCall
                        // `pre_pending` block).
                        let v = compile_scalar(b, &l.value, env, ctx)?;
                        let owned =
                            ensure_owned_composite(b, ctx, &l.value, v)?;
                        let var = b.declare_var(types::I64);
                        b.def_var(var, owned);
                        env.bind_composite(l.local.clone(), var);
                    }
                    GirType::Variant(_)
                    | GirType::Nullable(_)
                    | GirType::DateTime
                    | GirType::Duration
                    | GirType::Bytes | GirType::Map | GirType::Error => {
                        // Value-shape local: stored as a `(disc,
                        // payload)` pair (`ValueVar`). The source
                        // expression compiles to a `CompiledExpr::Value`;
                        // `ensure_owned_value` clones a Borrowed
                        // source (e.g. `let a = b` aliasing another
                        // Value local) via `graphix_value_clone` so
                        // this local exclusively owns its ref. Dropped
                        // at function exit (and on pending paths by
                        // the per-DynCall `pre_pending` block).
                        // datetime/duration share the `nullables`
                        // ValueVar slot (same `(disc, payload)` wire
                        // shape).
                        let cv = compile_expr(b, &l.value, env, ctx)?;
                        let (owned_disc, owned_payload) =
                            ensure_owned_value(b, ctx, &l.value, cv)?;
                        let disc_var = b.declare_var(types::I64);
                        let payload_var = b.declare_var(types::I64);
                        b.def_var(disc_var, owned_disc);
                        b.def_var(payload_var, owned_payload);
                        let vv =
                            ValueVar { disc: disc_var, payload: payload_var };
                        if matches!(l.value.typ, GirType::Variant(_)) {
                            env.bind_variant(l.local.clone(), vv);
                        } else {
                            env.bind_nullable(l.local.clone(), vv);
                        }
                    }
                    // Unreachable in well-formed GIR — `emit_bind_stmt`
                    // routes Unit-typed lets through `GirStmt::Discard`.
                    GirType::Unit => {
                        return Err(anyhow!(
                            "GIR malformed: GirStmt::Let with Unit value"
                        ));
                    }
                    // String-typed let: owned ArcStr ptr stored in
                    // an i64 Variable. Producers (ConstStr / Concat)
                    // return owned values; Local reads of String
                    // refcount-bump (so the source is also owned).
                    // No `ensure_owned_string` needed — every site
                    // that produces a String SSA value owns it.
                    // Dropped at scope exit by `drop_owned_strings`.
                    GirType::String => {
                        let v = compile_scalar(b, &l.value, env, ctx)?;
                        let var = b.declare_var(types::I64);
                        b.def_var(var, v);
                        env.bind_string(l.local.clone(), var);
                    }
                    // Bare `GirType::Null` is the singleton produced
                    // by `GirOp::ConstNull` — fusion always widens it
                    // to `Nullable<T>` before binding (`emit_select_as_expr`
                    // /  `unify_arm_types`), so a bare-`Null` let is a
                    // malformed kernel.
                    GirType::Null => {
                        return Err(anyhow!(
                            "GirStmt::Let with bare Null value — should \
                             have widened to Nullable<T> at construction"
                        ));
                    }
                }
            }
            GirStmt::Discard(e) => {
                // Evaluate for side effects, throw the SSA result
                // away. The expression's side-effecting machinery
                // (DynCalls, producer-op writes) already happened
                // during `compile_expr`; the unused SSA value just
                // gets DCE'd by cranelift.
                let _v = compile_scalar(b, e, env, ctx)?;
            }
            GirStmt::Return(e) => {
                // Borrowed return values (a `Local` still owned by
                // `env.composites` / `env.variants` / `env.nullables`)
                // must be cloned — otherwise the
                // `drop_owned_composites` below frees the buffer the
                // caller is about to receive. Owned sources (producer
                // ops, DynCall / Block / IfChain results) pass
                // through unchanged.
                //
                // Value-shape returns (Variant/Nullable) hand back
                // TWO CLIF values per the typed kernel signature;
                // scalar/composite returns hand back one.
                //
                // Pending check: a scalar `GirOp::DynCall` that
                // pends sets `DYNCALL_PENDING` but doesn't emit a
                // `pre_pending` branch (the sentinel-zero is harmless
                // for downstream scalar arithmetic). If we ran
                // through such a DynCall and now produce a composite
                // / Value return, the result is a real owned
                // allocation that `GirNode::update` will discard via
                // its `DYNCALL_PENDING` check — leaking the
                // allocation. Branch here: if pending fired anywhere
                // in the body, drop the just-computed result, route
                // through `pending_exit` (which emits a sentinel),
                // and `GirNode::update` will read `DYNCALL_PENDING`
                // (still set — `pending_take` no longer clears) and
                // return `None`.
                //
                // Scalar / Unit returns: no allocation, no leak —
                // skip the pending check.
                match &e.typ {
                    GirType::Variant(_)
                    | GirType::Nullable(_)
                    | GirType::DateTime
                    | GirType::Duration
                    | GirType::Bytes | GirType::Map | GirType::Error => {
                        let cv = compile_expr(b, e, env, ctx)?;
                        let (disc, payload) =
                            ensure_owned_value(b, ctx, e, cv)?;
                        emit_return_pending_check(
                            b, env, ctx,
                            ReturnDropShape::Value { disc, payload },
                        )?;
                        drop_owned_composites(b, env, ctx)?;
                        b.ins().return_(&[disc, payload]);
                    }
                    GirType::Array(_)
                    | GirType::Tuple(_)
                    | GirType::Struct(_) => {
                        let v = compile_scalar(b, e, env, ctx)?;
                        let v = ensure_owned_composite(b, ctx, e, v)?;
                        emit_return_pending_check(
                            b, env, ctx,
                            ReturnDropShape::Composite(v),
                        )?;
                        drop_owned_composites(b, env, ctx)?;
                        b.ins().return_(&[v]);
                    }
                    GirType::String => {
                        // String returns produce an owned `ArcStr`
                        // (refcount bumped). On the pending path
                        // `GirNode::update` discards the wrapper
                        // result without transmuting/dropping →
                        // refcount leak. Mirror the composite/Value
                        // pending-check flow: peek the flag, drop
                        // the just-built ArcStr if pending fired
                        // earlier, run pending cleanup, jump to
                        // pending_exit. Non-pending path returns the
                        // ArcStr's raw pointer as a single I64.
                        let v = compile_scalar(b, e, env, ctx)?;
                        emit_return_pending_check(
                            b, env, ctx,
                            ReturnDropShape::String(v),
                        )?;
                        drop_owned_composites(b, env, ctx)?;
                        b.ins().return_(&[v]);
                    }
                    _ => {
                        // Scalar / Unit: no owned allocation to leak;
                        // `GirNode::update`'s wrapper-level pending
                        // check still fires correctly via the still-
                        // set flag.
                        let v = compile_scalar(b, e, env, ctx)?;
                        let v = ensure_owned_composite(b, ctx, e, v)?;
                        drop_owned_composites(b, env, ctx)?;
                        b.ins().return_(&[v]);
                    }
                }
                return Ok(());
            }
            GirStmt::TailCall { args } => {
                let head = ctx.loop_head.ok_or_else(|| {
                    anyhow!("GIR malformed: TailCall in kernel without has_tail_loop")
                })?;
                // Evaluate every new arg into a CLIF SSA value first
                // (so an arg that reads an old param sees the old
                // value, not one we already overwrote).
                let mut new_vals = Vec::with_capacity(args.len());
                for a in args {
                    new_vals.push(compile_scalar(b, a, env, ctx)?);
                }
                // Back-compat: hand-built test kernels leave
                // `tail_call_slots` empty and assume all params are
                // scalar in declaration order. Drive the rebind
                // positionally in that case.
                if ctx.tail_call_slots.is_none() {
                    debug_assert_eq!(
                        new_vals.len(),
                        ctx.param_mark.locals
                    );
                    for (i, v) in new_vals.iter().enumerate() {
                        let (var, _) = (env.locals[i].1, env.locals[i].2);
                        b.def_var(var, *v);
                    }
                    env.truncate(ctx.param_mark);
                    b.ins().jump(head, &[]);
                    return Ok(());
                }
                let slots = ctx.tail_call_slots.unwrap();
                debug_assert_eq!(args.len(), slots.len());
                use crate::gir::TailCallSlotKind;
                let drop_helper = ctx
                    .helper_refs
                    .get("graphix_valarray_drop")
                    .ok_or_else(|| anyhow!("missing graphix_valarray_drop"))?;
                let clone_helper = ctx
                    .helper_refs
                    .get("graphix_valarray_clone")
                    .ok_or_else(|| anyhow!("missing graphix_valarray_clone"))?;
                // Detect each composite-rebind source so we know
                // whether the SSA value already holds a fresh owned
                // pointer (TupleNew / StructNew / etc.) or whether
                // it's a Local read sharing the same pointer as the
                // slot we're about to drop. The latter needs a
                // refcount bump before rebind so the drop balances.
                let composite_sources: Vec<CompositeSource> = args
                    .iter()
                    .map(|a| classify_composite_source(a))
                    .collect();
                let mut new_vals: Vec<ClifValue> = new_vals;
                for (i, slot) in slots.iter().enumerate() {
                    if matches!(slot.kind, TailCallSlotKind::ValArray)
                        && composite_sources[i] == CompositeSource::Borrowed
                    {
                        // Clone (refcount bump) so the next iteration
                        // holds an owned reference, separate from any
                        // other live alias.
                        let call =
                            b.ins().call(clone_helper, &[new_vals[i]]);
                        new_vals[i] = b.inst_results(call)[0];
                    }
                }
                for (slot, v) in slots.iter().zip(new_vals.iter()) {
                    match slot.kind {
                        TailCallSlotKind::Scalar(_) => {
                            let (var, _) =
                                env.lookup(&slot.name).ok_or_else(|| {
                                    anyhow!(
                                        "TailCall: scalar slot `{}` not in env",
                                        slot.name
                                    )
                                })?;
                            b.def_var(var, *v);
                        }
                        TailCallSlotKind::ValArray => {
                            // Composite rebind: drop the previously-
                            // owned pointer in the slot, then store
                            // the new owned `*mut ValArray`. This
                            // closes the leak we had in Phase 2.
                            let var = env
                                .lookup_composite(&slot.name)
                                .ok_or_else(|| {
                                    anyhow!(
                                        "TailCall: composite slot `{}` not in env",
                                        slot.name
                                    )
                                })?;
                            let old = b.use_var(var);
                            b.ins().call(drop_helper, &[old]);
                            b.def_var(var, *v);
                        }
                        TailCallSlotKind::Variant => {
                            return Err(anyhow!(
                                "JIT: variant tail-call rebind not yet supported"
                            ));
                        }
                        TailCallSlotKind::Nullable => {
                            // Nullable kernels route to the
                            // interpreter via `kernel_contains_null`,
                            // so this branch should be unreachable in
                            // practice. Returning an Err keeps the
                            // codegen safe if the routing ever drifts.
                            return Err(anyhow!(
                                "JIT: nullable tail-call rebind — kernel \
                                 should have been routed to interp via \
                                 kernel_contains_null"
                            ));
                        }
                        TailCallSlotKind::String | TailCallSlotKind::Value => {
                            // A recursive lambda whose tail-call rebinds
                            // a String / value-shape param. The JIT
                            // doesn't lower the owned-ArcStr / two-word
                            // Value rebind yet; bail so the kernel falls
                            // back to the interpreter (which handles it).
                            return Err(anyhow!(
                                "JIT: string/value tail-call rebind not \
                                 yet supported — falling back to interp"
                            ));
                        }
                    }
                }
                // Drop any owned composite/variant/nullable/string
                // locals introduced between `ctx.param_mark` and now
                // that ISN'T in `tail_call_slots` (slot rebinds drop
                // the old slot value above; non-slot lets above the
                // tail-call would leak per iteration otherwise).
                // Block / select-arm locals were already dropped at
                // runtime by their scope-exit code (`GirOp::Block`,
                // terminating statements), so iterating the env's
                // tail catches only the non-Block top-level lets —
                // i.e. `GirStmt::Let` between `param_mark` and this
                // `TailCall` that didn't get a rebind slot.
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
                // Composite slot rebinds already drop their old
                // value (above); skip any entry whose name matches a
                // `TailCallSlotKind::ValArray` slot. Same for
                // future variant/nullable rebind slots once they're
                // supported.
                let slot_names: std::collections::HashSet<&str> = slots
                    .iter()
                    .map(|s| s.name.as_str())
                    .collect();
                for (name, var) in &env.composites[ctx.param_mark.composites..] {
                    if slot_names.contains(name.as_str()) {
                        continue;
                    }
                    let ptr = b.use_var(*var);
                    b.ins().call(arr_drop, &[ptr]);
                }
                for (name, vv) in &env.variants[ctx.param_mark.variants..] {
                    if slot_names.contains(name.as_str()) {
                        continue;
                    }
                    let disc = b.use_var(vv.disc);
                    let payload = b.use_var(vv.payload);
                    b.ins().call(val_drop, &[disc, payload]);
                }
                for (name, vv) in &env.nullables[ctx.param_mark.nullables..] {
                    if slot_names.contains(name.as_str()) {
                        continue;
                    }
                    let disc = b.use_var(vv.disc);
                    let payload = b.use_var(vv.payload);
                    b.ins().call(val_drop, &[disc, payload]);
                }
                for (name, var) in &env.strings[ctx.param_mark.strings..] {
                    if slot_names.contains(name.as_str()) {
                        continue;
                    }
                    let ptr = b.use_var(*var);
                    b.ins().call(str_drop, &[ptr]);
                }
                // Compile-time env-Vec hygiene: pop everything above
                // the param mark so the next iteration starts with a
                // clean lexical state.
                env.truncate(ctx.param_mark);
                b.ins().jump(head, &[]);
                return Ok(());
            }
            GirStmt::Select { arms } => {
                compile_select_stmt(b, arms, env, ctx)?;
                // Each arm body terminates (return or tail-jump) so
                // there is no fallthrough after the select.
                return Ok(());
            }
        }
    }
    Err(anyhow!(
        "GIR malformed: body fell through with no return or tail call"
    ))
}

fn compile_select_stmt(
    b: &mut FunctionBuilder,
    arms: &[SelectArm],
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
    if arms.is_empty() {
        return Err(anyhow!("GIR malformed: empty select"));
    }
    // For each arm:
    //   - Conditional: brif the cond into the body block or a "next
    //     test" block. After compiling the body (which terminates),
    //     switch back to the next-test block to emit the following
    //     test.
    //   - Unconditional: jump straight to the body. After it
    //     terminates, no further arms are reachable so we return.
    //
    // The last-arm-conditional case fallthroughs to a trap block —
    // typecheck should forbid this, but in CLIF every block must end
    // in a terminator and we can't leave a dangling false-branch.
    for (i, arm) in arms.iter().enumerate() {
        let is_last = i == arms.len() - 1;
        let body_block = b.create_block();
        // `next_block` is `Some` when there's somewhere to switch to
        // after this arm finishes (i.e. for the next iteration's
        // test). `None` means this arm consumed control flow
        // unconditionally and there's nothing left to compile.
        let next_block: Option<Block> = match &arm.cond {
            None => {
                b.ins().jump(body_block, &[]);
                None
            }
            Some(cond) => {
                let cv = compile_scalar(b, cond, env, ctx)?;
                if is_last {
                    let trap_block = b.create_block();
                    b.ins().brif(cv, body_block, &[], trap_block, &[]);
                    b.switch_to_block(trap_block);
                    b.ins().trap(
                        cranelift_codegen::ir::TrapCode::user(1).unwrap(),
                    );
                    b.seal_block(trap_block);
                    None
                } else {
                    let next = b.create_block();
                    b.ins().brif(cv, body_block, &[], next, &[]);
                    Some(next)
                }
            }
        };
        let mark = env.mark();
        b.switch_to_block(body_block);
        b.seal_block(body_block);
        compile_body(b, &arm.body, env, ctx)?;
        env.truncate(mark);
        match next_block {
            Some(next) => {
                b.switch_to_block(next);
                b.seal_block(next);
            }
            None => return Ok(()),
        }
    }
    Ok(())
}

// ─── Expression compilation ──────────────────────────────────────

/// Compile a [`GirExpr`] to its CLIF representation, dispatched on
/// shape:
///   * Scalar / Pointer-shaped exprs → `CompiledExpr::Single(v)`.
///   * Variant / Nullable exprs      → `CompiledExpr::Value { disc, payload }`.
///
/// Callers that only handle one shape extract with `.single()? /
/// .value()?` (or call `compile_scalar` directly, which wraps
/// `compile_expr(...)?.single()?`). The shape an arm produces
/// follows the `GirExpr.typ` — typecheck guarantees consistency.
fn compile_expr(
    b: &mut FunctionBuilder,
    e: &GirExpr,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<CompiledExpr> {
    // Value-shape arms first — early return. These are the ops whose
    // result is a `repr(u64)` Value (Variant or Nullable):
    //   * `ConstNull`: synthesise (NULL_DISC, 0).
    //   * `Local` of Variant/Nullable type: read the (disc, payload)
    //     pair from `env.variants`/`env.nullables`.
    //   * `IfChain` whose result type is Variant/Nullable:
    //     `compile_ifchain` returns two CLIF values via merge phi-pair.
    //   * `Block` whose tail produces a Value shape: route to
    //     `compile_block_value`.
    //   * `DynCall` returning Variant/Nullable: dispatcher returns two
    //     words (disc, payload).
    //   * `VariantNew`: build (Array_disc, *mut ValArray as i64) or
    //     (String_disc, *const ArcStr as i64).
    //
    // Sites that need the full enum reach this branch via
    // `compile_expr` directly; sites that only handle scalars use
    // `compile_scalar` which asserts Single and bails on Value-shape.
    if e.typ.is_value_shape() || matches!(e.op, GirOp::ConstNull) {
        return compile_value_expr(b, e, env, ctx);
    }
    let v = compile_scalar_impl(b, e, env, ctx)?;
    Ok(CompiledExpr::Single(v))
}

/// Wrapper for sites that only handle scalar/pointer-shaped exprs.
/// Calls `compile_expr` and asserts `Single`; a Value-shape result
/// is a kernel-build bug (consumer in scalar position got a Value)
/// and returns an error.
fn compile_scalar(
    b: &mut FunctionBuilder,
    e: &GirExpr,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<ClifValue> {
    compile_expr(b, e, env, ctx)?.single()
}

/// Marshal a `GirOp::DynCall`'s args into a fresh `LPooled<Vec<Value>>`
/// buf and push the buf onto `ctx.dyncall_buf_stack` for the in-flight
/// window (so a nested composite-return DynCall that pends can drop
/// this outer buf from its `pre_pending` block). Returns the buf
/// pointer; the caller issues the `graphix_dyncall` and pops the stack.
///
/// Shared by BOTH DynCall codegen arms — the scalar/composite-return
/// arm in [`compile_scalar_impl`] and the Value-shape-return arm in
/// [`compile_value_expr`]. The per-arg-shape dispatch lives here once:
/// a Value-shape arg (variant / nullable / datetime / duration / bytes)
/// compiles via [`compile_value_expr`] and pushes its two `(disc,
/// payload)` words; every other shape compiles via [`compile_scalar`]
/// and pushes one word. Routing a Value-shape arg through
/// `compile_scalar` is the bug this consolidation fixes — `.single()`
/// bails on a `(disc, payload)` pair, and the 2-word push helpers
/// expect two args, not one.
fn marshal_dyncall_args(
    b: &mut FunctionBuilder,
    args: &[GirExpr],
    arg_types: &[GirType],
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<ClifValue> {
    let buf_new = ctx
        .helper_refs
        .get("graphix_value_buf_new")
        .ok_or_else(|| anyhow!("missing graphix_value_buf_new"))?;
    let cap = b.ins().iconst(types::I64, args.len() as i64);
    let call = b.ins().call(buf_new, &[cap]);
    let buf = b.inst_results(call)[0];
    let buf_var = b.declare_var(types::I64);
    b.def_var(buf_var, buf);
    ctx.dyncall_buf_stack.borrow_mut().push(buf_var);
    for (a, t) in args.iter().zip(arg_types.iter()) {
        // For composite/value args the push helper depends on where the
        // SSA value came from. A `Borrowed` source (a Local read — the
        // caller still owns it) refcount-bumps; an `Owned` source (a
        // producer op, or a composite/Value-return DynCall result not
        // bound to a local) transfers ownership into the buf. Using the
        // borrowed helper on an Owned source leaks the original; the
        // move helper on a Borrowed source double-frees it.
        let helper_name: &str = match t {
            GirType::Prim(p) => value_buf_push_helper(*p)?,
            GirType::Array(_) | GirType::Tuple(_) | GirType::Struct(_) => {
                match classify_composite_source(a) {
                    CompositeSource::Owned => "graphix_value_buf_push_array",
                    CompositeSource::Borrowed => {
                        "graphix_value_buf_push_array_borrowed"
                    }
                }
            }
            // Variant / Nullable / datetime / duration / bytes all ride
            // the two-word `(disc, payload)` Value wire shape.
            GirType::Variant(_)
            | GirType::Nullable(_)
            | GirType::DateTime
            | GirType::Duration
            | GirType::Bytes | GirType::Map | GirType::Error => match classify_composite_source(a) {
                CompositeSource::Owned => "graphix_value_buf_push_value",
                CompositeSource::Borrowed => {
                    "graphix_value_buf_push_value_borrowed"
                }
            },
            GirType::String => "graphix_value_buf_push_string",
            GirType::Unit => {
                return Err(anyhow!("GIR malformed: DynCall arg has Unit type"));
            }
            GirType::Null => {
                return Err(anyhow!(
                    "DynCall arg with bare Null type — should have widened \
                     to Nullable<T> at construction"
                ));
            }
        };
        let push = ctx
            .helper_refs
            .get(helper_name)
            .ok_or_else(|| anyhow!("missing push helper `{helper_name}`"))?;
        if t.is_value_shape() {
            let (disc, payload) = compile_value_expr(b, a, env, ctx)?.value()?;
            b.ins().call(push, &[buf, disc, payload]);
        } else {
            let v = compile_scalar(b, a, env, ctx)?;
            b.ins().call(push, &[buf, v]);
        }
    }
    Ok(buf)
}

/// Compile a Value-shape (Variant or Nullable) `GirExpr` to the
/// two-register `(disc, payload)` representation. Dispatched from
/// [`compile_expr`] when the expression's `GirType` is Variant or
/// Nullable, or for `GirOp::ConstNull` whose result is the singleton
/// `Value::Null`.
fn compile_value_expr(
    b: &mut FunctionBuilder,
    e: &GirExpr,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<CompiledExpr> {
    use cranelift_codegen::ir::types;
    match &e.op {
        GirOp::ConstNull => {
            // `Value::Null = 0x8000`, payload word unused (zeroed).
            let disc = b.ins().iconst(types::I64, value_disc::NULL);
            let payload = b.ins().iconst(types::I64, 0);
            Ok(CompiledExpr::Value { disc, payload })
        }
        // datetime/duration literal: bake a stable `*const Value` from
        // the kernel's value-constants table and clone it (bumps the
        // inner Arc) → an owned `(disc, payload)` Value.
        GirOp::ConstValue(v) => {
            let ptr = ctx.values.get(v) as i64;
            let ptr_val = b.ins().iconst(types::I64, ptr);
            let clone = ctx
                .helper_refs
                .get("graphix_value_clone_from_static")
                .ok_or_else(|| {
                    anyhow!("missing graphix_value_clone_from_static")
                })?;
            let call = b.ins().call(clone, &[ptr_val]);
            let r = b.inst_results(call);
            Ok(CompiledExpr::Value { disc: r[0], payload: r[1] })
        }
        // datetime/duration arithmetic: both operands as OWNED Values
        // (the helper consumes them, matching netidx's by-value
        // operators), then call the matching `graphix_value_<op>`.
        GirOp::ValueArith { op, lhs, rhs } => {
            let (ld, lp) = compile_owned_value_operand(b, lhs, env, ctx)?;
            let (rd, rp) = compile_owned_value_operand(b, rhs, env, ctx)?;
            let helper = match op {
                crate::gir::BinOp::Add => "graphix_value_add",
                crate::gir::BinOp::Sub => "graphix_value_sub",
                crate::gir::BinOp::Mul => "graphix_value_mul",
                crate::gir::BinOp::Div => "graphix_value_div",
                crate::gir::BinOp::Mod => "graphix_value_rem",
            };
            let fref = ctx
                .helper_refs
                .get(helper)
                .ok_or_else(|| anyhow!("missing {helper}"))?;
            let call = b.ins().call(fref, &[ld, lp, rd, rp]);
            let r = b.inst_results(call);
            Ok(CompiledExpr::Value { disc: r[0], payload: r[1] })
        }
        GirOp::Local(name) => {
            // Variant and Nullable (+ datetime/duration, which ride
            // the nullable slot) share storage shape; the type tells
            // us which table to read from. Reads are borrowed — the
            // env still owns the ref; consumers either drop their own
            // clone (via `ensure_owned_value`) or call borrow-mode
            // helpers that `mem::forget` the input.
            let vv = if matches!(e.typ, GirType::Variant(_)) {
                env.lookup_variant(name).ok_or_else(|| {
                    anyhow!("GIR malformed: undefined variant local `{name}`")
                })?
            } else {
                env.lookup_nullable(name).ok_or_else(|| {
                    anyhow!("GIR malformed: undefined value-shape local `{name}`")
                })?
            };
            Ok(CompiledExpr::Value {
                disc: b.use_var(vv.disc),
                payload: b.use_var(vv.payload),
            })
        }
        GirOp::IfChain { arms } => compile_ifchain(b, arms, &e.typ, env, ctx),
        GirOp::VariantNew { tag, payloads, payload_types } => {
            // Nullary variant → Value::String(tag): route through
            //   `graphix_value_new_string_from_arcstr`, which clones
            //   the interned `ArcStr` (refcount bump). We can't pack
            //   `(STRING_DISC, interned_arcstr_ptr)` inline without
            //   the clone — `KernelStrings` holds the ArcStr's only
            //   ref, so the eventual `graphix_value_drop` would
            //   decrement it and free the interned static, corrupting
            //   later uses.
            // With-payload variant → Value::Array([tag, p0, ...]):
            //   build the inner ValArray via the existing buf helpers,
            //   finalize to `*mut ValArray`, then unwrap into a
            //   two-register Value via `graphix_value_new_from_array`
            //   (the helper moves out of the box and returns the
            //   Value's (disc, payload) words directly).
            let tag_ptr = ctx.strings.get(tag) as i64;
            let tag_ptr_val = b.ins().iconst(types::I64, tag_ptr);
            if payloads.is_empty() {
                let new_str = ctx
                    .helper_refs
                    .get("graphix_value_new_string_from_arcstr")
                    .ok_or_else(|| {
                        anyhow!(
                            "missing graphix_value_new_string_from_arcstr"
                        )
                    })?;
                let call = b.ins().call(new_str, &[tag_ptr_val]);
                let results = b.inst_results(call);
                Ok(CompiledExpr::Value {
                    disc: results[0],
                    payload: results[1],
                })
            } else {
                let buf_new = ctx
                    .helper_refs
                    .get("graphix_value_buf_new")
                    .ok_or_else(|| {
                        anyhow!("missing graphix_value_buf_new")
                    })?;
                let push_arcstr = ctx
                    .helper_refs
                    .get("graphix_value_buf_push_arcstr")
                    .ok_or_else(|| {
                        anyhow!("missing graphix_value_buf_push_arcstr")
                    })?;
                let finalize = ctx
                    .helper_refs
                    .get("graphix_valarray_finalize")
                    .ok_or_else(|| {
                        anyhow!("missing graphix_valarray_finalize")
                    })?;
                let wrap_array = ctx
                    .helper_refs
                    .get("graphix_value_new_from_array")
                    .ok_or_else(|| {
                        anyhow!("missing graphix_value_new_from_array")
                    })?;
                let cap =
                    b.ins().iconst(types::I64, (payloads.len() + 1) as i64);
                let call = b.ins().call(buf_new, &[cap]);
                let buf = b.inst_results(call)[0];
                b.ins().call(push_arcstr, &[buf, tag_ptr_val]);
                // Composite-element lowering: payload types may be
                // composite/variant/string/nullable. Dispatch via
                // the shared helper.
                for (p, _t) in payloads.iter().zip(payload_types.iter()) {
                    compile_and_push_field(b, env, ctx, buf, p)?;
                }
                let call = b.ins().call(finalize, &[buf]);
                let arr = b.inst_results(call)[0];
                let call = b.ins().call(wrap_array, &[arr]);
                let results = b.inst_results(call);
                Ok(CompiledExpr::Value {
                    disc: results[0],
                    payload: results[1],
                })
            }
        }
        GirOp::DynCall { fn_index, args, arg_types, return_type } => {
            // Value-shape-return DynCall. Marshalling (incl. the
            // per-shape arg dispatch — Value-shape args push their two
            // `(disc, payload)` words, scalars/composites push one) is
            // shared with the scalar-return arm via
            // `marshal_dyncall_args`; here we only differ in the return
            // decode (ret_kind=2, two register-words).
            let dyncall = ctx
                .helper_refs
                .get("graphix_dyncall")
                .ok_or_else(|| anyhow!("missing graphix_dyncall"))?;
            let buf = marshal_dyncall_args(b, args, arg_types, env, ctx)?;
            // ret_kind=2 (Value-shape) — dispatcher boxes the
            // dispatcher's returned Value into a `Box<Value>` and
            // splits it into two register-words.
            let ret_kind: i64 = 2;
            // ret_kind=2 boxes any `Value` into two words, so every
            // value-shape return (Variant/Nullable/DateTime/Duration/
            // Bytes/Map) routes here, not just Variant/Nullable.
            debug_assert!(return_type.is_value_shape());
            let fn_idx_val =
                b.ins().iconst(types::I32, *fn_index as i64);
            let ret_kind_val = b.ins().iconst(types::I8, ret_kind);
            let call = b
                .ins()
                .call(dyncall, &[fn_idx_val, buf, ret_kind_val]);
            let results = b.inst_results(call);
            let (raw_disc, raw_payload) = (results[0], results[1]);
            ctx.dyncall_buf_stack.borrow_mut().pop();

            // Pending check + cleanup branch.
            let pending_take = ctx
                .helper_refs
                .get("graphix_dyncall_pending_take")
                .ok_or_else(|| {
                    anyhow!("missing graphix_dyncall_pending_take")
                })?;
            let call = b.ins().call(pending_take, &[]);
            let pending = b.inst_results(call)[0];

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
            b.ins().brif(pending, pre_pending, &[], continue_block, &[]);

            b.switch_to_block(pre_pending);
            b.seal_block(pre_pending);
            emit_pending_cleanup(b, env, ctx)?;
            b.ins().jump(pending_exit, &[]);

            b.switch_to_block(continue_block);
            b.seal_block(continue_block);
            Ok(CompiledExpr::Value {
                disc: raw_disc,
                payload: raw_payload,
            })
        }
        GirOp::Block { lets, tail } => {
            // Block whose tail is Value-shape. Mirrors the
            // scalar/composite Block arm in `compile_scalar_impl` —
            // same let-binding routing, same scope-exit drops — but
            // the tail compiles to a `(disc, payload)` pair which we
            // own through `ensure_owned_value` and hand back as
            // `CompiledExpr::Value` AFTER block-scoped locals have
            // dropped (so a tail that aliases a block-scoped Value
            // local outlives the block — the clone happens before
            // the original's drop).
            let mark = env.mark();
            for l in lets {
                match &l.value.typ {
                    GirType::Prim(p) => {
                        let v = compile_scalar(b, &l.value, env, ctx)?;
                        let var = b.declare_var(prim_to_clif(*p));
                        b.def_var(var, v);
                        env.bind(l.local.clone(), var, *p);
                    }
                    GirType::Array(_)
                    | GirType::Tuple(_)
                    | GirType::Struct(_) => {
                        let v = compile_scalar(b, &l.value, env, ctx)?;
                        let owned =
                            ensure_owned_composite(b, ctx, &l.value, v)?;
                        let var = b.declare_var(types::I64);
                        b.def_var(var, owned);
                        env.bind_composite(l.local.clone(), var);
                    }
                    GirType::Variant(_)
                    | GirType::Nullable(_)
                    | GirType::DateTime
                    | GirType::Duration
                    | GirType::Bytes | GirType::Map | GirType::Error => {
                        let cv = compile_expr(b, &l.value, env, ctx)?;
                        let (owned_disc, owned_payload) =
                            ensure_owned_value(b, ctx, &l.value, cv)?;
                        let disc_var = b.declare_var(types::I64);
                        let payload_var = b.declare_var(types::I64);
                        b.def_var(disc_var, owned_disc);
                        b.def_var(payload_var, owned_payload);
                        let vv =
                            ValueVar { disc: disc_var, payload: payload_var };
                        if matches!(l.value.typ, GirType::Variant(_)) {
                            env.bind_variant(l.local.clone(), vv);
                        } else {
                            // datetime/duration share the nullables slot.
                            env.bind_nullable(l.local.clone(), vv);
                        }
                    }
                    GirType::Unit => {
                        return Err(anyhow!(
                            "GIR malformed: Block let with Unit value"
                        ));
                    }
                    // String locals: the value SSA is an owned ArcStr
                    // (ConstStr / Concat / a clone from a Local-read or
                    // composite-element `TupleGet`). Bind into the
                    // `strings` slot; the block scope-exit drops it.
                    // Mirrors `GirStmt::Let`'s String arm — the
                    // String-locals work missed this Block arm, which
                    // kept any kernel with a string `let` (e.g. a
                    // `|(k, v)|` destructure over `Array<(string, _)>`)
                    // off the JIT.
                    GirType::String => {
                        let v = compile_scalar(b, &l.value, env, ctx)?;
                        let var = b.declare_var(types::I64);
                        b.def_var(var, v);
                        env.bind_string(l.local.clone(), var);
                    }
                    GirType::Null => {
                        return Err(anyhow!(
                            "Block let with bare Null value — should \
                             have widened to Nullable<T>"
                        ));
                    }
                }
            }
            // Tail. The cloning via `ensure_owned_value` happens
            // BEFORE the per-list drops below, so a tail that
            // aliases a block-scoped Value local (e.g.
            // `{let v = ...; v}`) gets a fresh refcount before its
            // original is dropped.
            let cv = compile_expr(b, tail, env, ctx)?;
            let (disc, payload) = ensure_owned_value(b, ctx, tail, cv)?;
            // Drop block-scoped locals — composite + variant +
            // nullable entries introduced since `mark`. The pending
            // path (a DynCall inside the block) is mutually
            // exclusive: `pre_pending` runs `emit_pending_cleanup`
            // which drops every `env.*` entry anyway.
            let arr_drop =
                ctx.helper_refs.get("graphix_valarray_drop").ok_or_else(
                    || anyhow!("missing graphix_valarray_drop"),
                )?;
            let val_drop =
                ctx.helper_refs.get("graphix_value_drop").ok_or_else(
                    || anyhow!("missing graphix_value_drop"),
                )?;
            for (_, var) in &env.composites[mark.composites..] {
                let ptr = b.use_var(*var);
                b.ins().call(arr_drop, &[ptr]);
            }
            for (_, vv) in &env.variants[mark.variants..] {
                let d = b.use_var(vv.disc);
                let p = b.use_var(vv.payload);
                b.ins().call(val_drop, &[d, p]);
            }
            for (_, vv) in &env.nullables[mark.nullables..] {
                let d = b.use_var(vv.disc);
                let p = b.use_var(vv.payload);
                b.ins().call(val_drop, &[d, p]);
            }
            // String locals bound in this block (e.g. a `|(k, v)|`
            // destructure leaf over `Array<(string, _)>`) hold an owned
            // ArcStr — drop them too. The scalar Block arm already does
            // this; the value-shape arm omitted it, leaking one ArcStr
            // per block entry (per element in a HOF loop body).
            let str_drop =
                ctx.helper_refs.get("graphix_arcstr_drop").ok_or_else(
                    || anyhow!("missing graphix_arcstr_drop"),
                )?;
            for (_, var) in &env.strings[mark.strings..] {
                let s = b.use_var(*var);
                b.ins().call(str_drop, &[s]);
            }
            env.truncate(mark);
            Ok(CompiledExpr::Value { disc, payload })
        }
        GirOp::Call { fn_name, args } => {
            // Cross-kernel call returning a value-shape (Variant /
            // Nullable). Same arg assembly + owned-arg drops as the
            // scalar/composite Call arm in `compile_scalar_impl`; the
            // return is the two-word (disc, payload) pair the callee
            // produced (owned — `classify_composite_source` classifies
            // a `GirOp::Call` result as Owned, so a downstream consumer
            // won't clone-then-leak it).
            let func_ref = ctx.callee_refs.get(fn_name).ok_or_else(|| {
                anyhow!(
                    "GIR malformed: GirOp::Call to `{fn_name}` but \
                     callee_refs has no entry for it (forgot to use \
                     compile_kernel_with_callees?)"
                )
            })?;
            let (clif_args, drops) =
                compile_call_clif_args(b, fn_name, args, env, ctx)?;
            let inst = b.ins().call(*func_ref, &clif_args);
            let (disc, payload) = {
                let results = b.inst_results(inst);
                if results.len() != 2 {
                    return Err(anyhow!(
                        "GIR malformed: value-shape callee `{fn_name}` \
                         returned {} values; expected 2 (disc, payload)",
                        results.len()
                    ));
                }
                (results[0], results[1])
            };
            emit_call_arg_drops(b, ctx, &drops)?;
            Ok(CompiledExpr::Value { disc, payload })
        }
        // `array[i]` — result type is always `Nullable<elem>` (=
        // `[elem, Error<…>]`). `graphix_valarray_index` does the bounds
        // check + negative-from-end handling and returns the element
        // (or the `ArrayIndexError` Value) as a two-word `Value`,
        // routing through the same shared `array_index` the node-walk
        // and interp use — so all three backends agree bit-for-bit.
        GirOp::ArrayGet { name, idx } => {
            let arr_var = env.lookup_composite(name).ok_or_else(|| {
                anyhow!("GIR malformed: undefined composite param `{name}`")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let idx_val = compile_scalar(b, idx, env, ctx)?;
            let idx_i64 = widen_to_i64(b, idx_val, prim_of(&idx.typ));
            let helper = ctx
                .helper_refs
                .get("graphix_valarray_index")
                .ok_or_else(|| anyhow!("missing graphix_valarray_index"))?;
            let call = b.ins().call(helper, &[arr_ptr, idx_i64]);
            let r = b.inst_results(call);
            Ok(CompiledExpr::Value { disc: r[0], payload: r[1] })
        }
        // Value-shape element reads for `t.0` / `s.field` whose element
        // type is a Variant / Nullable / DateTime / Duration / Bytes.
        // The `_get_value` helper returns an owned `Value` (two words);
        // `compile_element_read` packages it as a `CompiledExpr::Value`.
        // (Scalar / string / composite-pointer element types route to
        // `compile_scalar_impl` instead.) Tuple/struct indices are
        // statically valid, so there's no bounds check.
        GirOp::TupleGet { name, idx, elem_typ } => {
            let arr_var = env.lookup_composite(name).ok_or_else(|| {
                anyhow!("GIR malformed: undefined tuple param `{name}`")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let idx_const = b.ins().iconst(types::I64, *idx as i64);
            compile_element_read(b, arr_ptr, idx_const, elem_typ, false, ctx)
        }
        GirOp::StructGet { name, sorted_idx, elem_typ, .. } => {
            let arr_var = env.lookup_composite(name).ok_or_else(|| {
                anyhow!("GIR malformed: undefined struct param `{name}`")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let idx_const = b.ins().iconst(types::I64, *sorted_idx as i64);
            compile_element_read(b, arr_ptr, idx_const, elem_typ, true, ctx)
        }
        GirOp::BytesIndex { bytes, idx } => {
            // `bytes[i]` → Nullable<u8>. Compile the bytes operand to an
            // owned `(disc, payload)` (the helper consumes it) + the i64
            // index, then `graphix_bytes_index` does the bounds-checked
            // read, returning the result Value's two words.
            let (bd, bp) = compile_owned_value_operand(b, bytes, env, ctx)?;
            let i = compile_scalar(b, idx, env, ctx)?;
            let helper = ctx
                .helper_refs
                .get("graphix_bytes_index")
                .ok_or_else(|| anyhow!("missing graphix_bytes_index"))?;
            let call = b.ins().call(helper, &[bd, bp, i]);
            let rd = b.inst_results(call)[0];
            let rp = b.inst_results(call)[1];
            Ok(CompiledExpr::Value { disc: rd, payload: rp })
        }
        GirOp::MapRef { map, key } => {
            // `m{key}` → Nullable<V>. Both operands compile to owned
            // `(disc, payload)` Values (the helper consumes them);
            // `graphix_map_ref` does the lookup, returning the result
            // Value's two words.
            let (md, mp) = compile_owned_value_operand(b, map, env, ctx)?;
            let (kd, kp) = compile_owned_value_operand(b, key, env, ctx)?;
            let helper = ctx
                .helper_refs
                .get("graphix_map_ref")
                .ok_or_else(|| anyhow!("missing graphix_map_ref"))?;
            let call = b.ins().call(helper, &[md, mp, kd, kp]);
            let rd = b.inst_results(call)[0];
            let rp = b.inst_results(call)[1];
            Ok(CompiledExpr::Value { disc: rd, payload: rp })
        }
        GirOp::ArraySlice { source, start, end } => {
            // `a[i..j]` → Nullable<source>. Source compiles to an owned
            // `(disc, payload)` Value (the helper consumes it); present
            // bounds compile to i64 scalars, absent bounds pass 0 with a
            // cleared flag bit.
            let (sd, sp) = compile_owned_value_operand(b, source, env, ctx)?;
            let mut flags = 0i64;
            let start_v = match start {
                Some(e) => {
                    flags |= 1;
                    compile_scalar(b, e, env, ctx)?
                }
                None => b.ins().iconst(types::I64, 0),
            };
            let end_v = match end {
                Some(e) => {
                    flags |= 2;
                    compile_scalar(b, e, env, ctx)?
                }
                None => b.ins().iconst(types::I64, 0),
            };
            let flags_v = b.ins().iconst(types::I64, flags);
            let helper = ctx
                .helper_refs
                .get("graphix_array_slice")
                .ok_or_else(|| anyhow!("missing graphix_array_slice"))?;
            let call = b.ins().call(helper, &[sd, sp, start_v, end_v, flags_v]);
            let rd = b.inst_results(call)[0];
            let rp = b.inst_results(call)[1];
            Ok(CompiledExpr::Value { disc: rd, payload: rp })
        }
        GirOp::QopUnwrap { inner, success_typ } => {
            // Value-shape `?` (success type Variant/Nullable/DateTime/
            // Duration/Bytes/Map). The inner `Nullable<T>` compiles to an
            // owned `(disc, payload)` Value; on `Error` disc, drop it,
            // signal pending, and jump to `pending_exit`; otherwise the
            // non-error Value IS the result T (its own `(disc, payload)`,
            // passed through to the consumer which takes ownership). The
            // scalar/string/composite success cases live in the
            // `compile_scalar_impl` arm.
            debug_assert!(
                success_typ.is_value_shape(),
                "QopUnwrap in compile_value_expr with non-value-shape \
                 success_typ {success_typ:?}"
            );
            let (disc, payload) = compile_expr(b, inner, env, ctx)?.value()?;
            let is_err = b.ins().icmp_imm(
                cranelift_codegen::ir::condcodes::IntCC::Equal,
                disc,
                0x2000_0000_i64, // Typ::Error discriminant
            );
            let pending_set = ctx
                .helper_refs
                .get("graphix_dyncall_set_pending")
                .ok_or_else(|| anyhow!("missing graphix_dyncall_set_pending"))?;
            let value_drop = ctx
                .helper_refs
                .get("graphix_value_drop")
                .ok_or_else(|| anyhow!("missing graphix_value_drop"))?;
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
            b.ins().brif(is_err, pre_pending, &[], continue_block, &[]);

            b.switch_to_block(pre_pending);
            b.seal_block(pre_pending);
            // Drop the owned error Value before aborting — but ONLY when
            // `inner` is an owned producer. A Borrowed (Local) inner is
            // owned by its env slot, which `emit_pending_cleanup` ->
            // `drop_owned_composites` already drops; dropping it here too
            // would double-free (Arc double-decrement / use-after-free).
            if classify_composite_source(inner) == CompositeSource::Owned {
                b.ins().call(value_drop, &[disc, payload]);
            }
            b.ins().call(pending_set, &[]);
            emit_pending_cleanup(b, env, ctx)?;
            b.ins().jump(pending_exit, &[]);

            b.switch_to_block(continue_block);
            b.seal_block(continue_block);
            // The non-error Value IS the result T. Ensure it's owned: a
            // Borrowed (Local) inner aliases its env slot, which is also
            // dropped at scope exit — handing those bits to the consumer
            // (QopUnwrap is classified Owned) would double-free. An Owned
            // inner passes through unchanged.
            let (od, op) = ensure_owned_value(
                b,
                ctx,
                inner,
                CompiledExpr::Value { disc, payload },
            )?;
            Ok(CompiledExpr::Value { disc: od, payload: op })
        }
        GirOp::ArrayFind { array, elem, elem_local, predicate } => {
            // First element matching `predicate`, as a Nullable<elem>
            // (`null` if none). Early-exit loop whose two exit edges
            // (found / not-found) feed a two-word `(disc, payload)`
            // merge block — same Value-shape phi shape `compile_ifchain`
            // uses.
            let len_helper = ctx
                .helper_refs
                .get("graphix_valarray_len")
                .ok_or_else(|| anyhow!("missing graphix_valarray_len"))?;
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("GIR malformed: undefined array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let call = b.ins().call(len_helper, &[arr_ptr]);
            let len = b.inst_results(call)[0];
            let i_var = b.declare_var(types::I64);
            let zero = b.ins().iconst(types::I64, 0);
            b.def_var(i_var, zero);
            let loop_header = b.create_block();
            let loop_body = b.create_block();
            let found = b.create_block();
            let advance = b.create_block();
            let not_found = b.create_block();
            let exit = b.create_block();
            b.append_block_param(exit, types::I64); // disc
            b.append_block_param(exit, types::I64); // payload
            b.ins().jump(loop_header, &[]);
            b.switch_to_block(loop_header);
            let i_cur = b.use_var(i_var);
            let cond = b.ins().icmp(IntCC::SignedLessThan, i_cur, len);
            b.ins().brif(cond, loop_body, &[], not_found, &[]);
            b.switch_to_block(loop_body);
            let i_now = b.use_var(i_var);
            let mark = env.mark();
            // Element binding: scalar (locals) or composite (owned
            // `*ValArray`). `elem_prim` is `Some` for the scalar case; the
            // matched result is the element itself — for composite, the
            // owned element is consumed (wrapped into a Value) on the found
            // edge and dropped on the advance edge (matched-once / not-this-
            // iteration), like `ArrayFilter`'s conditional consume.
            let (elem_var, elem_prim) = match elem.as_prim() {
                Some(prim) => {
                    let get_helper = ctx
                        .helper_refs
                        .get(valarray_get_helper(prim)?)
                        .ok_or_else(|| anyhow!("missing valarray_get helper"))?;
                    let call = b.ins().call(get_helper, &[arr_ptr, i_now]);
                    let elem_val = b.inst_results(call)[0];
                    let elem_var = b.declare_var(prim_to_clif(prim));
                    b.def_var(elem_var, elem_val);
                    env.bind(elem_local.clone(), elem_var, prim);
                    (elem_var, Some(prim))
                }
                None => {
                    let get_helper = ctx
                        .helper_refs
                        .get("graphix_valarray_get_array")
                        .ok_or_else(|| {
                            anyhow!("missing graphix_valarray_get_array")
                        })?;
                    let call = b.ins().call(get_helper, &[arr_ptr, i_now]);
                    let elem_ptr = b.inst_results(call)[0];
                    let elem_var = b.declare_var(types::I64);
                    b.def_var(elem_var, elem_ptr);
                    env.bind_composite(elem_local.clone(), elem_var);
                    (elem_var, None)
                }
            };
            let keep = compile_scalar(b, predicate, env, ctx)?;
            env.truncate(mark);
            b.ins().brif(keep, found, &[], advance, &[]);
            b.switch_to_block(found);
            b.seal_block(found);
            let (disc, payload) = match elem_prim {
                Some(prim) => {
                    let elem_again = b.use_var(elem_var);
                    let disc =
                        b.ins().iconst(types::I64, prim_to_value_disc(prim));
                    let payload = scalar_to_payload_i64(b, prim, elem_again);
                    (disc, payload)
                }
                None => {
                    // Wrap the owned `*ValArray` element into a value-shape
                    // `(ARRAY_DISC, payload)` Value (consumes it).
                    let wrap = ctx
                        .helper_refs
                        .get("graphix_value_new_from_array")
                        .ok_or_else(|| {
                            anyhow!("missing graphix_value_new_from_array")
                        })?;
                    let elem_now = b.use_var(elem_var);
                    let call = b.ins().call(wrap, &[elem_now]);
                    let r = b.inst_results(call);
                    (r[0], r[1])
                }
            };
            b.ins()
                .jump(exit, &[BlockArg::Value(disc), BlockArg::Value(payload)]);
            b.switch_to_block(advance);
            if elem_prim.is_none() {
                // Not matched this iteration — drop the owned element.
                let drop_helper = ctx
                    .helper_refs
                    .get("graphix_valarray_drop")
                    .ok_or_else(|| anyhow!("missing graphix_valarray_drop"))?;
                let elem_now = b.use_var(elem_var);
                b.ins().call(drop_helper, &[elem_now]);
            }
            let one = b.ins().iconst(types::I64, 1);
            let i_next = b.ins().iadd(i_now, one);
            b.def_var(i_var, i_next);
            b.ins().jump(loop_header, &[]);
            b.seal_block(advance);
            b.seal_block(loop_body);
            b.seal_block(loop_header);
            b.switch_to_block(not_found);
            b.seal_block(not_found);
            let null_disc = b.ins().iconst(types::I64, value_disc::NULL);
            let null_payload = b.ins().iconst(types::I64, 0);
            b.ins().jump(
                exit,
                &[BlockArg::Value(null_disc), BlockArg::Value(null_payload)],
            );
            b.switch_to_block(exit);
            b.seal_block(exit);
            let disc = b.block_params(exit)[0];
            let payload = b.block_params(exit)[1];
            Ok(CompiledExpr::Value { disc, payload })
        }
        GirOp::ArrayFindMap { array, in_elem, elem_local, body } => {
            // Early-exit on the first non-null body result; the op's
            // value is that `Nullable<out>` (or null). Same merge shape
            // as ArrayFind, but the body produces the (disc, payload)
            // directly (not a predicate), and a composite element is the
            // owned `*ValArray` bound/dropped per iteration (the result
            // is the body value, so the element is dropped every pass).
            let len_helper = ctx
                .helper_refs
                .get("graphix_valarray_len")
                .ok_or_else(|| anyhow!("missing graphix_valarray_len"))?;
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("GIR malformed: undefined array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let call = b.ins().call(len_helper, &[arr_ptr]);
            let len = b.inst_results(call)[0];
            let i_var = b.declare_var(types::I64);
            let zero = b.ins().iconst(types::I64, 0);
            b.def_var(i_var, zero);
            let loop_header = b.create_block();
            let loop_body = b.create_block();
            let found = b.create_block();
            let advance = b.create_block();
            let not_found = b.create_block();
            let exit = b.create_block();
            b.append_block_param(exit, types::I64); // disc
            b.append_block_param(exit, types::I64); // payload
            b.ins().jump(loop_header, &[]);
            b.switch_to_block(loop_header);
            let i_cur = b.use_var(i_var);
            let cond = b.ins().icmp(IntCC::SignedLessThan, i_cur, len);
            b.ins().brif(cond, loop_body, &[], not_found, &[]);
            b.switch_to_block(loop_body);
            let i_now = b.use_var(i_var);
            let mark = env.mark();
            let composite_elem = match in_elem.as_prim() {
                Some(prim) => {
                    let get_helper = ctx
                        .helper_refs
                        .get(valarray_get_helper(prim)?)
                        .ok_or_else(|| anyhow!("missing valarray_get helper"))?;
                    let call = b.ins().call(get_helper, &[arr_ptr, i_now]);
                    let elem_val = b.inst_results(call)[0];
                    let elem_var = b.declare_var(prim_to_clif(prim));
                    b.def_var(elem_var, elem_val);
                    env.bind(elem_local.clone(), elem_var, prim);
                    None
                }
                None => {
                    let get_helper = ctx
                        .helper_refs
                        .get("graphix_valarray_get_array")
                        .ok_or_else(|| {
                            anyhow!("missing graphix_valarray_get_array")
                        })?;
                    let call = b.ins().call(get_helper, &[arr_ptr, i_now]);
                    let elem_ptr = b.inst_results(call)[0];
                    let elem_var = b.declare_var(types::I64);
                    b.def_var(elem_var, elem_ptr);
                    env.bind_composite(elem_local.clone(), elem_var);
                    Some(elem_var)
                }
            };
            // Body produces `Nullable<out>` as `(disc, payload)`.
            let (bdisc, bpayload) = compile_value_expr(b, body, env, ctx)?.value()?;
            if let Some(elem_var) = composite_elem {
                let drop_helper = ctx
                    .helper_refs
                    .get("graphix_valarray_drop")
                    .ok_or_else(|| anyhow!("missing graphix_valarray_drop"))?;
                let elem_now = b.use_var(elem_var);
                b.ins().call(drop_helper, &[elem_now]);
            }
            env.truncate(mark);
            let is_null = b.ins().icmp_imm(
                cranelift_codegen::ir::condcodes::IntCC::Equal,
                bdisc,
                value_disc::NULL,
            );
            // not-null → found; null → advance. `bdisc`/`bpayload`
            // dominate `found` (computed before the branch).
            b.ins().brif(is_null, advance, &[], found, &[]);
            b.switch_to_block(found);
            b.seal_block(found);
            b.ins().jump(
                exit,
                &[BlockArg::Value(bdisc), BlockArg::Value(bpayload)],
            );
            b.switch_to_block(advance);
            let one = b.ins().iconst(types::I64, 1);
            let i_next = b.ins().iadd(i_now, one);
            b.def_var(i_var, i_next);
            b.ins().jump(loop_header, &[]);
            b.seal_block(advance);
            b.seal_block(loop_body);
            b.seal_block(loop_header);
            b.switch_to_block(not_found);
            b.seal_block(not_found);
            let null_disc = b.ins().iconst(types::I64, value_disc::NULL);
            let null_payload = b.ins().iconst(types::I64, 0);
            b.ins().jump(
                exit,
                &[BlockArg::Value(null_disc), BlockArg::Value(null_payload)],
            );
            b.switch_to_block(exit);
            b.seal_block(exit);
            let disc = b.block_params(exit)[0];
            let payload = b.block_params(exit)[1];
            Ok(CompiledExpr::Value { disc, payload })
        }
        other => Err(anyhow!(
            "compile_value_expr called on op `{other:?}` with typ \
             `{:?}` — this op shouldn't produce a Value shape",
            e.typ
        )),
    }
}

/// Compile a [`GirExpr`] whose result type is scalar or composite-
/// pointer-shaped — i.e. anything that fits in a single CLIF
/// register. Most arms produce these; the few that produce a Value
/// shape (`ConstNull`, `IsNull`'s caller path, etc.) go through
/// [`compile_expr`] instead and short-circuit there.
///
/// Inner sub-expression evaluations call `compile_scalar` (a thin
/// wrapper that re-routes through `compile_expr` and asserts the
/// result is `Single`) when they only handle the scalar case, or
/// `compile_expr` directly when they want the full enum.
fn compile_scalar_impl(
    b: &mut FunctionBuilder,
    e: &GirExpr,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<ClifValue> {
    match &e.op {
        GirOp::Const(c) => Ok(compile_const(b, *c)),
        // Value-shape ops belong in `compile_value_expr`; reaching the
        // scalar path means routing drifted (or the kernel can't JIT,
        // which `compile_value_expr` reports as Err → interp fallback).
        GirOp::ConstValue(_) | GirOp::ValueArith { .. } => Err(anyhow!(
            "compile_scalar_impl reached for Value-shape datetime/duration \
             op — should route to compile_value_expr (interp fallback)"
        )),
        // ArrayFind produces a Nullable (Value-shape) result, so
        // `compile_expr` routes it to `compile_value_expr`; reaching the
        // scalar path means routing drifted.
        GirOp::ArrayFind { .. } => Err(anyhow!(
            "ArrayFind is Value-shape — should route to compile_value_expr"
        )),
        GirOp::BytesIndex { .. } => Err(anyhow!(
            "BytesIndex is Value-shape (Nullable<u8>) — should route to \
             compile_value_expr"
        )),
        GirOp::MapRef { .. } => Err(anyhow!(
            "MapRef is Value-shape (Nullable<V>) — should route to \
             compile_value_expr"
        )),
        GirOp::ArraySlice { .. } => Err(anyhow!(
            "ArraySlice is Value-shape (Nullable<source>) — should route \
             to compile_value_expr"
        )),
        GirOp::ConstStr(s) => {
            // Fetch the interned static `*const ArcStr` from this
            // kernel's `KernelStrings` table (built at compile time
            // by `collect_strings_*`) and bump its refcount. The
            // returned ArcStr is owned — the caller either passes
            // it to a consuming helper (e.g. push_arcstr) or returns
            // it from the kernel.
            let ptr = ctx.strings.get(s) as i64;
            let ptr_val = b.ins().iconst(types::I64, ptr);
            let helper = ctx
                .helper_refs
                .get("graphix_arcstr_clone_from_static")
                .ok_or_else(|| {
                    anyhow!("missing graphix_arcstr_clone_from_static")
                })?;
            let call = b.ins().call(helper, &[ptr_val]);
            Ok(b.inst_results(call)[0])
        }
        GirOp::Concat(parts) => compile_concat(b, parts, env, ctx),
        GirOp::ConstNull => {
            // Heap-allocate `Box::new(Value::Null)`; the returned
            // `*mut Value` is owned and routed through
            // `ensure_owned_composite` at every consumer site.
            let helper = ctx
                .helper_refs
                .get("graphix_value_new_null")
                .ok_or_else(|| anyhow!("missing graphix_value_new_null"))?;
            let call = b.ins().call(helper, &[]);
            Ok(b.inst_results(call)[0])
        }
        GirOp::IsNull(inner) => {
            // Operand is Null- or Nullable-typed (built by fusion's
            // `emit_arm` / `emit_type_predicate_cond`), so it lowers to
            // a two-register `(disc, payload)` Value. `null` iff
            // `disc == value_disc::NULL` — inline the disc compare; no
            // helper call (`graphix_value_is_null` is kept only for the
            // interpreter / direct tests). The `icmp_imm` result is an
            // `I8` 0/1 bool, the same shape `GirOp::Cmp` produces.
            let (disc, _payload) = compile_expr(b, inner, env, ctx)?.value()?;
            Ok(b.ins().icmp_imm(IntCC::Equal, disc, value_disc::NULL))
        }
        GirOp::QopUnwrap { inner, success_typ } => {
            // Compile inner — must be Nullable, so the result is a
            // Value-shape `(disc, payload)` pair. Compare disc
            // against `Typ::Error` (0x2000_0000); if equal, signal
            // pending and jump to `pending_exit`. Else extract
            // success T from the payload according to `success_typ`.
            //
            // For composite/Value-shape success types route to a
            // dedicated arm in `compile_value_expr` — this arm
            // handles only the scalar-shaped success cases (Prim,
            // String, composite-pointer).
            let cv = compile_expr(b, inner, env, ctx)?;
            let (disc, payload) = match cv {
                CompiledExpr::Value { disc, payload } => (disc, payload),
                CompiledExpr::Single(_) => {
                    return Err(anyhow!(
                        "GirOp::QopUnwrap: inner not Value-shape — \
                         emit_expr should only emit on Nullable inner"
                    ))
                }
            };
            // Error-disc check + pending-branch. Mirrors the existing
            // composite-DynCall pending-branch pattern: a side block
            // that runs `emit_pending_cleanup` + jumps to
            // `pending_exit` once we set the flag.
            let is_err = b.ins().icmp_imm(
                cranelift_codegen::ir::condcodes::IntCC::Equal,
                disc,
                0x2000_0000_i64, // Typ::Error discriminant
            );
            let pending_set = ctx
                .helper_refs
                .get("graphix_dyncall_set_pending")
                .ok_or_else(|| {
                    anyhow!("missing graphix_dyncall_set_pending")
                })?;
            let value_drop = ctx
                .helper_refs
                .get("graphix_value_drop")
                .ok_or_else(|| anyhow!("missing graphix_value_drop"))?;
            let inner_owned =
                classify_composite_source(inner) == CompositeSource::Owned;
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
            b.ins().brif(is_err, pre_pending, &[], continue_block, &[]);

            b.switch_to_block(pre_pending);
            b.seal_block(pre_pending);
            // Drop the owned error Value before aborting — only when
            // `inner` is an owned producer (a Borrowed Local inner is
            // owned by its env slot, which `emit_pending_cleanup` drops;
            // dropping it here too would double-free).
            if inner_owned {
                b.ins().call(value_drop, &[disc, payload]);
            }
            b.ins().call(pending_set, &[]);
            emit_pending_cleanup(b, env, ctx)?;
            b.ins().jump(pending_exit, &[]);

            b.switch_to_block(continue_block);
            b.seal_block(continue_block);
            // Extract success T from the (now-known-non-error) Value's
            // payload. For Prim, the payload word holds the bits cast
            // through `cast_u64_to_prim`. For String, the payload IS
            // the ArcStr's thin pointer bits. For composite shapes,
            // the payload is the `*mut ValArray`. QopUnwrap is classified
            // Owned, so a String/composite success from a Borrowed (Local)
            // inner must be cloned — otherwise the consumer and the env
            // slot both drop the same allocation (double-free).
            match success_typ {
                GirType::Prim(p) => Ok(cast_u64_to_prim(b, payload, *p)),
                GirType::String => {
                    if inner_owned {
                        Ok(payload)
                    } else {
                        let clone = ctx
                            .helper_refs
                            .get("graphix_arcstr_clone")
                            .ok_or_else(|| {
                                anyhow!("missing graphix_arcstr_clone")
                            })?;
                        let call = b.ins().call(clone, &[payload]);
                        Ok(b.inst_results(call)[0])
                    }
                }
                GirType::Array(_)
                | GirType::Tuple(_)
                | GirType::Struct(_) => {
                    if inner_owned {
                        Ok(payload)
                    } else {
                        let clone = ctx
                            .helper_refs
                            .get("graphix_valarray_clone")
                            .ok_or_else(|| {
                                anyhow!("missing graphix_valarray_clone")
                            })?;
                        let call = b.ins().call(clone, &[payload]);
                        Ok(b.inst_results(call)[0])
                    }
                }
                // Value-shape success types belong in
                // `compile_value_expr`; routing got here in error.
                GirType::Variant(_)
                | GirType::Nullable(_)
                | GirType::DateTime
                | GirType::Duration
                | GirType::Bytes | GirType::Map | GirType::Error => Err(anyhow!(
                    "QopUnwrap with Value-shape success_typ reached \
                     compile_scalar_impl — compile_expr should route \
                     to compile_value_expr"
                )),
                GirType::Unit | GirType::Null => Err(anyhow!(
                    "QopUnwrap with unsupported success_typ {:?}",
                    success_typ
                )),
            }
        }
        GirOp::Local(name) => {
            // Dispatch on the expression's GirType: scalar locals
            // sit in `env.locals`, composite locals (array/tuple/
            // struct pointers) in `env.composites`. The CLIF type
            // returned matches — scalars get their prim type,
            // composites get I64 (the pointer).
            match &e.typ {
                GirType::Prim(_) => {
                    let (var, _) = env.lookup(name).ok_or_else(|| {
                        anyhow!("GIR malformed: undefined scalar local `{name}`")
                    })?;
                    Ok(b.use_var(var))
                }
                GirType::Array(_)
                | GirType::Tuple(_)
                | GirType::Struct(_) => {
                    let var = env.lookup_composite(name).ok_or_else(|| {
                        anyhow!(
                            "GIR malformed: undefined composite local `{name}`"
                        )
                    })?;
                    Ok(b.use_var(var))
                }
                // Variant / Nullable Locals are Value-shape and
                // belong in `compile_value_expr`, not here.
                // `compile_expr` dispatches on the expression's typ
                // before calling `compile_scalar_impl`, so this arm
                // is unreachable in well-formed routing — but we
                // surface the error in case routing drifts.
                GirType::Variant(_)
                | GirType::Nullable(_)
                | GirType::DateTime
                | GirType::Duration
                | GirType::Bytes | GirType::Map | GirType::Error => Err(anyhow!(
                    "compile_scalar_impl reached for Value-shape Local \
                     `{name}` — `compile_expr` should have routed to \
                     `compile_value_expr`"
                )),
                GirType::Unit => Err(anyhow!(
                    "GIR malformed: Local `{name}` has Unit type"
                )),
                // String Local: read the slot's bits (an `ArcStr`
                // by value), refcount-bump via `graphix_arcstr_clone`
                // (which takes by value, `mem::forget`s the input,
                // returns a fresh clone). The slot's variable still
                // logically holds the original ref; the clone is
                // owned by this expression's caller.
                GirType::String => {
                    let var = env.lookup_string(name).ok_or_else(|| {
                        anyhow!(
                            "GIR malformed: undefined string local `{name}`"
                        )
                    })?;
                    let s = b.use_var(var);
                    let clone = ctx
                        .helper_refs
                        .get("graphix_arcstr_clone")
                        .ok_or_else(|| {
                            anyhow!("missing graphix_arcstr_clone")
                        })?;
                    let call = b.ins().call(clone, &[s]);
                    Ok(b.inst_results(call)[0])
                }
                // Bare `GirType::Null` locals don't exist; the only
                // `Null`-typed expression is the inline `ConstNull`
                // literal, which fusion always widens to `Nullable<T>`
                // before binding.
                GirType::Null => Err(anyhow!(
                    "Local `{name}` has bare Null type — should have \
                     widened to Nullable<T> at construction"
                )),
            }
        }
        GirOp::Bin { op, lhs, rhs } => {
            let l = compile_scalar(b, lhs, env, ctx)?;
            let r = compile_scalar(b, rhs, env, ctx)?;
            let prim = prim_of(&lhs.typ);
            // Integer div/rem: a zero divisor (or signed MIN/-1 overflow)
            // makes raw cranelift sdiv/udiv/srem/urem TRAP (#DE → SIGFPE,
            // crashing the whole process). The node-walk turns these into
            // an arith error and drops to bottom, so guard them and route
            // to `pending_exit` (kernel returns None = bottom) — matching
            // the node-walk and the interp's checked_div path. (#176
            // cluster A; the integer sibling of the #175 float-% trap.)
            if matches!(op, BinOp::Div | BinOp::Mod) && prim.is_integer() {
                use cranelift_codegen::ir::condcodes::IntCC;
                let is_zero = b.ins().icmp_imm(IntCC::Equal, r, 0);
                let bad = if prim.is_signed() {
                    let min: i64 = match prim {
                        PrimType::I8 => i8::MIN as i64,
                        PrimType::I16 => i16::MIN as i64,
                        PrimType::I32 => i32::MIN as i64,
                        _ => i64::MIN,
                    };
                    let is_min = b.ins().icmp_imm(IntCC::Equal, l, min);
                    let is_neg1 = b.ins().icmp_imm(IntCC::Equal, r, -1);
                    let overflow = b.ins().band(is_min, is_neg1);
                    b.ins().bor(is_zero, overflow)
                } else {
                    is_zero
                };
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
                b.ins().brif(bad, pre_pending, &[], continue_block, &[]);
                b.switch_to_block(pre_pending);
                b.seal_block(pre_pending);
                b.ins().call(pending_set, &[]);
                emit_pending_cleanup(b, env, ctx)?;
                b.ins().jump(pending_exit, &[]);
                b.switch_to_block(continue_block);
                b.seal_block(continue_block);
            }
            compile_bin(b, *op, prim, l, r)
        }
        GirOp::Cmp { op, lhs, rhs } => {
            let l = compile_scalar(b, lhs, env, ctx)?;
            let r = compile_scalar(b, rhs, env, ctx)?;
            Ok(compile_cmp(b, *op, prim_of(&lhs.typ), l, r))
        }
        GirOp::ValueEq { ne, lhs, rhs } => {
            // Value-shape `==`/`!=`: compile both operands to OWNED
            // `(disc, payload)` pairs (the helper consumes them) and
            // compare via netidx `Value` PartialEq.
            let (ld, lp) = compile_owned_value_operand(b, lhs, env, ctx)?;
            let (rd, rp) = compile_owned_value_operand(b, rhs, env, ctx)?;
            let helper = ctx
                .helper_refs
                .get("graphix_value_eq")
                .ok_or_else(|| anyhow!("missing graphix_value_eq"))?;
            let call = b.ins().call(helper, &[ld, lp, rd, rp]);
            let eq = b.inst_results(call)[0]; // I8 bool
            Ok(if *ne {
                let one = b.ins().iconst(types::I8, 1);
                b.ins().bxor(eq, one)
            } else {
                eq
            })
        }
        GirOp::BoolBin { op, lhs, rhs } => {
            // We use eager (non-short-circuit) evaluation here for
            // simplicity — GIR only ever emits BoolBin over pure
            // expressions, so eager eval has no observable effect.
            // The interpreter does short-circuit; the AOT-emitted
            // Rust `&&`/`||` short-circuit. No correctness diff for
            // pure code; if we ever fuse expressions with side-effects
            // we'll need to revisit.
            let l = compile_scalar(b, lhs, env, ctx)?;
            let r = compile_scalar(b, rhs, env, ctx)?;
            Ok(match op {
                BoolOp::And => b.ins().band(l, r),
                BoolOp::Or => b.ins().bor(l, r),
            })
        }
        GirOp::Not(inner) => {
            let v = compile_scalar(b, inner, env, ctx)?;
            // Bool is I8 in CLIF; XOR with 1 flips the low bit.
            let one = b.ins().iconst(types::I8, 1);
            Ok(b.ins().bxor(v, one))
        }
        GirOp::Cast { inner, target } => {
            let v = compile_scalar(b, inner, env, ctx)?;
            Ok(compile_cast(b, v, prim_of(&inner.typ), *target))
        }
        GirOp::Call { fn_name, args } => {
            // Cross-kernel call returning a scalar or composite pointer
            // (value-shape — Variant/Nullable — returns route through
            // `compile_value_expr` instead). Args are assembled in the
            // callee's kind-grouped ABI order; owned composite/value
            // args are dropped after the call (the callee clones every
            // composite/value param on entry). `callee_refs` is empty
            // only on the local-ctx path (no Calls) — the build should
            // have routed through `compile_kernel_with_callees`.
            let func_ref = ctx.callee_refs.get(fn_name).ok_or_else(|| {
                anyhow!(
                    "GIR malformed: GirOp::Call to `{fn_name}` but \
                     callee_refs has no entry for it (forgot to use \
                     compile_kernel_with_callees?)"
                )
            })?;
            let (clif_args, drops) =
                compile_call_clif_args(b, fn_name, args, env, ctx)?;
            let inst = b.ins().call(*func_ref, &clif_args);
            let result = {
                let results = b.inst_results(inst);
                if results.len() != 1 {
                    return Err(anyhow!(
                        "GIR malformed: callee `{fn_name}` returned {} \
                         values; a scalar / composite-pointer Call expects \
                         exactly 1",
                        results.len()
                    ));
                }
                results[0]
            };
            emit_call_arg_drops(b, ctx, &drops)?;
            Ok(result)
        }
        GirOp::Block { lets, tail } => {
            let mark = env.mark();
            for l in lets {
                match &l.value.typ {
                    GirType::Prim(p) => {
                        let v = compile_scalar(b, &l.value, env, ctx)?;
                        let var = b.declare_var(prim_to_clif(*p));
                        b.def_var(var, v);
                        env.bind(l.local.clone(), var, *p);
                    }
                    GirType::Array(_)
                    | GirType::Tuple(_)
                    | GirType::Struct(_) => {
                        // Owned `*mut ValArray`. `ensure_owned_composite`
                        // clones a Borrowed source so this block
                        // exclusively owns the local — otherwise the
                        // block-exit drop below would free a buffer the
                        // enclosing scope still holds.
                        let v = compile_scalar(b, &l.value, env, ctx)?;
                        let owned = ensure_owned_composite(
                            b, ctx, &l.value, v,
                        )?;
                        let var = b.declare_var(types::I64);
                        b.def_var(var, owned);
                        env.bind_composite(l.local.clone(), var);
                    }
                    GirType::Variant(_)
                    | GirType::Nullable(_)
                    | GirType::DateTime
                    | GirType::Duration
                    | GirType::Bytes | GirType::Map | GirType::Error => {
                        // Value-shape block local — same `ValueVar`
                        // pair + `ensure_owned_value` discipline as
                        // GirStmt::Let. Block-scope drop happens at
                        // exit (via `env.truncate` + the per-list
                        // `val_drop` calls below).
                        let cv = compile_expr(b, &l.value, env, ctx)?;
                        let (owned_disc, owned_payload) =
                            ensure_owned_value(b, ctx, &l.value, cv)?;
                        let disc_var = b.declare_var(types::I64);
                        let payload_var = b.declare_var(types::I64);
                        b.def_var(disc_var, owned_disc);
                        b.def_var(payload_var, owned_payload);
                        let vv =
                            ValueVar { disc: disc_var, payload: payload_var };
                        if matches!(l.value.typ, GirType::Variant(_)) {
                            env.bind_variant(l.local.clone(), vv);
                        } else {
                            env.bind_nullable(l.local.clone(), vv);
                        }
                    }
                    GirType::Unit => {
                        return Err(anyhow!(
                            "GIR malformed: GirOp::Block let with Unit value"
                        ));
                    }
                    GirType::String => {
                        // Block-scoped String local: same ownership
                        // discipline as `GirStmt::Let`'s String arm
                        // (producers / Local-reads both return
                        // owned ArcStr ptrs). Dropped at block exit
                        // alongside variants/nullables/composites.
                        let v = compile_scalar(b, &l.value, env, ctx)?;
                        let var = b.declare_var(types::I64);
                        b.def_var(var, v);
                        env.bind_string(l.local.clone(), var);
                    }
                    GirType::Null => {
                        return Err(anyhow!(
                            "GirOp::Block let with bare Null value — \
                             should have widened to Nullable<T>"
                        ));
                    }
                }
            }
            // The tail's value may alias a block-scoped composite
            // local we're about to drop (e.g. `tail = Local(block_let)`)
            // — `ensure_owned_composite` clones a Borrowed result so
            // the value handed back outlives the block.
            let result = compile_scalar(b, tail, env, ctx)?;
            let result = ensure_owned_composite(b, ctx, tail, result)?;
            // Drop the composite/variant locals introduced by THIS
            // block (owned pointers — they'd otherwise leak, once per
            // iteration if the block sits in a loop body). Scalars
            // need no drop. The pending path (a DynCall inside the
            // block) is mutually exclusive: its `pre_pending` block
            // runs `emit_pending_cleanup`, which drops every
            // `env.composites`/`env.variants` entry — these included.
            let arr_drop =
                ctx.helper_refs.get("graphix_valarray_drop").ok_or_else(
                    || anyhow!("missing graphix_valarray_drop"),
                )?;
            let val_drop =
                ctx.helper_refs.get("graphix_value_drop").ok_or_else(
                    || anyhow!("missing graphix_value_drop"),
                )?;
            let str_drop =
                ctx.helper_refs.get("graphix_arcstr_drop").ok_or_else(
                    || anyhow!("missing graphix_arcstr_drop"),
                )?;
            for (_, var) in &env.composites[mark.composites..] {
                let ptr = b.use_var(*var);
                b.ins().call(arr_drop, &[ptr]);
            }
            for (_, vv) in &env.variants[mark.variants..] {
                let disc = b.use_var(vv.disc);
                let payload = b.use_var(vv.payload);
                b.ins().call(val_drop, &[disc, payload]);
            }
            for (_, vv) in &env.nullables[mark.nullables..] {
                let disc = b.use_var(vv.disc);
                let payload = b.use_var(vv.payload);
                b.ins().call(val_drop, &[disc, payload]);
            }
            for (_, var) in &env.strings[mark.strings..] {
                let ptr = b.use_var(*var);
                b.ins().call(str_drop, &[ptr]);
            }
            env.truncate(mark);
            Ok(result)
        }
        GirOp::IfChain { arms } => {
            // Scalar / composite IfChain — Value-shape IfChains go
            // through `compile_value_expr` via `compile_expr`'s
            // dispatch, so reaching here means the result is single-
            // shape.
            compile_ifchain(b, arms, &e.typ, env, ctx)?.single()
        }
        GirOp::DynCall { fn_index, args, arg_types, return_type } => {
            // Marshal args into an `LPooled<Vec<Value>>` (per-shape
            // dispatch lives in `marshal_dyncall_args`), then call
            // `graphix_dyncall` which indirects through the thread-local
            // DynDispatchHandle. The buf is pushed onto
            // `dyncall_buf_stack` for the in-flight window (a nested
            // composite-return DynCall that pends drops it from its
            // `pre_pending` block).
            let dyncall = ctx
                .helper_refs
                .get("graphix_dyncall")
                .ok_or_else(|| anyhow!("missing graphix_dyncall"))?;
            let buf = marshal_dyncall_args(b, args, arg_types, env, ctx)?;
            let ret_kind: i64 = match return_type {
                GirType::Prim(_) => 0,
                GirType::Array(_) | GirType::Tuple(_) | GirType::Struct(_) => 1,
                // Variant and Nullable both come back as a boxed
                // `*mut Value` (dispatch_typed wraps the dispatcher's
                // returned `Value` in `Box::into_raw` regardless of the
                // outer enum tag), so they share the ret_kind=2 path.
                GirType::Variant(_)
                | GirType::Nullable(_)
                | GirType::DateTime
                | GirType::Duration
                | GirType::Bytes | GirType::Map | GirType::Error => 2,
                // Unit return: dispatcher returns 0 (the slot is
                // discarded by the caller). ret_kind=3 tells
                // dispatch_typed not to box anything.
                GirType::Unit => 3,
                // String return: dispatcher extracts the ArcStr from
                // `Value::String`, transmutes to raw u64 bits, returns
                // in word0. Caller's SSA reads the bits directly as
                // an owned `arcstr::ArcStr` (`repr(transparent)`).
                GirType::String => 4,
                GirType::Null => {
                    return Err(anyhow!(
                        "DynCall with bare Null return — should have \
                         widened to Nullable<T> at construction"
                    ));
                }
            };
            let fn_idx_val =
                b.ins().iconst(types::I32, *fn_index as i64);
            let ret_kind_val = b.ins().iconst(types::I8, ret_kind);
            let call =
                b.ins().call(dyncall, &[fn_idx_val, buf, ret_kind_val]);
            // `graphix_dyncall` now has two `I64` returns (disc,
            // payload) so that ret_kind=2 (Value-shape) can deliver
            // both words. For scalar/composite/unit return kinds the
            // first return holds the value (or pointer / 0); the
            // second is undefined for kinds 0/3 and holds the payload
            // for kinds 1/2. The scalar path reads return[0]; the
            // Value path reads both — and the Value path is handled
            // in `compile_value_expr`, not here.
            let raw_u64 = b.inst_results(call)[0];
            // `graphix_dyncall` consumed the args buf — pop it off
            // the in-flight stack before the (possible) pending
            // branch below, so a `pre_pending` block here doesn't
            // try to double-free it.
            ctx.dyncall_buf_stack.borrow_mut().pop();

            match return_type {
                GirType::Prim(p) => {
                    // Scalar return: 0 on pending is a harmless
                    // sentinel for downstream scalar arithmetic.
                    // No branch needed — the wrapper-level
                    // DYNCALL_PENDING check in GirNode::update
                    // discards the whole kernel result.
                    Ok(cast_u64_to_prim(b, raw_u64, *p))
                }
                GirType::Unit => {
                    // Unit return: dispatcher returned (0, _). The
                    // downstream `Discard` throws it away. The
                    // wrapper-level DYNCALL_PENDING check still
                    // fires.
                    Ok(raw_u64)
                }
                GirType::String => {
                    // String return: `raw_u64` is the ArcStr's raw
                    // thin-pointer bits (transferred ownership). The
                    // wrapper-level DYNCALL_PENDING check in
                    // GirNode::update discards the kernel result on
                    // pending; the 0 sentinel from a pending dispatch
                    // is never decoded.
                    Ok(raw_u64)
                }
                GirType::Null => {
                    return Err(anyhow!(
                        "DynCall with bare Null return — should have \
                         widened to Nullable<T> at construction"
                    ));
                }
                // Variant / Nullable DynCall returns are Value-shape
                // and belong in `compile_value_expr`; compile_expr
                // dispatches before reaching this scalar arm. Surface
                // an error in case routing drifts.
                GirType::Variant(_)
                | GirType::Nullable(_)
                | GirType::DateTime
                | GirType::Duration
                | GirType::Bytes | GirType::Map | GirType::Error => Err(anyhow!(
                    "DynCall with Value-shape return reached \
                     `compile_scalar_impl` — `compile_expr` should \
                     have routed to `compile_value_expr`"
                )),
                GirType::Array(_)
                | GirType::Tuple(_)
                | GirType::Struct(_) => {
                    // Composite return: `raw_u64` is an owned
                    // `*mut ValArray`, or null on pending. Null can't
                    // be deref'd by downstream ops, so we branch: on
                    // pending, drop every owned local + every outer
                    // in-flight DynCall buf, then jump to
                    // `pending_exit`.
                    let pending_take = ctx
                        .helper_refs
                        .get("graphix_dyncall_pending_take")
                        .ok_or_else(|| {
                            anyhow!("missing graphix_dyncall_pending_take")
                        })?;
                    let call = b.ins().call(pending_take, &[]);
                    let pending = b.inst_results(call)[0];

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
                    b.ins().brif(
                        pending,
                        pre_pending,
                        &[],
                        continue_block,
                        &[],
                    );

                    b.switch_to_block(pre_pending);
                    b.seal_block(pre_pending);
                    emit_pending_cleanup(b, env, ctx)?;
                    b.ins().jump(pending_exit, &[]);

                    b.switch_to_block(continue_block);
                    b.seal_block(continue_block);
                    Ok(raw_u64)
                }
            }
        }
        GirOp::ArrayLen { name } => {
            let helper = ctx.helper_refs.get("graphix_valarray_len").ok_or_else(
                || anyhow!("missing graphix_valarray_len helper"),
            )?;
            let arr_var = env.lookup_composite(name).ok_or_else(|| {
                anyhow!("GIR malformed: undefined composite param `{name}`")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let call = b.ins().call(helper, &[arr_ptr]);
            // Helper returns usize (i64); ArrayLen's GirExpr.typ is
            // Prim(U64), so width matches.
            Ok(b.inst_results(call)[0])
        }
        GirOp::ArrayGet { .. } => {
            // `array[i]`'s result type is always `Nullable<elem>` (it
            // can produce an `ArrayIndexError`), so `compile_expr`
            // routes it to `compile_value_expr`. Reaching the scalar
            // path means the op was mis-typed.
            Err(anyhow!(
                "GIR malformed: ArrayGet reached compile_scalar_impl — \
                 its result type must be Nullable<elem>"
            ))
        }
        GirOp::TupleGet { name, idx, elem_typ } => {
            let arr_var = env.lookup_composite(name).ok_or_else(|| {
                anyhow!("GIR malformed: undefined tuple param `{name}`")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let idx_const = b.ins().iconst(types::I64, *idx as i64);
            compile_element_read(b, arr_ptr, idx_const, elem_typ, false, ctx)?
                .single()
        }
        GirOp::StructGet { name, sorted_idx, elem_typ, .. } => {
            let arr_var = env.lookup_composite(name).ok_or_else(|| {
                anyhow!("GIR malformed: undefined struct param `{name}`")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let idx_const = b.ins().iconst(types::I64, *sorted_idx as i64);
            compile_element_read(b, arr_ptr, idx_const, elem_typ, true, ctx)?
                .single()
        }
        GirOp::TupleNew { fields, elem_types } => {
            // Build a `Vec<Value>` field-by-field via the producer
            // helpers, then finalize into an owned `*mut ValArray`.
            // Returned ClifValue is I64 (the pointer); consumers
            // (TupleGet, tail-call rebind, kernel return) treat it
            // as a `*const ValArray`.
            let buf_new = ctx
                .helper_refs
                .get("graphix_value_buf_new")
                .ok_or_else(|| anyhow!("missing graphix_value_buf_new"))?;
            let finalize = ctx
                .helper_refs
                .get("graphix_valarray_finalize")
                .ok_or_else(|| anyhow!("missing graphix_valarray_finalize"))?;
            let cap = b.ins().iconst(types::I64, fields.len() as i64);
            let call = b.ins().call(buf_new, &[cap]);
            let buf = b.inst_results(call)[0];
            // `_t` is the field's declared GirType; the dispatch
            // happens inside `compile_and_push_field` on the
            // compiled expression's `field.typ` (same value).
            for (f, _t) in fields.iter().zip(elem_types.iter()) {
                compile_and_push_field(b, env, ctx, buf, f)?;
            }
            let call = b.ins().call(finalize, &[buf]);
            Ok(b.inst_results(call)[0])
        }
        GirOp::StructNew { sorted_fields, sorted_types } => {
            // Layout: outer ValArray of inner [name, value] pairs.
            //   outer = buf_new(N)
            //   for each field:
            //     inner = buf_new(2)
            //     push_arcstr(inner, &interned_name)
            //     push_<T>(inner, value)
            //     inner_arr = finalize(inner)
            //     push_array(outer, inner_arr)   // takes ownership
            //   finalize(outer)
            let buf_new = ctx
                .helper_refs
                .get("graphix_value_buf_new")
                .ok_or_else(|| anyhow!("missing graphix_value_buf_new"))?;
            let push_arcstr = ctx
                .helper_refs
                .get("graphix_value_buf_push_arcstr")
                .ok_or_else(|| {
                    anyhow!("missing graphix_value_buf_push_arcstr")
                })?;
            let push_array = ctx
                .helper_refs
                .get("graphix_value_buf_push_array")
                .ok_or_else(|| {
                    anyhow!("missing graphix_value_buf_push_array")
                })?;
            let finalize = ctx
                .helper_refs
                .get("graphix_valarray_finalize")
                .ok_or_else(|| anyhow!("missing graphix_valarray_finalize"))?;
            let outer_cap =
                b.ins().iconst(types::I64, sorted_fields.len() as i64);
            let call = b.ins().call(buf_new, &[outer_cap]);
            let outer = b.inst_results(call)[0];
            for ((name, expr), (_, _typ)) in
                sorted_fields.iter().zip(sorted_types.iter())
            {
                let inner_cap = b.ins().iconst(types::I64, 2);
                let call = b.ins().call(buf_new, &[inner_cap]);
                let inner = b.inst_results(call)[0];
                let name_ptr = ctx.strings.get(name) as i64;
                let name_ptr_val = b.ins().iconst(types::I64, name_ptr);
                b.ins().call(push_arcstr, &[inner, name_ptr_val]);
                // Composite-element lowering: dispatch on the field
                // expression's GirType (composite, variant, string,
                // prim). `compile_and_push_field` picks the right
                // helper.
                compile_and_push_field(b, env, ctx, inner, expr)?;
                let call = b.ins().call(finalize, &[inner]);
                let inner_arr = b.inst_results(call)[0];
                b.ins().call(push_array, &[outer, inner_arr]);
            }
            let call = b.ins().call(finalize, &[outer]);
            Ok(b.inst_results(call)[0])
        }
        GirOp::VariantNew { .. } => {
            // VariantNew produces a Value-shape result and lives in
            // `compile_value_expr`; `compile_expr` dispatches Value-
            // shape exprs there before reaching this scalar arm.
            // Reaching here means routing drifted.
            Err(anyhow!(
                "compile_scalar_impl reached for VariantNew — \
                 `compile_expr` should have routed to `compile_value_expr`"
            ))
        }
        GirOp::ArrayInit { n, idx_local, body } => {
            // n_val = eval n.    Scalar I64-ish; widen if narrower.
            // buf = buf_new(n_val)
            // loop i in 0..n_val:
            //     bind idx_local to i in env
            //     compile_and_push_field(buf, body)  // any elem shape
            // arr = finalize(buf)
            //
            // Block structure: caller's current block jumps to
            // loop_header; loop_header tests the counter and brif's
            // to loop_body (push + increment) or loop_exit (finalize).
            // loop_exit is where compile_expr returns to.
            let buf_new = ctx
                .helper_refs
                .get("graphix_value_buf_new")
                .ok_or_else(|| anyhow!("missing graphix_value_buf_new"))?;
            let finalize = ctx
                .helper_refs
                .get("graphix_valarray_finalize")
                .ok_or_else(|| anyhow!("missing graphix_valarray_finalize"))?;
            let n_raw = compile_scalar(b, n, env, ctx)?;
            let n_widened = widen_to_i64(b, n_raw, prim_of(&n.typ));
            // Clamp a negative `n` to 0 (matching the node-walk's
            // `n.max(0)`): `buf_new(neg)` reserves usize::MAX and panics
            // across the extern "C" boundary. The loop guard
            // (`i < n_val`, signed) already wouldn't iterate, but the
            // allocation happens first.
            let zero_clamp = b.ins().iconst(types::I64, 0);
            let is_neg =
                b.ins().icmp(IntCC::SignedLessThan, n_widened, zero_clamp);
            let n_val = b.ins().select(is_neg, zero_clamp, n_widened);
            let call = b.ins().call(buf_new, &[n_val]);
            let buf = b.inst_results(call)[0];
            // Register the in-progress output buf for pending cleanup: a
            // value-shape/composite DynCall or a `?` (QopUnwrap) in the
            // body can pend mid-loop and jump to `pending_exit`; without
            // this the half-built buf (+ its accumulated elements) leaks.
            // Popped before `finalize` on the normal path.
            register_hof_buf(b, ctx, buf);
            let i_var = b.declare_var(types::I64);
            let zero = b.ins().iconst(types::I64, 0);
            b.def_var(i_var, zero);
            let loop_header = b.create_block();
            let loop_body = b.create_block();
            let loop_exit = b.create_block();
            b.ins().jump(loop_header, &[]);
            // Header: test i < n. brif uses I8 cond (0 or 1).
            b.switch_to_block(loop_header);
            let i_cur = b.use_var(i_var);
            let cond = b.ins().icmp(IntCC::SignedLessThan, i_cur, n_val);
            b.ins()
                .brif(cond, loop_body, &[], loop_exit, &[]);
            // Body: bind idx_local, compile body expr, push, increment.
            b.switch_to_block(loop_body);
            let mark = env.mark();
            env.bind(idx_local.clone(), i_var, PrimType::I64);
            // Compile the body and push it with the shape-appropriate
            // helper (prim → push_<T>, composite → push_array, value-
            // shape → push_value, string → push_arcstr).
            compile_and_push_field(b, env, ctx, buf, body)?;
            env.truncate(mark);
            let i_now = b.use_var(i_var);
            let one = b.ins().iconst(types::I64, 1);
            let i_next = b.ins().iadd(i_now, one);
            b.def_var(i_var, i_next);
            b.ins().jump(loop_header, &[]);
            b.seal_block(loop_body);
            b.seal_block(loop_header);
            // Exit: finalize and return the array pointer.
            b.switch_to_block(loop_exit);
            b.seal_block(loop_exit);
            unregister_hof_buf(ctx);
            let call = b.ins().call(finalize, &[buf]);
            Ok(b.inst_results(call)[0])
        }
        GirOp::ArrayMap { array, in_elem, elem_local, body } => {
            // Iterate over an existing composite param/local.
            //   arr_ptr = lookup_composite(array)
            //   len = call valarray_len(arr_ptr)
            //   buf = call buf_new(len)
            //   loop i in 0..len:
            //     elem = call valarray_get_<in_elem>(arr_ptr, i)
            //     bind elem_local = elem
            //     compile_and_push_field(buf, body)  // any out shape
            //   finalize(buf)
            //
            let buf_new = ctx
                .helper_refs
                .get("graphix_value_buf_new")
                .ok_or_else(|| anyhow!("missing graphix_value_buf_new"))?;
            let finalize = ctx
                .helper_refs
                .get("graphix_valarray_finalize")
                .ok_or_else(|| anyhow!("missing graphix_valarray_finalize"))?;
            let len_helper = ctx
                .helper_refs
                .get("graphix_valarray_len")
                .ok_or_else(|| anyhow!("missing graphix_valarray_len"))?;
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("GIR malformed: undefined array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let call = b.ins().call(len_helper, &[arr_ptr]);
            let len = b.inst_results(call)[0];
            let call = b.ins().call(buf_new, &[len]);
            let buf = b.inst_results(call)[0];
            register_hof_buf(b, ctx, buf); // pending-cleanup: drop half-built buf if body pends
            let i_var = b.declare_var(types::I64);
            let zero = b.ins().iconst(types::I64, 0);
            b.def_var(i_var, zero);
            let loop_header = b.create_block();
            let loop_body = b.create_block();
            let loop_exit = b.create_block();
            b.ins().jump(loop_header, &[]);
            b.switch_to_block(loop_header);
            let i_cur = b.use_var(i_var);
            let cond = b.ins().icmp(IntCC::SignedLessThan, i_cur, len);
            b.ins().brif(cond, loop_body, &[], loop_exit, &[]);
            b.switch_to_block(loop_body);
            let i_now = b.use_var(i_var);
            let mark = env.mark();
            match in_elem.as_prim() {
                // Scalar element: get the scalar, bind into the `locals`
                // slot.
                Some(prim) => {
                    let get_helper = ctx
                        .helper_refs
                        .get(valarray_get_helper(prim)?)
                        .ok_or_else(|| anyhow!("missing valarray_get helper"))?;
                    let call = b.ins().call(get_helper, &[arr_ptr, i_now]);
                    let elem = b.inst_results(call)[0];
                    let elem_var = b.declare_var(prim_to_clif(prim));
                    b.def_var(elem_var, elem);
                    env.bind(elem_local.clone(), elem_var, prim);
                    compile_and_push_field(b, env, ctx, buf, body)?;
                }
                // Composite element (`Array<(k,v)>` etc.): the element is
                // an owned `*mut ValArray`; bind it into the composite
                // slot, run the body (its `TupleGet`s clone the fields
                // they read), then drop the per-iteration element.
                None => {
                    let get_helper = ctx
                        .helper_refs
                        .get("graphix_valarray_get_array")
                        .ok_or_else(|| {
                            anyhow!("missing graphix_valarray_get_array")
                        })?;
                    let call = b.ins().call(get_helper, &[arr_ptr, i_now]);
                    let elem_ptr = b.inst_results(call)[0];
                    let elem_var = b.declare_var(types::I64);
                    b.def_var(elem_var, elem_ptr);
                    env.bind_composite(elem_local.clone(), elem_var);
                    compile_and_push_field(b, env, ctx, buf, body)?;
                    let drop_helper = ctx
                        .helper_refs
                        .get("graphix_valarray_drop")
                        .ok_or_else(|| anyhow!("missing graphix_valarray_drop"))?;
                    let elem_now = b.use_var(elem_var);
                    b.ins().call(drop_helper, &[elem_now]);
                }
            }
            env.truncate(mark);
            let one = b.ins().iconst(types::I64, 1);
            let i_next = b.ins().iadd(i_now, one);
            b.def_var(i_var, i_next);
            b.ins().jump(loop_header, &[]);
            b.seal_block(loop_body);
            b.seal_block(loop_header);
            b.switch_to_block(loop_exit);
            b.seal_block(loop_exit);
            unregister_hof_buf(ctx);
            let call = b.ins().call(finalize, &[buf]);
            Ok(b.inst_results(call)[0])
        }
        GirOp::ArrayFilter { array, elem, elem_local, predicate } => {
            // Same shape as ArrayMap but with a conditional push.
            //   loop i in 0..len:
            //     elem = get(arr, i); bind elem_local
            //     keep = eval predicate (Bool, CLIF I8)
            //     if keep: push elem; else (composite) drop elem
            //     i += 1
            // A composite element is an owned `*mut ValArray`: on keep it
            // moves into the output (`push_array`), on not-keep it must be
            // dropped — hence the extra `drop_block` for the composite case.
            let composite = elem.as_prim().is_none();
            let buf_new = ctx
                .helper_refs
                .get("graphix_value_buf_new")
                .ok_or_else(|| anyhow!("missing graphix_value_buf_new"))?;
            let finalize = ctx
                .helper_refs
                .get("graphix_valarray_finalize")
                .ok_or_else(|| anyhow!("missing graphix_valarray_finalize"))?;
            let len_helper = ctx
                .helper_refs
                .get("graphix_valarray_len")
                .ok_or_else(|| anyhow!("missing graphix_valarray_len"))?;
            let get_helper = match elem.as_prim() {
                Some(prim) => ctx
                    .helper_refs
                    .get(valarray_get_helper(prim)?)
                    .ok_or_else(|| anyhow!("missing valarray_get helper"))?,
                None => ctx
                    .helper_refs
                    .get("graphix_valarray_get_array")
                    .ok_or_else(|| anyhow!("missing graphix_valarray_get_array"))?,
            };
            let push = match elem.as_prim() {
                Some(prim) => ctx
                    .helper_refs
                    .get(value_buf_push_helper(prim)?)
                    .ok_or_else(|| anyhow!("missing push helper for {prim:?}"))?,
                None => ctx
                    .helper_refs
                    .get("graphix_value_buf_push_array")
                    .ok_or_else(|| anyhow!("missing graphix_value_buf_push_array"))?,
            };
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("GIR malformed: undefined array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let call = b.ins().call(len_helper, &[arr_ptr]);
            let len = b.inst_results(call)[0];
            let call = b.ins().call(buf_new, &[len]);
            let buf = b.inst_results(call)[0];
            register_hof_buf(b, ctx, buf); // pending-cleanup: drop half-built buf if body pends
            let i_var = b.declare_var(types::I64);
            let zero = b.ins().iconst(types::I64, 0);
            b.def_var(i_var, zero);
            let loop_header = b.create_block();
            let loop_body = b.create_block();
            let push_block = b.create_block();
            let advance = b.create_block();
            let loop_exit = b.create_block();
            // not-kept owned composite element → drop before advancing
            let drop_block = if composite {
                Some(b.create_block())
            } else {
                None
            };
            b.ins().jump(loop_header, &[]);
            b.switch_to_block(loop_header);
            let i_cur = b.use_var(i_var);
            let cond = b.ins().icmp(IntCC::SignedLessThan, i_cur, len);
            b.ins().brif(cond, loop_body, &[], loop_exit, &[]);
            b.switch_to_block(loop_body);
            let i_now = b.use_var(i_var);
            let call = b.ins().call(get_helper, &[arr_ptr, i_now]);
            let elem_val = b.inst_results(call)[0];
            let mark = env.mark();
            let elem_var = if composite {
                let v = b.declare_var(types::I64);
                b.def_var(v, elem_val);
                env.bind_composite(elem_local.clone(), v);
                v
            } else {
                let prim = elem.as_prim().unwrap();
                let v = b.declare_var(prim_to_clif(prim));
                b.def_var(v, elem_val);
                env.bind(elem_local.clone(), v, prim);
                v
            };
            let keep = compile_scalar(b, predicate, env, ctx)?;
            env.truncate(mark);
            let not_kept = drop_block.unwrap_or(advance);
            b.ins().brif(keep, push_block, &[], not_kept, &[]);
            b.switch_to_block(push_block);
            let elem_again = b.use_var(elem_var);
            b.ins().call(push, &[buf, elem_again]);
            b.ins().jump(advance, &[]);
            b.seal_block(push_block);
            if let Some(drop_block) = drop_block {
                b.switch_to_block(drop_block);
                let drop_helper = ctx
                    .helper_refs
                    .get("graphix_valarray_drop")
                    .ok_or_else(|| anyhow!("missing graphix_valarray_drop"))?;
                let elem_now = b.use_var(elem_var);
                b.ins().call(drop_helper, &[elem_now]);
                b.ins().jump(advance, &[]);
                b.seal_block(drop_block);
            }
            b.switch_to_block(advance);
            let one = b.ins().iconst(types::I64, 1);
            let i_next = b.ins().iadd(i_now, one);
            b.def_var(i_var, i_next);
            b.ins().jump(loop_header, &[]);
            b.seal_block(advance);
            b.seal_block(loop_body);
            b.seal_block(loop_header);
            b.switch_to_block(loop_exit);
            b.seal_block(loop_exit);
            unregister_hof_buf(ctx);
            let call = b.ins().call(finalize, &[buf]);
            Ok(b.inst_results(call)[0])
        }
        GirOp::ArrayFilterMap { array, in_elem, elem_local, out_elem, body } => {
            // Like ArrayFilter, but the body yields a `Nullable<out_elem>`
            // per element (Value-shape `(disc, payload)`); collect the
            // non-null payloads.
            //   loop i in 0..len:
            //     elem = get(arr, i); bind elem_local
            //     (disc, payload) = compile body
            //     if disc == NULL: skip; else push cast(payload, out_elem)
            let buf_new = ctx
                .helper_refs
                .get("graphix_value_buf_new")
                .ok_or_else(|| anyhow!("missing graphix_value_buf_new"))?;
            let finalize = ctx
                .helper_refs
                .get("graphix_valarray_finalize")
                .ok_or_else(|| anyhow!("missing graphix_valarray_finalize"))?;
            let len_helper = ctx
                .helper_refs
                .get("graphix_valarray_len")
                .ok_or_else(|| anyhow!("missing graphix_valarray_len"))?;
            let get_helper = ctx
                .helper_refs
                .get(valarray_get_helper(*in_elem)?)
                .ok_or_else(|| anyhow!("missing valarray_get helper"))?;
            let push = ctx
                .helper_refs
                .get(value_buf_push_helper(*out_elem)?)
                .ok_or_else(|| anyhow!("missing push helper for {out_elem:?}"))?;
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("GIR malformed: undefined array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let call = b.ins().call(len_helper, &[arr_ptr]);
            let len = b.inst_results(call)[0];
            let call = b.ins().call(buf_new, &[len]);
            let buf = b.inst_results(call)[0];
            register_hof_buf(b, ctx, buf); // pending-cleanup: drop half-built buf if body pends
            let i_var = b.declare_var(types::I64);
            let zero = b.ins().iconst(types::I64, 0);
            b.def_var(i_var, zero);
            let loop_header = b.create_block();
            let loop_body = b.create_block();
            let push_block = b.create_block();
            let advance = b.create_block();
            let loop_exit = b.create_block();
            b.ins().jump(loop_header, &[]);
            b.switch_to_block(loop_header);
            let i_cur = b.use_var(i_var);
            let cond = b.ins().icmp(IntCC::SignedLessThan, i_cur, len);
            b.ins().brif(cond, loop_body, &[], loop_exit, &[]);
            b.switch_to_block(loop_body);
            let i_now = b.use_var(i_var);
            let call = b.ins().call(get_helper, &[arr_ptr, i_now]);
            let elem_val = b.inst_results(call)[0];
            let elem_var = b.declare_var(prim_to_clif(*in_elem));
            b.def_var(elem_var, elem_val);
            let mark = env.mark();
            env.bind(elem_local.clone(), elem_var, *in_elem);
            let cv = compile_expr(b, body, env, ctx)?;
            env.truncate(mark);
            let (disc, payload) = match cv {
                CompiledExpr::Value { disc, payload } => (disc, payload),
                CompiledExpr::Single(_) => {
                    return Err(anyhow!(
                        "ArrayFilterMap body not Value-shape — expected \
                         a Nullable result"
                    ))
                }
            };
            let is_null =
                b.ins().icmp_imm(IntCC::Equal, disc, value_disc::NULL);
            b.ins().brif(is_null, advance, &[], push_block, &[]);
            b.switch_to_block(push_block);
            let scalar = cast_u64_to_prim(b, payload, *out_elem);
            b.ins().call(push, &[buf, scalar]);
            b.ins().jump(advance, &[]);
            b.seal_block(push_block);
            b.switch_to_block(advance);
            let one = b.ins().iconst(types::I64, 1);
            let i_next = b.ins().iadd(i_now, one);
            b.def_var(i_var, i_next);
            b.ins().jump(loop_header, &[]);
            b.seal_block(advance);
            b.seal_block(loop_body);
            b.seal_block(loop_header);
            b.switch_to_block(loop_exit);
            b.seal_block(loop_exit);
            unregister_hof_buf(ctx);
            let call = b.ins().call(finalize, &[buf]);
            Ok(b.inst_results(call)[0])
        }
        GirOp::ArrayFlatMap { array, in_elem, elem_local, out_elem: _, body } => {
            // Like ArrayMap, but each body result is an owned
            // `Array<out_elem>` (composite pointer) concatenated into the
            // output buf via `graphix_value_buf_extend_from_array` (which
            // flattens + drops it). Linear — no per-element branch.
            let buf_new = ctx
                .helper_refs
                .get("graphix_value_buf_new")
                .ok_or_else(|| anyhow!("missing graphix_value_buf_new"))?;
            let finalize = ctx
                .helper_refs
                .get("graphix_valarray_finalize")
                .ok_or_else(|| anyhow!("missing graphix_valarray_finalize"))?;
            let len_helper = ctx
                .helper_refs
                .get("graphix_valarray_len")
                .ok_or_else(|| anyhow!("missing graphix_valarray_len"))?;
            let extend = ctx
                .helper_refs
                .get("graphix_value_buf_extend_from_array")
                .ok_or_else(|| {
                    anyhow!("missing graphix_value_buf_extend_from_array")
                })?;
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("GIR malformed: undefined array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let call = b.ins().call(len_helper, &[arr_ptr]);
            let len = b.inst_results(call)[0];
            let call = b.ins().call(buf_new, &[len]);
            let buf = b.inst_results(call)[0];
            register_hof_buf(b, ctx, buf); // pending-cleanup: drop half-built buf if body pends
            let i_var = b.declare_var(types::I64);
            let zero = b.ins().iconst(types::I64, 0);
            b.def_var(i_var, zero);
            let loop_header = b.create_block();
            let loop_body = b.create_block();
            let loop_exit = b.create_block();
            b.ins().jump(loop_header, &[]);
            b.switch_to_block(loop_header);
            let i_cur = b.use_var(i_var);
            let cond = b.ins().icmp(IntCC::SignedLessThan, i_cur, len);
            b.ins().brif(cond, loop_body, &[], loop_exit, &[]);
            b.switch_to_block(loop_body);
            let i_now = b.use_var(i_var);
            let mark = env.mark();
            // Element binding: scalar (locals) or composite (owned
            // `*ValArray` in the composite slot, dropped after the body).
            let composite_elem = match in_elem.as_prim() {
                Some(prim) => {
                    let get_helper = ctx
                        .helper_refs
                        .get(valarray_get_helper(prim)?)
                        .ok_or_else(|| anyhow!("missing valarray_get helper"))?;
                    let call = b.ins().call(get_helper, &[arr_ptr, i_now]);
                    let elem_val = b.inst_results(call)[0];
                    let elem_var = b.declare_var(prim_to_clif(prim));
                    b.def_var(elem_var, elem_val);
                    env.bind(elem_local.clone(), elem_var, prim);
                    None
                }
                None => {
                    let get_helper = ctx
                        .helper_refs
                        .get("graphix_valarray_get_array")
                        .ok_or_else(|| {
                            anyhow!("missing graphix_valarray_get_array")
                        })?;
                    let call = b.ins().call(get_helper, &[arr_ptr, i_now]);
                    let elem_ptr = b.inst_results(call)[0];
                    let elem_var = b.declare_var(types::I64);
                    b.def_var(elem_var, elem_ptr);
                    env.bind_composite(elem_local.clone(), elem_var);
                    Some(elem_var)
                }
            };
            let body_ptr = compile_scalar(b, body, env, ctx)?;
            // Body must be an owned array for `extend` to consume; a
            // Borrowed source (Local read) is refcount-cloned first.
            let body_ptr = ensure_owned_composite(b, ctx, body, body_ptr)?;
            if let Some(elem_var) = composite_elem {
                let drop_helper = ctx
                    .helper_refs
                    .get("graphix_valarray_drop")
                    .ok_or_else(|| anyhow!("missing graphix_valarray_drop"))?;
                let elem_now = b.use_var(elem_var);
                b.ins().call(drop_helper, &[elem_now]);
            }
            env.truncate(mark);
            b.ins().call(extend, &[buf, body_ptr]);
            let one = b.ins().iconst(types::I64, 1);
            let i_next = b.ins().iadd(i_now, one);
            b.def_var(i_var, i_next);
            b.ins().jump(loop_header, &[]);
            b.seal_block(loop_body);
            b.seal_block(loop_header);
            b.switch_to_block(loop_exit);
            b.seal_block(loop_exit);
            unregister_hof_buf(ctx);
            let call = b.ins().call(finalize, &[buf]);
            Ok(b.inst_results(call)[0])
        }
        GirOp::ArrayFold { array, elem_typ, init, acc_local, elem_local, body } => {
            // acc_var = compile init  (scalar, type = body.typ.as_prim())
            // loop i in 0..len:
            //   elem = get(arr, i)
            //   bind acc_local = acc_var, elem_local = elem
            //   new_acc = compile body
            //   def_var(acc_var, new_acc)
            // result = use_var(acc_var)
            let acc_prim = prim_of(&e.typ);
            let len_helper = ctx
                .helper_refs
                .get("graphix_valarray_len")
                .ok_or_else(|| anyhow!("missing graphix_valarray_len"))?;
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("GIR malformed: undefined fold array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let call = b.ins().call(len_helper, &[arr_ptr]);
            let len = b.inst_results(call)[0];
            let acc_var = b.declare_var(prim_to_clif(acc_prim));
            let init_val = compile_scalar(b, init, env, ctx)?;
            b.def_var(acc_var, init_val);
            let i_var = b.declare_var(types::I64);
            let zero = b.ins().iconst(types::I64, 0);
            b.def_var(i_var, zero);
            let loop_header = b.create_block();
            let loop_body = b.create_block();
            let loop_exit = b.create_block();
            b.ins().jump(loop_header, &[]);
            b.switch_to_block(loop_header);
            let i_cur = b.use_var(i_var);
            let cond = b.ins().icmp(IntCC::SignedLessThan, i_cur, len);
            b.ins().brif(cond, loop_body, &[], loop_exit, &[]);
            b.switch_to_block(loop_body);
            let i_now = b.use_var(i_var);
            // Bind acc and elem locals so the body's GirOp::Local
            // resolutions work. The order matches the interp:
            // acc first then elem (GIR construction pins this).
            let mark = env.mark();
            env.bind(acc_local.clone(), acc_var, acc_prim);
            // Element binding: scalar (locals) or composite (an owned
            // `*mut ValArray` in the composite slot, dropped after the
            // body) — the same split as `ArrayMap`.
            let elem_composite_var = match elem_typ.as_prim() {
                Some(prim) => {
                    let get_helper = ctx
                        .helper_refs
                        .get(valarray_get_helper(prim)?)
                        .ok_or_else(|| anyhow!("missing valarray_get helper"))?;
                    let call = b.ins().call(get_helper, &[arr_ptr, i_now]);
                    let elem_val = b.inst_results(call)[0];
                    let elem_var = b.declare_var(prim_to_clif(prim));
                    b.def_var(elem_var, elem_val);
                    env.bind(elem_local.clone(), elem_var, prim);
                    None
                }
                None => {
                    let get_helper = ctx
                        .helper_refs
                        .get("graphix_valarray_get_array")
                        .ok_or_else(|| {
                            anyhow!("missing graphix_valarray_get_array")
                        })?;
                    let call = b.ins().call(get_helper, &[arr_ptr, i_now]);
                    let elem_ptr = b.inst_results(call)[0];
                    let elem_var = b.declare_var(types::I64);
                    b.def_var(elem_var, elem_ptr);
                    env.bind_composite(elem_local.clone(), elem_var);
                    Some(elem_var)
                }
            };
            let new_acc = compile_scalar(b, body, env, ctx)?;
            if let Some(elem_var) = elem_composite_var {
                let drop_helper = ctx
                    .helper_refs
                    .get("graphix_valarray_drop")
                    .ok_or_else(|| anyhow!("missing graphix_valarray_drop"))?;
                let elem_now = b.use_var(elem_var);
                b.ins().call(drop_helper, &[elem_now]);
            }
            env.truncate(mark);
            b.def_var(acc_var, new_acc);
            let one = b.ins().iconst(types::I64, 1);
            let i_next = b.ins().iadd(i_now, one);
            b.def_var(i_var, i_next);
            b.ins().jump(loop_header, &[]);
            b.seal_block(loop_body);
            b.seal_block(loop_header);
            b.switch_to_block(loop_exit);
            b.seal_block(loop_exit);
            Ok(b.use_var(acc_var))
        }
        GirOp::VariantTagEq { name, expected_tag } => {
            // Read the variant local as `(disc, payload)`; the helper
            // takes them by value (three args incl. the expected ArcStr
            // ptr) and `mem::forget`s on the way out so the local
            // retains its ref.
            let helper = ctx
                .helper_refs
                .get("graphix_variant_tag_eq")
                .ok_or_else(|| anyhow!("missing graphix_variant_tag_eq"))?;
            let vv = env.lookup_variant(name).ok_or_else(|| {
                anyhow!("GIR malformed: undefined variant `{name}`")
            })?;
            let disc = b.use_var(vv.disc);
            let payload = b.use_var(vv.payload);
            let tag_ptr = ctx.strings.get(expected_tag) as i64;
            let tag_val = b.ins().iconst(types::I64, tag_ptr);
            let call = b.ins().call(helper, &[disc, payload, tag_val]);
            Ok(b.inst_results(call)[0])
        }
        GirOp::VariantPayload { name, payload_idx, elem_typ } => {
            let helper_name = variant_payload_helper(*elem_typ)?;
            let helper = ctx
                .helper_refs
                .get(helper_name)
                .ok_or_else(|| anyhow!("missing helper `{helper_name}`"))?;
            let vv = env.lookup_variant(name).ok_or_else(|| {
                anyhow!("GIR malformed: undefined variant `{name}`")
            })?;
            let disc = b.use_var(vv.disc);
            let payload = b.use_var(vv.payload);
            let idx_const =
                b.ins().iconst(types::I64, *payload_idx as i64);
            let call = b.ins().call(helper, &[disc, payload, idx_const]);
            Ok(b.inst_results(call)[0])
        }
        // `ArrayFindMap` is value-shape (Nullable<out>) and composite-
        // input-capable; not yet JIT-lowered (the interp handles it via
        // `fuse()` fallback).
        GirOp::ArrayFindMap { .. } => Err(anyhow!(
            "ArrayFindMap not yet JIT-lowered"
        )),
    }
}

/// Cast a u64 (typically the raw bits of a scalar primitive packed
/// via [`pack_reg_to_u64`] or returned from `graphix_dyncall`) to a
/// CLIF value of the target prim type. Integer truncations use
/// `ireduce`; floats route through a same-width integer then
/// `bitcast`.
fn cast_u64_to_prim(
    b: &mut FunctionBuilder,
    raw: ClifValue,
    p: PrimType,
) -> ClifValue {
    match p {
        PrimType::I64 | PrimType::U64 => raw,
        PrimType::I32 | PrimType::U32 => b.ins().ireduce(types::I32, raw),
        PrimType::I16 | PrimType::U16 => b.ins().ireduce(types::I16, raw),
        PrimType::I8 | PrimType::U8 | PrimType::Bool => {
            b.ins().ireduce(types::I8, raw)
        }
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
            MemFlags::new()
                .with_endianness(cranelift_codegen::ir::Endianness::Little),
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
#[derive(Debug, PartialEq, Eq)]
enum CompositeSource {
    /// Expression produces a fresh owned pointer — TupleNew,
    /// StructNew, ArrayInit, etc. Transfer to the slot as-is.
    Owned,
    /// Expression reads from an existing binding that already owns
    /// the pointer (typically `Local(name)`). If we move it into a
    /// slot whose old contents we then drop, the drop frees the
    /// shared underlying buffer. Caller must clone before transfer.
    Borrowed,
}

fn classify_composite_source(e: &GirExpr) -> CompositeSource {
    match &e.op {
        // Producer ops yield owned pointers.
        GirOp::TupleNew { .. }
        | GirOp::StructNew { .. }
        | GirOp::ArrayInit { .. }
        | GirOp::ArrayMap { .. }
        | GirOp::ArrayFilter { .. }
        | GirOp::ArrayFind { .. }
        | GirOp::ArrayFilterMap { .. }
        | GirOp::ArrayFindMap { .. }
        | GirOp::ArrayFlatMap { .. }
        | GirOp::BytesIndex { .. }
        | GirOp::MapRef { .. }
        | GirOp::ArraySlice { .. }
        // A `ConstValue` (datetime/duration/bytes/map literal) is lowered
        // via `graphix_value_clone_from_static`, which bumps the inner
        // Arc — a fresh owned ref. `ValueArith` (datetime/duration math)
        // returns an owned Value from `graphix_value_<op>`. The
        // value-shape `QopUnwrap` success path runs its result through
        // `ensure_owned_value`, so it too hands out an owned Value.
        | GirOp::ConstValue(_)
        | GirOp::ValueArith { .. }
        | GirOp::QopUnwrap { .. }
        | GirOp::VariantNew { .. } => CompositeSource::Owned,
        // A composite-return DynCall hands back an owned `*mut
        // ValArray` / `*mut Value` (`Box::into_raw` of a fresh box in
        // `dispatch_typed`). Treating it as Borrowed would
        // refcount-clone-then-leak the original.
        GirOp::DynCall { .. } => CompositeSource::Owned,
        // A composite / value-shape cross-kernel `GirOp::Call` return is
        // owned: the callee's return path hands back an owned `*mut
        // ValArray` / `(disc, payload)` Value (its body ran the result
        // through `ensure_owned_composite` / `ensure_owned_value`). Same
        // as a composite-return DynCall — treat as Owned so a consumer
        // doesn't clone-then-leak it.
        GirOp::Call { .. } => CompositeSource::Owned,
        // `GirOp::Block` and `GirOp::IfChain` both run their composite
        // result through `ensure_owned_composite` before handing it
        // out (the Block at its tail, the IfChain at each arm), so by
        // the time the value reaches a consumer it's always owned.
        GirOp::Block { .. } | GirOp::IfChain { .. } => {
            CompositeSource::Owned
        }
        // Composite / value-shape element reads (`a[i]`, `t.0`,
        // `s.field`) clone the slot into a fresh box / refcount-bumped
        // Value (`graphix_*_get_array` / `_get_value`), so the result
        // is owned. Treating it as Borrowed would clone-then-leak it.
        GirOp::ArrayGet { .. }
        | GirOp::TupleGet { .. }
        | GirOp::StructGet { .. } => CompositeSource::Owned,
        // Anything else (Local reads, etc.) we conservatively treat as
        // borrowed. False positives just cost one extra refcount bump
        // — never an unsoundness.
        _ => CompositeSource::Borrowed,
    }
}

/// One owned composite/value cross-kernel-call arg that must be
/// dropped after the call returns. A `GirOp::Call` passes its args
/// borrowed — the callee clones every composite/value param on entry
/// (`compile_into_function`). An Owned-source arg (a producer like
/// `TupleNew`, or a composite-return `Call`) therefore leaves the
/// caller still holding the original after the callee took its own
/// clone; without this drop the original leaks.
enum CallArgDrop {
    Composite(ClifValue),
    Value { disc: ClifValue, payload: ClifValue },
}

/// Compile the args of a `GirOp::Call` into the CLIF call-argument
/// list in the callee's kind-grouped ABI order (see
/// [`GirKernel::abi_params`]): all scalar args first, then composite
/// pointers (array, then tuple, then struct — one word each), then
/// value-shape args (variant, then nullable — two words each).
/// Returns the arg list plus the owned composite/value args to drop
/// after the call.
///
/// Args arrive in inputs order (formals then captures); emitting them
/// kind by kind preserves the within-kind encounter order, which
/// matches the callee's per-kind param vectors (both built by walking
/// the same input list), so the assembled order lines up slot-for-slot
/// with the signature `push_abi_params` produced.
fn compile_call_clif_args(
    b: &mut FunctionBuilder,
    fn_name: &ArcStr,
    args: &[GirExpr],
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<(Vec<ClifValue>, Vec<CallArgDrop>)> {
    for a in args {
        // String and bare value-shape (DateTime/Duration/Bytes) args
        // are valid kernel params (the callee declares string_params /
        // value_params), but the *calling* kernel's JIT arg emit for
        // them isn't wired yet — bail so the caller falls back to the
        // interpreter (which routes these correctly). Unit/Null have
        // no kernel-param shape at all. Listing every remaining
        // GirType keeps a future variant from silently dropping an arg.
        match &a.typ {
            GirType::Prim(_)
            | GirType::Array(_)
            | GirType::Tuple(_)
            | GirType::Struct(_)
            | GirType::Variant(_)
            | GirType::Nullable(_) => {}
            GirType::String
            | GirType::DateTime
            | GirType::Duration
            | GirType::Bytes | GirType::Map | GirType::Error
            | GirType::Unit
            | GirType::Null => {
                return Err(anyhow!(
                    "JIT GirOp::Call `{fn_name}` arg type {:?} not yet \
                     lowered on the calling side — falling back to interp",
                    a.typ
                ));
            }
        }
    }
    let mut clif_args: Vec<ClifValue> = Vec::with_capacity(args.len());
    let mut drops: Vec<CallArgDrop> = Vec::new();
    // Scalars first.
    for a in args.iter().filter(|a| matches!(a.typ, GirType::Prim(_))) {
        clif_args.push(compile_scalar(b, a, env, ctx)?);
    }
    // Composite pointers: array, then tuple, then struct.
    let composite = args
        .iter()
        .filter(|a| matches!(a.typ, GirType::Array(_)))
        .chain(args.iter().filter(|a| matches!(a.typ, GirType::Tuple(_))))
        .chain(args.iter().filter(|a| matches!(a.typ, GirType::Struct(_))));
    for a in composite {
        let ptr = compile_scalar(b, a, env, ctx)?;
        clif_args.push(ptr);
        if matches!(classify_composite_source(a), CompositeSource::Owned) {
            drops.push(CallArgDrop::Composite(ptr));
        }
    }
    // Value-shape: variant, then nullable — two words each.
    let value = args
        .iter()
        .filter(|a| matches!(a.typ, GirType::Variant(_)))
        .chain(args.iter().filter(|a| matches!(a.typ, GirType::Nullable(_))));
    for a in value {
        let (disc, payload) = compile_value_expr(b, a, env, ctx)?.value()?;
        clif_args.push(disc);
        clif_args.push(payload);
        if matches!(classify_composite_source(a), CompositeSource::Owned) {
            drops.push(CallArgDrop::Value { disc, payload });
        }
    }
    Ok((clif_args, drops))
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

/// Ensure a composite-typed expression's value is *owned* — safe to
/// hand into a binding, a control-flow merge, or a return slot whose
/// downstream code will drop it. `Borrowed` sources (e.g. a `Local`
/// read that aliases a binding the caller still owns) get a refcount
/// clone; `Owned` sources (producer ops, DynCall results, Block /
/// IfChain results) pass through unchanged. Scalar expressions pass
/// through unchanged.
///
/// This is the single ownership-discipline choke point every
/// composite hand-off site uses — `GirStmt::Let`, `GirStmt::Return`,
/// the `GirOp::Block` tail, and each `compile_ifchain` arm.
fn ensure_owned_composite(
    b: &mut FunctionBuilder,
    ctx: &LowerCtx,
    e: &GirExpr,
    v: ClifValue,
) -> Result<ClifValue> {
    // String SSA values are always owned at the point they appear —
    // ConstStr clones from interned, Concat finalizes (returns
    // owned), and `GirOp::Local` of a String slot calls
    // `graphix_arcstr_clone` (refcount bump) at the lookup site.
    // Pass them through unchanged.
    if matches!(e.typ, GirType::Prim(_) | GirType::Unit | GirType::String)
        || classify_composite_source(e) == CompositeSource::Owned
    {
        return Ok(v);
    }
    match &e.typ {
        GirType::Array(_) | GirType::Tuple(_) | GirType::Struct(_) => {
            let helper = ctx
                .helper_refs
                .get("graphix_valarray_clone")
                .ok_or_else(|| anyhow!("missing graphix_valarray_clone"))?;
            let call = b.ins().call(helper, &[v]);
            Ok(b.inst_results(call)[0])
        }
        // Value-shape (Variant / Nullable) sources use
        // `ensure_owned_value` instead — they're two CLIF values, not
        // one, so they can't flow through this single-ClifValue path.
        // Reaching here means the consumer mis-routed.
        GirType::Variant(_)
        | GirType::Nullable(_)
        | GirType::DateTime
        | GirType::Duration
        | GirType::Bytes | GirType::Map | GirType::Error => Err(anyhow!(
            "ensure_owned_composite reached for Value-shape type \
             `{:?}` — caller should use `ensure_owned_value` instead",
            e.typ
        )),
        GirType::Prim(_) | GirType::Unit | GirType::String => {
            unreachable!("guarded above")
        }
        // Bare `Null` is the singleton from `GirOp::ConstNull` — fusion
        // always widens to `Nullable<T>` before any binding / merge,
        // so a bare-`Null` reaching this clone path is malformed.
        GirType::Null => Err(anyhow!(
            "ensure_owned_composite reached for bare Null type — \
             should have widened to Nullable<T> at construction"
        )),
    }
}

/// Value-shape sibling of [`ensure_owned_composite`]. Borrowed-source
/// Value exprs (e.g. `GirOp::Local(name)` reading a Variant/Nullable
/// from `env.variants` / `env.nullables`) get refcount-bumped via
/// `graphix_value_clone` so the resulting `(disc, payload)` pair is
/// owned and safe to hand off into a binding, merge, return slot,
/// or DynCall arg push that will drop it. Owned sources (producers
/// like `VariantNew`, `ConstNull`, IfChain merges via this very
/// helper at each arm) pass through unchanged.
fn ensure_owned_value(
    b: &mut FunctionBuilder,
    ctx: &LowerCtx,
    e: &GirExpr,
    cv: CompiledExpr,
) -> Result<(ClifValue, ClifValue)> {
    let (disc, payload) = cv.value()?;
    if classify_composite_source(e) == CompositeSource::Owned {
        return Ok((disc, payload));
    }
    let helper = ctx
        .helper_refs
        .get("graphix_value_clone")
        .ok_or_else(|| anyhow!("missing graphix_value_clone"))?;
    let call = b.ins().call(helper, &[disc, payload]);
    let results = b.inst_results(call);
    Ok((results[0], results[1]))
}

/// Compile a `ValueArith` / `ValueEq` operand as an OWNED `(disc,
/// payload)` Value — the helper consumes both operands. A value-shape
/// operand clones a Borrowed Local read via `ensure_owned_value`; a
/// scalar is promoted to a fresh `Value::<prim>`; a String/composite
/// (only reachable from `ValueEq`) is wrapped into a `Value::String` /
/// `Value::Array` via the boxing helpers, refcount-cloning a Borrowed
/// source first so the helper's consume doesn't free the caller's local.
fn compile_owned_value_operand(
    b: &mut FunctionBuilder,
    e: &GirExpr,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<(ClifValue, ClifValue)> {
    use cranelift_codegen::ir::types;
    match &e.typ {
        GirType::Variant(_)
        | GirType::Nullable(_)
        | GirType::DateTime
        | GirType::Duration
        | GirType::Bytes
        | GirType::Map => {
            let cv = compile_expr(b, e, env, ctx)?;
            ensure_owned_value(b, ctx, e, cv)
        }
        GirType::Prim(p) => {
            let s = compile_scalar(b, e, env, ctx)?;
            let disc = b.ins().iconst(types::I64, prim_to_value_disc(*p));
            let payload = scalar_to_payload_i64(b, *p, s);
            Ok((disc, payload))
        }
        GirType::String => {
            // ConstStr/Concat/Local-read all produce an owned ArcStr;
            // `graphix_value_new_string` consumes it into a Value.
            let s = compile_scalar(b, e, env, ctx)?;
            let helper = ctx
                .helper_refs
                .get("graphix_value_new_string")
                .ok_or_else(|| anyhow!("missing graphix_value_new_string"))?;
            let call = b.ins().call(helper, &[s]);
            let d = b.inst_results(call)[0];
            let p = b.inst_results(call)[1];
            Ok((d, p))
        }
        GirType::Array(_) | GirType::Tuple(_) | GirType::Struct(_) => {
            let cv = compile_expr(b, e, env, ctx)?;
            let ptr = cv.single()?;
            let ptr = ensure_owned_composite(b, ctx, e, ptr)?;
            let helper = ctx
                .helper_refs
                .get("graphix_value_new_from_array")
                .ok_or_else(|| {
                    anyhow!("missing graphix_value_new_from_array")
                })?;
            let call = b.ins().call(helper, &[ptr]);
            let d = b.inst_results(call)[0];
            let p = b.inst_results(call)[1];
            Ok((d, p))
        }
        other => Err(anyhow!(
            "value operand has unexpected type {other:?}"
        )),
    }
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
    for (_, var) in &env.composites {
        let ptr = b.use_var(*var);
        b.ins().call(arr_drop, &[ptr]);
    }
    // Variant params + locals are owned (params are refcount-cloned
    // on kernel entry via `graphix_value_clone`, locals come from
    // `VariantNew` or composite-return DynCall). Drop them via the
    // two-register `graphix_value_drop(v: Value)` ABI — passes the
    // `(disc, payload)` pair and the helper consumes (drops) it.
    for (_, vv) in &env.variants {
        let disc = b.use_var(vv.disc);
        let payload = b.use_var(vv.payload);
        b.ins().call(val_drop, &[disc, payload]);
    }
    // Nullables: same two-register drop discipline as variants.
    for (_, vv) in &env.nullables {
        let disc = b.use_var(vv.disc);
        let payload = b.use_var(vv.payload);
        b.ins().call(val_drop, &[disc, payload]);
    }
    // String locals: refcount decrement.
    let str_drop = ctx
        .helper_refs
        .get("graphix_arcstr_drop")
        .ok_or_else(|| anyhow!("missing graphix_arcstr_drop"))?;
    for (_, var) in &env.strings {
        let ptr = b.use_var(*var);
        b.ins().call(str_drop, &[ptr]);
    }
    Ok(())
}

/// Register an in-progress HOF output buf (`graphix_value_buf_new`) for
/// pending cleanup: declare it as a CLIF Variable and push onto
/// `ctx.dyncall_buf_stack`, so a value-shape/composite DynCall or `?`
/// (QopUnwrap) that pends inside the loop body drops it from its
/// `pre_pending` block via [`emit_pending_cleanup`]. Mirrors the
/// DynCall arg-buf registration in [`marshal_dyncall_args`]. Pair with
/// [`unregister_hof_buf`] before `finalize` on the normal (non-pending)
/// path — `finalize` consumes the buf there, so the runtime drop happens
/// exactly once (cleanup on pend, finalize otherwise).
fn register_hof_buf(b: &mut FunctionBuilder, ctx: &LowerCtx, buf: ClifValue) {
    let buf_var = b.declare_var(types::I64);
    b.def_var(buf_var, buf);
    ctx.dyncall_buf_stack.borrow_mut().push(buf_var);
}

/// Pop the HOF output buf registered by [`register_hof_buf`]. Called on
/// the normal path just before `finalize` consumes the buf.
fn unregister_hof_buf(ctx: &LowerCtx) {
    ctx.dyncall_buf_stack.borrow_mut().pop();
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
    // Owned composite + variant locals (and entry-cloned params).
    drop_owned_composites(b, env, ctx)
}

/// Shape of the about-to-return result that
/// [`emit_return_pending_check`] drops if `DYNCALL_PENDING` fires.
/// Scalar / Unit returns don't need this — there's no allocation,
/// so no leak risk — and so don't have a `ReturnDropShape` variant.
#[derive(Debug, Clone, Copy)]
enum ReturnDropShape {
    /// Owned `*mut ValArray` — drop via `graphix_valarray_drop`.
    Composite(ClifValue),
    /// Owned Value-shape `(disc, payload)` pair — drop via
    /// `graphix_value_drop`.
    Value { disc: ClifValue, payload: ClifValue },
    /// Owned `ArcStr` (thin pointer as `i64`) — drop via
    /// `graphix_arcstr_drop`. String returns are leak-prone on the
    /// pending path the same way composite/Value returns are: every
    /// `GirOp::ConstStr` / `Concat` / `Local`-read produces an owned
    /// ArcStr whose refcount would never decrement if
    /// `GirNode::update` discards the wrapper result.
    String(ClifValue),
}

/// At every composite / Value-shape `GirStmt::Return`, emit a
/// `DYNCALL_PENDING` peek. If pending fired earlier (e.g., a scalar
/// `GirOp::DynCall` deep in the body that doesn't short-circuit on
/// its own), the about-to-return result is an owned heap allocation
/// that `GirNode::update`'s wrapper-level pending check would
/// discard — leaking it. On pending: drop the result, run
/// `emit_pending_cleanup` to drop env / in-flight DynCall bufs,
/// jump to `pending_exit` (which emits a sentinel and returns).
/// `GirNode::update` then sees `DYNCALL_PENDING` (still set —
/// `pending_take` no longer clears) and returns `None`. On the
/// non-pending fall-through, control returns to the caller, which
/// emits its own `drop_owned_composites + return`.
fn emit_return_pending_check(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
    drop_shape: ReturnDropShape,
) -> Result<()> {
    let pending_take = ctx
        .helper_refs
        .get("graphix_dyncall_pending_take")
        .ok_or_else(|| anyhow!("missing graphix_dyncall_pending_take"))?;
    let call = b.ins().call(pending_take, &[]);
    let pending = b.inst_results(call)[0];

    let drop_block = b.create_block();
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
    b.ins().brif(pending, drop_block, &[], continue_block, &[]);

    // drop_block: drop the about-to-return result, then run the
    // normal pre_pending cleanup (env drops + in-flight DynCall
    // bufs), then jump to pending_exit.
    b.switch_to_block(drop_block);
    b.seal_block(drop_block);
    match drop_shape {
        ReturnDropShape::Composite(ptr) => {
            let arr_drop = ctx
                .helper_refs
                .get("graphix_valarray_drop")
                .ok_or_else(|| anyhow!("missing graphix_valarray_drop"))?;
            b.ins().call(arr_drop, &[ptr]);
        }
        ReturnDropShape::Value { disc, payload } => {
            let val_drop = ctx
                .helper_refs
                .get("graphix_value_drop")
                .ok_or_else(|| anyhow!("missing graphix_value_drop"))?;
            b.ins().call(val_drop, &[disc, payload]);
        }
        ReturnDropShape::String(ptr) => {
            let str_drop = ctx
                .helper_refs
                .get("graphix_arcstr_drop")
                .ok_or_else(|| anyhow!("missing graphix_arcstr_drop"))?;
            b.ins().call(str_drop, &[ptr]);
        }
    }
    emit_pending_cleanup(b, env, ctx)?;
    b.ins().jump(pending_exit, &[]);

    // continue: caller proceeds with the non-pending return path.
    b.switch_to_block(continue_block);
    b.seal_block(continue_block);
    Ok(())
}

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

/// Compile a producer-op field expression and emit the appropriate
/// `graphix_value_buf_push_*` call into `buf`. Centralises the
/// GirType-shape dispatch used by `GirOp::TupleNew`, `StructNew`,
/// and `VariantNew` for nested composite fields (mirrors what
/// `GirOp::DynCall`'s arg-marshalling already does for HOF dispatch).
///
/// Each shape's helper choice:
/// - **Prim**: `graphix_value_buf_push_<T>` per the prim.
/// - **Array/Tuple/Struct**: `graphix_value_buf_push_array` (owned)
///   or `_borrowed` (refcount-bumped) by [`classify_composite_source`].
/// - **Variant/Nullable**: `graphix_value_buf_push_value` or
///   `_borrowed`, picked the same way.
/// - **String**: `graphix_value_buf_push_arcstr`. String SSA values
///   are always owned (per `ensure_owned_composite`'s String pass-
///   through), so the push helper transfers ownership.
/// - **Unit/Null**: invalid as a field — caller should have rejected
///   in `emit_*_new`.
fn compile_and_push_field(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
    buf: ClifValue,
    field: &GirExpr,
) -> Result<()> {
    let helper_name: &str = match &field.typ {
        GirType::Prim(p) => value_buf_push_helper(*p)?,
        GirType::Array(_) | GirType::Tuple(_) | GirType::Struct(_) => {
            match classify_composite_source(field) {
                CompositeSource::Owned => "graphix_value_buf_push_array",
                CompositeSource::Borrowed => {
                    "graphix_value_buf_push_array_borrowed"
                }
            }
        }
        GirType::Variant(_)
        | GirType::Nullable(_)
        | GirType::DateTime
        | GirType::Duration
        | GirType::Bytes | GirType::Map | GirType::Error => {
            match classify_composite_source(field) {
                CompositeSource::Owned => "graphix_value_buf_push_value",
                CompositeSource::Borrowed => {
                    "graphix_value_buf_push_value_borrowed"
                }
            }
        }
        // String SSA is the ArcStr's raw thin-pointer bits (owned),
        // NOT a pointer-to-an-ArcStr struct. `_push_arcstr` takes a
        // `*const ArcStr` and dereferences it — using it here treats
        // the ArcStr's bits as a pointer to its own struct (UAF/UB).
        // `_push_string` takes ArcStr by value (consumes), which is
        // what we want for an owned String SSA field.
        GirType::String => "graphix_value_buf_push_string",
        GirType::Unit => {
            return Err(anyhow!(
                "producer-op field has Unit type — emit_*_new should reject"
            ));
        }
        GirType::Null => {
            return Err(anyhow!(
                "producer-op field has bare Null type — should widen to Nullable<T>"
            ));
        }
    };
    let push = ctx
        .helper_refs
        .get(helper_name)
        .ok_or_else(|| anyhow!("missing {helper_name}"))?;
    // Value-shape fields (Variant/Nullable/DateTime/Duration/Bytes/Map)
    // need both `(disc, payload)` registers from the typed Value ABI;
    // everything else is a single ClifValue. This dispatch MUST match the
    // helper-selection above — both key on the full value-shape set
    // (`is_value_shape()`). When it only listed Variant|Nullable, a
    // DateTime/Duration/Bytes/Map field fell to the `_` arm and
    // `.single()` Err'd, silently de-fusing the whole kernel.
    if field.typ.is_value_shape() {
        let cv = compile_expr(b, field, env, ctx)?;
        let (disc, payload) = match cv {
            CompiledExpr::Value { disc, payload } => (disc, payload),
            CompiledExpr::Single(_) => {
                return Err(anyhow!(
                    "Value-shape field compiled to Single — \
                     compile_expr dispatch is broken"
                ));
            }
        };
        b.ins().call(push, &[buf, disc, payload]);
    } else {
        let v = compile_scalar(b, field, env, ctx)?;
        b.ins().call(push, &[buf, v]);
    }
    Ok(())
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

/// Map an element [`GirType`] to its element-read helper symbol —
/// primitive (`get_<prim>`), String (`get_arcstr`), composite
/// (`get_array`, a `*mut ValArray`), or value-shape (`get_value`, a
/// two-word `Value`). `struct_access` picks the `struct_get_*` (two-
/// level kv-pair read) family over the flat `valarray_get_*` family.
fn element_read_helper(
    elem: &GirType,
    struct_access: bool,
) -> Result<&'static str> {
    Ok(match elem {
        GirType::Prim(p) => {
            if struct_access {
                struct_get_helper(*p)?
            } else {
                valarray_get_helper(*p)?
            }
        }
        GirType::String => {
            if struct_access {
                "graphix_struct_get_arcstr"
            } else {
                "graphix_valarray_get_arcstr"
            }
        }
        GirType::Array(_) | GirType::Tuple(_) | GirType::Struct(_) => {
            if struct_access {
                "graphix_struct_get_array"
            } else {
                "graphix_valarray_get_array"
            }
        }
        GirType::Variant(_)
        | GirType::Nullable(_)
        | GirType::DateTime
        | GirType::Duration
        | GirType::Bytes | GirType::Map | GirType::Error => {
            if struct_access {
                "graphix_struct_get_value"
            } else {
                "graphix_valarray_get_value"
            }
        }
        GirType::Unit | GirType::Null => {
            return Err(anyhow!(
                "element read of Unit/Null-typed slot — GIR is malformed"
            ));
        }
    })
}

/// Emit an element read: `arr_ptr[idx]` (or struct field) of the given
/// element `GirType`, dispatching to the right `..._get_*` helper. The
/// result is OWNED (fresh box / refcount-bumped clone). Returns
/// `CompiledExpr::Value` for a value-shape element (two-register
/// Value) and `CompiledExpr::Single` for scalar / string / composite-
/// pointer elements — so the same routine serves both the scalar arm
/// (`.single()`) and the value-shape arm of `compile_expr`.
fn compile_element_read(
    b: &mut FunctionBuilder,
    arr_ptr: ClifValue,
    idx_val: ClifValue,
    elem: &GirType,
    struct_access: bool,
    ctx: &LowerCtx,
) -> Result<CompiledExpr> {
    let helper_name = element_read_helper(elem, struct_access)?;
    let helper = ctx
        .helper_refs
        .get(helper_name)
        .ok_or_else(|| anyhow!("missing JIT helper `{helper_name}`"))?;
    let call = b.ins().call(helper, &[arr_ptr, idx_val]);
    let r = b.inst_results(call);
    if elem.is_value_shape() {
        Ok(CompiledExpr::Value { disc: r[0], payload: r[1] })
    } else {
        Ok(CompiledExpr::Single(r[0]))
    }
}

/// Widen a CLIF value to i64. Helpers expect a usize index; if the
/// caller's index expression was narrower (e.g. `i32` from a
/// `cast`), we zero/sign extend here.
fn widen_to_i64(
    b: &mut FunctionBuilder,
    v: ClifValue,
    p: PrimType,
) -> ClifValue {
    match p {
        PrimType::I64 | PrimType::U64 => v,
        PrimType::I8 | PrimType::I16 | PrimType::I32 => {
            b.ins().sextend(types::I64, v)
        }
        PrimType::U8 | PrimType::U16 | PrimType::U32 | PrimType::Bool => {
            b.ins().uextend(types::I64, v)
        }
        PrimType::F32 | PrimType::F64 => {
            panic!("widen_to_i64: float index — GIR malformed")
        }
    }
}

fn compile_ifchain(
    b: &mut FunctionBuilder,
    arms: &[(Option<GirExpr>, GirExpr)],
    result_typ: &GirType,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<CompiledExpr> {
    if arms.is_empty() {
        return Err(anyhow!("GIR malformed: empty if-chain"));
    }
    // Merge block holds the result via block parameters. For a
    // scalar / composite-pointer result, that's a single I64 (or
    // the prim's CLIF type). For a Value-shape result (Variant /
    // Nullable) it's two I64s (disc, payload). Every arm normalizes
    // its result to the merge shape before jumping.
    let is_value_shape =
        matches!(result_typ, GirType::Variant(_) | GirType::Nullable(_));
    let merge = b.create_block();
    if is_value_shape {
        b.append_block_param(merge, types::I64); // disc
        b.append_block_param(merge, types::I64); // payload
    } else {
        b.append_block_param(merge, clif_of(result_typ));
    }

    for (i, (cond, body)) in arms.iter().enumerate() {
        let is_last = i == arms.len() - 1;
        let body_block = b.create_block();
        let next_block: Option<Block> = match cond {
            None => {
                b.ins().jump(body_block, &[]);
                None
            }
            Some(c) => {
                let cv = compile_scalar(b, c, env, ctx)?;
                if is_last {
                    let trap_block = b.create_block();
                    b.ins().brif(cv, body_block, &[], trap_block, &[]);
                    b.switch_to_block(trap_block);
                    b.ins().trap(
                        cranelift_codegen::ir::TrapCode::user(2).unwrap(),
                    );
                    b.seal_block(trap_block);
                    None
                } else {
                    let n = b.create_block();
                    b.ins().brif(cv, body_block, &[], n, &[]);
                    Some(n)
                }
            }
        };
        b.switch_to_block(body_block);
        b.seal_block(body_block);
        let mark = env.mark();
        if is_value_shape {
            // Compile the arm body — may produce a CLIF scalar
            // (narrow arm widening to Nullable<T>), a Null, or a
            // Value-shape (Variant/Nullable). Normalize all three
            // to the merge's (disc, payload) param pair.
            let cv = compile_expr(b, body, env, ctx)?;
            let (disc, payload) =
                widen_arm_to_value(b, ctx, body, cv, result_typ)?;
            env.truncate(mark);
            b.ins().jump(
                merge,
                &[BlockArg::Value(disc), BlockArg::Value(payload)],
            );
        } else {
            let v = compile_scalar(b, body, env, ctx)?;
            let v = ensure_owned_composite(b, ctx, body, v)?;
            env.truncate(mark);
            b.ins().jump(merge, &[BlockArg::Value(v)]);
        }
        match next_block {
            Some(next) => {
                b.switch_to_block(next);
                b.seal_block(next);
            }
            None => break,
        }
    }
    b.switch_to_block(merge);
    b.seal_block(merge);
    if is_value_shape {
        let params = b.block_params(merge);
        let (disc, payload) = (params[0], params[1]);
        Ok(CompiledExpr::Value { disc, payload })
    } else {
        Ok(CompiledExpr::Single(b.block_params(merge)[0]))
    }
}

/// Widen an IfChain arm result to the merge's `(disc, payload)`
/// Value shape. Handles three input cases:
///   * `GirType::Null` — pack `(NULL_DISC, 0)` inline.
///   * `GirType::Prim(T)` — pack `(prim_to_value_disc(T), scalar)`
///     inline; floats bitcast to I64 first.
///   * `GirType::Variant(_)` / `GirType::Nullable(_)` — result is
///     already the right shape; refcount-bump borrowed sources via
///     `ensure_owned_value` so the merge owns a fresh ref.
fn widen_arm_to_value(
    b: &mut FunctionBuilder,
    ctx: &LowerCtx,
    body: &GirExpr,
    cv: CompiledExpr,
    _result_typ: &GirType,
) -> Result<(ClifValue, ClifValue)> {
    match (&body.typ, cv) {
        (GirType::Variant(_), _) | (GirType::Nullable(_), _) => {
            ensure_owned_value(b, ctx, body, cv)
        }
        (GirType::Null, _) => {
            let disc = b.ins().iconst(types::I64, value_disc::NULL);
            let payload = b.ins().iconst(types::I64, 0);
            Ok((disc, payload))
        }
        (GirType::Prim(p), CompiledExpr::Single(v)) => {
            let disc =
                b.ins().iconst(types::I64, prim_to_value_disc(*p));
            // Promote/bitcast the scalar to the I64 payload slot.
            let payload = scalar_to_payload_i64(b, *p, v);
            Ok((disc, payload))
        }
        (other, _) => Err(anyhow!(
            "widen_arm_to_value: source type {other:?} can't widen \
             to Value shape — fusion should have rejected this arm"
        )),
    }
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
        PrimType::I8 | PrimType::U8 | PrimType::Bool => {
            b.ins().uextend(types::I64, v)
        }
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

/// `GirOp::Concat` codegen — build a heap-owned `*mut String`, push
/// each part (formatted-Display for prims, append-as-str for ArcStr
/// children), and finalize into an owned ArcStr (single `i64` CLIF
/// value: the ArcStr's raw pointer).
///
/// Ownership of the buf is linear within this helper: `buf_new`
/// produces it, each push borrows it, `buf_finalize` consumes it.
/// No pending path exists *inside* Concat today (Concat's parts are
/// all scalar/string, no DynCall mid-Concat), so no special pending
/// cleanup of the buf is needed. If that changes, the buf would
/// need to join `dyncall_buf_stack`.
fn compile_concat(
    b: &mut FunctionBuilder,
    parts: &[GirExpr],
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<ClifValue> {
    let new_buf = ctx
        .helper_refs
        .get("graphix_string_buf_new")
        .ok_or_else(|| anyhow!("missing graphix_string_buf_new"))?;
    let call = b.ins().call(new_buf, &[]);
    let buf = b.inst_results(call)[0];
    for part in parts {
        match &part.typ {
            GirType::String => {
                let s = compile_scalar(b, part, env, ctx)?;
                let push = ctx
                    .helper_refs
                    .get("graphix_string_buf_push_arcstr")
                    .ok_or_else(|| {
                        anyhow!("missing graphix_string_buf_push_arcstr")
                    })?;
                b.ins().call(push, &[buf, s]);
            }
            GirType::Prim(p) => {
                let v = compile_scalar(b, part, env, ctx)?;
                let helper_name = match p {
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
                };
                let push = ctx.helper_refs.get(helper_name).ok_or_else(
                    || anyhow!("missing {helper_name}"),
                )?;
                b.ins().call(push, &[buf, v]);
            }
            _ => {
                return Err(anyhow!(
                    "Concat part has unexpected type {:?} — only \
                     String and Prim are accepted",
                    part.typ
                ));
            }
        }
    }
    let finalize = ctx
        .helper_refs
        .get("graphix_string_buf_finalize")
        .ok_or_else(|| anyhow!("missing graphix_string_buf_finalize"))?;
    let call = b.ins().call(finalize, &[buf]);
    Ok(b.inst_results(call)[0])
}

fn compile_const(b: &mut FunctionBuilder, c: ConstVal) -> ClifValue {
    match c {
        ConstVal::I8(x) => b.ins().iconst(types::I8, x as i64),
        ConstVal::I16(x) => b.ins().iconst(types::I16, x as i64),
        ConstVal::I32(x) => b.ins().iconst(types::I32, x as i64),
        ConstVal::I64(x) => b.ins().iconst(types::I64, x),
        ConstVal::U8(x) => b.ins().iconst(types::I8, x as i64),
        ConstVal::U16(x) => b.ins().iconst(types::I16, x as i64),
        ConstVal::U32(x) => b.ins().iconst(types::I32, x as i64),
        ConstVal::U64(x) => b.ins().iconst(types::I64, x as i64),
        ConstVal::F32(x) => b.ins().f32const(x),
        ConstVal::F64(x) => b.ins().f64const(x),
        ConstVal::Bool(true) => b.ins().iconst(types::I8, 1),
        ConstVal::Bool(false) => b.ins().iconst(types::I8, 0),
    }
}

/// A zero / false constant of the given prim type. Used for the
/// `pending_exit` block's sentinel return value (never observed —
/// `GirNode::update` discards the result on the pending path — but
/// CLIF needs a well-typed value of the right width).
fn zero_const(b: &mut FunctionBuilder, p: PrimType) -> ClifValue {
    match p {
        PrimType::I8 | PrimType::U8 | PrimType::Bool => {
            b.ins().iconst(types::I8, 0)
        }
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
        // bool ↔ integer/float — gir::cast refuses these at
        // construction; reaching this branch means something built a
        // malformed GirOp::Cast bypassing the constructor.
        unreachable!("compile_cast: bool casts should be rejected at IR construction");
    }
}

// ─── Type plumbing ───────────────────────────────────────────────

/// JIT v1 is scalar-only. Every `GirExpr.typ` we encounter inside a
/// JIT-compilable kernel must be a primitive — array ops route to the
/// interpreter via the explicit error arm in `compile_expr`. This
/// helper crashes loudly on misuse rather than silently producing
/// nonsense lowering.
fn prim_of(t: &GirType) -> PrimType {
    t.as_prim().expect("JIT scalar-only: array typ slipped past compile_expr")
}

/// CLIF type of a [`GirType`] at the JIT ABI level. Primitives map to
/// their natural CLIF type; every composite (array/tuple/struct/
/// variant) is a pointer, i.e. `I64` on a 64-bit target.
fn clif_of(t: &GirType) -> ClifType {
    match t {
        GirType::Prim(p) => prim_to_clif(*p),
        GirType::Array(_)
        | GirType::Tuple(_)
        | GirType::Struct(_)
        | GirType::Variant(_) => types::I64,
        // datetime/duration are Value-shape (two registers at the
        // boundary); the single-type defensive ABI is a pointer slot,
        // same as Nullable below.
        GirType::DateTime | GirType::Duration | GirType::Bytes | GirType::Map | GirType::Error => types::I64,
        // Unit at the ABI is just a zero pointer slot — see
        // `define_typed_kernel`'s return_clif handling.
        GirType::Unit => types::I64,
        // String should never reach JIT (guarded by
        // `kernel_contains_string`); ABI it as a pointer to keep this
        // helper total in case `clif_of` is called defensively.
        GirType::String => types::I64,
        // Null/Nullable should never reach JIT either; same
        // defensive pointer ABI as String.
        GirType::Null | GirType::Nullable(_) => types::I64,
    }
}

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gir::{
        arith, bool_op, cast, cmp, const_expr, local, BinOp, BoolOp, CmpOp,
        ConstVal, Input, GirExpr, GirKernel, GirOp, GirStmt, GirType, Let,
        PrimType, SelectArm,
    };

    fn input(name: &str, prim: PrimType) -> Input {
        Input {
            name: ArcStr::from(name),
            prim,
            bind_id: None,
        }
    }

    fn loc(name: &str, prim: PrimType) -> GirExpr {
        local(ArcStr::from(name), prim)
    }

    /// Compile a kernel and return the function pointer typed as a
    /// transmute-ready raw pointer. Tests cast to the appropriate
    /// extern "C" fn signature.
    fn jit(kernel: &GirKernel) -> (JitCtx, *const u8) {
        let mut ctx = JitCtx::new().expect("JitCtx::new");
        let compiled = compile_kernel(&mut ctx, kernel).expect("compile_kernel");
        let p = compiled.fn_ptr;
        (ctx, p)
    }

    #[test]
    fn arith_i64() {
        // |a, b, c| a + b * c
        let body = arith(
            loc("a", PrimType::I64),
            arith(
                loc("b", PrimType::I64),
                loc("c", PrimType::I64),
                BinOp::Mul,
            )
            .unwrap(),
            BinOp::Add,
        )
        .unwrap();
        let kernel = GirKernel {
            fn_name: ArcStr::from("ax_plus_bc"),
            params: vec![
                input("a", PrimType::I64),
                input("b", PrimType::I64),
                input("c", PrimType::I64),
            ],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![GirStmt::Return(body)],
        };
        let (_ctx, p) = jit(&kernel);
        let f: extern "C" fn(i64, i64, i64) -> i64 =
            unsafe { std::mem::transmute(p) };
        assert_eq!(f(2, 3, 4), 2 + 3 * 4);
        assert_eq!(f(-1, 100, 7), -1 + 100 * 7);
    }

    #[test]
    fn arith_f64_block() {
        // |a, b| { let c = a + b; c * 2.0 }
        let block = GirExpr {
            op: GirOp::Block {
                lets: vec![Let {
                    local: ArcStr::from("c"),
                    value: arith(
                        loc("a", PrimType::F64),
                        loc("b", PrimType::F64),
                        BinOp::Add,
                    )
                    .unwrap(),
                }],
                tail: Box::new(
                    arith(
                        loc("c", PrimType::F64),
                        const_expr(ConstVal::F64(2.0)),
                        BinOp::Mul,
                    )
                    .unwrap(),
                ),
            },
            typ: GirType::Prim(PrimType::F64),
        };
        let kernel = GirKernel {
            fn_name: ArcStr::from("scaled"),
            params: vec![input("a", PrimType::F64), input("b", PrimType::F64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::Prim(PrimType::F64),
            has_tail_loop: false,
            body: vec![GirStmt::Return(block)],
        };
        let (_ctx, p) = jit(&kernel);
        let f: extern "C" fn(f64, f64) -> f64 = unsafe { std::mem::transmute(p) };
        assert_eq!(f(3.0, 5.0), 16.0);
    }

    #[test]
    fn bool_and_cmp() {
        // |x, y| (x > 0.0) && (y < 10.0)
        let body = bool_op(
            cmp(
                loc("x", PrimType::F64),
                const_expr(ConstVal::F64(0.0)),
                CmpOp::Gt,
            )
            .unwrap(),
            cmp(
                loc("y", PrimType::F64),
                const_expr(ConstVal::F64(10.0)),
                CmpOp::Lt,
            )
            .unwrap(),
            BoolOp::And,
        )
        .unwrap();
        let kernel = GirKernel {
            fn_name: ArcStr::from("range_check"),
            params: vec![input("x", PrimType::F64), input("y", PrimType::F64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::Prim(PrimType::Bool),
            has_tail_loop: false,
            body: vec![GirStmt::Return(body)],
        };
        let (_ctx, p) = jit(&kernel);
        let f: extern "C" fn(f64, f64) -> u8 = unsafe { std::mem::transmute(p) };
        assert_eq!(f(5.0, 7.0), 1);
        assert_eq!(f(-1.0, 7.0), 0);
        assert_eq!(f(5.0, 20.0), 0);
    }

    #[test]
    fn cast_int_to_float() {
        // |i| cast<f64>(i)
        let body = cast(loc("i", PrimType::I64), PrimType::F64).unwrap();
        let kernel = GirKernel {
            fn_name: ArcStr::from("itof"),
            params: vec![input("i", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::Prim(PrimType::F64),
            has_tail_loop: false,
            body: vec![GirStmt::Return(body)],
        };
        let (_ctx, p) = jit(&kernel);
        let f: extern "C" fn(i64) -> f64 = unsafe { std::mem::transmute(p) };
        assert_eq!(f(42), 42.0);
        assert_eq!(f(-7), -7.0);
    }

    /// Hand-build mandelbrot iterate, same as the interpreter test,
    /// then JIT it and verify expected outputs.
    fn mandelbrot_iterate_kernel() -> GirKernel {
        let arm0 = SelectArm {
            cond: Some(
                cmp(
                    loc("i", PrimType::I64),
                    const_expr(ConstVal::I64(0)),
                    CmpOp::Eq,
                )
                .unwrap(),
            ),
            body: vec![GirStmt::Return(const_expr(ConstVal::I64(0)))],
        };
        let escaped_cond = cmp(
            arith(
                arith(
                    loc("zr", PrimType::F64),
                    loc("zr", PrimType::F64),
                    BinOp::Mul,
                )
                .unwrap(),
                arith(
                    loc("zi", PrimType::F64),
                    loc("zi", PrimType::F64),
                    BinOp::Mul,
                )
                .unwrap(),
                BinOp::Add,
            )
            .unwrap(),
            const_expr(ConstVal::F64(4.0)),
            CmpOp::Gt,
        )
        .unwrap();
        let arm1 = SelectArm {
            cond: Some(escaped_cond),
            body: vec![GirStmt::Return(loc("i", PrimType::I64))],
        };
        let new_zr = arith(
            arith(
                arith(
                    loc("zr", PrimType::F64),
                    loc("zr", PrimType::F64),
                    BinOp::Mul,
                )
                .unwrap(),
                arith(
                    loc("zi", PrimType::F64),
                    loc("zi", PrimType::F64),
                    BinOp::Mul,
                )
                .unwrap(),
                BinOp::Sub,
            )
            .unwrap(),
            loc("cr", PrimType::F64),
            BinOp::Add,
        )
        .unwrap();
        let new_zi = arith(
            arith(
                arith(
                    const_expr(ConstVal::F64(2.0)),
                    loc("zr", PrimType::F64),
                    BinOp::Mul,
                )
                .unwrap(),
                loc("zi", PrimType::F64),
                BinOp::Mul,
            )
            .unwrap(),
            loc("ci", PrimType::F64),
            BinOp::Add,
        )
        .unwrap();
        let new_i = arith(
            loc("i", PrimType::I64),
            const_expr(ConstVal::I64(1)),
            BinOp::Sub,
        )
        .unwrap();
        let arm2 = SelectArm {
            cond: None,
            body: vec![GirStmt::TailCall {
                args: vec![
                    new_zr,
                    new_zi,
                    loc("cr", PrimType::F64),
                    loc("ci", PrimType::F64),
                    new_i,
                ],
            }],
        };
        GirKernel {
            fn_name: ArcStr::from("iterate"),
            params: vec![
                input("zr", PrimType::F64),
                input("zi", PrimType::F64),
                input("cr", PrimType::F64),
                input("ci", PrimType::F64),
                input("i", PrimType::I64),
            ],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::Prim(PrimType::I64),
            has_tail_loop: true,
            body: vec![GirStmt::Select { arms: vec![arm0, arm1, arm2] }],
        }
    }

    #[test]
    fn jit_mandelbrot_iterate() {
        let kernel = mandelbrot_iterate_kernel();
        let (_ctx, p) = jit(&kernel);
        let f: extern "C" fn(f64, f64, f64, f64, i64) -> i64 =
            unsafe { std::mem::transmute(p) };
        // c = 0+0i: stays at 0 forever, exhaust i, return 0.
        assert_eq!(f(0.0, 0.0, 0.0, 0.0, 20), 0);
        // c = 1+0i: trace from i=10, escapes at i=7.
        assert_eq!(f(0.0, 0.0, 1.0, 0.0, 10), 7);
    }

    #[test]
    fn jit_countdown_tail_loop() {
        // countdown(n) → 0, exercises tail-call back-edge with a
        // million iterations.
        let arm0 = SelectArm {
            cond: Some(
                cmp(
                    loc("n", PrimType::I64),
                    const_expr(ConstVal::I64(0)),
                    CmpOp::Eq,
                )
                .unwrap(),
            ),
            body: vec![GirStmt::Return(const_expr(ConstVal::I64(0)))],
        };
        let arm1 = SelectArm {
            cond: None,
            body: vec![GirStmt::TailCall {
                args: vec![arith(
                    loc("n", PrimType::I64),
                    const_expr(ConstVal::I64(1)),
                    BinOp::Sub,
                )
                .unwrap()],
            }],
        };
        let kernel = GirKernel {
            fn_name: ArcStr::from("countdown"),
            params: vec![input("n", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::Prim(PrimType::I64),
            has_tail_loop: true,
            body: vec![GirStmt::Select { arms: vec![arm0, arm1] }],
        };
        let (_ctx, p) = jit(&kernel);
        let f: extern "C" fn(i64) -> i64 = unsafe { std::mem::transmute(p) };
        assert_eq!(f(1_000_000), 0);
    }

    #[test]
    fn jit_ifchain_expression() {
        // |x| if x > 0 { 1 } else if x < 0 { -1 } else { 0 }
        let chain = GirExpr {
            op: GirOp::IfChain {
                arms: vec![
                    (
                        Some(
                            cmp(
                                loc("x", PrimType::I64),
                                const_expr(ConstVal::I64(0)),
                                CmpOp::Gt,
                            )
                            .unwrap(),
                        ),
                        const_expr(ConstVal::I64(1)),
                    ),
                    (
                        Some(
                            cmp(
                                loc("x", PrimType::I64),
                                const_expr(ConstVal::I64(0)),
                                CmpOp::Lt,
                            )
                            .unwrap(),
                        ),
                        const_expr(ConstVal::I64(-1)),
                    ),
                    (None, const_expr(ConstVal::I64(0))),
                ],
            },
            typ: GirType::Prim(PrimType::I64),
        };
        let kernel = GirKernel {
            fn_name: ArcStr::from("sign"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![GirStmt::Return(chain)],
        };
        let (_ctx, p) = jit(&kernel);
        let f: extern "C" fn(i64) -> i64 = unsafe { std::mem::transmute(p) };
        assert_eq!(f(5), 1);
        assert_eq!(f(-3), -1);
        assert_eq!(f(0), 0);
    }

    // The pair of `wrapper_nullable_kernel_*` tests that lived here
    // exercised the boxed-Value Nullable ABI. With the move to
    // two-register Value passing, the helpers they relied on
    // (`graphix_value_new_i64`, the boxed return path) are gone. New
    // tests for the by-value Nullable ABI go in their place once
    // the refactor lands; for now this block intentionally has none.
    #[cfg(any())]
    fn wrapper_nullable_kernel_param() {
        use crate::gir_interp::RegValue;
        use netidx_value::Value;
        let is_null_check = GirExpr {
            op: GirOp::IsNull(Box::new(GirExpr {
                op: GirOp::Local(ArcStr::from("x")),
                typ: GirType::Nullable(Box::new(GirType::Prim(PrimType::I64))),
            })),
            typ: GirType::Prim(PrimType::Bool),
        };
        let chain = GirExpr {
            op: GirOp::IfChain {
                arms: vec![
                    (Some(is_null_check), const_expr(ConstVal::I64(-1))),
                    (None, const_expr(ConstVal::I64(7))),
                ],
            },
            typ: GirType::Prim(PrimType::I64),
        };
        let kernel = GirKernel {
            fn_name: ArcStr::from("nullable_dispatch"),
            params: vec![],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![crate::gir::NullableInput {
                name: ArcStr::from("x"),
                elem: GirType::Prim(PrimType::I64),
                bind_id: None,
            }],
            tail_call_slots: vec![crate::gir::TailCallSlot {
                name: ArcStr::from("x"),
                kind: crate::gir::TailCallSlotKind::Nullable,
            }],
            return_type: GirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![GirStmt::Return(chain)],
        };
        let wrapped =
            compile_kernel_with_wrapper(&kernel).expect("compile wrapper");
        let f = unsafe { wrapped.fn_ptr() };

        // Pass `Value::I64(42)`: IsNull false → return 7.
        let arg = Value::I64(42);
        let args = [&arg as *const Value as u64];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_reg(out, PrimType::I64), RegValue::I64(7));

        // Pass `Value::Null`: IsNull true → return -1.
        let arg = Value::Null;
        let args = [&arg as *const Value as u64];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_reg(out, PrimType::I64), RegValue::I64(-1));
    }

    #[cfg(any())]
    fn wrapper_nullable_kernel_return() {
        use netidx_value::Value;
        let body = GirExpr {
            op: GirOp::IfChain {
                arms: vec![
                    (
                        Some(
                            cmp(
                                loc("n", PrimType::I64),
                                const_expr(ConstVal::I64(0)),
                                CmpOp::Gt,
                            )
                            .unwrap(),
                        ),
                        arith(
                            loc("n", PrimType::I64),
                            const_expr(ConstVal::I64(1)),
                            BinOp::Add,
                        )
                        .unwrap(),
                    ),
                    (
                        None,
                        GirExpr {
                            op: GirOp::ConstNull,
                            typ: GirType::Null,
                        },
                    ),
                ],
            },
            typ: GirType::Nullable(Box::new(GirType::Prim(PrimType::I64))),
        };
        let kernel = GirKernel {
            fn_name: ArcStr::from("maybe_inc"),
            params: vec![input("n", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![crate::gir::TailCallSlot {
                name: ArcStr::from("n"),
                kind: crate::gir::TailCallSlotKind::Scalar(PrimType::I64),
            }],
            return_type: GirType::Nullable(Box::new(GirType::Prim(
                PrimType::I64,
            ))),
            has_tail_loop: false,
            body: vec![GirStmt::Return(body)],
        };
        let wrapped =
            compile_kernel_with_wrapper(&kernel).expect("compile wrapper");
        let f = unsafe { wrapped.fn_ptr() };

        // n=10: non-null branch → Value::I64(11)
        let args = [pack_reg_to_u64(&crate::gir_interp::RegValue::I64(10))];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        let returned =
            unsafe { *Box::from_raw(out as *mut Value) };
        assert_eq!(returned, Value::I64(11));

        // n=-3: null branch → Value::Null
        let args = [pack_reg_to_u64(&crate::gir_interp::RegValue::I64(-3))];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        let returned =
            unsafe { *Box::from_raw(out as *mut Value) };
        assert_eq!(returned, Value::Null);
    }

    #[test]
    fn wrapper_round_trips_mandelbrot() {
        // Compile the mandelbrot iterate kernel through the
        // wrapper-generating path. Pack args as u64 slots, call
        // through the canonical WrapperFn, unpack the result.
        // Verifies the (args*, out*) ABI works for mixed f64/i64
        // signatures with a tail-recursive body.
        use crate::gir_interp::RegValue;
        let kernel = mandelbrot_iterate_kernel();
        let wrapped = compile_kernel_with_wrapper(&kernel)
            .expect("compile wrapper");
        let f = unsafe { wrapped.fn_ptr() };

        // c=1+0i, i=10 → 7
        let args = [
            pack_reg_to_u64(&RegValue::F64(0.0)),
            pack_reg_to_u64(&RegValue::F64(0.0)),
            pack_reg_to_u64(&RegValue::F64(1.0)),
            pack_reg_to_u64(&RegValue::F64(0.0)),
            pack_reg_to_u64(&RegValue::I64(10)),
        ];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_reg(out, kernel.return_type.as_prim().unwrap()), RegValue::I64(7));

        // c=0+0i, i=20 → 0
        let args = [
            pack_reg_to_u64(&RegValue::F64(0.0)),
            pack_reg_to_u64(&RegValue::F64(0.0)),
            pack_reg_to_u64(&RegValue::F64(0.0)),
            pack_reg_to_u64(&RegValue::F64(0.0)),
            pack_reg_to_u64(&RegValue::I64(20)),
        ];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_reg(out, kernel.return_type.as_prim().unwrap()), RegValue::I64(0));
    }

    #[test]
    fn jit_integer_overflow_wraps() {
        // i64::MAX + 1 should wrap to i64::MIN.
        let body = arith(
            loc("x", PrimType::I64),
            const_expr(ConstVal::I64(1)),
            BinOp::Add,
        )
        .unwrap();
        let kernel = GirKernel {
            fn_name: ArcStr::from("inc"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![GirStmt::Return(body)],
        };
        let (_ctx, p) = jit(&kernel);
        let f: extern "C" fn(i64) -> i64 = unsafe { std::mem::transmute(p) };
        assert_eq!(f(i64::MAX), i64::MIN);
    }

    /// Cross-kernel CLIF call via the shared module path. The parent
    /// kernel `caller` invokes `square(x) + 10`. Verifies that the
    /// `GirOp::Call` lowering threads correctly through the shared JIT
    /// module and that the wrapper still yields the right end-to-end
    /// result.
    #[test]
    fn shared_module_cross_kernel_call() {
        use crate::gir_interp::RegValue;
        use std::sync::Arc;
        // square(x: i64) -> i64 { x * x }
        let square = Arc::new(GirKernel {
            fn_name: ArcStr::from("square"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![GirStmt::Return(
                arith(
                    loc("x", PrimType::I64),
                    loc("x", PrimType::I64),
                    BinOp::Mul,
                )
                .unwrap(),
            )],
        });
        // caller(n: i64) -> i64 { square(n) + 10 }
        let call_expr = GirExpr {
            op: GirOp::Call {
                fn_name: ArcStr::from("square"),
                args: vec![loc("n", PrimType::I64)],
            },
            typ: GirType::Prim(PrimType::I64),
        };
        let body = arith(call_expr, const_expr(ConstVal::I64(10)), BinOp::Add)
            .unwrap();
        let caller = Arc::new(GirKernel {
            fn_name: ArcStr::from("caller"),
            params: vec![input("n", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![GirStmt::Return(body)],
        });
        let mut callees = BTreeMap::new();
        callees.insert(ArcStr::from("square"), square);
        let mut jit = Jit::new().expect("Jit::new");
        let wrapped = compile_kernel_with_callees(&mut jit, &caller, &callees)
            .expect("compile_kernel_with_callees");
        let f = unsafe { wrapped.fn_ptr() };
        let args = [pack_reg_to_u64(&RegValue::I64(7))];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_reg(out, caller.return_type.as_prim().unwrap()), RegValue::I64(59));
    }

    /// #131-JIT: cross-kernel call with a COMPOSITE (tuple) arg. The
    /// callee `h(p: (i64,i64), n: i64)` has a kind-grouped signature
    /// `[n: scalar, p: tuple-ptr]`; the call site passes args in source
    /// order `[(a,b), c]`, so the JIT Call arm must re-assemble them
    /// into `[c, tuple_ptr]` to match. A naive positional pass would
    /// deref the scalar `c` as a `*ValArray` (the historical crash).
    /// The tuple arg is an owned `TupleNew` producer → dropped after
    /// the call.
    #[test]
    fn cross_kernel_call_tuple_arg() {
        use crate::gir::TupleInput;
        use crate::gir_interp::RegValue;
        use std::sync::Arc;
        let i64t = || GirType::Prim(PrimType::I64);
        let tget = |idx: usize| GirExpr {
            op: GirOp::TupleGet {
                name: ArcStr::from("p"),
                idx,
                elem_typ: i64t(),
            },
            typ: i64t(),
        };
        // h(p, n) = p.0 + p.1 + n
        let h_body = arith(
            arith(tget(0), tget(1), BinOp::Add).unwrap(),
            loc("n", PrimType::I64),
            BinOp::Add,
        )
        .unwrap();
        let h = Arc::new(GirKernel {
            fn_name: ArcStr::from("h"),
            params: vec![input("n", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![TupleInput {
                name: ArcStr::from("p"),
                elems: vec![i64t(), i64t()],
                bind_id: None,
            }],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: i64t(),
            has_tail_loop: false,
            body: vec![GirStmt::Return(h_body)],
        });
        // caller(a, b, c) = h((a, b), c)
        let tuple_arg = GirExpr {
            op: GirOp::TupleNew {
                fields: vec![loc("a", PrimType::I64), loc("b", PrimType::I64)],
                elem_types: vec![i64t(), i64t()],
            },
            typ: GirType::Tuple(vec![i64t(), i64t()]),
        };
        let call = GirExpr {
            op: GirOp::Call {
                fn_name: ArcStr::from("h"),
                args: vec![tuple_arg, loc("c", PrimType::I64)],
            },
            typ: i64t(),
        };
        let caller = Arc::new(GirKernel {
            fn_name: ArcStr::from("caller"),
            params: vec![
                input("a", PrimType::I64),
                input("b", PrimType::I64),
                input("c", PrimType::I64),
            ],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: i64t(),
            has_tail_loop: false,
            body: vec![GirStmt::Return(call)],
        });
        let mut callees = BTreeMap::new();
        callees.insert(ArcStr::from("h"), h);
        let mut jit = Jit::new().expect("Jit::new");
        let wrapped = compile_kernel_with_callees(&mut jit, &caller, &callees)
            .expect("compile_kernel_with_callees");
        let f = unsafe { wrapped.fn_ptr() };
        let args = [
            pack_reg_to_u64(&RegValue::I64(10)),
            pack_reg_to_u64(&RegValue::I64(20)),
            pack_reg_to_u64(&RegValue::I64(5)),
        ];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_reg(out, PrimType::I64), RegValue::I64(35));
    }

    /// #131-JIT: cross-kernel call RETURNING a value-shape (Nullable).
    /// `h(x) -> [i64, null]` returns via an IfChain that widens to
    /// Nullable; the caller forwards it. Exercises the `GirOp::Call`
    /// arm in `compile_value_expr` — the two-word (disc, payload)
    /// return decode.
    #[test]
    fn cross_kernel_call_nullable_return() {
        use crate::gir_interp::RegValue;
        use std::sync::Arc;
        let nullable_i64 =
            || GirType::Nullable(Box::new(GirType::Prim(PrimType::I64)));
        // h(x: i64) -> [i64, null] = if x == 0 { null } else { x }
        let h_body = GirExpr {
            op: GirOp::IfChain {
                arms: vec![
                    (
                        Some(
                            cmp(
                                loc("x", PrimType::I64),
                                const_expr(ConstVal::I64(0)),
                                CmpOp::Eq,
                            )
                            .unwrap(),
                        ),
                        GirExpr { op: GirOp::ConstNull, typ: GirType::Null },
                    ),
                    (None, loc("x", PrimType::I64)),
                ],
            },
            typ: nullable_i64(),
        };
        let h = Arc::new(GirKernel {
            fn_name: ArcStr::from("h"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: nullable_i64(),
            has_tail_loop: false,
            body: vec![GirStmt::Return(h_body)],
        });
        // caller(x) = h(x)
        let call = GirExpr {
            op: GirOp::Call {
                fn_name: ArcStr::from("h"),
                args: vec![loc("x", PrimType::I64)],
            },
            typ: nullable_i64(),
        };
        let caller = Arc::new(GirKernel {
            fn_name: ArcStr::from("caller"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: nullable_i64(),
            has_tail_loop: false,
            body: vec![GirStmt::Return(call)],
        });
        let mut callees = BTreeMap::new();
        callees.insert(ArcStr::from("h"), h);
        let mut jit = Jit::new().expect("Jit::new");
        let wrapped = compile_kernel_with_callees(&mut jit, &caller, &callees)
            .expect("compile_kernel_with_callees");
        let f = unsafe { wrapped.fn_ptr() };
        let decode = |x: i64| -> netidx_value::Value {
            let args = [pack_reg_to_u64(&RegValue::I64(x))];
            let mut out = [0u64; 2];
            unsafe { f(args.as_ptr(), out.as_mut_ptr()) };
            unsafe { std::mem::transmute::<[u64; 2], netidx_value::Value>(out) }
        };
        assert_eq!(decode(5), netidx_value::Value::I64(5));
        assert_eq!(decode(0), netidx_value::Value::Null);
    }

    /// #131-JIT: cross-kernel call with a value-shape (Nullable) ARG
    /// that is an OWNED producer. `caller(x) = h(if x==0 {null} else
    /// {x})` where `h(m) = m` — the arg is an IfChain (classified
    /// `Owned`), so the JIT Call arm compiles it via
    /// `compile_value_expr` into a (disc, payload) pair, passes it
    /// borrowed (the callee clones on entry), and emits a post-call
    /// `graphix_value_drop`. Exercises the value-shape arg branch + the
    /// owned value-arg drop, plus a Nullable kernel param + return on
    /// the callee. (Deliberately uses an identity callee rather than
    /// `is_null` — the JIT `GirOp::IsNull` arm has a separate
    /// pre-existing bug under the by-value `Value` ABI.)
    #[test]
    fn cross_kernel_call_owned_nullable_arg() {
        use crate::gir::NullableInput;
        use crate::gir_interp::RegValue;
        use std::sync::Arc;
        let nullable_i64 =
            || GirType::Nullable(Box::new(GirType::Prim(PrimType::I64)));
        // h(m: [i64, null]) -> [i64, null] = m
        let h = Arc::new(GirKernel {
            fn_name: ArcStr::from("h"),
            params: vec![],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![NullableInput {
                name: ArcStr::from("m"),
                elem: GirType::Prim(PrimType::I64),
                bind_id: None,
            }],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: nullable_i64(),
            has_tail_loop: false,
            body: vec![GirStmt::Return(GirExpr {
                op: GirOp::Local(ArcStr::from("m")),
                typ: nullable_i64(),
            })],
        });
        // caller(x) -> [i64, null] = h(if x == 0 { null } else { x })
        let arg = GirExpr {
            op: GirOp::IfChain {
                arms: vec![
                    (
                        Some(
                            cmp(
                                loc("x", PrimType::I64),
                                const_expr(ConstVal::I64(0)),
                                CmpOp::Eq,
                            )
                            .unwrap(),
                        ),
                        GirExpr { op: GirOp::ConstNull, typ: GirType::Null },
                    ),
                    (None, loc("x", PrimType::I64)),
                ],
            },
            typ: nullable_i64(),
        };
        let caller = Arc::new(GirKernel {
            fn_name: ArcStr::from("caller"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: nullable_i64(),
            has_tail_loop: false,
            body: vec![GirStmt::Return(GirExpr {
                op: GirOp::Call {
                    fn_name: ArcStr::from("h"),
                    args: vec![arg],
                },
                typ: nullable_i64(),
            })],
        });
        let mut callees = BTreeMap::new();
        callees.insert(ArcStr::from("h"), h);
        let mut jit = Jit::new().expect("Jit::new");
        let wrapped = compile_kernel_with_callees(&mut jit, &caller, &callees)
            .expect("compile_kernel_with_callees");
        let f = unsafe { wrapped.fn_ptr() };
        let decode = |x: i64| -> netidx_value::Value {
            let args = [pack_reg_to_u64(&RegValue::I64(x))];
            let mut out = [0u64; 2];
            unsafe { f(args.as_ptr(), out.as_mut_ptr()) };
            unsafe { std::mem::transmute::<[u64; 2], netidx_value::Value>(out) }
        };
        assert_eq!(decode(0), netidx_value::Value::Null);
        assert_eq!(decode(7), netidx_value::Value::I64(7));
    }

    /// Regression for #133: `GirOp::IsNull` on a `Nullable` operand
    /// must JIT-compile. The arm used to `compile_scalar` its operand
    /// (which lowers to a two-register Value pair under the by-value
    /// ABI) and pass one arg to a two-arg helper — both wrong; it now
    /// inlines `icmp disc, NULL_DISC`. `h(m: [i64, null]) -> bool =
    /// is_null(m)`.
    #[test]
    fn jit_is_null_on_nullable_param() {
        use crate::gir::NullableInput;
        use crate::gir_interp::RegValue;
        let nullable_i64 =
            || GirType::Nullable(Box::new(GirType::Prim(PrimType::I64)));
        let kernel = GirKernel {
            fn_name: ArcStr::from("h"),
            params: vec![],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![NullableInput {
                name: ArcStr::from("m"),
                elem: GirType::Prim(PrimType::I64),
                bind_id: None,
            }],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::Prim(PrimType::Bool),
            has_tail_loop: false,
            body: vec![GirStmt::Return(GirExpr {
                op: GirOp::IsNull(Box::new(GirExpr {
                    op: GirOp::Local(ArcStr::from("m")),
                    typ: nullable_i64(),
                })),
                typ: GirType::Prim(PrimType::Bool),
            })],
        };
        let wrapped = compile_kernel_with_wrapper(&kernel)
            .expect("compile_kernel_with_wrapper");
        let f = unsafe { wrapped.fn_ptr() };
        // Nullable param `m` arrives as its two `repr(u64)` Value words
        // (disc, payload) — the same packing `GirNode::update` does.
        let run = |v: netidx_value::Value| -> RegValue {
            let words: [u64; 2] = unsafe { std::mem::transmute(v) };
            let mut out = 0u64;
            unsafe { f(words.as_ptr(), &mut out) };
            unpack_u64_to_reg(out, PrimType::Bool)
        };
        assert_eq!(run(netidx_value::Value::Null), RegValue::Bool(true));
        assert_eq!(run(netidx_value::Value::I64(7)), RegValue::Bool(false));
    }

    /// String + bare value-shape (DateTime) kernel params, JIT side.
    /// Mirrors the interp `string_and_value_kernel_params` test through
    /// the cranelift wrapper: a String param rides one machine word (its
    /// ArcStr thin pointer), a DateTime param rides two (disc, payload),
    /// both cloned on entry and dropped on exit. Confirms the new
    /// `AbiParamKind::String` / `AbiParamKind::Value` arms in the
    /// signature builder, wrapper unpacker, and entry binder agree
    /// with the runtime packing.
    #[test]
    fn jit_string_and_value_kernel_params() {
        use netidx_value::Value;
        // (1) |s: string| -> string s
        let str_kernel = GirKernel {
            fn_name: ArcStr::from("sk"),
            params: vec![],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![crate::gir::StringInput {
                name: ArcStr::from("s"),
                bind_id: None,
            }],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::String,
            has_tail_loop: false,
            body: vec![GirStmt::Return(GirExpr {
                op: GirOp::Local(ArcStr::from("s")),
                typ: GirType::String,
            })],
        };
        let wrapped = compile_kernel_with_wrapper(&str_kernel)
            .expect("compile str kernel");
        let f = unsafe { wrapped.fn_ptr() };
        let s = arcstr::ArcStr::from("hello");
        // One word: the ArcStr's thin pointer (borrowed; kernel clones).
        let word = unsafe { *(&s as *const arcstr::ArcStr as *const u64) };
        let args = [word];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        // Return: an owned ArcStr (transferred). Reclaim and check.
        let got: arcstr::ArcStr =
            unsafe { std::mem::transmute::<u64, arcstr::ArcStr>(out) };
        assert_eq!(&*got, "hello");
        drop(s);

        // (2) |d: datetime| -> datetime d + duration:1s
        let one_sec = Value::Duration(triomphe::Arc::new(
            std::time::Duration::from_secs(1),
        ));
        let dt_kernel = GirKernel {
            fn_name: ArcStr::from("dk"),
            params: vec![],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![crate::gir::ValueInput {
                name: ArcStr::from("d"),
                typ: GirType::DateTime,
                bind_id: None,
            }],
            tail_call_slots: vec![],
            return_type: GirType::DateTime,
            has_tail_loop: false,
            body: vec![GirStmt::Return(GirExpr {
                op: GirOp::ValueArith {
                    op: BinOp::Add,
                    lhs: Box::new(GirExpr {
                        op: GirOp::Local(ArcStr::from("d")),
                        typ: GirType::DateTime,
                    }),
                    rhs: Box::new(GirExpr {
                        op: GirOp::ConstValue(one_sec.clone()),
                        typ: GirType::Duration,
                    }),
                },
                typ: GirType::DateTime,
            })],
        };
        let wrapped = compile_kernel_with_wrapper(&dt_kernel)
            .expect("compile dt kernel");
        let f = unsafe { wrapped.fn_ptr() };
        let base =
            chrono::DateTime::<chrono::Utc>::from_timestamp(1_700_000_000, 0)
                .unwrap();
        let arg = Value::DateTime(triomphe::Arc::new(base));
        // Two words (disc, payload), borrowed; kernel clones on entry.
        let words: [u64; 2] =
            unsafe { *(&arg as *const Value as *const [u64; 2]) };
        let mut out2: [u64; 2] = [0, 0];
        unsafe { f(words.as_ptr(), out2.as_mut_ptr()) };
        let got: Value = unsafe { std::mem::transmute::<[u64; 2], Value>(out2) };
        let expected = Value::DateTime(triomphe::Arc::new(
            base + chrono::Duration::seconds(1),
        ));
        assert_eq!(got, expected);
        drop(arg);
    }

    /// Non-tail self-recursion via `GirOp::Call` (e.g. naive fib's
    /// `fib(n-1) + fib(n-2)`). The shared-module path declares the
    /// parent's `FuncId` first, then routes any Call site whose name
    /// matches the parent back to that FuncId. Verifies the wrapper
    /// returns the right answer for a small fib input.
    #[test]
    fn shared_module_self_recursion() {
        use crate::gir_interp::RegValue;
        use std::sync::Arc;
        // fib(n) = if n < 2 { n } else { fib(n-1) + fib(n-2) }
        let fib_call = |which: i64| GirExpr {
            op: GirOp::Call {
                fn_name: ArcStr::from("fib"),
                args: vec![arith(
                    loc("n", PrimType::I64),
                    const_expr(ConstVal::I64(which)),
                    BinOp::Sub,
                )
                .unwrap()],
            },
            typ: GirType::Prim(PrimType::I64),
        };
        let recursive_body =
            arith(fib_call(1), fib_call(2), BinOp::Add).unwrap();
        let arms = vec![
            SelectArm {
                cond: Some(
                    cmp(
                        loc("n", PrimType::I64),
                        const_expr(ConstVal::I64(2)),
                        CmpOp::Lt,
                    )
                    .unwrap(),
                ),
                body: vec![GirStmt::Return(loc("n", PrimType::I64))],
            },
            SelectArm {
                cond: None,
                body: vec![GirStmt::Return(recursive_body)],
            },
        ];
        let fib = Arc::new(GirKernel {
            fn_name: ArcStr::from("fib"),
            params: vec![input("n", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![GirStmt::Select { arms }],
        });
        // Lazy fusion would put the parent's own kernel in the
        // registry to support self-recursion in the interpreter.
        // Mirror that — the JIT path skips it and uses the parent's
        // own FuncId.
        let mut callees = BTreeMap::new();
        callees.insert(ArcStr::from("fib"), fib.clone());
        let mut jit = Jit::new().expect("Jit::new");
        let wrapped = compile_kernel_with_callees(&mut jit, &fib, &callees)
            .expect("compile fib with self-recursion");
        let f = unsafe { wrapped.fn_ptr() };
        // fib(10) = 55, fib(15) = 610.
        let mut out = 0u64;
        let args = [pack_reg_to_u64(&RegValue::I64(10))];
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_reg(out, PrimType::I64), RegValue::I64(55));
        let mut out = 0u64;
        let args = [pack_reg_to_u64(&RegValue::I64(15))];
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_reg(out, PrimType::I64), RegValue::I64(610));
    }

    /// Three-level chain: outer → middle → leaf, all in the shared
    /// module via a single `compile_kernel_with_callees` call. The
    /// caller passes the *transitive* closure (outer + middle + leaf);
    /// the two-phase declare-then-define lets middle's body reference
    /// leaf's FuncId even though leaf wasn't declared at the time we
    /// started middle's compile. Tests that the M4d v3 transitive
    /// fan-out path works end-to-end.
    #[test]
    fn shared_module_transitive_fan_out() {
        use crate::gir_interp::RegValue;
        use std::sync::Arc;
        // leaf(x) = x + 1
        let leaf = Arc::new(GirKernel {
            fn_name: ArcStr::from("leaf"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![GirStmt::Return(
                arith(
                    loc("x", PrimType::I64),
                    const_expr(ConstVal::I64(1)),
                    BinOp::Add,
                )
                .unwrap(),
            )],
        });
        // middle(x) = leaf(x) * 2
        let middle = Arc::new(GirKernel {
            fn_name: ArcStr::from("middle"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![GirStmt::Return(
                arith(
                    GirExpr {
                        op: GirOp::Call {
                            fn_name: ArcStr::from("leaf"),
                            args: vec![loc("x", PrimType::I64)],
                        },
                        typ: GirType::Prim(PrimType::I64),
                    },
                    const_expr(ConstVal::I64(2)),
                    BinOp::Mul,
                )
                .unwrap(),
            )],
        });
        // outer(x) = middle(x) - 3
        let outer = Arc::new(GirKernel {
            fn_name: ArcStr::from("outer"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![GirStmt::Return(
                arith(
                    GirExpr {
                        op: GirOp::Call {
                            fn_name: ArcStr::from("middle"),
                            args: vec![loc("x", PrimType::I64)],
                        },
                        typ: GirType::Prim(PrimType::I64),
                    },
                    const_expr(ConstVal::I64(3)),
                    BinOp::Sub,
                )
                .unwrap(),
            )],
        });
        let mut callees = BTreeMap::new();
        callees.insert(ArcStr::from("middle"), middle);
        callees.insert(ArcStr::from("leaf"), leaf);
        let mut jit = Jit::new().expect("Jit::new");
        let wrapped = compile_kernel_with_callees(&mut jit, &outer, &callees)
            .expect("compile transitive chain");
        let f = unsafe { wrapped.fn_ptr() };
        // outer(10) = ((10 + 1) * 2) - 3 = 19
        let args = [pack_reg_to_u64(&RegValue::I64(10))];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_reg(out, PrimType::I64), RegValue::I64(19));
    }

    /// Same callee Arc invoked via two separate parent kernels. The
    /// `by_kernel` cache should compile `square` exactly once and let
    /// both parents share it. Verifies both parents return the right
    /// answer (so the shared callee is correctly addressable from each).
    #[test]
    fn shared_module_callee_dedup() {
        use crate::gir_interp::RegValue;
        use std::sync::Arc;
        let square = Arc::new(GirKernel {
            fn_name: ArcStr::from("square"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
            return_type: GirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![GirStmt::Return(
                arith(
                    loc("x", PrimType::I64),
                    loc("x", PrimType::I64),
                    BinOp::Mul,
                )
                .unwrap(),
            )],
        });
        let make_caller = |add_const: i64| {
            let call_expr = GirExpr {
                op: GirOp::Call {
                    fn_name: ArcStr::from("square"),
                    args: vec![loc("n", PrimType::I64)],
                },
                typ: GirType::Prim(PrimType::I64),
            };
            let body = arith(
                call_expr,
                const_expr(ConstVal::I64(add_const)),
                BinOp::Add,
            )
            .unwrap();
            Arc::new(GirKernel {
                fn_name: ArcStr::from("caller"),
                params: vec![input("n", PrimType::I64)],
                fn_params: vec![],
                array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![],
                return_type: GirType::Prim(PrimType::I64),
                has_tail_loop: false,
                body: vec![GirStmt::Return(body)],
            })
        };
        let caller_a = make_caller(1);
        let caller_b = make_caller(100);
        let mut callees = BTreeMap::new();
        callees.insert(ArcStr::from("square"), square.clone());
        // Share a single Jit instance across both compiles so that
        // the `square` callee dedupes (same `Arc<GirKernel>` key).
        // The cross-compile dedupe is the property the test
        // verifies — putting both into different Jits would
        // compile `square` twice.
        let mut jit = Jit::new().expect("Jit::new");
        let wa = compile_kernel_with_callees(&mut jit, &caller_a, &callees)
            .expect("compile a");
        let wb = compile_kernel_with_callees(&mut jit, &caller_b, &callees)
            .expect("compile b");
        let fa = unsafe { wa.fn_ptr() };
        let fb = unsafe { wb.fn_ptr() };
        let args = [pack_reg_to_u64(&RegValue::I64(5))];
        let (mut oa, mut ob) = (0u64, 0u64);
        unsafe {
            fa(args.as_ptr(), &mut oa);
            fb(args.as_ptr(), &mut ob);
        }
        assert_eq!(unpack_u64_to_reg(oa, PrimType::I64), RegValue::I64(26));
        assert_eq!(unpack_u64_to_reg(ob, PrimType::I64), RegValue::I64(125));
    }

    /// Regression: when `DYNCALL_PENDING` is set BEFORE the JIT'd
    /// kernel returns (simulating an upstream scalar DynCall having
    /// pended silently), the `emit_return_pending_check` for
    /// composite returns must short-circuit to `pending_exit` —
    /// dropping the owned `*mut ValArray` rather than handing the
    /// caller a leaked pointer + a "successful" sentinel-as-real-
    /// result.
    ///
    /// Verifies that:
    ///   1. The wrapper returns 0 (sentinel) in `out[0]`, not the
    ///      real ValArray pointer.
    ///   2. The pending flag remains set so `GirNode::update` knows
    ///      to return `None` (the peek behavior from Fix 1).
    ///
    /// Implicit verification: the ValArray that the kernel built
    /// must have been dropped via `graphix_valarray_drop` on the
    /// pre_pending path. We can't easily assert the drop happened
    /// without instrumenting the allocator, but the wrapper
    /// returning 0 (instead of the pointer) proves the pre_pending
    /// branch was taken.
    #[test]
    fn return_pending_check_drops_composite_result() {
        use crate::gir_interp::RegValue;
        // |x: i64| -> (i64, i64) (x, x + 1)
        // Body is just TupleNew of two scalars — no DynCall.
        let body = GirExpr {
            op: GirOp::TupleNew {
                fields: vec![
                    loc("x", PrimType::I64),
                    arith(
                        loc("x", PrimType::I64),
                        const_expr(ConstVal::I64(1)),
                        BinOp::Add,
                    )
                    .unwrap(),
                ],
                elem_types: vec![
                    GirType::Prim(PrimType::I64),
                    GirType::Prim(PrimType::I64),
                ],
            },
            typ: GirType::Tuple(vec![
                GirType::Prim(PrimType::I64),
                GirType::Prim(PrimType::I64),
            ]),
        };
        let kernel = GirKernel {
            fn_name: ArcStr::from("pair_of"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![crate::gir::TailCallSlot {
                name: ArcStr::from("x"),
                kind: crate::gir::TailCallSlotKind::Scalar(PrimType::I64),
            }],
            return_type: GirType::Tuple(vec![
                GirType::Prim(PrimType::I64),
                GirType::Prim(PrimType::I64),
            ]),
            has_tail_loop: false,
            body: vec![GirStmt::Return(body)],
        };
        let wrapped =
            compile_kernel_with_wrapper(&kernel).expect("compile wrapper");
        let f = unsafe { wrapped.fn_ptr() };
        let args = [pack_reg_to_u64(&RegValue::I64(21))];

        // First: baseline — pending NOT set, kernel returns a real
        // *mut ValArray pointer. Reclaim and verify the tuple
        // content is right.
        crate::gir_jit_helpers::DYNCALL_PENDING.with(|c| c.set(false));
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert!(
            out != 0,
            "non-pending kernel run returns a real ValArray ptr"
        );
        let owned = unsafe {
            *Box::from_raw(out as *mut netidx_value::ValArray)
        };
        assert_eq!(owned[0], netidx_value::Value::I64(21));
        assert_eq!(owned[1], netidx_value::Value::I64(22));

        // Now: pending SET before the call, simulating an upstream
        // scalar DynCall having pended earlier in the program. The
        // Return-time pending check must drop the tuple and write
        // 0 to *out.
        crate::gir_jit_helpers::DYNCALL_PENDING.with(|c| c.set(true));
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(
            out, 0,
            "pending-set kernel run returns sentinel 0, not a real \
             ValArray ptr — the Return-time pending check fired"
        );
        assert!(
            crate::gir_jit_helpers::DYNCALL_PENDING.with(|c| c.get()),
            "DYNCALL_PENDING stays set after the JIT pending branch — \
             GirNode::update sees it and returns None (Fix 1: peek, \
             not clear)"
        );
        // Tidy up so other tests don't see a stale set flag.
        crate::gir_jit_helpers::DYNCALL_PENDING.with(|c| c.set(false));
    }

    /// String-return version of [`return_pending_check_drops_composite_result`]:
    /// verify the JIT's String-return pending check fires and drops
    /// the just-built `ArcStr` instead of returning a stale pointer
    /// that `GirNode::update` would discard without refcount-decrementing
    /// (the bug Audit #1 caught in May 2026).
    ///
    /// Body: `|x: i64| -> string "hi"` (`ConstStr`). Forces the JIT
    /// to produce an owned ArcStr via `graphix_arcstr_clone_from_static`.
    ///
    /// Verifies:
    /// - Baseline (pending unset): wrapper returns the ArcStr's raw
    ///   pointer; transmuting back gives the string `"hi"`.
    /// - Pending set: wrapper returns sentinel `0`. The just-built
    ///   ArcStr was dropped by the return-time pending check, so the
    ///   interned static's refcount didn't bump from a leaked clone.
    /// - `DYNCALL_PENDING` stays set after the pending-path run
    ///   (peek, not clear).
    #[test]
    fn return_pending_check_drops_string_result() {
        use crate::gir_interp::RegValue;
        let body = GirExpr {
            op: GirOp::ConstStr(ArcStr::from("hi")),
            typ: GirType::String,
        };
        let kernel = GirKernel {
            fn_name: ArcStr::from("returns_str"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            nullable_params: vec![],
            string_params: vec![],
            value_params: vec![],
            tail_call_slots: vec![crate::gir::TailCallSlot {
                name: ArcStr::from("x"),
                kind: crate::gir::TailCallSlotKind::Scalar(PrimType::I64),
            }],
            return_type: GirType::String,
            has_tail_loop: false,
            body: vec![GirStmt::Return(body)],
        };
        let wrapped =
            compile_kernel_with_wrapper(&kernel).expect("compile wrapper");
        let f = unsafe { wrapped.fn_ptr() };
        let args = [pack_reg_to_u64(&RegValue::I64(0))];

        // Baseline: pending NOT set, wrapper returns a real ArcStr
        // pointer. Transmute back, verify content, drop properly.
        crate::gir_jit_helpers::DYNCALL_PENDING.with(|c| c.set(false));
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert!(out != 0, "non-pending run returns a real ArcStr ptr");
        let owned: arcstr::ArcStr =
            unsafe { std::mem::transmute::<u64, arcstr::ArcStr>(out) };
        assert_eq!(owned.as_str(), "hi");
        drop(owned);

        // Pending SET before the call: the Return-time String pending
        // check fires, drops the just-built ArcStr (refcount decrement
        // back to the interned static's original count), writes 0 to
        // *out. Without the fix, the ArcStr's bump would leak forever.
        crate::gir_jit_helpers::DYNCALL_PENDING.with(|c| c.set(true));
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(
            out, 0,
            "pending-set kernel run returns sentinel 0, not a real \
             ArcStr ptr — the Return-time String pending check fired"
        );
        assert!(
            crate::gir_jit_helpers::DYNCALL_PENDING.with(|c| c.get()),
            "DYNCALL_PENDING stays set after the JIT pending branch"
        );
        crate::gir_jit_helpers::DYNCALL_PENDING.with(|c| c.set(false));
    }

}
