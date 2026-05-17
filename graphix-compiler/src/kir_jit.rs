//! Cranelift JIT backend for the kernel IR.
//!
//! Lowers a [`KirKernel`] to native machine code via Cranelift,
//! returning a function pointer that the runtime can call directly.
//! The companion of [`crate::kir_interp`] (the KIR interpreter) —
//! same fusion analysis, same IR, different backend.
//!
//! ## Calling convention
//!
//! The compiled function uses the host platform's default C calling
//! convention (SystemV on Linux, Windows-fastcall on Windows). Each
//! parameter is passed as its natural primitive register type:
//!
//! - `i8/i16/i32/i64/u8/u16/u32/u64` → CLIF `I8`/`I16`/`I32`/`I64`
//! - `f32/f64` → CLIF `F32`/`F64`
//! - `bool` → CLIF `I8` (0 = false, non-zero = true)
//!
//! Call from Rust by transmuting the returned function pointer to
//! `extern "C" fn(...) -> ...` of the matching signature.
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

use crate::kernel_ir::{
    BinOp, BoolOp, CmpOp, ConstVal, KirExpr, KirKernel, KirOp, KirStmt, KirType,
    PrimType, SelectArm,
};
use anyhow::{anyhow, Context as AnyContext, Result};
use arcstr::ArcStr;
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
    /// Pre-declared FuncIds for the `kir_jit_helpers::*` runtime
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
        // Make the kir_jit_helpers entry points resolvable from JIT'd
        // code. Each one is `#[no_mangle] extern "C"`, registered
        // here under the same symbol name we use in `declare_function`.
        for (name, ptr) in crate::kir_jit_helpers::all_symbols() {
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
}

unsafe impl Send for CompiledKernel {}
unsafe impl Sync for CompiledKernel {}

// ─── Public entry point ──────────────────────────────────────────

/// JIT-compile `kernel` and return a function pointer to the entry
/// point. The pointer's call signature matches the kernel's params /
/// return type — see the module docstring for the calling convention.
pub fn compile_kernel(jit: &mut JitCtx, kernel: &KirKernel) -> Result<CompiledKernel> {
    // `compile_kernel` is the bare-entry-point path (no wrapper, no
    // string-table lifecycle). Strings would still need to live
    // somewhere for the JIT'd code to reach them — for now just
    // leak them by storing in the returned CompiledKernel, same
    // policy as the wrapped path.
    let (func_id, sig, strings) = define_typed_kernel(jit, kernel)?;
    jit.module
        .finalize_definitions()
        .context("finalize_definitions")?;
    let fn_ptr = jit.module.get_finalized_function(func_id);
    Ok(CompiledKernel { func_id, fn_ptr, signature: sig, _strings: strings })
}

/// Define the typed kernel function in the JIT module without
/// finalizing. Returns the FuncId and Signature; the caller is
/// responsible for finalizing before extracting fn pointers.
/// Splitting this out from `compile_kernel` lets us emit a typed
/// kernel and a wrapper that calls it in the same module before a
/// single `finalize_definitions` call.
fn define_typed_kernel(
    jit: &mut JitCtx,
    kernel: &KirKernel,
) -> Result<(FuncId, Signature, KernelStrings)> {
    // Fn-typed params (HOFs) dispatch through `graphix_dyncall` at
    // runtime via the dispatcher handle set in `KirNode::update`.
    // Both scalar- and composite-return DynCalls are supported:
    // scalar returns are branchless (0 is a harmless pending
    // sentinel for downstream arithmetic); composite returns get
    // a per-DynCall `pre_pending` block that drops the owned set
    // and jumps to `pending_exit`.
    let symbol = jit.next_symbol(&kernel.fn_name);
    let mut sig = Signature::new(jit.module.isa().default_call_conv());
    for p in &kernel.params {
        sig.params.push(AbiParam::new(prim_to_clif(p.prim)));
    }
    // Composite (array/tuple/struct) params follow scalars; each is
    // a `*const ValArray` passed as a pointer-sized integer. Variant
    // params follow those; they're `*const Value` (different shape
    // but same I64 pointer size at the ABI level).
    let n_composites = kernel.array_params.len()
        + kernel.tuple_params.len()
        + kernel.struct_params.len()
        + kernel.variant_params.len();
    for _ in 0..n_composites {
        sig.params.push(AbiParam::new(types::I64));
    }
    // Composite-return support: when the kernel's return type is
    // an Array/Tuple/Struct/Variant, the typed kernel returns an
    // I64 holding the owned `*mut ValArray` (for array/tuple/
    // struct) or `*mut Value` (for variant). The runtime caller
    // reads this pointer out of the wrapper's *out slot and
    // reclaims ownership via `Box::from_raw`.
    let return_clif = match &kernel.return_type {
        KirType::Prim(p) => prim_to_clif(*p),
        KirType::Array(_)
        | KirType::Tuple(_)
        | KirType::Struct(_)
        | KirType::Variant(_) => types::I64,
        // Unit return: still emit an I64 slot (so the ABI is uniform);
        // the kernel body writes 0 and the caller throws the result
        // away. Avoids special-casing zero-returns at the wrapper /
        // dispatch layer.
        KirType::Unit => types::I64,
        // String returns can't be JIT'd; the kernel should have
        // been routed to interp by `kernel_contains_string`.
        KirType::String => {
            return Err(anyhow!(
                "kernel returns KirType::String; JIT can't ABI it — \
                 should have routed to interp via kernel_contains_string"
            ))
        }
    };
    sig.returns.push(AbiParam::new(return_clif));

    let func_id = jit
        .module
        .declare_function(&symbol, Linkage::Local, &sig)
        .context("declare_function (typed)")?;
    jit.func_ctx.func.signature = sig.clone();
    jit.func_ctx.func.name =
        cranelift_codegen::ir::UserFuncName::user(0, func_id.as_u32());

    // Build the per-kernel string slot table. Each unique struct
    // field name / variant tag goes through the global intern table
    // (which dedupes across the whole process). The resulting
    // `KernelStrings` owns one canonical clone of each string; the
    // boxed slice has stable addresses for codegen to use.
    let strings = KernelStrings::build(kernel);
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
        )?;
        builder.finalize();
    }

    jit.module
        .define_function(func_id, &mut jit.func_ctx)
        .context("define_function (typed)")?;
    jit.module.clear_context(&mut jit.func_ctx);
    Ok((func_id, sig, strings))
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
/// Doesn't support kernels containing `KirOp::Call` — use
/// [`compile_kernel_with_callees`] for those.
pub fn compile_kernel_with_wrapper(kernel: &KirKernel) -> Result<WrappedKernel> {
    let mut ctx = JitCtx::new()?;
    let (typed_id, _, strings) = define_typed_kernel(&mut ctx, kernel)?;
    let wrapper_id = define_wrapper(&mut ctx, kernel, typed_id)?;
    ctx.module
        .finalize_definitions()
        .context("finalize_definitions (wrapper)")?;
    let wrapper_fn_ptr = ctx.module.get_finalized_function(wrapper_id);
    Ok(WrappedKernel {
        wrapper_fn_ptr,
        _ctx: Some(ctx),
        _strings: strings,
    })
}

// ─── Shared JIT module: cross-kernel CLIF calls ──────────────────
//
// All kernels with `KirOp::Call` go into a single global JIT module so
// that one kernel's compiled code can `call` another's directly via a
// CLIF `call` instruction. The module is never dropped — fn pointers
// it produces are valid for the program's lifetime.
//
// `by_kernel` keys by Arc identity so the same `Arc<KirKernel>`
// referenced from multiple parent kernels reuses one compilation.
// Names alone aren't unique enough — two distinct programs can both
// have a binding `foo` with different KIR.

struct SharedJit {
    ctx: JitCtx,
    /// Per-kernel cache: Arc<KirKernel> raw pointer → cached entry.
    /// We keep the Arc alive in the entry so the raw pointer key
    /// stays valid for the lifetime of the static. Without it,
    /// Arc-allocator reuse could land a different KIR at the same
    /// address and we'd return a stale FuncId pointing at code with
    /// the wrong signature.
    by_kernel: BTreeMap<usize, CachedKernel>,
}

struct CachedKernel {
    func_id: FuncId,
    signature: Signature,
    /// Holds the Arc alive so its raw pointer can't be reused by a
    /// later allocation.
    _kernel: std::sync::Arc<KirKernel>,
    /// Per-kernel string table. The JIT'd code for this kernel bakes
    /// in `*const ArcStr` pointers into this table's `Box<[ArcStr]>`
    /// (struct field names, variant tags). The shared module's code
    /// lives for the lifetime of `SHARED_JIT`, so the table must too
    /// — kept here, alongside the kernel's `FuncId`. Populated empty
    /// in phase 1 (`ensure_declared`); the real table is moved in
    /// during phase 2 (`define_kernel_body`). Moving the
    /// `KernelStrings` struct around the `BTreeMap` is fine — only
    /// the `Box` pointer moves, not the heap allocation the baked-in
    /// pointers reference.
    _strings: KernelStrings,
}

unsafe impl Send for SharedJit {}

static SHARED_JIT: std::sync::LazyLock<parking_lot::Mutex<SharedJit>> =
    std::sync::LazyLock::new(|| {
        parking_lot::Mutex::new(SharedJit {
            ctx: JitCtx::new().expect("init shared JitCtx"),
            by_kernel: BTreeMap::new(),
        })
    });

/// JIT-compile `kernel` alongside any kernels it calls (`callees`),
/// returning a [`WrappedKernel`] whose code can directly CLIF-call
/// the callees via `call` instructions (no interpreter dispatch on
/// the cross-kernel boundary). Used by lazy fusion when a kernel
/// body contains `KirOp::Call`.
///
/// `callees` must be the *transitive* closure: every kernel reachable
/// from `kernel` through `KirOp::Call`, by name. Each value is the
/// `Arc<KirKernel>` for that name. Each callee's KirKernel is keyed
/// by Arc identity in a global cache (`SharedJit::by_kernel`); the
/// same Arc referenced from multiple parents compiles once.
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
    kernel: &std::sync::Arc<KirKernel>,
    callees: &BTreeMap<ArcStr, std::sync::Arc<KirKernel>>,
) -> Result<WrappedKernel> {
    let mut shared = SHARED_JIT.lock();
    // Phase 1 — declare every kernel in the closure (parent + all
    // transitively-reachable callees). Cached entries reuse their
    // `FuncId`; fresh ones get a freshly-declared FuncId and queue
    // for phase-2 body definition.
    //
    // The parent and any callee with name == parent's fn_name share
    // the same FuncId; that's how self-recursion via `KirOp::Call`
    // resolves to a CLIF call back to the parent.
    let kernel_name = kernel.fn_name.clone();
    let mut funcids: BTreeMap<ArcStr, (FuncId, Signature)> = BTreeMap::new();
    let mut to_define: Vec<std::sync::Arc<KirKernel>> = Vec::new();
    let parent_entry = ensure_declared(&mut shared, kernel, &mut to_define)?;
    funcids.insert(kernel_name.clone(), parent_entry.clone());
    for (name, k) in callees {
        if name.as_str() == kernel_name.as_str() {
            funcids.insert(name.clone(), parent_entry.clone());
            continue;
        }
        let entry = ensure_declared(&mut shared, k, &mut to_define)?;
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
    // cache entry so it lives as long as the shared module's code.
    for k in &to_define {
        let strings = define_kernel_body(&mut shared.ctx, k, &funcids)?;
        let key = std::sync::Arc::as_ptr(k) as usize;
        if let Some(cached) = shared.by_kernel.get_mut(&key) {
            cached._strings = strings;
        }
    }
    // Phase 3 — compile the uniform wrapper for the parent and
    // finalize the module so the new code is mapped read-execute.
    let wrapper_id = define_wrapper(&mut shared.ctx, kernel, parent_entry.0)?;
    shared
        .ctx
        .module
        .finalize_definitions()
        .context("finalize_definitions (shared)")?;
    let wrapper_fn_ptr = shared.ctx.module.get_finalized_function(wrapper_id);
    // _ctx: None — the shared module is kept alive by the static.
    // _strings: empty here — for the shared-module path each kernel's
    // string table lives in `SharedJit::by_kernel[key]._strings`
    // (stored in phase 2), since the compiled code outlives this
    // `WrappedKernel` and is owned by the `SHARED_JIT` static.
    Ok(WrappedKernel {
        wrapper_fn_ptr,
        _ctx: None,
        _strings: KernelStrings::empty(),
    })
}

/// Phase-1 helper: ensure `k` has a `FuncId` declared in the shared
/// module. Cached kernels (by `Arc::as_ptr` identity) reuse their
/// existing entry. Freshly-declared kernels are pushed onto
/// `to_define` so phase 2 compiles their body.
fn ensure_declared(
    shared: &mut SharedJit,
    k: &std::sync::Arc<KirKernel>,
    to_define: &mut Vec<std::sync::Arc<KirKernel>>,
) -> Result<(FuncId, Signature)> {
    let key = std::sync::Arc::as_ptr(k) as usize;
    if let Some(e) = shared.by_kernel.get(&key) {
        return Ok((e.func_id, e.signature.clone()));
    }
    let symbol = shared.ctx.next_symbol(&k.fn_name);
    let mut sig = Signature::new(shared.ctx.module.isa().default_call_conv());
    for p in &k.params {
        sig.params.push(AbiParam::new(prim_to_clif(p.prim)));
    }
    let k_return_prim = k
        .return_type
        .as_prim()
        .ok_or_else(|| anyhow!("JIT does not support array return types"))?;
    sig.returns.push(AbiParam::new(prim_to_clif(k_return_prim)));
    let fid = shared
        .ctx
        .module
        .declare_function(&symbol, Linkage::Local, &sig)
        .context("declare_function (shared declare)")?;
    shared.by_kernel.insert(
        key,
        CachedKernel {
            func_id: fid,
            signature: sig.clone(),
            _kernel: k.clone(),
            // Filled in during phase 2 by `define_kernel_body`.
            _strings: KernelStrings::empty(),
        },
    );
    to_define.push(k.clone());
    Ok((fid, sig))
}

/// Phase-2 helper: compile `kernel`'s body and call `define_function`
/// on its pre-declared `FuncId`. `funcids` must contain entries for
/// the kernel itself and every callee its body references via
/// `KirOp::Call`.
fn define_kernel_body(
    jit: &mut JitCtx,
    kernel: &KirKernel,
    funcids: &BTreeMap<ArcStr, (FuncId, Signature)>,
) -> Result<KernelStrings> {
    let (func_id, sig) =
        funcids.get(&kernel.fn_name).cloned().ok_or_else(|| {
            anyhow!(
                "define_kernel_body: missing FuncId for kernel `{}` \
                 (phase-1 declare must have populated `funcids` first)",
                kernel.fn_name
            )
        })?;
    jit.func_ctx.func.signature = sig;
    jit.func_ctx.func.name =
        cranelift_codegen::ir::UserFuncName::user(0, func_id.as_u32());
    let strings = {
        // Declare each Call site's callee as a FuncRef in this
        // function. Done before constructing the FunctionBuilder
        // because both `declare_func_in_func` and `FunctionBuilder::new`
        // borrow `jit.func_ctx.func` mutably.
        let needed = crate::kernel_ir::collect_call_sites(kernel);
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
        )?;
        builder.finalize();
        // Hand `strings` back to the caller, which stores it in the
        // shared module's per-kernel cache entry — the JIT'd code
        // baked in `*const ArcStr` pointers into this table and the
        // shared module's code outlives this function.
        strings
    };
    jit.module
        .define_function(func_id, &mut jit.func_ctx)
        .context("define_function (shared body)")?;
    jit.module.clear_context(&mut jit.func_ctx);
    Ok(strings)
}

/// Define the (args*, out*) wrapper that adapts the typed kernel to a
/// uniform Rust-side calling convention. The wrapper:
/// 1. Loads each arg from the raw u64 slot at the correct CLIF type.
/// 2. Calls the typed kernel.
/// 3. Stores the result into the out slot as raw bits.
fn define_wrapper(
    jit: &mut JitCtx,
    kernel: &KirKernel,
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
    jit.func_ctx.func.signature = sig.clone();
    jit.func_ctx.func.name =
        cranelift_codegen::ir::UserFuncName::user(0, wrapper_id.as_u32());

    {
        let typed_ref = jit.module.declare_func_in_func(
            typed_func_id,
            &mut jit.func_ctx.func,
        );
        let mut b =
            FunctionBuilder::new(&mut jit.func_ctx.func, &mut jit.builder_ctx);
        let entry = b.create_block();
        b.append_block_params_for_function_params(entry);
        b.switch_to_block(entry);
        b.seal_block(entry);

        let args_ptr = b.block_params(entry)[0];
        let out_ptr = b.block_params(entry)[1];

        // Load each kernel param from args[i*8] at the appropriate
        // CLIF type. Scalar primitives load fewer-than-8 bytes from
        // the slot when applicable (upper bytes ignored). Composite
        // params (array/tuple/struct) load as i64 — the caller has
        // stored a `*const ValArray` cast to u64 in the slot.
        let total_params = kernel.params.len()
            + kernel.array_params.len()
            + kernel.tuple_params.len()
            + kernel.struct_params.len()
            + kernel.variant_params.len();
        let mut typed_args = Vec::with_capacity(total_params);
        let mut slot = 0;
        for p in kernel.params.iter() {
            let cty = prim_to_clif(p.prim);
            let offset = (slot as i32) * 8;
            let v = b.ins().load(cty, MemFlags::trusted(), args_ptr, offset);
            typed_args.push(v);
            slot += 1;
        }
        let n_composites = kernel.array_params.len()
            + kernel.tuple_params.len()
            + kernel.struct_params.len()
            + kernel.variant_params.len();
        for _ in 0..n_composites {
            let offset = (slot as i32) * 8;
            let v = b.ins().load(types::I64, MemFlags::trusted(), args_ptr, offset);
            typed_args.push(v);
            slot += 1;
        }

        let call = b.ins().call(typed_ref, &typed_args);
        let result = b.inst_results(call)[0];

        // Store result into *out. Width matches the kernel's return
        // type — the upper bytes of the slot stay whatever they were
        // (the caller knows the return type and reads the right
        // width).
        b.ins().store(MemFlags::trusted(), result, out_ptr, 0);
        b.ins().return_(&[]);

        b.seal_all_blocks();
        b.finalize();
    }

    jit.module
        .define_function(wrapper_id, &mut jit.func_ctx)
        .context("define_function (wrapper)")?;
    jit.module.clear_context(&mut jit.func_ctx);
    Ok(wrapper_id)
}

/// Pack a [`crate::kir_interp::RegValue`] into a u64 slot for passing
/// into a JIT'd wrapper. The bits represent the primitive's value;
/// for narrower primitives the upper bits are unused (the wrapper
/// loads at the CLIF type and ignores them).
pub fn pack_reg_to_u64(r: &crate::kir_interp::RegValue) -> u64 {
    use crate::kir_interp::RegValue as R;
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
/// appropriate [`crate::kir_interp::RegValue`] variant.
pub fn unpack_u64_to_reg(
    bits: u64,
    prim: PrimType,
) -> crate::kir_interp::RegValue {
    use crate::kir_interp::RegValue as R;
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
// the runtime path. KirNode holds an `Arc<AsyncJitSlot>` whose
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

/// Set-once JIT slot. KirNode reads via `fetch()` on every update
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

    /// Fast path checked on every KirNode update. Returns `None`
    /// while the worker is still compiling, `Some(_)` once the
    /// slot has been filled.
    pub fn fetch(&self) -> Option<std::sync::Arc<WrappedKernel>> {
        self.slot.get().cloned()
    }
}

struct CompileRequest {
    kernel: std::sync::Arc<crate::kernel_ir::KirKernel>,
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
                                "kir_jit (async): compile failed for {}: \
                                 {e:#}; KirNode will stay on interpreter",
                                req.name
                            );
                            // Slot stays empty — KirNode keeps
                            // interpreting forever. Acceptable
                            // graceful degradation.
                        }
                    }
                }
            })
            .expect("spawn kir_jit worker thread");
        tx
    });

/// Queue an async JIT compile of `kernel`. Returns immediately with
/// an empty [`AsyncJitSlot`]; the worker fills it when codegen
/// completes (typically tens to a few hundred ms later, depending
/// on kernel size). KirNode polls the slot on each update and swaps
/// from interpreter to native dispatch transparently.
pub fn submit_async_compile(
    kernel: std::sync::Arc<crate::kernel_ir::KirKernel>,
    name: arcstr::ArcStr,
) -> std::sync::Arc<AsyncJitSlot> {
    let slot = AsyncJitSlot::new();
    let req = CompileRequest { kernel, slot: slot.clone(), name };
    if JIT_WORKER.try_send(req).is_err() {
        // Channel full or worker dead. We log once (TODO: throttle)
        // and return the empty slot — KirNode just keeps interpreting.
        log::warn!(
            "kir_jit (async): worker queue full or thread gone; \
             skipping JIT for this kernel"
        );
    }
    slot
}

// ─── Function shape ──────────────────────────────────────────────

fn compile_into_function(
    b: &mut FunctionBuilder,
    kernel: &KirKernel,
    callee_refs: &BTreeMap<ArcStr, FuncRef>,
    helper_refs: &HelperRefs,
    strings: &KernelStrings,
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
    let mut next = 0usize;
    for p in kernel.params.iter() {
        let cty = prim_to_clif(p.prim);
        let var = b.declare_var(cty);
        b.def_var(var, initial_vals[next]);
        env.bind(p.name.clone(), var, p.prim);
        next += 1;
    }
    // Composite-typed params (array / tuple / struct) follow the
    // scalar params in the typed-kernel signature; each occupies one
    // pointer-sized slot. The CLIF Variable holds an OWNED `*mut
    // ValArray` (refcount-bumped from the caller's borrowed pointer
    // at entry). Going-owned at entry unifies the ownership story:
    //
    // - TupleNew / StructNew / ArrayInit produce owned pointers.
    // - Tail-call rebind drops the old owned value, stores the new
    //   owned value into the slot.
    // - Function exit drops all owned composite locals.
    //
    // The entry clone is one refcount bump per composite param per
    // kernel invocation — `triomphe::Arc` clone is a relaxed atomic
    // increment, ~ns-scale.
    let clone_helper = helper_refs
        .get("graphix_valarray_clone")
        .expect("graphix_valarray_clone helper must be registered");
    for p in kernel.array_params.iter() {
        let borrowed = initial_vals[next];
        let call = b.ins().call(clone_helper, &[borrowed]);
        let owned = b.inst_results(call)[0];
        let var = b.declare_var(types::I64);
        b.def_var(var, owned);
        env.bind_composite(p.name.clone(), var);
        next += 1;
    }
    for p in kernel.tuple_params.iter() {
        let borrowed = initial_vals[next];
        let call = b.ins().call(clone_helper, &[borrowed]);
        let owned = b.inst_results(call)[0];
        let var = b.declare_var(types::I64);
        b.def_var(var, owned);
        env.bind_composite(p.name.clone(), var);
        next += 1;
    }
    for p in kernel.struct_params.iter() {
        let borrowed = initial_vals[next];
        let call = b.ins().call(clone_helper, &[borrowed]);
        let owned = b.inst_results(call)[0];
        let var = b.declare_var(types::I64);
        b.def_var(var, owned);
        env.bind_composite(p.name.clone(), var);
        next += 1;
    }
    // Variant params: refcount-clone the caller's borrowed
    // `*const Value` into an owned `*mut Value` at entry, mirroring
    // the composite-param clone above. Going-owned uniformly means
    // `drop_owned_composites` / `emit_pending_cleanup` can drop
    // every `env.variants` entry unconditionally without having to
    // distinguish borrowed params from owned locals.
    let value_clone_helper = helper_refs
        .get("graphix_value_clone")
        .expect("graphix_value_clone helper must be registered");
    for p in kernel.variant_params.iter() {
        let borrowed = initial_vals[next];
        let call = b.ins().call(value_clone_helper, &[borrowed]);
        let owned = b.inst_results(call)[0];
        let var = b.declare_var(types::I64);
        b.def_var(var, owned);
        env.bind_variant(p.name.clone(), var);
        next += 1;
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
    // `KirNode::update` (which checks `DYNCALL_PENDING` after the
    // wrapper returns), so the sentinel value is never observed —
    // it just has to be a well-typed CLIF value of the right width.
    let pending_exit_block = *lower.pending_exit.borrow();
    if let Some(pe) = pending_exit_block {
        b.switch_to_block(pe);
        let sentinel = match &kernel.return_type {
            KirType::Prim(p) => zero_const(b, *p),
            // Composite / variant returns: the kernel's typed
            // signature returns an I64 pointer. The pending
            // sentinel is a null pointer.
            KirType::Array(_)
            | KirType::Tuple(_)
            | KirType::Struct(_)
            | KirType::Variant(_)
            // Unit returns the I64 ABI slot too (see
            // `define_typed_kernel`); the caller discards.
            | KirType::Unit => b.ins().iconst(types::I64, 0),
            // String-returning kernels never reach the JIT — guarded
            // by `kernel_contains_string`. If this fires, the routing
            // is broken.
            KirType::String => unreachable!(
                "kernel returns KirType::String but reached JIT \
                 pending-exit emission"
            ),
        };
        b.ins().return_(&[sentinel]);
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
/// global [`crate::kir_jit_intern`] table (which gives back a
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
    pub fn build(kernel: &KirKernel) -> Self {
        let mut unique: std::collections::BTreeSet<ArcStr> =
            std::collections::BTreeSet::new();
        for stmt in &kernel.body {
            collect_strings_stmt(stmt, &mut unique);
        }
        let mut slots: Vec<ArcStr> = Vec::with_capacity(unique.len());
        let mut index: BTreeMap<ArcStr, usize> = BTreeMap::new();
        for s in unique {
            let canonical = crate::kir_jit_intern::intern(&s);
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

fn collect_strings_stmt(s: &KirStmt, out: &mut std::collections::BTreeSet<ArcStr>) {
    match s {
        KirStmt::Let(l) => collect_strings_expr(&l.value, out),
        KirStmt::Return(e) => collect_strings_expr(e, out),
        KirStmt::Discard(e) => collect_strings_expr(e, out),
        KirStmt::TailCall { args } => {
            for a in args {
                collect_strings_expr(a, out);
            }
        }
        KirStmt::Select { arms } => {
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

fn collect_strings_expr(e: &KirExpr, out: &mut std::collections::BTreeSet<ArcStr>) {
    match &e.op {
        KirOp::Const(_) | KirOp::Local(_) => {}
        KirOp::ConstStr(s) => {
            // String-containing kernels are routed to interp via
            // `kernel_contains_string`; this branch shouldn't run.
            // Intern defensively in case routing changes.
            out.insert(s.clone());
        }
        KirOp::Concat(parts) => {
            for p in parts {
                collect_strings_expr(p, out);
            }
        }
        KirOp::Bin { lhs, rhs, .. }
        | KirOp::Cmp { lhs, rhs, .. }
        | KirOp::BoolBin { lhs, rhs, .. } => {
            collect_strings_expr(lhs, out);
            collect_strings_expr(rhs, out);
        }
        KirOp::Not(e) | KirOp::Cast { inner: e, .. } => {
            collect_strings_expr(e, out)
        }
        KirOp::Call { args, .. } | KirOp::DynCall { args, .. } => {
            for a in args {
                collect_strings_expr(a, out);
            }
        }
        KirOp::Block { lets, tail } => {
            for l in lets {
                collect_strings_expr(&l.value, out);
            }
            collect_strings_expr(tail, out);
        }
        KirOp::IfChain { arms } => {
            for (cond, body) in arms {
                if let Some(c) = cond {
                    collect_strings_expr(c, out);
                }
                collect_strings_expr(body, out);
            }
        }
        KirOp::ArrayLen { .. }
        | KirOp::ArrayGet { .. }
        | KirOp::TupleGet { .. }
        | KirOp::StructGet { .. }
        | KirOp::VariantPayload { .. } => {}
        KirOp::VariantTagEq { expected_tag, .. } => {
            // The expected tag is emitted as an iconst in codegen,
            // so the per-kernel KernelStrings must have it interned.
            out.insert(expected_tag.clone());
        }
        KirOp::ArrayFold { init, body, .. } => {
            collect_strings_expr(init, out);
            collect_strings_expr(body, out);
        }
        KirOp::ArrayInit { n, body, .. } => {
            collect_strings_expr(n, out);
            collect_strings_expr(body, out);
        }
        KirOp::ArrayMap { body, .. } => collect_strings_expr(body, out),
        KirOp::ArrayFilter { predicate, .. } => {
            collect_strings_expr(predicate, out)
        }
        KirOp::TupleNew { fields, .. } => {
            for f in fields {
                collect_strings_expr(f, out);
            }
        }
        KirOp::StructNew { sorted_fields, .. } => {
            for (name, e) in sorted_fields {
                out.insert(name.clone());
                collect_strings_expr(e, out);
            }
        }
        KirOp::VariantNew { tag, payloads, .. } => {
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
    /// `KirOp::Call { fn_name }` resolves through this map to a CLIF
    /// `FuncRef`. The caller must `declare_func_in_func` each callee's
    /// `FuncId` against the current function before constructing the
    /// FunctionBuilder, then pass the resulting refs in here. Empty
    /// for kernels with no `KirOp::Call` sites.
    callee_refs: &'a BTreeMap<ArcStr, FuncRef>,
    /// `FuncRef`s for the `kir_jit_helpers::*` runtime helpers.
    /// Declared in the current function before the FunctionBuilder
    /// is constructed (same constraint as `callee_refs`). Lookups
    /// are by helper name (e.g. `"graphix_valarray_get_i64"`).
    helper_refs: &'a HelperRefs,
    /// Per-source-position tail-call slot map (from
    /// `KirKernel::tail_call_slots`). Drives which Variable each
    /// tail-call arg rebinds into — scalar slots hit `env.locals`,
    /// composite slots hit `env.composites`. `None` for kernels
    /// without a tail loop (or that hand-built fixtures leave
    /// empty).
    tail_call_slots: Option<&'a [crate::kernel_ir::TailCallSlot]>,
    /// Per-kernel string table. StructNew / VariantNew codegen
    /// looks up field names / variant tags here to get stable
    /// `*const ArcStr` pointers to emit as iconst values. Lives
    /// on the resulting `WrappedKernel` so the pointers remain
    /// valid for as long as the compiled code does.
    strings: &'a KernelStrings,
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
        for (name, _ptr) in crate::kir_jit_helpers::all_symbols() {
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
        "graphix_value_buf_push_array"
        | "graphix_value_buf_push_value"
        | "graphix_value_buf_push_arcstr" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
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
        "graphix_valarray_drop" | "graphix_value_drop" => {
            sig.params.push(AbiParam::new(types::I64));
        }
        "graphix_value_new_from_array"
        | "graphix_value_new_string_from_arcstr"
        | "graphix_value_clone" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        // Variant consumer ops.
        "graphix_variant_tag_eq" => {
            // (v: *const Value = i64, expected: *const ArcStr = i64) -> u8
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I8));
        }
        "graphix_variant_payload_i64" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "graphix_variant_payload_f64" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::F64));
        }
        "graphix_variant_payload_i32"
        | "graphix_variant_payload_u32" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I32));
        }
        "graphix_variant_payload_f32" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::F32));
        }
        "graphix_variant_payload_bool"
        | "graphix_variant_payload_i8"
        | "graphix_variant_payload_u8" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I8));
        }
        "graphix_variant_payload_i16" | "graphix_variant_payload_u16" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I16));
        }
        "graphix_variant_payload_u64" => {
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        // DynCall dispatch:
        //   (fn_index: u32, args: *mut LPooled<Vec<Value>>, ret_kind: u8) -> u64
        "graphix_dyncall" => {
            sig.params.push(AbiParam::new(types::I32));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I8));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "graphix_dyncall_pending_take" => {
            sig.returns.push(AbiParam::new(types::I8));
        }
        "graphix_value_buf_push_array_borrowed"
        | "graphix_value_buf_push_value_borrowed" => {
            // (buf: *mut LPooled<Vec<Value>>, src: *const _) -> ()
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
        }
        "graphix_value_buf_drop" => {
            sig.params.push(AbiParam::new(types::I64));
        }
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
    /// Variant parameter pointer Variables. Separate from
    /// `composites` because the runtime representation is `Value`
    /// (`*const Value`), not `*const ValArray` — VariantTagEq /
    /// VariantPayload dispatch on the Value's outer shape.
    variants: Vec<(ArcStr, Variable)>,
}

impl JitEnv {
    fn new() -> Self {
        Self {
            locals: Vec::with_capacity(8),
            composites: Vec::new(),
            variants: Vec::new(),
        }
    }

    fn bind(&mut self, name: ArcStr, var: Variable, prim: PrimType) {
        self.locals.push((name, var, prim));
    }

    fn bind_composite(&mut self, name: ArcStr, var: Variable) {
        self.composites.push((name, var));
    }

    fn bind_variant(&mut self, name: ArcStr, var: Variable) {
        self.variants.push((name, var));
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

    fn lookup_variant(&self, name: &str) -> Option<Variable> {
        for (n, v) in self.variants.iter().rev() {
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
    /// scope-exit code (`KirOp::Block`) or the terminating statement
    /// (`KirStmt::Return` via `drop_owned_composites`); `truncate` is
    /// purely compile-time `env`-Vec hygiene.
    fn mark(&self) -> EnvMark {
        EnvMark {
            locals: self.locals.len(),
            composites: self.composites.len(),
            variants: self.variants.len(),
        }
    }

    fn truncate(&mut self, m: EnvMark) {
        self.locals.truncate(m.locals);
        self.composites.truncate(m.composites);
        self.variants.truncate(m.variants);
    }
}

/// A snapshot of a [`JitEnv`]'s binding-list lengths — see
/// [`JitEnv::mark`].
#[derive(Debug, Clone, Copy)]
struct EnvMark {
    locals: usize,
    composites: usize,
    variants: usize,
}

// ─── Body / statement compilation ────────────────────────────────

fn compile_body(
    b: &mut FunctionBuilder,
    stmts: &[KirStmt],
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
    for stmt in stmts {
        match stmt {
            KirStmt::Let(l) => {
                let v = compile_expr(b, &l.value, env, ctx)?;
                match &l.value.typ {
                    KirType::Prim(p) => {
                        let var = b.declare_var(prim_to_clif(*p));
                        b.def_var(var, v);
                        env.bind(l.local.clone(), var, *p);
                    }
                    KirType::Array(_)
                    | KirType::Tuple(_)
                    | KirType::Struct(_) => {
                        // Composite local: an owned `*mut ValArray`
                        // stored as I64. `ensure_owned_composite`
                        // clones a Borrowed source (e.g. `let a = b`
                        // aliasing another composite) so this local
                        // exclusively owns its buffer — otherwise
                        // `drop_owned_composites` at function exit
                        // would double-free. Dropped at function exit
                        // (and on pending paths by the per-DynCall
                        // `pre_pending` block).
                        let owned =
                            ensure_owned_composite(b, ctx, &l.value, v)?;
                        let var = b.declare_var(types::I64);
                        b.def_var(var, owned);
                        env.bind_composite(l.local.clone(), var);
                    }
                    KirType::Variant(_) => {
                        // Variant local: an owned `*mut Value`, same
                        // ownership discipline as the composite case.
                        let owned =
                            ensure_owned_composite(b, ctx, &l.value, v)?;
                        let var = b.declare_var(types::I64);
                        b.def_var(var, owned);
                        env.bind_variant(l.local.clone(), var);
                    }
                    // Unreachable in well-formed KIR — `emit_bind_stmt`
                    // routes Unit-typed lets through `KirStmt::Discard`.
                    KirType::Unit => {
                        return Err(anyhow!(
                            "KIR malformed: KirStmt::Let with Unit value"
                        ));
                    }
                    // String-typed lets shouldn't reach JIT — guarded
                    // by `kernel_contains_string`.
                    KirType::String => {
                        return Err(anyhow!(
                            "KirStmt::Let with String value reached JIT; \
                             should have routed to interp via \
                             kernel_contains_string"
                        ));
                    }
                }
            }
            KirStmt::Discard(e) => {
                // Evaluate for side effects, throw the SSA result
                // away. The expression's side-effecting machinery
                // (DynCalls, producer-op writes) already happened
                // during `compile_expr`; the unused SSA value just
                // gets DCE'd by cranelift.
                let _v = compile_expr(b, e, env, ctx)?;
            }
            KirStmt::Return(e) => {
                // A Borrowed composite return value (e.g. a `Local`
                // still owned by `env.composites`) must be cloned —
                // otherwise the `drop_owned_composites` below frees
                // the buffer the caller is about to receive. Owned
                // sources (producer ops, DynCall / Block / IfChain
                // results) pass through unchanged.
                let v = compile_expr(b, e, env, ctx)?;
                let v = ensure_owned_composite(b, ctx, e, v)?;
                drop_owned_composites(b, env, ctx)?;
                b.ins().return_(&[v]);
                return Ok(());
            }
            KirStmt::TailCall { args } => {
                let head = ctx.loop_head.ok_or_else(|| {
                    anyhow!("KIR malformed: TailCall in kernel without has_tail_loop")
                })?;
                // Evaluate every new arg into a CLIF SSA value first
                // (so an arg that reads an old param sees the old
                // value, not one we already overwrote).
                let mut new_vals = Vec::with_capacity(args.len());
                for a in args {
                    new_vals.push(compile_expr(b, a, env, ctx)?);
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
                use crate::kernel_ir::TailCallSlotKind;
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
                    }
                }
                // Reset env to the post-param snapshot so the next
                // iteration starts with a clean lexical state — any
                // block / select-arm locals (scalar, composite, or
                // variant) introduced this iteration are popped. The
                // composite/variant ones were already dropped at
                // runtime by their scope-exit code (`KirOp::Block`)
                // or terminating statement, so this is pure env-Vec
                // hygiene.
                env.truncate(ctx.param_mark);
                b.ins().jump(head, &[]);
                return Ok(());
            }
            KirStmt::Select { arms } => {
                compile_select_stmt(b, arms, env, ctx)?;
                // Each arm body terminates (return or tail-jump) so
                // there is no fallthrough after the select.
                return Ok(());
            }
        }
    }
    Err(anyhow!(
        "KIR malformed: body fell through with no return or tail call"
    ))
}

fn compile_select_stmt(
    b: &mut FunctionBuilder,
    arms: &[SelectArm],
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
    if arms.is_empty() {
        return Err(anyhow!("KIR malformed: empty select"));
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
                let cv = compile_expr(b, cond, env, ctx)?;
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

fn compile_expr(
    b: &mut FunctionBuilder,
    e: &KirExpr,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<ClifValue> {
    match &e.op {
        KirOp::Const(c) => Ok(compile_const(b, *c)),
        KirOp::ConstStr(_) | KirOp::Concat(_) => Err(anyhow!(
            "ConstStr/Concat are not JIT-compilable — the kernel should \
             have been routed to interp via `kernel_contains_string`"
        )),
        KirOp::Local(name) => {
            // Dispatch on the expression's KirType: scalar locals
            // sit in `env.locals`, composite locals (array/tuple/
            // struct pointers) in `env.composites`. The CLIF type
            // returned matches — scalars get their prim type,
            // composites get I64 (the pointer).
            match &e.typ {
                KirType::Prim(_) => {
                    let (var, _) = env.lookup(name).ok_or_else(|| {
                        anyhow!("KIR malformed: undefined scalar local `{name}`")
                    })?;
                    Ok(b.use_var(var))
                }
                KirType::Array(_)
                | KirType::Tuple(_)
                | KirType::Struct(_) => {
                    let var = env.lookup_composite(name).ok_or_else(|| {
                        anyhow!(
                            "KIR malformed: undefined composite local `{name}`"
                        )
                    })?;
                    Ok(b.use_var(var))
                }
                KirType::Variant(_) => {
                    let var = env.lookup_variant(name).ok_or_else(|| {
                        anyhow!(
                            "KIR malformed: undefined variant local `{name}`"
                        )
                    })?;
                    Ok(b.use_var(var))
                }
                KirType::Unit => Err(anyhow!(
                    "KIR malformed: Local `{name}` has Unit type"
                )),
                KirType::String => Err(anyhow!(
                    "Local `{name}` has String type; JIT cannot \
                     represent it — should have routed to interp"
                )),
            }
        }
        KirOp::Bin { op, lhs, rhs } => {
            let l = compile_expr(b, lhs, env, ctx)?;
            let r = compile_expr(b, rhs, env, ctx)?;
            Ok(compile_bin(b, *op, prim_of(&lhs.typ), l, r))
        }
        KirOp::Cmp { op, lhs, rhs } => {
            let l = compile_expr(b, lhs, env, ctx)?;
            let r = compile_expr(b, rhs, env, ctx)?;
            Ok(compile_cmp(b, *op, prim_of(&lhs.typ), l, r))
        }
        KirOp::BoolBin { op, lhs, rhs } => {
            // We use eager (non-short-circuit) evaluation here for
            // simplicity — KIR only ever emits BoolBin over pure
            // expressions, so eager eval has no observable effect.
            // The interpreter does short-circuit; the AOT-emitted
            // Rust `&&`/`||` short-circuit. No correctness diff for
            // pure code; if we ever fuse expressions with side-effects
            // we'll need to revisit.
            let l = compile_expr(b, lhs, env, ctx)?;
            let r = compile_expr(b, rhs, env, ctx)?;
            Ok(match op {
                BoolOp::And => b.ins().band(l, r),
                BoolOp::Or => b.ins().bor(l, r),
            })
        }
        KirOp::Not(inner) => {
            let v = compile_expr(b, inner, env, ctx)?;
            // Bool is I8 in CLIF; XOR with 1 flips the low bit.
            let one = b.ins().iconst(types::I8, 1);
            Ok(b.ins().bxor(v, one))
        }
        KirOp::Cast { inner, target } => {
            let v = compile_expr(b, inner, env, ctx)?;
            Ok(compile_cast(b, v, prim_of(&inner.typ), *target))
        }
        KirOp::Call { fn_name, args } => {
            // Cross-kernel call: look up the callee's `FuncRef` (the
            // caller declared it in the function via
            // `module.declare_func_in_func` before constructing the
            // builder). Empty `callee_refs` means this kernel was
            // compiled through the local-ctx path (which doesn't
            // support Calls); the build should have routed through
            // `compile_kernel_with_callees` instead.
            let func_ref = ctx.callee_refs.get(fn_name).ok_or_else(|| {
                anyhow!(
                    "KIR malformed: KirOp::Call to `{fn_name}` but \
                     callee_refs has no entry for it (forgot to use \
                     compile_kernel_with_callees?)"
                )
            })?;
            let mut clif_args = Vec::with_capacity(args.len());
            for a in args {
                clif_args.push(compile_expr(b, a, env, ctx)?);
            }
            let inst = b.ins().call(*func_ref, &clif_args);
            let results = b.inst_results(inst);
            if results.len() != 1 {
                return Err(anyhow!(
                    "KIR malformed: callee `{fn_name}` returned \
                     {} values; KIR expects exactly 1",
                    results.len()
                ));
            }
            Ok(results[0])
        }
        KirOp::Block { lets, tail } => {
            let mark = env.mark();
            for l in lets {
                let v = compile_expr(b, &l.value, env, ctx)?;
                match &l.value.typ {
                    KirType::Prim(p) => {
                        let var = b.declare_var(prim_to_clif(*p));
                        b.def_var(var, v);
                        env.bind(l.local.clone(), var, *p);
                    }
                    KirType::Array(_)
                    | KirType::Tuple(_)
                    | KirType::Struct(_) => {
                        // Owned `*mut ValArray`. `ensure_owned_composite`
                        // clones a Borrowed source so this block
                        // exclusively owns the local — otherwise the
                        // block-exit drop below would free a buffer the
                        // enclosing scope still holds.
                        let owned = ensure_owned_composite(
                            b, ctx, &l.value, v,
                        )?;
                        let var = b.declare_var(types::I64);
                        b.def_var(var, owned);
                        env.bind_composite(l.local.clone(), var);
                    }
                    KirType::Variant(_) => {
                        let owned = ensure_owned_composite(
                            b, ctx, &l.value, v,
                        )?;
                        let var = b.declare_var(types::I64);
                        b.def_var(var, owned);
                        env.bind_variant(l.local.clone(), var);
                    }
                    KirType::Unit => {
                        return Err(anyhow!(
                            "KIR malformed: KirOp::Block let with Unit value"
                        ));
                    }
                    KirType::String => {
                        return Err(anyhow!(
                            "KirOp::Block let with String value reached \
                             JIT; should have routed to interp via \
                             kernel_contains_string"
                        ));
                    }
                }
            }
            // The tail's value may alias a block-scoped composite
            // local we're about to drop (e.g. `tail = Local(block_let)`)
            // — `ensure_owned_composite` clones a Borrowed result so
            // the value handed back outlives the block.
            let result = compile_expr(b, tail, env, ctx)?;
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
            for (_, var) in &env.composites[mark.composites..] {
                let ptr = b.use_var(*var);
                b.ins().call(arr_drop, &[ptr]);
            }
            for (_, var) in &env.variants[mark.variants..] {
                let ptr = b.use_var(*var);
                b.ins().call(val_drop, &[ptr]);
            }
            env.truncate(mark);
            Ok(result)
        }
        KirOp::IfChain { arms } => {
            compile_ifchain(b, arms, clif_of(&e.typ), env, ctx)
        }
        KirOp::DynCall { fn_index, args, arg_types, return_type } => {
            // Marshal args into an `LPooled<Vec<Value>>` via the buf
            // helpers (scalar push_<T>, or borrow-mode push for
            // composite/variant args — the caller still owns its
            // local so we refcount-bump), then call `graphix_dyncall`
            // which indirects through the thread-local
            // DynDispatchHandle.
            //
            // The args buf is pushed onto `dyncall_buf_stack` for the
            // window it's in flight: if an arg expr is itself a
            // composite-return DynCall that pends, its `pre_pending`
            // block must drop THIS buf (an outer in-flight buf).
            let buf_new = ctx
                .helper_refs
                .get("graphix_value_buf_new")
                .ok_or_else(|| anyhow!("missing graphix_value_buf_new"))?;
            let dyncall = ctx
                .helper_refs
                .get("graphix_dyncall")
                .ok_or_else(|| anyhow!("missing graphix_dyncall"))?;
            let cap = b.ins().iconst(types::I64, args.len() as i64);
            let call = b.ins().call(buf_new, &[cap]);
            let buf = b.inst_results(call)[0];
            // Hold the buf in a Variable so a nested DynCall's
            // pre_pending block can `use_var` it across blocks.
            let buf_var = b.declare_var(types::I64);
            b.def_var(buf_var, buf);
            ctx.dyncall_buf_stack.borrow_mut().push(buf_var);
            for (a, t) in args.iter().zip(arg_types.iter()) {
                let v = compile_expr(b, a, env, ctx)?;
                // For composite args the push helper depends on where
                // the pointer came from. A `Borrowed` source (a Local
                // read — the caller still owns it) refcount-bumps; an
                // `Owned` source (an inline producer op, or a
                // composite-return DynCall result that isn't bound to
                // a local) transfers ownership into the buf. Using
                // the borrowed helper on an Owned source would leak
                // the original; using the move helper on a Borrowed
                // source would double-free it.
                let helper_name: &str = match t {
                    KirType::Prim(p) => value_buf_push_helper(*p)?,
                    KirType::Array(_)
                    | KirType::Tuple(_)
                    | KirType::Struct(_) => {
                        match classify_composite_source(a) {
                            CompositeSource::Owned => {
                                "graphix_value_buf_push_array"
                            }
                            CompositeSource::Borrowed => {
                                "graphix_value_buf_push_array_borrowed"
                            }
                        }
                    }
                    KirType::Variant(_) => {
                        match classify_composite_source(a) {
                            CompositeSource::Owned => {
                                "graphix_value_buf_push_value"
                            }
                            CompositeSource::Borrowed => {
                                "graphix_value_buf_push_value_borrowed"
                            }
                        }
                    }
                    KirType::Unit => {
                        return Err(anyhow!(
                            "KIR malformed: DynCall arg has Unit type"
                        ));
                    }
                    KirType::String => {
                        return Err(anyhow!(
                            "DynCall arg with String type reached JIT; \
                             should have routed to interp"
                        ));
                    }
                };
                let push =
                    ctx.helper_refs.get(helper_name).ok_or_else(|| {
                        anyhow!("missing push helper `{helper_name}`")
                    })?;
                b.ins().call(push, &[buf, v]);
            }
            let ret_kind: i64 = match return_type {
                KirType::Prim(_) => 0,
                KirType::Array(_) | KirType::Tuple(_) | KirType::Struct(_) => 1,
                KirType::Variant(_) => 2,
                // Unit return: dispatcher returns 0 (the slot is
                // discarded by the caller). ret_kind=3 tells
                // dispatch_typed not to box anything.
                KirType::Unit => 3,
                KirType::String => {
                    return Err(anyhow!(
                        "DynCall with String return reached JIT; \
                         should have routed to interp"
                    ));
                }
            };
            let fn_idx_val =
                b.ins().iconst(types::I32, *fn_index as i64);
            let ret_kind_val = b.ins().iconst(types::I8, ret_kind);
            let call =
                b.ins().call(dyncall, &[fn_idx_val, buf, ret_kind_val]);
            let raw_u64 = b.inst_results(call)[0];
            // `graphix_dyncall` consumed the args buf — pop it off
            // the in-flight stack before the (possible) pending
            // branch below, so a `pre_pending` block here doesn't
            // try to double-free it.
            ctx.dyncall_buf_stack.borrow_mut().pop();

            match return_type {
                KirType::Prim(p) => {
                    // Scalar return: 0 on pending is a harmless
                    // sentinel for downstream scalar arithmetic.
                    // No branch needed — the wrapper-level
                    // DYNCALL_PENDING check in KirNode::update
                    // discards the whole kernel result.
                    Ok(cast_u64_to_prim(b, raw_u64, *p))
                }
                KirType::Unit => {
                    // Unit return: dispatcher returned 0 (or could
                    // have set pending). The downstream `Discard`
                    // stmt throws raw_u64 away. Same harmless-zero
                    // semantics as the scalar path; the wrapper-
                    // level DYNCALL_PENDING check still fires.
                    Ok(raw_u64)
                }
                KirType::String => {
                    return Err(anyhow!(
                        "DynCall with String return reached JIT; \
                         should have routed to interp"
                    ));
                }
                KirType::Array(_)
                | KirType::Tuple(_)
                | KirType::Struct(_)
                | KirType::Variant(_) => {
                    // Composite return: `raw_u64` is an owned
                    // `*mut ValArray` / `*mut Value`, or null on
                    // pending. Null can't be deref'd by downstream
                    // ops, so we branch: on pending, drop every
                    // owned local + every outer in-flight DynCall
                    // buf, then jump to `pending_exit`.
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
                    // Lazily create the single per-function
                    // pending_exit block (body emitted at the end
                    // of compile_into_function).
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

                    // pre_pending: drop the owned set, jump to exit.
                    b.switch_to_block(pre_pending);
                    b.seal_block(pre_pending);
                    emit_pending_cleanup(b, env, ctx)?;
                    b.ins().jump(pending_exit, &[]);

                    // continue: result is the owned composite ptr.
                    b.switch_to_block(continue_block);
                    b.seal_block(continue_block);
                    Ok(raw_u64)
                }
            }
        }
        KirOp::ArrayLen { name } => {
            let helper = ctx.helper_refs.get("graphix_valarray_len").ok_or_else(
                || anyhow!("missing graphix_valarray_len helper"),
            )?;
            let arr_var = env.lookup_composite(name).ok_or_else(|| {
                anyhow!("KIR malformed: undefined composite param `{name}`")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let call = b.ins().call(helper, &[arr_ptr]);
            // Helper returns usize (i64); ArrayLen's KirExpr.typ is
            // Prim(U64), so width matches.
            Ok(b.inst_results(call)[0])
        }
        KirOp::ArrayGet { name, idx } => {
            // Composite-result ArrayGet routes to interp via the
            // `kernel_contains_composite_element_op` guard.
            let elem_p = e.typ.as_prim().ok_or_else(|| {
                anyhow!(
                    "ArrayGet with composite element type reached JIT; \
                     should have routed to interp via \
                     kernel_contains_composite_element_op"
                )
            })?;
            let helper_name = valarray_get_helper(elem_p)?;
            let helper =
                ctx.helper_refs.get(helper_name).ok_or_else(|| {
                    anyhow!("missing JIT helper `{helper_name}`")
                })?;
            let arr_var = env.lookup_composite(name).ok_or_else(|| {
                anyhow!("KIR malformed: undefined composite param `{name}`")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let idx_val = compile_expr(b, idx, env, ctx)?;
            // Helper expects usize — widen to i64 if the index
            // expression was narrower.
            let idx_i64 = widen_to_i64(b, idx_val, prim_of(&idx.typ));
            let call = b.ins().call(helper, &[arr_ptr, idx_i64]);
            Ok(b.inst_results(call)[0])
        }
        KirOp::TupleGet { name, idx, elem_typ } => {
            // Composite slots can't be JIT'd via the primitive
            // scalar helper — bail. Such kernels are guarded out
            // by `kernel_contains_composite_element_op` and routed
            // to the interpreter.
            let prim = elem_typ.as_prim().ok_or_else(|| {
                anyhow!(
                    "TupleGet with composite slot type reached JIT; \
                     should have routed to interp via \
                     kernel_contains_composite_element_op"
                )
            })?;
            let helper_name = valarray_get_helper(prim)?;
            let helper =
                ctx.helper_refs.get(helper_name).ok_or_else(|| {
                    anyhow!("missing JIT helper `{helper_name}`")
                })?;
            let arr_var = env.lookup_composite(name).ok_or_else(|| {
                anyhow!("KIR malformed: undefined tuple param `{name}`")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let idx_const = b.ins().iconst(types::I64, *idx as i64);
            let call = b.ins().call(helper, &[arr_ptr, idx_const]);
            Ok(b.inst_results(call)[0])
        }
        KirOp::StructGet { name, sorted_idx, elem_typ, .. } => {
            // Composite-field StructGet routes to interp via the
            // `kernel_contains_composite_element_op` guard.
            let prim = elem_typ.as_prim().ok_or_else(|| {
                anyhow!(
                    "StructGet with composite field type reached JIT; \
                     should have routed to interp via \
                     kernel_contains_composite_element_op"
                )
            })?;
            let helper_name = struct_get_helper(prim)?;
            let helper =
                ctx.helper_refs.get(helper_name).ok_or_else(|| {
                    anyhow!("missing JIT helper `{helper_name}`")
                })?;
            let arr_var = env.lookup_composite(name).ok_or_else(|| {
                anyhow!("KIR malformed: undefined struct param `{name}`")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let idx_const = b.ins().iconst(types::I64, *sorted_idx as i64);
            let call = b.ins().call(helper, &[arr_ptr, idx_const]);
            Ok(b.inst_results(call)[0])
        }
        KirOp::TupleNew { fields, elem_types } => {
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
            for (f, t) in fields.iter().zip(elem_types.iter()) {
                let push = ctx
                    .helper_refs
                    .get(value_buf_push_helper(*t)?)
                    .ok_or_else(|| {
                        anyhow!(
                            "missing JIT push helper for {t:?}"
                        )
                    })?;
                let v = compile_expr(b, f, env, ctx)?;
                b.ins().call(push, &[buf, v]);
            }
            let call = b.ins().call(finalize, &[buf]);
            Ok(b.inst_results(call)[0])
        }
        KirOp::StructNew { sorted_fields, sorted_types } => {
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
            for ((name, expr), (_, typ)) in
                sorted_fields.iter().zip(sorted_types.iter())
            {
                let inner_cap = b.ins().iconst(types::I64, 2);
                let call = b.ins().call(buf_new, &[inner_cap]);
                let inner = b.inst_results(call)[0];
                let name_ptr = ctx.strings.get(name) as i64;
                let name_ptr_val = b.ins().iconst(types::I64, name_ptr);
                b.ins().call(push_arcstr, &[inner, name_ptr_val]);
                let push = ctx
                    .helper_refs
                    .get(value_buf_push_helper(*typ)?)
                    .ok_or_else(|| {
                        anyhow!("missing JIT push helper for {typ:?}")
                    })?;
                let v = compile_expr(b, expr, env, ctx)?;
                b.ins().call(push, &[inner, v]);
                let call = b.ins().call(finalize, &[inner]);
                let inner_arr = b.inst_results(call)[0];
                b.ins().call(push_array, &[outer, inner_arr]);
            }
            let call = b.ins().call(finalize, &[outer]);
            Ok(b.inst_results(call)[0])
        }
        KirOp::VariantNew { tag, payloads, payload_types } => {
            // Nullary variant → Value::String(tag) directly.
            // With-payload variant → Value::Array([tag, p0, ...]).
            //
            // The result of VariantNew is a `*mut Value`, NOT a
            // `*mut ValArray`. We emit an I64 CLIF value either way
            // (always a pointer), and downstream consumers / the
            // composite-return ABI dispatch on KirType::Variant.
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
                Ok(b.inst_results(call)[0])
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
                for (p, t) in payloads.iter().zip(payload_types.iter()) {
                    let push = ctx
                        .helper_refs
                        .get(value_buf_push_helper(*t)?)
                        .ok_or_else(|| {
                            anyhow!("missing push helper for {t:?}")
                        })?;
                    let v = compile_expr(b, p, env, ctx)?;
                    b.ins().call(push, &[buf, v]);
                }
                let call = b.ins().call(finalize, &[buf]);
                let arr = b.inst_results(call)[0];
                let call = b.ins().call(wrap_array, &[arr]);
                Ok(b.inst_results(call)[0])
            }
        }
        KirOp::ArrayInit { n, idx_local, elem_typ, body } => {
            // n_val = eval n.    Scalar I64-ish; widen if narrower.
            // buf = buf_new(n_val)
            // loop i in 0..n_val:
            //     bind idx_local to i in env
            //     elem = eval body
            //     call push_<T>(buf, elem)
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
            let push = ctx
                .helper_refs
                .get(value_buf_push_helper(*elem_typ)?)
                .ok_or_else(|| {
                    anyhow!("missing push helper for {elem_typ:?}")
                })?;
            let n_raw = compile_expr(b, n, env, ctx)?;
            let n_val = widen_to_i64(b, n_raw, prim_of(&n.typ));
            let call = b.ins().call(buf_new, &[n_val]);
            let buf = b.inst_results(call)[0];
            // Loop counter `i` as a CLIF Variable so the loop-back
            // edge can update it. CLIF auto-inserts the SSA phi.
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
            let elem = compile_expr(b, body, env, ctx)?;
            env.truncate(mark);
            b.ins().call(push, &[buf, elem]);
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
            let call = b.ins().call(finalize, &[buf]);
            Ok(b.inst_results(call)[0])
        }
        KirOp::ArrayMap { array, in_elem, elem_local, out_elem, body } => {
            // Iterate over an existing composite param/local.
            //   arr_ptr = lookup_composite(array)
            //   len = call valarray_len(arr_ptr)
            //   buf = call buf_new(len)
            //   loop i in 0..len:
            //     elem = call valarray_get_<in_elem>(arr_ptr, i)
            //     bind elem_local = elem
            //     out_val = eval body
            //     call push_<out_elem>(buf, out_val)
            //   finalize(buf)
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
                .ok_or_else(|| {
                    anyhow!("missing push helper for {out_elem:?}")
                })?;
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("KIR malformed: undefined array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let call = b.ins().call(len_helper, &[arr_ptr]);
            let len = b.inst_results(call)[0];
            let call = b.ins().call(buf_new, &[len]);
            let buf = b.inst_results(call)[0];
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
            let call = b.ins().call(get_helper, &[arr_ptr, i_now]);
            let elem = b.inst_results(call)[0];
            let elem_var = b.declare_var(prim_to_clif(*in_elem));
            b.def_var(elem_var, elem);
            let mark = env.mark();
            env.bind(elem_local.clone(), elem_var, *in_elem);
            let out_val = compile_expr(b, body, env, ctx)?;
            env.truncate(mark);
            b.ins().call(push, &[buf, out_val]);
            let one = b.ins().iconst(types::I64, 1);
            let i_next = b.ins().iadd(i_now, one);
            b.def_var(i_var, i_next);
            b.ins().jump(loop_header, &[]);
            b.seal_block(loop_body);
            b.seal_block(loop_header);
            b.switch_to_block(loop_exit);
            b.seal_block(loop_exit);
            let call = b.ins().call(finalize, &[buf]);
            Ok(b.inst_results(call)[0])
        }
        KirOp::ArrayFilter { array, elem, elem_local, predicate } => {
            // Same shape as ArrayMap but with a conditional push.
            //   loop i in 0..len:
            //     elem = get(arr, i)
            //     bind elem_local
            //     keep = eval predicate (Bool, CLIF I8)
            //     if keep: push elem; else continue
            //     i += 1
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
                .get(valarray_get_helper(*elem)?)
                .ok_or_else(|| anyhow!("missing valarray_get helper"))?;
            let push = ctx
                .helper_refs
                .get(value_buf_push_helper(*elem)?)
                .ok_or_else(|| {
                    anyhow!("missing push helper for {elem:?}")
                })?;
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("KIR malformed: undefined array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let call = b.ins().call(len_helper, &[arr_ptr]);
            let len = b.inst_results(call)[0];
            let call = b.ins().call(buf_new, &[len]);
            let buf = b.inst_results(call)[0];
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
            let elem_var = b.declare_var(prim_to_clif(*elem));
            b.def_var(elem_var, elem_val);
            let mark = env.mark();
            env.bind(elem_local.clone(), elem_var, *elem);
            let keep = compile_expr(b, predicate, env, ctx)?;
            env.truncate(mark);
            b.ins().brif(keep, push_block, &[], advance, &[]);
            b.switch_to_block(push_block);
            let elem_again = b.use_var(elem_var);
            b.ins().call(push, &[buf, elem_again]);
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
            let call = b.ins().call(finalize, &[buf]);
            Ok(b.inst_results(call)[0])
        }
        KirOp::ArrayFold { array, elem_typ, init, acc_local, elem_local, body } => {
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
            let get_helper = ctx
                .helper_refs
                .get(valarray_get_helper(*elem_typ)?)
                .ok_or_else(|| anyhow!("missing valarray_get helper"))?;
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("KIR malformed: undefined fold array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let call = b.ins().call(len_helper, &[arr_ptr]);
            let len = b.inst_results(call)[0];
            let acc_var = b.declare_var(prim_to_clif(acc_prim));
            let init_val = compile_expr(b, init, env, ctx)?;
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
            let call = b.ins().call(get_helper, &[arr_ptr, i_now]);
            let elem_val = b.inst_results(call)[0];
            let elem_var = b.declare_var(prim_to_clif(*elem_typ));
            b.def_var(elem_var, elem_val);
            // Bind acc and elem locals so the body's KirOp::Local
            // resolutions work. The order matches the interp:
            // acc first then elem (KIR construction pins this).
            let mark = env.mark();
            env.bind(acc_local.clone(), acc_var, acc_prim);
            env.bind(elem_local.clone(), elem_var, *elem_typ);
            let new_acc = compile_expr(b, body, env, ctx)?;
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
        KirOp::VariantTagEq { name, expected_tag } => {
            let helper = ctx
                .helper_refs
                .get("graphix_variant_tag_eq")
                .ok_or_else(|| anyhow!("missing graphix_variant_tag_eq"))?;
            let var = env.lookup_variant(name).ok_or_else(|| {
                anyhow!("KIR malformed: undefined variant `{name}`")
            })?;
            let v_ptr = b.use_var(var);
            let tag_ptr = ctx.strings.get(expected_tag) as i64;
            let tag_val = b.ins().iconst(types::I64, tag_ptr);
            let call = b.ins().call(helper, &[v_ptr, tag_val]);
            Ok(b.inst_results(call)[0])
        }
        KirOp::VariantPayload { name, payload_idx, elem_typ } => {
            let helper_name = variant_payload_helper(*elem_typ)?;
            let helper = ctx
                .helper_refs
                .get(helper_name)
                .ok_or_else(|| anyhow!("missing helper `{helper_name}`"))?;
            let var = env.lookup_variant(name).ok_or_else(|| {
                anyhow!("KIR malformed: undefined variant `{name}`")
            })?;
            let v_ptr = b.use_var(var);
            let idx_const =
                b.ins().iconst(types::I64, *payload_idx as i64);
            let call = b.ins().call(helper, &[v_ptr, idx_const]);
            Ok(b.inst_results(call)[0])
        }
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
            b.ins().bitcast(types::F32, MemFlags::trusted(), bits32)
        }
        PrimType::F64 => b.ins().bitcast(types::F64, MemFlags::trusted(), raw),
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

fn classify_composite_source(e: &KirExpr) -> CompositeSource {
    match &e.op {
        // Producer ops yield owned pointers.
        KirOp::TupleNew { .. }
        | KirOp::StructNew { .. }
        | KirOp::ArrayInit { .. }
        | KirOp::ArrayMap { .. }
        | KirOp::ArrayFilter { .. }
        | KirOp::VariantNew { .. } => CompositeSource::Owned,
        // A composite-return DynCall hands back an owned `*mut
        // ValArray` / `*mut Value` (`Box::into_raw` of a fresh box in
        // `dispatch_typed`). Treating it as Borrowed would
        // refcount-clone-then-leak the original.
        KirOp::DynCall { .. } => CompositeSource::Owned,
        // `KirOp::Block` and `KirOp::IfChain` both run their composite
        // result through `ensure_owned_composite` before handing it
        // out (the Block at its tail, the IfChain at each arm), so by
        // the time the value reaches a consumer it's always owned.
        KirOp::Block { .. } | KirOp::IfChain { .. } => {
            CompositeSource::Owned
        }
        // Anything else (Local reads, etc.) we conservatively treat as
        // borrowed. False positives just cost one extra refcount bump
        // — never an unsoundness.
        _ => CompositeSource::Borrowed,
    }
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
/// composite hand-off site uses — `KirStmt::Let`, `KirStmt::Return`,
/// the `KirOp::Block` tail, and each `compile_ifchain` arm.
fn ensure_owned_composite(
    b: &mut FunctionBuilder,
    ctx: &LowerCtx,
    e: &KirExpr,
    v: ClifValue,
) -> Result<ClifValue> {
    if matches!(e.typ, KirType::Prim(_) | KirType::Unit)
        || classify_composite_source(e) == CompositeSource::Owned
    {
        return Ok(v);
    }
    let helper_name = match &e.typ {
        KirType::Variant(_) => "graphix_value_clone",
        KirType::Array(_) | KirType::Tuple(_) | KirType::Struct(_) => {
            "graphix_valarray_clone"
        }
        KirType::Prim(_) | KirType::Unit => unreachable!("guarded above"),
        KirType::String => {
            return Err(anyhow!(
                "ensure_owned_composite reached for String type — \
                 kernel should have been routed to interp"
            ));
        }
    };
    let helper = ctx
        .helper_refs
        .get(helper_name)
        .ok_or_else(|| anyhow!("missing helper `{helper_name}`"))?;
    let call = b.ins().call(helper, &[v]);
    Ok(b.inst_results(call)[0])
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
    // Variant params + locals are also owned (params are
    // refcount-cloned on kernel entry, locals come from VariantNew
    // or composite-return DynCall). Drop them too.
    for (_, var) in &env.variants {
        let ptr = b.use_var(*var);
        b.ins().call(val_drop, &[ptr]);
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
    // Owned composite + variant locals (and entry-cloned params).
    drop_owned_composites(b, env, ctx)
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
            panic!("widen_to_i64: float index — KIR malformed")
        }
    }
}

fn compile_ifchain(
    b: &mut FunctionBuilder,
    arms: &[(Option<KirExpr>, KirExpr)],
    result_clif: ClifType,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<ClifValue> {
    if arms.is_empty() {
        return Err(anyhow!("KIR malformed: empty if-chain"));
    }
    // Merge block holds the result via a block parameter. For a
    // composite-typed if-chain `result_clif` is `I64` — the merged
    // value is an owned `*mut ValArray` / `*mut Value` (each arm runs
    // its result through `ensure_owned_composite` so the merge always
    // receives an owned pointer regardless of which arm won).
    let merge = b.create_block();
    b.append_block_param(merge, result_clif);

    for (i, (cond, body)) in arms.iter().enumerate() {
        let is_last = i == arms.len() - 1;
        let body_block = b.create_block();
        let next_block: Option<Block> = match cond {
            None => {
                b.ins().jump(body_block, &[]);
                None
            }
            Some(c) => {
                let cv = compile_expr(b, c, env, ctx)?;
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
        let v = compile_expr(b, body, env, ctx)?;
        // A composite arm result must be owned before it crosses the
        // merge — otherwise an arm that yields a `Borrowed` pointer
        // (e.g. `Local(outer_composite)`) and an arm that yields an
        // `Owned` one would disagree on who frees the merged value.
        let v = ensure_owned_composite(b, ctx, body, v)?;
        env.truncate(mark);
        b.ins().jump(merge, &[BlockArg::Value(v)]);
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
    Ok(b.block_params(merge)[0])
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
/// `KirNode::update` discards the result on the pending path — but
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
) -> ClifValue {
    if typ.is_integer() {
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
                // Cranelift has no `frem`; the Rust `%` on f64 calls
                // libm `fmod`. Until we wire libcalls in, this is a
                // gap — emit a trap so the failure is visible. Fused
                // kernels for mandelbrot / fib / arith corpus don't
                // hit this path.
                let _ = (l, r);
                b.ins().trap(cranelift_codegen::ir::TrapCode::user(3).unwrap());
                // Return a dummy value of the right type — the trap
                // ensures we never actually use it.
                if typ == PrimType::F32 {
                    b.ins().f32const(0.0f32)
                } else {
                    b.ins().f64const(0.0f64)
                }
            }
        }
    }
}

fn compile_cmp(
    b: &mut FunctionBuilder,
    op: CmpOp,
    operand_typ: PrimType,
    l: ClifValue,
    r: ClifValue,
) -> ClifValue {
    if operand_typ.is_float() {
        let cc = match op {
            CmpOp::Eq => FloatCC::Equal,
            CmpOp::Ne => FloatCC::NotEqual,
            CmpOp::Lt => FloatCC::LessThan,
            CmpOp::Gt => FloatCC::GreaterThan,
            CmpOp::Lte => FloatCC::LessThanOrEqual,
            CmpOp::Gte => FloatCC::GreaterThanOrEqual,
        };
        b.ins().fcmp(cc, l, r)
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
        // bool ↔ integer/float — kernel_ir::cast refuses these at
        // construction; reaching this branch means something built a
        // malformed KirOp::Cast bypassing the constructor.
        unreachable!("compile_cast: bool casts should be rejected at IR construction");
    }
}

// ─── Type plumbing ───────────────────────────────────────────────

/// JIT v1 is scalar-only. Every `KirExpr.typ` we encounter inside a
/// JIT-compilable kernel must be a primitive — array ops route to the
/// interpreter via the explicit error arm in `compile_expr`. This
/// helper crashes loudly on misuse rather than silently producing
/// nonsense lowering.
fn prim_of(t: &KirType) -> PrimType {
    t.as_prim().expect("JIT scalar-only: array typ slipped past compile_expr")
}

/// CLIF type of a [`KirType`] at the JIT ABI level. Primitives map to
/// their natural CLIF type; every composite (array/tuple/struct/
/// variant) is a pointer, i.e. `I64` on a 64-bit target.
fn clif_of(t: &KirType) -> ClifType {
    match t {
        KirType::Prim(p) => prim_to_clif(*p),
        KirType::Array(_)
        | KirType::Tuple(_)
        | KirType::Struct(_)
        | KirType::Variant(_) => types::I64,
        // Unit at the ABI is just a zero pointer slot — see
        // `define_typed_kernel`'s return_clif handling.
        KirType::Unit => types::I64,
        // String should never reach JIT (guarded by
        // `kernel_contains_string`); ABI it as a pointer to keep this
        // helper total in case `clif_of` is called defensively.
        KirType::String => types::I64,
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
    use crate::kernel_ir::{
        arith, bool_op, cast, cmp, const_expr, local, BinOp, BoolOp, CmpOp,
        ConstVal, Input, KirExpr, KirKernel, KirOp, KirStmt, KirType, Let,
        PrimType, SelectArm,
    };

    fn input(name: &str, prim: PrimType) -> Input {
        Input {
            name: ArcStr::from(name),
            prim,
            bind_id: None,
        }
    }

    fn loc(name: &str, prim: PrimType) -> KirExpr {
        local(ArcStr::from(name), prim)
    }

    /// Compile a kernel and return the function pointer typed as a
    /// transmute-ready raw pointer. Tests cast to the appropriate
    /// extern "C" fn signature.
    fn jit(kernel: &KirKernel) -> (JitCtx, *const u8) {
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
        let kernel = KirKernel {
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
            tail_call_slots: vec![],
            return_type: KirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![KirStmt::Return(body)],
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
        let block = KirExpr {
            op: KirOp::Block {
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
            typ: KirType::Prim(PrimType::F64),
        };
        let kernel = KirKernel {
            fn_name: ArcStr::from("scaled"),
            params: vec![input("a", PrimType::F64), input("b", PrimType::F64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            tail_call_slots: vec![],
            return_type: KirType::Prim(PrimType::F64),
            has_tail_loop: false,
            body: vec![KirStmt::Return(block)],
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
        let kernel = KirKernel {
            fn_name: ArcStr::from("range_check"),
            params: vec![input("x", PrimType::F64), input("y", PrimType::F64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            tail_call_slots: vec![],
            return_type: KirType::Prim(PrimType::Bool),
            has_tail_loop: false,
            body: vec![KirStmt::Return(body)],
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
        let kernel = KirKernel {
            fn_name: ArcStr::from("itof"),
            params: vec![input("i", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            tail_call_slots: vec![],
            return_type: KirType::Prim(PrimType::F64),
            has_tail_loop: false,
            body: vec![KirStmt::Return(body)],
        };
        let (_ctx, p) = jit(&kernel);
        let f: extern "C" fn(i64) -> f64 = unsafe { std::mem::transmute(p) };
        assert_eq!(f(42), 42.0);
        assert_eq!(f(-7), -7.0);
    }

    /// Hand-build mandelbrot iterate, same as the interpreter test,
    /// then JIT it and verify expected outputs.
    fn mandelbrot_iterate_kernel() -> KirKernel {
        let arm0 = SelectArm {
            cond: Some(
                cmp(
                    loc("i", PrimType::I64),
                    const_expr(ConstVal::I64(0)),
                    CmpOp::Eq,
                )
                .unwrap(),
            ),
            body: vec![KirStmt::Return(const_expr(ConstVal::I64(0)))],
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
            body: vec![KirStmt::Return(loc("i", PrimType::I64))],
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
            body: vec![KirStmt::TailCall {
                args: vec![
                    new_zr,
                    new_zi,
                    loc("cr", PrimType::F64),
                    loc("ci", PrimType::F64),
                    new_i,
                ],
            }],
        };
        KirKernel {
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
            tail_call_slots: vec![],
            return_type: KirType::Prim(PrimType::I64),
            has_tail_loop: true,
            body: vec![KirStmt::Select { arms: vec![arm0, arm1, arm2] }],
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
            body: vec![KirStmt::Return(const_expr(ConstVal::I64(0)))],
        };
        let arm1 = SelectArm {
            cond: None,
            body: vec![KirStmt::TailCall {
                args: vec![arith(
                    loc("n", PrimType::I64),
                    const_expr(ConstVal::I64(1)),
                    BinOp::Sub,
                )
                .unwrap()],
            }],
        };
        let kernel = KirKernel {
            fn_name: ArcStr::from("countdown"),
            params: vec![input("n", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            tail_call_slots: vec![],
            return_type: KirType::Prim(PrimType::I64),
            has_tail_loop: true,
            body: vec![KirStmt::Select { arms: vec![arm0, arm1] }],
        };
        let (_ctx, p) = jit(&kernel);
        let f: extern "C" fn(i64) -> i64 = unsafe { std::mem::transmute(p) };
        assert_eq!(f(1_000_000), 0);
    }

    #[test]
    fn jit_ifchain_expression() {
        // |x| if x > 0 { 1 } else if x < 0 { -1 } else { 0 }
        let chain = KirExpr {
            op: KirOp::IfChain {
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
            typ: KirType::Prim(PrimType::I64),
        };
        let kernel = KirKernel {
            fn_name: ArcStr::from("sign"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            tail_call_slots: vec![],
            return_type: KirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![KirStmt::Return(chain)],
        };
        let (_ctx, p) = jit(&kernel);
        let f: extern "C" fn(i64) -> i64 = unsafe { std::mem::transmute(p) };
        assert_eq!(f(5), 1);
        assert_eq!(f(-3), -1);
        assert_eq!(f(0), 0);
    }

    #[test]
    fn wrapper_round_trips_mandelbrot() {
        // Compile the mandelbrot iterate kernel through the
        // wrapper-generating path. Pack args as u64 slots, call
        // through the canonical WrapperFn, unpack the result.
        // Verifies the (args*, out*) ABI works for mixed f64/i64
        // signatures with a tail-recursive body.
        use crate::kir_interp::RegValue;
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
        let kernel = KirKernel {
            fn_name: ArcStr::from("inc"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            tail_call_slots: vec![],
            return_type: KirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![KirStmt::Return(body)],
        };
        let (_ctx, p) = jit(&kernel);
        let f: extern "C" fn(i64) -> i64 = unsafe { std::mem::transmute(p) };
        assert_eq!(f(i64::MAX), i64::MIN);
    }

    /// Cross-kernel CLIF call via the shared module path. The parent
    /// kernel `caller` invokes `square(x) + 10`. Verifies that the
    /// `KirOp::Call` lowering threads correctly through the shared JIT
    /// module and that the wrapper still yields the right end-to-end
    /// result.
    #[test]
    fn shared_module_cross_kernel_call() {
        use crate::kir_interp::RegValue;
        use std::sync::Arc;
        // square(x: i64) -> i64 { x * x }
        let square = Arc::new(KirKernel {
            fn_name: ArcStr::from("square"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            tail_call_slots: vec![],
            return_type: KirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![KirStmt::Return(
                arith(
                    loc("x", PrimType::I64),
                    loc("x", PrimType::I64),
                    BinOp::Mul,
                )
                .unwrap(),
            )],
        });
        // caller(n: i64) -> i64 { square(n) + 10 }
        let call_expr = KirExpr {
            op: KirOp::Call {
                fn_name: ArcStr::from("square"),
                args: vec![loc("n", PrimType::I64)],
            },
            typ: KirType::Prim(PrimType::I64),
        };
        let body = arith(call_expr, const_expr(ConstVal::I64(10)), BinOp::Add)
            .unwrap();
        let caller = Arc::new(KirKernel {
            fn_name: ArcStr::from("caller"),
            params: vec![input("n", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            tail_call_slots: vec![],
            return_type: KirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![KirStmt::Return(body)],
        });
        let mut callees = BTreeMap::new();
        callees.insert(ArcStr::from("square"), square);
        let wrapped = compile_kernel_with_callees(&caller, &callees)
            .expect("compile_kernel_with_callees");
        let f = unsafe { wrapped.fn_ptr() };
        let args = [pack_reg_to_u64(&RegValue::I64(7))];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_reg(out, caller.return_type.as_prim().unwrap()), RegValue::I64(59));
    }

    /// Non-tail self-recursion via `KirOp::Call` (e.g. naive fib's
    /// `fib(n-1) + fib(n-2)`). The shared-module path declares the
    /// parent's `FuncId` first, then routes any Call site whose name
    /// matches the parent back to that FuncId. Verifies the wrapper
    /// returns the right answer for a small fib input.
    #[test]
    fn shared_module_self_recursion() {
        use crate::kir_interp::RegValue;
        use std::sync::Arc;
        // fib(n) = if n < 2 { n } else { fib(n-1) + fib(n-2) }
        let fib_call = |which: i64| KirExpr {
            op: KirOp::Call {
                fn_name: ArcStr::from("fib"),
                args: vec![arith(
                    loc("n", PrimType::I64),
                    const_expr(ConstVal::I64(which)),
                    BinOp::Sub,
                )
                .unwrap()],
            },
            typ: KirType::Prim(PrimType::I64),
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
                body: vec![KirStmt::Return(loc("n", PrimType::I64))],
            },
            SelectArm {
                cond: None,
                body: vec![KirStmt::Return(recursive_body)],
            },
        ];
        let fib = Arc::new(KirKernel {
            fn_name: ArcStr::from("fib"),
            params: vec![input("n", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            tail_call_slots: vec![],
            return_type: KirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![KirStmt::Select { arms }],
        });
        // Lazy fusion would put the parent's own kernel in the
        // registry to support self-recursion in the interpreter.
        // Mirror that — the JIT path skips it and uses the parent's
        // own FuncId.
        let mut callees = BTreeMap::new();
        callees.insert(ArcStr::from("fib"), fib.clone());
        let wrapped = compile_kernel_with_callees(&fib, &callees)
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
        use crate::kir_interp::RegValue;
        use std::sync::Arc;
        // leaf(x) = x + 1
        let leaf = Arc::new(KirKernel {
            fn_name: ArcStr::from("leaf"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            tail_call_slots: vec![],
            return_type: KirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![KirStmt::Return(
                arith(
                    loc("x", PrimType::I64),
                    const_expr(ConstVal::I64(1)),
                    BinOp::Add,
                )
                .unwrap(),
            )],
        });
        // middle(x) = leaf(x) * 2
        let middle = Arc::new(KirKernel {
            fn_name: ArcStr::from("middle"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            tail_call_slots: vec![],
            return_type: KirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![KirStmt::Return(
                arith(
                    KirExpr {
                        op: KirOp::Call {
                            fn_name: ArcStr::from("leaf"),
                            args: vec![loc("x", PrimType::I64)],
                        },
                        typ: KirType::Prim(PrimType::I64),
                    },
                    const_expr(ConstVal::I64(2)),
                    BinOp::Mul,
                )
                .unwrap(),
            )],
        });
        // outer(x) = middle(x) - 3
        let outer = Arc::new(KirKernel {
            fn_name: ArcStr::from("outer"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            tail_call_slots: vec![],
            return_type: KirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![KirStmt::Return(
                arith(
                    KirExpr {
                        op: KirOp::Call {
                            fn_name: ArcStr::from("middle"),
                            args: vec![loc("x", PrimType::I64)],
                        },
                        typ: KirType::Prim(PrimType::I64),
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
        let wrapped = compile_kernel_with_callees(&outer, &callees)
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
        use crate::kir_interp::RegValue;
        use std::sync::Arc;
        let square = Arc::new(KirKernel {
            fn_name: ArcStr::from("square"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            tail_call_slots: vec![],
            return_type: KirType::Prim(PrimType::I64),
            has_tail_loop: false,
            body: vec![KirStmt::Return(
                arith(
                    loc("x", PrimType::I64),
                    loc("x", PrimType::I64),
                    BinOp::Mul,
                )
                .unwrap(),
            )],
        });
        let make_caller = |add_const: i64| {
            let call_expr = KirExpr {
                op: KirOp::Call {
                    fn_name: ArcStr::from("square"),
                    args: vec![loc("n", PrimType::I64)],
                },
                typ: KirType::Prim(PrimType::I64),
            };
            let body = arith(
                call_expr,
                const_expr(ConstVal::I64(add_const)),
                BinOp::Add,
            )
            .unwrap();
            Arc::new(KirKernel {
                fn_name: ArcStr::from("caller"),
                params: vec![input("n", PrimType::I64)],
                fn_params: vec![],
                array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            tail_call_slots: vec![],
                return_type: KirType::Prim(PrimType::I64),
                has_tail_loop: false,
                body: vec![KirStmt::Return(body)],
            })
        };
        let caller_a = make_caller(1);
        let caller_b = make_caller(100);
        let mut callees = BTreeMap::new();
        callees.insert(ArcStr::from("square"), square.clone());
        let wa = compile_kernel_with_callees(&caller_a, &callees)
            .expect("compile a");
        let wb = compile_kernel_with_callees(&caller_b, &callees)
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
}
