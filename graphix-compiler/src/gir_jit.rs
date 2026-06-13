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
//! and the runtime arg packer in `gir_interp`). Per-kind wire shape:
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

use crate::gir::{
    self, AbiKind, AbiParamKind, AbiReturn, BinOp, BoolOp, CmpOp, KernelSig,
    PrimType,
};
use crate::typ::Type;
// Emission calls `Update` trait methods (`typ`, `view`, `emit_clif`)
// on region-root Nodes.
#[allow(unused_imports)]
use crate::Update as _;
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

/// The HOF loop scaffolds (`emit_map_loop` & co.) shared by the GIR
/// arms below and the direct node path's HOF emitters (Stage D2 of
/// `design/distributed_jit.md`).
#[path = "gir_jit_scaffold.rs"]
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
fn push_abi_returns(sig: &mut Signature, kernel: &KernelSig) -> Result<()> {
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

/// When the `GRAPHIX_DUMP_CLIF` env var is set, print the just-built
/// CLIF function to stderr with a `;; clif <label>` header. The dump
/// runs after `FunctionBuilder::finalize` but before
/// `define_function`, so it shows exactly what emission produced.
/// Baked pointer constants (interned strings/values) vary run-to-run;
/// normalize large `iconst` immediates before diffing two captures.
fn maybe_dump_clif(func: &cranelift_codegen::ir::Function, label: &str) {
    static DUMP: std::sync::LazyLock<bool> = std::sync::LazyLock::new(|| {
        std::env::var_os("GRAPHIX_DUMP_CLIF").is_some()
    });
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

// ─── Per-context JIT module: cross-kernel CLIF calls ─────────────
//
// All kernels from a given `ExecCtx` go into a
// single JIT module owned by that ExecCtx, so that one kernel's
// compiled code can `call` another's directly via a CLIF `call`
// instruction. The module lives as long as the ExecCtx; when the
// ExecCtx drops, the module drops and the mapped code goes with it.
//
// `by_kernel` keys by `Arc<KernelSig>` raw-pointer identity so the
// same `Arc<KernelSig>` referenced from multiple parent kernels
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
    /// Per-kernel cache: Arc<KernelSig> raw pointer → cached entry.
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
}

unsafe impl Send for Jit {}

/// Compile `kernel`, emitting BODIES by walking Nodes via
/// `emit_clif` recursion.
/// The kernel ABI (params / return / wrapper) still comes from each
/// `KernelSig`; only body codegen changes. The parent emits from
/// `root`; each callee with an entry in `callee_bodies` (keyed by
/// `Arc::as_ptr` — non-recursive callees, recorded by
/// `discover_lambda_calls`) emits from its body Node with EMPTY
/// apply/lambda site maps (a callee body is a self-contained
/// expression over its params: inner call sites are #203-unresolved,
/// so no site of either kind can exist in one today). A fresh callee
/// WITHOUT a recorded body (recursive until F0b; `sub_called`
/// transitive kernels) falls back to its GIR body.
pub fn compile_kernel_with_callees_direct<R: crate::Rt, E: crate::UserEvent>(
    jit: &mut Jit,
    kernel: &std::sync::Arc<KernelSig>,
    callees: &BTreeMap<ArcStr, std::sync::Arc<KernelSig>>,
    root: &crate::Node<R, E>,
    apply_sites: &nohash::IntMap<
        crate::expr::ExprId,
        crate::fusion::lowering::BuiltinCallSiteInfo,
    >,
    lambda_sites: &nohash::IntMap<
        crate::expr::ExprId,
        crate::fusion::LambdaCallInfo,
    >,
    callee_bodies: &BTreeMap<usize, crate::fusion::CalleeBody<'_, R, E>>,
    parent_self_call: Option<&(crate::BindId, crate::fusion::LambdaCallInfo)>,
    type_env: &crate::env::Env,
) -> Result<WrappedKernel> {
    let empty_apply = nohash::IntMap::default();
    let empty_lambda = nohash::IntMap::default();
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
    };
    let callee_emitters: Vec<(usize, NodeBodyEmitter<R, E>)> = callees
        .values()
        .filter_map(|k| {
            let key = std::sync::Arc::as_ptr(k) as usize;
            let cb = callee_bodies.get(&key)?;
            Some((
                key,
                NodeBodyEmitter {
                    root: cb.body,
                    return_type: &k.return_type,
                    apply_sites: &empty_apply,
                    lambda_sites: &empty_lambda,
                    self_call: cb.self_call.as_ref(),
                    type_env,
                },
            ))
        })
        .collect();
    let mut emitters: BTreeMap<usize, &dyn BodyEmitter> = callee_emitters
        .iter()
        .map(|(key, em)| (*key, em as &dyn BodyEmitter))
        .collect();
    emitters.insert(std::sync::Arc::as_ptr(kernel) as usize, &parent);
    compile_kernel_with_callees_impl(jit, kernel, callees, &emitters)
}

fn compile_kernel_with_callees_impl(
    jit: &mut Jit,
    kernel: &std::sync::Arc<KernelSig>,
    callees: &BTreeMap<ArcStr, std::sync::Arc<KernelSig>>,
    emitters: &BTreeMap<usize, &dyn BodyEmitter>,
) -> Result<WrappedKernel> {
    let mut to_define: Vec<std::sync::Arc<KernelSig>> = Vec::new();
    let r = compile_kernel_with_callees_inner(
        jit,
        kernel,
        callees,
        emitters,
        &mut to_define,
    );
    if r.is_err() {
        // Evict every declared-but-never-defined cache entry. On the
        // direct path a failed compile is the COMMON "doesn't fuse"
        // signal (any node without an emit_clif impl), and a kernel
        // `Arc` can be re-submitted later (lambda kernels are cached
        // and shared across call sites) — a stale entry would hand
        // out a `FuncId` whose body was never defined. It would also
        // pin the kernel `Arc` (and its declared symbol) forever.
        for k in &to_define {
            jit.by_kernel.remove(&(std::sync::Arc::as_ptr(k) as usize));
        }
    }
    r
}

fn compile_kernel_with_callees_inner(
    jit: &mut Jit,
    kernel: &std::sync::Arc<KernelSig>,
    callees: &BTreeMap<ArcStr, std::sync::Arc<KernelSig>>,
    emitters: &BTreeMap<usize, &dyn BodyEmitter>,
    to_define: &mut Vec<std::sync::Arc<KernelSig>>,
) -> Result<WrappedKernel> {
    // Phase 1 — declare every kernel in the closure (parent + all
    // transitively-reachable callees). Cached entries reuse their
    // `FuncId`; fresh ones get a freshly-declared FuncId and queue
    // for phase-2 body definition.
    //
    // The parent and any callee with name == parent's fn_name share
    // the same FuncId; that's how self-recursion
    // resolves to a CLIF call back to the parent.
    let kernel_name = kernel.fn_name.clone();
    let mut funcids: BTreeMap<ArcStr, (FuncId, Signature)> = BTreeMap::new();
    let parent_entry = ensure_declared(jit, kernel, to_define)?;
    funcids.insert(kernel_name.clone(), parent_entry.clone());
    for (name, k) in callees {
        if name.as_str() == kernel_name.as_str() {
            funcids.insert(name.clone(), parent_entry.clone());
            continue;
        }
        let entry = ensure_declared(jit, k, to_define)?;
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
    //
    // `emitters` keys body emitters by kernel identity (the parent +
    // every callee whose body discovery recorded). A kernel WITHOUT
    // an entry cannot compile — there is no other body source — so
    // bail (the whole region de-fuses). This can't fire today:
    // `discover_lambda_calls` records a body for every callee it
    // returns; the check guards the invariant rather than a known
    // path.
    for k in to_define.iter() {
        let key = std::sync::Arc::as_ptr(k) as usize;
        let body: &dyn BodyEmitter =
            *emitters.get(&key).ok_or_else(|| {
                anyhow!(
                    "no body emitter recorded for kernel `{}` — \
                     discovery must record every callee body",
                    k.fn_name
                )
            })?;
        let (strings, values) =
            define_kernel_body(&mut jit.ctx, k, &funcids, body)?;
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
    k: &std::sync::Arc<KernelSig>,
    to_define: &mut Vec<std::sync::Arc<KernelSig>>,
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
/// the kernel itself and every callee its body's discovered lambda
/// call sites reference.
fn define_kernel_body(
    jit: &mut JitCtx,
    kernel: &KernelSig,
    funcids: &BTreeMap<ArcStr, (FuncId, Signature)>,
    body_emitter: &dyn BodyEmitter,
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
        // Declare each lambda call site's callee as a FuncRef in this
        // function. Done before constructing the FunctionBuilder
        // because both `declare_func_in_func` and `FunctionBuilder::new`
        // borrow `jit.func_ctx.func` mutably.
        //
        // Exactly the fn_names the body's discovered lambda call sites
        // reference (the parent carries the region's direct callee
        // set; a CALLEE's map is empty — its inner sites are
        // #203-unresolved, so its only cross-kernel reference is
        // itself). The kernel's own name is excluded from sites; a
        // self-recursive body imports its own FuncRef via `self_call`
        // (funcids carries every callee by name, this kernel
        // included).
        let needed: std::collections::BTreeSet<ArcStr> = {
            let mut s: std::collections::BTreeSet<ArcStr> = body_emitter
                .lambda_call_sites()
                .map(|m| {
                    m.values()
                        .map(|info| info.fn_name.clone())
                        .filter(|n| n.as_str() != kernel.fn_name.as_str())
                        .collect()
                })
                .unwrap_or_default();
            if let Some((_, info)) = body_emitter.self_call() {
                s.insert(info.fn_name.clone());
            }
            s
        };
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
        // Lazy interning arenas — filled during emission via
        // `BodyCx::interned_str` / `interned_value`, merged into the
        // returned tables below so the baked addresses live as long
        // as the compiled code.
        let lazy_strings: std::cell::RefCell<Vec<Box<ArcStr>>> =
            std::cell::RefCell::new(Vec::new());
        let lazy_values: std::cell::RefCell<Vec<Box<Value>>> =
            std::cell::RefCell::new(Vec::new());
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
        )
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
    kernel: &KernelSig,
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
        // kind-grouped ABI order (see `KernelSig::abi_params`). Scalar
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
    maybe_dump_clif(&jit.func_ctx.func, &symbol);

    jit.module
        .define_function(wrapper_id, &mut jit.func_ctx)
        .context("define_function (wrapper)")?;
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    Ok(wrapper_id)
}

/// Pack a scalar [`Value`] into a u64 slot for passing into a JIT'd
/// wrapper, extracting the scalar according to the declared `prim`.
/// The bits represent the primitive's value; for narrower primitives
/// the upper bits are unused (the wrapper loads at the CLIF type and
/// ignores them). Panics if `v` isn't a scalar of `prim`'s shape — a
/// kernel built against a typechecker decision the runtime now
/// disagrees with, which is a bug. (`Z32`/`Z64`/`V32`/`V64` accepted
/// for the matching fixed-width prim.)
pub fn pack_value_to_u64(v: &Value, prim: PrimType) -> u64 {
    macro_rules! bad {
        () => {
            panic!("pack_value_to_u64: {v:?} isn't a {prim:?} scalar")
        };
    }
    match prim {
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
    }
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
    callee_refs: &BTreeMap<ArcStr, FuncRef>,
    helper_refs: &HelperRefs,
    lazy_strings: &std::cell::RefCell<Vec<Box<ArcStr>>>,
    lazy_values: &std::cell::RefCell<Vec<Box<Value>>>,
    body_emitter: &dyn BodyEmitter,
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
    for d in kernel.abi_params() {
        match d.kind {
            AbiParamKind::Scalar(prim) => {
                let cty = prim_to_clif(prim);
                let var = b.declare_var(cty);
                b.def_var(var, initial_vals[d.wire_slot]);
                env.bind_with_id(d.name.clone(), var, prim, d.bind_id);
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
        dyncall_buf_stack: std::cell::RefCell::new(Vec::new()),
        owned_input_stack: std::cell::RefCell::new(Vec::new()),
        pending_exit: std::cell::RefCell::new(None),
        lazy_strings,
        lazy_values,
        builtin_apply_sites: body_emitter.builtin_apply_sites(),
        lambda_call_sites: body_emitter.lambda_call_sites(),
        self_call: body_emitter.self_call(),
        type_env: body_emitter.type_env(),
    };
    // Body codegen: the `NodeBodyEmitter` walks the region-root Node
    // via `emit_clif` recursion.
    body_emitter.emit(b, &mut env, &lower)?;

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
        match gir::abi_kind(&kernel.return_type) {
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

    /// Stable `*const Value` for `v`. Panics on a pre-walk miss (a
    /// value-shape `Const`-using op the walk didn't cover).
    pub fn get(&self, v: &Value) -> *const Value {
        self.slots
            .iter()
            .find(|s| *s == v)
            .map(|s| s as *const Value)
            .unwrap_or_else(|| {
                panic!("KernelValues: lookup miss for a value-shape Const literal")
            })
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
    param_mark: EnvMark,
    /// Cross-kernel call sites resolve their callee's `fn_name`
    /// through this map to a CLIF `FuncRef`. The caller must
    /// `declare_func_in_func` each callee's `FuncId` against the
    /// current function before constructing the FunctionBuilder, then
    /// pass the resulting refs in here. Empty for kernels with no
    /// lambda call sites.
    callee_refs: &'a BTreeMap<ArcStr, FuncRef>,
    /// `FuncRef`s for the `gir_jit_helpers::*` runtime helpers.
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
    tail_call_slots: Option<&'a [crate::gir::TailCallSlot]>,
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
    /// Lazily-created single `pending_exit` block. Created on the
    /// first composite-return DynCall site; its body (sentinel +
    /// `return`) is emitted at the end of `compile_into_function`.
    /// All `pre_pending_<n>` blocks jump here after dropping the
    /// owned set.
    pending_exit: std::cell::RefCell<Option<Block>>,
    /// Discovered sync-builtin Apply sites for the direct path
    /// (`Some` only when a [`BodyEmitter`] supplies them) — keyed by
    /// the Apply's spec id, consumed by [`BodyCx::builtin_site`] so
    /// `CallSite::emit_clif` can lower a registered site to a DynCall.
    builtin_apply_sites: Option<
        &'a nohash::IntMap<
            crate::expr::ExprId,
            crate::fusion::lowering::BuiltinCallSiteInfo,
        >,
    >,
    /// Discovered statically-resolved lambda call sites — the direct
    /// path's `ExprId → LambdaCallInfo` map (`None` on the GIR path).
    /// `CallSite::emit_clif` resolves a registered site to a CLIF
    /// `call` against `callee_refs[info.fn_name]`.
    lambda_call_sites: Option<
        &'a nohash::IntMap<crate::expr::ExprId, crate::fusion::LambdaCallInfo>,
    >,
    /// `Some` when this kernel is a self-recursive lambda body being
    /// Node-emitted: the self binding + the kernel's own call
    /// descriptor. Tail-position self-calls rebind-and-jump
    /// (`emit_body_tail`); value-position ones call the kernel's own
    /// FuncRef (`CallSite::emit_clif`).
    self_call: Option<&'a (crate::BindId, crate::fusion::LambdaCallInfo)>,
    /// Type-resolution env snapshot for the direct path (`None` on the
    /// GIR path — its lowering pre-resolved every baked type). See
    /// [`BodyEmitter::type_env`] and [`resolve_node_typ`].
    type_env: Option<&'a crate::env::Env>,
}

/// Resolve named/abstract type refs in a node-carried `Type` through
/// the region's env snapshot (#218): node `typ` cells can hold
/// `Type::Ref`s to abstract type names (e.g. an interface's
/// `type Elem`) whose concrete rep `abi_kind`/freeze can't see —
/// `resolve_abstract` expands them (env `lookup_ref` + the abstract
/// registry). On the GIR path (`type_env: None`) the type returns
/// unchanged — its lowering already resolved everything it baked.
fn resolve_node_typ(ctx: &LowerCtx, t: &Type) -> Type {
    match ctx.type_env {
        Some(env) => crate::fusion::lowering::resolve_abstract(t, env, 0),
        None => t.clone(),
    }
}

/// [`gir::freeze_normalized`] with an abstract-Ref resolution RETRY
/// (#218): on failure, resolve through the region's env snapshot and
/// freeze again. The retry only runs when the plain freeze fails, so
/// the common (concrete-typed) path pays nothing; a freeze that
/// already succeeds can't be changed by resolution (it was fully
/// concrete).
fn freeze_node_typ(ctx: &LowerCtx, t: &Type) -> Option<Type> {
    gir::freeze_normalized(t)
        .or_else(|| gir::freeze_normalized(&resolve_node_typ(ctx, t)))
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

/// Result of emitting one expression node: either a single CLIF value
/// (for scalar/composite/pointer-shaped exprs) or a Value-shaped
/// `(disc, payload)` pair (for variant/nullable exprs). Consumers
/// dispatch on the expression's `GirType`; the helpers `single()` /
/// `value()` assert the expected variant and bail loudly if a site
/// mishandles a Value-shaped result.
#[derive(Debug, Clone, Copy)]
pub enum CompiledExpr {
    Single(ClifValue),
    /// A scalar that MAY be a value-bottom (div/mod-by-zero, signed
    /// MIN/-1, scalar `?` on an error). `valid` is an I8 0/1 bool —
    /// 1 = has a value, 0 = bottom. A scalar that can't be bottom
    /// stays [`CompiledExpr::Single`] (the taint optimization — zero
    /// codegen cost for the common case). Taint propagates through
    /// pure scalar ops (Bin/Cmp/BoolBin/Not/Cast/IfChain) and is
    /// resolved at the kernel OUTPUT: an invalid output makes
    /// `GirNode::update` return `None` (the boundary moves the abort
    /// from the producing site to the output, so an intermediate
    /// bottom an un-taken arm never consumes no longer aborts the
    /// whole kernel — see `design/representable_bottom.md`).
    Scalar2 { value: ClifValue, valid: ClifValue },
    Value { disc: ClifValue, payload: ClifValue },
}

impl CompiledExpr {
    /// Extract a definitely-valid scalar. A [`CompiledExpr::Scalar2`]
    /// (a possibly-bottom scalar) errors — the caller doesn't handle
    /// taint, so the kernel must fall back to the interpreter (which
    /// represents bottom faithfully). This is the safe default: any
    /// scalar consumer that hasn't been taught taint-propagation bails
    /// the whole kernel to interp rather than silently dropping a
    /// bottom. Consumers that DO propagate taint use
    /// [`CompiledExpr::scalar_with_validity`].
    fn single(self) -> Result<ClifValue> {
        match self {
            CompiledExpr::Single(v) => Ok(v),
            CompiledExpr::Scalar2 { .. } => Err(anyhow!(
                "JIT: scalar consumer reached a possibly-bottom (Scalar2) \
                 value but doesn't propagate validity — bail to interp"
            )),
            CompiledExpr::Value { .. } => Err(anyhow!(
                "JIT: expected single CLIF value, got Value-shaped (disc, \
                 payload) pair — GIR is malformed or consumer is wrong"
            )),
        }
    }

    /// Extract a scalar value together with its I8 validity bit. A
    /// non-tainted [`CompiledExpr::Single`] is always valid (validity
    /// = `iconst.I8 1`). Used by the taint-propagating consumers
    /// (Bin/Cmp/BoolBin/Not/Cast/IfChain/Return). Errors on a
    /// Value-shape result (consumer in scalar position).
    fn scalar_with_validity(
        self,
        b: &mut FunctionBuilder,
    ) -> Result<(ClifValue, ClifValue)> {
        match self {
            CompiledExpr::Single(v) => {
                let valid = b.ins().iconst(types::I8, 1);
                Ok((v, valid))
            }
            CompiledExpr::Scalar2 { value, valid } => Ok((value, valid)),
            CompiledExpr::Value { .. } => Err(anyhow!(
                "JIT: expected scalar, got Value-shaped (disc, payload) \
                 pair — GIR is malformed or consumer is wrong"
            )),
        }
    }

    fn value(self) -> Result<(ClifValue, ClifValue)> {
        match self {
            CompiledExpr::Value { disc, payload } => Ok((disc, payload)),
            CompiledExpr::Single(_) | CompiledExpr::Scalar2 { .. } => {
                Err(anyhow!(
                    "JIT: expected Value-shaped (disc, payload), got single \
                     — GIR is malformed or consumer is wrong"
                ))
            }
        }
    }
}

/// Combine the validity bits of several operands into the result
/// validity. Returns `None` when every operand is non-tainted
/// (`Single`) — the result should stay `Single` (the fast path, zero
/// codegen). Returns `Some(band(...))` when at least one operand is
/// tainted (`Scalar2`) — the result must be `Scalar2`. A bottom in any
/// consumed operand taints the result (`valid = AND of operand
/// validities`), mirroring the interp's `?`-absorb.
fn combine_validity(
    b: &mut FunctionBuilder,
    operands: &[CompiledExpr],
) -> Option<ClifValue> {
    let mut acc: Option<ClifValue> = None;
    let mut any_tainted = false;
    for op in operands {
        if let CompiledExpr::Scalar2 { valid, .. } = op {
            any_tainted = true;
            acc = Some(match acc {
                None => *valid,
                Some(a) => b.ins().band(a, *valid),
            });
        }
    }
    if any_tainted { Some(acc.unwrap()) } else { None }
}

/// Wrap a computed scalar `value` with combined operand validity:
/// `Single` if no operand was tainted, else `Scalar2`.
fn scalar_result(
    value: ClifValue,
    validity: Option<ClifValue>,
) -> CompiledExpr {
    match validity {
        None => CompiledExpr::Single(value),
        Some(valid) => CompiledExpr::Scalar2 { value, valid },
    }
}

// ─── Env: name → Variable lookup ─────────────────────────────────

pub(crate) struct JitEnv {
    /// Scalar locals: `(name, var, valid, prim)`. Lookups walk
    /// back-to-front for proper shadowing. `valid` is `Some(var)`
    /// only for a TAINTED local (`let v = 1/0`) — its I8 validity bit
    /// rides alongside the value so `let`-bound bottoms flow through a
    /// later read; a non-tainted local has `valid: None` (the fast
    /// path, read back as `CompiledExpr::Single`).
    locals: Vec<(ArcStr, Variable, Option<Variable>, PrimType, Option<crate::BindId>)>,
    /// Composite parameter pointer Variables: `(name, var, bind_id)`.
    /// The var holds a `*const ValArray` (CLIF i64 on 64-bit targets).
    /// Array, tuple, and struct params share this table — they differ
    /// only in how the kernel reads slots, not in the pointer layout.
    /// `bind_id` is `Some` only for slots bound via
    /// [`Self::bind_composite_with_id`] (currently the HOF loop
    /// element); lookups resolve BindId-first with name fallback,
    /// mirroring `locals`.
    composites: Vec<(ArcStr, Variable, Option<crate::BindId>)>,
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
    /// `ArcStr`'s thin pointer as a single `i64`. A local read
    /// of a String-typed Ref reads the slot and
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

    /// Bind a non-tainted scalar local carrying its source BindId —
    /// the direct path's params and lets, so `Ref` emission resolves
    /// BindId-first (exact under shadowing, where basenames alias —
    /// the #162/#167 bug class).
    fn bind_with_id(
        &mut self,
        name: ArcStr,
        var: Variable,
        prim: PrimType,
        bind_id: Option<crate::BindId>,
    ) {
        self.locals.push((name, var, None, prim, bind_id));
    }

    /// Bind a possibly-tainted scalar local — `valid` is its I8
    /// validity Variable — carrying its source BindId. A later local
    /// read returns `CompiledExpr::Scalar2 { value, valid }`.
    fn bind_tainted_with_id(
        &mut self,
        name: ArcStr,
        var: Variable,
        valid: Variable,
        prim: PrimType,
        bind_id: Option<crate::BindId>,
    ) {
        self.locals.push((name, var, Some(valid), prim, bind_id));
    }

    fn bind_composite(&mut self, name: ArcStr, var: Variable) {
        self.composites.push((name, var, None));
    }

    /// [`Self::bind_composite`] carrying its source BindId — the
    /// composite analogue of [`Self::bind_with_id`], so a composite
    /// `Ref` resolves BindId-first (exact under shadowing).
    fn bind_composite_with_id(
        &mut self,
        name: ArcStr,
        var: Variable,
        bind_id: Option<crate::BindId>,
    ) {
        self.composites.push((name, var, bind_id));
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

    fn lookup(
        &self,
        name: &str,
    ) -> Option<(Variable, Option<Variable>, PrimType)> {
        for (n, v, valid, p, _) in self.locals.iter().rev() {
            if n.as_str() == name {
                return Some((*v, *valid, *p));
            }
        }
        None
    }

    /// Resolve a scalar slot by its source BindId — exact (no
    /// shadowing ambiguity). Only slots bound via the `_with_id`
    /// variants are found here; callers fall back to the name lookup
    /// for id-less slots.
    fn lookup_bind_id(
        &self,
        id: crate::BindId,
    ) -> Option<(Variable, Option<Variable>, PrimType)> {
        for (_, v, valid, p, bid) in self.locals.iter().rev() {
            if *bid == Some(id) {
                return Some((*v, *valid, *p));
            }
        }
        None
    }

    fn lookup_composite(&self, name: &str) -> Option<Variable> {
        for (n, v, _) in self.composites.iter().rev() {
            if n.as_str() == name {
                return Some(*v);
            }
        }
        None
    }

    /// Resolve a composite slot by its source BindId — see
    /// [`Self::lookup_bind_id`]. Only slots bound via
    /// [`Self::bind_composite_with_id`] are found here; callers fall
    /// back to the name lookup for id-less slots.
    fn lookup_composite_bind_id(&self, id: crate::BindId) -> Option<Variable> {
        for (_, v, bid) in self.composites.iter().rev() {
            if *bid == Some(id) {
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
    /// scope-exit code (block emission) or the terminating return
    /// (`drop_owned_composites`); `truncate` is
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

/// The rebind-and-jump core of a self tail-call
/// ([`emit_self_tail_call`]).
/// `new_vals` are the already-evaluated replacement values for the
/// leading `new_vals.len()` tail-call slots — the FORMALS; trailing
/// capture slots stay bound (loop-invariant within one invocation).
/// `sources[i]` classifies each arg's composite provenance so a
/// Borrowed pointer is refcount-bumped before the old slot value
/// drops. Drops every owned non-slot local above the param mark
/// (per-iteration lets would leak otherwise), truncates the
/// compile-time env back to the params, and jumps to the loop head.
fn emit_tail_rebind_jump(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
    new_vals: Vec<ClifValue>,
    sources: &[CompositeSource],
) -> Result<()> {
    let head = ctx.loop_head.ok_or_else(|| {
        anyhow!("GIR malformed: TailCall in kernel without has_tail_loop")
    })?;
    // Back-compat: hand-built test kernels leave `tail_call_slots`
    // empty and assume all params are scalar in declaration order.
    // Drive the rebind positionally in that case.
    if ctx.tail_call_slots.is_none() {
        debug_assert_eq!(new_vals.len(), ctx.param_mark.locals);
        for (i, v) in new_vals.iter().enumerate() {
            let (var, _) = (env.locals[i].1, env.locals[i].2);
            b.def_var(var, *v);
        }
        env.truncate(ctx.param_mark);
        b.ins().jump(head, &[]);
        return Ok(());
    }
    let slots = ctx.tail_call_slots.unwrap();
    // Slots cover EVERY kernel value param (they double as the
    // runtime arg layout — `arg_layout`, gir_interp.rs); a tail call
    // rebinds only the leading FORMALS.
    debug_assert!(new_vals.len() <= slots.len());
    use crate::gir::TailCallSlotKind;
    let drop_helper = ctx
        .helper_refs
        .get("graphix_valarray_drop")
        .ok_or_else(|| anyhow!("missing graphix_valarray_drop"))?;
    let clone_helper = ctx
        .helper_refs
        .get("graphix_valarray_clone")
        .ok_or_else(|| anyhow!("missing graphix_valarray_clone"))?;
    let mut new_vals: Vec<ClifValue> = new_vals;
    for (i, slot) in slots.iter().take(new_vals.len()).enumerate() {
        if matches!(slot.kind, TailCallSlotKind::ValArray)
            && sources[i] == CompositeSource::Borrowed
        {
            // Clone (refcount bump) so the next iteration holds an
            // owned reference, separate from any other live alias.
            let call = b.ins().call(clone_helper, &[new_vals[i]]);
            new_vals[i] = b.inst_results(call)[0];
        }
    }
    for (slot, v) in slots.iter().zip(new_vals.iter()) {
        match slot.kind {
            TailCallSlotKind::Scalar(_) => {
                let (var, _, _) = env.lookup(&slot.name).ok_or_else(|| {
                    anyhow!("TailCall: scalar slot `{}` not in env", slot.name)
                })?;
                b.def_var(var, *v);
            }
            TailCallSlotKind::ValArray => {
                // Composite rebind: drop the previously-owned pointer
                // in the slot, then store the new owned
                // `*mut ValArray`. This closes the leak we had in
                // Phase 2.
                let var =
                    env.lookup_composite(&slot.name).ok_or_else(|| {
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
    // skip any entry whose name matches a `TailCallSlotKind::ValArray`
    // slot. Same for future variant/nullable rebind slots once
    // they're supported.
    let slot_names: std::collections::HashSet<&str> =
        slots.iter().map(|s| s.name.as_str()).collect();
    for (name, var, _) in &env.composites[ctx.param_mark.composites..] {
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
    // Compile-time env-Vec hygiene: pop everything above the param
    // mark so the next iteration starts with a clean lexical state.
    env.truncate(ctx.param_mark);
    b.ins().jump(head, &[]);
    Ok(())
}

/// Emit a value-bottom abort: when the I8 `valid` bit is 0, set the
/// pending flag, run `emit_pending_cleanup`, and jump to `pending_exit`
/// (so `GirNode::update` returns `None`). Falls through to a fresh
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

// ─── Node → CLIF body emission ──────────────────────────────────
//
// Bodies are emitted by walking the region-root [`crate::Node`] graph:
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
// [`CompiledExpr::Scalar2`] whose validity is resolved where it's
// consumed (`require_valid` / the kernel boundary).

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
    ) -> Option<
        &nohash::IntMap<
            crate::expr::ExprId,
            crate::fusion::lowering::BuiltinCallSiteInfo,
        >,
    > {
        None
    }

    /// Discovered statically-resolved lambda call sites for the region
    /// being emitted (`CallSite::emit_clif` lowers a registered site to
    /// a CLIF `call` via [`BodyCx::lambda_site`]). `None` for callee
    /// bodies (a callee's only cross-kernel reference is itself).
    fn lambda_call_sites(
        &self,
    ) -> Option<
        &nohash::IntMap<crate::expr::ExprId, crate::fusion::LambdaCallInfo>,
    > {
        None
    }

    /// `Some` when the kernel being emitted is a self-recursive lambda
    /// body: the binding its self-references carry + the kernel's own
    /// call descriptor. Drives tail-position rebind-and-jump
    /// (`emit_body_tail`), value-position self-calls
    /// (`CallSite::emit_clif` → the kernel's own FuncRef), and the
    /// self-FuncRef import in `define_kernel_body`.
    fn self_call(
        &self,
    ) -> Option<&(crate::BindId, crate::fusion::LambdaCallInfo)> {
        None
    }

    /// The environment snapshot for TYPE RESOLUTION ONLY (#218): node
    /// `typ` cells can carry `Type::Ref`s to abstract type names whose
    /// concrete rep needs `env.lookup_ref` + the abstract registry
    /// (`resolve_abstract`) before `abi_kind`/freeze can classify
    /// them. `None` on the GIR path — its lowering pre-resolved every
    /// type it baked into the body. NOT for binding lookups; those
    /// stay in the analysis phase, per the BodyCx design.
    fn type_env(&self) -> Option<&crate::env::Env> {
        None
    }
}

/// The body emitter — walks the region-root `Node` via `emit_clif`
/// recursion and emits the kernel return. `return_type` comes from
/// the `KernelSig` so the boundary marshalling agrees with the kernel
/// signature / wrapper / runtime arg-pack.
struct NodeBodyEmitter<'a, R: crate::Rt, E: crate::UserEvent> {
    root: &'a crate::Node<R, E>,
    return_type: &'a Type,
    apply_sites: &'a nohash::IntMap<
        crate::expr::ExprId,
        crate::fusion::lowering::BuiltinCallSiteInfo,
    >,
    lambda_sites: &'a nohash::IntMap<
        crate::expr::ExprId,
        crate::fusion::LambdaCallInfo,
    >,
    /// `Some` for a self-recursive callee body — see
    /// [`BodyEmitter::self_call`].
    self_call: Option<&'a (crate::BindId, crate::fusion::LambdaCallInfo)>,
    /// Type-resolution env snapshot — see [`BodyEmitter::type_env`].
    type_env: &'a crate::env::Env,
}

impl<R: crate::Rt, E: crate::UserEvent> BodyEmitter
    for NodeBodyEmitter<'_, R, E>
{
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
            None => {
                let cv = self.root.emit_clif(&mut cx)?;
                let src = node_composite_source(self.root);
                emit_kernel_return(&mut cx, self.return_type, cv, src)
            }
        }
    }

    fn builtin_apply_sites(
        &self,
    ) -> Option<
        &nohash::IntMap<
            crate::expr::ExprId,
            crate::fusion::lowering::BuiltinCallSiteInfo,
        >,
    > {
        Some(self.apply_sites)
    }

    fn lambda_call_sites(
        &self,
    ) -> Option<
        &nohash::IntMap<crate::expr::ExprId, crate::fusion::LambdaCallInfo>,
    > {
        Some(self.lambda_sites)
    }

    fn self_call(
        &self,
    ) -> Option<&(crate::BindId, crate::fusion::LambdaCallInfo)> {
        self.self_call
    }

    fn type_env(&self) -> Option<&crate::env::Env> {
        Some(self.type_env)
    }
}

/// The open-kernel emission context handed to
/// [`crate::Update::emit_clif`] / [`crate::Apply::emit_clif`] impls:
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

impl BodyCx<'_, '_, '_> {
    /// FuncRef for a registered `gir_jit_helpers` runtime helper.
    pub fn helper(&self, name: &str) -> Result<FuncRef> {
        self.ctx
            .helper_refs
            .get(name)
            .ok_or_else(|| anyhow!("missing helper {name}"))
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
                lazy.push(Box::new(crate::gir_jit_intern::intern(s)));
                lazy.last().unwrap().as_ref() as *const ArcStr
            }
        };
        self.b.ins().iconst(types::I64, ptr as i64)
    }

    /// The discovered builtin Apply-site info for `id`, if the
    /// region's pre-build discovery pass registered one (direct path
    /// only — `None` whenever no [`BodyEmitter`] supplied the map).
    pub(crate) fn builtin_site(
        &self,
        id: crate::expr::ExprId,
    ) -> Option<&crate::fusion::lowering::BuiltinCallSiteInfo> {
        self.ctx.builtin_apply_sites.and_then(|m| m.get(&id))
    }

    /// The discovered lambda call-site info for `id`, if `try_fuse`'s
    /// analysis registered one (direct path only). A `Some` means the
    /// callee kernel is declared in this function's `callee_refs` under
    /// `info.fn_name` and ready to `call`.
    pub(crate) fn lambda_site(
        &self,
        id: crate::expr::ExprId,
    ) -> Option<&crate::fusion::LambdaCallInfo> {
        self.ctx.lambda_call_sites.and_then(|m| m.get(&id))
    }

    /// The kernel's own self-call descriptor when emitting a
    /// self-recursive lambda body: `(the self binding, the kernel's
    /// own LambdaCallInfo)`. A value-position call site whose fnode
    /// Ref carries the binding calls the kernel's own FuncRef.
    pub(crate) fn self_call_info(
        &self,
    ) -> Option<&(crate::BindId, crate::fusion::LambdaCallInfo)> {
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
pub fn node_composite_source<R: crate::Rt, E: crate::UserEvent>(
    node: &crate::Node<R, E>,
) -> CompositeSource {
    use crate::NodeView;
    let mut n: &dyn crate::Update<R, E> = &**node;
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
            let clone = cx.helper("graphix_value_clone")?;
            let call = cx.b.ins().call(clone, &[disc, payload]);
            let r = cx.b.inst_results(call);
            Ok((r[0], r[1]))
        }
    }
}

/// Emit the kernel return. Borrowed results
/// (a `Ref` still owned by the env, possibly through parens / block
/// tails) are cloned so the scope-exit / kernel-exit drops below don't
/// free the buffer the caller is about to receive. Value / composite /
/// string returns run the pending check (an earlier DynCall that
/// pended means the caller discards the result — drop it instead of
/// leaking); a possibly-bottom scalar routes its validity through the
/// pending exit (the OUTPUT-consumes-bottom rule).
fn emit_kernel_return(
    cx: &mut BodyCx,
    return_type: &Type,
    cv: CompiledExpr,
    src: CompositeSource,
) -> Result<()> {
    match gir::abi_kind(return_type) {
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let (disc, payload) = match cv {
                CompiledExpr::Value { disc, payload } => (disc, payload),
                _ => {
                    return Err(anyhow!(
                        "emit_kernel_return: Value-shape return got a \
                         non-Value result"
                    ));
                }
            };
            let (disc, payload) =
                ensure_owned_value_src(cx, src, disc, payload)?;
            emit_return_pending_check(
                cx.b,
                cx.env,
                cx.ctx,
                ReturnDropShape::Value { disc, payload },
            )?;
            drop_owned_composites(cx.b, cx.env, cx.ctx)?;
            cx.b.ins().return_(&[disc, payload]);
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let v = cv.single()?;
            let v = ensure_owned_composite_src(cx, src, v)?;
            emit_return_pending_check(
                cx.b,
                cx.env,
                cx.ctx,
                ReturnDropShape::Composite(v),
            )?;
            drop_owned_composites(cx.b, cx.env, cx.ctx)?;
            cx.b.ins().return_(&[v]);
        }
        Some(AbiKind::String) => {
            // String results are owned at production (reads clone);
            // no ensure needed — mirror the GIR String return arm.
            let v = cv.single()?;
            emit_return_pending_check(
                cx.b,
                cx.env,
                cx.ctx,
                ReturnDropShape::String(v),
            )?;
            drop_owned_composites(cx.b, cx.env, cx.ctx)?;
            cx.b.ins().return_(&[v]);
        }
        Some(AbiKind::Scalar(_)) => match cv {
            CompiledExpr::Single(v) => {
                drop_owned_composites(cx.b, cx.env, cx.ctx)?;
                cx.b.ins().return_(&[v]);
            }
            CompiledExpr::Scalar2 { value, valid } => {
                let pending_set = cx.helper("graphix_dyncall_set_pending")?;
                let pre_pending = cx.b.create_block();
                let ret_block = cx.b.create_block();
                let pending_exit = {
                    let mut slot = cx.ctx.pending_exit.borrow_mut();
                    match *slot {
                        Some(blk) => blk,
                        None => {
                            let blk = cx.b.create_block();
                            *slot = Some(blk);
                            blk
                        }
                    }
                };
                cx.b.ins().brif(valid, ret_block, &[], pre_pending, &[]);
                cx.b.switch_to_block(pre_pending);
                cx.b.seal_block(pre_pending);
                cx.b.ins().call(pending_set, &[]);
                emit_pending_cleanup(cx.b, cx.env, cx.ctx)?;
                cx.b.ins().jump(pending_exit, &[]);
                cx.b.switch_to_block(ret_block);
                cx.b.seal_block(ret_block);
                drop_owned_composites(cx.b, cx.env, cx.ctx)?;
                cx.b.ins().return_(&[value]);
            }
            CompiledExpr::Value { .. } => {
                return Err(anyhow!(
                    "emit_kernel_return: scalar return got a Value-shape \
                     result"
                ));
            }
        },
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
fn ref_local_name(spec: &crate::expr::Expr) -> Option<&str> {
    let name = match &spec.kind {
        crate::expr::ExprKind::Ref { name } => name,
        _ => return None,
    };
    let s: &str = name.0.as_ref();
    Some(netidx::path::Path::basename(s).unwrap_or(s))
}

// ─── Distributed node-emission relays ─────────────────────────────
//
// The per-node `Update::emit_clif` impls (node/*.rs) are thin shims
// over these crate-internal helpers, which own the CLIF mechanics
// (`JitEnv`/`LowerCtx` stay private to this module). Each relay
// mirrors the corresponding GIR arm of `compile_expr` exactly — same
// helpers, same taint propagation — so the two paths can't drift
// while both are alive.

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
    match gir::abi_kind(typ) {
        Some(AbiKind::Scalar(prim)) => {
            Ok(CompiledExpr::Single(compile_const(cx.b, value, prim)))
        }
        Some(AbiKind::String) => {
            let s = match value {
                Value::String(s) => s,
                v => {
                    return Err(anyhow!(
                        "emit_clif: String-typed Constant holds {v:?}"
                    ));
                }
            };
            let ptr = cx.interned_str(s);
            let clone = cx.helper("graphix_arcstr_clone_from_static")?;
            let call = cx.b.ins().call(clone, &[ptr]);
            Ok(CompiledExpr::Single(cx.b.inst_results(call)[0]))
        }
        Some(AbiKind::Value) => {
            let ptr = cx.interned_value(value);
            let clone = cx.helper("graphix_value_clone_from_static")?;
            let call = cx.b.ins().call(clone, &[ptr]);
            let r = cx.b.inst_results(call);
            Ok(CompiledExpr::Value { disc: r[0], payload: r[1] })
        }
        other => Err(anyhow!(
            "emit_clif: Constant of shape {other:?} — not yet supported"
        )),
    }
}

/// A `{k => v, ...}` map literal. Mirrors the classic path's
/// `emit_map_new`: fuses ONLY when every key and value is a
/// compile-time constant — the `CMap` is built at compile time
/// (`insert_cow` in entry order, exactly as `Map::update` does at
/// runtime) and emitted as an interned Value constant. A dynamic
/// entry de-fuses; the runtime map producer isn't lowered on either
/// path.
pub(crate) fn emit_map_new_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    keys: &[crate::node::Cached<R, E>],
    vals: &[crate::node::Cached<R, E>],
    typ: &Type,
) -> Result<CompiledExpr> {
    let v = crate::fusion::lowering::const_map(keys, vals).ok_or_else(|| {
        anyhow!(
            "emit_clif: map literal with non-constant entries — \
             subtree node-walks"
        )
    })?;
    let typ = gir::freeze_concrete(typ).unwrap_or_else(gir::map_type);
    emit_const_node(cx, &v, &typ)
}

/// A binding read. Resolve the Ref's source name to the kernel param
/// / block-let slot in the env (same name the params were bound under
/// — see `compile_into_function`'s entry binder). Surfaces a tainted
/// local's validity bit, mirroring `compile_expr`'s scalar-Local
/// intercept.
pub(crate) fn emit_ref_node(
    cx: &mut BodyCx,
    spec: &crate::expr::Expr,
    typ: &Type,
    id: crate::BindId,
) -> Result<CompiledExpr> {
    match gir::abi_kind(typ) {
        // Scalar: BindId-first — exact under shadowing (an outer
        // capture and an inner let sharing a basename resolve to
        // different slots). Name fallback covers id-less slots (the
        // GIR path's synthetic locals; direct-path slots all carry
        // ids). Surfaces a tainted local's validity bit.
        Some(AbiKind::Scalar(_)) => {
            let (var, valid, _) = match cx.env.lookup_bind_id(id) {
                Some(slot) => slot,
                None => {
                    let name = ref_local_name(spec).ok_or_else(|| {
                        anyhow!("emit_clif: Ref spec isn't an ExprKind::Ref")
                    })?;
                    cx.env.lookup(name).ok_or_else(|| {
                        anyhow!("emit_clif: undefined scalar local `{name}`")
                    })?
                }
            };
            let value = cx.b.use_var(var);
            Ok(match valid {
                None => CompiledExpr::Single(value),
                Some(vv) => {
                    let valid = cx.b.use_var(vv);
                    CompiledExpr::Scalar2 { value, valid }
                }
            })
        }
        // String: read the slot and refcount-bump — each consumer gets
        // an independently-owned ArcStr; the slot keeps its own ref
        // until scope exit. Mirrors the GIR string-Local arm.
        Some(AbiKind::String) => {
            let name = ref_local_name(spec).ok_or_else(|| {
                anyhow!("emit_clif: Ref spec isn't an ExprKind::Ref")
            })?;
            let var = cx.env.lookup_string(name).ok_or_else(|| {
                anyhow!("emit_clif: undefined string local `{name}`")
            })?;
            let s = cx.b.use_var(var);
            let clone = cx.helper("graphix_arcstr_clone")?;
            let call = cx.b.ins().call(clone, &[s]);
            Ok(CompiledExpr::Single(cx.b.inst_results(call)[0]))
        }
        // Variant / Nullable / value-shape: BORROWED two-word read —
        // the env still owns the ref; consumers clone via
        // `ensure_owned_value_src` when they need ownership. Mirrors
        // the GIR value-Local arm.
        Some(AbiKind::Variant) => {
            let name = ref_local_name(spec).ok_or_else(|| {
                anyhow!("emit_clif: Ref spec isn't an ExprKind::Ref")
            })?;
            let vv = cx.env.lookup_variant(name).ok_or_else(|| {
                anyhow!("emit_clif: undefined variant local `{name}`")
            })?;
            Ok(CompiledExpr::Value {
                disc: cx.b.use_var(vv.disc),
                payload: cx.b.use_var(vv.payload),
            })
        }
        Some(AbiKind::Nullable | AbiKind::Value) => {
            let name = ref_local_name(spec).ok_or_else(|| {
                anyhow!("emit_clif: Ref spec isn't an ExprKind::Ref")
            })?;
            let vv = cx.env.lookup_nullable(name).ok_or_else(|| {
                anyhow!("emit_clif: undefined value-shape local `{name}`")
            })?;
            Ok(CompiledExpr::Value {
                disc: cx.b.use_var(vv.disc),
                payload: cx.b.use_var(vv.payload),
            })
        }
        // Composite: BORROWED pointer read — consumers clone via
        // `ensure_owned_composite_src` when they need ownership.
        // BindId-first like the scalar arm (exact under shadowing);
        // name fallback covers the id-less slots (params, lets, the
        // GIR path's locals).
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let var = match cx.env.lookup_composite_bind_id(id) {
                Some(var) => var,
                None => {
                    let name = ref_local_name(spec).ok_or_else(|| {
                        anyhow!("emit_clif: Ref spec isn't an ExprKind::Ref")
                    })?;
                    cx.env.lookup_composite(name).ok_or_else(|| {
                        anyhow!("emit_clif: undefined composite local `{name}`")
                    })?
                }
            };
            Ok(CompiledExpr::Single(cx.b.use_var(var)))
        }
        other => Err(anyhow!(
            "emit_clif: Ref of shape {other:?} — not yet supported"
        )),
    }
}

/// Arithmetic — compile both operands, then the SAME `compile_bin`
/// helper (including the div/mod taint/guard), propagating operand
/// validity exactly as the GIR `Bin` arm. A datetime/duration operand
/// routes to the `ValueArith` mirror first (netidx `Value` arithmetic
/// via the `graphix_value_<op>` helpers, both operands OWNED since the
/// helpers consume) — the same dispatch `emit_arith` makes when
/// lowering to GIR.
pub(crate) fn emit_arith_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    op: BinOp,
    lhs: &crate::Node<R, E>,
    rhs: &crate::Node<R, E>,
) -> Result<CompiledExpr> {
    if crate::fusion::lowering::is_datetime_or_duration(lhs.typ())
        || crate::fusion::lowering::is_datetime_or_duration(rhs.typ())
    {
        let (ld, lp) = emit_owned_value_operand_node(cx, lhs)?;
        let (rd, rp) = emit_owned_value_operand_node(cx, rhs)?;
        let helper = match op {
            BinOp::Add => "graphix_value_add",
            BinOp::Sub => "graphix_value_sub",
            BinOp::Mul => "graphix_value_mul",
            BinOp::Div => "graphix_value_div",
            BinOp::Mod => "graphix_value_rem",
        };
        let fref = cx.helper(helper)?;
        let call = cx.b.ins().call(fref, &[ld, lp, rd, rp]);
        let r = cx.b.inst_results(call);
        return Ok(CompiledExpr::Value { disc: r[0], payload: r[1] });
    }
    let lcv = lhs.emit_clif(cx)?;
    let rcv = rhs.emit_clif(cx)?;
    let (l, _) = lcv.scalar_with_validity(cx.b)?;
    let (r, _) = rcv.scalar_with_validity(cx.b)?;
    // Fallible prim derivation (not `prim_of`, which panics): an
    // operand's type may be typecheck's un-normalized union (a select
    // result) — scalar after `freeze_normalized` (with the abstract-
    // Ref resolution retry, #218), or Err → no fusion.
    let prim = freeze_node_typ(cx.ctx, lhs.typ())
        .as_ref()
        .and_then(gir::scalar_prim)
        .ok_or_else(|| {
            anyhow!(
                "emit_clif: arith operand of non-scalar type {:?}",
                lhs.typ()
            )
        })?;
    // Integer div/mod taint/guard — identical to the GIR `Bin` arm.
    if matches!(op, BinOp::Div | BinOp::Mod)
        && prim.is_integer()
        && node_int_div_may_bottom(lhs, rhs)
    {
        let b = &mut *cx.b;
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
        let one = b.ins().iconst(prim_to_clif(prim), 1);
        let safe_r = b.ins().select(bad, one, r);
        let value = compile_bin(b, op, prim, l, safe_r)?;
        let not_bad = {
            let one_i8 = b.ins().iconst(types::I8, 1);
            b.ins().bxor(bad, one_i8)
        };
        let mut valid = not_bad;
        if let CompiledExpr::Scalar2 { valid: lv, .. } = lcv {
            valid = b.ins().band(valid, lv);
        }
        if let CompiledExpr::Scalar2 { valid: rv, .. } = rcv {
            valid = b.ins().band(valid, rv);
        }
        return Ok(CompiledExpr::Scalar2 { value, valid });
    }
    let value = compile_bin(cx.b, op, prim, l, r)?;
    let valid = combine_validity(cx.b, &[lcv, rcv]);
    Ok(scalar_result(value, valid))
}

/// Checked arithmetic (`+?` / `-?` / `*?` / `/?` / `%?`) — NEW
/// direct-path coverage; the GIR path never lowered these. Both
/// operands are compiled as OWNED `(disc, payload)` Values (the same
/// route as `emit_arith_node`'s ValueArith dispatch — the helpers
/// consume), then the `graphix_value_checked_<op>` helper computes via
/// the SAME netidx `Value::checked_*` + [`crate::node::op::
/// wrap_arith_error`] core the node-walk's update uses. The result is
/// a Value: the success scalar, or the catchable `ArithError` error
/// VALUE (`[T, Error<`ArithError(string)>]` freezes to the Nullable
/// wire shape) — never bottom, unlike unchecked div0.
pub(crate) fn emit_checked_arith_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    op: BinOp,
    lhs: &crate::Node<R, E>,
    rhs: &crate::Node<R, E>,
) -> Result<CompiledExpr> {
    let (ld, lp) = emit_owned_value_operand_node(cx, lhs)?;
    let (rd, rp) = emit_owned_value_operand_node(cx, rhs)?;
    let helper = match op {
        BinOp::Add => "graphix_value_checked_add",
        BinOp::Sub => "graphix_value_checked_sub",
        BinOp::Mul => "graphix_value_checked_mul",
        BinOp::Div => "graphix_value_checked_div",
        BinOp::Mod => "graphix_value_checked_rem",
    };
    let fref = cx.helper(helper)?;
    let call = cx.b.ins().call(fref, &[ld, lp, rd, rp]);
    let r = cx.b.inst_results(call);
    Ok(CompiledExpr::Value { disc: r[0], payload: r[1] })
}

/// Comparison — `compile_cmp` (total-order floats) on scalar operands,
/// propagating operand validity. Non-scalar `==`/`!=` (String,
/// composite, value-shape) mirrors the GIR `ValueEq` arm: both operands
/// as OWNED `(disc, payload)` Values (the helper consumes them),
/// compared via netidx `Value` PartialEq. Ordering operators on
/// non-scalar operands aren't lowered (mirrors `gir::cmp`) — Err, the
/// region node-walks.
pub(crate) fn emit_cmp_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    op: CmpOp,
    lhs: &crate::Node<R, E>,
    rhs: &crate::Node<R, E>,
) -> Result<CompiledExpr> {
    let lprim =
        gir::freeze_normalized(lhs.typ()).as_ref().and_then(gir::scalar_prim);
    let rprim =
        gir::freeze_normalized(rhs.typ()).as_ref().and_then(gir::scalar_prim);
    if let (Some(lp), Some(_)) = (lprim, rprim) {
        let lcv = lhs.emit_clif(cx)?;
        let rcv = rhs.emit_clif(cx)?;
        let (l, _) = lcv.scalar_with_validity(cx.b)?;
        let (r, _) = rcv.scalar_with_validity(cx.b)?;
        let value = compile_cmp(cx.b, op, lp, l, r);
        let valid = combine_validity(cx.b, &[lcv, rcv]);
        return Ok(scalar_result(value, valid));
    }
    let ne = match op {
        CmpOp::Eq => false,
        CmpOp::Ne => true,
        other => {
            return Err(anyhow!(
                "emit_clif: ordering cmp {other:?} on non-scalar operands \
                 — not lowered (mirrors gir::cmp)"
            ));
        }
    };
    for t in [lhs.typ(), rhs.typ()] {
        if matches!(
            gir::abi_kind(t),
            Some(AbiKind::Unit | AbiKind::Null) | None
        ) {
            return Err(anyhow!(
                "emit_clif: ==/!= operand of type {t:?} has no comparable \
                 runtime form (mirrors gir::cmp)"
            ));
        }
    }
    let (ld, lp) = emit_owned_value_operand_node(cx, lhs)?;
    let (rd, rp) = emit_owned_value_operand_node(cx, rhs)?;
    let helper = cx.helper("graphix_value_eq")?;
    let call = cx.b.ins().call(helper, &[ld, lp, rd, rp]);
    let eq = cx.b.inst_results(call)[0]; // I8 bool
    Ok(CompiledExpr::Single(if ne {
        let one = cx.b.ins().iconst(types::I8, 1);
        cx.b.ins().bxor(eq, one)
    } else {
        eq
    }))
}

/// Logical — STRICT `band`/`bor` (both operands always compiled),
/// taint-propagating, matching the GIR `BoolBin` arm and the
/// node-walk's `bool_op!`.
pub(crate) fn emit_bool_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    op: BoolOp,
    lhs: &crate::Node<R, E>,
    rhs: &crate::Node<R, E>,
) -> Result<CompiledExpr> {
    let lcv = lhs.emit_clif(cx)?;
    let rcv = rhs.emit_clif(cx)?;
    let (l, _) = lcv.scalar_with_validity(cx.b)?;
    let (r, _) = rcv.scalar_with_validity(cx.b)?;
    let value = match op {
        BoolOp::And => cx.b.ins().band(l, r),
        BoolOp::Or => cx.b.ins().bor(l, r),
    };
    let valid = combine_validity(cx.b, &[lcv, rcv]);
    Ok(scalar_result(value, valid))
}

/// Logical NOT — taint-propagating.
pub(crate) fn emit_not_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    inner: &crate::Node<R, E>,
) -> Result<CompiledExpr> {
    let cv = inner.emit_clif(cx)?;
    let (v, _) = cv.scalar_with_validity(cx.b)?;
    let one = cx.b.ins().iconst(types::I8, 1);
    let value = cx.b.ins().bxor(v, one);
    let valid = combine_validity(cx.b, &[cv]);
    Ok(scalar_result(value, valid))
}

/// `cast<T>(x)` — `compile_cast`, taint-propagating.
pub(crate) fn emit_cast_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    inner: &crate::Node<R, E>,
    target: &Type,
) -> Result<CompiledExpr> {
    let target = PrimType::from_type(target).ok_or_else(|| {
        anyhow!("emit_clif: TypeCast to non-scalar target {target:?}")
    })?;
    let src = gir::scalar_prim(inner.typ()).ok_or_else(|| {
        anyhow!(
            "emit_clif: cast source of non-scalar typ {:?}",
            inner.typ()
        )
    })?;
    let cv = inner.emit_clif(cx)?;
    let (v, _) = cv.scalar_with_validity(cx.b)?;
    let value = compile_cast(cx.b, v, src, target);
    let valid = combine_validity(cx.b, &[cv]);
    Ok(scalar_result(value, valid))
}

/// String interpolation `"x is [x]"` — the Node twin of the GIR
/// `Concat` arm ([`compile_concat`]): build a heap-owned `*mut String`,
/// push each part (append-as-str for string parts — reads are already
/// owned clones, the push consumes; Display-rendered for scalars via
/// the shared [`string_buf_push_helper`]), finalize into an OWNED
/// ArcStr. Keeps the GIR restriction on part shapes: a non-scalar /
/// non-string part (a Nullable from `a[i]`, a composite, a value-shape
/// — see findings "StringInterpolate non-scalar part") is Err, the
/// subtree node-walks.
pub(crate) fn emit_string_interpolate_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    args: &[crate::node::Cached<R, E>],
) -> Result<CompiledExpr> {
    let new_buf = cx.helper("graphix_string_buf_new")?;
    let call = cx.b.ins().call(new_buf, &[]);
    let buf = cx.b.inst_results(call)[0];
    for a in args {
        let part = &a.node;
        // `freeze_normalized` so a select-valued part (whose type is
        // the un-normalized arm union) still classifies.
        let frozen = gir::freeze_normalized(part.typ());
        match frozen.as_ref().and_then(gir::abi_kind) {
            Some(AbiKind::String) => {
                let s = part.emit_clif(cx)?.single()?;
                let push = cx.helper("graphix_string_buf_push_arcstr")?;
                cx.b.ins().call(push, &[buf, s]);
            }
            Some(AbiKind::Scalar(p)) => {
                let v = part.emit_clif(cx)?.single()?;
                let push = cx.helper(string_buf_push_helper(p))?;
                cx.b.ins().call(push, &[buf, v]);
            }
            other => {
                return Err(anyhow!(
                    "emit_clif: string-interpolate part of shape {other:?} \
                     — only String and scalar parts are lowered (mirrors \
                     the GIR Concat restriction)"
                ));
            }
        }
    }
    let finalize = cx.helper("graphix_string_buf_finalize")?;
    let call = cx.b.ins().call(finalize, &[buf]);
    Ok(CompiledExpr::Single(cx.b.inst_results(call)[0]))
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
/// variables; a `?` with a catch handler writes the handler's
/// variable; `$` logs; a CallSite may target an async/effectful
/// builtin (we can't consult `builtin_effects` at emit time, so ALL
/// call sites are conservatively effectful). Everything else the
/// direct emitter can encounter is value-only.
fn stmt_subtree_effect_free<R: crate::Rt, E: crate::UserEvent>(
    node: &crate::Node<R, E>,
) -> bool {
    let mut ok = true;
    crate::fusion::for_each_node(node, &mut |n| match n.view() {
        crate::NodeView::Connect(_)
        | crate::NodeView::ConnectDeref(_)
        | crate::NodeView::CallSite(_)
        | crate::NodeView::Qop(_)
        | crate::NodeView::OrNever(_) => ok = false,
        _ => {}
    });
    ok
}

pub(crate) fn emit_block_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    children: &[crate::Node<R, E>],
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
            let src = node_composite_source(child);
            let result = match tail_cv {
                CompiledExpr::Single(v) => {
                    match gir::abi_kind(child.typ()) {
                        Some(
                            AbiKind::Array | AbiKind::Tuple | AbiKind::Struct,
                        ) => CompiledExpr::Single(
                            ensure_owned_composite_src(cx, src, v)?,
                        ),
                        // Scalars need no clone; a String read is
                        // already an owned clone (the Ref/Const arms
                        // bump the refcount at read).
                        _ => CompiledExpr::Single(v),
                    }
                }
                // A tainted tail is always a scalar — no clone.
                cv @ CompiledExpr::Scalar2 { .. } => cv,
                CompiledExpr::Value { disc, payload } => {
                    let (disc, payload) =
                        ensure_owned_value_src(cx, src, disc, payload)?;
                    CompiledExpr::Value { disc, payload }
                }
            };
            emit_scope_drops(cx, &mark)?;
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
        let dead = if matches!(child.view(), crate::NodeView::Bind(_)) {
            let mut bound = crate::Refs::default();
            child.refs(&mut bound);
            let mut suffix = crate::Refs::default();
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
fn emit_block_stmt<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    child: &crate::Node<R, E>,
) -> Result<()> {
    use crate::NodeView;
    match child.view() {
        NodeView::Bind(bind) => {
            let bspec = match &bind.spec.kind {
                crate::expr::ExprKind::Bind(be) => be,
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
                anyhow!(
                    "emit_clif: non-single-bind let pattern not supported"
                )
            })?;
            let bind_id = bind.single_bind_id();
            emit_let_node(cx, name, bind_id, &bind.node)?;
        }
        // Compile-time-only declarations — skip (mirrors emit_do).
        NodeView::Nop(_) | NodeView::TypeDef(_) | NodeView::Use(_) => {}
        // Expression statement — evaluate, discard the result.
        // (Async machinery can't appear here: any async node
        // fails its emit and the whole block falls back to the
        // node-walk.) A discarded may-bottom scalar is fine — the
        // bottom is never consumed. Owned non-scalar results are
        // dropped: discarding is consuming. (The GIR Discard arm
        // leaks these; doing better here can't diverge values.)
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
fn emit_body_tail<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    node: &crate::Node<R, E>,
    ret: &Type,
) -> Result<()> {
    use crate::NodeView;
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
            let (last, init) =
                blk.children.split_last().ok_or_else(|| {
                    anyhow!("emit_clif: empty block in tail position")
                })?;
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
        _ => {
            let cv = node.emit_clif(cx)?;
            let src = node_composite_source(node);
            emit_kernel_return(cx, ret, cv, src)
        }
    }
}

/// Tail-position select: the shared pattern chain with arms that
/// TERMINATE (return or self tail-call jump) instead of widening to a
/// merge block — the Node twin of the GIR `compile_select_stmt`.
fn emit_select_node_tail<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    sel: &crate::node::select::Select<R, E>,
    ret: &Type,
) -> Result<()> {
    if sel.arms.is_empty() {
        return Err(anyhow!("emit_clif: select with no arms"));
    }
    let (scrut, scrut_kind, scrut_typ) = classify_select_scrutinee(cx, sel)?;
    let scrut_valid = match scrut {
        SelectScrut::Scalar { valid, .. } => valid,
        SelectScrut::Value { .. } | SelectScrut::Opaque => None,
    };
    if scrut_valid.is_some() {
        // Terminating arms have no merge validity phi to poison — a
        // possibly-bottom scrutinee can't propagate its bottom through
        // a `return`. De-fuse (the node-walk handles it).
        return Err(anyhow!(
            "emit_clif: possibly-bottom scrutinee in a tail-position \
             select"
        ));
    }
    emit_select_arms(
        cx,
        sel,
        scrut,
        scrut_kind,
        &scrut_typ,
        scrut_valid,
        &mut |cx, body, mark| {
            emit_body_tail(cx, &body.node, ret)?;
            // The arm terminated; pop its binds for the next arm's
            // compile-time scope. (Arm binds are scalars — no drops.)
            cx.env.truncate(mark);
            Ok(())
        },
    )
}

/// A self-call in tail position: evaluate the new formal values,
/// rebind the leading tail-call slots via `emit_tail_rebind_jump`,
/// and jump to the loop head.
fn emit_self_tail_call<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    cs: &crate::node::callsite::CallSite<R, E>,
) -> Result<()> {
    let spec_apply = match &cs.spec().kind {
        crate::expr::ExprKind::Apply(a) => a,
        _ => {
            return Err(anyhow!(
                "emit_clif: self tail-call spec isn't an Apply"
            ));
        }
    };
    // Labeled args would need default materialization in source order;
    // de-fuse (parity with the GIR path's no-labels rule).
    if spec_apply.args.iter().any(|(label, _)| label.is_some()) {
        return Err(anyhow!(
            "emit_clif: labeled args on a self tail-call"
        ));
    }
    let n = spec_apply.args.len();
    let mut new_vals = Vec::with_capacity(n);
    let mut sources = Vec::with_capacity(n);
    for i in 0..n {
        let arg = cs.arg_positional(i).ok_or_else(|| {
            anyhow!("emit_clif: self tail-call arg {i} missing")
        })?;
        let cv = arg.emit_clif(cx)?;
        let v = match cv {
            CompiledExpr::Single(v) => v,
            // A possibly-bottom arg: no value this cycle means no
            // call — kernel-wide bottom (the node-walk's CallSite
            // wouldn't fire). Same seam as every other eager consumer.
            CompiledExpr::Scalar2 { value, valid } => {
                emit_bottom_abort(cx.b, cx.env, cx.ctx, valid)?;
                value
            }
            CompiledExpr::Value { .. } => {
                return Err(anyhow!(
                    "emit_clif: value-shape self tail-call arg — the \
                     rebind protocol covers scalar/composite slots only"
                ));
            }
        };
        new_vals.push(v);
        sources.push(node_composite_source(arg));
    }
    emit_tail_rebind_jump(cx.b, cx.env, cx.ctx, new_vals, &sources)
}

/// Bind one `let local = value` into the env by the value's runtime
/// shape — the direct-path mirror of `compile_block_scalar`'s let
/// arms (composite/value lets clone borrowed sources so this scope
/// exclusively owns them; the scope-exit drop would otherwise free a
/// buffer the enclosing scope still holds).
fn emit_let_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    name: &ArcStr,
    bind_id: Option<crate::BindId>,
    value: &crate::Node<R, E>,
) -> Result<()> {
    // `freeze_normalized` so a select-valued let (whose type is the
    // un-normalized arm union) still classifies.
    let frozen = gir::freeze_normalized(value.typ());
    match frozen.as_ref().and_then(gir::abi_kind) {
        Some(AbiKind::Scalar(p)) => {
            let cv = value.emit_clif(cx)?;
            match cv {
                CompiledExpr::Single(v) => {
                    let var = cx.b.declare_var(prim_to_clif(p));
                    cx.b.def_var(var, v);
                    cx.env.bind_with_id(name.clone(), var, p, bind_id);
                }
                CompiledExpr::Scalar2 { value, valid } => {
                    let var = cx.b.declare_var(prim_to_clif(p));
                    cx.b.def_var(var, value);
                    let valid_var = cx.b.declare_var(types::I8);
                    cx.b.def_var(valid_var, valid);
                    cx.env.bind_tainted_with_id(
                        name.clone(),
                        var,
                        valid_var,
                        p,
                        bind_id,
                    );
                }
                CompiledExpr::Value { .. } => {
                    return Err(anyhow!(
                        "emit_clif: scalar let got a Value-shape result"
                    ));
                }
            }
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let v = value.emit_clif(cx)?.single()?;
            let owned = ensure_owned_composite_src(
                cx,
                node_composite_source(value),
                v,
            )?;
            let var = cx.b.declare_var(types::I64);
            cx.b.def_var(var, owned);
            cx.env.bind_composite(name.clone(), var);
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let cv = value.emit_clif(cx)?;
            let (disc, payload) = match cv {
                CompiledExpr::Value { disc, payload } => (disc, payload),
                _ => {
                    return Err(anyhow!(
                        "emit_clif: value-shape let got a non-Value result"
                    ));
                }
            };
            let (disc, payload) = ensure_owned_value_src(
                cx,
                node_composite_source(value),
                disc,
                payload,
            )?;
            let disc_var = cx.b.declare_var(types::I64);
            let payload_var = cx.b.declare_var(types::I64);
            cx.b.def_var(disc_var, disc);
            cx.b.def_var(payload_var, payload);
            let vv = ValueVar { disc: disc_var, payload: payload_var };
            if matches!(
                frozen.as_ref().and_then(gir::abi_kind),
                Some(AbiKind::Variant)
            ) {
                cx.env.bind_variant(name.clone(), vv);
            } else {
                cx.env.bind_nullable(name.clone(), vv);
            }
        }
        Some(AbiKind::String) => {
            // String reads/consts are already owned clones.
            let v = value.emit_clif(cx)?.single()?;
            let var = cx.b.declare_var(types::I64);
            cx.b.def_var(var, v);
            cx.env.bind_string(name.clone(), var);
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
fn emit_discard_result<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    node: &crate::Node<R, E>,
    cv: CompiledExpr,
) -> Result<()> {
    let owned =
        matches!(node_composite_source(node), CompositeSource::Owned);
    match cv {
        CompiledExpr::Single(v) => match gir::abi_kind(node.typ()) {
            Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct)
                if owned =>
            {
                let drop = cx.helper("graphix_valarray_drop")?;
                cx.b.ins().call(drop, &[v]);
            }
            Some(AbiKind::String) => {
                let drop = cx.helper("graphix_arcstr_drop")?;
                cx.b.ins().call(drop, &[v]);
            }
            _ => {}
        },
        CompiledExpr::Scalar2 { .. } => {}
        CompiledExpr::Value { disc, payload } => {
            if owned {
                let drop = cx.helper("graphix_value_drop")?;
                cx.b.ins().call(drop, &[disc, payload]);
            }
        }
    }
    Ok(())
}

/// Emit runtime drops for every owned composite / variant / nullable /
/// string local this scope introduced above `mark` — the direct-path
/// mirror of `compile_block_scalar`'s scope-exit drop block. Scalars
/// need no drop.
fn emit_scope_drops(cx: &mut BodyCx, mark: &EnvMark) -> Result<()> {
    let arr_drop = cx.helper("graphix_valarray_drop")?;
    let val_drop = cx.helper("graphix_value_drop")?;
    let str_drop = cx.helper("graphix_arcstr_drop")?;
    for (_, var, _) in &cx.env.composites[mark.composites..] {
        let ptr = cx.b.use_var(*var);
        cx.b.ins().call(arr_drop, &[ptr]);
    }
    for (_, vv) in &cx.env.variants[mark.variants..] {
        let disc = cx.b.use_var(vv.disc);
        let payload = cx.b.use_var(vv.payload);
        cx.b.ins().call(val_drop, &[disc, payload]);
    }
    for (_, vv) in &cx.env.nullables[mark.nullables..] {
        let disc = cx.b.use_var(vv.disc);
        let payload = cx.b.use_var(vv.payload);
        cx.b.ins().call(val_drop, &[disc, payload]);
    }
    for (_, var) in &cx.env.strings[mark.strings..] {
        let ptr = cx.b.use_var(*var);
        cx.b.ins().call(str_drop, &[ptr]);
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
/// `graphix_value_new_from_array` (consumes). A possibly-bottom
/// (`Scalar2`) scalar errors — same as the GIR path's
/// `compile_scalar` → `.single()`.
fn emit_owned_value_operand_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    node: &crate::Node<R, E>,
) -> Result<(ClifValue, ClifValue)> {
    match gir::abi_kind(node.typ()) {
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let (disc, payload) = node.emit_clif(cx)?.value()?;
            ensure_owned_value_src(
                cx,
                node_composite_source(node),
                disc,
                payload,
            )
        }
        Some(AbiKind::Scalar(p)) => {
            let s = node.emit_clif(cx)?.single()?;
            let disc = cx.b.ins().iconst(types::I64, prim_to_value_disc(p));
            let payload = scalar_to_payload_i64(cx.b, p, s);
            Ok((disc, payload))
        }
        Some(AbiKind::String) => {
            // Const/Ref/Concat reads all produce an owned ArcStr;
            // `graphix_value_new_string` consumes it into a Value.
            let s = node.emit_clif(cx)?.single()?;
            let helper = cx.helper("graphix_value_new_string")?;
            let call = cx.b.ins().call(helper, &[s]);
            let r = cx.b.inst_results(call);
            Ok((r[0], r[1]))
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let ptr = node.emit_clif(cx)?.single()?;
            let ptr = ensure_owned_composite_src(
                cx,
                node_composite_source(node),
                ptr,
            )?;
            let helper = cx.helper("graphix_value_new_from_array")?;
            let call = cx.b.ins().call(helper, &[ptr]);
            let r = cx.b.inst_results(call);
            Ok((r[0], r[1]))
        }
        other => {
            Err(anyhow!("emit_clif: value operand has unexpected type {other:?}"))
        }
    }
}

/// Compile one producer-op field and emit the matching
/// `graphix_value_buf_push_*` call into `buf` — the Node twin of
/// `compile_and_push_field` (same helper choice per shape, same
/// owned/borrowed push variant via `node_composite_source`, same
/// bottom-abort for a `Scalar2` field).
fn emit_push_field_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    buf: ClifValue,
    field: &crate::Node<R, E>,
) -> Result<()> {
    let helper_name: &str = match gir::abi_kind(field.typ()) {
        Some(AbiKind::Scalar(p)) => value_buf_push_helper(p)?,
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            match node_composite_source(field) {
                CompositeSource::Owned => "graphix_value_buf_push_array",
                CompositeSource::Borrowed => {
                    "graphix_value_buf_push_array_borrowed"
                }
            }
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            match node_composite_source(field) {
                CompositeSource::Owned => "graphix_value_buf_push_value",
                CompositeSource::Borrowed => {
                    "graphix_value_buf_push_value_borrowed"
                }
            }
        }
        // String SSA is the ArcStr's raw thin-pointer bits (owned);
        // `_push_string` takes it by value (consumes). See the GIR
        // `compile_and_push_field` for why `_push_arcstr` (which
        // derefs a `*const ArcStr`) would be UB here.
        Some(AbiKind::String) => "graphix_value_buf_push_string",
        other => {
            return Err(anyhow!(
                "emit_clif: producer field of shape {other:?} — not \
                 representable"
            ));
        }
    };
    let push = cx.helper(helper_name)?;
    if gir::is_value_shape(field.typ()) {
        let (disc, payload) = field.emit_clif(cx)?.value()?;
        cx.b.ins().call(push, &[buf, disc, payload]);
    } else {
        // A composite producer has no per-field validity channel, so a
        // bottom (`Scalar2`) field must propagate to the kernel
        // OUTPUT: abort to `pending_exit` when the field is invalid,
        // then push the (now-known-valid) value.
        match field.emit_clif(cx)? {
            CompiledExpr::Single(v) => {
                cx.b.ins().call(push, &[buf, v]);
            }
            CompiledExpr::Scalar2 { value, valid } => {
                emit_bottom_abort(cx.b, cx.env, cx.ctx, valid)?;
                cx.b.ins().call(push, &[buf, value]);
            }
            CompiledExpr::Value { .. } => {
                return Err(anyhow!(
                    "emit_clif: scalar producer field compiled to \
                     Value-shape — dispatch is broken"
                ));
            }
        }
    }
    Ok(())
}

/// Tuple / array literal — build a `Vec<Value>` field-by-field via the
/// producer helpers, then finalize into an owned `*mut ValArray`.
/// Mirrors the GIR `TupleNew` arm (array literals lower to the same op
/// — the runtime shape is identical, only the static type differs).
pub(crate) fn emit_tuple_new_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    fields: &[crate::node::Cached<R, E>],
) -> Result<CompiledExpr> {
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let finalize = cx.helper("graphix_valarray_finalize")?;
    let cap = cx.b.ins().iconst(types::I64, fields.len() as i64);
    let call = cx.b.ins().call(buf_new, &[cap]);
    let buf = cx.b.inst_results(call)[0];
    for f in fields {
        emit_push_field_node(cx, buf, &f.node)?;
    }
    let call = cx.b.ins().call(finalize, &[buf]);
    Ok(CompiledExpr::Single(cx.b.inst_results(call)[0]))
}

/// Struct literal — mirrors the GIR `StructNew` arm: an outer ValArray
/// of inner `[name, value]` pairs, fields sorted alphabetically by
/// name (graphix's canonical struct layout, same sort
/// `emit_struct_new` applies when lowering to GIR). Field names are
/// interned lazily via [`BodyCx::interned_str`].
pub(crate) fn emit_struct_new_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    names: &[ArcStr],
    fields: &[crate::node::Cached<R, E>],
) -> Result<CompiledExpr> {
    if names.len() != fields.len() {
        return Err(anyhow!(
            "emit_clif: struct literal name/field arity mismatch"
        ));
    }
    let mut indexed: Vec<(&ArcStr, &crate::node::Cached<R, E>)> =
        names.iter().zip(fields.iter()).collect();
    indexed.sort_by(|a, b| a.0.cmp(b.0));
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let push_arcstr = cx.helper("graphix_value_buf_push_arcstr")?;
    let push_array = cx.helper("graphix_value_buf_push_array")?;
    let finalize = cx.helper("graphix_valarray_finalize")?;
    let outer_cap = cx.b.ins().iconst(types::I64, indexed.len() as i64);
    let call = cx.b.ins().call(buf_new, &[outer_cap]);
    let outer = cx.b.inst_results(call)[0];
    for (name, field) in indexed {
        let inner_cap = cx.b.ins().iconst(types::I64, 2);
        let call = cx.b.ins().call(buf_new, &[inner_cap]);
        let inner = cx.b.inst_results(call)[0];
        let name_ptr = cx.interned_str(name);
        cx.b.ins().call(push_arcstr, &[inner, name_ptr]);
        emit_push_field_node(cx, inner, &field.node)?;
        let call = cx.b.ins().call(finalize, &[inner]);
        let inner_arr = cx.b.inst_results(call)[0];
        cx.b.ins().call(push_array, &[outer, inner_arr]);
    }
    let call = cx.b.ins().call(finalize, &[outer]);
    Ok(CompiledExpr::Single(cx.b.inst_results(call)[0]))
}

/// Variant constructor — mirrors the GIR `VariantNew` arm. Nullary →
/// `Value::String(tag)` via `graphix_value_new_string_from_arcstr`
/// (clones the interned tag — see the GIR arm for why the clone is
/// mandatory). With payloads → `Value::Array([tag, p0, ...])` built
/// via the buf helpers and unwrapped into a two-register Value by
/// `graphix_value_new_from_array`. The tag is interned lazily via
/// [`BodyCx::interned_str`].
pub(crate) fn emit_variant_new_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    tag: &ArcStr,
    payloads: &[crate::node::Cached<R, E>],
) -> Result<CompiledExpr> {
    let tag_ptr = cx.interned_str(tag);
    if payloads.is_empty() {
        let new_str = cx.helper("graphix_value_new_string_from_arcstr")?;
        let call = cx.b.ins().call(new_str, &[tag_ptr]);
        let r = cx.b.inst_results(call);
        Ok(CompiledExpr::Value { disc: r[0], payload: r[1] })
    } else {
        let buf_new = cx.helper("graphix_value_buf_new")?;
        let push_arcstr = cx.helper("graphix_value_buf_push_arcstr")?;
        let finalize = cx.helper("graphix_valarray_finalize")?;
        let wrap_array = cx.helper("graphix_value_new_from_array")?;
        let cap = cx.b.ins().iconst(types::I64, (payloads.len() + 1) as i64);
        let call = cx.b.ins().call(buf_new, &[cap]);
        let buf = cx.b.inst_results(call)[0];
        cx.b.ins().call(push_arcstr, &[buf, tag_ptr]);
        for p in payloads {
            emit_push_field_node(cx, buf, &p.node)?;
        }
        let call = cx.b.ins().call(finalize, &[buf]);
        let arr = cx.b.inst_results(call)[0];
        let call = cx.b.ins().call(wrap_array, &[arr]);
        let r = cx.b.inst_results(call);
        Ok(CompiledExpr::Value { disc: r[0], payload: r[1] })
    }
}

/// Compile an accessor's composite source to a `*const ValArray`,
/// returning the pointer plus its ownership classification. Shared by
/// the tuple/struct/array element-read relays; the caller drops an
/// Owned pointer after the read (the element helpers clone the slot
/// out, so the temporary producer would otherwise leak — a Borrowed
/// read stays owned by its env slot).
fn emit_accessor_source_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    source: &crate::Node<R, E>,
    want: AbiKind,
) -> Result<(ClifValue, CompositeSource)> {
    if gir::abi_kind(source.typ()) != Some(want) {
        return Err(anyhow!(
            "emit_clif: accessor source of type {:?} isn't {want:?}",
            source.typ()
        ));
    }
    let src = node_composite_source(source);
    let ptr = source.emit_clif(cx)?.single()?;
    Ok((ptr, src))
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

/// `t.<idx>` — mirrors the GIR `TupleGet` arm: a statically-valid
/// index, read through `compile_element_read` (owned result; Value
/// shape for a value-shape element, Single otherwise).
pub(crate) fn emit_tuple_ref_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    source: &crate::Node<R, E>,
    idx: usize,
    elem_typ: &Type,
) -> Result<CompiledExpr> {
    // Node-carried elem types can be Refs to abstract type names
    // (#218) — resolve before the read classifies by abi_kind.
    let elem_typ = resolve_node_typ(cx.ctx, elem_typ);
    let (arr_ptr, src) =
        emit_accessor_source_node(cx, source, AbiKind::Tuple)?;
    let idx_const = cx.b.ins().iconst(types::I64, idx as i64);
    let result = compile_element_read(
        cx.b, arr_ptr, idx_const, &elem_typ, false, cx.ctx,
    )?;
    emit_accessor_source_drop(cx, arr_ptr, src)?;
    Ok(result)
}

/// `s.field` — mirrors the GIR `StructGet` arm (the two-level kv-pair
/// read via the `struct_get_*` helper family). `sorted_idx` is the
/// field's position in the struct type's canonical (sorted) layout —
/// resolved by the node's typecheck.
pub(crate) fn emit_struct_ref_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    source: &crate::Node<R, E>,
    sorted_idx: usize,
    elem_typ: &Type,
) -> Result<CompiledExpr> {
    // Same abstract-Ref resolution as the tuple read (#218).
    let elem_typ = resolve_node_typ(cx.ctx, elem_typ);
    let (arr_ptr, src) =
        emit_accessor_source_node(cx, source, AbiKind::Struct)?;
    let idx_const = cx.b.ins().iconst(types::I64, sorted_idx as i64);
    let result = compile_element_read(
        cx.b, arr_ptr, idx_const, &elem_typ, true, cx.ctx,
    )?;
    emit_accessor_source_drop(cx, arr_ptr, src)?;
    Ok(result)
}

/// `a[i]` / `bytes[i]` — mirrors the GIR `ArrayGet` / `BytesIndex`
/// arms. The result type is always `Nullable<elem>` (out-of-bounds →
/// the `ArrayIndexError` Value), produced by the shared bounds-checked
/// helpers (`graphix_valarray_index` routes through the node-walk's
/// own `node::array::array_index`, `graphix_bytes_index` through
/// `bytes_index` — all backends agree bit-for-bit).
pub(crate) fn emit_array_ref_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    source: &crate::Node<R, E>,
    idx: &crate::Node<R, E>,
) -> Result<CompiledExpr> {
    let idx_prim = gir::scalar_prim(idx.typ())
        .filter(|p| p.is_integer())
        .ok_or_else(|| {
            anyhow!(
                "emit_clif: index of non-integer type {:?}",
                idx.typ()
            )
        })?;
    if matches!(gir::abi_kind(source.typ()), Some(AbiKind::Array)) {
        let (arr_ptr, src) =
            emit_accessor_source_node(cx, source, AbiKind::Array)?;
        let idx_val = idx.emit_clif(cx)?.single()?;
        let idx_i64 = widen_to_i64(cx.b, idx_val, idx_prim);
        let helper = cx.helper("graphix_valarray_index")?;
        let call = cx.b.ins().call(helper, &[arr_ptr, idx_i64]);
        let r = cx.b.inst_results(call);
        let (disc, payload) = (r[0], r[1]);
        emit_accessor_source_drop(cx, arr_ptr, src)?;
        return Ok(CompiledExpr::Value { disc, payload });
    }
    if crate::fusion::lowering::is_bytes(source.typ()) {
        // The helper consumes the bytes operand — owned, like the GIR
        // `BytesIndex` arm.
        let (bd, bp) = emit_owned_value_operand_node(cx, source)?;
        let i = idx.emit_clif(cx)?.single()?;
        let helper = cx.helper("graphix_bytes_index")?;
        let call = cx.b.ins().call(helper, &[bd, bp, i]);
        let r = cx.b.inst_results(call);
        return Ok(CompiledExpr::Value { disc: r[0], payload: r[1] });
    }
    Err(anyhow!(
        "emit_clif: index source of type {:?} isn't an array or bytes",
        source.typ()
    ))
}

/// `m{key}` — mirrors the GIR `MapRef` arm: both operands as OWNED
/// `(disc, payload)` Values (the helper consumes them);
/// `graphix_map_ref` does the lookup (shared `node::map::map_get`
/// semantics), returning `Nullable<V>` as two words.
pub(crate) fn emit_map_ref_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    source: &crate::Node<R, E>,
    key: &crate::Node<R, E>,
) -> Result<CompiledExpr> {
    if !crate::fusion::lowering::is_map(source.typ()) {
        return Err(anyhow!(
            "emit_clif: map-ref source of type {:?} isn't a Map",
            source.typ()
        ));
    }
    let (md, mp) = emit_owned_value_operand_node(cx, source)?;
    let (kd, kp) = emit_owned_value_operand_node(cx, key)?;
    let helper = cx.helper("graphix_map_ref")?;
    let call = cx.b.ins().call(helper, &[md, mp, kd, kp]);
    let r = cx.b.inst_results(call);
    Ok(CompiledExpr::Value { disc: r[0], payload: r[1] })
}

/// `a[i..j]` — mirrors the GIR `ArraySlice` arm: the source as an
/// OWNED Value (the helper consumes it; a composite source is wrapped
/// via `graphix_value_new_from_array`), present bounds as integer
/// scalars with a flag bit each, absent bounds pass 0 with the bit
/// cleared. Result is `Nullable<source>` (shared
/// `node::array::array_slice` semantics).
pub(crate) fn emit_array_slice_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    source: &crate::Node<R, E>,
    start: Option<&crate::Node<R, E>>,
    end: Option<&crate::Node<R, E>>,
) -> Result<CompiledExpr> {
    if !(matches!(gir::abi_kind(source.typ()), Some(AbiKind::Array))
        || crate::fusion::lowering::is_bytes(source.typ()))
    {
        return Err(anyhow!(
            "emit_clif: slice source of type {:?} isn't an array or bytes",
            source.typ()
        ));
    }
    let (sd, sp) = emit_owned_value_operand_node(cx, source)?;
    let emit_bound = |cx: &mut BodyCx,
                          n: Option<&crate::Node<R, E>>,
                          flag: i64,
                          flags: &mut i64|
     -> Result<ClifValue> {
        match n {
            None => Ok(cx.b.ins().iconst(types::I64, 0)),
            Some(n) => {
                if !gir::scalar_prim(n.typ()).is_some_and(|p| p.is_integer())
                {
                    return Err(anyhow!(
                        "emit_clif: slice bound of non-integer type {:?}",
                        n.typ()
                    ));
                }
                *flags |= flag;
                n.emit_clif(cx)?.single()
            }
        }
    };
    let mut flags = 0i64;
    let start_v = emit_bound(cx, start, 1, &mut flags)?;
    let end_v = emit_bound(cx, end, 2, &mut flags)?;
    let flags_v = cx.b.ins().iconst(types::I64, flags);
    let helper = cx.helper("graphix_array_slice")?;
    let call = cx.b.ins().call(helper, &[sd, sp, start_v, end_v, flags_v]);
    let r = cx.b.inst_results(call);
    Ok(CompiledExpr::Value { disc: r[0], payload: r[1] })
}

/// `?` / `$` — both unwrap a Nullable<T> to T (else pass the value
/// through unchanged for a non-Nullable inner, mirroring `wrap_qop`'s
/// None branch). The Node twin of `wrap_qop` fused with the two GIR
/// `QopUnwrap` compile arms (`compile_scalar_qop_unwrap` for scalar /
/// string / composite success, the `compile_value_expr` arm for
/// Value-shape success).
pub(crate) fn emit_qop_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    inner: &crate::Node<R, E>,
) -> Result<CompiledExpr> {
    let Some(inner_typ) = gir::freeze_concrete(inner.typ()) else {
        return Err(anyhow!(
            "emit_clif: `?` inner type {:?} doesn't freeze concrete",
            inner.typ()
        ));
    };
    let Some(success_typ) = gir::nullable_inner(&inner_typ) else {
        return inner.emit_clif(cx);
    };
    let cv = inner.emit_clif(cx)?;
    let (disc, payload) = match cv {
        CompiledExpr::Value { disc, payload } => (disc, payload),
        _ => {
            return Err(anyhow!(
                "emit_clif: `?` inner not Value-shape — a Nullable inner \
                 must compile to (disc, payload)"
            ));
        }
    };
    // `disc == Typ::Error` (`0x2000_0000`) means bottom.
    let is_err = cx.b.ins().icmp_imm(IntCC::Equal, disc, 0x2000_0000_i64);
    match gir::abi_kind(&success_typ) {
        // Prim success — BRANCHLESS per-value validity. The payload
        // word holds the success bits when !is_err; on the error path
        // the bits are garbage but `valid=0` means they're never used.
        // The error Value isn't dropped here: a `Value::Error` carries
        // an `Arc<Value>` payload, but a scalar `Nullable` inner is a
        // by-value scalar (no heap), so nothing leaks. (A Local-read
        // Borrowed inner is owned by its env slot and dropped at scope
        // exit either way.)
        Some(AbiKind::Scalar(p)) => {
            let value = cast_u64_to_prim(cx.b, payload, p);
            let one = cx.b.ins().iconst(types::I8, 1);
            let valid = cx.b.ins().bxor(is_err, one); // !is_err
            Ok(CompiledExpr::Scalar2 { value, valid })
        }
        // String / composite success — keep the branch-abort path.
        Some(
            AbiKind::String | AbiKind::Array | AbiKind::Tuple | AbiKind::Struct,
        ) => {
            let pending_set = cx.helper("graphix_dyncall_set_pending")?;
            let value_drop = cx.helper("graphix_value_drop")?;
            let inner_owned =
                node_composite_source(inner) == CompositeSource::Owned;
            let pre_pending = cx.b.create_block();
            let continue_block = cx.b.create_block();
            let pending_exit = {
                let mut slot = cx.ctx.pending_exit.borrow_mut();
                match *slot {
                    Some(blk) => blk,
                    None => {
                        let blk = cx.b.create_block();
                        *slot = Some(blk);
                        blk
                    }
                }
            };
            cx.b.ins().brif(is_err, pre_pending, &[], continue_block, &[]);
            cx.b.switch_to_block(pre_pending);
            cx.b.seal_block(pre_pending);
            // Drop the owned error Value only when `inner` is an owned
            // producer (a Borrowed Local is owned by its env slot, which
            // `emit_pending_cleanup` drops — dropping here too would
            // double-free).
            if inner_owned {
                cx.b.ins().call(value_drop, &[disc, payload]);
            }
            cx.b.ins().call(pending_set, &[]);
            emit_pending_cleanup(cx.b, cx.env, cx.ctx)?;
            cx.b.ins().jump(pending_exit, &[]);
            cx.b.switch_to_block(continue_block);
            cx.b.seal_block(continue_block);
            // Extract success T (now known non-error). The unwrap
            // result is Owned, so a String/composite success from a
            // Borrowed inner must be cloned. String: the ArcStr bits
            // inside a Value ARE the string ABI's one-word
            // representation. Composite: the payload word is the
            // ValArray BITS, but the composite ABI is a boxed
            // `*mut ValArray` — re-box via `graphix_value_into_array`
            // (consumes) / `_borrowed` (clones inner); see the GIR arm
            // (#199).
            let v = match gir::abi_kind(&success_typ) {
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
                    let call = cx.b.ins().call(unbox, &[disc, payload]);
                    cx.b.inst_results(call)[0]
                }
            };
            Ok(CompiledExpr::Single(v))
        }
        // Value-shape success. On `Error` disc, drop the owned inner,
        // signal pending, jump to `pending_exit`; otherwise the
        // non-error Value IS the result T (its own `(disc, payload)`,
        // passed through to the consumer which takes ownership).
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let pending_set = cx.helper("graphix_dyncall_set_pending")?;
            let value_drop = cx.helper("graphix_value_drop")?;
            let src = node_composite_source(inner);
            let pre_pending = cx.b.create_block();
            let continue_block = cx.b.create_block();
            let pending_exit = {
                let mut slot = cx.ctx.pending_exit.borrow_mut();
                match *slot {
                    Some(blk) => blk,
                    None => {
                        let blk = cx.b.create_block();
                        *slot = Some(blk);
                        blk
                    }
                }
            };
            cx.b.ins().brif(is_err, pre_pending, &[], continue_block, &[]);
            cx.b.switch_to_block(pre_pending);
            cx.b.seal_block(pre_pending);
            // Drop the owned error Value before aborting — but ONLY when
            // `inner` is an owned producer. A Borrowed (Ref) inner is
            // owned by its env slot, which `emit_pending_cleanup` ->
            // `drop_owned_composites` already drops; dropping it here too
            // would double-free (Arc double-decrement / use-after-free).
            if src == CompositeSource::Owned {
                cx.b.ins().call(value_drop, &[disc, payload]);
            }
            cx.b.ins().call(pending_set, &[]);
            emit_pending_cleanup(cx.b, cx.env, cx.ctx)?;
            cx.b.ins().jump(pending_exit, &[]);
            cx.b.switch_to_block(continue_block);
            cx.b.seal_block(continue_block);
            // The non-error Value IS the result T. Ensure it's owned: a
            // Borrowed (Ref) inner aliases its env slot, which is also
            // dropped at scope exit — handing those bits to the consumer
            // (the unwrap result is Owned) would double-free. An Owned
            // inner passes through unchanged.
            let (od, op) = ensure_owned_value_src(cx, src, disc, payload)?;
            Ok(CompiledExpr::Value { disc: od, payload: op })
        }
        Some(AbiKind::Unit | AbiKind::Null) | None => Err(anyhow!(
            "emit_clif: `?` with unsupported success type {:?}",
            success_typ
        )),
    }
}

/// Builtin DynCall — marshal the (marshal-ordered) `args` into a
/// fresh `LPooled<Vec<Value>>` buf, dispatch via `graphix_dyncall`
/// against the `FnSource::Builtin` slot at `info.fn_index`, then
/// decode the return per shape. The Node twin of
/// `marshal_dyncall_args` fused with the two GIR `DynCall` decode
/// arms (`compile_scalar_impl` for scalar / unit / string / composite
/// returns, the `compile_value_expr` arm for Value-shape returns).
pub(crate) fn emit_dyncall_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    info: &crate::fusion::lowering::BuiltinCallSiteInfo,
    args: &[&crate::Node<R, E>],
) -> Result<CompiledExpr> {
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let cap = cx.b.ins().iconst(types::I64, args.len() as i64);
    let call = cx.b.ins().call(buf_new, &[cap]);
    let buf = cx.b.inst_results(call)[0];
    let buf_var = cx.b.declare_var(types::I64);
    cx.b.def_var(buf_var, buf);
    cx.ctx.dyncall_buf_stack.borrow_mut().push(buf_var);
    for (arg_node, t) in args.iter().zip(info.arg_types.iter()) {
        // Compare by runtime SHAPE (`AbiKind`), not exact `Type` —
        // the direct twin of the lowering-side agreement check. The
        // DynCall marshals by `info.arg_types`, so only the shape
        // needs to agree; a mismatch aborts the kernel (the classic
        // path refuses at lowering — same net effect, the subtree
        // node-walks).
        let Some(frozen) = gir::freeze_normalized(arg_node.typ()) else {
            return Err(anyhow!(
                "emit_clif: DynCall arg type {:?} doesn't freeze concrete",
                arg_node.typ()
            ));
        };
        if gir::abi_kind(&frozen) != gir::abi_kind(t) {
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
        let helper_name: &str = match gir::abi_kind(t) {
            Some(AbiKind::Scalar(p)) => value_buf_push_helper(p)?,
            Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                match node_composite_source(arg_node) {
                    CompositeSource::Owned => "graphix_value_buf_push_array",
                    CompositeSource::Borrowed => {
                        "graphix_value_buf_push_array_borrowed"
                    }
                }
            }
            // Variant / Nullable / datetime / duration / bytes / map /
            // error all ride the two-word `(disc, payload)` Value wire
            // shape.
            Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
                match node_composite_source(arg_node) {
                    CompositeSource::Owned => "graphix_value_buf_push_value",
                    CompositeSource::Borrowed => {
                        "graphix_value_buf_push_value_borrowed"
                    }
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
        if gir::is_value_shape(t) {
            let (disc, payload) = arg_node.emit_clif(cx)?.value()?;
            cx.b.ins().call(push, &[buf, disc, payload]);
        } else {
            // `.single()` rejects a possibly-bottom `Scalar2` arg — on
            // the classic path a may-bottom arg never reaches
            // marshalling; bailing the kernel here matches it.
            let v = arg_node.emit_clif(cx)?.single()?;
            cx.b.ins().call(push, &[buf, v]);
        }
    }
    let dyncall = cx.helper("graphix_dyncall")?;
    let ret_kind: i64 = match gir::abi_kind(&info.return_type) {
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
    let fn_idx_val = cx.b.ins().iconst(types::I32, info.fn_index as i64);
    let ret_kind_val = cx.b.ins().iconst(types::I8, ret_kind);
    let call = cx.b.ins().call(dyncall, &[fn_idx_val, buf, ret_kind_val]);
    // `graphix_dyncall` has two `I64` returns (disc, payload) so that
    // ret_kind=2 (Value-shape) can deliver both words. For
    // scalar/composite/unit/string return kinds the first return
    // holds the value (or pointer / 0); the second is undefined.
    let (raw0, raw1) = {
        let r = cx.b.inst_results(call);
        (r[0], r[1])
    };
    // `graphix_dyncall` consumed the args buf — pop it off the
    // in-flight stack before the (possible) pending branch below, so
    // a `pre_pending` block here doesn't try to double-free it.
    cx.ctx.dyncall_buf_stack.borrow_mut().pop();
    match gir::abi_kind(&info.return_type) {
        Some(AbiKind::Scalar(p)) => {
            // Scalar return: 0 on pending is a harmless sentinel for
            // downstream scalar arithmetic. No branch needed — the
            // wrapper-level DYNCALL_PENDING check in GirNode::update
            // discards the whole kernel result.
            Ok(CompiledExpr::Single(cast_u64_to_prim(cx.b, raw0, p)))
        }
        Some(AbiKind::Unit) => {
            // Unit return: dispatcher returned (0, _). The wrapper-
            // level DYNCALL_PENDING check still fires.
            Ok(CompiledExpr::Single(raw0))
        }
        Some(AbiKind::String) => {
            // String return: `raw0` is the ArcStr's raw thin-pointer
            // bits (transferred ownership), or the null sentinel on
            // pending. Null bits are NOT inert downstream the way a
            // scalar 0 is — every String position assumes a valid
            // owned ArcStr (`emit_return_pending_check` / scope-exit
            // `drop_owned_strings` would null-drop it; #214) — so
            // branch exactly like the composite arm.
            emit_dyncall_pending_branch(cx.b, cx.env, cx.ctx)?;
            Ok(CompiledExpr::Single(raw0))
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            // Composite return: `raw0` is an owned `*mut ValArray`,
            // or null on pending. Null can't be deref'd by downstream
            // ops, so we branch: on pending, drop every owned local +
            // every outer in-flight DynCall buf, then jump to
            // `pending_exit`. (`pending_take` READS without clearing,
            // so no re-set is needed.)
            emit_dyncall_pending_branch(cx.b, cx.env, cx.ctx)?;
            Ok(CompiledExpr::Single(raw0))
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            // Value-shape return: both register-words, same pending
            // branch as the composite arm (the boxed Value sentinel is
            // null on pending).
            emit_dyncall_pending_branch(cx.b, cx.env, cx.ctx)?;
            Ok(CompiledExpr::Value { disc: raw0, payload: raw1 })
        }
        Some(AbiKind::Null) | None => Err(anyhow!(
            "DynCall with bare Null / non-fusable return — \
             should have widened to Nullable<T> at construction"
        )),
    }
}

/// How a `select`'s arms merge into one result — derived from the
/// select node's frozen result type. Scalar merges always thread an I8
/// validity phi (`expr_may_value_bottom` has no Node twin, so every
/// arm passes a validity arg; the result is reported `Scalar2` only
/// when something actually tainted). String/composite merges have no
/// validity channel — a possibly-bottom scrutinee with one of those
/// result shapes refuses to fuse instead.
#[derive(Clone, Copy)]
enum SelectMerge {
    Scalar(PrimType),
    Value,
    Composite,
    String,
}

/// The select scrutinee, emitted exactly ONCE up front; every arm
/// condition and pattern bind reuses these SSA values (the direct
/// path's form of the GIR `__sel_scrut` stabilization — SSA reuse
/// gives eval-once for free). `Opaque` (string / composite) supports
/// only Ignore / guard arms, none of which can test the value.
#[derive(Clone, Copy)]
enum SelectScrut {
    Scalar { value: ClifValue, valid: Option<ClifValue>, prim: PrimType },
    Value { disc: ClifValue, payload: ClifValue },
    Opaque,
}

/// A pattern binding to install in the arm's matched region, under the
/// pattern's real `BindId` (the arm body's `Ref`s resolve BindId-first,
/// so no shadow guard is needed — unlike the GIR `known_consts`
/// channel).
enum SelectArmBind {
    /// `n => ...` — bind the scalar scrutinee itself.
    Scrut(crate::BindId),
    /// `` `Tag(n) `` — bind one scalar variant payload. The read uses
    /// `unreachable_unchecked` on a wrong-tag value, so it MUST be
    /// emitted inside the matched region (after the tag-eq branch) —
    /// never in the fall-through chain. (The GIR path inlines payload
    /// reads into the cond via known_consts; the node-walk evaluates
    /// binds only after the pattern matches — we follow the node-walk.)
    Payload { id: crate::BindId, idx: usize, prim: PrimType },
}

/// `select` at expression position — the Node twin of
/// `emit_select_as_expr` (lowering) + `compile_ifchain` (codegen)
/// fused into one pass. Canonical semantics are `Select::update` /
/// `PatternNode::is_match` (node/select.rs, node/pattern.rs):
///
/// - the scrutinee is evaluated once; no scrutinee value → no select
///   value (a `Scalar2` scrutinee's validity ANDs into every arm's
///   result validity — the #178 scrutinee gate);
/// - an explicit type predicate is TESTED (`null as _` → IsNull;
///   `i64 as _` over `[i64, null]` → NOT-null). The GIR path emits
///   the non-null case as trivially-true, which is order-unsound —
///   here the test is explicit so arm order is right by construction;
/// - a guard runs only after the pattern matches, with the pattern's
///   binds in scope; a bottom guard means the arm does NOT match;
/// - the first matching arm wins; an arm with no condition and no
///   guard takes the chain unconditionally.
///
/// The final-arm miss trap mirrors `compile_ifchain`, but is emitted
/// only where typecheck's exhaustiveness makes it unreachable: a
/// guarded final arm, or a conditional final arm under a possibly-
/// bottom scrutinee (whose garbage cond bits could miss every arm),
/// refuse to fuse instead. (The GIR path traps live in that second
/// case — see the C5 findings.)
pub(crate) fn emit_select_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    sel: &crate::node::select::Select<R, E>,
) -> Result<CompiledExpr> {
    if sel.arms.is_empty() {
        return Err(anyhow!("emit_clif: select with no arms"));
    }
    let result_typ = gir::freeze_normalized(sel.typ()).ok_or_else(|| {
        anyhow!(
            "emit_clif: select result type {:?} doesn't freeze concrete",
            sel.typ()
        )
    })?;
    let merge_shape = match gir::abi_kind(&result_typ) {
        Some(AbiKind::Scalar(p)) => SelectMerge::Scalar(p),
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            SelectMerge::Value
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            SelectMerge::Composite
        }
        Some(AbiKind::String) => SelectMerge::String,
        other @ (Some(AbiKind::Unit | AbiKind::Null) | None) => {
            return Err(anyhow!(
                "emit_clif: select result shape {other:?} not representable"
            ));
        }
    };
    let (scrut, scrut_kind, scrut_typ) = classify_select_scrutinee(cx, sel)?;
    let scrut_valid = match scrut {
        SelectScrut::Scalar { valid, .. } => valid,
        SelectScrut::Value { .. } | SelectScrut::Opaque => None,
    };
    if scrut_valid.is_some() && !matches!(merge_shape, SelectMerge::Scalar(_))
    {
        // Only the scalar merge has a validity phi to poison; the
        // node-walk's "bottom scrutinee → no select value" can't be
        // expressed through the other merge shapes here.
        return Err(anyhow!(
            "emit_clif: possibly-bottom scrutinee with a non-scalar \
             select result"
        ));
    }
    let merge = cx.b.create_block();
    match merge_shape {
        SelectMerge::Scalar(p) => {
            cx.b.append_block_param(merge, prim_to_clif(p));
            cx.b.append_block_param(merge, types::I8); // validity
        }
        SelectMerge::Value => {
            cx.b.append_block_param(merge, types::I64); // disc
            cx.b.append_block_param(merge, types::I64); // payload
        }
        SelectMerge::Composite | SelectMerge::String => {
            cx.b.append_block_param(merge, types::I64);
        }
    }
    let mut any_taint = scrut_valid.is_some();
    emit_select_arms(
        cx,
        sel,
        scrut,
        scrut_kind,
        &scrut_typ,
        scrut_valid,
        &mut |cx, body, mark| {
            emit_select_value_arm(
                cx,
                body,
                mark,
                merge_shape,
                merge,
                scrut_valid,
                &mut any_taint,
            )
        },
    )?;
    cx.b.switch_to_block(merge);
    cx.b.seal_block(merge);
    match merge_shape {
        SelectMerge::Scalar(_) => {
            let params = cx.b.block_params(merge);
            let (value, valid) = (params[0], params[1]);
            Ok(if any_taint {
                CompiledExpr::Scalar2 { value, valid }
            } else {
                // Nothing tainted: every arm passed `iconst 1` — the
                // validity phi is dead and the result composes as a
                // clean scalar (parity with the classic taint
                // optimization).
                CompiledExpr::Single(value)
            })
        }
        SelectMerge::Value => {
            let params = cx.b.block_params(merge);
            Ok(CompiledExpr::Value { disc: params[0], payload: params[1] })
        }
        SelectMerge::Composite | SelectMerge::String => {
            let params = cx.b.block_params(merge);
            Ok(CompiledExpr::Single(params[0]))
        }
    }
}

/// Classify (and emit the read of) a select scrutinee: the shared
/// prologue of the value-position and tail-position select emitters.
fn classify_select_scrutinee<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    sel: &crate::node::select::Select<R, E>,
) -> Result<(SelectScrut, AbiKind, Type)> {
    let scrut_typ =
        gir::freeze_normalized(sel.arg.node.typ()).ok_or_else(|| {
            anyhow!(
                "emit_clif: select scrutinee type {:?} doesn't freeze \
                 concrete",
                sel.arg.node.typ()
            )
        })?;
    let scrut_kind = gir::abi_kind(&scrut_typ).ok_or_else(|| {
        anyhow!("emit_clif: select scrutinee shape not classifiable")
    })?;
    let scrut = match scrut_kind {
        AbiKind::Scalar(p) => match sel.arg.node.emit_clif(cx)? {
            CompiledExpr::Single(value) => {
                SelectScrut::Scalar { value, valid: None, prim: p }
            }
            CompiledExpr::Scalar2 { value, valid } => {
                SelectScrut::Scalar { value, valid: Some(valid), prim: p }
            }
            CompiledExpr::Value { .. } => {
                return Err(anyhow!(
                    "emit_clif: scalar select scrutinee produced a \
                     Value-shape result"
                ));
            }
        },
        AbiKind::Variant | AbiKind::Nullable | AbiKind::Value => {
            // The (disc, payload) pair stays live across the whole arm
            // chain with no drop path, so it must be a borrowed env
            // slot (a Ref read) — the GIR variant arms had the same
            // Local-only rule. An owned producer scrutinee would leak.
            if node_composite_source(&sel.arg.node)
                != CompositeSource::Borrowed
            {
                return Err(anyhow!(
                    "emit_clif: owned value-shape select scrutinee — no \
                     drop path across the arm chain"
                ));
            }
            let (disc, payload) = sel.arg.node.emit_clif(cx)?.value()?;
            SelectScrut::Value { disc, payload }
        }
        // String / composite scrutinees support only Ignore / guard
        // arms (no condition can test them); a borrowed read has no
        // effects to evaluate, so nothing is emitted. (A string Ref
        // read CLONES — emitting it here would leak the clone.)
        AbiKind::String | AbiKind::Array | AbiKind::Tuple | AbiKind::Struct => {
            if node_composite_source(&sel.arg.node)
                != CompositeSource::Borrowed
            {
                return Err(anyhow!(
                    "emit_clif: owned opaque select scrutinee — no drop \
                     path across the arm chain"
                ));
            }
            SelectScrut::Opaque
        }
        AbiKind::Unit | AbiKind::Null => {
            return Err(anyhow!(
                "emit_clif: select scrutinee of shape {scrut_kind:?}"
            ));
        }
    };
    Ok((scrut, scrut_kind, scrut_typ))
}

/// The shared select arm chain: pattern conditions (type predicate /
/// structure / guard), per-arm binds, and the fail-block plumbing —
/// identical between value position (arms widen and jump to a merge
/// block) and tail position (arms terminate with a return or a self
/// tail-call jump). `emit_arm` supplies the position-specific arm-body
/// emission; it runs in the matched block with the arm's binds
/// installed and MUST leave the block terminated (jump or return).
/// `mark` is the env state to truncate back to after the body.
fn emit_select_arms<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    sel: &crate::node::select::Select<R, E>,
    scrut: SelectScrut,
    scrut_kind: AbiKind,
    scrut_typ: &Type,
    scrut_valid: Option<ClifValue>,
    emit_arm: &mut dyn FnMut(
        &mut BodyCx,
        &crate::node::Cached<R, E>,
        EnvMark,
    ) -> Result<()>,
) -> Result<()> {
    use crate::node::pattern::StructPatternNode;
    let n = sel.arms.len();
    for (i, (pat, body)) in sel.arms.iter().enumerate() {
        let is_last = i == n - 1;
        // Type-predicate condition. The node-walk tests the predicate
        // only when it's explicit (`PatternNode::is_match`); an
        // inferred predicate imposes no runtime test.
        let tcond: Option<ClifValue> = if !pat.explicit_type_predicate {
            None
        } else {
            let pred = gir::freeze_concrete(&pat.type_predicate)
                .ok_or_else(|| {
                    anyhow!(
                        "emit_clif: select type predicate {:?} doesn't \
                         freeze concrete",
                        pat.type_predicate
                    )
                })?;
            match &pred {
                Type::Primitive(p)
                    if p.contains(netidx_value::Typ::Null)
                        && p.iter().count() == 1 =>
                {
                    match scrut {
                        SelectScrut::Value { disc, .. }
                            if matches!(scrut_kind, AbiKind::Nullable) =>
                        {
                            Some(cx.b.ins().icmp_imm(
                                IntCC::Equal,
                                disc,
                                value_disc::NULL,
                            ))
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
                    if !p.contains(netidx_value::Typ::Null)
                        && p.iter().count() == 1 =>
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
                                && gir::nullable_inner(&scrut_typ)
                                    .as_ref()
                                    .and_then(gir::scalar_prim)
                                    == PrimType::from_typ(pt) =>
                        {
                            // `[T, null]` runtime value is T or null,
                            // so "is a T" ≡ "is not null" — tested,
                            // not assumed (order-sound).
                            Some(cx.b.ins().icmp_imm(
                                IntCC::NotEqual,
                                disc,
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
        // Structure condition + the binds to install once matched.
        let mut binds: Vec<SelectArmBind> = Vec::new();
        let scond: Option<ClifValue> = match &pat.structure_predicate {
            StructPatternNode::Ignore => None,
            StructPatternNode::Bind(id) => match scrut {
                SelectScrut::Scalar { .. } => {
                    binds.push(SelectArmBind::Scrut(*id));
                    None
                }
                SelectScrut::Value { .. } | SelectScrut::Opaque => {
                    return Err(anyhow!(
                        "emit_clif: non-scalar scrutinee bind pattern not \
                         yet lowerable"
                    ));
                }
            },
            StructPatternNode::Literal(v) => {
                let lit_prim =
                    gir::scalar_prim_of_value(v).ok_or_else(|| {
                        anyhow!(
                            "emit_clif: non-scalar literal pattern {v:?}"
                        )
                    })?;
                match scrut {
                    SelectScrut::Scalar { value, prim, .. }
                        if prim == lit_prim =>
                    {
                        let lit = compile_const(cx.b, v, lit_prim);
                        Some(compile_cmp(
                            cx.b,
                            CmpOp::Eq,
                            lit_prim,
                            value,
                            lit,
                        ))
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
                // this arm, unlike the GIR path's whole-scrutinee
                // VariantInfo case table.
                let pred = gir::freeze_concrete(&pat.type_predicate)
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
                for (idx, (sub, elt)) in
                    pbinds.iter().zip(elts.iter()).enumerate()
                {
                    match sub {
                        StructPatternNode::Bind(id) => {
                            let prim = gir::scalar_prim(elt)
                                .ok_or_else(|| {
                                    anyhow!(
                                        "emit_clif: non-scalar variant \
                                         payload {elt:?}"
                                    )
                                })?;
                            binds.push(SelectArmBind::Payload {
                                id: *id,
                                idx,
                                prim,
                            });
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
                let call =
                    cx.b.ins().call(helper, &[disc, payload, tag_ptr]);
                Some(cx.b.inst_results(call)[0])
            }
            StructPatternNode::Slice { .. }
            | StructPatternNode::SlicePrefix { .. }
            | StructPatternNode::SliceSuffix { .. }
            | StructPatternNode::Struct { .. } => {
                return Err(anyhow!(
                    "emit_clif: slice/tuple/struct select pattern not \
                     yet lowerable"
                ));
            }
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
        if is_last && pcond.is_some() && scrut_valid.is_some() {
            return Err(anyhow!(
                "emit_clif: possibly-bottom scrutinee with a conditional \
                 final arm — the chain could miss every arm"
            ));
        }
        let matched = cx.b.create_block();
        let fail: Option<Block> = if pcond.is_some() || has_guard {
            Some(cx.b.create_block())
        } else {
            None
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
                    let SelectScrut::Scalar { value, valid, prim } = scrut
                    else {
                        return Err(anyhow!(
                            "emit_clif: scrutinee bind without a scalar \
                             scrutinee"
                        ));
                    };
                    let name: ArcStr = compact_str::format_compact!(
                        "__pat{}",
                        id.inner()
                    )
                    .as_str()
                    .into();
                    let var = cx.b.declare_var(prim_to_clif(prim));
                    cx.b.def_var(var, value);
                    match valid {
                        None => {
                            cx.env.bind_with_id(name, var, prim, Some(*id))
                        }
                        Some(v) => {
                            let valid_var = cx.b.declare_var(types::I8);
                            cx.b.def_var(valid_var, v);
                            cx.env.bind_tainted_with_id(
                                name,
                                var,
                                valid_var,
                                prim,
                                Some(*id),
                            );
                        }
                    }
                }
                SelectArmBind::Payload { id, idx, prim } => {
                    let SelectScrut::Value { disc, payload } = scrut else {
                        return Err(anyhow!(
                            "emit_clif: payload bind without a variant \
                             scrutinee"
                        ));
                    };
                    let helper =
                        cx.helper(variant_payload_helper(*prim)?)?;
                    let idx_c = cx.b.ins().iconst(types::I64, *idx as i64);
                    let call =
                        cx.b.ins().call(helper, &[disc, payload, idx_c]);
                    let v = cx.b.inst_results(call)[0];
                    let var = cx.b.declare_var(prim_to_clif(*prim));
                    cx.b.def_var(var, v);
                    let name: ArcStr = compact_str::format_compact!(
                        "__pat{}",
                        id.inner()
                    )
                    .as_str()
                    .into();
                    cx.env.bind_with_id(name, var, *prim, Some(*id));
                }
            }
        }
        if let Some(g) = &pat.guard {
            // Canonical guard semantics: evaluated only after the
            // pattern matched, with the binds in scope; a bottom guard
            // (`valid` = 0) means the arm does NOT match.
            let (gv, gvalid) =
                g.node.emit_clif(cx)?.scalar_with_validity(cx.b)?;
            let eff = cx.b.ins().band(gv, gvalid);
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
                    // Unreachable by construction (see the refusals
                    // above); CLIF still requires a terminator.
                    cx.b.ins().trap(
                        cranelift_codegen::ir::TrapCode::user(2).unwrap(),
                    );
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
fn emit_select_value_arm<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    body: &crate::node::Cached<R, E>,
    mark: EnvMark,
    merge_shape: SelectMerge,
    merge: Block,
    scrut_valid: Option<ClifValue>,
    any_taint: &mut bool,
) -> Result<()> {
    use crate::NodeView;
    let body_frozen =
        gir::freeze_normalized(body.node.typ()).ok_or_else(|| {
            anyhow!(
                "emit_clif: select arm type {:?} doesn't freeze concrete",
                body.node.typ()
            )
        })?;
    match merge_shape {
        SelectMerge::Scalar(rp) => {
            if gir::scalar_prim(&body_frozen) != Some(rp) {
                return Err(anyhow!(
                    "emit_clif: select arm type {body_frozen:?} doesn't \
                     match the scalar merge {rp:?}"
                ));
            }
            let cv = body.node.emit_clif(cx)?;
            if matches!(cv, CompiledExpr::Scalar2 { .. }) {
                *any_taint = true;
            }
            let (v, valid) = cv.scalar_with_validity(cx.b)?;
            let valid = match scrut_valid {
                Some(sv) => cx.b.ins().band(valid, sv),
                None => valid,
            };
            cx.env.truncate(mark);
            cx.b.ins().jump(
                merge,
                &[BlockArg::Value(v), BlockArg::Value(valid)],
            );
        }
        SelectMerge::Value => {
            // Node twin of `widen_arm_to_value`, keyed on the arm
            // BODY's frozen type.
            let (d, p) = match gir::abi_kind(&body_frozen) {
                Some(AbiKind::Null) => {
                    // A bare-null arm body has nothing to emit (and a
                    // Null-shaped node can't emit anyway); only the
                    // literal constant form is recognized.
                    match body.node.view() {
                        NodeView::Constant(c)
                            if matches!(c.value, Value::Null) => {}
                        _ => {
                            return Err(anyhow!(
                                "emit_clif: null-typed select arm isn't \
                                 a null literal"
                            ));
                        }
                    }
                    let d = cx.b.ins().iconst(types::I64, value_disc::NULL);
                    let p = cx.b.ins().iconst(types::I64, 0);
                    (d, p)
                }
                Some(AbiKind::Scalar(p)) => {
                    let v = body.node.emit_clif(cx)?.single()?;
                    let d =
                        cx.b.ins().iconst(types::I64, prim_to_value_disc(p));
                    (d, scalar_to_payload_i64(cx.b, p, v))
                }
                Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
                    let (d, p) = body.node.emit_clif(cx)?.value()?;
                    ensure_owned_value_src(
                        cx,
                        node_composite_source(&body.node),
                        d,
                        p,
                    )?
                }
                other => {
                    return Err(anyhow!(
                        "emit_clif: select arm of shape {other:?} can't \
                         widen to the Value merge"
                    ));
                }
            };
            cx.env.truncate(mark);
            cx.b.ins().jump(merge, &[BlockArg::Value(d), BlockArg::Value(p)]);
        }
        SelectMerge::Composite => {
            if !matches!(
                gir::abi_kind(&body_frozen),
                Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct)
            ) {
                return Err(anyhow!(
                    "emit_clif: select arm type {body_frozen:?} doesn't \
                     match the composite merge"
                ));
            }
            let v = body.node.emit_clif(cx)?.single()?;
            let v = ensure_owned_composite_src(
                cx,
                node_composite_source(&body.node),
                v,
            )?;
            cx.env.truncate(mark);
            cx.b.ins().jump(merge, &[BlockArg::Value(v)]);
        }
        SelectMerge::String => {
            if !matches!(gir::abi_kind(&body_frozen), Some(AbiKind::String)) {
                return Err(anyhow!(
                    "emit_clif: select arm type {body_frozen:?} doesn't \
                     match the string merge"
                ));
            }
            // String reads/produces are owned at production.
            let v = body.node.emit_clif(cx)?.single()?;
            cx.env.truncate(mark);
            cx.b.ins().jump(merge, &[BlockArg::Value(v)]);
        }
    }
    Ok(())
}

/// Node-graph analog of `gir::int_div_may_bottom` — true unless the
/// divisor is a non-zero constant (and, for signed, the dividend isn't
/// the MIN/-1 overflow pair). Conservative `true` keeps the runtime
/// guard; a provable non-bottom skips it. Sees through `ExplicitParens`.
fn node_int_div_may_bottom<R: crate::Rt, E: crate::UserEvent>(
    lhs: &crate::Node<R, E>,
    rhs: &crate::Node<R, E>,
) -> bool {
    use crate::NodeView;
    fn const_value<'a, R: crate::Rt, E: crate::UserEvent>(
        n: &'a crate::Node<R, E>,
    ) -> Option<&'a Value> {
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
enum LambdaCallSlot<'a, R: crate::Rt, E: crate::UserEvent> {
    Arg(&'a crate::Node<R, E>, Type),
    Cap(&'a crate::fusion::lowering::CaptureSlot),
}

impl<R: crate::Rt, E: crate::UserEvent> LambdaCallSlot<'_, R, E> {
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
/// and the subtree node-walks. A may-bottom (`Scalar2`) scalar arg or
/// capture Errs = de-fuse (same `compile_scalar` contract as the GIR
/// caller). The result is unpacked per the callee's return ABI: one
/// CLIF result for scalar / composite-pointer returns, a two-word
/// `(disc, payload)` pair for variant/nullable — owned, like the GIR
/// arm's classification.
pub(crate) fn emit_lambda_call_node<R: crate::Rt, E: crate::UserEvent>(
    cx: &mut BodyCx,
    cs: &crate::node::callsite::CallSite<R, E>,
    info: &crate::fusion::LambdaCallInfo,
) -> Result<CompiledExpr> {
    let fn_name = &info.fn_name;
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
    let n_formal = info
        .arg_types
        .len()
        .checked_sub(info.captures.len())
        .ok_or_else(|| {
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
            crate::typ::FnArgKind::Positional { .. } => {
                let n = cs.arg_positional(pos);
                pos += 1;
                n
            }
            crate::typ::FnArgKind::Labeled { name, .. } => cs.arg_named(name),
        }
        .ok_or_else(|| {
            anyhow!("lambda call `{fn_name}`: missing call-site arg node")
        })?;
        slots.push(LambdaCallSlot::Arg(node, info.arg_types[i].clone()));
    }
    for cap in &info.captures {
        slots.push(LambdaCallSlot::Cap(cap));
    }
    // Shape gate — same as the GIR caller: scalar / composite /
    // variant / nullable only.
    for s in &slots {
        match gir::abi_kind(s.typ()) {
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
    let emit_arg_single = |cx: &mut BodyCx,
                           n: &crate::Node<R, E>|
     -> Result<ClifValue> {
        match n.emit_clif(cx)? {
            CompiledExpr::Single(v) => Ok(v),
            cv => Err(anyhow!(
                "lambda call `{fn_name}`: arg compiled to a \
                 possibly-bottom or non-single value ({cv:?}) — \
                 subtree node-walks"
            )),
        }
    };
    let cap_scalar = |cx: &mut BodyCx,
                      cap: &crate::fusion::lowering::CaptureSlot|
     -> Result<ClifValue> {
        let (var, valid, _) = cx
            .env
            .lookup_bind_id(cap.bind_id)
            .or_else(|| cx.env.lookup(cap.name.as_str()))
            .ok_or_else(|| {
                anyhow!(
                    "lambda call `{fn_name}`: capture `{}` not in the \
                     calling kernel's env",
                    cap.name
                )
            })?;
        if valid.is_some() {
            return Err(anyhow!(
                "lambda call `{fn_name}`: capture `{}` is possibly-bottom \
                 — subtree node-walks",
                cap.name
            ));
        }
        Ok(cx.b.use_var(var))
    };
    let cap_composite = |cx: &mut BodyCx,
                         cap: &crate::fusion::lowering::CaptureSlot|
     -> Result<ClifValue> {
        let var = cx
            .env
            .lookup_composite_bind_id(cap.bind_id)
            .or_else(|| cx.env.lookup_composite(cap.name.as_str()))
            .ok_or_else(|| {
                anyhow!(
                    "lambda call `{fn_name}`: composite capture `{}` not \
                     in the calling kernel's env",
                    cap.name
                )
            })?;
        Ok(cx.b.use_var(var))
    };
    let is_kind = |s: &&LambdaCallSlot<R, E>, k: AbiKind| {
        gir::abi_kind(s.typ()) == Some(k)
    };
    let mut clif_args: Vec<ClifValue> = Vec::with_capacity(slots.len());
    let mut drops: Vec<CallArgDrop> = Vec::new();
    // Scalars first.
    for s in slots
        .iter()
        .filter(|s| matches!(gir::abi_kind(s.typ()), Some(AbiKind::Scalar(_))))
    {
        let v = match s {
            LambdaCallSlot::Arg(n, _) => emit_arg_single(cx, n)?,
            LambdaCallSlot::Cap(c) => cap_scalar(cx, c)?,
        };
        clif_args.push(v);
    }
    // Composite pointers: array, then tuple, then struct.
    let composite = slots
        .iter()
        .filter(|s| is_kind(s, AbiKind::Array))
        .chain(slots.iter().filter(|s| is_kind(s, AbiKind::Tuple)))
        .chain(slots.iter().filter(|s| is_kind(s, AbiKind::Struct)));
    for s in composite {
        let ptr = match s {
            LambdaCallSlot::Arg(n, _) => {
                let p = emit_arg_single(cx, n)?;
                if node_composite_source(n) == CompositeSource::Owned {
                    drops.push(CallArgDrop::Composite(p));
                }
                p
            }
            LambdaCallSlot::Cap(c) => cap_composite(cx, c)?,
        };
        clif_args.push(ptr);
    }
    // Value-shape: variant, then nullable — two words each.
    let value = slots
        .iter()
        .filter(|s| is_kind(s, AbiKind::Variant))
        .chain(slots.iter().filter(|s| is_kind(s, AbiKind::Nullable)));
    for s in value {
        let (disc, payload) = match s {
            LambdaCallSlot::Arg(n, _) => {
                let (d, p) = n.emit_clif(cx)?.value()?;
                if node_composite_source(n) == CompositeSource::Owned {
                    drops.push(CallArgDrop::Value { disc: d, payload: p });
                }
                (d, p)
            }
            LambdaCallSlot::Cap(c) => {
                return Err(anyhow!(
                    "lambda call `{fn_name}`: value-shape capture `{}` — \
                     not yet lowered (name-keyed env table), subtree \
                     node-walks",
                    c.name
                ));
            }
        };
        clif_args.push(disc);
        clif_args.push(payload);
    }
    let func_ref = cx.ctx.callee_refs.get(fn_name).ok_or_else(|| {
        anyhow!(
            "lambda call `{fn_name}`: callee_refs has no entry — \
             discovery/declare drift"
        )
    })?;
    let inst = cx.b.ins().call(*func_ref, &clif_args);
    let ret = &info.kernel.return_type;
    let result = match gir::abi_kind(ret) {
        Some(
            AbiKind::Scalar(_) | AbiKind::Array | AbiKind::Tuple
            | AbiKind::Struct,
        ) => {
            let results = cx.b.inst_results(inst);
            if results.len() != 1 {
                return Err(anyhow!(
                    "lambda call `{fn_name}`: callee returned {} values, \
                     expected 1",
                    results.len()
                ));
            }
            CompiledExpr::Single(results[0])
        }
        Some(AbiKind::Variant | AbiKind::Nullable) => {
            let results = cx.b.inst_results(inst);
            if results.len() != 2 {
                return Err(anyhow!(
                    "lambda call `{fn_name}`: value-shape callee returned \
                     {} values, expected 2",
                    results.len()
                ));
            }
            CompiledExpr::Value { disc: results[0], payload: results[1] }
        }
        other => {
            return Err(anyhow!(
                "lambda call `{fn_name}`: return shape {other:?} not \
                 lowered — subtree node-walks"
            ));
        }
    };
    emit_call_arg_drops(cx.b, cx.ctx, &drops)?;
    Ok(result)
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
    for (_, var, _) in &env.composites {
        let ptr = b.use_var(*var);
        b.ins().call(arr_drop, &[ptr]);
    }
    // Variant params + locals are owned (params are refcount-cloned
    // on kernel entry via `graphix_value_clone`, locals come from
    // `VariantNew` or composite-return DynCall). Drop them via the
    // two-register `graphix_value_drop(disc, payload)` ABI — passes
    // the word pair and the helper consumes (drops) it.
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

/// The site-level pending branch shared by every non-scalar DynCall
/// result (composite / Value-shape / String): peek `DYNCALL_PENDING`;
/// if the dispatch pended, the result word(s) hold the null sentinel —
/// drop everything the kernel owns (`emit_pending_cleanup`) and jump
/// to `pending_exit`, so the sentinel never flows into downstream
/// code whose derefs and drops assume validity. Falls through on the
/// non-pending path. (`pending_take` READS without clearing, so the
/// wrapper-level check in `GirNode::update` still fires.)
///
/// Register-scalar results don't branch — their 0 sentinel is inert
/// for downstream arithmetic and the wrapper discards the kernel
/// result wholesale.
fn emit_dyncall_pending_branch(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
    let pending_take = ctx
        .helper_refs
        .get("graphix_dyncall_pending_take")
        .ok_or_else(|| anyhow!("missing graphix_dyncall_pending_take"))?;
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
    Ok(())
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
    /// string constant / concat / local read produces an owned
    /// ArcStr whose refcount would never decrement if
    /// `GirNode::update` discards the wrapper result.
    String(ClifValue),
}

/// At every composite / Value-shape kernel return, emit a
/// `DYNCALL_PENDING` peek. If pending fired earlier (e.g., a scalar
/// DynCall deep in the body that doesn't short-circuit on
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
    elem: &Type,
    struct_access: bool,
) -> Result<&'static str> {
    Ok(match gir::abi_kind(elem) {
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
                "element read of Unit/Null/non-fusable slot — GIR is malformed"
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
    elem: &Type,
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
    if gir::is_value_shape(&elem) {
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

/// The `graphix_string_buf_push_*` helper that Display-renders a
/// scalar of `p` into a Concat / string-interpolate buffer. Shared by
/// the GIR `Concat` arm ([`compile_concat`]) and the direct path
/// ([`emit_string_interpolate_node`]).
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
/// accepted for their fixed-width prim). Panics otherwise — a malformed
/// kernel.
fn compile_const(b: &mut FunctionBuilder, v: &Value, prim: PrimType) -> ClifValue {
    macro_rules! bad {
        () => {
            panic!("compile_const: {v:?} isn't a {prim:?} scalar")
        };
    }
    match prim {
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

