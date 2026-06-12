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
    self, AbiKind, AbiParamKind, AbiReturn, BinOp, BoolOp, CmpOp, GirExpr,
    GirKernel, GirOp, GirStmt, PrimType, SelectArm,
};
use crate::typ::Type;
// `compile_node` (Stage 1 of `design/delete_gir_ir.md`) calls `Update`
// trait methods (`typ`, `view`) on region-root Nodes.
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
        // GIR-body path (`body_emitter: None`) — the lazy arenas stay
        // empty (only direct-path emission interns lazily).
        let lazy_strings = std::cell::RefCell::new(Vec::new());
        let lazy_values = std::cell::RefCell::new(Vec::new());
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
            &lazy_strings,
            &lazy_values,
            None,
        )?;
        builder.finalize();
    }
    maybe_dump_clif(&jit.func_ctx.func, &kernel.fn_name);

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
    compile_kernel_with_callees_impl(jit, kernel, callees, &BTreeMap::new())
}

/// `CFlag::DirectNodeJit` entry: compile `kernel` but emit BODIES by
/// walking Nodes via `emit_clif` instead of lowering `GirStmt` lists.
/// The kernel ABI (params / return / wrapper) still comes from each
/// `GirKernel`; only body codegen changes. The parent emits from
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
    kernel: &std::sync::Arc<GirKernel>,
    callees: &BTreeMap<ArcStr, std::sync::Arc<GirKernel>>,
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
    kernel: &std::sync::Arc<GirKernel>,
    callees: &BTreeMap<ArcStr, std::sync::Arc<GirKernel>>,
    emitters: &BTreeMap<usize, &dyn BodyEmitter>,
) -> Result<WrappedKernel> {
    let mut to_define: Vec<std::sync::Arc<GirKernel>> = Vec::new();
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
    kernel: &std::sync::Arc<GirKernel>,
    callees: &BTreeMap<ArcStr, std::sync::Arc<GirKernel>>,
    emitters: &BTreeMap<usize, &dyn BodyEmitter>,
    to_define: &mut Vec<std::sync::Arc<GirKernel>>,
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
    // `emitters` keys Node body emitters by kernel identity (the
    // parent + every non-recursive direct-path callee); a kernel
    // without an entry compiles from its GIR body. The trace makes
    // which emitter ran VISIBLE — the silent-fallback class (a body
    // quietly compiling from the path you didn't intend) has cost
    // an investigation every time it appeared.
    for k in to_define.iter() {
        let key = std::sync::Arc::as_ptr(k) as usize;
        let body: Option<&dyn BodyEmitter> = emitters.get(&key).copied();
        log::trace!(
            "define kernel `{}`: {} body",
            k.fn_name,
            if body.is_some() { "Node-emitted" } else { "GIR" }
        );
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
    body_emitter: Option<&dyn BodyEmitter>,
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
        //
        // A Node-emitted kernel's GirKernel body is EMPTY (its calls
        // are CallSite nodes, not GirOp::Call), so collect_call_sites
        // finds nothing — declare exactly the fn_names its discovered
        // lambda call sites reference instead (for the parent that is
        // the region's direct callee set; for a Node-emitted CALLEE
        // the map is empty, matching the no-unused-FuncRefs shape its
        // GIR-emitted body had). The kernel's own name is excluded —
        // a region can't self-call (recursive callees keep their GIR
        // body until F0b, where the self FuncRef threads explicitly).
        let needed: std::collections::BTreeSet<ArcStr> =
            if let Some(em) = body_emitter {
                let mut s: std::collections::BTreeSet<ArcStr> = em
                    .lambda_call_sites()
                    .map(|m| {
                        m.values()
                            .map(|info| info.fn_name.clone())
                            .filter(|n| n.as_str() != kernel.fn_name.as_str())
                            .collect()
                    })
                    .unwrap_or_default();
                // A self-recursive body calls ITSELF — import the
                // kernel's own FuncRef (funcids carries every callee
                // by name, this kernel included).
                if let Some((_, info)) = em.self_call() {
                    s.insert(info.fn_name.clone());
                }
                s
            } else {
                crate::gir::collect_call_sites(kernel)
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
        let strings = KernelStrings::build(kernel);
        let values = KernelValues::build(kernel);
        // Direct-path lazy interning arenas — filled during emission
        // via `BodyCx::interned_str` / `interned_value`, merged into
        // the returned tables below so the baked addresses live as
        // long as the compiled code. Empty on the GIR path.
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
            &strings,
            &values,
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
            strings.with_lazy(lazy_strings.into_inner()),
            values.with_lazy(lazy_values.into_inner()),
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
    kernel: &GirKernel,
    callee_refs: &BTreeMap<ArcStr, FuncRef>,
    helper_refs: &HelperRefs,
    strings: &KernelStrings,
    values: &KernelValues,
    lazy_strings: &std::cell::RefCell<Vec<Box<ArcStr>>>,
    lazy_values: &std::cell::RefCell<Vec<Box<Value>>>,
    body_emitter: Option<&dyn BodyEmitter>,
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
        strings,
        values,
        dyncall_buf_stack: std::cell::RefCell::new(Vec::new()),
        owned_input_stack: std::cell::RefCell::new(Vec::new()),
        pending_exit: std::cell::RefCell::new(None),
        lazy_strings,
        lazy_values,
        builtin_apply_sites: body_emitter
            .and_then(|em| em.builtin_apply_sites()),
        lambda_call_sites: body_emitter
            .and_then(|em| em.lambda_call_sites()),
        self_call: body_emitter.and_then(|em| em.self_call()),
        type_env: body_emitter.and_then(|em| em.type_env()),
    };
    // Body codegen: the `DirectNodeJit` path supplies a `NodeBodyEmitter`
    // that walks the region-root Node via `compile_node`; otherwise the
    // production GIR path lowers `kernel.body` (the `GirStmt` list).
    match body_emitter {
        Some(em) => em.emit(b, &mut env, &lower)?,
        None => compile_body(b, &kernel.body, &mut env, &lower)?,
    }

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
        Self { slots: slots.into_boxed_slice(), index, lazy: Vec::new() }
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
/// [`GirOp::Const`]s (datetime/duration/bytes/map). Mirrors
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

    /// Pre-walk `kernel`, collect each distinct value-shape `Const`
    /// literal.
    pub fn build(kernel: &GirKernel) -> Self {
        let mut slots: Vec<Value> = Vec::new();
        for stmt in &kernel.body {
            collect_values_stmt(stmt, &mut slots);
        }
        Self { slots: slots.into_boxed_slice(), lazy: Vec::new() }
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

fn collect_values_stmt(s: &GirStmt, out: &mut Vec<Value>) {
    match s {
        GirStmt::Let(l) => collect_values_expr(&l.value, out),
        GirStmt::Return(e) | GirStmt::Discard(e) => collect_values_expr(e, out),
        GirStmt::TailCall { args } => {
            args.iter().for_each(|a| collect_values_expr(a, out))
        }
        GirStmt::Select { scrut, arms } => {
            if let Some(s) = scrut {
                collect_values_expr(s, out);
            }
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
    // Only VALUE-SHAPE `Const`s (datetime/duration/bytes/map) go in the
    // table — they bake a `*const Value`. Scalar `Const`s lower inline
    // via `compile_const` and aren't interned.
    if let GirOp::Const(v) = &e.op {
        if gir::is_value_shape(&e.typ) && !out.iter().any(|s| s == v) {
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
        GirOp::IfChain { scrut, arms } => {
            if let Some(s) = scrut {
                collect_values_expr(s, out);
            }
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
        GirStmt::Select { scrut, arms } => {
            if let Some(s) = scrut {
                collect_strings_expr(s, out);
            }
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
        GirOp::Const(_) | GirOp::ConstNull
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
        GirOp::IfChain { scrut, arms } => {
            if let Some(s) = scrut {
                collect_strings_expr(s, out);
            }
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
pub(crate) struct LowerCtx<'a> {
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
    /// Per-kernel value-shape constants table. Value-shape `GirOp::Const`
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

/// Result of compiling a [`GirExpr`]: either a single CLIF value
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

    /// Bind a non-tainted scalar local (the common path). Name-keyed
    /// only — the GIR path's lets and synthetic locals have no
    /// BindId.
    fn bind(&mut self, name: ArcStr, var: Variable, prim: PrimType) {
        self.locals.push((name, var, None, prim, None));
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
    /// validity Variable. A later `GirOp::Local` read returns
    /// `CompiledExpr::Scalar2 { value, valid }`.
    fn bind_tainted(
        &mut self,
        name: ArcStr,
        var: Variable,
        valid: Variable,
        prim: PrimType,
    ) {
        self.locals.push((name, var, Some(valid), prim, None));
    }

    /// [`Self::bind_tainted`] carrying its source BindId.
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

/// Bind a scalar `let local = value` into `env.locals`, preserving
/// per-value validity. A non-tainted value binds via `env.bind`
/// (read back as `Single`); a possibly-bottom (`Scalar2`) value binds
/// via `env.bind_tainted` so a later `GirOp::Local` read surfaces its
/// validity bit and the bottom flows. Shared by `GirStmt::Let`,
/// `GirOp::Block`'s let arm, and `compile_block_value`'s let arm.
fn bind_scalar_let(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
    name: &ArcStr,
    value: &GirExpr,
    p: PrimType,
) -> Result<()> {
    let cv = compile_expr(b, value, env, ctx)?;
    match cv {
        // Non-tainted: bind the value alone (read back as `Single`).
        CompiledExpr::Single(v) => {
            let var = b.declare_var(prim_to_clif(p));
            b.def_var(var, v);
            env.bind(name.clone(), var, p);
        }
        // Tainted: store the value AND its validity bit so a later
        // `GirOp::Local` read of this local surfaces the bottom.
        CompiledExpr::Scalar2 { value, valid } => {
            let var = b.declare_var(prim_to_clif(p));
            b.def_var(var, value);
            let valid_var = b.declare_var(types::I8);
            b.def_var(valid_var, valid);
            env.bind_tainted(name.clone(), var, valid_var, p);
        }
        CompiledExpr::Value { .. } => {
            return Err(anyhow!(
                "bind_scalar_let: Value-shape result for a scalar let — GIR \
                 is malformed"
            ));
        }
    }
    Ok(())
}

fn compile_body(
    b: &mut FunctionBuilder,
    stmts: &[GirStmt],
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
    for stmt in stmts {
        match stmt {
            GirStmt::Let(l) => {
                match gir::abi_kind(&l.value.typ) {
                    Some(AbiKind::Scalar(p)) => {
                        bind_scalar_let(b, env, ctx, &l.local, &l.value, p)?;
                    }
                    Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
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
                    Some(
                        AbiKind::Variant | AbiKind::Nullable | AbiKind::Value,
                    ) => {
                        // Value-shape local: stored as a `(disc,
                        // payload)` pair (`ValueVar`). The source
                        // expression compiles to a `CompiledExpr::Value`;
                        // `ensure_owned_value` clones a Borrowed
                        // source (e.g. `let a = b` aliasing another
                        // Value local) via `graphix_value_clone` so
                        // this local exclusively owns its ref. Dropped
                        // at function exit (and on pending paths by
                        // the per-DynCall `pre_pending` block).
                        // datetime/duration/bytes/map/error share the
                        // `nullables` ValueVar slot (same `(disc,
                        // payload)` wire shape).
                        let cv = compile_expr(b, &l.value, env, ctx)?;
                        let (owned_disc, owned_payload) =
                            ensure_owned_value(b, ctx, &l.value, cv)?;
                        let disc_var = b.declare_var(types::I64);
                        let payload_var = b.declare_var(types::I64);
                        b.def_var(disc_var, owned_disc);
                        b.def_var(payload_var, owned_payload);
                        let vv =
                            ValueVar { disc: disc_var, payload: payload_var };
                        if matches!(
                            gir::abi_kind(&l.value.typ),
                            Some(AbiKind::Variant)
                        ) {
                            env.bind_variant(l.local.clone(), vv);
                        } else {
                            env.bind_nullable(l.local.clone(), vv);
                        }
                    }
                    // Unreachable in well-formed GIR — `emit_bind_stmt`
                    // routes Unit-typed lets through `GirStmt::Discard`.
                    Some(AbiKind::Unit) => {
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
                    Some(AbiKind::String) => {
                        let v = compile_scalar(b, &l.value, env, ctx)?;
                        let var = b.declare_var(types::I64);
                        b.def_var(var, v);
                        env.bind_string(l.local.clone(), var);
                    }
                    // Bare `Null` is the singleton produced by
                    // `GirOp::ConstNull` — fusion always widens it to
                    // `Nullable<T>` before binding (`emit_select_as_expr`
                    // /  `unify_arm_types`), so a bare-`Null` let (or a
                    // non-fusable type) is a malformed kernel.
                    Some(AbiKind::Null) | None => {
                        return Err(anyhow!(
                            "GirStmt::Let with bare Null / non-fusable value \
                             — should have widened to Nullable<T> at \
                             construction"
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
                match gir::abi_kind(&e.typ) {
                    Some(
                        AbiKind::Variant | AbiKind::Nullable | AbiKind::Value,
                    ) => {
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
                    Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                        let v = compile_scalar(b, e, env, ctx)?;
                        let v = ensure_owned_composite(b, ctx, e, v)?;
                        emit_return_pending_check(
                            b, env, ctx,
                            ReturnDropShape::Composite(v),
                        )?;
                        drop_owned_composites(b, env, ctx)?;
                        b.ins().return_(&[v]);
                    }
                    Some(AbiKind::String) => {
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
                        // Scalar / Unit: no owned allocation to leak.
                        // BUT a Scalar2 (possibly-bottom) return is the
                        // OUTPUT-consumes-bottom case: when its validity
                        // bit is 0, set the pending flag and route to
                        // `pending_exit` so `GirNode::update` returns
                        // `None` (the abort moves from the producing site
                        // to the output — an intermediate bottom an
                        // un-taken arm never consumes no longer aborts).
                        // A `Single` return can't be bottom — skip the
                        // check.
                        let cv = compile_expr(b, e, env, ctx)?;
                        match cv {
                            CompiledExpr::Scalar2 { value, valid } => {
                                let pending_set = ctx
                                    .helper_refs
                                    .get("graphix_dyncall_set_pending")
                                    .ok_or_else(|| {
                                        anyhow!(
                                            "missing graphix_dyncall_set_pending"
                                        )
                                    })?;
                                let pre_pending = b.create_block();
                                let ret_block = b.create_block();
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
                                // valid == 0 → bottom output → pending.
                                b.ins().brif(
                                    valid, ret_block, &[], pre_pending, &[],
                                );
                                b.switch_to_block(pre_pending);
                                b.seal_block(pre_pending);
                                b.ins().call(pending_set, &[]);
                                emit_pending_cleanup(b, env, ctx)?;
                                b.ins().jump(pending_exit, &[]);
                                b.switch_to_block(ret_block);
                                b.seal_block(ret_block);
                                drop_owned_composites(b, env, ctx)?;
                                b.ins().return_(&[value]);
                            }
                            CompiledExpr::Single(v) => {
                                drop_owned_composites(b, env, ctx)?;
                                b.ins().return_(&[v]);
                            }
                            CompiledExpr::Value { .. } => {
                                return Err(anyhow!(
                                    "GirStmt::Return scalar arm got a \
                                     Value-shape result — GIR malformed"
                                ));
                            }
                        }
                    }
                }
                return Ok(());
            }
            GirStmt::TailCall { args } => {
                // Evaluate every new arg into a CLIF SSA value first
                // (so an arg that reads an old param sees the old
                // value, not one we already overwrote).
                let mut new_vals = Vec::with_capacity(args.len());
                for a in args {
                    new_vals.push(compile_scalar(b, a, env, ctx)?);
                }
                // Detect each composite-rebind source so we know
                // whether the SSA value already holds a fresh owned
                // pointer (TupleNew / StructNew / etc.) or whether
                // it's a Local read sharing the same pointer as the
                // slot we're about to drop. The latter needs a
                // refcount bump before rebind so the drop balances.
                let sources: Vec<CompositeSource> = args
                    .iter()
                    .map(|a| classify_composite_source(a))
                    .collect();
                emit_tail_rebind_jump(b, env, ctx, new_vals, &sources)?;
                return Ok(());
            }
            GirStmt::Select { scrut, arms } => {
                // A bottom SCRUTINEE poisons the whole select (the
                // node-walk's Select doesn't fire without a scrutinee
                // value). The scrutinee is folded into the arm conds; we
                // re-compile it here only to extract its validity bit and
                // route to `pending_exit` when invalid (kernel returns
                // None). A bottom GUARD, by contrast, only fails its own
                // arm — handled inside `compile_select_stmt`.
                compile_select_scrut_gate(b, scrut.as_ref(), env, ctx)?;
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

/// The rebind-and-jump core of a self tail-call, shared by the GIR
/// `GirStmt::TailCall` arm and the Node path's tail emission (F0b).
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
                // Nullable kernels route to the interpreter via
                // `kernel_contains_null`, so this branch should be
                // unreachable in practice. Returning an Err keeps the
                // codegen safe if the routing ever drifts.
                return Err(anyhow!(
                    "JIT: nullable tail-call rebind — kernel should \
                     have been routed to interp via kernel_contains_null"
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
    // runtime by their scope-exit code (`GirOp::Block`, terminating
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

/// Emit the statement-form `select`'s SCRUTINEE bottom-check gate. A
/// bottom scrutinee poisons the whole select (interp parity, #178): if
/// the scrutinee's validity bit is 0, set the pending flag and route to
/// `pending_exit` (kernel returns None). A non-tainted (`Single`) or
/// absent scrutinee is a no-op. The scrutinee is folded into the arm
/// conds, so we only need its validity bit here.
fn compile_select_scrut_gate(
    b: &mut FunctionBuilder,
    scrut: Option<&GirExpr>,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<()> {
    let Some(s) = scrut else { return Ok(()) };
    // Only scalar scrutinees can be a value-bottom (`Scalar2`). A
    // value-shape scrutinee carries bottom in-band via the existing
    // pending machinery at its producer.
    if !matches!(gir::abi_kind(&s.typ), Some(AbiKind::Scalar(_))) {
        return Ok(());
    }
    let cv = compile_expr(b, s, env, ctx)?;
    let CompiledExpr::Scalar2 { valid, .. } = cv else {
        return Ok(());
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
    // valid == 0 → bottom scrutinee → pending.
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
                // A bottom GUARD → the arm doesn't match (fall through).
                // `effective_cond = cond_value AND cond_valid`.
                let (cval, cvalid) =
                    compile_expr(b, cond, env, ctx)?.scalar_with_validity(b)?;
                let cv = b.ins().band(cval, cvalid);
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
    if gir::is_value_shape(&e.typ) || matches!(e.op, GirOp::ConstNull) {
        return compile_value_expr(b, e, env, ctx);
    }
    // Taint-aware scalar ops: these can PRODUCE a value-bottom
    // (`Bin` div/mod, `QopUnwrap`) or PROPAGATE one from their
    // operands (`Bin` non-div, `Cmp`, `BoolBin`, `Not`, `Cast`,
    // scalar `IfChain`). They route through `compile_tainted_scalar`
    // which returns `CompiledExpr::Scalar2` when a result may be
    // bottom; everything else stays `Single` (the taint optimization).
    match &e.op {
        GirOp::Bin { .. }
        | GirOp::Cmp { .. }
        | GirOp::BoolBin { .. }
        | GirOp::Not(_)
        | GirOp::Cast { .. }
        | GirOp::QopUnwrap { .. }
        | GirOp::IfChain { .. }
        | GirOp::Block { .. } => {
            return compile_tainted_scalar(b, e, env, ctx);
        }
        // A scalar `Local` read of a TAINTED local must surface its
        // validity bit (`let v = 1/0; … v …`). Non-tainted scalar
        // locals (and composite/string locals) fall through to
        // `compile_scalar_impl` → `Single`.
        GirOp::Local(name) if matches!(gir::abi_kind(&e.typ), Some(AbiKind::Scalar(_))) => {
            let (var, valid, _) = env.lookup(name).ok_or_else(|| {
                anyhow!("GIR malformed: undefined scalar local `{name}`")
            })?;
            let value = b.use_var(var);
            return Ok(match valid {
                None => CompiledExpr::Single(value),
                Some(vv) => {
                    CompiledExpr::Scalar2 { value, valid: b.use_var(vv) }
                }
            });
        }
        _ => {}
    }
    let v = compile_scalar_impl(b, e, env, ctx)?;
    Ok(CompiledExpr::Single(v))
}

/// Compile the scalar ops that can produce or propagate a value-bottom
/// (per-value validity). See [`CompiledExpr::Scalar2`]. Dispatched from
/// [`compile_expr`] before the plain `compile_scalar_impl` path; the
/// same ops in `compile_scalar_impl` are now unreachable (they error if
/// routing drifts).
fn compile_tainted_scalar(
    b: &mut FunctionBuilder,
    e: &GirExpr,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<CompiledExpr> {
    match &e.op {
        GirOp::Bin { op, lhs, rhs } => {
            let lcv = compile_expr(b, lhs, env, ctx)?;
            let rcv = compile_expr(b, rhs, env, ctx)?;
            let (l, _) = lcv.scalar_with_validity(b)?;
            let (r, _) = rcv.scalar_with_validity(b)?;
            let prim = prim_of(&lhs.typ);
            // Integer div/rem: a zero divisor (or signed MIN/-1
            // overflow) makes raw cranelift sdiv/udiv/srem/urem TRAP
            // (#DE → SIGFPE). Instead of aborting the kernel, compute
            // a per-value validity bit and a BRANCHLESS safe result:
            // `safe_r = select(bad, 1, r)` so the hardware op never
            // traps. The result is `Scalar2 { value, valid: !bad &
            // operand_valids }` — a bottom that the OUTPUT never
            // consumes (un-taken arm, dead let) no longer aborts the
            // kernel; the abort moves to the boundary (`GirStmt::Return`).
            if matches!(op, BinOp::Div | BinOp::Mod)
                && prim.is_integer()
                && gir::int_div_may_bottom(lhs, rhs)
            {
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
                // `bad` is an I8 bool; safe divisor = 1 when bad so the
                // op can't trap. `select` needs the same CLIF type for
                // both arms — produce `1` in the operand's prim width.
                let one = b.ins().iconst(prim_to_clif(prim), 1);
                let safe_r = b.ins().select(bad, one, r);
                let value = compile_bin(b, *op, prim, l, safe_r)?;
                // result valid = !bad AND operand validities.
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
            let value = compile_bin(b, *op, prim, l, r)?;
            let valid = combine_validity(b, &[lcv, rcv]);
            Ok(scalar_result(value, valid))
        }
        GirOp::Cmp { op, lhs, rhs } => {
            let lcv = compile_expr(b, lhs, env, ctx)?;
            let rcv = compile_expr(b, rhs, env, ctx)?;
            let (l, _) = lcv.scalar_with_validity(b)?;
            let (r, _) = rcv.scalar_with_validity(b)?;
            let value = compile_cmp(b, *op, prim_of(&lhs.typ), l, r);
            let valid = combine_validity(b, &[lcv, rcv]);
            Ok(scalar_result(value, valid))
        }
        GirOp::BoolBin { op, lhs, rhs } => {
            // STRICT — both operands are compiled, so a bottom operand
            // taints the result (uniform with every other binary op;
            // matches the node-walk's `bool_op!` and the interp).
            let lcv = compile_expr(b, lhs, env, ctx)?;
            let rcv = compile_expr(b, rhs, env, ctx)?;
            let (l, _) = lcv.scalar_with_validity(b)?;
            let (r, _) = rcv.scalar_with_validity(b)?;
            let value = match op {
                BoolOp::And => b.ins().band(l, r),
                BoolOp::Or => b.ins().bor(l, r),
            };
            let valid = combine_validity(b, &[lcv, rcv]);
            Ok(scalar_result(value, valid))
        }
        GirOp::Not(inner) => {
            let cv = compile_expr(b, inner, env, ctx)?;
            let (v, _) = cv.scalar_with_validity(b)?;
            let one = b.ins().iconst(types::I8, 1);
            let value = b.ins().bxor(v, one);
            let valid = combine_validity(b, &[cv]);
            Ok(scalar_result(value, valid))
        }
        GirOp::Cast { inner, target } => {
            let cv = compile_expr(b, inner, env, ctx)?;
            let (v, _) = cv.scalar_with_validity(b)?;
            let value = compile_cast(b, v, prim_of(&inner.typ), *target);
            let valid = combine_validity(b, &[cv]);
            Ok(scalar_result(value, valid))
        }
        GirOp::QopUnwrap { inner, success_typ } => {
            compile_scalar_qop_unwrap(b, inner, success_typ, env, ctx)
        }
        GirOp::IfChain { scrut, arms } => {
            // Scalar / composite IfChain — Value-shape IfChains go
            // through `compile_value_expr` via `compile_expr`'s
            // dispatch, so reaching here means the result is
            // single-shape (scalar or composite pointer).
            compile_ifchain(b, scrut.as_deref(), arms, &e.typ, env, ctx)
        }
        GirOp::Block { lets, tail } => compile_block_scalar(b, lets, tail, env, ctx),
        other => Err(anyhow!(
            "compile_tainted_scalar reached for non-taint op {other:?}"
        )),
    }
}

// ─── Direct Node → CLIF (Stage 1 of `design/delete_gir_ir.md`) ──────
//
// `compile_node` is the seed of the merged `node → CLIF` pass that will
// eventually replace `emit_node` (node → GIR) + `compile_expr`
// (GIR → CLIF). It walks the region-root [`crate::Node`] graph via
// `NodeView` and emits CLIF directly, reusing every existing helper
// (`compile_bin`/`compile_cmp`/`compile_cast`/`JitEnv`/`CompiledExpr`)
// so there is exactly one home for the CLIF logic. Gated by
// [`crate::CFlag::DirectNodeJit`]; the production path is still the GIR
// body lowering.
//
// **Scalar-only slice today.** Const/Ref/Add..Mod/Cmp/And/Or/Not/Cast/
// ExplicitParens, plus a scalar-let `Block`. Any other `NodeView` (or a
// non-scalar shape) returns `Err` → the kernel build fails → `fuse()`
// leaves the region un-spliced → it node-walks (the universal
// fallback). Bottom/taint propagation is identical to the GIR path:
// scalar div/mod-by-zero produces a [`CompiledExpr::Scalar2`] and the
// abort is resolved at the kernel boundary (`NodeBodyEmitter::emit`).

/// A type-erased producer of a kernel function BODY, given the
/// already-set-up entry block, bound params, and lowering context. The
/// sole implementor is [`NodeBodyEmitter`] (the `DirectNodeJit` path —
/// walks the region-root `Node` via [`compile_node`]); the production
/// GIR path is `compile_body` over the `GirKernel`'s `GirStmt` list,
/// reached when no emitter is supplied. Erasing the `R`/`E` of the Node
/// lets the (monomorphic) JIT-compile pipeline thread a generic
/// `&Node<R, E>` through without itself becoming generic.
trait BodyEmitter {
    fn emit(
        &self,
        b: &mut FunctionBuilder,
        env: &mut JitEnv,
        ctx: &LowerCtx,
    ) -> Result<()>;

    /// Discovered sync-builtin Apply sites for the region being
    /// emitted (`CallSite::emit_clif` lowers a registered site to a
    /// DynCall via [`BodyCx::builtin_site`]). `None` on the GIR path —
    /// its sites are already lowered into the `GirStmt` body.
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
    /// a CLIF `call` via [`BodyCx::lambda_site`]). `None` on the GIR
    /// path — its calls are already `GirOp::Call` in the body.
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

/// The `DirectNodeJit` body emitter — walks the region-root `Node` via
/// [`compile_node`] and emits the kernel return. `return_type` comes
/// from the (still-used) `GirKernel` so the boundary marshalling agrees
/// with the kernel signature / wrapper / runtime arg-pack.
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

/// Ownership classification of a Node-rooted result — the Node twin of
/// `classify_composite_source` (which reads GirExpr structure). A
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
/// owned one through. By-source variant of `ensure_owned_composite`
/// for the direct path (which classifies from the Node, not GirExpr).
/// `pub` for package crates' `Apply::emit_clif` impls (e.g.
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

/// Emit the kernel return for the direct path — the Node-classified
/// mirror of `compile_body`'s `GirStmt::Return` arm. Borrowed results
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

/// Constant literal, dispatched on its runtime shape. Mirrors the
/// GIR `GirOp::Const` / `GirOp::ConstStr` arms:
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
/// rebind the leading tail-call slots, and jump to the loop head —
/// `GirStmt::TailCall`'s Node twin, sharing `emit_tail_rebind_jump`.
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

/// Compile a scalar / composite-pointer `GirOp::Block` (lets + tail),
/// propagating per-value validity: if the tail is a value-bottom
/// (`Scalar2`) the Block result is too. Value-shape Blocks route to
/// `compile_block_value` instead (via `compile_expr`'s shape dispatch).
fn compile_block_scalar(
    b: &mut FunctionBuilder,
    lets: &[crate::gir::Let],
    tail: &GirExpr,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<CompiledExpr> {
    let mark = env.mark();
    for l in lets {
        match gir::abi_kind(&l.value.typ) {
            Some(AbiKind::Scalar(p)) => {
                bind_scalar_let(b, env, ctx, &l.local, &l.value, p)?;
            }
            Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                // Owned `*mut ValArray`. `ensure_owned_composite`
                // clones a Borrowed source so this block exclusively
                // owns the local — otherwise the block-exit drop below
                // would free a buffer the enclosing scope still holds.
                let v = compile_scalar(b, &l.value, env, ctx)?;
                let owned = ensure_owned_composite(b, ctx, &l.value, v)?;
                let var = b.declare_var(types::I64);
                b.def_var(var, owned);
                env.bind_composite(l.local.clone(), var);
            }
            Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
                // Value-shape block local — same `ValueVar` pair +
                // `ensure_owned_value` discipline as GirStmt::Let.
                let cv = compile_expr(b, &l.value, env, ctx)?;
                let (owned_disc, owned_payload) =
                    ensure_owned_value(b, ctx, &l.value, cv)?;
                let disc_var = b.declare_var(types::I64);
                let payload_var = b.declare_var(types::I64);
                b.def_var(disc_var, owned_disc);
                b.def_var(payload_var, owned_payload);
                let vv = ValueVar { disc: disc_var, payload: payload_var };
                if matches!(gir::abi_kind(&l.value.typ), Some(AbiKind::Variant)) {
                    env.bind_variant(l.local.clone(), vv);
                } else {
                    env.bind_nullable(l.local.clone(), vv);
                }
            }
            Some(AbiKind::Unit) => {
                return Err(anyhow!(
                    "GIR malformed: GirOp::Block let with Unit value"
                ));
            }
            Some(AbiKind::String) => {
                // Block-scoped String local — owned ArcStr ptr.
                let v = compile_scalar(b, &l.value, env, ctx)?;
                let var = b.declare_var(types::I64);
                b.def_var(var, v);
                env.bind_string(l.local.clone(), var);
            }
            Some(AbiKind::Null) | None => {
                return Err(anyhow!(
                    "GirOp::Block let with bare Null / non-fusable value — \
                     should have widened to Nullable<T>"
                ));
            }
        }
    }
    // The tail's value may alias a block-scoped composite local we're
    // about to drop — `ensure_owned_composite` clones a Borrowed result
    // so it outlives the block. A tainted tail surfaces its validity
    // (the block result is then a `Scalar2`).
    let tail_cv = compile_expr(b, tail, env, ctx)?;
    let result = match tail_cv {
        CompiledExpr::Single(v) => {
            CompiledExpr::Single(ensure_owned_composite(b, ctx, tail, v)?)
        }
        CompiledExpr::Scalar2 { value, valid } => {
            // A tainted tail is always a scalar (taint only rides
            // scalars), so `ensure_owned_composite` is a pass-through.
            let value = ensure_owned_composite(b, ctx, tail, value)?;
            CompiledExpr::Scalar2 { value, valid }
        }
        CompiledExpr::Value { .. } => {
            return Err(anyhow!(
                "compile_block_scalar: Value-shape tail — should route to \
                 compile_block_value"
            ));
        }
    };
    // Drop the composite/variant/nullable/string locals introduced by
    // THIS block (owned pointers — they'd otherwise leak per iteration
    // in a loop body). Scalars need no drop. The pending path is
    // mutually exclusive (its `pre_pending` runs `emit_pending_cleanup`).
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
    for (_, var, _) in &env.composites[mark.composites..] {
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

/// Compile a scalar-success `?` (`GirOp::QopUnwrap`). The inner is a
/// Nullable `(disc, payload)`; `disc == Typ::Error` (`0x2000_0000`)
/// means bottom. For a PRIM success type, this is BRANCHLESS: the
/// payload bits are cast to the prim and the result is
/// `Scalar2 { value, valid: !is_err }` — a `?` whose error the OUTPUT
/// never consumes (un-taken arm, dead let) no longer aborts; the abort
/// moves to the boundary, exactly like div/mod.
///
/// String / composite success types KEEP the branch-to-`pending_exit`
/// abort: on the error branch the payload is the error's `Arc<Value>`,
/// NOT an ArcStr/ValArray, so a branchless "extract the success" would
/// type-confuse the eventual ownership/drop. They return `Single`
/// (no per-value validity for those shapes in this phase).
fn compile_scalar_qop_unwrap(
    b: &mut FunctionBuilder,
    inner: &GirExpr,
    success_typ: &Type,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<CompiledExpr> {
    use cranelift_codegen::ir::condcodes::IntCC;
    let cv = compile_expr(b, inner, env, ctx)?;
    let (disc, payload) = match cv {
        CompiledExpr::Value { disc, payload } => (disc, payload),
        _ => {
            return Err(anyhow!(
                "GirOp::QopUnwrap: inner not Value-shape — emit_expr should \
                 only emit on Nullable inner"
            ))
        }
    };
    let is_err = b.ins().icmp_imm(IntCC::Equal, disc, 0x2000_0000_i64);

    match gir::abi_kind(success_typ) {
        // Prim success — BRANCHLESS per-value validity. The payload
        // word holds the success bits when !is_err; on the error path
        // the bits are garbage but `valid=0` means they're never used.
        // The error Value isn't dropped here: a `Value::Error` carries
        // an `Arc<Value>` payload, but a scalar `Nullable` inner is a
        // by-value scalar (no heap), so nothing leaks. (A Local-read
        // Borrowed inner is owned by its env slot and dropped at scope
        // exit either way.)
        Some(AbiKind::Scalar(p)) => {
            let value = cast_u64_to_prim(b, payload, p);
            let one = b.ins().iconst(types::I8, 1);
            let valid = b.ins().bxor(is_err, one); // !is_err
            Ok(CompiledExpr::Scalar2 { value, valid })
        }
        // String / composite success — keep the branch-abort path.
        Some(AbiKind::String | AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let pending_set = ctx
                .helper_refs
                .get("graphix_dyncall_set_pending")
                .ok_or_else(|| anyhow!("missing graphix_dyncall_set_pending"))?;
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
            // Drop the owned error Value only when `inner` is an owned
            // producer (a Borrowed Local is owned by its env slot, which
            // `emit_pending_cleanup` drops — dropping here too would
            // double-free).
            if inner_owned {
                b.ins().call(value_drop, &[disc, payload]);
            }
            b.ins().call(pending_set, &[]);
            emit_pending_cleanup(b, env, ctx)?;
            b.ins().jump(pending_exit, &[]);
            b.switch_to_block(continue_block);
            b.seal_block(continue_block);
            // Extract success T (now known non-error). QopUnwrap is
            // classified Owned, so a String/composite success from a
            // Borrowed (Local) inner must be cloned. String: the
            // ArcStr bits inside a Value ARE the string ABI's one-word
            // representation, so the payload passes through (owned) or
            // refcount-bumps (borrowed). Composite: the payload word is
            // the ValArray BITS, but the composite ABI is a boxed
            // `*mut ValArray` — pass through `graphix_value_into_array`
            // (consumes) / `_borrowed` (clones inner) to re-box; handing
            // the raw bits downstream is the type confusion behind #199
            // (drop did `Box::from_raw` on the Arc's data pointer).
            let v = match gir::abi_kind(success_typ) {
                Some(AbiKind::String) => {
                    if inner_owned {
                        payload
                    } else {
                        let clone = ctx
                            .helper_refs
                            .get("graphix_arcstr_clone")
                            .ok_or_else(|| anyhow!("missing graphix_arcstr_clone"))?;
                        let call = b.ins().call(clone, &[payload]);
                        b.inst_results(call)[0]
                    }
                }
                _ => {
                    let helper_name = if inner_owned {
                        "graphix_value_into_array"
                    } else {
                        "graphix_value_into_array_borrowed"
                    };
                    let unbox = ctx
                        .helper_refs
                        .get(helper_name)
                        .ok_or_else(|| anyhow!("missing {helper_name}"))?;
                    let call = b.ins().call(unbox, &[disc, payload]);
                    b.inst_results(call)[0]
                }
            };
            Ok(CompiledExpr::Single(v))
        }
        // Value-shape success types belong in `compile_value_expr`.
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => Err(anyhow!(
            "QopUnwrap with Value-shape success_typ reached the scalar path \
             — compile_expr should route to compile_value_expr"
        )),
        Some(AbiKind::Unit | AbiKind::Null) | None => Err(anyhow!(
            "QopUnwrap with unsupported success_typ {:?}",
            success_typ
        )),
    }
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
    arg_types: &[Type],
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
        let helper_name: &str = match gir::abi_kind(t) {
            Some(AbiKind::Scalar(p)) => value_buf_push_helper(p)?,
            Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                match classify_composite_source(a) {
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
                match classify_composite_source(a) {
                    CompositeSource::Owned => "graphix_value_buf_push_value",
                    CompositeSource::Borrowed => {
                        "graphix_value_buf_push_value_borrowed"
                    }
                }
            }
            Some(AbiKind::String) => "graphix_value_buf_push_string",
            Some(AbiKind::Unit) => {
                return Err(anyhow!("GIR malformed: DynCall arg has Unit type"));
            }
            Some(AbiKind::Null) | None => {
                return Err(anyhow!(
                    "DynCall arg with bare Null / non-fusable type — should \
                     have widened to Nullable<T> at construction"
                ));
            }
        };
        let push = ctx
            .helper_refs
            .get(helper_name)
            .ok_or_else(|| anyhow!("missing push helper `{helper_name}`"))?;
        if gir::is_value_shape(t) {
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
        // Value-shape constant (datetime/duration/bytes/map literal):
        // bake a stable `*const Value` from the kernel's value-constants
        // table and clone it (bumps the inner Arc) → an owned
        // `(disc, payload)` Value. (Scalar `Const`s route through
        // `compile_scalar_impl` via `gir::is_value_shape(&e.typ)`.)
        GirOp::Const(v) => {
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
            let vv = if matches!(gir::abi_kind(&e.typ), Some(AbiKind::Variant)) {
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
        // Value-shape IfChain. `scrut` is passed through to
        // `compile_ifchain`, which AND's a bottom scrutinee's validity
        // into the result for the scalar path; the value-shape merge
        // carries bottom in-band via the existing pending machinery, so
        // the scrut gate is a no-op for it today.
        GirOp::IfChain { scrut, arms } => {
            compile_ifchain(b, scrut.as_deref(), arms, &e.typ, env, ctx)
        }
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
            debug_assert!(gir::is_value_shape(&return_type));
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
            emit_dyncall_pending_branch(b, env, ctx)?;
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
                match gir::abi_kind(&l.value.typ) {
                    Some(AbiKind::Scalar(p)) => {
                        bind_scalar_let(b, env, ctx, &l.local, &l.value, p)?;
                    }
                    Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                        let v = compile_scalar(b, &l.value, env, ctx)?;
                        let owned =
                            ensure_owned_composite(b, ctx, &l.value, v)?;
                        let var = b.declare_var(types::I64);
                        b.def_var(var, owned);
                        env.bind_composite(l.local.clone(), var);
                    }
                    Some(
                        AbiKind::Variant | AbiKind::Nullable | AbiKind::Value,
                    ) => {
                        let cv = compile_expr(b, &l.value, env, ctx)?;
                        let (owned_disc, owned_payload) =
                            ensure_owned_value(b, ctx, &l.value, cv)?;
                        let disc_var = b.declare_var(types::I64);
                        let payload_var = b.declare_var(types::I64);
                        b.def_var(disc_var, owned_disc);
                        b.def_var(payload_var, owned_payload);
                        let vv =
                            ValueVar { disc: disc_var, payload: payload_var };
                        if matches!(gir::abi_kind(&l.value.typ), Some(AbiKind::Variant)) {
                            env.bind_variant(l.local.clone(), vv);
                        } else {
                            // datetime/duration/bytes/map/error share the
                            // nullables slot.
                            env.bind_nullable(l.local.clone(), vv);
                        }
                    }
                    Some(AbiKind::Unit) => {
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
                    Some(AbiKind::String) => {
                        let v = compile_scalar(b, &l.value, env, ctx)?;
                        let var = b.declare_var(types::I64);
                        b.def_var(var, v);
                        env.bind_string(l.local.clone(), var);
                    }
                    Some(AbiKind::Null) | None => {
                        return Err(anyhow!(
                            "Block let with bare Null / non-fusable value — \
                             should have widened to Nullable<T>"
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
            for (_, var, _) in &env.composites[mark.composites..] {
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
                gir::is_value_shape(&success_typ),
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
            // (`null` if none) — see [`scaffold::emit_find_loop`].
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("GIR malformed: undefined array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let mut cx = BodyCx { b: &mut *b, env: &mut *env, ctx };
            let (disc, payload) = scaffold::emit_find_loop(
                &mut cx,
                scaffold::ArraySrc { ptr: arr_ptr, owned: false },
                &scaffold::HofElem {
                    name: elem_local,
                    id: None,
                    typ: elem,
                    leaves: &[],
                },
                |cx| compile_scalar(cx.b, predicate, cx.env, cx.ctx),
            )?;
            Ok(CompiledExpr::Value { disc, payload })
        }
        GirOp::ArrayFindMap { array, in_elem, elem_local, body } => {
            // Early-exit on the first non-null body result; the op's
            // value is that `Nullable<out>` (or null) — see
            // [`scaffold::emit_find_map_loop`].
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("GIR malformed: undefined array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let mut cx = BodyCx { b: &mut *b, env: &mut *env, ctx };
            let (disc, payload) = scaffold::emit_find_map_loop(
                &mut cx,
                scaffold::ArraySrc { ptr: arr_ptr, owned: false },
                &scaffold::HofElem {
                    name: elem_local,
                    id: None,
                    typ: in_elem,
                    leaves: &[],
                },
                |cx| compile_value_expr(cx.b, body, cx.env, cx.ctx)?.value(),
            )?;
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
        // A scalar `Const` — the prim comes from the expression's `typ`
        // (a value-shape `Const` would have routed to
        // `compile_value_expr` via `gir::is_value_shape(&e.typ)`).
        GirOp::Const(c) => {
            let prim = gir::scalar_prim(&e.typ).ok_or_else(|| {
                anyhow!("compile_scalar_impl: Const with non-prim typ {:?}", e.typ)
            })?;
            Ok(compile_const(b, c, prim))
        }
        // Value-shape ops belong in `compile_value_expr`; reaching the
        // scalar path means routing drifted (or the kernel can't JIT,
        // which `compile_value_expr` reports as Err → interp fallback).
        GirOp::ValueArith { .. } => Err(anyhow!(
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
        GirOp::QopUnwrap { .. } => Err(anyhow!(
            "GirOp::QopUnwrap reached compile_scalar_impl — `compile_expr` \
             routes it through `compile_tainted_scalar`"
        )),
        GirOp::Local(name) => {
            // Dispatch on the expression's GirType: scalar locals
            // sit in `env.locals`, composite locals (array/tuple/
            // struct pointers) in `env.composites`. The CLIF type
            // returned matches — scalars get their prim type,
            // composites get I64 (the pointer).
            match gir::abi_kind(&e.typ) {
                // Scalar Locals are intercepted in `compile_expr` (so a
                // tainted local surfaces its validity as `Scalar2`); a
                // non-tainted local reaching here is fine, a tainted one
                // would lose its validity bit — error so it bails to
                // interp rather than silently dropping a bottom.
                Some(AbiKind::Scalar(_)) => {
                    let (var, valid, _) = env.lookup(name).ok_or_else(|| {
                        anyhow!("GIR malformed: undefined scalar local `{name}`")
                    })?;
                    if valid.is_some() {
                        return Err(anyhow!(
                            "compile_scalar_impl reached for tainted scalar \
                             local `{name}` — `compile_expr` should surface \
                             its validity"
                        ));
                    }
                    Ok(b.use_var(var))
                }
                Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                    let var = env.lookup_composite(name).ok_or_else(|| {
                        anyhow!(
                            "GIR malformed: undefined composite local `{name}`"
                        )
                    })?;
                    Ok(b.use_var(var))
                }
                // Variant / Nullable / value-shape Locals are Value-shape
                // and belong in `compile_value_expr`, not here.
                // `compile_expr` dispatches on the expression's typ
                // before calling `compile_scalar_impl`, so this arm
                // is unreachable in well-formed routing — but we
                // surface the error in case routing drifts.
                Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
                    Err(anyhow!(
                        "compile_scalar_impl reached for Value-shape Local \
                         `{name}` — `compile_expr` should have routed to \
                         `compile_value_expr`"
                    ))
                }
                Some(AbiKind::Unit) => Err(anyhow!(
                    "GIR malformed: Local `{name}` has Unit type"
                )),
                // String Local: read the slot's bits (an `ArcStr`
                // by value), refcount-bump via `graphix_arcstr_clone`
                // (which takes by value, `mem::forget`s the input,
                // returns a fresh clone). The slot's variable still
                // logically holds the original ref; the clone is
                // owned by this expression's caller.
                Some(AbiKind::String) => {
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
                // Bare `Null` locals don't exist; the only `Null`-typed
                // expression is the inline `ConstNull` literal, which
                // fusion always widens to `Nullable<T>` before binding.
                Some(AbiKind::Null) | None => Err(anyhow!(
                    "Local `{name}` has bare Null / non-fusable type — \
                     should have widened to Nullable<T> at construction"
                )),
            }
        }
        GirOp::Bin { .. } | GirOp::Cmp { .. } => Err(anyhow!(
            "GirOp::Bin / Cmp reached compile_scalar_impl — `compile_expr` \
             routes them through `compile_tainted_scalar`"
        )),
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
        GirOp::BoolBin { .. } | GirOp::Not(_) | GirOp::Cast { .. } => {
            Err(anyhow!(
                "GirOp::BoolBin / Not / Cast reached compile_scalar_impl — \
                 `compile_expr` routes them through `compile_tainted_scalar`"
            ))
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
        GirOp::Block { .. } => Err(anyhow!(
            "GirOp::Block reached compile_scalar_impl — `compile_expr` routes \
             scalar Blocks through `compile_tainted_scalar` (and Value-shape \
             ones through `compile_value_expr`)"
        )),
        GirOp::IfChain { .. } => Err(anyhow!(
            "GirOp::IfChain reached compile_scalar_impl — `compile_expr` \
             routes scalar IfChains through `compile_tainted_scalar` (and \
             Value-shape ones through `compile_value_expr`)"
        )),
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
            let ret_kind: i64 = match gir::abi_kind(return_type) {
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

            match gir::abi_kind(return_type) {
                Some(AbiKind::Scalar(p)) => {
                    // Scalar return: 0 on pending is a harmless
                    // sentinel for downstream scalar arithmetic.
                    // No branch needed — the wrapper-level
                    // DYNCALL_PENDING check in GirNode::update
                    // discards the whole kernel result.
                    Ok(cast_u64_to_prim(b, raw_u64, p))
                }
                Some(AbiKind::Unit) => {
                    // Unit return: dispatcher returned (0, _). The
                    // downstream `Discard` throws it away. The
                    // wrapper-level DYNCALL_PENDING check still
                    // fires.
                    Ok(raw_u64)
                }
                Some(AbiKind::String) => {
                    // String return: `raw_u64` is the ArcStr's raw
                    // thin-pointer bits (transferred ownership), or
                    // the null sentinel on pending. Null bits are NOT
                    // inert downstream the way a scalar 0 is — every
                    // String position assumes a valid owned ArcStr
                    // (`emit_return_pending_check` / scope-exit
                    // `drop_owned_strings` would null-drop it; #214) —
                    // so branch exactly like the composite arm.
                    emit_dyncall_pending_branch(b, env, ctx)?;
                    Ok(raw_u64)
                }
                Some(AbiKind::Null) | None => {
                    return Err(anyhow!(
                        "DynCall with bare Null / non-fusable return — \
                         should have widened to Nullable<T> at construction"
                    ));
                }
                // Variant / Nullable / value-shape DynCall returns are
                // Value-shape and belong in `compile_value_expr`;
                // compile_expr dispatches before reaching this scalar
                // arm. Surface an error in case routing drifts.
                Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
                    Err(anyhow!(
                        "DynCall with Value-shape return reached \
                         `compile_scalar_impl` — `compile_expr` should \
                         have routed to `compile_value_expr`"
                    ))
                }
                Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                    // Composite return: `raw_u64` is an owned
                    // `*mut ValArray`, or null on pending. Null can't
                    // be deref'd by downstream ops, so we branch: on
                    // pending, drop every owned local + every outer
                    // in-flight DynCall buf, then jump to
                    // `pending_exit`.
                    emit_dyncall_pending_branch(b, env, ctx)?;
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
            // Build an `n`-element array by pushing the body result
            // per index — see [`scaffold::emit_init_loop`].
            let n_raw = compile_scalar(b, n, env, ctx)?;
            let out_src = classify_composite_source(body);
            let mut cx = BodyCx { b: &mut *b, env: &mut *env, ctx };
            scaffold::emit_init_loop(
                &mut cx,
                n_raw,
                prim_of(&n.typ),
                idx_local,
                None,
                &body.typ,
                out_src,
                |cx| compile_expr(cx.b, body, cx.env, cx.ctx),
            )
        }
        GirOp::ArrayMap { array, in_elem, elem_local, body } => {
            // Push the body result per element of an existing
            // composite param/local — see [`scaffold::emit_map_loop`].
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("GIR malformed: undefined array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let out_src = classify_composite_source(body);
            let mut cx = BodyCx { b: &mut *b, env: &mut *env, ctx };
            scaffold::emit_map_loop(
                &mut cx,
                scaffold::ArraySrc { ptr: arr_ptr, owned: false },
                &scaffold::HofElem {
                    name: elem_local,
                    id: None,
                    typ: in_elem,
                    leaves: &[],
                },
                &body.typ,
                out_src,
                |cx| compile_expr(cx.b, body, cx.env, cx.ctx),
            )
        }
        GirOp::ArrayFilter { array, elem, elem_local, predicate } => {
            // Conditional push of the ORIGINAL element on a true
            // predicate — see [`scaffold::emit_filter_loop`]. The
            // predicate compiles via `compile_scalar`, so a Scalar2
            // (may-bottom) predicate Errs at build time = de-fuse.
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("GIR malformed: undefined array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let mut cx = BodyCx { b: &mut *b, env: &mut *env, ctx };
            scaffold::emit_filter_loop(
                &mut cx,
                scaffold::ArraySrc { ptr: arr_ptr, owned: false },
                &scaffold::HofElem {
                    name: elem_local,
                    id: None,
                    typ: elem,
                    leaves: &[],
                },
                |cx| compile_scalar(cx.b, predicate, cx.env, cx.ctx),
            )
        }
        GirOp::ArrayFilterMap { array, in_elem, elem_local, out_elem, body } => {
            // Collect the non-null `Nullable<out_elem>` body results —
            // see [`scaffold::emit_filter_map_loop`].
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("GIR malformed: undefined array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let mut cx = BodyCx { b: &mut *b, env: &mut *env, ctx };
            scaffold::emit_filter_map_loop(
                &mut cx,
                scaffold::ArraySrc { ptr: arr_ptr, owned: false },
                *in_elem,
                elem_local,
                None,
                *out_elem,
                |cx| compile_expr(cx.b, body, cx.env, cx.ctx),
            )
        }
        GirOp::ArrayFlatMap { array, in_elem, elem_local, out_elem: _, body } => {
            // Concatenate the per-element `Array<out_elem>` body
            // results — see [`scaffold::emit_flat_map_loop`]. The body
            // must hand the scaffold an OWNED array for `extend` to
            // consume; a Borrowed source (Local read) is refcount-
            // cloned first.
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("GIR malformed: undefined array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let mut cx = BodyCx { b: &mut *b, env: &mut *env, ctx };
            scaffold::emit_flat_map_loop(
                &mut cx,
                scaffold::ArraySrc { ptr: arr_ptr, owned: false },
                &scaffold::HofElem {
                    name: elem_local,
                    id: None,
                    typ: in_elem,
                    leaves: &[],
                },
                |cx| {
                    let p = compile_scalar(cx.b, body, cx.env, cx.ctx)?;
                    ensure_owned_composite(cx.b, cx.ctx, body, p)
                },
            )
        }
        GirOp::ArrayFold { array, elem_typ, init, acc_local, elem_local, body } => {
            // Scalar accumulator threaded through the loop — see
            // [`scaffold::emit_fold_loop`]. The acc prim is the FOLD's
            // own result type.
            let acc_prim = prim_of(&e.typ);
            let arr_var = env.lookup_composite(array).ok_or_else(|| {
                anyhow!("GIR malformed: undefined fold array `{array}` in JIT")
            })?;
            let arr_ptr = b.use_var(arr_var);
            let mut cx = BodyCx { b: &mut *b, env: &mut *env, ctx };
            scaffold::emit_fold_loop(
                &mut cx,
                scaffold::ArraySrc { ptr: arr_ptr, owned: false },
                acc_prim,
                acc_local,
                None,
                &scaffold::HofElem {
                    name: elem_local,
                    id: None,
                    typ: elem_typ,
                    leaves: &[],
                },
                |cx| compile_scalar(cx.b, init, cx.env, cx.ctx),
                |cx| compile_scalar(cx.b, body, cx.env, cx.ctx),
            )
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
        // ArrayFindMap produces a Nullable (Value-shape) result, so
        // `compile_expr` routes it to `compile_value_expr`; reaching the
        // scalar path means routing drifted.
        GirOp::ArrayFindMap { .. } => Err(anyhow!(
            "ArrayFindMap is Value-shape — should route to compile_value_expr"
        )),
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
        // A value-shape `Const` (datetime/duration/bytes/map literal) is
        // lowered via `graphix_value_clone_from_static`, which bumps the
        // inner Arc — a fresh owned ref. `ValueArith` (datetime/duration
        // math) returns an owned Value from `graphix_value_<op>`. The
        // value-shape `QopUnwrap` success path runs its result through
        // `ensure_owned_value`, so it too hands out an owned Value.
        | GirOp::Const(_)
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

/// Direct-path cross-kernel lambda call — the Node twin of the
/// `GirOp::Call` arms. The flat formals+captures list is assembled in
/// the same pre-group order the callee's input list uses (formal args
/// in FnType parameter order, then captures in `CaptureSlot` order),
/// then kind-grouped to the callee's ABI exactly like
/// [`compile_call_clif_args`]: scalars, composite pointers
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

fn compile_call_clif_args(
    b: &mut FunctionBuilder,
    fn_name: &ArcStr,
    args: &[GirExpr],
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<(Vec<ClifValue>, Vec<CallArgDrop>)> {
    for a in args {
        // Scalar / composite (array/tuple/struct) / Variant / Nullable
        // args are wired on the calling side. String, bare value-shape
        // (DateTime/Duration/Bytes/Map/Error) args are valid kernel
        // params (the callee declares string_params / value_params),
        // but the *calling* kernel's JIT arg emit for them isn't wired
        // yet — bail so the caller falls back to the interpreter (which
        // routes these correctly). Unit/Null have no kernel-param shape
        // at all. Exhaustive match keeps a future AbiKind from silently
        // dropping an arg.
        match gir::abi_kind(&a.typ) {
            Some(
                AbiKind::Scalar(_)
                | AbiKind::Array
                | AbiKind::Tuple
                | AbiKind::Struct
                | AbiKind::Variant
                | AbiKind::Nullable,
            ) => {}
            Some(
                AbiKind::String
                | AbiKind::Value
                | AbiKind::Unit
                | AbiKind::Null,
            )
            | None => {
                return Err(anyhow!(
                    "JIT GirOp::Call `{fn_name}` arg type {:?} not yet \
                     lowered on the calling side — falling back to interp",
                    a.typ
                ));
            }
        }
    }
    let is_kind = |a: &&GirExpr, k: AbiKind| gir::abi_kind(&a.typ) == Some(k);
    let mut clif_args: Vec<ClifValue> = Vec::with_capacity(args.len());
    let mut drops: Vec<CallArgDrop> = Vec::new();
    // Scalars first.
    for a in args.iter().filter(|a| matches!(gir::abi_kind(&a.typ), Some(AbiKind::Scalar(_)))) {
        clif_args.push(compile_scalar(b, a, env, ctx)?);
    }
    // Composite pointers: array, then tuple, then struct.
    let composite = args
        .iter()
        .filter(|a| is_kind(a, AbiKind::Array))
        .chain(args.iter().filter(|a| is_kind(a, AbiKind::Tuple)))
        .chain(args.iter().filter(|a| is_kind(a, AbiKind::Struct)));
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
        .filter(|a| is_kind(a, AbiKind::Variant))
        .chain(args.iter().filter(|a| is_kind(a, AbiKind::Nullable)));
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
    if matches!(
        gir::abi_kind(&e.typ),
        Some(AbiKind::Scalar(_) | AbiKind::Unit | AbiKind::String)
    ) || classify_composite_source(e) == CompositeSource::Owned
    {
        return Ok(v);
    }
    match gir::abi_kind(&e.typ) {
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let helper = ctx
                .helper_refs
                .get("graphix_valarray_clone")
                .ok_or_else(|| anyhow!("missing graphix_valarray_clone"))?;
            let call = b.ins().call(helper, &[v]);
            Ok(b.inst_results(call)[0])
        }
        // Value-shape (Variant / Nullable / datetime / …) sources use
        // `ensure_owned_value` instead — they're two CLIF values, not
        // one, so they can't flow through this single-ClifValue path.
        // Reaching here means the consumer mis-routed.
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            Err(anyhow!(
                "ensure_owned_composite reached for Value-shape type \
                 `{:?}` — caller should use `ensure_owned_value` instead",
                e.typ
            ))
        }
        Some(AbiKind::Scalar(_) | AbiKind::Unit | AbiKind::String) => {
            unreachable!("guarded above")
        }
        // Bare `Null` is the singleton from `GirOp::ConstNull` — fusion
        // always widens to `Nullable<T>` before any binding / merge,
        // so a bare-`Null` reaching this clone path is malformed.
        Some(AbiKind::Null) | None => Err(anyhow!(
            "ensure_owned_composite reached for bare Null / non-fusable \
             type — should have widened to Nullable<T> at construction"
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
    match gir::abi_kind(&e.typ) {
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let cv = compile_expr(b, e, env, ctx)?;
            ensure_owned_value(b, ctx, e, cv)
        }
        Some(AbiKind::Scalar(p)) => {
            let s = compile_scalar(b, e, env, ctx)?;
            let disc = b.ins().iconst(types::I64, prim_to_value_disc(p));
            let payload = scalar_to_payload_i64(b, p, s);
            Ok((disc, payload))
        }
        Some(AbiKind::String) => {
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
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
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

/// Compile a producer-op field expression and push it into `buf` —
/// the compile half; the helper selection + shape dispatch is
/// [`scaffold::push_field`]. Used by `GirOp::TupleNew`, `StructNew`,
/// and `VariantNew` for nested composite fields (the HOF loop
/// scaffolds call `push_field` directly with their body closures'
/// results).
fn compile_and_push_field(
    b: &mut FunctionBuilder,
    env: &mut JitEnv,
    ctx: &LowerCtx,
    buf: ClifValue,
    field: &GirExpr,
) -> Result<()> {
    let src = classify_composite_source(field);
    let cv = compile_expr(b, field, env, ctx)?;
    let mut cx = BodyCx { b, env, ctx };
    scaffold::push_field(&mut cx, buf, cv, &field.typ, src)
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

fn compile_ifchain(
    b: &mut FunctionBuilder,
    scrut: Option<&GirExpr>,
    arms: &[(Option<GirExpr>, GirExpr)],
    result_typ: &Type,
    env: &mut JitEnv,
    ctx: &LowerCtx,
) -> Result<CompiledExpr> {
    if arms.is_empty() {
        return Err(anyhow!("GIR malformed: empty if-chain"));
    }
    // Merge block holds the result via block parameters. For a
    // scalar / composite-pointer result, that's a single I64 (or
    // the prim's CLIF type). For a Value-shape result (Variant /
    // Nullable / datetime / …) it's two I64s (disc, payload). Every
    // arm normalizes its result to the merge shape before jumping.
    let is_value_shape = gir::is_value_shape(result_typ);

    // The SCRUTINEE gate (interp parity, #178): the node-walk's Select
    // fires iff the scrutinee has a value. A bottom scrutinee poisons
    // the WHOLE select — it AND's into the result validity (whereas a
    // bottom GUARD, below, just fails its arm). The JIT folds the
    // scrutinee into the arm conds (a `Local` / `__sel_scrut` temp), so
    // we re-compile it here only to extract its validity bit; the bits
    // themselves are unused.
    let scrut_valid: Option<ClifValue> = match scrut {
        Some(s) if !is_value_shape || matches!(gir::abi_kind(&s.typ), Some(AbiKind::Scalar(_))) => {
            match compile_expr(b, s, env, ctx)? {
                CompiledExpr::Scalar2 { valid, .. } => Some(valid),
                _ => None,
            }
        }
        _ => None,
    };

    // A possibly-bottom scrutinee makes every arm cond computed from
    // GARBAGE bits — with a conditional final arm, all arms can miss
    // and the trap below becomes reachable (#201, SIGILL). The
    // validity-AND already poisons the RESULT correctly; the conds it
    // can't fix. Refuse to fuse — the select node-walks (which
    // produces no value, the canonical semantics). The stmt form
    // (`compile_select_stmt`) is immune: its scrutinee gate branches
    // to `pending_exit` before any cond runs.
    if scrut_valid.is_some()
        && arms.last().is_some_and(|(c, _)| c.is_some())
    {
        return Err(anyhow!(
            "if-chain: possibly-bottom scrutinee with a conditional \
             final arm — the miss trap would be reachable; falls back \
             to the node-walk"
        ));
    }

    // For a scalar/composite result, thread an I8 validity bit through
    // the merge phi IFF a guard, the scrutinee, or any arm body can be
    // tainted (the taint optimization — a clean IfChain stays `Single`).
    // Value-shape results carry bottom in-band today via the existing
    // pending machinery, so they don't get a validity phi here.
    let scalar_taintable = !is_value_shape
        && (scrut_valid.is_some()
            || arms.iter().any(|(c, body)| {
                c.as_ref().is_some_and(gir::expr_may_value_bottom)
                    || gir::expr_may_value_bottom(body)
            }));

    let merge = b.create_block();
    if is_value_shape {
        b.append_block_param(merge, types::I64); // disc
        b.append_block_param(merge, types::I64); // payload
    } else {
        b.append_block_param(merge, clif_of(result_typ));
        if scalar_taintable {
            b.append_block_param(merge, types::I8); // validity
        }
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
                // A GUARD that is bottom → the arm does NOT match (the
                // node-walk's `is::match` on a bottom guard falls
                // through). `effective_cond = cond_value AND cond_valid`
                // — an invalid guard reads as `false`, advancing to the
                // next arm.
                let (cval, cvalid) =
                    compile_expr(b, c, env, ctx)?.scalar_with_validity(b)?;
                let cv = b.ins().band(cval, cvalid);
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
            let cv = compile_expr(b, body, env, ctx)?;
            if scalar_taintable {
                // Result validity = arm-body validity AND scrut validity
                // (a bottom scrutinee poisons every arm's output).
                let (v, valid) = cv.scalar_with_validity(b)?;
                // A tainted IfChain result is always a scalar (taint
                // only rides scalars), so `ensure_owned_composite` is a
                // no-op pass-through here; kept for symmetry with the
                // composite branch.
                let v = ensure_owned_composite(b, ctx, body, v)?;
                let valid = match scrut_valid {
                    Some(sv) => b.ins().band(valid, sv),
                    None => valid,
                };
                env.truncate(mark);
                b.ins().jump(
                    merge,
                    &[BlockArg::Value(v), BlockArg::Value(valid)],
                );
            } else {
                let v = cv.single()?;
                let v = ensure_owned_composite(b, ctx, body, v)?;
                env.truncate(mark);
                b.ins().jump(merge, &[BlockArg::Value(v)]);
            }
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
    } else if scalar_taintable {
        let params = b.block_params(merge);
        Ok(CompiledExpr::Scalar2 { value: params[0], valid: params[1] })
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
    _result_typ: &Type,
) -> Result<(ClifValue, ClifValue)> {
    match gir::abi_kind(&body.typ) {
        // Already value-shape (Variant / Nullable / datetime / …):
        // refcount-bump a borrowed source so the merge owns a fresh ref.
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            ensure_owned_value(b, ctx, body, cv)
        }
        Some(AbiKind::Null) => {
            let disc = b.ins().iconst(types::I64, value_disc::NULL);
            let payload = b.ins().iconst(types::I64, 0);
            Ok((disc, payload))
        }
        Some(AbiKind::Scalar(p)) => {
            let v = cv.single()?;
            let disc =
                b.ins().iconst(types::I64, prim_to_value_disc(p));
            // Promote/bitcast the scalar to the I64 payload slot.
            let payload = scalar_to_payload_i64(b, p, v);
            Ok((disc, payload))
        }
        other => Err(anyhow!(
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
        match gir::abi_kind(&part.typ) {
            Some(AbiKind::String) => {
                let s = compile_scalar(b, part, env, ctx)?;
                let push = ctx
                    .helper_refs
                    .get("graphix_string_buf_push_arcstr")
                    .ok_or_else(|| {
                        anyhow!("missing graphix_string_buf_push_arcstr")
                    })?;
                b.ins().call(push, &[buf, s]);
            }
            Some(AbiKind::Scalar(p)) => {
                let v = compile_scalar(b, part, env, ctx)?;
                let helper_name = string_buf_push_helper(p);
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

/// Lower a scalar [`Value`] constant of the given `prim` to a CLIF
/// `iconst`/`f32const`/`f64const`. `prim` comes from the `GirOp::Const`
/// expression's `typ`; `v` must be the matching scalar (`Z*`/`V*`
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
fn prim_of(t: &Type) -> PrimType {
    gir::scalar_prim(t).expect("JIT scalar-only: array typ slipped past compile_expr")
}

/// CLIF type of a [`Type`] at the JIT ABI level. Primitives map to
/// their natural CLIF type; every composite / value-shape / string /
/// option leaf is a pointer-or-register slot, i.e. `I64` on a 64-bit
/// target.
fn clif_of(t: &Type) -> ClifType {
    match gir::abi_kind(t) {
        Some(AbiKind::Scalar(p)) => prim_to_clif(p),
        // Composite pointers, value-shape (datetime/duration/bytes/map/
        // error and variant/nullable), Unit's zero slot, String's
        // thin pointer, and the bare-Null defensive slot all ABI as a
        // single `I64`.
        Some(
            AbiKind::Array
            | AbiKind::Tuple
            | AbiKind::Struct
            | AbiKind::Variant
            | AbiKind::Nullable
            | AbiKind::Value
            | AbiKind::Unit
            | AbiKind::String
            | AbiKind::Null,
        )
        | None => types::I64,
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
        arith, bool_op, cast, cmp, const_expr, datetime_type, duration_type,
        local, null_type, nullable_type, prim_type, string_type, tuple_type,
        BinOp, BoolOp, CmpOp, Input, GirExpr, GirKernel, GirOp, GirStmt,
        KernelSig, Let, PrimType, SelectArm,
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

    /// Pack a scalar `Value` into its ABI u64 slot, deriving the
    /// `PrimType` from the value's own variant. Test convenience for
    /// building wrapper-call arg arrays.
    fn pack(v: &Value) -> u64 {
        let prim = crate::gir::scalar_prim_of_value(v).expect("scalar value");
        pack_value_to_u64(v, prim)
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
            sig: KernelSig {
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
                return_type: prim_type(PrimType::I64),
                has_tail_loop: false,
            }
            .into(),
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
                        const_expr(Value::F64(2.0)),
                        BinOp::Mul,
                    )
                    .unwrap(),
                ),
            },
            typ: prim_type(PrimType::F64),
        };
        let kernel = GirKernel {
            sig: KernelSig {
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
                return_type: prim_type(PrimType::F64),
                has_tail_loop: false,
            }
            .into(),
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
                const_expr(Value::F64(0.0)),
                CmpOp::Gt,
            )
            .unwrap(),
            cmp(
                loc("y", PrimType::F64),
                const_expr(Value::F64(10.0)),
                CmpOp::Lt,
            )
            .unwrap(),
            BoolOp::And,
        )
        .unwrap();
        let kernel = GirKernel {
            sig: KernelSig {
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
                return_type: prim_type(PrimType::Bool),
                has_tail_loop: false,
            }
            .into(),
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
            sig: KernelSig {
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
                return_type: prim_type(PrimType::F64),
                has_tail_loop: false,
            }
            .into(),
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
                    const_expr(Value::I64(0)),
                    CmpOp::Eq,
                )
                .unwrap(),
            ),
            body: vec![GirStmt::Return(const_expr(Value::I64(0)))],
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
            const_expr(Value::F64(4.0)),
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
                    const_expr(Value::F64(2.0)),
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
            const_expr(Value::I64(1)),
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
            sig: KernelSig {
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
                return_type: prim_type(PrimType::I64),
                has_tail_loop: true,
            }
            .into(),
            body: vec![GirStmt::Select { scrut: None, arms: vec![arm0, arm1, arm2] }],
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
                    const_expr(Value::I64(0)),
                    CmpOp::Eq,
                )
                .unwrap(),
            ),
            body: vec![GirStmt::Return(const_expr(Value::I64(0)))],
        };
        let arm1 = SelectArm {
            cond: None,
            body: vec![GirStmt::TailCall {
                args: vec![arith(
                    loc("n", PrimType::I64),
                    const_expr(Value::I64(1)),
                    BinOp::Sub,
                )
                .unwrap()],
            }],
        };
        let kernel = GirKernel {
            sig: KernelSig {
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
                return_type: prim_type(PrimType::I64),
                has_tail_loop: true,
            }
            .into(),
            body: vec![GirStmt::Select { scrut: None, arms: vec![arm0, arm1] }],
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
                scrut: None,
                arms: vec![
                    (
                        Some(
                            cmp(
                                loc("x", PrimType::I64),
                                const_expr(Value::I64(0)),
                                CmpOp::Gt,
                            )
                            .unwrap(),
                        ),
                        const_expr(Value::I64(1)),
                    ),
                    (
                        Some(
                            cmp(
                                loc("x", PrimType::I64),
                                const_expr(Value::I64(0)),
                                CmpOp::Lt,
                            )
                            .unwrap(),
                        ),
                        const_expr(Value::I64(-1)),
                    ),
                    (None, const_expr(Value::I64(0))),
                ],
            },
            typ: prim_type(PrimType::I64),
        };
        let kernel = GirKernel {
            sig: KernelSig {
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
                return_type: prim_type(PrimType::I64),
                has_tail_loop: false,
            }
            .into(),
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
        use netidx_value::Value;
        let is_null_check = GirExpr {
            op: GirOp::IsNull(Box::new(GirExpr {
                op: GirOp::Local(ArcStr::from("x")),
                typ: nullable_type(prim_type(PrimType::I64)),
            })),
            typ: prim_type(PrimType::Bool),
        };
        let chain = GirExpr {
            op: GirOp::IfChain {
                scrut: None,
                arms: vec![
                    (Some(is_null_check), const_expr(Value::I64(-1))),
                    (None, const_expr(Value::I64(7))),
                ],
            },
            typ: prim_type(PrimType::I64),
        };
        let kernel = GirKernel {
            sig: KernelSig {
                fn_name: ArcStr::from("nullable_dispatch"),
                params: vec![],
                fn_params: vec![],
                array_params: vec![],
                tuple_params: vec![],
                struct_params: vec![],
                variant_params: vec![],
                nullable_params: vec![crate::gir::NullableInput {
                    name: ArcStr::from("x"),
                    elem: prim_type(PrimType::I64),
                    bind_id: None,
                }],
                tail_call_slots: vec![crate::gir::TailCallSlot {
                    name: ArcStr::from("x"),
                    kind: crate::gir::TailCallSlotKind::Nullable,
                }],
                return_type: prim_type(PrimType::I64),
                has_tail_loop: false,
            }
            .into(),
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
        assert_eq!(unpack_u64_to_value(out, PrimType::I64), Value::I64(7));

        // Pass `Value::Null`: IsNull true → return -1.
        let arg = Value::Null;
        let args = [&arg as *const Value as u64];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_value(out, PrimType::I64), Value::I64(-1));
    }

    #[cfg(any())]
    fn wrapper_nullable_kernel_return() {
        use netidx_value::Value;
        let body = GirExpr {
            op: GirOp::IfChain {
                scrut: None,
                arms: vec![
                    (
                        Some(
                            cmp(
                                loc("n", PrimType::I64),
                                const_expr(Value::I64(0)),
                                CmpOp::Gt,
                            )
                            .unwrap(),
                        ),
                        arith(
                            loc("n", PrimType::I64),
                            const_expr(Value::I64(1)),
                            BinOp::Add,
                        )
                        .unwrap(),
                    ),
                    (
                        None,
                        GirExpr {
                            op: GirOp::ConstNull,
                            typ: null_type(),
                        },
                    ),
                ],
            },
            typ: nullable_type(prim_type(PrimType::I64)),
        };
        let kernel = GirKernel {
            sig: KernelSig {
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
                return_type: nullable_type(prim_type(PrimType::I64)),
                has_tail_loop: false,
            }
            .into(),
            body: vec![GirStmt::Return(body)],
        };
        let wrapped =
            compile_kernel_with_wrapper(&kernel).expect("compile wrapper");
        let f = unsafe { wrapped.fn_ptr() };

        // n=10: non-null branch → Value::I64(11)
        let args = [pack(&Value::I64(10))];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        let returned =
            unsafe { *Box::from_raw(out as *mut Value) };
        assert_eq!(returned, Value::I64(11));

        // n=-3: null branch → Value::Null
        let args = [pack(&Value::I64(-3))];
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
        let kernel = mandelbrot_iterate_kernel();
        let wrapped = compile_kernel_with_wrapper(&kernel)
            .expect("compile wrapper");
        let f = unsafe { wrapped.fn_ptr() };

        // c=1+0i, i=10 → 7
        let args = [
            pack(&Value::F64(0.0)),
            pack(&Value::F64(0.0)),
            pack(&Value::F64(1.0)),
            pack(&Value::F64(0.0)),
            pack(&Value::I64(10)),
        ];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_value(out, gir::scalar_prim(&kernel.return_type).unwrap()), Value::I64(7));

        // c=0+0i, i=20 → 0
        let args = [
            pack(&Value::F64(0.0)),
            pack(&Value::F64(0.0)),
            pack(&Value::F64(0.0)),
            pack(&Value::F64(0.0)),
            pack(&Value::I64(20)),
        ];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_value(out, gir::scalar_prim(&kernel.return_type).unwrap()), Value::I64(0));
    }

    #[test]
    fn jit_integer_overflow_wraps() {
        // i64::MAX + 1 should wrap to i64::MIN.
        let body = arith(
            loc("x", PrimType::I64),
            const_expr(Value::I64(1)),
            BinOp::Add,
        )
        .unwrap();
        let kernel = GirKernel {
            sig: KernelSig {
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
                return_type: prim_type(PrimType::I64),
                has_tail_loop: false,
            }
            .into(),
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
        use std::sync::Arc;
        // square(x: i64) -> i64 { x * x }
        let square = Arc::new(GirKernel {
            sig: KernelSig {
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
                return_type: prim_type(PrimType::I64),
                has_tail_loop: false,
            }
            .into(),
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
            typ: prim_type(PrimType::I64),
        };
        let body = arith(call_expr, const_expr(Value::I64(10)), BinOp::Add)
            .unwrap();
        let caller = Arc::new(GirKernel {
            sig: KernelSig {
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
                return_type: prim_type(PrimType::I64),
                has_tail_loop: false,
            }
            .into(),
            body: vec![GirStmt::Return(body)],
        });
        let mut callees = BTreeMap::new();
        callees.insert(ArcStr::from("square"), square);
        let mut jit = Jit::new().expect("Jit::new");
        let wrapped = compile_kernel_with_callees(&mut jit, &caller, &callees)
            .expect("compile_kernel_with_callees");
        let f = unsafe { wrapped.fn_ptr() };
        let args = [pack(&Value::I64(7))];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_value(out, gir::scalar_prim(&caller.return_type).unwrap()), Value::I64(59));
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
        use std::sync::Arc;
        let i64t = || prim_type(PrimType::I64);
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
            sig: KernelSig {
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
            }
            .into(),
            body: vec![GirStmt::Return(h_body)],
        });
        // caller(a, b, c) = h((a, b), c)
        let tuple_arg = GirExpr {
            op: GirOp::TupleNew {
                fields: vec![loc("a", PrimType::I64), loc("b", PrimType::I64)],
                elem_types: vec![i64t(), i64t()],
            },
            typ: tuple_type(vec![i64t(), i64t()]),
        };
        let call = GirExpr {
            op: GirOp::Call {
                fn_name: ArcStr::from("h"),
                args: vec![tuple_arg, loc("c", PrimType::I64)],
            },
            typ: i64t(),
        };
        let caller = Arc::new(GirKernel {
            sig: KernelSig {
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
            }
            .into(),
            body: vec![GirStmt::Return(call)],
        });
        let mut callees = BTreeMap::new();
        callees.insert(ArcStr::from("h"), h);
        let mut jit = Jit::new().expect("Jit::new");
        let wrapped = compile_kernel_with_callees(&mut jit, &caller, &callees)
            .expect("compile_kernel_with_callees");
        let f = unsafe { wrapped.fn_ptr() };
        let args = [
            pack(&Value::I64(10)),
            pack(&Value::I64(20)),
            pack(&Value::I64(5)),
        ];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_value(out, PrimType::I64), Value::I64(35));
    }

    /// #131-JIT: cross-kernel call RETURNING a value-shape (Nullable).
    /// `h(x) -> [i64, null]` returns via an IfChain that widens to
    /// Nullable; the caller forwards it. Exercises the `GirOp::Call`
    /// arm in `compile_value_expr` — the two-word (disc, payload)
    /// return decode.
    #[test]
    fn cross_kernel_call_nullable_return() {
        use std::sync::Arc;
        let nullable_i64 =
            || nullable_type(prim_type(PrimType::I64));
        // h(x: i64) -> [i64, null] = if x == 0 { null } else { x }
        let h_body = GirExpr {
            op: GirOp::IfChain {
                scrut: None,
                arms: vec![
                    (
                        Some(
                            cmp(
                                loc("x", PrimType::I64),
                                const_expr(Value::I64(0)),
                                CmpOp::Eq,
                            )
                            .unwrap(),
                        ),
                        GirExpr { op: GirOp::ConstNull, typ: null_type() },
                    ),
                    (None, loc("x", PrimType::I64)),
                ],
            },
            typ: nullable_i64(),
        };
        let h = Arc::new(GirKernel {
            sig: KernelSig {
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
            }
            .into(),
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
            sig: KernelSig {
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
            }
            .into(),
            body: vec![GirStmt::Return(call)],
        });
        let mut callees = BTreeMap::new();
        callees.insert(ArcStr::from("h"), h);
        let mut jit = Jit::new().expect("Jit::new");
        let wrapped = compile_kernel_with_callees(&mut jit, &caller, &callees)
            .expect("compile_kernel_with_callees");
        let f = unsafe { wrapped.fn_ptr() };
        let decode = |x: i64| -> netidx_value::Value {
            let args = [pack(&Value::I64(x))];
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
        use std::sync::Arc;
        let nullable_i64 =
            || nullable_type(prim_type(PrimType::I64));
        // h(m: [i64, null]) -> [i64, null] = m
        let h = Arc::new(GirKernel {
            sig: KernelSig {
                fn_name: ArcStr::from("h"),
                params: vec![],
                fn_params: vec![],
                array_params: vec![],
                tuple_params: vec![],
                struct_params: vec![],
                variant_params: vec![],
                nullable_params: vec![NullableInput {
                    name: ArcStr::from("m"),
                    elem: prim_type(PrimType::I64),
                    bind_id: None,
                }],
                string_params: vec![],
                value_params: vec![],
                tail_call_slots: vec![],
                return_type: nullable_i64(),
                has_tail_loop: false,
            }
            .into(),
            body: vec![GirStmt::Return(GirExpr {
                op: GirOp::Local(ArcStr::from("m")),
                typ: nullable_i64(),
            })],
        });
        // caller(x) -> [i64, null] = h(if x == 0 { null } else { x })
        let arg = GirExpr {
            op: GirOp::IfChain {
                scrut: None,
                arms: vec![
                    (
                        Some(
                            cmp(
                                loc("x", PrimType::I64),
                                const_expr(Value::I64(0)),
                                CmpOp::Eq,
                            )
                            .unwrap(),
                        ),
                        GirExpr { op: GirOp::ConstNull, typ: null_type() },
                    ),
                    (None, loc("x", PrimType::I64)),
                ],
            },
            typ: nullable_i64(),
        };
        let caller = Arc::new(GirKernel {
            sig: KernelSig {
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
            }
            .into(),
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
            let args = [pack(&Value::I64(x))];
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
        let nullable_i64 =
            || nullable_type(prim_type(PrimType::I64));
        let kernel = GirKernel {
            sig: KernelSig {
                fn_name: ArcStr::from("h"),
                params: vec![],
                fn_params: vec![],
                array_params: vec![],
                tuple_params: vec![],
                struct_params: vec![],
                variant_params: vec![],
                nullable_params: vec![NullableInput {
                    name: ArcStr::from("m"),
                    elem: prim_type(PrimType::I64),
                    bind_id: None,
                }],
                string_params: vec![],
                value_params: vec![],
                tail_call_slots: vec![],
                return_type: prim_type(PrimType::Bool),
                has_tail_loop: false,
            }
            .into(),
            body: vec![GirStmt::Return(GirExpr {
                op: GirOp::IsNull(Box::new(GirExpr {
                    op: GirOp::Local(ArcStr::from("m")),
                    typ: nullable_i64(),
                })),
                typ: prim_type(PrimType::Bool),
            })],
        };
        let wrapped = compile_kernel_with_wrapper(&kernel)
            .expect("compile_kernel_with_wrapper");
        let f = unsafe { wrapped.fn_ptr() };
        // Nullable param `m` arrives as its two `repr(u64)` Value words
        // (disc, payload) — the same packing `GirNode::update` does.
        let run = |v: netidx_value::Value| -> Value {
            let words: [u64; 2] = unsafe { std::mem::transmute(v) };
            let mut out = 0u64;
            unsafe { f(words.as_ptr(), &mut out) };
            unpack_u64_to_value(out, PrimType::Bool)
        };
        assert_eq!(run(netidx_value::Value::Null), Value::Bool(true));
        assert_eq!(run(netidx_value::Value::I64(7)), Value::Bool(false));
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
            sig: KernelSig {
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
                return_type: string_type(),
                has_tail_loop: false,
            }
            .into(),
            body: vec![GirStmt::Return(GirExpr {
                op: GirOp::Local(ArcStr::from("s")),
                typ: string_type(),
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
            sig: KernelSig {
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
                    typ: datetime_type(),
                    bind_id: None,
                }],
                tail_call_slots: vec![],
                return_type: datetime_type(),
                has_tail_loop: false,
            }
            .into(),
            body: vec![GirStmt::Return(GirExpr {
                op: GirOp::ValueArith {
                    op: BinOp::Add,
                    lhs: Box::new(GirExpr {
                        op: GirOp::Local(ArcStr::from("d")),
                        typ: datetime_type(),
                    }),
                    rhs: Box::new(GirExpr {
                        op: GirOp::Const(one_sec.clone()),
                        typ: duration_type(),
                    }),
                },
                typ: datetime_type(),
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
        use std::sync::Arc;
        // fib(n) = if n < 2 { n } else { fib(n-1) + fib(n-2) }
        let fib_call = |which: i64| GirExpr {
            op: GirOp::Call {
                fn_name: ArcStr::from("fib"),
                args: vec![arith(
                    loc("n", PrimType::I64),
                    const_expr(Value::I64(which)),
                    BinOp::Sub,
                )
                .unwrap()],
            },
            typ: prim_type(PrimType::I64),
        };
        let recursive_body =
            arith(fib_call(1), fib_call(2), BinOp::Add).unwrap();
        let arms = vec![
            SelectArm {
                cond: Some(
                    cmp(
                        loc("n", PrimType::I64),
                        const_expr(Value::I64(2)),
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
            sig: KernelSig {
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
                return_type: prim_type(PrimType::I64),
                has_tail_loop: false,
            }
            .into(),
            body: vec![GirStmt::Select { scrut: None, arms }],
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
        let args = [pack(&Value::I64(10))];
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_value(out, PrimType::I64), Value::I64(55));
        let mut out = 0u64;
        let args = [pack(&Value::I64(15))];
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_value(out, PrimType::I64), Value::I64(610));
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
        use std::sync::Arc;
        // leaf(x) = x + 1
        let leaf = Arc::new(GirKernel {
            sig: KernelSig {
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
                return_type: prim_type(PrimType::I64),
                has_tail_loop: false,
            }
            .into(),
            body: vec![GirStmt::Return(
                arith(
                    loc("x", PrimType::I64),
                    const_expr(Value::I64(1)),
                    BinOp::Add,
                )
                .unwrap(),
            )],
        });
        // middle(x) = leaf(x) * 2
        let middle = Arc::new(GirKernel {
            sig: KernelSig {
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
                return_type: prim_type(PrimType::I64),
                has_tail_loop: false,
            }
            .into(),
            body: vec![GirStmt::Return(
                arith(
                    GirExpr {
                        op: GirOp::Call {
                            fn_name: ArcStr::from("leaf"),
                            args: vec![loc("x", PrimType::I64)],
                        },
                        typ: prim_type(PrimType::I64),
                    },
                    const_expr(Value::I64(2)),
                    BinOp::Mul,
                )
                .unwrap(),
            )],
        });
        // outer(x) = middle(x) - 3
        let outer = Arc::new(GirKernel {
            sig: KernelSig {
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
                return_type: prim_type(PrimType::I64),
                has_tail_loop: false,
            }
            .into(),
            body: vec![GirStmt::Return(
                arith(
                    GirExpr {
                        op: GirOp::Call {
                            fn_name: ArcStr::from("middle"),
                            args: vec![loc("x", PrimType::I64)],
                        },
                        typ: prim_type(PrimType::I64),
                    },
                    const_expr(Value::I64(3)),
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
        let args = [pack(&Value::I64(10))];
        let mut out = 0u64;
        unsafe { f(args.as_ptr(), &mut out) };
        assert_eq!(unpack_u64_to_value(out, PrimType::I64), Value::I64(19));
    }

    /// Same callee Arc invoked via two separate parent kernels. The
    /// `by_kernel` cache should compile `square` exactly once and let
    /// both parents share it. Verifies both parents return the right
    /// answer (so the shared callee is correctly addressable from each).
    #[test]
    fn shared_module_callee_dedup() {
        use std::sync::Arc;
        let square = Arc::new(GirKernel {
            sig: KernelSig {
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
                return_type: prim_type(PrimType::I64),
                has_tail_loop: false,
            }
            .into(),
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
                typ: prim_type(PrimType::I64),
            };
            let body = arith(
                call_expr,
                const_expr(Value::I64(add_const)),
                BinOp::Add,
            )
            .unwrap();
            Arc::new(GirKernel {
                sig: KernelSig {
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
                    return_type: prim_type(PrimType::I64),
                    has_tail_loop: false,
                }
                .into(),
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
        let args = [pack(&Value::I64(5))];
        let (mut oa, mut ob) = (0u64, 0u64);
        unsafe {
            fa(args.as_ptr(), &mut oa);
            fb(args.as_ptr(), &mut ob);
        }
        assert_eq!(unpack_u64_to_value(oa, PrimType::I64), Value::I64(26));
        assert_eq!(unpack_u64_to_value(ob, PrimType::I64), Value::I64(125));
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
        // |x: i64| -> (i64, i64) (x, x + 1)
        // Body is just TupleNew of two scalars — no DynCall.
        let body = GirExpr {
            op: GirOp::TupleNew {
                fields: vec![
                    loc("x", PrimType::I64),
                    arith(
                        loc("x", PrimType::I64),
                        const_expr(Value::I64(1)),
                        BinOp::Add,
                    )
                    .unwrap(),
                ],
                elem_types: vec![
                    prim_type(PrimType::I64),
                    prim_type(PrimType::I64),
                ],
            },
            typ: tuple_type(vec![
                prim_type(PrimType::I64),
                prim_type(PrimType::I64),
            ]),
        };
        let kernel = GirKernel {
            sig: KernelSig {
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
                return_type: tuple_type(vec![
                    prim_type(PrimType::I64),
                    prim_type(PrimType::I64),
                ]),
                has_tail_loop: false,
            }
            .into(),
            body: vec![GirStmt::Return(body)],
        };
        let wrapped =
            compile_kernel_with_wrapper(&kernel).expect("compile wrapper");
        let f = unsafe { wrapped.fn_ptr() };
        let args = [pack(&Value::I64(21))];

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
        let body = GirExpr {
            op: GirOp::ConstStr(ArcStr::from("hi")),
            typ: string_type(),
        };
        let kernel = GirKernel {
            sig: KernelSig {
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
                return_type: string_type(),
                has_tail_loop: false,
            }
            .into(),
            body: vec![GirStmt::Return(body)],
        };
        let wrapped =
            compile_kernel_with_wrapper(&kernel).expect("compile wrapper");
        let f = unsafe { wrapped.fn_ptr() };
        let args = [pack(&Value::I64(0))];

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
