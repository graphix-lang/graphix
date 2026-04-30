//! Cranelift JIT backend for the kernel IR.
//!
//! Lowers a [`KirKernel`] to native machine code via Cranelift,
//! returning a function pointer that the runtime can call directly.
//! The companion of [`crate::kernel_ir::kir_to_rust_kernel`] (the AOT
//! Rust-source backend) — same fusion analysis, same IR, different
//! backend.
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
    BinOp, BoolOp, CmpOp, ConstVal, KirExpr, KirKernel, KirOp, KirStmt, PrimType,
    SelectArm,
};
use anyhow::{anyhow, Context as AnyContext, Result};
use arcstr::ArcStr;
use cranelift_codegen::{
    ir::{
        condcodes::{FloatCC, IntCC},
        types, AbiParam, Block, BlockArg, InstBuilder, MemFlags, Signature,
        Type as ClifType, Value as ClifValue,
    },
    settings::{self, Configurable},
    Context,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{default_libcall_names, FuncId, Linkage, Module};

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
        let builder = JITBuilder::with_isa(isa, default_libcall_names());
        let module = JITModule::new(builder);
        Ok(Self {
            module,
            builder_ctx: FunctionBuilderContext::new(),
            func_ctx: Context::new(),
            counter: 0,
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
}

unsafe impl Send for CompiledKernel {}
unsafe impl Sync for CompiledKernel {}

// ─── Public entry point ──────────────────────────────────────────

/// JIT-compile `kernel` and return a function pointer to the entry
/// point. The pointer's call signature matches the kernel's params /
/// return type — see the module docstring for the calling convention.
pub fn compile_kernel(jit: &mut JitCtx, kernel: &KirKernel) -> Result<CompiledKernel> {
    let (func_id, sig) = define_typed_kernel(jit, kernel)?;
    jit.module
        .finalize_definitions()
        .context("finalize_definitions")?;
    let fn_ptr = jit.module.get_finalized_function(func_id);
    Ok(CompiledKernel { func_id, fn_ptr, signature: sig })
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
) -> Result<(FuncId, Signature)> {
    let symbol = jit.next_symbol(&kernel.fn_name);
    let mut sig = Signature::new(jit.module.isa().default_call_conv());
    for p in &kernel.params {
        sig.params.push(AbiParam::new(prim_to_clif(p.prim)));
    }
    sig.returns.push(AbiParam::new(prim_to_clif(kernel.return_type)));

    let func_id = jit
        .module
        .declare_function(&symbol, Linkage::Local, &sig)
        .context("declare_function (typed)")?;
    jit.func_ctx.func.signature = sig.clone();
    jit.func_ctx.func.name =
        cranelift_codegen::ir::UserFuncName::user(0, func_id.as_u32());

    {
        let mut builder =
            FunctionBuilder::new(&mut jit.func_ctx.func, &mut jit.builder_ctx);
        compile_into_function(&mut builder, kernel)?;
        builder.finalize();
    }

    jit.module
        .define_function(func_id, &mut jit.func_ctx)
        .context("define_function (typed)")?;
    jit.module.clear_context(&mut jit.func_ctx);
    Ok((func_id, sig))
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
/// Owns its `JitCtx` so the mmap'd code stays valid for the wrapper's
/// lifetime. Cloning shares that state via `Arc` at the user level.
pub struct WrappedKernel {
    /// Type-erased entry point. Cast via transmute to the
    /// canonical `WrapperFn` signature for invocation.
    pub wrapper_fn_ptr: *const u8,
    /// Owns the JIT module so the code stays mapped. Order matters:
    /// the function pointer is only valid as long as `_ctx` is alive.
    _ctx: JitCtx,
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
pub fn compile_kernel_with_wrapper(kernel: &KirKernel) -> Result<WrappedKernel> {
    let mut ctx = JitCtx::new()?;
    let (typed_id, _) = define_typed_kernel(&mut ctx, kernel)?;
    let wrapper_id = define_wrapper(&mut ctx, kernel, typed_id)?;
    ctx.module
        .finalize_definitions()
        .context("finalize_definitions (wrapper)")?;
    let wrapper_fn_ptr = ctx.module.get_finalized_function(wrapper_id);
    Ok(WrappedKernel { wrapper_fn_ptr, _ctx: ctx })
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
        // CLIF type. Smaller-than-8-byte primitives just load fewer
        // bytes from the slot — the upper bytes are ignored.
        let mut typed_args = Vec::with_capacity(kernel.params.len());
        for (i, p) in kernel.params.iter().enumerate() {
            let cty = prim_to_clif(p.prim);
            let offset = (i as i32) * 8;
            let v = b.ins().load(cty, MemFlags::trusted(), args_ptr, offset);
            typed_args.push(v);
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

fn compile_into_function(b: &mut FunctionBuilder, kernel: &KirKernel) -> Result<()> {
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
    for (i, p) in kernel.params.iter().enumerate() {
        let cty = prim_to_clif(p.prim);
        let var = b.declare_var(cty);
        b.def_var(var, initial_vals[i]);
        env.bind(p.name.clone(), var, p.prim);
    }
    let param_count = env.locals.len();
    b.seal_block(entry);

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

    let lower = LowerCtx { loop_head, param_count };
    compile_body(b, &kernel.body, &mut env, &lower)?;

    if let Some(head) = loop_head {
        b.seal_block(head);
    }

    // After body compilation, every block has been sealed except
    // possibly some auxiliary blocks (select arms, if-chain merges).
    // FunctionBuilder requires all blocks be sealed before finalize;
    // we rely on the body lowering to seal as it goes.
    b.seal_all_blocks();
    Ok(())
}

/// Per-function lowering context: things that don't change across
/// statements within a single body.
struct LowerCtx {
    /// `Some(block)` when the kernel has a tail loop; TailCall jumps
    /// here. `None` for non-tail-recursive kernels.
    loop_head: Option<Block>,
    /// Number of formal parameters — `env.locals[..param_count]` are
    /// the params. Used by TailCall to know which Variables to assign.
    param_count: usize,
}

// ─── Env: name → Variable lookup ─────────────────────────────────

struct JitEnv {
    /// Each entry is `(name, var, prim)`. Lookups walk back-to-front
    /// for proper shadowing. Same shape as the interpreter env, just
    /// storing CLIF Variables instead of RegValues. Variables are
    /// allocated by [`FunctionBuilder::declare_var`]; we only carry
    /// them through the env so name lookup at use sites finds the
    /// right one.
    locals: Vec<(ArcStr, Variable, PrimType)>,
}

impl JitEnv {
    fn new() -> Self {
        Self { locals: Vec::with_capacity(8) }
    }

    fn bind(&mut self, name: ArcStr, var: Variable, prim: PrimType) {
        self.locals.push((name, var, prim));
    }

    fn lookup(&self, name: &str) -> Option<(Variable, PrimType)> {
        for (n, v, p) in self.locals.iter().rev() {
            if n.as_str() == name {
                return Some((*v, *p));
            }
        }
        None
    }

    fn mark(&self) -> usize {
        self.locals.len()
    }

    fn truncate(&mut self, n: usize) {
        self.locals.truncate(n);
    }
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
                let v = compile_expr(b, &l.value, env)?;
                let var = b.declare_var(prim_to_clif(l.value.typ));
                b.def_var(var, v);
                env.bind(l.local.clone(), var, l.value.typ);
            }
            KirStmt::Return(e) => {
                let v = compile_expr(b, e, env)?;
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
                    new_vals.push(compile_expr(b, a, env)?);
                }
                debug_assert_eq!(new_vals.len(), ctx.param_count);
                for (i, v) in new_vals.iter().enumerate() {
                    let (var, _) = (env.locals[i].1, env.locals[i].2);
                    b.def_var(var, *v);
                }
                // Drop locals beyond params so the next iteration
                // starts with a clean lexical state.
                env.truncate(ctx.param_count);
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
                let cv = compile_expr(b, cond, env)?;
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
) -> Result<ClifValue> {
    match &e.op {
        KirOp::Const(c) => Ok(compile_const(b, *c)),
        KirOp::Local(name) => {
            let (var, _) = env.lookup(name).ok_or_else(|| {
                anyhow!("KIR malformed: undefined local `{name}`")
            })?;
            Ok(b.use_var(var))
        }
        KirOp::Bin { op, lhs, rhs } => {
            let l = compile_expr(b, lhs, env)?;
            let r = compile_expr(b, rhs, env)?;
            Ok(compile_bin(b, *op, lhs.typ, l, r))
        }
        KirOp::Cmp { op, lhs, rhs } => {
            let l = compile_expr(b, lhs, env)?;
            let r = compile_expr(b, rhs, env)?;
            Ok(compile_cmp(b, *op, lhs.typ, l, r))
        }
        KirOp::BoolBin { op, lhs, rhs } => {
            // We use eager (non-short-circuit) evaluation here for
            // simplicity — KIR only ever emits BoolBin over pure
            // expressions, so eager eval has no observable effect.
            // The interpreter does short-circuit; the AOT-emitted
            // Rust `&&`/`||` short-circuit. No correctness diff for
            // pure code; if we ever fuse expressions with side-effects
            // we'll need to revisit.
            let l = compile_expr(b, lhs, env)?;
            let r = compile_expr(b, rhs, env)?;
            Ok(match op {
                BoolOp::And => b.ins().band(l, r),
                BoolOp::Or => b.ins().bor(l, r),
            })
        }
        KirOp::Not(inner) => {
            let v = compile_expr(b, inner, env)?;
            // Bool is I8 in CLIF; XOR with 1 flips the low bit.
            let one = b.ins().iconst(types::I8, 1);
            Ok(b.ins().bxor(v, one))
        }
        KirOp::Cast { inner, target } => {
            let v = compile_expr(b, inner, env)?;
            Ok(compile_cast(b, v, inner.typ, *target))
        }
        KirOp::Call { fn_name: _, args: _ } => Err(anyhow!(
            "KirOp::Call not supported in JIT v1 (cross-kernel calls \
             land in M4 with the kernel registry)"
        )),
        KirOp::Block { lets, tail } => {
            let mark = env.mark();
            for l in lets {
                let v = compile_expr(b, &l.value, env)?;
                let var = b.declare_var(prim_to_clif(l.value.typ));
                b.def_var(var, v);
                env.bind(l.local.clone(), var, l.value.typ);
            }
            let result = compile_expr(b, tail, env)?;
            env.truncate(mark);
            Ok(result)
        }
        KirOp::IfChain { arms } => compile_ifchain(b, arms, e.typ, env),
    }
}

fn compile_ifchain(
    b: &mut FunctionBuilder,
    arms: &[(Option<KirExpr>, KirExpr)],
    result_type: PrimType,
    env: &mut JitEnv,
) -> Result<ClifValue> {
    if arms.is_empty() {
        return Err(anyhow!("KIR malformed: empty if-chain"));
    }
    let cty = prim_to_clif(result_type);
    // Merge block holds the result via a block parameter.
    let merge = b.create_block();
    b.append_block_param(merge, cty);

    for (i, (cond, body)) in arms.iter().enumerate() {
        let is_last = i == arms.len() - 1;
        let body_block = b.create_block();
        let next_block: Option<Block> = match cond {
            None => {
                b.ins().jump(body_block, &[]);
                None
            }
            Some(c) => {
                let cv = compile_expr(b, c, env)?;
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
        let v = compile_expr(b, body, env)?;
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
        ConstVal, Input, KirExpr, KirKernel, KirOp, KirStmt, Let, PrimType,
        SelectArm,
    };

    fn input(name: &str, prim: PrimType) -> Input {
        Input {
            name: ArcStr::from(name),
            prim,
            bind_id: None,
            rust_name: name.to_string(),
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
            return_type: PrimType::I64,
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
            typ: PrimType::F64,
        };
        let kernel = KirKernel {
            fn_name: ArcStr::from("scaled"),
            params: vec![input("a", PrimType::F64), input("b", PrimType::F64)],
            return_type: PrimType::F64,
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
            return_type: PrimType::Bool,
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
            return_type: PrimType::F64,
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
            return_type: PrimType::I64,
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
            return_type: PrimType::I64,
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
            typ: PrimType::I64,
        };
        let kernel = KirKernel {
            fn_name: ArcStr::from("sign"),
            params: vec![input("x", PrimType::I64)],
            return_type: PrimType::I64,
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
        assert_eq!(unpack_u64_to_reg(out, kernel.return_type), RegValue::I64(7));

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
        assert_eq!(unpack_u64_to_reg(out, kernel.return_type), RegValue::I64(0));
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
            return_type: PrimType::I64,
            has_tail_loop: false,
            body: vec![KirStmt::Return(body)],
        };
        let (_ctx, p) = jit(&kernel);
        let f: extern "C" fn(i64) -> i64 = unsafe { std::mem::transmute(p) };
        assert_eq!(f(i64::MAX), i64::MIN);
    }
}
