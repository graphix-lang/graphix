//! The per-context JIT pipeline: [`JitCtx`], the kernel
//! declare/define/wrap entry points, [`WrappedKernel`]/[`Jit`],
//! and the wrapper-seam value packing ([`pack_value_to_u64`]).

use crate::{
    Node, Rt, UserEvent,
    env::Env,
    expr::ExprId,
    fusion::{
        CalleeBody, LambdaCallInfo,
        emit_helpers::all_helpers,
        kernel_abi::{self, AbiKind, AbiParamKind, KernelKey, KernelSig, PrimType},
        lowering::BuiltinCallSiteInfo,
    },
    profile::{self, Phase},
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Context as AnyContext, Result, anyhow, bail};
use compact_str::{CompactString, format_compact};
use cranelift_codegen::{
    Context,
    control::ControlPlane,
    ir::{
        AbiParam, FuncRef, InstBuilder, MemFlags, Signature, TrapCode, UserFuncName,
        Value as ClifValue, types,
    },
    settings::{self, Configurable},
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{ArenaMemoryProvider, JITBuilder, JITModule};
use cranelift_module::{
    DataId, FuncId, Linkage, Module, ModuleError, ModuleReloc, ModuleRelocTarget,
    default_libcall_names,
};
use netidx_value::Value;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{cell::RefCell, collections::BTreeMap, mem::ManuallyDrop, sync::LazyLock};
use triomphe::Arc;

use super::{
    body::{BodyRole, BodySource, BodySpec, NodeBodyEmitter},
    lower::{EmittedBody, HelperFuncIds, SiteLayout, compile_into_function},
    record::{BodyRecord, EmitConst, RecordKind, RecordReloc, RelocTarget, SymbolTable},
    scalar::prim_to_clif,
};

/// The module's code arena is full: the caller retires the module and
/// retries in a fresh one.
#[derive(Debug)]
pub(crate) struct ArenaExhausted;

impl std::fmt::Display for ArenaExhausted {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("the JIT code arena is exhausted")
    }
}

impl std::error::Error for ArenaExhausted {}

/// The code and data of one JIT module, owned jointly by the [`JitCtx`]
/// compiling into it and every [`WrappedKernel`] it produced. The ctx
/// parks its module here when it drops; the last owner frees it.
struct CodeOwner(Mutex<Option<JITModule>>);

// SAFETY: the parked module is touched only by the last owner's drop;
// until then the `JitCtx` owns it, and it moves between threads as the
// `Jit` does.
unsafe impl Send for CodeOwner {}
unsafe impl Sync for CodeOwner {}

impl Drop for CodeOwner {
    fn drop(&mut self) {
        if let Some(module) = self.0.get_mut().take() {
            // SAFETY: the last owner is gone, so no kernel of this module
            // can run or be entered and no compile is in flight.
            unsafe { module.free_memory() }
        }
    }
}

/// A function's machine code before it is defined.
struct Compiled {
    bytes: Box<[u8]>,
    align: u64,
    relocs: Vec<ModuleReloc>,
}

/// Owns the Cranelift JIT module plus reusable per-function builder
/// contexts. One `JitCtx` can compile many kernels; its code lives
/// until the ctx and every kernel compiled into it have dropped.
pub(crate) struct JitCtx {
    module: ManuallyDrop<JITModule>,
    code: Arc<CodeOwner>,
    builder_ctx: FunctionBuilderContext,
    func_ctx: Context,
    /// Symbol suffix; one graphix name can occur in several fused lambdas.
    symbol_counter: u32,
    /// FuncIds for the `emit_helpers::*` runtime helpers, declared once.
    helper_ids: HelperFuncIds,
    /// The helpers by their ids, for naming a relocation's target.
    helper_names: BTreeMap<FuncId, &'static str>,
    /// Where a body's constant symbols resolve; the module's lookup fn
    /// reads it at finalization.
    symbols: SymbolTable,
}

impl Drop for JitCtx {
    fn drop(&mut self) {
        // SAFETY: `module` is not used again.
        let module = unsafe { ManuallyDrop::take(&mut self.module) };
        *self.code.0.lock() = Some(module);
    }
}

/// The host ISA every module compiles for.
fn host_isa() -> Result<cranelift_codegen::isa::OwnedTargetIsa> {
    let mut flag_builder = settings::builder();
    flag_builder.set("opt_level", "speed").context("set opt_level")?;
    flag_builder
        .set("use_colocated_libcalls", "false")
        .context("set use_colocated_libcalls")?;
    // cranelift-jit requires PIC off.
    flag_builder.set("is_pic", "false").context("set is_pic")?;
    let isa_builder = cranelift_native::builder()
        .map_err(|e| anyhow!("cranelift_native::builder failed: {e}"))?;
    isa_builder.finish(settings::Flags::new(flag_builder)).context("isa_builder.finish")
}

/// The target and flags every module compiles for; an image records it
/// so code written by another host is refused.
pub fn isa_description() -> String {
    static DESC: LazyLock<String> = LazyLock::new(|| match host_isa() {
        Ok(isa) => {
            let isa_flags: Vec<String> =
                isa.isa_flags().iter().map(|v| format!("{}={v}", v.name)).collect();
            format!("{} {} {}", isa.triple(), isa.flags(), isa_flags.join(","))
        }
        Err(e) => format!("no host isa: {e:#}"),
    });
    DESC.clone()
}

impl JitCtx {
    fn new() -> Result<Self> {
        let _profile = profile::phase(Phase::JitInit);
        let mut builder = JITBuilder::with_isa(host_isa()?, default_libcall_names());
        // One contiguous reservation: colocated (Linkage::Local) calls use a
        // ±2GiB PC-relative relocation and finalize panics if two functions
        // land further apart. `GRAPHIX_JIT_ARENA` (bytes) overrides the size.
        const JIT_ARENA_RESERVE: usize = 256 * 1024 * 1024;
        static ARENA_SIZE: LazyLock<usize> =
            LazyLock::new(|| match std::env::var("GRAPHIX_JIT_ARENA") {
                Ok(v) => v.parse().unwrap_or(JIT_ARENA_RESERVE),
                Err(_) => JIT_ARENA_RESERVE,
            });
        builder.memory_provider(Box::new(
            ArenaMemoryProvider::new_with_size(*ARENA_SIZE)
                .map_err(|e| anyhow!("jit arena reservation failed: {e}"))?,
        ));
        // Helpers resolve by pointer under the registry's symbol name, never
        // through the process symbol table.
        for h in all_helpers() {
            builder.symbol(h.name, h.ptr);
        }
        let symbols: SymbolTable = Arc::new(Mutex::new(AHashMap::new()));
        let table = symbols.clone();
        builder.symbol_lookup_fn(Box::new(move |name| {
            table.lock().get(name).map(|p| *p as *const u8)
        }));
        let mut module = JITModule::new(builder);
        let helper_ids = HelperFuncIds::new(&mut module)?;
        let helper_names = helper_ids.ids.iter().map(|(n, id)| (*id, *n)).collect();
        Ok(Self {
            module: ManuallyDrop::new(module),
            code: Arc::new(CodeOwner(Mutex::new(None))),
            builder_ctx: FunctionBuilderContext::new(),
            func_ctx: Context::new(),
            symbol_counter: 0,
            helper_ids,
            helper_names,
            symbols,
        })
    }

    /// Empty the function contexts for the next function; a failed
    /// build may have left them mid-function.
    fn reset_func(&mut self) {
        self.module.clear_context(&mut self.func_ctx);
        self.builder_ctx = FunctionBuilderContext::new();
    }

    /// Compile the function in `func_ctx`, which is named `id`, to bytes
    /// and relocations; nothing is defined yet.
    fn compile(&mut self, id: FuncId) -> Result<Compiled> {
        self.func_ctx
            .compile(self.module.isa(), &mut ControlPlane::default())
            .map_err(|e| anyhow!("compile: {}", e.inner))?;
        let code = self.func_ctx.compiled_code().expect("compiled above");
        let bytes: Box<[u8]> = code.code_buffer().into();
        let align = code.buffer.alignment as u64;
        let relocs = code
            .buffer
            .relocs()
            .iter()
            .map(|r| ModuleReloc::from_mach_reloc(r, &self.func_ctx.func, id))
            .collect();
        Ok(Compiled { bytes, align, relocs })
    }

    /// Define `id` from compiled bytes; a full arena is [`ArenaExhausted`].
    fn define(&mut self, id: FuncId, c: &Compiled) -> Result<()> {
        match self.module.define_function_bytes(id, c.align, &c.bytes, &c.relocs) {
            Ok(()) => Ok(()),
            Err(ModuleError::Allocation { .. }) => Err(ArenaExhausted.into()),
            Err(e) => Err(e.into()),
        }
    }

    /// Define `fid` as a body that traps, for a function abandoned after
    /// declaration: the module must not carry an undefined Local symbol a
    /// defined function relocates to into the next
    /// `finalize_definitions`. Never executed.
    fn define_stub(&mut self, fid: FuncId, sig: &Signature) -> Result<()> {
        self.reset_func();
        self.func_ctx.func.signature = sig.clone();
        self.func_ctx.func.name = UserFuncName::user(0, fid.as_u32());
        {
            let mut b =
                FunctionBuilder::new(&mut self.func_ctx.func, &mut self.builder_ctx);
            let entry = b.create_block();
            b.append_block_params_for_function_params(entry);
            b.switch_to_block(entry);
            b.seal_block(entry);
            b.ins().trap(TrapCode::user(1).expect("valid user trap code"));
            b.finalize();
        }
        let _backend_profile = profile::phase(Phase::BackendStub);
        let r = self
            .module
            .define_function(fid, &mut self.func_ctx)
            .context("define_function (abandon stub)");
        self.reset_func();
        r.map(|_| ())
    }

    /// [`Self::define_stub`], logging a failure: the caller is already
    /// returning the error that abandoned `fid`.
    fn stub_abandoned(&mut self, fid: FuncId, sig: &Signature, what: &str) {
        if let Err(e) = self.define_stub(fid, sig) {
            log::warn!("stub definition for abandoned `{what}` failed: {e:?}");
        }
    }

    /// The `(args, out)` signature of a thunk or wrapper.
    fn trampoline_signature(&self) -> Signature {
        let ptr_ty = self.module.target_config().pointer_type();
        let mut sig = Signature::new(self.module.isa().default_call_conv());
        sig.params.push(AbiParam::new(ptr_ty));
        sig.params.push(AbiParam::new(ptr_ty));
        sig
    }

    /// The constant symbols `{symbol}.c{i}` of a body whose definition
    /// failed: nothing will relocate to them.
    fn forget_consts(&self, symbol: &str, n: usize) {
        let mut table = self.symbols.lock();
        for i in 0..n {
            table.remove(format_compact!("{symbol}.c{i}").as_str());
        }
    }

    /// Name every relocation target symbolically: a helper, a recorded
    /// callee (entered in `callees`), the owner, the thunk, a libcall
    /// or one of the body's constants.
    fn record_relocs(
        &self,
        relocs: &[ModuleReloc],
        owner: FuncId,
        thunk: Option<FuncId>,
        consts: &[EmitConst],
        records: &BTreeMap<FuncId, Arc<BodyRecord>>,
        callees: &mut Vec<Arc<BodyRecord>>,
    ) -> Result<Vec<RecordReloc>> {
        relocs
            .iter()
            .map(|r| {
                let target = match r.name {
                    ModuleRelocTarget::User { namespace: 0, index } => {
                        let fid = FuncId::from_u32(index);
                        if fid == owner {
                            RelocTarget::Owner
                        } else if Some(fid) == thunk {
                            RelocTarget::Thunk
                        } else if let Some(name) = self.helper_names.get(&fid) {
                            RelocTarget::Helper((*name).into())
                        } else if let Some(rec) = records.get(&fid) {
                            let i = match callees.iter().position(|c| Arc::ptr_eq(c, rec))
                            {
                                Some(i) => i,
                                None => {
                                    callees.push(rec.clone());
                                    callees.len() - 1
                                }
                            };
                            RelocTarget::Callee(i as u32)
                        } else {
                            bail!("a relocation to an unrecorded function")
                        }
                    }
                    ModuleRelocTarget::User { namespace: 1, index } => {
                        let did = DataId::from_u32(index);
                        match consts.iter().position(|c| c.data == did) {
                            Some(i) => RelocTarget::Const(i as u32),
                            None => bail!("a relocation to an unrecorded constant"),
                        }
                    }
                    ModuleRelocTarget::LibCall(lc) => RelocTarget::LibCall(lc),
                    ModuleRelocTarget::User { .. }
                    | ModuleRelocTarget::KnownSymbol(_)
                    | ModuleRelocTarget::FunctionOffset(..) => {
                        bail!("an unsupported relocation target {:?}", r.name)
                    }
                };
                Ok(RecordReloc {
                    offset: r.offset,
                    kind: r.kind,
                    target,
                    addend: r.addend,
                })
            })
            .collect()
    }

    fn next_symbol(&mut self, fn_name: &str) -> CompactString {
        self.symbol_counter += 1;
        format_compact!("{fn_name}__kir_{}", self.symbol_counter)
    }
}

/// Push the kernel's parameter `AbiParam`s onto `sig` in the order
/// [`KernelSig::abi_params`] gives: the context words, then a
/// `(disc, payload)` pair per parameter.
fn push_abi_params(sig: &mut Signature, kernel: &KernelSig) {
    for _ in 0..kernel_abi::CTX_WIRE_SLOTS {
        sig.params.push(AbiParam::new(types::I64));
    }
    for d in kernel.abi_params() {
        sig.params.push(AbiParam::new(types::I64)); // disc
        let payload_ty = match d.kind {
            AbiParamKind::Scalar(p) => prim_to_clif(p),
            _ => types::I64,
        };
        sig.params.push(AbiParam::new(payload_ty));
    }
}

/// Push the return `AbiParam`s onto `sig`: every kernel returns the
/// `(disc, payload)` Value pair. Errors on a bare-`Null` return.
fn push_abi_returns(sig: &mut Signature, kernel: &KernelSig) -> Result<()> {
    if matches!(kernel_abi::abi_kind(&kernel.return_type), Some(AbiKind::Null) | None) {
        bail!(
            "kernel returns the bare Null type; should have widened to Nullable<T> at \
             construction"
        )
    }
    sig.returns.push(AbiParam::new(types::I64)); // disc
    sig.returns.push(AbiParam::new(types::I64)); // payload
    Ok(())
}

/// A kernel's own signature.
fn kernel_signature(jit: &JitCtx, kernel: &KernelSig) -> Result<Signature> {
    let mut sig = Signature::new(jit.module.isa().default_call_conv());
    push_abi_params(&mut sig, kernel);
    push_abi_returns(&mut sig, kernel)?;
    Ok(sig)
}

/// Print the CLIF to stderr when `GRAPHIX_DUMP_CLIF` is set.
fn maybe_dump_clif(func: &cranelift_codegen::ir::Function, label: &str) {
    if crate::dbgenv::graphix_dump_clif() {
        eprintln!(";; clif {label}\n{}", func.display());
    }
}

/// A compiled kernel behind the uniform [`WrapperFn`] convention:
/// `args` points at the context words then a `(disc, payload)` pair
/// per parameter, `out` receives the result's `(disc, payload)` pair.
/// [`pack_value_to_u64`] does the Rust-side packing.
pub struct WrappedKernel {
    /// Cast through [`Self::fn_ptr`].
    wrapper_fn_ptr: *const u8,
    /// The code `wrapper_fn_ptr` and everything it calls live in.
    _code: Arc<CodeOwner>,
    /// The wrapper's record, and through it the region's bodies: what
    /// an image writes for this kernel.
    pub(crate) wrapper: Arc<BodyRecord>,
    /// Per-instance state words the root body claimed. The runtime
    /// `FusedKernel` passes a zeroed buffer of this size in wire slot 1.
    pub(crate) state_words: usize,
    /// The root body's per-slot state-table anchors: each word holds a
    /// `Box<Vec<u64>>` chain owned by `graphix_slot_state_table` and
    /// freed by `FusedKernel`'s `Drop`.
    pub(crate) slot_table_words: Vec<kernel_abi::SiteAnchor>,
    /// The body's own per-call-site block layout. A caller supplies the
    /// block; for a region parent the runtime `FusedKernel` supplies it from
    /// its own per-instance storage.
    pub(crate) own_site: Option<SiteLayout>,
    /// Per-activation block-tree roots living in the parent's state
    /// buffer; `FusedKernel` frees and resets them with its own site block.
    pub(crate) state_self_blocks: Vec<kernel_abi::SelfBlock>,
}

// SAFETY: `wrapper_fn_ptr` points into `code`, which it keeps alive; the
// code is immutable once finalized.
unsafe impl Send for WrappedKernel {}
unsafe impl Sync for WrappedKernel {}

/// The uniform Rust-side signature the wrapper presents.
pub(crate) type WrapperFn = unsafe extern "C" fn(args: *const u64, out: *mut u64);

impl WrappedKernel {
    /// The wrapper entry point.
    ///
    /// # Safety
    /// Callers pass the `(args, out)` layout of the kernel's ABI.
    pub(crate) unsafe fn fn_ptr(&self) -> WrapperFn {
        unsafe { std::mem::transmute(self.wrapper_fn_ptr) }
    }
}

/// A compiled kernel body's identity in the [`Jit`]'s cache: a body
/// bakes its sibling kernels' FuncIds, which depend on the region's
/// ordered kernel list, so only identical layouts may share a
/// compilation. A body with no sibling sites uses layout 0.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct CacheKey {
    kernel: KernelKey,
    layout: u32,
}

/// The per-`ExecCtx` JIT module. Kernels call each other with direct
/// CLIF calls, so they share one module that lives as long as the
/// `ExecCtx` or its longest-lived kernel.
pub struct Jit {
    /// Boxed: cranelift's `Context` is ~5KB and this rides every
    /// `async fn` that moves a `GXConfig`.
    ctx: Box<JitCtx>,
    /// Lambda kernel bodies; the entry holds the `Arc` so the key's
    /// address cannot be reused by a later allocation.
    by_kernel: BTreeMap<CacheKey, CachedKernel>,
    /// Region layout → id, from 1; 0 is the layout-independent id.
    layout_ids: BTreeMap<SmallVec<[KernelKey; 8]>, u32>,
    /// Every defined kernel body's record, by its function; a caller's
    /// relocations name callees through it. Lives with the module: the
    /// code refers to the record's constants by address.
    records: BTreeMap<FuncId, Arc<BodyRecord>>,
    /// Records installed from an image, by the record's address.
    loaded: BTreeMap<usize, FuncId>,
}

// SAFETY: the module is used only through `&mut Jit`, and its raw
// pointers are into its own arena.
unsafe impl Send for Jit {}

impl Jit {
    /// Errs if cranelift cannot target the host ISA.
    pub fn new() -> Result<Self> {
        Ok(Self {
            ctx: Box::new(JitCtx::new()?),
            by_kernel: BTreeMap::new(),
            layout_ids: BTreeMap::new(),
            records: BTreeMap::new(),
            loaded: BTreeMap::new(),
        })
    }

    /// Install a record's function, its thunk and its callees (once per
    /// record) and return the function's id.
    fn load(&mut self, rec: &Arc<BodyRecord>) -> Result<FuncId> {
        let key = Arc::as_ptr(rec) as usize;
        if let Some(id) = self.loaded.get(&key) {
            return Ok(*id);
        }
        let callees: SmallVec<[FuncId; 8]> =
            rec.callees.iter().map(|c| self.load(c)).collect::<Result<_>>()?;
        let (id, sig) = self.declare_record(rec)?;
        let thunk_id = match rec.thunk() {
            Some(t) => Some(self.declare_record(t)?.0),
            None => None,
        };
        // The thunk first: a failure after it leaves a defined thunk
        // relocating to its body, which the stub below then defines.
        if let (Some(t), Some(tid)) = (rec.thunk(), thunk_id) {
            self.define_record(t, tid, None, id, &[])?;
        }
        if let Err(e) = self.define_record(rec, id, thunk_id, id, &callees) {
            if thunk_id.is_some() {
                self.ctx.stub_abandoned(id, &sig, &rec.label);
            }
            return Err(e);
        }
        self.loaded.insert(key, id);
        self.records.insert(id, rec.clone());
        Ok(id)
    }

    fn declare_record(&mut self, rec: &BodyRecord) -> Result<(FuncId, Signature)> {
        let sig = match rec.kind {
            RecordKind::Kernel { .. } => kernel_signature(&self.ctx, &rec.kernel)?,
            RecordKind::Thunk | RecordKind::Wrapper => self.ctx.trampoline_signature(),
        };
        let symbol = self.ctx.next_symbol(&rec.label);
        let id = self.ctx.module.declare_function(&symbol, Linkage::Local, &sig)?;
        Ok((id, sig))
    }

    /// Define `id` from the record's bytes: its constants become imports
    /// resolving to their pointers, its relocations name the module's
    /// ids. `owner` is the body a thunk serves (itself for a body).
    fn define_record(
        &mut self,
        rec: &BodyRecord,
        id: FuncId,
        thunk: Option<FuncId>,
        owner: FuncId,
        callees: &[FuncId],
    ) -> Result<()> {
        let symbol = self
            .ctx
            .module
            .declarations()
            .get_function_decl(id)
            .name
            .clone()
            .unwrap_or_default();
        let r = self.define_record_named(rec, &symbol, id, thunk, owner, callees);
        if r.is_err() {
            self.ctx.forget_consts(&symbol, rec.consts().len());
        }
        r
    }

    fn define_record_named(
        &mut self,
        rec: &BodyRecord,
        symbol: &str,
        id: FuncId,
        thunk: Option<FuncId>,
        owner: FuncId,
        callees: &[FuncId],
    ) -> Result<()> {
        let mut consts: SmallVec<[DataId; 8]> = SmallVec::new();
        for (i, c) in rec.consts().iter().enumerate() {
            let name = format_compact!("{symbol}.c{i}");
            let did =
                self.ctx.module.declare_data(&name, Linkage::Import, false, false)?;
            self.ctx.symbols.lock().insert(name, c.pointer(&rec.kernel));
            consts.push(did);
        }
        let mut relocs = Vec::with_capacity(rec.relocs.len());
        for r in &rec.relocs {
            let name = match &r.target {
                RelocTarget::Helper(n) => {
                    let fid = self
                        .ctx
                        .helper_ids
                        .ids
                        .get(n.as_str())
                        .ok_or_else(|| anyhow!("unknown helper {n}"))?;
                    ModuleRelocTarget::user(0, fid.as_u32())
                }
                RelocTarget::Callee(i) => {
                    let fid = callees
                        .get(*i as usize)
                        .ok_or_else(|| anyhow!("a relocation to a missing callee"))?;
                    ModuleRelocTarget::user(0, fid.as_u32())
                }
                RelocTarget::Owner => ModuleRelocTarget::user(0, owner.as_u32()),
                RelocTarget::Thunk => {
                    let tid = thunk.ok_or_else(|| anyhow!("a relocation to no thunk"))?;
                    ModuleRelocTarget::user(0, tid.as_u32())
                }
                RelocTarget::LibCall(lc) => ModuleRelocTarget::LibCall(*lc),
                RelocTarget::Const(i) => {
                    let did = consts
                        .get(*i as usize)
                        .ok_or_else(|| anyhow!("a relocation to a missing constant"))?;
                    ModuleRelocTarget::user(1, did.as_u32())
                }
            };
            relocs.push(ModuleReloc {
                offset: r.offset,
                kind: r.kind,
                name,
                addend: r.addend,
            });
        }
        let c = Compiled { bytes: rec.bytes.clone(), align: rec.align, relocs };
        self.ctx.define(id, &c)
    }

    /// The wrapped kernel a region restored from an image dispatches:
    /// its wrapper record installed and finalized, over the layout data
    /// the image carried.
    pub(crate) fn load_wrapped(
        &mut self,
        wrapper: &Arc<BodyRecord>,
        state_words: usize,
        slot_table_words: Vec<kernel_abi::SiteAnchor>,
        own_site: Option<SiteLayout>,
        state_self_blocks: Vec<kernel_abi::SelfBlock>,
    ) -> Result<WrappedKernel> {
        let id = self.load(wrapper)?;
        let _finalize = profile::phase(Phase::Finalize);
        self.ctx.module.finalize_definitions().context("finalize_definitions (image)")?;
        let wrapper_fn_ptr = self.ctx.module.get_finalized_function(id);
        Ok(WrappedKernel {
            wrapper_fn_ptr,
            _code: self.ctx.code.clone(),
            wrapper: wrapper.clone(),
            state_words,
            slot_table_words,
            state_self_blocks,
            own_site,
        })
    }

    fn intern_layout(&mut self, layout: SmallVec<[KernelKey; 8]>) -> u32 {
        let next = self.layout_ids.len() as u32 + 1;
        *self.layout_ids.entry(layout).or_insert(next)
    }
}

struct CachedKernel {
    func_id: FuncId,
    signature: Signature,
    /// See [`WrappedKernel::slot_table_words`]; filled in phase 2.
    slot_table_words: Vec<kernel_abi::SiteAnchor>,
    /// See [`WrappedKernel::state_self_blocks`]; filled in phase 2.
    state_self_blocks: Vec<kernel_abi::SelfBlock>,
    /// Filled when the body is defined. `None` at a caller's emission
    /// is a self-call, which roots a per-activation block tree.
    site_layout: Option<SiteLayout>,
    /// Holds the Arc so its pointer cannot be reused by a later allocation.
    _kernel: Arc<KernelSig>,
    /// See [`WrappedKernel::state_words`].
    state_words: usize,
}

/// Compile `kernel` and its callees by walking their Nodes' `emit_clif`.
/// The parent emits from `root`; each callee emits from its
/// `callee_bodies` entry with its own lambda and builtin sites. A callee
/// without a recorded body fails the whole region.
pub(crate) fn compile_kernel_with_callees_direct<R: Rt, E: UserEvent>(
    jit: &mut Jit,
    kernel: &Arc<KernelSig>,
    callees: &[(KernelKey, Arc<KernelSig>)],
    root: &Node<R, E>,
    apply_sites: &nohash::IntMap<ExprId, BuiltinCallSiteInfo>,
    lambda_sites: &nohash::IntMap<ExprId, LambdaCallInfo>,
    callee_bodies: &BTreeMap<KernelKey, CalleeBody<'_, R, E>>,
    type_env: &Env,
) -> Result<WrappedKernel> {
    let parent = NodeBodyEmitter { root, return_type: &kernel.return_type };
    let parent = BodySource {
        spec: BodySpec {
            builtin_apply_sites: apply_sites,
            lambda_call_sites: lambda_sites,
            type_env,
            role: BodyRole::Parent,
        },
        hook: &parent,
    };
    let callee_emitters: LPooled<Vec<(KernelKey, NodeBodyEmitter<R, E>, BodySpec)>> =
        callees
            .iter()
            .filter_map(|(key, k)| {
                let cb = callee_bodies.get(key)?;
                Some((
                    *key,
                    NodeBodyEmitter { root: cb.body, return_type: &k.return_type },
                    BodySpec {
                        builtin_apply_sites: &cb.apply_sites,
                        lambda_call_sites: &cb.sites,
                        type_env,
                        role: BodyRole::Callee(cb.self_call.as_ref()),
                    },
                ))
            })
            .collect();
    let emitters: LPooled<AHashMap<KernelKey, BodySource>> = callee_emitters
        .iter()
        .map(|(key, em, spec)| (*key, BodySource { spec: *spec, hook: em }))
        .collect();
    compile_region(jit, kernel, &parent, callees, &emitters)
}

/// The FuncIds a region attempt declared fresh, for the failure path.
#[derive(Default)]
struct Fresh {
    /// Fresh callee cache entries, in declaration order.
    callees: SmallVec<[(CacheKey, Arc<KernelSig>); 8]>,
    /// The fresh callee entries whose bodies were defined.
    defined: SmallVec<[CacheKey; 8]>,
    /// The parent's body, until it is defined.
    parent: Option<(FuncId, Signature)>,
}

fn compile_region(
    jit: &mut Jit,
    kernel: &Arc<KernelSig>,
    parent: &BodySource,
    callees: &[(KernelKey, Arc<KernelSig>)],
    emitters: &AHashMap<KernelKey, BodySource>,
) -> Result<WrappedKernel> {
    let mut build_profile = profile::phase(Phase::JitBuild);
    let mut fresh = Fresh::default();
    let r = compile_region_inner(jit, kernel, parent, callees, emitters, &mut fresh);
    if r.is_err() {
        profile::failed(&mut build_profile);
        // Evict the fresh entries (a stale one would hand out an undefined
        // FuncId) and trap-stub every declared-but-undefined body: the next
        // `finalize_definitions` panics on an undefined Local symbol a
        // defined function relocates to.
        for (key, k) in fresh.callees.iter() {
            if let Some(entry) = jit.by_kernel.remove(key)
                && !fresh.defined.contains(key)
            {
                jit.ctx.stub_abandoned(entry.func_id, &entry.signature, &k.fn_name);
            }
        }
        if let Some((fid, sig)) = fresh.parent.take() {
            jit.ctx.stub_abandoned(fid, &sig, &kernel.fn_name);
        }
    }
    r
}

fn compile_region_inner(
    jit: &mut Jit,
    kernel: &Arc<KernelSig>,
    parent: &BodySource,
    callees: &[(KernelKey, Arc<KernelSig>)],
    emitters: &AHashMap<KernelKey, BodySource>,
    fresh: &mut Fresh,
) -> Result<WrappedKernel> {
    // Phase 1: declare every kernel in the closure. A callee body with no
    // sibling sites keys on layout 0; the parent is fresh per attempt and
    // never cached.
    let layout_id = jit.intern_layout(callees.iter().map(|(key, _)| *key).collect());
    let layout_of = |key: KernelKey| -> u32 {
        let ext_sites = emitters.get(&key).is_some_and(|e| {
            e.spec
                .lambda_call_sites
                .values()
                .any(|info| kernel_abi::kernel_key(&info.kernel) != key)
        });
        if ext_sites { layout_id } else { 0 }
    };
    // Insertion order fixes the funcref numbering; a pointer-ordered map
    // makes it ASLR-dependent.
    let mut funcids: LPooled<Vec<(KernelKey, (FuncId, Signature))>> = LPooled::take();
    // Seeded only from this region's own cache keys: another layout
    // variant of a body may have a different SiteLayout, and sizing
    // blocks from it is an out-of-bounds write.
    let mut callee_layouts: LPooled<AHashMap<KernelKey, SiteLayout>> = LPooled::take();
    let parent_key = kernel_abi::kernel_key(kernel);
    let parent_sig = kernel_signature(&jit.ctx, kernel)?;
    let symbol = jit.ctx.next_symbol(&kernel.fn_name);
    let parent_fid = jit
        .ctx
        .module
        .declare_function(&symbol, Linkage::Local, &parent_sig)
        .context("declare_function (region parent)")?;
    fresh.parent = Some((parent_fid, parent_sig.clone()));
    funcids.push((parent_key, (parent_fid, parent_sig)));
    for (key, k) in callees {
        let key = CacheKey { kernel: *key, layout: layout_of(*key) };
        let entry = match jit.by_kernel.get(&key) {
            Some(e) => {
                if let Some(l) = e.site_layout.as_ref() {
                    callee_layouts.insert(key.kernel, l.clone());
                }
                (e.func_id, e.signature.clone())
            }
            None => {
                let sig = kernel_signature(&jit.ctx, k)?;
                let symbol = jit.ctx.next_symbol(&k.fn_name);
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
                        state_self_blocks: Vec::new(),
                        state_words: 0,
                        slot_table_words: Vec::new(),
                        site_layout: None,
                    },
                );
                fresh.callees.push((key, k.clone()));
                (fid, sig)
            }
        };
        funcids.push((key.kernel, entry));
    }
    // Phase 2: define the fresh callee bodies in topological order over
    // the static call edges, callees first, so a caller can read its
    // callees' `SiteLayout`s (the only one missing at definition is a
    // self-call's), then the parent.
    let order = def_order(&fresh.callees, emitters);
    for (key, k) in order.iter() {
        let body = emitters.get(&key.kernel).ok_or_else(|| {
            anyhow!(
                "no body emitter recorded for kernel `{}` — discovery must record \
                 every callee body",
                k.fn_name
            )
        })?;
        let db = define_kernel_body(
            &mut jit.ctx,
            k,
            &funcids,
            body,
            &callee_layouts,
            &jit.records,
        )?;
        fresh.defined.push(*key);
        let fid = funcids.iter().find(|(p, _)| *p == key.kernel).expect("declared").1.0;
        jit.records.insert(fid, db.record);
        callee_layouts.insert(key.kernel, db.site_layout.clone());
        if let Some(cached) = jit.by_kernel.get_mut(key) {
            cached.state_words = db.state_words;
            cached.slot_table_words = db.slot_table_words;
            cached.state_self_blocks = db.state_self_blocks;
            cached.site_layout = Some(db.site_layout);
        }
    }
    let db = define_kernel_body(
        &mut jit.ctx,
        kernel,
        &funcids,
        parent,
        &callee_layouts,
        &jit.records,
    )?;
    fresh.parent = None;
    jit.records.insert(parent_fid, db.record);
    // Phase 3: the parent's wrapper, then finalize.
    let (wrapper_id, wrapper) =
        define_wrapper(&mut jit.ctx, kernel, parent_fid, &jit.records)?;
    let finalize_profile = profile::phase(Phase::Finalize);
    jit.ctx
        .module
        .finalize_definitions()
        .context("finalize_definitions (per-context jit)")?;
    drop(finalize_profile);
    Ok(WrappedKernel {
        wrapper_fn_ptr: jit.ctx.module.get_finalized_function(wrapper_id),
        _code: jit.ctx.code.clone(),
        wrapper,
        state_words: db.state_words,
        slot_table_words: db.slot_table_words,
        state_self_blocks: db.state_self_blocks,
        own_site: Some(db.site_layout),
    })
}

/// The fresh callees in definition order: a depth-first postorder over
/// the static call edges between them, rooted in declaration order.
fn def_order(
    fresh: &[(CacheKey, Arc<KernelSig>)],
    emitters: &AHashMap<KernelKey, BodySource>,
) -> LPooled<Vec<(CacheKey, Arc<KernelSig>)>> {
    let pos = |k: KernelKey| fresh.iter().position(|(c, _)| c.kernel == k);
    let edges_of = |k: KernelKey| -> SmallVec<[usize; 8]> {
        let mut out: SmallVec<[usize; 8]> = emitters
            .get(&k)
            .map(|e| {
                e.spec
                    .lambda_call_sites
                    .values()
                    .map(|info| kernel_abi::kernel_key(&info.kernel))
                    .filter(|q| *q != k)
                    .filter_map(pos)
                    .collect()
            })
            .unwrap_or_default();
        out.sort_unstable();
        out.dedup();
        out
    };
    let mut done: LPooled<AHashSet<usize>> = LPooled::take();
    let mut order: LPooled<Vec<(CacheKey, Arc<KernelSig>)>> = LPooled::take();
    let mut stack: LPooled<Vec<(usize, SmallVec<[usize; 8]>, usize)>> = LPooled::take();
    for root in 0..fresh.len() {
        if !done.insert(root) {
            continue;
        }
        stack.push((root, edges_of(fresh[root].0.kernel), 0));
        while let Some((i, es, next)) = stack.pop() {
            match es.get(next).copied() {
                Some(q) => {
                    stack.push((i, es, next + 1));
                    if done.insert(q) {
                        stack.push((q, edges_of(fresh[q].0.kernel), 0));
                    }
                }
                None => order.push(fresh[i].clone()),
            }
        }
    }
    order
}

/// What defining one kernel body produced — stored onto the kernel's
/// `by_kernel` cache entry (the fields mirror the entry's).
struct DefinedBody {
    record: Arc<BodyRecord>,
    state_words: usize,
    slot_table_words: Vec<kernel_abi::SiteAnchor>,
    state_self_blocks: Vec<kernel_abi::SelfBlock>,
    site_layout: SiteLayout,
}

/// Compile `kernel`'s body and define it on its pre-declared `FuncId`.
/// `funcids` must hold the kernel itself and every callee its lambda
/// call sites reference. Nothing is defined until everything compiled,
/// and the spill thunk is defined before the body: on an error the
/// body is undefined, and a defined thunk relocating to it is left for
/// the caller's stub.
fn define_kernel_body(
    jit: &mut JitCtx,
    kernel: &Arc<KernelSig>,
    funcids: &[(KernelKey, (FuncId, Signature))],
    body_emitter: &BodySource,
    callee_layouts: &AHashMap<KernelKey, SiteLayout>,
    records: &BTreeMap<FuncId, Arc<BodyRecord>>,
) -> Result<DefinedBody> {
    let self_key = kernel_abi::kernel_key(kernel);
    let (func_id, sig) =
        funcids.iter().find(|(p, _)| *p == self_key).map(|(_, e)| e.clone()).ok_or_else(
            || {
                anyhow!(
                    "define_kernel_body: missing FuncId for kernel `{}` (phase-1 declare \
                     must have populated `funcids` first)",
                    kernel.fn_name
                )
            },
        )?;
    let symbol = jit.module.declarations().get_function_decl(func_id).name.clone();
    let symbol = symbol.unwrap_or_default();
    let consts: RefCell<Vec<EmitConst>> = RefCell::new(Vec::new());
    let body =
        KernelBody { kernel, func_id, sig: &sig, symbol: &symbol, consts: &consts };
    let r = define_kernel_body_inner(
        jit,
        &body,
        funcids,
        body_emitter,
        callee_layouts,
        records,
    );
    if r.is_err() {
        jit.forget_consts(&symbol, consts.borrow().len());
    }
    r
}

/// The body [`define_kernel_body`] is defining.
struct KernelBody<'a> {
    kernel: &'a Arc<KernelSig>,
    func_id: FuncId,
    sig: &'a Signature,
    symbol: &'a str,
    consts: &'a RefCell<Vec<EmitConst>>,
}

fn define_kernel_body_inner(
    jit: &mut JitCtx,
    body: &KernelBody,
    funcids: &[(KernelKey, (FuncId, Signature))],
    body_emitter: &BodySource,
    callee_layouts: &AHashMap<KernelKey, SiteLayout>,
    records: &BTreeMap<FuncId, Arc<BodyRecord>>,
) -> Result<DefinedBody> {
    let KernelBody { kernel, func_id, sig, symbol, consts } = *body;
    let mut clif_profile = profile::phase(Phase::Clif);
    let self_key = kernel_abi::kernel_key(kernel);
    let thunk_label = format_compact!("{}__spill", kernel.fn_name);
    let self_thunk_id = match body_emitter.spec.self_call() {
        Some(_) => {
            let symbol = jit.next_symbol(&thunk_label);
            let tsig = jit.trampoline_signature();
            Some(
                jit.module
                    .declare_function(&symbol, Linkage::Local, &tsig)
                    .context("declare_function (spill thunk)")?,
            )
        }
        None => None,
    };
    jit.reset_func();
    jit.func_ctx.func.signature = sig.clone();
    jit.func_ctx.func.name = UserFuncName::user(0, func_id.as_u32());
    let EmittedBody { state_words, slot_table_words, state_self_blocks, site_layout } = {
        // Callee FuncRefs are declared before the FunctionBuilder borrows
        // `func_ctx.func`. The set is the body's lambda sites plus its
        // self-call, keyed by kernel identity.
        let mut callee_keys: LPooled<AHashSet<KernelKey>> = body_emitter
            .spec
            .lambda_call_sites
            .values()
            .map(|info| kernel_abi::kernel_key(&info.kernel))
            .filter(|key| *key != self_key)
            .collect();
        if let Some((_, info)) = body_emitter.spec.self_call() {
            callee_keys.insert(kernel_abi::kernel_key(&info.kernel));
        }
        // Import in `funcids` order so the funcref numbering is deterministic.
        let mut callee_refs: BTreeMap<KernelKey, FuncRef> = BTreeMap::new();
        for (key, (fid, _)) in funcids {
            if callee_keys.contains(key) {
                let fref = jit.module.declare_func_in_func(*fid, &mut jit.func_ctx.func);
                callee_refs.insert(*key, fref);
            }
        }
        if callee_refs.len() != callee_keys.len() {
            bail!(
                "define_kernel_body: kernel `{}` calls a kernel with no entry in funcids",
                kernel.fn_name
            );
        }
        let self_thunk = self_thunk_id
            .map(|tid| jit.module.declare_func_in_func(tid, &mut jit.func_ctx.func));
        let module = RefCell::new(&mut *jit.module);
        let mut builder =
            FunctionBuilder::new(&mut jit.func_ctx.func, &mut jit.builder_ctx);
        let emitted = compile_into_function(
            &mut builder,
            kernel,
            &callee_refs,
            self_thunk,
            &jit.helper_ids,
            consts,
            &module,
            &jit.symbols,
            symbol,
            body_emitter,
            callee_layouts,
        );
        if emitted.is_err() {
            profile::failed(&mut clif_profile);
        }
        let emitted = emitted?;
        builder.finalize();
        maybe_dump_clif(&jit.func_ctx.func, &kernel.fn_name);
        emitted
    };
    drop(clif_profile);
    let backend_profile = profile::phase(Phase::BackendBody);
    let compiled = jit.compile(func_id).context("shared body")?;
    drop(backend_profile);
    let mut callees = Vec::new();
    let relocs = jit.record_relocs(
        &compiled.relocs,
        func_id,
        self_thunk_id,
        &consts.borrow(),
        records,
        &mut callees,
    )?;
    let thunk = match self_thunk_id {
        Some(tid) => {
            let _backend_profile = profile::phase(Phase::BackendSpill);
            let t =
                build_trampoline(jit, tid, func_id, sig, false).context("spill thunk")?;
            let mut none = Vec::new();
            let relocs = jit.record_relocs(
                &t.relocs,
                func_id,
                None,
                &[],
                &BTreeMap::new(),
                &mut none,
            )?;
            jit.define(tid, &t)?;
            Some(Arc::new(BodyRecord {
                kind: RecordKind::Thunk,
                label: thunk_label.as_str().into(),
                bytes: t.bytes,
                align: t.align,
                relocs,
                callees: Vec::new(),
                kernel: kernel.clone(),
            }))
        }
        None => None,
    };
    jit.define(func_id, &compiled)?;
    let record = Arc::new(BodyRecord {
        kind: RecordKind::Kernel {
            consts: consts.take().into_iter().map(|c| c.recipe).collect(),
            thunk,
        },
        label: kernel.fn_name.clone(),
        bytes: compiled.bytes,
        align: compiled.align,
        relocs,
        callees,
        kernel: kernel.clone(),
    });
    if crate::dbgenv::graphix_dbg_kernels() {
        eprintln!(
            "KERNEL DEFINED {}: state_words={} site_words={} self_blocks={}",
            kernel.fn_name,
            state_words,
            site_layout.words,
            site_layout.self_blocks.len()
        );
    }
    jit.reset_func();
    Ok(DefinedBody {
        record,
        state_words,
        slot_table_words,
        state_self_blocks,
        site_layout,
    })
}

/// Compile the `(args, out)` trampoline `id` into `target`: load each
/// of `target_sig`'s params from `args` at an 8-byte stride (the wire
/// layout of [`KernelSig::abi_params`]), call it, and store its two
/// result words to `out`. A wrapper also bumps the harness's
/// invocation counter in debug builds.
fn build_trampoline(
    jit: &mut JitCtx,
    id: FuncId,
    target: FuncId,
    target_sig: &Signature,
    wrapper: bool,
) -> Result<Compiled> {
    jit.reset_func();
    jit.func_ctx.func.signature = jit.trampoline_signature();
    jit.func_ctx.func.name = UserFuncName::user(0, id.as_u32());
    let target_ref = jit.module.declare_func_in_func(target, &mut jit.func_ctx.func);
    #[cfg(debug_assertions)]
    let record_ref = match wrapper {
        true => {
            let fid = jit
                .helper_ids
                .ids
                .get("graphix_record_jit_invocation")
                .copied()
                .ok_or_else(|| anyhow!("missing graphix_record_jit_invocation FuncId"))?;
            Some(jit.module.declare_func_in_func(fid, &mut jit.func_ctx.func))
        }
        false => None,
    };
    {
        let mut b = FunctionBuilder::new(&mut jit.func_ctx.func, &mut jit.builder_ctx);
        let entry = b.create_block();
        b.append_block_params_for_function_params(entry);
        b.switch_to_block(entry);
        b.seal_block(entry);
        // The test harness's `jit` mode reads this counter to verify the
        // JIT ran.
        #[cfg(debug_assertions)]
        if let Some(r) = record_ref {
            b.ins().call(r, &[]);
        }
        let args = b.block_params(entry)[0];
        let out = b.block_params(entry)[1];
        // Loading a scalar payload at its narrow CLIF type is sound because
        // the packer stores the sign/zero-extended form.
        let vals: SmallVec<[ClifValue; 16]> = target_sig
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                b.ins().load(p.value_type, MemFlags::trusted(), args, (8 * i) as i32)
            })
            .collect();
        let call = b.ins().call(target_ref, &vals);
        let results: SmallVec<[ClifValue; 2]> =
            b.inst_results(call).iter().copied().collect();
        for (i, r) in results.iter().enumerate() {
            b.ins().store(MemFlags::trusted(), *r, out, (8 * i) as i32);
        }
        b.ins().return_(&[]);
        b.finalize();
    }
    maybe_dump_clif(&jit.func_ctx.func, if wrapper { "wrapper" } else { "spill thunk" });
    jit.compile(id)
}

/// Declare and define the region's `(args, out)` wrapper around its
/// parent body `typed_func_id`.
fn define_wrapper(
    jit: &mut JitCtx,
    kernel: &Arc<KernelSig>,
    typed_func_id: FuncId,
    records: &BTreeMap<FuncId, Arc<BodyRecord>>,
) -> Result<(FuncId, Arc<BodyRecord>)> {
    let label = format_compact!("{}_wrap", kernel.fn_name);
    let symbol = jit.next_symbol(&label);
    let sig = jit.trampoline_signature();
    let wrapper_id = jit
        .module
        .declare_function(&symbol, Linkage::Local, &sig)
        .context("declare_function (wrapper)")?;
    let _backend_profile = profile::phase(Phase::BackendWrapper);
    let kernel_sig = kernel_signature(jit, kernel)?;
    let t = build_trampoline(jit, wrapper_id, typed_func_id, &kernel_sig, true)
        .context("wrapper")?;
    jit.reset_func();
    let mut callees = Vec::new();
    let relocs =
        jit.record_relocs(&t.relocs, wrapper_id, None, &[], records, &mut callees)?;
    jit.define(wrapper_id, &t)?;
    Ok((
        wrapper_id,
        Arc::new(BodyRecord {
            kind: RecordKind::Wrapper,
            label: label.as_str().into(),
            bytes: t.bytes,
            align: t.align,
            relocs,
            callees,
            kernel: kernel.clone(),
        }),
    ))
}

/// Pack a scalar [`Value`] into a u64 slot as `prim`: signed ints
/// sign-extend, unsigned zero-extend, floats keep their bits. `None`
/// when `v` is not a scalar of `prim`'s shape; the caller substitutes
/// the tainted placeholder. `Z32`/`Z64`/`V32`/`V64` pack as their
/// fixed-width prim.
pub fn pack_value_to_u64(v: &Value, prim: PrimType) -> Option<u64> {
    if kernel_abi::scalar_prim_of_value(v) != Some(prim) {
        return None;
    }
    Some(match *v {
        Value::I8(x) => x as i64 as u64,
        Value::I16(x) => x as i64 as u64,
        Value::I32(x) | Value::Z32(x) => x as i64 as u64,
        Value::I64(x) | Value::Z64(x) => x as u64,
        Value::U8(x) => x as u64,
        Value::U16(x) => x as u64,
        Value::U32(x) | Value::V32(x) => x as u64,
        Value::U64(x) | Value::V64(x) => x,
        Value::F32(x) => x.to_bits() as u64,
        Value::F64(x) => x.to_bits(),
        Value::Bool(b) => b as u64,
        _ => return None,
    })
}
