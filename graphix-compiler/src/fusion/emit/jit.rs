//! The per-context JIT pipeline: [`Jit`] emits a region's bodies
//! against its own id table ([`Names`]), the backend compiles each to a
//! [`BodyRecord`], and the region's wrapper record installs into the
//! current module [`Generation`], cold and warm alike; [`WrappedKernel`]
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
        AbiParam, ExtFuncData, ExternalName, FuncRef, Function, GlobalValue,
        GlobalValueData, InstBuilder, MemFlags, Signature, UserExternalName,
        UserFuncName, Value as ClifValue, immediates::Imm64, types,
    },
    isa::{OwnedTargetIsa, TargetIsa},
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

/// The module's code arena is full: the generation retires and the
/// install retries in a fresh one.
#[derive(Debug)]
pub(crate) struct ArenaExhausted;

impl std::fmt::Display for ArenaExhausted {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("the JIT code arena is exhausted")
    }
}

impl std::error::Error for ArenaExhausted {}

/// The code and data of one JIT module, owned jointly by the
/// [`Generation`] installing into it and every [`WrappedKernel`] it
/// produced. The generation parks its module here when it drops; the
/// last owner frees it.
struct CodeOwner(Mutex<Option<JITModule>>);

// SAFETY: the parked module is touched only by the last owner's drop;
// until then the `Generation` owns it, and it moves between threads as
// the `Jit` does.
unsafe impl Send for CodeOwner {}
unsafe impl Sync for CodeOwner {}

impl Drop for CodeOwner {
    fn drop(&mut self) {
        if let Some(module) = self.0.get_mut().take() {
            // SAFETY: the last owner is gone, so no kernel of this module
            // can run or be entered and no install is in flight.
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

/// The host ISA every module compiles for.
fn host_isa() -> Result<OwnedTargetIsa> {
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

/// The `(args, out)` signature of a thunk or wrapper.
fn trampoline_signature(isa: &dyn TargetIsa) -> Signature {
    let ptr_ty = isa.pointer_type();
    let mut sig = Signature::new(isa.default_call_conv());
    sig.params.push(AbiParam::new(ptr_ty));
    sig.params.push(AbiParam::new(ptr_ty));
    sig
}

/// What a body's CLIF names (kernel bodies, thunks, wrappers, the
/// runtime helpers and constants) by ids of this table alone. Nothing
/// named here is ever defined: a record names its targets symbolically,
/// and installing it mints the module's ids.
#[derive(Default)]
pub(super) struct Names {
    /// Each function's signature and whether calls to it are colocated.
    funcs: Vec<(Signature, bool)>,
    data: u32,
}

impl Names {
    fn func(&mut self, sig: &Signature, colocated: bool) -> FuncId {
        self.funcs.push((sig.clone(), colocated));
        FuncId::from_u32(self.funcs.len() as u32 - 1)
    }

    /// A function the module defines: a body, a thunk or a wrapper.
    fn local(&mut self, sig: &Signature) -> FuncId {
        self.func(sig, true)
    }

    pub(super) fn data(&mut self) -> DataId {
        self.data += 1;
        DataId::from_u32(self.data - 1)
    }

    /// `id` as a callee of `func`, the body being built.
    pub(super) fn import_func(&self, id: FuncId, func: &mut Function) -> FuncRef {
        let (sig, colocated) = &self.funcs[id.as_u32() as usize];
        let signature = func.import_signature(sig.clone());
        let name = func.declare_imported_user_function(UserExternalName {
            namespace: 0,
            index: id.as_u32(),
        });
        func.import_function(ExtFuncData {
            name: ExternalName::user(name),
            signature,
            colocated: *colocated,
            patchable: false,
        })
    }

    /// Constant `id`'s address in `func`: an imported symbol, resolved
    /// at install to the pointer its recipe names.
    pub(super) fn import_data(&self, id: DataId, func: &mut Function) -> GlobalValue {
        let name = func.declare_imported_user_function(UserExternalName {
            namespace: 1,
            index: id.as_u32(),
        });
        func.create_global_value(GlobalValueData::Symbol {
            name: ExternalName::user(name),
            offset: Imm64::new(0),
            colocated: false,
            tls: false,
        })
    }
}

/// The emission side of the JIT, which outlives module generations: the
/// ISA, the ids bodies are emitted against, and the builder scratch.
struct Emitter {
    isa: OwnedTargetIsa,
    names: Names,
    /// The runtime helpers' ids in `names`, declared once.
    helpers: HelperFuncIds,
    /// The helpers by their ids, for naming a relocation's target.
    helper_names: BTreeMap<FuncId, &'static str>,
    builder_ctx: FunctionBuilderContext,
    func_ctx: Context,
}

impl Emitter {
    fn new() -> Result<Self> {
        let isa = host_isa()?;
        let mut names = Names::default();
        let helpers = HelperFuncIds::new(isa.default_call_conv(), |_, sig| {
            Ok(names.func(sig, false))
        })?;
        let helper_names = helpers.ids.iter().map(|(n, id)| (*id, *n)).collect();
        Ok(Self {
            isa,
            names,
            helpers,
            helper_names,
            builder_ctx: FunctionBuilderContext::new(),
            func_ctx: Context::new(),
        })
    }

    /// Empty the function contexts for the next function; a failed
    /// build may have left them mid-function.
    fn reset_func(&mut self) {
        self.func_ctx.clear();
        self.func_ctx.func.signature.call_conv = self.isa.default_call_conv();
        self.builder_ctx = FunctionBuilderContext::new();
    }

    /// Compile the function in `func_ctx`, which is named `id`, to bytes
    /// and relocations.
    fn compile(&mut self, id: FuncId) -> Result<Compiled> {
        self.func_ctx
            .compile(&*self.isa, &mut ControlPlane::default())
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
}

/// One JIT module, where records install. A generation an install
/// failed in is never finalized again: it retires whole, and its code
/// is freed when its last kernel drops.
struct Generation {
    module: ManuallyDrop<JITModule>,
    code: Arc<CodeOwner>,
    /// The runtime helpers' ids in the module, declared once.
    helpers: HelperFuncIds,
    /// Where a constant's symbol resolves; the module's lookup fn reads
    /// it at finalization.
    symbols: SymbolTable,
    /// Symbol suffix; one graphix name can occur in several records.
    symbol_counter: u32,
    /// The records installed here, by the record's address. The entry
    /// holds the record: the code refers to its constants by address,
    /// and a freed record's address can be another's.
    loaded: BTreeMap<usize, (FuncId, Arc<BodyRecord>)>,
}

impl Drop for Generation {
    fn drop(&mut self) {
        // SAFETY: `module` is not used again.
        let module = unsafe { ManuallyDrop::take(&mut self.module) };
        *self.code.0.lock() = Some(module);
    }
}

impl Generation {
    fn new(isa: &OwnedTargetIsa) -> Result<Self> {
        let _profile = profile::phase(Phase::JitInit);
        let mut builder = JITBuilder::with_isa(isa.clone(), default_libcall_names());
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
        let helpers = HelperFuncIds::new(isa.default_call_conv(), |name, sig| {
            module
                .declare_function(name, Linkage::Import, sig)
                .with_context(|| format!("declare_function for helper `{name}`"))
        })?;
        Ok(Self {
            module: ManuallyDrop::new(module),
            code: Arc::new(CodeOwner(Mutex::new(None))),
            helpers,
            symbols,
            symbol_counter: 0,
            loaded: BTreeMap::new(),
        })
    }

    fn next_symbol(&mut self, fn_name: &str) -> CompactString {
        self.symbol_counter += 1;
        format_compact!("{fn_name}__kir_{}", self.symbol_counter)
    }

    /// Install `wrapper`'s record tree, finalize, and return its entry.
    fn install(&mut self, wrapper: &Arc<BodyRecord>) -> Result<*const u8> {
        let id = self.load(wrapper)?;
        let _finalize = profile::phase(Phase::Finalize);
        self.module.finalize_definitions().context("finalize_definitions")?;
        Ok(self.module.get_finalized_function(id))
    }

    /// Install a record's function, its thunk and its callees (once per
    /// record) and return the function's id.
    fn load(&mut self, rec: &Arc<BodyRecord>) -> Result<FuncId> {
        let key = Arc::as_ptr(rec) as usize;
        if let Some((id, _)) = self.loaded.get(&key) {
            return Ok(*id);
        }
        let callees: SmallVec<[FuncId; 8]> =
            rec.callees.iter().map(|c| self.load(c)).collect::<Result<_>>()?;
        let id = self.declare_record(rec)?;
        let thunk_id = match rec.thunk() {
            Some(t) => Some(self.declare_record(t)?),
            None => None,
        };
        if let (Some(t), Some(tid)) = (rec.thunk(), thunk_id) {
            self.define_record(t, tid, None, id, &[])?;
        }
        self.define_record(rec, id, thunk_id, id, &callees)?;
        self.loaded.insert(key, (id, rec.clone()));
        Ok(id)
    }

    fn declare_record(&mut self, rec: &BodyRecord) -> Result<FuncId> {
        let isa = self.module.isa();
        let sig = match rec.kind {
            RecordKind::Kernel { .. } => kernel_signature(isa, &rec.kernel)?,
            RecordKind::Thunk | RecordKind::Wrapper => trampoline_signature(isa),
        };
        let symbol = self.next_symbol(&rec.label);
        Ok(self.module.declare_function(&symbol, Linkage::Local, &sig)?)
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
            .module
            .declarations()
            .get_function_decl(id)
            .name
            .clone()
            .unwrap_or_default();
        let mut consts: SmallVec<[DataId; 8]> = SmallVec::new();
        for (i, c) in rec.consts().iter().enumerate() {
            let name = format_compact!("{symbol}.c{i}");
            let did = self.module.declare_data(&name, Linkage::Import, false, false)?;
            self.symbols.lock().insert(name, c.pointer(&rec.kernel));
            consts.push(did);
        }
        let mut relocs = Vec::with_capacity(rec.relocs.len());
        for r in &rec.relocs {
            let name = match &r.target {
                RelocTarget::Helper(n) => {
                    let fid = self
                        .helpers
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
        match self.module.define_function_bytes(id, rec.align, &rec.bytes, &relocs) {
            Ok(()) => Ok(()),
            Err(ModuleError::Allocation { .. }) => Err(ArenaExhausted.into()),
            Err(e) => Err(e.into()),
        }
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
fn kernel_signature(isa: &dyn TargetIsa, kernel: &KernelSig) -> Result<Signature> {
    let mut sig = Signature::new(isa.default_call_conv());
    push_abi_params(&mut sig, kernel);
    push_abi_returns(&mut sig, kernel)?;
    Ok(sig)
}

/// Print the CLIF to stderr when `GRAPHIX_DUMP_CLIF` is set.
fn maybe_dump_clif(func: &Function, label: &str) {
    if crate::dbgenv::graphix_dump_clif() {
        eprintln!(";; clif {label}\n{}", func.display());
    }
}

/// A compiled kernel behind the uniform [`WrapperFn`] convention:
/// `args` points at the context words then a `(disc, payload)` pair
/// per parameter, `out` receives the result's `(disc, payload)` pair.
/// [`pack_value_to_u64`] does the Rust-side packing.
#[derive(Clone)]
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

/// The per-`ExecCtx` JIT. Kernels call each other with direct CLIF
/// calls, so a region's records install into one module generation,
/// which lives as long as the `ExecCtx` or its longest-lived kernel.
pub struct Jit {
    /// Boxed: cranelift's `Context` is ~5KB and this rides every
    /// `async fn` that moves a `GXConfig`.
    emitter: Box<Emitter>,
    generation: Generation,
    /// Generations retired so far.
    retired: usize,
    /// Lambda kernel bodies; the entry holds the `Arc` so the key's
    /// address cannot be reused by a later allocation.
    by_kernel: BTreeMap<CacheKey, CachedKernel>,
    /// Region layout → id, from 1; 0 is the layout-independent id.
    layout_ids: BTreeMap<SmallVec<[KernelKey; 8]>, u32>,
    /// Every compiled kernel body's record, by its id in [`Names`]; a
    /// caller's relocations name callees through it.
    records: BTreeMap<FuncId, Arc<BodyRecord>>,
}

// SAFETY: the module is used only through `&mut Jit`, and its raw
// pointers are into its own arena.
unsafe impl Send for Jit {}

impl Jit {
    /// Errs if cranelift cannot target the host ISA.
    pub fn new() -> Result<Self> {
        let emitter = Box::new(Emitter::new()?);
        let generation = Generation::new(&emitter.isa)?;
        Ok(Self {
            emitter,
            generation,
            retired: 0,
            by_kernel: BTreeMap::new(),
            layout_ids: BTreeMap::new(),
            records: BTreeMap::new(),
        })
    }

    /// How many generations have retired.
    pub(crate) fn retired(&self) -> usize {
        self.retired
    }

    /// Install `wrapper`'s record tree and return its entry and the code
    /// it lives in. A failed install retires the generation; a full
    /// arena reinstalls once in a fresh one.
    fn install(
        &mut self,
        wrapper: &Arc<BodyRecord>,
    ) -> Result<(*const u8, Arc<CodeOwner>)> {
        let e = match self.generation.install(wrapper) {
            Ok(p) => return Ok((p, self.generation.code.clone())),
            Err(e) => e,
        };
        self.generation = Generation::new(&self.emitter.isa)?;
        self.retired += 1;
        if !e.chain().any(|c| c.is::<ArenaExhausted>()) {
            return Err(e);
        }
        log::warn!(
            "JIT code arena exhausted: retired generation {} (freed when its last \
             kernel drops) and reinstalling in a fresh module",
            self.retired
        );
        let p = self.generation.install(wrapper)?;
        Ok((p, self.generation.code.clone()))
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
        let (wrapper_fn_ptr, code) = self.install(wrapper)?;
        Ok(WrappedKernel {
            wrapper_fn_ptr,
            _code: code,
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
    /// The body's id in [`Names`].
    func_id: FuncId,
    signature: Signature,
    /// See [`WrappedKernel::slot_table_words`]; filled in phase 2.
    slot_table_words: Vec<kernel_abi::SiteAnchor>,
    /// See [`WrappedKernel::state_self_blocks`]; filled in phase 2.
    state_self_blocks: Vec<kernel_abi::SelfBlock>,
    /// Filled when the body is compiled. `None` at a caller's emission
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

fn compile_region(
    jit: &mut Jit,
    kernel: &Arc<KernelSig>,
    parent: &BodySource,
    callees: &[(KernelKey, Arc<KernelSig>)],
    emitters: &AHashMap<KernelKey, BodySource>,
) -> Result<WrappedKernel> {
    let mut build_profile = profile::phase(Phase::JitBuild);
    let mut fresh: SmallVec<[(CacheKey, Arc<KernelSig>); 8]> = SmallVec::new();
    let r = compile_region_inner(jit, kernel, parent, callees, emitters, &mut fresh);
    if r.is_err() {
        profile::failed(&mut build_profile);
        // A fresh entry of a failed region would hand out an id that no
        // record answers.
        for (key, _) in fresh.iter() {
            jit.by_kernel.remove(key);
        }
    }
    r
}

/// `fresh` collects the cache entries the region declares, in
/// declaration order.
fn compile_region_inner(
    jit: &mut Jit,
    kernel: &Arc<KernelSig>,
    parent: &BodySource,
    callees: &[(KernelKey, Arc<KernelSig>)],
    emitters: &AHashMap<KernelKey, BodySource>,
    fresh: &mut SmallVec<[(CacheKey, Arc<KernelSig>); 8]>,
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
    let parent_sig = kernel_signature(&*jit.emitter.isa, kernel)?;
    let parent_fid = jit.emitter.names.local(&parent_sig);
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
                let sig = kernel_signature(&*jit.emitter.isa, k)?;
                let fid = jit.emitter.names.local(&sig);
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
                fresh.push((key, k.clone()));
                (fid, sig)
            }
        };
        funcids.push((key.kernel, entry));
    }
    // Phase 2: compile the fresh callee bodies in topological order over
    // the static call edges, callees first, so a caller can read its
    // callees' `SiteLayout`s (the only one missing at emission is a
    // self-call's), then the parent.
    let order = def_order(fresh, emitters);
    for (key, k) in order.iter() {
        let body = emitters.get(&key.kernel).ok_or_else(|| {
            anyhow!(
                "no body emitter recorded for kernel `{}` — discovery must record \
                 every callee body",
                k.fn_name
            )
        })?;
        let db = define_kernel_body(
            &mut jit.emitter,
            k,
            &funcids,
            body,
            &callee_layouts,
            &jit.records,
        )?;
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
        &mut jit.emitter,
        kernel,
        &funcids,
        parent,
        &callee_layouts,
        &jit.records,
    )?;
    jit.records.insert(parent_fid, db.record);
    // Phase 3: the parent's wrapper, then install.
    let wrapper = define_wrapper(&mut jit.emitter, kernel, parent_fid, &jit.records)?;
    let (wrapper_fn_ptr, code) = jit.install(&wrapper)?;
    Ok(WrappedKernel {
        wrapper_fn_ptr,
        _code: code,
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

/// What compiling one kernel body produced — stored onto the kernel's
/// `by_kernel` cache entry (the fields mirror the entry's).
struct DefinedBody {
    record: Arc<BodyRecord>,
    state_words: usize,
    slot_table_words: Vec<kernel_abi::SiteAnchor>,
    state_self_blocks: Vec<kernel_abi::SelfBlock>,
    site_layout: SiteLayout,
}

/// Emit and compile `kernel`'s body, named by its pre-declared id, to
/// its record. `funcids` must hold the kernel itself and every callee
/// its lambda call sites reference.
fn define_kernel_body(
    em: &mut Emitter,
    kernel: &Arc<KernelSig>,
    funcids: &[(KernelKey, (FuncId, Signature))],
    body_emitter: &BodySource,
    callee_layouts: &AHashMap<KernelKey, SiteLayout>,
    records: &BTreeMap<FuncId, Arc<BodyRecord>>,
) -> Result<DefinedBody> {
    let mut clif_profile = profile::phase(Phase::Clif);
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
    let consts: RefCell<Vec<EmitConst>> = RefCell::new(Vec::new());
    let thunk_label = format_compact!("{}__spill", kernel.fn_name);
    let self_thunk_id = body_emitter.spec.self_call().map(|_| {
        let tsig = trampoline_signature(&*em.isa);
        em.names.local(&tsig)
    });
    em.reset_func();
    em.func_ctx.func.signature = sig.clone();
    em.func_ctx.func.name = UserFuncName::user(0, func_id.as_u32());
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
                let fref = em.names.import_func(*fid, &mut em.func_ctx.func);
                callee_refs.insert(*key, fref);
            }
        }
        if callee_refs.len() != callee_keys.len() {
            bail!(
                "define_kernel_body: kernel `{}` calls a kernel with no entry in funcids",
                kernel.fn_name
            );
        }
        let self_thunk =
            self_thunk_id.map(|tid| em.names.import_func(tid, &mut em.func_ctx.func));
        let names = RefCell::new(&mut em.names);
        let mut builder =
            FunctionBuilder::new(&mut em.func_ctx.func, &mut em.builder_ctx);
        let emitted = compile_into_function(
            &mut builder,
            kernel,
            &callee_refs,
            self_thunk,
            &em.helpers,
            &consts,
            &names,
            body_emitter,
            callee_layouts,
        );
        if emitted.is_err() {
            profile::failed(&mut clif_profile);
        }
        let emitted = emitted?;
        builder.finalize();
        maybe_dump_clif(&em.func_ctx.func, &kernel.fn_name);
        emitted
    };
    drop(clif_profile);
    let backend_profile = profile::phase(Phase::BackendBody);
    let compiled = em.compile(func_id).context("shared body")?;
    drop(backend_profile);
    let mut callees = Vec::new();
    let relocs = em.record_relocs(
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
                build_trampoline(em, tid, func_id, &sig, false).context("spill thunk")?;
            let mut none = Vec::new();
            let relocs = em.record_relocs(
                &t.relocs,
                func_id,
                None,
                &[],
                &BTreeMap::new(),
                &mut none,
            )?;
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
    em.reset_func();
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
    em: &mut Emitter,
    id: FuncId,
    target: FuncId,
    target_sig: &Signature,
    wrapper: bool,
) -> Result<Compiled> {
    em.reset_func();
    em.func_ctx.func.signature = trampoline_signature(&*em.isa);
    em.func_ctx.func.name = UserFuncName::user(0, id.as_u32());
    let target_ref = em.names.import_func(target, &mut em.func_ctx.func);
    #[cfg(debug_assertions)]
    let record_ref = match wrapper {
        true => {
            let fid =
                em.helpers.ids.get("graphix_record_jit_invocation").copied().ok_or_else(
                    || anyhow!("missing graphix_record_jit_invocation FuncId"),
                )?;
            Some(em.names.import_func(fid, &mut em.func_ctx.func))
        }
        false => None,
    };
    {
        let mut b = FunctionBuilder::new(&mut em.func_ctx.func, &mut em.builder_ctx);
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
    maybe_dump_clif(&em.func_ctx.func, if wrapper { "wrapper" } else { "spill thunk" });
    let compiled = em.compile(id);
    em.reset_func();
    compiled
}

/// The region's `(args, out)` wrapper around its parent body
/// `typed_func_id`, as a record.
fn define_wrapper(
    em: &mut Emitter,
    kernel: &Arc<KernelSig>,
    typed_func_id: FuncId,
    records: &BTreeMap<FuncId, Arc<BodyRecord>>,
) -> Result<Arc<BodyRecord>> {
    let label = format_compact!("{}_wrap", kernel.fn_name);
    let sig = trampoline_signature(&*em.isa);
    let wrapper_id = em.names.local(&sig);
    let _backend_profile = profile::phase(Phase::BackendWrapper);
    let kernel_sig = kernel_signature(&*em.isa, kernel)?;
    let t = build_trampoline(em, wrapper_id, typed_func_id, &kernel_sig, true)
        .context("wrapper")?;
    let mut callees = Vec::new();
    let relocs =
        em.record_relocs(&t.relocs, wrapper_id, None, &[], records, &mut callees)?;
    Ok(Arc::new(BodyRecord {
        kind: RecordKind::Wrapper,
        label: label.as_str().into(),
        bytes: t.bytes,
        align: t.align,
        relocs,
        callees,
        kernel: kernel.clone(),
    }))
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
