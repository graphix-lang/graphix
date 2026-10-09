//! The per-context JIT pipeline: an [`Emission`] emits a region's
//! bodies against its own id table ([`Names`]) and caches them
//! ([`Caches`]); at a link [`Jit`] compiles each to a [`BodyRecord`] and
//! installs the region's wrapper record into the current module
//! [`Generation`], cold and warm alike; [`WrappedKernel`] and the
//! wrapper-seam value packing ([`pack_value_to_u64`]).

use crate::{
    Node, Rt, UserEvent,
    env::Env,
    expr::ExprId,
    fusion::{
        CalleeBody, FusionCtx, LambdaCallInfo,
        emit_helpers::all_helpers,
        kernel_abi::{self, AbiKind, AbiParamKind, KernelKey, KernelSig, PrimType},
        lowering::BuiltinCallSiteInfo,
    },
    image::ImageBuf,
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
        UserExternalNameRef, UserFuncName, Value as ClifValue, immediates::Imm64, types,
    },
    isa::{CallConv, OwnedTargetIsa, TargetIsa},
    settings::{self, Configurable},
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{ArenaMemoryProvider, JITBuilder, JITModule};
use cranelift_module::{
    DataId, FuncId, Linkage, Module, ModuleError, ModuleReloc, ModuleRelocTarget,
    default_libcall_names,
};
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint};
use netidx_value::Value;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    cell::RefCell,
    collections::BTreeMap,
    mem::ManuallyDrop,
    sync::{LazyLock, OnceLock},
};
use triomphe::Arc;

use super::{
    body::{BodyRole, BodySource, BodySpec, NodeBodyEmitter},
    lower::{Callees, EmittedBody, HelperFuncIds, SiteLayout, compile_into_function},
    record::{
        BodyRecord, EmitConst, RecordKind, RecordReloc, RelocTarget, SymbolTable,
        record_decode, record_encode,
    },
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

/// The `(frame, lo, hi, out)` signature of an outlined loop's chunk
/// (`fusion::par_loop`).
pub(super) fn chunk_signature(call_conv: CallConv) -> Signature {
    let mut sig = Signature::new(call_conv);
    for _ in 0..4 {
        sig.params.push(AbiParam::new(types::I64));
    }
    sig
}

/// An outlined loop of a body being emitted: its function and the
/// constants its code names.
pub(super) struct ChunkFn {
    pub(super) id: FuncId,
    pub(super) func: Function,
    pub(super) consts: Vec<EmitConst>,
}

/// What a body's CLIF names (kernel bodies, thunks, wrappers, the
/// runtime helpers and constants) by ids of this table alone. Nothing
/// named here is ever defined: a record names its targets symbolically,
/// and installing it mints the module's ids.
///
/// A compile task's table extends its fork's frozen one ([`Emission`]):
/// it mints ids from the base's next, and its join renumbers them.
#[derive(Default)]
pub(super) struct Names {
    base: Option<Arc<Names>>,
    /// The first id this table mints.
    first: u32,
    /// Each function's signature and whether calls to it are colocated.
    funcs: Vec<(Signature, bool)>,
    data: u32,
}

impl Names {
    fn over(base: Arc<Names>) -> Self {
        Self { first: base.next(), data: base.data, funcs: Vec::new(), base: Some(base) }
    }

    fn next(&self) -> u32 {
        self.first + self.funcs.len() as u32
    }

    fn sig(&self, id: FuncId) -> &(Signature, bool) {
        let i = id.as_u32();
        match i.checked_sub(self.first) {
            Some(i) => &self.funcs[i as usize],
            None => self
                .base
                .as_ref()
                .expect("an id below a table's first is its base's")
                .sig(id),
        }
    }

    fn func(&mut self, sig: &Signature, colocated: bool) -> FuncId {
        self.funcs.push((sig.clone(), colocated));
        FuncId::from_u32(self.next() - 1)
    }

    /// A function the module defines: a body, a thunk or a wrapper.
    pub(super) fn local(&mut self, sig: &Signature) -> FuncId {
        self.func(sig, true)
    }

    pub(super) fn data(&mut self) -> DataId {
        self.data += 1;
        DataId::from_u32(self.data - 1)
    }

    /// `id` as a callee of `func`, the body being built.
    pub(super) fn import_func(&self, id: FuncId, func: &mut Function) -> FuncRef {
        let (sig, colocated) = self.sig(id);
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

/// What emission and linking share and never change: the ISA and the
/// runtime helpers' ids, declared first in every root [`Names`].
pub(crate) struct EmitShared {
    isa: OwnedTargetIsa,
    helpers: HelperFuncIds,
    /// The helpers by their ids, for naming a relocation's target.
    helper_names: BTreeMap<FuncId, &'static str>,
    /// The table the helpers are declared in, every root's start.
    helper_table: Arc<Names>,
}

impl EmitShared {
    fn new() -> Result<Self> {
        let isa = host_isa()?;
        let mut names = Names::default();
        let helpers = HelperFuncIds::new(isa.default_call_conv(), |_, sig| {
            Ok(names.func(sig, false))
        })?;
        let helper_names = helpers.ids.iter().map(|(n, id)| (*id, *n)).collect();
        Ok(Self { isa, helpers, helper_names, helper_table: Arc::new(names) })
    }

    /// Name every relocation target symbolically: a helper, a recorded
    /// callee (entered in `callees`), the owner, the thunk, one of the
    /// owner's chunks, a libcall or one of the function's constants.
    fn record_relocs(
        &self,
        relocs: &[ModuleReloc],
        owner: FuncId,
        thunk: Option<FuncId>,
        chunks: &[FuncId],
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
                        } else if let Some(i) = chunks.iter().position(|c| *c == fid) {
                            RelocTarget::Chunk(i as u32)
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

/// Compile `func`, named `id`, to bytes and relocations.
fn backend(
    isa: &dyn TargetIsa,
    ctx: &mut Context,
    id: FuncId,
    func: &mut Function,
) -> Result<Compiled> {
    ctx.clear();
    ctx.func = std::mem::replace(func, Function::new());
    ctx.compile(isa, &mut ControlPlane::default())
        .map_err(|e| anyhow!("compile: {}", e.inner))?;
    let code = ctx.compiled_code().expect("compiled above");
    let bytes: Box<[u8]> = code.code_buffer().into();
    let align = code.buffer.alignment as u64;
    let relocs = code
        .buffer
        .relocs()
        .iter()
        .map(|r| ModuleReloc::from_mach_reloc(r, &ctx.func, id))
        .collect();
    Ok(Compiled { bytes, align, relocs })
}

/// Every function of `pending`, its spill thunks and chunks included,
/// in order; `pending` keeps the rest of each. This is where the CLIF is
/// dumped: in join order with final ids, so a dump does not depend on
/// how the compile tasks were scheduled.
fn take_functions(pending: &mut [Pending]) -> Vec<(FuncId, Function)> {
    let mut work = Vec::with_capacity(pending.len());
    for p in pending.iter_mut() {
        let name = &p.kernel.fn_name;
        match &p.of {
            PendingOf::Body { .. } => maybe_dump_clif(&p.func, name),
            PendingOf::Wrapper { .. } => maybe_dump_clif(&p.func, "wrapper"),
        }
        work.push((p.id, std::mem::replace(&mut p.func, Function::new())));
        if let PendingOf::Body { thunk, chunks, .. } = &mut p.of {
            if let Some((tid, t)) = thunk {
                maybe_dump_clif(t, &format_compact!("{name} spill thunk"));
                work.push((*tid, std::mem::replace(t, Function::new())));
            }
            for c in chunks.iter_mut() {
                maybe_dump_clif(&c.func, &format_compact!("{name} chunk"));
                work.push((c.id, std::mem::replace(&mut c.func, Function::new())));
            }
        }
    }
    work
}

/// A compiling thread's stack: cranelift recurses over the function.
const STACK: usize = 8 << 20;

/// Compile every function in `work` on as many threads as there is work
/// for, up to the compile's thread budget (rayon's pool); the results are
/// in `work`'s order whatever the threads did.
fn backend_all(
    isa: &dyn TargetIsa,
    mut work: Vec<(FuncId, Function)>,
) -> Vec<Result<Compiled>> {
    const PER_THREAD: usize = 8;
    let work: Vec<(FuncId, &mut Function)> =
        work.iter_mut().map(|(id, f)| (*id, f)).collect();
    let threads = rayon::current_num_threads().min(work.len().div_ceil(PER_THREAD));
    if threads <= 1 {
        let mut ctx = Context::new();
        return work.into_iter().map(|(id, f)| backend(isa, &mut ctx, id, f)).collect();
    }
    let n = work.len();
    let queue = Mutex::new(work.into_iter().enumerate());
    let drain = || {
        let mut ctx = Context::new();
        let mut done = Vec::new();
        loop {
            let next = queue.lock().next();
            let Some((i, (id, f))) = next else { break };
            done.push((i, backend(isa, &mut ctx, id, f)));
        }
        done
    };
    let mut out: Vec<Option<Result<Compiled>>> = (0..n).map(|_| None).collect();
    std::thread::scope(|s| {
        // The calling thread drains too, so a worker that fails to spawn
        // leaves nothing behind.
        let workers: SmallVec<[_; 32]> = (1..threads)
            .filter_map(|_| {
                std::thread::Builder::new().stack_size(STACK).spawn_scoped(s, drain).ok()
            })
            .collect();
        let mut place = |done: Vec<(usize, Result<Compiled>)>| {
            for (i, r) in done {
                out[i] = Some(r);
            }
        };
        place(drain());
        for w in workers {
            match w.join() {
                Ok(done) => place(done),
                Err(panic) => std::panic::resume_unwind(panic),
            }
        }
    });
    out.into_iter().map(|r| r.expect("every item compiled")).collect()
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

    /// Install each wrapper's record tree, finalize once, and return the
    /// wrappers' entries in order.
    fn install(&mut self, wrappers: &[Arc<BodyRecord>]) -> Result<Vec<*const u8>> {
        let ids: LPooled<Vec<FuncId>> =
            wrappers.iter().map(|w| self.load(w)).collect::<Result<_>>()?;
        let _finalize = profile::phase(Phase::Finalize);
        self.module.finalize_definitions().context("finalize_definitions")?;
        Ok(ids.iter().map(|id| self.module.get_finalized_function(*id)).collect())
    }

    /// Install a record's function, its thunk, its chunks and its
    /// callees (once per record) and return the function's id.
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
            self.define_record(t, tid, None, &[], id, &[])?;
        }
        let mut chunks: SmallVec<[FuncId; 2]> = SmallVec::new();
        for c in rec.chunks() {
            let callees: SmallVec<[FuncId; 8]> =
                c.callees.iter().map(|c| self.load(c)).collect::<Result<_>>()?;
            let cid = self.declare_record(c)?;
            self.define_record(c, cid, thunk_id, &[], id, &callees)?;
            chunks.push(cid);
        }
        self.define_record(rec, id, thunk_id, &chunks, id, &callees)?;
        self.loaded.insert(key, (id, rec.clone()));
        Ok(id)
    }

    fn declare_record(&mut self, rec: &BodyRecord) -> Result<FuncId> {
        let isa = self.module.isa();
        let sig = match rec.kind {
            RecordKind::Kernel { .. } => kernel_signature(isa, &rec.kernel)?,
            RecordKind::Chunk { .. } => chunk_signature(isa.default_call_conv()),
            RecordKind::Thunk | RecordKind::Wrapper => trampoline_signature(isa),
        };
        let symbol = self.next_symbol(&rec.label);
        Ok(self.module.declare_function(&symbol, Linkage::Local, &sig)?)
    }

    /// Define `id` from the record's bytes: its constants become imports
    /// resolving to their pointers, its relocations name the module's
    /// ids. `owner` is the body a thunk or chunk serves (itself for a
    /// body).
    fn define_record(
        &mut self,
        rec: &BodyRecord,
        id: FuncId,
        thunk: Option<FuncId>,
        chunks: &[FuncId],
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
                RelocTarget::Chunk(i) => {
                    let cid = chunks
                        .get(*i as usize)
                        .ok_or_else(|| anyhow!("a relocation to a missing chunk"))?;
                    ModuleRelocTarget::user(0, cid.as_u32())
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

/// A kernel behind the uniform [`WrapperFn`] convention: `args` points
/// at the context words then a `(disc, payload)` pair per parameter,
/// `out` receives the result's `(disc, payload)` pair.
/// [`pack_value_to_u64`] does the Rust-side packing. The layout is known
/// when the region is emitted, the entry once its pass links.
#[derive(Clone)]
pub struct WrappedKernel {
    entry: Arc<OnceLock<Entry>>,
    /// Per-instance state words the root body claimed. The runtime
    /// `FusedKernel` passes a zeroed buffer of this size in wire slot 1.
    pub(crate) state_words: usize,
    /// The root body's per-slot state-table anchors: each word holds a
    /// `Box<Vec<u64>>` chain owned by `graphix_slot_state_table` and
    /// freed by `FusedKernel`'s `Drop`.
    pub(crate) slot_table_words: Vec<kernel_abi::SiteAnchor>,
    /// Per-activation block-tree roots living in the parent's state
    /// buffer; `FusedKernel` frees and resets them.
    pub(crate) state_self_blocks: Vec<kernel_abi::SelfBlock>,
}

impl WrappedKernel {
    /// What an image carries for this kernel: its layout and its
    /// wrapper's record.
    pub(crate) fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        encode_varint(self.state_words as u64, buf);
        self.slot_table_words.encode(buf)?;
        self.state_self_blocks.encode(buf)?;
        record_encode(self.wrapper(), buf)
    }

    /// A kernel an image carried, installed into `fusion`'s module.
    pub(crate) fn image_decode(
        fusion: &FusionCtx,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let state_words = decode_varint(buf)? as usize;
        let slot_table_words = Pack::decode(buf)?;
        let state_self_blocks = Pack::decode(buf)?;
        let wrapper = record_decode(buf)?;
        fusion
            .jit()
            .and_then(|mut jit| {
                jit.load_wrapped(
                    &wrapper,
                    state_words,
                    slot_table_words,
                    state_self_blocks,
                )
            })
            .map_err(|e| {
                log::warn!(
                    "loading the kernel `{}` from the image: {e:#}",
                    wrapper.label
                );
                PackError::InvalidFormat
            })
    }
}

/// A linked region's wrapper.
struct Entry {
    fn_ptr: *const u8,
    /// The code `fn_ptr` and everything it calls live in.
    _code: Arc<CodeOwner>,
    /// The wrapper's record, and through it the region's bodies: what
    /// an image writes for this kernel.
    wrapper: Arc<BodyRecord>,
}

// SAFETY: `fn_ptr` points into `code`, which it keeps alive; the code is
// immutable once finalized.
unsafe impl Send for Entry {}
unsafe impl Sync for Entry {}

/// The uniform Rust-side signature the wrapper presents.
pub(crate) type WrapperFn = unsafe extern "C" fn(args: *const u64, out: *mut u64);

impl WrappedKernel {
    fn entry(&self) -> &Entry {
        self.entry.get().expect("a kernel runs after its fusion pass links")
    }

    /// The wrapper entry point.
    ///
    /// # Safety
    /// Callers pass the `(args, out)` layout of the kernel's ABI.
    pub(crate) unsafe fn fn_ptr(&self) -> WrapperFn {
        unsafe { std::mem::transmute(self.entry().fn_ptr) }
    }

    /// The wrapper's record.
    pub(crate) fn wrapper(&self) -> &Arc<BodyRecord> {
        &self.entry().wrapper
    }
}

/// A compiled kernel body's identity in an [`Emission`]'s cache: a body
/// bakes its sibling kernels' FuncIds, which depend on the region's
/// ordered kernel list, so only identical layouts may share a
/// compilation. A body with no sibling sites uses layout 0.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct CacheKey {
    kernel: KernelKey,
    layout: u32,
}

/// A function emitted and not yet compiled.
pub(crate) struct Pending {
    id: FuncId,
    func: Function,
    kernel: Arc<KernelSig>,
    of: PendingOf,
}

enum PendingOf {
    /// A kernel body: the constants its code names, its spill thunk and
    /// its outlined loops.
    Body {
        consts: Vec<EmitConst>,
        thunk: Option<(FuncId, Function)>,
        chunks: Vec<ChunkFn>,
    },
    /// A region's wrapper, and where its entry goes.
    Wrapper { entry: Arc<OnceLock<Entry>> },
}

/// The emission caches, layered like [`Names`]: lambda kernel bodies by
/// key, region layouts, and the lambda kernels' signatures.
#[derive(Default)]
struct Caches {
    base: Option<Arc<Caches>>,
    /// Lambda kernel bodies; the entry holds the `Arc` so the key's
    /// address cannot be reused by a later allocation.
    by_kernel: BTreeMap<CacheKey, CachedBody>,
    /// The first layout id this layer mints; 0 is the layout-independent
    /// id.
    first_layout: u32,
    /// Region layout -> id.
    layout_ids: BTreeMap<SmallVec<[KernelKey; 8]>, u32>,
    /// The lambda kernels' signatures ([`crate::fusion::KernelCacheKey`]).
    kernels: BTreeMap<crate::fusion::KernelCacheKey, LambdaCallInfo>,
}

impl Caches {
    fn over(base: Arc<Caches>) -> Self {
        Self { first_layout: base.next_layout(), base: Some(base), ..Self::default() }
    }

    fn is_empty(&self) -> bool {
        self.by_kernel.is_empty() && self.layout_ids.is_empty() && self.kernels.is_empty()
    }

    fn next_layout(&self) -> u32 {
        self.first_layout + self.layout_ids.len() as u32
    }

    fn by_kernel(&self, key: &CacheKey) -> Option<&CachedBody> {
        self.by_kernel.get(key).or_else(|| self.base.as_ref()?.by_kernel(key))
    }

    fn layout(&self, layout: &SmallVec<[KernelKey; 8]>) -> Option<u32> {
        self.layout_ids
            .get(layout)
            .copied()
            .or_else(|| self.base.as_ref()?.layout(layout))
    }

    fn intern_layout(&mut self, layout: SmallVec<[KernelKey; 8]>) -> u32 {
        if let Some(id) = self.layout(&layout) {
            return id;
        }
        let id = self.next_layout();
        self.layout_ids.insert(layout, id);
        id
    }

    fn kernel(&self, key: &crate::fusion::KernelCacheKey) -> Option<&LambdaCallInfo> {
        self.kernels.get(key).or_else(|| self.base.as_ref()?.kernel(key))
    }
}

/// What emission builds before a link: the names its bodies are emitted
/// against, its caches and the functions waiting to compile. A compile
/// task emits into its own ([`Self::fork`]), over a frozen copy of its
/// parent's; its join ([`Self::join`]) renumbers its functions after the
/// parent's, in join order, so the result does not depend on the
/// schedule.
pub(crate) struct Emission {
    shared: Arc<EmitShared>,
    names: Names,
    caches: Caches,
    /// What the regions emitted since the last link, in emission order:
    /// a body after its callees, a wrapper after its region's bodies.
    pending: Vec<Pending>,
    builder_ctx: FunctionBuilderContext,
    /// The kernel signatures cached since [`Self::attempt`], which a
    /// region that fails forgets with its bodies.
    attempt_kernels: Vec<crate::fusion::KernelCacheKey>,
}

impl Emission {
    pub(crate) fn new(shared: Arc<EmitShared>) -> Self {
        let names = Names::over(shared.helper_table.clone());
        let caches = Caches { first_layout: 1, ..Caches::default() };
        Self {
            shared,
            names,
            caches,
            pending: Vec::new(),
            builder_ctx: FunctionBuilderContext::new(),
            attempt_kernels: Vec::new(),
        }
    }

    /// Whether this is the context's own emission, no layer of it frozen
    /// for a fork: only it links.
    pub(crate) fn is_root(&self) -> bool {
        self.caches.base.is_none()
    }

    /// How many regions wait for the next link.
    pub(crate) fn unlinked(&self) -> usize {
        self.pending.iter().filter(|p| matches!(p.of, PendingOf::Wrapper { .. })).count()
    }

    pub(crate) fn take_pending(&mut self) -> Vec<Pending> {
        std::mem::take(&mut self.pending)
    }

    pub(crate) fn kernel(
        &self,
        key: &crate::fusion::KernelCacheKey,
    ) -> Option<LambdaCallInfo> {
        self.caches.kernel(key).cloned()
    }

    pub(crate) fn cache_kernel(
        &mut self,
        key: crate::fusion::KernelCacheKey,
        info: LambdaCallInfo,
    ) {
        self.attempt_kernels.push(key.clone());
        self.caches.kernels.insert(key, info);
    }

    /// A region's attempt starts: what it caches stays only if it emits.
    pub(crate) fn attempt(&mut self) {
        self.attempt_kernels.clear()
    }

    /// The attempt failed: a kernel signature it cached has no body, and
    /// a later region reaching it would emit the body from its own
    /// instance against this one's slots.
    pub(crate) fn forget_attempt(&mut self) {
        for key in self.attempt_kernels.drain(..) {
            self.caches.kernels.remove(&key);
        }
    }

    /// A compile task's emission: this one's frozen, which both then
    /// extend. Siblings forked in a row share one frozen layer.
    pub(crate) fn fork(&mut self) -> Self {
        if !self.names.funcs.is_empty() || self.names.base.is_none() {
            let frozen = Arc::new(std::mem::take(&mut self.names));
            self.names = Names::over(frozen);
        }
        if !self.caches.is_empty() || self.caches.base.is_none() {
            let frozen = Arc::new(std::mem::take(&mut self.caches));
            self.caches = Caches::over(frozen);
        }
        let names = Names::over(self.names.base.clone().expect("frozen above"));
        let caches = Caches::over(self.caches.base.clone().expect("frozen above"));
        Self {
            shared: self.shared.clone(),
            names,
            caches,
            pending: Vec::new(),
            builder_ctx: FunctionBuilderContext::new(),
            attempt_kernels: Vec::new(),
        }
    }

    /// Take back a task forked from this one: its functions are numbered
    /// after this one's, a kernel body this one already has replaces the
    /// task's, and its functions wait after this one's.
    pub(crate) fn join(&mut self, fork: Self) {
        let Self {
            shared: _,
            names,
            caches,
            pending,
            builder_ctx: _,
            attempt_kernels: _,
        } = fork;
        let first = names.first;
        let to = self.names.next();
        let Caches { base: _, by_kernel, first_layout, layout_ids, kernels } = caches;
        let mut layouts: LPooled<AHashMap<u32, u32>> = LPooled::take();
        let mut by_id: LPooled<Vec<(u32, SmallVec<[KernelKey; 8]>)>> =
            layout_ids.into_iter().map(|(l, id)| (id, l)).collect();
        by_id.sort_unstable_by_key(|(id, _)| *id);
        for (id, l) in by_id.drain(..) {
            // CR claude for claude: [perf] This interns the task's layout lists before
            // `same` exists, so each list still names the task's own copies of kernels
            // the parent already has. A task body with an external call site (layout !=
            // 0) is rekeyed to a layout no parent region produces, so it is never
            // `moved`, and each task that reached it compiles its own copy; in practice
            // only layout-0 bodies dedupe. In the probe (20 array elements `g(x + i) +
            // fact(..)`, with g calling h), the program image holds 19 more records of
            // g under the default task walk than under GRAPHIX_FUSE_SERIAL=1, while h
            // and fact match, against CLAUDE.md's 'the output is the serial walk's'.
            // Compute `same` first and map each task layout list through it before
            // interning. probe: design/review-2026-10-05/repro/f-jit-06.sh (f-jit-06)
            layouts.insert(id, self.caches.intern_layout(l));
        }
        let layout = |l: u32| if l >= first_layout { layouts[&l] } else { l };
        // a kernel the task built that this one has: the task's calls go
        // to this one's body
        let mut same: LPooled<AHashMap<KernelKey, KernelKey>> = LPooled::take();
        for (key, info) in kernels {
            match self.caches.kernel(&key) {
                Some(have) => {
                    let (k, h) = (
                        kernel_abi::kernel_key(&info.kernel),
                        kernel_abi::kernel_key(&have.kernel),
                    );
                    if k != h {
                        same.insert(k, h);
                    }
                }
                None => {
                    self.caches.kernels.insert(key, info);
                }
            }
        }
        let mut moved: LPooled<AHashMap<u32, FuncId>> = LPooled::take();
        let mut kept: LPooled<Vec<(CacheKey, CachedBody)>> = LPooled::take();
        for (key, cached) in by_kernel {
            let key = CacheKey {
                kernel: same.get(&key.kernel).copied().unwrap_or(key.kernel),
                layout: layout(key.layout),
            };
            match self.caches.by_kernel(&key) {
                Some(have) => {
                    moved.insert(cached.func_id.as_u32(), have.func_id);
                }
                None => kept.push((key, cached)),
            }
        }
        let renumber = |id: FuncId| -> FuncId {
            let i = id.as_u32();
            if i < first {
                id
            } else if let Some(have) = moved.get(&i) {
                *have
            } else {
                FuncId::from_u32(i - first + to)
            }
        };
        for (key, mut cached) in kept.drain(..) {
            cached.func_id = renumber(cached.func_id);
            self.caches.by_kernel.insert(key, cached);
        }
        self.names.funcs.extend(names.funcs);
        self.names.data = self.names.data.max(names.data);
        for mut p in pending {
            if moved.contains_key(&p.id.as_u32()) {
                continue;
            }
            p.id = renumber(p.id);
            renumber_function(&mut p.func, &renumber);
            if let PendingOf::Body { thunk, chunks, .. } = &mut p.of {
                if let Some((tid, t)) = thunk {
                    *tid = renumber(*tid);
                    renumber_function(t, &renumber);
                }
                for c in chunks.iter_mut() {
                    c.id = renumber(c.id);
                    renumber_function(&mut c.func, &renumber);
                }
            }
            self.pending.push(p);
        }
    }

    /// Fold back every frozen layer no fork holds any more.
    pub(crate) fn thaw(&mut self) {
        while let Some(base) = self.names.base.take() {
            match Arc::try_unwrap(base) {
                Ok(mut base) => {
                    base.funcs.append(&mut self.names.funcs);
                    base.data = base.data.max(self.names.data);
                    self.names = base;
                }
                Err(base) => {
                    self.names.base = Some(base);
                    break;
                }
            }
        }
        while let Some(base) = self.caches.base.take() {
            match Arc::try_unwrap(base) {
                Ok(mut base) => {
                    base.by_kernel.append(&mut self.caches.by_kernel);
                    base.layout_ids.append(&mut self.caches.layout_ids);
                    base.kernels.append(&mut self.caches.kernels);
                    self.caches = base;
                }
                Err(base) => {
                    self.caches.base = Some(base);
                    break;
                }
            }
        }
    }

    /// Build the function `id` with `sig` through `emit`. A failed build
    /// leaves the builder mid-function, so it starts over.
    fn build(
        &mut self,
        id: FuncId,
        sig: Signature,
        emit: impl FnOnce(&mut Names, &HelperFuncIds, &mut FunctionBuilder) -> Result<()>,
    ) -> Result<Function> {
        let mut func =
            Function::with_name_signature(UserFuncName::user(0, id.as_u32()), sig);
        let mut b = FunctionBuilder::new(&mut func, &mut self.builder_ctx);
        match emit(&mut self.names, &self.shared.helpers, &mut b) {
            Ok(()) => {
                b.finalize();
                Ok(func)
            }
            Err(e) => {
                drop(b);
                self.builder_ctx = FunctionBuilderContext::new();
                Err(e)
            }
        }
    }
}

/// Point `func`'s name and the functions it calls through `renumber`.
fn renumber_function(func: &mut Function, renumber: &impl Fn(FuncId) -> FuncId) {
    if let UserFuncName::User(n) = &func.name
        && n.namespace == 0
    {
        let id = renumber(FuncId::from_u32(n.index));
        func.name = UserFuncName::user(0, id.as_u32());
    }
    let names: SmallVec<[(UserExternalNameRef, UserExternalName); 16]> =
        func.params.user_named_funcs().iter().map(|(r, n)| (r, n.clone())).collect();
    for (r, n) in names {
        if n.namespace == 0 {
            let index = renumber(FuncId::from_u32(n.index)).as_u32();
            func.params.reset_user_func_name(r, UserExternalName { namespace: 0, index });
        }
    }
}

/// The per-`ExecCtx` JIT. Kernels call each other with direct CLIF
/// calls, so a pass's records install into one module generation,
/// which lives as long as the `ExecCtx` or its longest-lived kernel.
pub struct Jit {
    shared: Arc<EmitShared>,
    generation: Generation,
    /// Generations retired so far.
    retired: usize,
    /// Every compiled kernel body's record, by its id in [`Names`]; a
    /// caller's relocations name callees through it.
    records: BTreeMap<FuncId, Arc<BodyRecord>>,
    /// The batch compiling while emission goes on; it installs before
    /// any later batch.
    in_flight: Option<InFlight>,
}

/// A batch whose functions compile on threads of their own.
struct InFlight {
    pending: Vec<Pending>,
    compiled: std::thread::JoinHandle<Vec<Result<Compiled>>>,
}

// SAFETY: the module is used only through `&mut Jit`, and its raw
// pointers are into its own arena.
unsafe impl Send for Jit {}

impl Jit {
    /// Errs if cranelift cannot target the host ISA.
    pub fn new() -> Result<Self> {
        let shared = Arc::new(EmitShared::new()?);
        let generation = Generation::new(&shared.isa)?;
        Ok(Self {
            shared,
            generation,
            retired: 0,
            records: BTreeMap::new(),
            in_flight: None,
        })
    }

    /// A fresh emission for this module's context.
    pub(crate) fn emission(&self) -> Emission {
        Emission::new(self.shared.clone())
    }

    /// How many generations have retired.
    pub(crate) fn retired(&self) -> usize {
        self.retired
    }

    /// Compile everything emitted since the last link, build its records
    /// and install its regions, giving each its entry. Its regions are
    /// spliced and emission accepted them, so a failure is a JIT bug that
    /// no graph could run past: it panics.
    pub(crate) fn link(&mut self, mut pending: Vec<Pending>) {
        let _profile = profile::phase(Phase::Link);
        self.finish_in_flight();
        if pending.is_empty() {
            return;
        }
        let compiled = backend_all(&*self.shared.isa, take_functions(&mut pending));
        self.install_or_panic(pending, compiled)
    }

    /// Start compiling everything emitted since the last link on threads
    /// of its own, while emission goes on; it installs at the next link
    /// or batch, so no region of it has its entry before then.
    pub(crate) fn link_batch(&mut self, mut pending: Vec<Pending>) {
        let _profile = profile::phase(Phase::Link);
        self.finish_in_flight();
        if pending.is_empty() {
            return;
        }
        let work = take_functions(&mut pending);
        let isa = self.shared.isa.clone();
        // the work goes to the thread once it exists: a refused spawn
        // leaves it here to link now
        let (tx, rx) = std::sync::mpsc::sync_channel(1);
        let compiled = std::thread::Builder::new()
            .stack_size(STACK)
            .spawn(move || backend_all(&*isa, rx.recv().expect("the batch's work")));
        match compiled {
            Ok(compiled) => {
                tx.send(work).expect("the batch thread waits for its work");
                self.in_flight = Some(InFlight { pending, compiled })
            }
            Err(_) => {
                let compiled = backend_all(&*self.shared.isa, work);
                self.install_or_panic(pending, compiled)
            }
        }
    }

    fn finish_in_flight(&mut self) {
        let Some(InFlight { pending, compiled }) = self.in_flight.take() else {
            return;
        };
        match compiled.join() {
            Ok(compiled) => self.install_or_panic(pending, compiled),
            Err(panic) => std::panic::resume_unwind(panic),
        }
    }

    fn install_or_panic(
        &mut self,
        pending: Vec<Pending>,
        compiled: Vec<Result<Compiled>>,
    ) {
        if let Err(e) = self.install_compiled(pending, compiled) {
            panic!("the JIT could not link emitted code: {e:#}")
        }
    }

    fn install_compiled(
        &mut self,
        pending: Vec<Pending>,
        compiled: Vec<Result<Compiled>>,
    ) -> Result<()> {
        let mut compiled = compiled.into_iter();
        let mut next = || compiled.next().expect("one result per function");
        let mut regions: Vec<(Arc<BodyRecord>, Arc<OnceLock<Entry>>)> = Vec::new();
        for p in pending {
            let c = next()?;
            let mut callees = Vec::new();
            match p.of {
                PendingOf::Body { consts, thunk, chunks } => {
                    let thunk = match thunk {
                        None => None,
                        Some((tid, _)) => Some((tid, next()?)),
                    };
                    let tid = thunk.as_ref().map(|(tid, _)| *tid);
                    let chunk_ids: SmallVec<[FuncId; 2]> =
                        chunks.iter().map(|c| c.id).collect();
                    let mut chunk_records = Vec::with_capacity(chunks.len());
                    for (i, chunk) in chunks.into_iter().enumerate() {
                        let cc = next()?;
                        let mut callees = Vec::new();
                        let relocs = self.shared.record_relocs(
                            &cc.relocs,
                            p.id,
                            tid,
                            &[],
                            &chunk.consts,
                            &self.records,
                            &mut callees,
                        )?;
                        chunk_records.push(Arc::new(BodyRecord {
                            kind: RecordKind::Chunk {
                                consts: chunk
                                    .consts
                                    .into_iter()
                                    .map(|c| c.recipe)
                                    .collect(),
                            },
                            label: format_compact!("{}__chunk{i}", p.kernel.fn_name)
                                .as_str()
                                .into(),
                            bytes: cc.bytes,
                            align: cc.align,
                            relocs,
                            callees,
                            kernel: p.kernel.clone(),
                        }));
                    }
                    let relocs = self.shared.record_relocs(
                        &c.relocs,
                        p.id,
                        tid,
                        &chunk_ids,
                        &consts,
                        &self.records,
                        &mut callees,
                    )?;
                    let thunk = match thunk {
                        None => None,
                        Some((_, t)) => {
                            let relocs = self.shared.record_relocs(
                                &t.relocs,
                                p.id,
                                None,
                                &[],
                                &[],
                                &BTreeMap::new(),
                                &mut Vec::new(),
                            )?;
                            Some(Arc::new(BodyRecord {
                                kind: RecordKind::Thunk,
                                label: format_compact!("{}__spill", p.kernel.fn_name)
                                    .as_str()
                                    .into(),
                                bytes: t.bytes,
                                align: t.align,
                                relocs,
                                callees: Vec::new(),
                                kernel: p.kernel.clone(),
                            }))
                        }
                    };
                    let record = Arc::new(BodyRecord {
                        kind: RecordKind::Kernel {
                            consts: consts.into_iter().map(|c| c.recipe).collect(),
                            thunk,
                            chunks: chunk_records,
                        },
                        label: p.kernel.fn_name.clone(),
                        bytes: c.bytes,
                        align: c.align,
                        relocs,
                        callees,
                        kernel: p.kernel,
                    });
                    self.records.insert(p.id, record);
                }
                PendingOf::Wrapper { entry } => {
                    let relocs = self.shared.record_relocs(
                        &c.relocs,
                        p.id,
                        None,
                        &[],
                        &[],
                        &self.records,
                        &mut callees,
                    )?;
                    let record = Arc::new(BodyRecord {
                        kind: RecordKind::Wrapper,
                        label: format_compact!("{}_wrap", p.kernel.fn_name)
                            .as_str()
                            .into(),
                        bytes: c.bytes,
                        align: c.align,
                        relocs,
                        callees,
                        kernel: p.kernel,
                    });
                    regions.push((record, entry));
                }
            }
        }
        let wrappers: LPooled<Vec<Arc<BodyRecord>>> =
            regions.iter().map(|(w, _)| w.clone()).collect();
        let (ptrs, code) = self.install(&wrappers)?;
        for ((wrapper, entry), fn_ptr) in regions.into_iter().zip(ptrs) {
            let linked =
                entry.set(Entry { fn_ptr, _code: code.clone(), wrapper }).is_ok();
            debug_assert!(linked, "a region links once");
        }
        Ok(())
    }

    /// Install the wrappers' record trees and return their entries and
    /// the code they live in. A failed install retires the generation; a
    /// full arena reinstalls once in a fresh one.
    fn install(
        &mut self,
        wrappers: &[Arc<BodyRecord>],
    ) -> Result<(Vec<*const u8>, Arc<CodeOwner>)> {
        let e = match self.try_install(wrappers) {
            Ok(r) => return Ok(r),
            Err(e) => e,
        };
        if !e.chain().any(|c| c.is::<ArenaExhausted>()) {
            return Err(e);
        }
        log::warn!(
            "JIT code arena exhausted: retired generation {} (freed when its last \
             kernel drops) and reinstalling in a fresh module",
            self.retired
        );
        self.try_install(wrappers)
    }

    /// Install into the current generation; a failure retires it, so a
    /// generation never holds a failed install's definitions.
    fn try_install(
        &mut self,
        wrappers: &[Arc<BodyRecord>],
    ) -> Result<(Vec<*const u8>, Arc<CodeOwner>)> {
        match self.generation.install(wrappers) {
            Ok(p) => Ok((p, self.generation.code.clone())),
            Err(e) => {
                self.generation = Generation::new(&self.shared.isa)?;
                self.retired += 1;
                Err(e)
            }
        }
    }

    /// The wrapped kernel a region restored from an image dispatches:
    /// its wrapper record installed and finalized, over the layout data
    /// the image carried.
    fn load_wrapped(
        &mut self,
        wrapper: &Arc<BodyRecord>,
        state_words: usize,
        slot_table_words: Vec<kernel_abi::SiteAnchor>,
        state_self_blocks: Vec<kernel_abi::SelfBlock>,
    ) -> Result<WrappedKernel> {
        // CR claude for claude: [perf] A warm start installs and finalizes each restored
        // region on its own: FusedKernel and SlotShare image_decode call this once per
        // region, and so does every lazily decoded body. cranelift-jit's arena never
        // extends a finalized segment, so each region costs at least a page of arena
        // and RSS plus an mprotect (and a membarrier IPI on aarch64), where a cold link
        // packs a whole batch under one finalize. With 600 one-line #[native] regions,
        // GRAPHIX_PROFILE counts 601 finalizes warm against 3 cold, and under
        // GRAPHIX_JIT_ARENA=1048576 the warm start retires two generations where the
        // cold run retires none. Queue the decoded wrappers and install them with one
        // Generation::install when the decoder session ends. probe:
        // design/review-2026-10-05/repro/f-jit-05.sh (f-jit-05)
        let (ptrs, code) = self.install(std::slice::from_ref(wrapper))?;
        let entry = Entry { fn_ptr: ptrs[0], _code: code, wrapper: wrapper.clone() };
        Ok(WrappedKernel {
            entry: Arc::new(OnceLock::from(entry)),
            state_words,
            slot_table_words,
            state_self_blocks,
        })
    }
}

struct CachedBody {
    /// The body's id in [`Names`].
    func_id: FuncId,
    signature: Signature,
    /// Filled when the body is emitted. `None` at a caller's emission
    /// is a self-call, which roots a per-activation block tree.
    site_layout: Option<SiteLayout>,
    /// Holds the Arc so its pointer cannot be reused by a later allocation.
    _kernel: Arc<KernelSig>,
}

/// Emit `kernel` and its callees by walking their Nodes' `emit_clif`,
/// for the next link to compile. The parent emits from `root`; each
/// callee emits from its `callee_bodies` entry with its own lambda and
/// builtin sites. A callee without a recorded body fails the whole
/// region.
pub(crate) fn compile_kernel_with_callees_direct<R: Rt, E: UserEvent>(
    em: &mut Emission,
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
            params: &[],
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
                        params: &cb.params,
                    },
                ))
            })
            .collect();
    let emitters: LPooled<AHashMap<KernelKey, BodySource>> = callee_emitters
        .iter()
        .map(|(key, em, spec)| (*key, BodySource { spec: *spec, hook: em }))
        .collect();
    compile_region(em, kernel, &parent, callees, &emitters)
}

fn compile_region(
    em: &mut Emission,
    kernel: &Arc<KernelSig>,
    parent: &BodySource,
    callees: &[(KernelKey, Arc<KernelSig>)],
    emitters: &AHashMap<KernelKey, BodySource>,
) -> Result<WrappedKernel> {
    let mut build_profile = profile::phase(Phase::JitBuild);
    let mut fresh: SmallVec<[(CacheKey, Arc<KernelSig>); 8]> = SmallVec::new();
    let mark = em.pending.len();
    let r = compile_region_inner(em, kernel, parent, callees, emitters, &mut fresh);
    if r.is_err() {
        profile::failed(&mut build_profile);
        // A fresh entry of a failed region would hand out an id that no
        // record answers.
        em.pending.truncate(mark);
        for (key, _) in fresh.iter() {
            em.caches.by_kernel.remove(key);
        }
    }
    r
}

/// `fresh` collects the cache entries the region declares, in
/// declaration order.
fn compile_region_inner(
    em: &mut Emission,
    kernel: &Arc<KernelSig>,
    parent: &BodySource,
    callees: &[(KernelKey, Arc<KernelSig>)],
    emitters: &AHashMap<KernelKey, BodySource>,
    fresh: &mut SmallVec<[(CacheKey, Arc<KernelSig>); 8]>,
) -> Result<WrappedKernel> {
    // Phase 1: declare every kernel in the closure. A callee body with no
    // sibling sites keys on layout 0; the parent is fresh per attempt and
    // never cached.
    let layout_id =
        em.caches.intern_layout(callees.iter().map(|(key, _)| *key).collect());
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
    let parent_sig = kernel_signature(&*em.shared.isa, kernel)?;
    let parent_fid = em.names.local(&parent_sig);
    funcids.push((parent_key, (parent_fid, parent_sig)));
    for (key, k) in callees {
        let key = CacheKey { kernel: *key, layout: layout_of(*key) };
        let entry = match em.caches.by_kernel(&key) {
            Some(e) => {
                if let Some(l) = e.site_layout.as_ref() {
                    callee_layouts.insert(key.kernel, l.clone());
                }
                (e.func_id, e.signature.clone())
            }
            None => {
                let sig = kernel_signature(&*em.shared.isa, k)?;
                let fid = em.names.local(&sig);
                em.caches.by_kernel.insert(
                    key,
                    CachedBody {
                        func_id: fid,
                        signature: sig.clone(),
                        _kernel: k.clone(),
                        site_layout: None,
                    },
                );
                fresh.push((key, k.clone()));
                (fid, sig)
            }
        };
        funcids.push((key.kernel, entry));
    }
    // Phase 2: emit the fresh callee bodies in topological order over
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
        let (emitted, pending) =
            emit_kernel_body(em, k, &funcids, body, &callee_layouts)?;
        em.pending.push(pending);
        callee_layouts.insert(key.kernel, emitted.site_layout.clone());
        if let Some(cached) = em.caches.by_kernel.get_mut(key) {
            cached.site_layout = Some(emitted.site_layout);
        }
    }
    let (emitted, pending) =
        emit_kernel_body(em, kernel, &funcids, parent, &callee_layouts)?;
    em.pending.push(pending);
    debug_assert_eq!(
        emitted.site_layout.words, 0,
        "a region parent claims no site words"
    );
    // Phase 3: the parent's wrapper.
    let (wrapper_id, func) = emit_wrapper(em, kernel, parent_fid)?;
    let entry = Arc::new(OnceLock::new());
    em.pending.push(Pending {
        id: wrapper_id,
        func,
        kernel: kernel.clone(),
        of: PendingOf::Wrapper { entry: entry.clone() },
    });
    Ok(WrappedKernel {
        entry,
        state_words: emitted.state_words,
        slot_table_words: emitted.slot_table_words,
        state_self_blocks: emitted.state_self_blocks,
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

/// Emit `kernel`'s body, named by its pre-declared id, for the next
/// link. `funcids` must hold the kernel itself and every callee its
/// lambda call sites reference.
fn emit_kernel_body(
    em: &mut Emission,
    kernel: &Arc<KernelSig>,
    funcids: &[(KernelKey, (FuncId, Signature))],
    body_emitter: &BodySource,
    callee_layouts: &AHashMap<KernelKey, SiteLayout>,
) -> Result<(EmittedBody, Pending)> {
    let mut clif_profile = profile::phase(Phase::Clif);
    let self_key = kernel_abi::kernel_key(kernel);
    let (func_id, sig) =
        funcids.iter().find(|(p, _)| *p == self_key).map(|(_, e)| e.clone()).ok_or_else(
            || {
                anyhow!(
                    "emit_kernel_body: missing FuncId for kernel `{}` (phase-1 declare \
                     must have populated `funcids` first)",
                    kernel.fn_name
                )
            },
        )?;
    // The set of callees is the body's lambda sites plus its self-call,
    // keyed by kernel identity.
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
    let self_thunk_id = body_emitter.spec.self_call().map(|_| {
        let tsig = trampoline_signature(&*em.shared.isa);
        em.names.local(&tsig)
    });
    let consts: RefCell<Vec<EmitConst>> = RefCell::new(Vec::new());
    let chunks: RefCell<Vec<ChunkFn>> = RefCell::new(Vec::new());
    let mut emitted = None;
    let func = em.build(func_id, sig.clone(), |names, helpers, b| {
        // Import in `funcids` order so the funcref numbering is deterministic.
        let mut callee_refs: BTreeMap<KernelKey, FuncRef> = BTreeMap::new();
        let mut callee_ids: BTreeMap<KernelKey, FuncId> = BTreeMap::new();
        for (key, (fid, _)) in funcids {
            if callee_keys.contains(key) {
                callee_refs.insert(*key, names.import_func(*fid, b.func));
                callee_ids.insert(*key, *fid);
            }
        }
        if callee_refs.len() != callee_keys.len() {
            bail!(
                "emit_kernel_body: kernel `{}` calls a kernel with no entry in funcids",
                kernel.fn_name
            );
        }
        let self_thunk = self_thunk_id.map(|tid| names.import_func(tid, b.func));
        let names = RefCell::new(names);
        emitted = Some(compile_into_function(
            b,
            kernel,
            Callees {
                refs: &callee_refs,
                ids: &callee_ids,
                thunk: self_thunk,
                thunk_id: self_thunk_id,
            },
            helpers,
            &consts,
            &chunks,
            &names,
            body_emitter,
            callee_layouts,
        )?);
        Ok(())
    });
    let func = match func {
        Ok(func) => func,
        Err(e) => {
            profile::failed(&mut clif_profile);
            return Err(e);
        }
    };
    let emitted: EmittedBody = emitted.expect("emitted on success");
    let chunks = chunks.into_inner();
    let thunk = match self_thunk_id {
        None => None,
        Some(tid) => Some((tid, emit_trampoline(em, tid, func_id, &sig, false)?)),
    };
    if crate::dbgenv::graphix_dbg_kernels() {
        eprintln!(
            "KERNEL DEFINED {}: state_words={} site_words={} self_blocks={}",
            kernel.fn_name,
            emitted.state_words,
            emitted.site_layout.words,
            emitted.site_layout.self_blocks.len()
        );
    }
    let pending = Pending {
        id: func_id,
        func,
        kernel: kernel.clone(),
        of: PendingOf::Body { consts: consts.into_inner(), thunk, chunks },
    };
    Ok((emitted, pending))
}

/// Emit the `(args, out)` trampoline `id` into `target`: load each of
/// `target_sig`'s params from `args` at an 8-byte stride (the wire
/// layout of [`KernelSig::abi_params`]), call it, and store its two
/// result words to `out`. A wrapper also bumps the harness's invocation
/// counter in debug builds.
fn emit_trampoline(
    em: &mut Emission,
    id: FuncId,
    target: FuncId,
    target_sig: &Signature,
    #[cfg_attr(not(debug_assertions), expect(unused_variables))] wrapper: bool,
) -> Result<Function> {
    let sig = trampoline_signature(&*em.shared.isa);
    let func = em.build(id, sig, |names, helpers, b| {
        let target_ref = names.import_func(target, b.func);
        #[cfg(debug_assertions)]
        let record_ref = match wrapper {
            true => {
                let fid = helpers
                    .ids
                    .get("graphix_record_jit_invocation")
                    .copied()
                    .ok_or_else(|| {
                        anyhow!("missing graphix_record_jit_invocation FuncId")
                    })?;
                Some(names.import_func(fid, b.func))
            }
            false => None,
        };
        #[cfg(not(debug_assertions))]
        let _ = helpers;
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
        // CR claude for eric: [risk] These narrow loads read a value's low bytes only
        // on a little-endian host. On a big-endian one (cranelift-native targets s390x,
        // and compile_top gates fusion only off Windows, lib.rs:2179) an i32 input of 5
        // loads as 0. The comment above holds only on little-endian, and so does the
        // value-word encoding both engines share: TagValue::masked (tval.rs:265)
        // transmutes value_words' widened payload back into a Value, so the node-walk
        // would misread every narrow scalar there too, and fixing this load alone fixes
        // nothing. No big-endian target is supported; a crate-level `compile_error!`
        // under `cfg(target_endian = "big")` would say so instead of computing wrong
        // values. (f-jit-10)
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
        Ok(())
    })?;
    Ok(func)
}

/// The region's `(args, out)` wrapper around its parent body
/// `typed_func_id`.
fn emit_wrapper(
    em: &mut Emission,
    kernel: &Arc<KernelSig>,
    typed_func_id: FuncId,
) -> Result<(FuncId, Function)> {
    let wrapper_id = em.names.local(&trampoline_signature(&*em.shared.isa));
    let kernel_sig = kernel_signature(&*em.shared.isa, kernel)?;
    let func = emit_trampoline(em, wrapper_id, typed_func_id, &kernel_sig, true)
        .context("wrapper")?;
    Ok((wrapper_id, func))
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
    // CR claude for claude: [structure] This match repeats value_words' scalar widening
    // (tval.rs:108-117: sign-extend, zero-extend, float bits), and the two must agree.
    // Kernel constants (scalar.rs:247) and the runtime's scalar staging (kernel.rs:265)
    // use this table, while every other seam uses value_words, whose doc points back
    // here. Once the prim check passes, `value_words(v)[1]` is the same word for every
    // scalar variant, so the body can be `(scalar_prim_of_value(v) ==
    // Some(prim)).then(|| value_words(v)[1])`. (f-jit-15)
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
