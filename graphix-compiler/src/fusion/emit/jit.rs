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
        kernel_abi::{self, AbiParamKind, AbiReturn, KernelSig, PrimType},
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

/// Owns the Cranelift JIT module plus reusable per-function builder
/// contexts. One `JitCtx` can compile many kernels; the compiled
/// function pointers live on it and stay valid until the ctx is
/// dropped.
pub struct JitCtx {
    module: JITModule,
    builder_ctx: FunctionBuilderContext,
    func_ctx: Context,
    /// Symbol suffix; one graphix name can occur in several fused lambdas.
    counter: u32,
    /// FuncIds for the `emit_helpers::*` runtime helpers, declared once.
    helper_ids: HelperFuncIds,
}

impl JitCtx {
    pub fn new() -> Result<Self> {
        let mut flag_builder = settings::builder();
        // cranelift-jit requires PIC off.
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
        // One contiguous reservation: colocated (Linkage::Local) calls use a
        // ±2GiB PC-relative relocation and finalize panics if two functions
        // land further apart. `GRAPHIX_JIT_ARENA` (bytes) overrides the size.
        const JIT_ARENA_RESERVE: usize = 256 * 1024 * 1024;
        static ARENA_SIZE: std::sync::LazyLock<usize> =
            std::sync::LazyLock::new(|| match std::env::var("GRAPHIX_JIT_ARENA") {
                Ok(v) => v.parse().unwrap_or(JIT_ARENA_RESERVE),
                Err(_) => JIT_ARENA_RESERVE,
            });
        builder.memory_provider(Box::new(
            cranelift_jit::ArenaMemoryProvider::new_with_size(*ARENA_SIZE)
                .map_err(|e| anyhow!("jit arena reservation failed: {e}"))?,
        ));
        // Helpers resolve by pointer under the registry's symbol name, never
        // through the process symbol table.
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
    match kernel.abi_return() {
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

/// Print the CLIF to stderr when `GRAPHIX_DUMP_CLIF` is set. Baked
/// pointer constants vary run to run; normalize large `iconst`
/// immediates before diffing two dumps.
fn maybe_dump_clif(func: &cranelift_codegen::ir::Function, label: &str) {
    static DUMP: std::sync::LazyLock<bool> =
        std::sync::LazyLock::new(|| std::env::var_os("GRAPHIX_DUMP_CLIF").is_some());
    if *DUMP {
        eprintln!(";; clif {label}\n{}", func.display());
    }
}

/// A compiled kernel behind the uniform [`WrapperFn`] convention:
/// `args` points at the context words then a `(disc, payload)` pair
/// per parameter, `out` receives the result's `(disc, payload)` pair.
/// [`pack_value_to_u64`] / [`unpack_u64_to_value`] do the Rust-side
/// packing.
pub struct WrappedKernel {
    /// Cast through [`Self::fn_ptr`].
    pub wrapper_fn_ptr: *const u8,
    /// `Some` when the kernel owns a private module; `None` when the
    /// `ExecCtx`'s [`Jit`] keeps the code mapped.
    _ctx: Option<JitCtx>,
    /// Per-instance state words the root body claimed. The runtime
    /// `Kernel` passes a zeroed buffer of this size in wire slot 1.
    pub state_words: usize,
    /// The root body's per-slot state-table anchors: each word holds a
    /// `Box<Vec<u64>>` chain owned by `graphix_slot_state_table` and
    /// freed by `Kernel`'s `Drop`.
    pub slot_table_words: Vec<kernel_abi::SiteAnchor>,
    /// The body's own per-call-site block layout. A caller supplies the
    /// block; for a region parent the runtime `Kernel` supplies it from
    /// its own per-instance storage.
    pub(crate) own_site: Option<SiteLayout>,
    /// Per-activation block-tree roots living in the parent's state
    /// buffer; `Kernel` frees and resets them with its own site block.
    pub(crate) state_self_blocks: Vec<kernel_abi::SelfBlock>,
    /// Strings the code references by stable `*const ArcStr`; they must
    /// outlive the compiled function.
    _strings: KernelStrings,
    /// Datetime/duration constants referenced by stable `*const Value`;
    /// same lifetime as `_strings`.
    _values: KernelValues,
}

unsafe impl Send for WrappedKernel {}
unsafe impl Sync for WrappedKernel {}

/// The uniform Rust-side signature the wrapper presents.
pub type WrapperFn = unsafe extern "C" fn(args: *const u64, out: *mut u64);

impl WrappedKernel {
    /// `wrapper_fn_ptr` must come from a successful compile; any other
    /// pointer is UB.
    pub unsafe fn fn_ptr(&self) -> WrapperFn {
        unsafe { std::mem::transmute(self.wrapper_fn_ptr) }
    }
}

/// The per-`ExecCtx` JIT module. Kernels call each other with direct
/// CLIF calls, so they share one module that lives as long as the
/// `ExecCtx`.
///
/// `by_kernel` keys on `(Arc<KernelSig> pointer, base, region layout)`:
/// a body bakes its sibling kernels' FuncIds, which depend on the
/// region's ordered kernel list, so only identical layouts may share a
/// compilation. A body with no sibling sites uses layout 0.
pub struct Jit {
    /// Boxed: cranelift's `Context` is ~5KB and this rides every
    /// `async fn` that moves a `GXConfig`.
    ctx: Box<JitCtx>,
    /// The entry holds the `Arc` so the pointer key cannot be reused by
    /// a later allocation.
    by_kernel: BTreeMap<(usize, u32, u32), CachedKernel>,
    /// Region layout → id, from 1; 0 is the layout-independent id.
    layouts: BTreeMap<Vec<(usize, u32)>, u32>,
}

impl Jit {
    /// Errs if cranelift cannot target the host ISA.
    pub fn new() -> Result<Self> {
        Ok(Self {
            ctx: Box::new(JitCtx::new()?),
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
    /// See [`WrappedKernel::slot_table_words`]; filled in phase 2.
    slot_table_words: Vec<kernel_abi::SiteAnchor>,
    /// See [`WrappedKernel::state_self_blocks`]; filled in phase 2.
    state_self_blocks: Vec<kernel_abi::SelfBlock>,
    /// Filled when the body is defined. `None` at a caller's emission
    /// is a recursive back-edge, which passes 0.
    site_layout: Option<SiteLayout>,
    /// Leaves whose addresses the code baked in; live with the code.
    _site_leaves: Vec<std::sync::Arc<kernel_abi::SiteLeaf>>,
    /// Holds the Arc so its pointer cannot be reused by a later allocation.
    _kernel: std::sync::Arc<KernelSig>,
    /// Strings the code references by pointer; must outlive the code.
    /// Moved in by `define_kernel_body`.
    _strings: KernelStrings,
    /// Datetime/duration constants; same lifetime as `_strings`.
    _values: KernelValues,
    /// See [`WrappedKernel::state_words`]; kept so a cached parent still
    /// sizes its runtime buffer.
    state_words: usize,
}

unsafe impl Send for Jit {}

/// Compile `kernel` and its callees by walking their Nodes' `emit_clif`.
/// The parent emits from `root`; each callee emits from its
/// `callee_bodies` entry (keyed by `Arc::as_ptr`) with its own lambda
/// and builtin sites. A callee without a recorded body fails the
/// whole region.
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
) -> Result<WrappedKernel> {
    let parent = NodeBodyEmitter { root, return_type: &kernel.return_type };
    let parent_spec = BodySpec {
        builtin_apply_sites: Some(apply_sites),
        lambda_call_sites: Some(lambda_sites),
        // `None` for a region parent; the collection callback path's parent
        // is a lambda kernel with its own self info.
        self_call: parent_self_call,
        type_env: Some(type_env),
        allow_state: true,
    };
    // A callee that is the parent shares the parent's body, as in phase 1.
    let parent_ptr = kernel_abi::kernel_key(kernel);
    let callee_emitters: Vec<(usize, NodeBodyEmitter<R, E>, BodySpec)> = callees
        .iter()
        .filter_map(|(key, k)| {
            let key = *key;
            if key == parent_ptr {
                return None;
            }
            let cb = callee_bodies.get(&key)?;
            Some((
                key,
                NodeBodyEmitter { root: cb.body, return_type: &k.return_type },
                BodySpec {
                    builtin_apply_sites: Some(&cb.apply_sites),
                    lambda_call_sites: Some(&cb.sites),
                    self_call: cb.self_call.as_ref(),
                    type_env: Some(type_env),
                    allow_state: false,
                },
            ))
        })
        .collect();
    let mut emitters: BTreeMap<usize, BodySource> = callee_emitters
        .iter()
        .map(|(key, em, spec)| (*key, BodySource { spec: *spec, hook: em }))
        .collect();
    emitters.insert(parent_ptr, BodySource { spec: parent_spec, hook: &parent });
    compile_kernel_with_callees_impl(jit, kernel, callees, &emitters)
}

fn compile_kernel_with_callees_impl(
    jit: &mut Jit,
    kernel: &std::sync::Arc<KernelSig>,
    callees: &[(usize, std::sync::Arc<KernelSig>)],
    emitters: &BTreeMap<usize, BodySource>,
) -> Result<WrappedKernel> {
    let mut to_define: poolshark::local::LPooled<
        Vec<(std::sync::Arc<KernelSig>, u32, u32)>,
    > = poolshark::local::LPooled::take();
    let mut defined: poolshark::local::LPooled<Vec<(usize, u32, u32)>> =
        poolshark::local::LPooled::take();
    let r = compile_kernel_with_callees_inner(
        jit,
        kernel,
        callees,
        emitters,
        &mut to_define,
        &mut defined,
    );
    if r.is_err() {
        // Evict the fresh entries (a stale one would hand out an undefined
        // FuncId) and trap-stub every declared-but-undefined body: the next
        // `finalize_definitions` panics on an undefined Local symbol.
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

/// Define `fid` as a body that traps, for a kernel abandoned after
/// declaration: the shared module must not carry an undefined Local
/// symbol into the next `finalize_definitions`. Never executed.
fn define_stub_body(jit: &mut JitCtx, fid: FuncId, sig: &Signature) -> Result<()> {
    use cranelift_codegen::ir::TrapCode;
    // The failed build may have left `func_ctx` mid-function.
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
) -> Result<WrappedKernel> {
    // Phase 1: declare every kernel in the closure. `funcids` keys on
    // kernel identity, so a self-recursive callee lands on the parent's
    // entry.
    let parent_ptr = kernel_abi::kernel_key(kernel);
    // A body with no sibling sites keys on layout 0.
    let layout_id = {
        let mut layout: Vec<(usize, u32)> = Vec::with_capacity(callees.len() + 1);
        layout.push((parent_ptr, 0));
        for (ptr, _) in callees {
            if *ptr == parent_ptr {
                continue;
            }
            layout.push((*ptr, 0));
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
        if !ext_sites { 0 } else { layout_id }
    };
    let parent_layout = layout_of(kernel);
    // Insertion order fixes the funcref numbering; a pointer-ordered map
    // makes it ASLR-dependent.
    let mut funcids: poolshark::local::LPooled<Vec<(usize, (FuncId, Signature))>> =
        poolshark::local::LPooled::take();
    // Seeded only from this region's own cache keys: another (base, layout)
    // variant of a body may have a different SiteLayout, and sizing blocks
    // from it is an out-of-bounds write.
    let mut callee_layouts: BTreeMap<usize, SiteLayout> = BTreeMap::new();
    let seed_layout = |jit: &Jit,
                       callee_layouts: &mut BTreeMap<usize, SiteLayout>,
                       ptr: usize,
                       base: u32,
                       layout: u32| {
        if let Some(l) =
            jit.by_kernel.get(&(ptr, base, layout)).and_then(|e| e.site_layout.as_ref())
        {
            callee_layouts.insert(ptr, l.clone());
        }
    };
    let parent_entry = ensure_declared(jit, kernel, 0, parent_layout, to_define)?;
    funcids.push((parent_ptr, parent_entry.clone()));
    seed_layout(jit, &mut callee_layouts, parent_ptr, 0, parent_layout);
    for (ptr, k) in callees {
        if *ptr == parent_ptr {
            continue;
        }
        let base = 0;
        let layout = layout_of(k);
        let entry = ensure_declared(jit, k, base, layout, to_define)?;
        funcids.push((*ptr, entry));
        seed_layout(jit, &mut callee_layouts, *ptr, base, layout);
    }
    // Phase 2: define the fresh bodies in topological order over the
    // static call edges, callees first, so a caller can read its callees'
    // `SiteLayout`s. The only layout missing at definition is a self-call's.
    let def_order: Vec<usize> = {
        use std::collections::BTreeMap;
        let mut pos: BTreeMap<usize, usize> = BTreeMap::new();
        let mut by_ptr: BTreeMap<usize, smallvec::SmallVec<[usize; 2]>> = BTreeMap::new();
        for (i, (k, _, _)) in to_define.iter().enumerate() {
            let ptr = std::sync::Arc::as_ptr(k) as usize;
            pos.entry(ptr).or_insert(i);
            by_ptr.entry(ptr).or_default().push(i);
        }
        let edges_of = |p: usize| -> smallvec::SmallVec<[usize; 8]> {
            let mut out: smallvec::SmallVec<[usize; 8]> = emitters
                .get(&p)
                .and_then(|e| e.spec.lambda_call_sites)
                .map(|m| {
                    m.values()
                        .map(|info| kernel_abi::kernel_key(&info.kernel))
                        .filter(|q| *q != p && pos.contains_key(q))
                        .collect()
                })
                .unwrap_or_default();
            out.sort_by_key(|q| pos[q]);
            out.dedup();
            out
        };
        let mut done: BTreeMap<usize, ()> = BTreeMap::new();
        let mut order: Vec<usize> = Vec::with_capacity(to_define.len());
        for (k, _, _) in to_define.iter() {
            let root = std::sync::Arc::as_ptr(k) as usize;
            if done.contains_key(&root) {
                continue;
            }
            done.insert(root, ());
            let mut stack: Vec<(usize, smallvec::SmallVec<[usize; 8]>, usize)> =
                vec![(root, edges_of(root), 0)];
            while let Some((ptr, es, i)) = stack.pop() {
                if i < es.len() {
                    let q = es[i];
                    stack.push((ptr, es, i + 1));
                    if !done.contains_key(&q) {
                        done.insert(q, ());
                        stack.push((q, edges_of(q), 0));
                    }
                } else {
                    order.extend(by_ptr.get(&ptr).into_iter().flatten().copied());
                }
            }
        }
        debug_assert_eq!(order.len(), to_define.len());
        order
    };
    for ti in def_order {
        let (k, base, layout) = &to_define[ti];
        let ptr = std::sync::Arc::as_ptr(k) as usize;
        let body: &BodySource = emitters.get(&ptr).ok_or_else(|| {
            anyhow!(
                "no body emitter recorded for kernel `{}` — \
                     discovery must record every callee body",
                k.fn_name
            )
        })?;
        let db = define_kernel_body(&mut jit.ctx, k, &funcids, body, &callee_layouts)?;
        defined.push((ptr, *base, *layout));
        if let Some(cached) = jit.by_kernel.get_mut(&(ptr, *base, *layout)) {
            cached._strings = db.strings;
            cached._values = db.values;
            cached.state_words = db.state_words;
            cached.slot_table_words = db.slot_table_words;
            cached.state_self_blocks = db.state_self_blocks;
            callee_layouts.insert(ptr, db.site_layout.clone());
            cached.site_layout = Some(db.site_layout);
            cached._site_leaves = db.site_leaves;
        }
    }
    // Phase 3: the parent's wrapper, then finalize.
    let wrapper_id = define_wrapper(&mut jit.ctx, kernel, parent_entry.0)?;
    jit.ctx
        .module
        .finalize_definitions()
        .context("finalize_definitions (per-context jit)")?;
    let wrapper_fn_ptr = jit.ctx.module.get_finalized_function(wrapper_id);
    // The code and its string tables are owned by `jit.by_kernel`; the
    // parent's state footprint comes from its cache entry so a cached
    // parent sizes its buffer correctly.
    let (state_words, slot_table_words, state_self_blocks, own_site) = jit
        .by_kernel
        .get(&(parent_ptr, 0, parent_layout))
        .map(|e| {
            (
                e.state_words,
                e.slot_table_words.clone(),
                e.state_self_blocks.clone(),
                e.site_layout.clone(),
            )
        })
        .ok_or_else(|| anyhow!("parent kernel missing from the by_kernel cache"))?;
    Ok(WrappedKernel {
        wrapper_fn_ptr,
        _ctx: None,
        _strings: KernelStrings::empty(),
        _values: KernelValues::empty(),
        state_words,
        slot_table_words,
        state_self_blocks,
        own_site,
    })
}

/// Ensure `k` has a `FuncId` under this layout; a fresh declaration is
/// queued on `to_define` for phase 2.
fn ensure_declared(
    jit: &mut Jit,
    k: &std::sync::Arc<KernelSig>,
    base: u32,
    layout: u32,
    to_define: &mut Vec<(std::sync::Arc<KernelSig>, u32, u32)>,
) -> Result<(FuncId, Signature)> {
    let key = (std::sync::Arc::as_ptr(k) as usize, base, layout);
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
            // Filled by `define_kernel_body`.
            _strings: KernelStrings::empty(),
            _values: KernelValues::empty(),
            state_self_blocks: Vec::new(),
            state_words: 0,
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
    slot_table_words: Vec<kernel_abi::SiteAnchor>,
    state_self_blocks: Vec<kernel_abi::SelfBlock>,
    site_layout: SiteLayout,
    site_leaves: Vec<std::sync::Arc<kernel_abi::SiteLeaf>>,
}

/// Declare the `fn(args: *const u64, out: *mut u64)` thunk
/// `graphix_grow_stack` re-enters a recursive kernel through on a fresh
/// stack segment. Declared before the body (which takes its address),
/// defined after it (it calls the body).
fn declare_spill_thunk(jit: &mut JitCtx, fn_name: &str) -> Result<FuncId> {
    use cranelift_codegen::ir::{AbiParam, Signature, types};
    let symbol = jit.next_symbol(&format!("{fn_name}__spill"));
    let mut sig = Signature::new(jit.module.isa().default_call_conv());
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    jit.module
        .declare_function(&symbol, Linkage::Local, &sig)
        .context("declare_function (spill thunk)")
}

/// Load each kernel parameter from `args` at an 8-byte stride, call the
/// kernel, store its two result words to `out`.
fn define_spill_thunk(
    jit: &mut JitCtx,
    thunk_id: FuncId,
    kernel_id: FuncId,
    kernel_sig: &cranelift_codegen::ir::Signature,
) -> Result<()> {
    use cranelift_codegen::ir::{AbiParam, InstBuilder, MemFlags, Signature, types};
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    let mut sig = Signature::new(jit.module.isa().default_call_conv());
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    jit.func_ctx.func.signature = sig;
    jit.func_ctx.func.name =
        cranelift_codegen::ir::UserFuncName::user(0, thunk_id.as_u32());
    let kernel_ref = jit.module.declare_func_in_func(kernel_id, &mut jit.func_ctx.func);
    let mut b = FunctionBuilder::new(&mut jit.func_ctx.func, &mut jit.builder_ctx);
    let entry = b.create_block();
    b.append_block_params_for_function_params(entry);
    b.switch_to_block(entry);
    b.seal_block(entry);
    let args = b.block_params(entry)[0];
    let out = b.block_params(entry)[1];
    let vals: Vec<_> = kernel_sig
        .params
        .iter()
        .enumerate()
        .map(|(i, p)| {
            b.ins().load(p.value_type, MemFlags::trusted(), args, (8 * i) as i32)
        })
        .collect();
    let call = b.ins().call(kernel_ref, &vals);
    let results = b.inst_results(call).to_vec();
    for (i, r) in results.iter().enumerate() {
        b.ins().store(MemFlags::trusted(), *r, out, (8 * i) as i32);
    }
    b.ins().return_(&[]);
    b.finalize();
    jit.module
        .define_function(thunk_id, &mut jit.func_ctx)
        .context("define_function (spill thunk)")
}

/// Compile `kernel`'s body and define it on its pre-declared `FuncId`.
/// `funcids` must hold the kernel itself and every callee its lambda
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
    let self_thunk_id = match body_emitter.spec.self_call {
        Some(_) => Some(declare_spill_thunk(jit, &kernel.fn_name)?),
        None => None,
    };
    let kernel_sig = sig.clone();
    // A prior failed compile may have left `func_ctx` dirty, and
    // `FunctionBuilder::new` asserts it is empty.
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    jit.func_ctx.func.signature = sig;
    jit.func_ctx.func.name =
        cranelift_codegen::ir::UserFuncName::user(0, func_id.as_u32());
    let (
        strings,
        values,
        state_words,
        slot_table_words,
        state_self_blocks,
        site_layout,
        site_leaves,
    ) = {
        // Callee FuncRefs are declared before the FunctionBuilder borrows
        // `func_ctx.func`. The set is the body's lambda sites plus its
        // self-call, keyed by kernel identity.
        let needed: poolshark::local::LPooled<nohash::IntSet<usize>> = {
            let mut s: poolshark::local::LPooled<nohash::IntSet<usize>> = body_emitter
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
        // Import in `funcids` order so the funcref numbering is deterministic.
        let mut callee_refs: BTreeMap<usize, FuncRef> = BTreeMap::new();
        for (ptr, (fid, _)) in funcids {
            if !needed.contains(ptr) {
                continue;
            }
            let fref = jit.module.declare_func_in_func(*fid, &mut jit.func_ctx.func);
            callee_refs.insert(*ptr, fref);
        }
        let self_thunk = self_thunk_id
            .map(|tid| jit.module.declare_func_in_func(tid, &mut jit.func_ctx.func));
        if callee_refs.len() != needed.len() {
            return Err(anyhow!(
                "define_kernel_body: kernel `{}` calls a kernel with \
                     no entry in funcids",
                kernel.fn_name
            ));
        }
        // Filled during emission; returned so the baked addresses outlive
        // the compiled code.
        let lazy_strings: std::cell::RefCell<Vec<Box<ArcStr>>> =
            std::cell::RefCell::new(Vec::new());
        let lazy_values: std::cell::RefCell<Vec<Box<Value>>> =
            std::cell::RefCell::new(Vec::new());
        let lazy_keep: std::cell::RefCell<Vec<Box<dyn std::any::Any + Send + Sync>>> =
            std::cell::RefCell::new(Vec::new());
        let lazy_site_leaves: std::cell::RefCell<
            Vec<std::sync::Arc<kernel_abi::SiteLeaf>>,
        > = std::cell::RefCell::new(Vec::new());
        let helper_refs =
            declare_helpers(&mut jit.module, &mut jit.func_ctx.func, &jit.helper_ids);
        let mut builder =
            FunctionBuilder::new(&mut jit.func_ctx.func, &mut jit.builder_ctx);
        let (state_words, slot_table_words, state_self_blocks, site_layout) =
            compile_into_function(
                &mut builder,
                kernel,
                &callee_refs,
                self_thunk,
                &helper_refs,
                &lazy_strings,
                &lazy_values,
                &lazy_keep,
                body_emitter,
                callee_layouts,
                &lazy_site_leaves,
            )?;
        builder.finalize();
        maybe_dump_clif(&jit.func_ctx.func, &kernel.fn_name);
        (
            KernelStrings::empty().with_lazy(lazy_strings.into_inner()),
            KernelValues::empty()
                .with_lazy(lazy_values.into_inner(), lazy_keep.into_inner()),
            state_words,
            slot_table_words,
            state_self_blocks,
            site_layout,
            lazy_site_leaves.into_inner(),
        )
    };
    jit.module
        .define_function(func_id, &mut jit.func_ctx)
        .context("define_function (shared body)")?;
    if let Some(tid) = self_thunk_id {
        define_spill_thunk(jit, tid, func_id, &kernel_sig)?;
    }
    if crate::dbgenv::graphix_dbg_kernels() {
        eprintln!(
            "KERNEL DEFINED {}: state_words={} site_words={} self_blocks={}",
            kernel.fn_name,
            state_words,
            site_layout.words,
            site_layout.self_blocks.len()
        );
    }
    kernel.defined.store(true, std::sync::atomic::Ordering::Relaxed);
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    Ok(DefinedBody {
        strings,
        values,
        state_words,
        slot_table_words,
        state_self_blocks,
        site_layout,
        site_leaves,
    })
}

/// Define the `(args, out)` wrapper: load each arg from its u64 slot
/// at the kernel's CLIF type, call the kernel, store the result words.
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
    // A failure here leaves `wrapper_id` declared but undefined; stub it
    // so the next `finalize_definitions` does not panic.
    match define_wrapper_body(jit, kernel, typed_func_id, wrapper_id, &sig, &symbol) {
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
    wrapper_id: FuncId,
    sig: &Signature,
    symbol: &str,
) -> Result<()> {
    // A prior failed compile may have left `func_ctx` dirty.
    jit.module.clear_context(&mut jit.func_ctx);
    jit.builder_ctx = FunctionBuilderContext::new();
    jit.func_ctx.func.signature = sig.clone();
    jit.func_ctx.func.name =
        cranelift_codegen::ir::UserFuncName::user(0, wrapper_id.as_u32());

    {
        let typed_ref =
            jit.module.declare_func_in_func(typed_func_id, &mut jit.func_ctx.func);
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

        // The test harness's `jit` mode reads this counter to verify the
        // JIT ran.
        #[cfg(debug_assertions)]
        {
            b.ins().call(record_ref, &[]);
        }

        let args_ptr = b.block_params(entry)[0];
        let out_ptr = b.block_params(entry)[1];

        // Loading a scalar payload at its narrow CLIF type is sound because
        // the packer stores the sign/zero-extended form. `wire_slot` is
        // already offset past the context words.
        let mut typed_args: poolshark::local::LPooled<Vec<cranelift_codegen::ir::Value>> =
            poolshark::local::LPooled::take();
        for i in 0..kernel_abi::CTX_WIRE_SLOTS {
            let v =
                b.ins().load(types::I64, MemFlags::trusted(), args_ptr, (i as i32) * 8);
            typed_args.push(v);
        }
        for d in kernel.abi_params() {
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

/// Pack a scalar [`Value`] into a u64 slot as `prim`. `None` when `v`
/// is not a scalar of `prim`'s shape; the caller substitutes the
/// tainted placeholder. `Z32`/`Z64`/`V32`/`V64` pack as their
/// fixed-width prim.
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

/// Unpack a u64 slot into the scalar [`Value`] of `prim`.
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
