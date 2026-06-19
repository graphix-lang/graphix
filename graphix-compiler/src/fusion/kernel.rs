//! Runtime wrapper around a JIT-compiled kernel.
//!
//! Fusion has exactly two evaluators: the node-walk (`Box<dyn Update>`
//! graph, the canonical model) and the cranelift JIT. Fusion builds a
//! [`KernelSig`], JIT-compiles it, and:
//!
//! - JIT success → splice the native kernel + delete the original
//!   nodes. [`Kernel`] is the [`Apply<R, E>`] wrapper that drives the
//!   feeders, packs args across the JIT ABI boundary, dispatches HOF
//!   arguments via the DynCall side-channel, and unpacks the result.
//! - JIT failure → DON'T splice. The original nodes
//!   stay in the graph and run through the node-walk, the universal
//!   fallback. A [`Kernel`] cannot be constructed without a JIT
//!   wrapper, so this is structural.
//!
//! This file keeps [`Kernel`], its arg-layout / arg-packing, and the
//! [`DynCallSlot`] cross-call machinery — everything the JIT boundary
//! needs.

use crate::{
    expr::{Expr, ExprId},
    fusion::{
        emit::{pack_value_to_u64, unpack_u64_to_value, WrappedKernel},
        emit_helpers::{
            record_fusion_invocation, DynCallRet, DynDispatchHandle,
            DYNCALL_PENDING, DYN_DISPATCH_HANDLE,
        },
        kernel_abi::{self, BuiltinSlot, FnSource, KernelSig},
    },
    node::{bind::Ref, compiler::compile, lambda::LambdaDef},
    typ::FnType,
    Apply, BindId, Event, ExecCtx, LambdaId, Node, Refs, Rt, Scope, UserEvent,
};
use netidx::subscriber::Value;
use netidx_value::ValArray;
use std::sync::Arc;

// ─── Kernel: the Apply<R, E> wrapper ────────────────────────────

/// Per-DynCall-site state. For each fn-typed param of the kernel we
/// pre-allocate a slot containing the BindIds the side-channel uses,
/// the [`Ref`]-style nodes that read from those BindIds (passed as
/// `from` to the inner Apply's `update`), and a cache of the most
/// recently constructed inner Apply (invalidated when the LambdaDef
/// pointer changes).
///
/// Generic over `R, E` because the cached `Box<dyn Apply<R, E>>` and
/// the arg-ref nodes are.
pub struct DynCallSlot<R: Rt, E: UserEvent> {
    /// One BindId per callee argument. Pre-allocated at Kernel
    /// construction. The DynCall-time dispatcher writes the converted
    /// arg `Value` into `event.variables[bind_ids[i]]`; the matching
    /// `Ref` node in `arg_refs` reads it back inside the inner
    /// Apply's `update`.
    bind_ids: Vec<BindId>,
    /// Per-arg `Ref` nodes that read from `bind_ids`. Passed as the
    /// `from: &mut [Node<R, E>]` slice to the inner Apply's `update`.
    arg_refs: Vec<Node<R, E>>,
    /// Cached `(LambdaDef pointer, Apply instance)`. Invalidated when
    /// a new LambdaDef arrives (different raw pointer) — typical case
    /// is the hot loop where the same callback is reused. For
    /// pre-bound slots (`pre_bound = true`) the pointer is a stable
    /// sentinel and `dispatch` never re-inits.
    current: Option<(*const u8, Box<dyn Apply<R, E>>)>,
    /// `true` when the slot was bound at Kernel construction time
    /// (e.g. `FnSource::Builtin` — the call target is fixed and
    /// can't change). `dispatch` short-circuits the LambdaDef
    /// downcast + rebind check for these slots.
    pre_bound: bool,
    /// External BindIds referenced by labeled-default Node trees of
    /// `BuiltinSlot::LabeledDefault` slots. At every dispatch cycle
    /// `Kernel::update` primes `event.variables[id]` from
    /// `ctx.cached[id]` for each entry here, so the default's `Ref`
    /// node finds its value (mirrors `CallSite::bind`'s default-arg
    /// priming step at the inner `compile_default!` call). Empty
    /// for non-builtin slots and for builtin slots whose defaults
    /// are pure literals with no external refs.
    default_external_refs: Vec<BindId>,
    /// `false` until the current inner Apply's FIRST dispatch has
    /// run. A freshly-constructed Apply's first update IS its init
    /// (the same contract `CallSite::bind` provides a fresh callee):
    /// its compiled labeled-default Nodes are Constants/exprs that
    /// only produce on `event.init` — but the OUTER cycle that first
    /// dispatches the kernel may be long past init (an async-fed
    /// region's first fire). `dispatch` forces `event.init = true`
    /// for the first inner update, then restores it.
    fired: bool,
    /// Lexical scope at the kernel's definition site. Re-passed to
    /// the inner Apply's `init` so it sees the right environment.
    scope: Scope,
    /// Top-level expression id for the inner Apply's diagnostics.
    top_id: ExprId,
}

unsafe impl<R: Rt, E: UserEvent> Send for DynCallSlot<R, E> {}
unsafe impl<R: Rt, E: UserEvent> Sync for DynCallSlot<R, E> {}

impl<R: Rt, E: UserEvent> std::fmt::Debug for DynCallSlot<R, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DynCallSlot")
            .field("bind_ids", &self.bind_ids.len())
            .field("cached", &self.current.is_some())
            .finish()
    }
}

impl<R: Rt, E: UserEvent> DynCallSlot<R, E> {
    /// Allocate a fresh slot for a kernel `FnParam`. `arg_types` gives
    /// the expected types of the callee's args; we allocate one
    /// BindId + one [`Ref`] node per arg.
    pub fn new(
        fn_param: &kernel_abi::FnParam,
        scope: Scope,
        top_id: ExprId,
    ) -> Self {
        let mut bind_ids = Vec::with_capacity(fn_param.arg_types.len());
        let mut arg_refs: Vec<Node<R, E>> = Vec::with_capacity(fn_param.arg_types.len());
        for arg_kty in &fn_param.arg_types {
            let id = BindId::new();
            bind_ids.push(id);
            // Ref reads `event.variables[id]` (or falls back to
            // `ctx.cached[id]`) on each `update`. `typ` is the
            // FnParam's declared (frozen) netidx `Type`, used directly.
            let typ = arg_kty.clone();
            let node = Ref::new::<R, E>(
                id,
                typ,
                top_id,
                Expr::default(),
            );
            arg_refs.push(node);
        }
        Self {
            bind_ids,
            arg_refs,
            current: None,
            pre_bound: false,
            default_external_refs: Vec::new(),
            fired: false,
            scope,
            top_id,
        }
    }

    /// Construct the builtin's `Apply<R, E>` immediately via its
    /// registered init fn and stash it as a pre-bound slot.
    /// Dispatch will route every call into this Apply without ever
    /// re-binding. Used for `FnSource::Builtin` fn_params at
    /// `Kernel::new` time.
    ///
    /// `layout` describes the callee's full formal-arg list (one
    /// entry per `typ.args` slot, declaration order). For each:
    /// - `Positional(call_idx)`: arg_refs[i] becomes a `Ref` reading
    ///   `bind_ids[call_idx]` (the kernel writes the dispatched
    ///   value to that BindId).
    /// - `LabeledDefault(expr)`: compile the default expression
    ///   into a `Node<R, E>` once; that Node becomes arg_refs[i]
    ///   and produces the default value on every call. Mirrors
    ///   `CallSite::bind`'s `compile_default!` macro, but the
    ///   compile happens once per kernel construction (the call
    ///   site never changes).
    pub fn pre_bind_builtin(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        builtin_name: &str,
        typ: &FnType,
        layout: &[BuiltinSlot],
        lambda_id: Option<LambdaId>,
    ) -> ::anyhow::Result<()> {
        use BuiltinSlot;
        use ::anyhow::anyhow;
        // Restore the lambda's env + lexical scope so labeled-default
        // expressions that reference free variables visible only in
        // the lambda's original module scope (e.g. `default_escape`
        // in `str::escape`'s `#esc = default_escape`) resolve at
        // compile time. Mirrors `CallSite::bind`'s `compile_default!`
        // macro. Without this, defaults that aren't pure literals
        // fail with "binding not in scope" and the kernel's
        // pre-binding bails — fusion silently falls back to interp.
        //
        // The default-compile context: lookup the runtime `LambdaDef`
        // by id, use its env (Arc-shared, cheap to clone) and lexical
        // scope. Dynamic scope stays as the kernel's own (callsite-
        // like). If `lambda_id` is None or the lookup fails we keep
        // the kernel's own scope/env — works for defaults that are
        // pure literals (no free vars).
        let default_env_scope =
            lambda_id.and_then(|id| ctx.lambda_defs.get(&id).cloned()).and_then(|val| {
                val.downcast_ref::<LambdaDef<R, E>>()
                    .map(|d| (d.env.clone(), d.scope.lexical.clone()))
            });
        let init = ctx.builtins.get(builtin_name).copied().ok_or_else(|| {
            anyhow!("DynCallSlot::pre_bind_builtin: unknown builtin `{}`", builtin_name)
        })?;
        // The slot's existing `arg_refs` has one Ref per kernel-
        // marshalled arg (i.e. one per Positional in the layout).
        // Re-shape into a per-formal `from[]` slice in
        // `typ.args` declaration order: Positional slots take their
        // matching Ref from the existing arg_refs; LabeledDefault
        // slots compile the captured default expression and use
        // the resulting Node.
        let mut new_arg_refs: Vec<Node<R, E>> = Vec::with_capacity(layout.len());
        // Drain self.arg_refs (one per positional) so we can move
        // each Ref into the right formal slot. Indexed by
        // BuiltinSlot::Positional(call_idx).
        let mut positional_refs: Vec<Option<Node<R, E>>> =
            self.arg_refs.drain(..).map(Some).collect();
        for slot in layout {
            match slot {
                BuiltinSlot::Positional(call_idx) => {
                    let r = positional_refs
                        .get_mut(*call_idx)
                        .and_then(|s| s.take())
                        .ok_or_else(|| {
                            anyhow!(
                                "DynCallSlot::pre_bind_builtin: layout \
                                 Positional({call_idx}) but only \
                                 {} positional refs allocated",
                                positional_refs.len()
                            )
                        })?;
                    new_arg_refs.push(r);
                }
                BuiltinSlot::LabeledDefault(expr) => {
                    // Compile the default expression. When we have the
                    // owning lambda's env + lexical scope, restore them
                    // first (mirrors `CallSite::bind`'s `compile_default!`)
                    // — a default that names another binding in the
                    // module (`#esc = default_escape`) needs the
                    // module-scope env to resolve. Pure-literal defaults
                    // (rand's `0.0` / `1.0`) work either way.
                    let node = match &default_env_scope {
                        Some((env, lex)) => ctx.with_restored(env.clone(), |ctx| {
                            let scope = Scope {
                                dynamic: self.scope.dynamic.clone(),
                                lexical: lex.clone(),
                            };
                            compile(
                                ctx,
                                enumflags2::BitFlags::empty(),
                                expr.clone(),
                                &scope,
                                self.top_id,
                            )
                        })?,
                        None => compile(
                            ctx,
                            enumflags2::BitFlags::empty(),
                            expr.clone(),
                            &self.scope,
                            self.top_id,
                        )?,
                    };
                    // Mirror `compile_default!`'s priming: walk the
                    // default node's external Refs and record them so
                    // `Kernel::update` can prime `event.variables[id]`
                    // from `ctx.cached[id]` at every cycle. Without
                    // this, a default like `default_escape` (Ref to
                    // a module-level binding) reads None on the first
                    // dispatch — the binding's value is in ctx.cached
                    // but never copied into the per-cycle event.
                    let mut refs = Refs::default();
                    node.refs(&mut refs);
                    refs.with_external_refs(|id| {
                        self.default_external_refs.push(id);
                    });
                    new_arg_refs.push(node);
                }
                BuiltinSlot::Variadic { from_call_idx, count } => {
                    // Variadic tail: forward `count` positional refs
                    // straight through. The inner Apply's own vargs
                    // handling (`CallSite::bind`) walks positional
                    // refs past the fixed formals and collects them
                    // into the declared `Array<varg_type>`. From the
                    // dispatch perspective, a variadic builtin is
                    // just a builtin with extra positional args.
                    for i in 0..*count {
                        let idx = from_call_idx + i;
                        let r = positional_refs
                            .get_mut(idx)
                            .and_then(|s| s.take())
                            .ok_or_else(|| {
                                anyhow!(
                                    "DynCallSlot::pre_bind_builtin: \
                                     layout Variadic at call_idx \
                                     {idx} but only {} positional refs \
                                     allocated",
                                    positional_refs.len()
                                )
                            })?;
                        new_arg_refs.push(r);
                    }
                }
            }
        }
        self.arg_refs = new_arg_refs;
        let apply = init(ctx, typ, Some(typ), &self.scope, &self.arg_refs, self.top_id)?;
        // Use the slot's own address as a stable sentinel pointer —
        // dispatch checks `pre_bound` first and never reads this.
        let sentinel = self as *const Self as *const u8;
        self.current = Some((sentinel, apply));
        self.pre_bound = true;
        Ok(())
    }

    /// Eagerly initialize the inner Apply against `lambda_value`'s
    /// LambdaDef. Used at Kernel construction for binding-source
    /// fn_params whose callee is known up front. Pre-initializing
    /// matters because the inner Apply's body wires up bind_id
    /// subscriptions via `ref_var(..., top_id)` during init — if we
    /// defer to first dispatch, those subscriptions exist but the
    /// runtime cycle that scheduled the parent kernel has already
    /// snapshotted its trigger set, so the parent never re-fires when
    /// the inner Apply's intermediate cycles need it to.
    ///
    /// Returns `Err` if the LambdaDef can't be downcast or the inner
    /// init fails. Caller should fall back to lazy init in that case.
    pub fn pre_init(
        &mut self,
        lambda_value: &Value,
        ctx: &mut ExecCtx<R, E>,
    ) -> ::anyhow::Result<()> {
        use ::anyhow::anyhow;
        let lambda_def = lambda_value
            .downcast_ref::<LambdaDef<R, E>>()
            .ok_or_else(|| anyhow!("DynCallSlot::pre_init: not a LambdaDef"))?;
        let lambda_ptr = lambda_def as *const _ as *const u8;
        let new_apply =
            (lambda_def.init)(&self.scope, ctx, &mut self.arg_refs, None, self.top_id)?;
        self.current = Some((lambda_ptr, new_apply));
        Ok(())
    }

    /// Dispatch the DynCall: look up (or initialize) the inner Apply,
    /// side-channel each arg through its BindId, run `apply.update`,
    /// clean up the BindIds, and return whatever Value the Apply
    /// produced this cycle (or `None` for synchronous-only-v1
    /// "no value yet").
    pub fn dispatch(
        &mut self,
        lambda_value: &Value,
        ctx: &mut ExecCtx<R, E>,
        event: &mut crate::Event<E>,
        args: &[Value],
    ) -> Option<Value> {
        debug_assert_eq!(args.len(), self.bind_ids.len(), "DynCall arity");
        // Pre-bound (FnSource::Builtin) slots: the target was fixed
        // at construction; never re-init, ignore `lambda_value`.
        if !self.pre_bound {
            // Resolve the callee's LambdaDef out of the Value via
            // `downcast_ref`. We key the cache by the LambdaDef's
            // address (stable for the lifetime of the inner
            // Arc<AbstractInner>), so the hot path of "same callback
            // re-invoked" reuses the existing Apply without
            // re-init'ing.
            let lambda_def = lambda_value
                .downcast_ref::<LambdaDef<R, E>>()
                .unwrap_or_else(|| {
                    panic!(
                        "DynCall: fn-arg value isn't a LambdaDef — \
                         typecheck should have rejected this"
                    )
                });
            let lambda_ptr = lambda_def as *const _ as *const u8;
            let needs_init = match &self.current {
                Some((p, _)) if *p == lambda_ptr => false,
                _ => true,
            };
            if needs_init {
                // Drop the old Apply (if any) so it releases resources
                // before we initialize a new one.
                if let Some((_, mut prev)) = self.current.take() {
                    prev.delete(ctx);
                }
                let new_apply = (lambda_def.init)(
                    &self.scope,
                    ctx,
                    &mut self.arg_refs,
                    None,
                    self.top_id,
                )
                .ok()?;
                self.current = Some((lambda_ptr, new_apply));
                // A fresh Apply: its next update is its init.
                self.fired = false;
            }
        }
        // Side-channel: stash each arg Value at its BindId so the
        // arg_refs `Ref` nodes read it inside `apply.update`.
        let mut set: poolshark::local::LPooled<Vec<BindId>> =
            poolshark::local::LPooled::take();
        for (i, v) in args.iter().enumerate() {
            let id = self.bind_ids[i];
            event.variables.insert(id, v.clone());
            set.push(id);
        }
        // First dispatch of a fresh inner Apply = its init cycle:
        // labeled-default Nodes (Constants / default exprs) only
        // produce on `event.init`, and the outer cycle may be long
        // past init (an async-fed region's first fire). Force the
        // init view for this one update, then restore.
        let first = !self.fired;
        self.fired = true;
        let saved_init = event.init;
        if first {
            event.init = true;
        }
        let result = {
            let apply = &mut self.current.as_mut().unwrap().1;
            apply.update(ctx, &mut self.arg_refs, event)
        };
        event.init = saved_init;
        // Cleanup: remove the side-channel entries so a downstream
        // dispatcher (or the outer event loop) doesn't see them.
        for id in set.drain(..) {
            event.variables.remove(&id);
        }
        result
    }
}

// ─── DynCall dispatch for JIT'd kernels ──────────────────────────
//
// When a JIT'd kernel calls a HOF (a DynCall site), the emitted code
// invokes `graphix_dyncall` which indirects through the thread-local
// `DYN_DISPATCH_HANDLE` to a monomorphized `dispatch_typed::<R, E>`.
// `Kernel::update` populates the handle before calling the wrapper,
// passing a `DispatcherState` whose erased pointer holds the per-call
// references (`dyn_slots`, `fn_arg_values`, `ctx`, `event`).

/// Per-call state shared between Rust-side `Kernel::update` and the
/// JIT-side `graphix_dyncall` dispatcher. Held by `Kernel::update`
/// on its stack for the duration of the wrapper call; the handle's
/// `state` pointer references this struct.
///
/// All fields are raw pointers so we can type-erase the struct
/// itself through `*mut u8` and reconstruct in
/// `dispatch_typed::<R, E>` without lifetime annotations carrying
/// through the FFI boundary.
#[repr(C)]
struct DispatcherState<R: Rt, E: UserEvent> {
    dyn_slots: *mut [DynCallSlot<R, E>],
    fn_arg_values: *const [Value],
    ctx: *mut ExecCtx<R, E>,
    event: *mut Event<E>,
}

/// Monomorphized DynCall dispatcher. The function pointer is stored
/// in `DynDispatchHandle.dispatch` per-call by `Kernel::update`.
///
/// SAFETY contract: `state_ptr` must point to a valid
/// `DispatcherState<R, E>` for THIS R, E. The per-call references
/// it holds (dyn_slots, ctx, event, fn_arg_values) must be live
/// for the duration of this call. Kernel::update ensures both.
pub unsafe extern "C" fn dispatch_typed<R: Rt, E: UserEvent>(
    state_ptr: *mut u8,
    fn_index: u32,
    args: *mut poolshark::local::LPooled<Vec<Value>>,
    ret_kind: u8,
) -> DynCallRet {
    use DynCallRet;
    let state = unsafe { &mut *state_ptr.cast::<DispatcherState<R, E>>() };
    let slots = unsafe { &mut *state.dyn_slots };
    let fn_arg_values = unsafe { &*state.fn_arg_values };
    let ctx = unsafe { &mut *state.ctx };
    let event = unsafe { &mut *state.event };
    // Drain args buf — helper takes ownership.
    let args_vec: Vec<Value> = unsafe {
        let mut owned = *Box::from_raw(args);
        owned.drain(..).collect()
    };
    let slot = &mut slots[fn_index as usize];
    let lambda_v = &fn_arg_values[fn_index as usize];
    match slot.dispatch(lambda_v, ctx, event, &args_vec) {
        Some(v) => match ret_kind {
            0 => {
                // Scalar return: pack the Value's bits into word0;
                // word1 unused.
                DynCallRet { word0: dyncall_scalar_return_bits(&v), word1: 0 }
            }
            1 => {
                // Composite ValArray return: extract the inner
                // array, box, transfer ownership. word1 unused.
                match v {
                    Value::Array(arr) => DynCallRet {
                        word0: Box::into_raw(Box::new(arr)) as u64,
                        word1: 0,
                    },
                    other => panic!(
                        "DynCall return ABI: ret_kind=1 (ValArray) but \
                         got {other:?}"
                    ),
                }
            }
            2 => {
                // Variant / Nullable return: hand back the Value's
                // two `repr(u64)` words directly — no Box. The JIT
                // reads both registers and treats the result as the
                // (disc, payload) pair of a Value it now owns.
                //
                // SAFETY: Value is `#[repr(u64)]`, 16 bytes /
                // 8-byte aligned — layout pinned by
                // `emit_helpers`. `ManuallyDrop` prevents the
                // local `v`'s Drop from running while we transmute
                // its bits out; ownership transfers to the caller.
                let v = std::mem::ManuallyDrop::new(v);
                let words: [u64; 2] = unsafe { std::mem::transmute_copy(&*v) };
                DynCallRet { word0: words[0], word1: words[1] }
            }
            3 => {
                // Unit return: caller discards. Both words zero.
                DynCallRet { word0: 0, word1: 0 }
            }
            4 => {
                // String return: extract ArcStr from Value::String,
                // transfer ownership through word0 as raw bits.
                // ArcStr is `repr(transparent)` over `NonNull<ThinInner>`
                // so the raw u64 is a valid ArcStr bit pattern; the
                // caller's String SSA reads it directly without a
                // boundary decode (the kernel-return path at
                // `Kernel::update`'s String arm uses the same
                // transmute pattern).
                match v {
                    Value::String(s) => {
                        let s = std::mem::ManuallyDrop::new(s);
                        let bits: u64 = unsafe {
                            std::mem::transmute_copy::<arcstr::ArcStr, u64>(&*s)
                        };
                        DynCallRet { word0: bits, word1: 0 }
                    }
                    other => panic!(
                        "DynCall return ABI: ret_kind=4 (String) but \
                         got {other:?}"
                    ),
                }
            }
            _ => panic!("DynCall return ABI: bad ret_kind {ret_kind}"),
        },
        None => {
            DYNCALL_PENDING.with(|c| c.set(true));
            DynCallRet { word0: 0, word1: 0 }
        }
    }
}

/// Pack a scalar [`Value`]'s bits into a u64 for the DynCall ABI's
/// scalar return path, deriving the `PrimType` from the value's own
/// variant. Same encoding as [`pack_value_to_u64`].
fn dyncall_scalar_return_bits(v: &Value) -> u64 {
    let prim = kernel_abi::scalar_prim_of_value(v).unwrap_or_else(|| {
        panic!("DynCall scalar return: callee produced non-scalar {v:?}")
    });
    pack_value_to_u64(v, prim)
}

/// Wraps a [`KernelSig`] as an [`Apply<R, E>`] so the runtime can call
/// into the interpreter through the same dispatch path it uses for
/// every other function. On each `update` cycle we drive the input
/// nodes, cache their values, and (once every input slot is populated)
/// run [`eval_kernel`] over the `Value` args — or, when the JIT slot is
/// filled, dispatch into native code via the wrapper.
///
/// Generic over `R, E` because the per-DynCall-slot state holds
/// `Box<dyn Apply<R, E>>` and `Node<R, E>`. (Pre-DynCall versions
/// were intentionally non-generic; the tradeoff is now in DynCall's
/// favor.)
pub struct Kernel<R: Rt, E: UserEvent> {
    /// The IR. `Arc` so structurally-identical kernels can share
    /// state (and, in M4e, share the JIT-compiled function pointer
    /// via an IR-hash cache).
    kernel: Arc<KernelSig>,
    /// Per-cycle input cache, parallel to the `from` slice the runtime
    /// passes into `update`. `None` means "haven't seen a value yet";
    /// the kernel runs once every slot is `Some`.
    args: Box<[Option<Value>]>,
    /// The compiled JIT wrapper this node dispatches into. Required:
    /// a fused node without a JIT cannot exist — JIT failure means
    /// the region was never spliced and the original nodes node-walk.
    jit: Arc<WrappedKernel>,
    /// One slot per `kernel.fn_params` entry. Empty for kernels with
    /// no HOF args. The DynCall dispatcher closure (assembled inside
    /// `Apply<R, E>::update`) borrows this slice mutably to invoke
    /// inner Applies.
    dyn_slots: Vec<DynCallSlot<R, E>>,
    /// Pre-computed mapping from call-site arg position (index into
    /// `args`) to whether that position is a primitive param (with
    /// its index in `kernel.params`) or a function param (with its
    /// index in `kernel.fn_params`). Computed once at construction
    /// to avoid scanning `fn_params` per cycle.
    arg_layout: Vec<ArgKind>,
}

/// Tag for each call-site arg position. The runtime walks the
/// incoming `from` slice in source-arg order; each entry classifies
/// the arg into the right slot list (scalar, array, tuple, struct,
/// variant, fn) and stores its index within that list.
#[derive(Debug, Clone, Copy)]
enum ArgKind {
    Prim(u32),
    Fn(u32),
    Array(u32),
    Tuple(u32),
    Struct(u32),
    Variant(u32),
    Nullable(u32),
    String(u32),
    Value(u32),
}

/// Total number of input slots the runtime passes into a Kernel for
/// this kernel — scalar params + all composite params + HOF-arg fn
/// params (Binding-source fn params resolve through ctx.cached and
/// don't count). Equals `arg_layout.len()`.
pub fn total_kernel_arity(kernel: &KernelSig) -> usize {
    use FnSource;
    let param_source_count = kernel
        .fn_params
        .iter()
        .filter(|fp| matches!(fp.source, FnSource::Param { .. }))
        .count();
    kernel.tail_call_slots.len() + param_source_count
}

fn build_arg_layout(kernel: &KernelSig) -> Vec<ArgKind> {
    use kernel_abi::{FnSource, TailCallSlotKind};
    // `tail_call_slots` is populated for every kernel and lists
    // params in source-declared order. Each slot carries a name
    // matching one of the kernel's *_params lists. Walking this list
    // gives us per-position routing; the corresponding within-list
    // index falls out of a running counter per kind.
    //
    // Fn params don't appear in `tail_call_slots` (the constructor
    // bails on fn args in tail-call kernels). For non-tail kernels
    // tail_call_slots is also populated for the non-fn params, so we
    // detect fn positions separately by walking fn_params.
    let mut out =
        Vec::with_capacity(kernel.tail_call_slots.len() + kernel.fn_params.len());
    let mut prim_idx: u32 = 0;
    let mut array_idx: u32 = 0;
    let mut tuple_idx: u32 = 0;
    let mut struct_idx: u32 = 0;
    let mut variant_idx: u32 = 0;
    let mut nullable_idx: u32 = 0;
    let mut string_idx: u32 = 0;
    let mut value_idx: u32 = 0;
    // Count of HOF-arg fn_params (Binding-source ones don't take a
    // positional input). Combined with `tail_call_slots.len()` to
    // size the layout.
    let param_source_count = kernel
        .fn_params
        .iter()
        .filter(|fp| matches!(fp.source, FnSource::Param { .. }))
        .count();
    let total = kernel.tail_call_slots.len() + param_source_count;
    let mut tail_iter = kernel.tail_call_slots.iter();
    for i in 0..total {
        // Fn params occupy a fixed position via FnSource::Param. If
        // this position is one of them, emit a Fn slot.
        let fn_match = kernel.fn_params.iter().position(|fp| {
            matches!(fp.source, FnSource::Param { arg_pos } if arg_pos as usize == i)
        });
        if let Some(fn_idx) = fn_match {
            out.push(ArgKind::Fn(fn_idx as u32));
            continue;
        }
        // Otherwise pull the next tail_call_slot entry — that
        // describes which scalar/composite list the arg belongs to.
        let slot = tail_iter.next().expect(
            "arg_layout: ran out of tail_call_slots before filling all \
             positions — fusion built an inconsistent kernel",
        );
        match slot.kind {
            TailCallSlotKind::Scalar(_) => {
                out.push(ArgKind::Prim(prim_idx));
                prim_idx += 1;
            }
            TailCallSlotKind::ValArray => {
                // Disambiguate by looking at which list the name
                // appears in. ValArray covers Array/Tuple/Struct;
                // names are unique across slot lists by construction.
                if kernel.array_params.iter().any(|a| a.name == slot.name) {
                    out.push(ArgKind::Array(array_idx));
                    array_idx += 1;
                } else if kernel.tuple_params.iter().any(|t| t.name == slot.name) {
                    out.push(ArgKind::Tuple(tuple_idx));
                    tuple_idx += 1;
                } else if kernel.struct_params.iter().any(|s| s.name == slot.name) {
                    out.push(ArgKind::Struct(struct_idx));
                    struct_idx += 1;
                } else {
                    panic!(
                        "arg_layout: ValArray slot `{}` not found in any \
                         composite param list",
                        slot.name
                    );
                }
            }
            TailCallSlotKind::Variant => {
                out.push(ArgKind::Variant(variant_idx));
                variant_idx += 1;
            }
            TailCallSlotKind::Nullable => {
                out.push(ArgKind::Nullable(nullable_idx));
                nullable_idx += 1;
            }
            TailCallSlotKind::String => {
                out.push(ArgKind::String(string_idx));
                string_idx += 1;
            }
            TailCallSlotKind::Value => {
                out.push(ArgKind::Value(value_idx));
                value_idx += 1;
            }
        }
    }
    out
}

impl<R: Rt, E: UserEvent> std::fmt::Debug for Kernel<R, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Kernel")
            .field("fn_name", &self.kernel.fn_name)
            .field("params", &self.kernel.params.len())
            .field("fn_params", &self.kernel.fn_params.len())
            .finish()
    }
}

impl<R: Rt, E: UserEvent> Kernel<R, E> {
    /// The compiled kernel IR this node executes. Used by graph
    /// introspection (`node_shape`) to assert on what a region
    /// actually fused into.
    pub fn kernel(&self) -> &Arc<KernelSig> {
        &self.kernel
    }

    /// Clone this node for a fresh array slot ([`crate::Update::clone_rebind`]):
    /// SHARE the immutable IR / JIT `Arc`s (the kernel is the
    /// incidental "what `update` does"), but re-run the `build` chokepoint
    /// so the per-cycle scratch is fresh and the `dyn_slots` are re-inited
    /// (each slot's inner Apply is a per-slot instance, not shared). `scope`
    /// + `top_id` are the cloned feeders' scope and the region's spec id.
    pub fn clone_shared(
        &self,
        ctx: &mut ExecCtx<R, E>,
        n_args: usize,
        scope: Scope,
        top_id: ExprId,
    ) -> ::anyhow::Result<Self> {
        Self::new(ctx, self.kernel.clone(), n_args, self.jit.clone(), scope, top_id)
    }

    /// Single construction chokepoint: a Kernel dispatches into
    /// `wrapped`, the JIT artifact — there is no other way to make
    /// one (JIT failure means the region is never spliced and the
    /// original nodes node-walk). Builds the Kernel and runs both
    /// pre-init helpers (`pre_init_binding_slots` for binding-source
    /// fn_params, `pre_init_builtin_slots` for builtin-source
    /// fn_params). Without those, the first `DynCall` into the kernel
    /// either silently fails to drive its inner Apply (binding case)
    /// or panics with "fn-arg value isn't a LambdaDef" (builtin case).
    ///
    /// `scope` and `top_id` initialize per-DynCall-slot state (the
    /// inner Applies that DynCall dispatches into).
    pub fn new(
        ctx: &mut ExecCtx<R, E>,
        kernel: Arc<KernelSig>,
        n_args: usize,
        wrapped: Arc<WrappedKernel>,
        scope: Scope,
        top_id: ExprId,
    ) -> ::anyhow::Result<Self> {
        debug_assert_eq!(
            n_args,
            total_kernel_arity(&kernel),
            "Kernel arity = sum of all slot kinds"
        );
        let dyn_slots = kernel
            .fn_params
            .iter()
            .map(|fp| DynCallSlot::new(fp, scope.clone(), top_id))
            .collect();
        let arg_layout = build_arg_layout(&kernel);
        let mut node = Self {
            kernel,
            args: vec![None; n_args].into_boxed_slice(),
            jit: wrapped,
            dyn_slots,
            arg_layout,
        };
        node.pre_init_binding_slots(ctx);
        node.pre_init_builtin_slots(ctx)?;
        Ok(node)
    }

    /// Eagerly initialize each binding-source DynCall slot so the
    /// inner Apply's body wires up its bind_id subscriptions during
    /// the current cycle. Without this, the runtime never re-
    /// schedules the parent kernel for the inner Apply's later
    /// cycles (we saw this hang `array::fold` calls dispatched via
    /// DynCall).
    ///
    /// Param-source slots (HOF args) are skipped — the callee value
    /// arrives per dispatch from the kernel's caller, not from a
    /// fixed binding.
    pub fn pre_init_binding_slots(&mut self, ctx: &mut ExecCtx<R, E>) {
        for (fn_idx, fp) in self.kernel.fn_params.iter().enumerate() {
            if let FnSource::Binding { bind_id } = &fp.source {
                if let Some(v) = ctx.cached.get(bind_id).cloned() {
                    if let Err(e) = self.dyn_slots[fn_idx].pre_init(&v, ctx) {
                        log::warn!(
                            "kernel: pre_init for fn_param `{}` failed: \
                             {e:#}; falling back to lazy init (multi-cycle \
                             callees may hang)",
                            fp.name
                        );
                    }
                }
                // If the LambdaDef isn't cached yet, leave the slot
                // uninitialized — dispatch will lazy-init when it's
                // first invoked. This case mostly hits at very early
                // startup before all let-bindings have evaluated.
            }
        }
    }

    /// Eagerly construct the Apply for each `FnSource::Builtin`
    /// fn_param. Must be called once after `Kernel::new` (typically
    /// right next to `pre_init_binding_slots`) — without it, the
    /// builtin slots stay empty and the first DynCall into them
    /// panics. Construction routes through `ctx.builtins[name].init`
    /// with the resolved FnType the analyzer captured at fusion time.
    pub fn pre_init_builtin_slots(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
    ) -> ::anyhow::Result<()> {
        for (fn_idx, fp) in self.kernel.fn_params.iter().enumerate() {
            if let FnSource::Builtin {
                name,
                typ,
                layout,
                lambda_id,
            } = &fp.source
            {
                let name = name.clone();
                let typ = typ.clone();
                let layout = layout.clone();
                let lambda_id = *lambda_id;
                self.dyn_slots[fn_idx].pre_bind_builtin(
                    ctx,
                    name.as_str(),
                    &typ,
                    &layout,
                    lambda_id,
                )?;
            }
        }
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Kernel<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        // Drive each child and cache its update. Mirrors what
        // `CachedVals::update` does: we want to fire the kernel only
        // when at least one input has produced this cycle, but use
        // cached values for inputs that didn't.
        let mut any_updated = false;
        for (i, src) in from.iter_mut().enumerate() {
            if let Some(v) = src.update(ctx, event) {
                self.args[i] = Some(v);
                any_updated = true;
            }
        }
        // Binding-source fn_params don't sit in `from` (they resolve
        // through ctx.cached at dispatch time) but they DO influence
        // the kernel's result — when a referenced LambdaDef binding
        // updates this cycle, we must re-fire so the new callee
        // dispatches. Without this check, a kernel that DynCalls
        // into `helper` never reruns after helper's first publish.
        for fp in self.kernel.fn_params.iter() {
            if let FnSource::Binding { bind_id } = &fp.source {
                if event.variables.contains_key(bind_id) {
                    any_updated = true;
                    break;
                }
            }
        }
        // Zero-DYNAMIC-input kernels need a way to fire on init.
        // "Dynamic" inputs are anything that can change between
        // cycles: `from` slots, Binding-source fn_params (their
        // LambdaDef can rebind), and Param-source fn_params
        // (passed in by the caller each cycle). `Builtin`-source
        // fn_params are pre-bound at construction and never
        // change, so they don't gate firing.
        //
        // Module kernels for a pure-constant top-level let-chain
        // (`let a = 5; let b = a + 1; …`) hit this. So do region
        // kernels whose only "input" is a sync builtin call like
        // `bit_and(i64:0xFF, i64:0x0F)` — the args are inlined
        // constants and the builtin's behavior is fixed. The
        // kernel needs to compute and publish on startup;
        // subsequent cycles never re-fire (nothing can change),
        // which is the correct semantics.
        let has_dynamic_fn_params = self.kernel.fn_params.iter().any(|fp| {
            matches!(
                fp.source,
                FnSource::Param { .. }
                    | FnSource::Binding { .. }
            )
        });
        if !any_updated && from.is_empty() && !has_dynamic_fn_params && event.init {
            any_updated = true;
        }
        if !any_updated {
            return None;
        }
        // Test instrumentation: a fused kernel has committed to
        // running this cycle (JIT or interp). Bump the fused-kernel
        // execution counter so the test harness can distinguish
        // "fused but ran on interp" from "no fusion". The JIT path
        // additionally bumps `JIT_INVOCATIONS` inside its wrapper.
        #[cfg(debug_assertions)]
        record_fusion_invocation();
        // Prime per-cycle `event.variables` from `ctx.cached` for
        // every external Ref appearing inside a `BuiltinSlot::
        // LabeledDefault`'s compiled Node. Mirrors `CallSite::bind`'s
        // priming step at the inner `compile_default!` site —
        // without it, a default that names another binding (e.g.
        // `#esc = default_escape`) reads None on dispatch because
        // its `Ref::update` only consults `event.variables` and
        // the binding's value lives in `ctx.cached`. Skipping
        // already-set entries is essential: an outer caller may
        // have updated `id` this cycle and we don't want to
        // clobber its new value with the stale cache.
        for slot in self.dyn_slots.iter() {
            for id in slot.default_external_refs.iter() {
                if !event.variables.contains_key(id) {
                    if let Some(v) = ctx.cached.get(id) {
                        event.variables.insert(*id, v.clone());
                    }
                }
            }
        }
        // Build the kernel's value-bearing args in declaration order
        // (`param_opts`) plus the fn-arg values for the DynCall
        // dispatcher. Unlike the old classification, a MISSING input is
        // NOT a whole-kernel abort: it feeds `None` (bottom) into
        // `param_opts`, and the kernel emits `None` only if the OUTPUT
        // consumes that bottom — `select c { 0 => x, 1 => never_fired }`
        // with `c=0` must still yield `x`. (The JIT path below still
        // aborts on a missing input — JIT bottom support is a later
        // phase.) `param_opts` slots are placed by per-kind base offset
        // so the order matches `eval_kernel_full`'s declaration-order
        // binding (scalars, arrays, tuples, structs, variants, nullables,
        // strings, values).
        let k = &self.kernel;
        let base_array = k.params.len();
        let base_tuple = base_array + k.array_params.len();
        let base_struct = base_tuple + k.tuple_params.len();
        let base_variant = base_struct + k.struct_params.len();
        let base_nullable = base_variant + k.variant_params.len();
        let base_string = base_nullable + k.nullable_params.len();
        let base_value = base_string + k.string_params.len();
        let n_params = base_value + k.value_params.len();
        let mut param_opts: Vec<Option<Value>> = vec![None; n_params];
        let mut fn_arg_values: smallvec::SmallVec<[Value; 4]> =
            smallvec::SmallVec::with_capacity(k.fn_params.len());
        for _ in 0..k.fn_params.len() {
            fn_arg_values.push(Value::Null);
        }
        for (i, kind) in self.arg_layout.iter().enumerate() {
            let v = self.args[i].clone();
            match *kind {
                ArgKind::Prim(idx) => param_opts[idx as usize] = v,
                ArgKind::Fn(fn_idx) => {
                    if let Some(v) = v {
                        fn_arg_values[fn_idx as usize] = v;
                    }
                }
                ArgKind::Array(idx) => param_opts[base_array + idx as usize] = v,
                ArgKind::Tuple(idx) => param_opts[base_tuple + idx as usize] = v,
                ArgKind::Struct(idx) => param_opts[base_struct + idx as usize] = v,
                ArgKind::Variant(idx) => param_opts[base_variant + idx as usize] = v,
                ArgKind::Nullable(idx) => param_opts[base_nullable + idx as usize] = v,
                ArgKind::String(idx) => param_opts[base_string + idx as usize] = v,
                ArgKind::Value(idx) => param_opts[base_value + idx as usize] = v,
            }
        }
        // Resolve Binding-source fn slots by reading the BindId out
        // of `event.variables` first (current-cycle update) or
        // falling back to `ctx.cached` (prior cycle's value). If
        // neither has a value yet, the kernel can't run — return
        // None and try again next cycle.
        for (fn_idx, fp) in self.kernel.fn_params.iter().enumerate() {
            if let FnSource::Binding { bind_id } = &fp.source {
                let v = event
                    .variables
                    .get(bind_id)
                    .cloned()
                    .or_else(|| ctx.cached.get(bind_id).cloned())?;
                fn_arg_values[fn_idx] = v;
            }
        }
        // JIT dispatch. The JIT supports kernels with
        // array/tuple/struct/variant INPUTS (each composite param is a
        // pointer in the wrapper's arg slot — `*const ValArray` for
        // array/tuple/struct, `*const Value` for variant). Re-derive
        // the typed per-kind args from `param_opts`, aborting (`?`) on
        // any missing input, pack each scalar arg's bits into a raw
        // u64 slot, composite args as `*const ValArray`, etc., call
        // through the wrapper, and unpack the result.
        let wrapped = &self.jit;
        let mut scalar_arg_bits: smallvec::SmallVec<[u64; 8]> =
            smallvec::SmallVec::with_capacity(k.params.len());
        for (i, p) in k.params.iter().enumerate() {
            let v = param_opts[i].as_ref()?;
            scalar_arg_bits.push(pack_value_to_u64(v, p.prim));
        }
        let composite_arr = |i: usize| -> Option<ValArray> {
            match param_opts[i].as_ref()? {
                Value::Array(a) => Some(a.clone()),
                v => panic!(
                    "Kernel: composite param expected Value::Array, \
                     got {v:?}"
                ),
            }
        };
        let mut array_args: smallvec::SmallVec<[ValArray; 4]> =
            smallvec::SmallVec::with_capacity(k.array_params.len());
        for j in 0..k.array_params.len() {
            array_args.push(composite_arr(base_array + j)?);
        }
        let mut tuple_args: smallvec::SmallVec<[ValArray; 4]> =
            smallvec::SmallVec::with_capacity(k.tuple_params.len());
        for j in 0..k.tuple_params.len() {
            tuple_args.push(composite_arr(base_tuple + j)?);
        }
        let mut struct_args: smallvec::SmallVec<[ValArray; 4]> =
            smallvec::SmallVec::with_capacity(k.struct_params.len());
        for j in 0..k.struct_params.len() {
            struct_args.push(composite_arr(base_struct + j)?);
        }
        let mut variant_args: smallvec::SmallVec<[Value; 4]> =
            smallvec::SmallVec::with_capacity(k.variant_params.len());
        for j in 0..k.variant_params.len() {
            variant_args.push(param_opts[base_variant + j].clone()?);
        }
        let mut nullable_args: smallvec::SmallVec<[Value; 4]> =
            smallvec::SmallVec::with_capacity(k.nullable_params.len());
        for j in 0..k.nullable_params.len() {
            nullable_args.push(param_opts[base_nullable + j].clone()?);
        }
        let mut string_args: smallvec::SmallVec<[arcstr::ArcStr; 4]> =
            smallvec::SmallVec::with_capacity(k.string_params.len());
        for j in 0..k.string_params.len() {
            match param_opts[base_string + j].as_ref()? {
                Value::String(s) => string_args.push(s.clone()),
                v => panic!(
                    "Kernel: string param expected Value::String, \
                     got {v:?}"
                ),
            }
        }
        let mut value_args: smallvec::SmallVec<[Value; 4]> =
            smallvec::SmallVec::with_capacity(k.value_params.len());
        for j in 0..k.value_params.len() {
            value_args.push(param_opts[base_value + j].clone()?);
        }
        //
        // Slot order is the canonical kind-grouped ABI layout
        // (`KernelSig::abi_params`): scalar params first, then
        // array/tuple/struct pointers, then variant/nullable
        // (disc, payload) pairs — exactly what the JIT wrapper
        // unpacks.
        //
        // The ValArray references stay borrowed by `array_args`
        // / `tuple_args` / `struct_args` (`SmallVec<ValArray>`)
        // for the duration of the wrapper call — the JIT'd
        // helpers dereference these pointers, so the originals
        // must outlive the call. They do: the smallvecs live
        // for the rest of `update`.
        let mut slots: smallvec::SmallVec<[u64; 16]> =
            smallvec::SmallVec::with_capacity(self.kernel.abi_param_wire_slots());
        for bits in &scalar_arg_bits {
            slots.push(*bits);
        }
        for a in &array_args {
            slots.push(a as *const ValArray as u64);
        }
        for a in &tuple_args {
            slots.push(a as *const ValArray as u64);
        }
        for a in &struct_args {
            slots.push(a as *const ValArray as u64);
        }
        // String boundary: pack the borrowed `ArcStr` as its
        // thin pointer (one word). `ArcStr` is
        // `repr(transparent)` over `NonNull<ThinInner>`, so the
        // pointer word IS the value. The kernel refcount-bumps
        // on entry via `graphix_arcstr_clone`; we keep the
        // `string_args` smallvec alive for the call's duration.
        for s in &string_args {
            let p = s as *const arcstr::ArcStr as *const u64;
            unsafe {
                slots.push(*p);
            }
        }
        // Variant / Nullable boundary: pack the borrowed
        // `Value` as its two `repr(u64)` words (disc, payload).
        // Safe: Value is `#[repr(u64)]`, 16 bytes / 8-byte
        // aligned, layout pinned by the const_assert in
        // `emit_helpers`. We don't transfer ownership —
        // the kernel refcount-bumps on entry via
        // `graphix_value_clone`.
        for v in &variant_args {
            let p = v as *const Value as *const u64;
            unsafe {
                slots.push(*p);
                slots.push(*p.add(1));
            }
        }
        for v in &nullable_args {
            let p = v as *const Value as *const u64;
            unsafe {
                slots.push(*p);
                slots.push(*p.add(1));
            }
        }
        // Bare value-shape (DateTime/Duration/Bytes) boundary:
        // two-word `Value` pack, same as variant/nullable. The
        // kernel clones on entry; we keep `value_args` alive.
        for v in &value_args {
            let p = v as *const Value as *const u64;
            unsafe {
                slots.push(*p);
                slots.push(*p.add(1));
            }
        }
        // Drift guard: the packed slot count must equal the
        // kernel's declared ABI footprint. A mismatch means the
        // per-kind arg vectors disagree with `abi_params` —
        // catch it here rather than as a silent misread in the
        // JIT wrapper.
        debug_assert_eq!(
            slots.len(),
            self.kernel.abi_param_wire_slots(),
            "packed slot count must match the kernel ABI layout"
        );
        let mut out: [u64; 2] = [0, 0];
        let f = unsafe { wrapped.fn_ptr() };
        // Set up the DynCall dispatcher handle so the JIT'd
        // code can invoke fn-typed params via `graphix_dyncall`.
        // Save the previous handle so nested JIT-to-JIT
        // HOF dispatches stack correctly.
        //
        // SAFETY: `state` lives on this stack frame for the
        // entire `f(...)` call. The raw pointers in it refer
        // to live mutable borrows of self/ctx/event/fn_arg_values
        // which we hold through the call. `dispatch_typed::<R, E>`
        // is monomorphized for THIS R, E so the typed downcast
        // inside it is sound.
        let mut state = DispatcherState::<R, E> {
            dyn_slots: &mut self.dyn_slots[..] as *mut [DynCallSlot<R, E>],
            fn_arg_values: &fn_arg_values[..] as *const [Value],
            ctx: ctx as *mut ExecCtx<R, E>,
            event: event as *mut Event<E>,
        };
        let handle = DynDispatchHandle {
            dispatch: dispatch_typed::<R, E>,
            state: (&mut state) as *mut _ as *mut u8,
        };
        let prev_handle = DYN_DISPATCH_HANDLE
            .with(|c| c.replace(&handle as *const _));
        // Always reset the pending flag before the call so
        // we can distinguish "this kernel pended" from
        // "some earlier kernel left the flag set."
        DYNCALL_PENDING.with(|c| c.set(false));
        unsafe {
            f(slots.as_ptr(), out.as_mut_ptr());
        }
        DYN_DISPATCH_HANDLE.with(|c| c.set(prev_handle));
        let pending =
            DYNCALL_PENDING.with(|c| c.replace(false));
        if pending {
            // The kernel's *out slot is either a garbage
            // scalar (no deref needed) or a null pointer
            // (for composite returns; the JIT emitted a
            // sentinel from the pre_pending block). Either
            // way, discard and re-fire next cycle.
            return None;
        }
        // Decode the wrapper's *out slot(s) according to the
        // kernel's declared return type into the boundary
        // `Option<Value>`:
        //
        // - Prim: out[0] bits → Value (unpack_u64_to_value).
        // - Array/Tuple/Struct: out[0] holds a `*mut ValArray`
        //   we own; reclaim via Box::from_raw.
        // - Variant/Nullable/value-shape: out[0] = Value disc,
        //   out[1] = payload. Transmute the two u64s back into a
        //   Value (`#[repr(u64)]`, 16 bytes / 8-byte aligned —
        //   layout pinned by `emit_helpers`).
        use kernel_abi::AbiKind;
        let v = match kernel_abi::abi_kind(
            &ctx.abstract_registry,
            &self.kernel.return_type,
        ) {
            Some(AbiKind::Scalar(p)) => {
                unpack_u64_to_value(out[0], p)
            }
            Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                let ptr = out[0] as *mut ValArray;
                let owned = unsafe { *Box::from_raw(ptr) };
                Value::Array(owned)
            }
            // Unit-returning kernel: the JIT writes 0 into
            // *out; nothing to decode. The caller discards via
            // a discarded statement so the value is never inspected —
            // a Bool placeholder is type-correct and cheap.
            Some(AbiKind::Unit) => Value::Bool(false),
            // String-returning kernel: `out[0]` is the ArcStr's
            // thin pointer (transferred ownership). ArcStr is
            // `repr(transparent)` over `NonNull<ThinInner>`, so
            // the raw u64 is a valid `ArcStr` bit pattern.
            Some(AbiKind::String) => {
                let raw = out[0];
                // SAFETY: the JIT'd kernel produced this via
                // `graphix_arcstr_clone_from_static` or
                // `graphix_string_buf_finalize`, both returning
                // owned ArcStr values.
                let s: arcstr::ArcStr =
                    unsafe { std::mem::transmute::<u64, arcstr::ArcStr>(raw) };
                Value::String(s)
            }
            // Variant / Nullable / value-shape (datetime /
            // duration / bytes / map / error): out[0] = disc,
            // out[1] = payload. Transmute the two u64s back into
            // a Value.
            Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => unsafe {
                std::mem::transmute(out)
            },
            Some(AbiKind::Null) | None => unreachable!(
                "JIT decode for a non-fusable / bare-Null kernel \
                 return — JIT should have bailed out before \
                 producing such a kernel"
            ),
        };
        Some(v)
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        for slot in self.args.iter_mut() {
            *slot = None;
        }
    }

    fn refs(&self, refs: &mut Refs) {
        // Kernel replaces a CallSite for fused lambdas. The
        // CallSite would have walked its inner Apply's refs to
        // build subscription state — when those BindIds fire, the
        // runtime re-triggers the parent. We must do the same: walk
        // every DynCallSlot's inner Apply (the actual callee) plus
        // the slot's arg-ref nodes, and register binding-source
        // fn_param BindIds. Without this, the runtime never re-
        // fires Kernel when the inner callee or its dependencies
        // update — exactly the DynCall hang we caught with the
        // differential harness.
        for slot in &self.dyn_slots {
            if let Some((_, inner)) = &slot.current {
                inner.refs(refs);
            }
            for n in &slot.arg_refs {
                n.refs(refs);
            }
            for id in &slot.bind_ids {
                refs.bound.insert(*id);
            }
        }
        for fp in &self.kernel.fn_params {
            if let FnSource::Binding { bind_id } = &fp.source {
                refs.refed.insert(*bind_id);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use kernel_abi::PrimType;

    #[test]
    fn value_boundary_bits_round_trip() {
        // Value → u64 bits (pack) → Value (unpack) should be lossless
        // for the scalar primitives that cross the JIT boundary.
        let cases: &[(Value, PrimType)] = &[
            (Value::I64(42), PrimType::I64),
            (Value::I64(i64::MIN), PrimType::I64),
            (Value::F64(3.14), PrimType::F64),
            (Value::F32(2.5), PrimType::F32),
            (Value::Bool(true), PrimType::Bool),
            (Value::Bool(false), PrimType::Bool),
            (Value::U32(7), PrimType::U32),
            (Value::U64(u64::MAX), PrimType::U64),
            (Value::I8(-1), PrimType::I8),
        ];
        for (v, p) in cases {
            let bits = pack_value_to_u64(v, *p);
            assert_eq!(unpack_u64_to_value(bits, *p), *v);
        }
    }
}
