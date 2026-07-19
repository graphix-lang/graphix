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

#[cfg(debug_assertions)]
use crate::fusion::emit_helpers::record_fusion_invocation;
use crate::{
    Apply, BindId, Event, ExecCtx, LambdaId, Node, Refs, Rt, Scope, UserEvent,
    expr::{Expr, ExprId},
    fusion::{
        emit::{STALE, TAINT, WrappedKernel, pack_value_to_u64, prim_to_value_disc},
        emit_helpers::{
            DYN_DISPATCH_HANDLE, DYNCALL_PENDING, DynCallRet, DynDispatchHandle, TagValue,
        },
        kernel_abi::{self, BuiltinSlot, FnSource, KernelSig},
    },
    node::{bind::Ref, compiler::compile, lambda::LambdaDef},
    typ::FnType,
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
    pub fn new(fn_param: &kernel_abi::FnParam, scope: Scope, top_id: ExprId) -> Self {
        let mut bind_ids = Vec::with_capacity(fn_param.arg_types.len());
        let mut arg_refs: Vec<Node<R, E>> = Vec::with_capacity(fn_param.arg_types.len());
        for arg_kty in &fn_param.arg_types {
            let id = BindId::new();
            bind_ids.push(id);
            // Ref reads `event.variables[id]` (or falls back to
            // `ctx.cached[id]`) on each `update`. `typ` is the
            // FnParam's declared (frozen) netidx `Type`, used directly.
            let typ = arg_kty.clone();
            let node = Ref::new::<R, E>(id, typ, top_id, Expr::default());
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
        use ::anyhow::anyhow;
        use BuiltinSlot;
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
        let new_apply = (lambda_def.init)(
            &self.scope,
            ctx,
            &mut self.arg_refs,
            crate::BindMode::Definition,
            self.top_id,
        )?;
        self.current = Some((lambda_ptr, new_apply));
        Ok(())
    }

    /// Pre-bind a `FnSource::Cast` slot: stash a `CastApply` carrying
    /// the destination `Type`. The slot was allocated with one arg
    /// (the cast source) by `DynCallSlot::new`, so its single
    /// `arg_refs[0]` already reads the side-channeled input — no layout
    /// reshaping (unlike `pre_bind_builtin`). Pre-bound, so dispatch
    /// runs `CastApply::update` directly and never re-binds.
    pub fn pre_bind_cast(&mut self, target: crate::typ::Type) {
        let apply: Box<dyn Apply<R, E>> =
            Box::new(CastApply { target, _p: std::marker::PhantomData });
        let sentinel = self as *const Self as *const u8;
        self.current = Some((sentinel, apply));
        self.pre_bound = true;
    }

    /// Pre-bind a `FnSource::QopDeliver` slot: a `QopDeliverApply`
    /// carrying the catch handler's BindId + the `?`'s spec. The single
    /// `arg_refs[0]` from `DynCallSlot::new` reads the side-channeled
    /// error value the kernel marshals on the qop's error path.
    pub fn pre_bind_qop_deliver(&mut self, handler_id: BindId, spec: Expr) {
        let apply: Box<dyn Apply<R, E>> =
            Box::new(crate::node::error::QopDeliverApply { handler_id, spec });
        let sentinel = self as *const Self as *const u8;
        self.current = Some((sentinel, apply));
        self.pre_bound = true;
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
            let lambda_def =
                lambda_value.downcast_ref::<LambdaDef<R, E>>().unwrap_or_else(|| {
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
                    crate::BindMode::Definition,
                    self.top_id,
                )
                .ok()?;
                self.current = Some((lambda_ptr, new_apply));
                // A fresh Apply: its next update is its init.
                self.fired = false;
            }
        }
        // Side-channel: stash each arg Value at its BindId so the
        // arg_refs `Ref` nodes read it inside `apply.update`. FIRED:
        // the kernel already decided this call happens — the delivery
        // is the call's argument event.
        let mut set: poolshark::local::LPooled<Vec<BindId>> =
            poolshark::local::LPooled::take();
        for (i, v) in args.iter().enumerate() {
            let id = self.bind_ids[i];
            event.variables.insert(id, crate::TagValue::fired(v.clone()));
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

/// The `cast<T>(x)` operator as an `Apply`, so a non-inline cast can be
/// dispatched through the same DynCall machinery as a builtin call (see
/// [`FnSource::Cast`]). `update` reads the single side-channeled source
/// value from `from[0]` and runs `target.cast_value(&ctx.env, v)` — the
/// EXACT function `TypeCast::update` (the node-walk) calls, so the two
/// evaluators agree by construction. Returns `None` (bottom) only when
/// the source itself produced no value this cycle.
pub(crate) struct CastApply<R: Rt, E: UserEvent> {
    target: crate::typ::Type,
    _p: std::marker::PhantomData<fn() -> (R, E)>,
}

impl<R: Rt, E: UserEvent> std::fmt::Debug for CastApply<R, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CastApply").field("target", &self.target).finish()
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for CastApply<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        from.get_mut(0)?
            .update(ctx, event)
            .map(|tv| self.target.cast_value(&ctx.env, tv.value()))
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
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
        Some(v) => {
            // Unified Value ABI: hand back the Value's two `repr(u64)`
            // words for EVERY return type — the call site adapts per
            // its static shape (narrow a scalar payload, adopt owned
            // ValArray/ArcStr bits, keep a value-shape pair, discard
            // Unit).
            //
            // SAFETY: Value is `#[repr(u64)]`, 16 bytes / 8-byte
            // aligned — layout pinned by `emit_helpers`.
            // `ManuallyDrop` prevents the local `v`'s Drop from
            // running while we transmute its bits out; ownership
            // transfers to the caller.
            let v = std::mem::ManuallyDrop::new(v);
            let words: [u64; 2] = unsafe { std::mem::transmute_copy(&*v) };
            DynCallRet { word0: words[0], word1: words[1] }
        }
        None => {
            // "No value this cycle" — the JIT'd call site
            // take-and-clears this immediately and converts it to a
            // #219 tainted placeholder that continues, so the bottom
            // stays local to the result's consumers (item 28).
            DYNCALL_PENDING.with(|c| c.set(true));
            DynCallRet { word0: 0, word1: 0 }
        }
    }
}

/// Monomorphized variable-write for a fused `connect` / handler-ful
/// `?`. Reaches `ctx` through the same `DispatcherState` the DynCall
/// dispatcher uses and calls `ctx.set_var` — the exact write the
/// node-walk `Connect::update` / `Qop::update` perform. A disc that is
/// `#219`-tainted (no value) OR STALE (did not fire this cycle) is
/// skipped — the write happens only when the RHS FIRED with a value,
/// mirroring the node-walk's `if let Some(v) = ..` guard. Never touches
/// the pending flag — a write is a side effect, not an abort.
///
/// SAFETY: same contract as `dispatch_typed` — `state_ptr` is a live
/// `DispatcherState<R, E>` for THIS R, E for the duration of the call.
pub unsafe extern "C" fn set_var_typed<R: Rt, E: UserEvent>(
    state_ptr: *mut u8,
    bind_id: u64,
    disc: u64,
    payload: u64,
) {
    // CONSUME the payload unconditionally: `emit_connect_node` marshals any
    // shape (scalar/string/composite/value) to an OWNED `(disc, payload)`, so
    // a skipped write must still DROP the owned value or it leaks. A scalar
    // payload is inline (drop is a no-op); a composite/string owns a heap
    // allocation. `TagValue::value` masks the tag byte, so a tainted / stale
    // disc materializes as a valid placeholder Value that is safe to drop.
    // SAFETY: emit_connect_node marshalled a real owned Value into
    // these words; only the tag byte may be set on top of it.
    let value = unsafe { TagValue::from_raw(disc, payload) }.value();
    if disc & ((TAINT | STALE) as u64) != 0 {
        // No value this cycle (tainted) or the RHS did not fire (stale) — drop
        // the owned value, no write (the node-walk's `if let Some(v) = ..`).
        drop(value);
        return;
    }
    let state = unsafe { &mut *state_ptr.cast::<DispatcherState<R, E>>() };
    let ctx = unsafe { &mut *state.ctx };
    ctx.rt.set_var(BindId::from_inner(bind_id), value);
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
    /// Per-INSTANCE cross-invocation state, `jit.state_words` zeroed
    /// `u64`s (empty for the common stateless kernel). Passed by
    /// pointer in wire slot 1 each invocation; emission sites claim a
    /// word each for firing bookkeeping (exact HOF resize detection,
    /// select selection memory — `design/kernel_instance_state.md`).
    /// Zero = "no previous observation": consumers store `value + 1`,
    /// so a fresh instance's init semantics fall out of the zeroing.
    state: Box<[u64]>,
    /// The kernel's RESULT slot on the value channel — the last value
    /// a run produced. A region is pure by construction (effects
    /// de-fuse), so when a poll delivers only STALE productions (an
    /// evaluation frame re-running a node-walked loop around this
    /// kernel — the only place stale productions originate) the cached
    /// result is exactly what a re-run would compute; re-surface it
    /// tagged STALE via [`Apply::out_tag`] instead of running the JIT.
    /// The `CachedArgs::last_result` twin.
    last_result: Option<Value>,
    /// The tag of the last value `update` returned (see `out_tag`).
    last_out: crate::Tag,
}

impl<R: Rt, E: UserEvent> Drop for Kernel<R, E> {
    fn drop(&mut self) {
        // Free the per-slot state-table chains the JIT'd code boxed
        // behind their claimed anchor words (`graphix_slot_state_table`
        // — scaffold-loop guarded-select selection memory; a nested
        // select's anchor owns `own_levels` directory levels, one per
        // enclosing loop). Semantic state: neither `sleep` nor
        // `reset_replay` touches these words, only instance death does.
        for a in self.jit.slot_table_words.iter() {
            let p = std::mem::replace(&mut self.state[a.rel as usize], 0);
            super::emit_helpers::free_slot_chain(
                p,
                a.own_levels as u64,
                a.leaf.as_deref(),
            );
        }
    }
}
/// Routing for one incoming runtime arg position: a value-bearing
/// kernel param (index into `KernelSig::params` — SOURCE order, the
/// unified single list) or an HOF fn arg (index into `fn_params`).
#[derive(Debug, Clone, Copy)]
enum ArgKind {
    Param(u32),
    Fn(u32),
}

/// Total number of input slots the runtime passes into a Kernel for
/// this kernel — value-bearing params + HOF-arg fn params
/// (Binding-source fn params resolve through ctx.cached and don't
/// count). Equals `arg_layout.len()`.
pub fn total_kernel_arity(kernel: &KernelSig) -> usize {
    use FnSource;
    let param_source_count = kernel
        .fn_params
        .iter()
        .filter(|fp| matches!(fp.source, FnSource::Param { .. }))
        .count();
    kernel.params.len() + param_source_count
}

/// Per-position routing for the runtime's incoming args: HOF fn args
/// sit at their source positions (`FnSource::Param { arg_pos }`),
/// value-bearing params fill the remaining positions in `params`
/// order (which IS source order — the unified list).
fn build_arg_layout(kernel: &KernelSig) -> Vec<ArgKind> {
    use kernel_abi::FnSource;
    let total = total_kernel_arity(kernel);
    let mut out = Vec::with_capacity(total);
    let mut param_idx: u32 = 0;
    for i in 0..total {
        let fn_match = kernel.fn_params.iter().position(|fp| {
            matches!(fp.source, FnSource::Param { arg_pos } if arg_pos as usize == i)
        });
        match fn_match {
            Some(fn_idx) => out.push(ArgKind::Fn(fn_idx as u32)),
            None => {
                out.push(ArgKind::Param(param_idx));
                param_idx += 1;
            }
        }
    }
    assert_eq!(
        param_idx as usize,
        kernel.params.len(),
        "arg_layout: fn positions and params disagree with total arity"
    );
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

    /// Free the RESET-kind slot chains (per-slot interior-bottom
    /// caches — `SiteAnchor::reset`) and null their anchors: the
    /// emitted code rebuilds a fresh zeroed chain on the next
    /// invocation (`graphix_slot_state_table` on a 0 word), so fresh
    /// = no history, exactly the flat `replay_state_words` zeroing.
    /// Selection-memory chains (`reset: false`) are untouched —
    /// semantic state, freed only by `Drop`.
    fn free_reset_chains(&mut self) {
        for a in self.jit.slot_table_words.iter().filter(|a| a.reset) {
            let p = std::mem::replace(&mut self.state[a.rel as usize], 0);
            super::emit_helpers::free_slot_chain(
                p,
                a.own_levels as u64,
                a.leaf.as_deref(),
            );
        }
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
        // `dyn_slots` follows the REGION-WIDE combined table on the
        // WrappedKernel (parent `fn_params` ++ each callee's), not just
        // this kernel's own `fn_params` — so a callee body's DynCall
        // (`fn_index = base + local`) lands on the right pre-bound slot.
        // Equal to `kernel.fn_params` when there are no callee DynCalls.
        let dyn_slots = wrapped
            .dyn_fn_params
            .iter()
            .map(|fp| DynCallSlot::new(fp, scope.clone(), top_id))
            .collect();
        let arg_layout = build_arg_layout(&kernel);
        let mut state = vec![0u64; wrapped.state_words].into_boxed_slice();
        // The reserved head words carry the lifted connect targets'
        // BindIds (KernelSig::lifted).
        for (i, id) in kernel.lifted.iter().enumerate() {
            state[i] = id.inner();
        }
        let mut node = Self {
            kernel,
            args: vec![None; n_args].into_boxed_slice(),
            jit: wrapped,
            dyn_slots,
            arg_layout,
            state,
            last_result: None,
            last_out: crate::Tag::FIRED,
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
        // Iterate the COMBINED slot table (parent ++ callees), parallel to
        // `dyn_slots`; clone the `Arc` out first so the loop doesn't hold
        // `self.jit` borrowed while it mutates `self.dyn_slots`. Only the
        // parent contributes Binding-source slots (callee slots are all
        // pre-bound builtin/cast/qop), but iterating the full table keeps
        // `fn_idx` aligned with `dyn_slots`.
        let fps = self.jit.dyn_fn_params.clone();
        for (fn_idx, fp) in fps.iter().enumerate() {
            if let FnSource::Binding { bind_id } = &fp.source {
                if let Some(v) = ctx.rt.cached().get(bind_id).cloned() {
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
        // The COMBINED slot table: the parent's builtin/cast/qop slots AND
        // every callee's (a callee body's DynCalls dispatch through its own
        // pre-bound slots in this same `dyn_slots` array). Clone the `Arc`
        // out so the loop doesn't hold `self.jit` borrowed while mutating
        // `self.dyn_slots`.
        let fps = self.jit.dyn_fn_params.clone();
        for (fn_idx, fp) in fps.iter().enumerate() {
            if let FnSource::Builtin { name, typ, layout, lambda_id } = &fp.source {
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
            if let FnSource::Cast { target } = &fp.source {
                self.dyn_slots[fn_idx].pre_bind_cast(target.clone());
            }
            if let FnSource::QopDeliver { handler_id, spec } = &fp.source {
                self.dyn_slots[fn_idx].pre_bind_qop_deliver(*handler_id, spec.clone());
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
        // Per-feeder "fired THIS cycle" (vs retained-from-a-prior-cycle).
        // `self.args[i]` records presence only — a `Some` may be fresh or
        // cached — so this is the sole source of the STALE distinction the
        // packing below stamps into each param's disc. Stack-resident:
        // region inputs are capped at 64 (`try_fuse`), so this never
        // spills to the heap.
        let mut fired_this_cycle: smallvec::SmallVec<[bool; 64]> =
            smallvec::smallvec![false; from.len()];
        // Any production at all (fired, tainted, OR merely stale) —
        // drives the stale-resurface below when nothing triggered.
        let mut any_produced = false;
        for (i, src) in from.iter_mut().enumerate() {
            if let Some(tv) = src.update(ctx, event) {
                any_produced = true;
                let (v, tag) = tv.into_parts();
                if tag.is_tainted() {
                    // A tainted feeder event: drop the retained value so
                    // the pack below feeds the TAINT placeholder — the
                    // kernel runs and bottoms only if the taken path
                    // consumes it (#219).
                    self.args[i] = None;
                    fired_this_cycle[i] = true;
                    any_updated = true;
                } else {
                    // A merely-STALE production refreshes the slot (the
                    // value channel) without firing the kernel; a fired
                    // one runs it.
                    self.args[i] = Some(v);
                    if tag.is_fired() {
                        fired_this_cycle[i] = true;
                        any_updated = true;
                    }
                }
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
                if event.variables.get(bind_id).is_some_and(|tv| tv.tag().triggers()) {
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
            matches!(fp.source, FnSource::Param { .. } | FnSource::Binding { .. })
        });
        // #219: fire at init even WITH dynamic `from` inputs. A kernel
        // whose missing inputs the output doesn't consume must still
        // produce at init — the node-walk evaluates every binding once at
        // init (sleeping arms keep an un-taken arm's missing input out of
        // the result), and the validity taint reproduces that here:
        // missing inputs are tainted, and the kernel bottoms only if the
        // taken path consumes one. Without this, a kernel whose only
        // dynamic input never fires never runs at all (interp produces a
        // value via the constant arm; jit times out). `has_dynamic_fn_params`
        // kernels keep the stricter gate — their first-dispatch init view
        // is driven by the DynCall protocol, not a plain init fire.
        if !any_updated && !has_dynamic_fn_params && event.init {
            any_updated = true;
        }
        if std::env::var_os("GXDBG_KPOLL").is_some() {
            eprintln!(
                "KPOLL {} init={} any_updated={any_updated} any_produced={any_produced} fired={:?} present={:?} fd={}",
                self.kernel.fn_name,
                event.init,
                &fired_this_cycle[..],
                self.args.iter().map(|a| a.is_some()).collect::<Vec<_>>(),
                ctx.frame_depth
            );
        }
        if !any_updated {
            // A poll that delivered only STALE productions (an
            // evaluation frame re-running the node-walked loop around
            // this kernel): the region is pure and its inputs' VALUES
            // are unchanged since the last run, so the cached result IS
            // what a re-run would compute — re-surface it on the value
            // channel (tagged STALE via `out_tag`) so the frame's acc
            // chain can advance without a firing (see `last_result`).
            // Inside a frame the resurface must not require a
            // production: a ZERO-input const kernel (an inner
            // callback's `|y| 7` region) has nothing that can produce
            // after its init poll, and its `None` bottomed the framed
            // loop via never-until-complete where the node-walk's
            // Constant now rides the STALE value channel (jul12b
            // 000000 — the nested-map stall). The Constant frame
            // rule's kernel twin.
            if (any_produced || ctx.frame_depth > 0)
                && let Some(v) = &self.last_result
            {
                self.last_out = crate::Tag::STALE;
                return Some(v.clone());
            }
            return None;
        }
        if std::env::var("GRAPHIX_DBG_INVOKE").is_ok() {
            eprintln!(
                "KERNEL INVOKE {} init={} fired={:?} present={:?}",
                self.kernel.fn_name,
                event.init,
                &fired_this_cycle[..],
                self.args.iter().map(|a| a.is_some()).collect::<Vec<_>>()
            );
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
                    if let Some(v) = ctx.rt.cached().get(id) {
                        event.variables.insert(*id, crate::TagValue::fired(v.clone()));
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
        // with `c=0` must still yield `x`. (The JIT path below behaves the
        // same via #219: a missing input feeds a taint-marked helper-safe
        // placeholder, and the kernel bottoms only if the taken output
        // path consumes it.) `param_opts` slots are placed by per-kind base offset
        // so the order matches `eval_kernel_full`'s declaration-order
        // binding (scalars, arrays, tuples, structs, variants, nullables,
        // strings, values).
        let k = &self.kernel;
        let n_params = k.params.len();
        let mut param_opts: Vec<Option<Value>> = vec![None; n_params];
        // Per-param-slot "fired this cycle", indexed like `param_opts`.
        // A present-but-not-fired param packs its disc with STALE (it
        // carries a cached value that did NOT update this cycle), so the
        // kernel's firing gates (return / set_var) read the node-walk's
        // combineLatest firing faithfully. Stack-resident (≤64 inputs).
        let mut param_fired: smallvec::SmallVec<[bool; 64]> =
            smallvec::smallvec![false; n_params];
        // Sized to the COMBINED slot table (`dyn_slots.len()`), not just
        // the parent's `fn_params`: `dispatch_typed` reads
        // `fn_arg_values[fn_index]` for EVERY slot, and a callee DynCall's
        // `fn_index` (base + local) can exceed `k.fn_params.len()`. The
        // parent's Param/Binding slots (always at the front) get their
        // values set below; callee slots are pre-bound (their value is
        // ignored) and keep their `Null`.
        let mut fn_arg_values: smallvec::SmallVec<[Value; 4]> =
            smallvec::SmallVec::with_capacity(self.dyn_slots.len());
        for _ in 0..self.dyn_slots.len() {
            fn_arg_values.push(Value::Null);
        }
        for (i, kind) in self.arg_layout.iter().enumerate() {
            let v = self.args[i].clone();
            let fired = fired_this_cycle[i];
            match *kind {
                ArgKind::Param(idx) => {
                    param_opts[idx as usize] = v;
                    param_fired[idx as usize] = fired;
                }
                ArgKind::Fn(fn_idx) => {
                    if let Some(v) = v {
                        fn_arg_values[fn_idx as usize] = v;
                    }
                }
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
                    .map(|tv| tv.value_cloned())
                    .or_else(|| ctx.rt.cached().get(bind_id).cloned())?;
                fn_arg_values[fn_idx] = v;
            }
        }
        // JIT dispatch — the unified Value ABI. Every param is two wire
        // words: a disc (a genuine one-hot Value discriminant carrying
        // #219 TAINT / STALE) then the genuine Value payload word. A
        // MISSING input is NOT an abort — it packs the kind's
        // helper-safe placeholder (`Value::Null` / empty ValArray /
        // empty ArcStr) with `TAINT` set, so the kernel runs and
        // bottoms only if the taken path consumes it. A value that
        // doesn't match the compiled slot shape is the
        // never-tvar/obs-4 typechecker-unsoundness class (the static
        // type lied about the runtime value) — treated as MISSING so
        // the runtime survives; the divergence stays visible to the
        // fuzzer as a missing fire.
        let wrapped = &self.jit;
        let taint = TAINT as u64;
        // A present param that did NOT fire this cycle (a retained cached
        // value) carries STALE: its node-walk `Cached` reported `false`,
        // so a consumer fires only if some OTHER input fired. A `None`
        // (never-fired) param keeps TAINT (no value at all).
        let stale = STALE as u64;
        // (disc, payload) words of a `repr(u64)` Value (16 bytes,
        // layout pinned by the const_assert in `emit_helpers`).
        let bits = |v: &Value| -> (u64, u64) {
            let p = v as *const Value as *const u64;
            unsafe { (*p, *p.add(1)) }
        };
        // STAGE one Value per param, in params (= ABI) order: the
        // present value validated against the declared shape, or the
        // kind's tainted placeholder. The staged Values stay alive in
        // this smallvec across the wrapper call (the kernel
        // refcount-bumps what it keeps at entry); the wire words are
        // exactly `bits(&staged)` — with a scalar exception below.
        use kernel_abi::ParamKind;
        let staged: smallvec::SmallVec<[(u64, Value); 16]> = k
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let flag = if param_fired[i] { 0 } else { stale };
                let mismatch = |v: &Value| {
                    log::error!(
                        "kernel param `{}`: runtime {v:?} doesn't match the \
                         compiled {:?} slot (typechecker static/dynamic \
                         mismatch) — treating as bottom",
                        p.name,
                        p.kind,
                    );
                };
                match (&p.kind, param_opts[i].as_ref()) {
                    // Scalars pack via `pack_value_to_u64` (exact
                    // sign/zero extension — a narrow Value's upper
                    // payload bytes are padding, so `bits` alone would
                    // read uninitialized memory). The staged Value is
                    // the ORIGINAL (kept for uniformity; scalars own
                    // nothing).
                    (ParamKind::Scalar(prim), Some(v)) => {
                        match pack_value_to_u64(v, *prim) {
                            Some(payload) => {
                                let disc = prim_to_value_disc(*prim) as u64 | flag;
                                return (disc, Value::Null, payload, true);
                            }
                            None => {
                                mismatch(v);
                                let disc = prim_to_value_disc(*prim) as u64 | taint;
                                return (disc, Value::Null, 0, true);
                            }
                        }
                    }
                    (ParamKind::Scalar(prim), None) => {
                        let disc = prim_to_value_disc(*prim) as u64 | taint;
                        return (disc, Value::Null, 0, true);
                    }
                    (
                        ParamKind::Array { .. }
                        | ParamKind::Tuple { .. }
                        | ParamKind::Struct { .. },
                        v,
                    ) => {
                        let staged = match v {
                            Some(v @ Value::Array(_)) => Some(v.clone()),
                            Some(v) => {
                                mismatch(v);
                                None
                            }
                            None => None,
                        };
                        match staged {
                            Some(v) => {
                                let disc = bits(&v).0 | flag;
                                (disc, v, 0, false)
                            }
                            None => {
                                let v = Value::Array(ValArray::from([]));
                                let disc = bits(&v).0 | taint;
                                (disc, v, 0, false)
                            }
                        }
                    }
                    (ParamKind::String, v) => {
                        let staged = match v {
                            Some(v @ Value::String(_)) => Some(v.clone()),
                            Some(v) => {
                                mismatch(v);
                                None
                            }
                            None => None,
                        };
                        match staged {
                            Some(v) => {
                                let disc = bits(&v).0 | flag;
                                (disc, v, 0, false)
                            }
                            None => {
                                let v = Value::String(arcstr::ArcStr::new());
                                let disc = bits(&v).0 | taint;
                                (disc, v, 0, false)
                            }
                        }
                    }
                    // Variant / Nullable / bare value shapes carry any
                    // Value with its real disc — no shape validation
                    // (a union's member set is the type system's
                    // concern; the kernel's consumers dispatch on the
                    // disc).
                    (
                        ParamKind::Variant { .. }
                        | ParamKind::Nullable { .. }
                        | ParamKind::Value { .. },
                        v,
                    ) => match v {
                        Some(v) => {
                            let disc = bits(v).0 | flag;
                            (disc, v.clone(), 0, false)
                        }
                        None => {
                            let v = Value::Null;
                            let disc = bits(&v).0 | taint;
                            (disc, v, 0, false)
                        }
                    },
                }
            })
            .map(|(disc, staged, scalar_payload, is_scalar)| {
                // Second stage folds the two shapes into (disc,
                // payload-source): scalars carry their packed word,
                // everything else reads the staged Value's payload word
                // at push time (below, while `staged` is alive).
                (disc, if is_scalar { Value::U64(scalar_payload) } else { staged })
            })
            .collect();
        // Pack the wire slots: context words then (disc, payload) per
        // param in ABI (= params) order.
        let mut slots: smallvec::SmallVec<[u64; 16]> =
            smallvec::SmallVec::with_capacity(self.kernel.abi_wire_slots_total());
        slots.push(event.init as u64);
        slots.push(if self.state.is_empty() {
            0
        } else {
            self.state.as_mut_ptr() as u64
        });
        // Slot 2: the per-call-site state block — a CALLEE-only channel
        // (`CTX_WIRE_SLOTS`); a region parent has none.
        slots.push(0);
        for (i, (disc, staged)) in staged.iter().enumerate() {
            slots.push(*disc);
            match &k.params[i].kind {
                ParamKind::Scalar(_) => match staged {
                    Value::U64(payload) => slots.push(*payload),
                    _ => unreachable!("scalar param staged non-U64 payload carrier"),
                },
                _ => slots.push(bits(staged).1),
            }
        }
        // Drift guard: the packed slot count must equal the kernel's
        // declared ABI footprint.
        debug_assert_eq!(
            slots.len(),
            self.kernel.abi_wire_slots_total(),
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
            set_var: set_var_typed::<R, E>,
            state: (&mut state) as *mut _ as *mut u8,
        };
        let prev_handle = DYN_DISPATCH_HANDLE.with(|c| c.replace(&handle as *const _));
        // Always reset the pending flag before the call so
        // we can distinguish "this kernel pended" from
        // "some earlier kernel left the flag set."
        DYNCALL_PENDING.with(|c| c.set(false));
        unsafe {
            f(slots.as_ptr(), out.as_mut_ptr());
        }
        DYN_DISPATCH_HANDLE.with(|c| c.set(prev_handle));
        let pending = DYNCALL_PENDING.with(|c| c.replace(false));
        if pending {
            // A GENUINE whole-kernel abort (interrupt poll, depth
            // trip, the return-gate force, a propagated callee
            // abort) — value-level DynCall pends were converted to
            // #219 taint at their sites and never reach here. The
            // kernel's *out slot holds the pending_exit sentinel
            // (garbage scalar / null pointer); every abort path
            // dropped the owned set before jumping there, so
            // discard and re-fire next cycle.
            //
            // INSIDE an evaluation frame the abort must surface as the
            // TAINTED placeholder, not None: the node-walk's in-frame
            // bottoms are tainted productions (op.rs/error.rs frame
            // gates) that poison downstream slot caches, where a None
            // leaves them holding the PREVIOUS iteration's value — a
            // consumer like `push(res, f(v))` then quietly re-used
            // element 1's result when element 2's kernel bottomed
            // (jul10h 000009). At depth 0 None stays v1.
            if ctx.frame_depth > 0 {
                self.last_out = crate::Tag::TAINT;
                return Some(Value::Null);
            }
            return None;
        }
        // Decode the wrapper's *out pair — the unified Value ABI:
        // every kernel returns the genuine (disc, payload) words of a
        // Value it owns (a scalar's payload widened per
        // `pack_value_to_u64`'s rules, a composite's ValArray bits, a
        // string's ArcStr bits, a value-shape's payload). Route
        // through `TagValue` (the sole raw-words -> Value gateway) so
        // TAINT/STALE bits the kernel leaked are MASKED, not
        // materialized as a corrupt `Value` (the UB class).
        //
        // SAFETY: the kernel's return path wrote a real Value's words
        // into the out slot (the pending path returned before the
        // decode).
        let v = unsafe { TagValue::from_raw(out[0], out[1]) }.value();
        // Fill the RESULT slot (the value channel — see `last_result`).
        self.last_result = Some(v.clone());
        self.last_out = crate::Tag::FIRED;
        Some(v)
    }

    fn out_tag(&self) -> crate::Tag {
        self.last_out
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        for slot in self.args.iter_mut() {
            *slot = None;
        }
        // Sleep restarts the arm: the interior-bottom taint caches are
        // node-walk `Cached` twins, which sleep clears.
        for w in self.jit.replay_state_words.iter() {
            self.state[*w as usize] = 0;
        }
        self.free_reset_chains();
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // The per-arg last-value slots are the kernel's combineLatest
        // memory — replay state, same as sleep clears.
        for slot in self.args.iter_mut() {
            *slot = None;
        }
        // Zero the emitted REPLAY state words (the interior-bottom
        // taint caches — `emit_scalar_taint_cache`): a value cached on
        // iteration i−1 must not bridge iteration i's bottom, exactly
        // the node-walk's per-frame cache reset. Semantic/config words
        // (lifted ids, first-call flags, select memory) survive.
        for w in self.jit.replay_state_words.iter() {
            self.state[*w as usize] = 0;
        }
        self.free_reset_chains();
        if std::env::var_os("GXDBG_RESET").is_some() {
            eprintln!("KERNEL-RESET words={:?}", self.jit.replay_state_words);
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
    use crate::fusion::emit::unpack_u64_to_value;
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
            let bits = pack_value_to_u64(v, *p).expect("matching prim");
            assert_eq!(unpack_u64_to_value(bits, *p), *v);
        }
    }
}
