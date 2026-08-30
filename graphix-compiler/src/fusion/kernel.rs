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
    Apply, BindId, Event, ExecCtx, LambdaId, ModPath, Node, Refs, Rt, Scope, Tag,
    UserEvent,
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
use netidx_value::ValArray;
use netidx_value::Value;
use std::sync::Arc;

// ─── Kernel: the Apply<R, E> wrapper ────────────────────────────

/// Per-DynCall-site state. For each fn-typed param of the kernel we
/// pre-allocate a slot containing the BindIds the side-channel uses,
/// the [`Ref`]-style nodes that read from those BindIds (passed as
/// `from` to the inner Apply's `update`), and the inner Apply
/// instances the dispatches run against.
///
/// SITE IDENTITY (dyncall-site-identity-jul2026): one slot serves ONE
/// static `graphix_dyncall` instruction in the region's compiled code
/// — but that instruction can be reached on behalf of MANY logical
/// call sites (a callee body compiled once, called from several
/// caller emit sites), where the node-walk instantiates the callee
/// body — and therefore the interior builtin's Apply and its
/// CachedArgs — per callsite. Sharing one Apply across those sites
/// let a masked (absent) delivery ride ANOTHER site's cached args
/// (soak jul23f). Each emission site therefore claims one identity
/// WORD from the same state channel selects use (region root →
/// instance word; callee root → per-call-site block word); the
/// dispatcher lazily mints a nonzero id into the word on first use
/// and `instances` keys a full inner Apply per id — cache AND any
/// builtin state get exactly the per-site identity the interp gives
/// them. `current` remains the KEY-0 bucket: sites with no identity
/// word (v1: scaffold-loop bodies, whose per-slot semantics keep the
/// documented init-mask approximation; recursive back-edges, whose
/// null site block is the pre-existing 0-bucket residual) share it,
/// exactly the pre-identity behavior — and it doubles as the SEED
/// for the first minted instance (a slot's dispatches are either all
/// key-0 or all identity-keyed, never mixed, so the roles can't
/// collide).
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
    /// Cached `(LambdaDef pointer, Apply instance)` for KEY-0
    /// dispatches (no identity word — see the struct doc). Invalidated
    /// when a new LambdaDef arrives (different raw pointer) — typical
    /// case is the hot loop where the same callback is reused. For
    /// pre-bound slots (`pre_bound = true`) the pointer is a stable
    /// sentinel and `dispatch` never re-inits. On an identity slot it
    /// instead holds the eagerly pre-bound/pre-inited Apply until the
    /// first mint takes it as that instance's seed.
    current: Option<(*const u8, Box<dyn Apply<R, E>>)>,
    /// Per-SITE-IDENTITY inner Apply instances, keyed by the minted
    /// identity-word value (never 0). Linear scan — a slot rarely has
    /// more than a couple of live sites. An id orphaned by a freed
    /// per-slot site block lingers here until slot death (its
    /// `delete` runs then; the interp deletes at truncation — the
    /// deferred cleanup is a documented v1 residual).
    instances: Vec<(u64, SiteInstance<R, E>)>,
    /// How to construct a FRESH inner Apply when a new site id mints
    /// and the seed is already taken. Captured at pre-bind time.
    recipe: SlotRecipe<R, E>,
    /// `true` when the slot was bound at Kernel construction time
    /// (e.g. `FnSource::Builtin` — the call target is fixed and
    /// can't change). `dispatch` short-circuits the LambdaDef
    /// downcast + rebind check for these slots.
    pre_bound: bool,
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

/// One per-site-identity inner Apply (see [`DynCallSlot`]'s SITE
/// IDENTITY doc): the site's own callee instance, cache and state
/// included, plus its own first-dispatch flag.
struct SiteInstance<R: Rt, E: UserEvent> {
    /// LambdaDef address the Apply was inited from (rebind check for
    /// non-pre-bound slots; a sentinel for pre-bound ones).
    lambda_ptr: *const u8,
    apply: Box<dyn Apply<R, E>>,
    /// `false` until this instance's first dispatch — forces the init
    /// view exactly like the slot-level flag does for the key-0 bucket.
    fired: bool,
}

/// How `dispatch` builds a fresh inner Apply for a newly minted site
/// id once the eager seed (`current`) is taken. The pre-bound
/// variants replicate what their `pre_bind_*` constructed; `Lambda`
/// inits from the LambdaDef value in hand at dispatch (the same path
/// a callback swap takes on the key-0 bucket).
enum SlotRecipe<R: Rt, E: UserEvent> {
    Lambda,
    /// `scope` is the init scope computed at `pre_bind_builtin` (the
    /// builtin lambda DEF's lexical scope when the def resolved) —
    /// per-site mints must init under the same scope as the pre-bound
    /// Apply, or scope-reporting builtins (`log`) diverge per site.
    Builtin {
        init: crate::BuiltInInitFn<R, E>,
        typ: FnType,
        scope: Scope,
    },
    Cast {
        target: crate::typ::Type,
    },
    QopDeliver {
        handler_id: BindId,
        handler_top: ExprId,
        own_top: ExprId,
        spec: Expr,
    },
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
        for (i, arg_kty) in fn_param.arg_types.iter().enumerate() {
            let id = BindId::new();
            bind_ids.push(id);
            // Ref reads `event.variables[id]` (or falls back to
            // `ctx.cached[id]`) on each `update`. The Ref carries the
            // call site's REAL spec and resolved (unfrozen) type when
            // the FnParam recorded them — a builtin that reports
            // source context or renders by type (`dbg`) then behaves
            // like a real call site (dyncall-apply-unwired-aug2026);
            // the frozen `arg_types` stays the marshal authority.
            let typ = fn_param
                .arg_orig_types
                .get(i)
                .cloned()
                .unwrap_or_else(|| arg_kty.clone());
            let spec = fn_param.arg_specs.get(i).cloned().unwrap_or_default();
            let node = Ref::new::<R, E>(id, typ, top_id, spec);
            arg_refs.push(node);
        }
        // The CALL SITE's scope when recorded — the fallback init
        // scope and the dynamic side of the default-compile scope
        // (`pre_bind_builtin` inits under the lambda DEF's lexical
        // scope when the def resolves, mirroring the interp).
        let scope = fn_param.scope.clone().unwrap_or(scope);
        Self {
            bind_ids,
            arg_refs,
            current: None,
            instances: Vec::new(),
            recipe: SlotRecipe::Lambda,
            pre_bound: false,
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
                val.downcast_ref::<LambdaDef<R, E>>().map(|d| {
                    // `LambdaDef.scope` is the def-SITE scope; the
                    // interp compiles defaults and inits the builtin
                    // under the lambda BODY's scope, one block level
                    // deeper (node/lambda.rs
                    // `scope.append_block("fn", id.0)`).
                    let body_lex = ModPath(
                        d.scope
                            .lexical
                            .append(crate::block_component("fn", d.id.0).as_str()),
                    );
                    (d.env.clone(), body_lex)
                })
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
        let mut positional_refs: poolshark::local::LPooled<Vec<Option<Node<R, E>>>> =
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
                    // A default naming an external binding (e.g.
                    // `#esc = default_escape`) needs no priming: the
                    // default node's `Ref` reads the persistent store
                    // through `read_var` (R2) — a standing entry
                    // serves Stale, or Fired under the first
                    // dispatch's forced init view, exactly the interp
                    // callsite's default-arg read.
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
        // The interp inits a builtin's Apply under the builtin-bodied
        // LAMBDA DEF's scope (`init(ctx, .., &def_scope, ..)` in
        // node/lambda.rs), and scope-reporting builtins (`log`) print
        // its lexical path — mirror it when the def resolves. Dynamic
        // scope stays the kernel's own, like the default-compile path
        // above. The recorded call-site scope remains the fallback.
        let init_scope = match &default_env_scope {
            Some((_, lex)) => {
                Scope { dynamic: self.scope.dynamic.clone(), lexical: lex.clone() }
            }
            None => self.scope.clone(),
        };
        let mut apply =
            init(ctx, typ, Some(typ), &init_scope, &self.arg_refs, self.top_id)?;
        // The interp's CallSite runs typecheck0 on a fresh Apply
        // before its first update; mirror it so type-derived builtin
        // state (`dbg`'s rendered type, set in its typecheck0) exists
        // in the pre-bound slot too (dyncall-apply-unwired-aug2026).
        apply.typecheck0(ctx, &mut self.arg_refs)?;
        // Use the slot's own address as a stable sentinel pointer —
        // dispatch checks `pre_bound` first and never reads this.
        let sentinel = self as *const Self as *const u8;
        self.current = Some((sentinel, apply));
        self.recipe = SlotRecipe::Builtin { init, typ: typ.clone(), scope: init_scope };
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
        let apply: Box<dyn Apply<R, E>> = Box::new(CastApply {
            target: target.clone(),
            out: TagValue::phantom(),
            _p: std::marker::PhantomData,
        });
        let sentinel = self as *const Self as *const u8;
        self.current = Some((sentinel, apply));
        self.recipe = SlotRecipe::Cast { target };
        self.pre_bound = true;
    }

    /// Pre-bind a `FnSource::QopDeliver` slot: a `QopDeliverApply`
    /// carrying the catch handler's BindId + the `?`'s spec. The single
    /// `arg_refs[0]` from `DynCallSlot::new` reads the side-channeled
    /// error value the kernel marshals on the qop's error path.
    /// `own_top` is the ORIGINAL `?`'s compile-time top (`Qop.top_id`),
    /// NOT this slot's `top_id` — the slot is constructed with the
    /// region's interior source id (see the `FusedKernel::new` call in
    /// `fusion::try_fuse`), which never equals any registration top;
    /// comparing it forced every fused delivery onto the next-cycle
    /// `set_var` path (caught by the stale-error-deliver pin's trace).
    pub fn pre_bind_qop_deliver(
        &mut self,
        handler_id: BindId,
        handler_top: ExprId,
        own_top: ExprId,
        spec: Expr,
    ) {
        let apply: Box<dyn Apply<R, E>> = Box::new(crate::node::error::QopDeliverApply {
            handler_id,
            handler_top,
            own_top,
            spec: spec.clone(),
            out: TagValue::phantom(),
        });
        let sentinel = self as *const Self as *const u8;
        self.current = Some((sentinel, apply));
        self.recipe = SlotRecipe::QopDeliver { handler_id, handler_top, own_top, spec };
        self.pre_bound = true;
    }

    /// Dispatch the DynCall: look up (or initialize) the inner Apply,
    /// side-channel each arg through its BindId, run `apply.update`,
    /// clean up the BindIds, and return whatever Value the Apply
    /// produced this cycle (or `None` for synchronous-only-v1
    /// "no value yet").
    /// Sleep the slot's bound apply — the arm-sleep twin of
    /// `CallSite::sleep` (which sleeps its callee apply and arg_refs):
    /// a builtin's `CachedArgs` sleeps its staging, an interpreted
    /// callback instance sleeps its whole body. SLEEP IS PAUSE:
    /// `fired` survives, exactly as `CallSite::sleep` keeps
    /// `first_update` — a re-woken site is resumed, not re-primed,
    /// and its next dispatch delivers the honest masks. The init view
    /// a becoming-selected arm grants its interior at depth 0 arrives
    /// through the ARGS (the arm's `init_override` folds the stale
    /// masks, R2 fires the region's inputs), never through a phantom
    /// first dispatch: resetting here made every post-wake dispatch a
    /// forced-init arrival, which inside a tail chain's quiet framed
    /// re-derivation re-fired constant args the interp delivers stale
    /// (aug20a epoch_refire, findings/quiet-frame-init-view-aug2026).
    pub fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some((_, apply)) = &mut self.current {
            apply.sleep(ctx);
        }
        for (_, inst) in &mut self.instances {
            inst.apply.sleep(ctx);
        }
        for n in &mut self.arg_refs {
            n.sleep(ctx);
        }
    }

    /// Semantic teardown for the slot's bound apply — the
    /// region-death twin of `CallSite::delete`. `Drop` alone frees
    /// memory but never runs `delete(ctx)`: an interpreted callback
    /// instance's wake-interest refs (and any published binds) leaked
    /// on every kernel death — LIVE via dynamic-module reloads, which
    /// delete module kernels each swap (C3, 2026-07-20).
    pub fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some((_, mut apply)) = self.current.take() {
            apply.delete(ctx);
        }
        for (_, mut inst) in self.instances.drain(..) {
            inst.apply.delete(ctx);
        }
        for n in &mut self.arg_refs {
            n.delete(ctx);
        }
    }

    pub fn dispatch(
        &mut self,
        lambda_value: &Value,
        ctx: &mut ExecCtx<R, E>,
        event: &mut crate::Event<E>,
        args: &[Value],
        taint_mask: u64,
        stale_mask: u64,
        site_id: u64,
    ) -> Option<TagValue> {
        debug_assert_eq!(args.len(), self.bind_ids.len(), "DynCall arity");
        // Resolve WHICH inner Apply runs: `None` = the key-0 bucket
        // (no identity word — see the struct doc), `Some(i)` = the
        // site's own instance at `instances[i]`.
        let inst_idx = if site_id != 0 {
            Some(self.ensure_instance(site_id, lambda_value, ctx)?)
        } else {
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
            None
        };
        // First dispatch of a fresh inner Apply = its init cycle:
        // labeled-default Nodes (Constants / default exprs) only
        // produce on `event.init`, and the outer cycle may be long
        // past init (an async-fed region's first fire). Force the
        // init view for this one update, then restore. Resolved
        // BEFORE the arg delivery below because `first` also selects
        // the delivery tag.
        let (apply, fired) = match inst_idx {
            None => {
                let cur = self.current.as_mut().unwrap();
                (&mut cur.1, &mut self.fired)
            }
            Some(i) => {
                let inst = &mut self.instances[i].1;
                (&mut inst.apply, &mut inst.fired)
            }
        };
        // `first` is the site's first dispatch EVER — the interp's
        // `Callee::Static::first_update`, a `bound` dispatch that runs
        // under the real init view and seeds its quiet args FIRED at
        // any frame depth (callsite.rs). Sleep keeps it (see `sleep`).
        let first = !*fired;
        *fired = true;
        // Side-channel: stash each arg's HONEST production at its
        // BindId so the arg_refs `Ref` nodes read it inside
        // `apply.update` (Seam B of the 5c flip — the four tag states
        // ride the two masks): a TAINT-masked slot delivers the
        // bottom itself — FreshBottom for a triggering poison,
        // StaleBottom for a standing one — and the wrapper's Q1 arm
        // bottoms the invocation without calling eval (bottom
        // propagates; the old absence/tombstone delivery and its
        // ride-own-history semantics are the re-blessed
        // dyncall-partial-args delta). A STALE-masked slot delivers
        // `TagValue::stale`: present, didn't fire, so production
        // rules that gate on argument FIRING (seam_arg's fired flag,
        // CachedArgs' eval re-run) see the per-argument truth instead
        // of a phantom fire per kernel invocation
        // (dyncall-stale-arg-fired-aug2026). On the FIRST dispatch
        // the init view makes everything an arrival (R2's fresh
        // reader: a standing value or bottom reads fresh), so the
        // STALE bit is honored only after. An entry present for
        // EVERY slot every dispatch also keeps the shared arg Refs'
        // residents out of play (the site-identity rule — a read_var
        // miss would ride ANOTHER site's last delivery).
        let mut set: poolshark::local::LPooled<Vec<BindId>> =
            poolshark::local::LPooled::take();
        for (i, v) in args.iter().enumerate() {
            let id = self.bind_ids[i];
            let standing = !first && stale_mask & (1u64 << i) != 0;
            let tv = if taint_mask & (1u64 << i) != 0 {
                let tag = if standing {
                    crate::Tag::STALE_BOTTOM
                } else {
                    crate::Tag::FRESH_BOTTOM
                };
                crate::TagValue::tagged(Value::Null, tag)
            } else if standing {
                crate::TagValue::stale(v.clone())
            } else {
                crate::TagValue::fired(v.clone())
            };
            event.variables.insert(id, tv);
            set.push(id);
        }
        let saved_init = event.init;
        if first {
            event.init = true;
        }
        // Return the inner Apply's production WHOLE — value and tag
        // (Seam B of the 5c flip): the call site's CLIF decodes the
        // in-band TAINT/STALE bits natively (they are its own
        // currency), so a bottomed invocation reaches the caller as a
        // taint-flagged pair (never as `Value::Null` masquerading as
        // a success — the masked_outer_call_cache_ride SEGV class)
        // and a stale resurface reads as a quiet production instead
        // of a phantom fire.
        let result = {
            let tv = apply.update(ctx, &mut self.arg_refs, event);
            Some(tv.clone())
        };
        if crate::dbgenv::gxdbg_dync() {
            let words = result.as_ref().map(|tv| {
                let tv = std::mem::ManuallyDrop::new(tv.clone());
                unsafe { std::mem::transmute_copy::<crate::TagValue, [u64; 2]>(&*tv) }
            });
            eprintln!("DYNC-RET first={first} prod={words:x?}");
        }
        event.init = saved_init;
        // Cleanup: remove the side-channel entries so a downstream
        // dispatcher (or the outer event loop) doesn't see them.
        for id in set.drain(..) {
            event.variables.remove(&id);
        }
        result
    }

    /// Find or mint the [`SiteInstance`] for `site_id`, rebinding a
    /// non-pre-bound instance whose LambdaDef changed (the per-site
    /// twin of the key-0 rebind). Returns the instance's index in
    /// `instances`; `None` when an init fails (the dispatch is
    /// skipped this cycle, matching the key-0 path's `.ok()?`).
    fn ensure_instance(
        &mut self,
        site_id: u64,
        lambda_value: &Value,
        ctx: &mut ExecCtx<R, E>,
    ) -> Option<usize> {
        let lambda_ptr = if self.pre_bound {
            None
        } else {
            let def =
                lambda_value.downcast_ref::<LambdaDef<R, E>>().unwrap_or_else(|| {
                    panic!(
                        "DynCall: fn-arg value isn't a LambdaDef — \
                         typecheck should have rejected this"
                    )
                });
            Some(def as *const _ as *const u8)
        };
        if let Some(i) = self.instances.iter().position(|(id, _)| *id == site_id) {
            if let Some(p) = lambda_ptr
                && self.instances[i].1.lambda_ptr != p
            {
                // Callback swap at this site: re-init, delete the old.
                let apply = self.mint(lambda_value, ctx)?;
                let mut prev = std::mem::replace(
                    &mut self.instances[i].1,
                    SiteInstance { lambda_ptr: p, apply, fired: false },
                );
                prev.apply.delete(ctx);
            }
            return Some(i);
        }
        // First dispatch for this id. A NON-pre-bound slot's eagerly
        // pre-inited Apply seeds it when its callee matches (that
        // path self-heals from a taken seed: a later key-0 dispatch
        // re-inits via needs_init). A PRE-BOUND slot NEVER surrenders
        // `current`: one slot's dispatches are NOT all-identity or
        // all-key-0 — a recursive back-edge activation executes the
        // same compiled instruction with a NULL site block, and the
        // pre-bound key-0 path relies on `current` being Some (jul23f
        // generate crash_000001: the taken seed left the back-edge
        // dispatch to unwrap None — a panic that can't unwind through
        // the JIT frames, so the process aborts). The recipe mints an
        // equivalent Apply for the identity site instead.
        let seed_ok = !self.pre_bound
            && match (&self.current, lambda_ptr) {
                (Some((p, _)), Some(np)) => *p == np,
                (Some(_), None) => true,
                (None, _) => false,
            };
        let inst = if seed_ok {
            let (p, apply) = self.current.take().unwrap();
            SiteInstance { lambda_ptr: p, apply, fired: self.fired }
        } else {
            let apply = self.mint(lambda_value, ctx)?;
            SiteInstance {
                lambda_ptr: lambda_ptr.unwrap_or(std::ptr::null()),
                apply,
                fired: false,
            }
        };
        self.instances.push((site_id, inst));
        Some(self.instances.len() - 1)
    }

    /// Construct a fresh inner Apply per the slot's recipe. `None` on
    /// init failure.
    fn mint(
        &mut self,
        lambda_value: &Value,
        ctx: &mut ExecCtx<R, E>,
    ) -> Option<Box<dyn Apply<R, E>>> {
        match &self.recipe {
            SlotRecipe::Lambda => {
                let def = lambda_value.downcast_ref::<LambdaDef<R, E>>()?;
                (def.init)(
                    &self.scope,
                    ctx,
                    &mut self.arg_refs,
                    crate::BindMode::Definition,
                    self.top_id,
                )
                .ok()
            }
            SlotRecipe::Builtin { init, typ, scope } => {
                let mut apply =
                    init(ctx, typ, Some(typ), scope, &self.arg_refs, self.top_id).ok()?;
                // Mirror the interp CallSite's fresh-Apply typecheck0
                // (see `pre_bind_builtin`): type-derived builtin state
                // (`dbg`'s rendered type) must exist in per-site
                // instances too (dyncall-apply-unwired-aug2026).
                apply.typecheck0(ctx, &mut self.arg_refs).ok()?;
                Some(apply)
            }
            SlotRecipe::Cast { target } => Some(Box::new(CastApply {
                target: target.clone(),
                out: TagValue::phantom(),
                _p: std::marker::PhantomData,
            })),
            SlotRecipe::QopDeliver { handler_id, handler_top, own_top, spec } => {
                Some(Box::new(crate::node::error::QopDeliverApply {
                    handler_id: *handler_id,
                    handler_top: *handler_top,
                    own_top: *own_top,
                    spec: spec.clone(),
                    out: TagValue::phantom(),
                }))
            }
        }
    }

    /// Subscription refs for every inner Apply this slot holds (the
    /// key-0 bucket plus per-site instances) and the arg-ref side
    /// channel — the slot's share of `Kernel::refs`.
    pub fn refs(&self, refs: &mut Refs) {
        if let Some((_, inner)) = &self.current {
            inner.refs(refs);
        }
        for (_, inst) in &self.instances {
            inst.apply.refs(refs);
        }
        for n in &self.arg_refs {
            n.refs(refs);
        }
        for id in &self.bind_ids {
            refs.bound.insert(*id);
        }
    }
}

/// The `cast<T>(x)` operator as an `Apply`, so a non-inline cast can be
/// dispatched through the same DynCall machinery as a builtin call (see
/// [`FnSource::Cast`]). `update` reads the single side-channeled source
/// value from `from[0]` and runs `target.cast_value(&ctx.env, v)` — the
/// EXACT function `TypeCast::update` (the node-walk) calls, so the two
/// evaluators agree by construction. Produces absent only when the
/// source itself produced no value this cycle.
pub(crate) struct CastApply<R: Rt, E: UserEvent> {
    target: crate::typ::Type,
    out: TagValue,
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
    ) -> &TagValue {
        let Some(src) = from.get_mut(0) else { return TagValue::phantom_ref() };
        let tv = src.update(ctx, event);
        let tag = tv.tag();
        if tag.is_bottom() {
            return if tag.triggers() {
                self.out.set(TagValue::tagged(Value::Null, crate::Tag::FRESH_BOTTOM))
            } else {
                self.out.ride()
            };
        }
        // recompute on triggering productions (or the first stale fill
        // of a bottom resident); quiet rides re-surface the last cast
        if tag.triggers() || self.out.tag().is_bottom() {
            let t = if tag.is_fired() { crate::Tag::FIRED } else { crate::Tag::STALE };
            let v = tv.value_cloned();
            self.out.set(TagValue::tagged(self.target.cast_value(&ctx.env, v), t))
        } else {
            self.out.ride()
        }
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

/// Site-identity id mint (see `dispatch_typed`). Starts at 1 — 0 is
/// the key-0 bucket. Global: ids only need uniqueness within one
/// slot's lifetime, which a process-wide counter gives trivially.
static NEXT_SITE_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);

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
    taint_mask: u64,
    stale_mask: u64,
    site_word: *mut u64,
) -> DynCallRet {
    let state = unsafe { &mut *state_ptr.cast::<DispatcherState<R, E>>() };
    let slots = unsafe { &mut *state.dyn_slots };
    let fn_arg_values = unsafe { &*state.fn_arg_values };
    let ctx = unsafe { &mut *state.ctx };
    let event = unsafe { &mut *state.event };
    // Take ownership of the args buf; the LPooled vec returns to its
    // pool when dropped at function end. (Previously drained into a
    // fresh Vec — a per-DynCall allocation that also forfeited the
    // pool return.)
    let args_vec = unsafe { *Box::from_raw(args) };
    // SITE IDENTITY: the emission site claimed one state word whose
    // ADDRESS rides in `site_word` (null = no identity — key-0 bucket:
    // v1 scaffold-loop sites, recursive back-edges, qop-deliver). A
    // zero word means first-ever dispatch through this site's storage:
    // mint a fresh nonzero id and store it — the word's VALUE is the
    // key, so storage freed and reused (a per-slot site block after a
    // resize) reads 0 again and mints FRESH, exactly the node-walk's
    // fresh per-position instance.
    let site_id = if site_word.is_null() {
        0
    } else {
        let w = unsafe { *site_word };
        if w == 0 {
            let id = NEXT_SITE_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            unsafe { *site_word = id };
            id
        } else {
            w
        }
    };
    let slot = &mut slots[fn_index as usize];
    let lambda_v = &fn_arg_values[fn_index as usize];
    // The tool for "what did the CLIF marshal actually hand this
    // dispatch": raw (disc, payload) words per arg — transmute_copy,
    // never a deref, so it is safe to run on a corrupt Value (it
    // located the masked_outer_call_cache_ride garbage-ArcStr in one
    // run where the SEGV backtrace only named the victim).
    if crate::dbgenv::gxdbg_dync() {
        let words: Vec<[u64; 2]> =
            args_vec.iter().map(crate::tval::value_words).collect();
        eprintln!(
            "DYNC fn={} site={} taint={:b} stale={:b} site_word={:x} args={:x?}",
            fn_index, site_id, taint_mask, stale_mask, site_word as u64, words
        );
    }
    match slot.dispatch(lambda_v, ctx, event, &args_vec, taint_mask, stale_mask, site_id)
    {
        Some(tv) => {
            // Unified Value ABI, honest tags in-band (Seam B): hand
            // back the production's two words — the disc carries the
            // TAINT/STALE tag in its high byte, the call site adapts
            // per its static shape (narrow a scalar payload, adopt
            // owned ValArray/ArcStr bits on the untainted path only,
            // keep a value-shape pair, discard Unit). A bottomed
            // production's payload is its helper-safe placeholder —
            // the call site's taint path never adopts it.
            //
            // SAFETY: TagValue is `#[repr(C)]` (disc, payload) — the
            // same 16-byte layout the Value transmute used, tag bits
            // included. `ManuallyDrop` prevents the local's Drop from
            // running while we transmute its bits out; ownership
            // transfers to the caller.
            let tv = std::mem::ManuallyDrop::new(tv);
            let words: [u64; 2] = unsafe { std::mem::transmute_copy(&*tv) };
            DynCallRet { word0: words[0], word1: words[1] }
        }
        None => {
            // A genuine dispatch abort (an instance init failure) —
            // the JIT'd call site take-and-clears this immediately
            // and converts it to a #219 tainted placeholder that
            // continues, so the bottom stays local to the result's
            // consumers (item 28).
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
/// into a compiled kernel through the same dispatch path it uses for
/// every other function. On each `update` cycle we drive the input
/// nodes, cache their values, decide whether anything fired, and
/// dispatch into native code via the wrapper.
///
/// Generic over `R, E` because the per-DynCall-slot state holds
/// `Box<dyn Apply<R, E>>` and `Node<R, E>`.
pub struct Kernel<R: Rt, E: UserEvent> {
    /// The kernel's ABI contract; the `Arc` is also its identity (the
    /// JIT's `by_kernel` cache keys on the pointer).
    kernel: Arc<KernelSig>,
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
    /// This instance's PER-CALL-SITE block (wire slot 2) when the
    /// compiled body claimed site words — the storage a kernel CALLER
    /// would normally supply. A region parent has no kernel caller, so
    /// it supplies its own here and writes the honor header, which is
    /// what makes its interior taint caches (scrutinee rides, call-
    /// result caches) live rather than inert.
    site: Box<[u64]>,
    /// The kernel's RESULT slot on the value channel — the last value
    /// a run produced, absent until the first run. A region is pure
    /// by construction (effects de-fuse), so when a poll delivers only
    /// STALE productions (an evaluation frame re-running a node-walked
    /// loop around this kernel — the only place stale productions
    /// originate) the retained result is exactly what a re-run would
    /// compute; re-surface it retagged STALE instead of running the
    /// JIT. The `CachedArgs::resident` twin.
    resident: TagValue,
    /// Recursion-shrink reclaim (the JIT twin of the interp's activation
    /// delete). `self_gen` is bumped each invocation and stamped into
    /// every activation block reached (via `SELF_BLOCK_GEN`); after the
    /// run, any per-activation `SelfBlock` NOT stamped with it is freed
    /// (`reclaim_self_block_tree`). `tree_size` is the count of live
    /// activation blocks — the reclaim walk is GATED on the reach count
    /// falling below it (no shrink → no walk), so a stable or growing
    /// recursion pays only the counter, never the O(depth) walk.
    self_gen: u64,
    tree_size: u64,
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
        // PER-ACTIVATION block trees (`SelfBlock`): a recursive call's
        // activations allocate their own blocks lazily, and instance
        // death is the only thing that reclaims them — the node-walk's
        // retained instance tree, freed when its owner dies.
        for b in self.jit.state_self_blocks.iter() {
            let p = std::mem::replace(&mut self.state[b.rel as usize], 0);
            super::emit_helpers::free_self_block_tree(p, &b.slots);
        }
        if let Some(l) = self.jit.own_site.as_ref() {
            for b in l.self_blocks.iter() {
                let p = std::mem::replace(&mut self.site[b.rel as usize], 0);
                super::emit_helpers::free_self_block_tree(p, &b.slots);
            }
        }
        // Anchors inside the block we supplied ourselves: a cross-kernel
        // caller frees the chains in the blocks it hands out, so a
        // region parent must free its own.
        if let Some(l) = self.jit.own_site.as_ref() {
            for a in l.anchors.iter() {
                let p = std::mem::replace(&mut self.site[a.rel as usize], 0);
                super::emit_helpers::free_slot_chain(
                    p,
                    a.own_levels as u64,
                    a.leaf.as_deref(),
                );
            }
        }
        self.drop_replay_values();
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

    /// Drop-and-zero the OWNED-value replay pairs
    /// ([`WrappedKernel::replay_value_pairs`] — the non-scalar
    /// interior-bottom caches, `emit_value_taint_cache`): each holds a
    /// (clean disc, payload) `Value` the emitted code cloned in, disc
    /// 0 = empty. Blind zeroing (the flat replay-word treatment) would
    /// leak the clone. Called from `sleep`/`reset_replay` (cache
    /// clearing) and `Drop` (instance death).
    fn drop_replay_values(&mut self) {
        for w in self.jit.replay_value_pairs.iter() {
            let d = std::mem::replace(&mut self.state[*w as usize], 0);
            let p = std::mem::replace(&mut self.state[*w as usize + 1], 0);
            if d != 0 {
                // SAFETY: the words were written by this kernel's own
                // emitted code as the (clean disc, payload) of an owned
                // `graphix_value_clone` result — a valid `Value` bit
                // pattern; the nonzero-disc guard excludes empty.
                drop(unsafe { TagValue::from_raw(d, p) });
            }
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
        lifted_ids: &[BindId],
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
        // BindIds — the PER-INSTANCE ids the splice minted, parallel to
        // `KernelSig::lifted` (which keeps the compile-time ids for
        // slot layout only). Seeding the sig's own ids here made every
        // instance of a multi-instance body (a lambda-body region
        // compiled per apply — a MapQ slot callback, a retained rec
        // activation) write ONE shared variable while the interp binds
        // per instance (aug18a class 3: n same-var writes queued one
        // per cycle burned n worked cycles, and an in-recursion
        // connect spun forever).
        debug_assert_eq!(lifted_ids.len(), kernel.lifted.len());
        for (i, id) in lifted_ids.iter().enumerate() {
            state[i] = id.inner();
        }
        // The parent's OWN call-site block, when its body was compiled
        // to the callee ABI. Honoring it (the header word) is what
        // activates the body's interior taint caches; the reset
        // contract they need is this node's `reset_replay`, below.
        let mut site =
            vec![0u64; wrapped.own_site.as_ref().map(|l| l.words as usize).unwrap_or(0)]
                .into_boxed_slice();
        if let Some(l) = wrapped.own_site.as_ref()
            && let Some(h) = l.replay_hdr
        {
            site[h as usize] = 1;
        }
        let mut node = Self {
            kernel,
            jit: wrapped,
            dyn_slots,
            arg_layout,
            state,
            site,
            resident: TagValue::phantom(),
            self_gen: 0,
            tree_size: 0,
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
                if let Some(v) = ctx.rt.store_value(bind_id) {
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
            if let FnSource::QopDeliver { handler_id, handler_top, own_top, spec } =
                &fp.source
            {
                self.dyn_slots[fn_idx].pre_bind_qop_deliver(
                    *handler_id,
                    *handler_top,
                    *own_top,
                    spec.clone(),
                );
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
    ) -> &TagValue {
        // Poll every feeder once and take its production HONESTLY
        // (Seam A of the 5c flip): the production's tag IS the staging
        // truth — no retained arg slots, no reconstructed fired flags.
        // The dense interp delivers every awake node's production
        // every cycle, so a quiet feeder's ride carries the same value
        // the old retained slot held, tagged STALE; the R2 store read
        // is the wake/arm-replay memory the slots duplicated. A
        // bottomed feeder (fresh, standing, or the never-produced
        // phantom) carries no value — the staging below packs the
        // param kind's helper-safe placeholder with TAINT, bare for a
        // TRIGGERING bottom (a poison event this cycle), TAINT|STALE
        // for a standing one (nothing new — must not fire loop/select
        // machinery). The kernel invokes iff any production TRIGGERED
        // (fired or FreshBottom) — the R1 recompute-skip otherwise.
        let mut any_updated = false;
        let mut polled: smallvec::SmallVec<[(Tag, Option<Value>); 16]> =
            smallvec::SmallVec::with_capacity(from.len());
        for src in from.iter_mut() {
            let tv = src.update(ctx, event);
            let tag = tv.tag();
            if tag.triggers() {
                any_updated = true;
            }
            let v = if tag.is_bottom() { None } else { Some(tv.value_cloned()) };
            polled.push((tag, v));
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
        if crate::dbgenv::gxdbg_kpoll() {
            eprintln!(
                "KPOLL {} init={} any_updated={any_updated} tags={:?} present={:?} fd={}",
                self.kernel.fn_name,
                event.init,
                polled.iter().map(|(t, _)| t.bits()).collect::<Vec<_>>(),
                polled.iter().map(|(_, v)| v.is_some()).collect::<Vec<_>>(),
                ctx.frame_depth
            );
        }
        if !any_updated {
            // Nothing triggered: the R1 skip — the region is pure and
            // its inputs' values are unchanged since the last run, so
            // the resident IS what a re-run would compute. RIDE it (the
            // dense quiet production: STALE set in place, bottomness
            // kept); a never-run kernel rides its phantom. This is the
            // 5c output flip's quiet arm — the old any_produced/absent
            // split was the sparse depth-0 hole (a quiet kernel
            // vanished where every dense node delivers).
            return self.resident.ride();
        }
        if crate::dbgenv::graphix_dbg_invoke() {
            eprintln!(
                "KERNEL INVOKE {} init={} fired={:?} present={:?}",
                self.kernel.fn_name,
                event.init,
                polled.iter().map(|(t, _)| t.is_fired()).collect::<Vec<_>>(),
                polled.iter().map(|(_, v)| v.is_some()).collect::<Vec<_>>()
            );
        }
        // Test instrumentation: a fused kernel has committed to
        // running this cycle (JIT or interp). Bump the fused-kernel
        // execution counter so the test harness can distinguish
        // "fused but ran on interp" from "no fusion". The JIT path
        // additionally bumps `JIT_INVOCATIONS` inside its wrapper.
        #[cfg(debug_assertions)]
        record_fusion_invocation();
        // Build the kernel's value-bearing args in params (= source)
        // order (`param_opts`) plus the fn-arg values for the DynCall
        // dispatcher. A MISSING input is NOT a whole-kernel abort: it
        // feeds `None` (bottom) into `param_opts`, and the kernel emits
        // `None` only if the OUTPUT consumes that bottom — `select c
        // { 0 => x, 1 => never_fired }` with `c=0` must still yield `x`
        // (#219: a missing input packs a taint-marked helper-safe
        // placeholder, and the kernel bottoms only if the taken output
        // path consumes it).
        let k = &self.kernel;
        let n_params = k.params.len();
        let mut param_opts: smallvec::SmallVec<[Option<Value>; 16]> =
            smallvec::smallvec![None; n_params];
        // Per-param production TAG, indexed like `param_opts` — the
        // honest staging truth (Seam A): a present-but-not-fired param
        // packs STALE, a triggering bottom packs bare TAINT, a
        // standing bottom TAINT|STALE. An unwired param (not in
        // `arg_layout` — shouldn't happen) defaults to the standing
        // bottom.
        let mut param_tags: smallvec::SmallVec<[Tag; 16]> =
            smallvec::smallvec![Tag::STALE_BOTTOM; n_params];
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
            let (tag, v) = std::mem::replace(&mut polled[i], (Tag::STALE_BOTTOM, None));
            match *kind {
                ArgKind::Param(idx) => {
                    param_opts[idx as usize] = v;
                    param_tags[idx as usize] = tag;
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
        // neither has a value yet, the kernel can't run — RIDE (a
        // never-run kernel rides its phantom, a bottom-flavored
        // production, not the sparse vanish) and try again next cycle.
        for (fn_idx, fp) in self.kernel.fn_params.iter().enumerate() {
            if let FnSource::Binding { bind_id } = &fp.source {
                let v = event
                    .variables
                    .get(bind_id)
                    .map(|tv| tv.value_cloned())
                    .or_else(|| ctx.rt.store_value(bind_id));
                match v {
                    Some(v) => fn_arg_values[fn_idx] = v,
                    None => return self.resident.ride(),
                }
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
        // A present param that did NOT fire this cycle carries STALE
        // (a value-channel ride: a consumer fires only if some OTHER
        // input fired). A bottomed param packs TAINT — bare for a
        // TRIGGERING bottom (a poison event this cycle), TAINT|STALE
        // for a standing one (a re-delivered bottom must not fire
        // loop/select machinery) — the honest per-param tag from
        // `polled` (Seam A), a shape MISMATCH being the one locally
        // minted fresh poison.
        let taint = TAINT as u64;
        let stale = STALE as u64;
        // (disc, payload) words of a `repr(u64)` Value (16 bytes,
        // layout pinned by the const_assert in `emit_helpers`).
        // Routed through `value_words` — a raw two-word read types out
        // the UNDEF payload lane of dataless/narrow variants (a
        // value-shaped param can stage `Value::Null` or a `Bool`),
        // the release-only poison class the aug13a fleet gate caught.
        let bits = |v: &Value| -> (u64, u64) {
            let [d, p] = crate::tval::value_words(v);
            (d, p)
        };
        // STAGE `(disc, payload word, keepalive Value)` per param, in
        // params (= ABI) order, in ONE pass: the present value
        // validated against the declared shape, or the kind's tainted
        // placeholder. The payload word is read while the staged Value
        // is on this stack and stays VALID across its move into the
        // smallvec (it's the Arc/inline payload word, unaffected by
        // the move); the keepalive Value holds the refcount across the
        // wrapper call (the kernel refcount-bumps what it keeps at
        // entry). Scalars keep nothing alive — their payload comes
        // from `pack_value_to_u64` (exact sign/zero extension: a
        // narrow Value's upper payload bytes are padding, so `bits`
        // alone would read uninitialized memory).
        use kernel_abi::ParamKind;
        let staged: smallvec::SmallVec<[(u64, u64, Value); 16]> = k
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let ptag = param_tags[i];
                let flag = if ptag.is_fired() { 0 } else { stale };
                // The bottom flags: TAINT plus STALE for a standing
                // bottom (a triggering one stages bare TAINT).
                let bflag = taint | if ptag.triggers() { 0 } else { stale };
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
                    (ParamKind::Scalar(prim), Some(v)) => {
                        match pack_value_to_u64(v, *prim) {
                            Some(payload) => {
                                let disc = prim_to_value_disc(*prim) as u64 | flag;
                                (disc, payload, Value::Null)
                            }
                            None => {
                                mismatch(v);
                                let disc = prim_to_value_disc(*prim) as u64 | taint;
                                (disc, 0, Value::Null)
                            }
                        }
                    }
                    (ParamKind::Scalar(prim), None) => {
                        let disc = prim_to_value_disc(*prim) as u64 | bflag;
                        (disc, 0, Value::Null)
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
                                let (disc, payload) = bits(&v);
                                (disc | flag, payload, v)
                            }
                            None => {
                                let v = Value::Array(ValArray::from([]));
                                let (disc, payload) = bits(&v);
                                (disc | bflag, payload, v)
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
                                let (disc, payload) = bits(&v);
                                (disc | flag, payload, v)
                            }
                            None => {
                                let v = Value::String(arcstr::ArcStr::new());
                                let (disc, payload) = bits(&v);
                                (disc | bflag, payload, v)
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
                            let v = v.clone();
                            let (disc, payload) = bits(&v);
                            (disc | flag, payload, v)
                        }
                        None => {
                            let v = Value::Null;
                            let (disc, payload) = bits(&v);
                            (disc | bflag, payload, v)
                        }
                    },
                }
            })
            .collect();
        // Pack the wire slots: context words then (disc, payload) per
        // param in ABI (= params) order.
        let mut slots: smallvec::SmallVec<[u64; 16]> =
            smallvec::SmallVec::with_capacity(self.kernel.abi_wire_slots_total());
        // Slot 0: the invocation-uniform init view the emitted
        // const_stale_gate reads (bit 0) — inside a frame the honest
        // view is `ctx.frame_init` (frames force `event.init`, so the
        // raw flag fired every in-frame const per pass — the Constant
        // node's own gate, node/mod.rs), the bind.rs/lambda.rs idiom
        // at the wire slot — and the QUIET bit (bit 1): the invocation
        // re-derives inside a frame that is not its own init, where a
        // re-selection or a first call is loop plumbing and grants no
        // init view (`LowerCtx::quiet_flag`).
        let init = if ctx.frame_depth > 0 { ctx.frame_init } else { event.init };
        let quiet = ctx.frame_depth > 0 && !ctx.frame_init;
        slots.push(init as u64 | (quiet as u64) << 1);
        slots.push(if self.state.is_empty() {
            0
        } else {
            self.state.as_mut_ptr() as u64
        });
        // Slot 2: the per-call-site state block. Supplied by the CALLER
        // for a cross-kernel call; a region parent supplies its own
        // (empty unless its body was compiled to the callee ABI).
        slots.push(if self.site.is_empty() { 0 } else { self.site.as_mut_ptr() as u64 });
        for (disc, payload, _keepalive) in staged.iter() {
            slots.push(*disc);
            slots.push(*payload);
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
        // Recursion-shrink reclaim setup (the JIT twin of the interp's
        // activation delete): only for kernels that carry a self-block
        // tree. Bump this instance's generation and stamp it into every
        // activation reached during the call (via `SELF_BLOCK_GEN` in
        // `graphix_site_child_block`), resetting the reach counter; save
        // the enclosing thread-local values so a NESTED kernel restores
        // ours (its reaches must not count toward this tree).
        let has_self_blocks = !self.jit.state_self_blocks.is_empty()
            || self.jit.own_site.as_ref().is_some_and(|l| !l.self_blocks.is_empty());
        let (shrink_gen, saved_gen, saved_reached) = if has_self_blocks {
            self.self_gen = self.self_gen.wrapping_add(1);
            let sg = super::emit_helpers::SELF_BLOCK_GEN.with(|c| c.replace(self.self_gen));
            let sr = super::emit_helpers::SELF_BLOCK_REACHED.with(|c| c.replace(0));
            (Some(self.self_gen), sg, sr)
        } else {
            (None, 0, 0)
        };
        // Value-hook loan (the core-trait seam): `graphix_value_eq`
        // and every other helper comparing or printing Values inside
        // this invocation honors core Eq/Ord/Display implementations,
        // exactly as the interp's armed operators do.
        crate::node::coretraits::with_value_hooks(ctx, event, |_, _| unsafe {
            f(slots.as_ptr(), out.as_mut_ptr());
        });
        DYN_DISPATCH_HANDLE.with(|c| c.set(prev_handle));
        let pending = DYNCALL_PENDING.with(|c| c.replace(false));
        // Recursion-shrink reclaim + restore the reach thread-locals.
        // MUST run on every exit path (the pending branch below returns
        // early), so it sits before that branch. On a clean run: if the
        // reach count fell below the live tree size, some depth was not
        // re-reached — free the unreached activation subtrees and null
        // their words (`Kernel::drop`/`reset_replay` then see 0 and
        // never double-free); update the tree size. A PENDING (aborted)
        // run reached only a prefix, so its reach count is not a
        // shrink signal — skip the reclaim, just restore.
        if let Some(generation) = shrink_gen {
            use super::emit_helpers::{
                SELF_BLOCK_GEN, SELF_BLOCK_REACHED, reclaim_self_block_tree,
            };
            let reached = SELF_BLOCK_REACHED.with(|c| c.get());
            if !pending {
                if reached < self.tree_size {
                    let jit = self.jit.clone();
                    for b in jit.state_self_blocks.iter() {
                        reclaim_self_block_tree(
                            (&mut self.state[b.rel as usize]) as *mut u64,
                            b.words as usize,
                            &b.slots,
                            generation,
                        );
                    }
                    if let Some(l) = jit.own_site.as_ref() {
                        for b in l.self_blocks.iter() {
                            reclaim_self_block_tree(
                                (&mut self.site[b.rel as usize]) as *mut u64,
                                b.words as usize,
                                &b.slots,
                                generation,
                            );
                        }
                    }
                }
                self.tree_size = reached;
            }
            SELF_BLOCK_GEN.with(|c| c.set(saved_gen));
            SELF_BLOCK_REACHED.with(|c| c.set(saved_reached));
        }
        if pending {
            // A GENUINE whole-kernel abort (interrupt poll, depth
            // trip, a propagated callee abort) — value-level DynCall
            // pends were converted to #219 taint at their sites and
            // never reach here, and the return-gate force is GONE (5c:
            // a stale/bottom result returns honestly, decoded below).
            // The kernel's *out slot holds the pending_exit sentinel
            // (garbage scalar / null pointer); every abort path
            // dropped the owned set before jumping there, so nothing
            // to decode. Split by CAUSE:
            //
            // - A DEPTH TRIP is a DELIVERED FreshBottom, not silence —
            //   the interp's tripped dispatch mints one and its
            //   consumers poison (missing_fire_epoch3_aug08e). Peek,
            //   don't take: the wrapping `FusedKernel` takes the flag
            //   for the diagnostic.
            // - An in-frame abort poisons the frame's slot caches
            //   (jul10h 000009 — an absence left them riding the
            //   previous iteration's value).
            // - An interrupt abort is "re-fire next cycle": nothing
            //   was computed — ride.
            if crate::dbgenv::graphix_dbg_invoke() {
                eprintln!(
                    "KERNEL RESULT {} PENDING fd={}",
                    self.kernel.fn_name, ctx.frame_depth
                );
            }
            if ctx.frame_depth > 0 {
                return TagValue::bottom_null(true);
            }
            return self.resident.ride();
        }
        // Decode the wrapper's *out pair — the unified Value ABI:
        // every kernel returns the genuine (disc, payload) words of a
        // Value it owns (a scalar's payload widened per
        // `pack_value_to_u64`'s rules, a composite's ValArray bits, a
        // string's ArcStr bits, a value-shape's payload), with its
        // honest TAINT/STALE tag riding the disc in-band (the return
        // gate is gone). Route through `TagValue` (the sole raw-words
        // -> Value gateway): `.value()` masks the tag bits before the
        // `Value` materializes, so a flagged disc never reaches a
        // clone/drop (the UB class).
        //
        // SAFETY: the kernel's return path wrote a real Value's words
        // into the out slot (the pending path returned before the
        // decode).
        let tv = unsafe { TagValue::from_raw(out[0], out[1]) };
        let tag = tv.tag();
        if crate::dbgenv::graphix_dbg_invoke() {
            eprintln!("KERNEL RESULT {} tag={tag:?} pending=false", self.kernel.fn_name);
        }
        if tag.is_bottom() {
            // A bottomed result: the returned words own a helper-safe
            // placeholder — free it and PERSIST the bottom on the value
            // channel (the interp op twin, node/op.rs): the R1 quiet
            // ride must deliver StaleBottom afterwards, because a
            // re-run against the same inputs would bottom again. Riding
            // the pre-bottom value instead let a de-fused consumer
            // re-fire it as real (soak aug14f).
            drop(tv.value());
            return self.resident.set(TagValue::tagged(Value::Null, tag));
        }
        // Fill the RESULT slot (the value channel — see `resident`)
        // and lend it: Fired for a fresh result, Stale for a quiet
        // one (the output chain didn't fire this invocation).
        let v = tv.value();
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        // Semantic teardown for the dyn slots' bound applies. The
        // Apply-trait default no-op left interpreted callback
        // instances' wake-interest refs leaking on every kernel death
        // — reached constantly via dynamic-module reloads (each swap
        // deletes the old module graph, kernels included) and any
        // region deletion (C3, 2026-07-20). Memory was never leaked
        // (`Drop` runs); the SEMANTIC cleanup was.
        for slot in self.dyn_slots.iter_mut() {
            slot.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        if crate::dbgenv::gxdbg_kernel_sleep() {
            eprintln!("KERNEL-APPLY-SLEEP {}", self.kernel.fn_name);
        }
        // Arm-wake replay memory is the R2 store read (a re-woken
        // arm's feeders read their standing store entries as Fired
        // under the forced init view) — the kernel retains no per-arg
        // slots (Seam A of the 5c flip).
        //
        // SLEEP IS PAUSE (Eric's ruling 2026-07-31): the interior-bottom
        // taint caches are the kernel twins of the interp's `Held` ride
        // residents, and those survive an arm's sleep — a re-selected
        // arm whose fresh computation bottoms RIDES its history. So
        // sleep clears NOTHING here; only a frame reset
        // (`reset_replay`) and instance death (`Drop`) do. Arm-region
        // fusion (2026-08-14) made this path live, and the day it did
        // the stale clearing became a missing fire: an arm-position
        // kernel returned to a bottomed scrutinee with no ride history
        // while the node-walk rode its own. The arm-rewake RESTART
        // builtins never reach here — the `SLEEP_RESTARTS` interior-
        // sleep gate de-fuses any arm that contains one.
        //
        // The dyn slots' bound applies sleep like any node-walked
        // callee's would (`CallSite::sleep` sleeps its apply).
        for slot in self.dyn_slots.iter_mut() {
            slot.sleep(ctx);
        }
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // Zero the emitted REPLAY state words (the interior-bottom
        // taint caches — `emit_scalar_taint_cache`): a value cached on
        // iteration i−1 must not bridge iteration i's bottom, exactly
        // the node-walk's per-frame cache reset. Semantic/config words
        // (lifted ids, first-call flags, select memory) survive.
        for w in self.jit.replay_state_words.iter() {
            self.state[*w as usize] = 0;
        }
        // The same contract for the block we own as our own caller
        // (`site`): honoring those caches obliges us to reset them.
        if let Some(l) = self.jit.own_site.as_ref() {
            for w in l.replay.iter() {
                self.site[*w as usize] = 0;
            }
        }
        // And through every per-activation block tree: a recursive
        // activation's caches are honored (the header rides down from
        // its parent), so they are reset with everything else.
        for b in self.jit.state_self_blocks.iter() {
            super::emit_helpers::reset_self_block_tree(
                self.state[b.rel as usize],
                &b.slots,
                &b.replay,
            );
        }
        if let Some(l) = self.jit.own_site.as_ref() {
            for b in l.self_blocks.iter() {
                super::emit_helpers::reset_self_block_tree(
                    self.site[b.rel as usize],
                    &b.slots,
                    &b.replay,
                );
            }
        }
        self.drop_replay_values();
        self.free_reset_chains();
        if crate::dbgenv::gxdbg_reset() {
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
            slot.refs(refs);
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
