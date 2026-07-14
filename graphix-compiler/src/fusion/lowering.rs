//! Fusion support library: builtin-call discovery, kernel signature
//! derivation (lambda callees and body sub-regions), and the
//! abstract-type resolver. Body code generation
//! lives with each node (`Update::emit_clif` / `Apply::emit_clif`,
//! see `design/distributed_jit.md`); this module supplies the
//! analysis those emitters consume.

use crate::{
    BindId, ExecCtx, Node, NodeView, Refs, Rt, Update, UserEvent,
    env::Env,
    expr::{ExprId, ExprKind, ModPath},
    fusion::{
        self,
        kernel_abi::{
            self, AbiKind, AbstractRegistry, BuiltinSlot, FnParam, FnSource, KernelSig,
            Seen, abi_kind, freeze_for_abi_normalized, scalar_prim,
        },
    },
    node::{Cached, callsite::CallSite, lambda::GXLambda},
    typ::{FnArgKind, FnType, Type},
};
use arcstr::ArcStr;
use netidx_value::Value;
use poolshark::local::LPooled;

// Re-export the canonical kernel-ABI types so existing callers
// (graphix-shell, in-tree tests) keep compiling. The definitive home
// is `kernel_abi`.
pub use kernel_abi::{Input, KnownFusedFn, PrimType};

/// Cached entry in [`FusionCtx::kernels`]. One per
/// `(LambdaId, Arc<FnType>)` monomorphization of a lambda definition.
///
/// `fn_name` is the synthetic kernel name call sites resolve against
/// (`funcids` / `callee_refs` in the JIT's declare/define phases).
#[derive(Debug, Clone)]
pub struct CachedKernel {
    pub fn_name: ArcStr,
    pub kernel: std::sync::Arc<KernelSig>,
    pub signature: KnownFusedFn,
    /// Captured outer-scope bindings the lambda body references,
    /// lifted to extra positional kernel arguments (closure
    /// conversion). Appended to the kernel signature *after* the
    /// formal args, in this list's order. Empty for non-capturing
    /// lambdas. The caller (`emit_lambda_call_node`) forwards each
    /// capture's current value as an extra call arg.
    pub captures: Vec<CaptureSlot>,
    /// The body references its own binding (self-recursion — tail or
    /// not).
    pub is_rec: bool,
    /// The binding the kernel was built FROM (the call site's fnode
    /// Ref id), when there was one. For a recursive callee the direct
    /// path's Node body emission recognises self-calls by this id.
    pub self_bind: Option<BindId>,
    /// Sync builtin/cast/qop Apply sites discovered in this kernel's
    /// BODY (`walk_node_for_builtin_calls`), parallel to the
    /// `fn_params` slots installed on `kernel`. The body emitter
    /// (`CallSite::emit_clif` via `BodyCx::builtin_site`) lowers each
    /// registered site to a DynCall against its slot. Empty for a body
    /// with no fusable builtin calls.
    pub apply_sites: nohash::IntMap<ExprId, BuiltinCallSiteInfo>,
}

/// One captured outer-scope binding lifted into a lambda kernel's
/// signature. See [`CachedKernel::captures`].
#[derive(Debug, Clone)]
pub struct CaptureSlot {
    /// The captured binding's `BindId` in the enclosing lexical
    /// environment. The caller resolves the capture's value by
    /// looking this id up in the parent kernel's input slots
    /// (`lookup_local_by_bind_id`) — BindId-keyed so a same-named
    /// shadow in the parent doesn't mis-route.
    pub bind_id: BindId,
    /// Source-level name of the captured binding. Used as the kernel
    /// input slot name (so the body's `Ref` resolves) and as the
    /// const-fallback lookup key.
    pub name: ArcStr,
    /// Frozen type of the capture — for caller-side typecheck and slot
    /// classification.
    pub typ: Type,
}

/// Per-Apply-site info captured by [`walk_node_for_builtin_calls`]
/// for `CallSite::emit_clif`'s builtin DynCall arm.
///
/// `fn_index` is the slot index in `KernelSig.fn_params` for this
/// call site's [`FnSource::Builtin`] entry.
///
/// `marshal_arg_indices` maps each kernel-level marshalled DynCall
/// arg position to its index in `Apply.args` (source order). At
/// emit time, the DynCall emitter walks these in order and lowers
/// each named `Apply.args[idx].1` for the DynCall `args` vector.
/// Skip-positions (labeled args resolved to defaults)
/// are not in this list — they have no marshalled value; the
/// builtin slot's `LabeledDefault` handles them runtime-side.
///
/// For variadic builtins, every variadic positional consumed at the
/// call site appears as its own entry here (each filling one
/// position in the marshalled args list, all with the same
/// arg-type clone of the variadic element type).
#[derive(Debug, Clone)]
pub struct BuiltinCallSiteInfo {
    pub fn_index: u32,
    pub marshal_arg_indices: Vec<usize>,
    pub arg_types: Vec<Type>,
    pub return_type: Type,
}

/// Output of [`walk_node_for_builtin_calls`].
#[derive(Debug, Default, Clone)]
pub struct BuiltinCallDiscovery {
    /// `FnParam` slots to install in the kernel sig's `fn_params` —
    /// one per discovered builtin Apply site, in walk order.
    pub fn_params: Vec<FnParam>,
    /// `Apply.spec.id → BuiltinCallSiteInfo` lookup so
    /// `CallSite::emit_clif` can recognise the call site at emit
    /// time and lower it to a DynCall.
    pub apply_sites: nohash::IntMap<ExprId, BuiltinCallSiteInfo>,
}

/// Walk a Node subtree to discover builtin Apply sites: every
/// `CallSite` whose target resolves to a sync builtin binding
/// registered in `ctx.builtin_bindings` gets a
/// [`FnParam`] slot ([`FnSource::Builtin`])
/// and an `apply_sites` entry, capturing the layout `emit_dyncall`
/// needs to dispatch via the runtime's builtin Apply machinery.
/// Descent is [`fusion::for_each_emitted_node`]: ordinary lambda bodies
/// remain separate kernels, while compiler-owned collection callbacks
/// are included because their bodies emit inline in the caller. Each
/// site reads the CallSite's resolved FnType and its compiled arg/return
/// sub-nodes' types (post-typecheck), never the AST.
///
/// Sites whose argument shape can't be lowered (unsupported arg
/// type, async builtin, mismatched arity, …) are simply omitted —
/// emission then fails on the un-registered site and the region
/// stays unfused.
pub fn walk_node_for_builtin_calls<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    ctx: &ExecCtx<R, E>,
    out: &mut BuiltinCallDiscovery,
) {
    fusion::for_each_emitted_node(node, &mut |n| match n.view() {
        NodeView::CallSite(cs) => {
            try_register_builtin_call_from_callsite(cs, ctx, out);
        }
        NodeView::TypeCast(tc) => try_register_cast(tc, ctx, out),
        NodeView::Qop(q) => try_register_qop_deliver(q, ctx, out),
        _ => {}
    });
}

/// Register the error-delivery DynCall for a handler-ful `?` (a `?`
/// caught by an enclosing `try`), if `emit_qop_node` will lower it
/// in-kernel. A handler-LESS `?` needs nothing (its error path is just
/// bottom). The error value delivered is always a 2-word `Value::Error`,
/// independent of the `?`'s SUCCESS type — so this registers for any
/// success type `emit_qop_node` lowers (scalar OR string/composite/
/// value-shape). A `Unit`/`Null` success would Err in emit (the region
/// de-fuses, the slot goes unused — harmless).
fn try_register_qop_deliver<R: Rt, E: UserEvent>(
    q: &crate::node::error::Qop<R, E>,
    ctx: &ExecCtx<R, E>,
    out: &mut BuiltinCallDiscovery,
) {
    let Some(handler_id) = q.id else { return };
    let reg = &ctx.fusion.abstract_registry;
    let Some(inner_typ) = freeze_for_abi_normalized(reg, q.n.typ()) else { return };
    if kernel_abi::nullable_inner(reg, &inner_typ).is_none() {
        return;
    }
    // The delivered arg is the inner's full Nullable value (the `Error` on
    // the error path) — Value-shape (2-word). `return_type` is unused: the
    // emitter hardcodes the Unit DynCall ret_kind and `QopDeliverApply`
    // returns `Null`; carry the arg type as a harmless placeholder.
    let fn_index = out.fn_params.len() as u32;
    out.fn_params.push(FnParam {
        name: arcstr::literal!("<qop_deliver>"),
        source: FnSource::QopDeliver { handler_id, spec: q.spec.clone() },
        arg_types: vec![inner_typ.clone()],
        return_type: inner_typ.clone(),
    });
    out.apply_sites.insert(
        q.spec.id,
        BuiltinCallSiteInfo {
            fn_index,
            marshal_arg_indices: vec![0],
            arg_types: vec![inner_typ.clone()],
            return_type: inner_typ,
        },
    );
}

/// Register a `cast<T>(x)` site as a one-argument DynCall to the cast
/// machinery, if it can't be emitted inline. A scalar→scalar cast stays
/// inline (`emit_cast_node`'s `compile_cast` fast path — pure register
/// arithmetic); ONLY a cast with a non-scalar source (e.g. a `datetime`)
/// or non-scalar target needs the machinery call. Registers a
/// `FnSource::Cast` slot + an `apply_sites` entry exactly like a builtin
/// call, so `emit_cast_node` lowers it through the shared
/// `emit_dyncall_node` path. The decision here MUST mirror
/// `emit_cast_node`'s inline test, or the site would (de)register out of
/// step with emission.
fn try_register_cast<R: Rt, E: UserEvent>(
    tc: &crate::node::TypeCast<R, E>,
    ctx: &ExecCtx<R, E>,
    out: &mut BuiltinCallDiscovery,
) {
    let reg = &ctx.fusion.abstract_registry;
    let source = tc.n.typ();
    // Inline-able scalar→scalar cast: no slot (matches emit_cast_node's
    // fast path — NUMERIC prims only; a bool cast needs the machinery
    // DynCall, so it registers a slot like any non-inline cast).
    if scalar_prim(reg, source).is_some_and(|p| p.is_numeric())
        && PrimType::from_type(&tc.target).is_some_and(|p| p.is_numeric())
    {
        return;
    }
    // The source must marshal across the DynCall ABI, and the cast's
    // fallible result type (`[T, Error]`) must decode — otherwise the
    // site can't be lowered and emission fails on the un-registered
    // node (the region stays unfused).
    let Some(arg_frozen) = freeze_for_abi_normalized(reg, source) else { return };
    let Some(ret_frozen) = freeze_for_abi_normalized(reg, &tc.typ) else { return };
    let fn_index = out.fn_params.len() as u32;
    out.fn_params.push(FnParam {
        name: arcstr::literal!("<cast>"),
        source: FnSource::Cast { target: tc.target.clone() },
        arg_types: vec![arg_frozen.clone()],
        return_type: ret_frozen.clone(),
    });
    out.apply_sites.insert(
        tc.spec.id,
        BuiltinCallSiteInfo {
            fn_index,
            marshal_arg_indices: vec![0],
            arg_types: vec![arg_frozen],
            return_type: ret_frozen,
        },
    );
}

/// Register one CallSite as a builtin DynCall site, if it qualifies.
/// The FnType shape comes from the CallSite (`callsite.ftype()
/// .map(|ft| ft.resolve_tvars())`); each concrete arg/return type
/// comes from the compiled sub-nodes (`cs.arg_positional` /
/// `cs.arg_named` / `cs.typ()`) — the node-resident types the
/// typechecker resolved. Neither reads the AST: the function
/// expression's FnType is unresolved for generic builtins (e.g.
/// `str::parse` with a polymorphic `'b: Cast` return), and an arg
/// node's `typ()` is its concrete instantiated type.
fn try_register_builtin_call_from_callsite<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    ctx: &ExecCtx<R, E>,
    out: &mut BuiltinCallDiscovery,
) {
    // Need the source Apply Expr (for ExprKind::Ref-vs-other check
    // on the function, and for ExprId to register the site). The
    // CallSite's spec is always `ExprKind::Apply`.
    let apply_expr = cs.spec();
    let a = match &apply_expr.kind {
        ExprKind::Apply(a) => a,
        _ => return,
    };
    // Only direct builtin calls qualify: the function must be a
    // direct binding Ref.
    let path = match &a.function.kind {
        ExprKind::Ref { name } => name,
        _ => return,
    };
    // Scope comes from the CallSite node itself — so a builtin called
    // inside a nested-scope lambda body (a callee/callback whose
    // discovery runs from `build_lambda_kernel`) resolves in its own
    // module scope, not the region root's. Behaviour-preserving at the
    // root (root call sites carry root scope).
    let (_, bind) = match ctx.env.lookup_bind(&cs.scope().lexical, path) {
        Some(b) => b,
        None => return,
    };
    let key = (bind.scope.clone(), bind.name.clone());
    let info = match ctx.builtin_bindings.get(&key) {
        Some(i) => i.clone(),
        None => return,
    };
    // Async builtins can't be fused — they may produce values on a
    // later cycle than the trigger.
    if !ctx.builtin_effect(info.name.as_str()).is_sync() {
        return;
    }
    // Use the CallSite's resolved FnType (TVars unified at typecheck
    // time to the call-site's concrete arg types) for the call shape.
    //
    // After typecheck has run for this CallSite, `cs.ftype()` is
    // `Some(ft)` carrying the lambda's signature with the call-site's
    // TVar bindings still live in the TVars' RwLock cells. Calling
    // `resolve_tvars()` deep-clones with every TVar dereffed to its
    // bound concrete type — exactly what fusion needs.
    //
    // `cs.ftype()` may be `None` if typecheck hasn't run yet, or
    // didn't reach this site (e.g. a compile error elsewhere). Fall
    // back to the binding's own (generic) FnType in that case as a
    // last resort — it usually won't freeze, so the site stays unfused.
    let fn_type: std::sync::Arc<FnType> = match cs.ftype() {
        Some(ft) => std::sync::Arc::new(ft.resolve_tvars()),
        None => {
            let inner: &FnType = &info.typ;
            std::sync::Arc::new(inner.clone())
        }
    };
    let apply_id = apply_expr.id;
    let mut call_positional: Vec<usize> = Vec::new();
    let mut call_labeled: ahash::AHashMap<&str, usize> = ahash::AHashMap::default();
    for (call_idx, (label, _)) in a.args.iter().enumerate() {
        match label {
            Some(name) => {
                call_labeled.insert(name.as_str(), call_idx);
            }
            None => call_positional.push(call_idx),
        }
    }
    let mut layout: Vec<BuiltinSlot> = Vec::new();
    let mut arg_types: Vec<Type> = Vec::new();
    let mut marshal_arg_indices: Vec<usize> = Vec::new();
    let mut pos_iter = call_positional.iter().enumerate();
    for fa in fn_type.args.iter() {
        use FnArgKind;
        match &fa.kind {
            FnArgKind::Positional { .. } => {
                let (pos_idx, call_idx) = match pos_iter.next() {
                    Some(p) => p,
                    None => return,
                };
                // Concrete arg type from the compiled arg node — the
                // node-resident value the typechecker resolved. Fall
                // back to the formal type only if the node is absent.
                let arg_typ = cs
                    .arg_positional(pos_idx)
                    .map(|n| n.typ().clone())
                    .unwrap_or_else(|| fa.typ.clone());
                let kt = match kernel_abi::freeze_for_abi_normalized(
                    &ctx.fusion.abstract_registry,
                    &arg_typ,
                ) {
                    Some(t) => t,
                    None => return,
                };
                let slot_idx = arg_types.len();
                layout.push(BuiltinSlot::Positional(slot_idx));
                arg_types.push(kt);
                marshal_arg_indices.push(*call_idx);
            }
            FnArgKind::Labeled { name, has_default } => {
                if let Some(call_idx) = call_labeled.remove(name.as_str()) {
                    let arg_typ = cs
                        .arg_named(name)
                        .map(|n| n.typ().clone())
                        .unwrap_or_else(|| fa.typ.clone());
                    let kt = match kernel_abi::freeze_for_abi_normalized(
                        &ctx.fusion.abstract_registry,
                        &arg_typ,
                    ) {
                        Some(t) => t,
                        None => return,
                    };
                    let slot_idx = arg_types.len();
                    layout.push(BuiltinSlot::Positional(slot_idx));
                    arg_types.push(kt);
                    marshal_arg_indices.push(call_idx);
                } else if *has_default {
                    let default = info.argspec.iter().find_map(|src| {
                        let n = src.pattern.single_bind()?;
                        if n.as_str() != name.as_str() {
                            return None;
                        }
                        if let Some(Some(d)) = &src.labeled {
                            Some(d.clone())
                        } else {
                            None
                        }
                    });
                    let default = match default {
                        Some(d) => d,
                        None => return,
                    };
                    layout.push(BuiltinSlot::LabeledDefault(default));
                } else {
                    return;
                }
            }
        }
    }
    let remaining: Vec<_> = pos_iter.collect();
    if !remaining.is_empty() {
        if fn_type.vargs.is_none() {
            return;
        }
        let from_call_idx = arg_types.len();
        let count = remaining.len();
        layout.push(BuiltinSlot::Variadic { from_call_idx, count });
        for (pos_idx, call_idx) in remaining {
            let arg_typ =
                cs.arg_positional(pos_idx).map(|n| n.typ().clone()).or_else(|| {
                    fn_type.vargs.as_ref().and_then(|t| t.with_deref(|t| t.cloned()))
                });
            let arg_typ = match arg_typ {
                Some(t) => t,
                None => return,
            };
            let kt = match kernel_abi::freeze_for_abi_normalized(
                &ctx.fusion.abstract_registry,
                &arg_typ,
            ) {
                Some(t) => t,
                None => return,
            };
            arg_types.push(kt);
            marshal_arg_indices.push(*call_idx);
        }
    }
    if !call_labeled.is_empty() {
        return;
    }
    // Return type — the CallSite's own resolved output type (the
    // node-resident value the typechecker propagated for this Apply).
    let ret_typ = cs.typ().clone();
    let return_type = match kernel_abi::freeze_for_abi_normalized(
        &ctx.fusion.abstract_registry,
        &ret_typ,
    ) {
        Some(t) => t,
        None => return,
    };
    if !arg_types
        .iter()
        .all(|t| is_dyncall_arg_supported(&ctx.fusion.abstract_registry, t))
    {
        return;
    }
    if !is_dyncall_return_supported(&ctx.fusion.abstract_registry, &return_type) {
        return;
    }
    let fn_index = out.fn_params.len() as u32;
    out.fn_params.push(FnParam {
        name: info.name.clone(),
        source: FnSource::Builtin {
            name: info.name.clone(),
            typ: fn_type,
            layout: std::sync::Arc::from(layout),
            lambda_id: info.lambda_id,
        },
        arg_types: arg_types.clone(),
        return_type: return_type.clone(),
    });
    out.apply_sites.insert(
        apply_id,
        BuiltinCallSiteInfo { fn_index, marshal_arg_indices, arg_types, return_type },
    );
}

pub(crate) fn ident_of(path: &ModPath) -> Option<&str> {
    // A Ref we can fuse must be a bare identifier (no module path),
    // because we read its value from a local binding in the kernel.
    // `foo::bar` would require a more elaborate story.
    let s: &str = path.0.as_ref();
    let base = netidx::path::Path::basename(s)?;
    if netidx::path::Path::levels(s) != 1 {
        return None;
    }
    Some(base)
}

// ─── Expression-position emitters ────────────────────────────────

/// The constant `Value` of a node, seeing through `ExplicitParens`.
/// `None` for anything that isn't a compile-time-known literal.
pub(crate) fn node_const_value<R: Rt, E: UserEvent>(node: &Node<R, E>) -> Option<Value> {
    use NodeView;
    match node.view() {
        NodeView::Constant(c) => Some(c.value.clone()),
        NodeView::ExplicitParens(ep) => node_const_value(&ep.n),
        // Composite literals fold when every element folds — the
        // runtime shape of an array/tuple is a flat `ValArray`, a map
        // is a `Value::Map`. Lets a constant nested composite (a map of
        // arrays, an array of maps, …) lower to one Value constant
        // instead of bailing the whole literal.
        NodeView::Array(a) => const_valarray(&a.n),
        NodeView::Tuple(t) => const_valarray(&t.n),
        NodeView::Map(m) => const_map(&m.keys, &m.vals),
        _ => None,
    }
}

/// Fold a slice of `Cached` element nodes into a constant
/// `Value::Array` (the flat `ValArray` runtime shape shared by array
/// and tuple literals), or `None` if any element isn't constant.
fn const_valarray<R: Rt, E: UserEvent>(elems: &[Cached<R, E>]) -> Option<Value> {
    let mut vals: poolshark::local::LPooled<Vec<Value>> =
        poolshark::local::LPooled::take();
    for c in elems.iter() {
        vals.push(node_const_value(&c.node)?);
    }
    Some(Value::Array(netidx_value::ValArray::from_iter_exact(vals.drain(..))))
}

/// Fold parallel key/value `Cached` node slices into a constant
/// `Value::Map`, or `None` if any entry isn't constant. Shared by
/// `node_const_value`'s `Map` arm and `emit_map_new`.
/// (`pub(crate)`: also the direct path's `emit_map_new_node` —
/// fusion::emit — const-folds through it.)
pub(crate) fn const_map<R: Rt, E: UserEvent>(
    keys: &[Cached<R, E>],
    vals: &[Cached<R, E>],
) -> Option<Value> {
    if keys.len() != vals.len() {
        return None;
    }
    let mut map = netidx_value::Map::new();
    for (k, v) in keys.iter().zip(vals.iter()) {
        map.insert_cow(node_const_value(&k.node)?, node_const_value(&v.node)?);
    }
    Some(Value::Map(map))
}

/// On-demand monomorphization for a user lambda encountered at a
/// `CallSite` whose `resolved_apply()` is `ApplyView::Lambda(&g)`.
///
/// Resolve named (`Type::Ref`) and abstract (`Type::Abstract`) types to
/// their concrete representation, recursing through composites, so
/// `abi_kind` / `freeze_for_abi` can determine an abstract-typed value's
/// runtime shape. This is fusion-internal only — the abstraction stays
/// opaque to the type system; the optimizer peeks at the registered
/// concrete rep purely to size kernel slots.
///
/// Recursive types (`List<'a> = [`Cons('a, List<'a>), `Nil]`) terminate
/// by [`Seen`] cycle detection on the expansion arms
/// (`Ref` and nullary `Abstract`): a re-occurring expansion is left
/// as-is, `abi_kind` then yields `None`, and the kernel simply doesn't
/// fuse — correct, since a recursive type has no fixed ABI layout.
/// Detection is by expansion IDENTITY (full `TypeRef` / `AbstractId`), so
/// structural depth never trips it — a deeply-nested but FINITE type
/// resolves fully (unlike the old depth cap, which conflated deep with
/// recursive and silently refused such types). The one case identity
/// can't catch is NON-regular recursion whose params grow each step
/// (`type T<'a> = T<Array<'a>>`); a generous length backstop on the
/// expansion chain (NOT structural depth) guarantees termination there.
/// Every caller is FUSION-side (kernel-sig derivation), where a
/// truncated resolution only de-fuses (freeze rejects the opaque
/// remainder — never a wrong value or a spurious typecheck failure), so
/// the budget is small: finishing the expansion of a huge type (the GUI
/// widget unions) buys nothing — such types have no kernel encoding —
/// and a per-region resolve of one was the 2026-07-13 compile wedge.
/// Typecheck-side resolution is [`resolve_internal_type`], with a
/// generous budget (truncation there can fail a check that should pass).
pub(crate) fn resolve_abstract(reg: &AbstractRegistry, typ: &Type, env: &Env) -> Type {
    let cx =
        ResolveCx { budget: 2_048, size_cap: FUSION_SIZE_CAP, ..ResolveCx::default() };
    resolve_abstract_d(reg, typ, env, None, None, &cx).unwrap_or_else(|| typ.clone())
}

/// No kernel encodes a type that unfolds past this; resolving further
/// is work the freeze would discard.
pub(crate) const FUSION_SIZE_CAP: u32 = 4_096;

/// TYPECHECK-side view bridging (scoped static-instance checks,
/// instance signature binding) — NAME-PRESERVING, replacing the old
/// EXPANDING typecheck resolver. Output is the same size as the input:
/// refs stay refs (their carried resolution cells make them
/// env-independent — see `TypeRef`), so no memo/budget/size machinery
/// is needed. Two rewrites, both toward the PRIVATE view and both
/// gated by the registry's scope-visibility check:
///
/// - a `Type::Ref` whose resolution body is `Type::Abstract{id}` (the
///   interface's public entry) is rebound to the registry's private
///   body TEMPLATE in a fresh cell — the shared original cell is never
///   overwritten. The instance body then sees the private form through
///   ordinary `lookup_ref`, still name-compressed;
/// - a bare `Type::Abstract{id, params}` node substitutes its
///   registry body (ref-compressed, seeded at `check_sig`), recursing
///   for nested abstracts under an `AbstractId` cycle guard.
///
/// TVars deref-and-recurse (the output carries the binding, like the
/// fusion resolver — clone out of the guard before recursing, lock
/// discipline); everything else is a COW walk.
pub(crate) fn privatize_type(
    reg: &AbstractRegistry,
    typ: &Type,
    env: &Env,
    scope: &crate::expr::ModPath,
) -> Type {
    let mut seen: LPooled<Vec<crate::typ::AbstractId>> = LPooled::take();
    privatize_d(reg, typ, env, scope, &mut seen).unwrap_or_else(|| typ.clone())
}

fn privatize_d(
    reg: &AbstractRegistry,
    typ: &Type,
    env: &Env,
    scope: &crate::expr::ModPath,
    seen: &mut Vec<crate::typ::AbstractId>,
) -> Option<Type> {
    use triomphe::Arc;
    match typ {
        Type::TVar(_) => match typ.with_deref(|t| t.cloned()) {
            Some(t) => Some(privatize_d(reg, &t, env, scope, seen).unwrap_or(t)),
            None => None,
        },
        Type::Ref(tr) => {
            let params =
                Type::cow_slice(&tr.params, |t| privatize_d(reg, t, env, scope, seen));
            // The walk's env view wins over the cell ONLY when both
            // resolve to the SAME DEFINITION with a different VIEW
            // (`same_def` && !`same_view`): a site ref reaching here
            // through the caller's sig-registered allocation carries
            // the PUBLIC view, whose body's nested refs would leak
            // abstracts into the instance body ("expected struct not
            // abstract", the interface-test class) — rebind to the
            // env's allocation, seeded from this env so the fill
            // invariant holds. A DIFFERENT definition in the env is a
            // stale-horizon artifact (f.env is a mid-registration
            // snapshot; a forward cross-submodule name like tui's
            // `list::List` resolves there to the list PACKAGE's type)
            // — the cell, filled post-registration, is the name's
            // true meaning and wins. Names the env can't see keep
            // their cells (that's the cell's whole point).
            let resolution = match (tr.resolve_pure(env), tr.resolved()) {
                (Some(e), Some(c)) if e.same_def(&c) && !e.same_view(&c) => {
                    e.typ().seed_refs(env);
                    Some((e, true))
                }
                (_, Some(c)) => Some((c, false)),
                // Empty cell: REBIND rather than fill — the cell may
                // be shared into caller-held types, and writing the
                // walk's (private) view there would leak it to every
                // aliasing context.
                (Some(e), None) => {
                    e.typ().seed_refs(env);
                    Some((e, true))
                }
                (None, None) => None,
            };
            let rebound = resolution.and_then(|(r, rebind)| match r.typ() {
                Type::Abstract { id, .. } => reg
                    .internal_template(id, scope)
                    .map(|(formals, body)| Arc::new(r.private_view(&formals, body))),
                _ => rebind.then_some(r),
            });
            match (rebound, params) {
                (None, None) => None,
                (rebound, params) => {
                    let params = params.unwrap_or_else(|| tr.params.clone());
                    Some(Type::Ref(match rebound {
                        Some(r) => tr.rebind_resolution(params, r),
                        None => tr.with_params(params),
                    }))
                }
            }
        }
        Type::Abstract { id, params } => {
            if seen.contains(id) {
                return None; // recursive abstract — leave opaque
            }
            let cow_params =
                Type::cow_slice(params, |t| privatize_d(reg, t, env, scope, seen));
            let params = cow_params.as_ref().unwrap_or(params);
            match reg.resolve_internal(id, params, scope) {
                Some(body) => {
                    seen.push(*id);
                    let r = privatize_d(reg, &body, env, scope, seen).unwrap_or(body);
                    seen.pop();
                    Some(r)
                }
                None => cow_params.map(|params| Type::Abstract { id: *id, params }),
            }
        }
        Type::Fn(ft) => ft
            .cow_walk(|t| privatize_d(reg, t, env, scope, seen))
            .map(|ft| Type::Fn(Arc::new(ft))),
        Type::Tuple(ts) => Type::cow_slice(ts, |t| privatize_d(reg, t, env, scope, seen))
            .map(Type::Tuple),
        Type::Variant(tag, ts) => {
            Type::cow_slice(ts, |t| privatize_d(reg, t, env, scope, seen))
                .map(|ts| Type::Variant(tag.clone(), ts))
        }
        Type::Array(t) => {
            privatize_d(reg, t, env, scope, seen).map(|t| Type::Array(Arc::new(t)))
        }
        Type::Error(t) => {
            privatize_d(reg, t, env, scope, seen).map(|t| Type::Error(Arc::new(t)))
        }
        Type::ByRef(t) => {
            privatize_d(reg, t, env, scope, seen).map(|t| Type::ByRef(Arc::new(t)))
        }
        Type::Map { key, value } => {
            match (
                privatize_d(reg, key, env, scope, seen),
                privatize_d(reg, value, env, scope, seen),
            ) {
                (None, None) => None,
                (k, v) => Some(Type::Map {
                    key: k.map(Arc::new).unwrap_or_else(|| key.clone()),
                    value: v.map(Arc::new).unwrap_or_else(|| value.clone()),
                }),
            }
        }
        Type::Set(ts) => {
            Type::cow_slice(ts, |t| privatize_d(reg, t, env, scope, seen)).map(Type::Set)
        }
        Type::Struct(fs) => Type::cow_slice(fs, |(n, t)| {
            privatize_d(reg, t, env, scope, seen).map(|t| (n.clone(), t))
        })
        .map(Type::Struct),
        Type::Bottom | Type::Any | Type::Primitive(_) => None,
    }
}

/// Per-top-level-resolve state. `Seen` bounds each expansion PATH, but
/// nothing bounded the expansion TREE: an unmemoized walk over types that
/// repeat at every nesting level is exponential in the nesting depth, and
/// the deep-cloned results are RETAINED per call site (the GUI
/// widget/style signatures hit the OOM killer at 41GB expanding per
/// static call site, 2026-07-13). `memo` caches expansions keyed by
/// (`ExpandKey`, the set of `Seen` keys the computation consulted): an
/// entry is valid on any path containing all its dependencies, and a hit
/// shares the stored `Type`'s Arcs, so a resolve builds a DAG sized by
/// DISTINCT types instead of a tree sized by paths. `expansions` is a
/// total-work backstop: exhaustion leaves the remainder opaque (de-fuse /
/// check-refuse, never a wrong value), the same failure mode as the
/// per-path length backstop.
struct ResolveCx {
    memo: std::cell::RefCell<LPooled<Vec<MemoEntry>>>,
    /// STRUCTURAL node memo: (variant discriminant, content Arc
    /// address(es)) → this pass's result for that shared subtree, valid
    /// on any path containing its recorded `Seen` dependencies. The
    /// expansion memo (`memo`) dedupes REF expansions; without this one
    /// every shared composite BETWEEN expansions was re-walked per path
    /// — tree-cost over the resolved DAG.
    nodes: std::cell::RefCell<LPooled<ahash::AHashMap<crate::typ::NormKey, NodeEntry>>>,
    /// `Seen` keys consulted by the CURRENT expansion frame's computation
    /// (the frame's own key excluded on merge-up — a self-hit is not a
    /// dependency on the caller's path).
    consulted: std::cell::RefCell<LPooled<Vec<kernel_abi::ExpandKey>>>,
    /// The current frame's result was truncated (budget, size cap, or
    /// path-length backstop) — not the true expansion, must not be
    /// memoized.
    poisoned: std::cell::Cell<bool>,
    expansions: std::cell::Cell<u32>,
    budget: u32,
    /// Running count of OUTPUT nodes (tree-wise — a memo hit charges its
    /// entry's recorded unfolded size, not 1), against `size_cap`.
    /// Callers that would DISCARD an over-large result (the instance-
    /// signature and fusion resolvers) set the cap so a widget-scale
    /// resolution aborts as soon as it is knowably too big instead of
    /// being computed and thrown away per call site.
    unfolded: std::cell::Cell<u32>,
    size_cap: u32,
}

impl Default for ResolveCx {
    fn default() -> Self {
        Self {
            memo: Default::default(),
            nodes: Default::default(),
            consulted: Default::default(),
            poisoned: Default::default(),
            expansions: Default::default(),
            budget: 65_536,
            unfolded: Default::default(),
            size_cap: u32::MAX,
        }
    }
}

struct NodeEntry {
    deps: Vec<(u64, kernel_abi::ExpandKey)>,
    resolved: Option<Type>,
    size: u32,
}

struct FrameResult {
    deps: Vec<(u64, kernel_abi::ExpandKey)>,
    poisoned: bool,
    size: u32,
}

#[derive(Clone)]
struct MemoEntry {
    /// [`expand_key_fp`] of `key` — compared first so a lookup scans
    /// the entry list with u64 compares instead of `TypeRef` string
    /// equality (which dominated the resolve at GUI scale).
    fp: u64,
    key: kernel_abi::ExpandKey,
    deps: Vec<(u64, kernel_abi::ExpandKey)>,
    /// `Some` = the expansion with abstracts substituted (inline it);
    /// `None` = nothing beneath the ref's expansion resolved — the ref
    /// stays OPAQUE. Inlining a substitution-free ref buys nothing and
    /// DESYNCS expansion states across the program: `contains`' ref-pair
    /// history keys on refs, so one pre-expanded side turns co-inductive
    /// recursion into an exponential structural walk (the 2026-07-13
    /// button-click wedge).
    resolved: Option<Type>,
    /// Unfolded (tree-wise) node count of `resolved`, charged against
    /// `ResolveCx::size_cap` on every hit.
    size: u32,
}

use kernel_abi::expand_key_fp;

impl std::fmt::Debug for MemoEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "MemoEntry(size {})", self.size)
    }
}

/// No unbound-TVar anywhere beneath — the type's identity is stable
/// under `PartialEq`, so it can key a cache. A BOUND cell's binding can
/// also drift between calls, so any TVar disqualifies.
fn tvar_free(t: &Type) -> bool {
    match t {
        Type::Bottom | Type::Any | Type::Primitive(_) => true,
        Type::TVar(_) => false,
        Type::Ref(tr) => tr.params.iter().all(tvar_free),
        Type::Abstract { params, .. } => params.iter().all(tvar_free),
        Type::Set(ts) | Type::Tuple(ts) | Type::Variant(_, ts) => {
            ts.iter().all(tvar_free)
        }
        Type::Struct(fs) => fs.iter().all(|(_, t)| tvar_free(t)),
        Type::Array(t) | Type::Error(t) | Type::ByRef(t) => tvar_free(t),
        Type::Map { key, value } => tvar_free(key) && tvar_free(value),
        Type::Fn(ft) => {
            ft.args.iter().all(|a| tvar_free(&a.typ))
                && ft.vargs.as_ref().is_none_or(tvar_free)
                && tvar_free(&ft.rtype)
                && tvar_free(&ft.throws)
        }
    }
}

fn key_closed(key: &kernel_abi::ExpandKey) -> bool {
    match key {
        kernel_abi::ExpandKey::Ref(tr) => tr.params.iter().all(tvar_free),
        kernel_abi::ExpandKey::Abstract(_) => false,
    }
}

impl ResolveCx {
    /// Record a `Seen` hit: the current frame's result depends on `key`
    /// being on the path.
    fn consult(&self, key: &kernel_abi::ExpandKey) {
        let mut cur = self.consulted.borrow_mut();
        if !cur.contains(key) {
            cur.push(key.clone());
        }
    }

    fn lookup(
        &self,
        key: &kernel_abi::ExpandKey,
        seen: Option<&Seen>,
    ) -> Option<Option<Type>> {
        let fp = expand_key_fp(key);
        let hit = |e: &MemoEntry| {
            e.fp == fp
                && &e.key == key
                && e.deps.iter().all(|(fp, d)| Seen::contains_fp(seen, *fp, d))
        };
        let entry = self.memo.borrow().iter().find(|e| hit(e))?.clone();
        self.count_nodes(entry.size);
        for (_, d) in &entry.deps {
            self.consult(d);
        }
        Some(entry.resolved)
    }

    /// Count `n` OUTPUT nodes against the size cap; `false` = the result
    /// has grown past the cap — poisoned, stop expanding.
    fn count_nodes(&self, n: u32) -> bool {
        let total = self.unfolded.get().saturating_add(n);
        self.unfolded.set(total);
        if total > self.size_cap {
            self.poisoned.set(true);
            return false;
        }
        true
    }

    /// Structural-memo hit: `Some(result)` when `key` has an entry whose
    /// path dependencies are all on the current `seen` path. Charges the
    /// entry's recorded size and re-registers its dependencies.
    fn node_lookup(
        &self,
        key: &crate::typ::NormKey,
        seen: Option<&Seen>,
    ) -> Option<Option<Type>> {
        let nodes = self.nodes.borrow();
        let e = nodes.get(key)?;
        if !e.deps.iter().all(|(fp, d)| Seen::contains_fp(seen, *fp, d)) {
            return None;
        }
        let resolved = e.resolved.clone();
        let deps = e.deps.clone();
        let size = e.size;
        drop(nodes);
        self.count_nodes(size);
        for (_, d) in &deps {
            self.consult(d);
        }
        Some(resolved)
    }

    /// Run `f` as a tracked frame: capture the `Seen` keys it consults,
    /// whether it was truncated, and the output size it accumulated —
    /// then merge consults/poisoning into the enclosing frame.
    fn node_frame<R>(&self, f: impl FnOnce(&Self) -> R) -> (R, FrameResult) {
        let saved_consulted =
            std::mem::replace(&mut *self.consulted.borrow_mut(), LPooled::take());
        let saved_poisoned = self.poisoned.replace(false);
        let before = self.unfolded.get();
        let r = f(self);
        let mut delta =
            std::mem::replace(&mut *self.consulted.borrow_mut(), saved_consulted);
        let poisoned = self.poisoned.get();
        let size = self.unfolded.get() - before;
        let deps: Vec<(u64, kernel_abi::ExpandKey)> =
            delta.iter().map(|k| (expand_key_fp(k), k.clone())).collect();
        for k in delta.drain(..) {
            let mut cur = self.consulted.borrow_mut();
            if !cur.contains(&k) {
                cur.push(k);
            }
        }
        self.poisoned.set(saved_poisoned || poisoned);
        (r, FrameResult { deps, poisoned, size })
    }

    /// Charge one expansion against the budget; `false` = exhausted, the
    /// caller must leave the type opaque.
    fn charge(&self) -> bool {
        let n = self.expansions.get();
        if n >= self.budget {
            if n == self.budget {
                self.expansions.set(n + 1);
                log::warn!(
                    "resolve_abstract: expansion budget ({}) exhausted — \
                     leaving the remainder opaque",
                    self.budget
                );
            }
            self.poisoned.set(true);
            return false;
        }
        self.expansions.set(n + 1);
        true
    }

    /// Run `f` as one expansion frame: track the `Seen` keys it consults,
    /// memoize the result against them when `memoize` (unless the frame
    /// was truncated), and merge its dependencies — minus `key` itself —
    /// into the enclosing frame.
    fn expand(
        &self,
        key: kernel_abi::ExpandKey,
        memoize: bool,
        f: impl FnOnce() -> Option<Type>,
    ) -> Option<Type> {
        let saved_consulted =
            std::mem::replace(&mut *self.consulted.borrow_mut(), LPooled::take());
        let saved_poisoned = self.poisoned.replace(false);
        let unfolded_before = self.unfolded.get();
        let resolved = f();
        let mut deps =
            std::mem::replace(&mut *self.consulted.borrow_mut(), saved_consulted);
        let poisoned = self.poisoned.get();
        deps.retain(|k| k != &key);
        // CLOSED keys only, even for the per-call memo: an unbound-TVar
        // param compares equal to any other unbound cell, so a hit for a
        // different-but-eq key would hand one site's cells to another
        // and skip `lookup_ref`'s constraint registration.
        if memoize && !poisoned && key_closed(&key) {
            let entry = MemoEntry {
                fp: expand_key_fp(&key),
                key,
                deps: deps.iter().map(|k| (expand_key_fp(k), k.clone())).collect(),
                resolved: resolved.clone(),
                size: self.unfolded.get() - unfolded_before,
            };
            self.memo.borrow_mut().push(entry);
        }
        for k in deps.drain(..) {
            let mut cur = self.consulted.borrow_mut();
            if !cur.contains(&k) {
                cur.push(k);
            }
        }
        self.poisoned.set(saved_poisoned || poisoned);
        resolved
    }
}

/// `None` = nothing beneath resolved — the caller keeps the original
/// (shared). Truncation (budget/size/path backstops) also returns `None`
/// with `cx.poisoned` set: the remainder stays opaque.
fn resolve_abstract_d<'a>(
    reg: &AbstractRegistry,
    typ: &Type,
    env: &Env,
    scope: Option<&crate::expr::ModPath>,
    seen: Option<&'a Seen<'a>>,
    cx: &ResolveCx,
) -> Option<Type> {
    use kernel_abi::Seen;
    // Non-regular-recursion backstop (see fn doc). Counts EXPANSIONS, not
    // structural nesting, so finite types of any structural depth are
    // unaffected. It bounds the number of distinct EXPANSIONS on one path
    // (named-alias / abstract hops): the case it's FOR is non-regular
    // recursion whose params grow each step (identity can't catch it), but
    // a pathologically long FINITE expansion chain (>256 distinct hops)
    // would also be left opaque and simply not fuse. A truncated result
    // is not the true expansion — poison memoization upward.
    if Seen::len(seen) > 256 {
        cx.poisoned.set(true);
        return None;
    }
    // Every visit costs ~one unit against the size cap; a memo hit
    // charges its recorded subtree size in `lookup` instead. Once the
    // accumulated work crosses the caller's cap, stop — the result would
    // be discarded anyway. `None` = the subtree is returned UNCHANGED
    // (shared) — abstract-free subtrees materialize nothing.
    if !cx.count_nodes(1) {
        return None;
    }
    // Structural node memo: a shared composite reached along many paths
    // resolves once per (path-dependency) context.
    let nkey = crate::typ::norm_key(typ);
    if let Some(k) = &nkey
        && let Some(hit) = cx.node_lookup(k, seen)
    {
        return hit;
    }
    let (r, frame) =
        cx.node_frame(|cx| resolve_abstract_node(reg, typ, env, scope, seen, cx));
    if let Some(k) = nkey
        && !frame.poisoned
    {
        cx.nodes.borrow_mut().insert(
            k,
            NodeEntry { deps: frame.deps, resolved: r.clone(), size: frame.size },
        );
    }
    r
}

fn resolve_abstract_node<'a>(
    reg: &AbstractRegistry,
    typ: &Type,
    env: &Env,
    scope: Option<&crate::expr::ModPath>,
    seen: Option<&'a Seen<'a>>,
    cx: &ResolveCx,
) -> Option<Type> {
    use kernel_abi::{ExpandKey, Seen};
    use triomphe::Arc;
    match typ {
        // Deref bound TVars and resolve through them — an INFERRED
        // binding type (e.g. a region input's `let` binding, #218) is
        // TVar-wrapped, unlike the declared signature types the
        // classic kernel-build callers pass. An unbound TVar returns
        // unchanged (freeze rejects it downstream, correctly). The
        // deref itself is a change: the output carries the BINDING, not
        // the cell.
        //
        // Clone the inner type OUT of `with_deref` and recurse with
        // the TVar's read guard DROPPED: recursing inside the closure
        // held the guard across `lookup_ref` / `ABSTRACT_REGISTRY`
        // acquisitions, and with parking_lot's fair (non-reentrant)
        // locks that cross-holding deadlocked concurrent compiles in
        // one process (caught as the post-flip parallel test wedge —
        // every worker parked on `check_sig`'s registry write).
        //
        // A TVar deref is not an expansion (it can't recurse forever —
        // TVar bindings are occurs-checked), so `seen` passes through.
        Type::TVar(_) => match typ.with_deref(|t| t.cloned()) {
            Some(t) => {
                Some(resolve_abstract_d(reg, &t, env, scope, seen, cx).unwrap_or(t))
            }
            None => None,
        },
        Type::Ref(tr) => {
            let key = ExpandKey::Ref(tr.clone());
            if Seen::contains(seen, &key) {
                cx.consult(&key);
                return None; // recursive named type — leave opaque
            }
            if let Some(t) = cx.lookup(&key, seen) {
                return t;
            }
            if !cx.charge() {
                return None;
            }
            match typ.lookup_ref(env) {
                // Refs INLINE unconditionally: the instance signature
                // must be env-INDEPENDENT — inside the defining module
                // `lookup_ref` resolves names to their PRIVATE forms,
                // and the caller's env cannot reproduce that view (the
                // scoped-abstract instance semantics, 2026-07-13). The
                // expansion-state mismatch this creates for `contains`
                // is paid for there (the pure-probe pair memo), not by
                // keeping refs opaque.
                Ok(resolved) => cx.expand(key, true, || {
                    let node = Seen::push(seen, ExpandKey::Ref(tr.clone()));
                    Some(
                        resolve_abstract_d(reg, &resolved, env, scope, Some(&node), cx)
                            .unwrap_or(resolved),
                    )
                }),
                _ => None,
            }
        }
        Type::Abstract { id, params } => {
            let key = ExpandKey::Abstract(*id);
            if Seen::contains(seen, &key) {
                cx.consult(&key);
                return None; // recursive abstract — leave opaque
            }
            if !cx.charge() {
                return None;
            }
            // No memo for abstracts (`memoize: false`):
            // `ExpandKey::Abstract` omits `params` — coarse is right for
            // cycle detection, wrong for caching an instantiation. The
            // Ref memo carries the tree collapse; the frame still runs so
            // the self-key drops out of the caller's dependencies.
            let resolved = match scope {
                Some(scope) => reg.resolve_internal(id, params, scope),
                None => reg.resolve(id, params),
            };
            match resolved {
                Some(concrete) => cx.expand(key, false, || {
                    let node = Seen::push(seen, ExpandKey::Abstract(*id));
                    Some(
                        resolve_abstract_d(reg, &concrete, env, scope, Some(&node), cx)
                            .unwrap_or(concrete),
                    )
                }),
                None => None,
            }
        }
        Type::Fn(ft) => ft
            .cow_walk(|t| resolve_abstract_d(reg, t, env, scope, seen, cx))
            .map(|ft| Type::Fn(Arc::new(ft))),
        Type::Tuple(ts) => {
            Type::cow_slice(ts, |t| resolve_abstract_d(reg, t, env, scope, seen, cx))
                .map(Type::Tuple)
        }
        Type::Variant(tag, ts) => {
            Type::cow_slice(ts, |t| resolve_abstract_d(reg, t, env, scope, seen, cx))
                .map(|ts| Type::Variant(tag.clone(), ts))
        }
        Type::Array(t) => resolve_abstract_d(reg, t, env, scope, seen, cx)
            .map(|t| Type::Array(Arc::new(t))),
        Type::Error(t) => resolve_abstract_d(reg, t, env, scope, seen, cx)
            .map(|t| Type::Error(Arc::new(t))),
        Type::ByRef(t) => resolve_abstract_d(reg, t, env, scope, seen, cx)
            .map(|t| Type::ByRef(Arc::new(t))),
        Type::Map { key, value } => {
            match (
                resolve_abstract_d(reg, key, env, scope, seen, cx),
                resolve_abstract_d(reg, value, env, scope, seen, cx),
            ) {
                (None, None) => None,
                (k, v) => Some(Type::Map {
                    key: k.map(Arc::new).unwrap_or_else(|| key.clone()),
                    value: v.map(Arc::new).unwrap_or_else(|| value.clone()),
                }),
            }
        }
        Type::Set(ts) => {
            Type::cow_slice(ts, |t| resolve_abstract_d(reg, t, env, scope, seen, cx))
                .map(Type::Set)
        }
        Type::Struct(fs) => Type::cow_slice(fs, |(n, t)| {
            resolve_abstract_d(reg, t, env, scope, seen, cx).map(|t| (n.clone(), t))
        })
        .map(Type::Struct),
        Type::Bottom | Type::Any | Type::Primitive(_) => None,
    }
}

/// FusionCtx-free core of [`ensure_lambda_kernel`]: build (or cache-hit)
/// the fused [`CachedKernel`] for a resolved, concretely-typed lambda
/// `g`, populating the per-`ExecCtx` `(LambdaId, resolved FnType)` cache.
/// Does NOT touch the transient `FusionCtx` (no `known_fns` /
/// `called_kernels`). Returns the cached kernel plus the transitively-
/// called sub-kernels discovered during this build (empty on a cache
/// hit). Used by `ensure_lambda_kernel`, the region-splice cross-kernel
/// call path, which then registers the callee.
///
/// `kernel_name` becomes the built `CachedKernel.fn_name`; a cache hit
/// returns the first builder's name. `GXLambda` carries its analyzed
/// self-binding and recursion facts.
pub(crate) fn build_lambda_kernel<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    site_ftype: &FnType,
    kernel_name: &ArcStr,
    ec: &mut ExecCtx<R, E>,
) -> Option<CachedKernel> {
    let self_bind = g.self_bind();
    // Cache key: (LambdaId, the CALL SITE's resolved FnType).
    // `resolve_tvars` deep-clones, dereffing every TVar to its bound
    // concrete type, so monomorphizations agree across syntactically-
    // distinct but structurally-equivalent FnTypes. The key must be the
    // SITE's type, not `g.typ()`: the lambda instance's FnType shares
    // TVar cells with the lambda def, and when one polymorphic lambda
    // is called at two monomorphizations the first site's unification
    // wins those cells — a later site's `g.typ()` reports the FIRST
    // monomorphization (audit-jul2026/02).
    let resolved_typ = std::sync::Arc::new(site_ftype.resolve_tvars());
    let key = (g.id(), resolved_typ);
    if let Some(cached) = ec.fusion.kernels.lock().get(&key).cloned() {
        return Some(cached);
    }
    // The kernel is BUILT from `g` — its body's node types are what
    // emission reads — so if the instance disagrees with the site
    // (the shared-cell corruption above), building would emit a kernel
    // whose CLIF types mismatch the call site (a cranelift verifier
    // panic, or worse a silent same-width misread). Refuse instead;
    // the site node-walks. Compare the pieces the build consumes
    // (args/vargs/return), not the whole FnType — constraint lists
    // carry TVar identities that differ benignly between the site copy
    // and the def.
    {
        let gt = g.typ().resolve_tvars();
        let st = &*key.1;
        let args_agree = gt.args.len() == st.args.len()
            && gt.args.iter().zip(st.args.iter()).all(|(a, b)| a.typ == b.typ);
        if !args_agree || gt.vargs != st.vargs || gt.rtype != st.rtype {
            log::trace!(
                "build_lambda_kernel: site mono {} disagrees with lambda \
                 instance {} — refusing (site node-walks)",
                st,
                gt
            );
            return None;
        }
    }
    // Re-entrancy guard: a lambda whose body (transitively) builds a
    // lambda that calls back into THIS one — mutual recursion — would
    // recurse this build forever: the cache entry above only lands on
    // completion, and the per-build `known_fns` registration only
    // covers SELF-recursion (a kernel knows its own name before its
    // body emits). Refuse the re-entered build instead: the inner
    // call site doesn't lower, the enclosing body emission fails, and
    // the whole chain de-fuses to the node-walk. Mutually-recursive
    // lambdas are a fusion gap, not a compiler hang.
    struct BuildingGuard(triomphe::Arc<parking_lot::Mutex<nohash::IntSet<u64>>>, u64);
    impl Drop for BuildingGuard {
        fn drop(&mut self) {
            self.0.lock().remove(&self.1);
        }
    }
    let lid = g.id().inner();
    if !ec.fusion.building.lock().insert(lid) {
        return None;
    }
    let _building = BuildingGuard(ec.fusion.building.clone(), lid);
    // Translate each lambda formal arg into a kernel input slot. Slots
    // carry the formal's PATTERN BindId when the pattern is a single
    // name — body `Ref`s resolve id-first, which is load-bearing when
    // the instance was resolved through a DECLARED fn type whose
    // parameter names are documentation (`f: fn(acc: 'b, x: 'a)` in a
    // .gxi) and differ from the callback literal's own names
    // (`|acc, v| …` — the body's `Ref(v)` never matches slot name
    // "x"). The FnArgKind name still names the slot for by-name
    // resolution of destructured formals (no single id).
    let typ = g.typ();
    let n_formal = typ.args.len();
    let mut inputs: Vec<(ArcStr, RegionInputKind, Option<BindId>)> =
        Vec::with_capacity(n_formal);
    // The frozen kernel-ABI slot type of each formal, kept for the
    // recursive-self-call ABI check below (#21).
    let mut formal_kts: Vec<Type> = Vec::with_capacity(n_formal);
    for (i, fa) in typ.args.iter().enumerate() {
        let name = match &fa.kind {
            FnArgKind::Positional { name: Some(n) } => n.clone(),
            FnArgKind::Labeled { name, .. } => name.clone(),
            _ => return None,
        };
        let arg_typ = resolve_abstract(&ec.fusion.abstract_registry, &fa.typ, &ec.env);
        let kt = kernel_abi::freeze_for_abi_normalized(
            &ec.fusion.abstract_registry,
            &arg_typ,
        )?;
        let kind = type_to_region_input_kind(&ec.fusion.abstract_registry, kt.clone())?;
        let id = g.args().get(i).and_then(|p| p.single_bind_id());
        if id.is_none() {
            // A DESTRUCTURED formal (`|(k, v)| …`) has no single id and
            // its leaves aren't bound as slots yet — refuse the build.
            // Falling through with a name-only slot is a WRONG-BINDING
            // hazard: the slot is named from the DECLARED FnType (a
            // .gxi doc name like "x"), and a body leaf that happens to
            // share it resolves by name to the whole composite —
            // `array::map(a, |(p, x)| x)` returned the tuple POINTER
            // as the scalar x (jit_destructure_probes).
            let mut has_ids = false;
            if let Some(p) = g.args().get(i) {
                p.ids(&mut |_| has_ids = true);
            }
            if has_ids {
                return None;
            }
        }
        inputs.push((name, kind, id));
        formal_kts.push(kt);
    }
    // Closure conversion: every binding the body references but
    // doesn't bind itself is a capture. Lift each value-typed capture
    // into an extra positional kernel arg (after the formal args).
    // Live semantics: the caller passes the capture's *current* value
    // at each call, matching `GXLambda`'s runtime behavior.
    //
    // `g.body().refs()` reports the body's referenced/bound ids, but
    // it does NOT account for the lambda's *formal arg* patterns —
    // those bind their ids in the lambda Node, outside the body. So
    // the body's `Ref(x)` to a formal arg `x` surfaces as "external"
    // here. Exclude the formal-arg ids explicitly so they aren't
    // mistaken for captures.
    let mut arg_ids: nohash::IntSet<BindId> = nohash::IntSet::default();
    for pat in g.args() {
        pat.ids(&mut |id| {
            arg_ids.insert(id);
        });
    }
    let mut refs = Refs::default();
    g.body().refs(&mut refs);
    let mut external: Vec<BindId> = Vec::new();
    refs.with_external_refs(|id| {
        if !arg_ids.contains(&id) {
            external.push(id);
        }
    });
    // Deterministic order so the kernel signature (and thus the
    // `(LambdaId, FnType)` cache entry) is stable across builds.
    external.sort_by_key(|id| id.inner());
    let mut captures: Vec<CaptureSlot> = Vec::new();
    for bind_id in external.iter().copied() {
        // The lambda's own binding is not a capture — a recursive
        // body's self-reference lowers as a call (`TailCall` in tail
        // position, a native call elsewhere), never as a value. It
        // can't be scanned as one anyway: a rec binding's env type is
        // a TVar-wrapped `Fn` that the shallow `Type::Fn` skip below
        // misses and `freeze_for_abi` then rejects, killing the
        // whole build (recursive lambdas never fused because of it).
        if Some(bind_id) == self_bind {
            continue;
        }
        let b = ec.env.by_id.get(&bind_id)?;
        let cap_typ = b.typ.clone();
        // Function-typed captures: a statically-resolvable callee is
        // handled by the body's own CallSite emit (it fires this same
        // CallSite emit and emits a CLIF call), so it isn't a value
        // capture — skip it. A dynamic fn binding (used as a value or
        // dispatched via DynCall) can't be lifted as a value slot;
        // bail (the lambda stays unfused, runtime takes the interp
        // path). Follow-up: dynamic fn captures via `fn_inputs` slots.
        if matches!(&cap_typ, Type::Fn(_)) {
            // We can't cheaply tell static-vs-dynamic here without the
            // call site; rely on the body emit to lower static calls
            // and to bail on dynamic use. Skipping is safe: if the
            // body actually needs this as a value, body emit fails and
            // the whole build returns None below.
            continue;
        }
        let kt = match kernel_abi::freeze_for_abi_normalized(
            &ec.fusion.abstract_registry,
            &resolve_abstract(&ec.fusion.abstract_registry, &cap_typ, &ec.env),
        ) {
            Some(t) => t,
            None => return None,
        };
        let kind =
            match type_to_region_input_kind(&ec.fusion.abstract_registry, kt.clone()) {
                Some(k) => k,
                None => return None,
            };
        let name = ArcStr::from(b.name.as_str());
        inputs.push((name.clone(), kind, Some(bind_id)));
        captures.push(CaptureSlot { bind_id, name, typ: kt });
    }
    let return_typ = kernel_abi::freeze_for_abi_normalized(
        &ec.fusion.abstract_registry,
        &resolve_abstract(&ec.fusion.abstract_registry, &typ.rtype, &ec.env),
    )?;
    // Cross-kernel calls support scalar + composite (array/tuple/
    // struct) + value-shape (variant/nullable) args, captures, and
    // returns. Args and captures are already restricted to those
    // kinds by `type_to_region_input_kind` above (String / Unit /
    // bare-Null bail there). The return is the one remaining shape to
    // gate: String / Unit / Null returns aren't marshalled through
    // the cross-kernel call boundary, so refuse those — the call
    // stays on the node-walk (via `GXLambda`).
    if matches!(
        kernel_abi::abi_kind(&ec.fusion.abstract_registry, &return_typ),
        Some(
            kernel_abi::AbiKind::String
                | kernel_abi::AbiKind::Unit
                | kernel_abi::AbiKind::Null
        )
    ) {
        return None;
    }
    // Self-recursion: the body references its own binding. With a
    // self tail-call present, the kernel compiles to a rebind-and-jump
    // loop (`has_tail_loop`) instead of a native recursive call —
    // constant stack at any depth. The JIT's tail-call rebind handles
    // Scalar and ValArray slots only, and only the formal params are
    // rebound (captures are loop-invariant within one kernel
    // invocation), so the loop is gated on every FORMAL being one of
    // those kinds; otherwise self-calls stay plain native recursion
    // (correct, native stack depth).
    let is_rec = g.self_recursive();
    // A recursive self-call is a SECOND source of values for the formals
    // (besides the outer call). If inference resolved a formal to the
    // outer call's type but the self-call feeds it a differently-shaped
    // value, the kernel would marshal that value under the wrong ABI (a
    // composite pointer read as a scalar leaks it; a scalar deref'd as a
    // pointer crashes). Static instance checking rejects that mismatch;
    // retain this ABI check as defense in depth and de-fuse on disagreement.
    if is_rec
        && let Some(sb) = self_bind
        && !self_calls_abi_consistent(g.body(), sb, &formal_kts, ec)
    {
        return None;
    }
    // The native-loop gate is the SHARED structural predicate — the same
    // one `analysis::analyze` uses (sync-gated) for the interpreter's
    // tail-loop, so the two backends can't disagree on which lambdas loop.
    let has_tail = g.tail_loop();
    // The build is pure signature derivation — the body is validated
    // by the compile attempt itself (`emit_clif` over the body Node;
    // "is it fusable IS the compile attempt"). A body that doesn't
    // emit fails the kernel define, and the call site's region
    // node-walks.
    let (mut sig, arg_types) = match fusion::sig_from_inputs(
        kernel_name.clone(),
        inputs.iter().map(|(name, kind, bind_id)| (name.clone(), kind, *bind_id)),
        return_typ.clone(),
    ) {
        Ok(v) => v,
        Err(e) => {
            // Malformed frozen input — the callee kernel can't be built,
            // so the call site node-walks (de-fuse, never panic).
            log::trace!("build_lambda_kernel: sig_from_inputs failed: {e:#}");
            return None;
        }
    };
    sig.has_tail_loop = has_tail;
    // Discover sync builtin/cast/qop Apply sites in the body so they
    // fuse as DynCalls (the same prepass `try_fuse` runs on a region
    // root). `fn_params` installs the slots on the sig; `apply_sites`
    // lets the body emitter recognise each call site. The inline collection
    // path consumes these directly (the callback body's own builtins);
    // the cross-kernel callee path consumes them via `CalleeBody`
    // (Stage 2 — the runtime combined-slot delivery).
    let mut discovery = BuiltinCallDiscovery::default();
    walk_node_for_builtin_calls(g.body(), ec, &mut discovery);
    sig.fn_params = discovery.fn_params;
    if std::env::var("GRAPHIX_DBG_KERNELS").is_ok() {
        crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
            eprintln!(
                "KERNEL BUILT {kernel_name}: ret={return_typ} kind={:?}",
                kernel_abi::abi_kind(&ec.fusion.abstract_registry, &return_typ)
            );
            Ok::<_, anyhow::Error>(())
        })
        .unwrap();
    }
    let signature = KnownFusedFn { arg_types, return_type: return_typ, self_bind };
    let cached = CachedKernel {
        fn_name: kernel_name.clone(),
        kernel: std::sync::Arc::new(sig),
        signature,
        captures,
        is_rec,
        self_bind,
        apply_sites: discovery.apply_sites,
    };
    ec.fusion.kernels.lock().insert(key, cached.clone());
    Some(cached)
}

/// The SHARED structural tail-loop predicate, behind both the JIT's
/// native-loop gate (`build_lambda_kernel`'s `has_tail`) and the
/// interpreter's per-`GXLambda` `tail_loop` gate (which
/// `analysis::analyze` additionally sync-gates). ONE seam, so the two
/// backends can never disagree on WHICH lambdas loop: `g` is
/// self-recursive w.r.t. `self_bind`, every formal is a register-loopable
/// kind (`Prim`/`Array`/`Tuple`/`Struct` — what the rebind-and-jump loop
/// can carry; other kinds keep native / per-cycle recursion), and the
/// body has a self-call in tail position. NOT sync-gated here — fusion
/// excludes effect on purpose (a fusable body is sync by construction;
/// an effect-inference false-negative must never flip a JIT loop to
/// recursion), and `analyze` applies the sync gate for the interpreter.
pub(crate) fn structural_tail_loop<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    self_bind: BindId,
    ec: &ExecCtx<R, E>,
) -> bool {
    // is_rec: the body references its own binding. (A formal can't be
    // `self_bind` — formals are pattern-bound, `self_bind` is a `let`.)
    let mut refs = Refs::default();
    g.body().refs(&mut refs);
    let mut is_rec = false;
    refs.with_external_refs(|id| {
        if id == self_bind {
            is_rec = true;
        }
    });
    if !is_rec {
        return false;
    }
    // Simple rebind only: all-positional formals, no vargs. The
    // interpreter's tail-loop rebinds the self-call's positional args
    // 1:1 onto the formals (`analysis::analyze`); gating both backends
    // here keeps them in lockstep (a labeled/varargs self-call stays
    // native / per-cycle recursion in BOTH).
    if g.typ().vargs.is_some() {
        return false;
    }
    // Every formal must be positional AND a register-loopable kind (the
    // same classification `build_lambda_kernel` derives for its slots).
    for fa in g.typ().args.iter() {
        if !matches!(fa.kind, FnArgKind::Positional { .. }) {
            return false;
        }
        let arg_typ = resolve_abstract(&ec.fusion.abstract_registry, &fa.typ, &ec.env);
        let kt = match kernel_abi::freeze_for_abi_normalized(
            &ec.fusion.abstract_registry,
            &arg_typ,
        ) {
            Some(t) => t,
            None => return false,
        };
        match type_to_region_input_kind(&ec.fusion.abstract_registry, kt) {
            Some(
                RegionInputKind::Prim(_)
                | RegionInputKind::Array(_)
                | RegionInputKind::Tuple(_)
                | RegionInputKind::Struct(_),
            ) => {}
            _ => return false,
        }
    }
    body_has_self_tail_call(g.body(), self_bind)
}

/// Pure tail-position pre-scan: does this body contain a self tail
/// call (a `CallSite` in tail position whose fnode `Ref` carries
/// `self_bind`)? Mirrors `emit_body_into` / `emit_do` / `emit_tail`'s
/// tail positions exactly: the root, a Block's LAST child, Select arm
/// bodies, and ExplicitParens. Decides `has_tail_loop` BEFORE
/// emission: `emit_tail` only lowers a `TailCall` when `self_info` is
/// `Some`, and the caller only passes `Some` when this returned true,
/// so a kernel can't emit a `TailCall` without its loop head. (The
/// converse over-approximation is harmless: a detected-but-rejected
/// tail call — e.g. an arg type mismatch at emit time — leaves a
/// vestigial loop head the body jumps into once.)
pub(crate) fn body_has_self_tail_call<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    self_bind: BindId,
) -> bool {
    use NodeView;
    match node.view() {
        NodeView::Block(b) => {
            b.children.last().is_some_and(|c| body_has_self_tail_call(c, self_bind))
        }
        NodeView::ExplicitParens(ep) => body_has_self_tail_call(&ep.n, self_bind),
        NodeView::Select(s) => {
            s.arms.iter().any(|(_, body)| body_has_self_tail_call(&body.node, self_bind))
        }
        NodeView::CallSite(cs) => {
            matches!(cs.fnode().view(), NodeView::Ref(r) if r.id == self_bind)
        }
        _ => false,
    }
}

/// A recursive lambda's self-call is a second value source for its
/// formals (besides the outer call). Verify every self-call in the body
/// feeds each positional formal a value whose frozen type is contained
/// in that formal's kernel-ABI slot type (`formal_kts`). A mismatch
/// means the formal's inferred type was narrower than the union it
/// should have had (the swallowed body recheck dropped the self-call's
/// contribution), so the kernel ABI would be a lie — return `false` and
/// the caller de-fuses (#21). Conservative: an arg that can't be frozen,
/// or a self-call that doesn't map 1:1 onto the positional formals, both
/// count as inconsistent. Descends the body via `for_each_node` (which
/// skips nested lambda bodies — a self-call buried in a nested closure
/// stays a fusion gap, not a soundness hole, since that closure fuses
/// under its own build).
fn self_calls_abi_consistent<R: Rt, E: UserEvent>(
    body: &Node<R, E>,
    self_bind: BindId,
    formal_kts: &[Type],
    ec: &ExecCtx<R, E>,
) -> bool {
    let reg = &ec.fusion.abstract_registry;
    let mut ok = true;
    fusion::for_each_node(body, &mut |n| {
        if !ok {
            return;
        }
        let NodeView::CallSite(cs) = n.view() else { return };
        if !matches!(cs.fnode().view(), NodeView::Ref(r) if r.id == self_bind) {
            return;
        }
        for (i, formal_kt) in formal_kts.iter().enumerate() {
            let Some(arg) = cs.arg_positional(i) else {
                ok = false;
                return;
            };
            let Some(arg_kt) = kernel_abi::freeze_for_abi_normalized(
                reg,
                &resolve_abstract(reg, arg.typ(), &ec.env),
            ) else {
                ok = false;
                return;
            };
            if formal_kt.check_contains(&ec.env, &arg_kt).is_err() {
                ok = false;
                return;
            }
        }
    });
    ok
}

/// Per-input slot classification. Mirrors the param shapes
/// [`build_kir_kernel`] derives from a `LambdaExpr`'s argspec —
/// scalar primitive, array of primitive, tuple/struct/variant of
/// primitives. Function-typed inputs are not supported in the M8.4
/// initial model (a region has no HOF params; HOF callees come in
/// through the `known` map as cross-kernel call targets).
#[derive(Debug, Clone)]
pub enum RegionInputKind {
    Prim(PrimType),
    /// Array of `Type` element (the carried `Type` is the *element*
    /// type, frozen).
    Array(Type),
    /// Tuple — the carried `Type` is the full `Type::Tuple` (per-slot
    /// types read back via `kernel_abi::tuple_slots`).
    Tuple(Type),
    /// Struct — the carried `Type` is the full `Type::Struct`.
    Struct(Type),
    /// Variant — the carried `Type` is the full variant type
    /// (`Type::Variant` or a `Type::Set` of single-Variant members).
    Variant(Type),
    /// `[T, null]` option shape — runtime representation is a `Value`
    /// that is either `Value::Null` or `T`'s form. The carried `Type`
    /// is the non-null inner element type (frozen).
    Nullable(Type),
    /// String — runtime representation is an owned `ArcStr` (one
    /// machine word). Bound into the kernel's `string_params`.
    String,
    /// Bare value-shape (`DateTime`/`Duration`/`Bytes`/`Map`/`Error`) —
    /// two-word `Value` wire shape; carries the full `Type` so a
    /// `Local` read re-wraps to the right type. Bound into
    /// `value_params`.
    Value(Type),
}

/// Classify a (frozen) value [`Type`] into the matching
/// [`RegionInputKind`] so it can be registered as a kernel input.
/// Returns `None` for types that have no kernel-input representation:
/// - `Unit` (no value), bare `Null` (always widened to `Nullable<T>`
///   at the construction site).
/// - function types (handled via the `fn_inputs` channel or the
///   statically-resolved-call path, not as a value slot).
///
/// `String` and the bare value-shape types (`DateTime`/`Duration`/
/// `Bytes`/`Map`/`Error`) ARE representable: a String input rides the
/// `string_params` 1-word ABI slot, a value-shape input rides the
/// `value_params` 2-word slot — both with full backend (interp + JIT)
/// marshalling.
pub(crate) fn type_to_region_input_kind(
    reg: &AbstractRegistry,
    t: Type,
) -> Option<RegionInputKind> {
    use AbiKind;
    // FREEZE at the boundary. The Type comes from a binding's declared
    // type, which may still wrap a (bound) TVar or a Ref. `abi_kind`
    // derefs, so it classifies a TVar-bound `Tuple` as `Tuple` — but the
    // non-derefing accessors (`tuple_slots`/`struct_fields`/`array_elem`)
    // that `populate_kernel_inputs` later runs on the STORED `Type`
    // would then return `None` and silently produce an empty slot. Store
    // the concrete frozen `Type` so those accessors always succeed.
    let t = kernel_abi::freeze_for_abi(reg, &t)?;
    match abi_kind(reg, &t)? {
        AbiKind::Scalar(p) => Some(RegionInputKind::Prim(p)),
        AbiKind::Array => {
            kernel_abi::array_elem(&t).map(|e| RegionInputKind::Array(e.clone()))
        }
        AbiKind::Tuple => Some(RegionInputKind::Tuple(t)),
        AbiKind::Struct => Some(RegionInputKind::Struct(t)),
        AbiKind::Variant => Some(RegionInputKind::Variant(t)),
        AbiKind::Nullable => {
            kernel_abi::nullable_inner(reg, &t).map(RegionInputKind::Nullable)
        }
        AbiKind::String => Some(RegionInputKind::String),
        AbiKind::Value => Some(RegionInputKind::Value(t)),
        AbiKind::Unit | AbiKind::Null => None,
    }
}

/// True if a (frozen) `Type` is a marshallable `DynCall` **argument**
/// shape — every fusable shape except `Unit` (no value to pass) and
/// bare `Null` (always widened to `Nullable<T>` at construction).
fn is_dyncall_arg_supported(reg: &AbstractRegistry, t: &Type) -> bool {
    use AbiKind;
    match abi_kind(reg, t) {
        Some(
            AbiKind::Scalar(_)
            | AbiKind::Array
            | AbiKind::Tuple
            | AbiKind::Struct
            | AbiKind::Variant
            | AbiKind::Nullable
            | AbiKind::Value
            | AbiKind::String,
        ) => true,
        Some(AbiKind::Unit | AbiKind::Null) | None => false,
    }
}

/// True if `t` derefs to the single-bit `bytes` primitive.
pub(crate) fn is_bytes(t: &Type) -> bool {
    t.with_deref(|r| match r {
        Some(Type::Primitive(p)) => {
            p.contains(netidx_value::Typ::Bytes) && p.iter().count() == 1
        }
        _ => false,
    })
}

/// True if `t` derefs to the single-bit `Map` shape.
pub(crate) fn is_map(t: &Type) -> bool {
    t.with_deref(|r| matches!(r, Some(Type::Map { .. })))
}

/// True if `t` derefs to one of the single-bit `datetime`/`duration`
/// value-shape primitives.
pub(crate) fn is_datetime_or_duration(t: &Type) -> bool {
    t.with_deref(|r| match r {
        Some(Type::Primitive(p)) if p.iter().count() == 1 => {
            p.contains(netidx_value::Typ::DateTime)
                || p.contains(netidx_value::Typ::Duration)
        }
        _ => false,
    })
}

/// True if a (frozen) `Type` is a marshallable `DynCall` **return**
/// shape — every fusable shape except bare `Null`. `Unit` IS allowed
/// (side-effect-only sync builtins like `println` return Bottom).
fn is_dyncall_return_supported(reg: &AbstractRegistry, t: &Type) -> bool {
    use AbiKind;
    match abi_kind(reg, t) {
        Some(
            AbiKind::Scalar(_)
            | AbiKind::Array
            | AbiKind::Tuple
            | AbiKind::Struct
            | AbiKind::Variant
            | AbiKind::Nullable
            | AbiKind::Value
            | AbiKind::Unit
            | AbiKind::String,
        ) => true,
        Some(AbiKind::Null) | None => false,
    }
}
