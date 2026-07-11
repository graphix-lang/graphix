//! Fusion support library: builtin-call discovery, kernel signature
//! derivation (lambda callees, per-slot HOF callbacks, body-split
//! sub-regions), and the abstract-type resolver. Body code generation
//! lives with each node (`Update::emit_clif` / `Apply::emit_clif`,
//! see `design/distributed_jit.md`); this module supplies the
//! analysis those emitters consume.

use crate::{
    ApplyView, BindId, ExecCtx, Node, NodeView, Refs, Rt, Scope, Update, UserEvent,
    env::Env,
    expr::{Expr, ExprId, ExprKind, ModPath},
    fusion::{
        self, LambdaCallInfo,
        emit::WrappedKernel,
        kernel_abi::{
            self, AbiKind, AbstractRegistry, BuiltinSlot, FnParam, FnSource, KernelSig,
            Seen, abi_kind, freeze_for_abi_normalized, scalar_prim,
        },
    },
    node::{Cached, callsite::CallSite, genn, lambda::GXLambda},
    typ::{FnArgKind, FnType, Type},
};
use arcstr::ArcStr;
use netidx_value::Value;

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
/// Descent is [`fusion::for_each_node`] — the canonical
/// full-coverage walker, so lambda bodies are NOT descended (they
/// are separate kernels with their own discovery pass). Each site
/// reads the CallSite's resolved FnType and its compiled arg/return
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
    // Collect the inline-emitted HOF callback bodies encountered this
    // pass; recurse into them afterward (a closure can't re-enter this
    // fn while it holds `out`). Each body's borrow ties to `node`.
    let mut hof_bodies: Vec<&Node<R, E>> = Vec::new();
    fusion::for_each_node(node, &mut |n| match n.view() {
        NodeView::CallSite(cs) => {
            try_register_builtin_call_from_callsite(cs, ctx, out);
            // A HOF builtin inline-emits its callback into the kernel, but
            // `for_each_node` skips lambda bodies — descend explicitly so
            // the callback's builtin-calls/casts/qops are discovered.
            if let Some(apply) = cs.callee_apply() {
                apply.for_each_hof_callback_body(&mut |body| hof_bodies.push(body));
            }
        }
        NodeView::TypeCast(tc) => try_register_cast(tc, ctx, out),
        NodeView::Qop(q) => try_register_qop_deliver(q, ctx, out),
        _ => {}
    });
    for body in hof_bodies {
        walk_node_for_builtin_calls(body, ctx, out);
    }
}

/// Given the synthesized callback-CallSite Node a HOF holds in its
/// `analysis_pred` (a resolved CallSite to the callback lambda), reach
/// the callback's BODY node — the SAME chain `MapQ::emit_clif` &c. use,
/// so the ExprIds match what discovery registers. `None` if the callback
/// isn't a statically-resolved lambda (then the HOF won't fuse anyway).
pub fn hof_callback_body<'a, R: Rt, E: UserEvent>(
    pred: &'a Node<R, E>,
) -> Option<&'a Node<R, E>> {
    let NodeView::CallSite(inner_cs) = pred.view() else {
        return None;
    };
    let crate::ApplyView::Lambda(g) = inner_cs.resolved_apply()? else {
        return None;
    };
    Some(g.body())
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
pub(crate) fn resolve_abstract(reg: &AbstractRegistry, typ: &Type, env: &Env) -> Type {
    resolve_abstract_d(reg, typ, env, None)
}

fn resolve_abstract_d<'a>(
    reg: &AbstractRegistry,
    typ: &Type,
    env: &Env,
    seen: Option<&'a Seen<'a>>,
) -> Type {
    use kernel_abi::{ExpandKey, Seen};
    use triomphe::Arc;
    // Non-regular-recursion backstop (see fn doc). Counts EXPANSIONS, not
    // structural nesting, so finite types of any structural depth are
    // unaffected. It bounds the number of distinct EXPANSIONS on one path
    // (named-alias / abstract hops): the case it's FOR is non-regular
    // recursion whose params grow each step (identity can't catch it), but
    // a pathologically long FINITE expansion chain (>256 distinct hops)
    // would also be left opaque and simply not fuse.
    if Seen::len(seen) > 256 {
        return typ.clone();
    }
    match typ {
        // Deref bound TVars and resolve through them — an INFERRED
        // binding type (e.g. a region input's `let` binding, #218) is
        // TVar-wrapped, unlike the declared signature types the
        // classic kernel-build callers pass. An unbound TVar returns
        // unchanged (freeze rejects it downstream, correctly).
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
            Some(t) => resolve_abstract_d(reg, &t, env, seen),
            None => typ.clone(),
        },
        Type::Ref(tr) => {
            let key = ExpandKey::Ref(tr.clone());
            if Seen::contains(seen, &key) {
                return typ.clone(); // recursive named type — leave opaque
            }
            match typ.lookup_ref(env) {
                Ok(resolved) if !matches!(&resolved, Type::Ref(_)) => {
                    let node = Seen::push(seen, key);
                    resolve_abstract_d(reg, &resolved, env, Some(&node))
                }
                _ => typ.clone(),
            }
        }
        Type::Abstract { id, params } if params.is_empty() => {
            let key = ExpandKey::Abstract(*id);
            if Seen::contains(seen, &key) {
                return typ.clone(); // recursive abstract — leave opaque
            }
            match reg.get(id) {
                Some(concrete) => {
                    let node = Seen::push(seen, key);
                    resolve_abstract_d(reg, &concrete, env, Some(&node))
                }
                None => typ.clone(),
            }
        }
        Type::Tuple(ts) => Type::Tuple(Arc::from_iter(
            ts.iter().map(|t| resolve_abstract_d(reg, t, env, seen)),
        )),
        Type::Array(t) => Type::Array(Arc::new(resolve_abstract_d(reg, t, env, seen))),
        Type::Set(ts) => Type::Set(Arc::from_iter(
            ts.iter().map(|t| resolve_abstract_d(reg, t, env, seen)),
        )),
        Type::Struct(fs) => Type::Struct(Arc::from_iter(
            fs.iter().map(|(n, t)| (n.clone(), resolve_abstract_d(reg, t, env, seen))),
        )),
        other => other.clone(),
    }
}

/// FusionCtx-free core of [`ensure_lambda_kernel`]: build (or cache-hit)
/// the fused [`CachedKernel`] for a resolved, concretely-typed lambda
/// `g`, populating the per-`ExecCtx` `(LambdaId, resolved FnType)` cache.
/// Does NOT touch the transient `FusionCtx` (no `known_fns` /
/// `called_kernels`). Returns the cached kernel plus the transitively-
/// called sub-kernels discovered during this build (empty on a cache
/// hit). Shared by `ensure_lambda_kernel` (the region-splice
/// cross-kernel call path, which then registered the callee) and — the
/// reason it's extracted — the per-slot HOF dispatch path
/// (`design/impure_hof_fusion.md`), which wraps the artifact in a
/// runtime `Kernel` instead.
///
/// `kernel_name` becomes the built `CachedKernel.fn_name`; a cache hit
/// returns the first builder's name. The region-splice path passes the
/// call site's source name; the per-slot path passes a unique
/// per-`LambdaId` name (the SAFETY INVARIANT above requires uniqueness
/// once a kernel is reached without a Ref fnode — there is no
/// name-keyed call resolution in the per-slot path, so a
/// first-builder-wins `fn_name` is benign there, but uniqueness keeps
/// the two paths from cross-contaminating a shared cache entry's name).
///
/// `self_bind` is the binding the call site's fnode references (its
/// `Ref` node's BindId), when there is one. It identifies the lambda's
/// OWN binding so that (a) the body's self-reference isn't mistaken
/// for a capture (a rec binding's env type is a TVar-wrapped `Fn`,
/// which the capture scan can't freeze), (b) self tail-calls lower to
/// `TailCall` (the rebind-and-jump loop), and (c) `emit_known_fused_call`
/// can verify a name-resolved call really targets this binding (#206).
pub(crate) fn build_lambda_kernel<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    site_ftype: &FnType,
    kernel_name: &ArcStr,
    self_bind: Option<BindId>,
    ec: &mut ExecCtx<R, E>,
) -> Option<CachedKernel> {
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
    let is_rec = self_bind.is_some_and(|sb| external.iter().any(|id| *id == sb));
    // A recursive self-call is a SECOND source of values for the formals
    // (besides the outer call). If inference resolved a formal to the
    // outer call's type but the self-call feeds it a differently-shaped
    // value — the union the formal should have had was dropped by the
    // swallowed body recheck — the kernel would marshal that value under
    // the wrong ABI (a composite pointer read as a scalar leaks it; a
    // scalar deref'd as a pointer crashes). Refuse to fuse such a lambda;
    // the call node-walks, which is canonical and correct (#21, soak
    // jul04). Well-typed recursion (self-call args match the formals)
    // passes this check untouched.
    if is_rec
        && let Some(sb) = self_bind
        && !self_calls_abi_consistent(g.body(), sb, &formal_kts, ec)
    {
        return None;
    }
    // The native-loop gate is the SHARED structural predicate — the same
    // one `analysis::analyze` uses (sync-gated) for the interpreter's
    // tail-loop, so the two backends can't disagree on which lambdas loop.
    let has_tail = self_bind.is_some_and(|sb| structural_tail_loop(g, sb, ec));
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
    // lets the body emitter recognise each call site. The per-slot HOF
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

/// A callback fused for per-slot reactive dispatch by a HOF builtin
/// (MapQ / FoldQ / …). Built once at compile time from a concretely-
/// typed, statically-resolved analysis CallSite via [`fuse_callsite`];
/// each per-slot invocation builds a fresh [`builder::FusedKernel`] Node
/// (its own per-instance state) that *shares* the compiled kernel
/// artifacts via `Arc`. This is the per-slot dispatch primitive of the
/// impure-HOF-fusion milestone (`design/impure_hof_fusion.md`): the
/// builtin keeps its own per-slot reactive lifecycle (one graph per
/// element, fresh BindIds), and only the pure kernel is shared.
pub struct FusedCallback {
    /// The whole-body kernel — `build_slot` wraps it in one
    /// `FusedKernel`. A callback becomes a `FusedCallback` only when its
    /// WHOLE body lowers to one kernel; an impure callback (async ops in
    /// the body) instead fuses its maximal sync sub-regions in place via
    /// the canonical `fusion::fuse` walk at the call site (see
    /// `MapQ`'s template build) and never becomes a `FusedCallback`.
    kernel: std::sync::Arc<KernelSig>,
    wrapped: Option<std::sync::Arc<WrappedKernel>>,
    /// Captured outer bindings (closure conversion), in kernel-input
    /// order *after* the formal args. Each is fed by a shared `Ref`
    /// feeder (to its bind_id) appended after the per-slot element
    /// feeder(s).
    captures: Vec<CaptureSlot>,
    /// Number of formal args — the per-slot element feeders the caller
    /// supplies. Kernel inputs are `formal args ++ captures`.
    n_formal: usize,
    /// Body spec + result type for the per-slot `FusedKernel` Node.
    spec: Expr,
    typ: Type,
}

impl std::fmt::Debug for FusedCallback {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FusedCallback")
            .field("n_formal", &self.n_formal)
            .field("captures", &self.captures.len())
            .field("jit", &self.wrapped.is_some())
            .finish()
    }
}

/// Build a per-slot-dispatchable [`FusedCallback`] from a concretely-
/// typed, statically-resolved analysis CallSite (e.g. MapQ's
/// `analysis_pred.pred`) when its WHOLE body lowers to one kernel.
/// Returns `None` when the callback isn't a resolved lambda or its body
/// has async ops (so it can't fuse as a single kernel) — the caller then
/// fuses the body's maximal sync sub-regions in place via
/// `fusion::fuse` (the impure-HOF split), or falls back to the
/// interpreted per-slot dispatch. JIT-compiles the kernel when
/// `ec.fusion.enabled` (interp fallback inside `FusedKernel` handles `None`).
pub fn fuse_callsite<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    ec: &mut ExecCtx<R, E>,
) -> Option<FusedCallback> {
    let g = match cs.resolved_apply()? {
        ApplyView::Lambda(g) => g,
        _ => return None,
    };
    // Unique per-`LambdaId` kernel name. There is no Ref fnode here, so
    // the source-name SAFETY INVARIANT in `ensure_lambda_kernel` would
    // be violated by reusing a source name — and the per-slot path keys
    // no name-keyed call resolution anyway, so uniqueness is both
    // required and sufficient.
    let kernel_name: ArcStr =
        compact_str::format_compact!("__hof_{}", g.id().inner()).as_str().into();
    // A NAMED callback (`array::map(a, f)`) still has a Ref fnode —
    // thread its BindId so a recursive callback's self-reference is
    // recognised (tail self-calls fuse via the BindId-matched
    // `TailCall`; non-tail self-calls de-fuse on the `__hof_*` /
    // source-name mismatch in `known_fns` — sound, just unfused).
    let self_bind = match cs.fnode().view() {
        NodeView::Ref(r) => Some(r.id),
        _ => None,
    };
    let n_formal = g.typ().args.len();
    let spec = g.body().spec().clone();
    let typ = g.body().typ().clone();
    // Phase 1: try fusing the WHOLE body into one kernel (wholly-sync
    // callback). The signature build is shape gating only; the body is
    // validated by the compile attempt itself, so a body that doesn't
    // emit (async ops, unsupported shapes) fails the JIT compile and
    // falls through to Phase 2: fuse the body's sync sub-regions
    // instead (`build_body_split`).
    let site_ftype = cs.resolved_ftype().or_else(|| cs.ftype())?.clone();
    if let Some(cached) = build_lambda_kernel(g, &site_ftype, &kernel_name, self_bind, ec)
    {
        // Self-call info for a recursive callback (Node emission
        // needs it; mirrors `discover_lambda_calls`).
        let self_call = cached.is_rec.then(|| {
            (
                cached.self_bind.expect(
                    "is_rec without self_bind — build_lambda_kernel \
                     derives is_rec FROM self_bind",
                ),
                LambdaCallInfo {
                    fn_name: cached.fn_name.clone(),
                    kernel: cached.kernel.clone(),
                    arg_types: cached.signature.arg_types.clone(),
                    captures: cached.captures.clone(),
                },
            )
        });
        if let Some(wrapped) = jit_compile_split_kernel(
            ec,
            &cached.kernel,
            g.body(),
            self_call.as_ref(),
            &cached.apply_sites,
        ) {
            return Some(FusedCallback {
                kernel: cached.kernel,
                wrapped: Some(wrapped),
                captures: cached.captures,
                n_formal,
                spec,
                typ,
            });
        }
    }
    // The whole body didn't lower to one kernel (async ops in the body).
    // The caller fuses its maximal sync sub-regions in place via
    // `fusion::fuse` (the impure-HOF split).
    None
}

/// JIT-compile the whole-body callback kernel when `ec.fusion.enabled`,
/// else `None` (the slot node-walks).
///
/// `body` is the kernel's body Node and `self_call` its self-recursion
/// info; the kernel emits by `emit_clif` over the Node. `apply_sites`
/// is the body's builtin-call discovery (empty for the whole-body
/// path). The callee set is always empty: a callback body's own call
/// sites are #203-unresolved, and a self-recursive body resolves
/// against the kernel's own declaration.
fn jit_compile_split_kernel<R: Rt, E: UserEvent>(
    ec: &mut ExecCtx<R, E>,
    kernel: &std::sync::Arc<KernelSig>,
    body: &Node<R, E>,
    self_call: Option<&(BindId, LambdaCallInfo)>,
    apply_sites: &nohash::IntMap<ExprId, BuiltinCallSiteInfo>,
) -> Option<std::sync::Arc<WrappedKernel>> {
    if !ec.fusion.enabled {
        return None;
    }
    // #203: discover lambda calls INSIDE the callback body (e.g. mandelbrot's
    // `iterate`), build their callee kernels, and thread them into the compile
    // — instead of the empty maps that forced every nested call to de-fuse.
    // `apply_sites` (the callback body's own sync builtin/cast/qop sites) is
    // passed by the caller: `build_lambda_kernel` now installs the matching
    // `fn_params` on this kernel's sig, so the per-slot `FusedKernel` pre-binds
    // those slots and the body's DynCalls dispatch in-kernel (offset 0 — the
    // callback IS the parent). A callee body's OWN builtins still need Stage 2's
    // combined-slot delivery; until then a callee-with-a-builtin de-fuses.
    let (lambda_sites, callees, callee_bodies) = fusion::discover_lambda_calls(body, ec);
    let r = fusion::emit::compile_kernel_with_callees_direct(
        &mut ec.fusion.jit.lock(),
        kernel,
        &callees,
        body,
        apply_sites,
        &lambda_sites,
        &callee_bodies,
        self_call,
        &ec.env,
        &ec.fusion.abstract_registry,
        // Per-slot HOF callback templates have no region-level lifts.
        &ahash::AHashSet::default(),
        // Lambda kernel: native cross-kernel entry bypasses the
        // node-level reset_replay, so replay words are refused (their
        // per-iteration reset contract can't be honored — jul10h
        // 000009). Stateful constructs keep the stateless fallback.
        false,
    );
    match r {
        Ok(w) => Some(std::sync::Arc::new(w)),
        Err(e) => {
            // A failed per-slot compile is a quiet de-fuse (the slot
            // node-walks) — but never a SILENT one.
            log::trace!("per-slot kernel `{}` failed to compile: {e:#}", kernel.fn_name);
            None
        }
    }
}

impl FusedCallback {
    /// Build a fresh per-slot [`builder::FusedKernel`] Node sharing this
    /// callback's compiled kernel `Arc`. `element_feeders` are the
    /// formal-arg feeders the caller supplies per slot (for MapQ: the
    /// slot's element binding Node); the capture feeders (shared `Ref`s
    /// to the captured bindings) are appended automatically, in the same
    /// `formal args ++ captures` order the kernel inputs were built.
    pub fn build_slot<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<R, E>,
        element_feeders: Vec<Node<R, E>>,
        scope: Scope,
        top_id: ExprId,
    ) -> anyhow::Result<Node<R, E>> {
        if element_feeders.len() != self.n_formal {
            anyhow::bail!(
                "fused HOF slot: expected {} formal feeder(s), got {}",
                self.n_formal,
                element_feeders.len()
            );
        }
        let kernel = self.kernel.clone();
        let mut feeders = element_feeders;
        for cap in &self.captures {
            let typ = ctx
                .env
                .by_id
                .get(&cap.bind_id)
                .map(|b| b.typ.clone())
                .unwrap_or(Type::Bottom);
            feeders.push(genn::reference(ctx, cap.bind_id, typ, top_id));
        }
        fusion::builder::FusedKernel::<R, E>::new(
            ctx,
            self.spec.clone(),
            self.typ.clone(),
            kernel,
            self.wrapped.clone(),
            feeders.into_boxed_slice(),
            scope,
            top_id,
        )
    }
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
