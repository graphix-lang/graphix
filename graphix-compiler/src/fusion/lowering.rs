//! Node fusion: identify pure-expression / pure-function subtrees and
//! lower them into the typed kernel IR ([`crate::gir`]). The
//! resulting [`GirExpr`] / [`GirKernel`] is then lowered to Cranelift
//! IR by the JIT path (see `crate::gir_jit`) for in-process JIT
//! compilation.
//!
//! This module is the front end (Graphix `Expr` → GIR). The IR types
//! live in [`crate::gir`].
//!
//! Driving examples are mandelbrot's `iterate` (primitive args, a
//! `select` with arithmetic guards, a self-recursive tail call) and
//! naive `fib` (primitive args, non-tail self recursion lowered as
//! direct recursion).

use crate::{
    expr::{Expr, ExprKind, ModPath, Pattern, StructurePattern},
    gir::{
        self as gir, ConstVal, GirExpr, GirKernel, GirOp, GirStmt, Let, SelectArm,
    },
    typ::Type,
    Update,
};
use arcstr::ArcStr;
use netidx_value::Value;

// Re-export the canonical GIR types so existing callers (graphix-shell,
// graphix-package-bench, in-tree tests) keep compiling. The IR's
// definitive home is `crate::gir`; these aliases exist purely to
// keep the public API surface stable across the move.
pub use crate::gir::{Input, GirType, KnownConst, KnownFusedFn, PrimType};

/// Lookup table the emitter consults whenever it sees a `Ref` — tells
/// us the variable's primitive type and the name to use in generated
/// code. Also carries a small registry of already-fused functions so a
/// kernel body can direct-call another fused kernel instead of round-
/// tripping through the interpreted CallSite, plus a registry of
/// compile-time-known primitive constants for inlining.
#[derive(Debug, Clone, Default)]
pub struct FusionCtx {
    pub inputs: Vec<Input>,
    /// Function-typed parameters of the kernel being built. Mirrors
    /// the kernel's `fn_params` list — `Apply{Ref(name)}` against any
    /// of these names lowers to [`GirOp::DynCall`] instead of a
    /// static call. Keyed by Graphix name; the value is the param's
    /// (zero-based) index in this list, which becomes the
    /// `fn_index` in the emitted DynCall.
    pub fn_inputs: Vec<crate::gir::FnParam>,
    /// Array-typed parameters of the kernel being built. Mirrors the
    /// kernel's `array_params` list — `Ref(name)` resolves against
    /// these for `array::len(name)` and `name[i]` lowering. Sibling
    /// to `inputs` (no shadowing — same name in both is a fusion
    /// abort by `find_array`'s caller).
    pub array_inputs: Vec<crate::gir::ArrayInput>,
    /// Tuple-typed kernel parameters. Looked up by name when
    /// emitting `t.0` / `t.1` (TupleRef) accesses and recognised at
    /// `(a, b, c)` literal sites that match a known param shape.
    pub tuple_inputs: Vec<crate::gir::TupleInput>,
    /// Struct-typed kernel parameters. Same pattern — `s.field`
    /// (StructRef) accesses look up here, with the field name
    /// resolved to a sorted index at lowering time.
    pub struct_inputs: Vec<crate::gir::StructInput>,
    /// Variant-typed kernel parameters. Looked up by name when
    /// emitting tag-match dispatches and payload reads. The cases
    /// list constrains which tags can flow through; the lowering
    /// for a select arm matching `` `Foo(a, b) `` checks both that
    /// the tag matches and that the case shape matches the param's
    /// declared cases.
    pub variant_inputs: Vec<crate::gir::VariantInput>,
    /// Nullable-typed kernel parameters / lets — graphix's `[T, null]`
    /// option shape. `GirOp::IsNull` checks against these; reads via
    /// `GirOp::Local` return `EvalResult::Nullable(Value)` so the
    /// caller can distinguish `Value::Null` from a wrapped `T`.
    pub nullable_inputs: Vec<crate::gir::NullableInput>,
    /// String let-bindings inside the kernel body. Strings only appear
    /// here as locals — never as params (no string args on either
    /// backend) and never as kernel inputs from an outer region (no
    /// `RegionInputKind::String` shape).
    pub string_inputs: Vec<crate::gir::StringInput>,
    /// Other kernels already fused in the current pass. Used by
    /// `emit_expr` to lower `Apply { fn: Ref(name) }` to a direct call
    /// when `name` is in this map. Keyed by the Graphix-level name so
    /// rewrites done earlier in the walk are visible to later fusions.
    pub known_fns: std::collections::BTreeMap<ArcStr, KnownFusedFn>,
    /// Compile-time-known primitive constants, inlined at Ref sites.
    /// Populated by the rewrite pass from `let <name> = <literal>;`
    /// bindings. Keyed by Graphix-level name.
    pub known_consts: std::collections::BTreeMap<ArcStr, KnownConst>,
    /// Maximal-fusion *lifted-input* table: maps an `ExprId` inside the
    /// region body to the synthetic name of the corresponding kernel
    /// input. When `emit_expr` encounters an ExprId in this table it
    /// short-circuits the normal lowering and emits a `Local` read of
    /// the named input (mirroring what an `ExprKind::Ref` would do).
    /// The named input has already been registered into one of the
    /// `*_inputs` lists by `populate_kernel_inputs`.
    ///
    /// Populated by `discover_region_inputs` when the carving phase
    /// promotes an Async sub-expression to a kernel input
    /// ([`RegionInput`] with [`RegionInputSource::Lifted`]). Empty for
    /// non-region kernel builds (lambdas, the legacy lazy path).
    pub lifted_inputs: nohash::IntMap<crate::expr::ExprId, ArcStr>,
    /// Apply-site lookup table for builtin DynCalls. Populated by
    /// `discover_builtin_calls` before `emit_body` runs; each entry
    /// maps the source `Apply.spec.id` to the marshalling info
    /// `emit_expr` needs to emit a [`GirOp::DynCall`] against the
    /// corresponding [`crate::gir::FnSource::Builtin`] slot
    /// in `fn_inputs`/`fn_params`.
    pub builtin_apply_sites:
        nohash::IntMap<crate::expr::ExprId, BuiltinCallSiteInfo>,
}

/// Per-Apply-site info captured by `discover_builtin_calls` for use
/// by `emit_known_fused_call`'s builtin DynCall arm.
///
/// `fn_index` is the slot index in `GirKernel.fn_params` for this
/// call site's [`crate::gir::FnSource::Builtin`] entry.
///
/// `marshal_arg_indices` maps each kernel-level marshalled DynCall
/// arg position to its index in `Apply.args` (source order). At
/// emit time, `emit_known_fused_call` walks these in order and
/// lowers each named `Apply.args[idx].1` into GIR for the DynCall
/// `args` vector. Skip-positions (labeled args resolved to defaults)
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
    pub arg_types: Vec<GirType>,
    pub return_type: GirType,
}

/// Output of `discover_builtin_calls`.
#[derive(Debug, Default, Clone)]
pub struct BuiltinCallDiscovery {
    /// `FnParam` slots to install in the kernel's `fn_inputs` —
    /// one per discovered builtin Apply site, in walk order.
    pub fn_params: Vec<crate::gir::FnParam>,
    /// `Apply.spec.id → BuiltinCallSiteInfo` lookup so
    /// `emit_known_fused_call` can recognise the call site at emit
    /// time and lower it to a `GirOp::DynCall`.
    pub apply_sites:
        nohash::IntMap<crate::expr::ExprId, BuiltinCallSiteInfo>,
}

/// Walk `body` looking for `Apply` sites whose target resolves to a
/// sync builtin binding registered in `ctx.builtin_bindings`. For
/// each such site build a [`crate::gir::FnParam`] with
/// [`crate::gir::FnSource::Builtin`], capturing the layout
/// fusion needs to dispatch via the runtime's builtin Apply
/// machinery.
///
/// Lambdas inside `body` are NOT descended into — they are separate
/// kernels and their own discovery pass handles their bodies.
///
/// Sites whose argument shape can't be lowered (any arg with a
/// non-`PrimType` graphix type that isn't yet supported, async
/// builtins, mismatched arity, etc.) are simply omitted — the
/// kernel build falls back to `None` if `emit_expr` later needs
/// the missing entry, and a `None` build means the candidate stays
/// unfused.
pub fn discover_builtin_calls<R: crate::Rt, E: crate::UserEvent>(
    body: &Expr,
    ctx: &crate::ExecCtx<R, E>,
    scope: &crate::expr::ModPath,
) -> BuiltinCallDiscovery {
    let mut out = BuiltinCallDiscovery::default();
    walk_for_builtin_calls(body, ctx, scope, &mut out);
    out
}

/// Node-based variant of [`discover_builtin_calls`] — walks the
/// compiled Node tree via [`crate::NodeView`] instead of the source
/// `Expr` tree. This is the variant production-side fusion uses
/// (`fusion::fuse` in `fusion/mod.rs`).
///
/// **Why Node-based**: the Node tree is the *decorated* AST. At a
/// CallSite the runtime carries the call-site-instantiated
/// `FnType` (resolved during typecheck — TVars unified with the
/// call site's concrete arg types) in `CallSite::ftype()`. That's
/// the correct FnType to consult for builtins like `str::parse`
/// where the rtype is polymorphic (`'b: Cast`) and only the
/// call-site instantiation tells us the target type.
///
/// The Expr-based [`discover_builtin_calls`] above reads
/// `a.function.typ.get()` — the function-expression's typed-AST
/// cell — which carries the lambda's *original* generic FnType.
/// That's a latent bug; this Node-based variant fixes it by
/// reading from the CallSite's resolved FnType directly.
pub fn discover_builtin_calls_node<R: crate::Rt, E: crate::UserEvent>(
    node: &crate::Node<R, E>,
    ctx: &crate::ExecCtx<R, E>,
    scope: &crate::expr::ModPath,
) -> BuiltinCallDiscovery {
    let mut out = BuiltinCallDiscovery::default();
    walk_node_for_builtin_calls(&**node, ctx, scope, &mut out);
    out
}

/// Walk a `&dyn Update` subtree to discover builtin Apply sites.
/// Public so `fusion::fuse` can call it on a borrowed subtree
/// (returned by `find_node_by_id`) without needing to own a
/// `Node<R, E>`.
pub fn walk_node_for_builtin_calls<R: crate::Rt, E: crate::UserEvent>(
    node: &dyn crate::Update<R, E>,
    ctx: &crate::ExecCtx<R, E>,
    scope: &crate::expr::ModPath,
    out: &mut BuiltinCallDiscovery,
) {
    use crate::NodeView;
    // First: if this node is a CallSite, attempt to register a
    // builtin DynCall site against it. The CallSite carries the
    // resolved FnType (post-typecheck instantiation) which we use
    // instead of reading the function expression's typed-AST cell.
    if let NodeView::CallSite(cs) = node.view() {
        try_register_builtin_call_from_callsite(cs, ctx, scope, out);
    }
    // Then: descend. Lambdas inside the body are separate kernels and
    // are NOT descended into — their bodies belong to their own
    // discovery pass.
    match node.view() {
        // Leaves — nothing to recurse into.
        NodeView::Constant(_) | NodeView::Nop(_) | NodeView::Ref(_)
        | NodeView::Use(_) | NodeView::TypeDef(_) => {}
        NodeView::Lambda(_) => {}
        // Containers: walk children. Each carries a typed accessor
        // chain into its Nodes; the source Expr remains addressable
        // via spec() for any data we don't yet have an accessor for.
        NodeView::Bind(b) => walk_node_for_builtin_calls(&*b.node, ctx, scope, out),
        NodeView::Block(blk) => {
            for c in blk.children.iter() {
                walk_node_for_builtin_calls(&**c, ctx, scope, out);
            }
        }
        NodeView::Module(m) => {
            for c in m.nodes.iter() {
                walk_node_for_builtin_calls(&**c, ctx, scope, out);
            }
        }
        NodeView::CallSite(cs) => {
            walk_node_for_builtin_calls(&**cs.fnode(), ctx, scope, out);
            // Iterate args in source order via `spec_args`, look up
            // each arg's compiled Node via positional / named lookup.
            for (idx, (label, _expr)) in cs.spec_args().iter().enumerate() {
                let child = match label {
                    Some(name) => cs.arg_named(name),
                    None => cs.arg_positional(idx),
                };
                if let Some(c) = child {
                    walk_node_for_builtin_calls(&**c, ctx, scope, out);
                }
            }
        }
        // For all other node shapes we don't have typed-accessor
        // descent yet, fall back to walking the source Expr via
        // node.spec(). The Expr-level walker handles every shape
        // uniformly; the CallSite arm above is the only one where
        // Node-vs-Expr makes a semantic difference. Refs inside the
        // Expr resolve against `ctx.env` exactly as the Node-based
        // path would — they're free vars from the kernel's POV.
        _ => walk_for_builtin_calls(node.spec(), ctx, scope, out),
    }
}

/// Node-based variant of [`try_register_builtin_call`]. Reads the
/// resolved FnType from the CallSite (`callsite.ftype().map(|ft|
/// ft.resolve_tvars())`) instead of pulling it from the function-
/// expression's typed-AST cell. This fixes the latent bug where
/// generic builtins (`str::parse` with polymorphic `'b: Cast`
/// return type) would see the lambda's unresolved FnType at
/// runtime, surfacing as "parse requires a concrete type
/// annotation".
fn try_register_builtin_call_from_callsite<R: crate::Rt, E: crate::UserEvent>(
    cs: &crate::node::callsite::CallSite<R, E>,
    ctx: &crate::ExecCtx<R, E>,
    scope: &crate::expr::ModPath,
    out: &mut BuiltinCallDiscovery,
) {
    // Need the source Apply Expr (for ExprKind::Ref-vs-other check
    // on the function, and for ExprId to register the site). The
    // CallSite's spec is always `ExprKind::Apply`.
    let apply_expr = cs.spec();
    let a = match &apply_expr.kind {
        crate::expr::ExprKind::Apply(a) => a,
        _ => return,
    };
    // Same call-flow as the Expr-based path: the function must be a
    // direct binding Ref.
    let path = match &a.function.kind {
        crate::expr::ExprKind::Ref { name } => name,
        _ => return,
    };
    let (_, bind) = match ctx.env.lookup_bind(scope, path) {
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
    use crate::effects::EffectKind;
    match ctx.builtin_effects.get(info.name.as_str()) {
        Some(EffectKind::Sync) => {}
        _ => return,
    }
    // **The payoff**: use the CallSite's resolved FnType (TVars
    // unified at typecheck time to the call-site's concrete arg
    // types), rather than reading the function expression's
    // typed-AST cell.
    //
    // After typecheck has run for this CallSite, `cs.ftype()` is
    // `Some(ft)` carrying the lambda's signature with the call-site's
    // TVar bindings still live in the TVars' RwLock cells. Calling
    // `resolve_tvars()` deep-clones with every TVar dereffed to its
    // bound concrete type — exactly what fusion needs.
    //
    // `cs.ftype()` may be `None` if typecheck hasn't run yet, or
    // didn't reach this site (e.g. a compile error elsewhere). Fall
    // back to the binding's own FnType in that case — same shape
    // the Expr-based path uses, just as the last-resort fallback.
    let fn_type: std::sync::Arc<crate::typ::FnType> = match cs.ftype() {
        Some(ft) => std::sync::Arc::new(ft.resolve_tvars()),
        None => {
            let inner: &crate::typ::FnType = &info.typ;
            std::sync::Arc::new(inner.clone())
        }
    };
    // From here on the layout-derivation is identical to the
    // Expr-based path. Re-using the same arg classification +
    // GirType derivation keeps semantics consistent across the two
    // walkers during the transition.
    let apply_id = apply_expr.id;
    let mut call_positional: Vec<(usize, &Expr)> = Vec::new();
    let mut call_labeled: ahash::AHashMap<&str, (usize, &Expr)> =
        ahash::AHashMap::default();
    for (call_idx, (label, e)) in a.args.iter().enumerate() {
        match label {
            Some(name) => {
                call_labeled.insert(name.as_str(), (call_idx, e));
            }
            None => call_positional.push((call_idx, e)),
        }
    }
    let mut layout: Vec<crate::gir::BuiltinSlot> = Vec::new();
    let mut arg_types: Vec<GirType> = Vec::new();
    let mut marshal_arg_indices: Vec<usize> = Vec::new();
    let mut pos_iter = call_positional.iter();
    for fa in fn_type.args.iter() {
        use crate::typ::FnArgKind;
        match &fa.kind {
            FnArgKind::Positional { .. } => {
                let (call_idx, arg_expr) = match pos_iter.next() {
                    Some(p) => p,
                    None => return,
                };
                // resolve_tvars on the FnType has already deref'd
                // every TVar, so `fa.typ` is the concrete arg
                // type. Prefer the call-site Expr's typ cell if
                // present (cheaper than re-dereffing) — both
                // should agree post-resolve_tvars.
                let arg_typ = arg_expr.typ.get().cloned().unwrap_or_else(|| {
                    fa.typ.clone()
                });
                let kt = match GirType::from_type(&arg_typ) {
                    Some(t) => t,
                    None => return,
                };
                let slot_idx = arg_types.len();
                layout.push(crate::gir::BuiltinSlot::Positional(slot_idx));
                arg_types.push(kt);
                marshal_arg_indices.push(*call_idx);
            }
            FnArgKind::Labeled { name, has_default } => {
                if let Some((call_idx, arg_expr)) =
                    call_labeled.remove(name.as_str())
                {
                    let arg_typ = arg_expr.typ.get().cloned().unwrap_or_else(|| {
                        fa.typ.clone()
                    });
                    let kt = match GirType::from_type(&arg_typ) {
                        Some(t) => t,
                        None => return,
                    };
                    let slot_idx = arg_types.len();
                    layout.push(
                        crate::gir::BuiltinSlot::Positional(slot_idx),
                    );
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
                    layout.push(
                        crate::gir::BuiltinSlot::LabeledDefault(default),
                    );
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
        layout.push(crate::gir::BuiltinSlot::Variadic {
            from_call_idx,
            count,
        });
        for (call_idx, arg_expr) in remaining {
            let arg_typ = arg_expr.typ.get().cloned().or_else(|| {
                fn_type
                    .vargs
                    .as_ref()
                    .and_then(|t| t.with_deref(|t| t.cloned()))
            });
            let arg_typ = match arg_typ {
                Some(t) => t,
                None => return,
            };
            let kt = match GirType::from_type(&arg_typ) {
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
    // Return type — resolved on the call-site FnType already, so
    // we can use it directly. The Apply Expr's typ cell would
    // agree if populated.
    let ret_typ = apply_expr.typ.get().cloned().unwrap_or_else(|| {
        fn_type.rtype.clone()
    });
    let return_type = match GirType::from_type(&ret_typ) {
        Some(t) => t,
        None => return,
    };
    fn is_dyncall_supported(kt: &GirType) -> bool {
        match kt {
            GirType::Prim(_)
            | GirType::Array(_)
            | GirType::Tuple(_)
            | GirType::Struct(_)
            | GirType::Variant(_)
            | GirType::Nullable(_)
            | GirType::String => true,
            GirType::Unit | GirType::Null => false,
        }
    }
    if !arg_types.iter().all(is_dyncall_supported) {
        return;
    }
    let return_ok = match &return_type {
        GirType::Prim(_)
        | GirType::Array(_)
        | GirType::Tuple(_)
        | GirType::Struct(_)
        | GirType::Variant(_)
        | GirType::Nullable(_)
        | GirType::Unit
        | GirType::String => true,
        GirType::Null => false,
    };
    if !return_ok {
        return;
    }
    let fn_index = out.fn_params.len() as u32;
    out.fn_params.push(crate::gir::FnParam {
        name: info.name.clone(),
        source: crate::gir::FnSource::Builtin {
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
        BuiltinCallSiteInfo {
            fn_index,
            marshal_arg_indices,
            arg_types,
            return_type,
        },
    );
}

fn walk_for_builtin_calls<R: crate::Rt, E: crate::UserEvent>(
    expr: &Expr,
    ctx: &crate::ExecCtx<R, E>,
    scope: &crate::expr::ModPath,
    out: &mut BuiltinCallDiscovery,
) {
    use crate::expr::ExprKind::*;
    if let Apply(a) = &expr.kind {
        // Try to register this Apply as a builtin DynCall site.
        // Failure leaves out unchanged — the kernel build will bail
        // when `emit_expr` can't lower this Apply.
        try_register_builtin_call(expr, a, ctx, scope, out);
    }
    match &expr.kind {
        // Leaves — nothing to recurse into.
        Constant(_) | NoOp | Use { .. } | Ref { .. } | TypeDef { .. } => {}
        // Lambdas are separate kernels — their bodies belong to
        // their own discovery pass.
        Lambda(_) => {}
        // Unresolved module decls have nothing fusable inside.
        Module { .. } => {}
        // Recursive container forms — walk every child.
        ExplicitParens(e) | Qop(e) | OrNever(e) | ByRef(e) | Deref(e)
        | Not { expr: e } | TypeCast { expr: e, .. } => {
            walk_for_builtin_calls(e, ctx, scope, out);
        }
        StructRef { source, .. } | TupleRef { source, .. } => {
            walk_for_builtin_calls(source, ctx, scope, out);
        }
        ArrayRef { source, i } => {
            walk_for_builtin_calls(source, ctx, scope, out);
            walk_for_builtin_calls(i, ctx, scope, out);
        }
        ArraySlice { source, start, end } => {
            walk_for_builtin_calls(source, ctx, scope, out);
            if let Some(s) = start {
                walk_for_builtin_calls(s, ctx, scope, out);
            }
            if let Some(e) = end {
                walk_for_builtin_calls(e, ctx, scope, out);
            }
        }
        MapRef { source, key } => {
            walk_for_builtin_calls(source, ctx, scope, out);
            walk_for_builtin_calls(key, ctx, scope, out);
        }
        Do { exprs } => {
            for e in exprs.iter() {
                walk_for_builtin_calls(e, ctx, scope, out);
            }
        }
        Bind(b) => walk_for_builtin_calls(&b.value, ctx, scope, out),
        Connect { value, .. } => {
            walk_for_builtin_calls(value, ctx, scope, out);
        }
        StructWith(crate::expr::StructWithExpr { source, replace }) => {
            walk_for_builtin_calls(source, ctx, scope, out);
            for (_, e) in replace.iter() {
                walk_for_builtin_calls(e, ctx, scope, out);
            }
        }
        Apply(a) => {
            walk_for_builtin_calls(&a.function, ctx, scope, out);
            for (_, e) in a.args.iter() {
                walk_for_builtin_calls(e, ctx, scope, out);
            }
        }
        Any { args }
        | Array { args }
        | Tuple { args }
        | Variant { args, .. }
        | StringInterpolate { args } => {
            for e in args.iter() {
                walk_for_builtin_calls(e, ctx, scope, out);
            }
        }
        Map { args } => {
            for (k, v) in args.iter() {
                walk_for_builtin_calls(k, ctx, scope, out);
                walk_for_builtin_calls(v, ctx, scope, out);
            }
        }
        Struct(crate::expr::StructExpr { args }) => {
            for (_, e) in args.iter() {
                walk_for_builtin_calls(e, ctx, scope, out);
            }
        }
        Select(crate::expr::SelectExpr { arg, arms }) => {
            walk_for_builtin_calls(arg, ctx, scope, out);
            for (_, e) in arms.iter() {
                walk_for_builtin_calls(e, ctx, scope, out);
            }
        }
        TryCatch(tc) => {
            walk_for_builtin_calls(&tc.handler, ctx, scope, out);
            for e in tc.exprs.iter() {
                walk_for_builtin_calls(e, ctx, scope, out);
            }
        }
        Sample { lhs, rhs }
        | Eq { lhs, rhs }
        | Ne { lhs, rhs }
        | Lt { lhs, rhs }
        | Gt { lhs, rhs }
        | Lte { lhs, rhs }
        | Gte { lhs, rhs }
        | And { lhs, rhs }
        | Or { lhs, rhs }
        | Add { lhs, rhs }
        | CheckedAdd { lhs, rhs }
        | Sub { lhs, rhs }
        | CheckedSub { lhs, rhs }
        | Mul { lhs, rhs }
        | CheckedMul { lhs, rhs }
        | Div { lhs, rhs }
        | CheckedDiv { lhs, rhs }
        | Mod { lhs, rhs }
        | CheckedMod { lhs, rhs } => {
            walk_for_builtin_calls(lhs, ctx, scope, out);
            walk_for_builtin_calls(rhs, ctx, scope, out);
        }
    }
}

/// If `a`'s function resolves to a sync builtin binding registered
/// in `ctx.builtin_bindings`, build the `FnParam` + layout and
/// register the call site in `out`. On any rejection (async,
/// unsupported arg shape, etc.) silently returns without
/// registering — the kernel build path treats the site as an
/// ordinary Apply, which will then fail to lower.
fn try_register_builtin_call<R: crate::Rt, E: crate::UserEvent>(
    apply_expr: &Expr,
    a: &crate::expr::ApplyExpr,
    ctx: &crate::ExecCtx<R, E>,
    scope: &crate::expr::ModPath,
    out: &mut BuiltinCallDiscovery,
) {
    let apply_id = apply_expr.id;
    let path = match &a.function.kind {
        crate::expr::ExprKind::Ref { name } => name,
        _ => return,
    };
    let (_, bind) = match ctx.env.lookup_bind(scope, path) {
        Some(b) => b,
        None => return,
    };
    let key = (bind.scope.clone(), bind.name.clone());
    let info = match ctx.builtin_bindings.get(&key) {
        Some(i) => i.clone(),
        None => return,
    };
    let _ = apply_id;
    // Async builtins can't be fused — they may produce values on a
    // later cycle than the trigger.
    use crate::effects::EffectKind;
    match ctx.builtin_effects.get(info.name.as_str()) {
        Some(EffectKind::Sync) => {}
        _ => return,
    }
    // Resolve the call-site-specific FnType (the binding's FnType
    // may be generic — e.g. `fn(x: 'a) -> 'a`; the typed-AST cell
    // on `a.function` holds the concrete instantiation).
    let fn_type = match a.function.typ.get() {
        Some(crate::typ::Type::Fn(ft)) => ft.clone(),
        _ => info.typ.clone(),
    };
    // Partition the call-site args by positional vs labeled. For
    // each formal arg, decide whether it's satisfied positionally,
    // by label (matching call-site label name), or by default.
    let mut call_positional: Vec<(usize, &Expr)> = Vec::new();
    let mut call_labeled: ahash::AHashMap<&str, (usize, &Expr)> =
        ahash::AHashMap::default();
    for (call_idx, (label, e)) in a.args.iter().enumerate() {
        match label {
            Some(name) => {
                call_labeled.insert(name.as_str(), (call_idx, e));
            }
            None => call_positional.push((call_idx, e)),
        }
    }
    let mut layout: Vec<crate::gir::BuiltinSlot> = Vec::new();
    let mut arg_types: Vec<GirType> = Vec::new();
    let mut marshal_arg_indices: Vec<usize> = Vec::new();
    let mut pos_iter = call_positional.iter();
    for fa in fn_type.args.iter() {
        use crate::typ::FnArgKind;
        match &fa.kind {
            FnArgKind::Positional { .. } => {
                let (call_idx, arg_expr) = match pos_iter.next() {
                    Some(p) => p,
                    None => return, // missing positional — typecheck
                                    // should have caught
                };
                // Use the call-site arg's RESOLVED type (post-
                // typecheck). The formal arg's type may still be a
                // TVar for generic builtins like `bit_and`; the
                // call-site Expr's typ cell has the concrete
                // instantiation.
                let arg_typ = arg_expr.typ.get().cloned().or_else(|| {
                    fa.typ.with_deref(|t| t.cloned())
                });
                let arg_typ = match arg_typ {
                    Some(t) => t,
                    None => return,
                };
                let kt = match GirType::from_type(&arg_typ) {
                    Some(t) => t,
                    None => return,
                };
                let slot_idx = arg_types.len();
                layout.push(crate::gir::BuiltinSlot::Positional(slot_idx));
                arg_types.push(kt);
                marshal_arg_indices.push(*call_idx);
            }
            FnArgKind::Labeled { name, has_default } => {
                if let Some((call_idx, arg_expr)) =
                    call_labeled.remove(name.as_str())
                {
                    // Same TVar-resolution issue as the positional
                    // arm — prefer the call-site arg's resolved
                    // type.
                    let arg_typ = arg_expr.typ.get().cloned().or_else(|| {
                        fa.typ.with_deref(|t| t.cloned())
                    });
                    let arg_typ = match arg_typ {
                        Some(t) => t,
                        None => return,
                    };
                    let kt = match GirType::from_type(&arg_typ) {
                        Some(t) => t,
                        None => return,
                    };
                    let slot_idx = arg_types.len();
                    layout.push(
                        crate::gir::BuiltinSlot::Positional(slot_idx),
                    );
                    arg_types.push(kt);
                    marshal_arg_indices.push(call_idx);
                } else if *has_default {
                    // Find the default expr in the binding's
                    // source argspec.
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
                    layout.push(
                        crate::gir::BuiltinSlot::LabeledDefault(default),
                    );
                } else {
                    // Required labeled arg missing — typecheck
                    // should have caught this.
                    return;
                }
            }
        }
    }
    // Any remaining positional call args are varargs. Allowed only
    // if the FnType declares vargs. Each call-site varg's type is
    // resolved INDIVIDUALLY from its own typed-AST cell, not from
    // the formal `fn_type.vargs` — that formal type can be a union
    // like `[string, Array<string>]` (e.g. `str::concat`), which
    // `GirType::from_type` rejects. The per-arg cell holds the
    // concrete instantiation post-typecheck, so each push uses the
    // right helper for that specific value's runtime shape.
    let remaining: Vec<_> = pos_iter.collect();
    if !remaining.is_empty() {
        if fn_type.vargs.is_none() {
            return; // extra positionals but no vargs declaration
        }
        let from_call_idx = arg_types.len();
        let count = remaining.len();
        layout.push(crate::gir::BuiltinSlot::Variadic {
            from_call_idx,
            count,
        });
        for (call_idx, arg_expr) in remaining {
            let arg_typ = arg_expr.typ.get().cloned().or_else(|| {
                fn_type
                    .vargs
                    .as_ref()
                    .and_then(|t| t.with_deref(|t| t.cloned()))
            });
            let arg_typ = match arg_typ {
                Some(t) => t,
                None => return,
            };
            let kt = match GirType::from_type(&arg_typ) {
                Some(t) => t,
                None => return,
            };
            arg_types.push(kt);
            marshal_arg_indices.push(*call_idx);
        }
    }
    // Reject unused labeled call args (the typechecker should have
    // caught these, but defensive).
    if !call_labeled.is_empty() {
        return;
    }
    // Use the call-site's resolved return type (the Apply Expr's
    // own typ cell). The fn_type's rtype may still be a TVar for
    // generic builtins; the Apply Expr's typ holds the concrete
    // instantiation post-typecheck.
    let ret_typ = apply_expr.typ.get().cloned().or_else(|| {
        fn_type.rtype.with_deref(|t| t.cloned())
    });
    let ret_typ = match ret_typ {
        Some(t) => t,
        None => return,
    };
    let return_type = match GirType::from_type(&ret_typ) {
        Some(t) => t,
        None => return,
    };
    // Skip shapes the JIT's DynCall ABI doesn't accept yet — the
    // JIT codegen would error mid-compile, corrupting the cranelift
    // `func_ctx`. Filter here so the candidate stays unfused and
    // the interpreter runs it normally.
    fn is_dyncall_supported(kt: &GirType) -> bool {
        match kt {
            GirType::Prim(_)
            | GirType::Array(_)
            | GirType::Tuple(_)
            | GirType::Struct(_)
            | GirType::Variant(_)
            | GirType::Nullable(_)
            | GirType::String => true,
            // Bare Unit args don't appear in practice (Unit is a
            // return-only shape). Bare Null is always widened to
            // Nullable<T> by the IR builder before construction.
            GirType::Unit | GirType::Null => false,
        }
    }
    if !arg_types.iter().all(is_dyncall_supported) {
        return;
    }
    let return_ok = match &return_type {
        GirType::Prim(_)
        | GirType::Array(_)
        | GirType::Tuple(_)
        | GirType::Struct(_)
        | GirType::Variant(_)
        | GirType::Nullable(_)
        | GirType::Unit
        | GirType::String => true,
        GirType::Null => false,
    };
    if !return_ok {
        return;
    }
    // Replace the FnType's rtype with the call site's RESOLVED return
    // type (the Apply Expr's typ cell). The function-expression's
    // `typ.get()` we read above carries the LAMBDA's signature with
    // potentially unbound polymorphic TVars (e.g. `str::parse`'s 'b);
    // a few sync builtins like `str::parse` read `resolved.rtype` at
    // init time to decide their target type, so a unbound TVar there
    // surfaces at runtime as "requires a concrete type annotation".
    // The Apply Expr's typ has the post-typecheck monomorphization
    // (e.g. `Result<i64, ParseError>` after `let v: i64 = ...?`), so
    // we splice it in.
    let resolved_fn_type = {
        let mut ft = (*fn_type).clone();
        // Use a TVar-dereferenced copy of the resolved return type —
        // some builtins (notably `str::parse`) match on `rtype` shape
        // at init time (`extract_cast_type`) and don't deref TVars,
        // so a TVar wrapping the resolved Set bypasses their lookup.
        let resolved_rtype =
            ret_typ.with_deref(|t| t.cloned()).unwrap_or(ret_typ);
        ft.rtype = resolved_rtype;
        std::sync::Arc::new(ft)
    };
    let fn_index = out.fn_params.len() as u32;
    out.fn_params.push(crate::gir::FnParam {
        name: info.name.clone(),
        source: crate::gir::FnSource::Builtin {
            name: info.name.clone(),
            typ: resolved_fn_type,
            layout: std::sync::Arc::from(layout),
            lambda_id: info.lambda_id,
        },
        arg_types: arg_types.clone(),
        return_type: return_type.clone(),
    });
    out.apply_sites.insert(
        apply_id,
        BuiltinCallSiteInfo {
            fn_index,
            marshal_arg_indices,
            arg_types,
            return_type,
        },
    );
}

impl FusionCtx {
    pub fn find(&self, name: &str) -> Option<&Input> {
        self.inputs.iter().find(|i| &*i.name == name)
    }

    pub fn find_fn(&self, name: &str) -> Option<&KnownFusedFn> {
        self.known_fns.get(name)
    }

    pub fn find_const(&self, name: &str) -> Option<&KnownConst> {
        self.known_consts.get(name)
    }

    /// Look up an array-typed kernel parameter by Graphix name.
    pub fn find_array(&self, name: &str) -> Option<&crate::gir::ArrayInput> {
        self.array_inputs.iter().find(|a| &*a.name == name)
    }

    /// Look up a tuple-typed kernel parameter by Graphix name.
    pub fn find_tuple(&self, name: &str) -> Option<&crate::gir::TupleInput> {
        self.tuple_inputs.iter().find(|t| &*t.name == name)
    }

    /// Look up a struct-typed kernel parameter by Graphix name.
    pub fn find_struct(&self, name: &str) -> Option<&crate::gir::StructInput> {
        self.struct_inputs.iter().find(|s| &*s.name == name)
    }

    /// Look up a variant-typed kernel parameter by Graphix name.
    pub fn find_variant(&self, name: &str) -> Option<&crate::gir::VariantInput> {
        self.variant_inputs.iter().find(|v| &*v.name == name)
    }

    /// Look up a nullable-typed kernel parameter / let-binding by name.
    pub fn find_nullable(
        &self,
        name: &str,
    ) -> Option<&crate::gir::NullableInput> {
        self.nullable_inputs.iter().find(|n| &*n.name == name)
    }

    /// Look up a string-typed let-binding by name.
    pub fn find_string(
        &self,
        name: &str,
    ) -> Option<&crate::gir::StringInput> {
        self.string_inputs.iter().find(|s| &*s.name == name)
    }

    /// Scoped push of a primitive [`Input`] for the duration of `f`.
    /// The canonical pattern for HOF intercepts whose callback's arg
    /// becomes a kernel input visible only while emitting the
    /// callback body — e.g. `array::map(arr, |x| body)` pushes `x`,
    /// emits `body`, then drops `x` on return.
    ///
    /// Equivalent to (and a one-liner shorthand for) the open-coded
    /// pattern:
    /// ```ignore
    /// let mut inner = ctx.clone();
    /// inner.inputs.push(input);
    /// let r = f(&inner);
    /// // `inner` drops, never observed by callers.
    /// ```
    pub fn with_input<F, T>(&self, input: Input, f: F) -> T
    where
        F: FnOnce(&FusionCtx) -> T,
    {
        let mut inner = self.clone();
        inner.inputs.push(input);
        f(&inner)
    }

    /// If `node` is a `Ref` to an array-typed kernel parameter,
    /// return that parameter's name. Used by HOF intercepts to
    /// confirm the array argument is something they can lower over
    /// (i.e. a kernel input, not an arbitrary expression).
    pub fn resolve_array_input<R: crate::Rt, E: crate::UserEvent>(
        &self,
        node: &crate::Node<R, E>,
    ) -> Option<ArcStr> {
        if let crate::NodeView::Ref(r) = node.view() {
            // The Ref's bind_id corresponds to a name in `ctx.env`,
            // which is what `array_inputs` is keyed by. We don't
            // have direct env access here, so resolve via the spec's
            // ExprKind::Ref name path (every Ref Node's spec is an
            // `ExprKind::Ref { name }`).
            if let crate::expr::ExprKind::Ref { name } = &node.spec().kind {
                let ident = ident_of(name)?;
                let _ = r; // bind_id available if needed later
                if let Some(ai) = self.find_array(ident) {
                    return Some(ai.name.clone());
                }
            }
        }
        None
    }

    /// Look up an input by name in any of the input lists (mirrors the
    /// per-shape dispatch in the `ExprKind::Ref` arm of `emit_expr`).
    /// Used by the maximal-fusion lifted-input path: when a lifted
    /// `ExprId` triggers a synthetic Local read, this resolves the
    /// synth name to the registered input slot.
    pub fn lookup_local(&self, name: &str) -> Option<GirExpr> {
        if let Some(input) = self.find(name) {
            return Some(gir::local(input.name.clone(), input.prim));
        }
        if let Some(ai) = self.find_array(name) {
            return Some(GirExpr {
                op: GirOp::Local(ai.name.clone()),
                typ: GirType::Array(Box::new(ai.elem.clone())),
            });
        }
        if let Some(ti) = self.find_tuple(name) {
            return Some(GirExpr {
                op: GirOp::Local(ti.name.clone()),
                typ: GirType::Tuple(ti.elems.clone()),
            });
        }
        if let Some(si) = self.find_struct(name) {
            return Some(GirExpr {
                op: GirOp::Local(si.name.clone()),
                typ: GirType::Struct(si.fields.clone()),
            });
        }
        if let Some(vi) = self.find_variant(name) {
            return Some(GirExpr {
                op: GirOp::Local(vi.name.clone()),
                typ: GirType::Variant(vi.cases.clone()),
            });
        }
        if let Some(ni) = self.find_nullable(name) {
            return Some(GirExpr {
                op: GirOp::Local(ni.name.clone()),
                typ: GirType::Nullable(Box::new(ni.elem.clone())),
            });
        }
        if let Some(si) = self.find_string(name) {
            return Some(GirExpr {
                op: GirOp::Local(si.name.clone()),
                typ: GirType::String,
            });
        }
        None
    }

    /// Look up a fn-typed kernel parameter by Graphix name and call
    /// arity, returning its zero-based index in `fn_inputs` (the
    /// `fn_index` for emitted `GirOp::DynCall`) plus the param itself.
    /// Variadic builtins produce one `FnParam` per *call arity* —
    /// `sum(a, b)` and `sum(a, b, c)` register as two separate slots,
    /// so arity must match too. For non-variadic callees, only one
    /// arity is ever registered and the check is degenerate.
    pub fn find_fn_input(
        &self,
        name: &str,
        arity: usize,
    ) -> Option<(u32, &crate::gir::FnParam)> {
        self.fn_inputs
            .iter()
            .enumerate()
            .find(|(_, fp)| {
                fp.name.as_str() == name && fp.arg_types.len() == arity
            })
            .map(|(i, fp)| (i as u32, fp))
    }
}

/// Return the last path segment of an `Apply { function: Ref(path) }`,
/// or `None` if the function isn't a `Ref`. Unlike [`ident_of`], this
/// accepts multi-segment paths (`array::len`, `str::contains`, …) and
/// returns just the last segment. Used to pattern-match stdlib
/// builtin call sites after module resolution has run.
fn trailing_segment(function: &Expr) -> Option<&str> {
    match &function.kind {
        ExprKind::Ref { name } => {
            netidx::path::Path::basename(name.0.as_ref())
        }
        _ => None,
    }
}

fn ident_of(path: &ModPath) -> Option<&str> {
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

/// Emit a `select` at expression position as a [`GirOp::IfChain`].
/// Every arm body must itself emit as a primitive expression, and
/// every arm body must have the same primitive type (since the chain
/// evaluates to a single value).
fn emit_select_as_expr<R: crate::Rt, E: crate::UserEvent>(
    sel: &crate::node::select::Select<R, E>,
    ctx: &FusionCtx,
) -> Option<GirExpr> {
    // Source-level patterns live on the spec; arm bodies (Nodes)
    // live in the Select Node's `arms` vec, in source order.
    let spec_select = match &sel.spec.kind {
        ExprKind::Select(se) => se,
        _ => return None,
    };
    let scrut = emit_node(&sel.arg.node, ctx)?;
    let n = sel.arms.len();
    if n == 0 {
        return None;
    }
    if spec_select.arms.len() != n {
        return None;
    }
    let mut arms: Vec<(Option<GirExpr>, GirExpr)> = Vec::with_capacity(n);
    let mut unified_typ: Option<GirType> = None;
    for (i, ((pat, _), (_pat_node, body_cached))) in
        spec_select.arms.iter().zip(sel.arms.iter()).enumerate()
    {
        let is_last = i == n - 1;
        let mut arm_ctx = ctx.clone();
        let type_cond = emit_type_predicate_cond(&scrut, &pat.type_predicate)?;
        let struct_cond =
            emit_arm_condition(&scrut, &pat.structure_predicate, &mut arm_ctx)?;
        let cond = combine_cond_and_guard(type_cond, struct_cond);
        let guard_kir = match &pat.guard {
            None => None,
            Some(_g) => {
                // Pattern guards (`if expr`) live on the source
                // Pattern as raw Expr. No compiled Node form is
                // wired through yet — bail until that's threaded.
                return None;
            }
        };
        let combined = combine_cond_and_guard(cond, guard_kir);
        let body_kir = emit_node(&body_cached.node, &arm_ctx)?;
        unified_typ = Some(match unified_typ {
            Some(prev) => unify_arm_types(prev, body_kir.typ.clone())?,
            None => body_kir.typ.clone(),
        });
        match (i, &combined) {
            (0, None) if is_last => arms.push((None, body_kir)),
            (_, _) if !is_last && combined.is_none() => {
                arms.push((None, body_kir));
            }
            _ => arms.push((combined, body_kir)),
        }
    }
    let typ = unified_typ?;
    Some(GirExpr { op: GirOp::IfChain { arms }, typ })
}

/// Compute the IfChain merge type when arms produced different
/// `GirType`s. Currently the only narrowing this understands is the
/// `[T, null]` widening — `Prim(T) ∪ Null → Nullable(Prim(T))`,
/// `Nullable<U> ∪ Null → Nullable<U>`, `Nullable<U> ∪ Prim(T) →
/// Nullable<U>` when `T == U`. Anything else returns `None` (the
/// caller bails on fusion).
fn unify_arm_types(a: GirType, b: GirType) -> Option<GirType> {
    if a == b {
        return Some(a);
    }
    let nullable_of = |t: GirType| GirType::Nullable(Box::new(t));
    match (a, b) {
        (GirType::Null, GirType::Null) => Some(GirType::Null),
        (GirType::Null, other) | (other, GirType::Null) => match other {
            GirType::Nullable(_) => Some(other),
            t @ (GirType::Prim(_)
            | GirType::Array(_)
            | GirType::Tuple(_)
            | GirType::Struct(_)
            | GirType::Variant(_)) => Some(nullable_of(t)),
            _ => None,
        },
        (GirType::Nullable(inner), other) | (other, GirType::Nullable(inner)) => {
            if *inner == other {
                Some(GirType::Nullable(inner))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Compose a structure-predicate condition (which may be `None` =
/// "always matches") with an optional guard expression into a single
/// optional bool [`GirExpr`].
fn combine_cond_and_guard(
    cond: Option<GirExpr>,
    guard: Option<GirExpr>,
) -> Option<GirExpr> {
    match (cond, guard) {
        (None, None) => None,
        (Some(c), None) => Some(c),
        (None, Some(g)) => Some(g),
        (Some(c), Some(g)) => gir::bool_op(c, g, gir::BoolOp::And),
    }
}

/// Emit an `Apply` call whose target is a Ref to a function already in
/// `ctx.known_fns`. Lowers to a [`GirOp::Call`].
///
/// Builtin DynCall sites take priority — if the Apply's spec.id is
/// registered in `ctx.builtin_apply_sites` (populated by
/// `discover_builtin_calls` before kernel-body emission), emit a
/// `GirOp::DynCall` against the matching `FnSource::Builtin` slot
/// regardless of label shape. This is the primary path for stdlib
/// function calls inside a fused kernel; the bare-name `ctx.known_fns`
/// path below handles cross-kernel `Call` references and the
/// hand-written `array::len` / `array::map` etc. intercepts further
/// down handle the small set of stdlib HOFs we inline directly.
fn emit_known_fused_call<R: crate::Rt, E: crate::UserEvent>(
    cs: &crate::node::callsite::CallSite<R, E>,
    ctx: &FusionCtx,
) -> Option<GirExpr> {
    let apply_id = cs.spec().id;
    let spec_apply = match &cs.spec().kind {
        ExprKind::Apply(a) => a,
        _ => return None,
    };
    // Builtin DynCall path.
    if let Some(info) = ctx.builtin_apply_sites.get(&apply_id) {
        let info = info.clone();
        let mut args = Vec::with_capacity(info.marshal_arg_indices.len());
        for (slot_idx, &call_idx) in info.marshal_arg_indices.iter().enumerate() {
            let arg_node = cs.arg_positional(call_idx)?;
            let e = emit_node(arg_node, ctx)?;
            if e.typ != info.arg_types[slot_idx] {
                return None;
            }
            args.push(e);
        }
        return Some(GirExpr {
            op: GirOp::DynCall {
                fn_index: info.fn_index,
                args,
                arg_types: info.arg_types.clone(),
                return_type: info.return_type.clone(),
            },
            typ: info.return_type,
        });
    }
    if spec_apply.args.iter().any(|(label, _)| label.is_some()) {
        return None;
    }
    // Stdlib `array::len(arr)` special case stays — small, no
    // builtin runtime needed since it's pure GIR. Other arr::* are
    // gone (handled by FusedBuiltin path).
    let trailing = trailing_segment(&spec_apply.function);
    if trailing == Some("len") && spec_apply.args.len() == 1 {
        let arr_node = cs.arg_positional(0)?;
        if let Some(arr_name) = ctx.resolve_array_input(arr_node) {
            return Some(GirExpr {
                op: GirOp::ArrayLen { name: arr_name },
                typ: GirType::Prim(PrimType::U64),
            });
        }
    }
    // Dispatch via fnode's name. Node-based: read the Ref Node's
    // spec for the source name.
    let name = match &cs.fnode().spec().kind {
        ExprKind::Ref { name } => ident_of(name)?,
        _ => return None,
    };
    // Prefer a DynCall against a fn-typed kernel parameter (HOF arg).
    if let Some((fn_index, fp)) = ctx.find_fn_input(name, spec_apply.args.len()) {
        let mut kargs = Vec::with_capacity(spec_apply.args.len());
        for (i, expected) in fp.arg_types.iter().enumerate() {
            let arg_node = cs.arg_positional(i)?;
            let e = emit_node(arg_node, ctx)?;
            if &e.typ != expected {
                return None;
            }
            kargs.push(e);
        }
        let return_type = fp.return_type.clone();
        return Some(GirExpr {
            op: GirOp::DynCall {
                fn_index,
                args: kargs,
                arg_types: fp.arg_types.clone(),
                return_type: return_type.clone(),
            },
            typ: return_type,
        });
    }
    // Static call to a previously-fused function.
    let fn_info = ctx.find_fn(name)?.clone();
    if spec_apply.args.len() != fn_info.arg_types.len() {
        return None;
    }
    let mut kargs = Vec::with_capacity(spec_apply.args.len());
    for (i, expected) in fn_info.arg_types.iter().enumerate() {
        let arg_node = cs.arg_positional(i)?;
        let e = emit_node(arg_node, ctx)?;
        if e.typ != *expected {
            return None;
        }
        kargs.push(e);
    }
    Some(GirExpr {
        op: GirOp::Call { fn_name: ArcStr::from(name), args: kargs },
        typ: fn_info.return_type,
    })
}

/// If `expr` is a `Ref` to an array-typed kernel parameter, return
/// its name (suitable for `GirOp::ArrayLen` / `GirOp::ArrayGet`).
/// Anything else (computed array values, intra-kernel let-bound
/// arrays) is rejected — composability with array producers waits
/// for M7.4's `GirType::Array` plumbing.


/// Lower a `` `Tag(p0, p1) `` variant literal to
/// [`GirOp::VariantNew`]. Each payload must emit as scalar GIR; the
/// per-slot primitive types come from those scalar results.
fn emit_variant_new<R: crate::Rt, E: crate::UserEvent>(
    tag: &ArcStr,
    args: &[crate::node::Cached<R, E>],
    ctx: &FusionCtx,
) -> Option<GirExpr> {
    let mut payloads: Vec<GirExpr> = Vec::with_capacity(args.len());
    let mut payload_types: Vec<GirType> = Vec::with_capacity(args.len());
    for a in args {
        let e = emit_node(&a.node, ctx)?;
        if matches!(e.typ, GirType::Unit | GirType::Null) {
            return None;
        }
        payload_types.push(e.typ.clone());
        payloads.push(e);
    }
    let typ = GirType::Variant(vec![(tag.clone(), payload_types.clone())]);
    Some(GirExpr {
        op: GirOp::VariantNew {
            tag: tag.clone(),
            payloads,
            payload_types,
        },
        typ,
    })
}

/// Lower `tup.<idx>` (i.e. `ExprKind::TupleRef`) to `GirOp::TupleGet`.
/// Source must be a `Ref` to a tuple kernel param; `idx` must be in
/// range. Result type is the tuple slot's primitive type.
fn emit_tuple_ref<R: crate::Rt, E: crate::UserEvent>(
    source: &crate::Node<R, E>,
    idx: usize,
    ctx: &FusionCtx,
) -> Option<GirExpr> {
    // Pull the kernel-input name from the source Node's spec — the
    // Node walker has a BindId, but kernel inputs key by source-level
    // name, so we read `ExprKind::Ref { name }` off the spec for the
    // identifier.
    let name = match &source.spec().kind {
        ExprKind::Ref { name } => ident_of(name)?,
        _ => return None,
    };
    let ti = ctx.find_tuple(name)?;
    let elem_typ = ti.elems.get(idx)?.clone();
    Some(GirExpr {
        op: GirOp::TupleGet {
            name: ti.name.clone(),
            idx,
            elem_typ: elem_typ.clone(),
        },
        typ: elem_typ,
    })
}

/// Lower `s.field` (i.e. `ExprKind::StructRef`) to `GirOp::StructGet`.
/// Source must be a `Ref` to a struct kernel param; `field` must
/// resolve to a known field. Result type is that field's primitive type.
fn emit_struct_ref<R: crate::Rt, E: crate::UserEvent>(
    source: &crate::Node<R, E>,
    field: &ArcStr,
    ctx: &FusionCtx,
) -> Option<GirExpr> {
    let name = match &source.spec().kind {
        ExprKind::Ref { name } => ident_of(name)?,
        _ => return None,
    };
    let si = ctx.find_struct(name)?;
    let sorted_idx = si.fields.iter().position(|(n, _)| n == field)?;
    let elem_typ = si.fields[sorted_idx].1.clone();
    Some(GirExpr {
        op: GirOp::StructGet {
            name: si.name.clone(),
            field: field.clone(),
            sorted_idx,
            elem_typ: elem_typ.clone(),
        },
        typ: elem_typ,
    })
}

/// Lower a `(a, b, c)` tuple literal to `GirOp::TupleNew`. Each
/// field must emit as scalar GIR. Result type is
/// `GirType::Tuple(<per-slot prim types>)`.
fn emit_tuple_new<R: crate::Rt, E: crate::UserEvent>(
    args: &[crate::node::Cached<R, E>],
    ctx: &FusionCtx,
) -> Option<GirExpr> {
    let mut fields: Vec<GirExpr> = Vec::with_capacity(args.len());
    let mut elem_types: Vec<GirType> = Vec::with_capacity(args.len());
    for a in args {
        let e = emit_node(&a.node, ctx)?;
        if matches!(e.typ, GirType::Unit | GirType::Null) {
            return None;
        }
        elem_types.push(e.typ.clone());
        fields.push(e);
    }
    let typ = GirType::Tuple(elem_types.clone());
    Some(GirExpr {
        op: GirOp::TupleNew { fields, elem_types },
        typ,
    })
}

/// Lower `[a, b, c]` to a TupleNew op tagged as `GirType::Array(elem)`.
/// The runtime shape (a `ValArray<Value>`) is identical to TupleNew;
/// only the outer GirType differs, so we reuse the TupleNew producer
/// op instead of carrying a parallel ArrayNew variant through every
/// IR walker. The element GirType is read from the expression's
/// typed-AST cell (`Type::Array(inner)`) so empty literals still
/// have a concrete elem type.
fn emit_array_new<R: crate::Rt, E: crate::UserEvent>(
    array_node: &crate::Node<R, E>,
    args: &[crate::node::Cached<R, E>],
    ctx: &FusionCtx,
) -> Option<GirExpr> {
    let arr_typ = array_node.typ();
    let elem_typ = arr_typ.with_deref(|resolved| match resolved? {
        crate::typ::Type::Array(inner) => GirType::from_type(inner),
        _ => None,
    })?;
    if matches!(elem_typ, GirType::Unit | GirType::Null) {
        return None;
    }
    let mut fields: Vec<GirExpr> = Vec::with_capacity(args.len());
    let mut elem_types: Vec<GirType> = Vec::with_capacity(args.len());
    for a in args {
        let e = emit_node(&a.node, ctx)?;
        if e.typ != elem_typ {
            return None;
        }
        elem_types.push(elem_typ.clone());
        fields.push(e);
    }
    Some(GirExpr {
        op: GirOp::TupleNew { fields, elem_types },
        typ: GirType::Array(Box::new(elem_typ)),
    })
}

/// Lower a `{x: a, y: b}` struct literal to `GirOp::StructNew`.
/// Sorts the fields alphabetically by name (graphix's canonical
/// struct layout). Each field value must emit as scalar GIR.
/// Lower a `{x: a, y: b}` struct literal — takes the field-name array
/// alongside the field-value Cached array (parallel; NodeView::Struct
/// exposes them as `s.names` + `s.n`).
fn emit_struct_new<R: crate::Rt, E: crate::UserEvent>(
    names: &[ArcStr],
    values: &[crate::node::Cached<R, E>],
    ctx: &FusionCtx,
) -> Option<GirExpr> {
    if names.len() != values.len() {
        return None;
    }
    // Sort alphabetically by name to match graphix's canonical
    // ValArray layout.
    let mut indexed: Vec<(ArcStr, &crate::node::Cached<R, E>)> =
        names.iter().cloned().zip(values.iter()).collect();
    indexed.sort_by(|a, b| a.0.cmp(&b.0));
    let mut sorted_fields: Vec<(ArcStr, GirExpr)> =
        Vec::with_capacity(indexed.len());
    let mut sorted_types: Vec<(ArcStr, GirType)> =
        Vec::with_capacity(indexed.len());
    for (n, c) in indexed {
        let gir = emit_node(&c.node, ctx)?;
        if matches!(gir.typ, GirType::Unit | GirType::Null) {
            return None;
        }
        sorted_types.push((n.clone(), gir.typ.clone()));
        sorted_fields.push((n, gir));
    }
    let typ = GirType::Struct(sorted_types.clone());
    Some(GirExpr {
        op: GirOp::StructNew { sorted_fields, sorted_types },
        typ,
    })
}

/// Lower `arr[i]` (i.e. `ExprKind::ArrayRef`) to `GirOp::ArrayGet`.
/// `arr` must be a Ref to an array kernel param; `i` must emit as a
/// scalar integer expression. Result type is the array's element
/// type.
fn emit_array_ref<R: crate::Rt, E: crate::UserEvent>(
    source: &crate::Node<R, E>,
    idx: &crate::Node<R, E>,
    ctx: &FusionCtx,
) -> Option<GirExpr> {
    let arr_name = ctx.resolve_array_input(source)?;
    let ai = ctx.find_array(&arr_name)?;
    let elem = ai.elem.clone();
    let idx_expr = emit_node(idx, ctx)?;
    if !idx_expr.typ.as_prim().is_some_and(|p| p.is_integer()) {
        return None;
    }
    Some(GirExpr {
        op: GirOp::ArrayGet { name: arr_name, idx: Box::new(idx_expr) },
        typ: elem,
    })
}

/// Emit a Graphix `{ let x = ...; let y = ...; body }` block as a
/// [`GirOp::Block`]. Each non-last statement must be a `let`-binding
/// whose value emits as a primitive expression; the final statement
/// provides the block's value.
fn emit_do_as_expr<R: crate::Rt, E: crate::UserEvent>(
    children: &[crate::Node<R, E>],
    ctx: &FusionCtx,
) -> Option<GirExpr> {
    if children.is_empty() {
        return None;
    }
    let mut local_ctx = ctx.clone();
    let mut lets: Vec<Let> = Vec::new();
    let last = children.len() - 1;
    for (i, child) in children.iter().enumerate() {
        if i == last {
            let body = emit_node(child, &local_ctx)?;
            let typ = body.typ.clone();
            return Some(GirExpr {
                op: GirOp::Block { lets, tail: Box::new(body) },
                typ,
            });
        }
        use crate::NodeView;
        match child.view() {
            NodeView::Bind(bind) => {
                // Source-level info comes from the Bind Node's spec.
                let b = match &bind.spec().kind {
                    ExprKind::Bind(b) => b,
                    _ => return None,
                };
                if b.rec {
                    return None;
                }
                let name = b.pattern.single_bind()?;
                let value = emit_node(&bind.node, &local_ctx)?;
                register_kir_binding(&mut local_ctx, name, &value.typ)?;
                lets.push(Let { local: name.clone(), value });
            }
            NodeView::Nop(_) => {}
            _ => return None,
        }
    }
    None
}

/// Register a kernel-local binding in `ctx` by routing it into the
/// right slot list based on its GIR type. Used by every kernel-let
/// emission site (`emit_do`'s let arm, `emit_do_as_expr`'s block-let
/// arm, etc.) so a new `GirType` variant only needs a new arm here.
///
/// Returns `None` for GirType variants we can't represent as a kernel
/// local — currently `Unit` (caller usually has already routed Unit
/// values to `GirStmt::Discard`) and `String` (no string_inputs slot
/// list yet). The caller short-circuits its parent on `None`.
fn register_kir_binding(
    ctx: &mut FusionCtx,
    name: &ArcStr,
    value_typ: &GirType,
) -> Option<()> {
    match value_typ {
        GirType::Prim(prim) => {
            ctx.inputs.push(Input {
                name: name.clone(),
                prim: *prim,
                bind_id: None,
            });
        }
        GirType::Array(elem) => {
            ctx.array_inputs.push(crate::gir::ArrayInput {
                name: name.clone(),
                elem: (**elem).clone(),
                bind_id: None,
            });
        }
        GirType::Tuple(elems) => {
            ctx.tuple_inputs.push(crate::gir::TupleInput {
                name: name.clone(),
                elems: elems.clone(),
                bind_id: None,
            });
        }
        GirType::Struct(fields) => {
            ctx.struct_inputs.push(crate::gir::StructInput {
                name: name.clone(),
                fields: fields.clone(),
                bind_id: None,
            });
        }
        GirType::Variant(cases) => {
            ctx.variant_inputs.push(crate::gir::VariantInput {
                name: name.clone(),
                cases: cases.clone(),
                bind_id: None,
            });
        }
        GirType::Nullable(elem) => {
            ctx.nullable_inputs.push(crate::gir::NullableInput {
                name: name.clone(),
                elem: (**elem).clone(),
                bind_id: None,
            });
        }
        GirType::String => {
            ctx.string_inputs.push(crate::gir::StringInput {
                name: name.clone(),
                bind_id: None,
            });
        }
        // Unit isn't usefully bindable as a kernel local (the caller
        // has already routed it to `GirStmt::Discard`). Bare `Null`
        // doesn't bind either (it would always be widened to
        // `Nullable<T>` first at the construction site).
        GirType::Unit | GirType::Null => return None,
    }
    Some(())
}

/// Node-based public entry point. Walks the compiled Node tree
/// (the decorated AST) via [`crate::NodeView`]. Today this is a
/// thin wrapper around the Expr-based [`emit_expr`] — Node access
/// adds value mainly at [`crate::NodeView::CallSite`] for resolved
/// FnType lookup, which is already handled out-of-band via the
/// pre-populated `ctx.builtin_apply_sites` map (keyed by the
/// Apply's `ExprId`). The internal recursion stays Expr-based.
///
/// Use this when you have a compiled `Node` in hand — `fusion::fuse`,
/// `build_region`, and tests that drive through the full pipeline.
/// Use [`emit_expr`] directly when you only have an Expr (e.g.
/// inline literals built by hand in tests).
pub fn emit_expr_node<R: crate::Rt, E: crate::UserEvent>(
    node: &crate::Node<R, E>,
    ctx: &FusionCtx,
) -> Option<GirExpr> {
    emit_node(node, ctx)
}

/// Emit a sub-expression as a [`GirExpr`]. Returns `None` if any sub-
/// tree is something the emitter doesn't handle yet — that short-
/// circuits the whole parent, so the caller falls back to the
/// interpreted path (or, in AOT mode, refuses fusion).
/// Node-based emit dispatch. Walks the compiled Node tree via
/// `NodeView` rather than the source Expr's `ExprKind`. This is the
/// canonical fusion entry point — `emit_expr_node` is a thin wrapper.
///
/// During the migration from `emit_expr` (Expr-based), child accesses
/// that don't yet have a Node-native helper fall back to walking via
/// `node.spec()` — temporary, removed as each helper is converted.
pub fn emit_node<R: crate::Rt, E: crate::UserEvent>(
    node: &crate::Node<R, E>,
    ctx: &FusionCtx,
) -> Option<GirExpr> {
    // Lifted-input intercept reads the source ExprId, identical to
    // the Expr-based path.
    if let Some(name) = ctx.lifted_inputs.get(&node.spec().id) {
        return ctx.lookup_local(name);
    }
    use crate::NodeView;
    match node.view() {
        // Arithmetic: lhs / rhs are `Cached<R, E>` — Node access via
        // `.node`.
        NodeView::Add(a) => {
            gir::arith(emit_node(&a.lhs.node, ctx)?, emit_node(&a.rhs.node, ctx)?, gir::BinOp::Add)
        }
        NodeView::Sub(a) => {
            gir::arith(emit_node(&a.lhs.node, ctx)?, emit_node(&a.rhs.node, ctx)?, gir::BinOp::Sub)
        }
        NodeView::Mul(a) => {
            gir::arith(emit_node(&a.lhs.node, ctx)?, emit_node(&a.rhs.node, ctx)?, gir::BinOp::Mul)
        }
        NodeView::Div(a) => {
            gir::arith(emit_node(&a.lhs.node, ctx)?, emit_node(&a.rhs.node, ctx)?, gir::BinOp::Div)
        }
        NodeView::Mod(a) => {
            gir::arith(emit_node(&a.lhs.node, ctx)?, emit_node(&a.rhs.node, ctx)?, gir::BinOp::Mod)
        }
        // Comparison
        NodeView::Eq(o) => {
            gir::cmp(emit_node(&o.lhs.node, ctx)?, emit_node(&o.rhs.node, ctx)?, gir::CmpOp::Eq)
        }
        NodeView::Ne(o) => {
            gir::cmp(emit_node(&o.lhs.node, ctx)?, emit_node(&o.rhs.node, ctx)?, gir::CmpOp::Ne)
        }
        NodeView::Lt(o) => {
            gir::cmp(emit_node(&o.lhs.node, ctx)?, emit_node(&o.rhs.node, ctx)?, gir::CmpOp::Lt)
        }
        NodeView::Gt(o) => {
            gir::cmp(emit_node(&o.lhs.node, ctx)?, emit_node(&o.rhs.node, ctx)?, gir::CmpOp::Gt)
        }
        NodeView::Lte(o) => {
            gir::cmp(emit_node(&o.lhs.node, ctx)?, emit_node(&o.rhs.node, ctx)?, gir::CmpOp::Lte)
        }
        NodeView::Gte(o) => {
            gir::cmp(emit_node(&o.lhs.node, ctx)?, emit_node(&o.rhs.node, ctx)?, gir::CmpOp::Gte)
        }
        NodeView::And(o) => gir::bool_op(
            emit_node(&o.lhs.node, ctx)?,
            emit_node(&o.rhs.node, ctx)?,
            gir::BoolOp::And,
        ),
        NodeView::Or(o) => gir::bool_op(
            emit_node(&o.lhs.node, ctx)?,
            emit_node(&o.rhs.node, ctx)?,
            gir::BoolOp::Or,
        ),
        NodeView::Not(n) => gir::not(emit_node(&n.n, ctx)?),
        NodeView::ExplicitParens(ep) => emit_node(&ep.n, ctx),
        NodeView::Qop(q) => {
            let lowered = emit_node(&q.n, ctx)?;
            wrap_qop(lowered)
        }
        NodeView::OrNever(o) => {
            let lowered = emit_node(&o.n, ctx)?;
            wrap_qop(lowered)
        }
        NodeView::TypeCast(tc) => {
            let target = PrimType::from_type(&tc.target)?;
            gir::cast(emit_node(&tc.n, ctx)?, target)
        }
        // Apply arm: try ApplyView::FusedBuiltin dispatch FIRST.
        // FusedBuiltin (e.g. MapQ/FoldQ) returns Some via its
        // `GirEmitter::emit_gir` when the callback's analysis_pred
        // is populated. Falls through to the Expr-based legacy
        // `emit_known_fused_call` for everything else (Init's
        // `array::init` special case, builtin DynCall sites,
        // fn-typed kernel-param DynCalls, static fused-fn Call).
        NodeView::CallSite(cs) => {
            if let Some(crate::ApplyView::FusedBuiltin(em)) = cs.resolved_apply() {
                let mut cloned = ctx.clone();
                if let Some(e) = em.emit_gir(cs, &[], &[], &mut cloned) {
                    return Some(e);
                }
            }
            emit_known_fused_call(cs, ctx)
        }
        NodeView::Constant(c) => {
            if let Value::String(s) = &c.value {
                return Some(GirExpr {
                    op: GirOp::ConstStr(s.clone()),
                    typ: GirType::String,
                });
            }
            if matches!(&c.value, Value::Null) {
                return Some(GirExpr {
                    op: GirOp::ConstNull,
                    typ: GirType::Null,
                });
            }
            let c = ConstVal::from_value(&c.value)?;
            Some(gir::const_expr(c))
        }
        NodeView::Ref(r) => {
            // Get the source name from the Ref Node's spec.
            let name = match &r.spec.kind {
                ExprKind::Ref { name } => name,
                _ => return None,
            };
            if let Some(ident) = ident_of(name) {
                if let Some(expr) = ctx.lookup_local(ident) {
                    return Some(expr);
                }
                if let Some(c) = ctx.find_const(ident) {
                    return Some(c.expr.clone());
                }
                let _ = r.id;
                return None;
            }
            let base = netidx::path::Path::basename(name.0.as_ref())?;
            ctx.lookup_local(base)
        }
        NodeView::StringInterpolate(si) => {
            let mut parts = Vec::with_capacity(si.args.len());
            for a in si.args.iter() {
                parts.push(emit_node(&a.node, ctx)?);
            }
            Some(GirExpr {
                op: GirOp::Concat(parts),
                typ: GirType::String,
            })
        }
        NodeView::Select(s) => emit_select_as_expr(s, ctx),
        NodeView::Block(b) => emit_do_as_expr(&b.children, ctx),
        NodeView::ArrayRef(ar) => emit_array_ref(&ar.source.node, &ar.i.node, ctx),
        NodeView::TupleRef(tr) => emit_tuple_ref(&tr.source, tr.field, ctx),
        NodeView::StructRef(sr) => emit_struct_ref(&sr.source, &sr.field_name, ctx),
        NodeView::Tuple(t) => emit_tuple_new(&t.n, ctx),
        NodeView::Array(a) => emit_array_new(node, &a.n, ctx),
        NodeView::Struct(s) => emit_struct_new(&s.names, &s.n, ctx),
        NodeView::Variant(v) => emit_variant_new(&v.tag, &v.n, ctx),
        // Lambda, Bind (at non-statement position), checked
        // arithmetic, Sample, anything reactive — abort fusion.
        _ => None,
    }
}

/// `?` and `$` lowering — both unwrap a Nullable<T> to T (else
/// pass-through the value unchanged for non-Nullable). Extracted as
/// a helper for shared use between Node-based and Expr-based paths.
fn wrap_qop(lowered: GirExpr) -> Option<GirExpr> {
    match &lowered.typ {
        GirType::Nullable(elem) => {
            let success_typ = (**elem).clone();
            Some(GirExpr {
                op: GirOp::QopUnwrap {
                    inner: Box::new(lowered),
                    success_typ: success_typ.clone(),
                },
                typ: success_typ,
            })
        }
        _ => Some(lowered),
    }
}


// ─── Body-position emitters ──────────────────────────────────────

/// Information about a self-recursive function, used by the body
/// emitter to detect tail calls and lower them to a [`GirStmt::TailCall`].
///
/// When `self_info` is `Some`, the kernel emitter wraps the body in a
/// `loop { ... }` and every tail call updates the loop variables and
/// continues. When it is `None`, tail positions emit a [`GirStmt::Return`].
#[derive(Debug, Clone)]
pub struct SelfInfo {
    /// The graphix name being bound to the lambda (e.g. "iterate").
    pub name: ArcStr,
    /// Scalar params in source order. Subset of the kernel's full
    /// argspec — composite params (array/tuple/struct) appear in
    /// the sibling lists below.
    pub params: Vec<Input>,
    /// Source-order full argspec for tail-call validation: one
    /// entry per lambda arg, recording the param's `GirType` so the
    /// validator can typecheck the new value.
    pub source_args: Vec<SelfArg>,
}

#[derive(Debug, Clone)]
pub struct SelfArg {
    pub name: ArcStr,
    pub typ: GirType,
}

/// Node-based public entry to [`emit_body`]. Walks the compiled
/// Node tree (the decorated AST) via [`crate::NodeView`]. Today
/// this is a thin wrapper around the Expr-based [`emit_body`] —
/// internal recursion stays Expr-based for now since Lambda Nodes
/// don't expose compiled body Nodes (the body is compiled lazily
/// per call-site inside InitFn) and HOF inlining still descends
/// into anonymous lambda Exprs via `node.spec()`.
pub fn emit_body_node<R: crate::Rt, E: crate::UserEvent>(
    node: &crate::Node<R, E>,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<Vec<GirStmt>> {
    emit_body(node, ctx, self_info)
}

/// Emit a sequence of [`GirStmt`]s evaluating `expr` as a function
/// body. Handles pure expressions (lowered to `Return`), self-tail
/// calls (lowered to `TailCall`), `select` over primitive scrutinees,
/// and `let`-style bindings.
///
/// Returns `None` if any sub-expression isn't in the supported subset.
pub fn emit_body<R: crate::Rt, E: crate::UserEvent>(
    node: &crate::Node<R, E>,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<Vec<GirStmt>> {
    let mut out = Vec::new();
    emit_body_into(&mut out, node, ctx, self_info)?;
    Some(out)
}

fn emit_body_into<R: crate::Rt, E: crate::UserEvent>(
    out: &mut Vec<GirStmt>,
    node: &crate::Node<R, E>,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<()> {
    use crate::NodeView;
    match node.view() {
        NodeView::Block(b) => emit_do(out, &b.children, ctx, self_info),
        NodeView::ExplicitParens(ep) => emit_body_into(out, &ep.n, ctx, self_info),
        NodeView::Select(s) => emit_select(out, s, ctx, self_info),
        _ => emit_tail(out, node, ctx, self_info),
    }
}

/// Emit a Do block as a sequence of body statements: each non-last
/// node becomes a `Let` (or skipped NoOp); the last is the tail.
fn emit_do<R: crate::Rt, E: crate::UserEvent>(
    out: &mut Vec<GirStmt>,
    children: &[crate::Node<R, E>],
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<()> {
    if children.is_empty() {
        return None;
    }
    let mut local_ctx = ctx.clone();
    let last = children.len() - 1;
    use crate::NodeView;
    for (i, child) in children.iter().enumerate() {
        if i == last {
            emit_body_into(out, child, &local_ctx, self_info)?;
        } else {
            match child.view() {
                NodeView::Bind(b) => emit_bind_stmt(out, b, &mut local_ctx)?,
                NodeView::Nop(_) => {}
                _ => {
                    let value = emit_node(child, &local_ctx)?;
                    if matches!(value.typ, GirType::Unit) {
                        out.push(GirStmt::Discard(value));
                    } else {
                        let discard_name = ArcStr::from(format!(
                            "__discard_{}",
                            out.len()
                        ));
                        out.push(GirStmt::Let(Let {
                            local: discard_name,
                            value,
                        }));
                    }
                }
            }
        }
    }
    Some(())
}

/// Emit a `let`-style binding as a [`GirStmt::Let`] and extend the
/// ctx so later emissions can see the new input.
fn emit_bind_stmt<R: crate::Rt, E: crate::UserEvent>(
    out: &mut Vec<GirStmt>,
    bind: &crate::node::bind::Bind<R, E>,
    ctx: &mut FusionCtx,
) -> Option<()> {
    let b = match &bind.spec().kind {
        ExprKind::Bind(b) => b,
        _ => return None,
    };
    if b.rec {
        return None;
    }
    let name = b.pattern.single_bind()?;
    let value = emit_node(&bind.node, ctx)?;
    // Route the let to the right slot list based on the value's GIR
    // type. The emitted `GirStmt::Let` is the same shape regardless
    // — the Rust emitter renders `let mut <name> = <expr>;` either
    // way, and `<expr>`'s type drives whether `<name>` ends up as a
    // scalar local or a ValArray local in the generated body.
    //
    // Scalar value → scalar `Input` in `ctx.inputs`. Downstream
    // `Ref(name)` lowers via `local(...)`.
    //
    // `Array<P>` value → `ArrayInput` in `ctx.array_inputs`.
    // Downstream `name[i]` / `array::len(name)` /
    // `array::fold(name, ...)` etc. resolve `name` via
    // `find_array` just like a kernel array param. This is what
    // makes a single fused kernel able to compose
    // `let products = array::init(...); array::fold(products, ...)`.
    //
    // `Tuple` / `Struct` values land in the matching slot list, so
    // `name.0` / `name.field` accesses inside the body lower
    // through the existing TupleGet/StructGet path.
    // A Unit-typed value (e.g. `let _ = println(...)` or an
    // implicit discard-let synthesized by `emit_do` for a sync
    // side-effect call) doesn't get bound — it has no consumable
    // value. Emit a `GirStmt::Discard` and don't push any input
    // slot. Subsequent code can't reference `name` usefully (its
    // type is Unit, no ops accept it), so leaving it un-registered
    // is correct.
    if matches!(value.typ, GirType::Unit) {
        out.push(GirStmt::Discard(value));
        return Some(());
    }
    register_kir_binding(ctx, name, &value.typ)?;
    out.push(GirStmt::Let(Let { local: name.clone(), value }));
    Some(())
}

/// Emit a `select` expression as a [`GirStmt::Select`]. Each arm body
/// is its own sub-body that either ends in a return or a tail call (or
/// flows into nested control flow).
fn emit_select<R: crate::Rt, E: crate::UserEvent>(
    out: &mut Vec<GirStmt>,
    sel: &crate::node::select::Select<R, E>,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<()> {
    let spec_select = match &sel.spec.kind {
        ExprKind::Select(se) => se,
        _ => return None,
    };
    let scrut = emit_node(&sel.arg.node, ctx)?;
    if spec_select.arms.len() != sel.arms.len() {
        return None;
    }
    let mut arms: Vec<SelectArm> = Vec::with_capacity(sel.arms.len());
    for ((pat, _), (_, body_cached)) in spec_select.arms.iter().zip(sel.arms.iter()) {
        let arm = emit_arm(&scrut, pat, &body_cached.node, ctx, self_info)?;
        arms.push(arm);
    }
    out.push(GirStmt::Select { arms });
    Some(())
}

/// Emit one `select` arm. Returns the constructed [`SelectArm`] —
/// whose `cond` is `None` for unconditional arms (Ignore / bare Bind
/// patterns with no guard) and `Some(...)` for conditional arms.
fn emit_arm<R: crate::Rt, E: crate::UserEvent>(
    scrut: &GirExpr,
    pat: &Pattern,
    arm_body: &crate::Node<R, E>,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<SelectArm> {
    let mut arm_ctx = ctx.clone();
    let type_cond = emit_type_predicate_cond(scrut, &pat.type_predicate)?;
    let struct_cond =
        emit_arm_condition(scrut, &pat.structure_predicate, &mut arm_ctx)?;
    let cond = combine_cond_and_guard(type_cond, struct_cond);
    let guard = match &pat.guard {
        None => None,
        Some(g) => {
            // Guard is a source-level Expr (no compiled Node form
            // — it's part of the source pattern). Compile-time
            // pattern guards need their own emission path; for now
            // we bail on guarded arms during the Node transition.
            // TODO: feed the compiled guard Node through arm structure.
            let _ = g;
            return None;
        }
    };
    let combined = combine_cond_and_guard(cond, guard);
    let mut body = Vec::new();
    emit_body_into(&mut body, arm_body, &arm_ctx, self_info)?;
    Some(SelectArm { cond: combined, body })
}

/// Emit the condition expression for an arm's type predicate.
/// Returns `Ok(None)` when the type predicate is absent (no narrowing)
/// or trivially matches; `Ok(Some(...))` for a real test; and the outer
/// `None` when the predicate isn't expressible in GIR (so the arm
/// can't fuse).
///
/// Recognised narrowings:
/// - `Type::Primitive(Typ::Null)` over a `Null` / `Nullable<T>`
///   scrutinee → `GirOp::IsNull(scrut)`.
/// - Any non-null predicate over a scrutinee whose GirType is
///   `GirType::Prim` and which carries the same primitive (trivially
///   always-true narrowing) → no condition needed.
/// - Any non-null predicate over a `Nullable<T>` scrutinee → no
///   condition needed (the surrounding select is expected to have
///   already handled the null arm via `IsNull`, leaving this arm to
///   cover the non-null exhaustion case — typecheck's exhaustiveness
///   check is what makes this sound).
///
/// **Outer-`None` (refuse to fuse) for anything else.** In particular
/// a multi-branch union like `[i64, string, bool]` with type-tag arms
/// can't be lowered: GIR has no runtime tag check for primitive
/// shapes, so emitting "always-matches" for an `i64 as n` arm would
/// silently bind `n` to a string at runtime. The defensive bail-out
/// pushes such a select to the interpreter (which has the runtime
/// type dispatch).
fn emit_type_predicate_cond(
    scrut: &GirExpr,
    pred: &Option<Type>,
) -> Option<Option<GirExpr>> {
    let Some(t) = pred.as_ref() else {
        return Some(None);
    };
    match t {
        Type::Primitive(p)
            if p.contains(netidx_value::Typ::Null)
                && p.iter().count() == 1 =>
        {
            // `null as _` or bare `null` pattern — emit IsNull on the
            // scrutinee. Scrutinee type must be Null or Nullable; for
            // anything else the predicate could never match and we
            // bail (typecheck should have rejected it anyway).
            match &scrut.typ {
                GirType::Null | GirType::Nullable(_) => Some(Some(GirExpr {
                    op: GirOp::IsNull(Box::new(scrut.clone())),
                    typ: GirType::Prim(PrimType::Bool),
                })),
                _ => None,
            }
        }
        Type::Primitive(p)
            if !p.contains(netidx_value::Typ::Null)
                && p.iter().count() == 1 =>
        {
            // Single non-null primitive predicate. Two cases are
            // soundly always-true: (a) the scrutinee is a matching
            // Prim (trivial), and (b) the scrutinee is `Nullable<T>`
            // and the surrounding select's null arm has already
            // filtered out `null` (exhaustiveness leaves this arm
            // to cover the non-null half).
            let pred_typ = p.iter().next();
            match (&scrut.typ, pred_typ) {
                (GirType::Prim(sp), Some(pt))
                    if PrimType::from_typ(pt) == Some(*sp) =>
                {
                    Some(None)
                }
                (GirType::Nullable(_), _) => Some(None),
                // Any other shape (e.g. a wider union scrutinee or
                // a mismatched Prim) we can't lower soundly. Refuse
                // to fuse — the surrounding kernel build bails and
                // the select runs on the interpreter.
                _ => None,
            }
        }
        // Multi-bit predicates (compound `[i64, null]`-shaped) and
        // composite type predicates aren't expressible in GIR yet;
        // refuse to fuse.
        _ => None,
    }
}

/// Emit the condition expression for an arm's structure predicate.
/// Returns `Ok(None)` when the predicate always matches (Ignore /
/// Bind), `Ok(Some(...))` for a real test, and `None` if the emitter
/// can't express the predicate (compound patterns — variants, tuples,
/// arrays — are not supported yet). Mutates `arm_ctx` to add any
/// bindings introduced by the pattern.
fn emit_arm_condition(
    scrut: &GirExpr,
    pat: &StructurePattern,
    arm_ctx: &mut FusionCtx,
) -> Option<Option<GirExpr>> {
    match pat {
        StructurePattern::Ignore => Some(None),
        StructurePattern::Bind(name) => {
            // Introduce a new local that aliases the scrutinee's type.
            // The scrutinee value itself isn't auto-bound by name here;
            // that would require emitting a `let` at the start of the
            // arm body. Existing tests don't exercise the pattern of
            // referencing the bound name with a complex scrutinee, so
            // we preserve the historic behaviour of just adding the
            // input to ctx — any lookup will resolve to the bound name
            // as a free identifier in the arm body, which works only
            // when the scrutinee was itself a bare local of that name.
            // Compound scrutinee + Bind would leave the bound name
            // dangling; the rewrite pass should reject that case.
            // Bind binds a scalar local — array scrutinees aren't
            // valid match positions per typecheck.
            let prim = scrut.typ.as_prim()?;
            arm_ctx.inputs.push(Input {
                name: name.clone(),
                prim,
                bind_id: None,
            });
            Some(None)
        }
        StructurePattern::Literal(v) => {
            let c = ConstVal::from_value(v)?;
            if GirType::Prim(c.typ()) != scrut.typ {
                return None;
            }
            // Small simplification for bool literals: `scrut == true`
            // becomes `scrut`, `scrut == false` becomes `!scrut`. The
            // generated machine code is identical either way, but the
            // rendered Rust is tidier.
            if let ConstVal::Bool(b) = c {
                if b {
                    return Some(Some(scrut.clone()));
                } else {
                    return Some(gir::not(scrut.clone()).map(Some)?);
                }
            }
            gir::cmp(scrut.clone(), gir::const_expr(c), gir::CmpOp::Eq).map(Some)
        }
        // `` `Tag(p0, p1, ...) `` — variant pattern. Requires the
        // scrutinee to be a Ref to a kernel's variant param (so we
        // can name it inside the tag-equality check); inline
        // variant values aren't supported in v0. Lowers to:
        //   - condition: `GirOp::VariantTagEq { name, expected_tag }`
        //   - bindings: each payload sub-pattern that's a simple
        //     `Bind(name)` adds a scalar Input to arm_ctx whose
        //     value lookup goes through `GirOp::VariantPayload`.
        //
        // Nested patterns inside payloads aren't supported in v0;
        // anything other than `Bind` / `Ignore` in a payload bails.
        StructurePattern::Variant { all: _, tag, binds } => {
            let var_name = match &scrut.op {
                GirOp::Local(n) => n.clone(),
                _ => return None,
            };
            // Clone the case shape so the immutable borrow of
            // arm_ctx is released before we mutate known_consts
            // below.
            // Today variant-pattern binding only handles primitive
            // payloads; composite payloads (variants containing
            // tuples / structs / nested variants) are a follow-up.
            // Extract a Vec<PrimType> or bail.
            let (var_name_owned, case_payloads): (ArcStr, Vec<PrimType>) = {
                let vi = arm_ctx.find_variant(&var_name)?;
                let case = vi.cases.iter().find(|(t, _)| t == tag)?;
                if case.1.len() != binds.len() {
                    return None;
                }
                let prims: Option<Vec<PrimType>> =
                    case.1.iter().map(GirType::as_prim).collect();
                (vi.name.clone(), prims?)
            };
            // For v0, payload bindings flow through the
            // `known_consts` channel: each named bind is mapped to a
            // synthetic GirExpr that reads
            // `GirOp::VariantPayload(name, idx)`. The arm body's
            // `Ref(bind_name)` lookup resolves to that expression.
            for (i, (payload_pat, payload_typ)) in
                binds.iter().zip(case_payloads.iter()).enumerate()
            {
                match payload_pat {
                    StructurePattern::Bind(bind_name) => {
                        let payload_expr = GirExpr {
                            op: GirOp::VariantPayload {
                                name: var_name_owned.clone(),
                                payload_idx: i,
                                elem_typ: *payload_typ,
                            },
                            typ: GirType::Prim(*payload_typ),
                        };
                        arm_ctx.known_consts.insert(
                            bind_name.clone(),
                            KnownConst { expr: payload_expr },
                        );
                    }
                    StructurePattern::Ignore => {}
                    _ => return None,
                }
            }
            Some(Some(GirExpr {
                op: GirOp::VariantTagEq {
                    name: var_name_owned,
                    expected_tag: tag.clone(),
                },
                typ: GirType::Prim(PrimType::Bool),
            }))
        }
        // Non-primitive patterns — not fusable yet.
        StructurePattern::Slice { .. }
        | StructurePattern::SlicePrefix { .. }
        | StructurePattern::SliceSuffix { .. }
        | StructurePattern::Tuple { .. }
        | StructurePattern::Struct { .. } => None,
    }
}

/// Emit a "tail" expression — either a pure expression that becomes
/// a `Return`, or a self-recursive tail call that becomes `TailCall`.
fn emit_tail<R: crate::Rt, E: crate::UserEvent>(
    out: &mut Vec<GirStmt>,
    node: &crate::Node<R, E>,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<()> {
    use crate::NodeView;
    if let Some(si) = self_info {
        if try_emit_tail_call(out, node, ctx, si).is_some() {
            return Some(());
        }
    }
    match node.view() {
        NodeView::Select(s) => return emit_select(out, s, ctx, self_info),
        NodeView::Block(b) => return emit_do(out, &b.children, ctx, self_info),
        NodeView::ExplicitParens(ep) => return emit_tail(out, &ep.n, ctx, self_info),
        _ => {}
    }
    let v = emit_node(node, ctx)?;
    out.push(GirStmt::Return(v));
    Some(())
}

fn try_emit_tail_call<R: crate::Rt, E: crate::UserEvent>(
    out: &mut Vec<GirStmt>,
    node: &crate::Node<R, E>,
    ctx: &FusionCtx,
    self_info: &SelfInfo,
) -> Option<()> {
    use crate::NodeView;
    let cs = match node.view() {
        NodeView::CallSite(cs) => cs,
        _ => return None,
    };
    // Source-level Apply for ident extraction.
    let spec_apply = match &cs.spec().kind {
        ExprKind::Apply(a) => a,
        _ => return None,
    };
    let fn_ref = match &spec_apply.function.kind {
        ExprKind::Ref { name } => name,
        _ => return None,
    };
    let fn_ident = ident_of(fn_ref)?;
    if fn_ident != self_info.name.as_str() {
        return None;
    }
    if spec_apply.args.len() != self_info.source_args.len() {
        return None;
    }
    if spec_apply.args.iter().any(|(label, _)| label.is_some()) {
        return None;
    }
    let mut args: Vec<GirExpr> = Vec::with_capacity(spec_apply.args.len());
    for (i, self_arg) in self_info.source_args.iter().enumerate() {
        let arg_node = cs.arg_positional(i)?;
        let e = emit_node(arg_node, ctx)?;
        if e.typ != self_arg.typ {
            return None;
        }
        args.push(e);
    }
    out.push(GirStmt::TailCall { args });
    Some(())
}

/// Try to determine the primitive return type of a function body
/// without requiring an explicit `-> T` annotation. Walks the body
/// structurally, looking at the tail position:
/// - a Do block's type is its last expression's
/// - a Select's type is the first arm whose body we can type
/// - an Apply to a known-fused function gives its declared return
/// - self-recursive calls contribute nothing (the type we'd be
///   inferring *is* the rtype — circular), so they're skipped
/// - any other pure expression's type is what `emit_expr` reports
fn infer_body_rtype<R: crate::Rt, E: crate::UserEvent>(
    body: &crate::Node<R, E>,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<GirType> {
    use crate::NodeView;
    // Self-recursion fast path.
    if let NodeView::CallSite(cs) = body.view() {
        if let ExprKind::Apply(a) = &cs.spec().kind {
            if let ExprKind::Ref { name } = &a.function.kind {
                if let Some(ident) = ident_of(name) {
                    if let Some(si) = self_info {
                        if si.name.as_str() == ident {
                            return None;
                        }
                    }
                    if let Some(kf) = ctx.find_fn(ident) {
                        return Some(kf.return_type.clone());
                    }
                }
            }
        }
    }
    // Typed-AST fast path via the Node's spec.
    if let Some(t) = body.spec().typ.get() {
        if let Some(kt) = GirType::from_type(t) {
            return Some(kt);
        }
    }
    emit_node(body, ctx).map(|e| e.typ)
}




// ─── Whole-graph fusion analyzer (M8.4) ────────────────────────────
//
// `analyze_program` carves each top-level expression into maximal
// *sync subgraphs* — connected regions of the dataflow graph
// containing no async edges. Each region becomes one GIR kernel
// (built in M8.4 step d); the runtime splices it in as a single node
// (M8.4 step e), replacing the chain of individual graph nodes it
// covers.
//
// An async edge — the boundary between regions — is any of:
// - an async-effect builtin call (timer, subscribe, IO, queue, …)
// - a call to an intrinsically-async user lambda
// - a read of an unstable binding (a `<-` write target): its value
//   depends on a future-cycle write, so reading it can't be fused
//   into the same kernel as its consumer.
//
// `program_effect_map` (phase A) classifies every `ExprId` Sync or
// Async by joining the intrinsic edge effect of the node with the
// effects of its same-scope children. Phase B (`carve_into`)
// extracts every maximal joined-Sync subtree: a node roots a region
// if it is joined-Sync and its parent is joined-Async (or it is a
// top-level expression). Async nodes are not absorbed into any
// region — `carve_into` recurses into their children hunting for
// sync sub-regions among them.
//
// M8.4 — initial model: a region is a fully-sync subtree. Async
// edges are only allowed as ancestors of a region, never inside or
// as direct children. The follow-up promotes async-edge direct
// children to kernel inputs, fusing sync ops that *consume* an async
// value into the same kernel as the rest of their sync subtree.
//
// Top-level only: nested lambda bodies are never split at an
// interior async edge. A lambda whose body contains an async edge
// has its `intrinsic_effect` already marked Async by `infer_effects`,
// so every call site of it is an async edge from the caller's POV —
// the lambda body stays its own (non-fused) thing.


/// One kernel input for a region — a value flowing from outside the
/// region into its kernel. Two kinds, distinguished by [`source`]:
///
/// - `Binding`: a free-variable `Ref` in the region body resolving to
///   a binding defined outside the region. The runtime feeds this
///   input via a freshly-compiled `Ref` Node subscribing to the
///   binding's `BindId`.
/// - `Lifted(Expr)`: an Async sub-expression promoted to a kernel
///   input by the maximal-fusion carving phase. The runtime feeds
///   this input by compiling the lifted `Expr` as a standalone Node
///   via the regular node-compilation pipeline.
#[derive(Debug, Clone)]
pub struct RegionInput {
    /// `ExprId` of the cross-edge sub-expression whose value feeds
    /// this slot — either the `Ref` site (binding source) or the
    /// lifted Async sub-expression's root (lifted source). The
    /// runtime splice step uses this to wire the input.
    pub expr_id: crate::expr::ExprId,
    /// The name the kernel body uses to refer to this slot. For a
    /// binding input it is the binding's own name (so the normal
    /// `Ref` lowering in `emit_expr` resolves to the slot without
    /// rewriting the region body). For a lifted input it is a
    /// synthetic `__lifted_<expr_id>` name that the lifted-input
    /// intercept at the top of `emit_expr` matches against
    /// [`FusionCtx::lifted_inputs`].
    pub name: ArcStr,
    /// Slot kind — drives which [`FusionCtx`] input list the slot
    /// lands in and the kernel param's [`GirType`].
    pub kind: RegionInputKind,
    /// How the runtime feeds this slot: either a binding subscription
    /// or a freshly-compiled standalone Node for the lifted Expr.
    pub source: RegionInputSource,
}

/// Where a region input's value comes from at runtime. Drives
/// [`crate::node::region::FusedRegion::from_subgraph`]'s arg-node
/// construction.
#[derive(Debug, Clone)]
pub enum RegionInputSource {
    /// Subscribe to the binding named on the carrier `RegionInput`.
    /// The runtime compiles a synthetic `ExprKind::Ref` feeder, same
    /// as today's initial-model behavior.
    Binding,
    /// Compile the carried Expr as a standalone Node and feed its
    /// output. Used for Async sub-expressions promoted to inputs by
    /// the maximal-fusion carving phase.
    Lifted(Expr),
}

/// Per-input slot classification. Mirrors the param shapes
/// [`build_kir_kernel`] derives from a `LambdaExpr`'s argspec —
/// scalar primitive, array of primitive, tuple/struct/variant of
/// primitives. Function-typed inputs are not supported in the M8.4
/// initial model (a region has no HOF params; HOF callees come in
/// through the `known` map as `GirOp::Call` targets).
#[derive(Debug, Clone)]
pub enum RegionInputKind {
    Prim(PrimType),
    Array(GirType),
    Tuple(Vec<GirType>),
    Struct(Vec<(ArcStr, GirType)>),
    Variant(Vec<(ArcStr, Vec<GirType>)>),
    /// `[T, null]` option shape — runtime representation is a `Value`
    /// that is either `Value::Null` or `T`'s form. `inner` is the
    /// non-null element type.
    Nullable(GirType),
}

















/// Accumulated parameter slots — the param-derivation output shared
/// between [`build_kir_kernel_with_binding_inputs`] (driven by a
/// `LambdaExpr`'s argspec) and [`build_kir_kernel_from_region`]
/// (driven by a `Vec<RegionInput>`). Bundling these in one struct
/// keeps [`finish_kernel`]'s signature short.
#[derive(Default)]
struct KernelParams {
    params: Vec<Input>,
    fn_params: Vec<crate::gir::FnParam>,
    array_params: Vec<crate::gir::ArrayInput>,
    tuple_params: Vec<crate::gir::TupleInput>,
    struct_params: Vec<crate::gir::StructInput>,
    variant_params: Vec<crate::gir::VariantInput>,
    nullable_params: Vec<crate::gir::NullableInput>,
    arg_types: Vec<GirType>,
}

/// Populate `ctx`'s slot lists + `params` + `tail_call_slots` from a
/// `&[RegionInput]` list. This is the single source of truth for
/// "given a typed input, where does its slot go": every kernel build
/// path — lambda, region, module — routes its value inputs through
/// here. Adding a new `RegionInputKind` variant means updating this
/// function and nothing else.
///
/// Lambdas with fn-typed args route those through the parallel
/// `fn_inputs` channel (`FnParam` / `FnSource::Param`), not through
/// here.
fn populate_kernel_inputs(
    value_inputs: &[RegionInput],
    ctx: &mut FusionCtx,
    p: &mut KernelParams,
    tail_call_slots: &mut Vec<crate::gir::TailCallSlot>,
) {
    for input in value_inputs {
        // Maximal-fusion: register lifted-input ExprId → synth-name
        // mapping so `emit_expr`'s top-level intercept matches when
        // it encounters the lifted ExprId in the region body.
        if let RegionInputSource::Lifted(expr) = &input.source {
            ctx.lifted_inputs.insert(expr.id, input.name.clone());
        }
        match &input.kind {
            RegionInputKind::Prim(prim) => {
                let i = Input {
                    name: input.name.clone(),
                    prim: *prim,
                    bind_id: None,
                };
                p.params.push(i.clone());
                ctx.inputs.push(i);
                p.arg_types.push(GirType::Prim(*prim));
                tail_call_slots.push(crate::gir::TailCallSlot {
                    name: input.name.clone(),
                    kind: crate::gir::TailCallSlotKind::Scalar(*prim),
                });
            }
            RegionInputKind::Array(elem) => {
                let ai = crate::gir::ArrayInput {
                    name: input.name.clone(),
                    elem: elem.clone(),
                    bind_id: None,
                };
                p.array_params.push(ai.clone());
                ctx.array_inputs.push(ai);
                p.arg_types.push(GirType::Array(Box::new(elem.clone())));
                tail_call_slots.push(crate::gir::TailCallSlot {
                    name: input.name.clone(),
                    kind: crate::gir::TailCallSlotKind::ValArray,
                });
            }
            RegionInputKind::Tuple(elems) => {
                let ti = crate::gir::TupleInput {
                    name: input.name.clone(),
                    elems: elems.clone(),
                    bind_id: None,
                };
                p.tuple_params.push(ti.clone());
                ctx.tuple_inputs.push(ti);
                p.arg_types.push(GirType::Tuple(elems.clone()));
                tail_call_slots.push(crate::gir::TailCallSlot {
                    name: input.name.clone(),
                    kind: crate::gir::TailCallSlotKind::ValArray,
                });
            }
            RegionInputKind::Struct(fields) => {
                let si = crate::gir::StructInput {
                    name: input.name.clone(),
                    fields: fields.clone(),
                    bind_id: None,
                };
                p.struct_params.push(si.clone());
                ctx.struct_inputs.push(si);
                p.arg_types.push(GirType::Struct(fields.clone()));
                tail_call_slots.push(crate::gir::TailCallSlot {
                    name: input.name.clone(),
                    kind: crate::gir::TailCallSlotKind::ValArray,
                });
            }
            RegionInputKind::Variant(cases) => {
                let vi = crate::gir::VariantInput {
                    name: input.name.clone(),
                    cases: cases.clone(),
                    bind_id: None,
                };
                p.variant_params.push(vi.clone());
                ctx.variant_inputs.push(vi);
                p.arg_types.push(GirType::Variant(cases.clone()));
                tail_call_slots.push(crate::gir::TailCallSlot {
                    name: input.name.clone(),
                    kind: crate::gir::TailCallSlotKind::Variant,
                });
            }
            RegionInputKind::Nullable(elem) => {
                let ni = crate::gir::NullableInput {
                    name: input.name.clone(),
                    elem: elem.clone(),
                    bind_id: None,
                };
                p.nullable_params.push(ni.clone());
                ctx.nullable_inputs.push(ni);
                p.arg_types.push(GirType::Nullable(Box::new(elem.clone())));
                tail_call_slots.push(crate::gir::TailCallSlot {
                    name: input.name.clone(),
                    kind: crate::gir::TailCallSlotKind::Nullable,
                });
            }
        }
    }
}

/// Unified kernel-build core. Every fusion entry point (lambda body,
/// region root, top-level module Do) routes through this. The three
/// thin wrappers above translate their caller-specific inputs into
/// this function's shape:
///
/// - `value_inputs` — every prim/array/tuple/struct/variant input.
///   Lambdas translate their argspec into these (with fn-typed args
///   diverted to `fn_inputs`); regions and module bodies already
///   have them in `&[RegionInput]` shape.
/// - `fn_inputs` — every fn-typed input. For lambdas: HOF args
///   (`FnSource::Param`) + binding-source slots. For regions /
///   modules: builtin-source + binding-source slots discovered via
///   `discover_builtin_fn_inputs` / `discover_binding_fn_inputs`.
/// - `return_type` — `Some(t)` if the caller already knows it
///   (lambda's `rtype` annotation, module's tuple-of-exports); `None`
///   to derive it from the body's typed AST via `infer_body_rtype`.
/// - `has_tail` / `self_info` — lambda-only. Both `false`/`None` for
///   regions and modules.
///
/// Returns `None` if anything in the body fails to lower; callers
/// fall back to non-fused execution.
fn build_kernel<R: crate::Rt, E: crate::UserEvent>(
    fn_name: &str,
    body: &crate::Node<R, E>,
    value_inputs: &[RegionInput],
    fn_inputs: &[crate::gir::FnParam],
    return_type: Option<GirType>,
    has_tail: bool,
    self_info: Option<&SelfInfo>,
    known: &std::collections::BTreeMap<ArcStr, KnownFusedFn>,
    consts: &std::collections::BTreeMap<ArcStr, KnownConst>,
    builtin_apply_sites: nohash::IntMap<crate::expr::ExprId, BuiltinCallSiteInfo>,
) -> Option<(GirKernel, KnownFusedFn)> {
    let mut ctx = FusionCtx {
        inputs: vec![],
        fn_inputs: fn_inputs.to_vec(),
        array_inputs: vec![],
        tuple_inputs: vec![],
        struct_inputs: vec![],
        variant_inputs: vec![],
        nullable_inputs: vec![],
        string_inputs: vec![],
        known_fns: known.clone(),
        known_consts: consts.clone(),
        lifted_inputs: nohash::IntMap::default(),
        builtin_apply_sites,
    };
    let mut p = KernelParams::default();
    p.fn_params.extend(fn_inputs.iter().cloned());
    let mut tail_call_slots: Vec<crate::gir::TailCallSlot> =
        Vec::with_capacity(value_inputs.len());
    populate_kernel_inputs(value_inputs, &mut ctx, &mut p, &mut tail_call_slots);
    let rtype = match return_type {
        Some(t) => t,
        None => infer_body_rtype(body, &ctx, self_info)?,
    };
    finish_kernel(
        fn_name,
        body,
        rtype,
        p,
        tail_call_slots,
        has_tail,
        &mut ctx,
        self_info,
    )
}

/// Final assembly shared between the lambda and region build paths.
/// Builds the [`KnownFusedFn`] signature, registers `fn_name` in
/// `ctx.known_fns` (so self-recursive `Call` sites in the body lower
/// before `emit_body` runs), emits the body to GIR, and packages
/// everything into a [`GirKernel`].
///
/// The caller is responsible for the bits this function can't decide
/// from a generic kernel build: `params` (the slot bundle),
/// `return_type` (from `lambda.rtype` / `infer_body_rtype` for a
/// lambda, or supplied directly for a region), `tail_call_slots` /
/// `has_tail_loop` / `self_info` (always lambda-specific; regions
/// pass empty / false / None).
fn finish_kernel<R: crate::Rt, E: crate::UserEvent>(
    fn_name: &str,
    body: &crate::Node<R, E>,
    return_type: GirType,
    params: KernelParams,
    tail_call_slots: Vec<crate::gir::TailCallSlot>,
    has_tail_loop: bool,
    ctx: &mut FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<(GirKernel, KnownFusedFn)> {
    let signature = KnownFusedFn {
        body_fn_name: format!("fused_{fn_name}_body"),
        arg_types: params.arg_types,
        return_type: return_type.clone(),
    };
    ctx.known_fns.insert(ArcStr::from(fn_name), signature.clone());
    let body_stmts = emit_body(body, ctx, self_info)?;
    let kernel = GirKernel {
        fn_name: ArcStr::from(fn_name),
        params: params.params,
        fn_params: params.fn_params,
        array_params: params.array_params,
        tuple_params: params.tuple_params,
        struct_params: params.struct_params,
        variant_params: params.variant_params,
        nullable_params: params.nullable_params,
        tail_call_slots,
        return_type,
        has_tail_loop,
        body: body_stmts,
    };
    Some((kernel, signature))
}



/// Build a [`GirKernel`] from an arbitrary `Expr` (the root of a
/// maximal sync region identified by [`analyze_program`]) plus a
/// typed input list. Mirrors [`build_kir_kernel_with_binding_inputs`]
/// for lambdas; a region has no argspec, no self-recursion, and no
/// tail loop, so this path is simpler — no `SelfInfo`,
/// `tail_call_slots` is empty, and `has_tail_loop` is always false.
///
/// `return_type` is the region root's `GirType`. Pass `Some(t)` when
/// the caller already knows it (e.g. for an `Apply` region root, from
/// the call-site's resolved FnType via
/// `resolved_fn_type(&apply.function).rtype`); pass `None` to have
/// this function infer it via [`infer_body_rtype`] over the region
/// body. Inference failure yields `None`.
pub fn build_kir_kernel_from_region<R: crate::Rt, E: crate::UserEvent>(
    fn_name: &str,
    region: &crate::Node<R, E>,
    inputs: &[RegionInput],
    extra_fn_inputs: &[crate::gir::FnParam],
    return_type: Option<GirType>,
    known: &std::collections::BTreeMap<ArcStr, KnownFusedFn>,
    consts: &std::collections::BTreeMap<ArcStr, KnownConst>,
    builtin_apply_sites: nohash::IntMap<crate::expr::ExprId, BuiltinCallSiteInfo>,
) -> Option<(GirKernel, KnownFusedFn)> {
    // Regions have no self-recursion, no tail loop. Inputs and
    // fn_inputs are already in the unified shape — everything
    // delegates to `build_kernel`.
    build_kernel(
        fn_name,
        region,
        inputs,
        extra_fn_inputs,
        return_type,
        false,
        None,
        known,
        consts,
        builtin_apply_sites,
    )
}



/// If `value` is a compile-time-computable primitive expression in
/// terms of already-known constants, record `(name, KnownConst)` into
/// `consts`. The GIR stored on the KnownConst is the full emitted
/// expression — the JIT folds it as appropriate at lowering.
///
/// Used by `Bind::compile` (the runtime path): the runtime fusion
/// attempts in `Lambda::compile` need const visibility for the
/// kernels they build.
pub fn record_const_binding(
    name: &ArcStr,
    value: &Expr,
    consts: &mut std::collections::BTreeMap<ArcStr, KnownConst>,
) {
    // Direct Constant is trivially constant.
    if let ExprKind::Constant(v) = &value.kind {
        if let Some(c) = ConstVal::from_value(v) {
            consts.insert(name.clone(), KnownConst { expr: gir::const_expr(c) });
            return;
        }
    }
    // Expression-valued bindings: try to emit them with only the
    // known-const registry visible (empty inputs). If emit_expr
    // succeeds, every Ref inside resolved to a constant — we can
    // freeze the whole expression.
    // Expression-valued const folding via emit-probe is on hold
    // during the Expr → Node migration. The Constant fast path
    // above still folds direct literals (which covers most
    // declarations); compound const expressions just don't get
    // pre-resolved by fusion, which costs a tiny bit of inlining
    // opportunity but not correctness.
    let _ = value;
    let _ = consts;
}


