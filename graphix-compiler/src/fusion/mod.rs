//! Fusion: graphix's GIR-emission + kernel-build + native-code
//! pipeline.
//!
//! Layered pipeline:
//!
//! 1. **Walker** ([`walker`]): traverses the compiled node graph via
//!    [`crate::NodeView`], producing a flat list of
//!    [`walker::KernelCandidate`]s. Each candidate identifies a
//!    Bind, Lambda, Block-module, or Region that should be fused.
//!
//! 2. **Builder** ([`builder`]): consumes candidates, emits each
//!    body's GIR via [`lowering::emit_expr`], wraps the result in a
//!    [`crate::gir::GirKernel`]. Calls into the JIT
//!    ([`crate::gir_jit`]) to produce native code.
//!
//! 3. **Splicer** ([`builder::splice`]): replaces the original
//!    `Node` in the runtime's node graph with a
//!    [`builder::FusedKernel`] wrapper that holds the JIT artifact
//!    and dispatches at `Update::update` time.
//!
//! 4. **Lowering** ([`lowering`]): the per-`ExprKind` → GirExpr
//!    translation. This is the largest piece by line count — it
//!    handles every shape of graphix expression we know how to
//!    fuse. The translation is independent of the walker / builder
//!    / splicer; those just orchestrate when and on what to call
//!    it.

pub mod builder;
pub mod lowering;
pub mod walker;

// Re-export public surface so callers see a flat `fusion::*` API.
pub use builder::{splice, BuiltKernel, FusedKernel, SpliceTarget};
pub use walker::{
    walk, CandidateCounts, CandidateKind, Candidates, KernelCandidate,
};

// Lowering re-exports — preserved during the v1→v2 transition so
// existing callers (callsite.rs, mod.rs, node/*.rs, gx.rs) keep
// finding `crate::fusion::emit_expr` etc. As the rip-out
// progresses these will narrow to just the genuinely-needed
// public surface.
pub use lowering::*;

/// Run the fusion phase on a compiled Node tree.
///
/// Called by [`crate::compile`] between typecheck phase 2 and
/// return. Walks the Node graph, identifies kernel candidates,
/// builds GIR kernels, JIT-compiles them, splices them into the
/// Node tree in place.
///
/// **Current scope**: zero-input `Region` candidates only — a
/// non-Bind module-top-level expression whose body has no free
/// variables (the canonical "constant kernel" case). Other
/// candidate kinds (`LambdaBind`, `AnonymousLambda`, `ValueBind`,
/// `Region` with free-var inputs) are recognized by the walker
/// but skipped here; subsequent iterations add their build/splice
/// support.
///
/// The function takes `&mut Node` rather than `&mut [Node]`
/// because — with `load()`'s Module wrapping in place — every
/// compile produces one Node (a module containing the file's
/// graph, or a single inline expression). Cross-Bind dependencies
/// are visible because they're inside the single Node tree.
pub fn fuse<R: crate::Rt, E: crate::UserEvent>(
    node: &mut crate::Node<R, E>,
    ctx: &mut crate::ExecCtx<R, E>,
    flags: enumflags2::BitFlags<crate::CFlag>,
) -> anyhow::Result<()> {
    // `FusionDisabled`: skip the whole phase — programs execute via
    // the regular node-graph interpreter. Used by the test
    // harness's `interp` mode as ground truth.
    if flags.contains(crate::CFlag::FusionDisabled) {
        return Ok(());
    }
    let jit_disabled = flags.contains(crate::CFlag::JitDisabled);
    // Phase 1: walk the typed Node graph to collect candidates.
    // The walker borrows into the tree, so we extract owned data
    // (source_id + cloned body Expr + free-var inputs) into a
    // separate plan list before any mutation. Refs analysis runs
    // here under the shared-borrow window — it needs to find the
    // candidate's subtree in the Node graph to enumerate its
    // referenced / bound BindIds.
    let plan: Vec<RegionPlan> = {
        let slice = std::slice::from_ref(node);
        let candidates = walk(slice, crate::expr::ModPath::root());
        candidates
            .items
            .into_iter()
            .filter_map(|c| {
                // Normalize each candidate into a (source_id, body)
                // pair. Region: those are the candidate's own
                // fields. ValueBind: the body is the Bind's value
                // Expr and the splice target is that value's node
                // — so the wrapper Bind keeps handling the
                // publish-to-BindId via `pattern.bind`. (Splicing
                // the Bind itself would skip the publish.)
                let (source_id, body) = match c.kind {
                    CandidateKind::Region { source_id, body } => {
                        // body is &Node<R, E>; extract its spec Expr
                        // for passing to the GIR build path.
                        (source_id, body.spec().clone())
                    }
                    CandidateKind::ValueBind { bind, .. } => {
                        let value_expr = match &bind.spec.kind {
                            crate::expr::ExprKind::Bind(be) => {
                                be.value.clone()
                            }
                            // Shouldn't happen — visit_bind only
                            // registers ValueBind when the node's
                            // spec is an ExprKind::Bind.
                            _ => return None,
                        };
                        let value_id = value_expr.id;
                        (value_id, value_expr)
                    }
                    // LambdaBind / AnonymousLambda land in
                    // subsequent iterations.
                    _ => return None,
                };
                // Locate the subtree Node so we can enumerate its
                // refs. If we can't find it (shouldn't happen — we
                // just got this ID from the same walker), skip the
                // candidate.
                let subtree = find_node_by_id::<R, E>(node, source_id)?;
                let inputs = collect_region_inputs::<R, E>(&**subtree, ctx);
                Some(RegionPlan { source_id, body, inputs })
            })
            .collect()
    };

    // Phase 2: build + JIT + splice for each Region candidate.
    for entry in plan {
        let fn_name = format!("region_{:?}", entry.source_id);
        // Build the region-input list the builder consumes.
        let region_inputs: Vec<crate::fusion::RegionInput> = entry
            .inputs
            .iter()
            .map(|fv| crate::fusion::RegionInput {
                expr_id: entry.source_id, // placeholder — emit_expr
                                          // only reads `name` for
                                          // Binding-source inputs
                name: fv.name.clone(),
                kind: fv.kind.clone(),
                source: crate::fusion::RegionInputSource::Binding,
                bind_id: Some(fv.bind_id),
            })
            .collect();
        // Walk the body for sync-builtin Apply sites so the kernel
        // build can lower them via `FnSource::Builtin` DynCalls.
        // Site discovery doesn't bind the runtime — the actual
        // `Apply` impl is constructed at `GirNode::new` time via
        // `pre_init_builtin_slots`.
        //
        // **Node-based discovery**: we walk the compiled Node tree
        // (via `NodeView`) rather than the source Expr tree. At
        // each `CallSite` we read the resolved call-site FnType
        // off the Node directly (`CallSite::ftype()`), which has
        // had its TVars unified with the call-site's concrete
        // types during typecheck. The Expr-based walker reads the
        // function expression's typed-AST cell — which carries the
        // lambda's *generic* FnType, surfacing as a runtime error
        // for builtins whose return type is polymorphic (e.g.
        // `str::parse`: "requires a concrete type annotation").
        let discovery = match find_node_by_id::<R, E>(
            node,
            entry.source_id,
        ) {
            Some(subtree) => {
                let mut out = crate::fusion::BuiltinCallDiscovery::default();
                crate::fusion::walk_node_for_builtin_calls::<R, E>(
                    &**subtree,
                    ctx,
                    &crate::expr::ModPath::root(),
                    &mut out,
                );
                out
            }
            None => crate::fusion::discover_builtin_calls::<R, E>(
                &entry.body,
                ctx,
                &crate::expr::ModPath::root(),
            ),
        };
        // Re-locate the body Node for the build step. `find_node_by_id`
        // is fast (small constant scan of the tree) so doing this twice
        // — once for discovery, once for the build — is cheap and
        // keeps the borrow-scopes simple.
        let body_node = match find_node_by_id::<R, E>(node, entry.source_id) {
            Some(n) => n,
            None => continue,
        };
        let built = match builder::build_region(
            body_node,
            entry.source_id,
            &fn_name,
            &region_inputs,
            discovery,
            ctx,
        ) {
            Ok(b) => b,
            Err(_) => continue,
        };
        // Skip kernels whose return type isn't representable at the
        // FusedKernel boundary. `Null` only appears as an
        // intermediate inside fused expressions — Nullable is the
        // proper kernel-return shape — and `Unit` is a side-effect-
        // only marker that we don't expose to the runtime here.
        use crate::gir::AbiKind;
        match crate::gir::abi_kind(&built.kernel.return_type) {
            Some(
                AbiKind::Scalar(_)
                | AbiKind::Array
                | AbiKind::Tuple
                | AbiKind::Struct
                | AbiKind::Variant
                | AbiKind::Nullable
                | AbiKind::Value
                | AbiKind::String,
            ) => {}
            Some(AbiKind::Unit | AbiKind::Null) | None => continue,
        }
        // Skip identity kernels — a region whose body is just
        // `Return(Local(x))` forwards one input unchanged and does no
        // work. Fusing it wraps a zero-compute kernel in dispatch
        // overhead; the runtime `Ref` feeder already produces that
        // value. NOTE: the `run!` test harness wraps every fixture as
        // `let result = {code}`, which yields exactly such a
        // `Return(Local("result"))` region. Suppressing it means a
        // fixture only registers as "fused" when its *body* genuinely
        // fuses into its own kernel — so the FuseExpect metric measures
        // real body fusion, not the hollow wrapper (see CLAUDE.md
        // "#139 identity-kernel" note).
        if built.kernel.is_identity_passthrough() {
            continue;
        }
        // JIT-compile unless `JitDisabled` is set. The interp
        // fallback path inside `FusedKernel::update` handles the
        // None case via `gir_interp`.
        //
        // Phase D — cross-kernel calls: pass every sub-kernel the
        // parent's body references via `GirOp::Call` (closure
        // conversion's lambda kernels, transitively collected in
        // `built.called_kernels`) as JIT callees. Without these, a
        // parent kernel containing a `Call` fails to JIT-compile and
        // silently falls back to the interpreter — so capturing
        // closures would never actually fuse natively. The keys here
        // (source binding names) match the `GirOp::Call.fn_name`
        // sites the emitter produced.
        let callees: std::collections::BTreeMap<
            arcstr::ArcStr,
            std::sync::Arc<crate::gir::GirKernel>,
        > = built
            .called_kernels
            .iter()
            .map(|(name, c)| (name.clone(), c.kernel.clone()))
            .collect();
        let wrapped: Option<std::sync::Arc<crate::gir_jit::WrappedKernel>> =
            if jit_disabled {
                None
            } else {
                match crate::gir_jit::compile_kernel_with_callees(
                    &mut ctx.jit.lock(),
                    &built.kernel,
                    &callees,
                ) {
                    Ok(w) => Some(std::sync::Arc::new(w)),
                    Err(_) => continue,
                }
            };
        let typ = entry
            .body
            .typ
            .get()
            .cloned()
            .unwrap_or(crate::typ::Type::Bottom);
        // Construct one Ref feeder per input.
        let feeders: Box<[crate::Node<R, E>]> = entry
            .inputs
            .iter()
            .map(|fv| {
                crate::node::genn::reference::<R, E>(
                    ctx,
                    fv.bind_id,
                    fv.typ.clone(),
                    entry.source_id,
                )
            })
            .collect();
        let kernel_node = match builder::FusedKernel::<R, E>::new(
            ctx,
            entry.body.clone(),
            typ,
            built.kernel.clone(),
            wrapped,
            feeders,
            crate::Scope::root(),
            entry.source_id,
            built.called_kernels.clone(),
        ) {
            Ok(n) => n,
            Err(_) => continue,
        };
        // Splice into the tree. If the candidate IS the root, the
        // splice helper handles that via std::mem::replace; for
        // nested candidates it descends via splice_child.
        match splice_into(node, entry.source_id, kernel_node) {
            Ok(mut old) => {
                old.delete(ctx);
                log::debug!(
                    "fusion::fuse: spliced `{}` at {:?} with {} input(s)",
                    fn_name,
                    entry.source_id,
                    entry.inputs.len(),
                );
            }
            Err(_returned) => {
                log::warn!(
                    "fusion::fuse: built kernel for region {:?} but splice \
                     failed to locate the target",
                    entry.source_id,
                );
            }
        }
    }
    Ok(())
}

/// One free-var input slot resolved during walker analysis.
#[derive(Debug, Clone)]
pub(crate) struct FreeVarInput {
    pub(crate) bind_id: crate::BindId,
    pub(crate) name: arcstr::ArcStr,
    /// Kernel-input classification, computed once from the binding's
    /// GIR type. Drives the `RegionInput.kind` at build time.
    pub(crate) kind: crate::fusion::lowering::RegionInputKind,
    /// Full graphix type — used to construct the runtime feeder Node
    /// (`genn::reference`), which needs the complete `Type`.
    pub(crate) typ: crate::typ::Type,
}

/// Owned data extracted from a Region candidate so we can free
/// the walker's borrow into the tree before splicing.
struct RegionPlan {
    source_id: crate::expr::ExprId,
    body: crate::expr::Expr,
    inputs: Vec<FreeVarInput>,
}

/// Walk the candidate subtree, collect every external Ref's
/// BindId (referenced but NOT bound inside the body), and resolve
/// each to its name + GIR-input classification via `ctx.env.by_id`.
///
/// Because `CallSite::refs` recurses into a statically-resolved
/// lambda's body (via `GXLambda::refs`), a lambda's captures
/// transitively surface here as external Refs of the enclosing
/// region — so this pre-pass registers them as parent kernel inputs,
/// and `emit_lambda_call`'s capture forwarding finds them via
/// `lookup_local_by_bind_id`. This is the mechanism that makes
/// closure conversion's capture cascade automatic.
///
/// Slots whose type has no kernel-input representation (function
/// types, bare `String`/`Null`/`Unit`) are skipped — the builder
/// fails those Refs when emitting them and the candidate stays
/// unfused (correct fall-back to the interpreter).
pub(crate) fn collect_region_inputs<R: crate::Rt, E: crate::UserEvent>(
    subtree: &dyn crate::Update<R, E>,
    ctx: &crate::ExecCtx<R, E>,
) -> Vec<FreeVarInput> {
    let mut refs = crate::Refs::default();
    subtree.refs(&mut refs);
    let mut out: Vec<FreeVarInput> = Vec::new();
    let mut seen: ahash::AHashSet<crate::BindId> = ahash::AHashSet::default();
    refs.with_external_refs(|id| {
        if !seen.insert(id) {
            return;
        }
        let Some(b) = ctx.env.by_id.get(&id) else { return };
        let Some(frozen) = crate::gir::freeze_concrete(&b.typ) else {
            return;
        };
        let Some(kind) =
            crate::fusion::lowering::type_to_region_input_kind(frozen)
        else {
            return;
        };
        out.push(FreeVarInput {
            bind_id: id,
            name: arcstr::ArcStr::from(b.name.as_str()),
            kind,
            typ: b.typ.clone(),
        });
    });
    out
}

/// Find a Node anywhere in the tree whose `spec().id == target`.
/// Returns the owning `&Node<R, E>` reference if found — callers
/// can call any `Update` trait method (including `view()` for
/// further NodeView dispatch).
pub(crate) fn find_node_by_id<'a, R: crate::Rt, E: crate::UserEvent>(
    node: &'a crate::Node<R, E>,
    target: crate::expr::ExprId,
) -> Option<&'a crate::Node<R, E>> {
    if node.spec().id == target {
        return Some(node);
    }
    use crate::NodeView;
    match node.view() {
        NodeView::Block(blk) => {
            for child in blk.children.iter() {
                if let Some(found) = find_node_by_id::<R, E>(child, target) {
                    return Some(found);
                }
            }
            None
        }
        NodeView::Module(m) => {
            for child in m.nodes.iter() {
                if let Some(found) = find_node_by_id::<R, E>(child, target) {
                    return Some(found);
                }
            }
            None
        }
        NodeView::Bind(b) => find_node_by_id::<R, E>(&b.node, target),
        // Other container nodes (callsite args, select arms, etc.)
        // can't host Region candidates today — the walker doesn't
        // descend into them.
        _ => None,
    }
}

/// Find the descendant Node whose `spec().id == target` (or
/// `node` itself if it matches) and replace it with
/// `replacement`. On match: returns `Ok(old_node)`. On miss:
/// returns `Err(replacement)` unchanged so the caller can drop or
/// retry.
pub fn splice_into<R: crate::Rt, E: crate::UserEvent>(
    node: &mut crate::Node<R, E>,
    target: crate::expr::ExprId,
    replacement: crate::Node<R, E>,
) -> std::result::Result<crate::Node<R, E>, crate::Node<R, E>> {
    if node.spec().id == target {
        Ok(std::mem::replace(node, replacement))
    } else {
        node.splice_child(target, replacement)
    }
}
