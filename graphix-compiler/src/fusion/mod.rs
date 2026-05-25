//! Fusion: graphix's KIR-emission + kernel-build + native-code
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
//!    body's KIR via [`lowering::emit_expr`], wraps the result in a
//!    [`crate::kernel_ir::KirKernel`]. Calls into the JIT
//!    ([`crate::kir_jit`]) to produce native code.
//!
//! 3. **Splicer** ([`builder::splice`]): replaces the original
//!    `Node` in the runtime's node graph with a
//!    [`builder::FusedKernel`] wrapper that holds the JIT artifact
//!    and dispatches at `Update::update` time.
//!
//! 4. **Lowering** ([`lowering`]): the per-`ExprKind` → KirExpr
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
/// builds KIR kernels, JIT-compiles them, splices them into the
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
                        (source_id, body.clone())
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
                let subtree = find_node_by_id::<R, E>(&**node, source_id)?;
                let inputs = collect_scalar_inputs::<R, E>(subtree, ctx);
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
                kind: crate::fusion::RegionInputKind::Prim(fv.prim),
                source: crate::fusion::RegionInputSource::Binding,
            })
            .collect();
        // Walk the body for sync-builtin Apply sites so the kernel
        // build can lower them via `FnSource::Builtin` DynCalls.
        // Site discovery doesn't bind the runtime — the actual
        // `Apply` impl is constructed at `KirNode::new` time via
        // `pre_init_builtin_slots`.
        let discovery = crate::fusion::discover_builtin_calls::<R, E>(
            &entry.body,
            ctx,
            &crate::expr::ModPath::root(),
        );
        let built = match builder::build_region(
            &fake_candidate::<R, E>(&entry),
            &fn_name,
            &region_inputs,
            discovery,
        ) {
            Ok(b) => b,
            Err(_) => continue,
        };
        // Skip kernels whose return type isn't representable at the
        // FusedKernel boundary. `Null` only appears as an
        // intermediate inside fused expressions — Nullable is the
        // proper kernel-return shape — and `Unit` is a side-effect-
        // only marker that we don't expose to the runtime here.
        match built.kernel.return_type {
            crate::kernel_ir::KirType::Prim(_)
            | crate::kernel_ir::KirType::Array(_)
            | crate::kernel_ir::KirType::Tuple(_)
            | crate::kernel_ir::KirType::Struct(_)
            | crate::kernel_ir::KirType::Variant(_)
            | crate::kernel_ir::KirType::Nullable(_)
            | crate::kernel_ir::KirType::String => {}
            crate::kernel_ir::KirType::Unit
            | crate::kernel_ir::KirType::Null => continue,
        }
        // JIT-compile unless `JitDisabled` is set. The interp
        // fallback path inside `FusedKernel::update` handles the
        // None case via `kir_interp`.
        let wrapped: Option<std::sync::Arc<crate::kir_jit::WrappedKernel>> =
            if jit_disabled {
                None
            } else {
                match crate::kir_jit::compile_kernel_with_callees(
                    &mut ctx.jit.lock(),
                    &built.kernel,
                    &Default::default(),
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
struct FreeVarInput {
    bind_id: crate::BindId,
    name: arcstr::ArcStr,
    prim: crate::kernel_ir::PrimType,
    typ: crate::typ::Type,
}

/// Owned data extracted from a Region candidate so we can free
/// the walker's borrow into the tree before splicing.
struct RegionPlan {
    source_id: crate::expr::ExprId,
    body: crate::expr::Expr,
    inputs: Vec<FreeVarInput>,
}

/// Reconstruct a Region-shaped `KernelCandidate` from a plan. The
/// builder's `build_region` only reads the `Region` body, so the
/// other candidate fields are filler.
fn fake_candidate<'a, R: crate::Rt, E: crate::UserEvent>(
    plan: &'a RegionPlan,
) -> KernelCandidate<'a, R, E> {
    KernelCandidate {
        kind: CandidateKind::Region {
            source_id: plan.source_id,
            body: &plan.body,
        },
        scope: crate::Scope::root(),
        source_id: plan.source_id,
    }
}

/// Walk the candidate subtree, collect every external Ref's
/// BindId (referenced but NOT bound inside the body), and resolve
/// each to its name + primitive type via `ctx.env.by_id`. Slots
/// whose type isn't a primitive (composite, function, etc.) are
/// skipped for this iteration — the builder will fail those Refs
/// when it tries to emit them and the candidate stays unfused.
fn collect_scalar_inputs<R: crate::Rt, E: crate::UserEvent>(
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
        let Some(prim) = crate::kernel_ir::PrimType::from_type(&b.typ) else {
            return;
        };
        out.push(FreeVarInput {
            bind_id: id,
            name: arcstr::ArcStr::from(b.name.as_str()),
            prim,
            typ: b.typ.clone(),
        });
    });
    out
}

/// Find a Node anywhere in the tree whose `spec().id == target`.
/// Returns a borrowed reference to it if found.
fn find_node_by_id<'a, R: crate::Rt, E: crate::UserEvent>(
    node: &'a dyn crate::Update<R, E>,
    target: crate::expr::ExprId,
) -> Option<&'a dyn crate::Update<R, E>> {
    if node.spec().id == target {
        return Some(node);
    }
    use crate::NodeView;
    match node.view() {
        NodeView::Block(blk) => {
            for child in blk.children.iter() {
                if let Some(found) = find_node_by_id::<R, E>(&**child, target) {
                    return Some(found);
                }
            }
            None
        }
        NodeView::Module(m) => {
            for child in m.nodes.iter() {
                if let Some(found) = find_node_by_id::<R, E>(&**child, target) {
                    return Some(found);
                }
            }
            None
        }
        NodeView::Bind(b) => find_node_by_id::<R, E>(&*b.node, target),
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
