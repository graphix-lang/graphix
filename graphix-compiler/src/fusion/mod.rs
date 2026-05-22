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
/// **Current scope**: no-op stub. The pipeline pieces exist
/// ([`walk`], [`builder`], [`splice`]) but they're not wired
/// together yet — the iterations called out in
/// `playful-growing-meerkat.md` add the wiring case-by-case as
/// each candidate kind gains build + splice support.
///
/// The function takes `&mut Node` rather than `&mut [Node]`
/// because — with `load()`'s Module wrapping in place — every
/// compile produces one Node (a module containing the file's
/// graph, or a single inline expression). Cross-Bind dependencies
/// are visible because they're inside the single Node tree.
pub fn fuse<R: crate::Rt, E: crate::UserEvent>(
    _node: &mut crate::Node<R, E>,
    _ctx: &mut crate::ExecCtx<R, E>,
) -> anyhow::Result<()> {
    // Stub — see plan for iteration sequence.
    Ok(())
}
