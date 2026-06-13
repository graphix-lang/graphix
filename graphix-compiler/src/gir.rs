//! Kernel-ABI vocabulary shared by fusion analysis and the JIT
//! backend: the [`KernelSig`] family (re-exported from
//! [`crate::kernel_abi`] — signature, per-kind input slots, ABI
//! derivation, type freezing) plus the scalar codegen operator enums
//! ([`BinOp`], [`CmpOp`], [`BoolOp`]) and the cross-kernel call
//! signature ([`KnownFusedFn`]).
//!
//! The GIR intermediate representation that used to live here
//! (`GirExpr`/`GirOp`/`GirStmt`) was deleted with the direct
//! node-emission flip (`design/distributed_jit.md`): body code
//! generation is `Update::emit_clif` / `Apply::emit_clif` walking the
//! node graph, so there is no IR between the nodes and CLIF.

use crate::typ::Type;

pub use crate::kernel_abi::*;


// ─── Operators ───────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Lte,
    Gte,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BoolOp {
    And,
    Or,
}

// ─── Cross-kernel call signature ─────────────────────────────────

/// Caller-side signature of a successfully-built lambda kernel. When
/// one fused kernel calls another, the call site marshals its args
/// against this (the types here were resolved + frozen at BUILD time,
/// so they are the authority — freezing caller-side node types
/// re-rejects abstract Refs, #218).
#[derive(Debug, Clone)]
pub struct KnownFusedFn {
    /// Flat per-input types in slot order: formal args first, then
    /// closure-converted captures.
    pub arg_types: Vec<Type>,
    /// Return type.
    pub return_type: Type,
    /// The `let` binding this kernel was built from, when known —
    /// names shadow, ids don't, so a name-resolved call must carry a
    /// matching fnode `Ref` id. Without the check, a body call to a
    /// shadowed same-name outer lambda (`let f = …; let f = |n|
    /// f(n) * 2`) resolves against the kernel ITSELF (#206: infinite
    /// native self-call, stack overflow).
    pub self_bind: Option<crate::BindId>,
}

