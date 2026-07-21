//! Cranelift JIT backend: compile a fused region's node graph to
//! native machine code.
//!
//! Body code generation is DISTRIBUTED (`design/distributed_jit.md`):
//! each node's `Update::emit_clif` (and each builtin's
//! `Apply::emit_clif`) emits its own computation into the open kernel,
//! recursing into children via `child.emit_clif(cx)`. This module is
//! the shared machinery those impls call: the per-context [`Jit`]
//! (declare/define/wrap pipeline + `by_kernel` cache), [`BodyCx`] /
//! [`JitEnv`] / [`LowerCtx`] (the emission context), [`CompiledExpr`]
//! (the SSA result shape), the `emit_*_node` helpers the node impls
//! delegate to, the kernel entry/return/pending machinery, and the
//! scalar codegen primitives (`compile_bin`/`compile_cmp`/
//! `compile_cast`). The loop scaffolds HOF builtins reuse live in
//! [`scaffold`].
//!
//! ## Calling convention
//!
//! The compiled function uses the host platform's default C calling
//! convention (SystemV on Linux, Windows-fastcall on Windows) and
//! the unified Value ABI (`design/unified_value_abi.md`): parameters
//! in SOURCE order — defined once by [`KernelSig::abi_params`] and
//! consumed by every ABI site (the signature builder, the wrapper
//! unpacker, the entry binder, and the runtime arg packer in
//! `kernel`) — each a two-word `(disc, payload)` pair. The disc is
//! an `I64` carrying the genuine one-hot `Value` discriminant plus
//! the TAINT/STALE tag bits; the payload is the genuine `Value`
//! payload word (`ValArray` bits, `ArcStr` bits, a value-shape's
//! payload), except that a SCALAR payload keeps its natural CLIF
//! register class (`F64`, `I32`, …) interior to and between kernels
//! — the widened memory form appears only at the wrapper/packer
//! seams (`scalar_to_payload_i64` / `pack_value_to_u64`).
//!
//! The runtime calls through the uniform-slot [`WrappedKernel`]
//! (args*, out* — see `define_wrapper`).

mod abi;
mod body;
mod call;
mod flow;
mod jit;
mod lower;
mod nodes;
/// The HOF loop scaffolds (`emit_map_loop` & co.) shared by the
/// direct node path's HOF emitters (Stage D2 of
/// `design/distributed_jit.md`).
pub mod scaffold;
mod scalar;
mod select;

pub use self::abi::{
    CompiledExpr, array_result, emit_forced, emit_forced_keep, scalar_result,
};
pub(crate) use self::abi::{STALE, TAINT, prim_to_value_disc};
pub use self::body::{
    BodyCx, ensure_owned_composite_src, ensure_owned_value_src, node_composite_source,
    node_is_bottom, node_loop_invariant_ref,
};
pub use self::call::CompositeSource;
pub(crate) use self::call::{emit_dyncall_node, emit_lambda_call_node};
pub(crate) use self::flow::{emit_block_node, emit_qop_node};
pub use self::jit::{
    Jit, JitCtx, WrappedKernel, WrapperFn, compile_kernel_with_callees_direct,
    pack_value_to_u64, unpack_u64_to_value,
};
pub use self::lower::{KernelStrings, KernelValues};
pub(crate) use self::nodes::{
    call_result_needs_value_widening, emit_arith_node, emit_array_ref_node,
    emit_array_slice_node, emit_bool_node, emit_cast_node, emit_checked_arith_node,
    emit_cmp_node, emit_connect_node, emit_const_node, emit_map_new_node,
    emit_map_ref_node, emit_neg_node, emit_not_node, emit_owned_value_operand_node,
    emit_ref_node, emit_string_interpolate_node, emit_struct_new_node,
    emit_struct_ref_node, emit_struct_with_node, emit_tuple_new_node,
    emit_tuple_ref_node, emit_variant_new_node, widen_result_to_value,
};
pub(crate) use self::select::{emit_select_node, slot_state_sites};
