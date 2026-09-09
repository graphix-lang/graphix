//! Cranelift JIT backend: compile a fused region's node graph to
//! native machine code.
//!
//! Code generation is distributed: each node's `Update::emit_clif` (and
//! each builtin's `Apply::emit_clif`) emits its own computation into the
//! open kernel and recurses into its children. This module holds the
//! shared machinery: [`Jit`] (declare/define/wrap + the `by_kernel`
//! cache), [`BodyCx`] / [`JitEnv`] / [`LowerCtx`] (the emission
//! context), [`CompiledExpr`] (the SSA result shape), the `emit_*_node`
//! helpers, and the scalar codegen primitives; HOF loops live in
//! [`scaffold`].
//!
//! Calling convention: the platform's default C convention. Parameters
//! come in source order from [`KernelSig::abi_params`], each a
//! `(disc, payload)` pair: the disc is an `I64` holding the `Value`
//! discriminant plus the TAINT/STALE bits; a scalar payload keeps its
//! natural CLIF register class between kernels and is widened only at
//! the wrapper/packer seams. The runtime calls through [`WrappedKernel`].

mod abi;
mod body;
mod call;
mod flow;
mod jit;
mod lower;
mod nodes;
/// The HOF loop scaffolds (`emit_map_loop` & co.) shared by the node
/// HOF emitters.
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
pub(crate) use self::call::{emit_builtin_call_node, emit_lambda_call_node};
pub(crate) use self::flow::{emit_block_node, emit_qop_node};
pub use self::jit::{
    Jit, JitCtx, WrappedKernel, WrapperFn, compile_kernel_with_callees_direct,
    pack_value_to_u64, unpack_u64_to_value,
};
pub use self::lower::{KernelStrings, KernelValues};
pub(crate) use self::nodes::{
    call_result_needs_value_widening, emit_abstract_ref_node, emit_arith_node,
    emit_array_ref_node, emit_array_slice_node, emit_bool_node, emit_cast_node,
    emit_checked_arith_node, emit_cmp_node, emit_const_node, emit_construct_node,
    emit_list_new_node, emit_map_new_node, emit_map_ref_node, emit_neg_node,
    emit_not_node, emit_owned_value_operand_node, emit_ref_node,
    emit_string_interpolate_node, emit_struct_new_node, emit_struct_ref_node,
    emit_struct_with_node, emit_tuple_new_node, emit_tuple_ref_node,
    emit_variant_new_node, widen_result_to_value,
};
pub(crate) use self::select::{emit_select_node, slot_state_sites};
