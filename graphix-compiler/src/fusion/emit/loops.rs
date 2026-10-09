//! The fused loops of the collection intrinsics (`node/collection.rs`):
//! each traversal's gate and its loop over the scaffold.

use super::{self as emit, BodyCx, CompiledExpr, CompositeSource, scaffold};
use crate::{
    BindId, Node, Rt, UserEvent,
    fusion::kernel_abi::{self, AbiKind, PrimType},
    node::{collection::Flavor, lambda::GXLambda},
    typ::{FnArgKind, Type},
};
use anyhow::Result;
use arcstr::ArcStr;
use cranelift_codegen::ir::{InstBuilder, Value as ClifValue};
use netidx_value::Typ;

#[derive(Debug)]
pub(crate) struct CallbackParam {
    name: ArcStr,
    id: Option<BindId>,
    binds: Vec<(BindId, usize)>,
}

impl CallbackParam {
    /// The loop's element bind for this parameter.
    fn elem<'a>(
        &'a self,
        typ: &'a Type,
        leaves: &'a [scaffold::Leaf],
    ) -> scaffold::HofElem<'a> {
        scaffold::HofElem { name: &self.name, id: self.id, typ, leaves }
    }
}

/// The callback's `index`-th positional parameter; `None` for a
/// callback with labeled parameters, which the collection interprets.
pub(crate) fn callback_param<R: Rt, E: UserEvent>(
    callback: &GXLambda<R, E>,
    index: usize,
    fallback: ArcStr,
) -> Option<CallbackParam> {
    if callback.typ().first_positional() > 0 {
        return None;
    }
    match callback.args().get(index)?.tuple_leaves() {
        Some(binds) => Some(CallbackParam { name: fallback, id: None, binds }),
        None => {
            let name = match &callback.typ().args.get(index)?.kind {
                FnArgKind::Positional { name: Some(name) }
                | FnArgKind::Labeled { name, .. } => name.clone(),
                _ => return None,
            };
            Some(CallbackParam {
                name,
                id: callback.args()[index].single_bind_id(),
                binds: Vec::new(),
            })
        }
    }
}

fn bindable_array_element(
    typ: &Type,
    binds: &[(BindId, usize)],
) -> Option<(Type, scaffold::Leaves)> {
    let typ = kernel_abi::freeze_for_abi_normalized(typ)?;
    let leaves = scaffold::elem_leaves(&typ, binds)?;
    match kernel_abi::abi_kind(&typ) {
        Some(
            AbiKind::Scalar(_)
            | AbiKind::Array
            | AbiKind::Tuple
            | AbiKind::Struct
            | AbiKind::String
            | AbiKind::Variant
            | AbiKind::Nullable
            | AbiKind::Value,
        ) => Some((typ, leaves)),
        _ => None,
    }
}

fn is_unit_or_null(typ: &Type) -> bool {
    matches!(kernel_abi::abi_kind(typ), Some(AbiKind::Unit | AbiKind::Null))
}

/// Whether a frozen type admits `null`, filter_map's drop marker.
/// An unknown shape answers true, so the caller keeps interpreting.
fn frozen_may_be_null(t: &Type) -> bool {
    t.with_deref(|t| match t {
        Some(Type::Primitive(p)) => p.contains(Typ::Null),
        Some(Type::Set(ms)) => ms.iter().any(frozen_may_be_null),
        Some(
            Type::Array(_)
            | Type::List(_)
            | Type::Tuple(_)
            | Type::Struct(_)
            | Type::Variant(_, _, _)
            | Type::Fn(_)
            | Type::Error(_)
            | Type::Map { .. }
            | Type::Abstract { .. }
            | Type::ByRef(..),
        ) => false,
        _ => true,
    })
}

/// Emit a List/Map HOF source: marshal the collection Value owned and
/// flatten it to a fresh ValArray through `helper`, which consumes it:
/// the [`scaffold::ArraySrc`] that owns the flattened array.
fn emit_flattened_source<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    helper: &'static str,
) -> Result<scaffold::ArraySrc> {
    let value = emit::emit_owned_value_operand_node(cx, source)?;
    let flatten = cx.helper(helper)?;
    let call = cx.b.ins().call(flatten, &[value.disc, value.payload]);
    let ptr = cx.b.inst_results(call)[0];
    Ok(scaffold::ArraySrc { ptr, disc: value.disc, ownership: CompositeSource::Owned })
}

/// The exit boundary for collection-returning loops: consume the
/// loop's finalize'd ValArray and rebuild the collection Value
/// (`graphix_valarray_into_list` / `graphix_valarray_into_cmap`).
fn convert_collection_result(
    cx: &mut BodyCx,
    ptr: ClifValue,
    helper: &'static str,
) -> Result<CompiledExpr> {
    let f = cx.helper(helper)?;
    let call = cx.b.ins().call(f, &[ptr]);
    let rs = cx.b.inst_results(call);
    let (disc, payload) = (rs[0], rs[1]);
    Ok(CompiledExpr::new(disc, payload))
}

impl Flavor {
    /// Emit the loop source as the scaffold's ValArray, whose disc drives
    /// the firing wrap.
    fn emit_source<R: Rt, E: UserEvent>(
        self,
        cx: &mut BodyCx,
        source: &Node<R, E>,
    ) -> Result<scaffold::ArraySrc> {
        match self {
            Self::Array => {
                let ownership = emit::node_composite_source(source);
                let array = source.emit_clif(cx)?;
                Ok(scaffold::ArraySrc { ptr: array.payload, disc: array.disc, ownership })
            }
            Self::List => emit_flattened_source(cx, source, "graphix_list_to_valarray"),
            Self::CMap => emit_flattened_source(cx, source, "graphix_cmap_to_pairs"),
        }
    }

    /// The exit boundary for collection-returning loops: the loop's
    /// finalized ValArray as this flavor's collection Value.
    fn emit_result(self, cx: &mut BodyCx, ptr: ClifValue) -> Result<CompiledExpr> {
        match self {
            Self::Array => Ok(emit::array_result(cx, ptr)),
            Self::List => {
                convert_collection_result(cx, ptr, "graphix_valarray_into_list")
            }
            Self::CMap => {
                convert_collection_result(cx, ptr, "graphix_valarray_into_cmap")
            }
        }
    }
}

/// The filter/find gate: the callback must compile to a bool scalar.
fn predicate_is_bool<R: Rt, E: UserEvent>(body: &Node<R, E>) -> bool {
    kernel_abi::freeze_for_abi_normalized(body.typ())
        .as_ref()
        .and_then(|typ| kernel_abi::scalar_prim(typ))
        == Some(PrimType::Bool)
}

/// Emit a loop over `source` built by `emit`, which gets the flattened
/// source; the source's firing folds into the loop's result.
fn emit_loop<'a, 'f, 'c, R: Rt, E: UserEvent>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    source: &Node<R, E>,
    flavor: Flavor,
    emit: impl FnOnce(
        &mut BodyCx<'a, 'f, 'c>,
        scaffold::ArraySrc,
    ) -> Result<(CompiledExpr, scaffold::SlotFlags)>,
) -> Result<Option<CompiledExpr>> {
    let src = flavor.emit_source(cx, source)?;
    let disc = src.disc;
    let (result, flags) = emit(cx, src)?;
    Ok(Some(flags.apply(cx, result, disc)))
}

pub(crate) fn emit_init_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    body: &Node<R, E>,
    param: &CallbackParam,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    if !param.binds.is_empty() {
        return Ok(None);
    }
    let count_prim = match kernel_abi::freeze_for_abi_normalized(source.typ())
        .as_ref()
        .and_then(|typ| kernel_abi::scalar_prim(typ))
    {
        Some(prim) if prim.is_integer() => prim,
        _ => return Ok(None),
    };
    let Some(output_type) = kernel_abi::freeze_for_abi_normalized(body.typ()) else {
        return Ok(None);
    };
    if is_unit_or_null(&output_type) {
        return Ok(None);
    }
    let count = source.emit_clif(cx)?;
    let output_source = emit::node_composite_source(body);
    let sites = emit::slot_state_sites(cx, body);
    let (ptr, flags, count_disc) = scaffold::emit_init_loop(
        cx,
        count.payload,
        count.disc,
        count_prim,
        &param.name,
        param.id,
        &output_type,
        output_source,
        &sites,
        |cx| body.emit_clif(cx),
    )?;
    let result = flavor.emit_result(cx, ptr)?;
    // The firing wrap must see an over-limit count as a tainted source.
    Ok(Some(flags.apply(cx, result, count_disc)))
}

pub(crate) fn emit_map_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    body: &Node<R, E>,
    param: &CallbackParam,
    element_type: &Type,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    let Some((element_type, leaves)) = bindable_array_element(element_type, &param.binds)
    else {
        return Ok(None);
    };
    let Some(output_type) = kernel_abi::freeze_for_abi_normalized(body.typ()) else {
        return Ok(None);
    };
    if is_unit_or_null(&output_type) {
        return Ok(None);
    }
    emit_loop(cx, source, flavor, |cx, src| {
        let output_source = emit::node_composite_source(body);
        let sites = emit::slot_state_sites(cx, body);
        let (ptr, flags) = scaffold::emit_map_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            &output_type,
            output_source,
            &sites,
            |cx| body.emit_clif(cx),
        )?;
        Ok((flavor.emit_result(cx, ptr)?, flags))
    })
}

pub(crate) fn emit_filter_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    body: &Node<R, E>,
    param: &CallbackParam,
    element_type: &Type,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    let Some((element_type, leaves)) = bindable_array_element(element_type, &param.binds)
    else {
        return Ok(None);
    };
    if !predicate_is_bool(body) {
        return Ok(None);
    }
    emit_loop(cx, source, flavor, |cx, src| {
        let sites = emit::slot_state_sites(cx, body);
        let (ptr, flags) = scaffold::emit_filter_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            &sites,
            |cx| body.emit_clif(cx),
        )?;
        Ok((flavor.emit_result(cx, ptr)?, flags))
    })
}

pub(crate) fn emit_filter_map_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    body: &Node<R, E>,
    param: &CallbackParam,
    element_type: &Type,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    let Some(output_type) = kernel_abi::freeze_for_abi_normalized(body.typ()) else {
        return Ok(None);
    };
    let Some(output_element) = kernel_abi::nullable_inner(&output_type) else {
        // A callback that can never return null makes filter_map a map.
        if frozen_may_be_null(&output_type) {
            return Ok(None);
        }
        return emit_map_kind(cx, source, body, param, element_type, flavor);
    };
    let Some((element_type, leaves)) = bindable_array_element(element_type, &param.binds)
    else {
        return Ok(None);
    };
    if is_unit_or_null(&output_element) {
        return Ok(None);
    }
    emit_loop(cx, source, flavor, |cx, src| {
        let output_source = emit::node_composite_source(body);
        let sites = emit::slot_state_sites(cx, body);
        let (ptr, flags) = scaffold::emit_filter_map_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            &output_element,
            output_source,
            &sites,
            |cx| body.emit_clif(cx),
        )?;
        Ok((flavor.emit_result(cx, ptr)?, flags))
    })
}

pub(crate) fn emit_flat_map_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    body: &Node<R, E>,
    param: &CallbackParam,
    element_type: &Type,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    let Some((element_type, leaves)) = bindable_array_element(element_type, &param.binds)
    else {
        return Ok(None);
    };
    // A List callback's return is an opaque Value; the extend helper walks
    // it. No Map flat_map intrinsic exists.
    let output_kind = kernel_abi::freeze_for_abi_normalized(body.typ())
        .as_ref()
        .and_then(|typ| kernel_abi::abi_kind(typ));
    let extend = match (flavor, output_kind) {
        (Flavor::Array, Some(AbiKind::Array)) => scaffold::FlatMapExtend::Array,
        (Flavor::List, Some(AbiKind::Value)) => scaffold::FlatMapExtend::List,
        _ => return Ok(None),
    };
    emit_loop(cx, source, flavor, |cx, src| {
        let body_source = emit::node_composite_source(body);
        let sites = emit::slot_state_sites(cx, body);
        let (ptr, flags) = scaffold::emit_flat_map_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            extend,
            &sites,
            |cx| {
                let value = body.emit_clif(cx)?;
                match extend {
                    scaffold::FlatMapExtend::Array => {
                        let payload = emit::ensure_owned_composite_src(
                            cx,
                            body_source,
                            value.payload,
                        )?;
                        Ok(CompiledExpr::new(value.disc, payload))
                    }
                    scaffold::FlatMapExtend::List => {
                        let (disc, payload) = emit::ensure_owned_value_src(
                            cx,
                            body_source,
                            value.disc,
                            value.payload,
                        )?;
                        Ok(CompiledExpr::new(disc, payload))
                    }
                }
            },
        )?;
        Ok((flavor.emit_result(cx, ptr)?, flags))
    })
}

pub(crate) fn emit_find_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    body: &Node<R, E>,
    param: &CallbackParam,
    element_type: &Type,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    let Some((element_type, leaves)) = bindable_array_element(element_type, &param.binds)
    else {
        return Ok(None);
    };
    if !predicate_is_bool(body) {
        return Ok(None);
    }
    emit_loop(cx, source, flavor, |cx, src| {
        let sites = emit::slot_state_sites(cx, body);
        let ((disc, payload), flags) = scaffold::emit_find_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            &sites,
            |cx| body.emit_clif(cx),
        )?;
        Ok((CompiledExpr::new(disc, payload), flags))
    })
}

pub(crate) fn emit_find_map_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    body: &Node<R, E>,
    param: &CallbackParam,
    element_type: &Type,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    let Some((element_type, leaves)) = bindable_array_element(element_type, &param.binds)
    else {
        return Ok(None);
    };
    let output_is_nullable = matches!(
        kernel_abi::freeze_for_abi_normalized(body.typ())
            .as_ref()
            .and_then(|typ| kernel_abi::abi_kind(typ)),
        Some(AbiKind::Nullable)
    );
    if !output_is_nullable {
        return Ok(None);
    }
    emit_loop(cx, source, flavor, |cx, src| {
        let body_source = emit::node_composite_source(body);
        let sites = emit::slot_state_sites(cx, body);
        let ((disc, payload), flags) = scaffold::emit_find_map_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            &sites,
            |cx| {
                let value = body.emit_clif(cx)?;
                emit::ensure_owned_value_src(cx, body_source, value.disc, value.payload)
            },
        )?;
        Ok((CompiledExpr::new(disc, payload), flags))
    })
}

/// A fold's callback parts: its init, body and parameters.
pub(crate) struct FoldParts<'a, R: Rt, E: UserEvent> {
    pub(crate) init: &'a Node<R, E>,
    pub(crate) body: &'a Node<R, E>,
    pub(crate) acc: &'a CallbackParam,
    pub(crate) element: &'a CallbackParam,
}

/// The fold kind. A List- or Map-valued accumulator has no `FoldAcc`
/// carry and stays interpreted.
pub(crate) fn emit_fold_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    fold: FoldParts<R, E>,
    acc_type: &Type,
    element_type: &Type,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    let FoldParts { init, body, acc, element } = fold;
    let Some((element_type, element_leaves)) =
        bindable_array_element(element_type, &element.binds)
    else {
        return Ok(None);
    };
    let Some(acc_type) = kernel_abi::freeze_for_abi_normalized(acc_type) else {
        return Ok(None);
    };
    // A Bottom-typed body unifies with any acc type but emits a
    // shapeless placeholder that violates the owned-acc discipline.
    if emit::node_is_bottom(body) {
        return Ok(None);
    }
    let acc_leaves;
    let acc_shape = match kernel_abi::abi_kind(&acc_type) {
        Some(AbiKind::Scalar(prim)) if acc.binds.is_empty() => {
            scaffold::FoldAcc::Scalar(prim)
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let Some(leaves) = scaffold::elem_leaves(&acc_type, &acc.binds) else {
                return Ok(None);
            };
            acc_leaves = leaves;
            scaffold::FoldAcc::Composite {
                init_src: emit::node_composite_source(init),
                body_src: emit::node_composite_source(body),
                leaves: &acc_leaves,
            }
        }
        Some(AbiKind::String) if acc.binds.is_empty() => scaffold::FoldAcc::Str,
        // The init and body may emit narrower members of the acc union;
        // `emit_owned_value_operand_node` normalizes them to an owned Value.
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value)
            if acc.binds.is_empty() =>
        {
            for n in [init, body] {
                match kernel_abi::abi_kind(n.typ()) {
                    Some(AbiKind::Unit) | None => return Ok(None),
                    Some(_) => {}
                }
            }
            scaffold::FoldAcc::Value {
                init_src: CompositeSource::Owned,
                body_src: CompositeSource::Owned,
            }
        }
        _ => return Ok(None),
    };
    let value_acc = matches!(acc_shape, scaffold::FoldAcc::Value { .. });
    let operand = move |cx: &mut BodyCx, n: &Node<R, E>| {
        if value_acc {
            emit::emit_owned_value_operand_node(cx, n)
        } else {
            n.emit_clif(cx)
        }
    };
    emit_loop(cx, source, flavor, |cx, src| {
        let sites = emit::slot_state_sites(cx, body);
        scaffold::emit_fold_loop(
            cx,
            src,
            acc_shape,
            &acc.name,
            acc.id,
            &element.elem(&element_type, &element_leaves),
            &sites,
            |cx| operand(cx, init),
            |cx| operand(cx, body),
        )
    })
}
