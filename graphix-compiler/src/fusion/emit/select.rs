//! `select` emission: scrutinee classification, pattern
//! conditions, arm dispatch, and the merge-shape protocol.

use crate::{
    BindId, Node, NodeView, Rt, Update, UserEvent,
    fusion::{
        self,
        kernel_abi::{self, AbiKind, PrimType},
    },
    node::{
        op::CmpOp,
        pattern::{PatternNode, SliceKind, StructPatternNode, set_members},
        select::Select,
    },
    stack,
    typ::Type,
};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use cranelift_codegen::ir::{
    Block, BlockArg, InstBuilder, Value as ClifValue, condcodes::IntCC, types,
};
use netidx_value::{Typ, Value};
use smallvec::SmallVec;

use super::{
    abi::{
        CompiledExpr, LocalKind, OwnedKind, STALE, TAINT, ValueVar, bind_local,
        clean_disc, is_tainted, is_untainted, local_payload_ty, owned_words,
        propagate_flags, propagate_stale, propagate_taint, scalar_disc, value_disc,
    },
    body::{BodyCx, ensure_owned_composite_src, fold_stale, node_composite_source},
    call::{CompositeSource, emit_drop_local},
    flow::{emit_discard_result, emit_scope_drops},
    lower::resolve_node_typ,
    nodes::{emit_bottom_of_kind, emit_owned_value_operand_node},
    scalar::{
        ElementRead, cast_u64_to_prim, compile_cmp, compile_const, element_read_helper,
        prim_to_clif, struct_get_helper, valarray_get_helper, variant_payload_helper,
    },
};

/// How a `select`'s arms merge into one result, derived from the
/// select node's frozen result type. Every shape phis (disc, payload),
/// so a tainted arm value propagates its bottom to the merged result.
#[derive(Clone, Copy)]
enum SelectMerge {
    Scalar(PrimType),
    Value,
    Composite,
    String,
}

impl SelectMerge {
    fn abi_kind(self) -> AbiKind {
        match self {
            SelectMerge::Scalar(p) => AbiKind::Scalar(p),
            SelectMerge::Value => AbiKind::Value,
            SelectMerge::Composite => AbiKind::Tuple,
            SelectMerge::String => AbiKind::String,
        }
    }
}

/// The select scrutinee, emitted once up front; every arm condition
/// and pattern bind reuses these SSA values. `disc` carries the
/// scrutinee's flags: a tainted scrutinee takes no arm (the chain
/// branches to the miss block), and its STALE bit folds into the
/// select's fire.
#[derive(Clone, Copy)]
pub(super) enum SelectScrut {
    Scalar {
        disc: ClifValue,
        value: ClifValue,
        prim: PrimType,
    },
    Value {
        kind: ValueKind,
        disc: ClifValue,
        payload: ClifValue,
    },
    /// An array/tuple/struct scrutinee whose pointer stays live across
    /// the whole arm chain; structural patterns read elements through it.
    Composite {
        disc: ClifValue,
        ptr: ClifValue,
    },
}

/// What a value scrutinee holds.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum ValueKind {
    Variant,
    /// An option or a result.
    Nullable,
    /// A primitive union, or any other value.
    Value,
    String,
}

impl SelectScrut {
    pub(super) fn disc(&self) -> ClifValue {
        match self {
            SelectScrut::Scalar { disc, .. }
            | SelectScrut::Value { disc, .. }
            | SelectScrut::Composite { disc, .. } => *disc,
        }
    }
}

/// Where a scalar pattern leaf lives in the composite scrutinee.
#[derive(Clone, Copy)]
enum ElemIdx {
    /// `a[idx]` — tuple / slice / slice-prefix leaves.
    FromStart(usize),
    /// `a[len - back]` — slice-suffix leaves (`len` is the scrutinee
    /// length SSA value read by the arm's structure condition).
    FromEnd { back: usize, len: ClifValue },
    /// `a[idx][1]` — a struct field's value (idx is the canonically-
    /// sorted field index resolved by typecheck).
    StructField(usize),
}

/// Read one scalar pattern leaf off the composite scrutinee. Total: an
/// out-of-range or mismatched slot reads 0 (a tainted scrutinee's
/// placeholder is an empty array).
fn read_scrut_elem(
    cx: &mut BodyCx,
    ptr: ClifValue,
    idx: ElemIdx,
    prim: PrimType,
) -> Result<ClifValue> {
    let (read, idx_v) = elem_index(cx, idx);
    let helper_name = match read {
        ElementRead::ArrayIndex => valarray_get_helper(prim),
        ElementRead::StructField => struct_get_helper(prim),
    };
    let helper = cx.helper(helper_name)?;
    let call = cx.b.ins().call(helper, &[ptr, idx_v]);
    Ok(cx.b.inst_results(call)[0])
}

/// The read family and index value of an element position.
fn elem_index(cx: &mut BodyCx, idx: ElemIdx) -> (ElementRead, ClifValue) {
    match idx {
        ElemIdx::FromStart(j) => {
            (ElementRead::ArrayIndex, cx.b.ins().iconst(types::I64, j as i64))
        }
        ElemIdx::FromEnd { back, len } => {
            let b = cx.b.ins().iconst(types::I64, back as i64);
            (ElementRead::ArrayIndex, cx.b.ins().isub(len, b))
        }
        ElemIdx::StructField(i) => {
            (ElementRead::StructField, cx.b.ins().iconst(types::I64, i as i64))
        }
    }
}

/// A pattern binding installed in the arm's matched region under the
/// pattern's `BindId`.
enum SelectArmBind {
    /// `n => ...` — bind the scalar scrutinee itself.
    Scrut(BindId),
    /// A scalar read out of a value scrutinee: an option's payload, a
    /// union's member, the scrutinee narrowed to one scalar.
    ValueScalar { id: BindId, prim: PrimType },
    /// The same for a non-scalar, cloned out as an owned local of `kind`
    /// (the whole scrutinee included), dropped at the arm's scope exit;
    /// legal under a mask, where a mismatch yields a drop-safe default.
    ValueOwned { id: BindId, kind: OwnedKind },
    /// `` `Tag(n) `` — bind one scalar variant payload; a wrong-tag read
    /// yields 0. `on` is the variant read when it is not the scrutinee
    /// (a payload of an enclosing variant, borrowed).
    Payload { id: BindId, idx: usize, prim: PrimType, on: Option<(ClifValue, ClifValue)> },
    /// `` `Tag(xs) `` — bind one non-scalar variant payload, cloned out
    /// as an owned local of `kind` and dropped at the arm's scope exit.
    /// Legal under a mask: a wrong-tag read yields a drop-safe default
    /// behind a tainted disc.
    PayloadValue {
        id: BindId,
        idx: usize,
        kind: OwnedKind,
        on: Option<(ClifValue, ClifValue)>,
    },
    /// `[<a, b>]` / `[<h, rest..>]` — bind the j-th head of a list
    /// scrutinee, cloned out as an owned local of `kind` (legal under a mask).
    ListHead { id: BindId, idx: usize, kind: LocalKind },
    /// The rest bind: the k-th TAIL itself — O(1), shares the spine.
    ListTail { id: BindId, k: usize },
    /// `(x, y)` / `{f, ..}` / `[h, ..]` — bind one scalar leaf of a
    /// composite scrutinee (the scrutinee, or a borrowed interior pointer
    /// for a nested pattern).
    Elem { id: BindId, idx: ElemIdx, prim: PrimType, parent_ptr: ClifValue },
    /// The same for a non-scalar leaf of type `typ`, cloned out as an
    /// owned local of `kind`; a short or mismatched read yields a
    /// drop-safe default.
    ElemValue {
        id: BindId,
        idx: ElemIdx,
        typ: Type,
        kind: OwnedKind,
        parent_ptr: ClifValue,
    },
    /// `[h, rest..]` / `[init.., l]` / `all@ [..]` — bind the elements
    /// `[start, len - back)` of an array scrutinee, cloned out as an
    /// owned composite local.
    Subslice { id: BindId, start: usize, back: usize, parent_ptr: ClifValue },
}

/// `select` at expression position. Canonical semantics are
/// `Select::update` / `PatternNode::is_match`: the scrutinee is
/// evaluated once and its disc folds into every arm's result; an
/// explicit type predicate is tested; a guard runs after the pattern
/// matches with its binds in scope, and a bottom guard stops the chain;
/// the first matching arm wins.
pub(crate) fn emit_select_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    sel: &Select<R, E>,
) -> Result<CompiledExpr> {
    if sel.arms.is_empty() {
        return Err(anyhow!("emit_clif: select with no arms"));
    }
    let result_typ =
        kernel_abi::freeze_for_abi_normalized(sel.typ()).ok_or_else(|| {
            anyhow!(
                "emit_clif: select result type {:?} doesn't freeze concrete",
                sel.typ()
            )
        })?;
    let merge_shape = match kernel_abi::abi_kind(&result_typ) {
        Some(AbiKind::Scalar(p)) => SelectMerge::Scalar(p),
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => SelectMerge::Value,
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => SelectMerge::Composite,
        Some(AbiKind::String) => SelectMerge::String,
        other @ (Some(AbiKind::Unit | AbiKind::Null) | None) => {
            return Err(anyhow!(
                "emit_clif: select result shape {other:?} not representable"
            ));
        }
    };
    let (scrut, scrut_typ, scrut_drop) = classify_select_scrutinee(cx, sel)?;
    let merge = cx.b.create_block();
    cx.b.append_block_param(merge, types::I64);
    let payload_ty = match merge_shape {
        SelectMerge::Scalar(p) => prim_to_clif(p),
        _ => types::I64,
    };
    cx.b.append_block_param(merge, payload_ty);
    let scrut_disc = scrut.disc();
    // A select fires iff a consumed input fires: the scrutinee's or a
    // consulted guard's STALE bit folds into every arm result.
    emit_select_arms(
        cx,
        sel,
        scrut,
        &scrut_typ,
        &mut |cx, body, mark, guards_stale| {
            emit_select_value_arm(
                cx,
                body,
                mark,
                merge_shape,
                merge,
                scrut_disc,
                guards_stale,
            )
        },
        // A standing bottom scrutinee does not re-fire the select.
        &mut |cx| {
            let s = cx.b.ins().band_imm(scrut_disc, STALE);
            emit_select_bottom_value(cx, merge_shape, merge, s)
        },
        &mut |cx, stale_bits| {
            emit_select_bottom_value(cx, merge_shape, merge, stale_bits)
        },
    )?;
    cx.b.switch_to_block(merge);
    cx.b.seal_block(merge);
    let (rdisc, rpayload) = {
        let params = cx.b.block_params(merge);
        (params[0], params[1])
    };
    // Every normal path crosses the merge, so an owned scrutinee drops
    // exactly once here; unbinding it keeps a later pending exit from
    // double-dropping it.
    if let Some(ScrutDrop { kind, vv, mark }) = scrut_drop {
        emit_drop_local(cx.b, cx.ctx, kind, vv)?;
        cx.env.truncate(mark);
    }
    Ok(CompiledExpr::new(rdisc, rpayload))
}

/// Jump to the merge with a drop-safe tainted bottom whose freshness
/// is `stale_bits` (0 = fresh).
fn emit_select_bottom_value(
    cx: &mut BodyCx,
    merge_shape: SelectMerge,
    merge: Block,
    stale_bits: ClifValue,
) -> Result<()> {
    let cv = emit_bottom_of_kind(cx, merge_shape.abi_kind())?;
    let disc = cx.b.ins().bor(cv.disc, stale_bits);
    cx.b.ins().jump(merge, &[BlockArg::Value(disc), BlockArg::Value(cv.payload)]);
    Ok(())
}

/// The merge-point obligation for an owned scrutinee: it is bound as an
/// env local so a mid-arm pending exit drops it, and the merge drops
/// then unbinds it on the normal path.
pub(super) struct ScrutDrop {
    kind: LocalKind,
    vv: ValueVar,
    mark: usize,
}

/// Classify and emit the read of a select scrutinee. An owned one is
/// adopted as an env local, the returned [`ScrutDrop`]: every
/// terminator drops the env, and a value-position select drops it at
/// its merge.
pub(super) fn classify_select_scrutinee<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    sel: &Select<R, E>,
) -> Result<(SelectScrut, Type, Option<ScrutDrop>)> {
    let scrut_typ = kernel_abi::freeze_for_abi_normalized(sel.arg.node.typ())
        .ok_or_else(|| {
            anyhow!(
                "emit_clif: select scrutinee type {:?} doesn't freeze \
                 concrete",
                sel.arg.node.typ()
            )
        })?;
    let scrut_kind = kernel_abi::abi_kind(&scrut_typ)
        .ok_or_else(|| anyhow!("emit_clif: select scrutinee shape not classifiable"))?;
    let owned = node_composite_source(&sel.arg.node) == CompositeSource::Owned;
    let mut drop_ob: Option<ScrutDrop> = None;
    let mut adopt = |cx: &mut BodyCx,
                     kind: LocalKind,
                     disc: ClifValue,
                     payload: ClifValue,
                     owned: bool| {
        if owned {
            let mark = cx.env.mark();
            let name: ArcStr =
                compact_str::format_compact!("__scrut{}", sel.spec.id.inner())
                    .as_str()
                    .into();
            let vv = bind_local(cx, name, disc, payload, kind, None);
            drop_ob = Some(ScrutDrop { kind, vv, mark });
        }
    };
    let scrut = match scrut_kind {
        AbiKind::Scalar(p) => {
            let cv = sel.arg.node.emit_clif(cx)?;
            SelectScrut::Scalar { disc: cv.disc, value: cv.payload, prim: p }
        }
        AbiKind::Variant | AbiKind::Nullable | AbiKind::Value => {
            let cv = sel.arg.node.emit_clif(cx)?;
            adopt(cx, LocalKind::Value, cv.disc, cv.payload, owned);
            let kind = match scrut_kind {
                AbiKind::Variant => ValueKind::Variant,
                AbiKind::Nullable => ValueKind::Nullable,
                _ => ValueKind::Value,
            };
            SelectScrut::Value { kind, disc: cv.disc, payload: cv.payload }
        }
        AbiKind::Array | AbiKind::Tuple | AbiKind::Struct => {
            let cv = sel.arg.node.emit_clif(cx)?;
            adopt(cx, LocalKind::Composite, cv.disc, cv.payload, owned);
            SelectScrut::Composite { disc: cv.disc, ptr: cv.payload }
        }
        // A string is a value whose read is always owned: the select
        // keeps it until the merge.
        AbiKind::String => {
            let cv = sel.arg.node.emit_clif(cx)?;
            let base = cx.b.ins().iconst(types::I64, value_disc::STRING);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            adopt(cx, LocalKind::String, disc, cv.payload, true);
            SelectScrut::Value { kind: ValueKind::String, disc, payload: cv.payload }
        }
        AbiKind::Unit | AbiKind::Null => {
            bail!("emit_clif: select scrutinee of shape {scrut_kind:?}");
        }
    };
    Ok((scrut, scrut_typ, drop_ob))
}

/// Structure condition + scalar leaf binds for a tuple/struct/slice
/// pattern over a composite scrutinee, mirroring
/// `StructPatternNode::is_match` / `bind`: Slice tests `len == N`,
/// SlicePrefix/SliceSuffix/Struct `len >= N`; suffix leaves index from
/// the end, struct leaves read `a[i][1]`.
///
/// Element reads are total, so the condition is the AND of the length
/// test and every leaf's own test, in one block. An array `@` or rest
/// bind lowers as a subslice and a non-scalar leaf as an element value;
/// a tuple or struct `@` and a nested variant, or- or abstract leaf
/// refuse (the select de-fuses).
fn emit_composite_pattern_cond(
    cx: &mut BodyCx,
    ptr: ClifValue,
    scrut_typ: &Type,
    pat: &StructPatternNode,
    binds: &mut SmallVec<[SelectArmBind; 8]>,
) -> Result<ClifValue> {
    stack::ensure_sufficient(|| {
        emit_composite_pattern_cond_inner(cx, ptr, scrut_typ, pat, binds)
    })
}

fn emit_composite_pattern_cond_inner(
    cx: &mut BodyCx,
    ptr: ClifValue,
    scrut_typ: &Type,
    pat: &StructPatternNode,
    binds: &mut SmallVec<[SelectArmBind; 8]>,
) -> Result<ClifValue> {
    let len_helper = cx.helper("graphix_valarray_len")?;
    let call = cx.b.ins().call(len_helper, &[ptr]);
    let len = cx.b.inst_results(call)[0];
    let styp = resolve_node_typ(cx.ctx, scrut_typ);
    struct LeafSpec<'p> {
        idx: ElemIdx,
        sub: &'p StructPatternNode,
        typ: Type,
    }
    let (leaves, len_cc, n): (SmallVec<[LeafSpec; 8]>, IntCC, usize) = match pat {
        StructPatternNode::Slice { kind, all, binds: pbinds } => {
            if matches!(kind, SliceKind::List) {
                return Err(anyhow!("emit_clif: list pattern not lowerable yet"));
            }
            if let Some(id) = all {
                binds.push(SelectArmBind::Subslice {
                    id: *id,
                    start: 0,
                    back: 0,
                    parent_ptr: ptr,
                });
            }
            let elt = |j: usize| -> Result<Type> {
                if matches!(kind, SliceKind::Tuple) {
                    match &styp {
                        Type::Tuple(elts) if elts.len() == pbinds.len() => {
                            Ok(elts[j].clone())
                        }
                        t => Err(anyhow!(
                            "emit_clif: tuple pattern over non-tuple \
                             scrutinee {t:?}"
                        )),
                    }
                } else {
                    match &styp {
                        Type::Array(t) => Ok((**t).clone()),
                        t => Err(anyhow!(
                            "emit_clif: slice pattern over non-array \
                             scrutinee {t:?}"
                        )),
                    }
                }
            };
            let leaves = pbinds
                .iter()
                .enumerate()
                .map(|(j, sub)| {
                    Ok(LeafSpec { idx: ElemIdx::FromStart(j), sub, typ: elt(j)? })
                })
                .collect::<Result<SmallVec<[_; 8]>>>()?;
            (leaves, IntCC::Equal, pbinds.len())
        }
        StructPatternNode::SlicePrefix { list, all, prefix, tail } => {
            if *list {
                return Err(anyhow!("emit_clif: list pattern not lowerable yet"));
            }
            for (id, start) in all
                .iter()
                .map(|id| (id, 0))
                .chain(tail.iter().map(|id| (id, prefix.len())))
            {
                binds.push(SelectArmBind::Subslice {
                    id: *id,
                    start,
                    back: 0,
                    parent_ptr: ptr,
                });
            }
            let t = match &styp {
                Type::Array(t) => (**t).clone(),
                t => {
                    return Err(anyhow!(
                        "emit_clif: slice pattern over non-array scrutinee {t:?}"
                    ));
                }
            };
            let leaves = prefix
                .iter()
                .enumerate()
                .map(|(j, sub)| LeafSpec {
                    idx: ElemIdx::FromStart(j),
                    sub,
                    typ: t.clone(),
                })
                .collect();
            (leaves, IntCC::SignedGreaterThanOrEqual, prefix.len())
        }
        StructPatternNode::SliceSuffix { all, head, suffix } => {
            for (id, back) in all
                .iter()
                .map(|id| (id, 0))
                .chain(head.iter().map(|id| (id, suffix.len())))
            {
                binds.push(SelectArmBind::Subslice {
                    id: *id,
                    start: 0,
                    back,
                    parent_ptr: ptr,
                });
            }
            let t = match &styp {
                Type::Array(t) => (**t).clone(),
                t => {
                    return Err(anyhow!(
                        "emit_clif: slice pattern over non-array scrutinee {t:?}"
                    ));
                }
            };
            let n = suffix.len();
            let leaves = suffix
                .iter()
                .enumerate()
                .map(|(j, sub)| LeafSpec {
                    idx: ElemIdx::FromEnd { back: n - j, len },
                    sub,
                    typ: t.clone(),
                })
                .collect();
            (leaves, IntCC::SignedGreaterThanOrEqual, n)
        }
        StructPatternNode::Struct { all, binds: sbinds } => {
            if let Some(id) = all {
                binds.push(SelectArmBind::Subslice {
                    id: *id,
                    start: 0,
                    back: 0,
                    parent_ptr: ptr,
                });
            }
            let flds = match &styp {
                Type::Struct(flds) => flds,
                t => {
                    return Err(anyhow!(
                        "emit_clif: struct pattern over non-struct scrutinee {t:?}"
                    ));
                }
            };
            let leaves = sbinds
                .iter()
                .map(|(_, i, sub)| {
                    let typ =
                        flds.get(*i).map(|(_, t, _)| t.clone()).ok_or_else(|| {
                            anyhow!(
                                "emit_clif: struct pattern field index {i} out \
                                 of range"
                            )
                        })?;
                    Ok(LeafSpec { idx: ElemIdx::StructField(*i), sub, typ })
                })
                .collect::<Result<SmallVec<[_; 8]>>>()?;
            (leaves, IntCC::SignedGreaterThanOrEqual, sbinds.len())
        }
        _ => return Err(anyhow!("emit_clif: not a composite structural pattern")),
    };
    let mut lit_leaves: SmallVec<[(ElemIdx, PrimType, &Value); 8]> = SmallVec::new();
    let mut nested: SmallVec<[(ElemIdx, &StructPatternNode, Type); 8]> = SmallVec::new();
    for leaf in &leaves {
        match leaf.sub {
            StructPatternNode::Abstract { .. } => {
                return Err(anyhow!("emit_clif: abstract pattern leaf not lowerable"));
            }
            StructPatternNode::Ignore => {}
            StructPatternNode::Bind(id) => match kernel_abi::scalar_prim(&leaf.typ) {
                Some(prim) => binds.push(SelectArmBind::Elem {
                    id: *id,
                    idx: leaf.idx,
                    prim,
                    parent_ptr: ptr,
                }),
                None => {
                    let kind = owned_kind(&leaf.typ).ok_or_else(|| {
                        anyhow!(
                            "emit_clif: select pattern leaf bind {:?} not lowerable",
                            leaf.typ
                        )
                    })?;
                    binds.push(SelectArmBind::ElemValue {
                        id: *id,
                        idx: leaf.idx,
                        typ: leaf.typ.clone(),
                        kind,
                        parent_ptr: ptr,
                    })
                }
            },
            StructPatternNode::Literal(v) => {
                let prim = kernel_abi::scalar_prim_of_value(v).ok_or_else(|| {
                    anyhow!("emit_clif: non-scalar literal pattern leaf {v:?}")
                })?;
                // The typed element read is total: a slot not of `prim`'s
                // family reads as 0, which a `0` literal would match.
                // Only the static type proves the read faithful.
                if kernel_abi::scalar_prim(&leaf.typ) != Some(prim) {
                    return Err(anyhow!(
                        "emit_clif: literal pattern leaf prim {prim:?} doesn't \
                         match the leaf's static type {:?}",
                        leaf.typ
                    ));
                }
                lit_leaves.push((leaf.idx, prim, v));
            }
            sub @ (StructPatternNode::Slice { .. }
            | StructPatternNode::SlicePrefix { .. }
            | StructPatternNode::SliceSuffix { .. }
            | StructPatternNode::Struct { .. }) => {
                match kernel_abi::abi_kind(&leaf.typ) {
                    Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                        nested.push((leaf.idx, sub, leaf.typ.clone()));
                    }
                    other => {
                        return Err(anyhow!(
                            "emit_clif: nested pattern over a leaf of shape \
                             {other:?} not lowerable"
                        ));
                    }
                }
            }
            StructPatternNode::Variant { .. } => {
                return Err(anyhow!(
                    "emit_clif: nested variant pattern leaf not lowerable"
                ));
            }
            StructPatternNode::Or { .. } => {
                return Err(anyhow!("emit_clif: or-pattern leaf not lowerable"));
            }
        }
    }
    let n_c = cx.b.ins().iconst(types::I64, n as i64);
    let mut cond = cx.b.ins().icmp(len_cc, len, n_c);
    for (idx, prim, v) in lit_leaves {
        let elem = read_scrut_elem(cx, ptr, idx, prim)?;
        let lit = compile_const(cx.b, v, prim)?;
        let c = compile_cmp(cx.b, CmpOp::Eq, prim, elem, lit);
        cond = cx.b.ins().band(cond, c);
    }
    for (idx, sub, typ) in nested {
        // A borrowed interior pointer: the root is a pinned borrowed
        // slot and values are immutable, so no ownership or drops.
        let (read, idx_v) = elem_index(cx, idx);
        let helper_name = match read {
            ElementRead::ArrayIndex => "graphix_valarray_get_array_borrowed",
            ElementRead::StructField => "graphix_struct_get_array_borrowed",
        };
        let call = cx.call_helper(helper_name, &[ptr, idx_v])?;
        let child_ptr = cx.b.inst_results(call)[0];
        let c = emit_composite_pattern_cond(cx, child_ptr, &typ, sub, binds)?;
        cond = cx.b.ins().band(cond, c);
    }
    Ok(cond)
}

/// One prologue-evaluated guard.
#[derive(Clone, Copy)]
struct GuardPlanes {
    /// The sound-true verdict.
    eff: ClifValue,
    /// The channel is bottom.
    gbot: ClifValue,
    /// The guard's STALE bit.
    gs: ClifValue,
}

/// Arm `pat`'s pattern condition against `scrut`, pushing its binds
/// onto `binds`; `None` when the arm matches unconditionally. An
/// or-pattern installs its shared binds itself, in the chain's done
/// block (`nomatch` as for [`emit_or_chain`]).
fn emit_arm_condition<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    pat: &PatternNode<R, E>,
    scrut: SelectScrut,
    scrut_typ: &Type,
    nomatch: Option<Block>,
    binds: &mut SmallVec<[SelectArmBind; 8]>,
) -> Result<Option<ClifValue>> {
    if let StructPatternNode::Or { alts } = &pat.structure_predicate {
        if pat.explicit_type_predicate {
            bail!(
                "emit_clif: explicit type predicate on an or-pattern arm not lowerable"
            );
        }
        let m = emit_or_chain(cx, alts, &pat.type_predicate, scrut, scrut_typ, nomatch)?;
        return Ok(Some(m));
    }
    let (tcond, scond) = emit_arm_cond(cx, pat, scrut, scrut_typ, binds)?;
    Ok(match (tcond, scond) {
        (None, None) => None,
        (Some(c), None) | (None, Some(c)) => Some(c),
        (Some(a), Some(b)) => Some(cx.b.ins().band(a, b)),
    })
}

/// The shared arm chain: pattern conditions, per-arm binds and the
/// fail-block plumbing, identical between value and tail position.
/// `emit_arm` runs in the matched block with the arm's binds installed
/// and must leave it terminated; it gets the env mark to truncate to
/// and the AND of the consulted guards' STALE bits at its point
/// (structure-failed arms and arms below the taken one contribute
/// nothing). `emit_miss` handles a tainted scrutinee, which makes no
/// selection and consults no guard; `emit_undet` the undecidable
/// outcome (a bottom consulted guard), given its STALE bits (0 = fresh).
pub(super) fn emit_select_arms<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    sel: &Select<R, E>,
    scrut: SelectScrut,
    scrut_typ: &Type,
    emit_arm: &mut dyn FnMut(&mut BodyCx, &Node<R, E>, usize, ClifValue) -> Result<()>,
    emit_miss: &mut dyn FnMut(&mut BodyCx) -> Result<()>,
    emit_undet: &mut dyn FnMut(&mut BodyCx, ClifValue) -> Result<()>,
) -> Result<()> {
    let sdisc = scrut.disc();
    let miss_bl = cx.b.create_block();
    let n = sel.arms.len();
    // The guard prologue: the node-walk ticks every arm's guard every
    // cycle before matching, so every guard that is not pure in its
    // binds is evaluated here once per invocation and the chain reads
    // it; the consulted folds happen along the chain into `acc`.
    let mut guard_vals: SmallVec<[Option<GuardPlanes>; 8]> = smallvec::smallvec![None; n];
    // a non-or arm's condition and bind specs from the prologue, which
    // dominates the chain: the chain reuses them
    let mut conds: SmallVec<
        [Option<(Option<ClifValue>, SmallVec<[SelectArmBind; 8]>)>; 8],
    > = (0..n).map(|_| None).collect();
    for (i, (pat, _)) in sel.arms.iter().enumerate() {
        let Some(g) = &pat.guard else { continue };
        if guard_is_pure_of_binds(pat, &g.node) {
            continue;
        }
        let gmark = cx.env.mark();
        let mut binds: SmallVec<[SelectArmBind; 8]> = SmallVec::new();
        let pcond = emit_arm_condition(cx, pat, scrut, scrut_typ, None, &mut binds)?;
        install_arm_binds(cx, &binds, scrut, pcond)?;
        let gcv = g.node.emit_clif(cx)?;
        let gs = cx.b.ins().band_imm(gcv.disc, STALE);
        let gbot = is_tainted(cx.b, gcv.disc);
        let valid = is_untainted(cx.b, gcv.disc);
        let eff = cx.b.ins().band(gcv.payload, valid);
        // The masked installs clone owned payload binds per invocation;
        // drop them before the truncate.
        emit_scope_drops(cx, gmark)?;
        cx.env.truncate(gmark);
        guard_vals[i] = Some(GuardPlanes { eff, gbot, gs });
        if !matches!(&pat.structure_predicate, StructPatternNode::Or { .. }) {
            conds[i] = Some((pcond, binds));
        }
    }
    // At any read this holds the fold over exactly the consultation
    // points control flow executed.
    let acc = cx.b.declare_var(types::I64);
    let init = cx.b.ins().iconst(types::I64, STALE);
    cx.b.def_var(acc, init);
    let chain_bl = cx.b.create_block();
    let clean = is_untainted(cx.b, sdisc);
    cx.b.ins().brif(clean, chain_bl, &[], miss_bl, &[]);
    cx.b.switch_to_block(chain_bl);
    cx.b.seal_block(chain_bl);
    let mut undet_bl: Option<Block> = None;
    for (i, (pat, body)) in sel.arms.iter().enumerate() {
        let is_last = i == n - 1;
        let has_guard = pat.guard.is_some();
        // The final-arm miss trap is sound only when a miss is
        // impossible; typecheck forbids a guarded final arm.
        if is_last && has_guard {
            bail!(
                "emit_clif: guard on the final select arm — the chain could miss \
                 every arm"
            );
        }
        // An or-chain binds the shared BindIds in its done block, so the
        // mark is taken before it and the arm-exit drops cover them.
        let mark = cx.env.mark();
        let or_fail = matches!(&pat.structure_predicate, StructPatternNode::Or { .. })
            .then(|| cx.b.create_block());
        let mut binds: SmallVec<[SelectArmBind; 8]> = SmallVec::new();
        // CR claude for eric: [perf] For an arm whose guard is not pure of its binds,
        // the prologue (:740-751) already emitted this pattern condition and cloned the
        // arm's owned binds. This emits both again, so the arm pays two tag tests or
        // list walks and two clones per invocation (a Subslice bind takes a pooled Arc
        // each time). The prologue dominates the chain, so its pcond and bind specs can
        // be kept per arm and reused here. probe:
        // design/review-2026-10-05/repro/f-select-13.gx (GRAPHIX_DUMP_CLIF=1: 4 tag
        // tests and 3 payload clones for 3 arms; 3 and 2 with a pure guard).
        // (f-select-13)
        // 2026-10-08 claude: Half done: the chain reuses a non-or arm's pattern condition
        // and bind specs from the prologue (the condition emitters are straight-line, so
        // their values dominate the chain), so the arm's tag test or list walk runs once.
        // The second clone remains: the prologue installs owned clones for the guard and
        // drops them before the chain clones again. Borrowing in the prologue needs a
        // local kind no cleanup drops, since an abort while the guard emits runs
        // emit_pending_cleanup over every owned local; that is the borrowed/owned split
        // f-helpers-03 asks for.
        // 2026-10-09 claude: Scope call for the remaining half: dropping the prologue's
        // clone needs a local that no cleanup drops (an ownership flag on Local, honored
        // by emit_scope_drops and emit_pending_cleanup) and borrowed read helpers for
        // every kind a bind can have (only the variant payload has one). That saves one
        // refcount pair per impure-guard arm per run. I lean accepting the clone: delete
        // this CR.
        let pcond = match conds[i].take() {
            Some((pcond, prologue_binds)) => {
                binds = prologue_binds;
                pcond
            }
            None => emit_arm_condition(cx, pat, scrut, scrut_typ, or_fail, &mut binds)?,
        };
        let matched = cx.b.create_block();
        let fail: Option<Block> = match or_fail {
            Some(f) => Some(f),
            None if pcond.is_some() || has_guard => Some(cx.b.create_block()),
            None => None,
        };
        match (pcond, fail) {
            (Some(c), Some(f)) => {
                cx.b.ins().brif(c, matched, &[], f, &[]);
            }
            _ => {
                cx.b.ins().jump(matched, &[]);
            }
        }
        cx.b.switch_to_block(matched);
        cx.b.seal_block(matched);
        install_arm_binds(cx, &binds, scrut, None)?;
        if let (Some(g), Some(fail)) = (&pat.guard, fail) {
            let eff = match guard_vals[i] {
                // The consultation point: fold the guard's STALE into the
                // accumulator; a bottom channel makes the selection
                // undecidable and the chain stops here.
                Some(GuardPlanes { eff, gbot, gs }) => {
                    let cur = cx.b.use_var(acc);
                    let n = cx.b.ins().band(cur, gs);
                    cx.b.def_var(acc, n);
                    let ub = *undet_bl.get_or_insert_with(|| cx.b.create_block());
                    let cont = cx.b.create_block();
                    // Drop this arm's owned binds before the shared undet block.
                    let ubdrop = cx.b.create_block();
                    cx.b.ins().brif(gbot, ubdrop, &[], cont, &[]);
                    cx.b.switch_to_block(ubdrop);
                    cx.b.seal_block(ubdrop);
                    emit_scope_drops(cx, mark)?;
                    cx.b.ins().jump(ub, &[]);
                    cx.b.switch_to_block(cont);
                    cx.b.seal_block(cont);
                    eff
                }
                // Schedule-free: pure and never bottom, so no undetermined
                // case; a consulted guard's fire still folds into the
                // accumulator (a constant in it fires at an init or a wake)
                None => {
                    let gcv = g.node.emit_clif(cx)?;
                    let gs = cx.b.ins().band_imm(gcv.disc, STALE);
                    let cur = cx.b.use_var(acc);
                    let n = cx.b.ins().band(cur, gs);
                    cx.b.def_var(acc, n);
                    let valid = is_untainted(cx.b, gcv.disc);
                    cx.b.ins().band(gcv.payload, valid)
                }
            };
            let body_blk = cx.b.create_block();
            // Guard-false falls through to the next arm; the matched
            // region's owned binds drop on the way out.
            let gfail = cx.b.create_block();
            cx.b.ins().brif(eff, body_blk, &[], gfail, &[]);
            cx.b.switch_to_block(gfail);
            cx.b.seal_block(gfail);
            emit_scope_drops(cx, mark)?;
            cx.b.ins().jump(fail, &[]);
            cx.b.switch_to_block(body_blk);
            cx.b.seal_block(body_blk);
        }
        let guards_stale = cx.b.use_var(acc);
        emit_arm(cx, body, mark, guards_stale)?;
        match fail {
            Some(f) => {
                cx.b.switch_to_block(f);
                cx.b.seal_block(f);
                if is_last {
                    // a valid scrutinee matches an exhaustive chain: this is
                    // a hole in the check, said as the node-walk says it
                    cx.call_helper("graphix_select_no_match", &[])?;
                    cx.b.ins().jump(miss_bl, &[]);
                }
            }
            // An unconditional arm consumed control flow.
            None => break,
        }
    }
    cx.b.switch_to_block(miss_bl);
    cx.b.seal_block(miss_bl);
    emit_miss(cx)?;
    if let Some(ub) = undet_bl {
        cx.b.switch_to_block(ub);
        cx.b.seal_block(ub);
        // The undecidable outcome is fresh iff a consumed input fired:
        // the scrutinee or a consulted guard.
        let ss = cx.b.ins().band_imm(sdisc, STALE);
        let af = cx.b.use_var(acc);
        let undet_stale = cx.b.ins().band(ss, af);
        emit_undet(cx, undet_stale)?;
    }
    Ok(())
}

/// True when a guard is a pure, never-bottom function of its arm's
/// binds and constants (comparisons, logicals, not, wrapping +/-/*/neg;
/// no div, indexing, calls or state). Such a guard is evaluated lazily
/// in the chain instead of in the prologue.
fn guard_is_pure_of_binds<R: Rt, E: UserEvent>(
    pat: &PatternNode<R, E>,
    guard: &Node<R, E>,
) -> bool {
    let mut bind_ids: SmallVec<[BindId; 8]> = SmallVec::new();
    pat.structure_predicate.ids(&mut |id| bind_ids.push(id));
    let mut ok = true;
    fusion::for_each_node(guard, &mut |n| match n.view() {
        NodeView::Constant(_) | NodeView::ExplicitParens(_) => {}
        NodeView::Ref(r) => {
            if !bind_ids.contains(&r.id) {
                ok = false;
            }
        }
        NodeView::Eq(_)
        | NodeView::Ne(_)
        | NodeView::Lt(_)
        | NodeView::Gt(_)
        | NodeView::Lte(_)
        | NodeView::Gte(_)
        | NodeView::And(_)
        | NodeView::Or(_)
        | NodeView::Not(_)
        | NodeView::Neg(_)
        | NodeView::Add(_)
        | NodeView::Sub(_)
        | NodeView::Mul(_) => {}
        _ => ok = false,
    });
    ok
}

/// Emit arm `pat`'s pattern condition against `scrut`: the type
/// predicate (`tcond`) and the structure condition (`scond`), pushing
/// the pattern's binds onto `binds`.
fn emit_arm_cond<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    pat: &PatternNode<R, E>,
    scrut: SelectScrut,
    scrut_typ: &Type,
    binds: &mut SmallVec<[SelectArmBind; 8]>,
) -> Result<(Option<ClifValue>, Option<ClifValue>)> {
    // An inferred predicate rejects nothing beyond what the arm's
    // structure rejects and what earlier unguarded arms took, both of
    // which the chain decides before it reaches this arm; only a written
    // one is tested here. The node-walk tests both (`PatternNode::
    // shape_matches`): a narrowing source that breaks this rule breaks
    // the agreement.
    let tcond: Option<ClifValue> = if !pat.explicit_type_predicate {
        None
    } else {
        let pred = kernel_abi::freeze_for_abi(&pat.type_predicate).ok_or_else(|| {
            anyhow!(
                "emit_clif: select type predicate {:?} doesn't \
                         freeze concrete",
                pat.type_predicate
            )
        })?;
        match &pred {
            // Over a primitive union, a primitive predicate is a test of
            // the value's tag against each of its members.
            Type::Primitive(p)
                if matches!(scrut_typ, Type::Primitive(_))
                    && matches!(
                        scrut,
                        SelectScrut::Value { kind: ValueKind::Value, .. }
                    ) =>
            {
                let disc = scrut.disc();
                let cd = clean_disc(cx.b, disc);
                let mut cond: Option<ClifValue> = None;
                for t in p.iter() {
                    let td = match t {
                        netidx_value::Typ::Null => {
                            cx.b.ins().iconst(types::I64, value_disc::NULL)
                        }
                        netidx_value::Typ::String => {
                            cx.b.ins().iconst(types::I64, value_disc::STRING)
                        }
                        t => match PrimType::from_typ(t) {
                            Some(prim) => scalar_disc(cx.b, prim),
                            None => bail!(
                                "emit_clif: type predicate member {t:?} over a \
                                 primitive union not lowerable"
                            ),
                        },
                    };
                    let eq = cx.b.ins().icmp(IntCC::Equal, cd, td);
                    cond = Some(match cond {
                        None => eq,
                        Some(c) => cx.b.ins().bor(c, eq),
                    });
                }
                cond
            }
            Type::Primitive(p)
                if p.contains(netidx_value::Typ::Null) && p.iter().count() == 1 =>
            {
                match scrut {
                    SelectScrut::Value { kind: ValueKind::Nullable, disc, .. } => {
                        // Only the option shape has a null member; a
                        // result union's non-success value is an error.
                        if kernel_abi::nullable_error_marked(&scrut_typ) != Some(false) {
                            return Err(anyhow!(
                                "emit_clif: null predicate over a \
                                     result union {scrut_typ:?}"
                            ));
                        }
                        let cd = clean_disc(cx.b, disc);
                        Some(cx.b.ins().icmp_imm(IntCC::Equal, cd, value_disc::NULL))
                    }
                    _ => {
                        return Err(anyhow!(
                            "emit_clif: null predicate over non-\
                                 Nullable scrutinee {scrut_typ:?}"
                        ));
                    }
                }
            }
            Type::Primitive(p)
                if !p.contains(netidx_value::Typ::Null) && p.iter().count() == 1 =>
            {
                let pt = p.iter().next().unwrap();
                match scrut {
                    SelectScrut::Scalar { prim, .. }
                        if PrimType::from_typ(pt) == Some(prim) =>
                    {
                        None
                    }
                    SelectScrut::Value { kind: ValueKind::Nullable, disc, .. } => {
                        let cd = clean_disc(cx.b, disc);
                        let exact = kernel_abi::nullable_inner(&scrut_typ).is_some_and(
                            |t| matches!(t, Type::Primitive(q) if q.exactly_one() == Some(pt)),
                        );
                        let tag = match PrimType::from_typ(pt) {
                            Some(prim) => Some(scalar_disc(cx.b, prim)),
                            None if pt == netidx_value::Typ::String => {
                                Some(cx.b.ins().iconst(types::I64, value_disc::STRING))
                            }
                            None if pt == netidx_value::Typ::Error => {
                                Some(cx.b.ins().iconst(types::I64, value_disc::ERROR))
                            }
                            None => None,
                        };
                        match (kernel_abi::nullable_error_marked(&scrut_typ), exact, tag)
                        {
                            // `[T, null]` tested for `T`: "is a T" is "is not null"
                            (Some(false), true, _) => Some(cx.b.ins().icmp_imm(
                                IntCC::NotEqual,
                                cd,
                                value_disc::NULL,
                            )),
                            // the value's own tag against the predicate's
                            (Some(_), _, Some(td)) => {
                                Some(cx.b.ins().icmp(IntCC::Equal, cd, td))
                            }
                            (Some(_), _, None) => {
                                return Err(anyhow!(
                                    "emit_clif: non-register type predicate \
                                     {pred:?} over {scrut_typ:?} not lowerable"
                                ));
                            }
                            (None, ..) => {
                                return Err(anyhow!(
                                    "emit_clif: Nullable scrutinee \
                                     {scrut_typ:?} has no marker shape"
                                ));
                            }
                        }
                    }
                    _ => {
                        return Err(anyhow!(
                            "emit_clif: type predicate {pred:?} over \
                                 scrutinee {scrut_typ:?} not lowerable"
                        ));
                    }
                }
            }
            _ => {
                return Err(anyhow!("emit_clif: type predicate {pred:?} not lowerable"));
            }
        }
    };
    let scond = emit_structure_cond(
        cx,
        &pat.structure_predicate,
        &pat.type_predicate,
        tcond.is_some(),
        scrut,
        scrut_typ,
        binds,
    )?;
    Ok((tcond, scond))
}

/// OR `TAINT|STALE` into `disc` when `cond` is false: the pattern did
/// not match, so nothing was delivered to this bind and a guard reading
/// it bottoms. `None` = the caller already branched on the condition.
fn mask_unmatched(
    cx: &mut BodyCx,
    disc: ClifValue,
    cond: Option<ClifValue>,
) -> ClifValue {
    match cond {
        None => disc,
        Some(c) => {
            let t = cx.b.ins().iconst(types::I64, TAINT | STALE);
            let z = cx.b.ins().iconst(types::I64, 0);
            let m = cx.b.ins().select(c, z, t);
            cx.b.ins().bor(disc, m)
        }
    }
}

/// The list-pattern condition + binds over a Value-kind scrutinee: one
/// `graphix_list_match` spine walk (`k` cells, `exact` requires nil
/// after); head binds clone out by ABI kind; the rest bind is the k-th
/// tail, shared. Nested element patterns refuse.
fn emit_list_pattern_cond(
    cx: &mut BodyCx,
    pat: &StructPatternNode,
    scrut: SelectScrut,
    scrut_typ: &Type,
    binds: &mut SmallVec<[SelectArmBind; 8]>,
) -> Result<ClifValue> {
    let (disc, payload) = match scrut {
        SelectScrut::Value { disc, payload, .. } => (disc, payload),
        _ => {
            return Err(anyhow!(
                "emit_clif: list pattern over a non-value scrutinee \
                     {scrut_typ:?}"
            ));
        }
    };
    let et = scrut_typ
        .with_deref(|t| match t {
            Some(Type::List(et)) => Some((**et).clone()),
            _ => None,
        })
        .ok_or_else(|| {
            anyhow!("emit_clif: list pattern over non-list scrutinee {scrut_typ:?}")
        })?;
    let (elems, k, exact, tail) = match pat {
        StructPatternNode::Slice { kind: SliceKind::List, all, binds: pb } => {
            if let Some(id) = all {
                binds.push(SelectArmBind::ValueOwned { id: *id, kind: OwnedKind::Value });
            }
            (pb, pb.len(), true, None)
        }
        StructPatternNode::SlicePrefix { list: true, all, prefix, tail } => {
            if let Some(id) = all {
                binds.push(SelectArmBind::ValueOwned { id: *id, kind: OwnedKind::Value });
            }
            (prefix, prefix.len(), false, tail.as_ref())
        }
        _ => unreachable!("emit_list_pattern_cond on a non-list pattern"),
    };
    for (j, sub) in elems.iter().enumerate() {
        match sub {
            StructPatternNode::Bind(id) => {
                let kind = payload_local_kind(&et).ok_or_else(|| {
                    anyhow!("emit_clif: list element shape {et:?} not lowerable")
                })?;
                binds.push(SelectArmBind::ListHead { id: *id, idx: j, kind });
            }
            StructPatternNode::Ignore => {}
            _ => {
                return Err(anyhow!(
                    "emit_clif: nested list element pattern not lowerable"
                ));
            }
        }
    }
    if let Some(id) = tail {
        binds.push(SelectArmBind::ListTail { id: *id, k });
    }
    let helper = cx.helper("graphix_list_match")?;
    let kc = cx.b.ins().iconst(types::I64, k as i64);
    let ex = cx.b.ins().iconst(types::I8, exact as i64);
    let call = cx.b.ins().call(helper, &[disc, payload, kc, ex]);
    Ok(cx.b.inst_results(call)[0])
}

/// The [`LocalKind`] a non-scalar bind takes (a variant payload, a
/// leaf, a list head, a nullable's payload), by its ABI kind.
/// `Unit`/`Null` (and shapes with no kernel encoding) refuse.
fn payload_local_kind(t: &Type) -> Option<LocalKind> {
    kernel_abi::abi_kind(t).and_then(LocalKind::of)
}

/// [`payload_local_kind`] of a non-scalar.
fn owned_kind(t: &Type) -> Option<OwnedKind> {
    kernel_abi::abi_kind(t).and_then(OwnedKind::of)
}

/// The container an owned bind is read out of.
#[derive(Clone, Copy)]
enum ReadFrom {
    /// The scrutinee itself, an option's payload or a union's member.
    Value,
    /// A variant payload.
    Payload,
    /// A list head.
    ListHead,
}

/// The helper reading an owned `kind` out of `from`.
fn owned_read_helper(from: ReadFrom, kind: OwnedKind) -> &'static str {
    match (from, kind) {
        (ReadFrom::Value, OwnedKind::Composite) => "graphix_nullable_array",
        (ReadFrom::Value, OwnedKind::String) => "graphix_nullable_string",
        (ReadFrom::Value, OwnedKind::Value) => "graphix_value_clone",
        (ReadFrom::Payload, OwnedKind::Composite) => "graphix_variant_payload_array",
        (ReadFrom::Payload, OwnedKind::String) => "graphix_variant_payload_string",
        (ReadFrom::Payload, OwnedKind::Value) => "graphix_variant_payload_value",
        (ReadFrom::ListHead, OwnedKind::Composite) => "graphix_list_get_array",
        (ReadFrom::ListHead, OwnedKind::String) => "graphix_list_get_string",
        (ReadFrom::ListHead, OwnedKind::Value) => "graphix_list_get_value",
    }
}

/// The `kind` list head the helper call over `args` reads.
fn owned_list_head(
    cx: &mut BodyCx,
    kind: OwnedKind,
    args: &[ClifValue],
) -> Result<(ClifValue, ClifValue)> {
    let call = cx.call_helper(owned_read_helper(ReadFrom::ListHead, kind), args)?;
    Ok(owned_words(cx.b, kind.into(), call))
}

/// Install an arm's `binds` into the env.
/// `mask` is the arm's pattern condition when the caller has NOT
/// branched on it (the guard prologue); the take chain installs
/// inside the matched block and passes `None`.
fn install_arm_binds(
    cx: &mut BodyCx,
    binds: &[SelectArmBind],
    scrut: SelectScrut,
    mask: Option<ClifValue>,
) -> Result<()> {
    for bind in binds {
        let (sdisc, spayload) = match scrut {
            SelectScrut::Value { disc, payload, .. } => (disc, Some(payload)),
            s => (s.disc(), None),
        };
        let value_payload = || {
            spayload.ok_or_else(|| {
                anyhow!("emit_clif: a variant, nullable or list bind without a value scrutinee")
            })
        };
        // The bound (disc, payload, kind); the disc takes the
        // scrutinee's flags below.
        let (id, disc, payload, kind) = match bind {
            SelectArmBind::Scrut(id) => {
                let SelectScrut::Scalar { value, prim, .. } = scrut else {
                    bail!("emit_clif: scrutinee bind without a scalar scrutinee");
                };
                (*id, scalar_disc(cx.b, prim), value, LocalKind::Scalar(prim))
            }
            SelectArmBind::ValueScalar { id, prim } => {
                let value = cast_u64_to_prim(cx.b, value_payload()?, *prim);
                (*id, scalar_disc(cx.b, *prim), value, LocalKind::Scalar(*prim))
            }
            SelectArmBind::ValueOwned { id, kind } => {
                let helper = owned_read_helper(ReadFrom::Value, *kind);
                let call = cx.call_helper(helper, &[sdisc, value_payload()?])?;
                let (d, p) = owned_words(cx.b, (*kind).into(), call);
                (*id, d, p, (*kind).into())
            }
            SelectArmBind::Payload { id, idx, prim, on } => {
                let (vd, vp) = match on {
                    Some(on) => *on,
                    None => (sdisc, value_payload()?),
                };
                let idx_c = cx.b.ins().iconst(types::I64, *idx as i64);
                let call =
                    cx.call_helper(variant_payload_helper(*prim), &[vd, vp, idx_c])?;
                let v = cx.b.inst_results(call)[0];
                (*id, scalar_disc(cx.b, *prim), v, LocalKind::Scalar(*prim))
            }
            SelectArmBind::PayloadValue { id, idx, kind, on } => {
                let (vd, vp) = match on {
                    Some(on) => *on,
                    None => (sdisc, value_payload()?),
                };
                let idx_c = cx.b.ins().iconst(types::I64, *idx as i64);
                let helper = owned_read_helper(ReadFrom::Payload, *kind);
                let call = cx.call_helper(helper, &[vd, vp, idx_c])?;
                let (d, p) = owned_words(cx.b, (*kind).into(), call);
                (*id, d, p, (*kind).into())
            }
            SelectArmBind::ListHead { id, idx, kind } => {
                let j = cx.b.ins().iconst(types::I64, *idx as i64);
                let args = [sdisc, value_payload()?, j];
                let (d, p) = match *kind {
                    LocalKind::Scalar(p) => {
                        let call = cx.call_helper("graphix_list_get_value", &args)?;
                        let raw = cx.b.inst_results(call)[1];
                        (scalar_disc(cx.b, p), cast_u64_to_prim(cx.b, raw, p))
                    }
                    LocalKind::Composite => {
                        owned_list_head(cx, OwnedKind::Composite, &args)?
                    }
                    LocalKind::String => owned_list_head(cx, OwnedKind::String, &args)?,
                    LocalKind::Value => owned_list_head(cx, OwnedKind::Value, &args)?,
                };
                (*id, d, p, *kind)
            }
            SelectArmBind::ListTail { id, k } => {
                let kc = cx.b.ins().iconst(types::I64, *k as i64);
                let call =
                    cx.call_helper("graphix_list_tail", &[sdisc, value_payload()?, kc])?;
                let rs = cx.b.inst_results(call);
                (*id, rs[0], rs[1], LocalKind::Value)
            }
            SelectArmBind::ElemValue { id, idx, typ, kind, parent_ptr } => {
                if !matches!(scrut, SelectScrut::Composite { .. }) {
                    bail!("emit_clif: element bind without a composite scrutinee");
                }
                let (read, idx_v) = elem_index(cx, *idx);
                let call = cx.call_helper(
                    element_read_helper(typ, read)?,
                    &[*parent_ptr, idx_v],
                )?;
                let (d, p) = owned_words(cx.b, (*kind).into(), call);
                (*id, d, p, (*kind).into())
            }
            SelectArmBind::Subslice { id, start, back, parent_ptr } => {
                if !matches!(scrut, SelectScrut::Composite { .. }) {
                    bail!("emit_clif: subslice bind without a composite scrutinee");
                }
                let start = cx.b.ins().iconst(types::I64, *start as i64);
                let back = cx.b.ins().iconst(types::I64, *back as i64);
                let call = cx.call_helper(
                    "graphix_valarray_subslice",
                    &[*parent_ptr, start, back],
                )?;
                let bits = cx.b.inst_results(call)[0];
                (
                    *id,
                    cx.b.ins().iconst(types::I64, value_disc::ARRAY),
                    bits,
                    LocalKind::Composite,
                )
            }
            SelectArmBind::Elem { id, idx, prim, parent_ptr } => {
                if !matches!(scrut, SelectScrut::Composite { .. }) {
                    bail!("emit_clif: element bind without a composite scrutinee");
                }
                let v = read_scrut_elem(cx, *parent_ptr, *idx, *prim)?;
                (*id, scalar_disc(cx.b, *prim), v, LocalKind::Scalar(*prim))
            }
        };
        let disc = propagate_flags(cx.b, disc, &[sdisc]);
        let disc = mask_unmatched(cx, disc, mask);
        bind_local(cx, PAT_BIND_NAME, disc, payload, kind, Some(id));
    }
    Ok(())
}

/// Value-position arm-body emission: widen the arm's result to the
/// select's merge shape and jump to the merge block. `guards_stale` is
/// the consulted guards' STALE fold at this arm.
fn emit_select_value_arm<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    body: &Node<R, E>,
    mark: usize,
    merge_shape: SelectMerge,
    merge: Block,
    scrut_disc: ClifValue,
    guards_stale: ClifValue,
) -> Result<()> {
    let body_frozen =
        kernel_abi::freeze_for_abi_normalized(body.typ()).ok_or_else(|| {
            anyhow!("emit_clif: select arm type {:?} doesn't freeze concrete", body.typ())
        })?;
    // A `never(args..)` arm is a standing bottom: it fires only with the
    // scrutinee, through the STALE fold below, whatever its args do; the
    // args are still consumed (a raise or an effect in one delivers or
    // de-fuses at its own emission). Any other bottom-typed body runs:
    // its production's fire is the arm's.
    let never_args = match body.view() {
        NodeView::Never(n) => Some(&n.n),
        _ => None,
    };
    let (disc, payload) = if never_args.is_some() || matches!(body_frozen, Type::Bottom) {
        let cv = emit_bottom_of_kind(cx, merge_shape.abi_kind())?;
        let disc = match never_args {
            Some(args) => {
                for arg in args.iter() {
                    let av = arg.emit_clif(cx)?;
                    emit_discard_result(cx, arg, av)?;
                }
                cx.b.ins().bor_imm(cv.disc, STALE)
            }
            None => {
                let produced = body.emit_clif(cx)?;
                propagate_flags(cx.b, cv.disc, &[produced.disc])
            }
        };
        (disc, cv.payload)
    } else {
        emit_select_arm_value(cx, body, &body_frozen, merge_shape)?
    };
    // TAINT = OR(arm, scrutinee). Firing = OR(arm production, scrutinee
    // delivery, consulted guard productions), as the STALE AND-fold
    // (the interp's `own_fired`).
    let base = clean_disc(cx.b, disc);
    let d = propagate_taint(cx.b, base, &[disc, scrut_disc]);
    let d = propagate_stale(cx.b, d, &[disc]);
    let scrut_stale = cx.b.ins().band_imm(scrut_disc, STALE);
    let d = fold_stale(cx.b, d, scrut_stale);
    let d = fold_stale(cx.b, d, guards_stale);
    // Drop the arm's owned pattern binds; the widening above made the
    // result independently owned.
    emit_scope_drops(cx, mark)?;
    cx.env.truncate(mark);
    cx.b.ins().jump(merge, &[BlockArg::Value(d), BlockArg::Value(payload)]);
    Ok(())
}

/// An ordinary arm's result widened to the select's merge shape.
fn emit_select_arm_value<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    body: &Node<R, E>,
    body_frozen: &Type,
    merge_shape: SelectMerge,
) -> Result<(ClifValue, ClifValue)> {
    let body_kind = kernel_abi::abi_kind(body_frozen);
    Ok(match merge_shape {
        SelectMerge::Scalar(rp) => {
            if kernel_abi::scalar_prim(body_frozen) != Some(rp) {
                bail!(
                    "emit_clif: select arm type {body_frozen:?} doesn't match the \
                     scalar merge {rp:?}"
                );
            }
            let cv = body.emit_clif(cx)?;
            (cv.disc, cv.payload)
        }
        SelectMerge::Value => {
            let cv = emit_owned_value_operand_node(cx, body)?;
            (cv.disc, cv.payload)
        }
        SelectMerge::Composite => {
            if !matches!(
                body_kind,
                Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct)
            ) {
                bail!(
                    "emit_clif: select arm type {body_frozen:?} doesn't match the \
                     composite merge"
                );
            }
            let cv = body.emit_clif(cx)?;
            let v =
                ensure_owned_composite_src(cx, node_composite_source(body), cv.payload)?;
            (cv.disc, v)
        }
        SelectMerge::String => {
            if body_kind != Some(AbiKind::String) {
                bail!(
                    "emit_clif: select arm type {body_frozen:?} doesn't match the \
                     string merge"
                );
            }
            let cv = body.emit_clif(cx)?;
            (cv.disc, cv.payload)
        }
    })
}

/// One structure pattern's condition against the scrutinee: the
/// per-shape half of [`emit_arm_cond`], also called by [`emit_or_chain`]
/// once per alternative with that alternative's member of the arm's
/// inferred predicate (`has_tcond` false).
fn emit_structure_cond(
    cx: &mut BodyCx,
    sp: &StructPatternNode,
    pred_typ: &Type,
    has_tcond: bool,
    scrut: SelectScrut,
    scrut_typ: &Type,
    binds: &mut SmallVec<[SelectArmBind; 8]>,
) -> Result<Option<ClifValue>> {
    let scond: Option<ClifValue> = match sp {
        StructPatternNode::Abstract { .. } => {
            return Err(anyhow!("emit_clif: abstract patterns are not lowered"));
        }
        StructPatternNode::Or { .. } => {
            return Err(anyhow!(
                "emit_clif: nested or-pattern alternative — flatness violated \
                 (the parser folds chains flat)"
            ));
        }
        StructPatternNode::Ignore => None,
        StructPatternNode::Bind(id) => match scrut {
            SelectScrut::Scalar { .. } => {
                binds.push(SelectArmBind::Scrut(*id));
                None
            }
            // A bind under a primitive union: the scalar the arm's
            // predicate names, its string, or the value itself.
            SelectScrut::Value { kind: ValueKind::Value | ValueKind::String, .. }
                if matches!(scrut_typ, Type::Primitive(_)) =>
            {
                let pred = kernel_abi::freeze_for_abi(pred_typ);
                binds.push(match pred.as_ref().and_then(kernel_abi::scalar_prim) {
                    Some(prim) => SelectArmBind::ValueScalar { id: *id, prim },
                    None => {
                        let kind = match pred.as_ref().and_then(kernel_abi::abi_kind) {
                            Some(AbiKind::String) => OwnedKind::String,
                            _ => OwnedKind::Value,
                        };
                        SelectArmBind::ValueOwned { id: *id, kind }
                    }
                });
                None
            }
            SelectScrut::Value { kind: ValueKind::Nullable, .. } => {
                // Every payload read is total (a scalar reads bits, a
                // clone defaults on a mismatch), so a bind needs no test of
                // its own: the chain took this arm.
                let pred = kernel_abi::freeze_for_abi(pred_typ);
                let bind = match pred.as_ref().and_then(kernel_abi::scalar_prim) {
                    Some(prim) => SelectArmBind::ValueScalar { id: *id, prim },
                    None => {
                        let kind = pred.as_ref().and_then(owned_kind).ok_or_else(|| {
                            anyhow!(
                                "emit_clif: nullable scrutinee bind predicate {pred:?} \
                                 not lowerable"
                            )
                        })?;
                        SelectArmBind::ValueOwned { id: *id, kind }
                    }
                };
                binds.push(bind);
                None
            }
            // the whole value, cloned out as an owned local of the bind's kind
            SelectScrut::Value { .. } => {
                let pred = kernel_abi::freeze_for_abi_normalized(pred_typ);
                let kind =
                    pred.as_ref().and_then(payload_local_kind).ok_or_else(|| {
                        anyhow!(
                            "emit_clif: scrutinee bind of type {pred:?} not lowerable"
                        )
                    })?;
                binds.push(match kind {
                    LocalKind::Scalar(prim) => {
                        SelectArmBind::ValueScalar { id: *id, prim }
                    }
                    LocalKind::Composite => {
                        SelectArmBind::ValueOwned { id: *id, kind: OwnedKind::Composite }
                    }
                    LocalKind::String => {
                        SelectArmBind::ValueOwned { id: *id, kind: OwnedKind::String }
                    }
                    LocalKind::Value => {
                        SelectArmBind::ValueOwned { id: *id, kind: OwnedKind::Value }
                    }
                });
                None
            }
            SelectScrut::Composite { ptr, .. } => {
                binds.push(SelectArmBind::Subslice {
                    id: *id,
                    start: 0,
                    back: 0,
                    parent_ptr: ptr,
                });
                None
            }
        },
        // a string literal: the value's tag and its string
        StructPatternNode::Literal(Value::String(lit)) => match scrut {
            SelectScrut::Value { disc, payload, .. } => {
                let lit = cx.interned_str(lit)?;
                let call =
                    cx.call_helper("graphix_value_is_str", &[disc, payload, lit])?;
                Some(cx.b.inst_results(call)[0])
            }
            _ => {
                return Err(anyhow!(
                    "emit_clif: a string literal pattern over {scrut_typ:?}"
                ));
            }
        },
        StructPatternNode::Literal(v) => {
            let lit_prim = kernel_abi::scalar_prim_of_value(v)
                .ok_or_else(|| anyhow!("emit_clif: non-scalar literal pattern {v:?}"))?;
            match scrut {
                SelectScrut::Scalar { value, prim, .. } if prim == lit_prim => {
                    let lit = compile_const(cx.b, v, lit_prim)?;
                    Some(compile_cmp(cx.b, CmpOp::Eq, lit_prim, value, lit))
                }
                // Over an option, a result or a primitive union: the
                // literal's tag, then its payload.
                SelectScrut::Value {
                    kind: ValueKind::Nullable | ValueKind::Value,
                    disc,
                    payload,
                } => {
                    let cd = clean_disc(cx.b, disc);
                    let td = cx.b.ins().iconst(types::I64, Typ::get(v) as i64);
                    let is_prim = cx.b.ins().icmp(IntCC::Equal, cd, td);
                    let value = cast_u64_to_prim(cx.b, payload, lit_prim);
                    let lit = compile_const(cx.b, v, lit_prim)?;
                    let eq = compile_cmp(cx.b, CmpOp::Eq, lit_prim, value, lit);
                    Some(cx.b.ins().band(is_prim, eq))
                }
                _ => {
                    return Err(anyhow!(
                        "emit_clif: literal pattern prim {lit_prim:?} \
                             doesn't match scrutinee {scrut_typ:?}"
                    ));
                }
            }
        }
        StructPatternNode::Variant { tag, all, binds: pbinds } => {
            if let Some(id) = all {
                binds.push(SelectArmBind::ValueOwned { id: *id, kind: OwnedKind::Value });
            }
            let (disc, payload) = match scrut {
                SelectScrut::Value { kind: ValueKind::Variant, disc, payload } => {
                    (disc, payload)
                }
                _ => {
                    return Err(anyhow!(
                        "emit_clif: variant pattern over non-variant \
                             scrutinee {scrut_typ:?}"
                    ));
                }
            };
            // A wildcard payload types as `Any` in an inferred predicate;
            // the scrutinee's member for the tag has the payload types.
            let pred = kernel_abi::freeze_for_abi(pred_typ)
                .or_else(|| kernel_abi::freeze_for_abi_normalized(scrut_typ))
                .ok_or_else(|| {
                    anyhow!(
                        "emit_clif: variant pattern predicate {:?} \
                                 doesn't freeze concrete",
                        pred_typ
                    )
                })?;
            Some(emit_variant_cond(cx, (disc, payload), None, tag, pbinds, &pred, binds)?)
        }
        p @ (StructPatternNode::Slice { kind: SliceKind::List, .. }
        | StructPatternNode::SlicePrefix { list: true, .. }) => {
            if has_tcond {
                return Err(anyhow!(
                    "emit_clif: explicit type predicate on a list pattern \
                         not lowerable"
                ));
            }
            Some(emit_list_pattern_cond(cx, p, scrut, scrut_typ, binds)?)
        }
        p @ (StructPatternNode::Slice { .. }
        | StructPatternNode::SlicePrefix { .. }
        | StructPatternNode::SliceSuffix { .. }
        | StructPatternNode::Struct { .. }) => match scrut {
            SelectScrut::Composite { ptr, .. } => {
                if has_tcond {
                    return Err(anyhow!(
                        "emit_clif: explicit type predicate on a \
                             structural composite pattern not lowerable"
                    ));
                }
                Some(emit_composite_pattern_cond(cx, ptr, scrut_typ, p, binds)?)
            }
            _ => {
                return Err(anyhow!(
                    "emit_clif: slice/tuple/struct select pattern over a \
                         non-composite scrutinee not lowerable"
                ));
            }
        },
    };
    Ok(scond)
}

/// A variant pattern's condition over the variant `v` (the scrutinee
/// when `on` is `None`, else a payload of an enclosing variant, borrowed,
/// whose words `on` also holds): its tag and arity, then each payload's
/// own pattern against its member of `pred`, the frozen predicate.
fn emit_variant_cond(
    cx: &mut BodyCx,
    v: (ClifValue, ClifValue),
    on: Option<(ClifValue, ClifValue)>,
    tag: &ArcStr,
    pbinds: &[StructPatternNode],
    pred: &Type,
    binds: &mut SmallVec<[SelectArmBind; 8]>,
) -> Result<ClifValue> {
    let elts = kernel_abi::variant_cases(pred)
        .and_then(|cases| {
            cases.into_iter().find(|(t, e)| t == tag && e.len() == pbinds.len())
        })
        .map(|(_, e)| e)
        .ok_or_else(|| {
            anyhow!(
                "emit_clif: variant pattern `{tag}` doesn't match its predicate {pred:?}"
            )
        })?;
    let tag_ptr = cx.interned_str(tag)?;
    let helper = cx.helper("graphix_variant_tag_eq")?;
    // The helper checks arity as well as tag: same-tag arms at different
    // arities are distinct cases.
    let arity = cx.b.ins().iconst(types::I64, pbinds.len() as i64);
    let call = cx.b.ins().call(helper, &[v.0, v.1, tag_ptr, arity]);
    let mut cond = cx.b.inst_results(call)[0];
    for (idx, (sub, elt)) in pbinds.iter().zip(elts.iter()).enumerate() {
        match sub {
            StructPatternNode::Bind(id) => match kernel_abi::scalar_prim(elt) {
                Some(prim) => {
                    binds.push(SelectArmBind::Payload { id: *id, idx, prim, on })
                }
                None => {
                    let kind = owned_kind(elt).ok_or_else(|| {
                        anyhow!("emit_clif: variant payload shape {elt:?} not lowerable")
                    })?;
                    binds.push(SelectArmBind::PayloadValue { id: *id, idx, kind, on })
                }
            },
            StructPatternNode::Ignore => {}
            StructPatternNode::Literal(Value::String(lit)) => {
                let idx_c = cx.b.ins().iconst(types::I64, idx as i64);
                let call = cx.call_helper(
                    "graphix_variant_payload_borrowed",
                    &[v.0, v.1, idx_c],
                )?;
                let (pd, pp) = {
                    let rs = cx.b.inst_results(call);
                    (rs[0], rs[1])
                };
                let lit = cx.interned_str(lit)?;
                let call = cx.call_helper("graphix_value_is_str", &[pd, pp, lit])?;
                let eq = cx.b.inst_results(call)[0];
                cond = cx.b.ins().band(cond, eq);
            }
            StructPatternNode::Literal(lit) => {
                // The typed payload read is total: only the static type
                // proves it faithful.
                let prim = kernel_abi::scalar_prim_of_value(lit)
                    .filter(|p| kernel_abi::scalar_prim(elt) == Some(*p))
                    .ok_or_else(|| {
                        anyhow!(
                            "emit_clif: variant payload literal {lit:?} over {elt:?} \
                             not lowerable"
                        )
                    })?;
                let idx_c = cx.b.ins().iconst(types::I64, idx as i64);
                let call =
                    cx.call_helper(variant_payload_helper(prim), &[v.0, v.1, idx_c])?;
                let value = cx.b.inst_results(call)[0];
                let lit = compile_const(cx.b, lit, prim)?;
                let eq = compile_cmp(cx.b, CmpOp::Eq, prim, value, lit);
                cond = cx.b.ins().band(cond, eq);
            }
            StructPatternNode::Variant { tag, all: None, binds: nbinds } => {
                let idx_c = cx.b.ins().iconst(types::I64, idx as i64);
                let call = cx.call_helper(
                    "graphix_variant_payload_borrowed",
                    &[v.0, v.1, idx_c],
                )?;
                let rs = cx.b.inst_results(call);
                let nested = (rs[0], rs[1]);
                let inner =
                    emit_variant_cond(cx, nested, Some(nested), tag, nbinds, elt, binds)?;
                cond = cx.b.ins().band(cond, inner);
            }
            _ => {
                return Err(anyhow!(
                    "emit_clif: nested variant payload pattern not lowerable"
                ));
            }
        }
    }
    Ok(cond)
}

/// The name every pattern bind installs under; binds resolve by
/// BindId, so the name is never looked up.
const PAT_BIND_NAME: ArcStr = arcstr::literal!("__pat");

/// The `BindId` a [`SelectArmBind`] installs.
fn select_bind_id(b: &SelectArmBind) -> BindId {
    match b {
        SelectArmBind::Scrut(id)
        | SelectArmBind::ValueScalar { id, .. }
        | SelectArmBind::ValueOwned { id, .. }
        | SelectArmBind::Payload { id, .. }
        | SelectArmBind::PayloadValue { id, .. }
        | SelectArmBind::ListHead { id, .. }
        | SelectArmBind::ListTail { id, .. }
        | SelectArmBind::Elem { id, .. }
        | SelectArmBind::ElemValue { id, .. }
        | SelectArmBind::Subslice { id, .. } => *id,
    }
}

/// A drop-safe TAINT|STALE placeholder pair for a local of `kind`: the
/// or-chain's no-match binds. Standing unconditionally, unlike
/// `nodes::emit_bottom_placeholder`: a delivery that never happened is
/// not an event, so there is no trigger to follow.
fn placeholder_for_kind(
    cx: &mut BodyCx,
    kind: LocalKind,
) -> Result<(ClifValue, ClifValue)> {
    let abi = match kind {
        LocalKind::Scalar(p) => AbiKind::Scalar(p),
        LocalKind::String => AbiKind::String,
        LocalKind::Composite => AbiKind::Array,
        LocalKind::Value => AbiKind::Value,
    };
    let cv = emit_bottom_of_kind(cx, abi)?;
    Ok((cx.b.ins().bor_imm(cv.disc, STALE), cv.payload))
}

/// Emit an or-pattern arm's alternative chain: alternatives test left
/// to right; the first match installs its binds and jumps to one `done`
/// block whose params bind the arm's locals once under the shared
/// BindIds (layout from alternative 0; a mismatch Errs, never
/// miscompiles). `nomatch`: `Some` = the caller's fail block, jumped
/// with no binds; `None` = route through `done` with matched=0 and
/// tainted placeholders. Returns the matched i8 with the builder in
/// `done` and the binds installed; the caller's env mark, taken before
/// this, scopes the arm-exit drops over them.
fn emit_or_chain(
    cx: &mut BodyCx,
    alts: &[StructPatternNode],
    pred_typ: &Type,
    scrut: SelectScrut,
    scrut_typ: &Type,
    nomatch: Option<Block>,
) -> Result<ClifValue> {
    // Each alternative tests against its own member of the arm's
    // inferred Set; a non-Set predicate applies whole.
    let alt_types = set_members(pred_typ, alts.len());
    let tests: SmallVec<[Block; 4]> =
        (0..alts.len()).map(|_| cx.b.create_block()).collect();
    // the last alternative fails to `nomatch`, else to a block of its own
    // that hands the done block placeholders
    let last_fail = nomatch.unwrap_or_else(|| cx.b.create_block());
    cx.b.ins().jump(tests[0], &[]);
    let mut done: Option<Block> = None;
    let mut layout: SmallVec<[(BindId, LocalKind); 8]> = SmallVec::new();
    for (k, alt) in alts.iter().enumerate() {
        cx.b.switch_to_block(tests[k]);
        cx.b.seal_block(tests[k]);
        let fail_to = if k + 1 < alts.len() { tests[k + 1] } else { last_fail };
        let at: &Type = match &alt_types {
            Some(ts) => &ts[k],
            None => pred_typ,
        };
        let mut binds: SmallVec<[SelectArmBind; 8]> = SmallVec::new();
        let scond =
            emit_structure_cond(cx, alt, at, false, scrut, scrut_typ, &mut binds)?;
        let mk = cx.b.create_block();
        match scond {
            Some(c) => {
                cx.b.ins().brif(c, mk, &[], fail_to, &[]);
            }
            // An irrefutable alternative (last position only) takes unconditionally.
            None => {
                cx.b.ins().jump(mk, &[]);
            }
        }
        cx.b.switch_to_block(mk);
        cx.b.seal_block(mk);
        let amark = cx.env.mark();
        install_arm_binds(cx, &binds, scrut, None)?;
        let mut ids: SmallVec<[BindId; 8]> = binds.iter().map(select_bind_id).collect();
        ids.sort_by_key(|id| id.inner());
        if k == 0 {
            for id in ids.iter() {
                let l = cx.env.lookup_id(*id).ok_or_else(|| {
                    anyhow!("emit_clif: or-chain bind not in env after install")
                })?;
                layout.push((*id, l.kind));
            }
            let d = cx.b.create_block();
            cx.b.append_block_param(d, types::I8);
            for (_, kind) in layout.iter() {
                cx.b.append_block_param(d, types::I64);
                cx.b.append_block_param(d, local_payload_ty(*kind));
            }
            done = Some(d);
        } else if ids.len() != layout.len()
            || ids.iter().zip(layout.iter()).any(|(a, (b, _))| a != b)
        {
            return Err(anyhow!(
                "emit_clif: or-pattern alternatives bind different id sets"
            ));
        }
        let one = cx.b.ins().iconst(types::I8, 1);
        let mut args: SmallVec<[BlockArg; 20]> = SmallVec::new();
        args.push(one.into());
        for (id, kind) in layout.iter() {
            let l = cx.env.lookup_id(*id).ok_or_else(|| {
                anyhow!("emit_clif: or-chain bind not in env after install")
            })?;
            if l.kind != *kind {
                return Err(anyhow!(
                    "emit_clif: or-pattern alternative binds {id:?} at a \
                     different kind"
                ));
            }
            let dv = cx.b.use_var(l.words.disc);
            let pv = cx.b.use_var(l.words.payload);
            args.push(dv.into());
            args.push(pv.into());
        }
        // Ownership is forwarded to the done block's binds; no drops.
        cx.env.truncate(amark);
        cx.b.ins().jump(done.expect("or chain layout unset"), &args);
    }
    if nomatch.is_none() {
        let ph = last_fail;
        cx.b.switch_to_block(ph);
        cx.b.seal_block(ph);
        let zero = cx.b.ins().iconst(types::I8, 0);
        let mut args: SmallVec<[BlockArg; 20]> = SmallVec::new();
        args.push(zero.into());
        for (_, kind) in layout.iter() {
            let (d, pv) = placeholder_for_kind(cx, *kind)?;
            args.push(d.into());
            args.push(pv.into());
        }
        cx.b.ins().jump(done.expect("or chain emitted no alternative"), &args);
    }
    let d = done.ok_or_else(|| anyhow!("emit_clif: empty or-pattern chain"))?;
    cx.b.switch_to_block(d);
    cx.b.seal_block(d);
    let params: SmallVec<[ClifValue; 20]> =
        cx.b.block_params(d).iter().copied().collect();
    let matched = params[0];
    for (i, (id, kind)) in layout.iter().enumerate() {
        bind_local(
            cx,
            PAT_BIND_NAME,
            params[1 + 2 * i],
            params[2 + 2 * i],
            *kind,
            Some(*id),
        );
    }
    Ok(matched)
}
