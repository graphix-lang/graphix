//! `select` emission: scrutinee classification, pattern
//! conditions, arm dispatch, and the merge-shape protocol.

use crate::{
    BindId, Node, NodeView, Rt, Update, UserEvent,
    expr::ExprId,
    fusion::{
        self,
        kernel_abi::{self, AbiKind, PrimType},
    },
    node::{
        op::CmpOp,
        pattern::{PatternNode, SliceKind, StructPatternNode},
        select::Select,
    },
    typ::Type,
};
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use cranelift_codegen::ir::{
    Block, BlockArg, InstBuilder, Value as ClifValue, condcodes::IntCC, types,
};
use netidx_value::Value;
use poolshark::local::LPooled;

use super::{
    abi::{
        CompiledExpr, LocalKind, STALE, TAINT, ValueVar, bind_local, clean_disc,
        const_stale_gate, is_tainted, is_untainted, local_payload_ty, prim_to_value_disc,
        propagate_flags, propagate_stale, propagate_taint, scalar_disc, value_disc,
    },
    body::{
        BodyCx, ensure_owned_composite_src, ensure_owned_value_src, fold_stale,
        node_composite_source,
    },
    call::CompositeSource,
    lower::resolve_node_typ,
    scalar::{
        cast_u64_to_prim, compile_cmp, compile_const, prim_to_clif,
        scalar_to_payload_i64, struct_get_helper, valarray_get_helper,
        variant_payload_helper, zero_const,
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

/// The select scrutinee, emitted once up front; every arm condition
/// and pattern bind reuses these SSA values. `Opaque` (string) supports
/// only Ignore / guard arms. `disc` carries the scrutinee's taint,
/// OR-ed into every arm's result so a bottom scrutinee bottoms the select.
#[derive(Clone, Copy)]
pub(super) enum SelectScrut {
    Scalar {
        disc: ClifValue,
        value: ClifValue,
        prim: PrimType,
    },
    Value {
        disc: ClifValue,
        payload: ClifValue,
    },
    /// An array/tuple/struct scrutinee whose pointer stays live across
    /// the whole arm chain; structural patterns read elements through it.
    Composite {
        disc: ClifValue,
        ptr: ClifValue,
    },
    Opaque {
        disc: ClifValue,
    },
}

impl SelectScrut {
    pub(super) fn disc(&self) -> ClifValue {
        match self {
            SelectScrut::Scalar { disc, .. }
            | SelectScrut::Value { disc, .. }
            | SelectScrut::Composite { disc, .. }
            | SelectScrut::Opaque { disc } => *disc,
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

/// Read one scalar pattern leaf off the composite scrutinee. The read
/// is unchecked: callers must have proven the arm's length test first
/// (a tainted scrutinee's placeholder is an empty array).
fn read_scrut_elem(
    cx: &mut BodyCx,
    ptr: ClifValue,
    idx: ElemIdx,
    prim: PrimType,
) -> Result<ClifValue> {
    let (helper_name, idx_v) = match idx {
        ElemIdx::FromStart(j) => {
            (valarray_get_helper(prim)?, cx.b.ins().iconst(types::I64, j as i64))
        }
        ElemIdx::FromEnd { back, len } => {
            let b = cx.b.ins().iconst(types::I64, back as i64);
            (valarray_get_helper(prim)?, cx.b.ins().isub(len, b))
        }
        ElemIdx::StructField(i) => {
            (struct_get_helper(prim)?, cx.b.ins().iconst(types::I64, i as i64))
        }
    };
    let helper = cx.helper(helper_name)?;
    let call = cx.b.ins().call(helper, &[ptr, idx_v]);
    Ok(cx.b.inst_results(call)[0])
}

/// A pattern binding installed in the arm's matched region under the
/// pattern's `BindId`.
enum SelectArmBind {
    /// `n => ...` — bind the scalar scrutinee itself.
    Scrut(BindId),
    /// `T as n` over a `[T, null]` scrutinee — bind the matched
    /// non-null scalar payload after the type-predicate branch.
    NullableScalar { id: BindId, prim: PrimType },
    /// `` `Tag(n) `` — bind one scalar variant payload. The read is
    /// unchecked on a wrong tag, so it must be emitted inside the
    /// matched region, never in the fall-through chain.
    Payload { id: BindId, idx: usize, prim: PrimType },
    /// `` `Tag(xs) `` — bind one non-scalar variant payload, cloned out
    /// as an owned local of `kind` and dropped at the arm's scope exit.
    /// Legal under a mask: a wrong-tag read yields a drop-safe default
    /// behind a tainted disc.
    PayloadValue { id: BindId, idx: usize, kind: LocalKind },
    /// `[<a, b>]` / `[<h, rest..>]` — bind the j-th head of a list
    /// scrutinee, cloned out as an owned local of `kind` (legal under a mask).
    ListHead { id: BindId, idx: usize, kind: LocalKind },
    /// The rest bind: the k-th TAIL itself — O(1), shares the spine.
    ListTail { id: BindId, k: usize },
    /// `(x, y)` / `{f, ..}` / `[h, ..]` — bind one scalar leaf of a
    /// composite scrutinee (the scrutinee, or a borrowed interior pointer
    /// for a nested pattern). Emitted inside
    /// the matched region: the length tests gate the unchecked read.
    Elem { id: BindId, idx: ElemIdx, prim: PrimType, parent_ptr: ClifValue },
}

/// Collect the per-slot state sites in a scaffold-loop body: the
/// callsite `ExprId` of every nested collection HOF call, each of which
/// claims one per-slot state chain (see [`BodyCx::open_slot_tables`]).
/// A nested callback body lives behind its own lambda def and anchors
/// its sites in the chain its own loop opens.
pub(crate) fn slot_state_sites<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
) -> LPooled<Vec<ExprId>> {
    let mut ids: LPooled<Vec<ExprId>> = LPooled::take();
    fusion::for_each_node(node, &mut |n| match n.view() {
        NodeView::CallSite(cs) => {
            if let Some(crate::ApplyView::Lambda(l)) = cs.resolved_apply()
                && l.inline_callback_body().is_some()
            {
                ids.push(n.spec().id);
            }
        }
        _ => {}
    });
    ids
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
    let (scrut, scrut_kind, scrut_typ, scrut_drop) =
        classify_select_scrutinee(cx, sel, true)?;
    let scrut_bfired = {
        let d = scrut.disc();
        let ts = cx.b.ins().band_imm(d, TAINT | STALE);
        Some(cx.b.ins().icmp_imm(IntCC::Equal, ts, TAINT))
    };
    // A tainted scrutinee makes no selection: the arm chain routes it
    // to the miss trap, which bottoms the merge.
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
        scrut_kind,
        &scrut_typ,
        scrut_bfired,
        &mut |cx, body, mark, fires| {
            emit_select_value_arm(cx, body, mark, merge_shape, merge, scrut_disc, fires)
        },
        &mut |cx| emit_select_miss_value(cx, merge_shape, merge, scrut_disc),
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
        match kind {
            LocalKind::Composite => {
                let drop = cx.helper("graphix_valarray_drop")?;
                let p = cx.b.use_var(vv.payload);
                cx.b.ins().call(drop, &[p]);
            }
            LocalKind::Variant | LocalKind::Nullable | LocalKind::Value => {
                let drop = cx.helper("graphix_value_drop")?;
                let d = cx.b.use_var(vv.disc);
                let p = cx.b.use_var(vv.payload);
                cx.b.ins().call(drop, &[d, p]);
            }
            LocalKind::Scalar(_) | LocalKind::String => {
                return Err(anyhow!(
                    "emit_clif: scrutinee drop obligation of shape {kind:?} — \
                     classify bug"
                ));
            }
        }
        cx.env.truncate(mark);
    }
    Ok(CompiledExpr::new(rdisc, rpayload))
}

/// The final-arm fail block of a value-position select, reached only
/// when a tainted scrutinee misses every conditional arm: jump to the
/// merge with a drop-safe tainted bottom whose freshness is the
/// scrutinee's, so a standing bottom does not re-fire.
fn emit_select_miss_value(
    cx: &mut BodyCx,
    merge_shape: SelectMerge,
    merge: Block,
    scrut_disc: ClifValue,
) -> Result<()> {
    let s = cx.b.ins().band_imm(scrut_disc, STALE);
    emit_select_bottom_value(cx, merge_shape, merge, s)
}

/// Jump to the merge with a drop-safe tainted bottom whose freshness
/// is `stale_bits` (0 = fresh).
fn emit_select_bottom_value(
    cx: &mut BodyCx,
    merge_shape: SelectMerge,
    merge: Block,
    stale_bits: ClifValue,
) -> Result<()> {
    let (disc, payload) = match merge_shape {
        SelectMerge::Scalar(p) => {
            let d = cx.b.ins().iconst(types::I64, prim_to_value_disc(p) | TAINT);
            // `iconst` of a float type is invalid CLIF.
            let z = zero_const(cx.b, p);
            (d, z)
        }
        SelectMerge::Value => {
            let d = cx.b.ins().iconst(types::I64, value_disc::NULL | TAINT);
            let z = cx.b.ins().iconst(types::I64, 0);
            (d, z)
        }
        SelectMerge::Composite => {
            let buf_new = cx.helper("graphix_value_buf_new")?;
            let zero = cx.b.ins().iconst(types::I64, 0);
            let call = cx.b.ins().call(buf_new, &[zero]);
            let buf = cx.b.inst_results(call)[0];
            let fin = cx.helper("graphix_valarray_finalize")?;
            let call = cx.b.ins().call(fin, &[buf]);
            let arr = cx.b.inst_results(call)[0];
            let d = cx.b.ins().iconst(types::I64, value_disc::ARRAY | TAINT);
            (d, arr)
        }
        SelectMerge::String => {
            let buf_new = cx.helper("graphix_string_buf_new")?;
            let call = cx.b.ins().call(buf_new, &[]);
            let buf = cx.b.inst_results(call)[0];
            let fin = cx.helper("graphix_string_buf_finalize")?;
            let call = cx.b.ins().call(fin, &[buf]);
            let s = cx.b.inst_results(call)[0];
            let d = cx.b.ins().iconst(types::I64, value_disc::STRING | TAINT);
            (d, s)
        }
    };
    let disc = cx.b.ins().bor(disc, stale_bits);
    cx.b.ins().jump(merge, &[BlockArg::Value(disc), BlockArg::Value(payload)]);
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

/// Classify and emit the read of a select scrutinee. `allow_owned`: the
/// value-position caller has one merge point to discharge a
/// [`ScrutDrop`] at; the tail-position caller's arms terminate
/// individually, so an owned scrutinee refuses there.
pub(super) fn classify_select_scrutinee<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    sel: &Select<R, E>,
    allow_owned: bool,
) -> Result<(SelectScrut, AbiKind, Type, Option<ScrutDrop>)> {
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
    let mut drop_ob: Option<ScrutDrop> = None;
    let adopt = |cx: &mut BodyCx,
                 kind: LocalKind,
                 disc: ClifValue,
                 payload: ClifValue|
     -> Option<ScrutDrop> {
        let mark = cx.env.mark();
        let name: ArcStr = compact_str::format_compact!("__scrut{}", sel.spec.id.inner())
            .as_str()
            .into();
        let vv = bind_local(cx, name, disc, payload, kind, None);
        Some(ScrutDrop { kind, vv, mark })
    };
    let scrut = match scrut_kind {
        AbiKind::Scalar(p) => {
            let cv = sel.arg.node.emit_clif(cx)?;
            SelectScrut::Scalar { disc: cv.disc, value: cv.payload, prim: p }
        }
        AbiKind::Variant | AbiKind::Nullable | AbiKind::Value => {
            let owned = node_composite_source(&sel.arg.node) != CompositeSource::Borrowed;
            if owned && !allow_owned {
                return Err(anyhow!(
                    "emit_clif: owned value-shape select scrutinee in tail \
                     position — no merge point to drop at"
                ));
            }
            let cv = sel.arg.node.emit_clif(cx)?;
            if owned {
                let kind = match scrut_kind {
                    AbiKind::Variant => LocalKind::Variant,
                    AbiKind::Nullable => LocalKind::Nullable,
                    _ => LocalKind::Value,
                };
                drop_ob = adopt(cx, kind, cv.disc, cv.payload);
            }
            SelectScrut::Value { disc: cv.disc, payload: cv.payload }
        }
        AbiKind::Array | AbiKind::Tuple | AbiKind::Struct => {
            let owned = node_composite_source(&sel.arg.node) != CompositeSource::Borrowed;
            if owned && !allow_owned {
                return Err(anyhow!(
                    "emit_clif: owned composite select scrutinee in tail \
                     position — no merge point to drop at"
                ));
            }
            let cv = sel.arg.node.emit_clif(cx)?;
            if owned {
                drop_ob = adopt(cx, LocalKind::Composite, cv.disc, cv.payload);
            }
            SelectScrut::Composite { disc: cv.disc, ptr: cv.payload }
        }
        // A string scrutinee supports only Ignore / guard arms, so only
        // its disc is kept; the read is an owned ArcStr either way.
        AbiKind::String => {
            let cv = sel.arg.node.emit_clif(cx)?;
            let drop = cx.helper("graphix_arcstr_drop")?;
            cx.b.ins().call(drop, &[cv.payload]);
            SelectScrut::Opaque { disc: cv.disc }
        }
        AbiKind::Unit | AbiKind::Null => {
            return Err(anyhow!("emit_clif: select scrutinee of shape {scrut_kind:?}"));
        }
    };
    Ok((scrut, scrut_kind, scrut_typ, drop_ob))
}

/// Structure condition + scalar leaf binds for a tuple/struct/slice
/// pattern over a composite scrutinee, mirroring
/// `StructPatternNode::is_match` / `bind`: Slice tests `len == N`,
/// SlicePrefix/SliceSuffix/Struct `len >= N`; suffix leaves index from
/// the end, struct leaves read `a[i][1]`.
///
/// The length test is also the taint gate: a tainted composite is an
/// empty placeholder array, so the unchecked element reads (emitted
/// after the test) never touch it. `@` bindings, rest bindings,
/// non-scalar leaves and nested variant leaves refuse (the select
/// de-fuses).
fn emit_composite_pattern_cond(
    cx: &mut BodyCx,
    ptr: ClifValue,
    scrut_typ: &Type,
    pat: &StructPatternNode,
    fail: Block,
    binds: &mut smallvec::SmallVec<[SelectArmBind; 8]>,
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
    let (leaves, len_cc, n): (smallvec::SmallVec<[LeafSpec; 8]>, IntCC, usize) = match pat
    {
        StructPatternNode::Slice { kind, all, binds: pbinds } => {
            if matches!(kind, SliceKind::List) {
                return Err(anyhow!("emit_clif: list pattern not lowerable yet"));
            }
            if all.is_some() {
                return Err(anyhow!(
                    "emit_clif: whole-slice @ binding not lowerable (owned \
                     composite arm local)"
                ));
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
                .collect::<Result<smallvec::SmallVec<[_; 8]>>>()?;
            (leaves, IntCC::Equal, pbinds.len())
        }
        StructPatternNode::SlicePrefix { list, all, prefix, tail } => {
            if *list {
                return Err(anyhow!("emit_clif: list pattern not lowerable yet"));
            }
            if all.is_some() || tail.is_some() {
                return Err(anyhow!(
                    "emit_clif: slice-prefix @/rest binding not lowerable \
                     (owned subslice arm local)"
                ));
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
            if all.is_some() || head.is_some() {
                return Err(anyhow!(
                    "emit_clif: slice-suffix @/head binding not lowerable \
                     (owned subslice arm local)"
                ));
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
            if all.is_some() {
                return Err(anyhow!(
                    "emit_clif: whole-struct @ binding not lowerable (owned \
                     composite arm local)"
                ));
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
                    let typ = flds.get(*i).map(|(_, t)| t.clone()).ok_or_else(|| {
                        anyhow!(
                            "emit_clif: struct pattern field index {i} out \
                                 of range"
                        )
                    })?;
                    Ok(LeafSpec { idx: ElemIdx::StructField(*i), sub, typ })
                })
                .collect::<Result<smallvec::SmallVec<[_; 8]>>>()?;
            (leaves, IntCC::SignedGreaterThanOrEqual, sbinds.len())
        }
        _ => return Err(anyhow!("emit_clif: not a composite structural pattern")),
    };
    let mut lit_leaves: smallvec::SmallVec<[(ElemIdx, PrimType, &Value); 8]> =
        smallvec::SmallVec::new();
    let mut nested: smallvec::SmallVec<[(ElemIdx, &StructPatternNode, Type); 8]> =
        smallvec::SmallVec::new();
    for leaf in &leaves {
        match leaf.sub {
            StructPatternNode::Abstract { .. } => {
                return Err(anyhow!("emit_clif: abstract pattern leaf not lowerable"));
            }
            StructPatternNode::Ignore => {}
            StructPatternNode::Bind(id) => {
                let prim = kernel_abi::scalar_prim(&leaf.typ).ok_or_else(|| {
                    anyhow!(
                        "emit_clif: non-scalar select pattern leaf bind {:?}",
                        leaf.typ
                    )
                })?;
                binds.push(SelectArmBind::Elem {
                    id: *id,
                    idx: leaf.idx,
                    prim,
                    parent_ptr: ptr,
                });
            }
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
                return Err(anyhow!(
                    "emit_clif: or-pattern leaf not lowerable \
                     (design/or_patterns.md P3)"
                ));
            }
        }
    }
    let n_c = cx.b.ins().iconst(types::I64, n as i64);
    let len_ok = cx.b.ins().icmp(len_cc, len, n_c);
    if lit_leaves.is_empty() && nested.is_empty() {
        return Ok(len_ok);
    }
    // The reads below are unchecked, so the length is proven first.
    let stage = cx.b.create_block();
    cx.b.ins().brif(len_ok, stage, &[], fail, &[]);
    cx.b.switch_to_block(stage);
    cx.b.seal_block(stage);
    let mut cond: Option<ClifValue> = None;
    let mut fold = |cx: &mut BodyCx, c: ClifValue| {
        cond = Some(match cond {
            None => c,
            Some(p) => cx.b.ins().band(p, c),
        });
    };
    for (idx, prim, v) in lit_leaves {
        let elem = read_scrut_elem(cx, ptr, idx, prim)?;
        let lit = compile_const(cx.b, v, prim)?;
        let c = compile_cmp(cx.b, CmpOp::Eq, prim, elem, lit);
        fold(cx, c);
    }
    for (idx, sub, typ) in nested {
        // A borrowed interior pointer: the root is a pinned borrowed
        // slot and values are immutable, so no ownership or drops.
        let (helper_name, idx_v) = match idx {
            ElemIdx::FromStart(j) => (
                "graphix_valarray_get_array_borrowed",
                cx.b.ins().iconst(types::I64, j as i64),
            ),
            ElemIdx::FromEnd { back, len } => {
                let b = cx.b.ins().iconst(types::I64, back as i64);
                ("graphix_valarray_get_array_borrowed", cx.b.ins().isub(len, b))
            }
            ElemIdx::StructField(i) => (
                "graphix_struct_get_array_borrowed",
                cx.b.ins().iconst(types::I64, i as i64),
            ),
        };
        let helper = cx.helper(helper_name)?;
        let call = cx.b.ins().call(helper, &[ptr, idx_v]);
        let child_ptr = cx.b.inst_results(call)[0];
        let c = emit_composite_pattern_cond(cx, child_ptr, &typ, sub, fail, binds)?;
        fold(cx, c);
    }
    Ok(cond.expect("staged composite pattern with no conditions"))
}

/// The select's own-fire summary handed to each arm emitter.
/// `sound_stale`: the AND of the consulted guards' sound-plane STALE
/// bits at this arm's point (structure-failed arms and arms below the
/// taken one contribute nothing). `bfired` (i8 bool): the scrutinee
/// delivery was a fresh bottom.
#[derive(Clone, Copy)]
pub(super) struct SelFires {
    pub(super) sound_stale: Option<ClifValue>,
    pub(super) bfired: Option<ClifValue>,
}

/// One prologue-evaluated guard's planes.
#[derive(Clone, Copy)]
struct GuardPlanes {
    /// The sound-true verdict.
    eff: ClifValue,
    /// The channel is bottom.
    gbot: ClifValue,
    /// Sound-plane STALE.
    gs_sound: ClifValue,
    /// Fired-plane STALE.
    gfire: ClifValue,
}

/// The shared arm chain: pattern conditions, per-arm binds and the
/// fail-block plumbing, identical between value and tail position.
/// `emit_arm` runs in the matched block with the arm's binds installed
/// and must leave it terminated; `mark` is the env mark to truncate to.
/// `emit_miss` handles the final-arm miss (a tainted scrutinee);
/// `emit_undet` the undecidable outcome (a bottom consulted guard),
/// given the outcome's STALE bits (0 = fresh).
pub(super) fn emit_select_arms<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    sel: &Select<R, E>,
    scrut: SelectScrut,
    scrut_kind: AbiKind,
    scrut_typ: &Type,
    scrut_bfired: Option<ClifValue>,
    emit_arm: &mut dyn FnMut(&mut BodyCx, &Node<R, E>, usize, SelFires) -> Result<()>,
    emit_miss: &mut dyn FnMut(&mut BodyCx) -> Result<()>,
    emit_undet: &mut dyn FnMut(&mut BodyCx, ClifValue) -> Result<()>,
) -> Result<()> {
    // A tainted scrutinee makes no selection and runs no arm body:
    // every matched path re-checks the scrutinee disc before its body
    // and routes a tainted take to the shared miss block.
    let sdisc = scrut.disc();
    let miss_bl = cx.b.create_block();
    let n = sel.arms.len();
    // The guard prologue: the node-walk ticks every arm's guard every
    // cycle before matching, so every guard that is not pure in its
    // binds is evaluated here once per invocation and the chain reads
    // it; the consulted folds happen along the chain into
    // acc_sound/acc_fires.
    let mut guard_vals: smallvec::SmallVec<[Option<GuardPlanes>; 8]> =
        smallvec::smallvec![None; n];
    for (i, (pat, _)) in sel.arms.iter().enumerate() {
        let Some(g) = &pat.guard else { continue };
        if guard_is_pure_of_binds(pat, &g.node) {
            continue;
        }
        let gmark = cx.env.mark();
        let mut binds: smallvec::SmallVec<[SelectArmBind; 8]> = smallvec::SmallVec::new();
        let pcond = if let StructPatternNode::Or { alts } = &pat.structure_predicate {
            // The or-chain binds the shared BindIds itself, so `binds`
            // stays empty.
            if pat.explicit_type_predicate {
                return Err(anyhow!(
                    "emit_clif: explicit type predicate on an or-pattern arm \
                     not lowerable"
                ));
            }
            Some(emit_or_chain(
                cx,
                alts,
                &pat.type_predicate,
                scrut,
                scrut_kind,
                scrut_typ,
                None,
            )?)
        } else if composite_structural_arm(pat, scrut) {
            let fail_bl = cx.b.create_block();
            let done = cx.b.create_block();
            cx.b.append_block_param(done, types::I8);
            let (tcond, scond) = emit_arm_cond(
                cx,
                pat,
                scrut,
                scrut_kind,
                scrut_typ,
                Some(fail_bl),
                &mut binds,
            )?;
            debug_assert!(tcond.is_none());
            let c = scond.expect("composite arm without a structure condition");
            cx.b.ins().jump(done, &[c.into()]);
            cx.b.switch_to_block(fail_bl);
            cx.b.seal_block(fail_bl);
            let z = cx.b.ins().iconst(types::I8, 0);
            cx.b.ins().jump(done, &[z.into()]);
            cx.b.switch_to_block(done);
            cx.b.seal_block(done);
            Some(cx.b.block_params(done)[0])
        } else {
            let (tcond, scond) =
                emit_arm_cond(cx, pat, scrut, scrut_kind, scrut_typ, None, &mut binds)?;
            match (tcond, scond) {
                (None, None) => None,
                (Some(c), None) | (None, Some(c)) => Some(c),
                (Some(a), Some(b)) => Some(cx.b.ins().band(a, b)),
            }
        };
        install_arm_binds(cx, &binds, scrut, pcond)?;
        let gcv = g.node.emit_clif(cx)?;
        // gs_sound stales tainted productions (TAINT >> 1 == STALE);
        // gfire is the fired plane, sound or bottom.
        let gs = cx.b.ins().band_imm(gcv.disc, STALE);
        let gt = cx.b.ins().band_imm(gcv.disc, TAINT);
        let gts = cx.b.ins().ushr_imm(gt, 1);
        let gs_sound = cx.b.ins().bor(gs, gts);
        let gbot = is_tainted(cx.b, gcv.disc);
        let valid = is_untainted(cx.b, gcv.disc);
        let eff = cx.b.ins().band(gcv.payload, valid);
        // The masked installs clone owned payload binds per invocation;
        // drop them before the truncate.
        super::flow::emit_scope_drops(cx, gmark)?;
        cx.env.truncate(gmark);
        guard_vals[i] = Some(GuardPlanes { eff, gbot, gs_sound, gfire: gs });
    }
    // At any read these hold the fold over exactly the consultation
    // points control flow executed.
    let (acc_sound, acc_fires) = {
        let sv = cx.b.declare_var(types::I64);
        let fv = cx.b.declare_var(types::I64);
        let init = cx.b.ins().iconst(types::I64, STALE);
        cx.b.def_var(sv, init);
        cx.b.def_var(fv, init);
        (sv, fv)
    };
    let mut undet_bl: Option<Block> = None;
    for (i, (pat, body)) in sel.arms.iter().enumerate() {
        let is_last = i == n - 1;
        // A composite or or-pattern condition stages across blocks, so
        // its fail edge must exist before emission.
        let is_or = matches!(&pat.structure_predicate, StructPatternNode::Or { .. });
        let early_fail = if is_or || composite_structural_arm(pat, scrut) {
            Some(cx.b.create_block())
        } else {
            None
        };
        // An or-chain binds the shared BindIds in its done block, so the
        // env mark is taken before it and the arm-exit drops cover them.
        let mut binds: smallvec::SmallVec<[SelectArmBind; 8]> = smallvec::SmallVec::new();
        let mut or_mark: Option<usize> = None;
        let (tcond, scond) = if let StructPatternNode::Or { alts } =
            &pat.structure_predicate
        {
            if pat.explicit_type_predicate {
                return Err(anyhow!(
                    "emit_clif: explicit type predicate on an or-pattern arm \
                     not lowerable"
                ));
            }
            or_mark = Some(cx.env.mark());
            let m = emit_or_chain(
                cx,
                alts,
                &pat.type_predicate,
                scrut,
                scrut_kind,
                scrut_typ,
                Some(early_fail.unwrap()),
            )?;
            (None, Some(m))
        } else {
            emit_arm_cond(cx, pat, scrut, scrut_kind, scrut_typ, early_fail, &mut binds)?
        };
        let pcond = match (tcond, scond) {
            (None, None) => None,
            (Some(c), None) | (None, Some(c)) => Some(c),
            (Some(a), Some(b)) => Some(cx.b.ins().band(a, b)),
        };
        let has_guard = pat.guard.is_some();
        // The final-arm miss trap is sound only when a miss is
        // impossible; typecheck forbids a guarded final arm.
        if is_last && has_guard {
            return Err(anyhow!(
                "emit_clif: guard on the final select arm — the chain \
                 could miss every arm"
            ));
        }
        let matched = cx.b.create_block();
        let fail: Option<Block> = match early_fail {
            Some(f) => Some(f),
            None if pcond.is_some() || has_guard => Some(cx.b.create_block()),
            None => None,
        };
        match pcond {
            Some(c) => {
                cx.b.ins().brif(c, matched, &[], fail.unwrap(), &[]);
            }
            None => {
                cx.b.ins().jump(matched, &[]);
            }
        }
        cx.b.switch_to_block(matched);
        cx.b.seal_block(matched);
        let mark = or_mark.unwrap_or_else(|| cx.env.mark());
        install_arm_binds(cx, &binds, scrut, None)?;
        if let Some(g) = &pat.guard {
            let eff = match guard_vals[i] {
                // The consultation point: fold the guard's planes into
                // the accumulators; a bottom channel makes the selection
                // undecidable and the chain stops here.
                Some(GuardPlanes { eff, gbot, gs_sound, gfire }) => {
                    let cur = cx.b.use_var(acc_sound);
                    let n = cx.b.ins().band(cur, gs_sound);
                    cx.b.def_var(acc_sound, n);
                    let cur = cx.b.use_var(acc_fires);
                    let n = cx.b.ins().band(cur, gfire);
                    cx.b.def_var(acc_fires, n);
                    let ub = *undet_bl.get_or_insert_with(|| cx.b.create_block());
                    let cont = cx.b.create_block();
                    // Drop this arm's owned binds before the shared undet block.
                    let ubdrop = cx.b.create_block();
                    cx.b.ins().brif(gbot, ubdrop, &[], cont, &[]);
                    cx.b.switch_to_block(ubdrop);
                    cx.b.seal_block(ubdrop);
                    super::flow::emit_scope_drops(cx, mark)?;
                    cx.b.ins().jump(ub, &[]);
                    cx.b.switch_to_block(cont);
                    cx.b.seal_block(cont);
                    eff
                }
                // Schedule-free: pure and never bottom, so no
                // undetermined case and no fold.
                None => {
                    let gcv = g.node.emit_clif(cx)?;
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
            super::flow::emit_scope_drops(cx, mark)?;
            cx.b.ins().jump(fail.unwrap(), &[]);
            cx.b.switch_to_block(body_blk);
            cx.b.seal_block(body_blk);
        }
        // A matched arm under a tainted scrutinee must not run its body:
        // route to the miss trap, dropping the arm's owned binds first.
        let body_ok = cx.b.create_block();
        let clean = is_untainted(cx.b, sdisc);
        let tdrop = cx.b.create_block();
        cx.b.ins().brif(clean, body_ok, &[], tdrop, &[]);
        cx.b.switch_to_block(tdrop);
        cx.b.seal_block(tdrop);
        super::flow::emit_scope_drops(cx, mark)?;
        cx.b.ins().jump(miss_bl, &[]);
        cx.b.switch_to_block(body_ok);
        cx.b.seal_block(body_ok);
        // Read here, the sound accumulator holds exactly the consulted
        // guards up to and including this arm.
        let fires =
            SelFires { sound_stale: Some(cx.b.use_var(acc_sound)), bfired: scrut_bfired };
        emit_arm(cx, body, mark, fires)?;
        match fail {
            Some(f) => {
                cx.b.switch_to_block(f);
                cx.b.seal_block(f);
                if is_last {
                    // Reached only under a tainted scrutinee: every arm missed.
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
        // the scrutinee, a consulted guard, or a fresh-bottom delivery.
        let undet_stale = {
            let ss = cx.b.ins().band_imm(sdisc, STALE);
            let af = cx.b.use_var(acc_fires);
            let st = cx.b.ins().band(ss, af);
            match scrut_bfired {
                Some(bf) => {
                    let z = cx.b.ins().iconst(types::I64, 0);
                    let stale = cx.b.ins().iconst(types::I64, STALE);
                    let bfs = cx.b.ins().select(bf, z, stale);
                    cx.b.ins().band(st, bfs)
                }
                None => st,
            }
        };
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
    let mut bind_ids: smallvec::SmallVec<[BindId; 8]> = smallvec::SmallVec::new();
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

/// True when `pat` is a composite structural pattern over a composite
/// scrutinee: its condition stages across blocks and needs a
/// pre-created fail edge before [`emit_arm_cond`] runs.
fn composite_structural_arm<R: Rt, E: UserEvent>(
    pat: &PatternNode<R, E>,
    scrut: SelectScrut,
) -> bool {
    matches!(
        &pat.structure_predicate,
        StructPatternNode::Slice { .. }
            | StructPatternNode::SlicePrefix { .. }
            | StructPatternNode::SliceSuffix { .. }
            | StructPatternNode::Struct { .. }
    ) && matches!(scrut, SelectScrut::Composite { .. })
}

/// Emit arm `pat`'s pattern condition against `scrut`: the type
/// predicate (`tcond`) and the structure condition (`scond`), pushing
/// the pattern's binds onto `binds`. Composite structural patterns
/// stage across blocks with fail edges into `early_fail`.
fn emit_arm_cond<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    pat: &PatternNode<R, E>,
    scrut: SelectScrut,
    scrut_kind: AbiKind,
    scrut_typ: &Type,
    early_fail: Option<Block>,
    binds: &mut smallvec::SmallVec<[SelectArmBind; 8]>,
) -> Result<(Option<ClifValue>, Option<ClifValue>)> {
    // The node-walk tests the type predicate only when it is explicit.
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
            Type::Primitive(p)
                if p.contains(netidx_value::Typ::Null) && p.iter().count() == 1 =>
            {
                match scrut {
                    SelectScrut::Value { disc, .. }
                        if matches!(scrut_kind, AbiKind::Nullable) =>
                    {
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
                    SelectScrut::Value { disc, .. }
                        if matches!(scrut_kind, AbiKind::Nullable)
                            && kernel_abi::nullable_inner(&scrut_typ)
                                .as_ref()
                                .and_then(|t| kernel_abi::scalar_prim(t))
                                == PrimType::from_typ(pt) =>
                    {
                        let cd = clean_disc(cx.b, disc);
                        match kernel_abi::nullable_error_marked(&scrut_typ) {
                            // `[T, null]`: "is a T" is "is not null".
                            Some(false) => Some(cx.b.ins().icmp_imm(
                                IntCC::NotEqual,
                                cd,
                                value_disc::NULL,
                            )),
                            // `[T, Error<E>]`: the error is not null, so
                            // "is a T" is the positive test against T's disc.
                            Some(true) => match PrimType::from_typ(pt) {
                                Some(prim) => {
                                    let td = scalar_disc(cx.b, prim);
                                    Some(cx.b.ins().icmp(IntCC::Equal, cd, td))
                                }
                                None if pt == netidx_value::Typ::String => {
                                    Some(cx.b.ins().icmp_imm(
                                        IntCC::Equal,
                                        cd,
                                        value_disc::STRING,
                                    ))
                                }
                                None => {
                                    return Err(anyhow!(
                                        "emit_clif: non-register type \
                                             predicate {pred:?} over a result \
                                             union not lowerable"
                                    ));
                                }
                            },
                            None => {
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
        pat.explicit_type_predicate,
        tcond.is_some(),
        scrut,
        scrut_kind,
        scrut_typ,
        early_fail,
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
    binds: &mut smallvec::SmallVec<[SelectArmBind; 8]>,
) -> Result<ClifValue> {
    let (disc, payload) = match scrut {
        SelectScrut::Value { disc, payload } => (disc, payload),
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
            if all.is_some() {
                return Err(anyhow!("emit_clif: whole-list @ binding not lowerable"));
            }
            (pb, pb.len(), true, None)
        }
        StructPatternNode::SlicePrefix { list: true, all, prefix, tail } => {
            if all.is_some() {
                return Err(anyhow!("emit_clif: whole-list @ binding not lowerable"));
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

/// The [`LocalKind`] a non-scalar variant payload element binds as —
/// by its ABI kind. `Unit`/`Null` payloads (and shapes with no kernel
/// encoding) refuse.
fn payload_local_kind(t: &Type) -> Option<LocalKind> {
    match kernel_abi::abi_kind(t)? {
        AbiKind::Scalar(p) => Some(LocalKind::Scalar(p)),
        AbiKind::Array | AbiKind::Tuple | AbiKind::Struct => Some(LocalKind::Composite),
        AbiKind::String => Some(LocalKind::String),
        AbiKind::Variant => Some(LocalKind::Variant),
        AbiKind::Nullable => Some(LocalKind::Nullable),
        AbiKind::Value => Some(LocalKind::Value),
        AbiKind::Unit | AbiKind::Null => None,
    }
}

/// Install an arm's `binds` into the env.
/// `mask` is the arm's pattern condition when the caller has NOT
/// branched on it (the guard prologue); the take chain installs
/// inside the matched block and passes `None`.
fn install_arm_binds(
    cx: &mut BodyCx,
    binds: &smallvec::SmallVec<[SelectArmBind; 8]>,
    scrut: SelectScrut,
    mask: Option<ClifValue>,
) -> Result<()> {
    for bind in binds {
        match bind {
            SelectArmBind::Scrut(id) => {
                let SelectScrut::Scalar { disc, value, prim } = scrut else {
                    return Err(anyhow!(
                        "emit_clif: scrutinee bind without a scalar \
                             scrutinee"
                    ));
                };
                let name: ArcStr =
                    compact_str::format_compact!("__pat{}", id.inner()).as_str().into();
                let disc = mask_unmatched(cx, disc, mask);
                bind_local(cx, name, disc, value, LocalKind::Scalar(prim), Some(*id));
            }
            SelectArmBind::NullableScalar { id, prim } => {
                let SelectScrut::Value { disc, payload } = scrut else {
                    return Err(anyhow!(
                        "emit_clif: nullable scalar bind without a value scrutinee"
                    ));
                };
                let name: ArcStr =
                    compact_str::format_compact!("__pat{}", id.inner()).as_str().into();
                let value = cast_u64_to_prim(cx.b, payload, *prim);
                let base = scalar_disc(cx.b, *prim);
                let bound_disc = propagate_flags(cx.b, base, &[disc]);
                let bound_disc = mask_unmatched(cx, bound_disc, mask);
                bind_local(
                    cx,
                    name,
                    bound_disc,
                    value,
                    LocalKind::Scalar(*prim),
                    Some(*id),
                );
            }
            SelectArmBind::Payload { id, idx, prim } => {
                let SelectScrut::Value { disc, payload } = scrut else {
                    return Err(anyhow!(
                        "emit_clif: payload bind without a variant \
                             scrutinee"
                    ));
                };
                let helper = cx.helper(variant_payload_helper(*prim)?)?;
                let idx_c = cx.b.ins().iconst(types::I64, *idx as i64);
                let call = cx.b.ins().call(helper, &[disc, payload, idx_c]);
                let v = cx.b.inst_results(call)[0];
                let name: ArcStr =
                    compact_str::format_compact!("__pat{}", id.inner()).as_str().into();
                let base = scalar_disc(cx.b, *prim);
                let pdisc = propagate_flags(cx.b, base, &[disc]);
                let pdisc = mask_unmatched(cx, pdisc, mask);
                bind_local(cx, name, pdisc, v, LocalKind::Scalar(*prim), Some(*id));
            }
            SelectArmBind::PayloadValue { id, idx, kind } => {
                let SelectScrut::Value { disc, payload } = scrut else {
                    return Err(anyhow!(
                        "emit_clif: payload bind without a variant \
                             scrutinee"
                    ));
                };
                let idx_c = cx.b.ins().iconst(types::I64, *idx as i64);
                let (vdisc, vpayload) = match kind {
                    LocalKind::Composite => {
                        let h = cx.helper("graphix_variant_payload_array")?;
                        let call = cx.b.ins().call(h, &[disc, payload, idx_c]);
                        let bits = cx.b.inst_results(call)[0];
                        (cx.b.ins().iconst(types::I64, value_disc::ARRAY), bits)
                    }
                    LocalKind::String => {
                        let h = cx.helper("graphix_variant_payload_string")?;
                        let call = cx.b.ins().call(h, &[disc, payload, idx_c]);
                        let bits = cx.b.inst_results(call)[0];
                        (cx.b.ins().iconst(types::I64, value_disc::STRING), bits)
                    }
                    LocalKind::Variant | LocalKind::Nullable | LocalKind::Value => {
                        let h = cx.helper("graphix_variant_payload_value")?;
                        let call = cx.b.ins().call(h, &[disc, payload, idx_c]);
                        let rs = cx.b.inst_results(call);
                        (rs[0], rs[1])
                    }
                    LocalKind::Scalar(_) => {
                        return Err(anyhow!(
                            "emit_clif: scalar payload routed to the \
                                 value bind path"
                        ));
                    }
                };
                let name: ArcStr =
                    compact_str::format_compact!("__pat{}", id.inner()).as_str().into();
                let pdisc = propagate_flags(cx.b, vdisc, &[disc]);
                let pdisc = mask_unmatched(cx, pdisc, mask);
                bind_local(cx, name, pdisc, vpayload, *kind, Some(*id));
            }
            SelectArmBind::ListHead { id, idx, kind } => {
                let SelectScrut::Value { disc, payload } = scrut else {
                    return Err(anyhow!(
                        "emit_clif: list head bind without a value scrutinee"
                    ));
                };
                let j = cx.b.ins().iconst(types::I64, *idx as i64);
                let (vdisc, vpayload) = match kind {
                    LocalKind::Composite => {
                        let h = cx.helper("graphix_list_get_array")?;
                        let call = cx.b.ins().call(h, &[disc, payload, j]);
                        let bits = cx.b.inst_results(call)[0];
                        (cx.b.ins().iconst(types::I64, value_disc::ARRAY), bits)
                    }
                    LocalKind::String => {
                        let h = cx.helper("graphix_list_get_string")?;
                        let call = cx.b.ins().call(h, &[disc, payload, j]);
                        let bits = cx.b.inst_results(call)[0];
                        (cx.b.ins().iconst(types::I64, value_disc::STRING), bits)
                    }
                    LocalKind::Scalar(p) => {
                        let h = cx.helper("graphix_list_get_value")?;
                        let call = cx.b.ins().call(h, &[disc, payload, j]);
                        let raw = cx.b.inst_results(call)[1];
                        (scalar_disc(cx.b, *p), cast_u64_to_prim(cx.b, raw, *p))
                    }
                    LocalKind::Variant | LocalKind::Nullable | LocalKind::Value => {
                        let h = cx.helper("graphix_list_get_value")?;
                        let call = cx.b.ins().call(h, &[disc, payload, j]);
                        let rs = cx.b.inst_results(call);
                        (rs[0], rs[1])
                    }
                };
                let name: ArcStr =
                    compact_str::format_compact!("__pat{}", id.inner()).as_str().into();
                let pdisc = propagate_flags(cx.b, vdisc, &[disc]);
                let pdisc = mask_unmatched(cx, pdisc, mask);
                bind_local(cx, name, pdisc, vpayload, *kind, Some(*id));
            }
            SelectArmBind::ListTail { id, k } => {
                let SelectScrut::Value { disc, payload } = scrut else {
                    return Err(anyhow!(
                        "emit_clif: list tail bind without a value scrutinee"
                    ));
                };
                let kc = cx.b.ins().iconst(types::I64, *k as i64);
                let h = cx.helper("graphix_list_tail")?;
                let call = cx.b.ins().call(h, &[disc, payload, kc]);
                let rs = cx.b.inst_results(call);
                let (vdisc, vpayload) = (rs[0], rs[1]);
                let name: ArcStr =
                    compact_str::format_compact!("__pat{}", id.inner()).as_str().into();
                let pdisc = propagate_flags(cx.b, vdisc, &[disc]);
                let pdisc = mask_unmatched(cx, pdisc, mask);
                bind_local(cx, name, pdisc, vpayload, LocalKind::Value, Some(*id));
            }
            SelectArmBind::Elem { id, idx, prim, parent_ptr } => {
                let SelectScrut::Composite { disc, .. } = scrut else {
                    return Err(anyhow!(
                        "emit_clif: element bind without a composite \
                             scrutinee"
                    ));
                };
                // The arm's length tests proved the element exists.
                let v = read_scrut_elem(cx, *parent_ptr, *idx, *prim)?;
                let name: ArcStr =
                    compact_str::format_compact!("__pat{}", id.inner()).as_str().into();
                let base = scalar_disc(cx.b, *prim);
                let pdisc = propagate_flags(cx.b, base, &[disc]);
                let pdisc = mask_unmatched(cx, pdisc, mask);
                bind_local(cx, name, pdisc, v, LocalKind::Scalar(*prim), Some(*id));
            }
        }
    }
    Ok(())
}

/// Value-position arm-body emission: widen the arm's result to the
/// select's merge shape and jump to the merge block.
fn emit_select_value_arm<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    body: &Node<R, E>,
    mark: usize,
    merge_shape: SelectMerge,
    merge: Block,
    scrut_disc: ClifValue,
    fires: SelFires,
) -> Result<()> {
    use NodeView;
    let body_frozen =
        kernel_abi::freeze_for_abi_normalized(body.typ()).ok_or_else(|| {
            anyhow!("emit_clif: select arm type {:?} doesn't freeze concrete", body.typ())
        })?;
    // A `never()` arm is a standing bottom: it fires only with the
    // scrutinee, through the STALE fold below.
    let bottom_arm =
        matches!(body_frozen, Type::Bottom) || matches!(body.view(), NodeView::Never(_));
    let (disc, payload) = if bottom_arm {
        let kind = match merge_shape {
            SelectMerge::Scalar(rp) => AbiKind::Scalar(rp),
            SelectMerge::Value => AbiKind::Value,
            SelectMerge::Composite => AbiKind::Tuple,
            SelectMerge::String => AbiKind::String,
        };
        let cv = super::nodes::emit_bottom_of_kind(cx, kind)?;
        (cx.b.ins().bor_imm(cv.disc, STALE), cv.payload)
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
    let d = match fires.sound_stale {
        Some(gs) => fold_stale(cx.b, d, gs),
        None => d,
    };
    // When every fired consumed input was a bottom (stale after the
    // sound folds, but a fresh-bottom fire happened) emit a fresh
    // TAINT; the payload stays valid and owned.
    let d = match fires.bfired {
        Some(bf) => {
            let sbit = cx.b.ins().band_imm(d, STALE);
            let quiet = cx.b.ins().icmp_imm(IntCC::NotEqual, sbit, 0);
            let ov = cx.b.ins().band(quiet, bf);
            let d_bot = cx.b.ins().band_imm(d, !STALE);
            let d_bot = cx.b.ins().bor_imm(d_bot, TAINT);
            cx.b.ins().select(ov, d_bot, d)
        }
        None => d,
    };
    // Drop the arm's owned pattern binds; the widening above made the
    // result independently owned.
    super::flow::emit_scope_drops(cx, mark)?;
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
    use NodeView;
    Ok(match merge_shape {
        SelectMerge::Scalar(rp) => {
            if kernel_abi::scalar_prim(&body_frozen) != Some(rp) {
                return Err(anyhow!(
                    "emit_clif: select arm type {body_frozen:?} doesn't \
                     match the scalar merge {rp:?}"
                ));
            }
            let cv = body.emit_clif(cx)?;
            (cv.disc, cv.payload)
        }
        SelectMerge::Value => {
            match kernel_abi::abi_kind(&body_frozen) {
                Some(AbiKind::Null) => {
                    // Only the literal null constant is recognized as a
                    // bare-null arm body.
                    match body.view() {
                        NodeView::Constant(c) if matches!(c.value, Value::Null) => {}
                        _ => {
                            return Err(anyhow!(
                                "emit_clif: null-typed select arm isn't \
                                 a null literal"
                            ));
                        }
                    }
                    // Same STALE gate as `emit_const_node`: a literal
                    // fires only at init.
                    let init = cx.init_flag();
                    let d = cx.b.ins().iconst(types::I64, value_disc::NULL);
                    let d = const_stale_gate(cx.b, init, d);
                    let p = cx.b.ins().iconst(types::I64, 0);
                    (d, p)
                }
                Some(AbiKind::Scalar(p)) => {
                    let cv = body.emit_clif(cx)?;
                    (cv.disc, scalar_to_payload_i64(cx.b, p, cv.payload))
                }
                Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
                    let cv = body.emit_clif(cx)?;
                    ensure_owned_value_src(
                        cx,
                        node_composite_source(body),
                        cv.disc,
                        cv.payload,
                    )?
                }
                other => {
                    return Err(anyhow!(
                        "emit_clif: select arm of shape {other:?} can't \
                         widen to the Value merge"
                    ));
                }
            }
        }
        SelectMerge::Composite => {
            if !matches!(
                kernel_abi::abi_kind(&body_frozen),
                Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct)
            ) {
                return Err(anyhow!(
                    "emit_clif: select arm type {body_frozen:?} doesn't \
                     match the composite merge"
                ));
            }
            let cv = body.emit_clif(cx)?;
            let v =
                ensure_owned_composite_src(cx, node_composite_source(body), cv.payload)?;
            (cv.disc, v)
        }
        SelectMerge::String => {
            if !matches!(kernel_abi::abi_kind(&body_frozen), Some(AbiKind::String)) {
                return Err(anyhow!(
                    "emit_clif: select arm type {body_frozen:?} doesn't \
                     match the string merge"
                ));
            }
            let cv = body.emit_clif(cx)?;
            (cv.disc, cv.payload)
        }
    })
}

/// One structure pattern's condition against the scrutinee: the
/// per-shape half of [`emit_arm_cond`], also called by [`emit_or_chain`]
/// once per alternative with that alternative's member of the arm's
/// inferred predicate (`explicit_pred`/`has_tcond` both false).
fn emit_structure_cond(
    cx: &mut BodyCx,
    sp: &StructPatternNode,
    pred_typ: &Type,
    explicit_pred: bool,
    has_tcond: bool,
    scrut: SelectScrut,
    scrut_kind: AbiKind,
    scrut_typ: &Type,
    early_fail: Option<Block>,
    binds: &mut smallvec::SmallVec<[SelectArmBind; 8]>,
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
            SelectScrut::Value { .. } if matches!(scrut_kind, AbiKind::Nullable) => {
                let pred = kernel_abi::freeze_for_abi(pred_typ);
                let Some(prim) =
                    pred.as_ref().and_then(|typ| kernel_abi::scalar_prim(typ))
                else {
                    return Err(anyhow!(
                        "emit_clif: nullable scrutinee bind predicate is not scalar"
                    ));
                };
                // Over a result union the payload read is safe only under
                // the explicit predicate's positive disc test; an
                // inferred-predicate bind has no test, so it refuses.
                if !explicit_pred
                    && kernel_abi::nullable_error_marked(&scrut_typ) != Some(false)
                {
                    return Err(anyhow!(
                        "emit_clif: untested bind over a result union \
                             {scrut_typ:?} not lowerable"
                    ));
                }
                binds.push(SelectArmBind::NullableScalar { id: *id, prim });
                None
            }
            SelectScrut::Value { .. }
            | SelectScrut::Composite { .. }
            | SelectScrut::Opaque { .. } => {
                return Err(anyhow!(
                    "emit_clif: non-scalar scrutinee bind pattern not \
                         yet lowerable"
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
                _ => {
                    return Err(anyhow!(
                        "emit_clif: literal pattern prim {lit_prim:?} \
                             doesn't match scrutinee {scrut_typ:?}"
                    ));
                }
            }
        }
        StructPatternNode::Variant { tag, all, binds: pbinds } => {
            if all.is_some() {
                return Err(anyhow!("emit_clif: whole-variant @ binding not lowerable"));
            }
            let (disc, payload) = match scrut {
                SelectScrut::Value { disc, payload }
                    if matches!(scrut_kind, AbiKind::Variant) =>
                {
                    (disc, payload)
                }
                _ => {
                    return Err(anyhow!(
                        "emit_clif: variant pattern over non-variant \
                             scrutinee {scrut_typ:?}"
                    ));
                }
            };
            let pred = kernel_abi::freeze_for_abi(pred_typ).ok_or_else(|| {
                anyhow!(
                    "emit_clif: variant pattern predicate {:?} \
                             doesn't freeze concrete",
                    pred_typ
                )
            })?;
            let elts = match &pred {
                Type::Variant(ptag, elts)
                    if ptag == tag && elts.len() == pbinds.len() =>
                {
                    elts
                }
                _ => {
                    return Err(anyhow!(
                        "emit_clif: variant pattern `{tag}` doesn't \
                             match its predicate {pred:?}"
                    ));
                }
            };
            for (idx, (sub, elt)) in pbinds.iter().zip(elts.iter()).enumerate() {
                match sub {
                    StructPatternNode::Bind(id) => match kernel_abi::scalar_prim(elt) {
                        Some(prim) => {
                            binds.push(SelectArmBind::Payload { id: *id, idx, prim })
                        }
                        None => {
                            let kind = payload_local_kind(elt).ok_or_else(|| {
                                anyhow!(
                                    "emit_clif: variant payload shape \
                                             {elt:?} not lowerable"
                                )
                            })?;
                            binds.push(SelectArmBind::PayloadValue {
                                id: *id,
                                idx,
                                kind,
                            });
                        }
                    },
                    StructPatternNode::Ignore => {}
                    StructPatternNode::Literal(_)
                    | StructPatternNode::Slice { .. }
                    | StructPatternNode::SlicePrefix { .. }
                    | StructPatternNode::SliceSuffix { .. }
                    | StructPatternNode::Struct { .. }
                    | StructPatternNode::Variant { .. }
                    | StructPatternNode::Abstract { .. }
                    | StructPatternNode::Or { .. } => {
                        return Err(anyhow!(
                            "emit_clif: nested variant payload \
                                 pattern not lowerable"
                        ));
                    }
                }
            }
            let tag_ptr = cx.interned_str(tag)?;
            let helper = cx.helper("graphix_variant_tag_eq")?;
            // The helper checks arity as well as tag: same-tag arms at
            // different arities are distinct cases.
            let arity = cx.b.ins().iconst(types::I64, pbinds.len() as i64);
            let call = cx.b.ins().call(helper, &[disc, payload, tag_ptr, arity]);
            Some(cx.b.inst_results(call)[0])
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
                Some(emit_composite_pattern_cond(
                    cx,
                    ptr,
                    scrut_typ,
                    p,
                    early_fail.unwrap(),
                    binds,
                )?)
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

/// The arm-local name a pattern bind installs under.
fn pat_bind_name(id: BindId) -> ArcStr {
    compact_str::format_compact!("__pat{}", id.inner()).as_str().into()
}

/// The `BindId` a [`SelectArmBind`] installs.
fn select_bind_id(b: &SelectArmBind) -> BindId {
    match b {
        SelectArmBind::Scrut(id)
        | SelectArmBind::NullableScalar { id, .. }
        | SelectArmBind::Payload { id, .. }
        | SelectArmBind::PayloadValue { id, .. }
        | SelectArmBind::ListHead { id, .. }
        | SelectArmBind::ListTail { id, .. }
        | SelectArmBind::Elem { id, .. } => *id,
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
    Ok(match kind {
        LocalKind::Scalar(p) => {
            let d = cx.b.ins().iconst(types::I64, prim_to_value_disc(p) | TAINT | STALE);
            (d, zero_const(cx.b, p))
        }
        LocalKind::String => {
            let h = cx.helper("graphix_arcstr_empty")?;
            let call = cx.b.ins().call(h, &[]);
            let sp = cx.b.inst_results(call)[0];
            let d = cx.b.ins().iconst(types::I64, value_disc::STRING | TAINT | STALE);
            (d, sp)
        }
        LocalKind::Composite => {
            let h = cx.helper("graphix_valarray_empty")?;
            let call = cx.b.ins().call(h, &[]);
            let a = cx.b.inst_results(call)[0];
            let d = cx.b.ins().iconst(types::I64, value_disc::ARRAY | TAINT | STALE);
            (d, a)
        }
        LocalKind::Variant | LocalKind::Nullable | LocalKind::Value => {
            let d = cx.b.ins().iconst(types::I64, value_disc::NULL | TAINT | STALE);
            let z = cx.b.ins().iconst(types::I64, 0);
            (d, z)
        }
    })
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
    scrut_kind: AbiKind,
    scrut_typ: &Type,
    nomatch: Option<Block>,
) -> Result<ClifValue> {
    // Each alternative tests against its own member of the arm's
    // inferred Set; a non-Set predicate applies whole.
    let alt_types = pred_typ.with_deref(|t| match t {
        Some(Type::Set(ts)) if ts.len() == alts.len() => Some(ts.clone()),
        _ => None,
    });
    let tests: smallvec::SmallVec<[Block; 4]> =
        (0..alts.len()).map(|_| cx.b.create_block()).collect();
    let ph_bl = match nomatch {
        None => Some(cx.b.create_block()),
        Some(_) => None,
    };
    cx.b.ins().jump(tests[0], &[]);
    let mut done: Option<Block> = None;
    let mut layout: smallvec::SmallVec<[(BindId, LocalKind); 8]> =
        smallvec::SmallVec::new();
    for (k, alt) in alts.iter().enumerate() {
        cx.b.switch_to_block(tests[k]);
        cx.b.seal_block(tests[k]);
        let fail_to = if k + 1 < alts.len() {
            tests[k + 1]
        } else {
            match (nomatch, ph_bl) {
                (Some(f), _) => f,
                (None, Some(ph)) => ph,
                (None, None) => unreachable!(),
            }
        };
        let at: &Type = match &alt_types {
            Some(ts) => &ts[k],
            None => pred_typ,
        };
        let mut binds: smallvec::SmallVec<[SelectArmBind; 8]> = smallvec::SmallVec::new();
        let scond = emit_structure_cond(
            cx,
            alt,
            at,
            false,
            false,
            scrut,
            scrut_kind,
            scrut_typ,
            Some(fail_to),
            &mut binds,
        )?;
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
        let mut ids: smallvec::SmallVec<[BindId; 8]> =
            binds.iter().map(select_bind_id).collect();
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
        let mut args: smallvec::SmallVec<[BlockArg; 20]> = smallvec::SmallVec::new();
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
    if let Some(ph) = ph_bl {
        cx.b.switch_to_block(ph);
        cx.b.seal_block(ph);
        let zero = cx.b.ins().iconst(types::I8, 0);
        let mut args: smallvec::SmallVec<[BlockArg; 20]> = smallvec::SmallVec::new();
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
    let params: smallvec::SmallVec<[ClifValue; 20]> =
        cx.b.block_params(d).iter().copied().collect();
    let matched = params[0];
    for (i, (id, kind)) in layout.iter().enumerate() {
        bind_local(
            cx,
            pat_bind_name(*id),
            params[1 + 2 * i],
            params[2 + 2 * i],
            *kind,
            Some(*id),
        );
    }
    Ok(matched)
}
