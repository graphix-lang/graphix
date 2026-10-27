//! `select` emission: scrutinee classification, pattern
//! conditions, arm dispatch, and the merge-shape protocol.

use crate::{
    BindId, Node, NodeView, Refs, Rt, Update,
    expr::ExprId,
    fusion::{
        self,
        kernel_abi::{self, AbiKind, PrimType},
    },
    node::{
        op::CmpOp,
        pattern::{PatternNode, SliceKind, StructPatternNode, StructWidth, set_members},
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
use enumflags2::BitFlags;
use netidx_value::{Typ, Value};
use smallvec::SmallVec;

use super::{
    abi::{
        CompiledExpr, LocalKind, OwnedKind, STALE, TAINT, ValueVar, bind_local,
        clean_disc, is_tainted, is_untainted, local_payload_ty, owned_words,
        propagate_flags, propagate_stale, propagate_taint, scalar_disc, value_disc,
    },
    body::{
        BodyCx, emit_into_slot, ensure_owned_composite_src, fold_stale,
        node_composite_source,
    },
    call::{CompositeSource, emit_drop_local},
    flow::emit_scope_drops,
    lower::{freeze_node_typ, resolve_node_typ},
    nodes::{emit_bottom_of_kind, emit_never, emit_owned_value_operand_node},
    record::{KernelConst, NoMatchSite},
    scalar::{
        ElementRead, cast_u64_to_prim, compile_cmp, compile_const, element_read_helper,
        prim_to_clif, scalar_to_payload_i64, struct_get_helper, valarray_get_helper,
        variant_payload_helper,
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
    Ok(cx.call_helper(helper_name, &[ptr, idx_v])?[0])
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
#[derive(Clone)]
enum SelectArmBind {
    /// `n => ...` — bind the scalar scrutinee itself.
    Scrut(BindId),
    /// A read out of a value scrutinee: an option's payload, a union's
    /// member, the scrutinee narrowed to one type (the whole scrutinee
    /// included). A non-scalar is cloned out as an owned local, dropped at
    /// the arm's scope exit; legal under a mask, where a mismatch yields a
    /// drop-safe default.
    Value { id: BindId, kind: LocalKind },
    /// `` `Tag(x) `` — bind payload `idx` of the variant whose borrowed
    /// words are `on`; a wrong-tag read yields 0, or a drop-safe default
    /// behind a tainted disc for a non-scalar, cloned out as an owned local.
    Payload { id: BindId, idx: usize, kind: LocalKind, on: (ClifValue, ClifValue) },
    /// `[<a, b>]` / `[<h, rest..>]` — bind the j-th head of the list
    /// whose words are `list` (the scrutinee, or a borrowed leaf or
    /// payload), cloned out as an owned local of `kind` (legal under a
    /// mask).
    ListHead { id: BindId, idx: usize, kind: LocalKind, list: (ClifValue, ClifValue) },
    /// The rest bind: the k-th TAIL of `list` itself — O(1), shares the
    /// spine.
    ListTail { id: BindId, k: usize, list: (ClifValue, ClifValue) },
    /// `(x, y)` / `{f, ..}` / `[h, ..]` — bind one leaf of type `typ` of a
    /// composite scrutinee (the scrutinee, or a borrowed interior pointer
    /// for a nested pattern); a non-scalar is cloned out as an owned local,
    /// a short or mismatched read yielding a drop-safe default.
    Elem { id: BindId, idx: ElemIdx, typ: Type, kind: LocalKind, parent_ptr: ClifValue },
    /// `[h, rest..]` / `[init.., l]` / `all@ [_, ..]` — bind the elements
    /// `[start, len - back)` of an array scrutinee, cloned out as an
    /// owned composite local.
    Subslice { id: BindId, start: usize, back: usize, parent_ptr: ClifValue },
    /// A binding or-pattern below the arm's top (`(`A(x) | `B(x), n)`):
    /// each alternative's test and binds, the first that matches binding
    /// the names they share.
    Or(Box<[AltMatch]>),
}

impl SelectArmBind {
    /// The names this binds.
    fn ids(&self, f: &mut impl FnMut(BindId)) {
        match self {
            Self::Scrut(id)
            | Self::Value { id, .. }
            | Self::Payload { id, .. }
            | Self::ListHead { id, .. }
            | Self::ListTail { id, .. }
            | Self::Elem { id, .. }
            | Self::Subslice { id, .. } => f(*id),
            // the alternatives bind the same names
            Self::Or(alts) => {
                for b in alts.first().into_iter().flat_map(|(_, b)| b.iter()) {
                    b.ids(f)
                }
            }
        }
    }
}

/// Keep only the binds whose names `reads` reads.
fn retain_read(binds: &mut SmallVec<[SelectArmBind; 8]>, reads: &Refs) {
    binds.retain(|b| match b {
        SelectArmBind::Or(alts) => {
            for (_, b) in alts.iter_mut() {
                retain_read(b, reads)
            }
            alts.first().is_some_and(|(_, b)| !b.is_empty())
        }
        b => {
            let mut read = false;
            b.ids(&mut |id| read |= reads.refed.contains(&id));
            read
        }
    });
}

/// `select` at expression position. Canonical semantics are
/// `Select::update` / `PatternNode::is_match`: the scrutinee is
/// evaluated once and its disc folds into every arm's result; an
/// explicit type predicate is tested; a guard runs after the pattern
/// matches with its binds in scope, and a bottom guard stops the chain;
/// the first matching arm wins.
pub(crate) fn emit_select_node<R: Rt>(
    cx: &mut BodyCx,
    sel: &Select<R>,
) -> Result<CompiledExpr> {
    if sel.arms.is_empty() {
        return Err(anyhow!("emit_clif: select with no arms"));
    }
    let result_typ =
        kernel_abi::freeze_for_abi_normalized(sel.typ()).ok_or_else(|| {
            anyhow!("emit_clif: select result type {} doesn't freeze concrete", sel.typ())
        })?;
    let merge_shape = match kernel_abi::abi_kind(&result_typ) {
        Some(AbiKind::Scalar(p)) => SelectMerge::Scalar(p),
        // a null merges as the Value it is
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value | AbiKind::Null) => {
            SelectMerge::Value
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => SelectMerge::Composite,
        Some(AbiKind::String) => SelectMerge::String,
        other @ (Some(AbiKind::Unit) | None) => {
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
        &mut |cx, body, mark, drops_from, guards_stale| {
            emit_select_value_arm(
                cx,
                body,
                mark,
                drops_from,
                merge_shape,
                merge,
                scrut_disc,
                guards_stale,
            )
        },
        // A standing bottom scrutinee does not re-fire the select.
        &mut |cx| {
            let s = cx.b.ins().band_imm_u(scrut_disc, STALE);
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
pub(super) fn classify_select_scrutinee<R: Rt>(
    cx: &mut BodyCx,
    sel: &Select<R>,
) -> Result<(SelectScrut, Type, Option<ScrutDrop>)> {
    let typ = sel.arg.node.typ();
    let scrut_typ = kernel_abi::freeze_for_abi_normalized(typ).ok_or_else(|| {
        anyhow!("emit_clif: select scrutinee type {typ} doesn't freeze concrete")
    })?;
    classify_scrutinee(cx, &sel.arg.node, scrut_typ, sel.spec.id)
}

/// A destructuring `let pat = value`: its pattern's leaves bound over
/// `value` as a select arm's are, into the block's scope. An owned value
/// stays a block local, dropped with the block's others. The pattern is
/// irrefutable, so its test fails only over a bottom value, whose flags
/// the binds already carry: they install unmasked.
pub(super) fn emit_let_binds<R: Rt>(
    cx: &mut BodyCx,
    pattern: &StructPatternNode,
    typ: &Type,
    value: &Node<R>,
    id: ExprId,
) -> Result<()> {
    let typ = freeze_node_typ(cx.ctx, typ)
        .ok_or_else(|| anyhow!("emit_clif: let pattern type {typ} doesn't freeze"))?;
    let (scrut, scrut_typ, _kept) = classify_scrutinee(cx, value, typ, id)?;
    let mut binds: SmallVec<[SelectArmBind; 8]> = SmallVec::new();
    emit_structure_cond(cx, pattern, &scrut_typ, false, scrut, &scrut_typ, &mut binds)?;
    install_arm_binds(cx, &binds, scrut, None)
}

/// An irrefutable composite pattern's names bound over the composite
/// `(disc, ptr)` of the frozen type `typ`, as a `let` destructure binds
/// them: a loop's element or accumulator.
pub(super) fn emit_composite_binds(
    cx: &mut BodyCx,
    pattern: &StructPatternNode,
    typ: &Type,
    disc: ClifValue,
    ptr: ClifValue,
) -> Result<()> {
    let scrut = SelectScrut::Composite { disc, ptr };
    let mut binds: SmallVec<[SelectArmBind; 8]> = SmallVec::new();
    emit_structure_cond(cx, pattern, typ, false, scrut, typ, &mut binds)?;
    install_arm_binds(cx, &binds, scrut, None)
}

/// [`classify_select_scrutinee`] for the value `node`, of the frozen
/// type `scrut_typ`, of the expression `id`.
fn classify_scrutinee<R: Rt>(
    cx: &mut BodyCx,
    node: &Node<R>,
    scrut_typ: Type,
    id: ExprId,
) -> Result<(SelectScrut, Type, Option<ScrutDrop>)> {
    let scrut_kind = kernel_abi::abi_kind(&scrut_typ)
        .ok_or_else(|| anyhow!("emit_clif: select scrutinee shape not classifiable"))?;
    let owned = node_composite_source(node) == CompositeSource::Owned;
    let mut drop_ob: Option<ScrutDrop> = None;
    let mut adopt = |cx: &mut BodyCx,
                     kind: LocalKind,
                     disc: ClifValue,
                     payload: ClifValue,
                     owned: bool| {
        if owned {
            let mark = cx.env.mark();
            let name: ArcStr =
                compact_str::format_compact!("__scrut{}", id.inner()).as_str().into();
            let vv = bind_local(cx, name, disc, payload, kind, None);
            drop_ob = Some(ScrutDrop { kind, vv, mark });
        }
    };
    let scrut = match scrut_kind {
        AbiKind::Scalar(p) => {
            let cv = node.emit_clif(cx)?;
            SelectScrut::Scalar { disc: cv.disc, value: cv.payload, prim: p }
        }
        AbiKind::Variant | AbiKind::Nullable | AbiKind::Value => {
            let cv = node.emit_clif(cx)?;
            adopt(cx, LocalKind::Value, cv.disc, cv.payload, owned);
            let kind = match scrut_kind {
                AbiKind::Variant => ValueKind::Variant,
                AbiKind::Nullable => ValueKind::Nullable,
                _ => ValueKind::Value,
            };
            SelectScrut::Value { kind, disc: cv.disc, payload: cv.payload }
        }
        // a value-shaped production (a union of composites, a let's
        // value read at the let's type) reads as the composite
        AbiKind::Array | AbiKind::Tuple | AbiKind::Struct => {
            let s = emit_into_slot(cx, node, LocalKind::Composite)?;
            let owned = s.source == CompositeSource::Owned;
            adopt(cx, LocalKind::Composite, s.cv.disc, s.cv.payload, owned);
            SelectScrut::Composite { disc: s.cv.disc, ptr: s.cv.payload }
        }
        // A string is a value whose read is always owned: the select
        // keeps it until the merge.
        AbiKind::String => {
            let cv = node.emit_clif(cx)?;
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
/// SlicePrefix/SliceSuffix `len >= N`, Struct its width; suffix leaves index from
/// the end, struct leaves read `a[i][1]`.
///
/// Element reads are total, so the condition is the AND of the length
/// test and every leaf's own test ([`emit_leaf_cond`]), in one block. An
/// array, tuple or struct `@` or rest bind lowers as a subslice.
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
    let call = cx.call_helper("graphix_valarray_len", &[ptr])?;
    let len = call[0];
    let styp = resolve_node_typ(cx.ctx, scrut_typ);
    struct LeafSpec<'p> {
        idx: ElemIdx,
        sub: &'p StructPatternNode,
        typ: Type,
    }
    let (leaves, len_cc, n): (SmallVec<[LeafSpec; 8]>, IntCC, usize) = match pat {
        StructPatternNode::Slice {
            kind: kind @ (SliceKind::Tuple | SliceKind::Array),
            all,
            binds: pbinds,
        } => {
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
                             scrutinee {t}"
                        )),
                    }
                } else {
                    match &styp {
                        Type::Array(t) => Ok((**t).clone()),
                        t => Err(anyhow!(
                            "emit_clif: slice pattern over non-array \
                             scrutinee {t}"
                        )),
                    }
                }
            };
            // a tuple's elements start at its base in its array
            let base = match kind {
                SliceKind::Tuple => crate::rep::TUPLE_BASE,
                _ => 0,
            };
            let leaves = pbinds
                .iter()
                .enumerate()
                .map(|(j, sub)| {
                    Ok(LeafSpec { idx: ElemIdx::FromStart(base + j), sub, typ: elt(j)? })
                })
                .collect::<Result<SmallVec<[_; 8]>>>()?;
            (leaves, IntCC::Equal, base + pbinds.len())
        }
        StructPatternNode::SlicePrefix { list: false, all, prefix, tail } => {
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
                        "emit_clif: slice pattern over non-array scrutinee {t}"
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
                        "emit_clif: slice pattern over non-array scrutinee {t}"
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
        StructPatternNode::Struct { all, width, binds: sbinds } => {
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
                        "emit_clif: struct pattern over non-struct scrutinee {t}"
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
            // a struct's fields start at its base in its array
            match width {
                StructWidth::Exact(n) => {
                    (leaves, IntCC::Equal, crate::rep::STRUCT_BASE + n)
                }
                StructWidth::AtLeast(n) => {
                    (leaves, IntCC::SignedGreaterThanOrEqual, crate::rep::STRUCT_BASE + n)
                }
            }
        }
        _ => return Err(anyhow!("emit_clif: not a composite structural pattern")),
    };
    let n_c = cx.b.ins().iconst(types::I64, n as i64);
    let mut cond = cx.b.ins().icmp(len_cc, len, n_c);
    for leaf in &leaves {
        let at = LeafAt::Elem { ptr, idx: leaf.idx };
        if let Some(c) = emit_leaf_cond(cx, at, leaf.sub, &leaf.typ, binds)? {
            cond = cx.b.ins().band(cond, c);
        }
    }
    Ok(cond)
}

/// Where a sub-pattern's value lives: every read of it is total.
#[derive(Clone, Copy)]
enum LeafAt {
    /// An element or field of the composite `ptr`.
    Elem { ptr: ClifValue, idx: ElemIdx },
    /// Payload `idx` of the variant whose borrowed words are `v`.
    Payload { v: (ClifValue, ClifValue), idx: usize },
    /// Head `idx` of the list whose words are `list`.
    ListHead { list: (ClifValue, ClifValue), idx: usize },
}

impl LeafAt {
    /// The bind of the value here, of type `typ`, as a local of `kind`.
    fn bind(self, id: BindId, typ: &Type, kind: LocalKind) -> SelectArmBind {
        match self {
            LeafAt::Elem { ptr, idx } => {
                SelectArmBind::Elem { id, idx, typ: typ.clone(), kind, parent_ptr: ptr }
            }
            LeafAt::Payload { v, idx } => SelectArmBind::Payload { id, idx, kind, on: v },
            LeafAt::ListHead { list, idx } => {
                SelectArmBind::ListHead { id, idx, kind, list }
            }
        }
    }

    /// The value's words, borrowed; null where there is none.
    fn words(self, cx: &mut BodyCx) -> Result<(ClifValue, ClifValue)> {
        let (helper, (d, p), idx) = match self {
            LeafAt::Elem { ptr, idx } => return value_leaf_words(cx, ptr, idx),
            LeafAt::Payload { v, idx } => ("graphix_variant_payload_borrowed", v, idx),
            LeafAt::ListHead { list, idx } => ("graphix_list_head_borrowed", list, idx),
        };
        let idx = cx.b.ins().iconst(types::I64, idx as i64);
        let call = cx.call_helper(helper, &[d, p, idx])?;
        Ok((call[0], call[1]))
    }

    /// The value of the composite type `typ` as a borrowed array, the empty
    /// array where it is none, and the test that it is of `typ` where its
    /// type does not say so. A payload's type is its arm's predicate's,
    /// which another case of the same tag and arity may share.
    fn array(
        self,
        cx: &mut BodyCx,
        typ: &Type,
    ) -> Result<(ClifValue, Option<ClifValue>)> {
        if let LeafAt::Elem { ptr, idx } = self {
            let (read, idx_v) = elem_index(cx, idx);
            let helper = match read {
                ElementRead::ArrayIndex => "graphix_valarray_get_array_borrowed",
                ElementRead::StructField => "graphix_struct_get_array_borrowed",
            };
            return Ok((cx.call_helper(helper, &[ptr, idx_v])?[0], None));
        }
        let (disc, payload) = self.words(cx)?;
        let test = match self {
            LeafAt::Payload { .. } => {
                let t = cx.interned_type(typ)?;
                let call =
                    cx.call_helper("graphix_inferred_type_matches", &[disc, payload, t])?;
                cx.b.ins().icmp_imm_s(IntCC::NotEqual, call[0], 0)
            }
            _ => {
                let cd = clean_disc(cx.b, disc);
                cx.b.ins().icmp_imm_s(IntCC::Equal, cd, value_disc::ARRAY)
            }
        };
        let empty = cx.call_helper("graphix_valarray_empty_borrowed", &[])?[0];
        let ptr = cx.b.ins().select(test, payload, empty);
        Ok((ptr, matches!(self, LeafAt::Payload { .. }).then_some(test)))
    }
}

/// The condition of the sub-pattern `sub` over the value of type `typ`
/// at `at`, pushing its binds onto `binds`; `None` when every value
/// matches. A tuple, array or struct pattern reads the value as an array.
fn emit_leaf_cond(
    cx: &mut BodyCx,
    at: LeafAt,
    sub: &StructPatternNode,
    typ: &Type,
    binds: &mut SmallVec<[SelectArmBind; 8]>,
) -> Result<Option<ClifValue>> {
    stack::ensure_sufficient(|| emit_leaf_cond_inner(cx, at, sub, typ, binds))
}

fn emit_leaf_cond_inner(
    cx: &mut BodyCx,
    at: LeafAt,
    sub: &StructPatternNode,
    typ: &Type,
    binds: &mut SmallVec<[SelectArmBind; 8]>,
) -> Result<Option<ClifValue>> {
    Ok(match sub {
        StructPatternNode::Abstract { .. } => {
            bail!("emit_clif: abstract pattern leaf not lowerable")
        }
        StructPatternNode::Ignore => None,
        StructPatternNode::Bind(id) => {
            let kind = payload_local_kind(typ).ok_or_else(|| {
                anyhow!("emit_clif: select pattern leaf bind {typ:?} not lowerable")
            })?;
            binds.push(at.bind(*id, typ, kind));
            None
        }
        // The typed element read is total: a slot not of `prim`'s family
        // reads as 0, which a `0` literal would match. Only the static
        // type proves the read faithful; any other literal tests the
        // value.
        StructPatternNode::Literal(v)
            if let LeafAt::Elem { ptr, idx } = at
                && let Some(prim) = kernel_abi::scalar_prim_of_value(v)
                && kernel_abi::scalar_prim(typ) == Some(prim) =>
        {
            let elem = read_scrut_elem(cx, ptr, idx, prim)?;
            let lit = compile_const(cx.b, v, prim)?;
            Some(compile_cmp(cx.b, CmpOp::Eq, prim, elem, lit))
        }
        StructPatternNode::Literal(_) => {
            let words = at.words(cx)?;
            Some(emit_value_leaf_cond(cx, sub, typ, words)?)
        }
        StructPatternNode::Or { .. } if !binds_any(sub) => {
            let words = at.words(cx)?;
            Some(emit_value_leaf_cond(cx, sub, typ, words)?)
        }
        StructPatternNode::Or { alts } => {
            let alts = alts
                .iter()
                .map(|alt| {
                    let mut binds: SmallVec<[SelectArmBind; 8]> = SmallVec::new();
                    let c = emit_leaf_cond(cx, at, alt, typ, &mut binds)?;
                    Ok((c, binds))
                })
                .collect::<Result<Box<[AltMatch]>>>()?;
            let any = or_of_alts(cx, &alts);
            binds.push(SelectArmBind::Or(alts));
            any
        }
        StructPatternNode::Variant { tag, all, binds: payloads } => {
            if let Some(id) = all {
                binds.push(at.bind(*id, typ, LocalKind::Value));
            }
            let words = at.words(cx)?;
            Some(emit_variant_cond(cx, words, tag, payloads, typ, binds)?)
        }
        StructPatternNode::Slice { kind: SliceKind::List, .. }
        | StructPatternNode::SlicePrefix { list: true, .. } => {
            let words = at.words(cx)?;
            let whole = |id| at.bind(id, typ, LocalKind::Value);
            Some(emit_list_pattern_cond(cx, sub, words, typ, whole, binds)?)
        }
        StructPatternNode::Slice { .. }
        | StructPatternNode::SlicePrefix { .. }
        | StructPatternNode::SliceSuffix { .. }
        | StructPatternNode::Struct { .. } => match kernel_abi::abi_kind(typ) {
            Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                let (ptr, test) = at.array(cx, typ)?;
                let c = emit_composite_pattern_cond(cx, ptr, typ, sub, binds)?;
                Some(match test {
                    Some(t) => cx.b.ins().band(t, c),
                    None => c,
                })
            }
            other => bail!(
                "emit_clif: nested pattern over a leaf of shape {other:?} not lowerable"
            ),
        },
    })
}

/// The borrowed Value words of the leaf at `idx` of the composite `ptr`.
fn value_leaf_words(
    cx: &mut BodyCx,
    ptr: ClifValue,
    idx: ElemIdx,
) -> Result<(ClifValue, ClifValue)> {
    let (read, idx_v) = elem_index(cx, idx);
    let helper = match read {
        ElementRead::ArrayIndex => "graphix_valarray_get_value_borrowed",
        ElementRead::StructField => "graphix_struct_get_value_borrowed",
    };
    let call = cx.call_helper(helper, &[ptr, idx_v])?;
    Ok((call[0], call[1]))
}

/// Whether a pattern binds a name.
fn binds_any(p: &StructPatternNode) -> bool {
    let mut any = false;
    p.ids(&mut |_| any = true);
    any
}

/// The test of a pattern that binds nothing (a literal of any type, an
/// or-pattern of such) against borrowed Value words `v` of type `typ`:
/// an element, a payload. The arm's own scrutinee tests run the same.
fn emit_value_leaf_cond(
    cx: &mut BodyCx,
    sub: &StructPatternNode,
    typ: &Type,
    v: (ClifValue, ClifValue),
) -> Result<ClifValue> {
    let kind = match kernel_abi::abi_kind(typ) {
        Some(AbiKind::Variant) => ValueKind::Variant,
        Some(AbiKind::Nullable) => ValueKind::Nullable,
        Some(AbiKind::String) => ValueKind::String,
        _ => ValueKind::Value,
    };
    let scrut = SelectScrut::Value { kind, disc: v.0, payload: v.1 };
    // every alternative tests the whole type; the reads are total, so
    // their tests OR in one block
    let alts = match sub {
        StructPatternNode::Or { alts } => &alts[..],
        sub => std::slice::from_ref(sub),
    };
    let mut binds: SmallVec<[SelectArmBind; 8]> = SmallVec::new();
    let mut cond: Option<ClifValue> = None;
    for alt in alts {
        let c = match emit_structure_cond(cx, alt, typ, false, scrut, typ, &mut binds)? {
            Some(c) => c,
            None => cx.b.ins().iconst(types::I8, 1),
        };
        cond = Some(match cond {
            None => c,
            Some(acc) => cx.b.ins().bor(acc, c),
        });
    }
    if !binds.is_empty() {
        bail!("emit_clif: a value leaf pattern binds")
    }
    cond.ok_or_else(|| anyhow!("emit_clif: an empty or-pattern"))
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

/// What the guard prologue emitted of an arm, for the chain to reuse.
enum Tested {
    /// A guarded arm: its condition; the binds it reads are installed.
    Guarded(Option<ClifValue>),
    /// An unguarded non-or arm: its condition and its bind specs.
    Unguarded(Option<ClifValue>, SmallVec<[SelectArmBind; 8]>),
    /// An unguarded or-arm: each alternative's condition and bind specs.
    Or(SmallVec<[AltMatch; 4]>),
}

/// Arm `pat`'s pattern condition against `scrut`, pushing its binds
/// onto `binds`; `None` when the arm matches unconditionally. An
/// or-pattern installs its shared binds itself, in the chain's done
/// block ([`emit_or_chain`]): a miss goes to `nomatch`, else binds
/// standing placeholders; `pre` holds every alternative's test when a
/// block dominating this one emitted them ([`emit_alt_matches`]).
fn emit_arm_condition<R: Rt>(
    cx: &mut BodyCx,
    pat: &PatternNode<R>,
    scrut: SelectScrut,
    scrut_typ: &Type,
    nomatch: Option<Block>,
    mask: Option<ClifValue>,
    pre: Option<SmallVec<[AltMatch; 4]>>,
    binds: &mut SmallVec<[SelectArmBind; 8]>,
) -> Result<Option<ClifValue>> {
    if let StructPatternNode::Or { alts } = &pat.structure_predicate {
        if pat.explicit_type_predicate {
            bail!(
                "emit_clif: explicit type predicate on an or-pattern arm not lowerable"
            );
        }
        let mut pre = pre;
        if pre.as_ref().is_some_and(|m| m.len() != alts.len()) {
            bail!("emit_clif: an or-chain's tests are not its alternatives'")
        }
        let pred = &pat.type_predicate;
        let alt_types = set_members(pred, alts.len());
        let nomatch = match nomatch {
            Some(b) => NoMatch::Fail(b),
            None => NoMatch::Placeholders(cx.b.ins().iconst(types::I64, STALE)),
        };
        let mut alt = |cx: &mut BodyCx, k: usize| match pre.as_mut() {
            Some(m) => Ok(std::mem::take(&mut m[k])),
            None => {
                emit_alt_match(cx, alts, k, alt_types.as_deref(), pred, scrut, scrut_typ)
            }
        };
        let m = emit_or_chain(cx, alts.len(), &mut alt, scrut, nomatch, mask)?;
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
/// and must leave it terminated; it gets the env mark to truncate to,
/// the mark every local above which it drops (the guard prologue's binds
/// live until the select ends, so every exit drops them), and the AND of
/// the consulted guards' STALE bits at its point
/// (structure-failed arms and arms below the taken one contribute
/// nothing). `emit_miss` handles a tainted scrutinee, which makes no
/// selection and consults no guard; `emit_undet` the undecidable
/// outcome (a bottom consulted guard), given its STALE bits (0 = fresh).
pub(super) fn emit_select_arms<R: Rt>(
    cx: &mut BodyCx,
    sel: &Select<R>,
    scrut: SelectScrut,
    scrut_typ: &Type,
    emit_arm: &mut dyn FnMut(
        &mut BodyCx,
        &Node<R>,
        usize,
        usize,
        ClifValue,
    ) -> Result<()>,
    emit_miss: &mut dyn FnMut(&mut BodyCx) -> Result<()>,
    emit_undet: &mut dyn FnMut(&mut BodyCx, ClifValue) -> Result<()>,
) -> Result<()> {
    let sdisc = scrut.disc();
    let miss_bl = cx.b.create_block();
    let n = sel.arms.len();
    // The guard prologue: the node-walk ticks every arm's guard every
    // cycle before matching, so every guard down to the last that is not
    // pure in its binds is evaluated here once per invocation and the
    // chain reads it; the consulted folds happen along the chain into
    // `acc`. A guard sees its binds only where the chain reaches its arm
    // (`open`: no earlier arm claimed the value, no guard was bottom).
    let mut guard_vals: SmallVec<[Option<GuardPlanes>; 8]> = smallvec::smallvec![None; n];
    // the prologue's binds stay installed, masked by their arm's
    // condition and reach, for the arm's body to read; every exit drops
    // them
    let smark = cx.env.mark();
    // what the prologue, which dominates the chain, emitted of each arm
    let mut tested: SmallVec<[Option<Tested>; 8]> = (0..n).map(|_| None).collect();
    let impure = sel.arms.iter().rposition(|(pat, _)| {
        pat.guard.as_ref().is_some_and(|g| !guard_is_pure_of_binds(pat, &g.node))
    });
    let open = cx.b.declare_var(types::I8);
    let reaches_first = cx.b.ins().iconst(types::I8, 1);
    cx.b.def_var(open, reaches_first);
    for (i, (pat, body)) in sel.arms.iter().enumerate().take(impure.map_or(0, |l| l + 1))
    {
        let reached = cx.b.use_var(open);
        let Some(g) = &pat.guard else {
            // an unguarded arm claims by its condition; its binds are the
            // chain's
            let (cond, t) = match &pat.structure_predicate {
                StructPatternNode::Or { alts } => {
                    let m = emit_alt_matches(
                        cx,
                        alts,
                        &pat.type_predicate,
                        scrut,
                        scrut_typ,
                    )?;
                    (or_of_alts(cx, &m), Tested::Or(m))
                }
                _ => {
                    let mut binds: SmallVec<[SelectArmBind; 8]> = SmallVec::new();
                    let cond = emit_arm_condition(
                        cx, pat, scrut, scrut_typ, None, None, None, &mut binds,
                    )?;
                    (cond, Tested::Unguarded(cond, binds))
                }
            };
            tested[i] = Some(t);
            let still = match cond {
                Some(c) => cx.b.ins().band_not(reached, c),
                None => cx.b.ins().iconst(types::I8, 0),
            };
            cx.b.def_var(open, still);
            continue;
        };
        let mut binds: SmallVec<[SelectArmBind; 8]> = SmallVec::new();
        let pcond = emit_arm_condition(
            cx,
            pat,
            scrut,
            scrut_typ,
            None,
            Some(reached),
            None,
            &mut binds,
        )?;
        let mask = match pcond {
            Some(c) => cx.b.ins().band(c, reached),
            None => reached,
        };
        let mut reads = Refs::default();
        body.refs(&mut reads);
        g.node.refs(&mut reads);
        retain_read(&mut binds, &reads);
        install_arm_binds(cx, &binds, scrut, Some(mask))?;
        // CR claude for claude: [bug/medium] The prologue runs this guard on every
        // invocation, also where an earlier arm claimed the value. A `$` or
        // handler-less `?` here whose operand ignores the binds then logs a swallowed
        // failure the node-walk never had, because the node-walk runs only the guards
        // the chain reaches (CLAUDE.md). The comment above the prologue says the
        // node-walk ticks every guard every cycle, which is no longer the rule. This is
        // the reverse of f-call-flow-02. Decide: evaluate a prologue guard only under
        // its `mask` (branch around it, pass eff/gbot/gs as block params), or have
        // lowering::unfollowed_guard refuse such Qops as it refuses handler-ful raises.
        // probe: design/review-2026-10-26/repro/f-select-unreached-guard-logs.sh
        // (f-select-02)
        // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q63,
        // Swallowed failures in fused code (round-1 q1, follow-up)): yes, keep fusion
        // (the recommendation: keep fusion: predictable fusion matters more than the
        // duplicate log.)
        let gcv = g.node.emit_clif(cx)?;
        let gs = cx.b.ins().band_imm_u(gcv.disc, STALE);
        let gbot = is_tainted(cx.b, gcv.disc);
        let valid = is_untainted(cx.b, gcv.disc);
        let eff = cx.b.ins().band(gcv.payload, valid);
        guard_vals[i] = Some(GuardPlanes { eff, gbot, gs });
        // the chain stops at this arm when it takes it or its guard is bottom
        let stops = cx.b.ins().bor(eff, gbot);
        let claims = cx.b.ins().band(mask, stops);
        let still = cx.b.ins().band_not(reached, claims);
        cx.b.def_var(open, still);
        tested[i] = Some(Tested::Guarded(pcond));
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
        let pcond = match tested[i].take() {
            Some(Tested::Guarded(pcond)) => pcond,
            Some(Tested::Unguarded(pcond, b)) => {
                binds = b;
                pcond
            }
            Some(Tested::Or(m)) => emit_arm_condition(
                cx,
                pat,
                scrut,
                scrut_typ,
                or_fail,
                None,
                Some(m),
                &mut binds,
            )?,
            None => emit_arm_condition(
                cx, pat, scrut, scrut_typ, or_fail, None, None, &mut binds,
            )?,
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
        let mut reads = Refs::default();
        body.refs(&mut reads);
        if let Some(g) = &pat.guard
            && guard_vals[i].is_none()
        {
            g.node.refs(&mut reads);
        }
        retain_read(&mut binds, &reads);
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
                    let gs = cx.b.ins().band_imm_u(gcv.disc, STALE);
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
        emit_arm(cx, body, mark, smark, guards_stale)?;
        match fail {
            Some(f) => {
                cx.b.switch_to_block(f);
                cx.b.seal_block(f);
                if is_last {
                    // a valid scrutinee matches an exhaustive chain: this is
                    // a hole in the check, said as the node-walk says it
                    let site = cx.const_ptr(KernelConst::NoMatchSite(Box::new(
                        NoMatchSite::new(sel.spec.clone()),
                    )))?;
                    cx.call_helper("graphix_select_no_match", &[site])?;
                    cx.b.ins().jump(miss_bl, &[]);
                }
            }
            // An unconditional arm consumed control flow.
            None => break,
        }
    }
    // past the chain the prologue's binds are out of scope, dropped here
    // on the two exits no arm took
    let prologue: SmallVec<[(LocalKind, ValueVar); 8]> =
        cx.env.locals_above(smark).collect();
    cx.env.truncate(smark);
    let drop_prologue = |cx: &mut BodyCx| -> Result<()> {
        for (kind, vv) in prologue.iter() {
            emit_drop_local(cx.b, cx.ctx, *kind, *vv)?;
        }
        Ok(())
    };
    cx.b.switch_to_block(miss_bl);
    cx.b.seal_block(miss_bl);
    drop_prologue(cx)?;
    emit_miss(cx)?;
    if let Some(ub) = undet_bl {
        cx.b.switch_to_block(ub);
        cx.b.seal_block(ub);
        drop_prologue(cx)?;
        // The undecidable outcome is fresh iff a consumed input fired:
        // the scrutinee or a consulted guard.
        let ss = cx.b.ins().band_imm_u(sdisc, STALE);
        let af = cx.b.use_var(acc);
        let undet_stale = cx.b.ins().band(ss, af);
        emit_undet(cx, undet_stale)?;
    }
    Ok(())
}

/// True when a guard is a pure, never-bottom function of its arm's
/// binds and constants (comparisons, logicals, not, scalar +/-/*/neg;
/// no div, indexing, calls or state). Such a guard is evaluated lazily
/// in the chain instead of in the prologue.
fn guard_is_pure_of_binds<R: Rt>(pat: &PatternNode<R>, guard: &Node<R>) -> bool {
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
        | NodeView::Not(_) => {}
        NodeView::Neg(_) | NodeView::Add(_) | NodeView::Sub(_) | NodeView::Mul(_) => {
            ok &= kernel_abi::scalar_prim(n.typ()).is_some();
        }
        _ => ok = false,
    });
    ok
}

fn emit_inferred_guard_cond(
    cx: &mut BodyCx,
    pred: &Type,
    scrut: SelectScrut,
    scrut_typ: &Type,
) -> Result<Option<ClifValue>> {
    let pred = resolve_node_typ(cx.ctx, pred);
    if pred == *scrut_typ || matches!(pred, Type::Any) {
        return Ok(None);
    }
    let pred = pred.shallow_discriminant(cx.ctx.type_env, scrut_typ).unwrap_or(pred);
    let cond = match &pred {
        Type::Primitive(prims) => {
            let disc = clean_disc(cx.b, scrut.disc());
            let mut cond = cx.b.ins().iconst(types::I8, 0);
            for prim in prims.iter() {
                let matches = cx.b.ins().icmp_imm_s(IntCC::Equal, disc, prim as i64);
                cond = cx.b.ins().bor(cond, matches);
            }
            cond
        }
        Type::Any => return Ok(None),
        _ => {
            let payload = match scrut {
                SelectScrut::Scalar { value, prim, .. } => {
                    scalar_to_payload_i64(cx.b, prim, value)
                }
                SelectScrut::Value { payload, .. } => payload,
                SelectScrut::Composite { ptr, .. } => ptr,
            };
            let typ = cx.interned_type(&pred)?;
            cx.call_helper(
                "graphix_inferred_type_matches",
                &[scrut.disc(), payload, typ],
            )?[0]
        }
    };
    Ok(Some(cond))
}

/// Emit arm `pat`'s pattern condition against `scrut`: the type
/// predicate (`tcond`) and the structure condition (`scond`), pushing
/// the pattern's binds onto `binds`.
fn emit_arm_cond<R: Rt>(
    cx: &mut BodyCx,
    pat: &PatternNode<R>,
    scrut: SelectScrut,
    scrut_typ: &Type,
    binds: &mut SmallVec<[SelectArmBind; 8]>,
) -> Result<(Option<ClifValue>, Option<ClifValue>)> {
    if pat.explicit_type_predicate
        && let Some(m) = emit_member_cond(
            cx,
            &pat.structure_predicate,
            &pat.type_predicate,
            true,
            scrut,
            scrut_typ,
            binds,
        )?
    {
        return Ok((Some(m.0), m.1));
    }
    let tcond: Option<ClifValue> = if !pat.explicit_type_predicate
        || type_test_is_trivial(cx, pat, scrut_typ)
    {
        None
    } else {
        let pred = freeze_node_typ(cx.ctx, &pat.type_predicate).ok_or_else(|| {
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
                    let td = cx.b.ins().iconst(types::I64, t as i64);
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
                Some(emit_null_cond(cx, scrut, scrut_typ)?)
            }
            // Over any other union, the value's own test against the
            // predicate, as the node-walk tests a written one.
            _ if matches!(scrut_typ, Type::Set(_))
                && matches!(scrut, SelectScrut::Value { kind: ValueKind::Value, .. }) =>
            {
                let SelectScrut::Value { disc, payload, .. } = scrut else {
                    unreachable!()
                };
                let typ = cx.interned_type(&pat.type_predicate.resolve_tvars())?;
                let call = cx.call_helper("graphix_value_is_a", &[typ, disc, payload])?;
                Some(cx.b.ins().icmp_imm_s(IntCC::NotEqual, call[0], 0))
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
                            (Some(false), true, _) => Some(cx.b.ins().icmp_imm_s(
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
                                     {pred} over {scrut_typ} not lowerable"
                                ));
                            }
                            (None, ..) => {
                                return Err(anyhow!(
                                    "emit_clif: Nullable scrutinee \
                                     {scrut_typ} has no marker shape"
                                ));
                            }
                        }
                    }
                    _ => {
                        return Err(anyhow!(
                            "emit_clif: type predicate {pred} over \
                                 scrutinee {scrut_typ} not lowerable"
                        ));
                    }
                }
            }
            _ => {
                return Err(anyhow!("emit_clif: type predicate {pred} not lowerable"));
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

/// Whether every value of the scrutinee's type passes an arm's written
/// type test: the test holds the type, which holds no abstract type
/// (whose test is nominal).
fn type_test_is_trivial<R: Rt>(
    cx: &BodyCx,
    pat: &PatternNode<R>,
    scrut_typ: &Type,
) -> bool {
    let env = cx.ctx.type_env;
    !scrut_typ.holds_abstract(env)
        && resolve_node_typ(cx.ctx, &pat.type_predicate)
            .contains_with_flags(BitFlags::empty(), env, scrut_typ)
            .unwrap_or(false)
}

fn emit_null_cond(cx: &mut BodyCx, scrut: SelectScrut, typ: &Type) -> Result<ClifValue> {
    match scrut {
        SelectScrut::Value { kind: ValueKind::Nullable, .. }
            if kernel_abi::nullable_error_marked(typ) != Some(false) =>
        {
            bail!("emit_clif: null predicate over a result union {typ}")
        }
        SelectScrut::Value {
            kind: ValueKind::Nullable | ValueKind::Value, disc, ..
        } => {
            let disc = clean_disc(cx.b, disc);
            Ok(cx.b.ins().icmp_imm_s(IntCC::Equal, disc, value_disc::NULL))
        }
        _ => bail!("emit_clif: null predicate over {typ}"),
    }
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

/// The list-pattern condition + binds over the list whose words are
/// `list`, of type `list_typ`: one `graphix_list_match` spine walk (`k`
/// cells, `exact` requires nil after); head binds clone out by ABI kind;
/// the rest bind is the k-th tail, shared; `whole` is the `@` bind of
/// the list where it was read. Each head's own pattern is a leaf.
fn emit_list_pattern_cond(
    cx: &mut BodyCx,
    pat: &StructPatternNode,
    list: (ClifValue, ClifValue),
    list_typ: &Type,
    whole: impl FnOnce(BindId) -> SelectArmBind,
    binds: &mut SmallVec<[SelectArmBind; 8]>,
) -> Result<ClifValue> {
    let et = list_typ
        .with_deref(|t| match t {
            Some(Type::List(et)) => Some((**et).clone()),
            _ => None,
        })
        .ok_or_else(|| anyhow!("emit_clif: list pattern over non-list {list_typ}"))?;
    let (all, elems, exact, tail) = match pat {
        StructPatternNode::Slice { kind: SliceKind::List, all, binds: pb } => {
            (all, pb, true, None)
        }
        StructPatternNode::SlicePrefix { list: true, all, prefix, tail } => {
            (all, prefix, false, tail.as_ref())
        }
        _ => bail!("emit_clif: a list pattern condition of a non-list pattern"),
    };
    if let Some(id) = all {
        binds.push(whole(*id));
    }
    let k = elems.len();
    let kc = cx.b.ins().iconst(types::I64, k as i64);
    let ex = cx.b.ins().iconst(types::I8, exact as i64);
    let mut cond = cx.call_helper("graphix_list_match", &[list.0, list.1, kc, ex])?[0];
    for (idx, sub) in elems.iter().enumerate() {
        if let Some(c) =
            emit_leaf_cond(cx, LeafAt::ListHead { list, idx }, sub, &et, binds)?
        {
            cond = cx.b.ins().band(cond, c);
        }
    }
    if let Some(id) = tail {
        binds.push(SelectArmBind::ListTail { id: *id, k, list });
    }
    Ok(cond)
}

/// The [`LocalKind`] a non-scalar bind takes (a variant payload, a
/// leaf, a list head, a nullable's payload), by its ABI kind.
/// `Unit`/`Null` (and shapes with no kernel encoding) refuse.
fn payload_local_kind(t: &Type) -> Option<LocalKind> {
    kernel_abi::abi_kind(t).and_then(LocalKind::of)
}

/// [`payload_local_kind`] of a non-scalar.
fn owned_kind(t: &Type) -> Option<OwnedKind> {
    payload_local_kind(t).and_then(|l| l.owned().ok())
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
    Ok(owned_words(cx.b, kind.into(), &call))
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
                anyhow!("emit_clif: a variant or nullable bind without a value scrutinee")
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
            SelectArmBind::Value { id, kind: LocalKind::Scalar(prim) } => {
                let value = cast_u64_to_prim(cx.b, value_payload()?, *prim);
                (*id, scalar_disc(cx.b, *prim), value, LocalKind::Scalar(*prim))
            }
            SelectArmBind::Value { id, kind } => {
                let owned = kind.owned()?;
                let helper = owned_read_helper(ReadFrom::Value, owned);
                let call = cx.call_helper(helper, &[sdisc, value_payload()?])?;
                let (d, p) = owned_words(cx.b, *kind, &call);
                (*id, d, p, *kind)
            }
            SelectArmBind::Payload { id, idx, kind, on: (vd, vp) } => {
                let (vd, vp) = (*vd, *vp);
                let idx_c = cx.b.ins().iconst(types::I64, *idx as i64);
                match kind {
                    LocalKind::Scalar(prim) => {
                        let call = cx.call_helper(
                            variant_payload_helper(*prim),
                            &[vd, vp, idx_c],
                        )?;
                        (*id, scalar_disc(cx.b, *prim), call[0], *kind)
                    }
                    kind => {
                        let helper = owned_read_helper(ReadFrom::Payload, kind.owned()?);
                        let call = cx.call_helper(helper, &[vd, vp, idx_c])?;
                        let (d, p) = owned_words(cx.b, *kind, &call);
                        (*id, d, p, *kind)
                    }
                }
            }
            SelectArmBind::ListHead { id, idx, kind, list } => {
                let j = cx.b.ins().iconst(types::I64, *idx as i64);
                let args = [list.0, list.1, j];
                let (d, p) = match *kind {
                    LocalKind::Scalar(p) => {
                        let call = cx.call_helper("graphix_list_get_value", &args)?;
                        let raw = call[1];
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
            SelectArmBind::ListTail { id, k, list } => {
                let kc = cx.b.ins().iconst(types::I64, *k as i64);
                let call = cx.call_helper("graphix_list_tail", &[list.0, list.1, kc])?;
                (*id, call[0], call[1], LocalKind::Value)
            }
            SelectArmBind::Elem { id, idx, typ, kind, parent_ptr } => match kind {
                LocalKind::Scalar(prim) => {
                    let v = read_scrut_elem(cx, *parent_ptr, *idx, *prim)?;
                    (*id, scalar_disc(cx.b, *prim), v, *kind)
                }
                kind => {
                    let (read, idx_v) = elem_index(cx, *idx);
                    let call = cx.call_helper(
                        element_read_helper(typ, read)?,
                        &[*parent_ptr, idx_v],
                    )?;
                    let (d, p) = owned_words(cx.b, *kind, &call);
                    (*id, d, p, *kind)
                }
            },
            SelectArmBind::Subslice { id, start, back, parent_ptr } => {
                let start = cx.b.ins().iconst(types::I64, *start as i64);
                let back = cx.b.ins().iconst(types::I64, *back as i64);
                let call = cx.call_helper(
                    "graphix_valarray_subslice",
                    &[*parent_ptr, start, back],
                )?;
                let bits = call[0];
                (
                    *id,
                    cx.b.ins().iconst(types::I64, value_disc::ARRAY),
                    bits,
                    LocalKind::Composite,
                )
            }
            SelectArmBind::Or(alts) => {
                let stale = cx.b.ins().band_imm_u(sdisc, STALE);
                let nomatch = NoMatch::Placeholders(stale);
                let mut alt = |_: &mut BodyCx, k: usize| Ok(alts[k].clone());
                stack::ensure_sufficient(|| {
                    emit_or_chain(cx, alts.len(), &mut alt, scrut, nomatch, mask)
                })?;
                continue;
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
fn emit_select_value_arm<R: Rt>(
    cx: &mut BodyCx,
    body: &Node<R>,
    mark: usize,
    drops_from: usize,
    merge_shape: SelectMerge,
    merge: Block,
    scrut_disc: ClifValue,
    guards_stale: ClifValue,
) -> Result<()> {
    let body_frozen =
        kernel_abi::freeze_for_abi_normalized(body.typ()).ok_or_else(|| {
            anyhow!("emit_clif: select arm type {} doesn't freeze concrete", body.typ())
        })?;
    // A never arm's fire follows the scrutinee and consulted guards,
    // independent of its arguments' fires. Its arguments still run.
    // Any other bottom-typed body's production contributes its own fire.
    let never_args = match body.view() {
        NodeView::Never(n) => Some(&n.n),
        _ => None,
    };
    let (disc, payload) = match never_args {
        Some(args) => {
            let cv = emit_never(cx, args, merge_shape.abi_kind())?;
            (cv.disc, cv.payload)
        }
        None if matches!(body_frozen, Type::Bottom) => {
            let cv = emit_bottom_of_kind(cx, merge_shape.abi_kind())?;
            let produced = body.emit_clif(cx)?;
            (propagate_flags(cx.b, cv.disc, &[produced.disc]), cv.payload)
        }
        None => emit_select_arm_value(cx, body, &body_frozen, merge_shape)?,
    };
    // TAINT = OR(arm, scrutinee). Firing = OR(arm production, scrutinee
    // delivery, consulted guard productions), as the STALE AND-fold
    // (the interp's `own_fired`).
    let base = clean_disc(cx.b, disc);
    let d = propagate_taint(cx.b, base, &[disc, scrut_disc]);
    let d = propagate_stale(cx.b, d, &[disc]);
    let scrut_stale = cx.b.ins().band_imm_u(scrut_disc, STALE);
    let d = fold_stale(cx.b, d, scrut_stale);
    let d = fold_stale(cx.b, d, guards_stale);
    // Drop the arm's owned pattern binds; the widening above made the
    // result independently owned.
    emit_scope_drops(cx, drops_from)?;
    cx.env.truncate(mark);
    cx.b.ins().jump(merge, &[BlockArg::Value(d), BlockArg::Value(payload)]);
    Ok(())
}

/// An ordinary arm's result widened to the select's merge shape.
fn emit_select_arm_value<R: Rt>(
    cx: &mut BodyCx,
    body: &Node<R>,
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
                    Some(prim) => {
                        SelectArmBind::Value { id: *id, kind: LocalKind::Scalar(prim) }
                    }
                    None => {
                        let kind = match pred.as_ref().and_then(kernel_abi::abi_kind) {
                            Some(AbiKind::String) => OwnedKind::String,
                            _ => OwnedKind::Value,
                        };
                        SelectArmBind::Value { id: *id, kind: kind.into() }
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
                    Some(prim) => {
                        SelectArmBind::Value { id: *id, kind: LocalKind::Scalar(prim) }
                    }
                    None => {
                        let kind = pred.as_ref().and_then(owned_kind).ok_or_else(|| {
                            anyhow!(
                                "emit_clif: nullable scrutinee bind predicate {pred:?} \
                                 not lowerable"
                            )
                        })?;
                        SelectArmBind::Value { id: *id, kind: kind.into() }
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
                binds.push(SelectArmBind::Value { id: *id, kind });
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
                Some(call[0])
            }
            _ => {
                return Err(anyhow!(
                    "emit_clif: a string literal pattern over {scrut_typ}"
                ));
            }
        },
        StructPatternNode::Literal(Value::Null) => {
            Some(emit_null_cond(cx, scrut, scrut_typ)?)
        }
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
                             doesn't match scrutinee {scrut_typ}"
                    ));
                }
            }
        }
        StructPatternNode::Variant { tag, all, binds: pbinds } => {
            if let Some(id) = all {
                binds.push(SelectArmBind::Value { id: *id, kind: LocalKind::Value });
            }
            // the tag test is false of any other value, null included
            let (disc, payload) = match scrut {
                SelectScrut::Value {
                    kind: ValueKind::Variant | ValueKind::Nullable | ValueKind::Value,
                    disc,
                    payload,
                } => (disc, payload),
                _ => {
                    return Err(anyhow!(
                        "emit_clif: variant pattern over non-variant \
                             scrutinee {scrut_typ}"
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
            Some(emit_variant_cond(cx, (disc, payload), tag, pbinds, &pred, binds)?)
        }
        p @ (StructPatternNode::Slice { kind: SliceKind::List, .. }
        | StructPatternNode::SlicePrefix { list: true, .. }) => {
            if has_tcond {
                return Err(anyhow!(
                    "emit_clif: explicit type predicate on a list pattern \
                         not lowerable"
                ));
            }
            let SelectScrut::Value { disc, payload, .. } = scrut else {
                bail!("emit_clif: list pattern over a non-value scrutinee {scrut_typ}")
            };
            let whole = |id| SelectArmBind::Value { id, kind: LocalKind::Value };
            Some(emit_list_pattern_cond(cx, p, (disc, payload), scrut_typ, whole, binds)?)
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
                let member = match has_tcond {
                    true => None,
                    false => {
                        emit_member_cond(cx, p, pred_typ, false, scrut, scrut_typ, binds)?
                    }
                };
                match member {
                    Some((test, Some(c))) => Some(cx.b.ins().band(test, c)),
                    Some((test, None)) => Some(test),
                    None => {
                        return Err(anyhow!(
                            "emit_clif: slice/tuple/struct select pattern over \
                             {scrut_typ} not lowerable"
                        ));
                    }
                }
            }
        },
    };
    Ok(scond)
}

/// `sp` over the composite member of a value scrutinee (an option or a
/// result of a composite, a union holding composites) that the arm's
/// predicate `pred_typ` names: the test that the value is that member,
/// and `sp`'s condition over the member. A value failing the test reads
/// as the empty array, which every read of it survives. An option's or
/// a result's composite is the one array the value may be; any other
/// member is tested by type, a `written` predicate strictly, an inferred
/// one as the node-walk's shape test reads it. `None` where the
/// scrutinee is no such value or the member no composite.
fn emit_member_cond(
    cx: &mut BodyCx,
    sp: &StructPatternNode,
    pred_typ: &Type,
    written: bool,
    scrut: SelectScrut,
    scrut_typ: &Type,
    binds: &mut SmallVec<[SelectArmBind; 8]>,
) -> Result<Option<(ClifValue, Option<ClifValue>)>> {
    let SelectScrut::Value {
        kind: ValueKind::Nullable | ValueKind::Value,
        disc,
        payload,
    } = scrut
    else {
        return Ok(None);
    };
    let composite = |t: &Type| {
        matches!(
            kernel_abi::abi_kind(t),
            Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct)
        )
    };
    let pred = freeze_node_typ(cx.ctx, pred_typ).filter(composite);
    let (test, member) =
        match (kernel_abi::nullable_inner(scrut_typ).filter(composite), pred) {
            (Some(inner), pred) if !written || pred.as_ref() == Some(&inner) => {
                let cd = clean_disc(cx.b, disc);
                (cx.b.ins().icmp_imm_s(IntCC::Equal, cd, value_disc::ARRAY), inner)
            }
            (_, Some(pred)) if written => {
                let typ = cx.interned_type(&pred_typ.resolve_tvars())?;
                let call = cx.call_helper("graphix_value_is_a", &[typ, disc, payload])?;
                (cx.b.ins().icmp_imm_s(IntCC::NotEqual, call[0], 0), pred)
            }
            (_, Some(pred)) => {
                let test = match emit_inferred_guard_cond(cx, pred_typ, scrut, scrut_typ)?
                {
                    Some(test) => test,
                    None => cx.b.ins().iconst(types::I8, 1),
                };
                (test, pred)
            }
            (_, None) => return Ok(None),
        };
    let empty = cx.call_helper("graphix_valarray_empty_borrowed", &[])?[0];
    let ptr = cx.b.ins().select(test, payload, empty);
    let member_scrut = SelectScrut::Composite { disc, ptr };
    let c = emit_structure_cond(cx, sp, pred_typ, false, member_scrut, &member, binds)?;
    Ok(Some((test, c)))
}

/// A variant pattern's condition over the variant whose words are `v`
/// (the scrutinee, or a borrowed leaf or payload): its tag and arity,
/// then each payload's own pattern against its member of `pred`, the
/// frozen predicate.
fn emit_variant_cond(
    cx: &mut BodyCx,
    v: (ClifValue, ClifValue),
    tag: &ArcStr,
    pbinds: &[StructPatternNode],
    pred: &Type,
    binds: &mut SmallVec<[SelectArmBind; 8]>,
) -> Result<ClifValue> {
    // each payload is read at its one type: the predicate's cases of this
    // tag and arity must agree on it
    let mut cases = kernel_abi::variant_cases(pred)
        .into_iter()
        .flatten()
        .filter(|(t, e)| t == tag && e.len() == pbinds.len())
        .map(|(_, e)| e);
    let elts = cases.next().ok_or_else(|| {
        anyhow!("emit_clif: variant pattern `{tag}` doesn't match its predicate {pred}")
    })?;
    if cases.any(|e| e != elts) {
        bail!(
            "emit_clif: variant pattern `{tag}` over cases of {pred} whose payloads differ"
        )
    }
    let tag_ptr = cx.interned_str(tag)?;
    // The helper checks arity as well as tag: same-tag arms at different
    // arities are distinct cases.
    let arity = cx.b.ins().iconst(types::I64, pbinds.len() as i64);
    let call = cx.call_helper("graphix_variant_tag_eq", &[v.0, v.1, tag_ptr, arity])?;
    let mut cond = call[0];
    for (idx, (sub, elt)) in pbinds.iter().zip(elts.iter()).enumerate() {
        if let Some(c) = emit_leaf_cond(cx, LeafAt::Payload { v, idx }, sub, elt, binds)?
        {
            cond = cx.b.ins().band(cond, c);
        }
    }
    Ok(cond)
}

/// The name every pattern bind installs under; binds resolve by
/// BindId, so the name is never looked up.
const PAT_BIND_NAME: ArcStr = arcstr::literal!("__pat");

/// A drop-safe tainted placeholder pair for a local of `kind` whose
/// STALE bits are `stale`: the or-chain's no-match binds.
fn placeholder_for_kind(
    cx: &mut BodyCx,
    kind: LocalKind,
    stale: ClifValue,
) -> Result<(ClifValue, ClifValue)> {
    let abi = match kind {
        LocalKind::Scalar(p) => AbiKind::Scalar(p),
        LocalKind::String => AbiKind::String,
        LocalKind::Composite => AbiKind::Array,
        LocalKind::Value => AbiKind::Value,
    };
    let cv = emit_bottom_of_kind(cx, abi)?;
    Ok((cx.b.ins().bor(cv.disc, stale), cv.payload))
}

/// Where an or-chain goes when no alternative matches.
#[derive(Clone, Copy)]
enum NoMatch {
    /// The caller's fail block, with no binds.
    Fail(Block),
    /// The done block, binding tainted placeholders whose STALE bits are
    /// these: STALE for an arm that was not delivered (a delivery that
    /// never happened is not an event), the scrutinee's for a leaf, which
    /// misses only over a bottom scrutinee.
    Placeholders(ClifValue),
}

/// An or-pattern alternative's structure condition (`None`: it takes
/// every value) and its bind specs.
type AltMatch = (Option<ClifValue>, SmallVec<[SelectArmBind; 8]>);

/// Alternative `k`'s [`AltMatch`]: it tests against its own member of
/// the arm's inferred Set (`alt_types`); a non-Set predicate applies
/// whole.
fn emit_alt_match(
    cx: &mut BodyCx,
    alts: &[StructPatternNode],
    k: usize,
    alt_types: Option<&[Type]>,
    pred_typ: &Type,
    scrut: SelectScrut,
    scrut_typ: &Type,
) -> Result<AltMatch> {
    let at = alt_types.map_or(pred_typ, |ts| &ts[k]);
    let mut binds: SmallVec<[SelectArmBind; 8]> = SmallVec::new();
    let c = emit_structure_cond(cx, &alts[k], at, false, scrut, scrut_typ, &mut binds)?;
    Ok((c, binds))
}

/// Every alternative's [`AltMatch`], all emitted in the current block:
/// a structure test's reads are total.
fn emit_alt_matches(
    cx: &mut BodyCx,
    alts: &[StructPatternNode],
    pred_typ: &Type,
    scrut: SelectScrut,
    scrut_typ: &Type,
) -> Result<SmallVec<[AltMatch; 4]>> {
    let alt_types = set_members(pred_typ, alts.len());
    (0..alts.len())
        .map(|k| {
            emit_alt_match(cx, alts, k, alt_types.as_deref(), pred_typ, scrut, scrut_typ)
        })
        .collect()
}

/// Whether any alternative matched; `None` when one takes every value.
fn or_of_alts(cx: &mut BodyCx, m: &[AltMatch]) -> Option<ClifValue> {
    let mut any: Option<ClifValue> = None;
    for (c, _) in m {
        let c = (*c)?;
        any = Some(match any {
            None => c,
            Some(a) => cx.b.ins().bor(a, c),
        });
    }
    any
}

/// Emit an or-pattern's alternative chain over the `n` alternatives
/// `alt` yields (each emitted where the chain reaches it, or handed over
/// from a block dominating this one): alternatives test left to right;
/// the first match installs its binds and jumps to one `done` block whose
/// params bind the locals once under the shared BindIds, each at its
/// alternatives' common kind, or as a Value where they differ (a shared
/// name's type is the union of theirs). Where `mask` is false the binds
/// read as bottom too (an arm the chain does not reach). Returns the
/// matched i8 with the builder in `done` and the binds installed; the
/// caller's env mark, taken before this, scopes the arm-exit drops over
/// them.
fn emit_or_chain(
    cx: &mut BodyCx,
    n: usize,
    alt: &mut dyn FnMut(&mut BodyCx, usize) -> Result<AltMatch>,
    scrut: SelectScrut,
    nomatch: NoMatch,
    mask: Option<ClifValue>,
) -> Result<ClifValue> {
    let tests: SmallVec<[Block; 4]> = (0..n).map(|_| cx.b.create_block()).collect();
    // the last alternative fails to `nomatch`, else to a block of its own
    // that hands the done block placeholders
    let last_fail = match nomatch {
        NoMatch::Fail(b) => b,
        NoMatch::Placeholders(_) => cx.b.create_block(),
    };
    cx.b.ins().jump(tests[0], &[]);
    // each matched alternative hands its binds, at its own kinds, to a
    // block of its own, which joins them to the done block's layout
    let mut matched: SmallVec<[(Block, SmallVec<[(BindId, LocalKind); 8]>); 4]> =
        SmallVec::new();
    for k in 0..n {
        cx.b.switch_to_block(tests[k]);
        cx.b.seal_block(tests[k]);
        let fail_to = if k + 1 < n { tests[k + 1] } else { last_fail };
        let (scond, binds) = alt(cx, k)?;
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
        let mut ids: SmallVec<[BindId; 8]> = SmallVec::new();
        for b in binds.iter() {
            b.ids(&mut |id| ids.push(id));
        }
        ids.sort_by_key(|id| id.inner());
        if let Some((_, first)) = matched.first()
            && (ids.len() != first.len()
                || ids.iter().zip(first.iter()).any(|(a, (b, _))| a != b))
        {
            return Err(anyhow!(
                "emit_clif: or-pattern alternatives bind different id sets"
            ));
        }
        let joined = cx.b.create_block();
        let mut kinds: SmallVec<[(BindId, LocalKind); 8]> = SmallVec::new();
        let mut args: SmallVec<[BlockArg; 16]> = SmallVec::new();
        for id in ids.iter() {
            let l = cx.env.lookup_id(*id).ok_or_else(|| {
                anyhow!("emit_clif: or-chain bind not in env after install")
            })?;
            kinds.push((*id, l.kind));
            cx.b.append_block_param(joined, types::I64);
            cx.b.append_block_param(joined, local_payload_ty(l.kind));
            let (dv, pv) = (cx.b.use_var(l.words.disc), cx.b.use_var(l.words.payload));
            args.push(dv.into());
            args.push(pv.into());
        }
        // Ownership is forwarded to the done block's binds; no drops.
        cx.env.truncate(amark);
        cx.b.ins().jump(joined, &args);
        matched.push((joined, kinds));
    }
    let Some((_, first)) = matched.first() else {
        bail!("emit_clif: empty or-pattern chain")
    };
    let layout: SmallVec<[(BindId, LocalKind); 8]> = first
        .iter()
        .enumerate()
        .map(|(i, (id, kind))| {
            let same = matched.iter().all(|(_, ks)| ks[i].1 == *kind);
            (*id, if same { *kind } else { LocalKind::Value })
        })
        .collect();
    let done = cx.b.create_block();
    cx.b.append_block_param(done, types::I8);
    for (_, kind) in layout.iter() {
        cx.b.append_block_param(done, types::I64);
        cx.b.append_block_param(done, local_payload_ty(*kind));
    }
    for (joined, kinds) in matched.iter() {
        cx.b.switch_to_block(*joined);
        cx.b.seal_block(*joined);
        let params: SmallVec<[ClifValue; 16]> =
            cx.b.block_params(*joined).iter().copied().collect();
        let one = cx.b.ins().iconst(types::I8, 1);
        let mut args: SmallVec<[BlockArg; 20]> = SmallVec::new();
        args.push(one.into());
        for (i, ((_, from), (_, to))) in kinds.iter().zip(layout.iter()).enumerate() {
            let (disc, payload) = (params[2 * i], params[2 * i + 1]);
            // a composite's and a string's words already are a Value's
            let payload = match (*from, *to) {
                (LocalKind::Scalar(p), LocalKind::Value) => {
                    scalar_to_payload_i64(cx.b, p, payload)
                }
                _ => payload,
            };
            args.push(disc.into());
            args.push(payload.into());
        }
        cx.b.ins().jump(done, &args);
    }
    if let NoMatch::Placeholders(stale) = nomatch {
        let ph = last_fail;
        cx.b.switch_to_block(ph);
        cx.b.seal_block(ph);
        let zero = cx.b.ins().iconst(types::I8, 0);
        let mut args: SmallVec<[BlockArg; 20]> = SmallVec::new();
        args.push(zero.into());
        for (_, kind) in layout.iter() {
            let (d, pv) = placeholder_for_kind(cx, *kind, stale)?;
            args.push(d.into());
            args.push(pv.into());
        }
        cx.b.ins().jump(done, &args);
    }
    cx.b.switch_to_block(done);
    cx.b.seal_block(done);
    let params: SmallVec<[ClifValue; 20]> =
        cx.b.block_params(done).iter().copied().collect();
    let matched = params[0];
    for (i, (id, kind)) in layout.iter().enumerate() {
        let disc = mask_unmatched(cx, params[1 + 2 * i], mask);
        bind_local(cx, PAT_BIND_NAME, disc, params[2 + 2 * i], *kind, Some(*id));
    }
    Ok(matched)
}
