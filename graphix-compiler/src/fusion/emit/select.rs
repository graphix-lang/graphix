//! `select` emission: scrutinee classification, pattern
//! conditions, arm dispatch, and the merge-shape protocol.

use crate::{
    BindId, Node, NodeView, Refs, Rt, Update, UserEvent,
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
    lower::{freeze_node_typ, resolve_node_typ},
    nodes::{emit_bottom_of_kind, emit_owned_value_operand_node},
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
enum SelectArmBind {
    /// `n => ...` — bind the scalar scrutinee itself.
    Scrut(BindId),
    /// A read out of a value scrutinee: an option's payload, a union's
    /// member, the scrutinee narrowed to one type (the whole scrutinee
    /// included). A non-scalar is cloned out as an owned local, dropped at
    /// the arm's scope exit; legal under a mask, where a mismatch yields a
    /// drop-safe default.
    Value { id: BindId, kind: LocalKind },
    /// `` `Tag(x) `` — bind one variant payload; a wrong-tag read yields 0,
    /// or a drop-safe default behind a tainted disc for a non-scalar,
    /// cloned out as an owned local. `on` is the variant read when it is
    /// not the scrutinee (a payload of an enclosing variant, borrowed).
    Payload {
        id: BindId,
        idx: usize,
        kind: LocalKind,
        on: Option<(ClifValue, ClifValue)>,
    },
    /// `[<a, b>]` / `[<h, rest..>]` — bind the j-th head of a list
    /// scrutinee, cloned out as an owned local of `kind` (legal under a mask).
    ListHead { id: BindId, idx: usize, kind: LocalKind },
    /// The rest bind: the k-th TAIL itself — O(1), shares the spine.
    ListTail { id: BindId, k: usize },
    /// `(x, y)` / `{f, ..}` / `[h, ..]` — bind one leaf of type `typ` of a
    /// composite scrutinee (the scrutinee, or a borrowed interior pointer
    /// for a nested pattern); a non-scalar is cloned out as an owned local,
    /// a short or mismatched read yielding a drop-safe default.
    Elem { id: BindId, idx: ElemIdx, typ: Type, kind: LocalKind, parent_ptr: ClifValue },
    /// `[h, rest..]` / `[init.., l]` / `all@ [..]` — bind the elements
    /// `[start, len - back)` of an array scrutinee, cloned out as an
    /// owned composite local.
    Subslice { id: BindId, start: usize, back: usize, parent_ptr: ClifValue },
}

impl SelectArmBind {
    fn id(&self) -> BindId {
        match self {
            Self::Scrut(id)
            | Self::Value { id, .. }
            | Self::Payload { id, .. }
            | Self::ListHead { id, .. }
            | Self::ListTail { id, .. }
            | Self::Elem { id, .. }
            | Self::Subslice { id, .. } => *id,
        }
    }
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
    let typ = sel.arg.node.typ();
    let scrut_typ = kernel_abi::freeze_for_abi_normalized(typ).ok_or_else(|| {
        anyhow!("emit_clif: select scrutinee type {typ} doesn't freeze concrete")
    })?;
    classify_scrutinee(cx, &sel.arg.node, scrut_typ, sel.spec.id)
}

/// A destructuring `let pat = value`: its pattern's leaves bound over
/// `value` as a select arm's are, into the block's scope. An owned value
/// stays a block local, dropped with the block's others.
pub(super) fn emit_let_binds<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    pattern: &StructPatternNode,
    typ: &Type,
    value: &Node<R, E>,
    id: ExprId,
) -> Result<()> {
    let typ = freeze_node_typ(cx.ctx, typ)
        .ok_or_else(|| anyhow!("emit_clif: let pattern type {typ} doesn't freeze"))?;
    let (scrut, scrut_typ, _kept) = classify_scrutinee(cx, value, typ, id)?;
    let mut binds: SmallVec<[SelectArmBind; 8]> = SmallVec::new();
    let cond = emit_structure_cond(
        cx, pattern, &scrut_typ, false, scrut, &scrut_typ, &mut binds,
    )?;
    install_arm_binds(cx, &binds, scrut, cond)
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
    let cond = emit_structure_cond(cx, pattern, typ, false, scrut, typ, &mut binds)?;
    install_arm_binds(cx, &binds, scrut, cond)
}

/// [`classify_select_scrutinee`] for the value `node`, of the frozen
/// type `scrut_typ`, of the expression `id`.
fn classify_scrutinee<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    node: &Node<R, E>,
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
        AbiKind::Array | AbiKind::Tuple | AbiKind::Struct => {
            let cv = node.emit_clif(cx)?;
            adopt(cx, LocalKind::Composite, cv.disc, cv.payload, owned);
            SelectScrut::Composite { disc: cv.disc, ptr: cv.payload }
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
/// test and every leaf's own test, in one block. An array, tuple or
/// struct `@` or rest bind lowers as a subslice. Variant leaves test
/// borrowed words; non-scalar binds clone owned locals. Or- and abstract
/// leaves refuse.
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
    // XCR claude for claude: [perf/medium] An element leaf lowers only as a bind, an
    // exact-scalar literal or a nested composite. A string literal (`("a", n)`, `["a",
    // ..]`), a literal over a nullable/union element (`[1, ..]` over Array<[i64,
    // null]>) and an or-pattern (`(true, `A | `B)`, `[x, 1 | 2]`, `` `A(1 | 2) ``) all
    // de-fuse here. Each of them fuses as a top-level arm, and CLAUDE.md says
    // or-patterns at element positions fuse natively. The tests hide it:
    // coverage_nested_or pins FuseExpect::None, and or_nested and or_in_slice_element
    // pass FuseExpect::Jit only because an unrelated region fused. Fix: read the
    // element as a borrowed value (as the Variant leaf below does) and emit the
    // top-level literal / union-literal / or-chain condition against it; then put
    // #[native] on those fixtures. probe:
    // design/review-2026-10-19/repro/tests-select-element-leaf-defuses.sh
    // (tests-select-02)
    // 2026-10-19 claude: An element leaf that is a literal off its exact scalar type
    // (a string, a null, a literal over a union element) or an or-pattern that binds
    // nothing reads the element's borrowed Value words and tests them with
    // emit_structure_cond's own literal and variant tests (emit_value_leaf_cond:
    // alternatives OR in one block); a variant payload's literals and bind-free
    // or-patterns take the same test. An or-pattern leaf that binds still refuses.
    // #[native] now on coverage_nested_or (FuseExpect::Jit), or_nested and
    // or_in_slice_element. Pin: graphix-tests --test fusion_review
    // select_leaves_options_or_kinds_and_own_type_tests (t0..t8) and the review's
    // tests-select-element-leaf-defuses.sh, which prints nothing.
    let mut lit_leaves: SmallVec<[(ElemIdx, PrimType, &Value); 8]> = SmallVec::new();
    let mut nested: SmallVec<[(ElemIdx, &StructPatternNode, Type); 8]> = SmallVec::new();
    let mut value_leaves: SmallVec<[(ElemIdx, &StructPatternNode, Type); 8]> =
        SmallVec::new();
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
                    typ: leaf.typ.clone(),
                    kind: LocalKind::Scalar(prim),
                    parent_ptr: ptr,
                }),
                None => {
                    let kind = owned_kind(&leaf.typ).ok_or_else(|| {
                        anyhow!(
                            "emit_clif: select pattern leaf bind {:?} not lowerable",
                            leaf.typ
                        )
                    })?;
                    binds.push(SelectArmBind::Elem {
                        id: *id,
                        idx: leaf.idx,
                        typ: leaf.typ.clone(),
                        kind: kind.into(),
                        parent_ptr: ptr,
                    })
                }
            },
            // XCR claude for claude: [perf/medium] A string, null or union-typed scalar
            // literal at a tuple/struct/array leaf, and a list pattern at a leaf,
            // de-fuse the whole select. This arm accepts only a literal whose prim is
            // the leaf's exact static type, and the nested-leaf arm below admits no
            // Value-kind leaf except a variant. The same literals fuse at the top level
            // (emit_structure_cond) and in a variant payload (emit_variant_cond). So
            // `("quit", _)`, `(null, _)` and `(0, _)` over `[i64, null]` all node-walk.
            // Fix: read such a leaf as borrowed Value words, as the Variant leaf does,
            // and test it with one shared literal test (tag + typed payload compare,
            // graphix_value_is_str, disc == NULL). Use that test in emit_structure_cond
            // and emit_variant_cond too, which today each carry their own copy. probe:
            // design/review-2026-10-19/repro/f-select-literal-leaf-defuse.gx
            // (f-select-01)
            // The typed element read is total: a slot not of `prim`'s
            // family reads as 0, which a `0` literal would match. Only the
            // static type proves the read faithful; any other literal tests
            // the element's value.
            // 2026-10-19 claude: Fixed with tests-select-02: a string, null or
            // union-typed literal leaf tests the element's borrowed Value words through
            // emit_value_leaf_cond, which emit_variant_cond's payload literals share
            // (its two hand-written copies are gone). A list pattern at a leaf still
            // refuses: its head binds read from the arm's scrutinee. Pin: fusion_review
            // select_leaves_options_or_kinds_and_own_type_tests (t0..t2, t6..t8); the
            // repro's f, g, h and k fuse, l (the list leaf) does not.
            StructPatternNode::Literal(v)
                if let Some(prim) = kernel_abi::scalar_prim_of_value(v)
                    && kernel_abi::scalar_prim(&leaf.typ) == Some(prim) =>
            {
                lit_leaves.push((leaf.idx, prim, v));
            }
            sub @ StructPatternNode::Literal(_) => {
                value_leaves.push((leaf.idx, sub, leaf.typ.clone()))
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
            sub @ StructPatternNode::Variant { .. } => {
                nested.push((leaf.idx, sub, leaf.typ.clone()));
            }
            sub @ StructPatternNode::Or { .. } if !binds_any(sub) => {
                value_leaves.push((leaf.idx, sub, leaf.typ.clone()))
            }
            StructPatternNode::Or { .. } => {
                return Err(anyhow!(
                    "emit_clif: a binding or-pattern leaf not lowerable"
                ));
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
        let (read, idx_v) = elem_index(cx, idx);
        let c = match sub {
            StructPatternNode::Variant { tag, all, binds: payloads } => {
                if let Some(id) = all {
                    binds.push(SelectArmBind::Elem {
                        id: *id,
                        idx,
                        typ: typ.clone(),
                        kind: LocalKind::Value,
                        parent_ptr: ptr,
                    });
                }
                let helper = match read {
                    ElementRead::ArrayIndex => "graphix_valarray_get_value_borrowed",
                    ElementRead::StructField => "graphix_struct_get_value_borrowed",
                };
                let call = cx.call_helper(helper, &[ptr, idx_v])?;
                let child = (call[0], call[1]);
                emit_variant_cond(cx, child, Some(child), tag, payloads, &typ, binds)?
            }
            _ => {
                let helper = match read {
                    ElementRead::ArrayIndex => "graphix_valarray_get_array_borrowed",
                    ElementRead::StructField => "graphix_struct_get_array_borrowed",
                };
                let child = cx.call_helper(helper, &[ptr, idx_v])?[0];
                emit_composite_pattern_cond(cx, child, &typ, sub, binds)?
            }
        };
        cond = cx.b.ins().band(cond, c);
    }
    for (idx, sub, typ) in value_leaves {
        let (read, idx_v) = elem_index(cx, idx);
        let helper = match read {
            ElementRead::ArrayIndex => "graphix_valarray_get_value_borrowed",
            ElementRead::StructField => "graphix_struct_get_value_borrowed",
        };
        let call = cx.call_helper(helper, &[ptr, idx_v])?;
        let c = emit_value_leaf_cond(cx, sub, &typ, (call[0], call[1]))?;
        cond = cx.b.ins().band(cond, c);
    }
    Ok(cond)
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

/// Arm `pat`'s pattern condition against `scrut`, pushing its binds
/// onto `binds`; `None` when the arm matches unconditionally. An
/// or-pattern installs its shared binds itself, in the chain's done
/// block (`nomatch` and `mask` as for [`emit_or_chain`]).
fn emit_arm_condition<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    pat: &PatternNode<R, E>,
    scrut: SelectScrut,
    scrut_typ: &Type,
    nomatch: Option<Block>,
    mask: Option<ClifValue>,
    binds: &mut SmallVec<[SelectArmBind; 8]>,
) -> Result<Option<ClifValue>> {
    if let StructPatternNode::Or { alts } = &pat.structure_predicate {
        if pat.explicit_type_predicate {
            bail!(
                "emit_clif: explicit type predicate on an or-pattern arm not lowerable"
            );
        }
        let m = emit_or_chain(
            cx,
            alts,
            &pat.type_predicate,
            scrut,
            scrut_typ,
            nomatch,
            mask,
        )?;
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
pub(super) fn emit_select_arms<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    sel: &Select<R, E>,
    scrut: SelectScrut,
    scrut_typ: &Type,
    emit_arm: &mut dyn FnMut(
        &mut BodyCx,
        &Node<R, E>,
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
    // a non-or arm's condition and bind specs from the prologue, which
    // dominates the chain: the chain reuses them
    let mut conds: SmallVec<[Option<Option<ClifValue>>; 8]> =
        (0..n).map(|_| None).collect();
    // an unguarded non-or arm's bind specs from the prologue, which the
    // chain installs where it takes the arm
    let mut prologue_binds: SmallVec<[Option<SmallVec<[SelectArmBind; 8]>>; 8]> =
        (0..n).map(|_| None).collect();
    let impure = sel.arms.iter().rposition(|(pat, _)| {
        pat.guard.as_ref().is_some_and(|g| !guard_is_pure_of_binds(pat, &g.node))
    });
    let open = cx.b.declare_var(types::I8);
    let reaches_first = cx.b.ins().iconst(types::I8, 1);
    cx.b.def_var(open, reaches_first);
    for (i, (pat, _)) in sel.arms.iter().enumerate().take(impure.map_or(0, |l| l + 1)) {
        let reached = cx.b.use_var(open);
        let Some(g) = &pat.guard else {
            // an unguarded arm claims by its condition; its binds are the
            // chain's
            let mark = cx.env.mark();
            let mut binds: SmallVec<[SelectArmBind; 8]> = SmallVec::new();
            let cond =
                emit_arm_condition(cx, pat, scrut, scrut_typ, None, None, &mut binds)?;
            emit_scope_drops(cx, mark)?;
            cx.env.truncate(mark);
            let still = match cond {
                Some(c) => cx.b.ins().band_not(reached, c),
                None => cx.b.ins().iconst(types::I8, 0),
            };
            cx.b.def_var(open, still);
            // XCR claude for claude: [perf/low] An unguarded arm before the last impure
            // guard has its condition emitted here only to update `open`. Nothing is
            // stored in `conds`, so the chain emits the same condition again and
            // repeats every helper call (graphix_list_match's spine walk,
            // graphix_variant_tag_eq, graphix_value_is_a). For an unguarded or-arm,
            // emit_or_chain also clones the matching alternative's owned binds, and the
            // emit_scope_drops just below drops them unread. Fix: keep this arm's
            // condition and bind specs for the chain to reuse. For an or-arm, compute
            // only the OR of its alternatives' structure conditions; their reads are
            // total, so no installs or done block are needed. Found by reading; not
            // run. (f-select-05)
            // 2026-10-19 claude: An unguarded non-or arm the guard prologue tests keeps
            // its condition (conds) and its bind specs (prologue_binds); the chain
            // reuses the condition and installs the specs where it takes the arm, so
            // its helper calls run once per invocation. An or-arm still recomputes in
            // the chain (the second half of the suggested fix is not taken). Pin:
            // fusion_review prologue_unguarded_arm_binds_reach_the_chain (an unguarded
            // bind before an impure guard, read in the chain; a perf pin it is not).
            if !matches!(&pat.structure_predicate, StructPatternNode::Or { .. }) {
                conds[i] = Some(cond);
                prologue_binds[i] = Some(binds);
            }
            continue;
        };
        let mut binds: SmallVec<[SelectArmBind; 8]> = SmallVec::new();
        let mut pcond = emit_arm_condition(
            cx,
            pat,
            scrut,
            scrut_typ,
            None,
            Some(reached),
            &mut binds,
        )?;
        if !pat.explicit_type_predicate
            && let Some(typed) =
                emit_inferred_guard_cond(cx, &pat.type_predicate, scrut, scrut_typ)?
        {
            pcond = Some(match pcond {
                Some(shape) => cx.b.ins().band(shape, typed),
                None => typed,
            });
        }
        let mask = match pcond {
            Some(c) => cx.b.ins().band(c, reached),
            None => reached,
        };
        install_arm_binds(cx, &binds, scrut, Some(mask))?;
        let gcv = g.node.emit_clif(cx)?;
        let gs = cx.b.ins().band_imm(gcv.disc, STALE);
        let gbot = is_tainted(cx.b, gcv.disc);
        let valid = is_untainted(cx.b, gcv.disc);
        let eff = cx.b.ins().band(gcv.payload, valid);
        guard_vals[i] = Some(GuardPlanes { eff, gbot, gs });
        // the chain stops at this arm when it takes it or its guard is bottom
        let stops = cx.b.ins().bor(eff, gbot);
        let claims = cx.b.ins().band(mask, stops);
        let still = cx.b.ins().band_not(reached, claims);
        cx.b.def_var(open, still);
        if !matches!(&pat.structure_predicate, StructPatternNode::Or { .. }) {
            conds[i] = Some(pcond);
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
        // a guarded non-or arm the prologue ran has its binds installed
        // already; an unguarded one hands its bind specs to this match
        let pcond = match conds[i].take() {
            Some(pcond) => {
                if let Some(b) = prologue_binds[i].take() {
                    binds = b;
                }
                pcond
            }
            None => {
                emit_arm_condition(cx, pat, scrut, scrut_typ, or_fail, None, &mut binds)?
            }
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
        // CR claude for eric: [perf/low] An arm with an impure guard reuses the
        // prologue's condition here but installs its owned binds a second time. The
        // prologue already cloned the payload for the guard and dropped it. The arm
        // therefore pays two payload clones per invocation, even when its body never
        // reads the bind. In the kernel for `A(xs) if array::len(xs) > lim => 1`, the
        // clone helper runs in the prologue, again in arm 1's matched block, and again
        // in arm 2. The control without the guard runs it twice. Fix: keep the
        // prologue's installed binds alive into the chain for a non-or arm and drop
        // them on the guard-false and undetermined exits, or never clone a bind the arm
        // body does not read. This is the remaining half of f-select-13. (x-rerun-3-04)
        // the arm installs what its body reads, and a guard it runs here; a
        // guard the prologue ran read its own copies
        // 2026-10-16 claude: The second fix offered here is in (see f-select-06): an
        // arm installs only the binds its body, or a guard it runs itself, reads, so
        // the A(xs) if array::len(xs) > lim => 1 arm no longer clones xs in its matched
        // block.
        // 2026-10-17 claude: The first fix offered here is in too (f-select-06): a
        // non-or arm the guard prologue ran keeps the prologue's masked binds into the
        // chain, its body reads them, and every exit drops them (an arm's exit, the
        // miss and undecided blocks; a tail terminator drops the env). Both halves are
        // done, so this arm clones its payload once per invocation. The A(xs) if
        // array::len(xs) > lim => array::len(xs) kernel calls the clone helper once
        // where it called it twice. Pin
        // lang::fusion::native_guard_and_body_share_a_bind.
        let mut reads = Refs::default();
        body.refs(&mut reads);
        if let Some(g) = &pat.guard
            && guard_vals[i].is_none()
        {
            g.node.refs(&mut reads);
        }
        binds.retain(|b| reads.refed.contains(&b.id()));
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
        emit_arm(cx, body, mark, smark, guards_stale)?;
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
        let ss = cx.b.ins().band_imm(sdisc, STALE);
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
                let matches = cx.b.ins().icmp_imm(IntCC::Equal, disc, prim as i64);
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
fn emit_arm_cond<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    pat: &PatternNode<R, E>,
    scrut: SelectScrut,
    scrut_typ: &Type,
    binds: &mut SmallVec<[SelectArmBind; 8]>,
) -> Result<(Option<ClifValue>, Option<ClifValue>)> {
    // a composite member of a union of composites: the value's own shape
    // tested as the node-walk tests it, then destructured as that member;
    // a value that fails the test reads as the empty array, which every
    // read of it survives
    if pat.explicit_type_predicate
        && let SelectScrut::Value {
            kind: ValueKind::Value | ValueKind::Nullable,
            disc,
            payload,
        } = scrut
        && let Some(pred) = freeze_node_typ(cx.ctx, &pat.type_predicate)
        && matches!(
            kernel_abi::abi_kind(&pred),
            Some(AbiKind::Tuple | AbiKind::Struct | AbiKind::Array)
        )
    {
        let typ = cx.interned_type(&pat.type_predicate.resolve_tvars())?;
        let call = cx.call_helper("graphix_value_is_a", &[typ, disc, payload])?;
        let tcond = cx.b.ins().icmp_imm(IntCC::NotEqual, call[0], 0);
        let empty = cx.call_helper("graphix_valarray_empty_borrowed", &[])?[0];
        let ptr = cx.b.ins().select(tcond, payload, empty);
        let member = SelectScrut::Composite { disc, ptr };
        let scond = emit_structure_cond(
            cx,
            &pat.structure_predicate,
            &pat.type_predicate,
            false,
            member,
            &pred,
            binds,
        )?;
        return Ok((Some(tcond), scond));
    }
    // The chain has excluded earlier arms. The guard prologue has not,
    // so it also tests the inferred predicate before delivering binds.
    // CR claude for eric: [readability/low] The comment above says the guard prologue
    // has not excluded earlier arms. It has: every prologue bind is masked by `open`,
    // which drops values an earlier arm claimed. So the stated reason for
    // emit_inferred_guard_cond is gone, and while narrowing is sound that test only
    // repeats the reach mask. Where shallow_discriminant cannot reduce the predicate,
    // it costs a graphix_inferred_type_matches call per invocation. Decide whether it
    // stays as a deliberate mirror of the node-walk's shape_matches, and say so, or
    // delete it. Then rewrite this comment to match. Found by reading; not run.
    // (f-select-06)
    // XCR claude for claude: [perf/medium] A type test that every value of the scrutinee
    // passes, such as `string as t` over a string or `T as {a, ..}` over a T, is
    // refused below ('type predicate .. not lowerable'), so a redundant annotation
    // turns a kernel into node-walk. The same arm without `T as` fuses. selectstruct,
    // nestedmatch1 and never_arms_absorb carry FuseExpect::Jit but their selects
    // node-walk for this reason; an unrelated region satisfies the claim. Fix: when the
    // frozen scrutinee type is contained in the frozen predicate, tcond is None, and
    // only the structure test runs. Then put #[native] on those fixtures. probe:
    // design/review-2026-10-19/repro/tests-select-trivial-type-test-defuses.sh
    // (tests-select-03)
    // 2026-10-19 claude: emit_arm_cond yields no type test when the frozen predicate
    // equals the frozen scrutinee type and holds no abstract type
    // (type_test_is_trivial), so a composite pattern under it reaches
    // emit_structure_cond untested. Equality, not containment: the node-walk's is_a is
    // strict. #[native] now on selectstruct, nestedmatch1 and never_arms_absorb. Pin:
    // those fixtures and fusion_review
    // select_leaves_options_or_kinds_and_own_type_tests (y0..y2);
    // tests-select-trivial-type-test-defuses.sh prints nothing.
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
        // XCR claude for claude: [perf/low] A type test the scrutinee's static type
        // already proves de-fuses every non-scalar select. Examples are `Array<i64> as
        // [h, ..]` over an Array<i64>, `(i64, string) as (n, _)` over that tuple, and
        // `string as x` over a string. Only the scalar case below, where the prim
        // equals the predicate, yields no test. Fix: yield None when the frozen
        // predicate equals scrut_typ and holds no abstract part, so the node-walk's
        // strict is_a agrees. Then a composite pattern under it reaches
        // emit_structure_cond with has_tcond false. probe:
        // design/review-2026-10-19/repro/f-select-vacuous-type-test-defuse.gx
        // (f-select-04)
        // 2026-10-19 claude: Fixed with tests-select-03 (type_test_is_trivial). Pin:
        // fusion_review select_leaves_options_or_kinds_and_own_type_tests (y0..y2 are
        // the repro's f, g and h).
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

/// Whether an arm's written type test names the scrutinee's own type,
/// holding no abstract type (whose test is nominal): every value passes.
fn type_test_is_trivial<R: Rt, E: UserEvent>(
    cx: &BodyCx,
    pat: &PatternNode<R, E>,
    scrut_typ: &Type,
) -> bool {
    freeze_node_typ(cx.ctx, &pat.type_predicate)
        .is_some_and(|p| &p == scrut_typ && !p.holds_abstract(cx.ctx.type_env))
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
            Ok(cx.b.ins().icmp_imm(IntCC::Equal, disc, value_disc::NULL))
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
                     {scrut_typ}"
            ));
        }
    };
    let et = scrut_typ
        .with_deref(|t| match t {
            Some(Type::List(et)) => Some((**et).clone()),
            _ => None,
        })
        .ok_or_else(|| {
            anyhow!("emit_clif: list pattern over non-list scrutinee {scrut_typ}")
        })?;
    let (elems, k, exact, tail) = match pat {
        StructPatternNode::Slice { kind: SliceKind::List, all, binds: pb } => {
            if let Some(id) = all {
                binds.push(SelectArmBind::Value { id: *id, kind: LocalKind::Value });
            }
            (pb, pb.len(), true, None)
        }
        StructPatternNode::SlicePrefix { list: true, all, prefix, tail } => {
            if let Some(id) = all {
                binds.push(SelectArmBind::Value { id: *id, kind: LocalKind::Value });
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
    let kc = cx.b.ins().iconst(types::I64, k as i64);
    let ex = cx.b.ins().iconst(types::I8, exact as i64);
    let call = cx.call_helper("graphix_list_match", &[disc, payload, kc, ex])?;
    Ok(call[0])
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
            SelectArmBind::Payload { id, idx, kind, on } => {
                let (vd, vp) = match on {
                    Some(on) => *on,
                    None => (sdisc, value_payload()?),
                };
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
            SelectArmBind::ListHead { id, idx, kind } => {
                let j = cx.b.ins().iconst(types::I64, *idx as i64);
                let args = [sdisc, value_payload()?, j];
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
            SelectArmBind::ListTail { id, k } => {
                let kc = cx.b.ins().iconst(types::I64, *k as i64);
                let call =
                    cx.call_helper("graphix_list_tail", &[sdisc, value_payload()?, kc])?;
                let rs = call;
                (*id, rs[0], rs[1], LocalKind::Value)
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
    emit_scope_drops(cx, drops_from)?;
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
        // XCR claude for claude: [perf/medium] A slice, tuple or struct pattern over a
        // nullable scrutinee de-fuses the whole select, because this arm lowers
        // composite structure only over SelectScrut::Composite. Examples: `null as _ =>
        // .., [s, rest..] => .., [] => ..` over `[Array<string>, null]`, and `(x, y)`
        // over `[(i64, i64), null]`. The netidx-admin port writes that ladder in
        // several panels. emit_arm_cond already lowers the same structure over a
        // Value-kind union of composites: it selects the payload or a borrowed empty
        // array and destructures it as the member. But that path is gated on an
        // explicit type predicate. Fix: take that path here for any Value/Nullable
        // scrutinee whose arm predicate freezes to Array/Tuple/Struct, and fold the
        // explicit-predicate copy in emit_arm_cond into it. probe:
        // design/review-2026-10-19/repro/f-select-nullable-composite-defuse.gx
        // (f-select-02)
        // 2026-10-19 claude: emit_structure_cond destructures a slice, tuple or struct
        // pattern over an option of a composite (nullable_error_marked Some(false), a
        // composite nullable_inner) through its payload, a null reading as the borrowed
        // empty array, ANDed with the not-null test; emit_arm_cond's explicit
        // composite-member path now takes a Nullable scrutinee too (the explicit
        // spelling over an option). A result of a composite and a union of composites
        // without an explicit test still refuse. Pin: fusion_review
        // select_leaves_options_or_kinds_and_own_type_tests (o0..o3);
        // select_slice_cover_union and select_tuple_union_member_native (renamed from
        // _node_walks) now fuse.
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
            // an option of a composite: its payload destructured as the
            // member, a null reading as the empty array every read survives
            SelectScrut::Value { kind: ValueKind::Nullable, disc, payload }
                if !has_tcond
                    && kernel_abi::nullable_error_marked(scrut_typ) == Some(false)
                    && let Some(member) = kernel_abi::nullable_inner(scrut_typ)
                    && matches!(
                        kernel_abi::abi_kind(&member),
                        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct)
                    ) =>
            {
                let cd = clean_disc(cx.b, disc);
                let some = cx.b.ins().icmp_imm(IntCC::NotEqual, cd, value_disc::NULL);
                let empty = cx.call_helper("graphix_valarray_empty_borrowed", &[])?[0];
                let ptr = cx.b.ins().select(some, payload, empty);
                let c = emit_composite_pattern_cond(cx, ptr, &member, p, binds)?;
                Some(cx.b.ins().band(some, c))
            }
            _ => {
                // XCR claude for claude: [perf/medium] An option of a tuple or struct
                // (`[(i64, i64), null]`) is a Nullable value scrutinee, so the `(x, y)`
                // or `{x, y}` arm after `null as _` lands here and the whole kernel
                // node-walks. The explicit `(i64, i64) as (x, y)` spelling is refused
                // too, while selecting into a scalar option and destructuring a bare
                // tuple both fuse. Extend emit_arm_cond's composite-member path (today:
                // ValueKind::Value with an explicit predicate only) to Nullable
                // scrutinees and to arms narrowed to one composite member: test the
                // member, then destructure through SelectScrut::Composite { disc, ptr:
                // payload }. probe:
                // design/review-2026-10-19/repro/fuzz-gen-a-option-tuple-defuse.gx
                // (fuzz-gen-a-03)
                // 2026-10-19 claude: Fixed with f-select-02: an option of a tuple or
                // struct destructures through its payload, and the explicit (i64, i64)
                // as (x, y) spelling over it takes the is_a member path. Pin:
                // fusion_review select_leaves_options_or_kinds_and_own_type_tests
                // (o1..o3); the repro's #[native] f, g and h build.
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
                "emit_clif: variant pattern `{tag}` doesn't match its predicate {pred}"
            )
        })?;
    let tag_ptr = cx.interned_str(tag)?;
    // The helper checks arity as well as tag: same-tag arms at different
    // arities are distinct cases.
    let arity = cx.b.ins().iconst(types::I64, pbinds.len() as i64);
    let call = cx.call_helper("graphix_variant_tag_eq", &[v.0, v.1, tag_ptr, arity])?;
    let mut cond = call[0];
    for (idx, (sub, elt)) in pbinds.iter().zip(elts.iter()).enumerate() {
        match sub {
            StructPatternNode::Bind(id) => match kernel_abi::scalar_prim(elt) {
                Some(prim) => binds.push(SelectArmBind::Payload {
                    id: *id,
                    idx,
                    kind: LocalKind::Scalar(prim),
                    on,
                }),
                None => {
                    let kind = owned_kind(elt).ok_or_else(|| {
                        anyhow!("emit_clif: variant payload shape {elt:?} not lowerable")
                    })?;
                    binds.push(SelectArmBind::Payload {
                        id: *id,
                        idx,
                        kind: kind.into(),
                        on,
                    })
                }
            },
            StructPatternNode::Ignore => {}
            sub @ StructPatternNode::Literal(_) => {
                let idx_c = cx.b.ins().iconst(types::I64, idx as i64);
                let call = cx.call_helper(
                    "graphix_variant_payload_borrowed",
                    &[v.0, v.1, idx_c],
                )?;
                let eq = emit_value_leaf_cond(cx, sub, elt, (call[0], call[1]))?;
                cond = cx.b.ins().band(cond, eq);
            }
            sub @ StructPatternNode::Or { .. } if !binds_any(sub) => {
                let idx_c = cx.b.ins().iconst(types::I64, idx as i64);
                let call = cx.call_helper(
                    "graphix_variant_payload_borrowed",
                    &[v.0, v.1, idx_c],
                )?;
                let eq = emit_value_leaf_cond(cx, sub, elt, (call[0], call[1]))?;
                cond = cx.b.ins().band(cond, eq);
            }
            StructPatternNode::Variant { tag, all: None, binds: nbinds } => {
                let idx_c = cx.b.ins().iconst(types::I64, idx as i64);
                let call = cx.call_helper(
                    "graphix_variant_payload_borrowed",
                    &[v.0, v.1, idx_c],
                )?;
                let rs = call;
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
/// BindIds, each at its alternatives' common kind, or as a Value where
/// they differ (a shared name's type is the union of theirs). `nomatch`:
/// `Some` = the caller's fail block, jumped with no binds; `None` = route
/// through `done` with matched=0 and tainted placeholders. Where `mask`
/// is false the binds read as bottom too (an arm the chain does not
/// reach). Returns the matched i8 with the builder in `done` and the
/// binds installed; the caller's env mark, taken before this, scopes the
/// arm-exit drops over them.
fn emit_or_chain(
    cx: &mut BodyCx,
    alts: &[StructPatternNode],
    pred_typ: &Type,
    scrut: SelectScrut,
    scrut_typ: &Type,
    nomatch: Option<Block>,
    mask: Option<ClifValue>,
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
    // each matched alternative hands its binds, at its own kinds, to a
    // block of its own, which joins them to the done block's layout
    let mut matched: SmallVec<[(Block, SmallVec<[(BindId, LocalKind); 8]>); 4]> =
        SmallVec::new();
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
        let mut ids: SmallVec<[BindId; 8]> =
            binds.iter().map(SelectArmBind::id).collect();
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
    // XCR claude for claude: [perf/medium] An or-arm whose shared name binds at
    // a different kind per alternative de-fuses the whole select. `` `A(x) |
    // `B(x) => "[x]" `` over `A(i64)`/`B(f64)`, or `A(i64)`/`B(string)`, errs
    // here because the layout's kinds come from alternative 0. The shared
    // name's type is the union of the alternatives' types
    // (design/or_patterns.md), and that union has a Value form. Fix: give each
    // shared bind its alternatives' common kind, or LocalKind::Value when they
    // differ. Each alternative then widens its narrower local to Value words
    // before jumping to done, as emit_owned_value_operand_node widens. Update
    // the or_patterns.md Engines text, which documents this Err. probe:
    // design/review-2026-10-19/repro/f-select-or-kind-defuse.gx (f-select-03)
    // 2026-10-19 claude: emit_or_chain hands each matched alternative's binds,
    // at its own kinds, to a block of its own; the done block's layout gives
    // each id its alternatives' common kind or LocalKind::Value where they
    // differ, and an alternative's block widens a scalar to Value words (a
    // composite's and a string's words already are a Value's).
    // design/or_patterns.md's Engines text says so. Pin: fusion_review
    // select_leaves_options_or_kinds_and_own_type_tests (w0, w1).
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
