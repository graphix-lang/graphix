//! `select` emission: scrutinee classification, pattern
//! conditions, arm dispatch with per-slot selection memory, and
//! the merge-shape protocol.

use crate::{
    BindId, Node, NodeView, Rt, Update, UserEvent,
    expr::ExprId,
    fusion::{
        self,
        kernel_abi::{self, AbiKind, PrimType},
    },
    node::{Cached, op::CmpOp, pattern::StructPatternNode, select::Select},
    typ::Type,
};
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use cranelift_codegen::ir::{
    Block, BlockArg, InstBuilder, MemFlags, Value as ClifValue, condcodes::IntCC, types,
};
use cranelift_frontend::Variable;
use netidx_value::Value;
use poolshark::local::LPooled;

use super::{
    abi::{
        CompiledExpr, LocalKind, STALE, TAINT, ValueVar, bind_local, clean_disc,
        const_stale_gate, is_untainted, prim_to_value_disc, propagate_flags,
        propagate_stale, propagate_taint, scalar_disc, value_disc,
    },
    body::{
        BodyCx, ensure_owned_composite_src, ensure_owned_value_src, node_composite_source,
    },
    call::CompositeSource,
    lower::{SelWord, resolve_node_typ},
    scalar::{
        cast_u64_to_prim, compile_cmp, compile_const, prim_to_clif,
        scalar_to_payload_i64, struct_get_helper, valarray_get_helper,
        variant_payload_helper, zero_const,
    },
};

/// How a `select`'s arms merge into one result — derived from the
/// select node's frozen result type. Scalar merges thread the disc
/// (carrying `TAINT`/`STALE`) through the arm phi alongside the payload,
/// so a tainted arm value propagates its bottom to the merged result.
/// String/composite merges have no such per-arm channel — a possibly-
/// bottom scrutinee with one of those result shapes refuses to fuse
/// instead.
#[derive(Clone, Copy)]
enum SelectMerge {
    Scalar(PrimType),
    Value,
    Composite,
    String,
}

/// The select scrutinee, emitted exactly ONCE up front; every arm
/// condition and pattern bind reuses these SSA values (SSA reuse
/// gives eval-once for free). `Opaque` (string / composite) supports
/// only Ignore / guard arms, none of which can test the value. The
/// `disc` always carries the scrutinee's #219 taint — OR-ed into every
/// arm's result so a bottom (missing) scrutinee bottoms the select.
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
    /// A BORROWED array/tuple/struct scrutinee: the ValArray bits
    /// stays live across the whole arm chain with no drop (the env slot
    /// owns it — the owned-producer case de-fuses in
    /// [`classify_select_scrutinee`]). Structural patterns
    /// (tuple/struct/slice) test and read elements through it.
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

/// Read one scalar pattern leaf off the borrowed composite scrutinee.
/// Callers MUST have proven the arm's length test first — under a
/// tainted (missing) scrutinee the placeholder is an EMPTY array, and
/// these reads are unchecked.
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

/// A pattern binding to install in the arm's matched region, under the
/// pattern's real `BindId` (the arm body's `Ref`s resolve BindId-first,
/// so no shadow guard is needed).
enum SelectArmBind {
    /// `n => ...` — bind the scalar scrutinee itself.
    Scrut(BindId),
    /// `T as n` over a `[T, null]` scrutinee — bind the matched
    /// non-null scalar payload after the type-predicate branch.
    NullableScalar { id: BindId, prim: PrimType },
    /// `` `Tag(n) `` — bind one scalar variant payload. The read uses
    /// `unreachable_unchecked` on a wrong-tag value, so it MUST be
    /// emitted inside the matched region (after the tag-eq branch) —
    /// never in the fall-through chain. (The node-walk evaluates binds
    /// only after the pattern matches — we follow the node-walk.)
    Payload { id: BindId, idx: usize, prim: PrimType },
    /// `(x, y)` / `{f, ..}` / `[h, ..]` — bind one scalar leaf of a
    /// composite scrutinee. `ptr` is the (borrowed) composite the leaf
    /// reads from: the scrutinee itself, or — for a NESTED pattern — a
    /// borrowed interior pointer read during the arm's structure
    /// condition. Emitted inside the matched region: the length tests
    /// (the structure condition stages) are the memory-safety gate — a
    /// tainted scrutinee's empty placeholder fails them, so the
    /// unchecked element read never touches the placeholder.
    Elem { id: BindId, idx: ElemIdx, prim: PrimType, ptr: ClifValue },
}

/// `select` at expression position — the Node twin of
/// `emit_select_as_expr` (lowering) + `compile_ifchain` (codegen)
/// fused into one pass. Canonical semantics are `Select::update` /
/// `PatternNode::is_match` (node/select.rs, node/pattern.rs):
///
/// - the scrutinee is evaluated once; no scrutinee value → no select
///   value (the scrutinee's disc `TAINT`/`STALE` folds into every arm's
///   result disc — the #178 scrutinee gate);
/// - an explicit type predicate is TESTED (`null as _` → IsNull;
///   `i64 as _` over `[i64, null]` → NOT-null), so arm order is right
///   by construction;
/// - a guard runs only after the pattern matches, with the pattern's
///   binds in scope; a bottom guard means the arm does NOT match;
/// - the first matching arm wins; an arm with no condition and no
///   guard takes the chain unconditionally.
///
/// The final-arm miss trap mirrors `compile_ifchain`, but is emitted
/// only where typecheck's exhaustiveness makes it unreachable: a
/// guarded final arm, or a conditional final arm under a possibly-
/// bottom scrutinee (whose garbage cond bits could miss every arm),
/// refuse to fuse instead.
/// Collect the per-slot STATE sites in a scaffold-loop body: the
/// `Select::spec.id` of every guarded select (selection memory) and
/// the callsite `ExprId` of every nested collection HOF call (a
/// per-slot PREV-LENGTH word for its loop's exact firing rule —
/// jul16a fuzz class A: the conservative fallback re-fired a ragged
/// nested loop on every source refresh). The walk sees exactly the
/// tree the loop will emit inline (a nested collection HOF's callback
/// body lives behind its own lambda def, unreachable from here — its
/// own sites anchor in the chain its loop opens). The loop emitters
/// claim one per-slot state chain per site (see
/// [`BodyCx::open_slot_tables`]).
pub(crate) fn slot_state_sites<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
) -> LPooled<Vec<ExprId>> {
    let mut ids: LPooled<Vec<ExprId>> = LPooled::take();
    fusion::for_each_node(node, &mut |n| match n.view() {
        NodeView::Select(s) => {
            if s.arms.iter().any(|(pat, _)| pat.guard.is_some()) {
                ids.push(s.spec.id);
            }
        }
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

pub(crate) fn emit_select_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    sel: &Select<R, E>,
) -> Result<CompiledExpr> {
    if sel.arms.is_empty() {
        return Err(anyhow!("emit_clif: select with no arms"));
    }
    let result_typ = kernel_abi::freeze_for_abi_normalized(cx.registry(), sel.typ())
        .ok_or_else(|| {
            anyhow!(
                "emit_clif: select result type {:?} doesn't freeze concrete",
                sel.typ()
            )
        })?;
    let merge_shape = match kernel_abi::abi_kind(cx.registry(), &result_typ) {
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
    // Every merge shape phis (disc, payload) — the scrutinee's taint
    // rides the disc into every arm result, so there's no separate
    // validity phi and no possibly-bottom-scrutinee gate (#219).
    let merge = cx.b.create_block();
    cx.b.append_block_param(merge, types::I64); // disc
    let payload_ty = match merge_shape {
        SelectMerge::Scalar(p) => prim_to_clif(p),
        _ => types::I64,
    };
    cx.b.append_block_param(merge, payload_ty); // payload
    let scrut_disc = scrut.disc();
    // A select FIRES iff its scrutinee OR the taken arm OR a GUARD
    // fired. Guards are evaluated inside the pattern chain (only on the
    // paths that reach them), so "a guard fired" is computed HERE, path-
    // independently, from the guards' FEEDERS: AND-reduce the STALE bit
    // of every kernel-visible local any arm's guard reads — the word is
    // STALE only if no guard input fired. (The node-walk re-matches on
    // any pattern/guard update, so any guard feeder firing can re-fire
    // the select.) This replaces the old force-the-result-FRESH
    // treatment of guarded selects, which fired the select on EVERY
    // kernel invocation — a `count` over a guarded select with an
    // unrelated reactive input in the region counted every event
    // (interp 1, jit 5).
    //
    // The guard term is further refined by SELECTION MEMORY when a
    // per-instance state word is available (root body, outside loops —
    // see `BodyCx::claim_state_word`): the node-walk emits on a
    // guard-only event only when the SELECTION changes (verified on
    // the node-walk 2026-07-03: guard feeder fires, same arm re-taken,
    // const body → NO emission; every selection change → emission), so
    // each taken arm compares-and-records its index and the guard term
    // fires only on a change (fuzz/triage-fuzzer-v2/firing_000005).
    // Inside a scaffold loop the select is one PER SLOT in the
    // node-walk (each MapQ/FoldQ slot's subgraph has its own Select
    // instance), so the static claim is refused and the word comes
    // from the loop's per-slot side table instead — `table + i*8`
    // (soak-jul14b fuzz divergence 000009: a guard-only event with an
    // unchanged selection re-fired every slot). Without either
    // (callee body, nested loop) the unrefined guard term stands — a
    // deliberate residual duplicate-fire, never a wrong value.
    // Guard FEEDERS: the kernel-visible locals any arm's guard reads.
    // Arm binds aren't in the env (the chain binds them per arm) —
    // their firing is the scrutinee's, already folded — so a select
    // whose guards read ONLY pattern binds (`` `Num(x) if x == 0.0 ``,
    // the symbolic-simplify shape) has an EMPTY feeder set: its guard
    // term would fold over nothing (a constant) and its selection can
    // only change when the scrutinee fires, which already fires the
    // select. Such a select claims NO selection memory and emits no
    // guard term — the guardless treatment; claiming anyway cost the
    // recursion-hot symbolic bench ~14% in null-guard branches for
    // memory that could never refine anything.
    let guard_feeders: Vec<Variable> = {
        let mut ids: Vec<BindId> = Vec::new();
        for (pat, _) in sel.arms.iter() {
            if let Some(g) = &pat.guard {
                let mut refs = crate::Refs::default();
                g.node.refs(&mut refs);
                refs.with_external_refs(|id| {
                    if !ids.contains(&id) {
                        ids.push(id);
                    }
                });
            }
        }
        ids.iter().filter_map(|id| cx.env.lookup_by_id(*id).map(|l| l.vv.disc)).collect()
    };
    let has_guard = !guard_feeders.is_empty();
    let guard_stale: Option<ClifValue> = if has_guard {
        let mut word = cx.b.ins().iconst(types::I64, STALE);
        for dv in guard_feeders {
            let d = cx.b.use_var(dv);
            let sb = cx.b.ins().band_imm(d, STALE);
            word = cx.b.ins().band(word, sb);
        }
        Some(word)
    } else {
        None
    };
    // An arm body holding a LIFTED connect target (`let s = 0; s <-
    // …; s` — the per-arm reactive accumulator) needs the state word
    // unconditionally: the node-walk RE-SEEDS the bind on every arm
    // wake (the init-view contract above), which the arm emitter
    // reproduces from the selection-changed bit. Without a word
    // (loop/callee context) that semantics is unrepresentable — Err,
    // de-fuse (soak jul08g fuzz divergence 6).
    let has_arm_lift = sel.arms.iter().any(|(_, body)| {
        let mut found = false;
        fusion::for_each_node(&body.node, &mut |n| {
            if let NodeView::Bind(b) = n.view() {
                if b.single_bind_id().is_some_and(|id| cx.ctx.lifted.contains(&id)) {
                    found = true;
                }
            }
        });
        found
    });
    // The word's ADDRESS: static (state_ptr + off) at root level,
    // this slot's entry in the loop's side table, or — in a CALLEE
    // body at its root level — a word in the per-call-site state
    // block (null-guarded: 0 on recursive back-edges). The arm-lift
    // re-seed never reads a table or a site block — the lifted write
    // target is per INSTANCE (one state word), so neither per-slot
    // nor per-call-site memory can represent it.
    let sel_state = if has_guard || has_arm_lift {
        match cx.claim_state_word() {
            Some(off) => {
                let sp = cx.state_ptr();
                Some(SelWord::Sure(cx.b.ins().iadd_imm(sp, off as i64)))
            }
            None if !has_arm_lift => match cx.slot_select_word(sel.spec.id) {
                Some(w) => Some(w),
                // Site words are per CALL SITE, not per slot: a
                // loop-context select without a table entry must NOT
                // claim one (it would alias slots) — fall back to the
                // unrefined guard term instead.
                None if cx.ctx.loop_depth.get() == 0 => cx.claim_site_word().map(|off| {
                    let base = cx.site_ptr();
                    let addr = cx.b.ins().iadd_imm(base, off as i64);
                    SelWord::Guarded { base, addr }
                }),
                None => None,
            },
            None => None,
        }
    } else {
        None
    };
    if has_arm_lift && sel_state.is_none() {
        return Err(anyhow!(
            "emit_clif: select arm holds a lifted connect target but no \
             per-instance state word is available here — the arm-wake \
             re-seed can't be reproduced"
        ));
    }
    let arm_index = std::cell::Cell::new(0usize);
    emit_select_arms(
        cx,
        sel,
        scrut,
        scrut_kind,
        &scrut_typ,
        &mut |cx, body, mark| {
            let idx = arm_index.get();
            arm_index.set(idx + 1);
            emit_select_value_arm(
                cx,
                body,
                mark,
                merge_shape,
                merge,
                scrut_disc,
                guard_stale,
                sel_state.map(|w| (w, idx)),
            )
        },
        &mut |cx| emit_select_miss_value(cx, merge_shape, merge),
    )?;
    cx.b.switch_to_block(merge);
    cx.b.seal_block(merge);
    let (rdisc, rpayload) = {
        let params = cx.b.block_params(merge);
        (params[0], params[1])
    };
    // Discharge an OWNED scrutinee: every normal path (each arm + the
    // miss trap) crosses the merge, so this drops exactly once; a
    // mid-arm pending exit dropped it as an env local instead. Unbind
    // (truncate) so a LATER pending exit elsewhere in the kernel can't
    // double-drop the already-freed value.
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

/// The final-arm fail block of a VALUE-position select: reached only
/// when a tainted (missing) scrutinee misses every conditional arm.
/// Jump to the merge with a drop-safe tainted bottom — the disc's TAINT
/// forces a bottom at the output, and the payload is a valid empty
/// allocation so a scope-exit drop is null-safe.
fn emit_select_miss_value(
    cx: &mut BodyCx,
    merge_shape: SelectMerge,
    merge: Block,
) -> Result<()> {
    let (disc, payload) = match merge_shape {
        SelectMerge::Scalar(p) => {
            let d = cx.b.ins().iconst(types::I64, prim_to_value_disc(p) | TAINT);
            // `zero_const`, NOT `iconst(prim_to_clif(p))`: an `iconst.f64`
            // is invalid CLIF (a verifier panic) — a float-result select
            // with a conditional final arm reaches this trap.
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
    cx.b.ins().jump(merge, &[BlockArg::Value(disc), BlockArg::Value(payload)]);
    Ok(())
}

/// What the value-position select owes at its merge point for an OWNED
/// (fresh-producer) scrutinee: the scrutinee was bound as an env local
/// (so a mid-arm pending exit drops it via `drop_owned_composites`), and
/// the merge emits the normal-path drop then unbinds it (the env mark) —
/// exactly once on either path.
pub(super) struct ScrutDrop {
    kind: LocalKind,
    vv: ValueVar,
    mark: usize,
}

/// Classify (and emit the read of) a select scrutinee: the shared
/// prologue of the value-position and tail-position select emitters.
/// `allow_owned`: the value-position caller has a single merge point
/// every path crosses, so it can accept an OWNED composite/Value
/// scrutinee and discharge the returned [`ScrutDrop`] there; the
/// tail-position caller's arms terminate individually (no merge), so it
/// passes `false` and owned scrutinees keep de-fusing.
pub(super) fn classify_select_scrutinee<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    sel: &Select<R, E>,
    allow_owned: bool,
) -> Result<(SelectScrut, AbiKind, Type, Option<ScrutDrop>)> {
    let scrut_typ =
        kernel_abi::freeze_for_abi_normalized(cx.registry(), sel.arg.node.typ())
            .ok_or_else(|| {
                anyhow!(
                    "emit_clif: select scrutinee type {:?} doesn't freeze \
                 concrete",
                    sel.arg.node.typ()
                )
            })?;
    let scrut_kind = kernel_abi::abi_kind(cx.registry(), &scrut_typ)
        .ok_or_else(|| anyhow!("emit_clif: select scrutinee shape not classifiable"))?;
    let mut drop_ob: Option<ScrutDrop> = None;
    // Bind an OWNED scrutinee as an env local of its kind: a mid-arm
    // pending exit drops it via `drop_owned_composites`, and the caller
    // discharges the ScrutDrop (drop + unbind) at the merge on the
    // normal path — exactly once on either path.
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
            // The (disc, payload) pair stays live across the whole arm
            // chain: a BORROWED env slot needs nothing; an OWNED
            // producer needs the merge-point drop (value position only).
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
        // A composite scrutinee keeps its pointer live across the arm
        // chain: borrowed = env-owned; owned = merge-point drop.
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
        // A String scrutinee supports only Ignore / guard arms (no
        // condition can test it); we read it only for its disc (#219
        // taint). The read is an owned ArcStr either way (a borrowed
        // slot read CLONES, an owned producer transfers) — drop it
        // immediately, keeping only the disc. No cross-arm retention,
        // so owned strings are fine in both positions.
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
/// pattern over a BORROWED composite scrutinee. Mirrors
/// `StructPatternNode::is_match` / `bind` (node/pattern.rs — the
/// canonical semantics):
///
/// - Slice (tuple or array literal pattern): `len == N`, leaves at
///   `a[j]`;
/// - SlicePrefix: `len >= N`, leaves at `a[j]`;
/// - SliceSuffix: `len >= N`, leaves at `a[len - (N - j)]`;
/// - Struct: `len >= N`, leaf values at `a[i][1]` (canonically-sorted
///   field index from typecheck).
///
/// The LENGTH test is also the taint gate: a missing (#219) composite
/// input is an EMPTY placeholder array, so every length test with
/// `N > 0` fails under taint and the unchecked element reads (emitted
/// after the test) never touch the placeholder — the chain falls
/// through to the final-arm miss trap, which produces the tainted
/// bottom. Literal leaves are tested in a second block AFTER the
/// length branch (same reason); `Bind` leaves are recorded in `binds`
/// and read in the matched region.
///
/// Deferred (Err → the select de-fuses, node-walks): whole-composite
/// `@` bindings, named prefix/suffix rest bindings (both allocate an
/// owned composite local inside the arm — `JitEnv::truncate` emits no
/// drops, so they'd leak on the normal path), non-scalar leaves, and
/// nested structural leaves.
fn emit_composite_pattern_cond(
    cx: &mut BodyCx,
    ptr: ClifValue,
    scrut_typ: &Type,
    pat: &StructPatternNode,
    fail: Block,
    binds: &mut Vec<SelectArmBind>,
) -> Result<ClifValue> {
    // The length read up front (safe on the empty taint placeholder) —
    // suffix leaves index relative to it.
    let len_helper = cx.helper("graphix_valarray_len")?;
    let call = cx.b.ins().call(len_helper, &[ptr]);
    let len = cx.b.inst_results(call)[0];
    // (leaf position, sub-pattern, element type) + the length compare.
    let styp = resolve_node_typ(cx.ctx, scrut_typ);
    struct LeafSpec<'p> {
        idx: ElemIdx,
        sub: &'p StructPatternNode,
        typ: Type,
    }
    let (leaves, len_cc, n): (Vec<LeafSpec>, IntCC, usize) = match pat {
        StructPatternNode::Slice { tuple, all, binds: pbinds } => {
            if all.is_some() {
                return Err(anyhow!(
                    "emit_clif: whole-slice @ binding not lowerable (owned \
                     composite arm local)"
                ));
            }
            let elt = |j: usize| -> Result<Type> {
                if *tuple {
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
                .collect::<Result<Vec<_>>>()?;
            (leaves, IntCC::Equal, pbinds.len())
        }
        StructPatternNode::SlicePrefix { all, prefix, tail } => {
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
            // suffix leaf j lives at a[len - (N - j)].
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
                .collect::<Result<Vec<_>>>()?;
            (leaves, IntCC::SignedGreaterThanOrEqual, sbinds.len())
        }
        _ => return Err(anyhow!("emit_clif: not a composite structural pattern")),
    };
    // Classify each leaf BEFORE emitting anything (an Err mid-emission
    // would abandon the kernel build — fine — but classify-first keeps
    // the failure cheap and the emission below straight-line).
    let mut lit_leaves: Vec<(ElemIdx, PrimType, &Value)> = Vec::new();
    let mut nested: Vec<(ElemIdx, &StructPatternNode, Type)> = Vec::new();
    for leaf in &leaves {
        match leaf.sub {
            StructPatternNode::Ignore => {}
            StructPatternNode::Bind(id) => {
                let prim = kernel_abi::scalar_prim(cx.registry(), &leaf.typ).ok_or_else(
                    || {
                        anyhow!(
                            "emit_clif: non-scalar select pattern leaf bind {:?}",
                            leaf.typ
                        )
                    },
                )?;
                binds.push(SelectArmBind::Elem { id: *id, idx: leaf.idx, prim, ptr });
            }
            StructPatternNode::Literal(v) => {
                let prim = kernel_abi::scalar_prim_of_value(v).ok_or_else(|| {
                    anyhow!("emit_clif: non-scalar literal pattern leaf {v:?}")
                })?;
                lit_leaves.push((leaf.idx, prim, v));
            }
            sub @ (StructPatternNode::Slice { .. }
            | StructPatternNode::SlicePrefix { .. }
            | StructPatternNode::SliceSuffix { .. }
            | StructPatternNode::Struct { .. }) => {
                // A NESTED structural pattern over a composite-shaped
                // leaf recurses through a BORROWED interior pointer —
                // read after this level's length test (below).
                match kernel_abi::abi_kind(cx.registry(), &leaf.typ) {
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
        }
    }
    // The length test for THIS level.
    let n_c = cx.b.ins().iconst(types::I64, n as i64);
    let len_ok = cx.b.ins().icmp(len_cc, len, n_c);
    if lit_leaves.is_empty() && nested.is_empty() {
        // Nothing to read before the matched region — the length test
        // IS the condition (the caller's arm brif consumes it).
        return Ok(len_ok);
    }
    // Reads happen below, so the length must be proven FIRST: branch to
    // a staging block (the reads are unchecked — see the taint note
    // above), then test literal leaves and recurse into nested patterns.
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
        // Borrowed interior pointer into this level's element slot —
        // stable for the whole arm chain (the root scrutinee is a
        // pinned borrowed env slot and values are immutable), so the
        // recursion's reads and the matched-region leaf binds need no
        // ownership or drops.
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

/// The shared select arm chain: pattern conditions (type predicate /
/// structure / guard), per-arm binds, and the fail-block plumbing —
/// identical between value position (arms widen and jump to a merge
/// block) and tail position (arms terminate with a return or a self
/// tail-call jump). `emit_arm` supplies the position-specific arm-body
/// emission; it runs in the matched block with the arm's binds
/// installed and MUST leave the block terminated (jump or return).
/// `mark` is the env state to truncate back to after the body.
pub(super) fn emit_select_arms<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    sel: &Select<R, E>,
    scrut: SelectScrut,
    scrut_kind: AbiKind,
    scrut_typ: &Type,
    emit_arm: &mut dyn FnMut(&mut BodyCx, &Cached<R, E>, usize) -> Result<()>,
    // The final-arm miss handler (reached only under a tainted
    // scrutinee): value position jumps to the merge with a tainted
    // bottom; tail position sets pending and exits.
    emit_miss: &mut dyn FnMut(&mut BodyCx) -> Result<()>,
) -> Result<()> {
    use StructPatternNode;
    let n = sel.arms.len();
    for (i, (pat, body)) in sel.arms.iter().enumerate() {
        let is_last = i == n - 1;
        // Type-predicate condition. The node-walk tests the predicate
        // only when it's explicit (`PatternNode::is_match`); an
        // inferred predicate imposes no runtime test.
        let tcond: Option<ClifValue> = if !pat.explicit_type_predicate {
            None
        } else {
            let pred = kernel_abi::freeze_for_abi(cx.registry(), &pat.type_predicate)
                .ok_or_else(|| {
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
                            // Mask taint before the structural compare —
                            // a tainted disc is not a clean tag (#219).
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
                                && kernel_abi::nullable_inner(
                                    cx.registry(),
                                    &scrut_typ,
                                )
                                .as_ref()
                                .and_then(|t| {
                                    kernel_abi::scalar_prim(cx.registry(), t)
                                }) == PrimType::from_typ(pt) =>
                        {
                            // `[T, null]` runtime value is T or null,
                            // so "is a T" ≡ "is not null" — tested,
                            // not assumed (order-sound). Mask taint
                            // before the structural compare (#219).
                            let cd = clean_disc(cx.b, disc);
                            Some(cx.b.ins().icmp_imm(
                                IntCC::NotEqual,
                                cd,
                                value_disc::NULL,
                            ))
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
                    return Err(anyhow!(
                        "emit_clif: type predicate {pred:?} not lowerable"
                    ));
                }
            }
        };
        // A composite structural pattern (tuple/struct/slice) stages its
        // condition across blocks (length branch, then literal-leaf
        // tests), so its fail edge must exist BEFORE the condition is
        // emitted — pre-create it (block creation order is free).
        let composite_structural = matches!(
            &pat.structure_predicate,
            StructPatternNode::Slice { .. }
                | StructPatternNode::SlicePrefix { .. }
                | StructPatternNode::SliceSuffix { .. }
                | StructPatternNode::Struct { .. }
        ) && matches!(scrut, SelectScrut::Composite { .. });
        let early_fail =
            if composite_structural { Some(cx.b.create_block()) } else { None };
        // Structure condition + the binds to install once matched.
        let mut binds: Vec<SelectArmBind> = Vec::new();
        let scond: Option<ClifValue> = match &pat.structure_predicate {
            StructPatternNode::Ignore => None,
            StructPatternNode::Bind(id) => match scrut {
                SelectScrut::Scalar { .. } => {
                    binds.push(SelectArmBind::Scrut(*id));
                    None
                }
                SelectScrut::Value { .. } if matches!(scrut_kind, AbiKind::Nullable) => {
                    let pred =
                        kernel_abi::freeze_for_abi(cx.registry(), &pat.type_predicate);
                    let Some(prim) = pred
                        .as_ref()
                        .and_then(|typ| kernel_abi::scalar_prim(cx.registry(), typ))
                    else {
                        return Err(anyhow!(
                            "emit_clif: nullable scrutinee bind predicate is not scalar"
                        ));
                    };
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
                let lit_prim = kernel_abi::scalar_prim_of_value(v).ok_or_else(|| {
                    anyhow!("emit_clif: non-scalar literal pattern {v:?}")
                })?;
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
                    return Err(anyhow!(
                        "emit_clif: whole-variant @ binding not lowerable"
                    ));
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
                // Payload types come from the arm's own (frozen)
                // type predicate — `Variant(tag, elts)` for exactly
                // this arm.
                let pred = kernel_abi::freeze_for_abi(cx.registry(), &pat.type_predicate)
                    .ok_or_else(|| {
                        anyhow!(
                            "emit_clif: variant pattern predicate {:?} \
                             doesn't freeze concrete",
                            pat.type_predicate
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
                        StructPatternNode::Bind(id) => {
                            let prim = kernel_abi::scalar_prim(cx.registry(), elt)
                                .ok_or_else(|| {
                                    anyhow!(
                                        "emit_clif: non-scalar variant \
                                         payload {elt:?}"
                                    )
                                })?;
                            binds.push(SelectArmBind::Payload { id: *id, idx, prim });
                        }
                        StructPatternNode::Ignore => {}
                        StructPatternNode::Literal(_)
                        | StructPatternNode::Slice { .. }
                        | StructPatternNode::SlicePrefix { .. }
                        | StructPatternNode::SliceSuffix { .. }
                        | StructPatternNode::Struct { .. }
                        | StructPatternNode::Variant { .. } => {
                            return Err(anyhow!(
                                "emit_clif: nested variant payload \
                                 pattern not lowerable"
                            ));
                        }
                    }
                }
                let tag_ptr = cx.interned_str(tag);
                let helper = cx.helper("graphix_variant_tag_eq")?;
                // Mask taint — the helper reads the disc as a clean tag.
                let call = cx.b.ins().call(helper, &[disc, payload, tag_ptr]);
                Some(cx.b.inst_results(call)[0])
            }
            p @ (StructPatternNode::Slice { .. }
            | StructPatternNode::SlicePrefix { .. }
            | StructPatternNode::SliceSuffix { .. }
            | StructPatternNode::Struct { .. }) => match scrut {
                SelectScrut::Composite { ptr, .. } => {
                    if tcond.is_some() {
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
                        &mut binds,
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
        let pcond = match (tcond, scond) {
            (None, None) => None,
            (Some(c), None) | (None, Some(c)) => Some(c),
            (Some(a), Some(b)) => Some(cx.b.ins().band(a, b)),
        };
        let has_guard = pat.guard.is_some();
        // The final-arm miss trap below is sound only when typecheck's
        // exhaustiveness makes a miss impossible. A guarded final arm
        // (typecheck forbids it today — defensive) or garbage cond
        // bits from a possibly-bottom scrutinee could miss every arm.
        if is_last && has_guard {
            return Err(anyhow!(
                "emit_clif: guard on the final select arm — the chain \
                 could miss every arm"
            ));
        }
        // #219: a conditional final arm CAN miss every arm under a
        // tainted (missing) scrutinee. That's no longer a refusal — the
        // final fail block runs `emit_miss` (a tainted bottom), which is
        // dead code for an exhaustive non-tainted scrutinee.
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
        let mark = cx.env.mark();
        for bind in &binds {
            match bind {
                SelectArmBind::Scrut(id) => {
                    let SelectScrut::Scalar { disc, value, prim } = scrut else {
                        return Err(anyhow!(
                            "emit_clif: scrutinee bind without a scalar \
                             scrutinee"
                        ));
                    };
                    let name: ArcStr =
                        compact_str::format_compact!("__pat{}", id.inner())
                            .as_str()
                            .into();
                    // The bound local carries the scrutinee's taint in its
                    // disc (#219).
                    bind_local(cx, name, disc, value, LocalKind::Scalar(prim), Some(*id));
                }
                SelectArmBind::NullableScalar { id, prim } => {
                    let SelectScrut::Value { disc, payload } = scrut else {
                        return Err(anyhow!(
                            "emit_clif: nullable scalar bind without a value scrutinee"
                        ));
                    };
                    let name: ArcStr =
                        compact_str::format_compact!("__pat{}", id.inner())
                            .as_str()
                            .into();
                    let value = cast_u64_to_prim(cx.b, payload, *prim);
                    let base = scalar_disc(cx.b, *prim);
                    let bound_disc = propagate_flags(cx.b, base, &[disc]);
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
                    // Clean the scrutinee disc for the payload read; the
                    // payload inherits the variant's taint.
                    let call = cx.b.ins().call(helper, &[disc, payload, idx_c]);
                    let v = cx.b.inst_results(call)[0];
                    let name: ArcStr =
                        compact_str::format_compact!("__pat{}", id.inner())
                            .as_str()
                            .into();
                    // The bound payload fires iff its variant scrutinee
                    // fired — inherit the scrutinee's STALE (and taint), so
                    // an arm body reading it stays faithful.
                    let base = scalar_disc(cx.b, *prim);
                    let pdisc = propagate_flags(cx.b, base, &[disc]);
                    bind_local(cx, name, pdisc, v, LocalKind::Scalar(*prim), Some(*id));
                }
                SelectArmBind::Elem { id, idx, prim, ptr } => {
                    let SelectScrut::Composite { disc, .. } = scrut else {
                        return Err(anyhow!(
                            "emit_clif: element bind without a composite \
                             scrutinee"
                        ));
                    };
                    // Safe here: the arm's length tests (the structure
                    // condition stages guarding this matched region) proved
                    // the element exists — a tainted scrutinee's empty
                    // placeholder failed them.
                    let v = read_scrut_elem(cx, *ptr, *idx, *prim)?;
                    let name: ArcStr =
                        compact_str::format_compact!("__pat{}", id.inner())
                            .as_str()
                            .into();
                    // The bound leaf fires iff its composite scrutinee
                    // fired — inherit the scrutinee's STALE (and taint).
                    let base = scalar_disc(cx.b, *prim);
                    let pdisc = propagate_flags(cx.b, base, &[disc]);
                    bind_local(cx, name, pdisc, v, LocalKind::Scalar(*prim), Some(*id));
                }
            }
        }
        if let Some(g) = &pat.guard {
            // Canonical guard semantics: evaluated only after the
            // pattern matched, with the binds in scope; a bottom guard
            // (`valid` = 0) means the arm does NOT match.
            let gcv = g.node.emit_clif(cx)?;
            // A bottom guard (tainted disc) means the arm does NOT match.
            let valid = is_untainted(cx.b, gcv.disc);
            let eff = cx.b.ins().band(gcv.payload, valid);
            let body_blk = cx.b.create_block();
            cx.b.ins().brif(eff, body_blk, &[], fail.unwrap(), &[]);
            cx.b.switch_to_block(body_blk);
            cx.b.seal_block(body_blk);
        }
        emit_arm(cx, body, mark)?;
        match fail {
            Some(f) => {
                cx.b.switch_to_block(f);
                cx.b.seal_block(f);
                if is_last {
                    // Reached only under a tainted (missing) scrutinee —
                    // produce a tainted bottom (#219). Dead code for an
                    // exhaustive non-tainted scrutinee.
                    emit_miss(cx)?;
                }
            }
            // An unconditional arm consumed control flow; any
            // remaining arms are unreachable (typecheck's dead-arm
            // check forbids them anyway). Mirrors `compile_ifchain`.
            None => break,
        }
    }
    Ok(())
}

/// Value-position arm-body emission: widen the arm's result to the
/// select's merge shape and jump to the merge block. Extracted
/// verbatim from the pre-F0b `emit_select_node` arm loop.
fn emit_select_value_arm<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    body: &Cached<R, E>,
    mark: usize,
    merge_shape: SelectMerge,
    merge: Block,
    scrut_disc: ClifValue,
    guard_stale: Option<ClifValue>,
    sel_state: Option<(SelWord, usize)>,
) -> Result<()> {
    use NodeView;
    let body_frozen =
        kernel_abi::freeze_for_abi_normalized(cx.registry(), body.node.typ())
            .ok_or_else(|| {
                anyhow!(
                    "emit_clif: select arm type {:?} doesn't freeze concrete",
                    body.node.typ()
                )
            })?;
    // Selection memory (see `emit_select_node`): compare-and-record
    // this arm's index BEFORE the body — the changed bit both refines
    // the guard firing term (below) and drives the arm's EFFECTIVE
    // INIT. The node-walk updates a newly-taken arm with `event.init =
    // true` (node/select.rs — the wake is an init view), so the body
    // is emitted under `kernel_init | (changed & valid)`: seeds
    // re-fire, constants re-deliver, lifted connect targets re-seed
    // (soak jul08g fuzz divergence 6). Recording (and the init view)
    // is skipped for a TAINTED scrutinee: the arm is taken
    // structurally but the node-walk made no selection.
    let base_init = cx.init_flag();
    let record = |cx: &mut BodyCx, addr: ClifValue, idx: usize| {
        let stored = cx.b.ins().load(types::I64, MemFlags::trusted(), addr, 0);
        let tag = cx.b.ins().iconst(types::I64, idx as i64 + 1);
        let changed = cx.b.ins().icmp(IntCC::NotEqual, stored, tag);
        let valid = is_untainted(cx.b, scrut_disc);
        let recorded = cx.b.ins().select(valid, tag, stored);
        cx.b.ins().store(MemFlags::trusted(), recorded, addr, 0);
        let woke = cx.b.ins().band(changed, valid);
        let woke = cx.b.ins().uextend(types::I64, woke);
        let eff_init = cx.b.ins().bor(base_init, woke);
        (changed, eff_init)
    };
    let sel_changed = sel_state.map(|(word, idx)| match word {
        SelWord::Sure(addr) => record(cx, addr, idx),
        // A site-block word: 0 base = a recursive back-edge's fresh
        // transient activation — degrade to the no-memory semantics
        // (unrefined guard term: changed, no forced arm init).
        SelWord::Guarded { base, addr } => {
            let has = cx.b.ins().icmp_imm(IntCC::NotEqual, base, 0);
            let mem_bl = cx.b.create_block();
            let nomem_bl = cx.b.create_block();
            let merge = cx.b.create_block();
            cx.b.append_block_param(merge, types::I8); // changed
            cx.b.append_block_param(merge, types::I64); // eff_init
            cx.b.ins().brif(has, mem_bl, &[], nomem_bl, &[]);
            cx.b.switch_to_block(mem_bl);
            cx.b.seal_block(mem_bl);
            let (changed, eff_init) = record(cx, addr, idx);
            cx.b.ins()
                .jump(merge, &[BlockArg::Value(changed), BlockArg::Value(eff_init)]);
            cx.b.switch_to_block(nomem_bl);
            cx.b.seal_block(nomem_bl);
            let t = cx.b.ins().iconst(types::I8, 1);
            cx.b.ins().jump(merge, &[BlockArg::Value(t), BlockArg::Value(base_init)]);
            cx.b.switch_to_block(merge);
            cx.b.seal_block(merge);
            let params = cx.b.block_params(merge);
            (params[0], params[1])
        }
    });
    let prev_override = match sel_changed {
        Some((_, eff_init)) => cx.ctx.init_override.replace(Some(eff_init)),
        None => cx.ctx.init_override.get(),
    };
    let (disc, payload) = match merge_shape {
        SelectMerge::Scalar(rp) => {
            if kernel_abi::scalar_prim(cx.registry(), &body_frozen) != Some(rp) {
                return Err(anyhow!(
                    "emit_clif: select arm type {body_frozen:?} doesn't \
                     match the scalar merge {rp:?}"
                ));
            }
            let cv = body.node.emit_clif(cx)?;
            (cv.disc, cv.payload)
        }
        SelectMerge::Value => {
            // Node twin of `widen_arm_to_value`, keyed on the arm
            // BODY's frozen type.
            match kernel_abi::abi_kind(cx.registry(), &body_frozen) {
                Some(AbiKind::Null) => {
                    // A bare-null arm body has nothing to emit (and a
                    // Null-shaped node can't emit anyway); only the
                    // literal constant form is recognized.
                    match body.node.view() {
                        NodeView::Constant(c) if matches!(c.value, Value::Null) => {}
                        _ => {
                            return Err(anyhow!(
                                "emit_clif: null-typed select arm isn't \
                                 a null literal"
                            ));
                        }
                    }
                    // Same STALE gate as `emit_const_node`: a literal
                    // fires only at init. A raw (always-FRESH) disc here
                    // made the STALE AND-fold below unable to sleep the
                    // arm, so a guarded select taking a null arm re-fired
                    // on every kernel invocation (soak-jul06c B3).
                    let init = cx.init_flag();
                    let d = cx.b.ins().iconst(types::I64, value_disc::NULL);
                    let d = const_stale_gate(cx.b, init, d);
                    let p = cx.b.ins().iconst(types::I64, 0);
                    (d, p)
                }
                Some(AbiKind::Scalar(p)) => {
                    let cv = body.node.emit_clif(cx)?;
                    (cv.disc, scalar_to_payload_i64(cx.b, p, cv.payload))
                }
                Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
                    let cv = body.node.emit_clif(cx)?;
                    ensure_owned_value_src(
                        cx,
                        node_composite_source(&body.node),
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
                kernel_abi::abi_kind(cx.registry(), &body_frozen),
                Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct)
            ) {
                return Err(anyhow!(
                    "emit_clif: select arm type {body_frozen:?} doesn't \
                     match the composite merge"
                ));
            }
            let cv = body.node.emit_clif(cx)?;
            let v = ensure_owned_composite_src(
                cx,
                node_composite_source(&body.node),
                cv.payload,
            )?;
            (cv.disc, v)
        }
        SelectMerge::String => {
            if !matches!(
                kernel_abi::abi_kind(cx.registry(), &body_frozen),
                Some(AbiKind::String)
            ) {
                return Err(anyhow!(
                    "emit_clif: select arm type {body_frozen:?} doesn't \
                     match the string merge"
                ));
            }
            // String reads/produces are owned at production.
            let cv = body.node.emit_clif(cx)?;
            (cv.disc, cv.payload)
        }
    };
    // Fold the scrutinee's flags into the arm result. A select FIRES iff
    // its scrutinee OR its taken arm (OR a guard) fired. TAINT = OR(arm,
    // scrut): a missing scrutinee bottoms regardless of arm. For STALE,
    // off a cleaned base (so the arm's own STALE is recombined, not
    // blindly kept):
    //  - UNGUARDED: STALE = AND(arm, scrut) — fires iff arm or scrut fired.
    //    A stale-CONSTANT arm (`null => true`) must still fire when the
    //    scrutinee fired, else it wrong-bottoms.
    //  - GUARDED: additionally AND in the guard-feeder stale word
    //    computed before the chain — the select also fires when any
    //    guard input fired (see `emit_select_node`).
    cx.ctx.init_override.set(prev_override);
    let base = clean_disc(cx.b, disc);
    let disc = match guard_stale {
        None => propagate_flags(cx.b, base, &[disc, scrut_disc]),
        Some(gw) => {
            // Selection memory: with a state word, the guard term
            // fires only when the SELECTION changed (compare-recorded
            // above, before the body; stored as idx+1, 0 = no previous
            // selection, so a fresh instance's first selection always
            // reads as changed).
            let gw = match sel_changed {
                None => gw,
                Some((changed, _)) => {
                    let quiet = cx.b.ins().iconst(types::I64, STALE);
                    cx.b.ins().select(changed, gw, quiet)
                }
            };
            let d = propagate_taint(cx.b, base, &[disc, scrut_disc]);
            propagate_stale(cx.b, d, &[disc, scrut_disc, gw])
        }
    };
    cx.env.truncate(mark);
    cx.b.ins().jump(merge, &[BlockArg::Value(disc), BlockArg::Value(payload)]);
    Ok(())
}
