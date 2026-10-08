//! HOF loop scaffolds for the cranelift JIT backend.
//!
//! Each `emit_*_loop` owns the mechanics of one loop shape (length and
//! buf calls, the counter, block creation and sealing, per-iteration
//! element binding and dropping, pending-cleanup registration); the
//! caller supplies the body as a closure over the [`BodyCx`].

use crate::{
    ApplyView, BindId, Node, NodeView, Rt, UserEvent,
    expr::ExprId,
    fusion::{
        self,
        kernel_abi::{self, AbiKind, PrimType},
    },
    tval::TAG_MASK,
    typ::Type,
};
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use cranelift_codegen::ir::{
    Block, BlockArg, InstBuilder, MemFlags, Value as ClifValue, condcodes::IntCC, types,
};
use cranelift_frontend::Variable;
use poolshark::local::LPooled;
use smallvec::SmallVec;

pub(crate) use super::abi::LocalKind;
use super::{
    abi::{
        CompiledExpr, FIRE_TEST, STALE, TAINT, ValueVar, WAKE, bind_local,
        bind_scalar_var_with_disc, clean_disc, is_fresh, prim_to_value_disc, scalar_disc,
        value_disc,
    },
    body::{
        BodyCx, emit_interrupt_check, ensure_owned_composite_src, ensure_owned_value_src,
    },
    call::{CompositeSource, emit_drop_local, finalize_valarray, open_value_buf},
    lower::SelWord,
    outline,
    scalar::{prim_to_clif, scalar_to_payload_i64, valarray_get_helper, widen_to_i64},
};

/// The input array for a HOF loop. The scaffold drops an `Owned` one
/// after the loop; an input bound as an env local is `Borrowed` or it
/// double-drops on the normal path.
pub struct ArraySrc {
    pub ptr: ClifValue,
    /// The source's full disc: its STALE bit is inherited by the bound
    /// elements and its TAINT bit rides into the result.
    pub disc: ClifValue,
    pub ownership: CompositeSource,
}

/// One `|(k, v)|` destructure leaf: its pattern `BindId`, tuple
/// position and local kind (see [`elem_leaves`]).
pub(crate) type Leaf = (BindId, usize, LocalKind);

/// The leaves of one destructure pattern.
pub(crate) type Leaves = SmallVec<[Leaf; 4]>;

/// Loop element binding: bound under `name` (and `id`, when the
/// callback's element arg has a `BindId`) for each iteration, with
/// shape dispatch from `typ`.
pub(crate) struct HofElem<'a> {
    pub name: &'a ArcStr,
    pub id: Option<BindId>,
    pub typ: &'a Type,
    /// Destructure leaves for a `|(k, v)|` callback. Empty for
    /// single-name callbacks; only valid on a composite element
    /// ([`bind_elem`] Errs otherwise).
    pub leaves: &'a [Leaf],
}

/// The owned locals one iteration bound; each is an env local, so a
/// mid-body pending exit drops it.
type OwnedLocals = SmallVec<[(LocalKind, ValueVar); 4]>;

/// A bound per-iteration element and its destructure leaves. The
/// element must be moved into the output or dropped, and the leaves
/// dropped, before the iteration ends.
struct Bound {
    elem: (LocalKind, ValueVar),
    leaves: OwnedLocals,
}

impl Bound {
    /// Drop the leaves once the body or predicate has consumed them (a
    /// body result may borrow a leaf until the push copies it); leaves
    /// never move into the output.
    fn drop_leaves(&self, cx: &mut BodyCx) -> Result<()> {
        drop_locals(cx, &self.leaves)
    }

    fn drop_elem(&self, cx: &mut BodyCx) -> Result<()> {
        let (kind, vv) = self.elem;
        emit_drop_local(cx.b, cx.ctx, kind, vv)
    }
}

fn drop_locals(cx: &mut BodyCx, locals: &[(LocalKind, ValueVar)]) -> Result<()> {
    for (kind, vv) in locals {
        emit_drop_local(cx.b, cx.ctx, *kind, *vv)?;
    }
    Ok(())
}

/// An element's disc: `base` plus the source's TAINT and STALE (an
/// element has its source collection's freshness).
fn elem_disc(cx: &mut BodyCx, base: ClifValue, src_disc: ClifValue) -> ClifValue {
    carry_disc(cx, base, src_disc, TAINT | STALE)
}

/// Fold the caller-selected tag bits from `src_disc` onto `base`.
fn carry_disc(
    cx: &mut BodyCx,
    base: ClifValue,
    src_disc: ClifValue,
    mask: i64,
) -> ClifValue {
    let sb = cx.b.ins().band_imm(src_disc, mask);
    cx.b.ins().bor(base, sb)
}

/// Read element `idx` of the composite `arr_ptr` as a local of `kind`:
/// its clean disc and owned payload.
fn read_elem(
    cx: &mut BodyCx,
    arr_ptr: ClifValue,
    idx: ClifValue,
    kind: LocalKind,
) -> Result<(ClifValue, ClifValue)> {
    let helper = match kind {
        LocalKind::Scalar(p) => valarray_get_helper(p),
        LocalKind::Composite => "graphix_valarray_get_array",
        LocalKind::String => "graphix_valarray_get_arcstr",
        LocalKind::Value => "graphix_valarray_get_value",
    };
    let call = cx.call_helper(helper, &[arr_ptr, idx])?;
    let r = cx.b.inst_results(call);
    let (r0, r1) = (r[0], r.get(1).copied());
    Ok(match (kind, r1) {
        (LocalKind::Value, Some(payload)) => (r0, payload),
        (LocalKind::Scalar(p), _) => (scalar_disc(cx.b, p), r0),
        (LocalKind::Composite, _) => {
            (cx.b.ins().iconst(types::I64, value_disc::ARRAY), r0)
        }
        (LocalKind::String, _) => (cx.b.ins().iconst(types::I64, value_disc::STRING), r0),
        (LocalKind::Value, None) => unreachable!("a value read returns a pair"),
    })
}

/// Read and bind the `|(k, v)|` destructure leaves of a composite
/// `base_ptr`, each under its pattern `BindId`. Each leaf disc carries
/// `src_disc & mask`; the owned ones are returned for dropping.
fn bind_leaves(
    cx: &mut BodyCx,
    base_ptr: ClifValue,
    src_disc: ClifValue,
    mask: i64,
    leaves: &[Leaf],
) -> Result<OwnedLocals> {
    let mut owned = OwnedLocals::new();
    for (id, idx, kind) in leaves {
        let idx_c = cx.b.ins().iconst(types::I64, *idx as i64);
        let (d, p) = read_elem(cx, base_ptr, idx_c, *kind)?;
        let d = carry_disc(cx, d, src_disc, mask);
        let vv = bind_local(cx, ArcStr::new(), d, p, *kind, Some(*id));
        if !matches!(kind, LocalKind::Scalar(_)) {
            owned.push((*kind, vv));
        }
    }
    Ok(owned)
}

/// Fetch element `i_now` of `arr_ptr` and bind it under `elem`, plus
/// its destructure leaves.
fn bind_elem(
    cx: &mut BodyCx,
    src_disc: ClifValue,
    arr_ptr: ClifValue,
    i_now: ClifValue,
    elem: &HofElem,
) -> Result<Bound> {
    let kind =
        kernel_abi::abi_kind(elem.typ).and_then(LocalKind::of).ok_or_else(|| {
            anyhow!("HOF element shape not supported by the JIT loop scaffolds")
        })?;
    if !elem.leaves.is_empty() && kind != LocalKind::Composite {
        return Err(anyhow!(
            "destructure leaves on a non-composite HOF element — caller bug"
        ));
    }
    let (d, p) = read_elem(cx, arr_ptr, i_now, kind)?;
    let d = elem_disc(cx, d, src_disc);
    let vv = bind_local(cx, elem.name.clone(), d, p, kind, elem.id);
    let leaves = match kind {
        LocalKind::Composite => bind_leaves(cx, p, src_disc, TAINT | STALE, elem.leaves)?,
        _ => OwnedLocals::new(),
    };
    Ok(Bound { elem: (kind, vv), leaves })
}

/// Register an owned input array on `owned_input_stack` so a
/// bottom-abort inside the loop frees it. Pair with [`drop_owned_src`]
/// after the loop: exactly one drop on either path.
fn adopt_owned_src(cx: &mut BodyCx, arr: &ArraySrc) {
    if arr.ownership == CompositeSource::Owned {
        let var = cx.b.declare_var(types::I64);
        cx.b.def_var(var, arr.ptr);
        cx.ctx.owned_input_stack.borrow_mut().push(var);
    }
}

/// Drop an owned input array at the post-loop merge point and pop its
/// registration.
fn drop_owned_src(cx: &mut BodyCx, arr: &ArraySrc) -> Result<()> {
    if arr.ownership == CompositeSource::Owned {
        cx.call_helper("graphix_valarray_drop", &[arr.ptr])?;
        cx.ctx.owned_input_stack.borrow_mut().pop();
    }
    Ok(())
}

/// `len = valarray_len(arr_ptr)`.
fn input_len(cx: &mut BodyCx, arr_ptr: ClifValue) -> Result<ClifValue> {
    let call = cx.call_helper("graphix_valarray_len", &[arr_ptr])?;
    Ok(cx.b.inst_results(call)[0])
}

/// An open scaffold loop over a range of slots: the counter, the
/// header, body and exit blocks and the slot-table frame.
/// [`LoopFrame::run`] emits one iteration's binds and body in the body
/// block; [`LoopFrame::close`] ends the loop in the exit block.
pub(super) struct LoopFrame {
    i_var: Variable,
    /// The iteration's slot ordinal.
    i: ClifValue,
    header: Block,
    body: Block,
    exit: Block,
    /// The env mark the iteration's binds truncate back to.
    mark: usize,
    /// The slots the loop instance entered before this run
    /// ([`SlotFlags`]): a slot at or past it is new.
    entered: ClifValue,
}

/// Open a loop over `len` slots of a source with disc `src_disc`, of an
/// instance that `entered` slots before; the builder ends in the loop
/// body.
fn open_loop(
    cx: &mut BodyCx,
    len: ClifValue,
    src_disc: ClifValue,
    sel_sites: &[ExprId],
    entered: ClifValue,
) -> Result<LoopFrame> {
    let i_var = cx.b.declare_var(types::I64);
    let zero = cx.b.ins().iconst(types::I64, 0);
    cx.b.def_var(i_var, zero);
    cx.open_slot_tables(sel_sites, len, src_disc, i_var)?;
    Ok(LoopFrame::blocks(cx, i_var, len, entered))
}

impl LoopFrame {
    /// The loop's blocks over slots `i_var..hi`, its slot-table frame
    /// already open; the builder ends in the loop body.
    pub(super) fn blocks(
        cx: &mut BodyCx,
        i_var: Variable,
        hi: ClifValue,
        entered: ClifValue,
    ) -> Self {
        let header = cx.b.create_block();
        let body = cx.b.create_block();
        let exit = cx.b.create_block();
        cx.b.ins().jump(header, &[]);
        cx.b.switch_to_block(header);
        let i_cur = cx.b.use_var(i_var);
        let cond = cx.b.ins().icmp(IntCC::SignedLessThan, i_cur, hi);
        cx.b.ins().brif(cond, body, &[], exit, &[]);
        cx.b.switch_to_block(body);
        let i = cx.b.use_var(i_var);
        LoopFrame { i_var, i, header, body, exit, mark: cx.env.mark(), entered }
    }
}

impl LoopFrame {
    /// Bind the iteration's names, poll the interrupt (its abort drops
    /// them) and emit the body, all in the loop's scope, under the slot's
    /// init view (its first iteration is one: the slot is a new
    /// instance); the scope and the slot-table frame close before an
    /// error propagates.
    fn run<'a, 'f, 'c, B, T>(
        &self,
        cx: &mut BodyCx<'a, 'f, 'c>,
        bind: impl FnOnce(&mut BodyCx<'a, 'f, 'c>, ClifValue) -> Result<B>,
        body: impl FnOnce(&mut BodyCx<'a, 'f, 'c>) -> Result<T>,
    ) -> Result<(B, T)> {
        cx.enter_loop();
        let outer = cx.env.slot_init;
        let new =
            cx.b.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, self.i, self.entered);
        let new = cx.b.ins().uextend(types::I64, new);
        let init = cx.init_flag();
        // CR claude for claude: [bug] The slot's init view reaches only constants, the
        // builtin stale mask and the callee first-call word. A read of a local bound
        // outside the loop keeps its STALE disc: a capture, a kernel param or an outer
        // let, read through emit_ref_node or as a capture in marshal_args. A new fold
        // slot's acc likewise carries the STALE of the init or of the previous slot.
        // The node-walk runs a new slot under event.init, where a standing read is
        // FIRED (node/mod.rs standing_view), and it seeds a fresh fold slot's acc FIRED
        // (node/collection.rs FoldQ::update). So `err?` over a standing error bound
        // outside the callback, or `acc?` in a fold, raises once per new slot in the
        // node-walk and never in the JIT. Under a genuine slot init (new slot, no wake)
        // those reads need STALE cleared with TAINT kept, and a new fold slot's acc
        // must read FIRED; probe: design/review-2026-10-05/repro/f-scaffold-body-02.gx
        // (graphix-fuzz check: interp (3, 3) then (4, 4), jit (0, 0)).
        // (f-scaffold-body-02)
        cx.env.slot_init = Some(cx.b.ins().bor(init, new));
        let r = bind(cx, self.i).and_then(|bound| {
            emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
            Ok((bound, body(cx)?))
        });
        cx.env.slot_init = outer;
        cx.exit_loop();
        cx.close_slot_tables();
        r
    }

    /// End the loop, then run the slot truncates in its exit block.
    fn close(self, cx: &mut BodyCx) -> Result<()> {
        self.end(cx);
        cx.emit_slot_truncates()
    }

    /// `i += 1` back to the header from the current block, then end the
    /// loop in its exit block.
    pub(super) fn end(self, cx: &mut BodyCx) {
        let one = cx.b.ins().iconst(types::I64, 1);
        let next = cx.b.ins().iadd(self.i, one);
        cx.b.def_var(self.i_var, next);
        cx.b.ins().jump(self.header, &[]);
        cx.b.seal_block(self.body);
        cx.b.seal_block(self.header);
        cx.b.switch_to_block(self.exit);
        cx.b.seal_block(self.exit);
    }
}

/// Push a compiled field into a `graphix_value_buf`, choosing the
/// helper by shape and `src`. Strings ignore `src`: string SSA is
/// always owned and `_push_string` consumes it. A tainted field does
/// not abort; the caller folds its taint into the result.
pub fn push_field(
    cx: &mut BodyCx,
    buf: ClifValue,
    cv: CompiledExpr,
    typ: &Type,
    src: CompositeSource,
) -> Result<()> {
    let helper_name: &str = match kernel_abi::abi_kind(typ) {
        Some(AbiKind::Scalar(p)) => super::scalar::value_buf_push_helper(p),
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => match src {
            CompositeSource::Owned => "graphix_value_buf_push_array",
            CompositeSource::Borrowed => "graphix_value_buf_push_array_borrowed",
        },
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => match src {
            CompositeSource::Owned => "graphix_value_buf_push_value",
            CompositeSource::Borrowed => "graphix_value_buf_push_value_borrowed",
        },
        Some(AbiKind::String) => "graphix_value_buf_push_string",
        Some(AbiKind::Unit) => {
            return Err(anyhow!(
                "producer-op field has Unit type — emit_*_new should reject"
            ));
        }
        Some(AbiKind::Null) | None => {
            return Err(anyhow!(
                "producer-op field has bare Null / non-fusable type — should \
                 widen to Nullable<T>"
            ));
        }
    };
    // Pushing a tainted field is safe: every push helper masks the tag
    // byte before cloning the value.
    if kernel_abi::is_value_shape(typ) {
        cx.call_helper(helper_name, &[buf, cv.disc, cv.payload])?;
    } else {
        cx.call_helper(helper_name, &[buf, cv.payload])?;
    }
    Ok(())
}

/// How a loop's result fires beyond a resize or a fired slot.
#[derive(Clone, Copy, PartialEq, Eq)]
enum LoopKind {
    /// map, filter_map, flat_map, find_map, init: the slots alone.
    Slots,
    /// filter, find: the result reads the source's elements, so a
    /// same-length source refresh with quiet slots still fires.
    PassThrough,
    /// fold: the result's own STALE, the acc carry, is a firing source
    /// beside the slots word; it alone covers zero iterations.
    Fold,
}

/// Loop-carried slot-flags accumulators: per-slot TAINT is OR-reduced
/// and per-slot STALE is AND-reduced across the loop's slots.
#[derive(Clone, Copy)]
pub(super) struct Accs {
    pub(super) taint: Variable,
    pub(super) stale: Variable,
}

impl Accs {
    pub(super) fn new(cx: &mut BodyCx) -> Self {
        let taint = cx.b.declare_var(types::I64);
        let z = cx.b.ins().iconst(types::I64, 0);
        cx.b.def_var(taint, z);
        let stale = cx.b.declare_var(types::I64);
        let st = cx.b.ins().iconst(types::I64, STALE);
        cx.b.def_var(stale, st);
        Accs { taint, stale }
    }

    /// Fold one slot's disc into the accumulators.
    fn fold(&self, cx: &mut BodyCx, disc: ClifValue) {
        let cur = cx.b.use_var(self.taint);
        let t = cx.b.ins().band_imm(disc, TAINT);
        let n = cx.b.ins().bor(cur, t);
        cx.b.def_var(self.taint, n);
        self.fold_stale(cx, disc);
    }

    /// Fold one slot's STALE bit alone: in a fold TAINT rides only the
    /// acc carry, so an acc-ignoring callback recovers.
    fn fold_stale(&self, cx: &mut BodyCx, disc: ClifValue) {
        let cur = cx.b.use_var(self.stale);
        let sb = cx.b.ins().band_imm(disc, STALE);
        let n = cx.b.ins().band(cur, sb);
        cx.b.def_var(self.stale, n);
    }
}

/// A loop's slot flags ([`Accs`]). A tainted slot taints the whole HOF
/// result but never the kernel. The loop fires iff a loop input fired
/// and the evaluation produced an event ([`Self::apply`]).
pub struct SlotFlags {
    accs: Accs,
    /// The source's element count, for the resize and empty-source terms.
    len: ClifValue,
    kind: LoopKind,
    /// The loop instance's prev-length word: `len + 1` after a run over
    /// a source, 0 before any; a bottom source sets [`FORGOT`] over it,
    /// forgetting the length (its return is a resize) but not the slots.
    word: Option<SelWord>,
    /// The slots the instance entered before this run (all, `u64::MAX`,
    /// without a word): a slot at or past it is new.
    entered: ClifValue,
}

/// A prev-length word's mark of a bottom source.
const FORGOT: i64 = i64::MIN;

impl SlotFlags {
    /// Emit in the loop's preheader: the instance's word is claimed
    /// (its state, its call-site block, or in a loop the enclosing
    /// slot's chain word) and read.
    fn new(cx: &mut BodyCx, len: ClifValue, kind: LoopKind) -> Self {
        let accs = Accs::new(cx);
        let word = match cx.claim_state_word() {
            Some(off) => {
                let sp = cx.state_ptr();
                Some(SelWord::Sure(cx.b.ins().iadd_imm(sp, off as i64)))
            }
            None => match cx.collection_site().and_then(|id| cx.slot_select_word(id)) {
                Some(w) => Some(w),
                // The site block base may be 0 on a recursive back-edge.
                None if cx.env.loop_depth == 0 => cx.claim_site_word().map(|off| {
                    let base = cx.site_ptr();
                    SelWord::Guarded { base, addr: cx.b.ins().iadd_imm(base, off as i64) }
                }),
                None => None,
            },
        };
        let entered = |cx: &mut BodyCx, addr: ClifValue| {
            let stored = cx.b.ins().load(types::I64, MemFlags::trusted(), addr, 0);
            let lenp1 = cx.b.ins().band_imm(stored, !FORGOT);
            let none = cx.b.ins().icmp_imm(IntCC::Equal, lenp1, 0);
            let len = cx.b.ins().iadd_imm(lenp1, -1);
            let zero = cx.b.ins().iconst(types::I64, 0);
            cx.b.ins().select(none, zero, len)
        };
        let all = cx.b.ins().iconst(types::I64, -1);
        let entered = match word {
            None => all,
            Some(SelWord::Sure(addr)) => entered(cx, addr),
            Some(SelWord::Guarded { base, addr }) => {
                let has = cx.b.ins().icmp_imm(IntCC::NotEqual, base, 0);
                let read = cx.b.create_block();
                let merge = cx.b.create_block();
                cx.b.append_block_param(merge, types::I64);
                cx.b.ins().brif(has, read, &[], merge, &[BlockArg::Value(all)]);
                cx.b.switch_to_block(read);
                cx.b.seal_block(read);
                let e = entered(cx, addr);
                cx.b.ins().jump(merge, &[BlockArg::Value(e)]);
                cx.b.switch_to_block(merge);
                cx.b.seal_block(merge);
                cx.b.block_params(merge)[0]
            }
        };
        SlotFlags { accs, len, kind, word, entered }
    }

    /// Fold the accumulated flags and the source disc `src` into `r`'s
    /// disc. Uses the exact firing rule when a prev-length word is
    /// available (a state word, a call-site word, or in a loop the
    /// enclosing slot's chain word); a nested loop without a chain word
    /// falls back to the conservative source-or-slot rule.
    // CR claude for claude: [structure] apply borrows the SlotFlags, so nothing stops a
    // second application, which would compare against the len + 1 the first one just
    // stored and miss a resize. Take self: SlotFlags is not Copy, so reusing it would
    // then fail to compile. finish_loop_result (node/collection.rs:607) only forwards
    // here; its two callers (emit_loop and emit_init_kind) can call apply directly.
    // (f-scaffold-body-08)
    pub fn apply(
        &self,
        cx: &mut BodyCx,
        mut r: CompiledExpr,
        src: ClifValue,
    ) -> CompiledExpr {
        let t = cx.b.use_var(self.accs.taint);
        r.disc = cx.b.ins().bor(r.disc, t);
        // the slots' fold carries both bits: bit 61 says whether a slot
        // fired, bit 60 whether every fire was a wake's own
        let slots_acc = cx.b.use_var(self.accs.stale);
        let src_word = cx.b.ins().band_imm(src, FIRE_TEST);
        let src_wake = cx.b.ins().band_imm(src, WAKE);
        let src_taint = cx.b.ins().band_imm(src, TAINT);
        r.disc = cx.b.ins().bor(r.disc, src_taint);
        let acc = if self.kind == LoopKind::Fold {
            let rs = cx.b.ins().band_imm(r.disc, STALE);
            cx.b.ins().band(rs, slots_acc)
        } else {
            slots_acc
        };
        let fired_word = cx.b.ins().band_imm(acc, FIRE_TEST);
        let slots_wake = cx.b.ins().band_imm(acc, WAKE);
        r.disc = cx.b.ins().band_imm(r.disc, !STALE);
        let (stale, src_counts) = match self.word {
            None => self.conservative_stale(cx, fired_word, src_word),
            Some(SelWord::Sure(addr)) => {
                self.exact_stale(cx, addr, fired_word, src_word, src_taint)
            }
            Some(SelWord::Guarded { base, addr }) => {
                self.guarded_exact_stale(cx, base, addr, fired_word, src_word, src_taint)
            }
        };
        // A fresh-tainted source is an event none of the firing terms
        // can see (no resize, quiet slots, not empty): fire it here.
        let tainted = cx.b.ins().icmp_imm(IntCC::NotEqual, src_taint, 0);
        let src_fired = cx.b.ins().icmp_imm(IntCC::Equal, src_word, 0);
        let fresh_taint = cx.b.ins().band(tainted, src_fired);
        let zero = cx.b.ins().iconst(types::I64, 0);
        let stale = cx.b.ins().select(fresh_taint, zero, stale);
        r.disc = cx.b.ins().bor(r.disc, stale);
        // a wake's own fire when every fire that made it was: the slots'
        // and, where it counts, the source's
        let src_counts = cx.b.ins().bor(src_counts, fresh_taint);
        let all = cx.b.ins().iconst(types::I64, WAKE);
        let src_wake = cx.b.ins().select(src_counts, src_wake, all);
        let wake = cx.b.ins().band(slots_wake, src_wake);
        r.disc = cx.b.ins().bor(r.disc, wake);
        r
    }

    /// [`exact_stale`](Self::exact_stale) behind a null-guard on `base`
    /// (0 on a recursive back-edge); the conservative rule on the 0 path.
    fn guarded_exact_stale(
        &self,
        cx: &mut BodyCx,
        base: ClifValue,
        addr: ClifValue,
        fired_word: ClifValue,
        src_word: ClifValue,
        src_taint: ClifValue,
    ) -> (ClifValue, ClifValue) {
        let has = cx.b.ins().icmp_imm(IntCC::NotEqual, base, 0);
        let exact_bl = cx.b.create_block();
        let cons_bl = cx.b.create_block();
        let merge = cx.b.create_block();
        cx.b.append_block_param(merge, types::I64);
        cx.b.append_block_param(merge, types::I8);
        cx.b.ins().brif(has, exact_bl, &[], cons_bl, &[]);
        cx.b.seal_block(exact_bl);
        cx.b.seal_block(cons_bl);
        cx.b.switch_to_block(exact_bl);
        let (stale, counts) = self.exact_stale(cx, addr, fired_word, src_word, src_taint);
        cx.b.ins().jump(merge, &[BlockArg::Value(stale), BlockArg::Value(counts)]);
        cx.b.switch_to_block(cons_bl);
        let (stale, counts) = self.conservative_stale(cx, fired_word, src_word);
        cx.b.ins().jump(merge, &[BlockArg::Value(stale), BlockArg::Value(counts)]);
        cx.b.seal_block(merge);
        cx.b.switch_to_block(merge);
        let ps = cx.b.block_params(merge);
        (ps[0], ps[1])
    }

    /// The exact firing rule's STALE contribution: fires iff resized, a
    /// slot fired, or the source fired empty, against the prev-length
    /// word at `addr` (stored `len + 1`; 0 = no previous observation);
    /// and whether the source's fire is one that made it.
    fn exact_stale(
        &self,
        cx: &mut BodyCx,
        addr: ClifValue,
        fired_word: ClifValue,
        src_word: ClifValue,
        src_taint: ClifValue,
    ) -> (ClifValue, ClifValue) {
        let len = self.len;
        let stored = cx.b.ins().load(types::I64, MemFlags::trusted(), addr, 0);
        let lenp1 = cx.b.ins().iadd_imm(len, 1);
        let valid = cx.b.ins().icmp_imm(IntCC::Equal, src_taint, 0);
        // A tainted source is never a resize and forgets the length: the
        // source's return is one, whether or not a slot fires. Its slots
        // stay entered.
        let resized = cx.b.ins().icmp(IntCC::NotEqual, stored, lenp1);
        let resized = cx.b.ins().band(resized, valid);
        let forgot = cx.b.ins().bor_imm(stored, FORGOT);
        let recorded = cx.b.ins().select(valid, lenp1, forgot);
        cx.b.ins().store(MemFlags::trusted(), recorded, addr, 0);
        let slot_fired = cx.b.ins().icmp_imm(IntCC::Equal, fired_word, 0);
        let src_fired = cx.b.ins().icmp_imm(IntCC::Equal, src_word, 0);
        let empty = cx.b.ins().icmp_imm(IntCC::Equal, len, 0);
        let src_empty = cx.b.ins().band(src_fired, empty);
        let src_counts = cx.b.ins().bor(resized, src_empty);
        let src_counts = if self.kind == LoopKind::PassThrough {
            cx.b.ins().bor(src_counts, src_fired)
        } else {
            src_counts
        };
        let fires = cx.b.ins().bor(src_counts, slot_fired);
        let quiet = cx.b.ins().iconst(types::I64, STALE);
        let zero = cx.b.ins().iconst(types::I64, 0);
        (cx.b.ins().select(fires, zero, quiet), src_counts)
    }

    /// The conservative STALE contribution (no prev-length word): fired
    /// when a slot or the source fired; and whether the source's fire
    /// is one that made it.
    fn conservative_stale(
        &self,
        cx: &mut BodyCx,
        fired_word: ClifValue,
        src_word: ClifValue,
    ) -> (ClifValue, ClifValue) {
        let src_counts = match self.kind {
            LoopKind::Fold => cx.b.ins().iconst(types::I8, 0),
            _ => cx.b.ins().icmp_imm(IntCC::Equal, src_word, 0),
        };
        let slot_fired = cx.b.ins().icmp_imm(IntCC::Equal, fired_word, 0);
        let fires = cx.b.ins().bor(slot_fired, src_counts);
        let quiet = cx.b.ins().iconst(types::I64, STALE);
        let zero = cx.b.ins().iconst(types::I64, 0);
        (cx.b.ins().select(fires, zero, quiet), src_counts)
    }
}

/// Where a loop's slots put their results.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum SinkKind {
    /// A value buf, in slot order.
    Buf,
    /// A find's first taken value.
    Find,
}

/// The sink of a loop open in the function being emitted.
#[derive(Clone, Copy)]
pub(super) enum Sink {
    Buf(ClifValue),
    /// The taken flag and value, bound as an env local above `mark`.
    Find {
        found: Variable,
        disc: Variable,
        payload: Variable,
        mark: usize,
    },
}

/// What one iteration of a loop works with, in the function it is
/// emitted in: the source, the sink and the slot flags' accumulators.
pub(super) struct Slots {
    pub(super) src: ClifValue,
    pub(super) src_disc: ClifValue,
    pub(super) sink: Sink,
    pub(super) accs: Accs,
}

impl Slots {
    fn buf(&self) -> ClifValue {
        match self.sink {
            Sink::Buf(buf) => buf,
            Sink::Find { .. } => unreachable!("a buf loop's sink"),
        }
    }

    fn found(&self) -> (Variable, Variable, Variable) {
        match self.sink {
            Sink::Find { found, disc, payload, .. } => (found, disc, payload),
            Sink::Buf(_) => unreachable!("a find loop's sink"),
        }
    }
}

/// A finished loop's result: owned ValArray bits, or a find's
/// `(disc, payload)`.
pub(super) enum Sunk {
    Buf(ClifValue),
    Find(ClifValue, ClifValue),
}

impl Sunk {
    fn array(self) -> ClifValue {
        match self {
            Sunk::Buf(ptr) => ptr,
            Sunk::Find(..) => unreachable!("a buf loop's result"),
        }
    }

    fn taken(self) -> (ClifValue, ClifValue) {
        match self {
            Sunk::Find(disc, payload) => (disc, payload),
            Sunk::Buf(_) => unreachable!("a find loop's result"),
        }
    }
}

/// Open a sink for `cap` slots: a registered value buf, or a find's
/// result bound so an abort in a later slot drops a taken value.
pub(super) fn open_sink(cx: &mut BodyCx, kind: SinkKind, cap: ClifValue) -> Result<Sink> {
    Ok(match kind {
        SinkKind::Buf => Sink::Buf(open_value_buf(cx, cap)?),
        SinkKind::Find => {
            let (found, disc, payload) = found_vars(cx);
            let mark = bind_taken(cx, disc, payload);
            Sink::Find { found, disc, payload, mark }
        }
    })
}

/// One iteration of a loop, emitted in whichever function runs it.
pub(super) trait Iteration:
    for<'x, 'y, 'z> FnOnce(&mut BodyCx<'x, 'y, 'z>, &LoopFrame, &Slots) -> Result<()>
{
}

impl<I> Iteration for I where
    I: for<'x, 'y, 'z> FnOnce(&mut BodyCx<'x, 'y, 'z>, &LoopFrame, &Slots) -> Result<()>
{
}

/// Run `iteration` over `len` slots of a source with disc `src_disc`,
/// folding them into `accs` and `kind`'s sink: inline, or outlined into
/// a chunk at a body's top level, where its slots may fork.
fn emit_slots(
    cx: &mut BodyCx,
    flags: &SlotFlags,
    kind: SinkKind,
    (src, src_disc, len): (ClifValue, ClifValue, ClifValue),
    sel_sites: &[ExprId],
    iteration: impl Iteration,
) -> Result<Sunk> {
    // CR claude for claude: [bug] Under a tainted source this runs `len` iterations over
    // the source's placeholder. emit_fold_loop (line 1160) does the same, and so does
    // emit_init_loop, which clamps only an oversize count (795). The node-walk builds
    // no slot under a bottom source and runs exactly the slots it kept
    // (MapQ/FoldQ::update). When the placeholder is longer than the kept slots, the
    // extra iterations are phantom slots: a tainted run leaves `entered` unchanged, so
    // each one runs under an init view, a constant error under `?` raises there and
    // raises again when the source turns valid, and its fires make the bottom result
    // fresh. When it is shorter, kept slots are skipped: a missing input is staged as
    // an empty array, so a kept slot's raise is lost. probe:
    // design/review-2026-10-05/repro/f-scaffold-body-01.gx (interp (2, 2), jit (4, 0)).
    // A fix also needs a ruling on the bottom-source firing rule: MapQ ignores its kept
    // slots' fires (collection.rs:1082), while FoldQ counts them (1517).
    // (f-scaffold-body-01)
    let (accs, entered) = (flags.accs, flags.entered);
    if cx.env.loop_depth == 0 && !crate::dbgenv::graphix_no_outline() {
        let lp = outline::Loop { kind, src, src_disc, len, entered, sel_sites };
        return outline::emit_outlined(cx, lp, accs, iteration);
    }
    let sink = open_sink(cx, kind, len)?;
    let lp = open_loop(cx, len, src_disc, sel_sites, entered)?;
    iteration(cx, &lp, &Slots { src, src_disc, sink, accs })?;
    lp.close(cx)?;
    Ok(match sink {
        Sink::Buf(buf) => Sunk::Buf(finalize_valarray(cx, buf)?),
        Sink::Find { disc, payload, mark, .. } => {
            cx.env.truncate(mark);
            Sunk::Find(cx.b.use_var(disc), cx.b.use_var(payload))
        }
    })
}

pub(crate) fn emit_init_loop<F>(
    cx: &mut BodyCx,
    n_raw: ClifValue,
    n_disc: ClifValue,
    n_prim: PrimType,
    idx_name: &ArcStr,
    idx_id: Option<BindId>,
    out_typ: &Type,
    out_src: CompositeSource,
    sel_sites: &[ExprId],
    body: F,
) -> Result<(ClifValue, SlotFlags, ClifValue)>
where
    F: for<'x, 'y, 'z> FnOnce(&mut BodyCx<'x, 'y, 'z>) -> Result<CompiledExpr>,
{
    let n_widened = widen_to_i64(cx.b, n_raw, n_prim)?;
    let zero = cx.b.ins().iconst(types::I64, 0);
    let is_negative = cx.b.ins().icmp(IntCC::SignedLessThan, n_widened, zero);
    let n = cx.b.ins().select(is_negative, zero, n_widened);
    let max = cx.b.ins().iconst(types::I64, crate::node::MAX_ARRAY_INIT_LEN);
    let oversize = cx.b.ins().icmp(IntCC::SignedGreaterThan, n, max);
    {
        let fired = is_fresh(cx.b, n_disc);
        let report = cx.b.ins().band(oversize, fired);
        let report_bl = cx.b.create_block();
        let cont_bl = cx.b.create_block();
        cx.b.ins().brif(report, report_bl, &[], cont_bl, &[]);
        cx.b.switch_to_block(report_bl);
        cx.b.seal_block(report_bl);
        cx.call_helper("graphix_init_oversize", &[n])?;
        cx.b.ins().jump(cont_bl, &[]);
        cx.b.switch_to_block(cont_bl);
        cx.b.seal_block(cont_bl);
    }
    // An over-limit count is bottom: taint the count's disc (so the
    // stored length and slot tables are kept, not reset to 0) but keep
    // its STALE bit, since a count that fired over the limit is a fresh
    // bottom. The loop bound clamps to 0.
    let forced_bits = cx.b.ins().iconst(types::I64, TAINT);
    let forced_disc = cx.b.ins().bor(n_disc, forced_bits);
    let n_disc = cx.b.ins().select(oversize, forced_disc, n_disc);
    let n = cx.b.ins().select(oversize, zero, n);
    let flags = SlotFlags::new(cx, n, LoopKind::Slots);
    let stale = cx.b.ins().iconst(types::I64, STALE);
    let tainted = cx.b.ins().iconst(types::I64, TAINT | STALE);
    let disc = cx.b.ins().select(oversize, tainted, stale);
    flags.accs.fold(cx, disc);
    let sunk = emit_slots(cx, &flags, SinkKind::Buf, (zero, n_disc, n), sel_sites, {
        |cx, lp, s| {
            let ((), value) = lp.run(
                cx,
                |cx, _| {
                    let idx_disc = scalar_disc(cx.b, PrimType::I64);
                    let idx_disc = elem_disc(cx, idx_disc, s.src_disc);
                    let idx_disc_var = cx.b.declare_var(types::I64);
                    cx.b.def_var(idx_disc_var, idx_disc);
                    let (name, i_var) = (idx_name.clone(), lp.i_var);
                    bind_scalar_var_with_disc(
                        cx,
                        name,
                        PrimType::I64,
                        i_var,
                        idx_disc_var,
                        idx_id,
                    );
                    Ok(())
                },
                body,
            )?;
            s.accs.fold(cx, value.disc);
            push_field(cx, s.buf(), value, out_typ, out_src)?;
            cx.env.truncate(lp.mark);
            Ok(())
        }
    })?;
    Ok((sunk.array(), flags, n_disc))
}

pub(crate) fn emit_map_loop<F>(
    cx: &mut BodyCx,
    arr: ArraySrc,
    elem: &HofElem,
    out_typ: &Type,
    out_src: CompositeSource,
    sel_sites: &[ExprId],
    body: F,
) -> Result<(ClifValue, SlotFlags)>
where
    F: for<'x, 'y, 'z> FnOnce(&mut BodyCx<'x, 'y, 'z>) -> Result<CompiledExpr>,
{
    adopt_owned_src(cx, &arr);
    let len = input_len(cx, arr.ptr)?;
    let flags = SlotFlags::new(cx, len, LoopKind::Slots);
    let source = (arr.ptr, arr.disc, len);
    let sunk = emit_slots(cx, &flags, SinkKind::Buf, source, sel_sites, {
        |cx, lp, s| {
            let (bound, value) =
                lp.run(cx, |cx, i| bind_elem(cx, s.src_disc, s.src, i, elem), body)?;
            s.accs.fold(cx, value.disc);
            push_field(cx, s.buf(), value, out_typ, out_src)?;
            bound.drop_leaves(cx)?;
            bound.drop_elem(cx)?;
            cx.env.truncate(lp.mark);
            Ok(())
        }
    })?;
    drop_owned_src(cx, &arr)?;
    Ok((sunk.array(), flags))
}

pub(crate) fn emit_filter_loop<F>(
    cx: &mut BodyCx,
    arr: ArraySrc,
    elem: &HofElem,
    sel_sites: &[ExprId],
    predicate: F,
) -> Result<(ClifValue, SlotFlags)>
where
    F: for<'x, 'y, 'z> FnOnce(&mut BodyCx<'x, 'y, 'z>) -> Result<CompiledExpr>,
{
    adopt_owned_src(cx, &arr);
    let len = input_len(cx, arr.ptr)?;
    let flags = SlotFlags::new(cx, len, LoopKind::PassThrough);
    let source = (arr.ptr, arr.disc, len);
    let sunk = emit_slots(cx, &flags, SinkKind::Buf, source, sel_sites, {
        |cx, lp, s| {
            let (bound, keep) =
                lp.run(cx, |cx, i| bind_elem(cx, s.src_disc, s.src, i, elem), predicate)?;
            s.accs.fold(cx, keep.disc);
            bound.drop_leaves(cx)?;
            cx.env.truncate(lp.mark);
            let push_block = cx.b.create_block();
            let drop_block = cx.b.create_block();
            let advance = cx.b.create_block();
            cx.b.ins().brif(keep.payload, push_block, &[], drop_block, &[]);
            cx.b.switch_to_block(push_block);
            cx.b.seal_block(push_block);
            // A kept element moves into the result.
            let (_, vv) = bound.elem;
            let cv = CompiledExpr::new(cx.b.use_var(vv.disc), cx.b.use_var(vv.payload));
            push_field(cx, s.buf(), cv, elem.typ, CompositeSource::Owned)?;
            cx.b.ins().jump(advance, &[]);
            cx.b.switch_to_block(drop_block);
            cx.b.seal_block(drop_block);
            bound.drop_elem(cx)?;
            cx.b.ins().jump(advance, &[]);
            cx.b.switch_to_block(advance);
            cx.b.seal_block(advance);
            Ok(())
        }
    })?;
    drop_owned_src(cx, &arr)?;
    Ok((sunk.array(), flags))
}

pub(crate) fn emit_filter_map_loop<F>(
    cx: &mut BodyCx,
    arr: ArraySrc,
    elem: &HofElem,
    out_elem: &Type,
    out_src: CompositeSource,
    sel_sites: &[ExprId],
    body: F,
) -> Result<(ClifValue, SlotFlags)>
where
    F: for<'x, 'y, 'z> FnOnce(&mut BodyCx<'x, 'y, 'z>) -> Result<CompiledExpr>,
{
    if matches!(
        kernel_abi::abi_kind(out_elem),
        Some(AbiKind::Unit | AbiKind::Null) | None
    ) {
        return Err(anyhow!("filter_map output element is not representable"));
    }
    adopt_owned_src(cx, &arr);
    let len = input_len(cx, arr.ptr)?;
    let flags = SlotFlags::new(cx, len, LoopKind::Slots);
    let source = (arr.ptr, arr.disc, len);
    let sunk = emit_slots(cx, &flags, SinkKind::Buf, source, sel_sites, {
        |cx, lp, s| {
            let (bound, value) =
                lp.run(cx, |cx, i| bind_elem(cx, s.src_disc, s.src, i, elem), body)?;
            cx.env.truncate(lp.mark);
            s.accs.fold(cx, value.disc);
            let disc = clean_disc(cx.b, value.disc);
            let is_null = cx.b.ins().icmp_imm(IntCC::Equal, disc, value_disc::NULL);
            let push_block = cx.b.create_block();
            let advance = cx.b.create_block();
            cx.b.ins().brif(is_null, advance, &[], push_block, &[]);
            cx.b.switch_to_block(push_block);
            cx.b.seal_block(push_block);
            // The result is always the callback's 2-word Nullable-shaped
            // Value, never an unwrapped element (a `[T, Error]` result may
            // hold an Error where the arm expects T): push it as a value,
            // bit-for-bit.
            let helper = match out_src {
                CompositeSource::Owned => "graphix_value_buf_push_value",
                CompositeSource::Borrowed => "graphix_value_buf_push_value_borrowed",
            };
            cx.call_helper(helper, &[s.buf(), value.disc, value.payload])?;
            cx.b.ins().jump(advance, &[]);
            cx.b.switch_to_block(advance);
            cx.b.seal_block(advance);
            bound.drop_leaves(cx)?;
            bound.drop_elem(cx)?;
            Ok(())
        }
    })?;
    drop_owned_src(cx, &arr)?;
    Ok((sunk.array(), flags))
}

/// How a flat_map body result splices into the output buf: an array
/// result extends via its owned ValArray ptr; a list result walks the
/// cons chain (a non-list value pushes as a single element).
#[derive(Clone, Copy)]
pub enum FlatMapExtend {
    Array,
    List,
}

pub(crate) fn emit_flat_map_loop<F>(
    cx: &mut BodyCx,
    arr: ArraySrc,
    elem: &HofElem,
    extend_kind: FlatMapExtend,
    sel_sites: &[ExprId],
    body: F,
) -> Result<(ClifValue, SlotFlags)>
where
    F: for<'x, 'y, 'z> FnOnce(&mut BodyCx<'x, 'y, 'z>) -> Result<CompiledExpr>,
{
    adopt_owned_src(cx, &arr);
    let len = input_len(cx, arr.ptr)?;
    let flags = SlotFlags::new(cx, len, LoopKind::Slots);
    let source = (arr.ptr, arr.disc, len);
    let sunk = emit_slots(cx, &flags, SinkKind::Buf, source, sel_sites, {
        |cx, lp, s| {
            let (bound, value) =
                lp.run(cx, |cx, i| bind_elem(cx, s.src_disc, s.src, i, elem), body)?;
            s.accs.fold(cx, value.disc);
            bound.drop_leaves(cx)?;
            bound.drop_elem(cx)?;
            cx.env.truncate(lp.mark);
            match extend_kind {
                FlatMapExtend::Array => cx.call_helper(
                    "graphix_value_buf_extend_from_array",
                    &[s.buf(), value.payload],
                )?,
                FlatMapExtend::List => cx.call_helper(
                    "graphix_value_buf_extend_from_list",
                    &[s.buf(), value.disc, value.payload],
                )?,
            };
            Ok(())
        }
    })?;
    drop_owned_src(cx, &arr)?;
    Ok((sunk.array(), flags))
}

/// Destructure leaves for a `|(k, v)|`-style pattern over a tuple-typed
/// value: per bound leaf its pattern `BindId`, tuple position and
/// [`LocalKind`]. `None` when the type isn't a tuple or a bound position
/// has no kernel shape (the caller node-walks); empty binds give
/// `Some(empty)`.
pub(crate) fn elem_leaves(
    in_elem: &Type,
    elem_binds: &[(BindId, usize)],
) -> Option<Leaves> {
    if elem_binds.is_empty() {
        return Some(Leaves::new());
    }
    let Type::Tuple(ts) = in_elem else { return None };
    elem_binds
        .iter()
        .map(|(id, i)| {
            let kind = kernel_abi::abi_kind(ts.get(*i)?).and_then(LocalKind::of)?;
            Some((*id, *i, kind))
        })
        .collect()
}

/// The fold accumulator's shape: how the loop-carried value is held,
/// made owned, and dropped when replaced.
pub(crate) enum FoldAcc<'a> {
    /// A register scalar. Owns nothing.
    Scalar(PrimType),
    /// Owned ValArray bits. The loop owns the current acc: each
    /// iteration the body's result is made owned per `body_src` and the
    /// old acc dropped. `leaves` are the acc pattern's destructure
    /// leaves, re-read off the current acc each iteration.
    Composite { init_src: CompositeSource, body_src: CompositeSource, leaves: &'a [Leaf] },
    /// An owned `ArcStr`. String reads always clone, so results are
    /// already owned; the old acc still drops when replaced.
    Str,
    /// An owned two-word Value. Its real value disc varies per
    /// iteration (a nullable acc alternates Null and its value), so the
    /// disc Variable carries the whole disc, never a re-based constant.
    Value { init_src: CompositeSource, body_src: CompositeSource },
}

impl FoldAcc<'_> {
    fn local_kind(&self) -> LocalKind {
        match self {
            FoldAcc::Scalar(p) => LocalKind::Scalar(*p),
            FoldAcc::Composite { .. } => LocalKind::Composite,
            FoldAcc::Str => LocalKind::String,
            FoldAcc::Value { .. } => LocalKind::Value,
        }
    }

    /// Where the init's result comes from.
    fn init_src(&self) -> CompositeSource {
        match self {
            FoldAcc::Composite { init_src, .. } | FoldAcc::Value { init_src, .. } => {
                *init_src
            }
            FoldAcc::Scalar(_) | FoldAcc::Str => CompositeSource::Owned,
        }
    }

    /// Where the body's result comes from.
    fn body_src(&self) -> CompositeSource {
        match self {
            FoldAcc::Composite { body_src, .. } | FoldAcc::Value { body_src, .. } => {
                *body_src
            }
            FoldAcc::Scalar(_) | FoldAcc::Str => CompositeSource::Owned,
        }
    }

    /// `(payload, disc)` of a new acc made owned from `cv`, which came
    /// from `src`.
    fn owned(
        &self,
        cx: &mut BodyCx,
        cv: CompiledExpr,
        src: CompositeSource,
    ) -> Result<(ClifValue, ClifValue)> {
        Ok(match self {
            FoldAcc::Composite { .. } => {
                (ensure_owned_composite_src(cx, src, cv.payload)?, cv.disc)
            }
            FoldAcc::Scalar(_) | FoldAcc::Str => (cv.payload, cv.disc),
            FoldAcc::Value { .. } => {
                let (d, p) = ensure_owned_value_src(cx, src, cv.disc, cv.payload)?;
                (p, d)
            }
        })
    }

    /// The next carried disc: the whole disc minus the other tag bits for
    /// [`FoldAcc::Value`], re-based on the shape's clean disc otherwise so
    /// only TAINT and STALE ride.
    fn carry_disc(&self, cx: &mut BodyCx, from_disc: ClifValue) -> ClifValue {
        let base = match self {
            FoldAcc::Value { .. } => {
                const KEEP: i64 = !TAG_MASK as i64 | TAINT | STALE;
                return cx.b.ins().band_imm(from_disc, KEEP);
            }
            FoldAcc::Scalar(p) => scalar_disc(cx.b, *p),
            FoldAcc::Composite { .. } => cx.b.ins().iconst(types::I64, value_disc::ARRAY),
            FoldAcc::Str => cx.b.ins().iconst(types::I64, value_disc::STRING),
        };
        let t = cx.b.ins().band_imm(from_disc, TAINT | STALE);
        cx.b.ins().bor(base, t)
    }
}

/// `array::fold(arr, init, |acc, x| body)`. Each iteration binds the
/// acc first, then the element (the order is load-bearing); the body's
/// result becomes the next acc.
pub(crate) fn emit_fold_loop<'a, 'f, 'c, I, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    acc: FoldAcc,
    acc_name: &ArcStr,
    acc_id: Option<BindId>,
    elem: &HofElem,
    sel_sites: &[ExprId],
    init: I,
    body: F,
) -> Result<(CompiledExpr, SlotFlags)>
where
    I: FnOnce(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
    F: FnOnce(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    adopt_owned_src(cx, &arr);
    let len = input_len(cx, arr.ptr)?;
    let kind = acc.local_kind();
    let acc_var = cx.b.declare_var(match kind {
        LocalKind::Scalar(p) => prim_to_clif(p),
        _ => types::I64,
    });
    // The acc's TAINT and STALE are loop-carried in its own disc; STALE
    // must ride or an acc-consuming body reads FIRED on every run.
    let acc_disc_var = cx.b.declare_var(types::I64);
    let acc_vv = ValueVar { disc: acc_disc_var, payload: acc_var };
    let flags = SlotFlags::new(cx, len, LoopKind::Fold);
    let init_cv = init(cx)?;
    // A pointer-shaped acc is loop-owned from the start: a borrowed init
    // clones here.
    let (init_pay, init_disc) = acc.owned(cx, init_cv, acc.init_src())?;
    cx.b.def_var(acc_var, init_pay);
    let d0 = acc.carry_disc(cx, init_disc);
    cx.b.def_var(acc_disc_var, d0);
    let lp = open_loop(cx, len, arr.disc, sel_sites, flags.entered)?;
    // The acc binds before the interrupt poll so the poll's abort cleanup
    // drops an owned acc. Acc leaves carry the acc's loop-carried
    // TAINT|STALE; unlike an element, the acc can be tainted.
    let ((bound, acc_leaves), new_acc) = lp.run(
        cx,
        |cx, i| {
            cx.env.bind(acc_name.clone(), acc_vv, kind, acc_id);
            let bound = bind_elem(cx, arr.disc, arr.ptr, i, elem)?;
            let acc_leaves = match &acc {
                FoldAcc::Composite { leaves, .. } if !leaves.is_empty() => {
                    let acc_ptr = cx.b.use_var(acc_var);
                    let acc_disc = cx.b.use_var(acc_disc_var);
                    bind_leaves(cx, acc_ptr, acc_disc, TAINT | STALE, leaves)?
                }
                _ => OwnedLocals::new(),
            };
            Ok((bound, acc_leaves))
        },
        body,
    )?;
    // Make the new acc owned before anything drops: a borrowed body
    // result (`|acc, x| acc`) may alias the old acc, an element, or a leaf.
    let (new_pay, new_disc) = acc.owned(cx, new_acc, acc.body_src())?;
    emit_drop_local(cx.b, cx.ctx, kind, acc_vv)?;
    drop_locals(cx, &acc_leaves)?;
    bound.drop_leaves(cx)?;
    bound.drop_elem(cx)?;
    cx.env.truncate(lp.mark);
    // Each body evaluation's STALE folds into the firing flags (a
    // mid-chain body that consumed a fired acc fires the fold even if
    // the final carry is stale); TAINT travels only the acc carry.
    flags.accs.fold_stale(cx, new_disc);
    cx.b.def_var(acc_var, new_pay);
    let d = acc.carry_disc(cx, new_disc);
    cx.b.def_var(acc_disc_var, d);
    lp.close(cx)?;
    drop_owned_src(cx, &arr)?;
    let payload = cx.b.use_var(acc_var);
    let disc = cx.b.use_var(acc_disc_var);
    Ok((CompiledExpr::new(disc, payload), flags))
}

/// Bind a find loop's result pair as an owned Value local, so an abort
/// in a later iteration drops a taken value; returns the mark that
/// unbinds it once the loop hands the result over.
fn bind_taken(cx: &mut BodyCx, disc: Variable, payload: Variable) -> usize {
    let mark = cx.env.mark();
    cx.env.bind(ArcStr::new(), ValueVar { disc, payload }, LocalKind::Value, None);
    mark
}

/// A find loop's result pair: `Value::Null` until an iteration takes
/// its value.
fn found_vars(cx: &mut BodyCx) -> (Variable, Variable, Variable) {
    let found = cx.b.declare_var(types::I8);
    let zero8 = cx.b.ins().iconst(types::I8, 0);
    cx.b.def_var(found, zero8);
    let disc = cx.b.declare_var(types::I64);
    let null_disc = cx.b.ins().iconst(types::I64, value_disc::NULL);
    cx.b.def_var(disc, null_disc);
    let payload = cx.b.declare_var(types::I64);
    let zero64 = cx.b.ins().iconst(types::I64, 0);
    cx.b.def_var(payload, zero64);
    (found, disc, payload)
}

/// Branch to a take block when `cond` holds and nothing was taken yet;
/// the builder ends in the take block and the discard block is returned.
fn branch_take(cx: &mut BodyCx, cond: ClifValue, found: Variable) -> (Block, Block) {
    let not_found = {
        let found = cx.b.use_var(found);
        cx.b.ins().icmp_imm(IntCC::Equal, found, 0)
    };
    let take = cx.b.ins().band(cond, not_found);
    let take_block = cx.b.create_block();
    let discard = cx.b.create_block();
    let advance = cx.b.create_block();
    cx.b.ins().brif(take, take_block, &[], discard, &[]);
    cx.b.switch_to_block(take_block);
    cx.b.seal_block(take_block);
    (discard, advance)
}

pub(crate) fn emit_find_loop<F>(
    cx: &mut BodyCx,
    arr: ArraySrc,
    elem: &HofElem,
    sel_sites: &[ExprId],
    predicate: F,
) -> Result<((ClifValue, ClifValue), SlotFlags)>
where
    F: for<'x, 'y, 'z> FnOnce(&mut BodyCx<'x, 'y, 'z>) -> Result<CompiledExpr>,
{
    adopt_owned_src(cx, &arr);
    let len = input_len(cx, arr.ptr)?;
    let flags = SlotFlags::new(cx, len, LoopKind::PassThrough);
    let source = (arr.ptr, arr.disc, len);
    let sunk = emit_slots(cx, &flags, SinkKind::Find, source, sel_sites, {
        |cx, lp, s| {
            let (found_var, result_disc_var, result_payload_var) = s.found();
            let (bound, keep) =
                lp.run(cx, |cx, i| bind_elem(cx, s.src_disc, s.src, i, elem), predicate)?;
            s.accs.fold(cx, keep.disc);
            bound.drop_leaves(cx)?;
            cx.env.truncate(lp.mark);
            let (discard, advance) = branch_take(cx, keep.payload, found_var);
            // The taken element moves into the result, in its Value encoding.
            let (disc, payload) = match bound.elem {
                (LocalKind::Scalar(p), vv) => {
                    let value = cx.b.use_var(vv.payload);
                    let disc = cx.b.ins().iconst(types::I64, prim_to_value_disc(p));
                    (disc, scalar_to_payload_i64(cx.b, p, value))
                }
                (LocalKind::Composite, vv) => (
                    cx.b.ins().iconst(types::I64, value_disc::ARRAY),
                    cx.b.use_var(vv.payload),
                ),
                (LocalKind::String, vv) => (
                    cx.b.ins().iconst(types::I64, value_disc::STRING),
                    cx.b.use_var(vv.payload),
                ),
                (LocalKind::Value, vv) => {
                    (cx.b.use_var(vv.disc), cx.b.use_var(vv.payload))
                }
            };
            cx.b.def_var(result_disc_var, disc);
            cx.b.def_var(result_payload_var, payload);
            let one = cx.b.ins().iconst(types::I8, 1);
            cx.b.def_var(found_var, one);
            cx.b.ins().jump(advance, &[]);
            cx.b.switch_to_block(discard);
            cx.b.seal_block(discard);
            bound.drop_elem(cx)?;
            cx.b.ins().jump(advance, &[]);
            cx.b.switch_to_block(advance);
            cx.b.seal_block(advance);
            Ok(())
        }
    })?;
    drop_owned_src(cx, &arr)?;
    Ok((sunk.taken(), flags))
}

pub(crate) fn emit_find_map_loop<F>(
    cx: &mut BodyCx,
    arr: ArraySrc,
    elem: &HofElem,
    sel_sites: &[ExprId],
    body: F,
) -> Result<((ClifValue, ClifValue), SlotFlags)>
where
    F: for<'x, 'y, 'z> FnOnce(&mut BodyCx<'x, 'y, 'z>) -> Result<(ClifValue, ClifValue)>,
{
    adopt_owned_src(cx, &arr);
    let len = input_len(cx, arr.ptr)?;
    let flags = SlotFlags::new(cx, len, LoopKind::Slots);
    let source = (arr.ptr, arr.disc, len);
    let sunk = emit_slots(cx, &flags, SinkKind::Find, source, sel_sites, {
        |cx, lp, s| {
            let (found_var, result_disc_var, result_payload_var) = s.found();
            let (bound, (disc, payload)) =
                lp.run(cx, |cx, i| bind_elem(cx, s.src_disc, s.src, i, elem), body)?;
            bound.drop_leaves(cx)?;
            bound.drop_elem(cx)?;
            cx.env.truncate(lp.mark);
            s.accs.fold(cx, disc);
            let clean = clean_disc(cx.b, disc);
            let non_null = cx.b.ins().icmp_imm(IntCC::NotEqual, clean, value_disc::NULL);
            let (discard, advance) = branch_take(cx, non_null, found_var);
            cx.b.def_var(result_disc_var, clean);
            cx.b.def_var(result_payload_var, payload);
            let one = cx.b.ins().iconst(types::I8, 1);
            cx.b.def_var(found_var, one);
            cx.b.ins().jump(advance, &[]);
            cx.b.switch_to_block(discard);
            cx.b.seal_block(discard);
            cx.call_helper("graphix_value_drop", &[disc, payload])?;
            cx.b.ins().jump(advance, &[]);
            cx.b.switch_to_block(advance);
            cx.b.seal_block(advance);
            Ok(())
        }
    })?;
    drop_owned_src(cx, &arr)?;
    Ok((sunk.taken(), flags))
}

/// Collect the per-slot state sites in a scaffold-loop body: the
/// callsite `ExprId` of every nested collection HOF call (its
/// prev-length word) and of every cross-kernel lambda call (its
/// first-call word), each of which claims one per-slot state chain (see
/// [`BodyCx::open_slot_tables`]). A nested callback body lives behind
/// its own lambda def and anchors its sites in the chain its own loop
/// opens.
pub(crate) fn slot_state_sites<R: Rt, E: UserEvent>(
    cx: &BodyCx,
    node: &Node<R, E>,
) -> LPooled<Vec<ExprId>> {
    let mut ids: LPooled<Vec<ExprId>> = LPooled::take();
    fusion::for_each_node(node, &mut |n| match n.view() {
        NodeView::CallSite(cs) => {
            let id = n.spec().id;
            let collection = matches!(
                cs.resolved_apply(),
                Some(ApplyView::Lambda(l)) if l.inline_callback_body().is_some()
            );
            if collection || cx.ctx.lambda_call_sites.contains_key(&id) {
                ids.push(id);
            }
        }
        _ => {}
    });
    ids
}
