//! HOF loop scaffolds for the cranelift JIT backend.
//!
//! Each `emit_*_loop` owns the mechanics of one loop shape (length and
//! buf calls, the counter, block creation and sealing, per-iteration
//! element binding and dropping, pending-cleanup registration); the
//! caller supplies the body as a closure over the [`BodyCx`].
//!
//! The emitted CLIF must stay instruction-for-instruction stable:
//! preserve instruction, block-creation and variable-declaration order.

use crate::{
    expr::ExprId,
    fusion::kernel_abi::{self, AbiKind, PrimType},
    typ::Type,
};
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use cranelift_codegen::ir::{
    Block, BlockArg, InstBuilder, MemFlags, Value as ClifValue, condcodes::IntCC, types,
};
use cranelift_frontend::{FunctionBuilder, Variable};

use super::{
    abi::{
        CompiledExpr, LocalKind, STALE, TAINT, ValueVar, bind_local,
        bind_scalar_var_with_disc, clean_disc, prim_to_value_disc, scalar_disc,
        value_disc,
    },
    body::{
        BodyCx, emit_interrupt_check, ensure_owned_composite_src, ensure_owned_value_src,
    },
    call::CompositeSource,
    lower::{LowerCtx, SelWord},
    scalar::{
        prim_to_clif, scalar_to_payload_i64, valarray_get_helper, value_buf_push_helper,
        widen_to_i64,
    },
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

/// Loop element binding: bound under `name` (and `id`, when the
/// callback's element arg has a `BindId`) for each iteration, with
/// shape dispatch from `typ`.
pub struct HofElem<'a> {
    pub name: &'a ArcStr,
    pub id: Option<crate::BindId>,
    pub typ: &'a Type,
    /// Destructure leaves for a `|(k, v)|` callback: per bound leaf its
    /// pattern `BindId`, tuple position and shape. Empty for single-name
    /// callbacks; only valid on a composite element ([`bind_elem`] Errs
    /// otherwise).
    pub leaves: &'a [(crate::BindId, usize, LeafShape)],
}

/// The shape of one `|(k, v)|` destructure leaf (see [`elem_leaves`]).
#[derive(Clone, Copy, Debug)]
pub enum LeafShape {
    /// A register scalar; nothing to drop.
    Scalar(PrimType),
    /// Owned ValArray bits.
    Composite,
    /// An owned `ArcStr`.
    String,
    /// An owned two-word Value, bound under the given local kind.
    Value(ValueLeafKind),
}

/// Which value-shape local kind a [`LeafShape::Value`] leaf binds as.
#[derive(Clone, Copy, Debug)]
pub enum ValueLeafKind {
    Variant,
    Nullable,
    Value,
}

/// A bound per-iteration element (see [`bind_elem`]). The owned kinds
/// must be moved into the output or dropped via [`drop_owned_elem`]
/// before the iteration ends.
pub(crate) enum BoundElem {
    Scalar {
        var: Variable,
        prim: PrimType,
    },
    /// Owned ValArray bits.
    Composite {
        var: Variable,
    },
    /// An owned `ArcStr` (its raw thin-pointer bits).
    String {
        var: Variable,
    },
    /// An owned two-word `(disc, payload)` Value.
    Value {
        disc: Variable,
        payload: Variable,
    },
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

/// Read and bind the `|(k, v)|` destructure leaves of a composite
/// `base_ptr`, each under its pattern `BindId`. Owned leaves are env
/// locals (a pending exit drops them) and are returned for the
/// normal-path [`drop_owned_leaves`]. Each leaf disc carries
/// `src_disc & mask`.
fn bind_leaves(
    cx: &mut BodyCx,
    base_ptr: ClifValue,
    src_disc: ClifValue,
    mask: i64,
    leaves: &[(crate::BindId, usize, LeafShape)],
) -> Result<Vec<BoundElem>> {
    let mut owned_leaves: Vec<BoundElem> = Vec::new();
    for (id, idx, shape) in leaves {
        let idx_c = cx.b.ins().iconst(types::I64, *idx as i64);
        let name: ArcStr =
            compact_str::format_compact!("__leaf{}", id.inner()).as_str().into();
        match shape {
            LeafShape::Scalar(prim) => {
                let get = cx.helper(valarray_get_helper(*prim)?)?;
                let call = cx.b.ins().call(get, &[base_ptr, idx_c]);
                let v = cx.b.inst_results(call)[0];
                let d = scalar_disc(cx.b, *prim);
                let d = carry_disc(cx, d, src_disc, mask);
                bind_local(cx, name, d, v, LocalKind::Scalar(*prim), Some(*id));
            }
            LeafShape::Composite => {
                let get = cx.helper("graphix_valarray_get_array")?;
                let call = cx.b.ins().call(get, &[base_ptr, idx_c]);
                let p = cx.b.inst_results(call)[0];
                let d = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
                let d = carry_disc(cx, d, src_disc, mask);
                let vv = bind_local(cx, name, d, p, LocalKind::Composite, Some(*id));
                owned_leaves.push(BoundElem::Composite { var: vv.payload });
            }
            LeafShape::String => {
                let get = cx.helper("graphix_valarray_get_arcstr")?;
                let call = cx.b.ins().call(get, &[base_ptr, idx_c]);
                let s = cx.b.inst_results(call)[0];
                let d = cx.b.ins().iconst(types::I64, value_disc::STRING);
                let d = carry_disc(cx, d, src_disc, mask);
                let vv = bind_local(cx, name, d, s, LocalKind::String, Some(*id));
                owned_leaves.push(BoundElem::String { var: vv.payload });
            }
            LeafShape::Value(vk) => {
                let get = cx.helper("graphix_valarray_get_value")?;
                let call = cx.b.ins().call(get, &[base_ptr, idx_c]);
                let (d, p) = {
                    let r = cx.b.inst_results(call);
                    (r[0], r[1])
                };
                let d = carry_disc(cx, d, src_disc, mask);
                let kind = match vk {
                    ValueLeafKind::Variant => LocalKind::Variant,
                    ValueLeafKind::Nullable => LocalKind::Nullable,
                    ValueLeafKind::Value => LocalKind::Value,
                };
                let disc = cx.b.declare_var(types::I64);
                cx.b.def_var(disc, d);
                let payload = cx.b.declare_var(types::I64);
                cx.b.def_var(payload, p);
                cx.env.bind(name, ValueVar { disc, payload }, kind, Some(*id));
                owned_leaves.push(BoundElem::Value { disc, payload });
            }
        }
    }
    Ok(owned_leaves)
}

/// Fetch element `i_now` of `arr_ptr` and bind it under `elem`, plus
/// its owned destructure leaves. The owned kinds are env locals, so a
/// mid-body pending exit drops them.
fn bind_elem(
    cx: &mut BodyCx,
    src_disc: ClifValue,
    arr_ptr: ClifValue,
    i_now: ClifValue,
    elem: &HofElem,
) -> Result<(BoundElem, Vec<BoundElem>)> {
    match kernel_abi::abi_kind(elem.typ) {
        Some(AbiKind::Scalar(prim)) => {
            if !elem.leaves.is_empty() {
                return Err(anyhow!(
                    "destructure leaves on a scalar HOF element — \
                     caller bug"
                ));
            }
            let get_helper = cx.helper(valarray_get_helper(prim)?)?;
            let call = cx.b.ins().call(get_helper, &[arr_ptr, i_now]);
            let elem_val = cx.b.inst_results(call)[0];
            let disc = scalar_disc(cx.b, prim);
            let disc = elem_disc(cx, disc, src_disc);
            let var = cx.b.declare_var(prim_to_clif(prim));
            cx.b.def_var(var, elem_val);
            let dv = cx.b.declare_var(types::I64);
            cx.b.def_var(dv, disc);
            cx.env.bind(
                elem.name.clone(),
                ValueVar { disc: dv, payload: var },
                LocalKind::Scalar(prim),
                elem.id,
            );
            Ok((BoundElem::Scalar { var, prim }, Vec::new()))
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let get_helper = cx.helper("graphix_valarray_get_array")?;
            let call = cx.b.ins().call(get_helper, &[arr_ptr, i_now]);
            let elem_ptr = cx.b.inst_results(call)[0];
            let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
            let disc = elem_disc(cx, disc, src_disc);
            let var = cx.b.declare_var(types::I64);
            cx.b.def_var(var, elem_ptr);
            let dv = cx.b.declare_var(types::I64);
            cx.b.def_var(dv, disc);
            cx.env.bind(
                elem.name.clone(),
                ValueVar { disc: dv, payload: var },
                LocalKind::Composite,
                elem.id,
            );
            let owned_leaves =
                bind_leaves(cx, elem_ptr, src_disc, TAINT | STALE, elem.leaves)?;
            Ok((BoundElem::Composite { var }, owned_leaves))
        }
        Some(AbiKind::String) => {
            if !elem.leaves.is_empty() {
                return Err(anyhow!(
                    "destructure leaves on a string HOF element — caller bug"
                ));
            }
            let get = cx.helper("graphix_valarray_get_arcstr")?;
            let call = cx.b.ins().call(get, &[arr_ptr, i_now]);
            let bits = cx.b.inst_results(call)[0];
            let disc = cx.b.ins().iconst(types::I64, value_disc::STRING);
            let disc = elem_disc(cx, disc, src_disc);
            let var = cx.b.declare_var(types::I64);
            cx.b.def_var(var, bits);
            let dv = cx.b.declare_var(types::I64);
            cx.b.def_var(dv, disc);
            cx.env.bind(
                elem.name.clone(),
                ValueVar { disc: dv, payload: var },
                LocalKind::String,
                elem.id,
            );
            Ok((BoundElem::String { var }, Vec::new()))
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            if !elem.leaves.is_empty() {
                return Err(anyhow!(
                    "destructure leaves on a value-shape HOF element — caller bug"
                ));
            }
            let get = cx.helper("graphix_valarray_get_value")?;
            let call = cx.b.ins().call(get, &[arr_ptr, i_now]);
            let (d, p) = {
                let r = cx.b.inst_results(call);
                (r[0], r[1])
            };
            let d = elem_disc(cx, d, src_disc);
            let disc = cx.b.declare_var(types::I64);
            cx.b.def_var(disc, d);
            let payload = cx.b.declare_var(types::I64);
            cx.b.def_var(payload, p);
            let kind = match kernel_abi::abi_kind(elem.typ) {
                Some(AbiKind::Variant) => LocalKind::Variant,
                Some(AbiKind::Nullable) => LocalKind::Nullable,
                _ => LocalKind::Value,
            };
            cx.env.bind(elem.name.clone(), ValueVar { disc, payload }, kind, elem.id);
            Ok((BoundElem::Value { disc, payload }, Vec::new()))
        }
        _ => Err(anyhow!("HOF element shape not supported by the JIT loop scaffolds")),
    }
}

/// Drop one iteration's owned leaves once the body or predicate has
/// consumed them (a body result may borrow a leaf until the push
/// copies it). Leaves are never moved into the output.
fn drop_owned_leaves(cx: &mut BodyCx, leaves: &[BoundElem]) -> Result<()> {
    for l in leaves {
        drop_owned_elem(cx, l)?;
    }
    Ok(())
}

/// Drop an owned per-iteration element (no-op for scalars).
fn drop_owned_elem(cx: &mut BodyCx, elem: &BoundElem) -> Result<()> {
    match elem {
        BoundElem::Scalar { .. } => {}
        BoundElem::Composite { var } => {
            let drop = cx.helper("graphix_valarray_drop")?;
            let v = cx.b.use_var(*var);
            cx.b.ins().call(drop, &[v]);
        }
        BoundElem::String { var } => {
            let drop = cx.helper("graphix_arcstr_drop")?;
            let v = cx.b.use_var(*var);
            cx.b.ins().call(drop, &[v]);
        }
        BoundElem::Value { disc, payload } => {
            let drop = cx.helper("graphix_value_drop")?;
            let d = cx.b.use_var(*disc);
            let p = cx.b.use_var(*payload);
            cx.b.ins().call(drop, &[d, p]);
        }
    }
    Ok(())
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
        let drop_helper = cx.helper("graphix_valarray_drop")?;
        cx.b.ins().call(drop_helper, &[arr.ptr]);
        cx.ctx.owned_input_stack.borrow_mut().pop();
    }
    Ok(())
}

fn register_hof_buf(b: &mut FunctionBuilder, ctx: &LowerCtx, buf: ClifValue) {
    let buf_var = b.declare_var(types::I64);
    b.def_var(buf_var, buf);
    ctx.value_buf_stack.borrow_mut().push(buf_var);
}

fn unregister_hof_buf(ctx: &LowerCtx) {
    ctx.value_buf_stack.borrow_mut().pop();
}

/// `len = valarray_len(arr_ptr)`.
fn input_len(cx: &mut BodyCx, arr_ptr: ClifValue) -> Result<ClifValue> {
    let len_helper = cx.helper("graphix_valarray_len")?;
    let call = cx.b.ins().call(len_helper, &[arr_ptr]);
    Ok(cx.b.inst_results(call)[0])
}

fn input_sized_buf(
    cx: &mut BodyCx,
    arr_ptr: ClifValue,
) -> Result<(ClifValue, ClifValue)> {
    let len = input_len(cx, arr_ptr)?;
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let call = cx.b.ins().call(buf_new, &[len]);
    let buf = cx.b.inst_results(call)[0];
    register_hof_buf(cx.b, cx.ctx, buf);
    Ok((len, buf))
}

/// Declare the I64 loop counter and zero it.
fn init_counter(cx: &mut BodyCx) -> Variable {
    let i_var = cx.b.declare_var(types::I64);
    let zero = cx.b.ins().iconst(types::I64, 0);
    cx.b.def_var(i_var, zero);
    i_var
}

/// Emit the `i < len` header test. The builder must be positioned in
/// the header block.
fn emit_loop_header(
    cx: &mut BodyCx,
    i_var: Variable,
    len: ClifValue,
    loop_body: Block,
    loop_exit: Block,
) {
    let i_cur = cx.b.use_var(i_var);
    let cond = cx.b.ins().icmp(IntCC::SignedLessThan, i_cur, len);
    cx.b.ins().brif(cond, loop_body, &[], loop_exit, &[]);
}

/// `i += 1; jump loop_header`. `i_now` is the caller's already-read
/// counter value; its read position differs per scaffold.
fn emit_increment(
    cx: &mut BodyCx,
    i_var: Variable,
    i_now: ClifValue,
    loop_header: Block,
) {
    let one = cx.b.ins().iconst(types::I64, 1);
    let i_next = cx.b.ins().iadd(i_now, one);
    cx.b.def_var(i_var, i_next);
    cx.b.ins().jump(loop_header, &[]);
}

fn finalize_buf(cx: &mut BodyCx, buf: ClifValue) -> Result<ClifValue> {
    unregister_hof_buf(cx.ctx);
    let finalize = cx.helper("graphix_valarray_finalize")?;
    let call = cx.b.ins().call(finalize, &[buf]);
    Ok(cx.b.inst_results(call)[0])
}

/// Push a compiled field into a `graphix_value_buf`, choosing the
/// helper by shape and `src`. Strings ignore `src`: string SSA is
/// always owned and `_push_string` consumes it (`_push_arcstr` would
/// dereference the bits as an `ArcStr` struct — UB). A tainted field
/// does not abort; the caller folds its taint into the result.
pub fn push_field(
    cx: &mut BodyCx,
    buf: ClifValue,
    cv: CompiledExpr,
    typ: &Type,
    src: CompositeSource,
) -> Result<()> {
    let helper_name: &str = match kernel_abi::abi_kind(typ) {
        Some(AbiKind::Scalar(p)) => value_buf_push_helper(p)?,
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
    let push = cx.helper(helper_name)?;
    // Pushing a tainted field is safe: every push helper masks the tag
    // byte before cloning the value.
    if kernel_abi::is_value_shape(typ) {
        cx.b.ins().call(push, &[buf, cv.disc, cv.payload]);
    } else {
        cx.b.ins().call(push, &[buf, cv.payload]);
    }
    Ok(())
}

/// Loop-carried slot-flags accumulator: per-slot TAINT is OR-reduced
/// and per-slot STALE is AND-reduced across the loop. A tainted slot
/// taints the whole HOF result but never the kernel. The loop fires
/// iff a loop input fired and the evaluation produced an event
/// ([`Self::apply`]).
pub struct SlotFlags {
    taint: Variable,
    stale: Variable,
    len: Option<ClifValue>,
    /// The result's own STALE is an additional firing source beside the
    /// slots word (fold: the acc carry alone covers zero iterations).
    result_also_fires: bool,
    src_invariant: bool,
    /// Pass-through kinds (filter, find) read the source elements into
    /// the result, so a same-length source refresh with quiet slots
    /// still fires.
    pass_through: bool,
    /// The collection callsite this loop lowers: the key a nested loop's
    /// prev-length word is chained under in the enclosing frame.
    site_id: Option<crate::expr::ExprId>,
}

impl SlotFlags {
    pub fn new(cx: &mut BodyCx) -> Self {
        let taint = cx.b.declare_var(types::I64);
        let z = cx.b.ins().iconst(types::I64, 0);
        cx.b.def_var(taint, z);
        let stale = cx.b.declare_var(types::I64);
        let st = cx.b.ins().iconst(types::I64, STALE);
        cx.b.def_var(stale, st);
        SlotFlags {
            taint,
            stale,
            len: None,
            result_also_fires: false,
            src_invariant: false,
            pass_through: false,
            site_id: cx.collection_site(),
        }
    }

    pub fn set_src_invariant(&mut self) {
        self.src_invariant = true;
    }

    pub fn result_also_fires(&mut self) {
        self.result_also_fires = true;
    }

    pub fn set_pass_through(&mut self) {
        self.pass_through = true;
    }

    /// Record the source's element count for `apply`'s empty-source term.
    pub fn set_len(&mut self, len: ClifValue) {
        self.len = Some(len);
    }

    /// Fold one slot's disc into the accumulators.
    pub fn fold(&self, cx: &mut BodyCx, disc: ClifValue) {
        let cur = cx.b.use_var(self.taint);
        let t = cx.b.ins().band_imm(disc, TAINT);
        let n = cx.b.ins().bor(cur, t);
        cx.b.def_var(self.taint, n);
        self.fold_stale(cx, disc);
    }

    /// Fold one slot's STALE bit alone: in a fold TAINT rides only the
    /// acc carry, so an acc-ignoring callback recovers.
    pub fn fold_stale(&self, cx: &mut BodyCx, disc: ClifValue) {
        let cur = cx.b.use_var(self.stale);
        let sb = cx.b.ins().band_imm(disc, STALE);
        let n = cx.b.ins().band(cur, sb);
        cx.b.def_var(self.stale, n);
    }

    /// Fold the accumulated flags and the source discs into `r`'s disc.
    /// Uses the exact firing rule when a prev-length word is available
    /// (a state word, a chain word, or a call-site word); a nested loop
    /// over a variant-length source without a chain word falls back to
    /// the conservative source-or-slot rule.
    pub fn apply(
        &self,
        cx: &mut BodyCx,
        mut r: CompiledExpr,
        srcs: &[ClifValue],
    ) -> CompiledExpr {
        let t = cx.b.use_var(self.taint);
        r.disc = cx.b.ins().bor(r.disc, t);
        let slots_word = cx.b.use_var(self.stale);
        let mut src_word = cx.b.ins().iconst(types::I64, STALE);
        let mut src_taint = cx.b.ins().iconst(types::I64, 0);
        for s in srcs {
            let ss = cx.b.ins().band_imm(*s, STALE);
            src_word = cx.b.ins().band(src_word, ss);
            let st = cx.b.ins().band_imm(*s, TAINT);
            src_taint = cx.b.ins().bor(src_taint, st);
        }
        r.disc = cx.b.ins().bor(r.disc, src_taint);
        let fired_word = if self.result_also_fires {
            let rs = cx.b.ins().band_imm(r.disc, STALE);
            cx.b.ins().band(rs, slots_word)
        } else {
            slots_word
        };
        r.disc = cx.b.ins().band_imm(r.disc, !STALE);
        enum PrevLen {
            State(i32),
            Chain(SelWord),
            Site(i32),
        }
        let claim = match self.len {
            None => None,
            Some(_) => {
                let state = if self.src_invariant {
                    cx.claim_state_word_loop_invariant()
                } else {
                    cx.claim_state_word()
                };
                match state {
                    Some(off) => Some(PrevLen::State(off)),
                    // Nested loop: a per-enclosing-slot word from the
                    // enclosing frame's chain.
                    None => match self.site_id.and_then(|id| cx.slot_select_word(id)) {
                        Some(w) => Some(PrevLen::Chain(w)),
                        // A call-site word is exact only when the length
                        // is per-instance; a variant length under enclosing
                        // loops would alias iterations.
                        None if cx.ctx.loop_depth.get() == 0 || self.src_invariant => {
                            cx.claim_site_word().map(PrevLen::Site)
                        }
                        None => None,
                    },
                }
            }
        };
        let stale = match claim {
            None => self.conservative_stale(cx, fired_word, src_word),
            Some(PrevLen::State(off)) => {
                let sp = cx.state_ptr();
                let addr = cx.b.ins().iadd_imm(sp, off as i64);
                self.exact_stale(cx, addr, fired_word, src_word, src_taint)
            }
            Some(PrevLen::Chain(SelWord::Sure(addr))) => {
                self.exact_stale(cx, addr, fired_word, src_word, src_taint)
            }
            Some(PrevLen::Chain(SelWord::Guarded { base, addr })) => {
                self.guarded_exact_stale(cx, base, addr, fired_word, src_word, src_taint)
            }
            // The site block base may be 0 on a recursive back-edge.
            Some(PrevLen::Site(off)) => {
                let base = cx.site_ptr();
                let addr = cx.b.ins().iadd_imm(base, off as i64);
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
    ) -> ClifValue {
        let has = cx.b.ins().icmp_imm(IntCC::NotEqual, base, 0);
        let exact_bl = cx.b.create_block();
        let cons_bl = cx.b.create_block();
        let merge = cx.b.create_block();
        cx.b.append_block_param(merge, types::I64);
        cx.b.ins().brif(has, exact_bl, &[], cons_bl, &[]);
        cx.b.seal_block(exact_bl);
        cx.b.seal_block(cons_bl);
        cx.b.switch_to_block(exact_bl);
        let stale = self.exact_stale(cx, addr, fired_word, src_word, src_taint);
        cx.b.ins().jump(merge, &[BlockArg::Value(stale)]);
        cx.b.switch_to_block(cons_bl);
        let stale = self.conservative_stale(cx, fired_word, src_word);
        cx.b.ins().jump(merge, &[BlockArg::Value(stale)]);
        cx.b.seal_block(merge);
        cx.b.switch_to_block(merge);
        cx.b.block_params(merge)[0]
    }

    /// The exact firing rule's STALE contribution: fires iff resized, a
    /// slot fired, or the source fired empty, against the prev-length
    /// word at `addr` (stored `len + 1`; 0 = no previous observation).
    fn exact_stale(
        &self,
        cx: &mut BodyCx,
        addr: ClifValue,
        fired_word: ClifValue,
        src_word: ClifValue,
        src_taint: ClifValue,
    ) -> ClifValue {
        let len = self.len.expect("exact_stale requires a recorded len");
        let stored = cx.b.ins().load(types::I64, MemFlags::trusted(), addr, 0);
        let lenp1 = cx.b.ins().iadd_imm(len, 1);
        let valid = cx.b.ins().icmp_imm(IntCC::Equal, src_taint, 0);
        // A tainted source is never a resize and leaves the stored word
        // untouched (the node-walk computes no length for it).
        let resized = cx.b.ins().icmp(IntCC::NotEqual, stored, lenp1);
        let resized = cx.b.ins().band(resized, valid);
        let recorded = cx.b.ins().select(valid, lenp1, stored);
        cx.b.ins().store(MemFlags::trusted(), recorded, addr, 0);
        let slot_fired = cx.b.ins().icmp_imm(IntCC::Equal, fired_word, 0);
        let src_fired = cx.b.ins().icmp_imm(IntCC::Equal, src_word, 0);
        let empty = cx.b.ins().icmp_imm(IntCC::Equal, len, 0);
        let src_empty = cx.b.ins().band(src_fired, empty);
        let fires = cx.b.ins().bor(resized, slot_fired);
        let fires = cx.b.ins().bor(fires, src_empty);
        // Pass-through kinds fire on any source fire.
        let fires =
            if self.pass_through { cx.b.ins().bor(fires, src_fired) } else { fires };
        let quiet = cx.b.ins().iconst(types::I64, STALE);
        let zero = cx.b.ins().iconst(types::I64, 0);
        cx.b.ins().select(fires, zero, quiet)
    }

    /// The conservative STALE contribution (no prev-length word): fired
    /// when a slot or the source fired.
    fn conservative_stale(
        &self,
        cx: &mut BodyCx,
        fired_word: ClifValue,
        src_word: ClifValue,
    ) -> ClifValue {
        if self.result_also_fires {
            fired_word
        } else {
            cx.b.ins().band(fired_word, src_word)
        }
    }
}

pub fn emit_init_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    n_raw: ClifValue,
    n_disc: ClifValue,
    n_prim: PrimType,
    idx_name: &ArcStr,
    idx_id: Option<crate::BindId>,
    out_typ: &Type,
    out_src: CompositeSource,
    sel_sites: &[ExprId],
    mut body: F,
) -> Result<(ClifValue, SlotFlags, ClifValue)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let mut flags = SlotFlags::new(cx);
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let n_widened = widen_to_i64(cx.b, n_raw, n_prim)?;
    let zero = cx.b.ins().iconst(types::I64, 0);
    let is_negative = cx.b.ins().icmp(IntCC::SignedLessThan, n_widened, zero);
    let n = cx.b.ins().select(is_negative, zero, n_widened);
    let max = cx.b.ins().iconst(types::I64, crate::node::MAX_ARRAY_INIT_LEN);
    let oversize = cx.b.ins().icmp(IntCC::SignedGreaterThan, n, max);
    // An over-limit count is bottom: taint the count's disc (so the
    // stored length and slot tables are kept, not reset to 0) but keep
    // its STALE bit, since a count that fired over the limit is a fresh
    // bottom. The loop bound clamps to 0.
    let forced_bits = cx.b.ins().iconst(types::I64, TAINT);
    let forced_disc = cx.b.ins().bor(n_disc, forced_bits);
    let n_disc = cx.b.ins().select(oversize, forced_disc, n_disc);
    let stale = cx.b.ins().iconst(types::I64, STALE);
    let tainted = cx.b.ins().iconst(types::I64, TAINT | STALE);
    let disc = cx.b.ins().select(oversize, tainted, stale);
    flags.fold(cx, disc);
    let n = cx.b.ins().select(oversize, zero, n);
    flags.set_len(n);
    let call = cx.b.ins().call(buf_new, &[n]);
    let buf = cx.b.inst_results(call)[0];
    register_hof_buf(cx.b, cx.ctx, buf);
    let i_var = init_counter(cx);
    cx.open_slot_tables(sel_sites, n, n_disc, i_var)?;
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, n, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let mark = cx.env.mark();
    let idx_disc = scalar_disc(cx.b, PrimType::I64);
    let idx_disc = elem_disc(cx, idx_disc, n_disc);
    let idx_disc_var = cx.b.declare_var(types::I64);
    cx.b.def_var(idx_disc_var, idx_disc);
    cx.enter_loop();
    bind_scalar_var_with_disc(
        cx,
        idx_name.clone(),
        PrimType::I64,
        i_var,
        idx_disc_var,
        idx_id,
    );
    let value = body(cx);
    cx.exit_loop();
    cx.close_slot_tables();
    let value = value?;
    flags.fold(cx, value.disc);
    push_field(cx, buf, value, out_typ, out_src)?;
    cx.env.truncate(mark);
    let i = cx.b.use_var(i_var);
    emit_increment(cx, i_var, i, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    cx.emit_slot_truncates()?;
    Ok((finalize_buf(cx, buf)?, flags, n_disc))
}

pub fn emit_map_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    out_typ: &Type,
    out_src: CompositeSource,
    sel_sites: &[ExprId],
    mut body: F,
) -> Result<(ClifValue, SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let mut flags = SlotFlags::new(cx);
    adopt_owned_src(cx, &arr);
    let (len, buf) = input_sized_buf(cx, arr.ptr)?;
    flags.set_len(len);
    let i_var = init_counter(cx);
    cx.open_slot_tables(sel_sites, len, arr.disc, i_var)?;
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let i = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    cx.enter_loop();
    let (bound, leaves) = bind_elem(cx, arr.disc, arr.ptr, i, elem)?;
    let value = body(cx);
    cx.exit_loop();
    cx.close_slot_tables();
    let value = value?;
    flags.fold(cx, value.disc);
    push_field(cx, buf, value, out_typ, out_src)?;
    drop_owned_leaves(cx, &leaves)?;
    drop_owned_elem(cx, &bound)?;
    cx.env.truncate(mark);
    emit_increment(cx, i_var, i, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    cx.emit_slot_truncates()?;
    let result = finalize_buf(cx, buf)?;
    drop_owned_src(cx, &arr)?;
    Ok((result, flags))
}

pub fn emit_filter_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    sel_sites: &[ExprId],
    mut predicate: F,
) -> Result<(ClifValue, SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let mut flags = SlotFlags::new(cx);
    flags.set_pass_through();
    let owns_drop = !matches!(kernel_abi::abi_kind(elem.typ), Some(AbiKind::Scalar(_)));
    adopt_owned_src(cx, &arr);
    let (len, buf) = input_sized_buf(cx, arr.ptr)?;
    flags.set_len(len);
    let i_var = init_counter(cx);
    cx.open_slot_tables(sel_sites, len, arr.disc, i_var)?;
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let push_block = cx.b.create_block();
    let advance = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    let drop_block = owns_drop.then(|| cx.b.create_block());
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let i = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    cx.enter_loop();
    let (bound, leaves) = bind_elem(cx, arr.disc, arr.ptr, i, elem)?;
    let keep = predicate(cx);
    cx.exit_loop();
    cx.close_slot_tables();
    let keep = keep?;
    flags.fold(cx, keep.disc);
    drop_owned_leaves(cx, &leaves)?;
    cx.env.truncate(mark);
    let not_kept = drop_block.unwrap_or(advance);
    cx.b.ins().brif(keep.payload, push_block, &[], not_kept, &[]);
    cx.b.switch_to_block(push_block);
    match &bound {
        BoundElem::Scalar { prim, var } => {
            let push = cx.helper(value_buf_push_helper(*prim)?)?;
            let value = cx.b.use_var(*var);
            cx.b.ins().call(push, &[buf, value]);
        }
        BoundElem::Composite { var } => {
            let push = cx.helper("graphix_value_buf_push_array")?;
            let value = cx.b.use_var(*var);
            cx.b.ins().call(push, &[buf, value]);
        }
        BoundElem::String { var } => {
            let push = cx.helper("graphix_value_buf_push_string")?;
            let value = cx.b.use_var(*var);
            cx.b.ins().call(push, &[buf, value]);
        }
        BoundElem::Value { disc, payload } => {
            let push = cx.helper("graphix_value_buf_push_value")?;
            let disc = cx.b.use_var(*disc);
            let payload = cx.b.use_var(*payload);
            cx.b.ins().call(push, &[buf, disc, payload]);
        }
    }
    cx.b.ins().jump(advance, &[]);
    cx.b.seal_block(push_block);
    if let Some(drop_block) = drop_block {
        cx.b.switch_to_block(drop_block);
        drop_owned_elem(cx, &bound)?;
        cx.b.ins().jump(advance, &[]);
        cx.b.seal_block(drop_block);
    }
    cx.b.switch_to_block(advance);
    emit_increment(cx, i_var, i, loop_header);
    cx.b.seal_block(advance);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    cx.emit_slot_truncates()?;
    let result = finalize_buf(cx, buf)?;
    drop_owned_src(cx, &arr)?;
    Ok((result, flags))
}

pub fn emit_filter_map_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    out_elem: &Type,
    out_src: CompositeSource,
    sel_sites: &[ExprId],
    mut body: F,
) -> Result<(ClifValue, SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let mut flags = SlotFlags::new(cx);
    adopt_owned_src(cx, &arr);
    let (len, buf) = input_sized_buf(cx, arr.ptr)?;
    flags.set_len(len);
    let i_var = init_counter(cx);
    cx.open_slot_tables(sel_sites, len, arr.disc, i_var)?;
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let push_block = cx.b.create_block();
    let advance = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let i = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    cx.enter_loop();
    let (bound, leaves) = bind_elem(cx, arr.disc, arr.ptr, i, elem)?;
    let value = body(cx);
    cx.exit_loop();
    cx.close_slot_tables();
    let value = value?;
    cx.env.truncate(mark);
    flags.fold(cx, value.disc);
    let disc = clean_disc(cx.b, value.disc);
    let is_null = cx.b.ins().icmp_imm(IntCC::Equal, disc, value_disc::NULL);
    cx.b.ins().brif(is_null, advance, &[], push_block, &[]);
    cx.b.switch_to_block(push_block);
    // The result is always the callback's 2-word Nullable-shaped Value,
    // never an unwrapped element (a `[T, Error]` result may hold an
    // Error where the arm expects T): push it as a value, bit-for-bit.
    match kernel_abi::abi_kind(out_elem) {
        Some(
            AbiKind::Scalar(_)
            | AbiKind::Array
            | AbiKind::Tuple
            | AbiKind::Struct
            | AbiKind::String
            | AbiKind::Variant
            | AbiKind::Nullable
            | AbiKind::Value,
        ) => {
            let helper = match out_src {
                CompositeSource::Owned => "graphix_value_buf_push_value",
                CompositeSource::Borrowed => "graphix_value_buf_push_value_borrowed",
            };
            let push = cx.helper(helper)?;
            cx.b.ins().call(push, &[buf, value.disc, value.payload]);
        }
        Some(AbiKind::Unit | AbiKind::Null) | None => {
            return Err(anyhow!("filter_map output element is not representable"));
        }
    }
    cx.b.ins().jump(advance, &[]);
    cx.b.seal_block(push_block);
    cx.b.switch_to_block(advance);
    drop_owned_leaves(cx, &leaves)?;
    drop_owned_elem(cx, &bound)?;
    emit_increment(cx, i_var, i, loop_header);
    cx.b.seal_block(advance);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    cx.emit_slot_truncates()?;
    let result = finalize_buf(cx, buf)?;
    drop_owned_src(cx, &arr)?;
    Ok((result, flags))
}

/// How a flat_map body result splices into the output buf: an array
/// result extends via its owned ValArray ptr; a list result walks the
/// cons chain (a non-list value pushes as a single element).
#[derive(Clone, Copy)]
pub enum FlatMapExtend {
    Array,
    List,
}

pub fn emit_flat_map_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    extend_kind: FlatMapExtend,
    sel_sites: &[ExprId],
    mut body: F,
) -> Result<(ClifValue, SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let mut flags = SlotFlags::new(cx);
    let extend = cx.helper(match extend_kind {
        FlatMapExtend::Array => "graphix_value_buf_extend_from_array",
        FlatMapExtend::List => "graphix_value_buf_extend_from_list",
    })?;
    adopt_owned_src(cx, &arr);
    let (len, buf) = input_sized_buf(cx, arr.ptr)?;
    flags.set_len(len);
    let i_var = init_counter(cx);
    cx.open_slot_tables(sel_sites, len, arr.disc, i_var)?;
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let i = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    cx.enter_loop();
    let (bound, leaves) = bind_elem(cx, arr.disc, arr.ptr, i, elem)?;
    let value = body(cx);
    cx.exit_loop();
    cx.close_slot_tables();
    let value = value?;
    flags.fold(cx, value.disc);
    drop_owned_leaves(cx, &leaves)?;
    drop_owned_elem(cx, &bound)?;
    cx.env.truncate(mark);
    match extend_kind {
        FlatMapExtend::Array => {
            cx.b.ins().call(extend, &[buf, value.payload]);
        }
        FlatMapExtend::List => {
            cx.b.ins().call(extend, &[buf, value.disc, value.payload]);
        }
    }
    emit_increment(cx, i_var, i, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    cx.emit_slot_truncates()?;
    let result = finalize_buf(cx, buf)?;
    drop_owned_src(cx, &arr)?;
    Ok((result, flags))
}

/// Destructure-leaf shapes for a `|(k, v)|`-style pattern over a
/// tuple-typed value: per bound leaf its pattern `BindId`, tuple
/// position and [`LeafShape`]. `None` when the type isn't a tuple or a
/// bound position has no kernel shape (the caller node-walks); empty
/// binds give `Some(empty)`.
pub fn elem_leaves(
    in_elem: &Type,
    elem_binds: &[(crate::BindId, usize)],
) -> Option<Vec<(crate::BindId, usize, LeafShape)>> {
    use kernel_abi::AbiKind;
    if elem_binds.is_empty() {
        return Some(Vec::new());
    }
    let Type::Tuple(ts) = in_elem else { return None };
    elem_binds
        .iter()
        .map(|(id, i)| {
            let shape = match kernel_abi::abi_kind(ts.get(*i)?) {
                Some(AbiKind::Scalar(p)) => LeafShape::Scalar(p),
                Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                    LeafShape::Composite
                }
                Some(AbiKind::String) => LeafShape::String,
                Some(AbiKind::Variant) => LeafShape::Value(ValueLeafKind::Variant),
                Some(AbiKind::Nullable) => LeafShape::Value(ValueLeafKind::Nullable),
                Some(AbiKind::Value) => LeafShape::Value(ValueLeafKind::Value),
                Some(AbiKind::Unit | AbiKind::Null) | None => return None,
            };
            Some((*id, *i, shape))
        })
        .collect()
}

/// The fold accumulator's shape: how the loop-carried value is held,
/// made owned, and dropped when replaced.
pub enum FoldAcc<'a> {
    /// A register scalar. Owns nothing.
    Scalar(PrimType),
    /// Owned ValArray bits. The loop owns the current acc: each
    /// iteration the body's result is made owned per `body_src` and the
    /// old acc dropped. `leaves` are the acc pattern's destructure
    /// leaves, re-read off the current acc each iteration.
    Composite {
        init_src: CompositeSource,
        body_src: CompositeSource,
        leaves: &'a [(crate::BindId, usize, LeafShape)],
    },
    /// An owned `ArcStr`. String reads always clone, so results are
    /// already owned; the old acc still drops when replaced.
    Str,
    /// An owned two-word Value. Its real value disc varies per
    /// iteration (a nullable acc alternates Null and its value), so the
    /// disc Variable carries the whole disc, never a re-based constant.
    Value { init_src: CompositeSource, body_src: CompositeSource, kind: ValueLeafKind },
}

impl FoldAcc<'_> {
    fn local_kind(&self) -> LocalKind {
        match self {
            FoldAcc::Scalar(p) => LocalKind::Scalar(*p),
            FoldAcc::Composite { .. } => LocalKind::Composite,
            FoldAcc::Str => LocalKind::String,
            FoldAcc::Value { kind, .. } => match kind {
                ValueLeafKind::Variant => LocalKind::Variant,
                ValueLeafKind::Nullable => LocalKind::Nullable,
                ValueLeafKind::Value => LocalKind::Value,
            },
        }
    }

    /// The clean disc for the carried acc shape; each carry re-bases on
    /// it so only TAINT and STALE ride. Unreachable for
    /// [`FoldAcc::Value`], whose disc carries whole.
    fn base_disc(&self, cx: &mut BodyCx) -> ClifValue {
        match self {
            FoldAcc::Scalar(p) => scalar_disc(cx.b, *p),
            FoldAcc::Composite { .. } => cx.b.ins().iconst(types::I64, value_disc::ARRAY),
            FoldAcc::Str => cx.b.ins().iconst(types::I64, value_disc::STRING),
            FoldAcc::Value { .. } => {
                unreachable!("a Value acc's disc is carried whole, never re-based")
            }
        }
    }

    /// Drop the old carried acc when a new one replaces it. Emits nothing
    /// for a scalar, not even the `use_var`.
    fn drop_old(
        &self,
        cx: &mut BodyCx,
        acc_var: Variable,
        acc_disc_var: Variable,
    ) -> Result<()> {
        match self {
            FoldAcc::Scalar(_) => {}
            FoldAcc::Composite { .. } => {
                let drop = cx.helper("graphix_valarray_drop")?;
                let old = cx.b.use_var(acc_var);
                cx.b.ins().call(drop, &[old]);
            }
            FoldAcc::Str => {
                let drop = cx.helper("graphix_arcstr_drop")?;
                let old = cx.b.use_var(acc_var);
                cx.b.ins().call(drop, &[old]);
            }
            FoldAcc::Value { .. } => {
                let drop = cx.helper("graphix_value_drop")?;
                let old_disc = cx.b.use_var(acc_disc_var);
                let old_pay = cx.b.use_var(acc_var);
                cx.b.ins().call(drop, &[old_disc, old_pay]);
            }
        }
        Ok(())
    }

    /// The next carried disc: the whole disc minus the other tag bits for
    /// [`FoldAcc::Value`], re-based on the shape constant otherwise.
    fn carry_disc(&self, cx: &mut BodyCx, from_disc: ClifValue) -> ClifValue {
        match self {
            FoldAcc::Value { .. } => {
                const KEEP: i64 = !(0xFFu64 << 56) as i64 | super::TAINT | super::STALE;
                cx.b.ins().band_imm(from_disc, KEEP)
            }
            _ => {
                let base = self.base_disc(cx);
                let t = cx.b.ins().band_imm(from_disc, TAINT | STALE);
                cx.b.ins().bor(base, t)
            }
        }
    }
}

/// `array::fold(arr, init, |acc, x| body)`. Each iteration binds the
/// acc first, then the element (the order is load-bearing); the body's
/// result becomes the next acc.
pub fn emit_fold_loop<'a, 'f, 'c, I, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    acc: FoldAcc,
    acc_name: &ArcStr,
    acc_id: Option<crate::BindId>,
    elem: &HofElem,
    sel_sites: &[ExprId],
    init: I,
    mut body: F,
) -> Result<(CompiledExpr, SlotFlags)>
where
    I: FnOnce(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    adopt_owned_src(cx, &arr);
    let len = input_len(cx, arr.ptr)?;
    let acc_var = cx.b.declare_var(match &acc {
        FoldAcc::Scalar(p) => prim_to_clif(*p),
        FoldAcc::Composite { .. } | FoldAcc::Str | FoldAcc::Value { .. } => types::I64,
    });
    // The acc's TAINT and STALE are loop-carried in its own disc; STALE
    // must ride or an acc-consuming body reads FIRED on every run.
    let acc_disc_var = cx.b.declare_var(types::I64);
    let mut taint = SlotFlags::new(cx);
    taint.set_len(len);
    taint.result_also_fires();
    let init_cv = init(cx)?;
    // A pointer-shaped acc is loop-owned from the start: a borrowed init
    // clones here.
    let (init_pay, init_disc) = match &acc {
        FoldAcc::Composite { init_src, .. } => {
            (ensure_owned_composite_src(cx, *init_src, init_cv.payload)?, init_cv.disc)
        }
        FoldAcc::Scalar(_) | FoldAcc::Str => (init_cv.payload, init_cv.disc),
        FoldAcc::Value { init_src, .. } => {
            let (d, p) =
                ensure_owned_value_src(cx, *init_src, init_cv.disc, init_cv.payload)?;
            (p, d)
        }
    };
    cx.b.def_var(acc_var, init_pay);
    let d0 = acc.carry_disc(cx, init_disc);
    cx.b.def_var(acc_disc_var, d0);
    let i_var = init_counter(cx);
    cx.open_slot_tables(sel_sites, len, arr.disc, i_var)?;
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    let mark = cx.env.mark();
    cx.enter_loop();
    // The acc binds before the interrupt poll so the poll's abort cleanup
    // drops an owned acc.
    cx.env.bind(
        acc_name.clone(),
        ValueVar { disc: acc_disc_var, payload: acc_var },
        acc.local_kind(),
        acc_id,
    );
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let i_now = cx.b.use_var(i_var);
    let (bound, owned_leaves) = bind_elem(cx, arr.disc, arr.ptr, i_now, elem)?;
    // Acc leaves carry the acc's loop-carried TAINT|STALE; unlike an
    // element, the acc can be tainted.
    let acc_owned_leaves = match &acc {
        FoldAcc::Composite { leaves, .. } if !leaves.is_empty() => {
            let acc_ptr = cx.b.use_var(acc_var);
            let acc_disc = cx.b.use_var(acc_disc_var);
            bind_leaves(cx, acc_ptr, acc_disc, TAINT | STALE, leaves)?
        }
        _ => Vec::new(),
    };
    let new_acc = body(cx);
    cx.exit_loop();
    cx.close_slot_tables();
    let new_acc = new_acc?;
    // Make the new acc owned before anything drops: a borrowed body
    // result (`|acc, x| acc`) may alias the old acc, an element, or a leaf.
    let (new_pay, new_disc) = match &acc {
        FoldAcc::Composite { body_src, .. } => {
            (ensure_owned_composite_src(cx, *body_src, new_acc.payload)?, new_acc.disc)
        }
        FoldAcc::Scalar(_) | FoldAcc::Str => (new_acc.payload, new_acc.disc),
        FoldAcc::Value { body_src, .. } => {
            let (d, p) =
                ensure_owned_value_src(cx, *body_src, new_acc.disc, new_acc.payload)?;
            (p, d)
        }
    };
    acc.drop_old(cx, acc_var, acc_disc_var)?;
    drop_owned_leaves(cx, &acc_owned_leaves)?;
    drop_owned_leaves(cx, &owned_leaves)?;
    drop_owned_elem(cx, &bound)?;
    cx.env.truncate(mark);
    // Each body evaluation's STALE folds into the firing flags (a
    // mid-chain body that consumed a fired acc fires the fold even if
    // the final carry is stale); TAINT travels only the acc carry.
    taint.fold_stale(cx, new_disc);
    cx.b.def_var(acc_var, new_pay);
    let d = acc.carry_disc(cx, new_disc);
    cx.b.def_var(acc_disc_var, d);
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    cx.emit_slot_truncates()?;
    drop_owned_src(cx, &arr)?;
    let payload = cx.b.use_var(acc_var);
    let disc = cx.b.use_var(acc_disc_var);
    Ok((CompiledExpr::new(disc, payload), taint))
}

pub fn emit_find_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    sel_sites: &[ExprId],
    mut predicate: F,
) -> Result<((ClifValue, ClifValue), SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let mut flags = SlotFlags::new(cx);
    flags.set_pass_through();
    adopt_owned_src(cx, &arr);
    let len = input_len(cx, arr.ptr)?;
    flags.set_len(len);
    let found_var = cx.b.declare_var(types::I8);
    let zero8 = cx.b.ins().iconst(types::I8, 0);
    cx.b.def_var(found_var, zero8);
    let result_disc_var = cx.b.declare_var(types::I64);
    let null_disc = cx.b.ins().iconst(types::I64, value_disc::NULL);
    cx.b.def_var(result_disc_var, null_disc);
    let result_payload_var = cx.b.declare_var(types::I64);
    let zero64 = cx.b.ins().iconst(types::I64, 0);
    cx.b.def_var(result_payload_var, zero64);
    let i_var = init_counter(cx);
    cx.open_slot_tables(sel_sites, len, arr.disc, i_var)?;
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let take_block = cx.b.create_block();
    let discard = cx.b.create_block();
    let advance = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let i = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    cx.enter_loop();
    let (bound, leaves) = bind_elem(cx, arr.disc, arr.ptr, i, elem)?;
    let keep = predicate(cx);
    cx.exit_loop();
    cx.close_slot_tables();
    let keep = keep?;
    flags.fold(cx, keep.disc);
    drop_owned_leaves(cx, &leaves)?;
    cx.env.truncate(mark);
    let not_found = {
        let found = cx.b.use_var(found_var);
        cx.b.ins().icmp_imm(IntCC::Equal, found, 0)
    };
    let take = cx.b.ins().band(keep.payload, not_found);
    cx.b.ins().brif(take, take_block, &[], discard, &[]);
    cx.b.switch_to_block(take_block);
    cx.b.seal_block(take_block);
    let (disc, payload) = match &bound {
        BoundElem::Scalar { var, prim } => {
            let value = cx.b.use_var(*var);
            let disc = cx.b.ins().iconst(types::I64, prim_to_value_disc(*prim));
            let payload = scalar_to_payload_i64(cx.b, *prim, value);
            (disc, payload)
        }
        BoundElem::Composite { var } => {
            let value = cx.b.use_var(*var);
            let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
            (disc, value)
        }
        BoundElem::String { var } => {
            let value = cx.b.use_var(*var);
            let disc = cx.b.ins().iconst(types::I64, value_disc::STRING);
            (disc, value)
        }
        BoundElem::Value { disc, payload } => {
            (cx.b.use_var(*disc), cx.b.use_var(*payload))
        }
    };
    cx.b.def_var(result_disc_var, disc);
    cx.b.def_var(result_payload_var, payload);
    let one = cx.b.ins().iconst(types::I8, 1);
    cx.b.def_var(found_var, one);
    cx.b.ins().jump(advance, &[]);
    cx.b.switch_to_block(discard);
    cx.b.seal_block(discard);
    drop_owned_elem(cx, &bound)?;
    cx.b.ins().jump(advance, &[]);
    cx.b.switch_to_block(advance);
    cx.b.seal_block(advance);
    emit_increment(cx, i_var, i, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    cx.emit_slot_truncates()?;
    let disc = cx.b.use_var(result_disc_var);
    let payload = cx.b.use_var(result_payload_var);
    drop_owned_src(cx, &arr)?;
    Ok(((disc, payload), flags))
}

pub fn emit_find_map_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    sel_sites: &[ExprId],
    mut body: F,
) -> Result<((ClifValue, ClifValue), SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<(ClifValue, ClifValue)>,
{
    let mut flags = SlotFlags::new(cx);
    adopt_owned_src(cx, &arr);
    let len = input_len(cx, arr.ptr)?;
    flags.set_len(len);
    let found_var = cx.b.declare_var(types::I8);
    let zero8 = cx.b.ins().iconst(types::I8, 0);
    cx.b.def_var(found_var, zero8);
    let result_disc_var = cx.b.declare_var(types::I64);
    let null_disc = cx.b.ins().iconst(types::I64, value_disc::NULL);
    cx.b.def_var(result_disc_var, null_disc);
    let result_payload_var = cx.b.declare_var(types::I64);
    let zero64 = cx.b.ins().iconst(types::I64, 0);
    cx.b.def_var(result_payload_var, zero64);
    let i_var = init_counter(cx);
    cx.open_slot_tables(sel_sites, len, arr.disc, i_var)?;
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let take_block = cx.b.create_block();
    let discard = cx.b.create_block();
    let advance = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let i = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    cx.enter_loop();
    let (bound, leaves) = bind_elem(cx, arr.disc, arr.ptr, i, elem)?;
    let value = body(cx);
    cx.exit_loop();
    cx.close_slot_tables();
    let (disc, payload) = value?;
    drop_owned_leaves(cx, &leaves)?;
    drop_owned_elem(cx, &bound)?;
    cx.env.truncate(mark);
    flags.fold(cx, disc);
    let clean = clean_disc(cx.b, disc);
    let non_null = cx.b.ins().icmp_imm(IntCC::NotEqual, clean, value_disc::NULL);
    let not_found = {
        let found = cx.b.use_var(found_var);
        cx.b.ins().icmp_imm(IntCC::Equal, found, 0)
    };
    let take = cx.b.ins().band(non_null, not_found);
    cx.b.ins().brif(take, take_block, &[], discard, &[]);
    cx.b.switch_to_block(take_block);
    cx.b.seal_block(take_block);
    cx.b.def_var(result_disc_var, clean);
    cx.b.def_var(result_payload_var, payload);
    let one = cx.b.ins().iconst(types::I8, 1);
    cx.b.def_var(found_var, one);
    cx.b.ins().jump(advance, &[]);
    cx.b.switch_to_block(discard);
    cx.b.seal_block(discard);
    let drop_value = cx.helper("graphix_value_drop")?;
    cx.b.ins().call(drop_value, &[disc, payload]);
    cx.b.ins().jump(advance, &[]);
    cx.b.switch_to_block(advance);
    cx.b.seal_block(advance);
    emit_increment(cx, i_var, i, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    cx.emit_slot_truncates()?;
    let disc = cx.b.use_var(result_disc_var);
    let payload = cx.b.use_var(result_payload_var);
    drop_owned_src(cx, &arr)?;
    Ok(((disc, payload), flags))
}
