//! HOF loop scaffolds for the cranelift JIT backend.
//!
//! Each `emit_*_loop` owns the MECHANICS of one HOF loop shape: the
//! length / buf-new calls, the counter variable, block creation and
//! sealing order, per-iteration element binding and dropping, and the
//! output-buf pending-cleanup registration. The CALLER owns the
//! policy — WHAT the loop body computes — via a body closure that
//! compiles the body through the [`BodyCx`] handed to it.
//!
//! Two callers share the scaffolds:
//! - the classic GIR arms in [`super`] (`GirOp::ArrayMap` & co.),
//!   until Stage F of `design/distributed_jit.md` deletes them, and
//! - the direct node path's `Apply::emit_clif` HOF impls (Stage D2),
//!   whose closures compile Node bodies via `node.emit_clif(cx)`.
//!
//! The emitted CLIF is instruction-for-instruction what the GIR arms
//! emitted before the extraction; changes here must preserve the
//! emission sequence exactly (instruction order, block-creation
//! order, variable-declaration order).

use super::*;

/// The input array for a HOF loop. `owned` ⇒ the scaffold emits a
/// `graphix_valarray_drop(ptr)` after the loop completes — at the
/// single post-loop merge point ONLY. A pending abort inside the loop
/// body (composite-return DynCall pend, QopUnwrap, `push_field`'s
/// bottom-abort) jumps to `pending_exit` WITHOUT dropping it:
/// `emit_pending_cleanup` only sees `dyncall_buf_stack` entries and
/// env-bound locals, and a raw `ArraySrc.ptr` is neither. So the
/// contract for an owned fresh-producer input (Stage D2) is
/// one-or-the-other: either register the ptr for pending cleanup (a
/// valarray analogue of [`register_hof_buf`]) and pass `owned: true`,
/// or bind it into the env as an owned composite local and pass
/// `owned: false` (env scope-exit drops it — passing `owned: true` as
/// well would DOUBLE-drop on the normal path). The classic GIR arms
/// always pass `owned: false` (the array is a borrowed env local).
pub struct ArraySrc {
    pub ptr: ClifValue,
    pub owned: bool,
}

/// Loop element binding: bound under `name` in the `JitEnv` for the
/// duration of each iteration, with shape dispatch from `typ`. `id` is
/// the callback's element-arg `BindId` when the caller has one (the
/// direct node path, where the body's element `Ref`s carry it and
/// resolve BindId-first — the #162/#167 shadowing class); the classic
/// GIR arms pass `None` (their synthetic locals are name-only). Bind
/// bookkeeping emits no instructions, so `Some` vs `None` never
/// changes the emitted CLIF.
pub struct HofElem<'a> {
    pub name: &'a ArcStr,
    pub id: Option<crate::BindId>,
    pub typ: &'a Type,
}

/// A bound per-iteration element — see [`bind_elem`].
pub(crate) enum BoundElem {
    Scalar { var: Variable, prim: PrimType },
    /// An owned `*mut ValArray`: a consumer that doesn't move it into
    /// the output must drop it before the iteration ends.
    Composite { var: Variable },
}

impl BoundElem {
    fn var(&self) -> Variable {
        match self {
            BoundElem::Scalar { var, .. } | BoundElem::Composite { var } => {
                *var
            }
        }
    }

    /// The element Variable when it's an owned composite (the drop
    /// sites' dispatch); `None` for scalars (nothing to drop).
    fn composite_var(&self) -> Option<Variable> {
        match self {
            BoundElem::Scalar { .. } => None,
            BoundElem::Composite { var } => Some(*var),
        }
    }
}

/// Fetch element `i_now` of `arr_ptr` and bind it under `elem.name`:
/// a scalar element reads via `graphix_valarray_get_<prim>` into a
/// scalar local; an array/tuple/struct element reads via
/// `graphix_valarray_get_array` into a composite local (an OWNED
/// `*mut ValArray` the iteration must consume or drop). Any other
/// shape errs — lowering never produces string / value-shape loop
/// elements (the #150 gap), and an explicit refusal turns would-be
/// type confusion on unreachable input into a clean de-fuse.
fn bind_elem(
    cx: &mut BodyCx,
    arr_ptr: ClifValue,
    i_now: ClifValue,
    elem: &HofElem,
) -> Result<BoundElem> {
    match gir::abi_kind(elem.typ) {
        Some(AbiKind::Scalar(prim)) => {
            let get_helper = cx.helper(valarray_get_helper(prim)?)?;
            let call = cx.b.ins().call(get_helper, &[arr_ptr, i_now]);
            let elem_val = cx.b.inst_results(call)[0];
            let var = cx.b.declare_var(prim_to_clif(prim));
            cx.b.def_var(var, elem_val);
            cx.env.bind_with_id(elem.name.clone(), var, prim, elem.id);
            Ok(BoundElem::Scalar { var, prim })
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let get_helper = cx.helper("graphix_valarray_get_array")?;
            let call = cx.b.ins().call(get_helper, &[arr_ptr, i_now]);
            let elem_ptr = cx.b.inst_results(call)[0];
            let var = cx.b.declare_var(types::I64);
            cx.b.def_var(var, elem_ptr);
            cx.env.bind_composite_with_id(elem.name.clone(), var, elem.id);
            Ok(BoundElem::Composite { var })
        }
        _ => Err(anyhow!(
            "HOF element shape not supported by the JIT loop scaffolds"
        )),
    }
}

/// Drop an owned composite element (no-op for scalars). The
/// per-iteration counterpart of [`bind_elem`]'s composite arm.
fn drop_composite_elem(cx: &mut BodyCx, elem: &BoundElem) -> Result<()> {
    if let Some(var) = elem.composite_var() {
        let drop_helper = cx.helper("graphix_valarray_drop")?;
        let elem_now = cx.b.use_var(var);
        cx.b.ins().call(drop_helper, &[elem_now]);
    }
    Ok(())
}

/// Drop the input array when the caller passed it owned — emitted at
/// the single post-loop merge point of each scaffold. The classic GIR
/// arms always pass `owned: false`, so this emits nothing for them.
fn drop_owned_src(cx: &mut BodyCx, arr: &ArraySrc) -> Result<()> {
    if arr.owned {
        let drop_helper = cx.helper("graphix_valarray_drop")?;
        cx.b.ins().call(drop_helper, &[arr.ptr]);
    }
    Ok(())
}

/// Register an in-progress HOF output buf (`graphix_value_buf_new`) for
/// pending cleanup: declare it as a CLIF Variable and push onto
/// `ctx.dyncall_buf_stack`, so a value-shape/composite DynCall or `?`
/// (QopUnwrap) that pends inside the loop body drops it from its
/// `pre_pending` block via [`super::emit_pending_cleanup`]. Mirrors the
/// DynCall arg-buf registration in [`super::marshal_dyncall_args`]. Pair
/// with [`unregister_hof_buf`] before `finalize` on the normal
/// (non-pending) path — `finalize` consumes the buf there, so the
/// runtime drop happens exactly once (cleanup on pend, finalize
/// otherwise).
///
/// No Err path pops this stack: a scaffold Err between register and
/// finalize abandons the WHOLE kernel build (the caller discards the
/// function and `clear_context`s; `LowerCtx` and its
/// `dyncall_buf_stack` are constructed fresh per build). Catching a
/// scaffold Err and continuing to emit within the same build would
/// leave a stale entry that poisons every later pending-cleanup —
/// don't.
fn register_hof_buf(b: &mut FunctionBuilder, ctx: &LowerCtx, buf: ClifValue) {
    let buf_var = b.declare_var(types::I64);
    b.def_var(buf_var, buf);
    ctx.dyncall_buf_stack.borrow_mut().push(buf_var);
}

/// Pop the HOF output buf registered by [`register_hof_buf`]. Called on
/// the normal path just before `finalize` consumes the buf.
fn unregister_hof_buf(ctx: &LowerCtx) {
    ctx.dyncall_buf_stack.borrow_mut().pop();
}

/// `len = valarray_len(arr_ptr)` — the input-length read every
/// array-consuming scaffold opens with.
fn input_len(cx: &mut BodyCx, arr_ptr: ClifValue) -> Result<ClifValue> {
    let len_helper = cx.helper("graphix_valarray_len")?;
    let call = cx.b.ins().call(len_helper, &[arr_ptr]);
    Ok(cx.b.inst_results(call)[0])
}

/// The shared prologue of every buf-producing loop over an input
/// array: read the length, `buf_new(len)`, register the buf for
/// pending cleanup. Returns `(len, buf)`.
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

/// Emit the `i < len` header test: brif to `loop_body` / `loop_exit`.
/// The builder must be positioned in the (already-jumped-to) header
/// block.
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

/// `i += 1; jump loop_header` — the loop back-edge. `i_now` is the
/// caller's already-read counter value (its read position differs per
/// scaffold and must be preserved).
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

/// Unregister the output buf and `finalize` it into the result array
/// pointer. The epilogue of every buf-producing scaffold.
fn finalize_buf(cx: &mut BodyCx, buf: ClifValue) -> Result<ClifValue> {
    unregister_hof_buf(cx.ctx);
    let finalize = cx.helper("graphix_valarray_finalize")?;
    let call = cx.b.ins().call(finalize, &[buf]);
    Ok(cx.b.inst_results(call)[0])
}

/// Push an already-compiled field result into a `graphix_value_buf` —
/// the push half of [`super::compile_and_push_field`]. Helper choice
/// per shape:
/// - **Scalar**: `graphix_value_buf_push_<T>` per the prim. A
///   possibly-bottom (`Scalar2`) field aborts the kernel to pending
///   via [`super::emit_bottom_abort`] before the push (a composite
///   producer has no per-field validity channel, so a bottom field
///   must propagate to the kernel OUTPUT).
/// - **Array/Tuple/Struct**: `graphix_value_buf_push_array` (owned)
///   or `_borrowed` (refcount-bumped), by `src`.
/// - **Variant/Nullable/Value**: `graphix_value_buf_push_value` or
///   `_borrowed`, picked the same way; pushes both `(disc, payload)`
///   words.
/// - **String**: `graphix_value_buf_push_string`, UNCONDITIONALLY —
///   `src` is ignored because string SSA is always owned (every
///   producer on both paths — ConstStr, Concat, Local/Ref reads —
///   hands out a fresh refcount; see `ReturnDropShape::String`). The
///   bits are the ArcStr's raw thin pointer, NOT a pointer to an
///   ArcStr struct — `_push_arcstr` would dereference them as one
///   (UAF/UB); `_push_string` takes the ArcStr by value (consumes).
///   A caller with a genuinely borrowed string SSA must clone first.
/// - **Unit/Null**: invalid as a field — caller should have rejected.
pub fn push_field(
    cx: &mut BodyCx,
    buf: ClifValue,
    cv: CompiledExpr,
    typ: &Type,
    src: CompositeSource,
) -> Result<()> {
    let helper_name: &str = match gir::abi_kind(typ) {
        Some(AbiKind::Scalar(p)) => value_buf_push_helper(p)?,
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => match src {
            CompositeSource::Owned => "graphix_value_buf_push_array",
            CompositeSource::Borrowed => {
                "graphix_value_buf_push_array_borrowed"
            }
        },
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            match src {
                CompositeSource::Owned => "graphix_value_buf_push_value",
                CompositeSource::Borrowed => {
                    "graphix_value_buf_push_value_borrowed"
                }
            }
        }
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
    // Value-shape fields (Variant/Nullable/DateTime/Duration/Bytes/Map)
    // need both `(disc, payload)` registers from the typed Value ABI;
    // everything else is a single ClifValue. This dispatch MUST match
    // the helper-selection above — both key on the full value-shape set
    // (`is_value_shape()`). When it only listed Variant|Nullable, a
    // DateTime/Duration/Bytes/Map field fell to the `_` arm and
    // `.single()` Err'd, silently de-fusing the whole kernel.
    if gir::is_value_shape(typ) {
        let (disc, payload) = match cv {
            CompiledExpr::Value { disc, payload } => (disc, payload),
            _ => {
                return Err(anyhow!(
                    "Value-shape field compiled to Single — \
                     compile_expr dispatch is broken"
                ));
            }
        };
        cx.b.ins().call(push, &[buf, disc, payload]);
    } else {
        match cv {
            CompiledExpr::Single(v) => {
                cx.b.ins().call(push, &[buf, v]);
            }
            CompiledExpr::Scalar2 { value, valid } => {
                emit_bottom_abort(cx.b, cx.env, cx.ctx, valid)?;
                cx.b.ins().call(push, &[buf, value]);
            }
            CompiledExpr::Value { .. } => {
                return Err(anyhow!(
                    "scalar producer field compiled to Value-shape — \
                     compile_expr dispatch is broken"
                ));
            }
        }
    }
    Ok(())
}

/// `array::init(n, |idx| body)` — build an `n_raw`-element array by
/// pushing the body result per index. `n_raw` is the caller-compiled
/// count (widened here, with a negative `n` clamped to 0 to match the
/// node-walk's `n.max(0)`: `buf_new(neg)` would reserve `usize::MAX`
/// and panic across the extern "C" boundary). `idx_name` binds the
/// I64 loop counter Variable itself — no per-iteration copy.
pub fn emit_init_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    n_raw: ClifValue,
    n_prim: PrimType,
    idx_name: &ArcStr,
    out_typ: &Type,
    out_src: CompositeSource,
    mut body: F,
) -> Result<ClifValue>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let n_widened = widen_to_i64(cx.b, n_raw, n_prim);
    let zero_clamp = cx.b.ins().iconst(types::I64, 0);
    let is_neg =
        cx.b.ins().icmp(IntCC::SignedLessThan, n_widened, zero_clamp);
    let n_val = cx.b.ins().select(is_neg, zero_clamp, n_widened);
    let call = cx.b.ins().call(buf_new, &[n_val]);
    let buf = cx.b.inst_results(call)[0];
    register_hof_buf(cx.b, cx.ctx, buf);
    let i_var = init_counter(cx);
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, n_val, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    let mark = cx.env.mark();
    cx.env.bind(idx_name.clone(), i_var, PrimType::I64);
    let cv = body(cx)?;
    push_field(cx, buf, cv, out_typ, out_src)?;
    cx.env.truncate(mark);
    let i_now = cx.b.use_var(i_var);
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    finalize_buf(cx, buf)
}

/// `array::map(arr, |x| body)` — push the body result per element. A
/// composite element is an owned `*mut ValArray`, dropped AFTER the
/// push (the body's element reads clone the fields they touch).
pub fn emit_map_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    out_typ: &Type,
    out_src: CompositeSource,
    mut body: F,
) -> Result<ClifValue>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let (len, buf) = input_sized_buf(cx, arr.ptr)?;
    let i_var = init_counter(cx);
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    let i_now = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    let bound = bind_elem(cx, arr.ptr, i_now, elem)?;
    let cv = body(cx)?;
    push_field(cx, buf, cv, out_typ, out_src)?;
    drop_composite_elem(cx, &bound)?;
    cx.env.truncate(mark);
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    let result = finalize_buf(cx, buf)?;
    drop_owned_src(cx, &arr)?;
    Ok(result)
}

/// `array::filter(arr, |x| pred)` — push the ORIGINAL element when
/// the predicate (an I8 keep flag) is true. A composite element is an
/// owned `*mut ValArray`: on keep it MOVES into the output
/// (`push_array`); on not-keep it must be dropped — hence the extra
/// `drop_block` for the composite case (created LAST, after
/// `loop_exit` — block-creation order is part of the preserved
/// emission sequence).
pub fn emit_filter_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    mut pred: F,
) -> Result<ClifValue>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<ClifValue>,
{
    let composite = matches!(
        gir::abi_kind(elem.typ),
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct)
    );
    let (len, buf) = input_sized_buf(cx, arr.ptr)?;
    let i_var = init_counter(cx);
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let push_block = cx.b.create_block();
    let advance = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    // not-kept owned composite element → drop before advancing
    let drop_block = if composite { Some(cx.b.create_block()) } else { None };
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    let i_now = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    let bound = bind_elem(cx, arr.ptr, i_now, elem)?;
    let keep = pred(cx)?;
    cx.env.truncate(mark);
    let not_kept = drop_block.unwrap_or(advance);
    cx.b.ins().brif(keep, push_block, &[], not_kept, &[]);
    cx.b.switch_to_block(push_block);
    let push = match &bound {
        BoundElem::Scalar { prim, .. } => {
            cx.helper(value_buf_push_helper(*prim)?)?
        }
        BoundElem::Composite { .. } => {
            cx.helper("graphix_value_buf_push_array")?
        }
    };
    let elem_again = cx.b.use_var(bound.var());
    cx.b.ins().call(push, &[buf, elem_again]);
    cx.b.ins().jump(advance, &[]);
    cx.b.seal_block(push_block);
    if let Some(drop_block) = drop_block {
        cx.b.switch_to_block(drop_block);
        drop_composite_elem(cx, &bound)?;
        cx.b.ins().jump(advance, &[]);
        cx.b.seal_block(drop_block);
    }
    cx.b.switch_to_block(advance);
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(advance);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    let result = finalize_buf(cx, buf)?;
    drop_owned_src(cx, &arr)?;
    Ok(result)
}

/// `array::filter_map(arr, |x| body)` — the body yields a
/// `Nullable<out_elem>` per element as Value-shape `(disc, payload)`;
/// collect the non-null payloads (cast back to `out_elem`'s register
/// type). Scalar-only in/out element types.
pub fn emit_filter_map_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    in_elem: PrimType,
    elem_name: &ArcStr,
    out_elem: PrimType,
    mut body: F,
) -> Result<ClifValue>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let get_helper = cx.helper(valarray_get_helper(in_elem)?)?;
    let push = cx.helper(value_buf_push_helper(out_elem)?)?;
    let (len, buf) = input_sized_buf(cx, arr.ptr)?;
    let i_var = init_counter(cx);
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let push_block = cx.b.create_block();
    let advance = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    let i_now = cx.b.use_var(i_var);
    let call = cx.b.ins().call(get_helper, &[arr.ptr, i_now]);
    let elem_val = cx.b.inst_results(call)[0];
    let elem_var = cx.b.declare_var(prim_to_clif(in_elem));
    cx.b.def_var(elem_var, elem_val);
    let mark = cx.env.mark();
    cx.env.bind(elem_name.clone(), elem_var, in_elem);
    let cv = body(cx)?;
    cx.env.truncate(mark);
    let (disc, payload) = match cv {
        CompiledExpr::Value { disc, payload } => (disc, payload),
        _ => {
            return Err(anyhow!(
                "ArrayFilterMap body not Value-shape — expected \
                 a Nullable result"
            ))
        }
    };
    let is_null = cx.b.ins().icmp_imm(IntCC::Equal, disc, value_disc::NULL);
    cx.b.ins().brif(is_null, advance, &[], push_block, &[]);
    cx.b.switch_to_block(push_block);
    let scalar = cast_u64_to_prim(cx.b, payload, out_elem);
    cx.b.ins().call(push, &[buf, scalar]);
    cx.b.ins().jump(advance, &[]);
    cx.b.seal_block(push_block);
    cx.b.switch_to_block(advance);
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(advance);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    let result = finalize_buf(cx, buf)?;
    drop_owned_src(cx, &arr)?;
    Ok(result)
}

/// `array::flat_map(arr, |x| body)` — the body closure returns an
/// OWNED `Array<out>` pointer per element (the caller's closure runs
/// its ownership fixup, e.g. `ensure_owned_composite`), concatenated
/// into the output via `graphix_value_buf_extend_from_array` (which
/// flattens + drops it). Linear — no per-element branch. A composite
/// element is dropped after the body, before the extend.
pub fn emit_flat_map_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    mut body: F,
) -> Result<ClifValue>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<ClifValue>,
{
    let extend = cx.helper("graphix_value_buf_extend_from_array")?;
    let (len, buf) = input_sized_buf(cx, arr.ptr)?;
    let i_var = init_counter(cx);
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    let i_now = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    let bound = bind_elem(cx, arr.ptr, i_now, elem)?;
    let body_ptr = body(cx)?;
    drop_composite_elem(cx, &bound)?;
    cx.env.truncate(mark);
    cx.b.ins().call(extend, &[buf, body_ptr]);
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    let result = finalize_buf(cx, buf)?;
    drop_owned_src(cx, &arr)?;
    Ok(result)
}

/// `array::fold(arr, init, |acc, x| body)` — a scalar accumulator
/// Variable threaded through the loop. Per iteration the acc is bound
/// FIRST, then the element (GIR construction pins this order); the
/// body's result re-defines the acc Variable. No output buf, no
/// pending-cleanup registration.
///
/// The `init`/`body` closures must yield a DEFINITELY-VALID scalar:
/// the design contract is that a may-bottom fold body de-fuses at
/// BUILD time, so a closure holding a `Scalar2` must Err (the GIR
/// arms get this from `compile_scalar`'s `.single()`), never strip
/// the validity bit and return the bare value. Same contract for
/// [`emit_filter_loop`]/[`emit_find_loop`] predicates and
/// [`emit_flat_map_loop`] bodies. Contrast [`push_field`] (the map
/// path), which accepts `Scalar2` and bottom-aborts at RUNTIME.
pub fn emit_fold_loop<'a, 'f, 'c, I, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    acc_prim: PrimType,
    acc_name: &ArcStr,
    acc_id: Option<crate::BindId>,
    elem: &HofElem,
    init: I,
    mut body: F,
) -> Result<ClifValue>
where
    I: FnOnce(&mut BodyCx<'a, 'f, 'c>) -> Result<ClifValue>,
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<ClifValue>,
{
    let len = input_len(cx, arr.ptr)?;
    let acc_var = cx.b.declare_var(prim_to_clif(acc_prim));
    let init_val = init(cx)?;
    cx.b.def_var(acc_var, init_val);
    let i_var = init_counter(cx);
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    let i_now = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    cx.env.bind_with_id(acc_name.clone(), acc_var, acc_prim, acc_id);
    let bound = bind_elem(cx, arr.ptr, i_now, elem)?;
    let new_acc = body(cx)?;
    drop_composite_elem(cx, &bound)?;
    cx.env.truncate(mark);
    cx.b.def_var(acc_var, new_acc);
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    drop_owned_src(cx, &arr)?;
    Ok(cx.b.use_var(acc_var))
}

/// `array::find(arr, |x| pred)` — early-exit on the first element
/// whose predicate (I8 keep flag) is true; the result is the
/// `Nullable<elem>` `(disc, payload)` pair (null when none match).
/// The found / not-found edges merge into a two-block-param exit. A
/// scalar match packs `(prim_disc, payload)` inline; a composite
/// match CONSUMES the owned element via `graphix_value_new_from_array`
/// on the found edge and drops it on the advance edge (matched-once /
/// not-this-iteration).
pub fn emit_find_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    mut pred: F,
) -> Result<(ClifValue, ClifValue)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<ClifValue>,
{
    let len = input_len(cx, arr.ptr)?;
    let i_var = init_counter(cx);
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let found = cx.b.create_block();
    let advance = cx.b.create_block();
    let not_found = cx.b.create_block();
    let exit = cx.b.create_block();
    cx.b.append_block_param(exit, types::I64); // disc
    cx.b.append_block_param(exit, types::I64); // payload
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, not_found);
    cx.b.switch_to_block(loop_body);
    let i_now = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    let bound = bind_elem(cx, arr.ptr, i_now, elem)?;
    let keep = pred(cx)?;
    cx.env.truncate(mark);
    cx.b.ins().brif(keep, found, &[], advance, &[]);
    cx.b.switch_to_block(found);
    cx.b.seal_block(found);
    let (disc, payload) = match &bound {
        BoundElem::Scalar { var, prim } => {
            let elem_again = cx.b.use_var(*var);
            let disc =
                cx.b.ins().iconst(types::I64, prim_to_value_disc(*prim));
            let payload = scalar_to_payload_i64(cx.b, *prim, elem_again);
            (disc, payload)
        }
        BoundElem::Composite { var } => {
            // Wrap the owned `*ValArray` element into a value-shape
            // `(ARRAY_DISC, payload)` Value (consumes it).
            let wrap = cx.helper("graphix_value_new_from_array")?;
            let elem_now = cx.b.use_var(*var);
            let call = cx.b.ins().call(wrap, &[elem_now]);
            let r = cx.b.inst_results(call);
            (r[0], r[1])
        }
    };
    cx.b
        .ins()
        .jump(exit, &[BlockArg::Value(disc), BlockArg::Value(payload)]);
    cx.b.switch_to_block(advance);
    // Not matched this iteration — drop an owned composite element.
    drop_composite_elem(cx, &bound)?;
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(advance);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(not_found);
    cx.b.seal_block(not_found);
    let null_disc = cx.b.ins().iconst(types::I64, value_disc::NULL);
    let null_payload = cx.b.ins().iconst(types::I64, 0);
    cx.b.ins().jump(
        exit,
        &[BlockArg::Value(null_disc), BlockArg::Value(null_payload)],
    );
    cx.b.switch_to_block(exit);
    cx.b.seal_block(exit);
    let disc = cx.b.block_params(exit)[0];
    let payload = cx.b.block_params(exit)[1];
    drop_owned_src(cx, &arr)?;
    Ok((disc, payload))
}

/// `array::find_map(arr, |x| body)` — early-exit on the first
/// non-null body result; the result is that `Nullable<out>` `(disc,
/// payload)` pair (or null). Same merge shape as [`emit_find_loop`],
/// but the body produces the pair directly (not a predicate), and a
/// composite element is dropped EVERY iteration right after the body
/// (the result is the body value, never the element). The closure's
/// pair must be OWNED and independent of the element — the element is
/// dropped before the null test, and on the found edge the pair
/// leaves the loop as the kernel's result (like
/// [`emit_flat_map_loop`]'s owned-ptr requirement).
pub fn emit_find_map_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    mut body: F,
) -> Result<(ClifValue, ClifValue)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<(ClifValue, ClifValue)>,
{
    let len = input_len(cx, arr.ptr)?;
    let i_var = init_counter(cx);
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let found = cx.b.create_block();
    let advance = cx.b.create_block();
    let not_found = cx.b.create_block();
    let exit = cx.b.create_block();
    cx.b.append_block_param(exit, types::I64); // disc
    cx.b.append_block_param(exit, types::I64); // payload
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, not_found);
    cx.b.switch_to_block(loop_body);
    let i_now = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    let bound = bind_elem(cx, arr.ptr, i_now, elem)?;
    let (bdisc, bpayload) = body(cx)?;
    drop_composite_elem(cx, &bound)?;
    cx.env.truncate(mark);
    let is_null =
        cx.b.ins().icmp_imm(IntCC::Equal, bdisc, value_disc::NULL);
    // not-null → found; null → advance. `bdisc`/`bpayload`
    // dominate `found` (computed before the branch).
    cx.b.ins().brif(is_null, advance, &[], found, &[]);
    cx.b.switch_to_block(found);
    cx.b.seal_block(found);
    cx.b
        .ins()
        .jump(exit, &[BlockArg::Value(bdisc), BlockArg::Value(bpayload)]);
    cx.b.switch_to_block(advance);
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(advance);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(not_found);
    cx.b.seal_block(not_found);
    let null_disc = cx.b.ins().iconst(types::I64, value_disc::NULL);
    let null_payload = cx.b.ins().iconst(types::I64, 0);
    cx.b.ins().jump(
        exit,
        &[BlockArg::Value(null_disc), BlockArg::Value(null_payload)],
    );
    cx.b.switch_to_block(exit);
    cx.b.seal_block(exit);
    let disc = cx.b.block_params(exit)[0];
    let payload = cx.b.block_params(exit)[1];
    drop_owned_src(cx, &arr)?;
    Ok((disc, payload))
}
