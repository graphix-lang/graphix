//! HOF loop scaffolds for the cranelift JIT backend.
//!
//! Each `emit_*_loop` owns the MECHANICS of one HOF loop shape: the
//! length / buf-new calls, the counter variable, block creation and
//! sealing order, per-iteration element binding and dropping, and the
//! output-buf pending-cleanup registration. The CALLER owns the
//! policy — WHAT the loop body computes — via a body closure that
//! compiles the body through the [`BodyCx`] handed to it.
//!
//! The scaffolds' caller is the direct node path's
//! `Apply::emit_clif` HOF impls (Stage D2), whose closures compile
//! Node bodies via `node.emit_clif(cx)`.
//!
//! The emitted CLIF must stay instruction-for-instruction stable;
//! changes here must preserve the emission sequence exactly
//! (instruction order, block-creation order, variable-declaration
//! order).

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
/// well would DOUBLE-drop on the normal path). A borrowed env-local
/// array passes `owned: false`.
pub struct ArraySrc {
    pub ptr: ClifValue,
    /// The source expression's full disc — its STALE bit is "the source
    /// fired this invocation" (inherited by the bound elements, so slot
    /// bodies fire with their elements) and its TAINT bit rides into
    /// the result.
    pub disc: ClifValue,
    pub owned: bool,
}

/// Loop element binding: bound under `name` in the `JitEnv` for the
/// duration of each iteration, with shape dispatch from `typ`. `id` is
/// the callback's element-arg `BindId` when the caller has one (the
/// direct node path, where the body's element `Ref`s carry it and
/// resolve BindId-first — the #162/#167 shadowing class); a name-only
/// caller passes `None` (its synthetic locals are looked up by name).
/// Bind bookkeeping emits no instructions, so `Some` vs `None` never
/// changes the emitted CLIF.
pub struct HofElem<'a> {
    pub name: &'a ArcStr,
    pub id: Option<crate::BindId>,
    pub typ: &'a Type,
    /// Destructure leaves for a `|(k, v)|` callback: per bound leaf,
    /// its pattern `BindId`, tuple position, and shape. [`bind_elem`]
    /// reads each off the owned composite element and binds it
    /// BindId-first, so the body's leaf `Ref`s resolve without name
    /// plumbing. Scalar leaves are by-value copies; composite / string /
    /// value-shape leaves are OWNED clones the loop drops at body end
    /// (see [`drop_owned_elem`] — the leaf list `bind_elem` returns).
    /// Sparse positions (`|(k, _)|`) simply have no entry. Empty for
    /// single-name callbacks. Only valid on a composite element —
    /// leaves on a non-composite element are a caller bug
    /// ([`bind_elem`] Errs).
    pub leaves: &'a [(crate::BindId, usize, LeafShape)],
}

/// The shape of one `|(k, v)|` destructure leaf — decided by the HOF
/// gate (`elem_leaves` in the array package) from the tuple element
/// type, and dispatched by [`bind_elem`]'s per-leaf reads.
#[derive(Clone, Copy, Debug)]
pub enum LeafShape {
    /// A register scalar — by-value copy, nothing to drop.
    Scalar(PrimType),
    /// Array/tuple/struct — an owned `*mut ValArray` clone.
    Composite,
    /// An owned `ArcStr` clone.
    String,
    /// Variant / Nullable / DateTime / Duration / Bytes / Map — an owned
    /// two-word Value clone, bound under the given local kind.
    Value(ValueLeafKind),
}

/// Which value-shape local kind a [`LeafShape::Value`] leaf binds as.
#[derive(Clone, Copy, Debug)]
pub enum ValueLeafKind {
    Variant,
    Nullable,
    Value,
}

/// A bound per-iteration element — see [`bind_elem`]. The owned kinds
/// (Composite/String/Value) hold an owned refcount that a consumer must
/// either MOVE into the output or drop via [`drop_owned_elem`] before the
/// iteration ends.
pub(crate) enum BoundElem {
    Scalar {
        var: Variable,
        prim: PrimType,
    },
    /// An owned `*mut ValArray`.
    Composite {
        var: Variable,
    },
    /// An owned `ArcStr` (its raw thin-pointer bits).
    String {
        var: Variable,
    },
    /// An owned two-word `(disc, payload)` Value (variant/nullable/value).
    Value {
        disc: Variable,
        payload: Variable,
    },
}

/// Fetch element `i_now` of `arr_ptr` and bind it under `elem.name`:
/// a scalar element reads via `graphix_valarray_get_<prim>` into a
/// scalar local; an array/tuple/struct element reads via
/// `graphix_valarray_get_array` into a composite local (an OWNED
/// `*mut ValArray` the iteration must consume or drop). Any other
/// shape errs — lowering never produces string / value-shape loop
/// elements (the #150 gap), and an explicit refusal turns would-be
/// type confusion on unreachable input into a clean de-fuse.
/// Fold the SOURCE's STALE bit onto a synthesized element/leaf disc:
/// an element "fires" exactly when its source array fired, so slot
/// bodies that consume the element fire with the source — and stay
/// quiet on capture-only cycles (the sleeping-select-arm class).
fn elem_disc(cx: &mut BodyCx, base: ClifValue, src_disc: ClifValue) -> ClifValue {
    let sb = cx.b.ins().band_imm(src_disc, STALE);
    cx.b.ins().bor(base, sb)
}

fn bind_elem(
    cx: &mut BodyCx,
    src_disc: ClifValue,
    arr_ptr: ClifValue,
    i_now: ClifValue,
    elem: &HofElem,
) -> Result<(BoundElem, Vec<BoundElem>)> {
    match kernel_abi::abi_kind(cx.registry(), elem.typ) {
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
            // The element of a valid array is untainted (a tainted source
            // array bottoms the whole HOF separately).
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
            // Destructure leaves, bound under their pattern BindIds (the
            // body's leaf Refs resolve BindId-first; the synthetic name is
            // never looked up). Scalar leaves are by-value copies (the
            // element's own drop covers the allocation); composite /
            // string / value leaves are OWNED clones — bound as env
            // locals of the matching kind (so a mid-body pending exit
            // drops them via `drop_owned_composites`) and returned for
            // the loop's normal-path [`drop_owned_leaves`].
            let mut owned_leaves: Vec<BoundElem> = Vec::new();
            for (id, idx, shape) in elem.leaves {
                let idx_c = cx.b.ins().iconst(types::I64, *idx as i64);
                let name: ArcStr =
                    compact_str::format_compact!("__leaf{}", id.inner()).as_str().into();
                match shape {
                    LeafShape::Scalar(prim) => {
                        let get = cx.helper(valarray_get_helper(*prim)?)?;
                        let call = cx.b.ins().call(get, &[elem_ptr, idx_c]);
                        let v = cx.b.inst_results(call)[0];
                        let d = scalar_disc(cx.b, *prim);
                        let d = elem_disc(cx, d, src_disc);
                        bind_local(cx, name, d, v, LocalKind::Scalar(*prim), Some(*id));
                    }
                    LeafShape::Composite => {
                        let get = cx.helper("graphix_valarray_get_array")?;
                        let call = cx.b.ins().call(get, &[elem_ptr, idx_c]);
                        let p = cx.b.inst_results(call)[0];
                        let d = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
                        let d = elem_disc(cx, d, src_disc);
                        let vv =
                            bind_local(cx, name, d, p, LocalKind::Composite, Some(*id));
                        owned_leaves.push(BoundElem::Composite { var: vv.payload });
                    }
                    LeafShape::String => {
                        let get = cx.helper("graphix_valarray_get_arcstr")?;
                        let call = cx.b.ins().call(get, &[elem_ptr, idx_c]);
                        let s = cx.b.inst_results(call)[0];
                        let d = cx.b.ins().iconst(types::I64, value_disc::STRING);
                        let d = elem_disc(cx, d, src_disc);
                        let vv = bind_local(cx, name, d, s, LocalKind::String, Some(*id));
                        owned_leaves.push(BoundElem::String { var: vv.payload });
                    }
                    LeafShape::Value(vk) => {
                        let get = cx.helper("graphix_valarray_get_value")?;
                        let call = cx.b.ins().call(get, &[elem_ptr, idx_c]);
                        let (d, p) = {
                            let r = cx.b.inst_results(call);
                            (r[0], r[1])
                        };
                        let d = elem_disc(cx, d, src_disc);
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
            Ok((BoundElem::Composite { var }, owned_leaves))
        }
        Some(AbiKind::String) => {
            if !elem.leaves.is_empty() {
                return Err(anyhow!(
                    "destructure leaves on a string HOF element — caller bug"
                ));
            }
            // `graphix_valarray_get_arcstr` returns an OWNED (refcount-bumped)
            // ArcStr; bound as a `LocalKind::String` local so a mid-body
            // pending exit drops it via `drop_owned_composites` (the same
            // free coverage composite elements get).
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
            // `graphix_valarray_get_value` returns an OWNED two-word Value;
            // bound as the matching `LocalKind::{Variant,Nullable,Value}`
            // local (uniform drop, mid-body-pending coverage as above).
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
            let kind = match kernel_abi::abi_kind(cx.registry(), elem.typ) {
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

/// Drop the owned destructure-leaf clones of one iteration — emitted
/// once the body (or predicate) has fully consumed them: after the
/// output push in map (a body result may BORROW a leaf local until the
/// push copies it), and right after the predicate in filter/find (the
/// kept/found edges move only the ELEMENT — leaves are never moved).
/// The mid-body pending exit needs nothing here: each owned leaf is an
/// env local of its kind, dropped by `drop_owned_composites`.
fn drop_owned_leaves(cx: &mut BodyCx, leaves: &[BoundElem]) -> Result<()> {
    for l in leaves {
        drop_owned_elem(cx, l)?;
    }
    Ok(())
}

/// Drop an owned per-iteration element (no-op for scalars): the
/// counterpart of [`bind_elem`]'s owned arms, dispatching the matching
/// sentinel-guarded drop helper.
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

/// Drop the input array when the caller passed it owned — emitted at
/// the single post-loop merge point of each scaffold. A borrowed
/// array (`owned: false`) emits nothing.
/// Register an OWNED input array (a fresh producer the caller just
/// emitted — literal, slice, inlined-HOF result) for pending cleanup:
/// a DynCall / `?` / bottom-abort that pends inside the loop body
/// frees it from `emit_pending_cleanup` via the ValArray-typed
/// `owned_input_stack` (the buf stack uses the buf destructor — wrong
/// type). Pair with [`drop_owned_src`] after the loop: drop on the
/// normal path, pop the registration (cleanup on pend, explicit drop
/// otherwise — exactly once on either path). A Borrowed source is
/// env-owned and needs neither.
fn adopt_owned_src(cx: &mut BodyCx, arr: &ArraySrc) {
    if arr.owned {
        let var = cx.b.declare_var(types::I64);
        cx.b.def_var(var, arr.ptr);
        cx.ctx.owned_input_stack.borrow_mut().push(var);
    }
}

fn drop_owned_src(cx: &mut BodyCx, arr: &ArraySrc) -> Result<()> {
    if arr.owned {
        let drop_helper = cx.helper("graphix_valarray_drop")?;
        cx.b.ins().call(drop_helper, &[arr.ptr]);
        cx.ctx.owned_input_stack.borrow_mut().pop();
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
///   may-bottom (tainted-disc) field aborts the kernel to pending
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
    let helper_name: &str = match kernel_abi::abi_kind(cx.registry(), typ) {
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
    // A tainted slot does NOT abort the kernel — the caller accumulates
    // it (SlotFlags) into the HOF's RESULT disc (#219). Pushing the
    // tainted slot is safe: its payload is the helper-safe placeholder,
    // and every push helper masks the tag byte through the `TagValue`
    // gateway before the value is cloned or stored.
    // Value-shape fields (Variant/Nullable/DateTime/Duration/Bytes/Map)
    // push both `(disc, payload)` registers; everything else pushes the
    // payload word.
    if kernel_abi::is_value_shape(cx.registry(), typ) {
        cx.b.ins().call(push, &[buf, cv.disc, cv.payload]);
    } else {
        cx.b.ins().call(push, &[buf, cv.payload]);
    }
    Ok(())
}

/// Loop-carried slot-flags accumulator: per-slot TAINT is OR-reduced
/// and per-slot STALE is AND-reduced across the loop.
///
/// TAINT: a tainted (bottom) body / predicate slot taints the WHOLE
/// HOF result — the node-walk's HOF node emits nothing while a slot is
/// incomplete — but never the kernel: unrelated outputs still fire
/// (#219; these paths used to runtime-abort the kernel). The pushed
/// slot values are the helper-safe placeholders, masked at every
/// helper boundary.
///
/// STALE (firing): the node-walk's HOF aggregation is SLOT-DRIVEN
/// (MapQ::update: emit iff resized or any slot pred emitted, and all
/// slots determined; plus an unconditional emit when the source fires
/// while EMPTY). With a per-instance state word (top-level HOF in the
/// root body — `BodyCx::claim_state_word`) `apply` implements that
/// rule EXACTLY: the word remembers the previous source length, so
/// "resized" is real and a same-length source fire with all-quiet
/// (const-callback) slots stays quiet, matching the node-walk
/// (fuzz/triage-fuzzer-v2/firing_000007). Without a word (a nested
/// HOF inside another loop) the stateless approximation stands — the
/// result fires iff the SOURCE fired or ANY slot body fired — a
/// deliberate residual duplicate-fire, never a wrong value
/// (suppressing source-only fires without length memory would
/// under-fire the shrink-with-unchanged-prefix case: no slot fires
/// but the shorter array MUST emit).
pub struct SlotFlags {
    taint: Variable,
    stale: Variable,
    /// The source's slot count (array length; `array::init`'s clamped
    /// `n`) — set by each loop emitter, consumed by `apply`'s exact
    /// resize detection. `None` disables exactness (approximation).
    len: Option<ClifValue>,
}

impl SlotFlags {
    pub fn new(cx: &mut BodyCx) -> Self {
        let taint = cx.b.declare_var(types::I64);
        let z = cx.b.ins().iconst(types::I64, 0);
        cx.b.def_var(taint, z);
        let stale = cx.b.declare_var(types::I64);
        // All-stale start: an empty loop contributes no firing.
        let st = cx.b.ins().iconst(types::I64, STALE);
        cx.b.def_var(stale, st);
        SlotFlags { taint, stale, len: None }
    }

    /// Record the source's slot count for `apply`'s exact resize
    /// detection. Every loop emitter calls this once, before the loop.
    pub fn set_len(&mut self, len: ClifValue) {
        self.len = Some(len);
    }

    /// Fold one slot's disc into the accumulators (inside the loop).
    pub fn fold(&self, cx: &mut BodyCx, disc: ClifValue) {
        let cur = cx.b.use_var(self.taint);
        let t = cx.b.ins().band_imm(disc, TAINT);
        let n = cx.b.ins().bor(cur, t);
        cx.b.def_var(self.taint, n);
        let cur = cx.b.use_var(self.stale);
        let sb = cx.b.ins().band_imm(disc, STALE);
        let n = cx.b.ins().band(cur, sb);
        cx.b.def_var(self.stale, n);
    }

    /// Fold ONLY the STALE (firing) bit of `disc` — fold's INIT
    /// participates in firing (init fired → the fold fires) but its
    /// TAINT rides the accumulator carry instead: a bottom init
    /// RECOVERS when the callback never consumes the acc, so folding
    /// its taint here would re-bottom the recovered result.
    pub fn fold_stale(&self, cx: &mut BodyCx, disc: ClifValue) {
        let cur = cx.b.use_var(self.stale);
        let sb = cx.b.ins().band_imm(disc, STALE);
        let n = cx.b.ins().band(cur, sb);
        cx.b.def_var(self.stale, n);
    }

    /// Finish the HOF's result disc (after the loop): OR the slot
    /// taint (plus each source's TAINT), and set the firing bit.
    ///
    /// With a state word available (this runs AFTER the loop closes,
    /// so a top-level HOF claims while a nested one inside another
    /// loop is refused — exactly the per-slot-memory boundary), the
    /// firing rule is MapQ's, exact:
    ///   fires = resized ∨ any-slot-fired ∨ (source-fired ∧ empty)
    /// where `resized` compares the remembered previous length
    /// (stored as len+1; 0 = no previous, so a fresh instance's first
    /// real array always counts as resized — which is also MapQ's
    /// unconditional emit when the source first arrives). The record
    /// is skipped while any source is TAINTED: the placeholder's
    /// length is phantom and the node-walk's slot machinery makes no
    /// progress on a missing input.
    ///
    /// Without a word: fires = source-fired ∨ any-slot-fired.
    pub fn apply(
        &self,
        cx: &mut BodyCx,
        mut r: CompiledExpr,
        srcs: &[ClifValue],
    ) -> CompiledExpr {
        let t = cx.b.use_var(self.taint);
        r.disc = cx.b.ins().bor(r.disc, t);
        // STALE words (bit SET = quiet): the loop's slot accumulator,
        // and the AND-fold over the sources. Source TAINT ORs into the
        // result as before.
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
        let exact = match self.len {
            Some(len) => cx.claim_state_word().map(|off| (len, off)),
            None => None,
        };
        match exact {
            None => {
                // Approximation: fires = source-fired ∨ any-slot-fired.
                let sb = cx.b.ins().band(slots_word, src_word);
                r.disc = cx.b.ins().bor(r.disc, sb);
            }
            Some((len, off)) => {
                let sp = cx.state_ptr();
                let stored = cx.b.ins().load(types::I64, MemFlags::trusted(), sp, off);
                let lenp1 = cx.b.ins().iadd_imm(len, 1);
                let resized = cx.b.ins().icmp(IntCC::NotEqual, stored, lenp1);
                let valid = cx.b.ins().icmp_imm(IntCC::Equal, src_taint, 0);
                let recorded = cx.b.ins().select(valid, lenp1, stored);
                cx.b.ins().store(MemFlags::trusted(), recorded, sp, off);
                let slot_fired = cx.b.ins().icmp_imm(IntCC::Equal, slots_word, 0);
                let src_fired = cx.b.ins().icmp_imm(IntCC::Equal, src_word, 0);
                let empty = cx.b.ins().icmp_imm(IntCC::Equal, len, 0);
                let src_empty = cx.b.ins().band(src_fired, empty);
                let fires = cx.b.ins().bor(resized, slot_fired);
                let fires = cx.b.ins().bor(fires, src_empty);
                let quiet = cx.b.ins().iconst(types::I64, STALE);
                let zero = cx.b.ins().iconst(types::I64, 0);
                let sbit = cx.b.ins().select(fires, zero, quiet);
                r.disc = cx.b.ins().bor(r.disc, sbit);
            }
        }
        r
    }
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
    n_disc: ClifValue,
    n_prim: PrimType,
    idx_name: &ArcStr,
    idx_id: Option<crate::BindId>,
    out_typ: &Type,
    out_src: CompositeSource,
    mut body: F,
) -> Result<(ClifValue, SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let mut taint = SlotFlags::new(cx);
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let n_widened = widen_to_i64(cx.b, n_raw, n_prim)?;
    let zero_clamp = cx.b.ins().iconst(types::I64, 0);
    let is_neg = cx.b.ins().icmp(IntCC::SignedLessThan, n_widened, zero_clamp);
    let n_val = cx.b.ins().select(is_neg, zero_clamp, n_widened);
    // Runaway sizes abort the kernel to bottom, at the SAME limit the
    // node-walk's `array::init` logs+bottoms at (`MAX_ARRAY_INIT_LEN`):
    // `buf_new(i64:MAX)` would capacity-overflow-panic the result
    // buffer's reserve across the extern "C" boundary, killing the
    // process (the negative clamp above only handles the sign).
    {
        let max = cx.b.ins().iconst(types::I64, crate::node::array::MAX_ARRAY_INIT_LEN);
        let valid = cx.b.ins().icmp(IntCC::SignedLessThanOrEqual, n_val, max);
        emit_bottom_abort(cx.b, cx.env, cx.ctx, valid)?;
    }
    taint.set_len(n_val);
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
    // Cooperative interrupt poll at the scaffold loop head: a wedged
    // map/fold/filter/… over a huge array aborts to bottom; the in-flight
    // result buffer (on `dyncall_buf_stack`) is freed by the abort path.
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let mark = cx.env.mark();
    // The index "fires" with the count (see `elem_disc`).
    let idisc = {
        let base = scalar_disc(cx.b, PrimType::I64);
        elem_disc(cx, base, n_disc)
    };
    let idv = cx.b.declare_var(types::I64);
    cx.b.def_var(idv, idisc);
    bind_scalar_var_with_disc(cx, idx_name.clone(), PrimType::I64, i_var, idv, idx_id);
    cx.enter_loop();
    let cv = body(cx);
    cx.exit_loop();
    let cv = cv?;
    taint.fold(cx, cv.disc);
    push_field(cx, buf, cv, out_typ, out_src)?;
    cx.env.truncate(mark);
    let i_now = cx.b.use_var(i_var);
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    Ok((finalize_buf(cx, buf)?, taint))
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
) -> Result<(ClifValue, SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let mut taint = SlotFlags::new(cx);
    adopt_owned_src(cx, &arr);
    let (len, buf) = input_sized_buf(cx, arr.ptr)?;
    taint.set_len(len);
    let i_var = init_counter(cx);
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    // Cooperative interrupt poll at the scaffold loop head: a wedged
    // map/fold/filter/… over a huge array aborts to bottom; the in-flight
    // result buffer (on `dyncall_buf_stack`) is freed by the abort path.
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let i_now = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    let (bound, owned_leaves) = bind_elem(cx, arr.disc, arr.ptr, i_now, elem)?;
    cx.enter_loop();
    let cv = body(cx);
    cx.exit_loop();
    let cv = cv?;
    taint.fold(cx, cv.disc);
    push_field(cx, buf, cv, out_typ, out_src)?;
    // Leaves drop AFTER the push — a body result may BORROW a leaf local
    // until the push copies it into the buf.
    drop_owned_leaves(cx, &owned_leaves)?;
    drop_owned_elem(cx, &bound)?;
    cx.env.truncate(mark);
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    let result = finalize_buf(cx, buf)?;
    drop_owned_src(cx, &arr)?;
    Ok((result, taint))
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
) -> Result<(ClifValue, SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let mut taint = SlotFlags::new(cx);
    // The kept element is MOVED into the output; the not-kept edge must DROP
    // an owned element (composite/string/value). A scalar owns nothing.
    let owns_drop = !matches!(
        kernel_abi::abi_kind(cx.registry(), elem.typ),
        Some(AbiKind::Scalar(_))
    );
    adopt_owned_src(cx, &arr);
    let (len, buf) = input_sized_buf(cx, arr.ptr)?;
    taint.set_len(len);
    let i_var = init_counter(cx);
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let push_block = cx.b.create_block();
    let advance = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    // not-kept owned composite element → drop before advancing
    let drop_block = if owns_drop { Some(cx.b.create_block()) } else { None };
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    // Cooperative interrupt poll at the scaffold loop head: a wedged
    // map/fold/filter/… over a huge array aborts to bottom; the in-flight
    // result buffer (on `dyncall_buf_stack`) is freed by the abort path.
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let i_now = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    let (bound, owned_leaves) = bind_elem(cx, arr.disc, arr.ptr, i_now, elem)?;
    cx.enter_loop();
    let keep_cv = pred(cx);
    cx.exit_loop();
    let keep_cv = keep_cv?;
    taint.fold(cx, keep_cv.disc);
    // A tainted predicate's placeholder keep flag is 0 (not kept) —
    // the slot contributes nothing, and the accumulated taint bottoms
    // the whole result anyway.
    let keep = keep_cv.payload;
    // Leaves are consumed by the predicate; the kept edge moves only the
    // ELEMENT into the output, so drop leaves once, pre-branch.
    drop_owned_leaves(cx, &owned_leaves)?;
    cx.env.truncate(mark);
    let not_kept = drop_block.unwrap_or(advance);
    cx.b.ins().brif(keep, push_block, &[], not_kept, &[]);
    cx.b.switch_to_block(push_block);
    // Kept: MOVE the owned element into the output buf (consumes it, so no
    // drop on this edge). A value-shape element pushes both words.
    match &bound {
        BoundElem::Scalar { prim, var } => {
            let push = cx.helper(value_buf_push_helper(*prim)?)?;
            let v = cx.b.use_var(*var);
            cx.b.ins().call(push, &[buf, v]);
        }
        BoundElem::Composite { var } => {
            let push = cx.helper("graphix_value_buf_push_array")?;
            let v = cx.b.use_var(*var);
            cx.b.ins().call(push, &[buf, v]);
        }
        BoundElem::String { var } => {
            let push = cx.helper("graphix_value_buf_push_string")?;
            let v = cx.b.use_var(*var);
            cx.b.ins().call(push, &[buf, v]);
        }
        BoundElem::Value { disc, payload } => {
            let push = cx.helper("graphix_value_buf_push_value")?;
            let d = cx.b.use_var(*disc);
            let p = cx.b.use_var(*payload);
            cx.b.ins().call(push, &[buf, d, p]);
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
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(advance);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    let result = finalize_buf(cx, buf)?;
    drop_owned_src(cx, &arr)?;
    Ok((result, taint))
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
    elem_id: Option<crate::BindId>,
    out_elem: PrimType,
    mut body: F,
) -> Result<(ClifValue, SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let mut taint = SlotFlags::new(cx);
    let get_helper = cx.helper(valarray_get_helper(in_elem)?)?;
    let push = cx.helper(value_buf_push_helper(out_elem)?)?;
    adopt_owned_src(cx, &arr);
    let (len, buf) = input_sized_buf(cx, arr.ptr)?;
    taint.set_len(len);
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
    // Cooperative interrupt poll at the scaffold loop head: a wedged
    // map/fold/filter/… over a huge array aborts to bottom; the in-flight
    // result buffer (on `dyncall_buf_stack`) is freed by the abort path.
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let i_now = cx.b.use_var(i_var);
    let call = cx.b.ins().call(get_helper, &[arr.ptr, i_now]);
    let elem_val = cx.b.inst_results(call)[0];
    let elem_var = cx.b.declare_var(prim_to_clif(in_elem));
    cx.b.def_var(elem_var, elem_val);
    let mark = cx.env.mark();
    // The element fires with its source (see `elem_disc`).
    let ed = {
        let base = scalar_disc(cx.b, in_elem);
        elem_disc(cx, base, arr.disc)
    };
    let edv = cx.b.declare_var(types::I64);
    cx.b.def_var(edv, ed);
    bind_scalar_var_with_disc(cx, elem_name.clone(), in_elem, elem_var, edv, elem_id);
    cx.enter_loop();
    let cv = body(cx);
    cx.exit_loop();
    let cv = cv?;
    cx.env.truncate(mark);
    // A tainted body slot (e.g. `elem / 0`) taints the whole HOF
    // result; its placeholder is Null (skipped by the null test).
    taint.fold(cx, cv.disc);
    let disc = clean_disc(cx.b, cv.disc);
    let payload = cv.payload;
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
    Ok((result, taint))
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
) -> Result<(ClifValue, SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let mut taint = SlotFlags::new(cx);
    let extend = cx.helper("graphix_value_buf_extend_from_array")?;
    adopt_owned_src(cx, &arr);
    let (len, buf) = input_sized_buf(cx, arr.ptr)?;
    taint.set_len(len);
    let i_var = init_counter(cx);
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    // Cooperative interrupt poll at the scaffold loop head: a wedged
    // map/fold/filter/… over a huge array aborts to bottom; the in-flight
    // result buffer (on `dyncall_buf_stack`) is freed by the abort path.
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let i_now = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    let (bound, owned_leaves) = bind_elem(cx, arr.disc, arr.ptr, i_now, elem)?;
    cx.enter_loop();
    let body_cv = body(cx);
    cx.exit_loop();
    let body_cv = body_cv?;
    taint.fold(cx, body_cv.disc);
    drop_owned_leaves(cx, &owned_leaves)?;
    drop_owned_elem(cx, &bound)?;
    cx.env.truncate(mark);
    cx.b.ins().call(extend, &[buf, body_cv.payload]);
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    let result = finalize_buf(cx, buf)?;
    drop_owned_src(cx, &arr)?;
    Ok((result, taint))
}

/// `array::fold(arr, init, |acc, x| body)` — a scalar accumulator
/// Variable threaded through the loop. Per iteration the acc is bound
/// FIRST, then the element (this binding order is load-bearing); the
/// body's result re-defines the acc Variable. No output buf, no
/// pending-cleanup registration.
///
/// The `init`/`body` closures return a register-scalar payload whose
/// #219 taint has ALREADY been forced: callers wrap the compiled node in
/// [`super::emit_forced`], which RUNTIME-aborts the whole kernel to bottom
/// if the value taints (folds to no branch for a definitely-valid value).
/// So a may-bottom fold body fuses and bottoms at runtime — faithful,
/// since a bottom acc poisons every later iteration (the whole fold
/// blocks). Same convention for [`emit_filter_loop`]/[`emit_find_loop`]
/// predicates and [`emit_flat_map_loop`] bodies, and for the map path
/// ([`push_field`]). There is NO build-time may-bottom de-fuse.
pub fn emit_fold_loop<'a, 'f, 'c, I, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    acc_prim: PrimType,
    acc_name: &ArcStr,
    acc_id: Option<crate::BindId>,
    elem: &HofElem,
    init: I,
    mut body: F,
) -> Result<(CompiledExpr, SlotFlags)>
where
    I: FnOnce(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    adopt_owned_src(cx, &arr);
    let len = input_len(cx, arr.ptr)?;
    let acc_var = cx.b.declare_var(prim_to_clif(acc_prim));
    // The acc's TAINT and STALE are LOOP-CARRIED in its own disc
    // Variable — the node-walk's per-slot dataflow means a bottom init
    // poisons the fold ONLY while the callback actually consumes the
    // accumulator (`|acc, x| x` recovers on the first iteration;
    // `|acc, x| acc + x` stays bottom). A kernel abort here diverged
    // from the node-walk (fuzz/triage-fuzzer-v2/divergence_000003:
    // init 1/0, callback ignoring acc — interp folds to the last
    // element, jit bottomed). Each carry re-bases on the clean scalar
    // tag so only TAINT and STALE ride. STALE must ride too: rebasing
    // to the always-fired scalar tag made an acc-consuming body read
    // FIRED on every kernel run, so a fold over a quiet const array
    // re-fired forever — a `s <- fold(a, …)` self-connect busy-spun
    // where the node-walk quiesced (findings/hof-connect-jun2026/01,
    // re-caught by the trace oracle after the SlotFlags rework).
    let acc_disc_var = cx.b.declare_var(types::I64);
    let mut taint = SlotFlags::new(cx);
    taint.set_len(len);
    let init_cv = init(cx)?;
    taint.fold_stale(cx, init_cv.disc);
    cx.b.def_var(acc_var, init_cv.payload);
    let base = scalar_disc(cx.b, acc_prim);
    let t = cx.b.ins().band_imm(init_cv.disc, TAINT | STALE);
    let d0 = cx.b.ins().bor(base, t);
    cx.b.def_var(acc_disc_var, d0);
    let i_var = init_counter(cx);
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    // Cooperative interrupt poll at the scaffold loop head: a wedged
    // map/fold/filter/… over a huge array aborts to bottom; the in-flight
    // result buffer (on `dyncall_buf_stack`) is freed by the abort path.
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let i_now = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    bind_scalar_var_with_disc(
        cx,
        acc_name.clone(),
        acc_prim,
        acc_var,
        acc_disc_var,
        acc_id,
    );
    let (bound, owned_leaves) = bind_elem(cx, arr.disc, arr.ptr, i_now, elem)?;
    cx.enter_loop();
    let new_acc = body(cx);
    cx.exit_loop();
    let new_acc = new_acc?;
    drop_owned_leaves(cx, &owned_leaves)?;
    drop_owned_elem(cx, &bound)?;
    cx.env.truncate(mark);
    taint.fold(cx, new_acc.disc);
    cx.b.def_var(acc_var, new_acc.payload);
    let base = scalar_disc(cx.b, acc_prim);
    let t = cx.b.ins().band_imm(new_acc.disc, TAINT | STALE);
    let d = cx.b.ins().bor(base, t);
    cx.b.def_var(acc_disc_var, d);
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    drop_owned_src(cx, &arr)?;
    let payload = cx.b.use_var(acc_var);
    let disc = cx.b.use_var(acc_disc_var);
    Ok((CompiledExpr::new(disc, payload), taint))
}

/// `array::find(arr, |x| pred)` — the result is the `Nullable<elem>`
/// `(disc, payload)` pair for the FIRST element whose predicate (I8
/// keep flag) is true (null when none match). The loop scans EVERY
/// element — no early exit: the node-walk's find aggregator requires
/// every slot's predicate to have completed before it emits, so a
/// bottom predicate on a slot AFTER the match must still bottom the
/// whole find (the early-exiting loop returned the match where the
/// node-walk produced nothing). The first match is stashed in
/// Variables (wrap CONSUMES the owned element on the take edge; every
/// other iteration's element drops on the discard edge) and
/// first-match-wins is enforced by the found flag.
pub fn emit_find_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    mut pred: F,
) -> Result<((ClifValue, ClifValue), SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let mut taint = SlotFlags::new(cx);
    adopt_owned_src(cx, &arr);
    let len = input_len(cx, arr.ptr)?;
    taint.set_len(len);
    let found_var = cx.b.declare_var(types::I8);
    let zero8 = cx.b.ins().iconst(types::I8, 0);
    cx.b.def_var(found_var, zero8);
    let rdisc_var = cx.b.declare_var(types::I64);
    let null_disc = cx.b.ins().iconst(types::I64, value_disc::NULL);
    cx.b.def_var(rdisc_var, null_disc);
    let rpay_var = cx.b.declare_var(types::I64);
    let zero64 = cx.b.ins().iconst(types::I64, 0);
    cx.b.def_var(rpay_var, zero64);
    let i_var = init_counter(cx);
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
    // Cooperative interrupt poll at the scaffold loop head: a wedged
    // map/fold/filter/… over a huge array aborts to bottom; the in-flight
    // result buffer (on `dyncall_buf_stack`) is freed by the abort path.
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let i_now = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    let (bound, owned_leaves) = bind_elem(cx, arr.disc, arr.ptr, i_now, elem)?;
    cx.enter_loop();
    let keep_cv = pred(cx);
    cx.exit_loop();
    let keep_cv = keep_cv?;
    // A tainted predicate slot taints the whole find; its placeholder
    // keep flag is 0, so it never takes.
    taint.fold(cx, keep_cv.disc);
    // Leaves are consumed by the predicate; the take edge moves only
    // the ELEMENT into the result, so drop leaves once, pre-branch.
    drop_owned_leaves(cx, &owned_leaves)?;
    cx.env.truncate(mark);
    let not_found_yet = {
        let f = cx.b.use_var(found_var);
        cx.b.ins().icmp_imm(IntCC::Equal, f, 0)
    };
    let take = cx.b.ins().band(keep_cv.payload, not_found_yet);
    cx.b.ins().brif(take, take_block, &[], discard, &[]);
    cx.b.switch_to_block(take_block);
    cx.b.seal_block(take_block);
    let (disc, payload) = match &bound {
        BoundElem::Scalar { var, prim } => {
            let elem_again = cx.b.use_var(*var);
            let disc = cx.b.ins().iconst(types::I64, prim_to_value_disc(*prim));
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
        BoundElem::String { var } => {
            // Wrap the owned ArcStr into a value-shape `(STRING_DISC, payload)`
            // Value (consumes it).
            let wrap = cx.helper("graphix_value_new_string")?;
            let s = cx.b.use_var(*var);
            let call = cx.b.ins().call(wrap, &[s]);
            let r = cx.b.inst_results(call);
            (r[0], r[1])
        }
        BoundElem::Value { disc, payload } => {
            // The element is already a two-word Value — pass it through (move).
            (cx.b.use_var(*disc), cx.b.use_var(*payload))
        }
    };
    cx.b.def_var(rdisc_var, disc);
    cx.b.def_var(rpay_var, payload);
    let one8 = cx.b.ins().iconst(types::I8, 1);
    cx.b.def_var(found_var, one8);
    cx.b.ins().jump(advance, &[]);
    cx.b.switch_to_block(discard);
    cx.b.seal_block(discard);
    // Not taken this iteration — drop an owned element.
    drop_owned_elem(cx, &bound)?;
    cx.b.ins().jump(advance, &[]);
    cx.b.switch_to_block(advance);
    cx.b.seal_block(advance);
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    let disc = cx.b.use_var(rdisc_var);
    let payload = cx.b.use_var(rpay_var);
    drop_owned_src(cx, &arr)?;
    Ok(((disc, payload), taint))
}

/// `array::find_map(arr, |x| body)` — the result is the FIRST
/// non-null `Nullable<out>` `(disc, payload)` body result (or null).
/// Like [`emit_find_loop`] the loop scans EVERY element — no early
/// exit, the node-walk requires every slot complete — stashing the
/// first non-null pair and DROPPING every other body result (an owned
/// pair not taken must be freed; dropping the null placeholder is
/// harmless). The closure's pair must be OWNED and independent of the
/// element — the element is dropped before the null test.
pub fn emit_find_map_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    mut body: F,
) -> Result<((ClifValue, ClifValue), SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<(ClifValue, ClifValue)>,
{
    let mut taint = SlotFlags::new(cx);
    adopt_owned_src(cx, &arr);
    let len = input_len(cx, arr.ptr)?;
    taint.set_len(len);
    let found_var = cx.b.declare_var(types::I8);
    let zero8 = cx.b.ins().iconst(types::I8, 0);
    cx.b.def_var(found_var, zero8);
    let rdisc_var = cx.b.declare_var(types::I64);
    let null_disc = cx.b.ins().iconst(types::I64, value_disc::NULL);
    cx.b.def_var(rdisc_var, null_disc);
    let rpay_var = cx.b.declare_var(types::I64);
    let zero64 = cx.b.ins().iconst(types::I64, 0);
    cx.b.def_var(rpay_var, zero64);
    let i_var = init_counter(cx);
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
    // Cooperative interrupt poll at the scaffold loop head: a wedged
    // map/fold/filter/… over a huge array aborts to bottom; the in-flight
    // result buffer (on `dyncall_buf_stack`) is freed by the abort path.
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let i_now = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    let (bound, owned_leaves) = bind_elem(cx, arr.disc, arr.ptr, i_now, elem)?;
    cx.enter_loop();
    let bp = body(cx);
    cx.exit_loop();
    let (bdisc, bpayload) = bp?;
    // The body result is already OWNED (the caller ensures ownership
    // before the merge), so the element and leaves drop safely here.
    drop_owned_leaves(cx, &owned_leaves)?;
    drop_owned_elem(cx, &bound)?;
    cx.env.truncate(mark);
    // A tainted body slot taints the whole find_map; its placeholder is
    // Null (never taken). Clean the disc for the null test.
    taint.fold(cx, bdisc);
    let clean = clean_disc(cx.b, bdisc);
    let non_null = cx.b.ins().icmp_imm(IntCC::NotEqual, clean, value_disc::NULL);
    let not_found_yet = {
        let f = cx.b.use_var(found_var);
        cx.b.ins().icmp_imm(IntCC::Equal, f, 0)
    };
    let take = cx.b.ins().band(non_null, not_found_yet);
    cx.b.ins().brif(take, take_block, &[], discard, &[]);
    cx.b.switch_to_block(take_block);
    cx.b.seal_block(take_block);
    cx.b.def_var(rdisc_var, clean);
    cx.b.def_var(rpay_var, bpayload);
    let one8 = cx.b.ins().iconst(types::I8, 1);
    cx.b.def_var(found_var, one8);
    cx.b.ins().jump(advance, &[]);
    cx.b.switch_to_block(discard);
    cx.b.seal_block(discard);
    // Not taken — drop the owned pair (a null or placeholder pair
    // drops harmlessly; `graphix_value_drop` masks the tag).
    let vdrop = cx.helper("graphix_value_drop")?;
    cx.b.ins().call(vdrop, &[bdisc, bpayload]);
    cx.b.ins().jump(advance, &[]);
    cx.b.switch_to_block(advance);
    cx.b.seal_block(advance);
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    let disc = cx.b.use_var(rdisc_var);
    let payload = cx.b.use_var(rpay_var);
    drop_owned_src(cx, &arr)?;
    Ok(((disc, payload), taint))
}
