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
/// An element has the source collection's event freshness. A source
/// update delivers every element to its retained callback slot; an
/// unrelated kernel input leaves those bindings stale.
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
/// `base_ptr`, each under its pattern `BindId` (BindId-first
/// resolution — the synthetic composite name is never looked up).
/// Scalar leaves are by-value copies (the composite's own drop covers
/// the allocation); composite / string / value leaves are OWNED
/// clones — bound as env locals of the matching kind (so a mid-body
/// pending exit drops them via `drop_owned_composites`) and returned
/// for the loop's normal-path [`drop_owned_leaves`]. Each leaf disc
/// folds `src_disc & mask` onto its shape base ([`carry_disc`]).
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
            // Destructure leaves — see [`bind_leaves`]. Elements carry
            // STALE only (a valid array's elements are untainted).
            let owned_leaves = bind_leaves(cx, elem_ptr, src_disc, 0, elem.leaves)?;
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

fn register_hof_buf(b: &mut FunctionBuilder, ctx: &LowerCtx, buf: ClifValue) {
    let buf_var = b.declare_var(types::I64);
    b.def_var(buf_var, buf);
    ctx.dyncall_buf_stack.borrow_mut().push(buf_var);
}

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

/// One call-depth unit covering a whole collection scaffold — the
/// kernel twin of the node-walk's per-element callback dispatch.
/// Every element dispatches at the same depth, so one enter/pop pair
/// around the scaffold reproduces the trip point without per-element
/// cost. On a trip the returned loop bound is zero — the inlined
/// body, and any charged call sites inside it, must not run — and the
/// result rides TAINT through `taint` (the node-walk's per-element
/// dispatch yields bottom, so its HOF never emits; a tainted kernel
/// result bottoms the same consumers). `graphix_depth_enter`
/// increments UNCONDITIONALLY so the paired [`emit_depth_unit_exit`]
/// after the loop never branches. Every loop emitter calls both, once.
fn emit_depth_unit(
    cx: &mut BodyCx,
    taint: &SlotFlags,
    bound: ClifValue,
) -> Result<ClifValue> {
    let enter = cx.helper("graphix_depth_enter")?;
    let call = cx.b.ins().call(enter, &[]);
    let ok = cx.b.inst_results(call)[0];
    let tripped = cx.b.ins().icmp_imm(IntCC::Equal, ok, 0);
    let stale = cx.b.ins().iconst(types::I64, STALE);
    let tainted = cx.b.ins().iconst(types::I64, TAINT | STALE);
    let synth = cx.b.ins().select(tripped, tainted, stale);
    taint.fold(cx, synth);
    let zero = cx.b.ins().iconst(types::I64, 0);
    Ok(cx.b.ins().select(tripped, zero, bound))
}

/// Exit the [`emit_depth_unit`] entered before the loop. An interrupt
/// abort inside the loop skips this pop; the cycle-end `depth_reset`
/// clears the leak (the cycle is being torn down anyway).
fn emit_depth_unit_exit(cx: &mut BodyCx) -> Result<()> {
    let pop = cx.helper("graphix_depth_pop")?;
    cx.b.ins().call(pop, &[]);
    Ok(())
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
///   may-bottom (tainted-disc) slot does NOT abort — the caller
///   accumulates its taint into the HOF result disc (see the body
///   comment below).
/// - **Array/Tuple/Struct**: `graphix_value_buf_push_array` (owned)
///   or `_borrowed` (refcount-bumped), by `src`.
/// - **Variant/Nullable/Value**: `graphix_value_buf_push_value` or
///   `_borrowed`, picked the same way; pushes both `(disc, payload)`
///   words.
/// - **String**: `graphix_value_buf_push_string`, UNCONDITIONALLY —
///   `src` is ignored because string SSA is always owned (every
///   producer on both paths — ConstStr, Concat, Local/Ref reads —
///   hands out a fresh refcount). The
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
/// STALE (firing): the native loop represents one collection node and
/// fires iff a loop input fired and the evaluation produced an event.
/// Elements and the accumulator deliver FIRED by convention, so the
/// input gate keeps an inline loop quiet when an unrelated region input
/// invoked the kernel (see [`Self::apply`]).
pub struct SlotFlags {
    taint: Variable,
    stale: Variable,
    len: Option<ClifValue>,
    result_is_firing: bool,
    src_invariant: bool,
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
        SlotFlags {
            taint,
            stale,
            len: None,
            result_is_firing: false,
            src_invariant: false,
        }
    }

    pub fn set_src_invariant(&mut self) {
        self.src_invariant = true;
    }

    pub fn result_is_firing(&mut self) {
        self.result_is_firing = true;
    }

    /// Record the source's element count for `apply`'s empty-source
    /// term (a fold over an EMPTY array emits its init when an input
    /// fired — there are no body evaluations to gate on).
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

    /// Apply the exact MapQ/FoldQ firing rule when a state word is
    /// available. Nested loops over variant sources use the conservative
    /// source-or-slot approximation because one shared length cannot
    /// represent independent per-iteration slot sets.
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
        let fired_word = if self.result_is_firing {
            cx.b.ins().band_imm(r.disc, STALE)
        } else {
            slots_word
        };
        r.disc = cx.b.ins().band_imm(r.disc, !STALE);
        let exact = match self.len {
            Some(len) => {
                let claim = if self.src_invariant {
                    cx.claim_state_word_loop_invariant()
                } else {
                    cx.claim_state_word()
                };
                claim.map(|off| (len, off))
            }
            None => None,
        };
        match exact {
            None => {
                let stale = if self.result_is_firing {
                    fired_word
                } else {
                    cx.b.ins().band(fired_word, src_word)
                };
                r.disc = cx.b.ins().bor(r.disc, stale);
            }
            Some((len, off)) => {
                let sp = cx.state_ptr();
                let stored = cx.b.ins().load(types::I64, MemFlags::trusted(), sp, off);
                let lenp1 = cx.b.ins().iadd_imm(len, 1);
                let resized = cx.b.ins().icmp(IntCC::NotEqual, stored, lenp1);
                let valid = cx.b.ins().icmp_imm(IntCC::Equal, src_taint, 0);
                let recorded = cx.b.ins().select(valid, lenp1, stored);
                cx.b.ins().store(MemFlags::trusted(), recorded, sp, off);
                let slot_fired = cx.b.ins().icmp_imm(IntCC::Equal, fired_word, 0);
                let src_fired = cx.b.ins().icmp_imm(IntCC::Equal, src_word, 0);
                let empty = cx.b.ins().icmp_imm(IntCC::Equal, len, 0);
                let src_empty = cx.b.ins().band(src_fired, empty);
                let fires = cx.b.ins().bor(resized, slot_fired);
                let fires = cx.b.ins().bor(fires, src_empty);
                let quiet = cx.b.ins().iconst(types::I64, STALE);
                let zero = cx.b.ins().iconst(types::I64, 0);
                let stale = cx.b.ins().select(fires, zero, quiet);
                r.disc = cx.b.ins().bor(r.disc, stale);
            }
        }
        r
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
    mut body: F,
) -> Result<(ClifValue, SlotFlags)>
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
    let stale = cx.b.ins().iconst(types::I64, STALE);
    let tainted = cx.b.ins().iconst(types::I64, TAINT | STALE);
    let disc = cx.b.ins().select(oversize, tainted, stale);
    flags.fold(cx, disc);
    let n = cx.b.ins().select(oversize, zero, n);
    flags.set_len(n);
    let n = emit_depth_unit(cx, &flags, n)?;
    let call = cx.b.ins().call(buf_new, &[n]);
    let buf = cx.b.inst_results(call)[0];
    register_hof_buf(cx.b, cx.ctx, buf);
    let i_var = init_counter(cx);
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
    emit_depth_unit_exit(cx)?;
    Ok((finalize_buf(cx, buf)?, flags))
}

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
    let mut flags = SlotFlags::new(cx);
    adopt_owned_src(cx, &arr);
    let (len, buf) = input_sized_buf(cx, arr.ptr)?;
    flags.set_len(len);
    let len = emit_depth_unit(cx, &flags, len)?;
    let i_var = init_counter(cx);
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
    emit_depth_unit_exit(cx)?;
    let result = finalize_buf(cx, buf)?;
    drop_owned_src(cx, &arr)?;
    Ok((result, flags))
}

pub fn emit_filter_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    mut predicate: F,
) -> Result<(ClifValue, SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let mut flags = SlotFlags::new(cx);
    let owns_drop = !matches!(
        kernel_abi::abi_kind(cx.registry(), elem.typ),
        Some(AbiKind::Scalar(_))
    );
    adopt_owned_src(cx, &arr);
    let (len, buf) = input_sized_buf(cx, arr.ptr)?;
    flags.set_len(len);
    let len = emit_depth_unit(cx, &flags, len)?;
    let i_var = init_counter(cx);
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
    emit_depth_unit_exit(cx)?;
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
    mut body: F,
) -> Result<(ClifValue, SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let mut flags = SlotFlags::new(cx);
    adopt_owned_src(cx, &arr);
    let (len, buf) = input_sized_buf(cx, arr.ptr)?;
    flags.set_len(len);
    let len = emit_depth_unit(cx, &flags, len)?;
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
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let i = cx.b.use_var(i_var);
    let mark = cx.env.mark();
    cx.enter_loop();
    let (bound, leaves) = bind_elem(cx, arr.disc, arr.ptr, i, elem)?;
    let value = body(cx);
    cx.exit_loop();
    let value = value?;
    cx.env.truncate(mark);
    flags.fold(cx, value.disc);
    let disc = clean_disc(cx.b, value.disc);
    let is_null = cx.b.ins().icmp_imm(IntCC::Equal, disc, value_disc::NULL);
    cx.b.ins().brif(is_null, advance, &[], push_block, &[]);
    cx.b.switch_to_block(push_block);
    match kernel_abi::abi_kind(cx.registry(), out_elem) {
        Some(AbiKind::Scalar(prim)) => {
            let push = cx.helper(value_buf_push_helper(prim)?)?;
            let scalar = cast_u64_to_prim(cx.b, value.payload, prim);
            cx.b.ins().call(push, &[buf, scalar]);
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let helper = match out_src {
                CompositeSource::Owned => "graphix_value_buf_push_array",
                CompositeSource::Borrowed => "graphix_value_buf_push_array_borrowed",
            };
            let push = cx.helper(helper)?;
            cx.b.ins().call(push, &[buf, value.payload]);
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let helper = match out_src {
                CompositeSource::Owned => "graphix_value_buf_push_value",
                CompositeSource::Borrowed => "graphix_value_buf_push_value_borrowed",
            };
            let push = cx.helper(helper)?;
            cx.b.ins().call(push, &[buf, value.disc, value.payload]);
        }
        Some(AbiKind::String) => {
            let push = cx.helper("graphix_value_buf_push_string")?;
            cx.b.ins().call(push, &[buf, value.payload]);
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
    emit_depth_unit_exit(cx)?;
    let result = finalize_buf(cx, buf)?;
    drop_owned_src(cx, &arr)?;
    Ok((result, flags))
}

pub fn emit_flat_map_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    mut body: F,
) -> Result<(ClifValue, SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let mut flags = SlotFlags::new(cx);
    let extend = cx.helper("graphix_value_buf_extend_from_array")?;
    adopt_owned_src(cx, &arr);
    let (len, buf) = input_sized_buf(cx, arr.ptr)?;
    flags.set_len(len);
    let len = emit_depth_unit(cx, &flags, len)?;
    let i_var = init_counter(cx);
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
    let value = value?;
    flags.fold(cx, value.disc);
    drop_owned_leaves(cx, &leaves)?;
    drop_owned_elem(cx, &bound)?;
    cx.env.truncate(mark);
    cx.b.ins().call(extend, &[buf, value.payload]);
    emit_increment(cx, i_var, i, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    emit_depth_unit_exit(cx)?;
    let result = finalize_buf(cx, buf)?;
    drop_owned_src(cx, &arr)?;
    Ok((result, flags))
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
/// Destructure-leaf shapes for a `|(k, v)|`-style pattern over a
/// tuple-typed value: per bound leaf, its pattern `BindId`, tuple
/// position, and [`LeafShape`]. `None` when the (frozen) type isn't a
/// tuple or a bound position has no register/heap shape — those
/// callers node-walk. Empty binds (single-name pattern) is trivially
/// `Some(empty)`.
pub fn elem_leaves(
    reg: &kernel_abi::AbstractRegistry,
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
            let shape = match kernel_abi::abi_kind(reg, ts.get(*i)?) {
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

/// The fold accumulator's shape — how the loop-carried value is held,
/// bound for the body, made owned, and dropped when replaced.
pub enum FoldAcc<'a> {
    /// A register scalar in a prim-typed Variable. Owns nothing.
    Scalar(PrimType),
    /// An OWNED `*mut ValArray` (array / tuple / struct) in an I64
    /// Variable. The loop owns the current acc: each iteration the
    /// body's result is made independently owned (`ensure_owned` per
    /// `body_src` — a borrowed Ref result clones) and the old acc is
    /// dropped. `leaves` are the `|(a, b), v|` destructure leaves of
    /// the acc pattern, re-read off the CURRENT acc each iteration
    /// with TAINT|STALE carried from the acc disc.
    Composite {
        init_src: CompositeSource,
        body_src: CompositeSource,
        leaves: &'a [(crate::BindId, usize, LeafShape)],
    },
    /// An OWNED `ArcStr` in an I64 Variable. String reads always CLONE
    /// (`LocalKind::String`), so init and body results are already
    /// independently owned — no `ensure_owned` step; the old acc still
    /// drops when replaced.
    Str,
}

impl FoldAcc<'_> {
    fn local_kind(&self) -> LocalKind {
        match self {
            FoldAcc::Scalar(p) => LocalKind::Scalar(*p),
            FoldAcc::Composite { .. } => LocalKind::Composite,
            FoldAcc::Str => LocalKind::String,
        }
    }

    /// The clean (untainted, fired) disc for the carried acc shape —
    /// each carry re-bases on this so only TAINT and STALE ride.
    fn base_disc(&self, cx: &mut BodyCx) -> ClifValue {
        match self {
            FoldAcc::Scalar(p) => scalar_disc(cx.b, *p),
            FoldAcc::Composite { .. } => cx.b.ins().iconst(types::I64, value_disc::ARRAY),
            FoldAcc::Str => cx.b.ins().iconst(types::I64, value_disc::STRING),
        }
    }

    /// Drop the old carried acc when a new one replaces it (and on the
    /// loop's pending-abort edges it is dropped as the env local the
    /// loop binds it to — `drop_owned_composites`). Emits nothing for
    /// a scalar (not even the `use_var` — scalar CLIF is preserved
    /// instruction-for-instruction).
    fn drop_old(&self, cx: &mut BodyCx, acc_var: Variable) -> Result<()> {
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
        }
        Ok(())
    }
}

pub fn emit_fold_loop<'a, 'f, 'c, I, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    acc: FoldAcc,
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
    let acc_var = cx.b.declare_var(match &acc {
        FoldAcc::Scalar(p) => prim_to_clif(*p),
        FoldAcc::Composite { .. } | FoldAcc::Str => types::I64,
    });
    // The acc's TAINT and STALE are LOOP-CARRIED in its own disc
    // Variable. Each carry re-bases on the clean scalar
    // tag so only TAINT and STALE ride. STALE must ride too: rebasing
    // to the always-fired scalar tag made an acc-consuming body read
    // FIRED on every kernel run, so a fold over a quiet const array
    // re-fired forever — a `s <- fold(a, …)` self-connect busy-spun
    // where the node-walk quiesced (findings/hof-connect-jun2026/01,
    // re-caught by the trace oracle after the SlotFlags rework).
    let acc_disc_var = cx.b.declare_var(types::I64);
    let mut taint = SlotFlags::new(cx);
    taint.set_len(len);
    taint.result_is_firing();
    let init_cv = init(cx)?;
    // A pointer-shaped acc is loop-OWNED from the start: a borrowed
    // init (a Ref to a kernel input / outer local) clones here. String
    // reads already clone; a scalar owns nothing.
    let init_pay = match &acc {
        FoldAcc::Composite { init_src, .. } => {
            ensure_owned_composite_src(cx, *init_src, init_cv.payload)?
        }
        FoldAcc::Scalar(_) | FoldAcc::Str => init_cv.payload,
    };
    cx.b.def_var(acc_var, init_pay);
    let base = acc.base_disc(cx);
    let t = cx.b.ins().band_imm(init_cv.disc, TAINT | STALE);
    let d0 = cx.b.ins().bor(base, t);
    cx.b.def_var(acc_disc_var, d0);
    // After the init emit — the node-walk evaluates fold's init at the
    // CALLER's depth level; only the per-element callback dispatch
    // enters a unit.
    let len = emit_depth_unit(cx, &taint, len)?;
    let i_var = init_counter(cx);
    let loop_header = cx.b.create_block();
    let loop_body = cx.b.create_block();
    let loop_exit = cx.b.create_block();
    cx.b.ins().jump(loop_header, &[]);
    cx.b.switch_to_block(loop_header);
    emit_loop_header(cx, i_var, len, loop_body, loop_exit);
    cx.b.switch_to_block(loop_body);
    let mark = cx.env.mark();
    cx.enter_loop();
    // The acc binds BEFORE the interrupt poll (env bookkeeping — no
    // instructions): an owned acc shape must be in the poll's abort
    // cleanup (`drop_owned_composites`) or an interrupt would leak the
    // loop-carried value.
    cx.env.bind(
        acc_name.clone(),
        ValueVar { disc: acc_disc_var, payload: acc_var },
        acc.local_kind(),
        acc_id,
    );
    // Cooperative interrupt poll at the scaffold loop head: a wedged
    // map/fold/filter/… over a huge array aborts to bottom; the in-flight
    // result buffer (on `dyncall_buf_stack`) is freed by the abort path.
    emit_interrupt_check(cx.b, cx.env, cx.ctx)?;
    let i_now = cx.b.use_var(i_var);
    let (bound, owned_leaves) = bind_elem(cx, arr.disc, arr.ptr, i_now, elem)?;
    // `|(a, b), v|` acc-destructure leaves re-read off the CURRENT acc
    // each iteration; their discs carry the acc's loop-carried
    // TAINT|STALE (unlike elements, the acc can be tainted).
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
    let new_acc = new_acc?;
    // The new acc is made independently owned BEFORE anything drops: a
    // borrowed body result (`|acc, x| acc`) may alias the old acc, an
    // element, or a leaf local.
    let new_pay = match &acc {
        FoldAcc::Composite { body_src, .. } => {
            ensure_owned_composite_src(cx, *body_src, new_acc.payload)?
        }
        FoldAcc::Scalar(_) | FoldAcc::Str => new_acc.payload,
    };
    acc.drop_old(cx, acc_var)?;
    drop_owned_leaves(cx, &acc_owned_leaves)?;
    drop_owned_leaves(cx, &owned_leaves)?;
    drop_owned_elem(cx, &bound)?;
    cx.env.truncate(mark);
    // The BODY EVALUATION's disc folds into the flags: its STALE is
    // one more coarse firing source, and its TAINT is STICKY — a
    // bottomed body evaluation bottoms the whole collection result,
    // matching the node-walk's incomplete slot chain. The INIT's taint
    // still rides only the carry (`d0` above, never the sticky word): a
    // bottom init with a callback that never consumes the acc can recover
    // on the first iteration in both modes.
    cx.b.def_var(acc_var, new_pay);
    let base = acc.base_disc(cx);
    let t = cx.b.ins().band_imm(new_acc.disc, TAINT | STALE);
    let d = cx.b.ins().bor(base, t);
    cx.b.def_var(acc_disc_var, d);
    emit_increment(cx, i_var, i_now, loop_header);
    cx.b.seal_block(loop_body);
    cx.b.seal_block(loop_header);
    cx.b.switch_to_block(loop_exit);
    cx.b.seal_block(loop_exit);
    emit_depth_unit_exit(cx)?;
    drop_owned_src(cx, &arr)?;
    let payload = cx.b.use_var(acc_var);
    let disc = cx.b.use_var(acc_disc_var);
    Ok((CompiledExpr::new(disc, payload), taint))
}

pub fn emit_find_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    mut predicate: F,
) -> Result<((ClifValue, ClifValue), SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<CompiledExpr>,
{
    let mut flags = SlotFlags::new(cx);
    adopt_owned_src(cx, &arr);
    let len = input_len(cx, arr.ptr)?;
    flags.set_len(len);
    let len = emit_depth_unit(cx, &flags, len)?;
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
            let wrap = cx.helper("graphix_value_new_from_array")?;
            let value = cx.b.use_var(*var);
            let call = cx.b.ins().call(wrap, &[value]);
            let result = cx.b.inst_results(call);
            (result[0], result[1])
        }
        BoundElem::String { var } => {
            let wrap = cx.helper("graphix_value_new_string")?;
            let value = cx.b.use_var(*var);
            let call = cx.b.ins().call(wrap, &[value]);
            let result = cx.b.inst_results(call);
            (result[0], result[1])
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
    emit_depth_unit_exit(cx)?;
    let disc = cx.b.use_var(result_disc_var);
    let payload = cx.b.use_var(result_payload_var);
    drop_owned_src(cx, &arr)?;
    Ok(((disc, payload), flags))
}

pub fn emit_find_map_loop<'a, 'f, 'c, F>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    arr: ArraySrc,
    elem: &HofElem,
    mut body: F,
) -> Result<((ClifValue, ClifValue), SlotFlags)>
where
    F: FnMut(&mut BodyCx<'a, 'f, 'c>) -> Result<(ClifValue, ClifValue)>,
{
    let mut flags = SlotFlags::new(cx);
    adopt_owned_src(cx, &arr);
    let len = input_len(cx, arr.ptr)?;
    flags.set_len(len);
    let len = emit_depth_unit(cx, &flags, len)?;
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
    emit_depth_unit_exit(cx)?;
    let disc = cx.b.use_var(result_disc_var);
    let payload = cx.b.use_var(result_payload_var);
    drop_owned_src(cx, &arr)?;
    Ok(((disc, payload), flags))
}
