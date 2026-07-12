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
/// An element delivers FIRED: the sequential node-walk `For` binds
/// the element (and acc) into `event.variables` on EVERY loop run, so
/// a body that consumes them fires — loop-result firing is gated on
/// the BODY-evaluATION discs (the slots-word), not on element
/// freshness. `src_disc` is kept in the signature for taint
/// uniformity (elements of a valid array are untainted; a tainted
/// source bottoms the loop via the sticky flags fold).
fn elem_disc(_cx: &mut BodyCx, base: ClifValue, _src_disc: ClifValue) -> ClifValue {
    base
}

/// [`elem_disc`] with a caller-chosen carry mask: fold the masked bits
/// of `src_disc` onto `base`. Elements carry STALE only (a valid
/// array's elements are untainted); a fold ACCUMULATOR's destructure
/// leaves carry TAINT|STALE (the acc's taint is loop-carried).
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

/// `len = valarray_len(arr_ptr)` — the input-length read every
/// array-consuming scaffold opens with.
fn input_len(cx: &mut BodyCx, arr_ptr: ClifValue) -> Result<ClifValue> {
    let len_helper = cx.helper("graphix_valarray_len")?;
    let call = cx.b.ins().call(len_helper, &[arr_ptr]);
    Ok(cx.b.inst_results(call)[0])
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

/// One call-depth unit covering a whole scaffold loop — the kernel
/// twin of the node-walk's per-element callback dispatch
/// (`GXLambda::update` pushes one unit per element, but sequential
/// dispatches all sit at the SAME level, so one enter/pop pair around
/// the loop reproduces the trip point exactly at zero per-element
/// cost). On a trip the returned loop bound is zero — the inlined
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
/// STALE (firing): COARSE SEQUENTIAL (sync-subset P4) — the loop is
/// ONE reactive node and fires iff a loop INPUT fired (the source
/// array, the init, or a captured external — the interp `For` top
/// gate) AND the evaluation was an event (a body evaluation fired,
/// the source is empty, or the first-complete-run priming). Elements
/// and the acc deliver FIRED by convention, so the input gate is
/// what keeps an inline loop quiet when an UNRELATED region input
/// invoked the kernel (see [`Self::apply`]). The old per-slot
/// precision (exact resize state-words, the chain-only fold rule)
/// died with the per-slot machinery.
pub struct SlotFlags {
    taint: Variable,
    stale: Variable,
    /// The source's element count — see [`Self::set_len`].
    len: Option<ClifValue>,
    /// The loop INIT's disc — see [`Self::set_init_disc`]. Feeds ONLY
    /// the empty-source firing term in [`Self::apply`]: a fold over an
    /// EMPTY source IS its init, so an init fire must fire the result
    /// (the node-walk emits the init per gate pass). For a non-empty
    /// source the init's firing flows through acc-chain CONSUMPTION
    /// (body-driven) and this disc is not consulted — which is what
    /// keeps `fold(const_arr, firing_init, |a,b| K)` quiet (the
    /// hof-lift-firing pin).
    init_disc: Option<ClifValue>,
    /// I8 bool: this evaluation is the loop's FIRST with both source
    /// and init present (the `For::primed` twin) — [`Self::apply`]
    /// forces the result FIRED. `None` = stateless fallback.
    first_run: Option<ClifValue>,
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
        SlotFlags { taint, stale, len: None, init_disc: None, first_run: None }
    }

    /// Record the INIT's disc for the empty-source firing term.
    pub fn set_init_disc(&mut self, disc: ClifValue) {
        self.init_disc = Some(disc);
    }

    /// Record the first-complete-run bool (see the field).
    pub fn set_first_run(&mut self, first: Option<ClifValue>) {
        self.first_run = first;
    }

    /// OR a disc's TAINT into the sticky taint word WITHOUT touching
    /// the slots-word — for loop INPUTS (the init) whose bottom must
    /// bottom the loop but whose firing must not count as a body
    /// evaluation firing.
    pub fn fold_taint(&self, cx: &mut BodyCx, disc: ClifValue) {
        let cur = cx.b.use_var(self.taint);
        let t = cx.b.ins().band_imm(disc, TAINT);
        let n = cx.b.ins().bor(cur, t);
        cx.b.def_var(self.taint, n);
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

    /// Finish the loop's result disc (after the loop): OR the sticky
    /// taint word (a bottomed body evaluation or init bottoms the
    /// loop — the sequential node-walk's break), then set the firing
    /// bit.
    ///
    /// FIRING IS BODY-DRIVEN (matching the sequential node-walk `For`,
    /// which delivers the element and acc as FIRED on every run and
    /// emits iff every body evaluation produced): the loop result
    /// fires iff any body evaluation fired (the slots-word — a body
    /// that consumes its element, acc, or a fired capture fires; a
    /// bare-const body stays quiet after the kernel's init view, its
    /// callee reporting STALE through `CALLEE_RESULT_FLAGS`), or the
    /// source is EMPTY and an input fired (no body evaluations exist
    /// to gate on — the fold emits its init).
    pub fn apply(
        &self,
        cx: &mut BodyCx,
        mut r: CompiledExpr,
        srcs: &[ClifValue],
        fire_srcs: &[ClifValue],
    ) -> CompiledExpr {
        let t = cx.b.use_var(self.taint);
        r.disc = cx.b.ins().bor(r.disc, t);
        // STALE (bit SET = quiet). `srcs` (the source array): STALE
        // AND-folds into the empty-source term AND their TAINT is
        // strict (a missing array bottoms the loop — the node-walk
        // returns None without a source). `fire_srcs` (the body's
        // captured externals): STALE only — their TAINT rides
        // per-consumption through the body's own dataflow (a capture
        // read only in a sleeping select arm must NOT bottom the
        // loop: the interp emits at cycle 0 with the capture still
        // missing — soak jul10c fuzz 000000, interp 5 vs jit 4).
        let slots_word = cx.b.use_var(self.stale);
        let mut src_quiet = cx.b.ins().iconst(types::I64, STALE);
        for s in srcs {
            let ss = cx.b.ins().band_imm(*s, STALE);
            src_quiet = cx.b.ins().band(src_quiet, ss);
            let st = cx.b.ins().band_imm(*s, TAINT);
            r.disc = cx.b.ins().bor(r.disc, st);
        }
        for s in fire_srcs {
            let ss = cx.b.ins().band_imm(*s, STALE);
            src_quiet = cx.b.ins().band(src_quiet, ss);
        }
        // The INIT is an "input" for the empty-source term: an empty
        // fold's result IS the init, so an init fire fires it. Taint
        // is already strict via `fold_taint`; only the STALE bit joins
        // here, and `src_quiet` is consulted ONLY on the empty branch.
        if let Some(d) = self.init_disc {
            let ss = cx.b.ins().band_imm(d, STALE);
            src_quiet = cx.b.ins().band(src_quiet, ss);
        }
        // The loop-INPUT gate, the interp `For` top gate's twin
        // (`!(iter_up || init_up || ext_fired || event.init) → None`):
        // a loop EVALUATION only happens when one of the loop's own
        // inputs fired — elements/acc deliver FIRED by convention, so
        // an elem-consuming body fires whenever the loop RUNS, and an
        // inline loop runs whenever its REGION is invoked. Without
        // the gate, an unrelated region input (a select guard's
        // feeder) re-fired a const-fed fold on every kernel
        // invocation where the node-walk's gated For stayed quiet
        // (jul12d 000000: `select sync { …for…for… } { _ if in0… }`
        // re-emitted per in0 event). `src_quiet` is exactly the gate
        // word: sources + captured externals + the init all quiet.
        //
        // quiet = empty ? sources-quiet
        //       : slots-quiet OR loop-inputs-quiet
        let quiet = match self.len {
            Some(len) => {
                let empty = cx.b.ins().icmp_imm(IntCC::Equal, len, 0);
                let gated = cx.b.ins().bor(slots_word, src_quiet);
                cx.b.ins().select(empty, src_quiet, gated)
            }
            None => slots_word,
        };
        // The first COMPLETE run fires unconditionally (the `For::primed`
        // init-force twin — see `first_run`).
        let quiet = match self.first_run {
            Some(first) => {
                let zero = cx.b.ins().iconst(types::I64, 0);
                cx.b.ins().select(first, zero, quiet)
            }
            None => quiet,
        };
        r.disc = cx.b.ins().band_imm(r.disc, !STALE);
        r.disc = cx.b.ins().bor(r.disc, quiet);
        r
    }
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
/// `Some(empty)`. (Moved here from the array package for the `For`
/// node's emitter — sync-subset P4 final.)
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
    // Variable. (Historically a bottom init poisoned the fold only
    // while the callback consumed the acc — FoldQ's per-slot dataflow;
    // strict sequential semantics now bottom the whole loop via the
    // sticky flags fold above, and the carry just keeps values
    // coherent.) Each carry re-bases on the clean scalar
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
    // The init's TAINT is STICKY (`let mut acc = ⊥` bottoms the loop —
    // the node-walk `For` returns None on a missing init). Its STALE
    // must NOT touch the slots-word: firing is body-driven — but it
    // DOES join the empty-source term (an empty fold IS its init).
    taint.fold_taint(cx, init_cv.disc);
    taint.set_init_disc(init_cv.disc);
    // FIRST-COMPLETE-RUN priming (the node-walk `For::primed` twin):
    // the loop's first ITERATING evaluation with BOTH the source and
    // the init present is init-like — its result FIRES even though a
    // const body's evaluations stay quiet. The kernel's own init view
    // can predate an async-late input (`fold(arr, iter(..), f)` — the
    // init arrives cycles after the kernel's first dispatch), so the
    // priming is a per-instance state word consumed on the first
    // untainted NON-EMPTY (source, init) pair, NOT the init flag. An
    // empty evaluation must not consume it (its firing is the
    // empty-source term's job, and the interp's `For::primed` primes
    // on the first iterating run — jul10h 000007 diverged on an
    // iter-driven source that grows 0→n). SEMANTIC state (survives
    // reset_replay, like `For::primed`). Stateless fallback (nested
    // loops, callee bodies): the init view was the only priming — the
    // pre-existing behavior.
    let first_run = cx.claim_state_word().map(|off| {
        let sp = cx.state_ptr();
        let primed = cx.b.ins().load(types::I64, MemFlags::trusted(), sp, off);
        let it = cx.b.ins().band_imm(init_cv.disc, TAINT);
        let init_ok = cx.b.ins().icmp_imm(IntCC::Equal, it, 0);
        let at = cx.b.ins().band_imm(arr.disc, TAINT);
        let arr_ok = cx.b.ins().icmp_imm(IntCC::Equal, at, 0);
        let ok = cx.b.ins().band(init_ok, arr_ok);
        let non_empty = cx.b.ins().icmp_imm(IntCC::NotEqual, len, 0);
        let ok = cx.b.ins().band(ok, non_empty);
        let ok64 = cx.b.ins().uextend(types::I64, ok);
        let np = cx.b.ins().bor(primed, ok64);
        cx.b.ins().store(MemFlags::trusted(), np, sp, off);
        let unprimed = cx.b.ins().icmp_imm(IntCC::Equal, primed, 0);
        cx.b.ins().band(unprimed, ok)
    });
    taint.set_first_run(first_run);
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
    let t = cx.b.ins().band_imm(init_cv.disc, TAINT);
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
            bind_leaves(cx, acc_ptr, acc_disc, TAINT, leaves)?
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
    // bottomed body evaluation bottoms the whole loop, matching the
    // sequential node-walk's break (never-until-complete). This
    // deliberately reverts the per-slot recovery pin (soak finding
    // corpus-generate/divergence_000021 — a later acc-ignoring
    // callback recovered under FoldQ's per-slot dataflow); sequential
    // semantics ARE the semantics now. The INIT's taint still rides
    // ONLY the carry (`d0` above, never the sticky word): a bottom
    // init with a callback that never consumes the acc recovers on the
    // first iteration in BOTH modes (the node-walk skip-binds the
    // missing acc).
    taint.fold(cx, new_acc.disc);
    cx.b.def_var(acc_var, new_pay);
    let base = acc.base_disc(cx);
    let t = cx.b.ins().band_imm(new_acc.disc, TAINT);
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
