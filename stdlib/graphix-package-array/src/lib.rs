#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use ahash::AHashSet;
use anyhow::{Result, bail};
use arcstr::ArcStr;
use compact_str::format_compact;
use graphix_compiler::{
    Apply, BindId, BuiltIn, Event, ExecCtx, LambdaId, Node, Refs, Rt, Scope, UserEvent,
    effects::EffectKind,
    expr::ExprId,
    fusion::{
        emit::{self, BodyCx, CompiledExpr, CompositeSource, scaffold},
        kernel_abi::{self, PrimType},
    },
    node::genn,
    typ::{FnType, Type},
};
use graphix_package_core::{
    CachedArgs, CachedVals, EvalCached, FoldFn, FoldQ, MapFn, MapQ, Slot,
};
use graphix_rt::GXRt;
use netidx::{publisher::Typ, subscriber::Value, utils::Either};
use netidx_value::ValArray;
use poolshark::local::LPooled;
use smallvec::{SmallVec, smallvec};
use std::{
    collections::{VecDeque, hash_map::Entry},
    fmt::Debug,
    iter,
};
use triomphe::Arc as TArc;

/// True if a (frozen) `Type`'s top-level shape is `Unit` (side-effect-
/// only) or bare `Null` — neither is a valid array element shape.
fn is_unit_or_null(reg: &kernel_abi::AbstractRegistry, t: &Type) -> bool {
    use kernel_abi::AbiKind;
    matches!(kernel_abi::abi_kind(reg, t), Some(AbiKind::Unit | AbiKind::Null))
}

/// Destructure leaves (`|(k, v)|` — pattern BindId + tuple position)
/// lowered to the `(id, position, prim)` triples
/// `scaffold::bind_elem` binds per iteration off the composite
/// element. `None` when the (frozen) element isn't a tuple carrying a
/// register scalar at every BOUND position — those callbacks
/// node-walk (composite leaves are a future widening). An empty
/// `elem_binds` (single-name callback) is trivially `Some(empty)`.
fn scalar_leaves(
    reg: &kernel_abi::AbstractRegistry,
    in_elem: &Type,
    elem_binds: &[(BindId, usize)],
) -> Option<Vec<(BindId, usize, PrimType)>> {
    if elem_binds.is_empty() {
        return Some(Vec::new());
    }
    let Type::Tuple(ts) = in_elem else { return None };
    elem_binds
        .iter()
        .map(|(id, i)| {
            ts.get(*i).and_then(|t| kernel_abi::scalar_prim(reg, t)).map(|p| (*id, *i, p))
        })
        .collect()
}

#[derive(Debug, Default)]
struct MapImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for MapImpl {
    type Collection = ValArray;

    const NAME: &str = "array_map";

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ValArray) -> Option<Value> {
        Some(Value::Array(ValArray::from_iter_exact(
            slots.iter().map(|s| s.cur.clone().unwrap()),
        )))
    }

    /// Direct-path map loop via `scaffold::emit_map_loop`. `Ok(None)`
    /// gates, all decided before the first emitted instruction: a
    /// destructured `|(k, v)|` callback (Stage D3), an element shape
    /// `scaffold::bind_elem` can't bind, an OWNED input array (a fresh
    /// producer — literal / slice / call result — needs the
    /// pending-cleanup registration that lands with the owned-arg
    /// stage; until then those sites DynCall or node-walk, the status
    /// quo), and a body result that won't freeze concrete or is
    /// Unit/bare-Null (the `is_unit_or_null` gate).
    fn emit_clif(
        cx: &mut BodyCx,
        array_arg: &Node<R, E>,
        body: &Node<R, E>,
        elem_name: &ArcStr,
        elem_id: Option<BindId>,
        in_elem: &Type,
        elem_binds: &[(BindId, usize)],
    ) -> Result<Option<CompiledExpr>> {
        use kernel_abi::AbiKind;
        // Element shape: only what `scaffold::bind_elem` accepts.
        let Some(in_elem) = kernel_abi::freeze_for_abi_normalized(cx.registry(), in_elem)
        else {
            return Ok(None);
        };
        // A destructured `|(k, v)|` callback binds per-leaf reads off
        // the composite element — register-scalar leaves only
        // (composite leaves node-walk).
        let Some(leaves) = scalar_leaves(cx.registry(), &in_elem, elem_binds) else {
            return Ok(None);
        };
        match kernel_abi::abi_kind(cx.registry(), &in_elem) {
            Some(
                AbiKind::Scalar(_)
                | AbiKind::Array
                | AbiKind::Tuple
                | AbiKind::Struct
                | AbiKind::String
                | AbiKind::Variant
                | AbiKind::Nullable
                | AbiKind::Value,
            ) => {}
            _ => return Ok(None),
        }
        // Borrowed inputs are env-owned; an OWNED input (fresh
        // producer — literal, slice, inlined-HOF result) is adopted by
        // the scaffold for both-path cleanup (the `ArraySrc` contract).
        let owned = emit::node_composite_source(array_arg) == CompositeSource::Owned;
        // Output element type is the body's result — `freeze_for_abi_normalized`
        // because typecheck can leave a select-valued body's type as the
        // un-flattened arm union.
        let Some(out_typ) =
            kernel_abi::freeze_for_abi_normalized(cx.registry(), body.typ())
        else {
            return Ok(None);
        };
        if is_unit_or_null(cx.registry(), &out_typ) {
            return Ok(None);
        }
        // Gates done — emit. From here a mismatch is a build bug, so
        // Err (abort the kernel), never Ok(None).
        let arr = emit::emit_forced_keep(cx, array_arg)?;
        let out_src = emit::node_composite_source(body);
        scaffold::emit_map_loop(
            cx,
            scaffold::ArraySrc { ptr: arr.payload, owned },
            &scaffold::HofElem {
                name: elem_name,
                id: elem_id,
                typ: &in_elem,
                leaves: &leaves,
            },
            &out_typ,
            out_src,
            |cx| body.emit_clif(cx),
        )
        .map(|v| {
            let r = emit::array_result(cx, v);
            Some(emit::inherit_hof_firing(cx, r, arr.disc, &[body], elem_id, elem_binds, None))
        })
    }
}

type Map<R, E> = MapQ<R, E, MapImpl>;

#[derive(Debug, Default)]
struct FilterImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for FilterImpl {
    type Collection = ValArray;

    const NAME: &str = "array_filter";

    fn finish(&mut self, slots: &[Slot<R, E>], a: &ValArray) -> Option<Value> {
        Some(Value::Array(ValArray::from_iter(slots.iter().zip(a.iter()).filter_map(
            |(p, v)| match p.cur {
                Some(Value::Bool(true)) => Some(v.clone()),
                _ => None,
            },
        ))))
    }

    /// Direct-path filter loop via `scaffold::emit_filter_loop`. Same
    /// `Ok(None)` gates as `MapImpl::emit_clif` (destructured callback,
    /// unbindable element shape, owned input array), plus the predicate
    /// type must freeze to `bool`. A may-bottom predicate (e.g. `10 / x`)
    /// fuses like a map body: `emit_forced` RUNTIME-aborts the whole
    /// filter to bottom if the predicate actually taints — faithful,
    /// since a bottom predicate has no keep-vs-drop answer, so the whole
    /// filter output blocks (== a tainted kernel result).
    fn emit_clif(
        cx: &mut BodyCx,
        array_arg: &Node<R, E>,
        body: &Node<R, E>,
        elem_name: &ArcStr,
        elem_id: Option<BindId>,
        in_elem: &Type,
        elem_binds: &[(BindId, usize)],
    ) -> Result<Option<CompiledExpr>> {
        use kernel_abi::AbiKind;
        // Element shape: only what `scaffold::bind_elem` accepts.
        let Some(in_elem) = kernel_abi::freeze_for_abi_normalized(cx.registry(), in_elem)
        else {
            return Ok(None);
        };
        // A destructured `|(k, v)|` callback binds per-leaf reads off
        // the composite element — register-scalar leaves only
        // (composite leaves node-walk).
        let Some(leaves) = scalar_leaves(cx.registry(), &in_elem, elem_binds) else {
            return Ok(None);
        };
        match kernel_abi::abi_kind(cx.registry(), &in_elem) {
            Some(
                AbiKind::Scalar(_)
                | AbiKind::Array
                | AbiKind::Tuple
                | AbiKind::Struct
                | AbiKind::String
                | AbiKind::Variant
                | AbiKind::Nullable
                | AbiKind::Value,
            ) => {}
            _ => return Ok(None),
        }
        // Borrowed inputs are env-owned; an OWNED input (fresh
        // producer — literal, slice, inlined-HOF result) is adopted by
        // the scaffold for both-path cleanup (the `ArraySrc` contract).
        let owned = emit::node_composite_source(array_arg) == CompositeSource::Owned;
        match kernel_abi::freeze_for_abi_normalized(cx.registry(), body.typ()) {
            Some(t)
                if matches!(
                    kernel_abi::scalar_prim(cx.registry(), &t),
                    Some(PrimType::Bool)
                ) => {}
            _ => return Ok(None),
        }
        // Gates done — emit. From here a mismatch is a build bug or a
        // de-fuse, so Err (abort the kernel), never Ok(None).
        let arr = emit::emit_forced_keep(cx, array_arg)?;
        scaffold::emit_filter_loop(
            cx,
            scaffold::ArraySrc { ptr: arr.payload, owned },
            &scaffold::HofElem {
                name: elem_name,
                id: elem_id,
                typ: &in_elem,
                leaves: &leaves,
            },
            |cx| emit::emit_forced(cx, body),
        )
        .map(|v| {
            let r = emit::array_result(cx, v);
            Some(emit::inherit_hof_firing(cx, r, arr.disc, &[body], elem_id, elem_binds, None))
        })
    }
}

type Filter<R, E> = MapQ<R, E, FilterImpl>;

#[derive(Debug, Default)]
struct FlatMapImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for FlatMapImpl {
    type Collection = ValArray;

    const NAME: &str = "array_flat_map";

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ValArray) -> Option<Value> {
        Some(Value::Array(ValArray::from_iter(slots.iter().flat_map(|s| {
            match s.cur.as_ref().unwrap() {
                Value::Array(a) => Either::Left(a.clone().into_iter()),
                v => Either::Right(iter::once(v.clone())),
            }
        }))))
    }

    /// Direct-path flat_map loop via `scaffold::emit_flat_map_loop`.
    /// Same `Ok(None)` gates as `MapImpl::emit_clif`, plus the body
    /// must freeze to `Array<scalar>` — the array-returning shape of
    /// the `['b, Array<'b>]` callback union, mirroring map's
    /// `array_scalar_prim` gate (a bare-element body node-walks). The
    /// body closure hands the scaffold an OWNED array for `extend` to
    /// consume — a Borrowed body source (e.g. a Ref to an outer
    /// array) is refcount-cloned per iteration.
    fn emit_clif(
        cx: &mut BodyCx,
        array_arg: &Node<R, E>,
        body: &Node<R, E>,
        elem_name: &ArcStr,
        elem_id: Option<BindId>,
        in_elem: &Type,
        elem_binds: &[(BindId, usize)],
    ) -> Result<Option<CompiledExpr>> {
        use kernel_abi::AbiKind;
        // Element shape: only what `scaffold::bind_elem` accepts.
        let Some(in_elem) = kernel_abi::freeze_for_abi_normalized(cx.registry(), in_elem)
        else {
            return Ok(None);
        };
        // A destructured `|(k, v)|` callback binds per-leaf reads off
        // the composite element — register-scalar leaves only
        // (composite leaves node-walk).
        let Some(leaves) = scalar_leaves(cx.registry(), &in_elem, elem_binds) else {
            return Ok(None);
        };
        match kernel_abi::abi_kind(cx.registry(), &in_elem) {
            Some(
                AbiKind::Scalar(_)
                | AbiKind::Array
                | AbiKind::Tuple
                | AbiKind::Struct
                | AbiKind::String
                | AbiKind::Variant
                | AbiKind::Nullable
                | AbiKind::Value,
            ) => {}
            _ => return Ok(None),
        }
        // Borrowed inputs are env-owned; an OWNED input (fresh
        // producer — literal, slice, inlined-HOF result) is adopted by
        // the scaffold for both-path cleanup (the `ArraySrc` contract).
        let owned = emit::node_composite_source(array_arg) == CompositeSource::Owned;
        match kernel_abi::freeze_for_abi_normalized(cx.registry(), body.typ())
            .as_ref()
            .and_then(|t| kernel_abi::array_scalar_prim(cx.registry(), t))
        {
            Some(_) => {}
            None => return Ok(None),
        }
        // Gates done — emit. From here a mismatch is a build bug or a
        // de-fuse, so Err (abort the kernel), never Ok(None).
        let arr = emit::emit_forced_keep(cx, array_arg)?;
        let body_src = emit::node_composite_source(body);
        scaffold::emit_flat_map_loop(
            cx,
            scaffold::ArraySrc { ptr: arr.payload, owned },
            &scaffold::HofElem {
                name: elem_name,
                id: elem_id,
                typ: &in_elem,
                leaves: &leaves,
            },
            |cx| {
                let p = emit::emit_forced(cx, body)?;
                emit::ensure_owned_composite_src(cx, body_src, p)
            },
        )
        .map(|v| {
            let r = emit::array_result(cx, v);
            Some(emit::inherit_hof_firing(cx, r, arr.disc, &[body], elem_id, elem_binds, None))
        })
    }
}

type FlatMap<R, E> = MapQ<R, E, FlatMapImpl>;

#[derive(Debug, Default)]
struct FilterMapImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for FilterMapImpl {
    type Collection = ValArray;

    const NAME: &str = "array_filter_map";

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ValArray) -> Option<Value> {
        Some(Value::Array(ValArray::from_iter(slots.iter().filter_map(|s| {
            match s.cur.as_ref().unwrap() {
                Value::Null => None,
                v => Some(v.clone()),
            }
        }))))
    }

    /// Direct-path filter_map loop via
    /// `scaffold::emit_filter_map_loop`. The scaffold is scalar-only
    /// on BOTH sides (it binds the element through the per-prim
    /// getter, no `bind_elem`, and casts the non-null payload back to
    /// the out prim), so the gates are: register-scalar element,
    /// body freezes to `Nullable<scalar>` (find_map's
    /// `nullable_inner` + `scalar_prim`). The scaffold itself Errs on
    /// a non-Value-shape body = build-time de-fuse.
    fn emit_clif(
        cx: &mut BodyCx,
        array_arg: &Node<R, E>,
        body: &Node<R, E>,
        elem_name: &ArcStr,
        elem_id: Option<BindId>,
        in_elem: &Type,
        elem_binds: &[(BindId, usize)],
    ) -> Result<Option<CompiledExpr>> {
        // The filter_map scaffold is scalar-elem-only (it binds the
        // element through the per-prim getter, no `bind_elem`), so
        // destructured callbacks node-walk — widen with #150.
        if !elem_binds.is_empty() {
            return Ok(None);
        }
        let in_prim = match kernel_abi::freeze_for_abi_normalized(cx.registry(), in_elem)
            .as_ref()
            .and_then(|t| kernel_abi::scalar_prim(cx.registry(), t))
        {
            Some(p) => p,
            None => return Ok(None),
        };
        // Borrowed inputs are env-owned; an OWNED input (fresh
        // producer — literal, slice, inlined-HOF result) is adopted by
        // the scaffold for both-path cleanup (the `ArraySrc` contract).
        let owned = emit::node_composite_source(array_arg) == CompositeSource::Owned;
        let out_prim =
            match kernel_abi::freeze_for_abi_normalized(cx.registry(), body.typ())
                .and_then(|t| kernel_abi::nullable_inner(cx.registry(), &t))
                .as_ref()
                .and_then(|t| kernel_abi::scalar_prim(cx.registry(), t))
            {
                Some(p) => p,
                None => return Ok(None),
            };
        // Gates done — emit. From here a mismatch is a build bug or a
        // de-fuse, so Err (abort the kernel), never Ok(None).
        let arr = emit::emit_forced_keep(cx, array_arg)?;
        scaffold::emit_filter_map_loop(
            cx,
            scaffold::ArraySrc { ptr: arr.payload, owned },
            in_prim,
            elem_name,
            elem_id,
            out_prim,
            |cx| body.emit_clif(cx),
        )
        .map(|v| {
            let r = emit::array_result(cx, v);
            Some(emit::inherit_hof_firing(cx, r, arr.disc, &[body], elem_id, elem_binds, None))
        })
    }
}

type FilterMap<R, E> = MapQ<R, E, FilterMapImpl>;

#[derive(Debug, Default)]
struct FindImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for FindImpl {
    type Collection = ValArray;

    const NAME: &str = "array_find";

    fn finish(&mut self, slots: &[Slot<R, E>], a: &ValArray) -> Option<Value> {
        let r = slots
            .iter()
            .enumerate()
            .find(|(_, s)| match s.cur.as_ref() {
                Some(Value::Bool(true)) => true,
                _ => false,
            })
            .map(|(i, _)| a[i].clone())
            .unwrap_or(Value::Null);
        Some(r)
    }

    /// Direct-path find loop via `scaffold::emit_find_loop` — early
    /// exit on the first matching element, result is the
    /// `Nullable<elem>` `(disc, payload)` pair. Same gates and
    /// predicate convention as `FilterImpl::emit_clif`: a may-bottom
    /// predicate fuses and `emit_forced` RUNTIME-aborts the whole find
    /// to bottom if it taints (a bottom predicate has no match/no-match
    /// answer; canonically the pred slot never fires and find's output
    /// blocks == a tainted kernel result).
    fn emit_clif(
        cx: &mut BodyCx,
        array_arg: &Node<R, E>,
        body: &Node<R, E>,
        elem_name: &ArcStr,
        elem_id: Option<BindId>,
        in_elem: &Type,
        elem_binds: &[(BindId, usize)],
    ) -> Result<Option<CompiledExpr>> {
        use kernel_abi::AbiKind;
        // Element shape: only what `scaffold::bind_elem` accepts.
        let Some(in_elem) = kernel_abi::freeze_for_abi_normalized(cx.registry(), in_elem)
        else {
            return Ok(None);
        };
        // A destructured `|(k, v)|` callback binds per-leaf reads off
        // the composite element — register-scalar leaves only
        // (composite leaves node-walk).
        let Some(leaves) = scalar_leaves(cx.registry(), &in_elem, elem_binds) else {
            return Ok(None);
        };
        match kernel_abi::abi_kind(cx.registry(), &in_elem) {
            Some(
                AbiKind::Scalar(_)
                | AbiKind::Array
                | AbiKind::Tuple
                | AbiKind::Struct
                | AbiKind::String
                | AbiKind::Variant
                | AbiKind::Nullable
                | AbiKind::Value,
            ) => {}
            _ => return Ok(None),
        }
        // Borrowed inputs are env-owned; an OWNED input (fresh
        // producer — literal, slice, inlined-HOF result) is adopted by
        // the scaffold for both-path cleanup (the `ArraySrc` contract).
        let owned = emit::node_composite_source(array_arg) == CompositeSource::Owned;
        match kernel_abi::freeze_for_abi_normalized(cx.registry(), body.typ()) {
            Some(t)
                if matches!(
                    kernel_abi::scalar_prim(cx.registry(), &t),
                    Some(PrimType::Bool)
                ) => {}
            _ => return Ok(None),
        }
        // Gates done — emit. From here a mismatch is a build bug or a
        // de-fuse, so Err (abort the kernel), never Ok(None).
        let arr = emit::emit_forced_keep(cx, array_arg)?;
        scaffold::emit_find_loop(
            cx,
            scaffold::ArraySrc { ptr: arr.payload, owned },
            &scaffold::HofElem {
                name: elem_name,
                id: elem_id,
                typ: &in_elem,
                leaves: &leaves,
            },
            |cx| emit::emit_forced(cx, body),
        )
        .map(|(disc, payload)| {
            let r = CompiledExpr::new(disc, payload);
            Some(emit::inherit_hof_firing(cx, r, arr.disc, &[body], elem_id, elem_binds, None))
        })
    }
}

type Find<R, E> = MapQ<R, E, FindImpl>;

#[derive(Debug, Default)]
struct FindMapImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for FindMapImpl {
    type Collection = ValArray;

    const NAME: &str = "array_find_map";

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ValArray) -> Option<Value> {
        let r = slots
            .iter()
            .find_map(|s| match s.cur.as_ref().unwrap() {
                Value::Null => None,
                v => Some(v.clone()),
            })
            .unwrap_or(Value::Null);
        Some(r)
    }

    /// Direct-path find_map loop via `scaffold::emit_find_map_loop` —
    /// early exit on the first non-null body result, which IS the
    /// kernel's `Nullable<out>` result. The body must freeze to a
    /// Nullable shape. The body pair leaves the
    /// loop as the result, so a Borrowed body source (a bare Ref read)
    /// is refcount-cloned via `ensure_owned_value_src` — the scaffold's
    /// owned-pair contract.
    fn emit_clif(
        cx: &mut BodyCx,
        array_arg: &Node<R, E>,
        body: &Node<R, E>,
        elem_name: &ArcStr,
        elem_id: Option<BindId>,
        in_elem: &Type,
        elem_binds: &[(BindId, usize)],
    ) -> Result<Option<CompiledExpr>> {
        use kernel_abi::AbiKind;
        // Element shape: only what `scaffold::bind_elem` accepts.
        let Some(in_elem) = kernel_abi::freeze_for_abi_normalized(cx.registry(), in_elem)
        else {
            return Ok(None);
        };
        // A destructured `|(k, v)|` callback binds per-leaf reads off
        // the composite element — register-scalar leaves only
        // (composite leaves node-walk).
        let Some(leaves) = scalar_leaves(cx.registry(), &in_elem, elem_binds) else {
            return Ok(None);
        };
        match kernel_abi::abi_kind(cx.registry(), &in_elem) {
            Some(
                AbiKind::Scalar(_)
                | AbiKind::Array
                | AbiKind::Tuple
                | AbiKind::Struct
                | AbiKind::String
                | AbiKind::Variant
                | AbiKind::Nullable
                | AbiKind::Value,
            ) => {}
            _ => return Ok(None),
        }
        // Borrowed inputs are env-owned; an OWNED input (fresh
        // producer — literal, slice, inlined-HOF result) is adopted by
        // the scaffold for both-path cleanup (the `ArraySrc` contract).
        let owned = emit::node_composite_source(array_arg) == CompositeSource::Owned;
        match kernel_abi::freeze_for_abi_normalized(cx.registry(), body.typ())
            .as_ref()
            .map(|t| kernel_abi::abi_kind(cx.registry(), t))
        {
            Some(Some(AbiKind::Nullable)) => {}
            _ => return Ok(None),
        }
        // Gates done — emit. From here a mismatch is a build bug or a
        // de-fuse, so Err (abort the kernel), never Ok(None).
        let arr = emit::emit_forced_keep(cx, array_arg)?;
        let body_src = emit::node_composite_source(body);
        let (disc, payload) = scaffold::emit_find_map_loop(
            cx,
            scaffold::ArraySrc { ptr: arr.payload, owned },
            &scaffold::HofElem {
                name: elem_name,
                id: elem_id,
                typ: &in_elem,
                leaves: &leaves,
            },
            |cx| {
                let cv = body.emit_clif(cx)?;
                emit::ensure_owned_value_src(cx, body_src, cv.disc, cv.payload)
            },
        )?;
        let r = CompiledExpr::new(disc, payload);
        Ok(Some(emit::inherit_hof_firing(cx, r, arr.disc, &[body], elem_id, elem_binds, None)))
    }
}

type FindMap<R, E> = MapQ<R, E, FindMapImpl>;

#[derive(Debug)]
struct FoldImpl;

impl<R: Rt, E: UserEvent> FoldFn<R, E> for FoldImpl {
    type Collection = ValArray;

    const NAME: &str = "array_fold";

    /// Direct-path fold loop via `scaffold::emit_fold_loop`. Same
    /// `Ok(None)` gates as `MapImpl::emit_clif`, plus the accumulator
    /// must be a register scalar whose prim the init and body types
    /// agree on (`scalar_prim(init)` +
    /// `body == acc` checks). The init and body both route through
    /// `emit_forced`: a may-bottom init/body fuses and RUNTIME-aborts
    /// the whole fold to bottom if it taints — faithful, since a bottom
    /// accumulator poisons every later iteration (the acc slot never
    /// fires and fold's output blocks == a tainted kernel result).
    fn emit_clif(
        cx: &mut BodyCx,
        array_arg: &Node<R, E>,
        init_arg: &Node<R, E>,
        body: &Node<R, E>,
        acc_name: &ArcStr,
        acc_id: Option<BindId>,
        elem_name: &ArcStr,
        elem_id: Option<BindId>,
        in_elem: &Type,
        elem_binds: &[(BindId, usize)],
    ) -> Result<Option<CompiledExpr>> {
        use kernel_abi::AbiKind;
        // Element shape: only what `scaffold::bind_elem` accepts.
        let Some(in_elem) = kernel_abi::freeze_for_abi_normalized(cx.registry(), in_elem)
        else {
            return Ok(None);
        };
        // A destructured `|acc, (k, v)|` callback binds per-leaf reads
        // off the composite element — register-scalar leaves only
        // (composite leaves node-walk).
        let Some(leaves) = scalar_leaves(cx.registry(), &in_elem, elem_binds) else {
            return Ok(None);
        };
        match kernel_abi::abi_kind(cx.registry(), &in_elem) {
            Some(
                AbiKind::Scalar(_)
                | AbiKind::Array
                | AbiKind::Tuple
                | AbiKind::Struct
                | AbiKind::String
                | AbiKind::Variant
                | AbiKind::Nullable
                | AbiKind::Value,
            ) => {}
            _ => return Ok(None),
        }
        // Borrowed inputs are env-owned; an OWNED input (fresh
        // producer — literal, slice, inlined-HOF result) is adopted by
        // the scaffold for both-path cleanup (the `ArraySrc` contract).
        let owned = emit::node_composite_source(array_arg) == CompositeSource::Owned;
        // The acc threads through the loop as a register Variable —
        // init and body must both freeze to the same register scalar.
        let acc_prim =
            match kernel_abi::freeze_for_abi_normalized(cx.registry(), init_arg.typ())
                .as_ref()
                .and_then(|t| kernel_abi::scalar_prim(cx.registry(), t))
            {
                Some(p) => p,
                None => return Ok(None),
            };
        match kernel_abi::freeze_for_abi_normalized(cx.registry(), body.typ())
            .as_ref()
            .and_then(|t| kernel_abi::scalar_prim(cx.registry(), t))
        {
            Some(p) if p == acc_prim => {}
            _ => return Ok(None),
        }
        // Gates done — emit. From here a mismatch is a build bug or a
        // de-fuse, so Err (abort the kernel), never Ok(None).
        let arr = emit::emit_forced_keep(cx, array_arg)?;
        scaffold::emit_fold_loop(
            cx,
            scaffold::ArraySrc { ptr: arr.payload, owned },
            acc_prim,
            acc_name,
            acc_id,
            &scaffold::HofElem {
                name: elem_name,
                id: elem_id,
                typ: &in_elem,
                leaves: &leaves,
            },
            // #219: a tainted init / body bottoms the whole fold (a
            // bottom acc poisons all later iterations).
            |cx| emit::emit_forced(cx, init_arg),
            |cx| emit::emit_forced(cx, body),
        )
        .map(|v| {
            let r = emit::scalar_result(cx, acc_prim, v);
            Some(emit::inherit_hof_firing(cx, r, arr.disc, &[body, init_arg], elem_id, elem_binds, acc_id))
        })
    }
}

type Fold<R, E> = FoldQ<R, E, FoldImpl>;

#[derive(Debug, Default)]
struct ConcatEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ConcatEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "array_concat";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let mut present = true;
        for v in from.0.iter() {
            match v {
                Some(Value::Array(a)) => {
                    for v in a.iter() {
                        self.0.push(v.clone())
                    }
                }
                Some(v) => self.0.push(v.clone()),
                None => present = false,
            }
        }
        if present {
            let a = ValArray::from_iter_exact(self.0.drain(..));
            Some(Value::Array(a))
        } else {
            self.0.clear();
            None
        }
    }
}

type Concat = CachedArgs<ConcatEv>;

#[derive(Debug, Default)]
struct PushBackEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for PushBackEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "array_push_back";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let mut present = true;
        match &from.0[..] {
            [Some(Value::Array(a)), tl @ ..] => {
                self.0.extend(a.iter().map(|v| v.clone()));
                for v in tl {
                    match v {
                        Some(v) => self.0.push(v.clone()),
                        None => present = false,
                    }
                }
            }
            [] | [None, ..] | [Some(_), ..] => present = false,
        }
        if present {
            let a = ValArray::from_iter_exact(self.0.drain(..));
            Some(Value::Array(a))
        } else {
            self.0.clear();
            None
        }
    }
}

type PushBack = CachedArgs<PushBackEv>;

#[derive(Debug, Default)]
struct PushFrontEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for PushFrontEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "array_push_front";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let mut present = true;
        match &from.0[..] {
            [Some(Value::Array(a)), tl @ ..] => {
                for v in tl {
                    match v {
                        Some(v) => self.0.push(v.clone()),
                        None => present = false,
                    }
                }
                self.0.extend(a.iter().map(|v| v.clone()));
            }
            [] | [None, ..] | [Some(_), ..] => present = false,
        }
        if present {
            let a = ValArray::from_iter_exact(self.0.drain(..));
            Some(Value::Array(a))
        } else {
            self.0.clear();
            None
        }
    }
}

type PushFront = CachedArgs<PushFrontEv>;

#[derive(Debug, Default)]
struct WindowEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for WindowEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "array_window";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let mut present = true;
        match &from.0[..] {
            [Some(Value::I64(window)), Some(Value::Array(a)), tl @ ..] => {
                let window = *window as usize;
                let total = a.len() + tl.len();
                if total <= window {
                    self.0.extend(a.iter().cloned());
                    for v in tl {
                        match v {
                            Some(v) => self.0.push(v.clone()),
                            None => present = false,
                        }
                    }
                } else if a.len() >= (total - window) {
                    self.0.extend(a[(total - window)..].iter().cloned());
                    for v in tl {
                        match v {
                            Some(v) => self.0.push(v.clone()),
                            None => present = false,
                        }
                    }
                } else {
                    for v in &tl[tl.len() - window..] {
                        match v {
                            Some(v) => self.0.push(v.clone()),
                            None => present = false,
                        }
                    }
                }
            }
            [] | [_] | [_, None, ..] | [None, _, ..] | [Some(_), Some(_), ..] => {
                present = false
            }
        }
        if present {
            let a = ValArray::from_iter_exact(self.0.drain(..));
            Some(Value::Array(a))
        } else {
            self.0.clear();
            None
        }
    }
}

type Window = CachedArgs<WindowEv>;

#[derive(Debug, Default)]
struct LenEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for LenEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "array_len";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::Array(a)) => Some(Value::I64(a.len() as i64)),
            Some(_) | None => None,
        }
    }

    // `array::len(arr)` lowers to a pure array-length read (a
    // length read off the array kernel input) — no DynCall needed. Only
    // fuses when the array arg resolves to a kernel input; otherwise
    // `None` falls back to DynCall.
}

type Len = CachedArgs<LenEv>;

#[derive(Debug, Default)]
struct FlattenEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for FlattenEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "array_flatten";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::Array(a)) => {
                for v in a.iter() {
                    match v {
                        Value::Array(a) => self.0.extend(a.iter().map(|v| v.clone())),
                        v => self.0.push(v.clone()),
                    }
                }
                let a = ValArray::from_iter_exact(self.0.drain(..));
                Some(Value::Array(a))
            }
            Some(_) | None => None,
        }
    }
}

type Flatten = CachedArgs<FlattenEv>;

#[derive(Debug, Default)]
struct SortEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for SortEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "array_sort";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fn cn(v: &Value) -> Value {
            v.clone().cast(Typ::F64).unwrap_or_else(|| v.clone())
        }
        match &from.0[..] {
            [
                Some(Value::String(dir)),
                Some(Value::Bool(numeric)),
                Some(Value::Array(a)),
            ] => match &**dir {
                "Ascending" => {
                    self.0.extend(a.iter().cloned());
                    if *numeric {
                        self.0.sort_by(|v0, v1| cn(v0).cmp(&cn(v1)))
                    } else {
                        self.0.sort();
                    }
                    Some(Value::Array(ValArray::from_iter_exact(self.0.drain(..))))
                }
                "Descending" => {
                    self.0.extend(a.iter().cloned());
                    if *numeric {
                        self.0.sort_by(|a0, a1| cn(a1).cmp(&cn(a0)))
                    } else {
                        self.0.sort_by(|a0, a1| a1.cmp(a0));
                    }
                    Some(Value::Array(ValArray::from_iter_exact(self.0.drain(..))))
                }
                _ => None,
            },
            _ => None,
        }
    }
}

type Sort = CachedArgs<SortEv>;

#[derive(Debug, Default)]
struct DedupEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for DedupEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "array_dedup";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::Array(a)) => {
                let mut seen: LPooled<AHashSet<Value>> = LPooled::take();
                for v in a.iter() {
                    if !seen.contains(v) {
                        seen.insert(v.clone());
                        self.0.push(v.clone());
                    }
                }
                Some(Value::Array(ValArray::from_iter_exact(self.0.drain(..))))
            }
            Some(_) | None => None,
        }
    }
}

type Dedup = CachedArgs<DedupEv>;

#[derive(Debug, Default)]
struct EnumerateEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for EnumerateEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "array_enumerate";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        if let Some(Value::Array(a)) = &from.0[0] {
            let a = ValArray::from_iter_exact(
                a.iter().enumerate().map(|(i, v)| (i, v.clone()).into()),
            );
            return Some(Value::Array(a));
        }
        None
    }
}

type Enumerate = CachedArgs<EnumerateEv>;

#[derive(Debug, Default)]
struct ZipEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ZipEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "array_zip";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[..] {
            [Some(Value::Array(a0)), Some(Value::Array(a1))] => {
                Some(Value::Array(ValArray::from_iter_exact(
                    a0.iter().cloned().zip(a1.iter().cloned()).map(|p| p.into()),
                )))
            }
            _ => None,
        }
    }
}

type Zip = CachedArgs<ZipEv>;

#[derive(Debug, Default)]
struct UnzipEv {
    t0: Vec<Value>,
    t1: Vec<Value>,
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for UnzipEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "array_unzip";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[..] {
            [Some(Value::Array(a))] => {
                for v in a {
                    if let Value::Array(a) = v {
                        match &a[..] {
                            [v0, v1] => {
                                self.t0.push(v0.clone());
                                self.t1.push(v1.clone());
                            }
                            _ => (),
                        }
                    }
                }
                let v0 = Value::Array(ValArray::from_iter_exact(self.t0.drain(..)));
                let v1 = Value::Array(ValArray::from_iter_exact(self.t1.drain(..)));
                Some(Value::Array(ValArray::from_iter_exact([v0, v1].into_iter())))
            }
            _ => None,
        }
    }
}

type Unzip = CachedArgs<UnzipEv>;

#[derive(Debug)]
struct Group<R: Rt, E: UserEvent> {
    queue: VecDeque<Value>,
    buf: SmallVec<[Value; 16]>,
    pred: Node<R, E>,
    ready: bool,
    pid: BindId,
    nid: BindId,
    xid: BindId,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Group<R, E> {
    // Intrinsic sync; predicate effect joins at the call site (M6).
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "array_group";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b graphix_compiler::Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _] => {
                let typ = resolved.unwrap_or(typ);
                let scope =
                    scope.append(&format_compact!("fn{}", LambdaId::new().inner()));
                let n_typ = Type::Primitive(Typ::I64.into());
                let etyp = typ.args[0].typ.clone();
                let mftyp = match &typ.args[1].typ {
                    Type::Fn(ft) => ft.clone(),
                    t => bail!("expected function not {t}"),
                };
                let (nid, n) =
                    genn::bind(ctx, &scope.lexical, "n", n_typ.clone(), top_id);
                let (xid, x) = genn::bind(ctx, &scope.lexical, "x", etyp.clone(), top_id);
                let pid = BindId::new();
                let fnode = genn::reference(ctx, pid, Type::Fn(mftyp.clone()), top_id);
                let pred = genn::apply(fnode, scope, vec![n, x], &mftyp, top_id);
                Ok(Box::new(Self {
                    queue: VecDeque::new(),
                    buf: smallvec![],
                    pred,
                    ready: true,
                    pid,
                    nid,
                    xid,
                }))
            }
            _ => bail!("expected two arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Group<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        macro_rules! set {
            ($v:expr) => {{
                self.ready = false;
                self.buf.push($v.clone());
                let len = Value::I64(self.buf.len() as i64);
                ctx.cached.insert(self.nid, len.clone());
                event.variables.insert(self.nid, len);
                ctx.cached.insert(self.xid, $v.clone());
                event.variables.insert(self.xid, $v);
            }};
        }
        if let Some(v) = from[0].update(ctx, event) {
            self.queue.push_back(v);
        }
        if let Some(v) = from[1].update(ctx, event) {
            ctx.cached.insert(self.pid, v.clone());
            event.variables.insert(self.pid, v);
        }
        if self.ready && self.queue.len() > 0 {
            let v = self.queue.pop_front().unwrap();
            set!(v);
        }
        loop {
            // Cooperative interrupt: abort a wedged grouping loop.
            if ctx.interrupted() {
                break None;
            }
            match self.pred.update(ctx, event) {
                None => break None,
                Some(v) => {
                    self.ready = true;
                    match v {
                        Value::Bool(true) => {
                            break Some(Value::Array(ValArray::from_iter_exact(
                                self.buf.drain(..),
                            )));
                        }
                        _ => match self.queue.pop_front() {
                            None => break None,
                            Some(v) => set!(v),
                        },
                    }
                }
            }
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> anyhow::Result<()> {
        self.pred.typecheck0(ctx)?;
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        self.pred.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.cached.remove(&self.nid);
        ctx.cached.remove(&self.pid);
        ctx.cached.remove(&self.xid);
        self.pred.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.pred.sleep(ctx);
    }
}

#[derive(Debug)]
struct Iter(BindId, ExprId);

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Iter {
    const NAME: &str = "array_iter";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b graphix_compiler::Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        Ok(Box::new(Iter(id, top_id)))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Iter {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if let Some(Value::Array(a)) = from[0].update(ctx, event) {
            for v in a.iter() {
                // Cooperative interrupt: abort a wedged iter over a huge
                // array (partial emit is accepted for a deliberate kill).
                if ctx.interrupted() {
                    return None;
                }
                ctx.rt.set_var(self.0, v.clone());
            }
        }
        event.variables.get(&self.0).map(|v| v.clone())
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.0, self.1)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.0, self.1);
        self.0 = BindId::new();
        ctx.rt.ref_var(self.0, self.1);
    }
}

#[derive(Debug)]
struct IterQ {
    triggered: usize,
    queue: VecDeque<(usize, ValArray)>,
    id: BindId,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for IterQ {
    const NAME: &str = "array_iterq";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b graphix_compiler::Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        Ok(Box::new(IterQ { triggered: 0, queue: VecDeque::new(), id, top_id }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for IterQ {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if from[0].update(ctx, event).is_some() {
            self.triggered += 1;
        }
        if let Some(Value::Array(a)) = from[1].update(ctx, event) {
            if a.len() > 0 {
                self.queue.push_back((0, a));
            }
        }
        while self.triggered > 0 && self.queue.len() > 0 {
            let (i, a) = self.queue.front_mut().unwrap();
            while self.triggered > 0 && *i < a.len() {
                if ctx.interrupted() {
                    return None;
                }
                ctx.rt.set_var(self.id, a[*i].clone());
                *i += 1;
                self.triggered -= 1;
            }
            if *i == a.len() {
                self.queue.pop_front();
            }
        }
        event.variables.get(&self.id).cloned()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        self.queue.clear();
        self.triggered = 0;
    }
}

#[derive(Debug)]
struct Init<R: Rt, E: UserEvent> {
    scope: Scope,
    fid: BindId,
    top_id: ExprId,
    mftyp: TArc<FnType>,
    slots: Vec<Slot<R, E>>,
    /// Analysis-only Slot pre-materialized by the bound-instance firing
    /// of `Apply::typecheck1` (with `fn_args`) when the callback is
    /// statically resolvable. See `MapQ::analysis_pred` for the
    /// full design rationale. `None` for dynamic callbacks
    /// (fusion falls back to DynCall in that case).
    analysis_pred: Option<Slot<R, E>>,
    /// The per-slot callback template: a fused CLONE of `analysis_pred`,
    /// built at COMPILE time in `Apply::fuse` and cloned per index slot
    /// in `update`. `None` when fusion is disabled or no static callback.
    /// See `MapQ::fused_template`.
    fused_template: Option<Node<R, E>>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Init<R, E> {
    // Intrinsic sync; predicate effect joins at the call site (M6).
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "array_init";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'c FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _] => {
                let typ = resolved.unwrap_or(typ);
                Ok(Box::new(Self {
                    scope: scope
                        .append(&format_compact!("fn{}", LambdaId::new().inner())),
                    fid: BindId::new(),
                    top_id,
                    mftyp: match &typ.args[1].typ {
                        Type::Fn(ft) => ft.clone(),
                        t => bail!("expected a function not {t}"),
                    },
                    slots: vec![],
                    analysis_pred: None,
                    fused_template: None,
                }))
            }
            _ => bail!("expected two arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Init<R, E> {
    fn for_each_hof_callback_body<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        if let Some(slot) = self.analysis_pred.as_ref() {
            if let Some(body) =
                graphix_compiler::fusion::lowering::hof_callback_body(&slot.pred)
            {
                f(body);
            }
        }
    }

    fn for_each_hof_fused_body<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        // The FUSED index-callback template body for post-fusion attribute
        // checks (see `MapQ::for_each_hof_fused_body`).
        if let Some(t) = &self.fused_template {
            if let Some(body) = graphix_compiler::fusion::lowering::hof_callback_body(t) {
                f(body);
            }
        }
    }

    fn typecheck1(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _resolved: &FnType,
        fn_args: &[graphix_compiler::StaticFnArg<'_, R, E>],
    ) -> Result<()> {
        // HOF callback pre-materialization (bound-instance firing). Init
        // takes the callback at positional arg index 1 (index 0 is the
        // size). Empty `fn_args` (scratch firing) → `find` None → no-op.
        let Some(cb) = fn_args.iter().find(|a| a.arg_idx == 1) else {
            return Ok(());
        };
        let i_typ = Type::Primitive(Typ::I64.into());
        let (id, idx_node) =
            genn::bind(ctx, &self.scope.lexical, "i", i_typ, self.top_id);
        let fnode =
            genn::reference(ctx, self.fid, Type::Fn(self.mftyp.clone()), self.top_id);
        let mut pred = genn::apply(
            fnode,
            self.scope.clone(),
            vec![idx_node],
            &self.mftyp,
            self.top_id,
        );
        let fv = match ctx.lambda_defs.get(&cb.lambda.id).cloned() {
            Some(v) => v,
            None => return Ok(()),
        };
        let any: &mut dyn std::any::Any = &mut *pred;
        let Some(cs) =
            any.downcast_mut::<graphix_compiler::node::callsite::CallSite<R, E>>()
        else {
            return Ok(());
        };
        cs.resolve_static(ctx, cb.lambda, fv)?;
        self.analysis_pred = Some(Slot { id, pred, cur: None });
        Ok(())
    }

    /// Build the per-slot index-callback template at COMPILE time (the
    /// fusion phase), reached via `CallSite::fuse` when the `array::init`
    /// call site did not inline. The "element" is the `i64` index. See
    /// `MapQ::fuse` for the full rationale; errors are swallowed
    /// (de-fuse, never fail the compile).
    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        if self.fused_template.is_none() && self.analysis_pred.is_some() {
            let scope = self.scope.clone();
            let ap = self.analysis_pred.as_ref().unwrap();
            let feeders = [(ap.id, Type::Primitive(Typ::I64.into()))];
            let t = graphix_compiler::fusion::lowering::build_fused_template(
                ctx,
                &ap.pred,
                &scope,
                &feeders,
                self.top_id,
            );
            self.fused_template = Some(t);
        }
        Ok(())
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let slen = self.slots.len();
        if let Some(v) = from[1].update(ctx, event) {
            ctx.cached.insert(self.fid, v.clone());
            event.variables.insert(self.fid, v);
        }
        let (size_fired, resized) = match from[0].update(ctx, event) {
            Some(Value::I64(n)) => {
                let n = n.max(0) as usize;
                if n == slen {
                    (true, false)
                } else if n < slen {
                    while self.slots.len() > n {
                        if let Some(mut s) = self.slots.pop() {
                            s.delete(ctx)
                        }
                    }
                    (true, true)
                } else {
                    let i_typ = Type::Primitive(Typ::I64.into());
                    while self.slots.len() < n {
                        let i = self.slots.len();
                        // Mint this slot's fresh index binding "i" (no
                        // ref_var). The cloned template's index ref
                        // resolves "i" to THIS slot's id via the env name
                        // map; the genn::apply fallback builds its own ref.
                        let id = ctx
                            .env
                            .bind_variable(
                                &self.scope.lexical,
                                "i",
                                i_typ.clone(),
                                Default::default(),
                                TArc::new(graphix_compiler::expr::Origin::default()),
                            )
                            .id;
                        ctx.cached.insert(id, Value::I64(i as i64));
                        let pred = match &self.fused_template {
                            Some(t) => {
                                let scope = self.scope.clone();
                                t.clone_rebind(ctx, &scope)
                            }
                            None => {
                                let node =
                                    genn::reference(ctx, id, i_typ.clone(), self.top_id);
                                let fnode = genn::reference(
                                    ctx,
                                    self.fid,
                                    Type::Fn(self.mftyp.clone()),
                                    self.top_id,
                                );
                                genn::apply(
                                    fnode,
                                    self.scope.clone(),
                                    vec![node],
                                    &self.mftyp,
                                    self.top_id,
                                )
                            }
                        };
                        self.slots.push(Slot { id, pred, cur: None });
                    }
                    (true, true)
                }
            }
            _ => (false, false),
        };
        // set index bindings for new slots
        if resized && self.slots.len() > slen {
            for i in slen..self.slots.len() {
                let id = self.slots[i].id;
                event.variables.insert(id, Value::I64(i as i64));
            }
        }
        if size_fired && self.slots.is_empty() {
            return Some(Value::Array(ValArray::default()));
        }
        let init = event.init;
        let mut up = resized;
        for (i, s) in self.slots.iter_mut().enumerate() {
            // Cooperative interrupt: abort a wedged init; restore init.
            if ctx.interrupted() {
                event.init = init;
                return None;
            }
            if i == slen {
                event.init = true;
                if let Entry::Vacant(e) = event.variables.entry(self.fid)
                    && let Some(v) = ctx.cached.get(&self.fid)
                {
                    e.insert(v.clone());
                }
            }
            if let Some(v) = s.pred.update(ctx, event) {
                s.cur = Some(v);
                up = true;
            }
        }
        event.init = init;
        if up && self.slots.iter().all(|s| s.cur.is_some()) {
            Some(Value::Array(ValArray::from_iter_exact(
                self.slots.iter().map(|s| s.cur.clone().unwrap()),
            )))
        } else {
            None
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> anyhow::Result<()> {
        let i_typ = Type::Primitive(Typ::I64.into());
        let (_, node) = genn::bind(ctx, &self.scope.lexical, "i", i_typ, self.top_id);
        let ft = self.mftyp.clone();
        let fnode = genn::reference(ctx, self.fid, Type::Fn(ft.clone()), self.top_id);
        let mut node =
            genn::apply(fnode, self.scope.clone(), vec![node], &ft, self.top_id);
        let r = node.typecheck0(ctx);
        node.delete(ctx);
        r?;
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        for s in &self.slots {
            s.pred.refs(refs)
        }
        if let Some(s) = &self.analysis_pred {
            // Mask the analysis-only synthetic bindings (the index `i`
            // and the callback-function handle `fid`) so fusion's
            // region-input discovery surfaces only the callback's real
            // captures. See `MapQ::refs` in graphix-package-core.
            refs.mark_bound(s.id);
            refs.mark_bound(self.fid);
            s.pred.refs(refs);
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.cached.remove(&self.fid);
        for sl in &mut self.slots {
            sl.delete(ctx)
        }
        if let Some(mut s) = self.analysis_pred.take() {
            s.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        for sl in &mut self.slots {
            sl.cur = None;
            sl.pred.sleep(ctx);
        }
        // analysis_pred is analysis-only — no runtime sleep needed.
    }

    /// `array::init` codegen:
    /// `n` (positional 0) must freeze to an integer scalar, the index
    /// param binds the loop counter Variable itself (no per-iteration
    /// copy), and the body's per-index result is pushed via
    /// `scaffold::push_field` (the runtime bottom-abort seam, same as
    /// map). A may-bottom `n` routes through `emit_forced` and
    /// RUNTIME-aborts the whole init to bottom if it taints.
    fn emit_clif(
        &self,
        callsite: &graphix_compiler::node::callsite::CallSite<R, E>,
        cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        let Some(slot) = self.analysis_pred.as_ref() else {
            return Ok(None);
        };
        // Positional arg 0 is the array size (`n`).
        let Some(n_node) = callsite.arg_positional(0) else {
            return Ok(None);
        };
        let n_prim =
            match kernel_abi::freeze_for_abi_normalized(cx.registry(), n_node.typ())
                .as_ref()
                .and_then(|t| kernel_abi::scalar_prim(cx.registry(), t))
            {
                Some(p) if p.is_integer() => p,
                _ => return Ok(None),
            };
        // Body Node via the inner CallSite's resolved Apply.
        let inner_cs = match slot.pred.view() {
            graphix_compiler::NodeView::CallSite(cs) => cs,
            _ => return Ok(None),
        };
        let Some(inner_apply) = inner_cs.resolved_apply() else {
            return Ok(None);
        };
        let g = match inner_apply {
            graphix_compiler::ApplyView::Lambda(g) => g,
            _ => return Ok(None),
        };
        let body = g.body();
        // Index param's name from the lambda's FnType, its BindId
        // from the arg pattern (Refs resolve BindId-first).
        let idx_name = match g.typ().args.first().map(|a| &a.kind) {
            Some(graphix_compiler::typ::FnArgKind::Positional { name: Some(n) }) => {
                n.clone()
            }
            Some(graphix_compiler::typ::FnArgKind::Labeled { name, .. }) => name.clone(),
            _ => return Ok(None),
        };
        let idx_id = g.args().first().and_then(|p| p.single_bind_id());
        // Output element type is the body's result.
        let Some(out_typ) =
            kernel_abi::freeze_for_abi_normalized(cx.registry(), body.typ())
        else {
            return Ok(None);
        };
        if is_unit_or_null(cx.registry(), &out_typ) {
            return Ok(None);
        }
        // Gates done — emit. From here a mismatch is a build bug or a
        // de-fuse, so Err (abort the kernel), never Ok(None).
        let n = emit::emit_forced_keep(cx, n_node)?;
        let out_src = emit::node_composite_source(body);
        scaffold::emit_init_loop(
            cx,
            n.payload,
            n_prim,
            &idx_name,
            idx_id,
            &out_typ,
            out_src,
            |cx| body.emit_clif(cx),
        )
        .map(|v| {
            let r = emit::array_result(cx, v);
            Some(emit::inherit_hof_firing(cx, r, n.disc, &[body], idx_id, &[], None))
        })
    }
}

graphix_derive::defpackage! {
    builtins => [
        Concat,
        Dedup,
        Filter as Filter<GXRt<X>, X::UserEvent>,
        FilterMap as FilterMap<GXRt<X>, X::UserEvent>,
        Find as Find<GXRt<X>, X::UserEvent>,
        FindMap as FindMap<GXRt<X>, X::UserEvent>,
        FlatMap as FlatMap<GXRt<X>, X::UserEvent>,
        Enumerate,
        Zip,
        Unzip,
        Flatten,
        Fold as Fold<GXRt<X>, X::UserEvent>,
        Group as Group<GXRt<X>, X::UserEvent>,
        Init as Init<GXRt<X>, X::UserEvent>,
        Iter,
        IterQ,
        Len,
        Map as Map<GXRt<X>, X::UserEvent>,
        PushBack,
        PushFront,
        Sort,
        Window,
    ],
}
