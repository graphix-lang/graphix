#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use ahash::AHashSet;
use anyhow::{bail, Result};
use compact_str::format_compact;
use arcstr::ArcStr;
use graphix_compiler::{
    effects::EffectKind,
    expr::ExprId,
    fusion::lowering::{emit_expr_node, FusionCtx, Input},
    gir::{self, GirExpr, GirOp, PrimType},
    gir_jit::{self, scaffold, BodyCx, CompiledExpr, CompositeSource},
    node::genn,
    typ::{FnType, Type},
    Apply, BindId, BuiltIn, Event, ExecCtx, LambdaId, Node, Refs, Rt, Scope,
    TypecheckPhase, UserEvent,
};
use graphix_package_core::{
    CachedArgs, CachedVals, EvalCached, FoldFn, FoldQ, MapFn, MapQ, Slot,
};
use graphix_rt::GXRt;
use netidx::{publisher::Typ, subscriber::Value, utils::Either};
use netidx_value::ValArray;
use poolshark::local::LPooled;
use smallvec::{smallvec, SmallVec};
use std::{collections::hash_map::Entry, collections::VecDeque, fmt::Debug, iter};
use triomphe::Arc as TArc;

/// True if a (frozen) `Type`'s top-level shape is `Unit` (side-effect-
/// only) or bare `Null` — neither is a valid array element shape.
fn is_unit_or_null(t: &Type) -> bool {
    use gir::AbiKind;
    matches!(gir::abi_kind(t), Some(AbiKind::Unit | AbiKind::Null))
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

    fn emit_gir(
        ctx: &mut FusionCtx,
        ec: &mut ExecCtx<R, E>,
        array_arg: &Node<R, E>,
        body: &Node<R, E>,
        elem_name: &ArcStr,
        in_elem: Type,
        elem_binds: &[(BindId, usize)],
    ) -> Option<GirExpr> {
        // Array param name (the array kernel input the loop reads).
        let array_name = ctx.resolve_array_input(array_arg)?;
        // The body emits inside a scope that has the per-element binding
        // visible: a single scalar `Input` for `|x|`, or (for a
        // `|(k, v)|` destructure) a composite element local plus the
        // per-leaf `TupleGet` lets, wrapped into the body's `Block`.
        let (in_elem, body_kir) = if elem_binds.is_empty() {
            let prim = gir::scalar_prim(&in_elem)?;
            let body_kir = ctx.with_input(
                Input { name: elem_name.clone(), prim, bind_id: None },
                |inner| emit_expr_node(body, inner, ec),
            )?;
            (gir::prim_type(prim), body_kir)
        } else {
            let body_block = graphix_compiler::fusion::lowering::emit_hof_body_destructured(
                ctx, ec, body, elem_name, &in_elem, elem_binds,
            )?;
            (in_elem, body_block)
        };
        // Output element type is the body's type — prim or composite
        // (e.g. a nested `array::map` producing `Array<_>`).
        let out = body_kir.typ.clone();
        if is_unit_or_null(&out) {
            return None;
        }
        Some(GirExpr {
            op: GirOp::ArrayMap {
                array: array_name,
                in_elem,
                elem_local: elem_name.clone(),
                body: Box::new(body_kir),
            },
            typ: gir::array_type(out),
        })
    }

    /// Direct-path map loop via `scaffold::emit_map_loop`. `Ok(None)`
    /// gates, all decided before the first emitted instruction: a
    /// destructured `|(k, v)|` callback (Stage D3), an element shape
    /// `scaffold::bind_elem` can't bind, an OWNED input array (a fresh
    /// producer — literal / slice / call result — needs the
    /// pending-cleanup registration that lands with the owned-arg
    /// stage; until then those sites DynCall or node-walk, the status
    /// quo), and a body result that won't freeze concrete or is
    /// Unit/bare-Null (mirroring `emit_gir`'s `is_unit_or_null` gate).
    fn emit_clif(
        cx: &mut BodyCx,
        array_arg: &Node<R, E>,
        body: &Node<R, E>,
        elem_name: &ArcStr,
        elem_id: Option<BindId>,
        in_elem: &Type,
        elem_binds: &[(BindId, usize)],
    ) -> Result<Option<CompiledExpr>> {
        use gir::AbiKind;
        // Destructured `|(k, v)|` callbacks are Stage D3.
        if !elem_binds.is_empty() {
            return Ok(None);
        }
        // Element shape: only what `scaffold::bind_elem` accepts.
        let Some(in_elem) = gir::freeze_normalized(in_elem) else {
            return Ok(None);
        };
        match gir::abi_kind(&in_elem) {
            Some(
                AbiKind::Scalar(_)
                | AbiKind::Array
                | AbiKind::Tuple
                | AbiKind::Struct,
            ) => {}
            _ => return Ok(None),
        }
        // V1: borrowed (env-owned) input arrays only — see the gate
        // list above and the `ArraySrc` ownership contract.
        if gir_jit::node_composite_source(array_arg)
            != CompositeSource::Borrowed
        {
            return Ok(None);
        }
        // Output element type is the body's result — `freeze_normalized`
        // because typecheck can leave a select-valued body's type as the
        // un-flattened arm union.
        let Some(out_typ) = gir::freeze_normalized(body.typ()) else {
            return Ok(None);
        };
        if is_unit_or_null(&out_typ) {
            return Ok(None);
        }
        // Gates done — emit. From here a mismatch is a build bug, so
        // Err (abort the kernel), never Ok(None).
        let arr_ptr = match array_arg.emit_clif(cx)? {
            CompiledExpr::Single(v) => v,
            cv => bail!(
                "array::map emit_clif: borrowed array arg compiled to \
                 non-Single {cv:?}"
            ),
        };
        let out_src = gir_jit::node_composite_source(body);
        scaffold::emit_map_loop(
            cx,
            scaffold::ArraySrc { ptr: arr_ptr, owned: false },
            &scaffold::HofElem { name: elem_name, id: elem_id, typ: &in_elem },
            &out_typ,
            out_src,
            |cx| body.emit_clif(cx),
        )
        .map(|v| Some(CompiledExpr::Single(v)))
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

    fn emit_gir(
        ctx: &mut FusionCtx,
        ec: &mut ExecCtx<R, E>,
        array_arg: &Node<R, E>,
        body: &Node<R, E>,
        elem_name: &ArcStr,
        in_elem: Type,
        elem_binds: &[(BindId, usize)],
    ) -> Option<GirExpr> {
        let array_name = ctx.resolve_array_input(array_arg)?;
        let (elem, pred_kir) = if elem_binds.is_empty() {
            let prim = gir::scalar_prim(&in_elem)?;
            let pred_kir = ctx.with_input(
                Input { name: elem_name.clone(), prim, bind_id: None },
                |inner| emit_expr_node(body, inner, ec),
            )?;
            (gir::prim_type(prim), pred_kir)
        } else {
            let pred_block =
                graphix_compiler::fusion::lowering::emit_hof_body_destructured(
                    ctx, ec, body, elem_name, &in_elem, elem_binds,
                )?;
            (in_elem, pred_block)
        };
        // Predicate must return bool.
        if pred_kir.typ != gir::prim_type(PrimType::Bool) {
            return None;
        }
        Some(GirExpr {
            op: GirOp::ArrayFilter {
                array: array_name,
                elem: elem.clone(),
                elem_local: elem_name.clone(),
                predicate: Box::new(pred_kir),
            },
            typ: gir::array_type(elem),
        })
    }

    /// Direct-path filter loop via `scaffold::emit_filter_loop`. Same
    /// `Ok(None)` gates as `MapImpl::emit_clif` (destructured callback,
    /// unbindable element shape, owned input array), plus the predicate
    /// type must freeze to `bool` (mirroring `emit_gir`). Unlike a map
    /// body, a may-bottom (`Scalar2`) predicate Errs — the kernel
    /// de-fuses at build time and the call site node-walks. There is no
    /// runtime seam that could decide keep-vs-drop for a bottom
    /// predicate, so only the node-walk represents it faithfully.
    fn emit_clif(
        cx: &mut BodyCx,
        array_arg: &Node<R, E>,
        body: &Node<R, E>,
        elem_name: &ArcStr,
        elem_id: Option<BindId>,
        in_elem: &Type,
        elem_binds: &[(BindId, usize)],
    ) -> Result<Option<CompiledExpr>> {
        use gir::AbiKind;
        // Destructured `|(k, v)|` callbacks are Stage D3.
        if !elem_binds.is_empty() {
            return Ok(None);
        }
        // Element shape: only what `scaffold::bind_elem` accepts.
        let Some(in_elem) = gir::freeze_normalized(in_elem) else {
            return Ok(None);
        };
        match gir::abi_kind(&in_elem) {
            Some(
                AbiKind::Scalar(_)
                | AbiKind::Array
                | AbiKind::Tuple
                | AbiKind::Struct,
            ) => {}
            _ => return Ok(None),
        }
        // V1: borrowed (env-owned) input arrays only — see
        // `MapImpl::emit_clif` and the `ArraySrc` ownership contract.
        if gir_jit::node_composite_source(array_arg)
            != CompositeSource::Borrowed
        {
            return Ok(None);
        }
        match gir::freeze_normalized(body.typ()) {
            Some(t) if matches!(gir::scalar_prim(&t), Some(PrimType::Bool)) => {
            }
            _ => return Ok(None),
        }
        // Gates done — emit. From here a mismatch is a build bug or a
        // de-fuse, so Err (abort the kernel), never Ok(None).
        let arr_ptr = match array_arg.emit_clif(cx)? {
            CompiledExpr::Single(v) => v,
            cv => bail!(
                "array::filter emit_clif: borrowed array arg compiled to \
                 non-Single {cv:?}"
            ),
        };
        scaffold::emit_filter_loop(
            cx,
            scaffold::ArraySrc { ptr: arr_ptr, owned: false },
            &scaffold::HofElem { name: elem_name, id: elem_id, typ: &in_elem },
            |cx| match body.emit_clif(cx)? {
                CompiledExpr::Single(v) => Ok(v),
                cv => bail!(
                    "array::filter predicate compiled to a possibly-bottom \
                     or non-scalar value ({cv:?}) — de-fuse to node-walk"
                ),
            },
        )
        .map(|v| Some(CompiledExpr::Single(v)))
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

    fn emit_gir(
        ctx: &mut FusionCtx,
        ec: &mut ExecCtx<R, E>,
        array_arg: &Node<R, E>,
        body: &Node<R, E>,
        elem_name: &ArcStr,
        in_elem: Type,
        elem_binds: &[(BindId, usize)],
    ) -> Option<GirExpr> {
        let array_name = ctx.resolve_array_input(array_arg)?;
        let body_kir = if elem_binds.is_empty() {
            let prim = gir::scalar_prim(&in_elem)?;
            ctx.with_input(
                Input { name: elem_name.clone(), prim, bind_id: None },
                |inner| emit_expr_node(body, inner, ec),
            )?
        } else {
            graphix_compiler::fusion::lowering::emit_hof_body_destructured(
                ctx, ec, body, elem_name, &in_elem, elem_binds,
            )?
        };
        // Fuse only the array-returning shape of the `['b, Array<'b>]`
        // callback (a bare-element body bails to the runtime dispatch).
        let out_elem = gir::array_scalar_prim(&body_kir.typ)?;
        Some(GirExpr {
            op: GirOp::ArrayFlatMap {
                array: array_name,
                in_elem,
                elem_local: elem_name.clone(),
                out_elem,
                body: Box::new(body_kir),
            },
            typ: gir::array_type(gir::prim_type(out_elem)),
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

    fn emit_gir(
        ctx: &mut FusionCtx,
        ec: &mut ExecCtx<R, E>,
        array_arg: &Node<R, E>,
        body: &Node<R, E>,
        elem_name: &ArcStr,
        in_elem: Type,
        elem_binds: &[(BindId, usize)],
    ) -> Option<GirExpr> {
        if !elem_binds.is_empty() {
            return None; // tuple-destructure callbacks not supported here yet
        }
        let in_elem = gir::scalar_prim(&in_elem)?;
        let array_name = ctx.resolve_array_input(array_arg)?;
        let body_kir = ctx.with_input(
            Input { name: elem_name.clone(), prim: in_elem, bind_id: None },
            |inner| emit_expr_node(body, inner, ec),
        )?;
        // Body must return an option `[out_elem, null]`.
        let out_elem = gir::scalar_prim(&gir::nullable_inner(&body_kir.typ)?)?;
        Some(GirExpr {
            op: GirOp::ArrayFilterMap {
                array: array_name,
                in_elem,
                elem_local: elem_name.clone(),
                out_elem,
                body: Box::new(body_kir),
            },
            typ: gir::array_type(gir::prim_type(out_elem)),
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

    fn emit_gir(
        ctx: &mut FusionCtx,
        ec: &mut ExecCtx<R, E>,
        array_arg: &Node<R, E>,
        body: &Node<R, E>,
        elem_name: &ArcStr,
        in_elem: Type,
        elem_binds: &[(BindId, usize)],
    ) -> Option<GirExpr> {
        let array_name = ctx.resolve_array_input(array_arg)?;
        let pred_kir = if elem_binds.is_empty() {
            let prim = gir::scalar_prim(&in_elem)?;
            ctx.with_input(
                Input { name: elem_name.clone(), prim, bind_id: None },
                |inner| emit_expr_node(body, inner, ec),
            )?
        } else {
            graphix_compiler::fusion::lowering::emit_hof_body_destructured(
                ctx, ec, body, elem_name, &in_elem, elem_binds,
            )?
        };
        // Predicate must return bool.
        if pred_kir.typ != gir::prim_type(PrimType::Bool) {
            return None;
        }
        // The matched element (scalar or composite) is the result, as an
        // option (`null` if none match).
        Some(GirExpr {
            op: GirOp::ArrayFind {
                array: array_name,
                elem: in_elem.clone(),
                elem_local: elem_name.clone(),
                predicate: Box::new(pred_kir),
            },
            typ: gir::nullable_type(in_elem),
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

    fn emit_gir(
        ctx: &mut FusionCtx,
        ec: &mut ExecCtx<R, E>,
        array_arg: &Node<R, E>,
        body: &Node<R, E>,
        elem_name: &ArcStr,
        in_elem: Type,
        elem_binds: &[(BindId, usize)],
    ) -> Option<GirExpr> {
        let array_name = ctx.resolve_array_input(array_arg)?;
        let (elem_typ, body_kir) = if elem_binds.is_empty() {
            let prim = gir::scalar_prim(&in_elem)?;
            let body_kir = ctx.with_input(
                Input { name: elem_name.clone(), prim, bind_id: None },
                |inner| emit_expr_node(body, inner, ec),
            )?;
            (gir::prim_type(prim), body_kir)
        } else {
            let body_block =
                graphix_compiler::fusion::lowering::emit_hof_body_destructured(
                    ctx, ec, body, elem_name, &in_elem, elem_binds,
                )?;
            (in_elem, body_block)
        };
        // Body must return an option `[out, null]`; the op returns the
        // first non-null body value as that same `Nullable<out>`.
        if !matches!(
            gir::abi_kind(&body_kir.typ),
            Some(gir::AbiKind::Nullable)
        ) {
            return None;
        }
        let out_ty = body_kir.typ.clone();
        Some(GirExpr {
            op: GirOp::ArrayFindMap {
                array: array_name,
                in_elem: elem_typ,
                elem_local: elem_name.clone(),
                body: Box::new(body_kir),
            },
            typ: out_ty,
        })
    }
}

type FindMap<R, E> = MapQ<R, E, FindMapImpl>;

#[derive(Debug)]
struct FoldImpl;

impl<R: Rt, E: UserEvent> FoldFn<R, E> for FoldImpl {
    type Collection = ValArray;

    const NAME: &str = "array_fold";

    fn emit_gir(
        ctx: &mut FusionCtx,
        ec: &mut ExecCtx<R, E>,
        array_arg: &Node<R, E>,
        init_arg: &Node<R, E>,
        body: &Node<R, E>,
        acc_name: &ArcStr,
        elem_name: &ArcStr,
        in_elem: Type,
        elem_binds: &[(BindId, usize)],
    ) -> Option<GirExpr> {
        let array_name = ctx.resolve_array_input(array_arg)?;
        // Lower init in the outer ctx — no new locals needed.
        let init_kir = emit_expr_node(init_arg, ctx, ec)?;
        let acc_typ = gir::scalar_prim(&init_kir.typ)?;
        // The body emits with the accumulator (always scalar) plus the
        // element binding in scope: a single scalar `Input` for `|acc,
        // x|`, or (for `|acc, (k, v)|`) a composite element local + the
        // per-leaf `TupleGet` lets wrapped into the body's `Block`.
        let (elem_typ, body_kir) = if elem_binds.is_empty() {
            let prim = gir::scalar_prim(&in_elem)?;
            let body_kir = ctx.with_input(
                Input { name: acc_name.clone(), prim: acc_typ, bind_id: None },
                |inner| {
                    inner.with_input(
                        Input { name: elem_name.clone(), prim, bind_id: None },
                        |inner2| emit_expr_node(body, inner2, ec),
                    )
                },
            )?;
            (gir::prim_type(prim), body_kir)
        } else {
            let body_block = ctx.with_input(
                Input { name: acc_name.clone(), prim: acc_typ, bind_id: None },
                |inner| {
                    graphix_compiler::fusion::lowering::emit_hof_body_destructured(
                        inner, ec, body, elem_name, &in_elem, elem_binds,
                    )
                },
            )?;
            (in_elem, body_block)
        };
        if body_kir.typ != gir::prim_type(acc_typ) {
            return None;
        }
        Some(GirExpr {
            op: GirOp::ArrayFold {
                array: array_name,
                elem_typ,
                init: Box::new(init_kir),
                acc_local: acc_name.clone(),
                elem_local: elem_name.clone(),
                body: Box::new(body_kir),
            },
            typ: gir::prim_type(acc_typ),
        })
    }

    /// Direct-path fold loop via `scaffold::emit_fold_loop`. Same
    /// `Ok(None)` gates as `MapImpl::emit_clif`, plus the accumulator
    /// must be a register scalar whose prim the init and body types
    /// agree on (mirroring `emit_gir`'s `scalar_prim(init)` +
    /// `body == acc` checks). The init and body closures both carry
    /// the BUILD-time de-fuse contract: a may-bottom (`Scalar2`)
    /// result Errs — a bottom accumulator poisons every later
    /// iteration, so there is no per-element runtime seam; only the
    /// node-walk represents it (the acc slot never fires and fold's
    /// output blocks).
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
        use gir::AbiKind;
        // Destructured `|acc, (k, v)|` callbacks are Stage D3.
        if !elem_binds.is_empty() {
            return Ok(None);
        }
        // Element shape: only what `scaffold::bind_elem` accepts.
        let Some(in_elem) = gir::freeze_normalized(in_elem) else {
            return Ok(None);
        };
        match gir::abi_kind(&in_elem) {
            Some(
                AbiKind::Scalar(_)
                | AbiKind::Array
                | AbiKind::Tuple
                | AbiKind::Struct,
            ) => {}
            _ => return Ok(None),
        }
        // V1: borrowed (env-owned) input arrays only — see
        // `MapImpl::emit_clif` and the `ArraySrc` ownership contract.
        if gir_jit::node_composite_source(array_arg)
            != CompositeSource::Borrowed
        {
            return Ok(None);
        }
        // The acc threads through the loop as a register Variable —
        // init and body must both freeze to the same register scalar.
        let acc_prim = match gir::freeze_normalized(init_arg.typ())
            .as_ref()
            .and_then(gir::scalar_prim)
        {
            Some(p) => p,
            None => return Ok(None),
        };
        match gir::freeze_normalized(body.typ())
            .as_ref()
            .and_then(gir::scalar_prim)
        {
            Some(p) if p == acc_prim => {}
            _ => return Ok(None),
        }
        // Gates done — emit. From here a mismatch is a build bug or a
        // de-fuse, so Err (abort the kernel), never Ok(None).
        let arr_ptr = match array_arg.emit_clif(cx)? {
            CompiledExpr::Single(v) => v,
            cv => bail!(
                "array::fold emit_clif: borrowed array arg compiled to \
                 non-Single {cv:?}"
            ),
        };
        let valid_scalar = |what: &str, cv: CompiledExpr| match cv {
            CompiledExpr::Single(v) => Ok(v),
            cv => Err(anyhow::anyhow!(
                "array::fold {what} compiled to a possibly-bottom or \
                 non-scalar value ({cv:?}) — de-fuse to node-walk"
            )),
        };
        scaffold::emit_fold_loop(
            cx,
            scaffold::ArraySrc { ptr: arr_ptr, owned: false },
            acc_prim,
            acc_name,
            acc_id,
            &scaffold::HofElem { name: elem_name, id: elem_id, typ: &in_elem },
            |cx| valid_scalar("init", init_arg.emit_clif(cx)?),
            |cx| valid_scalar("body", body.emit_clif(cx)?),
        )
        .map(|v| Some(CompiledExpr::Single(v)))
    }
}

type Fold<R, E> = FoldQ<R, E, FoldImpl>;

#[derive(Debug, Default)]
struct ConcatEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ConcatEv {
    const NAME: &str = "array_concat";
    const NEEDS_CALLSITE: bool = false;
    const EFFECT: EffectKind = EffectKind::Sync;

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
    const NAME: &str = "array_push_back";
    const NEEDS_CALLSITE: bool = false;
    const EFFECT: EffectKind = EffectKind::Sync;

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
    const NAME: &str = "array_push_front";
    const NEEDS_CALLSITE: bool = false;
    const EFFECT: EffectKind = EffectKind::Sync;

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
    const NAME: &str = "array_window";
    const NEEDS_CALLSITE: bool = false;
    const EFFECT: EffectKind = EffectKind::Sync;

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
    const NAME: &str = "array_len";
    const NEEDS_CALLSITE: bool = false;
    const EFFECT: EffectKind = EffectKind::Sync;
    const FUSABLE: bool = true;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::Array(a)) => Some(Value::I64(a.len() as i64)),
            Some(_) | None => None,
        }
    }

    // `array::len(arr)` lowers to the pure-GIR `GirOp::ArrayLen` (a
    // length read off the array kernel input) — no DynCall needed. Only
    // fuses when the array arg resolves to a kernel input; otherwise
    // `None` falls back to DynCall.
    fn emit_gir(
        &self,
        callsite: &graphix_compiler::node::callsite::CallSite<R, E>,
        _args: &[(Option<ArcStr>, &Node<R, E>)],
        _arg_refs: &[Node<R, E>],
        ctx: &mut graphix_compiler::fusion::lowering::FusionCtx,
        _ec: &mut ExecCtx<R, E>,
    ) -> Option<graphix_compiler::gir::GirExpr> {
        let arr_name = ctx.resolve_array_input(callsite.arg_positional(0)?)?;
        Some(GirExpr {
            op: GirOp::ArrayLen { name: arr_name },
            typ: gir::prim_type(PrimType::U64),
        })
    }
}

type Len = CachedArgs<LenEv>;

#[derive(Debug, Default)]
struct FlattenEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for FlattenEv {
    const NAME: &str = "array_flatten";
    const NEEDS_CALLSITE: bool = false;
    const EFFECT: EffectKind = EffectKind::Sync;

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
    const NAME: &str = "array_sort";
    const NEEDS_CALLSITE: bool = false;
    const EFFECT: EffectKind = EffectKind::Sync;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fn cn(v: &Value) -> Value {
            v.clone().cast(Typ::F64).unwrap_or_else(|| v.clone())
        }
        match &from.0[..] {
            [Some(Value::String(dir)), Some(Value::Bool(numeric)), Some(Value::Array(a))] => {
                match &**dir {
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
                }
            }
            _ => None,
        }
    }
}

type Sort = CachedArgs<SortEv>;

#[derive(Debug, Default)]
struct DedupEv(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for DedupEv {
    const NAME: &str = "array_dedup";
    const NEEDS_CALLSITE: bool = false;
    const EFFECT: EffectKind = EffectKind::Sync;

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
    const NAME: &str = "array_enumerate";
    const NEEDS_CALLSITE: bool = false;
    const EFFECT: EffectKind = EffectKind::Sync;

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
    const NAME: &str = "array_zip";
    const NEEDS_CALLSITE: bool = false;
    const EFFECT: EffectKind = EffectKind::Sync;

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
    const NAME: &str = "array_unzip";
    const NEEDS_CALLSITE: bool = false;
    const EFFECT: EffectKind = EffectKind::Sync;

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
    const NAME: &str = "array_group";
    const NEEDS_CALLSITE: bool = false;
    // Intrinsic sync; predicate effect joins at the call site (M6).
    const EFFECT: EffectKind = EffectKind::Sync;

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
            match self.pred.update(ctx, event) {
                None => break None,
                Some(v) => {
                    self.ready = true;
                    match v {
                        Value::Bool(true) => {
                            break Some(Value::Array(ValArray::from_iter_exact(
                                self.buf.drain(..),
                            )))
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

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _phase: TypecheckPhase<'_>,
    ) -> anyhow::Result<()> {
        self.pred.typecheck(ctx)?;
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
    const NEEDS_CALLSITE: bool = false;

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
    const NEEDS_CALLSITE: bool = false;

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
    /// Analysis-only Slot pre-materialized by
    /// `Apply::static_resolve_fn_args` when the callback is
    /// statically resolvable. See `MapQ::analysis_pred` for the
    /// full design rationale. `None` for dynamic callbacks
    /// (fusion falls back to DynCall in that case).
    analysis_pred: Option<Slot<R, E>>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Init<R, E> {
    const NAME: &str = "array_init";
    const NEEDS_CALLSITE: bool = false;
    // Intrinsic sync; predicate effect joins at the call site (M6).
    const EFFECT: EffectKind = EffectKind::Sync;

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
                }))
            }
            _ => bail!("expected two arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Init<R, E> {
    fn view(&self) -> graphix_compiler::ApplyView<'_, R, E> {
        graphix_compiler::ApplyView::FusedBuiltin(self)
    }

    fn view_mut(&mut self) -> graphix_compiler::ApplyViewMut<'_, R, E> {
        graphix_compiler::ApplyViewMut::FusedBuiltin(self)
    }

    fn static_resolve_fn_args(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        fn_args: &[graphix_compiler::StaticFnArg<'_, R, E>],
    ) -> Result<()> {
        // Init takes the callback at positional arg index 1
        // (index 0 is the size).
        let Some(cb) = fn_args.iter().find(|a| a.arg_idx == 1) else {
            return Ok(());
        };
        let i_typ = Type::Primitive(Typ::I64.into());
        let (id, idx_node) = genn::bind(
            ctx,
            &self.scope.lexical,
            "i",
            i_typ,
            self.top_id,
        );
        let fnode = genn::reference(
            ctx,
            self.fid,
            Type::Fn(self.mftyp.clone()),
            self.top_id,
        );
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
        let Some(cs) = any.downcast_mut::<graphix_compiler::node::callsite::CallSite<R, E>>() else {
            return Ok(());
        };
        cs.resolve_static(ctx, cb.lambda, fv)?;
        self.analysis_pred = Some(Slot { id, pred, cur: None });
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
                        let (id, node) = genn::bind(
                            ctx,
                            &self.scope.lexical,
                            "i",
                            i_typ.clone(),
                            self.top_id,
                        );
                        ctx.cached.insert(id, Value::I64(i as i64));
                        let fnode = genn::reference(
                            ctx,
                            self.fid,
                            Type::Fn(self.mftyp.clone()),
                            self.top_id,
                        );
                        let pred = genn::apply(
                            fnode,
                            self.scope.clone(),
                            vec![node],
                            &self.mftyp,
                            self.top_id,
                        );
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

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _phase: TypecheckPhase<'_>,
    ) -> anyhow::Result<()> {
        let i_typ = Type::Primitive(Typ::I64.into());
        let (_, node) = genn::bind(ctx, &self.scope.lexical, "i", i_typ, self.top_id);
        let ft = self.mftyp.clone();
        let fnode = genn::reference(ctx, self.fid, Type::Fn(ft.clone()), self.top_id);
        let mut node =
            genn::apply(fnode, self.scope.clone(), vec![node], &ft, self.top_id);
        let r = node.typecheck(ctx);
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
}

/// Fusion-time codegen for `array::init`. Lowers to `GirOp::ArrayInit`
/// when the callback was statically resolvable (`analysis_pred` is
/// populated); falls back to DynCall otherwise.
impl<R: Rt, E: UserEvent> graphix_compiler::GirEmitter<R, E> for Init<R, E> {
    fn emit_gir(
        &self,
        callsite: &graphix_compiler::node::callsite::CallSite<R, E>,
        _args: &[(Option<arcstr::ArcStr>, &Node<R, E>)],
        _arg_refs: &[Node<R, E>],
        ctx: &mut graphix_compiler::fusion::lowering::FusionCtx,
        ec: &mut ExecCtx<R, E>,
    ) -> Option<graphix_compiler::gir::GirExpr> {
        let slot = self.analysis_pred.as_ref()?;
        // Positional arg 0 is the array size (`n`).
        let n_node = callsite.arg_positional(0)?;
        let n_kir = emit_expr_node(n_node, ctx, ec)?;
        if !gir::scalar_prim(&n_kir.typ).is_some_and(|p| p.is_integer()) {
            return None;
        }
        // Body Node via the inner CallSite's resolved Apply.
        let pred_view = slot.pred.view();
        let inner_cs = match pred_view {
            graphix_compiler::NodeView::CallSite(cs) => cs,
            _ => return None,
        };
        let g = match inner_cs.resolved_apply()? {
            graphix_compiler::ApplyView::Lambda(g) => g,
            _ => return None,
        };
        let body = g.body();
        // Index param's name — from the lambda's FnType.args[0].kind.
        let idx_name = match &g.typ().args.first()?.kind {
            graphix_compiler::typ::FnArgKind::Positional { name: Some(n) } => n.clone(),
            graphix_compiler::typ::FnArgKind::Labeled { name, .. } => name.clone(),
            _ => return None,
        };
        let body_kir = ctx.with_input(
            Input { name: idx_name.clone(), prim: PrimType::I64, bind_id: None },
            |inner| emit_expr_node(body, inner, ec),
        )?;
        // Output element type is the body's type — prim or composite
        // (tuple/struct/variant/…). Reject shapes with no array-element
        // representation.
        let elem = body_kir.typ.clone();
        if is_unit_or_null(&elem) {
            return None;
        }
        Some(GirExpr {
            op: GirOp::ArrayInit {
                n: Box::new(n_kir),
                idx_local: idx_name,
                body: Box::new(body_kir),
            },
            typ: gir::array_type(elem),
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
