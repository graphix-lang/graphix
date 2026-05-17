//! Node fusion: identify pure-expression / pure-function subtrees and
//! lower them into the typed kernel IR ([`crate::kernel_ir`]). The
//! resulting [`KirExpr`] / [`KirKernel`] is then lowered to Cranelift
//! IR by the JIT path (see `crate::kir_jit`) for in-process JIT
//! compilation.
//!
//! This module is the front end (Graphix `Expr` → KIR). The IR types
//! live in [`crate::kernel_ir`].
//!
//! Driving examples are mandelbrot's `iterate` (primitive args, a
//! `select` with arithmetic guards, a self-recursive tail call) and
//! naive `fib` (primitive args, non-tail self recursion lowered as
//! direct recursion).

use crate::{
    effects::EffectKind,
    env::Env,
    expr::{Expr, ExprKind, ModPath, Pattern, StructurePattern},
    kernel_ir::{
        self as kir, ConstVal, KirExpr, KirKernel, KirOp, KirStmt, Let, SelectArm,
    },
    typ::Type,
    ExecCtx, Rt, Scope, UserEvent,
};
use arcstr::ArcStr;
use netidx_value::Value;
use std::sync::Arc as SArc;

// Re-export the canonical KIR types so existing callers (graphix-shell,
// graphix-package-bench, in-tree tests) keep compiling. The IR's
// definitive home is `crate::kernel_ir`; these aliases exist purely to
// keep the public API surface stable across the move.
pub use crate::kernel_ir::{Input, KirType, KnownConst, KnownFusedFn, PrimType};

/// Lookup table the emitter consults whenever it sees a `Ref` — tells
/// us the variable's primitive type and the name to use in generated
/// code. Also carries a small registry of already-fused functions so a
/// kernel body can direct-call another fused kernel instead of round-
/// tripping through the interpreted CallSite, plus a registry of
/// compile-time-known primitive constants for inlining.
#[derive(Debug, Clone, Default)]
pub struct FusionCtx {
    pub inputs: Vec<Input>,
    /// Function-typed parameters of the kernel being built. Mirrors
    /// the kernel's `fn_params` list — `Apply{Ref(name)}` against any
    /// of these names lowers to [`KirOp::DynCall`] instead of a
    /// static call. Keyed by Graphix name; the value is the param's
    /// (zero-based) index in this list, which becomes the
    /// `fn_index` in the emitted DynCall.
    pub fn_inputs: Vec<crate::kernel_ir::FnParam>,
    /// Array-typed parameters of the kernel being built. Mirrors the
    /// kernel's `array_params` list — `Ref(name)` resolves against
    /// these for `array::len(name)` and `name[i]` lowering. Sibling
    /// to `inputs` (no shadowing — same name in both is a fusion
    /// abort by `find_array`'s caller).
    pub array_inputs: Vec<crate::kernel_ir::ArrayInput>,
    /// Tuple-typed kernel parameters. Looked up by name when
    /// emitting `t.0` / `t.1` (TupleRef) accesses and recognised at
    /// `(a, b, c)` literal sites that match a known param shape.
    pub tuple_inputs: Vec<crate::kernel_ir::TupleInput>,
    /// Struct-typed kernel parameters. Same pattern — `s.field`
    /// (StructRef) accesses look up here, with the field name
    /// resolved to a sorted index at lowering time.
    pub struct_inputs: Vec<crate::kernel_ir::StructInput>,
    /// Variant-typed kernel parameters. Looked up by name when
    /// emitting tag-match dispatches and payload reads. The cases
    /// list constrains which tags can flow through; the lowering
    /// for a select arm matching `` `Foo(a, b) `` checks both that
    /// the tag matches and that the case shape matches the param's
    /// declared cases.
    pub variant_inputs: Vec<crate::kernel_ir::VariantInput>,
    /// Other kernels already fused in the current pass. Used by
    /// `emit_expr` to lower `Apply { fn: Ref(name) }` to a direct call
    /// when `name` is in this map. Keyed by the Graphix-level name so
    /// rewrites done earlier in the walk are visible to later fusions.
    pub known_fns: std::collections::BTreeMap<ArcStr, KnownFusedFn>,
    /// Compile-time-known primitive constants, inlined at Ref sites.
    /// Populated by the rewrite pass from `let <name> = <literal>;`
    /// bindings. Keyed by Graphix-level name.
    pub known_consts: std::collections::BTreeMap<ArcStr, KnownConst>,
}

impl FusionCtx {
    pub fn find(&self, name: &str) -> Option<&Input> {
        self.inputs.iter().find(|i| &*i.name == name)
    }

    pub fn find_fn(&self, name: &str) -> Option<&KnownFusedFn> {
        self.known_fns.get(name)
    }

    pub fn find_const(&self, name: &str) -> Option<&KnownConst> {
        self.known_consts.get(name)
    }

    /// Look up an array-typed kernel parameter by Graphix name.
    pub fn find_array(&self, name: &str) -> Option<&crate::kernel_ir::ArrayInput> {
        self.array_inputs.iter().find(|a| &*a.name == name)
    }

    /// Look up a tuple-typed kernel parameter by Graphix name.
    pub fn find_tuple(&self, name: &str) -> Option<&crate::kernel_ir::TupleInput> {
        self.tuple_inputs.iter().find(|t| &*t.name == name)
    }

    /// Look up a struct-typed kernel parameter by Graphix name.
    pub fn find_struct(&self, name: &str) -> Option<&crate::kernel_ir::StructInput> {
        self.struct_inputs.iter().find(|s| &*s.name == name)
    }

    /// Look up a variant-typed kernel parameter by Graphix name.
    pub fn find_variant(&self, name: &str) -> Option<&crate::kernel_ir::VariantInput> {
        self.variant_inputs.iter().find(|v| &*v.name == name)
    }

    /// Look up a fn-typed kernel parameter by Graphix name and call
    /// arity, returning its zero-based index in `fn_inputs` (the
    /// `fn_index` for emitted `KirOp::DynCall`) plus the param itself.
    /// Variadic builtins produce one `FnParam` per *call arity* —
    /// `sum(a, b)` and `sum(a, b, c)` register as two separate slots,
    /// so arity must match too. For non-variadic callees, only one
    /// arity is ever registered and the check is degenerate.
    pub fn find_fn_input(
        &self,
        name: &str,
        arity: usize,
    ) -> Option<(u32, &crate::kernel_ir::FnParam)> {
        self.fn_inputs
            .iter()
            .enumerate()
            .find(|(_, fp)| {
                fp.name.as_str() == name && fp.arg_types.len() == arity
            })
            .map(|(i, fp)| (i as u32, fp))
    }
}

/// Return the last path segment of an `Apply { function: Ref(path) }`,
/// or `None` if the function isn't a `Ref`. Unlike [`ident_of`], this
/// accepts multi-segment paths (`array::len`, `str::contains`, …) and
/// returns just the last segment. Used to pattern-match stdlib
/// builtin call sites after module resolution has run.
fn trailing_segment(function: &Expr) -> Option<&str> {
    match &function.kind {
        ExprKind::Ref { name } => {
            netidx::path::Path::basename(name.0.as_ref())
        }
        _ => None,
    }
}

fn ident_of(path: &ModPath) -> Option<&str> {
    // A Ref we can fuse must be a bare identifier (no module path),
    // because we read its value from a local binding in the kernel.
    // `foo::bar` would require a more elaborate story.
    let s: &str = path.0.as_ref();
    let base = netidx::path::Path::basename(s)?;
    if netidx::path::Path::levels(s) != 1 {
        return None;
    }
    Some(base)
}

// ─── Expression-position emitters ────────────────────────────────

/// Emit a `select` at expression position as a [`KirOp::IfChain`].
/// Every arm body must itself emit as a primitive expression, and
/// every arm body must have the same primitive type (since the chain
/// evaluates to a single value).
fn emit_select_as_expr(
    s: &crate::expr::SelectExpr,
    ctx: &FusionCtx,
) -> Option<KirExpr> {
    let scrut = emit_expr(&s.arg, ctx)?;
    let n = s.arms.len();
    if n == 0 {
        return None;
    }
    let mut arms: Vec<(Option<KirExpr>, KirExpr)> = Vec::with_capacity(n);
    let mut unified_typ: Option<KirType> = None;
    for (i, (pat, body)) in s.arms.iter().enumerate() {
        let is_last = i == n - 1;
        let mut arm_ctx = ctx.clone();
        let cond_kir =
            emit_arm_condition(&scrut, &pat.structure_predicate, &mut arm_ctx)?;
        let guard_kir = match &pat.guard {
            None => None,
            Some(g) => {
                let g = emit_expr(g, &arm_ctx)?;
                if g.typ != KirType::Prim(PrimType::Bool) {
                    return None;
                }
                Some(g)
            }
        };
        let combined = combine_cond_and_guard(cond_kir, guard_kir);
        let body_kir = emit_expr(body, &arm_ctx)?;
        match unified_typ.as_ref() {
            Some(t) if *t != body_kir.typ => return None,
            None => unified_typ = Some(body_kir.typ.clone()),
            _ => (),
        }
        // First arm with no condition + only one arm total: just emit
        // the body as the unconditional branch.
        match (i, &combined) {
            (0, None) if is_last => {
                arms.push((None, body_kir));
            }
            (_, _) if !is_last && combined.is_none() => {
                // Unconditional non-last arm. Subsequent arms are dead;
                // keep emitting them, but mark this one as
                // unconditional so the renderer doesn't add an `if`.
                arms.push((None, body_kir));
            }
            _ => {
                arms.push((combined, body_kir));
            }
        }
    }
    let typ = unified_typ?;
    Some(KirExpr { op: KirOp::IfChain { arms }, typ })
}

/// Compose a structure-predicate condition (which may be `None` =
/// "always matches") with an optional guard expression into a single
/// optional bool [`KirExpr`].
fn combine_cond_and_guard(
    cond: Option<KirExpr>,
    guard: Option<KirExpr>,
) -> Option<KirExpr> {
    match (cond, guard) {
        (None, None) => None,
        (Some(c), None) => Some(c),
        (None, Some(g)) => Some(g),
        (Some(c), Some(g)) => kir::bool_op(c, g, kir::BoolOp::And),
    }
}

/// Emit an `Apply` call whose target is a Ref to a function already in
/// `ctx.known_fns`. Lowers to a [`KirOp::Call`].
fn emit_known_fused_call(
    a: &crate::expr::ApplyExpr,
    ctx: &FusionCtx,
) -> Option<KirExpr> {
    if a.args.iter().any(|(label, _)| label.is_some()) {
        return None;
    }
    // Stdlib helpers we recognise by trailing path segment regardless
    // of whether the user wrote `array::len(xs)` or — after `use array;`
    // — a bare `len(xs)`. The leading segment is consumed by module
    // resolution; what remains uniquely identifies the builtin
    // intended at this call site, because the typechecker has already
    // proven the argument types match. See M7 design notes.
    let trailing = trailing_segment(&a.function);
    // `array::len(arr)` — when `arr` resolves to an array kernel
    // param, lower to `KirOp::ArrayLen` (no DynCall, no kernel call).
    if trailing == Some("len") && a.args.len() == 1 {
        if let Some(arr_name) = array_param_name(&a.args[0].1, ctx) {
            return Some(KirExpr {
                op: KirOp::ArrayLen { name: arr_name },
                typ: KirType::Prim(PrimType::U64),
            });
        }
        // `len` on something else (a string, a map…) — fall through to
        // the regular call-resolution path. Returning None now would
        // strand legitimate non-array `len` calls.
    }
    // `array::fold(arr, init, |acc, x| body)` — when `arr` is an
    // array kernel param, `init` and `body` lower to scalar KIR, and
    // the callback is an inline lambda with two scalar params, lower
    // to `KirOp::ArrayFold`. Anything else (callback bound elsewhere,
    // non-primitive accumulator, …) falls through to the regular
    // call-resolution path.
    if trailing == Some("fold") && a.args.len() == 3 {
        if let Some(folded) = emit_array_fold(&a.args[0].1, &a.args[1].1, &a.args[2].1, ctx) {
            return Some(folded);
        }
    }
    // `array::init(n, |idx: i64| body)` — produces an Array<T> where
    // T is the body's primitive type. Result KirExpr.typ is
    // KirType::Array(T). Composes with downstream ArrayMap/ArrayFold
    // at the source level (separate kernels), but never intra-kernel
    // because ArrayInit is kernel-body-only in v1.
    if trailing == Some("init") && a.args.len() == 2 {
        if let Some(produced) = emit_array_init(&a.args[0].1, &a.args[1].1, ctx) {
            return Some(produced);
        }
    }
    // `array::map(arr, |x| body)` — produces Array<U> where U is the
    // body's primitive type. Inline callback with one primitive param
    // matching the input array's element type.
    if trailing == Some("map") && a.args.len() == 2 {
        if let Some(produced) = emit_array_map(&a.args[0].1, &a.args[1].1, ctx) {
            return Some(produced);
        }
    }
    // `array::filter(arr, |x| pred)` — preserves the input element
    // type; result length is dynamic. Predicate must lower to scalar
    // bool KIR.
    if trailing == Some("filter") && a.args.len() == 2 {
        if let Some(produced) = emit_array_filter(&a.args[0].1, &a.args[1].1, ctx) {
            return Some(produced);
        }
    }
    let name = match &a.function.kind {
        ExprKind::Ref { name } => ident_of(name)?,
        _ => return None,
    };
    // Prefer a DynCall against a fn-typed kernel parameter (HOF arg)
    // when the name shadows one. Fn-typed params are local to the
    // current kernel, so they win over any same-named fused-static
    // entry in known_fns.
    if let Some((fn_index, fp)) = ctx.find_fn_input(name, a.args.len()) {
        let mut kargs = Vec::with_capacity(a.args.len());
        for ((_, expr), expected) in a.args.iter().zip(&fp.arg_types) {
            let e = emit_expr(expr, ctx)?;
            if &e.typ != expected {
                return None;
            }
            kargs.push(e);
        }
        let return_type = fp.return_type.clone();
        return Some(KirExpr {
            op: KirOp::DynCall {
                fn_index,
                args: kargs,
                arg_types: fp.arg_types.clone(),
                return_type: return_type.clone(),
            },
            typ: return_type,
        });
    }
    // Static call to a previously-fused function.
    let fn_info = ctx.find_fn(name)?.clone();
    if a.args.len() != fn_info.arg_types.len() {
        return None;
    }
    let mut kargs = Vec::with_capacity(a.args.len());
    for ((_, expr), expected) in a.args.iter().zip(&fn_info.arg_types) {
        let e = emit_expr(expr, ctx)?;
        if e.typ != *expected {
            return None;
        }
        kargs.push(e);
    }
    Some(KirExpr {
        op: KirOp::Call { fn_name: ArcStr::from(name), args: kargs },
        typ: fn_info.return_type,
    })
}

/// If `expr` is a `Ref` to an array-typed kernel parameter, return
/// its name (suitable for `KirOp::ArrayLen` / `KirOp::ArrayGet`).
/// Anything else (computed array values, intra-kernel let-bound
/// arrays) is rejected — composability with array producers waits
/// for M7.4's `KirType::Array` plumbing.
fn array_param_name(expr: &Expr, ctx: &FusionCtx) -> Option<ArcStr> {
    if let ExprKind::Ref { name } = &expr.kind {
        let ident = ident_of(name)?;
        if let Some(ai) = ctx.find_array(ident) {
            return Some(ai.name.clone());
        }
    }
    None
}

/// Lower `array::fold(arr, init, |acc, x| body)` to
/// [`KirOp::ArrayFold`]. The callback must be an inline lambda with
/// two non-labelled positional params; both must annotate as
/// primitives matching the array's element type and `init`'s type.
/// `init` and `body` must each emit as scalar KIR over the surrounding
/// kernel's locals plus the two callback params.
fn emit_array_fold(
    arr_expr: &Expr,
    init_expr: &Expr,
    cb_expr: &Expr,
    ctx: &FusionCtx,
) -> Option<KirExpr> {
    let arr_name = array_param_name(arr_expr, ctx)?;
    // emit_array_fold only handles primitive elements today; composite
    // element fold support is a separate work item. Extract the
    // primitive or bail.
    let elem = ctx.find_array(&arr_name)?.elem.as_prim()?;
    // Callback must be an inline 2-arg primitive lambda. Anything else
    // (bound elsewhere, non-primitive types) — fall through.
    let lam = match &cb_expr.kind {
        ExprKind::Lambda(l) => l,
        _ => return None,
    };
    if lam.args.len() != 2 || lam.args.iter().any(|a| a.labeled.is_some()) {
        return None;
    }
    let acc_typ = lam.args[0].constraint.as_ref().and_then(PrimType::from_type)?;
    let elem_typ = lam.args[1].constraint.as_ref().and_then(PrimType::from_type)?;
    if elem_typ != elem {
        return None;
    }
    let acc_name = lam.args[0].pattern.single_bind()?;
    let elem_name = lam.args[1].pattern.single_bind()?;
    let body = match &lam.body {
        netidx::utils::Either::Left(e) => e,
        netidx::utils::Either::Right(_) => return None,
    };
    // Build a per-fold context: surrounding kernel's locals plus the
    // callback's two params as scalar inputs. Emit init in the outer
    // ctx, body in the inner ctx.
    let init = emit_expr(init_expr, ctx)?;
    if init.typ != KirType::Prim(acc_typ) {
        return None;
    }
    let mut inner = ctx.clone();
    inner.inputs.push(Input {
        name: acc_name.clone(),
        prim: acc_typ,
        bind_id: None,
    });
    inner.inputs.push(Input {
        name: elem_name.clone(),
        prim: elem_typ,
        bind_id: None,
    });
    let body_kir = emit_expr(body, &inner)?;
    if body_kir.typ != KirType::Prim(acc_typ) {
        return None;
    }
    Some(KirExpr {
        op: KirOp::ArrayFold {
            array: arr_name,
            elem_typ: elem,
            init: Box::new(init),
            acc_local: acc_name.clone(),
            elem_local: elem_name.clone(),
            body: Box::new(body_kir),
        },
        typ: KirType::Prim(acc_typ),
    })
}

/// Lower a `` `Tag(p0, p1) `` variant literal to
/// [`KirOp::VariantNew`]. Each payload must emit as scalar KIR; the
/// per-slot primitive types come from those scalar results.
fn emit_variant_new(
    tag: &ArcStr,
    args: &[Expr],
    ctx: &FusionCtx,
) -> Option<KirExpr> {
    let mut payloads: Vec<KirExpr> = Vec::with_capacity(args.len());
    let mut payload_types: Vec<PrimType> = Vec::with_capacity(args.len());
    for a in args {
        let e = emit_expr(a, ctx)?;
        let p = e.typ.as_prim()?;
        payloads.push(e);
        payload_types.push(p);
    }
    let case_types: Vec<KirType> =
        payload_types.iter().copied().map(KirType::Prim).collect();
    let typ = KirType::Variant(vec![(tag.clone(), case_types)]);
    Some(KirExpr {
        op: KirOp::VariantNew {
            tag: tag.clone(),
            payloads,
            payload_types,
        },
        typ,
    })
}

/// Lower `array::init(n, |idx: i64| body)` to [`KirOp::ArrayInit`].
/// The callback must be an inline 1-arg lambda whose param is an
/// `i64` index; the body emits as scalar KIR with that index visible
/// as a local. Result type is `KirType::Array(elem)` where `elem` is
/// the body's primitive type.
fn emit_array_init(
    n_expr: &Expr,
    cb_expr: &Expr,
    ctx: &FusionCtx,
) -> Option<KirExpr> {
    let n = emit_expr(n_expr, ctx)?;
    if !n.typ.as_prim().is_some_and(|p| p.is_integer()) {
        return None;
    }
    let lam = match &cb_expr.kind {
        ExprKind::Lambda(l) => l,
        _ => return None,
    };
    if lam.args.len() != 1 || lam.args.iter().any(|a| a.labeled.is_some()) {
        return None;
    }
    let idx_typ = lam.args[0].constraint.as_ref().and_then(PrimType::from_type)?;
    if idx_typ != PrimType::I64 {
        return None;
    }
    let idx_name = lam.args[0].pattern.single_bind()?;
    let body = match &lam.body {
        netidx::utils::Either::Left(e) => e,
        netidx::utils::Either::Right(_) => return None,
    };
    let mut inner = ctx.clone();
    inner.inputs.push(Input {
        name: idx_name.clone(),
        prim: PrimType::I64,
        bind_id: None,
    });
    let body_kir = emit_expr(body, &inner)?;
    let elem = body_kir.typ.as_prim()?;
    Some(KirExpr {
        op: KirOp::ArrayInit {
            n: Box::new(n),
            idx_local: idx_name.clone(),
            elem_typ: elem,
            body: Box::new(body_kir),
        },
        typ: KirType::Array(Box::new(KirType::Prim(elem))),
    })
}

/// Lower `array::map(arr, |x: E| body)` to [`KirOp::ArrayMap`].
/// `arr` must be a Ref to an array kernel param; the callback must be
/// an inline 1-arg lambda whose param annotates with a primitive type
/// matching the array's element type. Result type is
/// `KirType::Array(out_elem)` where `out_elem` is the body's
/// primitive type.
fn emit_array_map(
    arr_expr: &Expr,
    cb_expr: &Expr,
    ctx: &FusionCtx,
) -> Option<KirExpr> {
    let arr_name = array_param_name(arr_expr, ctx)?;
    // emit_array_map only handles primitive elements today.
    let in_elem = ctx.find_array(&arr_name)?.elem.as_prim()?;
    let lam = match &cb_expr.kind {
        ExprKind::Lambda(l) => l,
        _ => return None,
    };
    if lam.args.len() != 1 || lam.args.iter().any(|a| a.labeled.is_some()) {
        return None;
    }
    let x_typ = lam.args[0].constraint.as_ref().and_then(PrimType::from_type)?;
    if x_typ != in_elem {
        return None;
    }
    let x_name = lam.args[0].pattern.single_bind()?;
    let body = match &lam.body {
        netidx::utils::Either::Left(e) => e,
        netidx::utils::Either::Right(_) => return None,
    };
    let mut inner = ctx.clone();
    inner.inputs.push(Input {
        name: x_name.clone(),
        prim: in_elem,
        bind_id: None,
    });
    let body_kir = emit_expr(body, &inner)?;
    let out_elem = body_kir.typ.as_prim()?;
    Some(KirExpr {
        op: KirOp::ArrayMap {
            array: arr_name,
            in_elem,
            elem_local: x_name.clone(),
            out_elem,
            body: Box::new(body_kir),
        },
        typ: KirType::Array(Box::new(KirType::Prim(out_elem))),
    })
}

/// Lower `array::filter(arr, |x: E| pred)` to [`KirOp::ArrayFilter`].
/// `arr` must be a Ref to an array kernel param; the callback must be
/// an inline 1-arg lambda with a primitive param matching the array's
/// element type whose body returns `bool`. Result type is the same
/// `KirType::Array(elem)` as the input — filter preserves elements.
fn emit_array_filter(
    arr_expr: &Expr,
    cb_expr: &Expr,
    ctx: &FusionCtx,
) -> Option<KirExpr> {
    let arr_name = array_param_name(arr_expr, ctx)?;
    // emit_array_filter only handles primitive elements today.
    let elem = ctx.find_array(&arr_name)?.elem.as_prim()?;
    let lam = match &cb_expr.kind {
        ExprKind::Lambda(l) => l,
        _ => return None,
    };
    if lam.args.len() != 1 || lam.args.iter().any(|a| a.labeled.is_some()) {
        return None;
    }
    let x_typ = lam.args[0].constraint.as_ref().and_then(PrimType::from_type)?;
    if x_typ != elem {
        return None;
    }
    let x_name = lam.args[0].pattern.single_bind()?;
    let body = match &lam.body {
        netidx::utils::Either::Left(e) => e,
        netidx::utils::Either::Right(_) => return None,
    };
    let mut inner = ctx.clone();
    inner.inputs.push(Input {
        name: x_name.clone(),
        prim: elem,
        bind_id: None,
    });
    let pred_kir = emit_expr(body, &inner)?;
    if pred_kir.typ != KirType::Prim(PrimType::Bool) {
        return None;
    }
    Some(KirExpr {
        op: KirOp::ArrayFilter {
            array: arr_name,
            elem,
            elem_local: x_name.clone(),
            predicate: Box::new(pred_kir),
        },
        typ: KirType::Array(Box::new(KirType::Prim(elem))),
    })
}

/// Lower `tup.<idx>` (i.e. `ExprKind::TupleRef`) to `KirOp::TupleGet`.
/// Source must be a `Ref` to a tuple kernel param; `idx` must be in
/// range. Result type is the tuple slot's primitive type.
fn emit_tuple_ref(source: &Expr, idx: usize, ctx: &FusionCtx) -> Option<KirExpr> {
    let name = match &source.kind {
        ExprKind::Ref { name } => ident_of(name)?,
        _ => return None,
    };
    let ti = ctx.find_tuple(name)?;
    // Element type can be any `KirType` — primitive or composite.
    // Composite-element kernels route to the interpreter via the
    // `kernel_contains_composite_element_op` JIT guard; the
    // interpreter handles both shapes natively.
    let elem_typ = ti.elems.get(idx)?.clone();
    Some(KirExpr {
        op: KirOp::TupleGet {
            name: ti.name.clone(),
            idx,
            elem_typ: elem_typ.clone(),
        },
        typ: elem_typ,
    })
}

/// Lower `s.field` (i.e. `ExprKind::StructRef`) to `KirOp::StructGet`.
/// Source must be a `Ref` to a struct kernel param; `field` must
/// resolve to a known field. Result type is that field's primitive type.
fn emit_struct_ref(
    source: &Expr,
    field: &ArcStr,
    ctx: &FusionCtx,
) -> Option<KirExpr> {
    let name = match &source.kind {
        ExprKind::Ref { name } => ident_of(name)?,
        _ => return None,
    };
    let si = ctx.find_struct(name)?;
    let sorted_idx = si.fields.iter().position(|(n, _)| n == field)?;
    // Element type can be any `KirType` — primitive or composite.
    let elem_typ = si.fields[sorted_idx].1.clone();
    Some(KirExpr {
        op: KirOp::StructGet {
            name: si.name.clone(),
            field: field.clone(),
            sorted_idx,
            elem_typ: elem_typ.clone(),
        },
        typ: elem_typ,
    })
}

/// Lower a `(a, b, c)` tuple literal to `KirOp::TupleNew`. Each
/// field must emit as scalar KIR. Result type is
/// `KirType::Tuple(<per-slot prim types>)`.
fn emit_tuple_new(args: &[Expr], ctx: &FusionCtx) -> Option<KirExpr> {
    let mut fields: Vec<KirExpr> = Vec::with_capacity(args.len());
    let mut elem_types: Vec<PrimType> = Vec::with_capacity(args.len());
    for a in args {
        let e = emit_expr(a, ctx)?;
        let p = e.typ.as_prim()?;
        fields.push(e);
        elem_types.push(p);
    }
    let kir_elems: Vec<KirType> =
        elem_types.iter().copied().map(KirType::Prim).collect();
    let typ = KirType::Tuple(kir_elems);
    Some(KirExpr {
        op: KirOp::TupleNew { fields, elem_types },
        typ,
    })
}

/// Lower a `{x: a, y: b}` struct literal to `KirOp::StructNew`.
/// Sorts the fields alphabetically by name (graphix's canonical
/// struct layout). Each field value must emit as scalar KIR.
fn emit_struct_new(
    args: &[(ArcStr, Expr)],
    ctx: &FusionCtx,
) -> Option<KirExpr> {
    // Sort alphabetically by name to match graphix's canonical
    // ValArray layout. Cloning into a Vec we can sort independently
    // of the source order.
    let mut sorted_pairs: Vec<(ArcStr, &Expr)> =
        args.iter().map(|(n, e)| (n.clone(), e)).collect();
    sorted_pairs.sort_by(|a, b| a.0.cmp(&b.0));
    let mut sorted_fields: Vec<(ArcStr, KirExpr)> =
        Vec::with_capacity(sorted_pairs.len());
    let mut sorted_types: Vec<(ArcStr, PrimType)> =
        Vec::with_capacity(sorted_pairs.len());
    for (n, e) in sorted_pairs {
        let kir = emit_expr(e, ctx)?;
        let p = kir.typ.as_prim()?;
        sorted_fields.push((n.clone(), kir));
        sorted_types.push((n, p));
    }
    let kir_fields: Vec<(ArcStr, KirType)> = sorted_types
        .iter()
        .map(|(n, p)| (n.clone(), KirType::Prim(*p)))
        .collect();
    let typ = KirType::Struct(kir_fields);
    Some(KirExpr {
        op: KirOp::StructNew { sorted_fields, sorted_types },
        typ,
    })
}

/// Lower `arr[i]` (i.e. `ExprKind::ArrayRef`) to `KirOp::ArrayGet`.
/// `arr` must be a Ref to an array kernel param; `i` must emit as a
/// scalar integer expression. Result type is the array's element
/// type.
fn emit_array_ref(source: &Expr, idx: &Expr, ctx: &FusionCtx) -> Option<KirExpr> {
    let arr_name = array_param_name(source, ctx)?;
    // Element type comes from the array param. Bool elements are not
    // valid index targets but are valid result types — no special
    // handling needed beyond the lookup.
    let ai = ctx.find_array(&arr_name)?;
    // Element type can be any KirType — primitive uses the JIT's
    // fast scalar extraction; composite routes to the interpreter
    // via `kernel_contains_composite_element_op`.
    let elem = ai.elem.clone();
    let idx_expr = emit_expr(idx, ctx)?;
    if !idx_expr.typ.as_prim().is_some_and(|p| p.is_integer()) {
        return None;
    }
    Some(KirExpr {
        op: KirOp::ArrayGet { name: arr_name, idx: Box::new(idx_expr) },
        typ: elem,
    })
}

/// Emit a Graphix `{ let x = ...; let y = ...; body }` block as a
/// [`KirOp::Block`]. Each non-last statement must be a `let`-binding
/// whose value emits as a primitive expression; the final statement
/// provides the block's value.
fn emit_do_as_expr(exprs: &[Expr], ctx: &FusionCtx) -> Option<KirExpr> {
    if exprs.is_empty() {
        return None;
    }
    let mut local_ctx = ctx.clone();
    let mut lets: Vec<Let> = Vec::new();
    let last = exprs.len() - 1;
    for (i, e) in exprs.iter().enumerate() {
        if i == last {
            let body = emit_expr(e, &local_ctx)?;
            let typ = body.typ.clone();
            return Some(KirExpr {
                op: KirOp::Block { lets, tail: Box::new(body) },
                typ,
            });
        }
        match &e.kind {
            ExprKind::Bind(b) => {
                if b.rec {
                    return None;
                }
                let name = b.pattern.single_bind()?;
                let value = emit_expr(&b.value, &local_ctx)?;
                // Route the let to the right slot list based on the
                // value's KIR type. Same shape as `emit_bind_stmt`
                // (body-level let) — composing
                // `let xs = array::init(...);
                //  array::fold(xs, ...)` inside an expression-form
                // block works the same way as the equivalent
                // statement-form block.
                // Unit/String aren't usefully bindable — `register_kir_binding`
                // bails for both. A Unit-typed Block let would have meant
                // binding a side-effect call to a name (typecheck should
                // have rejected), so bailing defensively is correct.
                register_kir_binding(&mut local_ctx, name, &value.typ)?;
                lets.push(Let { local: name.clone(), value });
            }
            ExprKind::NoOp => {}
            _ => return None,
        }
    }
    None
}

/// Register a kernel-local binding in `ctx` by routing it into the
/// right slot list based on its KIR type. Used by every kernel-let
/// emission site (`emit_do`'s let arm, `emit_do_as_expr`'s block-let
/// arm, etc.) so a new `KirType` variant only needs a new arm here.
///
/// Returns `None` for KirType variants we can't represent as a kernel
/// local — currently `Unit` (caller usually has already routed Unit
/// values to `KirStmt::Discard`) and `String` (no string_inputs slot
/// list yet). The caller short-circuits its parent on `None`.
fn register_kir_binding(
    ctx: &mut FusionCtx,
    name: &ArcStr,
    value_typ: &KirType,
) -> Option<()> {
    match value_typ {
        KirType::Prim(prim) => {
            ctx.inputs.push(Input {
                name: name.clone(),
                prim: *prim,
                bind_id: None,
            });
        }
        KirType::Array(elem) => {
            ctx.array_inputs.push(crate::kernel_ir::ArrayInput {
                name: name.clone(),
                elem: (**elem).clone(),
                bind_id: None,
            });
        }
        KirType::Tuple(elems) => {
            ctx.tuple_inputs.push(crate::kernel_ir::TupleInput {
                name: name.clone(),
                elems: elems.clone(),
                bind_id: None,
            });
        }
        KirType::Struct(fields) => {
            ctx.struct_inputs.push(crate::kernel_ir::StructInput {
                name: name.clone(),
                fields: fields.clone(),
                bind_id: None,
            });
        }
        KirType::Variant(cases) => {
            ctx.variant_inputs.push(crate::kernel_ir::VariantInput {
                name: name.clone(),
                cases: cases.clone(),
                bind_id: None,
            });
        }
        // Unit/String aren't usefully bindable as kernel locals. The
        // caller bails (its surrounding fusion attempt fails and the
        // expression stays interpreted).
        KirType::Unit | KirType::String => return None,
    }
    Some(())
}

/// Emit a sub-expression as a [`KirExpr`]. Returns `None` if any sub-
/// tree is something the emitter doesn't handle yet — that short-
/// circuits the whole parent, so the caller falls back to the
/// interpreted path (or, in AOT mode, refuses fusion).
pub fn emit_expr(expr: &Expr, ctx: &FusionCtx) -> Option<KirExpr> {
    match &expr.kind {
        ExprKind::Constant(v) => {
            // String literals go through KirOp::ConstStr (ArcStr can't
            // fit in the Copy `ConstVal`). All other primitives go
            // through the scalar `ConstVal` path.
            if let Value::String(s) = v {
                return Some(KirExpr {
                    op: KirOp::ConstStr(s.clone()),
                    typ: KirType::String,
                });
            }
            let c = ConstVal::from_value(v)?;
            Some(kir::const_expr(c))
        }
        ExprKind::StringInterpolate { args } => {
            // Lower each piece — interpolations may be arbitrary
            // sub-expressions, plain string literals stay ConstStr —
            // and emit KirOp::Concat which the interpreter renders
            // by appending Display of each part.
            let mut parts = Vec::with_capacity(args.len());
            for a in args.iter() {
                parts.push(emit_expr(a, ctx)?);
            }
            Some(KirExpr {
                op: KirOp::Concat(parts),
                typ: KirType::String,
            })
        }
        ExprKind::Ref { name } => {
            let ident = ident_of(name)?;
            // Prefer lambda-arg / let-bound scalar locals, then array,
            // tuple, struct locals; finally fall back to the known-
            // constants registry (outer-scope `let x = <lit>;`
            // bindings inlined at compile time). All four locals
            // render to bare `<name>` in Rust; the type drives
            // downstream lowering choices.
            if let Some(input) = ctx.find(ident) {
                return Some(kir::local(input.name.clone(), input.prim));
            }
            if let Some(ai) = ctx.find_array(ident) {
                return Some(KirExpr {
                    op: KirOp::Local(ai.name.clone()),
                    typ: KirType::Array(Box::new(ai.elem.clone())),
                });
            }
            if let Some(ti) = ctx.find_tuple(ident) {
                return Some(KirExpr {
                    op: KirOp::Local(ti.name.clone()),
                    typ: KirType::Tuple(ti.elems.clone()),
                });
            }
            if let Some(si) = ctx.find_struct(ident) {
                return Some(KirExpr {
                    op: KirOp::Local(si.name.clone()),
                    typ: KirType::Struct(si.fields.clone()),
                });
            }
            if let Some(vi) = ctx.find_variant(ident) {
                return Some(KirExpr {
                    op: KirOp::Local(vi.name.clone()),
                    typ: KirType::Variant(vi.cases.clone()),
                });
            }
            if let Some(c) = ctx.find_const(ident) {
                // Inlining a known-const: clone the stored KIR. The
                // const's expr is closed (no free locals), so cloning
                // it into this position is always sound.
                return Some(c.expr.clone());
            }
            None
        }
        ExprKind::ExplicitParens(inner) => emit_expr(inner, ctx),
        ExprKind::Add { lhs, rhs } => {
            kir::arith(emit_expr(lhs, ctx)?, emit_expr(rhs, ctx)?, kir::BinOp::Add)
        }
        ExprKind::Sub { lhs, rhs } => {
            kir::arith(emit_expr(lhs, ctx)?, emit_expr(rhs, ctx)?, kir::BinOp::Sub)
        }
        ExprKind::Mul { lhs, rhs } => {
            kir::arith(emit_expr(lhs, ctx)?, emit_expr(rhs, ctx)?, kir::BinOp::Mul)
        }
        ExprKind::Div { lhs, rhs } => {
            kir::arith(emit_expr(lhs, ctx)?, emit_expr(rhs, ctx)?, kir::BinOp::Div)
        }
        ExprKind::Mod { lhs, rhs } => {
            kir::arith(emit_expr(lhs, ctx)?, emit_expr(rhs, ctx)?, kir::BinOp::Mod)
        }
        ExprKind::Eq { lhs, rhs } => {
            kir::cmp(emit_expr(lhs, ctx)?, emit_expr(rhs, ctx)?, kir::CmpOp::Eq)
        }
        ExprKind::Ne { lhs, rhs } => {
            kir::cmp(emit_expr(lhs, ctx)?, emit_expr(rhs, ctx)?, kir::CmpOp::Ne)
        }
        ExprKind::Lt { lhs, rhs } => {
            kir::cmp(emit_expr(lhs, ctx)?, emit_expr(rhs, ctx)?, kir::CmpOp::Lt)
        }
        ExprKind::Gt { lhs, rhs } => {
            kir::cmp(emit_expr(lhs, ctx)?, emit_expr(rhs, ctx)?, kir::CmpOp::Gt)
        }
        ExprKind::Lte { lhs, rhs } => {
            kir::cmp(emit_expr(lhs, ctx)?, emit_expr(rhs, ctx)?, kir::CmpOp::Lte)
        }
        ExprKind::Gte { lhs, rhs } => {
            kir::cmp(emit_expr(lhs, ctx)?, emit_expr(rhs, ctx)?, kir::CmpOp::Gte)
        }
        ExprKind::And { lhs, rhs } => kir::bool_op(
            emit_expr(lhs, ctx)?,
            emit_expr(rhs, ctx)?,
            kir::BoolOp::And,
        ),
        ExprKind::Or { lhs, rhs } => kir::bool_op(
            emit_expr(lhs, ctx)?,
            emit_expr(rhs, ctx)?,
            kir::BoolOp::Or,
        ),
        ExprKind::Not { expr } => kir::not(emit_expr(expr, ctx)?),
        // A Select at expression position lowers to an if-chain.
        ExprKind::Select(s) => emit_select_as_expr(s, ctx),
        // A Do block at expression position lowers to a Block.
        ExprKind::Do { exprs } => emit_do_as_expr(exprs, ctx),
        // Direct call to an already-fused function — lowered to a Call.
        // Special-cased Apply forms (e.g. `array::len(arr)`) are
        // handled inside `emit_known_fused_call`.
        ExprKind::Apply(a) => emit_known_fused_call(a, ctx),
        // `arr[i]` — when `arr` is an array param of the kernel, this
        // lowers to a `KirOp::ArrayGet`. The element type comes from
        // the param's `elem`. Anything else (nested arrays, an
        // expression that produces an array intra-kernel) currently
        // bails out — composability with array-producing ops lands
        // in M7.4 once `KirExpr.typ` covers `KirType::Array`.
        ExprKind::ArrayRef { source, i } => emit_array_ref(source, i, ctx),
        // `tup.0` — tuple field access by literal index. Source must
        // be a Ref to a tuple kernel param.
        ExprKind::TupleRef { source, field } => emit_tuple_ref(source, *field, ctx),
        // `s.field` — struct field access by name. Source must be a
        // Ref to a struct kernel param; `field` resolves to a sorted
        // index at lowering time.
        ExprKind::StructRef { source, field } => emit_struct_ref(source, field, ctx),
        // `(a, b, c)` literal — every field must lower to scalar KIR;
        // result type is `KirType::Tuple(per-slot prim types)`.
        ExprKind::Tuple { args } => emit_tuple_new(args, ctx),
        // `{x: a, y: b}` literal — fields are sorted alphabetically,
        // each field value lowers to scalar KIR; result type is
        // `KirType::Struct(sorted fields)`.
        ExprKind::Struct(s) => emit_struct_new(&s.args, ctx),
        // `` `Tag(p0, p1) `` or `` `Tag `` — variant constructor.
        // VariantNew handles both: nullary tags render as
        // `Value::String(literal!("tag"))` and with-payload tags as
        // `Value::Array(ValArray::from_iter_exact([...]))`.
        ExprKind::Variant { tag, args } => {
            emit_variant_new(tag, args, ctx)
        }
        // `cast<T>(expr)` between primitives → KirOp::Cast.
        ExprKind::TypeCast { expr, typ } => {
            let target = PrimType::from_type(typ)?;
            kir::cast(emit_expr(expr, ctx)?, target)
        }
        // `$` (or-never) and `?` (qop) on a post-typecheck expression
        // assert that the inner value succeeded. For a fused kernel
        // the types were proven at compile time, so we just emit the
        // inner expression. If the runtime would actually error, our
        // fusion candidate-selection rejects the expression upstream —
        // we only emit fused code for operations that can't fail.
        ExprKind::OrNever(inner) | ExprKind::Qop(inner) => emit_expr(inner, ctx),
        // Deliberately not-yet-supported: Bind (at non-statement
        // position), Lambda, checked arithmetic, Sample, and anything
        // reactive. Presence of those inside a fusion candidate
        // aborts the attempt.
        _ => None,
    }
}

// ─── Body-position emitters ──────────────────────────────────────

/// Information about a self-recursive function, used by the body
/// emitter to detect tail calls and lower them to a [`KirStmt::TailCall`].
///
/// When `self_info` is `Some`, the kernel emitter wraps the body in a
/// `loop { ... }` and every tail call updates the loop variables and
/// continues. When it is `None`, tail positions emit a [`KirStmt::Return`].
#[derive(Debug, Clone)]
pub struct SelfInfo {
    /// The graphix name being bound to the lambda (e.g. "iterate").
    pub name: ArcStr,
    /// Scalar params in source order. Subset of the kernel's full
    /// argspec — composite params (array/tuple/struct) appear in
    /// the sibling lists below.
    pub params: Vec<Input>,
    /// Source-order full argspec for tail-call validation: one
    /// entry per lambda arg, recording the param's `KirType` so the
    /// validator can typecheck the new value.
    pub source_args: Vec<SelfArg>,
}

#[derive(Debug, Clone)]
pub struct SelfArg {
    pub name: ArcStr,
    pub typ: KirType,
}

/// Emit a sequence of [`KirStmt`]s evaluating `expr` as a function
/// body. Handles pure expressions (lowered to `Return`), self-tail
/// calls (lowered to `TailCall`), `select` over primitive scrutinees,
/// and `let`-style bindings.
///
/// Returns `None` if any sub-expression isn't in the supported subset.
pub fn emit_body(
    expr: &Expr,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<Vec<KirStmt>> {
    let mut out = Vec::new();
    emit_body_into(&mut out, expr, ctx, self_info)?;
    Some(out)
}

fn emit_body_into(
    out: &mut Vec<KirStmt>,
    expr: &Expr,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<()> {
    match &expr.kind {
        ExprKind::Do { exprs } => emit_do(out, exprs, ctx, self_info),
        ExprKind::ExplicitParens(inner) => emit_body_into(out, inner, ctx, self_info),
        ExprKind::Select(s) => emit_select(out, s, ctx, self_info),
        _ => emit_tail(out, expr, ctx, self_info),
    }
}

/// Emit a Do block as a sequence of body statements: each non-last
/// expr becomes a `Let` (or skipped NoOp); the last is the tail.
fn emit_do(
    out: &mut Vec<KirStmt>,
    exprs: &[Expr],
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<()> {
    if exprs.is_empty() {
        return None;
    }
    // Walk a fresh ctx copy so bindings added in the block are visible
    // to later statements but don't leak out.
    let mut local_ctx = ctx.clone();
    let last = exprs.len() - 1;
    for (i, e) in exprs.iter().enumerate() {
        if i == last {
            emit_body_into(out, e, &local_ctx, self_info)?;
        } else {
            match &e.kind {
                ExprKind::Bind(b) => emit_bind_stmt(out, b, &mut local_ctx)?,
                ExprKind::NoOp => {}
                _ => {
                    // Sync side-effect statement: evaluate it for the
                    // effect, discard the value. Unit-typed results
                    // (sync builtin calls like `println(...)`) lower
                    // to `KirStmt::Discard` — no Let wrapper, no
                    // local. Other types get a synthetic-name Let
                    // (composite-typed discards auto-drop at block
                    // exit per the §9 composite-local scope work).
                    // If `emit_expr` returns None (async builtin, an
                    // unsupported construct, etc.) we bail like
                    // before — the surrounding kernel build fails
                    // and the program runs unfused.
                    let value = emit_expr(e, &local_ctx)?;
                    if matches!(value.typ, KirType::Unit) {
                        out.push(KirStmt::Discard(value));
                    } else {
                        let discard_name = ArcStr::from(format!(
                            "__discard_{}",
                            out.len()
                        ));
                        out.push(KirStmt::Let(Let {
                            local: discard_name,
                            value,
                        }));
                    }
                }
            }
        }
    }
    Some(())
}

/// Emit a `let`-style binding as a [`KirStmt::Let`] and extend the
/// ctx so later emissions can see the new input.
fn emit_bind_stmt(
    out: &mut Vec<KirStmt>,
    b: &crate::expr::BindExpr,
    ctx: &mut FusionCtx,
) -> Option<()> {
    if b.rec {
        return None;
    }
    let name = b.pattern.single_bind()?;
    let value = emit_expr(&b.value, ctx)?;
    // Route the let to the right slot list based on the value's KIR
    // type. The emitted `KirStmt::Let` is the same shape regardless
    // — the Rust emitter renders `let mut <name> = <expr>;` either
    // way, and `<expr>`'s type drives whether `<name>` ends up as a
    // scalar local or a ValArray local in the generated body.
    //
    // Scalar value → scalar `Input` in `ctx.inputs`. Downstream
    // `Ref(name)` lowers via `local(...)`.
    //
    // `Array<P>` value → `ArrayInput` in `ctx.array_inputs`.
    // Downstream `name[i]` / `array::len(name)` /
    // `array::fold(name, ...)` etc. resolve `name` via
    // `find_array` just like a kernel array param. This is what
    // makes a single fused kernel able to compose
    // `let products = array::init(...); array::fold(products, ...)`.
    //
    // `Tuple` / `Struct` values land in the matching slot list, so
    // `name.0` / `name.field` accesses inside the body lower
    // through the existing TupleGet/StructGet path.
    // A Unit-typed value (e.g. `let _ = println(...)` or an
    // implicit discard-let synthesized by `emit_do` for a sync
    // side-effect call) doesn't get bound — it has no consumable
    // value. Emit a `KirStmt::Discard` and don't push any input
    // slot. Subsequent code can't reference `name` usefully (its
    // type is Unit, no ops accept it), so leaving it un-registered
    // is correct.
    if matches!(value.typ, KirType::Unit) {
        out.push(KirStmt::Discard(value));
        return Some(());
    }
    register_kir_binding(ctx, name, &value.typ)?;
    out.push(KirStmt::Let(Let { local: name.clone(), value }));
    Some(())
}

/// Emit a `select` expression as a [`KirStmt::Select`]. Each arm body
/// is its own sub-body that either ends in a return or a tail call (or
/// flows into nested control flow).
fn emit_select(
    out: &mut Vec<KirStmt>,
    s: &crate::expr::SelectExpr,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<()> {
    let scrut = emit_expr(&s.arg, ctx)?;
    let n = s.arms.len();
    let mut arms: Vec<SelectArm> = Vec::with_capacity(n);
    for (pat, arm_body) in s.arms.iter() {
        let arm = emit_arm(&scrut, pat, arm_body, ctx, self_info)?;
        arms.push(arm);
    }
    out.push(KirStmt::Select { arms });
    let _ = n;
    Some(())
}

/// Emit one `select` arm. Returns the constructed [`SelectArm`] —
/// whose `cond` is `None` for unconditional arms (Ignore / bare Bind
/// patterns with no guard) and `Some(...)` for conditional arms.
fn emit_arm(
    scrut: &KirExpr,
    pat: &Pattern,
    arm_body: &Expr,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<SelectArm> {
    let mut arm_ctx = ctx.clone();
    let cond = emit_arm_condition(scrut, &pat.structure_predicate, &mut arm_ctx)?;
    let guard = match &pat.guard {
        None => None,
        Some(g) => {
            let g = emit_expr(g, &arm_ctx)?;
            if g.typ != KirType::Prim(PrimType::Bool) {
                return None;
            }
            Some(g)
        }
    };
    let combined = combine_cond_and_guard(cond, guard);
    let mut body = Vec::new();
    emit_body_into(&mut body, arm_body, &arm_ctx, self_info)?;
    Some(SelectArm { cond: combined, body })
}

/// Emit the condition expression for an arm's structure predicate.
/// Returns `Ok(None)` when the predicate always matches (Ignore /
/// Bind), `Ok(Some(...))` for a real test, and `None` if the emitter
/// can't express the predicate (compound patterns — variants, tuples,
/// arrays — are not supported yet). Mutates `arm_ctx` to add any
/// bindings introduced by the pattern.
fn emit_arm_condition(
    scrut: &KirExpr,
    pat: &StructurePattern,
    arm_ctx: &mut FusionCtx,
) -> Option<Option<KirExpr>> {
    match pat {
        StructurePattern::Ignore => Some(None),
        StructurePattern::Bind(name) => {
            // Introduce a new local that aliases the scrutinee's type.
            // The scrutinee value itself isn't auto-bound by name here;
            // that would require emitting a `let` at the start of the
            // arm body. Existing tests don't exercise the pattern of
            // referencing the bound name with a complex scrutinee, so
            // we preserve the historic behaviour of just adding the
            // input to ctx — any lookup will resolve to the bound name
            // as a free identifier in the arm body, which works only
            // when the scrutinee was itself a bare local of that name.
            // Compound scrutinee + Bind would leave the bound name
            // dangling; the rewrite pass should reject that case.
            // Bind binds a scalar local — array scrutinees aren't
            // valid match positions per typecheck.
            let prim = scrut.typ.as_prim()?;
            arm_ctx.inputs.push(Input {
                name: name.clone(),
                prim,
                bind_id: None,
            });
            Some(None)
        }
        StructurePattern::Literal(v) => {
            let c = ConstVal::from_value(v)?;
            if KirType::Prim(c.typ()) != scrut.typ {
                return None;
            }
            // Small simplification for bool literals: `scrut == true`
            // becomes `scrut`, `scrut == false` becomes `!scrut`. The
            // generated machine code is identical either way, but the
            // rendered Rust is tidier.
            if let ConstVal::Bool(b) = c {
                if b {
                    return Some(Some(scrut.clone()));
                } else {
                    return Some(kir::not(scrut.clone()).map(Some)?);
                }
            }
            kir::cmp(scrut.clone(), kir::const_expr(c), kir::CmpOp::Eq).map(Some)
        }
        // `` `Tag(p0, p1, ...) `` — variant pattern. Requires the
        // scrutinee to be a Ref to a kernel's variant param (so we
        // can name it inside the tag-equality check); inline
        // variant values aren't supported in v0. Lowers to:
        //   - condition: `KirOp::VariantTagEq { name, expected_tag }`
        //   - bindings: each payload sub-pattern that's a simple
        //     `Bind(name)` adds a scalar Input to arm_ctx whose
        //     value lookup goes through `KirOp::VariantPayload`.
        //
        // Nested patterns inside payloads aren't supported in v0;
        // anything other than `Bind` / `Ignore` in a payload bails.
        StructurePattern::Variant { all: _, tag, binds } => {
            let var_name = match &scrut.op {
                KirOp::Local(n) => n.clone(),
                _ => return None,
            };
            // Clone the case shape so the immutable borrow of
            // arm_ctx is released before we mutate known_consts
            // below.
            // Today variant-pattern binding only handles primitive
            // payloads; composite payloads (variants containing
            // tuples / structs / nested variants) are a follow-up.
            // Extract a Vec<PrimType> or bail.
            let (var_name_owned, case_payloads): (ArcStr, Vec<PrimType>) = {
                let vi = arm_ctx.find_variant(&var_name)?;
                let case = vi.cases.iter().find(|(t, _)| t == tag)?;
                if case.1.len() != binds.len() {
                    return None;
                }
                let prims: Option<Vec<PrimType>> =
                    case.1.iter().map(KirType::as_prim).collect();
                (vi.name.clone(), prims?)
            };
            // For v0, payload bindings flow through the
            // `known_consts` channel: each named bind is mapped to a
            // synthetic KirExpr that reads
            // `KirOp::VariantPayload(name, idx)`. The arm body's
            // `Ref(bind_name)` lookup resolves to that expression.
            for (i, (payload_pat, payload_typ)) in
                binds.iter().zip(case_payloads.iter()).enumerate()
            {
                match payload_pat {
                    StructurePattern::Bind(bind_name) => {
                        let payload_expr = KirExpr {
                            op: KirOp::VariantPayload {
                                name: var_name_owned.clone(),
                                payload_idx: i,
                                elem_typ: *payload_typ,
                            },
                            typ: KirType::Prim(*payload_typ),
                        };
                        arm_ctx.known_consts.insert(
                            bind_name.clone(),
                            KnownConst { expr: payload_expr },
                        );
                    }
                    StructurePattern::Ignore => {}
                    _ => return None,
                }
            }
            Some(Some(KirExpr {
                op: KirOp::VariantTagEq {
                    name: var_name_owned,
                    expected_tag: tag.clone(),
                },
                typ: KirType::Prim(PrimType::Bool),
            }))
        }
        // Non-primitive patterns — not fusable yet.
        StructurePattern::Slice { .. }
        | StructurePattern::SlicePrefix { .. }
        | StructurePattern::SliceSuffix { .. }
        | StructurePattern::Tuple { .. }
        | StructurePattern::Struct { .. } => None,
    }
}

/// Emit a "tail" expression — either a pure expression that becomes
/// a `Return`, or a self-recursive tail call that becomes `TailCall`.
fn emit_tail(
    out: &mut Vec<KirStmt>,
    expr: &Expr,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<()> {
    if let Some(self_info) = self_info {
        if try_emit_tail_call(out, expr, ctx, self_info).is_some() {
            return Some(());
        }
    }
    if let ExprKind::Select(s) = &expr.kind {
        return emit_select(out, s, ctx, self_info);
    }
    if let ExprKind::Do { exprs } = &expr.kind {
        return emit_do(out, exprs, ctx, self_info);
    }
    if let ExprKind::ExplicitParens(inner) = &expr.kind {
        return emit_tail(out, inner, ctx, self_info);
    }
    let v = emit_expr(expr, ctx)?;
    out.push(KirStmt::Return(v));
    Some(())
}

/// Try to emit `expr` as a tail call to `self_info.name`. Returns
/// `None` if it isn't a self-call or any arg can't be fused.
fn try_emit_tail_call(
    out: &mut Vec<KirStmt>,
    expr: &Expr,
    ctx: &FusionCtx,
    self_info: &SelfInfo,
) -> Option<()> {
    let apply = match &expr.kind {
        ExprKind::Apply(a) => a,
        _ => return None,
    };
    let fn_ref = match &apply.function.kind {
        ExprKind::Ref { name } => name,
        _ => return None,
    };
    let fn_ident = ident_of(fn_ref)?;
    if fn_ident != self_info.name.as_str() {
        return None;
    }
    if apply.args.len() != self_info.source_args.len() {
        return None;
    }
    if apply.args.iter().any(|(label, _)| label.is_some()) {
        return None;
    }
    let mut args: Vec<KirExpr> = Vec::with_capacity(apply.args.len());
    for ((_, arg_expr), self_arg) in
        apply.args.iter().zip(&self_info.source_args)
    {
        let e = emit_expr(arg_expr, ctx)?;
        if e.typ != self_arg.typ {
            return None;
        }
        args.push(e);
    }
    out.push(KirStmt::TailCall { args });
    Some(())
}

/// Try to determine the primitive return type of a function body
/// without requiring an explicit `-> T` annotation. Walks the body
/// structurally, looking at the tail position:
/// - a Do block's type is its last expression's
/// - a Select's type is the first arm whose body we can type
/// - an Apply to a known-fused function gives its declared return
/// - self-recursive calls contribute nothing (the type we'd be
///   inferring *is* the rtype — circular), so they're skipped
/// - any other pure expression's type is what `emit_expr` reports
fn infer_body_rtype(
    body: &Expr,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<KirType> {
    // Self-recursion fast path: a call to the current fn would loop
    // forever if we tried to emit it. Check this *before* anything
    // else, since the type-cell path below would happily walk into
    // recursive callees.
    if let ExprKind::Apply(a) = &body.kind {
        if let ExprKind::Ref { name } = &a.function.kind {
            if let Some(ident) = ident_of(name) {
                if let Some(si) = self_info {
                    if si.name.as_str() == ident {
                        return None;
                    }
                }
                // Fast path: direct call to a known fused fn —
                // use its cached return type without re-emitting.
                if let Some(kf) = ctx.find_fn(ident) {
                    return Some(kf.return_type.clone());
                }
            }
        }
    }
    // Typed-AST fast path (Phase 0): the typechecker filled
    // `body.typ` for every real Expr. Translate to KirType
    // directly — no walk needed. Falls through to the emit path
    // only for synthesized Exprs whose `typ` cell is empty (e.g.
    // module-kernel's synth tail tuple) or for expressions whose
    // graphix type doesn't map cleanly to KirType but whose
    // sub-tree emission would still produce a valid KirType.
    if let Some(t) = body.typ.get() {
        if let Some(kt) = KirType::from_type(t) {
            return Some(kt);
        }
    }
    // Fallback: actually emit the expression and take the resulting
    // KIR's type. This is the only path that handles synthesized
    // Exprs and certain non-direct compositions correctly.
    emit_expr(body, ctx).map(|e| e.typ)
}

fn body_has_tail_call(expr: &Expr, name: &str) -> bool {
    match &expr.kind {
        ExprKind::Apply(a) => match &a.function.kind {
            ExprKind::Ref { name: modpath } => ident_of(modpath) == Some(name),
            _ => false,
        },
        ExprKind::Do { exprs } => exprs
            .last()
            .map(|e| body_has_tail_call(e, name))
            .unwrap_or(false),
        ExprKind::Select(s) => s.arms.iter().any(|(_, arm)| body_has_tail_call(arm, name)),
        ExprKind::ExplicitParens(inner) => body_has_tail_call(inner, name),
        _ => false,
    }
}

/// Walk an Expr collecting names that appear as the LHS of a
/// `Connect` (the `<-` operator). These are bindings that may be
/// updated at runtime; callers cross-fusing into one would silently
/// dispatch into the stale kernel after the rebind. The lazy fusion
/// path skips registering such bindings in `ctx.fusion_lambdas` so
/// callers fall back to `GXLambda` (until `KirOp::DynCall` lands as
/// the late-bound alternative). Top-level callers run this once on
/// the program AST and populate `ctx.unstable_bindings` before
/// invoking `compile`.
pub fn scan_connect_targets(expr: &Expr, out: &mut std::collections::BTreeSet<ArcStr>) {
    match &expr.kind {
        ExprKind::Connect { name, value, .. } => {
            if let Some(ident) = ident_of(name) {
                out.insert(ArcStr::from(ident));
            }
            scan_connect_targets(value, out);
        }
        ExprKind::Bind(b) => scan_connect_targets(&b.value, out),
        ExprKind::Do { exprs } => {
            for e in exprs.iter() {
                scan_connect_targets(e, out);
            }
        }
        ExprKind::Module { value, .. } => match value {
            crate::expr::ModuleKind::Resolved { exprs, .. } => {
                for e in exprs.iter() {
                    scan_connect_targets(e, out);
                }
            }
            crate::expr::ModuleKind::Dynamic { source, .. } => {
                scan_connect_targets(source, out);
            }
            crate::expr::ModuleKind::Unresolved { .. } => {}
        },
        ExprKind::Lambda(l) => {
            if let netidx::utils::Either::Left(body) = &l.body {
                scan_connect_targets(body, out);
            }
        }
        ExprKind::Apply(a) => {
            scan_connect_targets(&a.function, out);
            for (_, arg) in a.args.iter() {
                scan_connect_targets(arg, out);
            }
        }
        ExprKind::Select(s) => {
            scan_connect_targets(&s.arg, out);
            for (_, arm) in s.arms.iter() {
                scan_connect_targets(arm, out);
            }
        }
        ExprKind::ExplicitParens(e)
        | ExprKind::Qop(e)
        | ExprKind::OrNever(e)
        | ExprKind::ByRef(e)
        | ExprKind::Deref(e)
        | ExprKind::Not { expr: e }
        | ExprKind::TypeCast { expr: e, .. } => scan_connect_targets(e, out),
        ExprKind::Add { lhs, rhs }
        | ExprKind::Sub { lhs, rhs }
        | ExprKind::Mul { lhs, rhs }
        | ExprKind::Div { lhs, rhs }
        | ExprKind::Mod { lhs, rhs }
        | ExprKind::CheckedAdd { lhs, rhs }
        | ExprKind::CheckedSub { lhs, rhs }
        | ExprKind::CheckedMul { lhs, rhs }
        | ExprKind::CheckedDiv { lhs, rhs }
        | ExprKind::CheckedMod { lhs, rhs }
        | ExprKind::Eq { lhs, rhs }
        | ExprKind::Ne { lhs, rhs }
        | ExprKind::Lt { lhs, rhs }
        | ExprKind::Gt { lhs, rhs }
        | ExprKind::Lte { lhs, rhs }
        | ExprKind::Gte { lhs, rhs }
        | ExprKind::And { lhs, rhs }
        | ExprKind::Or { lhs, rhs }
        | ExprKind::Sample { lhs, rhs } => {
            scan_connect_targets(lhs, out);
            scan_connect_targets(rhs, out);
        }
        // Other variants either don't contain Connect nodes or
        // don't reach our `<-` shape. Conservative leaf — anything
        // we don't visit just doesn't contribute targets, which is
        // safe (unstable bindings undetected, conservative loss of
        // optimization, never wrong fusion).
        _ => {}
    }
}

/// Walk an Expr collecting `(callee_name, function_typ_cell)` pairs
/// for every `Apply { function: Ref { name } }` site. The cell is
/// `a.function.typ.clone()` — the typed-AST `Arc<OnceLock<Type>>`
/// on the function expression. `CallSite::typecheck` runs unification
/// against the actual arg expressions and the unified TVars flow
/// through this cell automatically (TVars are `Arc<RwLock>` so
/// in-place updates are visible to any holder of the surrounding
/// Type). The lazy fusion path uses this cell to look up the
/// call-site-specialised FnType and patch unannotated callee
/// argspecs.
pub fn discover_callee_names(
    expr: &Expr,
) -> Vec<(ArcStr, triomphe::Arc<std::sync::OnceLock<crate::typ::Type>>)> {
    let mut out = Vec::new();
    walk_for_callees(expr, &mut out);
    out
}

fn walk_for_callees(
    expr: &Expr,
    out: &mut Vec<(ArcStr, triomphe::Arc<std::sync::OnceLock<crate::typ::Type>>)>,
) {
    match &expr.kind {
        ExprKind::Apply(a) => {
            if let ExprKind::Ref { name } = &a.function.kind {
                if let Some(ident) = ident_of(name) {
                    // Yield the function expr's typ cell — this is
                    // where the call-site-resolved FnType lives (set
                    // by `CallSite::typecheck` → trait-default
                    // propagation). `lazy_resolve_kernel` uses it as
                    // its `apply_site_hint` to patch the lambda's
                    // argspec with unified TVars.
                    out.push((ArcStr::from(ident), a.function.typ.clone()));
                }
            }
            walk_for_callees(&a.function, out);
            for (_, arg) in a.args.iter() {
                walk_for_callees(arg, out);
            }
        }
        ExprKind::ExplicitParens(e)
        | ExprKind::Qop(e)
        | ExprKind::OrNever(e)
        | ExprKind::Not { expr: e } => walk_for_callees(e, out),
        ExprKind::Add { lhs, rhs }
        | ExprKind::Sub { lhs, rhs }
        | ExprKind::Mul { lhs, rhs }
        | ExprKind::Div { lhs, rhs }
        | ExprKind::Mod { lhs, rhs }
        | ExprKind::Eq { lhs, rhs }
        | ExprKind::Ne { lhs, rhs }
        | ExprKind::Lt { lhs, rhs }
        | ExprKind::Gt { lhs, rhs }
        | ExprKind::Lte { lhs, rhs }
        | ExprKind::Gte { lhs, rhs }
        | ExprKind::And { lhs, rhs }
        | ExprKind::Or { lhs, rhs } => {
            walk_for_callees(lhs, out);
            walk_for_callees(rhs, out);
        }
        ExprKind::Select(s) => {
            walk_for_callees(&s.arg, out);
            for (_, arm) in s.arms.iter() {
                walk_for_callees(arm, out);
            }
        }
        ExprKind::Do { exprs } => {
            for e in exprs.iter() {
                walk_for_callees(e, out);
            }
        }
        ExprKind::Bind(b) => walk_for_callees(&b.value, out),
        ExprKind::TypeCast { expr, .. } => walk_for_callees(expr, out),
        // Other variants either can't appear in fusable bodies or
        // don't reach Apply call sites we care about. If they ever
        // do, fusion will just fail to find the call rather than
        // succeed with stale info.
        _ => {}
    }
}

/// Compute intrinsic sync/async effects for every let-bound lambda
/// registered in `ctx.fusion_lambdas`. Walks each lambda's body looking
/// for async edges:
///
/// - **Async-effect builtin call**: `Apply { fn: Ref(name) }` where
///   `name` resolves to a builtin with `EFFECT = Async` (looked up via
///   `ctx.builtin_effects`).
/// - **Async user-fn call**: `Apply { fn: Ref(name) }` where `name`
///   resolves to a let-bound lambda whose own effect is currently
///   `Async`.
///
/// Function-typed parameter calls (HOFs) do NOT contribute here — they
/// are handled at the call site via the lattice join with the resolved
/// fn-arg's effect (M6c). Reads of unstable bindings produce async
/// values but don't make the function intrinsically async (see
/// `design/whole_graph_fusion.md` — a closure that reads `counter` and
/// schedules `counter <- counter + 1` is intrinsically Sync).
///
/// Iterates to fixed point. Effects only flow Sync → Async, never the
/// reverse, so termination is guaranteed in O(N) passes for N lambdas.
/// Should be called after compilation finishes, when `fusion_lambdas`
/// is fully populated.
pub fn infer_effects<R: Rt, E: UserEvent>(ctx: &ExecCtx<R, E>) {
    // Initialize all to Sync (already the case via Bind::compile, but
    // defensively reset in case this is being re-run on the same ctx).
    for entry in ctx.fusion_lambdas.values() {
        *entry.effect.lock() = EffectKind::Sync;
    }
    let mut changed = true;
    while changed {
        changed = false;
        for entry in ctx.fusion_lambdas.values() {
            if matches!(*entry.effect.lock(), EffectKind::Async) {
                continue;
            }
            let body = match &entry.lambda.body {
                netidx::utils::Either::Left(body) => body,
                // Builtin-shim lambdas (`let X = builtin "foo"`): their
                // effect is the builtin's effect. Already covered when
                // any caller invokes them via Apply{Ref(builtin_name)};
                // their own intrinsic stays Sync because they have no
                // body of their own. Skip.
                netidx::utils::Either::Right(_) => continue,
            };
            let mut effect = EffectKind::Sync;
            walk_for_effect(body, ctx, &mut effect);
            if matches!(effect, EffectKind::Async) {
                *entry.effect.lock() = EffectKind::Async;
                changed = true;
            }
        }
    }
}

fn walk_for_effect<R: Rt, E: UserEvent>(
    expr: &Expr,
    ctx: &ExecCtx<R, E>,
    out: &mut EffectKind,
) {
    if matches!(*out, EffectKind::Async) {
        return;
    }
    match &expr.kind {
        ExprKind::Apply(a) => {
            if let ExprKind::Ref { name } = &a.function.kind {
                if let Some(ident) = ident_of(name) {
                    let key: ArcStr = ArcStr::from(ident);
                    if let Some(eff) = ctx.builtin_effects.get(&*key) {
                        *out = out.join(*eff);
                    } else if let Some(callee) = ctx.fusion_lambdas.get(&key) {
                        *out = out.join(*callee.effect.lock());
                    }
                }
            }
            walk_for_effect(&a.function, ctx, out);
            for (_, arg) in a.args.iter() {
                walk_for_effect(arg, ctx, out);
            }
        }
        ExprKind::Bind(b) => walk_for_effect(&b.value, ctx, out),
        ExprKind::Connect { value, .. } => walk_for_effect(value, ctx, out),
        ExprKind::Do { exprs } => {
            for e in exprs.iter() {
                walk_for_effect(e, ctx, out);
            }
        }
        ExprKind::Module { value, .. } => match value {
            crate::expr::ModuleKind::Resolved { exprs, .. } => {
                for e in exprs.iter() {
                    walk_for_effect(e, ctx, out);
                }
            }
            crate::expr::ModuleKind::Dynamic { source, .. } => {
                walk_for_effect(source, ctx, out);
            }
            crate::expr::ModuleKind::Unresolved { .. } => {}
        },
        ExprKind::Lambda(_) => {
            // Nested lambdas have their own intrinsic effect — they
            // don't contribute to the outer lambda's effect just by
            // being constructed as a value. (If the outer lambda
            // calls them, that's a separate Apply that we already
            // handle.)
        }
        ExprKind::Select(s) => {
            walk_for_effect(&s.arg, ctx, out);
            for (_, arm) in s.arms.iter() {
                walk_for_effect(arm, ctx, out);
            }
        }
        ExprKind::TryCatch(tc) => {
            for e in tc.exprs.iter() {
                walk_for_effect(e, ctx, out);
            }
            walk_for_effect(&tc.handler, ctx, out);
        }
        ExprKind::ExplicitParens(e)
        | ExprKind::Qop(e)
        | ExprKind::OrNever(e)
        | ExprKind::ByRef(e)
        | ExprKind::Deref(e)
        | ExprKind::Not { expr: e }
        | ExprKind::TypeCast { expr: e, .. } => walk_for_effect(e, ctx, out),
        ExprKind::StringInterpolate { args }
        | ExprKind::Any { args }
        | ExprKind::Array { args }
        | ExprKind::Tuple { args }
        | ExprKind::Variant { args, .. } => {
            for e in args.iter() {
                walk_for_effect(e, ctx, out);
            }
        }
        ExprKind::Map { args } => {
            for (k, v) in args.iter() {
                walk_for_effect(k, ctx, out);
                walk_for_effect(v, ctx, out);
            }
        }
        ExprKind::Struct(s) => {
            for (_, e) in s.args.iter() {
                walk_for_effect(e, ctx, out);
            }
        }
        ExprKind::StructWith(sw) => {
            walk_for_effect(&sw.source, ctx, out);
            for (_, e) in sw.replace.iter() {
                walk_for_effect(e, ctx, out);
            }
        }
        ExprKind::StructRef { source, .. } | ExprKind::TupleRef { source, .. } => {
            walk_for_effect(source, ctx, out);
        }
        ExprKind::ArrayRef { source, i } => {
            walk_for_effect(source, ctx, out);
            walk_for_effect(i, ctx, out);
        }
        ExprKind::ArraySlice { source, start, end } => {
            walk_for_effect(source, ctx, out);
            if let Some(s) = start {
                walk_for_effect(s, ctx, out);
            }
            if let Some(e) = end {
                walk_for_effect(e, ctx, out);
            }
        }
        ExprKind::MapRef { source, key } => {
            walk_for_effect(source, ctx, out);
            walk_for_effect(key, ctx, out);
        }
        ExprKind::Add { lhs, rhs }
        | ExprKind::Sub { lhs, rhs }
        | ExprKind::Mul { lhs, rhs }
        | ExprKind::Div { lhs, rhs }
        | ExprKind::Mod { lhs, rhs }
        | ExprKind::CheckedAdd { lhs, rhs }
        | ExprKind::CheckedSub { lhs, rhs }
        | ExprKind::CheckedMul { lhs, rhs }
        | ExprKind::CheckedDiv { lhs, rhs }
        | ExprKind::CheckedMod { lhs, rhs }
        | ExprKind::Eq { lhs, rhs }
        | ExprKind::Ne { lhs, rhs }
        | ExprKind::Lt { lhs, rhs }
        | ExprKind::Gt { lhs, rhs }
        | ExprKind::Lte { lhs, rhs }
        | ExprKind::Gte { lhs, rhs }
        | ExprKind::And { lhs, rhs }
        | ExprKind::Or { lhs, rhs }
        | ExprKind::Sample { lhs, rhs } => {
            walk_for_effect(lhs, ctx, out);
            walk_for_effect(rhs, ctx, out);
        }
        // Leaves with no sub-expressions to recurse into. Listed
        // exhaustively so a new variant is a compile error here, not
        // a silent miss in effect inference.
        ExprKind::NoOp
        | ExprKind::Constant(_)
        | ExprKind::Use { .. }
        | ExprKind::Ref { .. }
        | ExprKind::TypeDef(_) => {}
    }
}

/// Effect of a value expression that's about to be passed as a fn-typed
/// argument to a HOF call site. Inline `Lambda` evaluates its body's
/// effect; `Ref(name)` looks up the binding in `fusion_lambdas` /
/// `builtin_effects`. Any other shape (a fn-valued expression we can't
/// statically resolve) is conservatively `Async` — see the design doc
/// "Function values from unstable bindings" section.
fn fn_arg_effect<R: Rt, E: UserEvent>(arg: &Expr, ctx: &ExecCtx<R, E>) -> EffectKind {
    match &arg.kind {
        ExprKind::Lambda(l) => match &l.body {
            netidx::utils::Either::Left(body) => {
                let mut eff = EffectKind::Sync;
                walk_for_effect(body, ctx, &mut eff);
                eff
            }
            // Builtin-shim lambda: defer to the builtin's classification.
            netidx::utils::Either::Right(name) => ctx
                .builtin_effects
                .get(name.as_str())
                .copied()
                .unwrap_or(EffectKind::Async),
        },
        ExprKind::Ref { name } => {
            if let Some(ident) = ident_of(name) {
                if let Some(eff) = ctx.builtin_effects.get(ident) {
                    return *eff;
                }
                if let Some(entry) = ctx.fusion_lambdas.get(&ArcStr::from(ident)) {
                    return *entry.effect.lock();
                }
            }
            // Unknown ref — could be a parameter from the enclosing
            // scope or a Bind we haven't seen. Conservative.
            EffectKind::Async
        }
        // Anything else evaluating to a fn value is opaque to static
        // analysis (e.g. `select`-chosen lambda, fn returned from a
        // call). Conservative.
        _ => EffectKind::Async,
    }
}

/// Compute the call-site effect of an `Apply` per the lattice rule from
/// `design/whole_graph_fusion.md`:
///
/// ```text
/// callsite_effect = callee_intrinsic ⊔ ⨆(fn_arg_effects at this site)
/// ```
///
/// `callee_intrinsic` comes from `builtin_effects` for builtin shims or
/// `fusion_lambdas` for user lambdas, falling back to `Async` for an
/// unresolvable reference. `fn_arg_effects` is the join over arguments
/// whose **resolved** type at this call site is `Type::Fn`. The resolved
/// type is read from `apply.function.typ` (the typed-AST cell on the
/// function expression, set by `CallSite::typecheck` → trait-default
/// propagation); falls back to a syntactic check when the cell isn't
/// populated yet.
///
/// Used by M8 (whole-graph fusion analyzer) at every Apply boundary to
/// decide whether the call site can be absorbed into the surrounding
/// sync kernel.
pub fn apply_site_effect<R: Rt, E: UserEvent>(
    apply: &crate::expr::ApplyExpr,
    _apply_id: crate::expr::ExprId,
    ctx: &ExecCtx<R, E>,
) -> EffectKind {
    let mut eff = match &apply.function.kind {
        ExprKind::Ref { name } => match ident_of(name) {
            Some(ident) => ctx
                .builtin_effects
                .get(ident)
                .copied()
                .or_else(|| {
                    ctx.fusion_lambdas
                        .get(&ArcStr::from(ident))
                        .map(|e| *e.effect.lock())
                })
                .unwrap_or(EffectKind::Async),
            None => EffectKind::Async,
        },
        // Computed callee (closure result, select arm, etc.) — opaque.
        _ => EffectKind::Async,
    };
    if matches!(eff, EffectKind::Async) {
        return eff;
    }
    // Identify fn-typed positions from the resolved FnType so we agree
    // with the typechecker about which args carry functions. Read it
    // off the function expression's typed-AST cell. Falls back to a
    // syntactic check when the cell isn't populated yet (e.g. mid-
    // typecheck or for synthesized exprs).
    let resolved = resolved_fn_type(&apply.function);
    for (i, (_, arg)) in apply.args.iter().enumerate() {
        let is_fn_typed = match &resolved {
            Some(ft) => matches!(
                ft.args.get(i).map(|a| &a.typ),
                Some(crate::typ::Type::Fn(_))
            ),
            // No resolved type: be conservative — only treat the arg as
            // fn-typed if it syntactically looks like one (Lambda or
            // Ref to a known fn). Otherwise we'd over-classify args
            // like literal i64s as Async.
            None => matches!(
                arg.kind,
                ExprKind::Lambda(_) | ExprKind::Ref { .. }
            ),
        };
        if is_fn_typed {
            eff = eff.join(fn_arg_effect(arg, ctx));
            if matches!(eff, EffectKind::Async) {
                return eff;
            }
        }
    }
    eff
}

// ─── Whole-graph fusion analyzer (M8.4) ────────────────────────────
//
// `analyze_program` carves each top-level expression into maximal
// *sync subgraphs* — connected regions of the dataflow graph
// containing no async edges. Each region becomes one KIR kernel
// (built in M8.4 step d); the runtime splices it in as a single node
// (M8.4 step e), replacing the chain of individual graph nodes it
// covers.
//
// An async edge — the boundary between regions — is any of:
// - an async-effect builtin call (timer, subscribe, IO, queue, …)
// - a call to an intrinsically-async user lambda
// - a read of an unstable binding (a `<-` write target): its value
//   depends on a future-cycle write, so reading it can't be fused
//   into the same kernel as its consumer.
//
// `program_effect_map` (phase A) classifies every `ExprId` Sync or
// Async by joining the intrinsic edge effect of the node with the
// effects of its same-scope children. Phase B (`carve_into`)
// extracts every maximal joined-Sync subtree: a node roots a region
// if it is joined-Sync and its parent is joined-Async (or it is a
// top-level expression). Async nodes are not absorbed into any
// region — `carve_into` recurses into their children hunting for
// sync sub-regions among them.
//
// M8.4 — initial model: a region is a fully-sync subtree. Async
// edges are only allowed as ancestors of a region, never inside or
// as direct children. The follow-up promotes async-edge direct
// children to kernel inputs, fusing sync ops that *consume* an async
// value into the same kernel as the rest of their sync subtree.
//
// Top-level only: nested lambda bodies are never split at an
// interior async edge. A lambda whose body contains an async edge
// has its `intrinsic_effect` already marked Async by `infer_effects`,
// so every call site of it is an async edge from the caller's POV —
// the lambda body stays its own (non-fused) thing.

/// One maximal sync region identified by [`analyze_program`] — the
/// unit of fusion. Every region is Sync by construction (it's the
/// connected joined-sync subtree between async edges).
#[derive(Debug, Clone)]
pub struct FusedSubgraph {
    /// `ExprId` of the region's root — the expression whose value
    /// the region (and its eventual kernel) produces. The runtime
    /// splice step (M8.4 step e) replaces this expression's compiled
    /// node with the kernel node.
    pub root_id: crate::expr::ExprId,
    /// Every `ExprId` absorbed into this region (including
    /// `root_id`). The splice step uses this to know which graph
    /// nodes the fused kernel subsumes.
    pub members: Box<[crate::expr::ExprId]>,
    /// Number of `Apply` sites among `members`. Rough "worth
    /// fusing?" proxy — a region with 0 applies is a pure value
    /// expression that doesn't benefit from a kernel.
    pub apply_count: usize,
    /// Free-variable kernel inputs — every binding the region body
    /// reads that isn't introduced by an in-region `let`. Each
    /// input's type comes from `ctx.env` at analyzer time. Empty
    /// when the region failed to discover inputs or has none.
    pub inputs: Vec<RegionInput>,
    /// The built kernel, if `analyze_program` successfully lowered
    /// this region's body via [`build_kir_kernel_from_region`].
    /// `None` means the region stays as the existing node-graph
    /// representation — emit_expr bailed (unsupported construct,
    /// unresolvable callee, unrepresentable input type, etc.). The
    /// splice step never replaces a region whose kernel is `None`.
    pub kernel: Option<SArc<KirKernel>>,
    /// `KnownFusedFn` signature of `kernel`. `Some` iff
    /// `kernel.is_some()`.
    pub signature: Option<KnownFusedFn>,
    /// `KernelRegistry` for this region's `KirOp::Call` callees,
    /// built from the transitive closure of `collect_call_sites`
    /// over `kernel`. The runtime `KirNode` dispatches Call sites
    /// through this map. Empty when `kernel.is_none()` or when the
    /// region's kernel contains no cross-kernel calls.
    pub registry: SArc<crate::kir_interp::KernelRegistry>,
}

/// One kernel input for a region — a value flowing from outside the
/// region into its kernel. In the M8.4 initial model (where regions
/// are fully-sync subtrees) every input is a free-variable `Ref` to a
/// binding defined outside the region; the runtime wires each one to
/// the node that produces the value. M8.4 step (d) discovers and
/// populates inputs as part of the per-region kernel build; the
/// initial-model follow-up adds async-edge children as inputs too.
#[derive(Debug, Clone)]
pub struct RegionInput {
    /// `ExprId` of the cross-edge sub-expression whose value feeds
    /// this slot (e.g. the `Ref` site in the region body, or — in
    /// the follow-up model — the async-edge child node). The
    /// runtime splice step uses this to wire the input.
    pub expr_id: crate::expr::ExprId,
    /// The name the kernel body uses to refer to this slot. For a
    /// free-variable input it is the binding's own name, so the
    /// existing `Ref` lowering in `emit_expr` resolves to the slot
    /// without any rewriting of the region body.
    pub name: ArcStr,
    /// Slot kind — drives which [`FusionCtx`] input list the slot
    /// lands in and the kernel param's [`KirType`].
    pub kind: RegionInputKind,
}

/// Per-input slot classification. Mirrors the param shapes
/// [`build_kir_kernel`] derives from a `LambdaExpr`'s argspec —
/// scalar primitive, array of primitive, tuple/struct/variant of
/// primitives. Function-typed inputs are not supported in the M8.4
/// initial model (a region has no HOF params; HOF callees come in
/// through the `known` map as `KirOp::Call` targets).
#[derive(Debug, Clone)]
pub enum RegionInputKind {
    Prim(PrimType),
    Array(KirType),
    Tuple(Vec<KirType>),
    Struct(Vec<(ArcStr, KirType)>),
    Variant(Vec<(ArcStr, Vec<KirType>)>),
}

/// Carve every top-level expression into maximal sync subgraphs and
/// build a [`KirKernel`] for each one. Returns one [`FusedSubgraph`]
/// per region — a single top-level expression may yield several
/// (sync islands separated by interior async edges) or none (if it's
/// entirely async at every level).
///
/// `ctx` is taken `&mut` because the eager program-kernel-table pass
/// drives every fusable let-bound lambda through
/// [`lazy_resolve_kernel`], which writes into each entry's cache. M8.4
/// step (g) replaces this reuse with a `analyze_program`-owned
/// kernel-table builder + the `lazy_resolve_kernel` deletion; for now
/// the reuse keeps the kernel-build code paths converged and the
/// step (d) diff focused on the new region path.
pub fn analyze_program<R: Rt, E: UserEvent>(
    exprs: &[Expr],
    ctx: &mut ExecCtx<R, E>,
) -> Vec<FusedSubgraph> {
    // Eager program kernel table — try to resolve every let-bound
    // lambda once, accumulate both the signature (for cross-kernel
    // call lowering) and the `Arc<KirKernel>` (for the runtime's
    // `KernelRegistry`). A region whose body calls `f` finds `f`'s
    // signature here and lowers the call as `KirOp::Call`; the
    // runtime `KirNode` then dispatches via the per-region registry
    // built from this same table. Lambdas that don't resolve just
    // won't appear here, and any region calling them will fail to
    // fuse — conservative, no regression vs. the lazy path.
    let names: Vec<ArcStr> = ctx.fusion_lambdas.keys().cloned().collect();
    let mut known: std::collections::BTreeMap<ArcStr, KnownFusedFn> =
        std::collections::BTreeMap::new();
    let mut program_kernels: std::collections::BTreeMap<
        ArcStr,
        SArc<KirKernel>,
    > = std::collections::BTreeMap::new();
    for name in &names {
        if let Some((sig, kernel)) = lazy_resolve_kernel(ctx, name, None) {
            known.insert(name.clone(), sig);
            program_kernels.insert(name.clone(), kernel);
        }
    }
    let consts = ctx.fusion_known_consts.clone();
    // Program-root scope: `discover_region_inputs` looks free-var
    // Refs up here via `env.lookup_bind`. The resolver has already
    // qualified each Ref's module path, so the env's `find_visible`
    // walks back from this scope to the actual binding.
    let scope = ModPath::root();

    let mut regions = Vec::new();
    for e in exprs {
        let mut effects: nohash::IntMap<crate::expr::ExprId, EffectKind> =
            nohash::IntMap::default();
        program_effect_map(e, ctx, &mut effects);
        carve_and_build(
            e,
            &effects,
            &known,
            &consts,
            &scope,
            ctx,
            &program_kernels,
            &mut regions,
        );
    }
    regions
}

/// Phase B + per-region kernel build, fused into one recursive
/// descent so we have the region's `&Expr` in hand at build time
/// (versus carrying ExprIds across passes and re-finding the
/// `&Expr` by id). If `expr` is joined-Sync it roots a maximal sync
/// region: collect members, discover inputs, build the kernel. If
/// joined-Async, recurse into children hunting for sync sub-regions.
#[allow(clippy::too_many_arguments)]
fn carve_and_build<R: Rt, E: UserEvent>(
    expr: &Expr,
    effects: &nohash::IntMap<crate::expr::ExprId, EffectKind>,
    known: &std::collections::BTreeMap<ArcStr, KnownFusedFn>,
    consts: &std::collections::BTreeMap<ArcStr, KnownConst>,
    scope: &ModPath,
    exec_ctx: &ExecCtx<R, E>,
    program_kernels: &std::collections::BTreeMap<ArcStr, SArc<KirKernel>>,
    out: &mut Vec<FusedSubgraph>,
) {
    // Default to Async for a missing entry — see `carve_into`
    // history; defaulting conservatively means a stray miss can
    // never spuriously fuse an unclassified node.
    let eff = effects.get(&expr.id).copied().unwrap_or(EffectKind::Async);
    match eff {
        EffectKind::Sync => {
            let mut members = Vec::new();
            let mut apply_count = 0usize;
            collect_region(expr, &mut members, &mut apply_count);
            // Try to build the kernel. Any failure (input type
            // unresolvable, region body has unsupported constructs,
            // a callee not in `known`, etc.) leaves
            // `kernel: None` + `signature: None`; the runtime
            // splice step keeps such a region as the existing node
            // graph. Conservative — never a regression vs. lazy.
            let kernel_name = format!("region_{}", out.len());
            let (inputs, kernel, signature) = build_region_kernel(
                &kernel_name, expr, scope, exec_ctx, known, consts,
            );
            // Assemble the per-region `KernelRegistry` from the
            // program kernel table — the transitive closure of
            // `collect_call_sites` over this region's kernel. The
            // runtime `KirNode` looks each `KirOp::Call` target up
            // here. Empty when the region didn't build a kernel.
            let registry =
                build_registry(kernel.as_ref(), program_kernels);
            out.push(FusedSubgraph {
                root_id: expr.id,
                members: members.into_boxed_slice(),
                apply_count,
                inputs,
                kernel,
                signature,
                registry,
            });
        }
        EffectKind::Async => {
            for_each_child(expr, &mut |c| {
                carve_and_build(
                    c, effects, known, consts, scope, exec_ctx,
                    program_kernels, out,
                );
            });
        }
    }
}

/// Discover the region's inputs, try to build its kernel. Returns
/// `(inputs, kernel, signature)` — the kernel/signature pair is
/// `Some` only if every step succeeded; `inputs` is populated even
/// on build failure (the splice step doesn't use it then, but it's
/// useful for diagnostics / future incremental fusion).
fn build_region_kernel<R: Rt, E: UserEvent>(
    name: &str,
    region: &Expr,
    scope: &ModPath,
    exec_ctx: &ExecCtx<R, E>,
    known: &std::collections::BTreeMap<ArcStr, KnownFusedFn>,
    consts: &std::collections::BTreeMap<ArcStr, KnownConst>,
) -> (Vec<RegionInput>, Option<SArc<KirKernel>>, Option<KnownFusedFn>) {
    let Some(inputs) = discover_region_inputs(region, scope, &exec_ctx.env)
    else {
        return (Vec::new(), None, None);
    };
    let extras = discover_builtin_fn_inputs(region, exec_ctx);
    match build_kir_kernel_from_region(
        name, region, &inputs, &extras, None, known, consts,
    ) {
        Some((kernel, sig)) => (inputs, Some(SArc::new(kernel)), Some(sig)),
        None => (inputs, None, None),
    }
}

/// Build the per-region [`KernelRegistry`] from the program kernel
/// table. Walks the closure of `KirOp::Call` callees reachable from
/// `kernel` and packs each one's `Arc<KirKernel>` into the registry,
/// so the runtime `KirNode` can dispatch every cross-kernel call.
/// Returns an empty registry when `kernel` is `None`.
fn build_registry(
    kernel: Option<&SArc<KirKernel>>,
    program_kernels: &std::collections::BTreeMap<ArcStr, SArc<KirKernel>>,
) -> SArc<crate::kir_interp::KernelRegistry> {
    let Some(k) = kernel else {
        return SArc::new(crate::kir_interp::KernelRegistry::default());
    };
    let mut needed: std::collections::BTreeSet<ArcStr> =
        std::collections::BTreeSet::new();
    let mut frontier: Vec<ArcStr> =
        crate::kernel_ir::collect_call_sites(k).into_iter().collect();
    while let Some(name) = frontier.pop() {
        if !needed.insert(name.clone()) {
            continue;
        }
        if let Some(callee) = program_kernels.get(&name) {
            for c in crate::kernel_ir::collect_call_sites(callee) {
                if !needed.contains(&c) {
                    frontier.push(c);
                }
            }
        }
    }
    let mut kernels = std::collections::BTreeMap::new();
    for name in needed {
        if let Some(callee) = program_kernels.get(&name) {
            kernels.insert(name, callee.clone());
        }
    }
    SArc::new(crate::kir_interp::KernelRegistry { kernels })
}

/// One in-region top-level `Bind` that the module kernel publishes
/// to the runtime variable system. The kernel's return value is a
/// `Tuple([value_of_export_0, value_of_export_1, …])`; the runtime
/// [`crate::node::region::FusedRegion`] unpacks each slot and calls
/// `ctx.set_var(bind_id, value)` so other modules / non-fused code
/// see the binding through the normal pub/sub variable machinery.
#[derive(Debug, Clone)]
pub struct ModuleExport {
    pub bind_id: crate::BindId,
    pub name: ArcStr,
}

/// The output of [`build_module_kernel`]: a kernel that computes
/// every sync top-level `Bind`'s value in one pass, with the runtime
/// wiring metadata needed to splice it in.
pub struct ModuleKernel {
    pub kernel: SArc<KirKernel>,
    pub signature: KnownFusedFn,
    pub registry: SArc<crate::kir_interp::KernelRegistry>,
    pub inputs: Vec<RegionInput>,
    /// In tuple-slot order — the i-th slot of the kernel's tuple
    /// return value publishes to `exports[i].bind_id`.
    pub exports: Vec<ModuleExport>,
    /// `ExprId`s of the top-level expressions the kernel subsumes
    /// — every `Bind` whose value lives in the kernel, plus every
    /// sync-builtin Apply that runs as a discard side-effect.
    /// The splice step in `gx.rs` deletes the compiled nodes for
    /// these ids before inserting the `FusedRegion`.
    pub subsumed_top_ids: Vec<crate::expr::ExprId>,
}

/// Build a single kernel that computes every sync top-level `Bind`'s
/// value from `exprs` as one fused pass. Returns `None` if the batch
/// can't fuse — any non-`Bind` statement (a side-effecting call, an
/// `<-` assignment, an async builtin call, etc.) currently bails,
/// since `emit_do` only accepts `Bind` / `NoOp` for non-tail
/// positions. Conservative.
///
/// The kernel's return type is `KirType::Tuple([…])` with one slot
/// per top-level `Bind` whose type is fusion-representable; the
/// runtime publishes each slot to its corresponding `BindId`.
/// Lambda-typed binds are skipped (function values aren't tuple-
/// representable in KIR) — those continue to flow through
/// `ctx.fusion_lambdas` for the per-kernel-call mechanism.
///
/// Expects `ctx.env` to already have every `Bind`'s `BindId`
/// assigned — i.e. call this *after* `compile` has run on each
/// top-level expr. (The kernel build doesn't depend on the compiled
/// `Node`s, but it does need the resolved BindIds to thread through
/// to the splice.)
pub fn build_module_kernel<R: Rt, E: UserEvent>(
    exprs: &[Expr],
    scope: &Scope,
    ctx: &mut ExecCtx<R, E>,
    fn_name: &str,
) -> Option<ModuleKernel> {
    use crate::expr::ModPath;
    // Step 1 — collect every top-level Bind's (name, BindId, KirType).
    // Skip binds whose type isn't fusion-representable (functions,
    // user-defined types, …); their compiled node stays and runs
    // outside the fused kernel. Capture each export's KirType so the
    // kernel return type can be computed up front (Step 4 below) —
    // `infer_body_rtype` can't deduce it through the synthetic Do's
    // let bindings on its own (it doesn't extend ctx as it walks).
    let mut exports: Vec<ModuleExport> = Vec::new();
    let mut export_prims: Vec<PrimType> = Vec::new();
    for e in exprs.iter() {
        let ExprKind::Bind(b) = &e.kind else { continue };
        if b.rec {
            // `let rec` binds can be self-referential and currently
            // need the per-lambda lazy path; skip.
            continue;
        }
        let Some(name) = b.pattern.single_bind() else { continue };
        // Skip lambda-typed binds — function values can't go in a
        // tuple slot at the KIR level. Their resolution still goes
        // through `ctx.fusion_lambdas`.
        if matches!(b.value.kind, ExprKind::Lambda(_)) {
            continue;
        }
        let path = ModPath::from_iter([name.as_str()]);
        let Some((_, bind)) = ctx.env.lookup_bind(&scope.lexical, &path) else {
            return None;
        };
        // Skip exports whose type isn't a primitive — KirType::Tuple
        // can only hold `PrimType` elements, so a tuple-return kernel
        // can't carry composite/variant/string exports. Skip rather
        // than bail; the unfused export keeps its original `Bind`
        // node and the kernel just doesn't publish to its BindId.
        let Some(prim) = PrimType::from_type(&bind.typ) else {
            continue;
        };
        exports.push(ModuleExport {
            bind_id: bind.id,
            name: name.clone(),
        });
        export_prims.push(prim);
    }
    if exports.is_empty() {
        // No fusable top-level binds — nothing to fuse here.
        return None;
    }
    // The kernel returns a tuple of every export's value, so its
    // return type is precisely `KirType::Tuple(export_prims)`.
    // Compute it up front and pass it to
    // `build_kir_kernel_from_region` so `infer_body_rtype` doesn't
    // get a chance to bail on the synthetic Do (its Do arm just walks
    // the last expr — the synthetic tail Tuple of
    // `Ref(let-bound-name)` — without populating ctx from the lets,
    // so a non-constant export value would otherwise cause the synth
    // tail's emit_expr to fail to resolve the ref).
    let export_kts: Vec<KirType> =
        export_prims.into_iter().map(KirType::Prim).collect();
    let return_type = KirType::Tuple(export_kts);
    // Step 2 — synthesize a `Do` whose tail is a `Tuple` of `Ref`s
    // to every export's name, so `emit_do` + `emit_tail` produce a
    // `KirStmt::Return(TupleNew(…))` over the exported locals.
    //
    // Filter the original exprs: keep `Bind`s (they define exports)
    // and `Apply { Ref(name) }` where `name` is a sync builtin (they
    // lower as discard-let side effects via the M8.4-step-f
    // emit_do extension). Skip everything else (multi-segment
    // calls like `sys::exit`, `Use`/`TypeDef`/`Connect`, async
    // builtins) — those stay as their own top-level nodes. Skipping
    // is sound because the kernel only computes what `do_exprs`
    // covers; the skipped exprs' original compiled nodes still fire
    // and publish to their own BindIds in parallel.
    let mut do_exprs: Vec<Expr> = Vec::with_capacity(exprs.len() + 1);
    let mut subsumed_top_ids: Vec<crate::expr::ExprId> = Vec::new();
    for e in exprs.iter() {
        let mut include = false;
        match &e.kind {
            ExprKind::Bind(b) => {
                // Match the filter `step 1` used to populate
                // `exports` — anything that isn't an export-worthy
                // Bind isn't useful inside the synth Do either, so
                // skip rec/lambda binds to keep the kernel's
                // semantics aligned with `exports`.
                include = !b.rec
                    && b.pattern.single_bind().is_some()
                    && !matches!(b.value.kind, ExprKind::Lambda(_));
            }
            ExprKind::NoOp => {
                include = true;
            }
            ExprKind::Apply(a) => {
                if let ExprKind::Ref { name } = &a.function.kind {
                    if let Some(ident) = ident_of(name) {
                        let user_name = ArcStr::from(ident);
                        if !ctx.unstable_bindings.contains(&user_name) {
                            if let Some(entry) =
                                ctx.fusion_lambdas.get(&user_name)
                            {
                                if let netidx::utils::Either::Right(shim) =
                                    &entry.lambda.body
                                {
                                    if ctx.builtins.contains_key(shim.as_str())
                                        && ctx.builtin_effect(shim.as_str())
                                            == EffectKind::Sync
                                    {
                                        include = true;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        if include {
            do_exprs.push(e.clone());
            subsumed_top_ids.push(e.id);
        }
    }
    let tuple_args: Vec<Expr> = exports
        .iter()
        .map(|exp| {
            let path = ModPath::from_iter([exp.name.as_str()]);
            ExprKind::Ref { name: path }.to_expr_nopos()
        })
        .collect();
    let tail = ExprKind::Tuple {
        args: triomphe::Arc::from_iter(tuple_args),
    }
    .to_expr_nopos();
    do_exprs.push(tail);
    let synth_do = ExprKind::Do {
        exprs: triomphe::Arc::from_iter(do_exprs),
    }
    .to_expr_nopos();
    // Step 3 — eagerly build the program kernel table (mirrors what
    // `analyze_program` does) so cross-kernel calls inside any
    // export's value lower via the `known` map.
    let names: Vec<ArcStr> =
        ctx.fusion_lambdas.keys().cloned().collect();
    let mut known: std::collections::BTreeMap<ArcStr, KnownFusedFn> =
        std::collections::BTreeMap::new();
    let mut program_kernels: std::collections::BTreeMap<
        ArcStr,
        SArc<KirKernel>,
    > = std::collections::BTreeMap::new();
    for name in &names {
        if let Some((sig, kernel)) = lazy_resolve_kernel(ctx, name, None) {
            known.insert(name.clone(), sig);
            program_kernels.insert(name.clone(), kernel);
        }
    }
    let consts = ctx.fusion_known_consts.clone();
    // Step 4 — discover free-variable inputs of the synthetic Do.
    // Reuses `discover_region_inputs`, which already does the
    // sequential-let scope tracking; in-Do let-introduced names
    // aren't counted as free vars.
    let inputs = discover_region_inputs(&synth_do, &scope.lexical, &ctx.env)?;
    // Discover sync-builtin call sites in the synthetic body — each
    // becomes an `extra_fn_input` so `emit_apply_expr` lowers the
    // call as `KirOp::DynCall` rather than bailing.
    let extras = discover_builtin_fn_inputs(&synth_do, ctx);
    // Step 5 — build the kernel. Return-type inference falls out of
    // the synthetic tuple tail (`infer_body_rtype` walks the Do down
    // to the Tuple).
    let (kernel, signature) = build_kir_kernel_from_region(
        fn_name,
        &synth_do,
        &inputs,
        &extras,
        Some(return_type),
        &known,
        &consts,
    )?;
    let kernel = SArc::new(kernel);
    let registry = build_registry(Some(&kernel), &program_kernels);
    Some(ModuleKernel {
        kernel,
        signature,
        registry,
        inputs,
        exports,
        subsumed_top_ids,
    })
}

/// Walk a joined-Sync subtree, gathering every `ExprId` as a region
/// member and counting `Apply` sites. Caller has already verified
/// `expr` is joined-Sync; all descendants are Sync by monotonicity
/// of the effect join.
fn collect_region(
    expr: &Expr,
    members: &mut Vec<crate::expr::ExprId>,
    apply_count: &mut usize,
) {
    members.push(expr.id);
    if matches!(expr.kind, ExprKind::Apply(_)) {
        *apply_count += 1;
    }
    for_each_child(expr, &mut |c| collect_region(c, members, apply_count));
}

/// Per-`ExprId` effect classification for whole-graph fusion. Walks
/// `expr`, and for every node records into `out` whether that node —
/// and the value it produces — is `Sync` (fusable into a kernel) or
/// `Async` (a fusion boundary). Returns `expr`'s own effect.
///
/// A node is `Async` if it is itself an async edge — a `Ref` to an
/// unstable binding, or an `Apply` whose call-site effect
/// ([`apply_site_effect`]) is async — or if any sub-expression whose
/// value it consumes is `Async` (async-ness propagates up by value).
///
/// Differs from [`walk_for_effect`] (which computes a function's
/// *intrinsic* effect) in two ways: it treats reads of unstable
/// bindings as async (a cross-cycle read breaks a sync subgraph
/// boundary, even though it doesn't make the enclosing lambda
/// intrinsically async), and it joins per-`Apply` via
/// [`apply_site_effect`] for the proper call-site join. Unlike the
/// old whole-expr rollup it does NOT early-out on the first async
/// edge — phase B (region carving) needs every node's effect,
/// including sync siblings of an async child.
///
/// Top-level only: nested lambda bodies are not descended into.
/// Constructing a lambda *value* is sync; a lambda body with an
/// interior async edge already has its `intrinsic_effect` marked
/// `Async` by `infer_effects`, which `apply_site_effect` folds in at
/// every call site. Splitting a lambda body at an interior async
/// edge is a separate, later milestone.
fn program_effect_map<R: Rt, E: UserEvent>(
    expr: &Expr,
    ctx: &ExecCtx<R, E>,
    out: &mut nohash::IntMap<crate::expr::ExprId, EffectKind>,
) -> EffectKind {
    // Start from the join of every same-scope child. `for_each_child`
    // does not descend into nested lambda bodies, so a lambda *value*
    // contributes nothing and stays Sync — matching the top-level-
    // only scope of the analyzer.
    let mut eff = EffectKind::Sync;
    for_each_child(expr, &mut |c| {
        eff = eff.join(program_effect_map(c, ctx, out));
    });
    // Two node kinds are async edges in their own right (independent
    // of their children); fold their intrinsic edge effect in here.
    match &expr.kind {
        // A read of an unstable binding — its value depends on a
        // future-cycle `<-` write.
        ExprKind::Ref { name } => {
            if let Some(ident) = ident_of(name) {
                if ctx.unstable_bindings.contains(&ArcStr::from(ident)) {
                    eff = EffectKind::Async;
                }
            }
        }
        // An Apply joins in the call-site effect (callee intrinsic ⊔
        // fn-arg effects).
        ExprKind::Apply(a) => {
            eff = eff.join(apply_site_effect(a, expr.id, ctx));
        }
        _ => {}
    }
    out.insert(expr.id, eff);
    eff
}

/// Invoke `f` once per immediate sub-expression of `expr` that
/// belongs to the *same* kernel scope — every child except a nested
/// lambda's body. Nested lambdas are fusion-scope boundaries: their
/// bodies form their own kernels, so the whole-graph analyzer
/// ([`program_effect_map`], region carving) and [`count_applies`] all
/// stop at the lambda. This is the single exhaustive `ExprKind` walk
/// the analyzer relies on — adding an `ExprKind` variant forces an
/// update here, which keeps every consumer in sync.
fn for_each_child(expr: &Expr, f: &mut impl FnMut(&Expr)) {
    match &expr.kind {
        ExprKind::Apply(a) => {
            f(&a.function);
            for (_, arg) in a.args.iter() {
                f(arg);
            }
        }
        ExprKind::Bind(b) => f(&b.value),
        ExprKind::Connect { value, .. } => f(value),
        ExprKind::Do { exprs } => {
            for e in exprs.iter() {
                f(e);
            }
        }
        ExprKind::Module { value, .. } => match value {
            crate::expr::ModuleKind::Resolved { exprs, .. } => {
                for e in exprs.iter() {
                    f(e);
                }
            }
            crate::expr::ModuleKind::Dynamic { source, .. } => f(source),
            crate::expr::ModuleKind::Unresolved { .. } => {}
        },
        // A lambda *value* is a leaf from the enclosing kernel's POV
        // — its body is a separate fusion scope.
        ExprKind::Lambda(_) => {}
        ExprKind::Select(s) => {
            f(&s.arg);
            for (_, arm) in s.arms.iter() {
                f(arm);
            }
        }
        ExprKind::TryCatch(tc) => {
            for e in tc.exprs.iter() {
                f(e);
            }
            f(&tc.handler);
        }
        ExprKind::ExplicitParens(e)
        | ExprKind::Qop(e)
        | ExprKind::OrNever(e)
        | ExprKind::ByRef(e)
        | ExprKind::Deref(e)
        | ExprKind::Not { expr: e }
        | ExprKind::TypeCast { expr: e, .. } => f(e),
        ExprKind::StringInterpolate { args }
        | ExprKind::Any { args }
        | ExprKind::Array { args }
        | ExprKind::Tuple { args }
        | ExprKind::Variant { args, .. } => {
            for e in args.iter() {
                f(e);
            }
        }
        ExprKind::Map { args } => {
            for (k, v) in args.iter() {
                f(k);
                f(v);
            }
        }
        ExprKind::Struct(s) => {
            for (_, e) in s.args.iter() {
                f(e);
            }
        }
        ExprKind::StructWith(sw) => {
            f(&sw.source);
            for (_, e) in sw.replace.iter() {
                f(e);
            }
        }
        ExprKind::StructRef { source, .. }
        | ExprKind::TupleRef { source, .. } => f(source),
        ExprKind::ArrayRef { source, i } => {
            f(source);
            f(i);
        }
        ExprKind::ArraySlice { source, start, end } => {
            f(source);
            if let Some(s) = start {
                f(s);
            }
            if let Some(e) = end {
                f(e);
            }
        }
        ExprKind::MapRef { source, key } => {
            f(source);
            f(key);
        }
        ExprKind::Add { lhs, rhs }
        | ExprKind::Sub { lhs, rhs }
        | ExprKind::Mul { lhs, rhs }
        | ExprKind::Div { lhs, rhs }
        | ExprKind::Mod { lhs, rhs }
        | ExprKind::CheckedAdd { lhs, rhs }
        | ExprKind::CheckedSub { lhs, rhs }
        | ExprKind::CheckedMul { lhs, rhs }
        | ExprKind::CheckedDiv { lhs, rhs }
        | ExprKind::CheckedMod { lhs, rhs }
        | ExprKind::Eq { lhs, rhs }
        | ExprKind::Ne { lhs, rhs }
        | ExprKind::Lt { lhs, rhs }
        | ExprKind::Gt { lhs, rhs }
        | ExprKind::Lte { lhs, rhs }
        | ExprKind::Gte { lhs, rhs }
        | ExprKind::And { lhs, rhs }
        | ExprKind::Or { lhs, rhs }
        | ExprKind::Sample { lhs, rhs } => {
            f(lhs);
            f(rhs);
        }
        ExprKind::NoOp
        | ExprKind::Constant(_)
        | ExprKind::Ref { .. }
        | ExprKind::Use { .. }
        | ExprKind::TypeDef(_) => {}
    }
}

/// Lazy on-demand resolution of a fused kernel by binding name.
/// Looks `name` up in `ctx.fusion_lambdas` (populated by
/// `Bind::compile` for every `let X = lambda` in scope), locks the
/// entry's cache, and builds the kernel on first request. Subsequent
/// requests return the cached result. Cycles (a kernel transitively
/// referencing itself through other kernels) are broken by the
/// `InProgress` cache state — recursive calls to a still-building
/// kernel return `None`.
///
/// `apply_site_hint` is the function-expression `typ` cell of one
/// Apply that calls `name` (i.e. `a.function.typ.clone()`). The lazy
/// build path prefers the call-site resolved FnType (read from the
/// hint cell) over the lambda's own `spec_typ`, because the spec
/// typ may have unbound TVars (no typechecker constraint from the
/// lambda's own definition) while the Apply's function typ is
/// always concrete after `CallSite::typecheck`. This is what lets
/// unannotated callees fuse.
///
/// Returns the callee's signature (for compile-time call lowering)
/// and `Arc<KirKernel>` (for runtime dispatch via the interpreter's
/// `KirOp::Call` path).
pub fn lazy_resolve_kernel<R: crate::Rt, E: crate::UserEvent>(
    ctx: &mut crate::ExecCtx<R, E>,
    name: &str,
    apply_site_hint: Option<triomphe::Arc<std::sync::OnceLock<crate::typ::Type>>>,
) -> Option<(KnownFusedFn, std::sync::Arc<KirKernel>)> {
    let entry = ctx.fusion_lambdas.get(name).cloned()?;
    // M6c: short-circuit async lambdas. KIR can't represent async-
    // effect ops (timer, subscribe, queue, etc.), so a kernel build
    // for an async lambda would fail anyway — fail fast and mark the
    // cache as Failed so future calls skip too.
    if matches!(*entry.effect.lock(), EffectKind::Async) {
        let mut cache = entry.cache.lock();
        if matches!(*cache, crate::FusionLazyCache::NotAttempted) {
            *cache = crate::FusionLazyCache::Failed;
        }
        return None;
    }
    // First: check current cache state without forcing rebuild.
    {
        let cache = entry.cache.lock();
        match &*cache {
            crate::FusionLazyCache::Built { signature, kernel } => {
                return Some((signature.clone(), kernel.clone()));
            }
            crate::FusionLazyCache::Failed
            | crate::FusionLazyCache::InProgress => {
                return None;
            }
            crate::FusionLazyCache::NotAttempted => {}
        }
    }
    // Transition NotAttempted → InProgress. Re-check under the lock
    // because another caller may have raced ahead.
    {
        let mut cache = entry.cache.lock();
        match &*cache {
            crate::FusionLazyCache::Built { signature, kernel } => {
                return Some((signature.clone(), kernel.clone()));
            }
            crate::FusionLazyCache::Failed
            | crate::FusionLazyCache::InProgress => return None,
            crate::FusionLazyCache::NotAttempted => {
                *cache = crate::FusionLazyCache::InProgress;
            }
        }
    }
    // Build with the entry's cache lock dropped, so recursion into
    // sibling kernels (which may transitively touch this one) sees
    // InProgress and breaks the cycle cleanly.
    let result = try_build_lazy(
        ctx,
        &entry.fn_name,
        apply_site_hint,
        &entry.spec_typ,
        &entry.lambda,
    );
    // Commit the final state.
    let mut cache = entry.cache.lock();
    *cache = match &result {
        Some((sig, kernel)) => crate::FusionLazyCache::Built {
            signature: sig.clone(),
            kernel: kernel.clone(),
        },
        None => crate::FusionLazyCache::Failed,
    };
    result
}

fn try_build_lazy<R: crate::Rt, E: crate::UserEvent>(
    ctx: &mut crate::ExecCtx<R, E>,
    fn_name: &ArcStr,
    apply_site_hint: Option<triomphe::Arc<std::sync::OnceLock<crate::typ::Type>>>,
    spec_typ: &triomphe::Arc<std::sync::OnceLock<crate::typ::Type>>,
    lambda: &triomphe::Arc<crate::expr::LambdaExpr>,
) -> Option<(KnownFusedFn, std::sync::Arc<KirKernel>)> {
    use netidx::utils::Either;
    // Patch the lambda's argspec / rtype from the typechecker's
    // resolved FnType. We prefer the call-site Apply's function-
    // expression typ cell (populated by `CallSite::typecheck` ->
    // trait-default propagation, with TVars unified against the
    // actual arg expressions) over the lambda's own spec typ —
    // the spec typ may still have unbound TVars when the lambda
    // is polymorphic or hasn't been called yet. Falls back to the
    // lambda's spec typ if no apply hint is available.
    let mut patched = lambda.clone();
    let extract_fntype =
        |cell: &triomphe::Arc<std::sync::OnceLock<crate::typ::Type>>| -> Option<crate::typ::FnType> {
            cell.get().and_then(|t| {
                t.with_deref(|resolved| match resolved? {
                    crate::typ::Type::Fn(ft) => Some((**ft).clone()),
                    _ => None,
                })
            })
        };
    let ft = apply_site_hint
        .as_ref()
        .and_then(extract_fntype)
        .or_else(|| extract_fntype(spec_typ));
    if let Some(ft) = ft {
        apply_fntype_to_lambda(&mut patched, &ft);
    }
    let mut known: std::collections::BTreeMap<ArcStr, KnownFusedFn> =
        std::collections::BTreeMap::new();
    if let Either::Left(body) = &patched.body {
        let mut seen: std::collections::BTreeSet<ArcStr> =
            std::collections::BTreeSet::new();
        for (callee, apply_id) in discover_callee_names(body) {
            // Skip self-calls — `build_kir_kernel` registers the
            // kernel-being-built in its own internal known_fns
            // before emit_body runs, so self-recursion lowers
            // correctly without going through the lazy registry.
            if callee.as_str() == fn_name.as_str() {
                continue;
            }
            if !seen.insert(callee.clone()) {
                continue;
            }
            if let Some((sig, _kernel)) =
                lazy_resolve_kernel(ctx, &callee, Some(apply_id))
            {
                known.insert(callee, sig);
            }
            // Else: callee couldn't be lazy-built (no entry in
            // fusion_lambdas, in-progress cycle, or its own build
            // failed). The caller's body fusion will fail at this
            // call site if the callee ends up being needed.
        }
    }
    // Discover sync-builtin call sites in the lambda body and
    // register one FnParam per unique builtin as an `extra_fn_input`,
    // so `emit_apply_expr`'s `find_fn_input` lookup catches them and
    // lowers each call as `KirOp::DynCall` (dispatched through the
    // pre-bound builtin slot at `KirNode::new`).
    let mut extra_fn_inputs: Vec<crate::kernel_ir::FnParam> = Vec::new();
    if let Either::Left(body) = &patched.body {
        extra_fn_inputs.extend(discover_builtin_fn_inputs(body, ctx));
    }
    let consts = ctx.fusion_known_consts.clone();
    let (kernel, sig) = build_kir_kernel_with_binding_inputs(
        fn_name.as_str(),
        &patched,
        &known,
        &consts,
        &extra_fn_inputs,
    )?;
    Some((sig, std::sync::Arc::new(kernel)))
}

/// Accumulated parameter slots — the param-derivation output shared
/// between [`build_kir_kernel_with_binding_inputs`] (driven by a
/// `LambdaExpr`'s argspec) and [`build_kir_kernel_from_region`]
/// (driven by a `Vec<RegionInput>`). Bundling these in one struct
/// keeps [`finish_kernel`]'s signature short.
#[derive(Default)]
struct KernelParams {
    params: Vec<Input>,
    fn_params: Vec<crate::kernel_ir::FnParam>,
    array_params: Vec<crate::kernel_ir::ArrayInput>,
    tuple_params: Vec<crate::kernel_ir::TupleInput>,
    struct_params: Vec<crate::kernel_ir::StructInput>,
    variant_params: Vec<crate::kernel_ir::VariantInput>,
    arg_types: Vec<KirType>,
}

/// Populate `ctx`'s slot lists + `params` + `tail_call_slots` from a
/// `&[RegionInput]` list. This is the single source of truth for
/// "given a typed input, where does its slot go": every kernel build
/// path — lambda, region, module — routes its value inputs through
/// here. Adding a new `RegionInputKind` variant means updating this
/// function and nothing else.
///
/// Lambdas with fn-typed args route those through the parallel
/// `fn_inputs` channel (`FnParam` / `FnSource::Param`), not through
/// here.
fn populate_kernel_inputs(
    value_inputs: &[RegionInput],
    ctx: &mut FusionCtx,
    p: &mut KernelParams,
    tail_call_slots: &mut Vec<crate::kernel_ir::TailCallSlot>,
) {
    for input in value_inputs {
        match &input.kind {
            RegionInputKind::Prim(prim) => {
                let i = Input {
                    name: input.name.clone(),
                    prim: *prim,
                    bind_id: None,
                };
                p.params.push(i.clone());
                ctx.inputs.push(i);
                p.arg_types.push(KirType::Prim(*prim));
                tail_call_slots.push(crate::kernel_ir::TailCallSlot {
                    name: input.name.clone(),
                    kind: crate::kernel_ir::TailCallSlotKind::Scalar(*prim),
                });
            }
            RegionInputKind::Array(elem) => {
                let ai = crate::kernel_ir::ArrayInput {
                    name: input.name.clone(),
                    elem: elem.clone(),
                    bind_id: None,
                };
                p.array_params.push(ai.clone());
                ctx.array_inputs.push(ai);
                p.arg_types.push(KirType::Array(Box::new(elem.clone())));
                tail_call_slots.push(crate::kernel_ir::TailCallSlot {
                    name: input.name.clone(),
                    kind: crate::kernel_ir::TailCallSlotKind::ValArray,
                });
            }
            RegionInputKind::Tuple(elems) => {
                let ti = crate::kernel_ir::TupleInput {
                    name: input.name.clone(),
                    elems: elems.clone(),
                    bind_id: None,
                };
                p.tuple_params.push(ti.clone());
                ctx.tuple_inputs.push(ti);
                p.arg_types.push(KirType::Tuple(elems.clone()));
                tail_call_slots.push(crate::kernel_ir::TailCallSlot {
                    name: input.name.clone(),
                    kind: crate::kernel_ir::TailCallSlotKind::ValArray,
                });
            }
            RegionInputKind::Struct(fields) => {
                let si = crate::kernel_ir::StructInput {
                    name: input.name.clone(),
                    fields: fields.clone(),
                    bind_id: None,
                };
                p.struct_params.push(si.clone());
                ctx.struct_inputs.push(si);
                p.arg_types.push(KirType::Struct(fields.clone()));
                tail_call_slots.push(crate::kernel_ir::TailCallSlot {
                    name: input.name.clone(),
                    kind: crate::kernel_ir::TailCallSlotKind::ValArray,
                });
            }
            RegionInputKind::Variant(cases) => {
                let vi = crate::kernel_ir::VariantInput {
                    name: input.name.clone(),
                    cases: cases.clone(),
                    bind_id: None,
                };
                p.variant_params.push(vi.clone());
                ctx.variant_inputs.push(vi);
                p.arg_types.push(KirType::Variant(cases.clone()));
                tail_call_slots.push(crate::kernel_ir::TailCallSlot {
                    name: input.name.clone(),
                    kind: crate::kernel_ir::TailCallSlotKind::Variant,
                });
            }
        }
    }
}

/// Unified kernel-build core. Every fusion entry point (lambda body,
/// region root, top-level module Do) routes through this. The three
/// thin wrappers above translate their caller-specific inputs into
/// this function's shape:
///
/// - `value_inputs` — every prim/array/tuple/struct/variant input.
///   Lambdas translate their argspec into these (with fn-typed args
///   diverted to `fn_inputs`); regions and module bodies already
///   have them in `&[RegionInput]` shape.
/// - `fn_inputs` — every fn-typed input. For lambdas: HOF args
///   (`FnSource::Param`) + binding-source slots. For regions /
///   modules: builtin-source + binding-source slots discovered via
///   `discover_builtin_fn_inputs` / `discover_binding_fn_inputs`.
/// - `return_type` — `Some(t)` if the caller already knows it
///   (lambda's `rtype` annotation, module's tuple-of-exports); `None`
///   to derive it from the body's typed AST via `infer_body_rtype`.
/// - `has_tail` / `self_info` — lambda-only. Both `false`/`None` for
///   regions and modules.
///
/// Returns `None` if anything in the body fails to lower; callers
/// fall back to non-fused execution.
fn build_kernel(
    fn_name: &str,
    body: &Expr,
    value_inputs: &[RegionInput],
    fn_inputs: &[crate::kernel_ir::FnParam],
    return_type: Option<KirType>,
    has_tail: bool,
    self_info: Option<&SelfInfo>,
    known: &std::collections::BTreeMap<ArcStr, KnownFusedFn>,
    consts: &std::collections::BTreeMap<ArcStr, KnownConst>,
) -> Option<(KirKernel, KnownFusedFn)> {
    let mut ctx = FusionCtx {
        inputs: vec![],
        fn_inputs: fn_inputs.to_vec(),
        array_inputs: vec![],
        tuple_inputs: vec![],
        struct_inputs: vec![],
        variant_inputs: vec![],
        known_fns: known.clone(),
        known_consts: consts.clone(),
    };
    let mut p = KernelParams::default();
    p.fn_params.extend(fn_inputs.iter().cloned());
    let mut tail_call_slots: Vec<crate::kernel_ir::TailCallSlot> =
        Vec::with_capacity(value_inputs.len());
    populate_kernel_inputs(value_inputs, &mut ctx, &mut p, &mut tail_call_slots);
    let rtype = match return_type {
        Some(t) => t,
        None => infer_body_rtype(body, &ctx, self_info)?,
    };
    finish_kernel(
        fn_name,
        body,
        rtype,
        p,
        tail_call_slots,
        has_tail,
        &mut ctx,
        self_info,
    )
}

/// Final assembly shared between the lambda and region build paths.
/// Builds the [`KnownFusedFn`] signature, registers `fn_name` in
/// `ctx.known_fns` (so self-recursive `Call` sites in the body lower
/// before `emit_body` runs), emits the body to KIR, and packages
/// everything into a [`KirKernel`].
///
/// The caller is responsible for the bits this function can't decide
/// from a generic kernel build: `params` (the slot bundle),
/// `return_type` (from `lambda.rtype` / `infer_body_rtype` for a
/// lambda, or supplied directly for a region), `tail_call_slots` /
/// `has_tail_loop` / `self_info` (always lambda-specific; regions
/// pass empty / false / None).
fn finish_kernel(
    fn_name: &str,
    body: &Expr,
    return_type: KirType,
    params: KernelParams,
    tail_call_slots: Vec<crate::kernel_ir::TailCallSlot>,
    has_tail_loop: bool,
    ctx: &mut FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<(KirKernel, KnownFusedFn)> {
    let signature = KnownFusedFn {
        body_fn_name: format!("fused_{fn_name}_body"),
        arg_types: params.arg_types,
        return_type: return_type.clone(),
    };
    ctx.known_fns.insert(ArcStr::from(fn_name), signature.clone());
    let body_stmts = emit_body(body, ctx, self_info)?;
    let kernel = KirKernel {
        fn_name: ArcStr::from(fn_name),
        params: params.params,
        fn_params: params.fn_params,
        array_params: params.array_params,
        tuple_params: params.tuple_params,
        struct_params: params.struct_params,
        variant_params: params.variant_params,
        tail_call_slots,
        return_type,
        has_tail_loop,
        body: body_stmts,
    };
    Some((kernel, signature))
}

/// Build a [`KirKernel`] from a lambda's argspec + body, plus
/// optional registries of already-fused kernels and known constants.
/// This is the path the JIT and interpreter share: both consume the
/// `KirKernel` directly.
///
/// Returns `None` if the lambda can't be fused (non-primitive arg
/// types, labeled args, body uses unsupported constructs, etc.).
pub fn build_kir_kernel(
    fn_name: &str,
    lambda: &crate::expr::LambdaExpr,
    known: &std::collections::BTreeMap<ArcStr, KnownFusedFn>,
    consts: &std::collections::BTreeMap<ArcStr, KnownConst>,
) -> Option<(KirKernel, KnownFusedFn)> {
    build_kir_kernel_with_binding_inputs(fn_name, lambda, known, consts, &[])
}

/// Like [`build_kir_kernel`] but accepts additional `Binding`-source
/// fn-typed inputs. lambda.rs's lazy-fusion InitFn uses this to
/// register stable-bound but non-fusable callees as DynCall slots,
/// so the parent kernel can fuse around them.
pub fn build_kir_kernel_with_binding_inputs(
    fn_name: &str,
    lambda: &crate::expr::LambdaExpr,
    known: &std::collections::BTreeMap<ArcStr, KnownFusedFn>,
    consts: &std::collections::BTreeMap<ArcStr, KnownConst>,
    binding_fn_inputs: &[crate::kernel_ir::FnParam],
) -> Option<(KirKernel, KnownFusedFn)> {
    use netidx::utils::Either;
    let body = match &lambda.body {
        Either::Left(e) => e,
        Either::Right(_) => return None,
    };
    // Classify each lambda arg into either a value input (prim /
    // array / tuple / struct / variant — modeled as `RegionInput`) or
    // a fn-typed input (HOF arg, modeled as `FnParam` with
    // `FnSource::Param`). The shared `build_kernel` core handles both
    // channels.
    let mut value_inputs: Vec<RegionInput> =
        Vec::with_capacity(lambda.args.len());
    let mut fn_inputs: Vec<crate::kernel_ir::FnParam> = Vec::new();
    for (arg_pos, arg) in lambda.args.iter().enumerate() {
        if arg.labeled.is_some() {
            return None;
        }
        let typ = arg.constraint.as_ref()?;
        let name = arg.pattern.single_bind()?;
        // Try fn-typed first so HOF args (which would also match
        // primitive shape via Bottom/Any in degenerate cases) get
        // routed to the fn channel.
        if let Some(fp) =
            try_fn_param_from_type(typ, name.clone(), arg_pos as u32)
        {
            fn_inputs.push(fp);
            continue;
        }
        // Value input: classify via the same machinery regions use.
        if let Some(prim) = PrimType::from_type(typ) {
            value_inputs.push(RegionInput {
                expr_id: crate::expr::ExprId::new(),
                name: name.clone(),
                kind: RegionInputKind::Prim(prim),
            });
            continue;
        }
        if let Some(kind) = type_to_region_input_kind(typ) {
            value_inputs.push(RegionInput {
                expr_id: crate::expr::ExprId::new(),
                name: name.clone(),
                kind,
            });
            continue;
        }
        // None of prim / fn / array / tuple / struct / variant — bail.
        return None;
    }
    // Fold in binding-source fn_inputs (DynCall slots that resolve
    // through `ctx.cached[bind_id]` at dispatch time). Pushed last
    // so an HOF-arg with the same name shadows any outer binding —
    // `find_fn_input` is a linear scan that returns the first hit.
    fn_inputs.extend_from_slice(binding_fn_inputs);
    let has_tail = body_has_tail_call(body, fn_name);
    // Walk lambda.args in declaration order to build `source_args`
    // (tail-call validation) — each entry maps to a value input or
    // bails for fn args (only allowed when `!has_tail`).
    let mut source_args: Vec<SelfArg> = Vec::with_capacity(lambda.args.len());
    for arg in lambda.args.iter() {
        let arg_name = arg.pattern.single_bind()?;
        match value_inputs.iter().find(|ri| &ri.name == arg_name) {
            Some(ri) => source_args.push(SelfArg {
                name: ri.name.clone(),
                typ: region_input_kind_to_kirtype(&ri.kind),
            }),
            None => {
                // Must be an fn-typed input then. A fn_param + tail-
                // loop kernel can't fuse (a tail call referencing a
                // bare fn_param has no emit_expr lowering); bail
                // explicitly so `source_args` never drifts.
                if has_tail {
                    return None;
                }
            }
        }
    }
    // `SelfInfo.params` is the prim subset of value inputs (composite
    // params don't appear in `params` — they go to `array_params` /
    // `tuple_params` / etc.).
    let self_params: Vec<Input> = value_inputs
        .iter()
        .filter_map(|ri| match &ri.kind {
            RegionInputKind::Prim(p) => Some(Input {
                name: ri.name.clone(),
                prim: *p,
                bind_id: None,
            }),
            _ => None,
        })
        .collect();
    let self_info = SelfInfo {
        name: ArcStr::from(fn_name),
        params: self_params,
        source_args,
    };
    // `Some(rtype)` if annotated (must convert or bail); `None` to
    // ask `build_kernel` to infer via `infer_body_rtype`.
    let return_type = match lambda.rtype.as_ref() {
        Some(rtype) => Some(KirType::from_type(rtype)?),
        None => None,
    };
    build_kernel(
        fn_name,
        body,
        &value_inputs,
        &fn_inputs,
        return_type,
        has_tail,
        Some(&self_info),
        known,
        consts,
    )
}

/// Pull the resolved [`FnType`](crate::typ::FnType) off an expression
/// whose typed-AST cell carries `Type::Fn(_)`. Used to look up the
/// call-site FnType for an `Apply` via `a.function.typ` (instead of
/// the soon-to-be-deprecated `ctx.fn_types[apply.id]` sidecar). The
/// returned `FnType` is what the typechecker stored — TVars unify
/// in-place through their `Arc<RwLock>`, so callers see the live
/// resolved view automatically.
fn resolved_fn_type(expr: &Expr) -> Option<crate::typ::FnType> {
    let t = expr.typ.get()?;
    t.with_deref(|resolved| match resolved? {
        crate::typ::Type::Fn(ft) => Some((**ft).clone()),
        _ => None,
    })
}

/// Convert a [`RegionInputKind`] back to its full [`KirType`] (with
/// the Array variant boxed). Used when building `SelfArg`s for tail-
/// call validation — they carry KirType, not the kind enum.
fn region_input_kind_to_kirtype(kind: &RegionInputKind) -> KirType {
    match kind {
        RegionInputKind::Prim(p) => KirType::Prim(*p),
        RegionInputKind::Array(elem) => KirType::Array(Box::new(elem.clone())),
        RegionInputKind::Tuple(elems) => KirType::Tuple(elems.clone()),
        RegionInputKind::Struct(fields) => KirType::Struct(fields.clone()),
        RegionInputKind::Variant(cases) => KirType::Variant(cases.clone()),
    }
}

/// Build a [`KirKernel`] from an arbitrary `Expr` (the root of a
/// maximal sync region identified by [`analyze_program`]) plus a
/// typed input list. Mirrors [`build_kir_kernel_with_binding_inputs`]
/// for lambdas; a region has no argspec, no self-recursion, and no
/// tail loop, so this path is simpler — no `SelfInfo`,
/// `tail_call_slots` is empty, and `has_tail_loop` is always false.
///
/// `return_type` is the region root's `KirType`. Pass `Some(t)` when
/// the caller already knows it (e.g. for an `Apply` region root from
/// `ctx.fn_types[apply].rtype`); pass `None` to have this function
/// infer it via [`infer_body_rtype`] over the region body. Inference
/// failure yields `None`.
pub fn build_kir_kernel_from_region(
    fn_name: &str,
    region: &Expr,
    inputs: &[RegionInput],
    extra_fn_inputs: &[crate::kernel_ir::FnParam],
    return_type: Option<KirType>,
    known: &std::collections::BTreeMap<ArcStr, KnownFusedFn>,
    consts: &std::collections::BTreeMap<ArcStr, KnownConst>,
) -> Option<(KirKernel, KnownFusedFn)> {
    // Regions have no self-recursion, no tail loop. Inputs and
    // fn_inputs are already in the unified shape — everything
    // delegates to `build_kernel`.
    build_kernel(
        fn_name,
        region,
        inputs,
        extra_fn_inputs,
        return_type,
        false,
        None,
        known,
        consts,
    )
}

/// Resolve a graphix [`Type`] to the [`RegionInputKind`] for that
/// kernel slot — used by [`discover_region_inputs`] when classifying
/// each free-variable input. Mirrors the lambda path's param-type
/// classification ([`build_kir_kernel_with_binding_inputs`]) for
/// prim / array / tuple / struct / variant shapes. Returns `None`
/// for any shape the kernel build can't represent (e.g. a function
/// type — fn-typed inputs aren't supported in the M8.4 initial
/// model).
fn type_to_region_input_kind(typ: &Type) -> Option<RegionInputKind> {
    if let Some(prim) = PrimType::from_type(typ) {
        return Some(RegionInputKind::Prim(prim));
    }
    if let Some(elem) = array_elem_prim(typ) {
        return Some(RegionInputKind::Array(KirType::Prim(elem)));
    }
    match KirType::from_type(typ)? {
        KirType::Array(elem) => Some(RegionInputKind::Array((*elem).clone())),
        KirType::Tuple(elems) => Some(RegionInputKind::Tuple(elems)),
        KirType::Struct(fields) => Some(RegionInputKind::Struct(fields)),
        KirType::Variant(cases) => Some(RegionInputKind::Variant(cases)),
        // `KirType::Prim` is handled above via the dedicated extractor.
        KirType::Prim(_) => None,
        // `Unit` is a return-only shape — it can't appear as an
        // input slot.
        KirType::Unit => None,
        // Strings can't be region free-var inputs yet — would need
        // a string_inputs slot list.
        KirType::String => None,
    }
}

/// Walk the region rooted at `root` and collect every free-variable
/// `Ref` — a read of a binding not introduced by an in-region `let`.
/// Each free var becomes a [`RegionInput`] keyed by the binding's
/// name; its type comes from `env.lookup_bind(scope, name).typ`.
/// Returns `None` if any free var has a type the kernel can't accept
/// (e.g. a function type, a user-defined type, an `Any`) — the
/// region build then bails and the region stays as the existing
/// node graph.
///
/// In-region let scoping: a `Bind` introduces its name into the
/// `bound` set after its value is walked, so subsequent uses of the
/// name within the region are treated as in-region (NOT inputs).
/// The set tracks names flatly, so a region whose code shadows an
/// in-region let with a later reference to the *outer* binding of
/// the same name will under-report inputs — a conservative miss
/// (the kernel fails to build, region stays non-fused). Rare enough
/// to defer; revisit if M8.4 step (f) shows it matters.
///
/// Lambdas inside the region are leaf values (per `for_each_child`),
/// so their args and bodies don't pollute the scope tracking.
fn discover_region_inputs(
    root: &Expr,
    scope: &ModPath,
    env: &Env,
) -> Option<Vec<RegionInput>> {
    let mut bound: std::collections::BTreeSet<ArcStr> =
        std::collections::BTreeSet::new();
    let mut seen: std::collections::BTreeSet<ArcStr> =
        std::collections::BTreeSet::new();
    let mut inputs: Vec<RegionInput> = Vec::new();
    let mut ok = true;
    walk_free_refs(root, scope, env, &mut bound, &mut seen, &mut inputs, &mut ok);
    if !ok {
        return None;
    }
    Some(inputs)
}

fn walk_free_refs(
    expr: &Expr,
    scope: &ModPath,
    env: &Env,
    bound: &mut std::collections::BTreeSet<ArcStr>,
    seen: &mut std::collections::BTreeSet<ArcStr>,
    inputs: &mut Vec<RegionInput>,
    ok: &mut bool,
) {
    if !*ok {
        return;
    }
    match &expr.kind {
        ExprKind::Ref { name } => {
            let Some(ident) = ident_of(name) else { return };
            let key = ArcStr::from(ident);
            if bound.contains(&key) || seen.contains(&key) {
                return;
            }
            // Look up the binding's type in the program env. The
            // resolver has already qualified `name`'s scope; we
            // pass the program-root scope and let `find_visible`
            // walk back to find the binding.
            let Some((_, bind)) = env.lookup_bind(scope, name) else {
                *ok = false;
                return;
            };
            // Function-typed bindings (builtins like `println`, user
            // lambdas referenced as callees) aren't region *inputs*
            // — they're handled via `FnSource::Builtin` /
            // `FnSource::Binding` slots populated by
            // `discover_builtin_fn_inputs` / `resolve_binding_fn_input`.
            // Just skip them rather than bailing the whole region.
            if matches!(bind.typ.with_deref(|t| matches!(t, Some(Type::Fn(_)))), true)
            {
                return;
            }
            let Some(kind) = type_to_region_input_kind(&bind.typ) else {
                *ok = false;
                return;
            };
            seen.insert(key.clone());
            inputs.push(RegionInput { expr_id: expr.id, name: key, kind });
        }
        ExprKind::Bind(b) => {
            // The value is evaluated in the outer scope (the bind's
            // name isn't in scope yet); walk it first, then add the
            // name to `bound` so subsequent siblings see it.
            walk_free_refs(&b.value, scope, env, bound, seen, inputs, ok);
            if let Some(name) = b.pattern.single_bind() {
                bound.insert(name.clone());
            }
        }
        _ => {
            for_each_child(expr, &mut |c| {
                walk_free_refs(c, scope, env, bound, seen, inputs, ok);
            });
        }
    }
}

/// Try to interpret a [`Type`] as a function whose arg and return
/// types are all fusion-representable (`KirType`). Returns the
/// FnParam (with `FnSource::Param { arg_pos }`) ready to insert
/// into the kernel's `fn_params`, or `None` if any part of the
/// signature isn't a `KirType` shape.
fn try_fn_param_from_type(
    typ: &Type,
    name: ArcStr,
    arg_pos: u32,
) -> Option<crate::kernel_ir::FnParam> {
    let (arg_types, return_type) = extract_fn_signature(typ)?;
    Some(crate::kernel_ir::FnParam {
        name,
        source: crate::kernel_ir::FnSource::Param { arg_pos },
        arg_types,
        return_type,
    })
}

/// Try to interpret `typ` as `Array<P>` for some primitive `P`, and
/// return that element type. Returns `None` for any other shape (not
/// an array, nested arrays, non-primitive elements). Used to pick out
/// fusable array params and array-typed call sites.
fn array_elem_prim(typ: &Type) -> Option<PrimType> {
    typ.with_deref(|resolved| match resolved? {
        Type::Array(inner) => PrimType::from_type(inner),
        _ => None,
    })
}

/// Try to register a stable-bound but non-fusable callee `name` as
/// a `FnSource::Binding` DynCall slot. Returns the FnParam ready to
/// pass into [`build_kir_kernel_with_binding_inputs`] as one of the
/// extra fn-inputs, or `None` if `name` doesn't resolve to a stable
/// binding with a primitive function signature.
///
/// Stability: rejects names in `ctx.unstable_bindings` so a later
/// `<-` can't silently dispatch into a stale function value mid-
/// kernel — that's a separate motivation for DynCall (M4g v2 v3
/// follow-up) that wants different correctness guarantees.
pub fn resolve_binding_fn_input<R: crate::Rt, E: crate::UserEvent>(
    ctx: &crate::ExecCtx<R, E>,
    scope: &crate::Scope,
    name: &ArcStr,
) -> Option<crate::kernel_ir::FnParam> {
    if ctx.unstable_bindings.contains(name) {
        return None;
    }
    let path: crate::expr::ModPath = [name.clone()].into_iter().collect();
    let bind_id = ctx.env.lookup_bind(&scope.lexical, &path)?.1.id;
    let bind = ctx.env.by_id.get(&bind_id)?;
    let (arg_types, return_type) = bind
        .typ
        .with_deref(|resolved| {
            extract_fn_signature(resolved?)
        })?;
    Some(crate::kernel_ir::FnParam {
        name: name.clone(),
        source: crate::kernel_ir::FnSource::Binding { bind_id },
        arg_types,
        return_type,
    })
}

/// Walk `body` and return one [`FnParam`] (with `FnSource::Builtin`)
/// per unique sync-builtin Apply{Ref} call site whose name isn't in
/// `ctx.unstable_bindings`. The caller folds these into a kernel's
/// `extra_fn_inputs` before `emit_body` runs, so the existing
/// `find_fn_input` lookup in `emit_apply_expr` resolves builtin
/// calls to `KirOp::DynCall` automatically.
///
/// Skips:
/// - non-sync builtins (they're fusion boundaries by definition)
/// - unstable-bound names (the M4g gate — a name that's a `<-`
///   target somewhere could be rebound to something other than the
///   builtin; baking a fused dispatch into stale state would be a
///   correctness bug)
/// - builtins with a fn-typed or otherwise non-`KirType`
///   argument/return (we don't know how to marshal them yet)
/// - builtins not in `ctx.fn_types` for their apply id (typecheck
///   ran but didn't record the resolved type — defensive)
/// - names that shadow a fn-typed kernel param or another
///   discovered builtin (first occurrence wins)
pub fn discover_builtin_fn_inputs<R: crate::Rt, E: crate::UserEvent>(
    body: &Expr,
    ctx: &crate::ExecCtx<R, E>,
) -> Vec<crate::kernel_ir::FnParam> {
    let mut out: Vec<crate::kernel_ir::FnParam> = Vec::new();
    // Keyed by (user_name, call_arity): variadic builtins called at
    // multiple arities register one FnParam per arity. Non-variadic
    // callees only ever appear at one arity so the dedup degenerates.
    let mut seen: std::collections::BTreeSet<(ArcStr, usize)> =
        std::collections::BTreeSet::new();
    walk_builtin_calls(body, ctx, &mut seen, &mut out);
    out
}

fn walk_builtin_calls<R: crate::Rt, E: crate::UserEvent>(
    expr: &Expr,
    ctx: &crate::ExecCtx<R, E>,
    seen: &mut std::collections::BTreeSet<(ArcStr, usize)>,
    out: &mut Vec<crate::kernel_ir::FnParam>,
) {
    if let ExprKind::Apply(a) = &expr.kind {
        if let Some(fp) = try_register_builtin_call(a, expr.id, ctx, seen) {
            out.push(fp);
        }
    }
    for_each_child(expr, &mut |c| {
        walk_builtin_calls(c, ctx, seen, out);
    });
}

/// Try to build a `FnParam` for a builtin call site. The four-step
/// chain — resolve user name → builtin shim → check sync → extract
/// signature with labeled-default reconciliation — produces `None`
/// at any miss. `seen` deduplicates per-`user_name` (one slot per
/// builtin per kernel; later call sites reuse).
fn try_register_builtin_call<R: crate::Rt, E: crate::UserEvent>(
    a: &crate::expr::ApplyExpr,
    _apply_id: crate::expr::ExprId,
    ctx: &crate::ExecCtx<R, E>,
    seen: &mut std::collections::BTreeSet<(ArcStr, usize)>,
) -> Option<crate::kernel_ir::FnParam> {
    let ExprKind::Ref { name } = &a.function.kind else { return None };
    let ident = ident_of(name)?;
    let user_name = ArcStr::from(ident);
    let call_arity = a.args.len();
    if seen.contains(&(user_name.clone(), call_arity)) {
        return None;
    }
    if ctx.unstable_bindings.contains(&user_name) {
        return None;
    }
    // Two-step builtin resolution: graphix stdlib declares
    // `let println = |...| ... 'core_println` — the user-facing name
    // `println` resolves through `ctx.fusion_lambdas` to a
    // `LambdaExpr` whose body is `Either::Right("core_println")`.
    // The actual registered builtin lives in `ctx.builtins[shim]`,
    // not under the user-facing name. The lambda's argspec also
    // carries any default expressions for labeled args, which we
    // capture into the layout for `pre_bind_builtin` to compile.
    let entry = ctx.fusion_lambdas.get(&user_name)?;
    let shim_name = match &entry.lambda.body {
        netidx::utils::Either::Right(shim) => shim.clone(),
        _ => return None,
    };
    if !ctx.builtins.contains_key(shim_name.as_str())
        || ctx.builtin_effect(shim_name.as_str()) != EffectKind::Sync
    {
        return None;
    }
    // Pull the call-site FnType off the function expression's typed
    // AST cell. `apply.function` is a Ref to the user-facing lambda
    // (`println`, `sum`, etc.); its `typ` carries `Type::Fn(_)` set
    // by the typechecker.
    let ft = resolved_fn_type(&a.function)?;
    let ft = &ft;
    // Reconciliation: walk the lambda's argspec and the call's
    // `a.args` together. For each formal slot:
    // - Positional formal → take next positional call arg, record
    //   its KirType in `arg_types`, emit `BuiltinSlot::Positional`
    // - Labeled formal with a default → emit
    //   `BuiltinSlot::LabeledDefault(expr)`
    // - Labeled formal without default → bail (the call had to
    //   supply it by name; we don't handle that yet — Step B v2)
    //
    // Reject the call entirely if it has any labeled arg (the v1
    // limitation).
    if a.args.iter().any(|(label, _)| label.is_some()) {
        return None;
    }
    let argspec = &entry.lambda.args;
    let mut layout: Vec<crate::kernel_ir::BuiltinSlot> =
        Vec::with_capacity(ft.args.len());
    let mut arg_types: Vec<KirType> = Vec::with_capacity(call_arity);
    let mut call_idx = 0usize;
    for (i, fa) in ft.args.iter().enumerate() {
        if fa.label().is_some() {
            // Labeled formal. Get the default expr from the lambda's
            // argspec (same index). Missing/explicit-None default
            // means the call had to supply it; bail in v1.
            let arg = argspec.get(i)?;
            let default_expr = match &arg.labeled {
                Some(Some(expr)) => expr.clone(),
                _ => return None,
            };
            layout.push(crate::kernel_ir::BuiltinSlot::LabeledDefault(
                default_expr,
            ));
        } else {
            // Positional formal: consume the next positional call arg.
            if call_idx >= call_arity {
                return None;
            }
            let kt = KirType::from_type(&fa.typ)?;
            arg_types.push(kt);
            layout.push(crate::kernel_ir::BuiltinSlot::Positional(call_idx));
            call_idx += 1;
        }
    }
    // Variadic tail: any remaining positional call args feed the
    // callee's `@args: T` slot. The Apply's own vargs handling
    // (in CallSite::bind) collects refs into the expected `Array<T>`,
    // so all we do here is declare one `Variadic` layout entry and
    // append `varg_type`-typed entries to `arg_types`. One `FnParam`
    // is registered per (name, call_arity), so different call sites
    // with different arities get distinct slots.
    if call_idx < call_arity {
        let varg_ty = ft.vargs.as_ref()?;
        let varg_kt = KirType::from_type(varg_ty)?;
        let count = call_arity - call_idx;
        layout.push(crate::kernel_ir::BuiltinSlot::Variadic {
            from_call_idx: call_idx,
            count,
        });
        for _ in 0..count {
            arg_types.push(varg_kt.clone());
        }
    }
    let return_type = KirType::from_type(&ft.rtype)?;
    seen.insert((user_name.clone(), call_arity));
    Some(crate::kernel_ir::FnParam {
        name: user_name,
        source: crate::kernel_ir::FnSource::Builtin {
            name: shim_name,
            typ: std::sync::Arc::new(ft.clone()),
            layout: layout.into(),
        },
        arg_types,
        return_type,
    })
}

/// Try to extract a primitive-only `(arg_types, return_type)` pair
/// from a function-typed `Type`. Used by both HOF-arg detection
/// (where the arg of the lambda is fn-typed) and Binding-source
/// detection (where a let-bound non-fusable callee's signature is
/// fn-typed). Returns `None` if the type isn't a function or if any
/// part of the signature is non-primitive / labeled / variadic.
pub fn extract_fn_signature(
    typ: &Type,
) -> Option<(Vec<KirType>, KirType)> {
    typ.with_deref(|resolved| match resolved? {
        Type::Fn(ft) => {
            let mut arg_types: Vec<KirType> =
                Vec::with_capacity(ft.args.len());
            for fa in ft.args.iter() {
                if fa.label().is_some() {
                    return None;
                }
                arg_types.push(KirType::from_type(&fa.typ)?);
            }
            if ft.vargs.is_some() {
                return None;
            }
            let return_type = KirType::from_type(&ft.rtype)?;
            Some((arg_types, return_type))
        }
        _ => None,
    })
}

// ─── Discovery & top-level program rewrite ───────────────────────

/// Walk an `Expr` and build a `FusionCtx` by resolving every bare-
/// identifier `Ref` against the env. Returns `None` if the subtree
/// contains anything the emitter can't handle — any compound `ModPath`
/// (`a::b`), any non-primitive bound type, or any unresolvable name.
pub fn discover_inputs(
    expr: &Expr,
    env: &Env,
    scope: &Scope,
) -> Option<FusionCtx> {
    fn walk(
        expr: &Expr,
        env: &Env,
        scope: &Scope,
        ctx: &mut FusionCtx,
    ) -> Option<()> {
        match &expr.kind {
            ExprKind::Ref { name } => {
                let ident = ident_of(name)?;
                if ctx.find(ident).is_some() {
                    return Some(());
                }
                let (_found_scope, bind) = env.lookup_bind(&scope.lexical, name)?;
                let prim = PrimType::from_type(&bind.typ)?;
                ctx.inputs.push(Input {
                    name: ArcStr::from(ident),
                    prim,
                    bind_id: Some(bind.id),
                });
                Some(())
            }
            ExprKind::Constant(_) => Some(()),
            ExprKind::ExplicitParens(e) => walk(e, env, scope, ctx),
            ExprKind::Add { lhs, rhs }
            | ExprKind::Sub { lhs, rhs }
            | ExprKind::Mul { lhs, rhs }
            | ExprKind::Div { lhs, rhs }
            | ExprKind::Mod { lhs, rhs }
            | ExprKind::Eq { lhs, rhs }
            | ExprKind::Ne { lhs, rhs }
            | ExprKind::Lt { lhs, rhs }
            | ExprKind::Gt { lhs, rhs }
            | ExprKind::Lte { lhs, rhs }
            | ExprKind::Gte { lhs, rhs }
            | ExprKind::And { lhs, rhs }
            | ExprKind::Or { lhs, rhs } => {
                walk(lhs, env, scope, ctx)?;
                walk(rhs, env, scope, ctx)
            }
            ExprKind::Not { expr } => walk(expr, env, scope, ctx),
            _ => None,
        }
    }
    let mut ctx = FusionCtx::default();
    walk(expr, env, scope, &mut ctx)?;
    Some(ctx)
}

/// Patch a lambda's argspec / rtype with primitive types resolved
/// from a `FnType` (typically the call-site resolved type from
/// CallSite::typecheck). Where the user wrote no annotation, fill
/// in from `ft`; user-provided annotations stay. Used by the runtime
/// deferred-fusion path (Lambda's InitFn at first call uses the call
/// site's resolved FnType to give previously-unfusable callbacks like
/// `|idx| ...` the typed argspec they need to fuse).
pub fn apply_fntype_to_lambda(
    lambda_arc: &mut triomphe::Arc<crate::expr::LambdaExpr>,
    ft: &crate::typ::FnType,
) {
    let needs_arg_patch = lambda_arc
        .args
        .iter()
        .zip(ft.args.iter())
        .any(|(a, ft_arg)| a.constraint.is_none() && PrimType::from_type(&ft_arg.typ).is_some());
    let needs_rtype_patch =
        lambda_arc.rtype.is_none() && PrimType::from_type(&ft.rtype).is_some();
    if !needs_arg_patch && !needs_rtype_patch {
        return;
    }
    let mut new_args: Vec<crate::expr::Arg> = lambda_arc.args.iter().cloned().collect();
    for (a, ft_arg) in new_args.iter_mut().zip(ft.args.iter()) {
        if a.constraint.is_some() {
            continue;
        }
        if let Some(prim) = PrimType::from_type(&ft_arg.typ) {
            a.constraint = Some(prim_type_to_graphix(prim));
        }
    }
    let rtype = if needs_rtype_patch {
        PrimType::from_type(&ft.rtype).map(prim_type_to_graphix)
    } else {
        lambda_arc.rtype.clone()
    };
    let new_lambda = crate::expr::LambdaExpr {
        args: triomphe::Arc::from_iter(new_args),
        vargs: lambda_arc.vargs.clone(),
        rtype,
        constraints: lambda_arc.constraints.clone(),
        throws: lambda_arc.throws.clone(),
        body: lambda_arc.body.clone(),
    };
    *lambda_arc = triomphe::Arc::new(new_lambda);
}

fn prim_type_to_graphix(p: PrimType) -> Type {
    let typ = match p {
        PrimType::I8 => netidx_value::Typ::I8,
        PrimType::I16 => netidx_value::Typ::I16,
        PrimType::I32 => netidx_value::Typ::I32,
        PrimType::I64 => netidx_value::Typ::I64,
        PrimType::U8 => netidx_value::Typ::U8,
        PrimType::U16 => netidx_value::Typ::U16,
        PrimType::U32 => netidx_value::Typ::U32,
        PrimType::U64 => netidx_value::Typ::U64,
        PrimType::F32 => netidx_value::Typ::F32,
        PrimType::F64 => netidx_value::Typ::F64,
        PrimType::Bool => netidx_value::Typ::Bool,
    };
    Type::Primitive(typ.into())
}

/// If `value` is a compile-time-computable primitive expression in
/// terms of already-known constants, record `(name, KnownConst)` into
/// `consts`. The KIR stored on the KnownConst is the full emitted
/// expression — the JIT folds it as appropriate at lowering.
///
/// Used by `Bind::compile` (the runtime path): the runtime fusion
/// attempts in `Lambda::compile` need const visibility for the
/// kernels they build.
pub fn record_const_binding(
    name: &ArcStr,
    value: &Expr,
    consts: &mut std::collections::BTreeMap<ArcStr, KnownConst>,
) {
    // Direct Constant is trivially constant.
    if let ExprKind::Constant(v) = &value.kind {
        if let Some(c) = ConstVal::from_value(v) {
            consts.insert(name.clone(), KnownConst { expr: kir::const_expr(c) });
            return;
        }
    }
    // Expression-valued bindings: try to emit them with only the
    // known-const registry visible (empty inputs). If emit_expr
    // succeeds, every Ref inside resolved to a constant — we can
    // freeze the whole expression.
    let probe = FusionCtx {
        inputs: vec![],
        fn_inputs: vec![],
        array_inputs: vec![],
        tuple_inputs: vec![],
        struct_inputs: vec![],
        variant_inputs: vec![],
        known_fns: std::collections::BTreeMap::new(),
        known_consts: consts.clone(),
    };
    if let Some(e) = emit_expr(value, &probe) {
        consts.insert(name.clone(), KnownConst { expr: e });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::{parser::parse_one, Expr, ExprKind};
    use netidx_value::Value;

    fn input(name: &str, prim: PrimType) -> Input {
        Input {
            name: ArcStr::from(name),
            prim,
            bind_id: None,
        }
    }

    fn ref_expr(name: &str) -> Expr {
        ExprKind::Ref { name: ModPath::from_iter([name]) }.to_expr_nopos()
    }

    fn f64c(x: f64) -> Expr {
        ExprKind::Constant(Value::F64(x)).to_expr_nopos()
    }

    fn bin<F>(lhs: Expr, rhs: Expr, f: F) -> Expr
    where
        F: FnOnce(triomphe::Arc<Expr>, triomphe::Arc<Expr>) -> ExprKind,
    {
        let kind = f(triomphe::Arc::new(lhs), triomphe::Arc::new(rhs));
        kind.to_expr_nopos()
    }

    #[test]
    fn emit_simple_arith() {
        // Mirrors mandelbrot's `zr * zr + zi * zi > 4.0`.
        let ctx = FusionCtx { inputs: vec![input("zr", PrimType::F64), input("zi", PrimType::F64)], ..Default::default() };
        let zr_sq = bin(ref_expr("zr"), ref_expr("zr"), |l, r| ExprKind::Mul { lhs: l, rhs: r });
        let zi_sq = bin(ref_expr("zi"), ref_expr("zi"), |l, r| ExprKind::Mul { lhs: l, rhs: r });
        let sum = bin(zr_sq, zi_sq, |l, r| ExprKind::Add { lhs: l, rhs: r });
        let escaped = bin(sum, f64c(4.0), |l, r| ExprKind::Gt { lhs: l, rhs: r });

        let e = emit_expr(&escaped, &ctx).expect("should fuse");
        assert_eq!(e.typ, KirType::Prim(PrimType::Bool));
    }

    #[test]
    fn reject_type_mismatch() {
        let ctx = FusionCtx { inputs: vec![input("a", PrimType::I64)], ..Default::default() };
        let bad = bin(ref_expr("a"), f64c(1.0), |l, r| ExprKind::Add { lhs: l, rhs: r });
        assert!(emit_expr(&bad, &ctx).is_none());
    }

    #[test]
    fn reject_unknown_ref() {
        let ctx = FusionCtx::default();
        let e = bin(ref_expr("x"), f64c(1.0), |l, r| ExprKind::Add { lhs: l, rhs: r });
        assert!(emit_expr(&e, &ctx).is_none());
    }

    fn parse_fuse(source: &str, ctx: &FusionCtx) -> Option<KirExpr> {
        let e = parse_one(source).expect("parse");
        emit_expr(&e, ctx)
    }

    #[test]
    fn parser_simple_arith() {
        let ctx = FusionCtx { inputs: vec![
                input("a", PrimType::I64),
                input("b", PrimType::I64),
                input("c", PrimType::I64),
            ], ..Default::default() };
        let e = parse_fuse("a + b * c", &ctx).expect("should fuse");
        assert_eq!(e.typ, KirType::Prim(PrimType::I64));
    }

    #[test]
    fn parser_mandelbrot_escape_test() {
        let ctx = FusionCtx { inputs: vec![input("zr", PrimType::F64), input("zi", PrimType::F64)], ..Default::default() };
        let e = parse_fuse("zr * zr + zi * zi > 4.0", &ctx).expect("should fuse");
        assert_eq!(e.typ, KirType::Prim(PrimType::Bool));
    }

    #[test]
    fn select_bool_literal_arms_fuse_cleanly() {
        let ctx = FusionCtx { inputs: vec![input("x", PrimType::F64)], ..Default::default() };
        let e = parse_one("select x > 0.0 { true => x, false => 0.0 - x }")
            .expect("parse");
        let e = emit_expr(&e, &ctx).expect("should fuse");
        assert_eq!(e.typ, KirType::Prim(PrimType::F64));
    }

    #[test]
    fn type_cast_and_qop_fuse() {
        let ctx = FusionCtx {
            inputs: vec![input("px", PrimType::I64), input("dx", PrimType::F64)],
            ..Default::default()
        };
        let e = parse_one("cast<f64>(px)$ * dx").expect("parse");
        let out = emit_expr(&e, &ctx).expect("should fuse");
        assert_eq!(out.typ, KirType::Prim(PrimType::F64));
    }

    #[test]
    fn cast_non_primitive_rejected() {
        let ctx = FusionCtx {
            inputs: vec![input("x", PrimType::Bool)],
            ..Default::default()
        };
        let e = parse_one("cast<i64>(x)").expect("parse");
        assert!(emit_expr(&e, &ctx).is_none());
    }

    /// `arr[i]` against an array param lowers to `KirOp::ArrayGet`,
    /// the result type is the element type.
    #[test]
    fn array_ref_lowering() {
        use crate::kernel_ir::ArrayInput;
        let ctx = FusionCtx {
            inputs: vec![input("i", PrimType::I64)],
            array_inputs: vec![ArrayInput {
                name: ArcStr::from("xs"),
                elem: KirType::Prim(PrimType::F64),
                bind_id: None,
            }],
            ..Default::default()
        };
        let e = parse_fuse("xs[i]", &ctx).expect("should fuse");
        assert_eq!(e.typ, KirType::Prim(PrimType::F64));
        match &e.op {
            KirOp::ArrayGet { name, .. } => assert_eq!(name.as_str(), "xs"),
            other => panic!("expected ArrayGet, got {other:?}"),
        }
    }

    /// `array::fold(arr, init, |acc: T, x: E| body)` lowers to
    /// `KirOp::ArrayFold` when arr is an array param, init emits as
    /// scalar matching the accumulator type, and the callback's two
    /// params have primitive type annotations matching the array's
    /// element type / accumulator type.
    #[test]
    fn array_fold_lowering() {
        use crate::kernel_ir::ArrayInput;
        let ctx = FusionCtx {
            array_inputs: vec![ArrayInput {
                name: ArcStr::from("xs"),
                elem: KirType::Prim(PrimType::F64),
                bind_id: None,
            }],
            ..Default::default()
        };
        let e = parse_fuse(
            "array::fold(xs, 0.0, |acc: f64, x: f64| acc + x)",
            &ctx,
        )
        .expect("should fuse");
        assert_eq!(e.typ, KirType::Prim(PrimType::F64));
        match &e.op {
            KirOp::ArrayFold { array, acc_local, elem_local, .. } => {
                assert_eq!(array.as_str(), "xs");
                assert_eq!(acc_local.as_str(), "acc");
                assert_eq!(elem_local.as_str(), "x");
            }
            other => panic!("expected ArrayFold, got {other:?}"),
        }
    }

    /// `array::init(n, |idx: i64| body)` lowers to `KirOp::ArrayInit`
    /// with `KirType::Array(<body elem>)` result.
    #[test]
    fn array_init_lowering() {
        let ctx = FusionCtx::default();
        let e = parse_fuse(
            "array::init(1000, |idx: i64| cast<f64>(idx)$ * 0.5)",
            &ctx,
        )
        .expect("should fuse");
        assert_eq!(e.typ, KirType::Array(Box::new(KirType::Prim(PrimType::F64))));
        match &e.op {
            KirOp::ArrayInit { idx_local, elem_typ, .. } => {
                assert_eq!(idx_local.as_str(), "idx");
                assert_eq!(*elem_typ, PrimType::F64);
            }
            other => panic!("expected ArrayInit, got {other:?}"),
        }
    }

    /// `array::map(arr, |x: f64| body)` against an array param lowers
    /// to `KirOp::ArrayMap` whose result type matches the body's elem
    /// type.
    #[test]
    fn array_map_lowering() {
        use crate::kernel_ir::ArrayInput;
        let ctx = FusionCtx {
            array_inputs: vec![ArrayInput {
                name: ArcStr::from("xs"),
                elem: KirType::Prim(PrimType::F64),
                bind_id: None,
            }],
            ..Default::default()
        };
        let e = parse_fuse("array::map(xs, |x: f64| x * 2.0)", &ctx)
            .expect("should fuse");
        assert_eq!(e.typ, KirType::Array(Box::new(KirType::Prim(PrimType::F64))));
        match &e.op {
            KirOp::ArrayMap { array, in_elem, out_elem, elem_local, .. } => {
                assert_eq!(array.as_str(), "xs");
                assert_eq!(*in_elem, PrimType::F64);
                assert_eq!(*out_elem, PrimType::F64);
                assert_eq!(elem_local.as_str(), "x");
            }
            other => panic!("expected ArrayMap, got {other:?}"),
        }
    }

    /// `` `Foo(1, 2.5) `` literal lowers to `KirOp::VariantNew`
    /// with payload types `[I64, F64]`. Result type wraps the case
    /// in `KirType::Variant(vec![(tag, [I64, F64])])`.
    #[test]
    fn variant_new_lowering() {
        let ctx = FusionCtx::default();
        let e = parse_fuse("`Foo(1, 2.5)", &ctx).expect("should fuse");
        match &e.typ {
            KirType::Variant(cases) => {
                assert_eq!(cases.len(), 1);
                assert_eq!(cases[0].0.as_str(), "Foo");
                assert_eq!(cases[0].1, vec![KirType::Prim(PrimType::I64), KirType::Prim(PrimType::F64)]);
            }
            other => panic!("expected Variant type, got {other:?}"),
        }
        match &e.op {
            KirOp::VariantNew { tag, payload_types, .. } => {
                assert_eq!(tag.as_str(), "Foo");
                assert_eq!(*payload_types, vec![PrimType::I64, PrimType::F64]);
            }
            other => panic!("expected VariantNew, got {other:?}"),
        }
    }

    /// Nullary variant construction (`` `Red ``) — the variant's
    /// result type includes the nullary case with an empty payload
    /// list.
    #[test]
    fn nullary_variant_construction() {
        let ctx = FusionCtx::default();
        let e = parse_fuse("`Red", &ctx).expect("should fuse");
        match &e.typ {
            KirType::Variant(cases) => {
                assert_eq!(cases.len(), 1);
                assert_eq!(cases[0].0.as_str(), "Red");
                assert!(cases[0].1.is_empty(), "nullary case: no payloads");
            }
            other => panic!("expected Variant type, got {other:?}"),
        }
        match &e.op {
            KirOp::VariantNew { tag, payloads, .. } => {
                assert_eq!(tag.as_str(), "Red");
                assert!(payloads.is_empty(), "nullary VariantNew has no payloads");
            }
            other => panic!("expected VariantNew, got {other:?}"),
        }
    }

    /// Mixed nullary + with-payload variant pattern match.
    /// Validates that the variant param boundary handles both shapes —
    /// `` `Red `` (nullary) and `` `Rgb(r, g, b) `` (with-payload).
    #[test]
    fn mixed_variant_pattern_match() {
        use crate::kernel_ir::VariantInput;
        let ctx = FusionCtx {
            variant_inputs: vec![VariantInput {
                name: ArcStr::from("c"),
                cases: vec![
                    (ArcStr::from("Red"), vec![]),
                    (ArcStr::from("Green"), vec![]),
                    (ArcStr::from("Rgb"), vec![KirType::Prim(PrimType::I64), KirType::Prim(PrimType::I64), KirType::Prim(PrimType::I64)]),
                ],
                bind_id: None,
            }],
            ..Default::default()
        };
        let e = parse_fuse(
            "select c { `Red => 255, `Green => 128, `Rgb(r, g, b) => r + g + b }",
            &ctx,
        )
        .expect("should fuse");
        assert_eq!(e.typ, KirType::Prim(PrimType::I64));
    }

    /// Pattern match on a variant param: `select v { \`Foo(a, b) => a + b, ... }`.
    /// Lowers to a `KirOp::VariantTagEq` condition and the payload
    /// bindings resolve through the synthetic `VariantPayload` ops.
    #[test]
    fn variant_pattern_match_lowering() {
        use crate::kernel_ir::VariantInput;
        let ctx = FusionCtx {
            variant_inputs: vec![VariantInput {
                name: ArcStr::from("v"),
                cases: vec![
                    (ArcStr::from("Foo"), vec![KirType::Prim(PrimType::I64), KirType::Prim(PrimType::I64)]),
                    (ArcStr::from("Bar"), vec![KirType::Prim(PrimType::F64)]),
                ],
                bind_id: None,
            }],
            ..Default::default()
        };
        let e = parse_fuse(
            "select v { `Foo(a, b) => a + b, `Bar(x) => cast<i64>(x)$ }",
            &ctx,
        )
        .expect("should fuse");
        assert_eq!(e.typ, KirType::Prim(PrimType::I64));
    }

    /// Structural sanity-check on [`for_each_child`] — the
    /// foundation of the whole-graph analyzer's walks. Counts
    /// `Apply` sites in small ASTs without ever building an
    /// `ExecCtx`; if `for_each_child` mis-handles a variant, the
    /// counts here drift.
    #[test]
    fn for_each_child_visits_apply_sites() {
        use crate::expr::parser::parse_one;
        fn count_apply(e: &Expr) -> usize {
            let mut n =
                if matches!(e.kind, ExprKind::Apply(_)) { 1 } else { 0 };
            for_each_child(e, &mut |c| n += count_apply(c));
            n
        }
        assert_eq!(count_apply(&parse_one("1 + 2").expect("p")), 0);
        assert_eq!(count_apply(&parse_one("f(1)").expect("p")), 1);
        assert_eq!(count_apply(&parse_one("f(g(1), h(2))").expect("p")), 3);
        // Lambda bodies are a separate fusion scope — their internal
        // Applies belong to the lambda's own kernel, not the
        // enclosing one. `for_each_child` does not descend into them.
        assert_eq!(count_apply(&parse_one("|x| f(g(x))").expect("p")), 0);
        // Let-binding: count applies in the value.
        assert_eq!(
            count_apply(&parse_one("let x = f(1) + g(2)").expect("p")),
            2
        );
    }

    /// Tuple-literal `(1, 3.14, true)` lowers to `KirOp::TupleNew`
    /// with `KirType::Tuple([I64, F64, Bool])` result.
    #[test]
    fn tuple_new_lowering() {
        let ctx = FusionCtx::default();
        let e = parse_fuse("(1, 3.14, true)", &ctx).expect("should fuse");
        assert_eq!(
            e.typ,
            KirType::Tuple(vec![KirType::Prim(PrimType::I64), KirType::Prim(PrimType::F64), KirType::Prim(PrimType::Bool)])
        );
        match &e.op {
            KirOp::TupleNew { fields, elem_types } => {
                assert_eq!(fields.len(), 3);
                assert_eq!(
                    *elem_types,
                    vec![PrimType::I64, PrimType::F64, PrimType::Bool]
                );
            }
            other => panic!("expected TupleNew, got {other:?}"),
        }
    }

    /// Struct literal `{x: 1.0, y: 2.0}` lowers to `KirOp::StructNew`
    /// with fields canonicalized in alphabetical order.
    #[test]
    fn struct_new_lowering() {
        let ctx = FusionCtx::default();
        // Source order y,x but canonical layout is x,y (alphabetical).
        let e = parse_fuse("{y: 2.0, x: 1.0}", &ctx).expect("should fuse");
        match &e.typ {
            KirType::Struct(fields) => {
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].0.as_str(), "x");
                assert_eq!(fields[0].1, KirType::Prim(PrimType::F64));
                assert_eq!(fields[1].0.as_str(), "y");
                assert_eq!(fields[1].1, KirType::Prim(PrimType::F64));
            }
            other => panic!("expected Struct type, got {other:?}"),
        }
        match &e.op {
            KirOp::StructNew { sorted_fields, sorted_types } => {
                // Sorted fields land in alphabetical order regardless
                // of how the user wrote them.
                assert_eq!(sorted_fields[0].0.as_str(), "x");
                assert_eq!(sorted_fields[1].0.as_str(), "y");
                assert_eq!(sorted_types[0].1, PrimType::F64);
                assert_eq!(sorted_types[1].1, PrimType::F64);
            }
            other => panic!("expected StructNew, got {other:?}"),
        }
    }

    /// `s.field` against a struct kernel param lowers to
    /// `KirOp::StructGet`. The field name resolves to its
    /// alphabetical sort index at compile time.
    #[test]
    fn struct_ref_lowering() {
        use crate::kernel_ir::StructInput;
        // Struct param with two F64 fields. The fusion ctx stores
        // them already canonically sorted (x before y).
        let ctx = FusionCtx {
            struct_inputs: vec![StructInput {
                name: ArcStr::from("p"),
                fields: vec![
                    (ArcStr::from("x"), KirType::Prim(PrimType::F64)),
                    (ArcStr::from("y"), KirType::Prim(PrimType::F64)),
                ],
                bind_id: None,
            }],
            ..Default::default()
        };
        let e = parse_fuse("p.y", &ctx).expect("should fuse");
        assert_eq!(e.typ, KirType::Prim(PrimType::F64));
        match &e.op {
            KirOp::StructGet { name, field, sorted_idx, elem_typ } => {
                assert_eq!(name.as_str(), "p");
                assert_eq!(field.as_str(), "y");
                // y is at sorted index 1 (after x).
                assert_eq!(*sorted_idx, 1usize);
                assert_eq!(*elem_typ, KirType::Prim(PrimType::F64));
            }
            other => panic!("expected StructGet, got {other:?}"),
        }
    }

    /// `t.0` against a tuple kernel param lowers to
    /// `KirOp::TupleGet` with the slot's primitive type.
    #[test]
    fn tuple_ref_lowering() {
        use crate::kernel_ir::TupleInput;
        let ctx = FusionCtx {
            tuple_inputs: vec![TupleInput {
                name: ArcStr::from("p"),
                elems: vec![KirType::Prim(PrimType::F64), KirType::Prim(PrimType::F64)],
                bind_id: None,
            }],
            ..Default::default()
        };
        let e = parse_fuse("p.1", &ctx).expect("should fuse");
        assert_eq!(e.typ, KirType::Prim(PrimType::F64));
        match &e.op {
            KirOp::TupleGet { name, idx, elem_typ } => {
                assert_eq!(name.as_str(), "p");
                assert_eq!(*idx, 1usize);
                assert_eq!(*elem_typ, KirType::Prim(PrimType::F64));
            }
            other => panic!("expected TupleGet, got {other:?}"),
        }
    }

    /// `array::filter(arr, |x: f64| x > 0.0)` against an array param
    /// lowers to `KirOp::ArrayFilter` preserving the element type.
    #[test]
    fn array_filter_lowering() {
        use crate::kernel_ir::ArrayInput;
        let ctx = FusionCtx {
            array_inputs: vec![ArrayInput {
                name: ArcStr::from("xs"),
                elem: KirType::Prim(PrimType::F64),
                bind_id: None,
            }],
            ..Default::default()
        };
        let e = parse_fuse("array::filter(xs, |x: f64| x > 0.0)", &ctx)
            .expect("should fuse");
        assert_eq!(e.typ, KirType::Array(Box::new(KirType::Prim(PrimType::F64))));
        match &e.op {
            KirOp::ArrayFilter { array, elem, elem_local, .. } => {
                assert_eq!(array.as_str(), "xs");
                assert_eq!(*elem, PrimType::F64);
                assert_eq!(elem_local.as_str(), "x");
            }
            other => panic!("expected ArrayFilter, got {other:?}"),
        }
    }

    /// `array::len(arr)` against an array param lowers to
    /// `KirOp::ArrayLen` with `u64` result type.
    #[test]
    fn array_len_lowering() {
        use crate::kernel_ir::ArrayInput;
        let ctx = FusionCtx {
            array_inputs: vec![ArrayInput {
                name: ArcStr::from("xs"),
                elem: KirType::Prim(PrimType::F64),
                bind_id: None,
            }],
            ..Default::default()
        };
        let e = parse_fuse("array::len(xs)", &ctx).expect("should fuse");
        assert_eq!(e.typ, KirType::Prim(PrimType::U64));
        match &e.op {
            KirOp::ArrayLen { name } => assert_eq!(name.as_str(), "xs"),
            other => panic!("expected ArrayLen, got {other:?}"),
        }
    }

    /// Build a kernel from a synthetic region — a single sync
    /// expression with one scalar free variable input. Exercises
    /// `build_kir_kernel_from_region`'s param-derivation and the
    /// shared `finish_kernel` path.
    #[test]
    fn region_kernel_scalar_input() {
        let e = parse_one("x + 1").expect("parse");
        let inputs = [RegionInput {
            expr_id: e.id,
            name: ArcStr::from("x"),
            kind: RegionInputKind::Prim(PrimType::I64),
        }];
        let (kernel, sig) = build_kir_kernel_from_region(
            "test_scalar",
            &e,
            &inputs,
            &[],
            Some(KirType::Prim(PrimType::I64)),
            &Default::default(),
            &Default::default(),
        )
        .expect("region builds");
        assert_eq!(sig.arg_types, vec![KirType::Prim(PrimType::I64)]);
        assert_eq!(sig.return_type, KirType::Prim(PrimType::I64));
        assert_eq!(kernel.params.len(), 1);
        assert_eq!(kernel.params[0].name.as_str(), "x");
        assert_eq!(kernel.params[0].prim, PrimType::I64);
        // Regions never tail-loop and never have fn_params, but the
        // input list populates `tail_call_slots` so the runtime
        // arg-routing map (`build_arg_layout`) sees every input.
        assert!(!kernel.has_tail_loop);
        assert!(kernel.fn_params.is_empty());
        assert_eq!(kernel.tail_call_slots.len(), 1);
        assert_eq!(kernel.tail_call_slots[0].name.as_str(), "x");
    }

    /// A region whose input is a tuple — verifies the
    /// `RegionInputKind::Tuple` path routes through `ctx.tuple_inputs`
    /// so `TupleGet` lowering resolves.
    #[test]
    fn region_kernel_tuple_input() {
        let e = parse_one("p.0 + p.1").expect("parse");
        let inputs = [RegionInput {
            expr_id: e.id,
            name: ArcStr::from("p"),
            kind: RegionInputKind::Tuple(vec![KirType::Prim(PrimType::I64), KirType::Prim(PrimType::I64)]),
        }];
        let (kernel, sig) = build_kir_kernel_from_region(
            "test_tuple",
            &e,
            &inputs,
            &[],
            Some(KirType::Prim(PrimType::I64)),
            &Default::default(),
            &Default::default(),
        )
        .expect("region builds");
        assert_eq!(
            sig.arg_types,
            vec![KirType::Tuple(vec![KirType::Prim(PrimType::I64), KirType::Prim(PrimType::I64)])]
        );
        assert_eq!(kernel.tuple_params.len(), 1);
        assert_eq!(kernel.tuple_params[0].name.as_str(), "p");
    }

    /// A region with no free variables — `1 + 2`, all literals. The
    /// kernel has zero params and the signature is `() -> i64`.
    #[test]
    fn region_kernel_no_inputs() {
        let e = parse_one("1 + 2").expect("parse");
        let (kernel, sig) = build_kir_kernel_from_region(
            "test_const",
            &e,
            &[],
            &[],
            Some(KirType::Prim(PrimType::I64)),
            &Default::default(),
            &Default::default(),
        )
        .expect("region builds");
        assert!(sig.arg_types.is_empty());
        assert_eq!(sig.return_type, KirType::Prim(PrimType::I64));
        assert!(kernel.params.is_empty());
    }

    /// A region body referencing a name not in `inputs` must fail
    /// to build — `emit_expr` returns None for an unresolved `Ref`
    /// and the build propagates that.
    #[test]
    fn region_kernel_unknown_ref_fails() {
        let e = parse_one("y + 1").expect("parse");
        // No input named "y", so the body's `Ref(y)` can't resolve.
        assert!(
            build_kir_kernel_from_region(
                "test_bad",
                &e,
                &[],
                &[],
                Some(KirType::Prim(PrimType::I64)),
                &Default::default(),
                &Default::default(),
            )
            .is_none()
        );
    }
}

