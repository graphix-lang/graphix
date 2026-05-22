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
    /// Nullable-typed kernel parameters / lets — graphix's `[T, null]`
    /// option shape. `KirOp::IsNull` checks against these; reads via
    /// `KirOp::Local` return `EvalResult::Nullable(Value)` so the
    /// caller can distinguish `Value::Null` from a wrapped `T`.
    pub nullable_inputs: Vec<crate::kernel_ir::NullableInput>,
    /// String let-bindings inside the kernel body. Strings only appear
    /// here as locals — never as params (no string args on either
    /// backend) and never as kernel inputs from an outer region (no
    /// `RegionInputKind::String` shape).
    pub string_inputs: Vec<crate::kernel_ir::StringInput>,
    /// Other kernels already fused in the current pass. Used by
    /// `emit_expr` to lower `Apply { fn: Ref(name) }` to a direct call
    /// when `name` is in this map. Keyed by the Graphix-level name so
    /// rewrites done earlier in the walk are visible to later fusions.
    pub known_fns: std::collections::BTreeMap<ArcStr, KnownFusedFn>,
    /// Compile-time-known primitive constants, inlined at Ref sites.
    /// Populated by the rewrite pass from `let <name> = <literal>;`
    /// bindings. Keyed by Graphix-level name.
    pub known_consts: std::collections::BTreeMap<ArcStr, KnownConst>,
    /// Maximal-fusion *lifted-input* table: maps an `ExprId` inside the
    /// region body to the synthetic name of the corresponding kernel
    /// input. When `emit_expr` encounters an ExprId in this table it
    /// short-circuits the normal lowering and emits a `Local` read of
    /// the named input (mirroring what an `ExprKind::Ref` would do).
    /// The named input has already been registered into one of the
    /// `*_inputs` lists by `populate_kernel_inputs`.
    ///
    /// Populated by `discover_region_inputs` when the carving phase
    /// promotes an Async sub-expression to a kernel input
    /// ([`RegionInput`] with [`RegionInputSource::Lifted`]). Empty for
    /// non-region kernel builds (lambdas, the legacy lazy path).
    pub lifted_inputs: nohash::IntMap<crate::expr::ExprId, ArcStr>,
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

    /// Look up a nullable-typed kernel parameter / let-binding by name.
    pub fn find_nullable(
        &self,
        name: &str,
    ) -> Option<&crate::kernel_ir::NullableInput> {
        self.nullable_inputs.iter().find(|n| &*n.name == name)
    }

    /// Look up a string-typed let-binding by name.
    pub fn find_string(
        &self,
        name: &str,
    ) -> Option<&crate::kernel_ir::StringInput> {
        self.string_inputs.iter().find(|s| &*s.name == name)
    }

    /// Look up an input by name in any of the input lists (mirrors the
    /// per-shape dispatch in the `ExprKind::Ref` arm of `emit_expr`).
    /// Used by the maximal-fusion lifted-input path: when a lifted
    /// `ExprId` triggers a synthetic Local read, this resolves the
    /// synth name to the registered input slot.
    pub fn lookup_local(&self, name: &str) -> Option<KirExpr> {
        if let Some(input) = self.find(name) {
            return Some(kir::local(input.name.clone(), input.prim));
        }
        if let Some(ai) = self.find_array(name) {
            return Some(KirExpr {
                op: KirOp::Local(ai.name.clone()),
                typ: KirType::Array(Box::new(ai.elem.clone())),
            });
        }
        if let Some(ti) = self.find_tuple(name) {
            return Some(KirExpr {
                op: KirOp::Local(ti.name.clone()),
                typ: KirType::Tuple(ti.elems.clone()),
            });
        }
        if let Some(si) = self.find_struct(name) {
            return Some(KirExpr {
                op: KirOp::Local(si.name.clone()),
                typ: KirType::Struct(si.fields.clone()),
            });
        }
        if let Some(vi) = self.find_variant(name) {
            return Some(KirExpr {
                op: KirOp::Local(vi.name.clone()),
                typ: KirType::Variant(vi.cases.clone()),
            });
        }
        if let Some(ni) = self.find_nullable(name) {
            return Some(KirExpr {
                op: KirOp::Local(ni.name.clone()),
                typ: KirType::Nullable(Box::new(ni.elem.clone())),
            });
        }
        if let Some(si) = self.find_string(name) {
            return Some(KirExpr {
                op: KirOp::Local(si.name.clone()),
                typ: KirType::String,
            });
        }
        None
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
        let type_cond = emit_type_predicate_cond(&scrut, &pat.type_predicate)?;
        let struct_cond =
            emit_arm_condition(&scrut, &pat.structure_predicate, &mut arm_ctx)?;
        let cond = combine_cond_and_guard(type_cond, struct_cond);
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
        let combined = combine_cond_and_guard(cond, guard_kir);
        let body_kir = emit_expr(body, &arm_ctx)?;
        unified_typ = Some(match unified_typ {
            Some(prev) => unify_arm_types(prev, body_kir.typ.clone())?,
            None => body_kir.typ.clone(),
        });
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

/// Compute the IfChain merge type when arms produced different
/// `KirType`s. Currently the only narrowing this understands is the
/// `[T, null]` widening — `Prim(T) ∪ Null → Nullable(Prim(T))`,
/// `Nullable<U> ∪ Null → Nullable<U>`, `Nullable<U> ∪ Prim(T) →
/// Nullable<U>` when `T == U`. Anything else returns `None` (the
/// caller bails on fusion).
fn unify_arm_types(a: KirType, b: KirType) -> Option<KirType> {
    if a == b {
        return Some(a);
    }
    let nullable_of = |t: KirType| KirType::Nullable(Box::new(t));
    match (a, b) {
        (KirType::Null, KirType::Null) => Some(KirType::Null),
        (KirType::Null, other) | (other, KirType::Null) => match other {
            KirType::Nullable(_) => Some(other),
            t @ (KirType::Prim(_)
            | KirType::Array(_)
            | KirType::Tuple(_)
            | KirType::Struct(_)
            | KirType::Variant(_)) => Some(nullable_of(t)),
            _ => None,
        },
        (KirType::Nullable(inner), other) | (other, KirType::Nullable(inner)) => {
            if *inner == other {
                Some(KirType::Nullable(inner))
            } else {
                None
            }
        }
        _ => None,
    }
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
    let mut payload_types: Vec<KirType> = Vec::with_capacity(args.len());
    for a in args {
        let e = emit_expr(a, ctx)?;
        // Composite-element lowering: any KirType is a valid
        // payload shape (matches TupleNew / StructNew). Unit and
        // bare Null aren't useful payload types — Unit has no Value
        // form, and Null only appears via the inline ConstNull
        // path (always widened to Nullable<T> at the construction
        // site).
        if matches!(e.typ, KirType::Unit | KirType::Null) {
            return None;
        }
        payload_types.push(e.typ.clone());
        payloads.push(e);
    }
    let typ = KirType::Variant(vec![(tag.clone(), payload_types.clone())]);
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
    let mut elem_types: Vec<KirType> = Vec::with_capacity(args.len());
    for a in args {
        let e = emit_expr(a, ctx)?;
        // Composite-element lowering: any KirType (Prim, Array,
        // Tuple, Struct, Variant, Nullable, String) is accepted as a
        // tuple field. The interpreter / JIT producer codegen
        // dispatches by field kind when packing each value into the
        // outer ValArray. The leaf type `Unit` isn't a valid field
        // (no useful Value representation) and gets rejected here.
        if matches!(e.typ, KirType::Unit | KirType::Null) {
            return None;
        }
        elem_types.push(e.typ.clone());
        fields.push(e);
    }
    let typ = KirType::Tuple(elem_types.clone());
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
    let mut sorted_types: Vec<(ArcStr, KirType)> =
        Vec::with_capacity(sorted_pairs.len());
    for (n, e) in sorted_pairs {
        let kir = emit_expr(e, ctx)?;
        // Composite-element lowering: any KirType is a valid field
        // shape (matches TupleNew). Unit / bare Null are rejected
        // — neither has a useful Value runtime representation.
        if matches!(kir.typ, KirType::Unit | KirType::Null) {
            return None;
        }
        sorted_types.push((n.clone(), kir.typ.clone()));
        sorted_fields.push((n, kir));
    }
    let typ = KirType::Struct(sorted_types.clone());
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
        KirType::Nullable(elem) => {
            ctx.nullable_inputs.push(crate::kernel_ir::NullableInput {
                name: name.clone(),
                elem: (**elem).clone(),
                bind_id: None,
            });
        }
        KirType::String => {
            ctx.string_inputs.push(crate::kernel_ir::StringInput {
                name: name.clone(),
                bind_id: None,
            });
        }
        // Unit isn't usefully bindable as a kernel local (the caller
        // has already routed it to `KirStmt::Discard`). Bare `Null`
        // doesn't bind either (it would always be widened to
        // `Nullable<T>` first at the construction site).
        KirType::Unit | KirType::Null => return None,
    }
    Some(())
}

/// Emit a sub-expression as a [`KirExpr`]. Returns `None` if any sub-
/// tree is something the emitter doesn't handle yet — that short-
/// circuits the whole parent, so the caller falls back to the
/// interpreted path (or, in AOT mode, refuses fusion).
pub fn emit_expr(expr: &Expr, ctx: &FusionCtx) -> Option<KirExpr> {
    // Maximal-fusion intercept: if this ExprId has been promoted to a
    // kernel input by `discover_region_inputs`, replace the normal
    // lowering with a Local read of the synthetic input name. The
    // runtime feeds the actual value via a separately-compiled Node
    // (`FusedRegion::from_subgraph` arg_node), so the kernel itself
    // sees only the input slot — no recursion into the lifted sub-tree.
    if let Some(name) = ctx.lifted_inputs.get(&expr.id) {
        return ctx.lookup_local(name);
    }
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
            if matches!(v, Value::Null) {
                return Some(KirExpr {
                    op: KirOp::ConstNull,
                    typ: KirType::Null,
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
            // Prefer lambda-arg / let-bound locals (any shape), then
            // fall back to the known-constants registry (outer-scope
            // `let x = <lit>;` bindings inlined at compile time).
            if let Some(expr) = ctx.lookup_local(ident) {
                return Some(expr);
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
    let type_cond = emit_type_predicate_cond(scrut, &pat.type_predicate)?;
    let struct_cond =
        emit_arm_condition(scrut, &pat.structure_predicate, &mut arm_ctx)?;
    let cond = combine_cond_and_guard(type_cond, struct_cond);
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

/// Emit the condition expression for an arm's type predicate.
/// Returns `Ok(None)` when the type predicate is absent (no narrowing)
/// or trivially matches; `Ok(Some(...))` for a real test; and the outer
/// `None` when the predicate isn't expressible in KIR (so the arm
/// can't fuse).
///
/// Recognised narrowings:
/// - `Type::Primitive(Typ::Null)` over a `Null` / `Nullable<T>`
///   scrutinee → `KirOp::IsNull(scrut)`.
/// - Any non-null predicate over a scrutinee whose KirType is
///   `KirType::Prim` and which carries the same primitive (trivially
///   always-true narrowing) → no condition needed.
/// - Any non-null predicate over a `Nullable<T>` scrutinee → no
///   condition needed (the surrounding select is expected to have
///   already handled the null arm via `IsNull`, leaving this arm to
///   cover the non-null exhaustion case — typecheck's exhaustiveness
///   check is what makes this sound).
///
/// **Outer-`None` (refuse to fuse) for anything else.** In particular
/// a multi-branch union like `[i64, string, bool]` with type-tag arms
/// can't be lowered: KIR has no runtime tag check for primitive
/// shapes, so emitting "always-matches" for an `i64 as n` arm would
/// silently bind `n` to a string at runtime. The defensive bail-out
/// pushes such a select to the interpreter (which has the runtime
/// type dispatch).
fn emit_type_predicate_cond(
    scrut: &KirExpr,
    pred: &Option<Type>,
) -> Option<Option<KirExpr>> {
    let Some(t) = pred.as_ref() else {
        return Some(None);
    };
    match t {
        Type::Primitive(p)
            if p.contains(netidx_value::Typ::Null)
                && p.iter().count() == 1 =>
        {
            // `null as _` or bare `null` pattern — emit IsNull on the
            // scrutinee. Scrutinee type must be Null or Nullable; for
            // anything else the predicate could never match and we
            // bail (typecheck should have rejected it anyway).
            match &scrut.typ {
                KirType::Null | KirType::Nullable(_) => Some(Some(KirExpr {
                    op: KirOp::IsNull(Box::new(scrut.clone())),
                    typ: KirType::Prim(PrimType::Bool),
                })),
                _ => None,
            }
        }
        Type::Primitive(p)
            if !p.contains(netidx_value::Typ::Null)
                && p.iter().count() == 1 =>
        {
            // Single non-null primitive predicate. Two cases are
            // soundly always-true: (a) the scrutinee is a matching
            // Prim (trivial), and (b) the scrutinee is `Nullable<T>`
            // and the surrounding select's null arm has already
            // filtered out `null` (exhaustiveness leaves this arm
            // to cover the non-null half).
            let pred_typ = p.iter().next();
            match (&scrut.typ, pred_typ) {
                (KirType::Prim(sp), Some(pt))
                    if PrimType::from_typ(pt) == Some(*sp) =>
                {
                    Some(None)
                }
                (KirType::Nullable(_), _) => Some(None),
                // Any other shape (e.g. a wider union scrutinee or
                // a mismatched Prim) we can't lower soundly. Refuse
                // to fuse — the surrounding kernel build bails and
                // the select runs on the interpreter.
                _ => None,
            }
        }
        // Multi-bit predicates (compound `[i64, null]`-shaped) and
        // composite type predicates aren't expressible in KIR yet;
        // refuse to fuse.
        _ => None,
    }
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


/// One kernel input for a region — a value flowing from outside the
/// region into its kernel. Two kinds, distinguished by [`source`]:
///
/// - `Binding`: a free-variable `Ref` in the region body resolving to
///   a binding defined outside the region. The runtime feeds this
///   input via a freshly-compiled `Ref` Node subscribing to the
///   binding's `BindId`.
/// - `Lifted(Expr)`: an Async sub-expression promoted to a kernel
///   input by the maximal-fusion carving phase. The runtime feeds
///   this input by compiling the lifted `Expr` as a standalone Node
///   via the regular node-compilation pipeline.
#[derive(Debug, Clone)]
pub struct RegionInput {
    /// `ExprId` of the cross-edge sub-expression whose value feeds
    /// this slot — either the `Ref` site (binding source) or the
    /// lifted Async sub-expression's root (lifted source). The
    /// runtime splice step uses this to wire the input.
    pub expr_id: crate::expr::ExprId,
    /// The name the kernel body uses to refer to this slot. For a
    /// binding input it is the binding's own name (so the normal
    /// `Ref` lowering in `emit_expr` resolves to the slot without
    /// rewriting the region body). For a lifted input it is a
    /// synthetic `__lifted_<expr_id>` name that the lifted-input
    /// intercept at the top of `emit_expr` matches against
    /// [`FusionCtx::lifted_inputs`].
    pub name: ArcStr,
    /// Slot kind — drives which [`FusionCtx`] input list the slot
    /// lands in and the kernel param's [`KirType`].
    pub kind: RegionInputKind,
    /// How the runtime feeds this slot: either a binding subscription
    /// or a freshly-compiled standalone Node for the lifted Expr.
    pub source: RegionInputSource,
}

/// Where a region input's value comes from at runtime. Drives
/// [`crate::node::region::FusedRegion::from_subgraph`]'s arg-node
/// construction.
#[derive(Debug, Clone)]
pub enum RegionInputSource {
    /// Subscribe to the binding named on the carrier `RegionInput`.
    /// The runtime compiles a synthetic `ExprKind::Ref` feeder, same
    /// as today's initial-model behavior.
    Binding,
    /// Compile the carried Expr as a standalone Node and feed its
    /// output. Used for Async sub-expressions promoted to inputs by
    /// the maximal-fusion carving phase.
    Lifted(Expr),
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
    /// `[T, null]` option shape — runtime representation is a `Value`
    /// that is either `Value::Null` or `T`'s form. `inner` is the
    /// non-null element type.
    Nullable(KirType),
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
    nullable_params: Vec<crate::kernel_ir::NullableInput>,
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
        // Maximal-fusion: register lifted-input ExprId → synth-name
        // mapping so `emit_expr`'s top-level intercept matches when
        // it encounters the lifted ExprId in the region body.
        if let RegionInputSource::Lifted(expr) = &input.source {
            ctx.lifted_inputs.insert(expr.id, input.name.clone());
        }
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
            RegionInputKind::Nullable(elem) => {
                let ni = crate::kernel_ir::NullableInput {
                    name: input.name.clone(),
                    elem: elem.clone(),
                    bind_id: None,
                };
                p.nullable_params.push(ni.clone());
                ctx.nullable_inputs.push(ni);
                p.arg_types.push(KirType::Nullable(Box::new(elem.clone())));
                tail_call_slots.push(crate::kernel_ir::TailCallSlot {
                    name: input.name.clone(),
                    kind: crate::kernel_ir::TailCallSlotKind::Nullable,
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
        nullable_inputs: vec![],
        string_inputs: vec![],
        known_fns: known.clone(),
        known_consts: consts.clone(),
        lifted_inputs: nohash::IntMap::default(),
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
        nullable_params: params.nullable_params,
        tail_call_slots,
        return_type,
        has_tail_loop,
        body: body_stmts,
    };
    Some((kernel, signature))
}



/// Pull the resolved [`FnType`](crate::typ::FnType) off an expression
/// whose typed-AST cell carries `Type::Fn(_)`. Used to look up the
/// call-site FnType for an `Apply` via `a.function.typ` — the typed
/// AST is the only source of resolved-FnType truth (the legacy
/// `ctx.fn_types` sidecar was removed in Refactor Phase 5). The
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
        RegionInputKind::Nullable(elem) => KirType::Nullable(Box::new(elem.clone())),
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
/// the caller already knows it (e.g. for an `Apply` region root, from
/// the call-site's resolved FnType via
/// `resolved_fn_type(&apply.function).rtype`); pass `None` to have
/// this function infer it via [`infer_body_rtype`] over the region
/// body. Inference failure yields `None`.
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
        KirType::Nullable(elem) => Some(RegionInputKind::Nullable((*elem).clone())),
        // `KirType::Prim` is handled above via the dedicated extractor.
        KirType::Prim(_) => None,
        // `Unit` is a return-only shape — it can't appear as an
        // input slot.
        KirType::Unit => None,
        // Strings can't be region free-var inputs yet — would need
        // a string_inputs slot list.
        KirType::String => None,
        // Bare `Null` is the singleton shape produced only by literal
        // `null`; it's never a free-variable type. Producers always
        // widen to `Nullable<T>` before binding.
        KirType::Null => None,
    }
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






// ─── Discovery & top-level program rewrite ───────────────────────



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
        nullable_inputs: vec![],
        string_inputs: vec![],
        known_fns: std::collections::BTreeMap::new(),
        known_consts: consts.clone(),
        lifted_inputs: nohash::IntMap::default(),
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
                assert_eq!(
                    *payload_types,
                    vec![
                        KirType::Prim(PrimType::I64),
                        KirType::Prim(PrimType::F64),
                    ]
                );
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
                    vec![
                        KirType::Prim(PrimType::I64),
                        KirType::Prim(PrimType::F64),
                        KirType::Prim(PrimType::Bool),
                    ]
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
                assert_eq!(sorted_types[0].1, KirType::Prim(PrimType::F64));
                assert_eq!(sorted_types[1].1, KirType::Prim(PrimType::F64));
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
            source: RegionInputSource::Binding,
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
            source: RegionInputSource::Binding,
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


    /// Maximal-fusion: a `RegionInput` whose `source` is
    /// `Lifted(expr)` should populate `FusionCtx::lifted_inputs` so
    /// that `emit_expr` intercepts the lifted ExprId and resolves it
    /// via the synthetic input name. The region body's `y` Ref
    /// resolves to a Local read of the lifted slot — no binding
    /// lookup needed.
    #[test]
    fn region_kernel_lifted_input() {
        // Body: `y + 1`. We'll mark `y`'s ExprId as a lifted input
        // and check the kernel emits a Local read for it via the
        // intercept (bypassing the normal Ref binding-resolution
        // path). Set `y`'s typ on the synth Expr so
        // `discover_region_inputs` accepts it.
        let e = parse_one("y + 1").expect("parse");
        let y_ref = match &e.kind {
            ExprKind::Add { lhs, rhs: _ } => (**lhs).clone(),
            _ => panic!("expected Add at top of `y + 1`"),
        };
        let _ = y_ref.typ.set(Type::Primitive(netidx_value::Typ::I64.into()));
        let lifted_input = RegionInput {
            expr_id: y_ref.id,
            name: ArcStr::from(
                compact_str::format_compact!("__lifted_{}", y_ref.id.inner())
                    .as_str(),
            ),
            kind: RegionInputKind::Prim(PrimType::I64),
            source: RegionInputSource::Lifted(y_ref.clone()),
        };
        let (kernel, sig) = build_kir_kernel_from_region(
            "test_lifted",
            &e,
            std::slice::from_ref(&lifted_input),
            &[],
            Some(KirType::Prim(PrimType::I64)),
            &Default::default(),
            &Default::default(),
        )
        .expect("lifted-input region builds");
        // Kernel takes one scalar param named after the synth slot,
        // and the param's type is i64.
        assert_eq!(kernel.params.len(), 1);
        assert_eq!(kernel.params[0].prim, PrimType::I64);
        assert_eq!(
            kernel.params[0].name.as_str(),
            lifted_input.name.as_str()
        );
        assert_eq!(sig.return_type, KirType::Prim(PrimType::I64));
    }
}

