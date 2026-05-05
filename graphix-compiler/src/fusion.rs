//! Node fusion: identify pure-expression / pure-function subtrees and
//! lower them into the typed kernel IR ([`crate::kernel_ir`]). The
//! resulting [`KirExpr`] / [`KirKernel`] then has two backends:
//!
//! - **Rust source** — the AOT path (`graphix compile`) calls
//!   [`kernel_ir::kir_to_rust_kernel`] to produce a free function and
//!   wraps it in a Cargo crate that rustc compiles.
//!
//! - **Cranelift IR** — the JIT path (future, see `crate::jit`) lowers
//!   the same KIR to CLIF for in-process JIT compilation.
//!
//! This module is the front end (Graphix `Expr` → KIR) plus the
//! AOT-side packaging glue: the Apply shim / `BuiltIn` impl / Cargo
//! scaffolding that drops a fused kernel into a generated package.
//! The IR types and Rust-source backend live in [`crate::kernel_ir`].
//!
//! Driving examples are mandelbrot's `iterate` (primitive args, a
//! `select` with arithmetic guards, a self-recursive tail call) and
//! naive `fib` (primitive args, non-tail self recursion lowered as
//! direct Rust recursion).

use crate::{
    env::Env,
    expr::{Expr, ExprKind, ModPath, Pattern, StructurePattern},
    kernel_ir::{
        self as kir, kir_to_rust_expr, kir_to_rust_kernel, ConstVal, KirExpr, KirKernel,
        KirOp, KirStmt, Let, SelectArm,
    },
    typ::Type,
    Scope,
};
use arcstr::ArcStr;
use std::fmt::Write;

// Re-export the canonical KIR types so existing callers (graphix-shell,
// graphix-package-bench, in-tree tests) keep compiling. The IR's
// definitive home is `crate::kernel_ir`; these aliases exist purely to
// keep the public API surface stable across the move.
pub use crate::kernel_ir::{Input, KnownConst, KnownFusedFn, PrimType};

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

    /// Look up a fn-typed kernel parameter by Graphix name, returning
    /// its zero-based index in `fn_inputs` (the `fn_index` for
    /// emitted `KirOp::DynCall`) plus the param itself.
    pub fn find_fn_input(&self, name: &str) -> Option<(u32, &crate::kernel_ir::FnParam)> {
        self.fn_inputs
            .iter()
            .enumerate()
            .find(|(_, fp)| fp.name.as_str() == name)
            .map(|(i, fp)| (i as u32, fp))
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
    let mut unified_typ: Option<PrimType> = None;
    for (i, (pat, body)) in s.arms.iter().enumerate() {
        let is_last = i == n - 1;
        let mut arm_ctx = ctx.clone();
        let cond_kir =
            emit_arm_condition(&scrut, &pat.structure_predicate, &mut arm_ctx)?;
        let guard_kir = match &pat.guard {
            None => None,
            Some(g) => {
                let g = emit_expr(g, &arm_ctx)?;
                if g.typ != PrimType::Bool {
                    return None;
                }
                Some(g)
            }
        };
        let combined = combine_cond_and_guard(cond_kir, guard_kir);
        let body_kir = emit_expr(body, &arm_ctx)?;
        match unified_typ {
            Some(t) if t != body_kir.typ => return None,
            None => unified_typ = Some(body_kir.typ),
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
    let name = match &a.function.kind {
        ExprKind::Ref { name } => ident_of(name)?,
        _ => return None,
    };
    if a.args.iter().any(|(label, _)| label.is_some()) {
        return None;
    }
    // Prefer a DynCall against a fn-typed kernel parameter (HOF arg)
    // when the name shadows one. Fn-typed params are local to the
    // current kernel, so they win over any same-named fused-static
    // entry in known_fns.
    if let Some((fn_index, fp)) = ctx.find_fn_input(name) {
        if a.args.len() != fp.arg_types.len() {
            return None;
        }
        let mut kargs = Vec::with_capacity(a.args.len());
        for ((_, expr), expected) in a.args.iter().zip(&fp.arg_types) {
            let e = emit_expr(expr, ctx)?;
            if e.typ != *expected {
                return None;
            }
            kargs.push(e);
        }
        return Some(KirExpr {
            op: KirOp::DynCall {
                fn_index,
                args: kargs,
                arg_types: fp.arg_types.clone(),
                return_type: fp.return_type,
            },
            typ: fp.return_type,
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
            let typ = body.typ;
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
                let prim = value.typ;
                lets.push(Let { local: name.clone(), value });
                local_ctx.inputs.push(Input {
                    name: name.clone(),
                    prim,
                    bind_id: None,
                    rust_name: name.to_string(),
                });
            }
            ExprKind::NoOp => {}
            _ => return None,
        }
    }
    None
}

/// Emit a sub-expression as a [`KirExpr`]. Returns `None` if any sub-
/// tree is something the emitter doesn't handle yet — that short-
/// circuits the whole parent, so the caller falls back to the
/// interpreted path (or, in AOT mode, refuses fusion).
pub fn emit_expr(expr: &Expr, ctx: &FusionCtx) -> Option<KirExpr> {
    match &expr.kind {
        ExprKind::Constant(v) => {
            let c = ConstVal::from_value(v)?;
            Some(kir::const_expr(c))
        }
        ExprKind::Ref { name } => {
            let ident = ident_of(name)?;
            // Prefer lambda-arg / let-bound locals, then fall back to
            // the known-constants registry (outer-scope `let x = <lit>;`
            // bindings inlined at compile time).
            if let Some(input) = ctx.find(ident) {
                return Some(kir::local(input.name.clone(), input.prim));
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
        ExprKind::Apply(a) => emit_known_fused_call(a, ctx),
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
    /// Arg names in signature order. Must match the lambda's argspec.
    /// These are the loop variables updated by each tail call.
    pub params: Vec<Input>,
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
            // Everything before the last must be a binding (we can't
            // fuse arbitrary statement sequences — Graphix blocks only
            // return the last expr's value). A plain expression with a
            // side-effect would be unfusable anyway.
            match &e.kind {
                ExprKind::Bind(b) => emit_bind_stmt(out, b, &mut local_ctx)?,
                ExprKind::NoOp => {}
                _ => return None,
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
    let prim = value.typ;
    out.push(KirStmt::Let(Let { local: name.clone(), value }));
    ctx.inputs.push(Input {
        name: name.clone(),
        prim,
        bind_id: None,
        rust_name: name.to_string(),
    });
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
            if g.typ != PrimType::Bool {
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
            arm_ctx.inputs.push(Input {
                name: name.clone(),
                prim: scrut.typ,
                bind_id: None,
                rust_name: name.to_string(),
            });
            Some(None)
        }
        StructurePattern::Literal(v) => {
            let c = ConstVal::from_value(v)?;
            if c.typ() != scrut.typ {
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
        // Non-primitive patterns — not fusable yet.
        StructurePattern::Slice { .. }
        | StructurePattern::SlicePrefix { .. }
        | StructurePattern::SliceSuffix { .. }
        | StructurePattern::Tuple { .. }
        | StructurePattern::Variant { .. }
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
    if apply.args.len() != self_info.params.len() {
        return None;
    }
    if apply.args.iter().any(|(label, _)| label.is_some()) {
        return None;
    }
    let mut args: Vec<KirExpr> = Vec::with_capacity(apply.args.len());
    for ((_, arg_expr), param) in apply.args.iter().zip(&self_info.params) {
        let e = emit_expr(arg_expr, ctx)?;
        if e.typ != param.prim {
            return None;
        }
        args.push(e);
    }
    out.push(KirStmt::TailCall { args });
    Some(())
}

// ─── Kernel emitters ─────────────────────────────────────────────

/// Emit a complete fused-kernel struct as Rust source for the
/// single-expression-body shape. The struct implements `Update<R, E>`
/// and evaluates `body` by pulling each input via `get_as_unchecked`,
/// running the emitted expression, and wrapping the result in the
/// appropriate `Value` variant.
///
/// Returns `None` if the body isn't fully fusable as a single
/// expression. The caller picks `struct_name`; the inputs come from
/// `ctx.inputs`.
pub fn emit_kernel(
    struct_name: &str,
    body: &Expr,
    ctx: &FusionCtx,
) -> Option<String> {
    let result = emit_expr(body, ctx)?;
    let result_variant = result.typ.value_variant();
    let result_rust = result.typ.rust_name();
    let result_src = kir_to_rust_expr(&result);

    let mut out = String::new();
    writeln!(out, "// AUTO-GENERATED by graphix fusion pass. Do not edit by hand.").ok()?;
    writeln!(out, "// Fused body: {}", body.kind).ok()?;
    writeln!(out).ok()?;
    writeln!(out, "#[derive(Debug)]").ok()?;
    writeln!(out, "pub struct {struct_name} {{").ok()?;
    writeln!(out, "    pub spec: ::graphix_compiler::expr::Expr,").ok()?;
    writeln!(out, "    pub typ: ::graphix_compiler::typ::Type,").ok()?;
    for input in &ctx.inputs {
        writeln!(
            out,
            "    pub {}_id: ::graphix_compiler::BindId,",
            input.rust_name
        )
        .ok()?;
    }
    writeln!(out, "}}").ok()?;
    writeln!(out).ok()?;
    writeln!(
        out,
        "impl<R: ::graphix_compiler::Rt, E: ::graphix_compiler::UserEvent>"
    )
    .ok()?;
    writeln!(out, "    ::graphix_compiler::Update<R, E> for {struct_name}").ok()?;
    writeln!(out, "{{").ok()?;
    writeln!(out, "    fn update(").ok()?;
    writeln!(out, "        &mut self,").ok()?;
    writeln!(out, "        _ctx: &mut ::graphix_compiler::ExecCtx<R, E>,").ok()?;
    writeln!(out, "        event: &mut ::graphix_compiler::Event<E>,").ok()?;
    writeln!(
        out,
        "    ) -> ::std::option::Option<::netidx::subscriber::Value> {{"
    )
    .ok()?;
    for input in &ctx.inputs {
        writeln!(out, "        let {} = unsafe {{", input.rust_name).ok()?;
        writeln!(
            out,
            "            *event.variables.get(&self.{}_id)?.get_as_unchecked::<{}>()",
            input.rust_name,
            input.prim.rust_name(),
        )
        .ok()?;
        writeln!(out, "        }};").ok()?;
    }
    writeln!(out, "        let result: {result_rust} = {result_src};").ok()?;
    writeln!(
        out,
        "        ::std::option::Option::Some(::netidx::subscriber::Value::{result_variant}(result))",
    )
    .ok()?;
    writeln!(out, "    }}").ok()?;
    writeln!(out).ok()?;
    writeln!(
        out,
        "    fn spec(&self) -> &::graphix_compiler::expr::Expr {{ &self.spec }}"
    )
    .ok()?;
    writeln!(
        out,
        "    fn typ(&self) -> &::graphix_compiler::typ::Type {{ &self.typ }}"
    )
    .ok()?;
    writeln!(out, "    fn refs(&self, refs: &mut ::graphix_compiler::Refs) {{").ok()?;
    for input in &ctx.inputs {
        writeln!(
            out,
            "        refs.refed.insert(self.{}_id);",
            input.rust_name
        )
        .ok()?;
    }
    writeln!(out, "    }}").ok()?;
    writeln!(
        out,
        "    fn delete(&mut self, _ctx: &mut ::graphix_compiler::ExecCtx<R, E>) {{}}"
    )
    .ok()?;
    writeln!(
        out,
        "    fn sleep(&mut self, _ctx: &mut ::graphix_compiler::ExecCtx<R, E>) {{}}"
    )
    .ok()?;
    writeln!(
        out,
        "    fn typecheck(&mut self, _ctx: &mut ::graphix_compiler::ExecCtx<R, E>) -> ::anyhow::Result<()> {{"
    )
    .ok()?;
    writeln!(out, "        Ok(())").ok()?;
    writeln!(out, "    }}").ok()?;
    writeln!(out, "}}").ok()?;
    Some(out)
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
) -> Option<PrimType> {
    match &body.kind {
        ExprKind::Do { exprs } => {
            let last = exprs.last()?;
            infer_body_rtype(last, ctx, self_info)
        }
        ExprKind::Select(s) => {
            for (_, arm) in s.arms.iter() {
                if let Some(t) = infer_body_rtype(arm, ctx, self_info) {
                    return Some(t);
                }
            }
            None
        }
        ExprKind::ExplicitParens(inner) => infer_body_rtype(inner, ctx, self_info),
        ExprKind::Apply(a) => {
            let ExprKind::Ref { name } = &a.function.kind else {
                return None;
            };
            let ident = ident_of(name)?;
            if let Some(si) = self_info {
                if si.name.as_str() == ident {
                    return None; // circular
                }
            }
            ctx.find_fn(ident).map(|kf| kf.return_type)
        }
        _ => emit_expr(body, ctx).map(|e| e.typ),
    }
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

/// Emit a complete "function-shaped" fused kernel as Rust source.
/// Three pieces of code, ready to drop into a generated module:
///
/// 1. A free `fn fused_<name>_body(...)` whose arguments match the
///    lambda's argspec and whose body is the emitted loop / select /
///    arithmetic.
/// 2. A struct `struct_name` with a `CachedVals` field.
/// 3. `BuiltIn<R, E>` and `Apply<R, E>` impls that pull primitives out
///    of the cached values and call the free function.
pub fn emit_function_kernel(
    struct_name: &str,
    builtin_name: &str,
    fn_name: &str,
    lambda: &crate::expr::LambdaExpr,
) -> Option<String> {
    let known = std::collections::BTreeMap::new();
    emit_function_kernel_with_known(
        struct_name,
        builtin_name,
        fn_name,
        lambda,
        &known,
    )
    .map(|(src, _sig)| src)
}

pub fn emit_function_kernel_with_known(
    struct_name: &str,
    builtin_name: &str,
    fn_name: &str,
    lambda: &crate::expr::LambdaExpr,
    known: &std::collections::BTreeMap<ArcStr, KnownFusedFn>,
) -> Option<(String, KnownFusedFn)> {
    emit_function_kernel_with_known_and_consts(
        struct_name,
        builtin_name,
        fn_name,
        lambda,
        known,
        &std::collections::BTreeMap::new(),
    )
}

pub fn emit_function_kernel_with_known_and_consts(
    struct_name: &str,
    builtin_name: &str,
    fn_name: &str,
    lambda: &crate::expr::LambdaExpr,
    known: &std::collections::BTreeMap<ArcStr, KnownFusedFn>,
    consts: &std::collections::BTreeMap<ArcStr, KnownConst>,
) -> Option<(String, KnownFusedFn)> {
    let (kernel, signature) = build_kir_kernel(fn_name, lambda, known, consts)?;
    let params = kernel.params.clone();
    let rprim = kernel.return_type;
    let kernel_src = kir_to_rust_kernel(&kernel);

    let mut out = String::new();
    writeln!(out, "// AUTO-GENERATED by graphix fusion pass. Do not edit by hand.")
        .ok()?;
    writeln!(out, "// Source lambda: {}", fn_name).ok()?;
    writeln!(out).ok()?;
    out.push_str(&kernel_src);
    writeln!(out).ok()?;

    // Apply shim — pulls args out of CachedVals and calls the free fn.
    writeln!(out, "#[derive(Debug)]").ok()?;
    writeln!(out, "pub struct {struct_name} {{").ok()?;
    writeln!(out, "    pub args: ::graphix_package_core::CachedVals,").ok()?;
    writeln!(out, "}}").ok()?;
    writeln!(out).ok()?;

    writeln!(
        out,
        "impl<R: ::graphix_compiler::Rt, E: ::graphix_compiler::UserEvent>"
    )
    .ok()?;
    writeln!(
        out,
        "    ::graphix_compiler::BuiltIn<R, E> for {struct_name}"
    )
    .ok()?;
    writeln!(out, "{{").ok()?;
    writeln!(out, "    const NAME: &'static str = \"{builtin_name}\";").ok()?;
    writeln!(out, "    const NEEDS_CALLSITE: bool = false;").ok()?;
    writeln!(out).ok()?;
    writeln!(out, "    fn init<'a, 'b, 'c, 'd>(").ok()?;
    writeln!(out, "        _ctx: &'a mut ::graphix_compiler::ExecCtx<R, E>,").ok()?;
    writeln!(out, "        _typ: &'a ::graphix_compiler::typ::FnType,").ok()?;
    writeln!(
        out,
        "        _resolved: ::std::option::Option<&'d ::graphix_compiler::typ::FnType>,"
    )
    .ok()?;
    writeln!(out, "        _scope: &'b ::graphix_compiler::Scope,").ok()?;
    writeln!(out, "        from: &'c [::graphix_compiler::Node<R, E>],").ok()?;
    writeln!(out, "        _top_id: ::graphix_compiler::expr::ExprId,").ok()?;
    writeln!(
        out,
        "    ) -> ::anyhow::Result<::std::boxed::Box<dyn ::graphix_compiler::Apply<R, E>>> {{"
    )
    .ok()?;
    writeln!(
        out,
        "        ::std::result::Result::Ok(::std::boxed::Box::new({struct_name} {{"
    )
    .ok()?;
    writeln!(
        out,
        "            args: ::graphix_package_core::CachedVals::new(from),"
    )
    .ok()?;
    writeln!(out, "        }}))").ok()?;
    writeln!(out, "    }}").ok()?;
    writeln!(out, "}}").ok()?;
    writeln!(out).ok()?;

    writeln!(
        out,
        "impl<R: ::graphix_compiler::Rt, E: ::graphix_compiler::UserEvent>"
    )
    .ok()?;
    writeln!(out, "    ::graphix_compiler::Apply<R, E> for {struct_name}").ok()?;
    writeln!(out, "{{").ok()?;
    writeln!(out, "    fn update(").ok()?;
    writeln!(out, "        &mut self,").ok()?;
    writeln!(out, "        ctx: &mut ::graphix_compiler::ExecCtx<R, E>,").ok()?;
    writeln!(out, "        from: &mut [::graphix_compiler::Node<R, E>],").ok()?;
    writeln!(out, "        event: &mut ::graphix_compiler::Event<E>,").ok()?;
    writeln!(
        out,
        "    ) -> ::std::option::Option<::netidx::subscriber::Value> {{"
    )
    .ok()?;
    writeln!(out, "        if !self.args.update(ctx, from, event) {{").ok()?;
    writeln!(out, "            return ::std::option::Option::None;").ok()?;
    writeln!(out, "        }}").ok()?;
    // Unsafe fast-path dispatch.
    //
    // SAFETY: self.args.update() returning true guarantees every slot
    // in self.args.0 is Some(_). The typechecker ran before this
    // kernel was picked up by the fusion pass and proved every arg's
    // type, which is what drove the params[] choices. get_as_unchecked
    // is sound precisely when the Value's tag matches T's variant —
    // an invariant the typechecker enforces.
    writeln!(out, "        unsafe {{").ok()?;
    for (i, p) in params.iter().enumerate() {
        writeln!(
            out,
            "            let __a{i}: {rust} = *self.args.0.get_unchecked({i}).as_ref().unwrap_unchecked().get_as_unchecked::<{rust}>();",
            i = i,
            rust = p.prim.rust_name(),
        )
        .ok()?;
    }
    write!(out, "            let __r = fused_{fn_name}_body(").ok()?;
    for (i, _) in params.iter().enumerate() {
        if i > 0 {
            write!(out, ", ").ok()?;
        }
        write!(out, "__a{i}").ok()?;
    }
    writeln!(out, ");").ok()?;
    writeln!(
        out,
        "            ::std::option::Option::Some(::netidx::subscriber::Value::{}(__r))",
        rprim.value_variant()
    )
    .ok()?;
    writeln!(out, "        }}").ok()?;
    writeln!(out, "    }}").ok()?;
    writeln!(out).ok()?;
    writeln!(
        out,
        "    fn sleep(&mut self, _ctx: &mut ::graphix_compiler::ExecCtx<R, E>) {{"
    )
    .ok()?;
    writeln!(out, "        self.args.clear()").ok()?;
    writeln!(out, "    }}").ok()?;
    writeln!(out, "}}").ok()?;
    Some((out, signature))
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

/// Walk an Expr collecting `(callee_name, apply_expr_id)` pairs for
/// every `Apply { function: Ref { name } }` site. The apply's
/// `ExprId` is what `CallSite::typecheck` uses as the key in
/// `ctx.fn_types` — this is where the *resolved* FnType for the
/// call site lives (with TVars unified against the actual arg
/// expressions). The lazy fusion path uses the resolved type from
/// fn_types[apply_id] to patch unannotated callee argspecs.
pub fn discover_callee_names(expr: &Expr) -> Vec<(ArcStr, crate::expr::ExprId)> {
    let mut out = Vec::new();
    walk_for_callees(expr, &mut out);
    out
}

fn walk_for_callees(expr: &Expr, out: &mut Vec<(ArcStr, crate::expr::ExprId)>) {
    match &expr.kind {
        ExprKind::Apply(a) => {
            if let ExprKind::Ref { name } = &a.function.kind {
                if let Some(ident) = ident_of(name) {
                    out.push((ArcStr::from(ident), expr.id));
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

/// Lazy on-demand resolution of a fused kernel by binding name.
/// Looks `name` up in `ctx.fusion_lambdas` (populated by
/// `Bind::compile` for every `let X = lambda` in scope), locks the
/// entry's cache, and builds the kernel on first request. Subsequent
/// requests return the cached result. Cycles (a kernel transitively
/// referencing itself through other kernels) are broken by the
/// `InProgress` cache state — recursive calls to a still-building
/// kernel return `None`.
///
/// `apply_site_hint` is the `ExprId` of one Apply expression that
/// calls `name`. The lazy build path prefers the call-site resolved
/// FnType (via `ctx.fn_types[apply_site_hint]`) over the lambda's
/// own typ, because the lambda's typ may have unbound TVars (no
/// typechecker constraint from the lambda's own definition) while
/// the Apply's typ is always concrete after CallSite::typecheck.
/// This is what lets unannotated callees fuse.
///
/// Returns the callee's signature (for compile-time call lowering)
/// and `Arc<KirKernel>` (for runtime dispatch via the interpreter's
/// `KirOp::Call` path).
pub fn lazy_resolve_kernel<R: crate::Rt, E: crate::UserEvent>(
    ctx: &mut crate::ExecCtx<R, E>,
    name: &str,
    apply_site_hint: Option<crate::expr::ExprId>,
) -> Option<(KnownFusedFn, std::sync::Arc<KirKernel>)> {
    let entry = ctx.fusion_lambdas.get(name).cloned()?;
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
        entry.spec_id,
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
    apply_site_hint: Option<crate::expr::ExprId>,
    spec_id: crate::expr::ExprId,
    lambda: &triomphe::Arc<crate::expr::LambdaExpr>,
) -> Option<(KnownFusedFn, std::sync::Arc<KirKernel>)> {
    use netidx::utils::Either;
    // Patch the lambda's argspec / rtype from the typechecker's
    // resolved FnType. We prefer the call-site Apply's resolved
    // FnType (`ctx.fn_types[apply_site_hint]`, populated by
    // `CallSite::typecheck` with TVars unified against the actual
    // arg expressions) over the lambda's own typ — the lambda's
    // typ may still have unbound TVars when the lambda is
    // polymorphic or hasn't been called yet. Falls back to the
    // lambda's typ if no apply hint is available.
    let mut patched = lambda.clone();
    let ft = apply_site_hint
        .and_then(|id| ctx.fn_types.get(&id).cloned())
        .or_else(|| ctx.fn_types.get(&spec_id).cloned());
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
    let consts = ctx.fusion_known_consts.clone();
    let (kernel, sig) =
        build_kir_kernel(fn_name.as_str(), &patched, &known, &consts)?;
    Some((sig, std::sync::Arc::new(kernel)))
}

/// Build a [`KirKernel`] from a lambda's argspec + body, plus
/// optional registries of already-fused kernels and known constants.
/// This is the path the JIT and interpreter share: both consume the
/// `KirKernel` directly. The AOT path goes through here too via
/// [`emit_function_kernel_with_known_and_consts`], which then renders
/// the kernel to Rust source.
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
    let mut ctx = FusionCtx {
        inputs: vec![],
        fn_inputs: vec![],
        known_fns: known.clone(),
        known_consts: consts.clone(),
    };
    let mut params: Vec<Input> = Vec::new();
    let mut arg_types: Vec<PrimType> = Vec::new();
    let mut fn_params: Vec<crate::kernel_ir::FnParam> = Vec::new();
    for (arg_pos, arg) in lambda.args.iter().enumerate() {
        if arg.labeled.is_some() {
            return None;
        }
        let typ = arg.constraint.as_ref()?;
        let name = arg.pattern.single_bind()?;
        // Try the param as a primitive first (the common case).
        if let Some(prim) = PrimType::from_type(typ) {
            let input = Input {
                name: name.clone(),
                prim,
                bind_id: None,
                rust_name: name.to_string(),
            };
            params.push(input.clone());
            ctx.inputs.push(input);
            arg_types.push(prim);
            continue;
        }
        // Try the param as a function type — yields a DynCall slot
        // with `FnSource::Param`.
        if let Some(fp) =
            try_fn_param_from_type(typ, name.clone(), arg_pos as u32)
        {
            ctx.fn_inputs.push(fp.clone());
            fn_params.push(fp);
            continue;
        }
        // Neither prim nor fn — bail.
        return None;
    }
    // After the HOF-arg pass, fold in the binding-source fn_inputs
    // (DynCall slots that resolve through `ctx.cached[bind_id]` at
    // dispatch time, not from an incoming arg slot). Pushed last so
    // an HOF-arg with the same name shadows any outer binding —
    // `find_fn_input` is a linear scan that returns the first hit.
    for fp in binding_fn_inputs {
        ctx.fn_inputs.push(fp.clone());
        fn_params.push(fp.clone());
    }
    let self_info = SelfInfo { name: ArcStr::from(fn_name), params: params.clone() };
    let rprim = match lambda.rtype.as_ref() {
        Some(rtype) => PrimType::from_type(rtype)?,
        None => infer_body_rtype(body, &ctx, Some(&self_info))?,
    };
    let has_tail = body_has_tail_call(body, fn_name);
    let signature = KnownFusedFn {
        body_fn_name: format!("fused_{fn_name}_body"),
        arg_types,
        return_type: rprim,
    };
    // Register self in known_fns so non-tail-position self-recursive
    // calls in the body lower as direct recursive calls rather than
    // tripping the "unknown function" path in emit_expr. Tail calls
    // still go through TailCall via SelfInfo.
    ctx.known_fns.insert(ArcStr::from(fn_name), signature.clone());
    let body_stmts = emit_body(body, &ctx, Some(&self_info))?;
    let kernel = KirKernel {
        fn_name: ArcStr::from(fn_name),
        params,
        fn_params,
        return_type: rprim,
        has_tail_loop: has_tail,
        body: body_stmts,
    };
    Some((kernel, signature))
}

/// Try to interpret a [`Type`] as a function whose arg and return
/// types are all primitives — the only shape DynCall supports in v1.
/// Returns the FnParam (with `FnSource::Param { arg_pos }`) ready to
/// insert into the kernel's `fn_params`, or `None` if any part of the
/// signature is non-primitive.
fn try_fn_param_from_type(
    typ: &Type,
    name: ArcStr,
    arg_pos: u32,
) -> Option<crate::kernel_ir::FnParam> {
    let (arg_types, return_type) = extract_prim_signature(typ)?;
    Some(crate::kernel_ir::FnParam {
        name,
        source: crate::kernel_ir::FnSource::Param { arg_pos },
        arg_types,
        return_type,
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
            extract_prim_signature(resolved?)
        })?;
    Some(crate::kernel_ir::FnParam {
        name: name.clone(),
        source: crate::kernel_ir::FnSource::Binding { bind_id },
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
pub fn extract_prim_signature(
    typ: &Type,
) -> Option<(Vec<PrimType>, PrimType)> {
    typ.with_deref(|resolved| match resolved? {
        Type::Fn(ft) => {
            let mut arg_types: Vec<PrimType> =
                Vec::with_capacity(ft.args.len());
            for fa in ft.args.iter() {
                if fa.label.is_some() {
                    return None;
                }
                arg_types.push(PrimType::from_type(&fa.typ)?);
            }
            if ft.vargs.is_some() {
                return None;
            }
            let return_type = PrimType::from_type(&ft.rtype)?;
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
                    rust_name: ident.to_string(),
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

/// Report a single fusion attempt against a lambda embedded in a
/// larger program.
#[derive(Debug, Clone)]
pub struct FusionReport {
    pub name: Option<ArcStr>,
    pub id: crate::expr::ExprId,
    pub outcome: FusionOutcome,
}

#[derive(Debug, Clone)]
pub enum FusionOutcome {
    Fused(String),
    Rejected(&'static str),
}

/// Walk a whole program (parsed but not necessarily typechecked) and
/// attempt fusion for every lambda we encounter — both top-level
/// definitions and nested let-bound ones. Non-lambda sub-expressions
/// are traversed for their children but not themselves reported.
pub fn walk_and_fuse(expr: &Expr) -> Vec<FusionReport> {
    let mut reports = Vec::new();
    walk_collect_lambdas(expr, None, &mut reports);
    reports
}

fn walk_collect_lambdas(
    expr: &Expr,
    bound_name: Option<&ArcStr>,
    out: &mut Vec<FusionReport>,
) {
    use netidx::utils::Either;
    match &expr.kind {
        ExprKind::Lambda(l) => {
            let name_str = bound_name
                .map(|n| format_compact_name(n))
                .unwrap_or_else(|| format!("FusedLambda_{}", expr.id.inner()));
            let report = try_fuse_lambda(l, &name_str);
            let outcome = match report {
                Some(src) => FusionOutcome::Fused(src),
                None => FusionOutcome::Rejected("args not primitive, or body not fusable"),
            };
            out.push(FusionReport { name: bound_name.cloned(), id: expr.id, outcome });
            if let Either::Left(body) = &l.body {
                walk_collect_lambdas(body, None, out);
            }
        }
        ExprKind::Bind(b) => {
            let name = b.pattern.single_bind().cloned();
            walk_collect_lambdas(&b.value, name.as_ref(), out);
        }
        ExprKind::Do { exprs } => {
            for e in exprs.iter() {
                walk_collect_lambdas(e, None, out);
            }
        }
        ExprKind::Module { value, .. } => match value {
            crate::expr::ModuleKind::Resolved { exprs, .. } => {
                for e in exprs.iter() {
                    walk_collect_lambdas(e, None, out);
                }
            }
            crate::expr::ModuleKind::Dynamic { source, .. } => {
                walk_collect_lambdas(source, None, out);
            }
            crate::expr::ModuleKind::Unresolved { .. } => {}
        },
        ExprKind::Apply(a) => {
            walk_collect_lambdas(&a.function, None, out);
            for (_, arg) in a.args.iter() {
                walk_collect_lambdas(arg, None, out);
            }
        }
        ExprKind::ExplicitParens(e)
        | ExprKind::Qop(e)
        | ExprKind::OrNever(e)
        | ExprKind::ByRef(e)
        | ExprKind::Deref(e)
        | ExprKind::Not { expr: e } => walk_collect_lambdas(e, None, out),
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
            walk_collect_lambdas(lhs, None, out);
            walk_collect_lambdas(rhs, None, out);
        }
        ExprKind::Connect { value, .. } => walk_collect_lambdas(value, None, out),
        ExprKind::Select(s) => {
            walk_collect_lambdas(&s.arg, None, out);
            for (_, arm) in s.arms.iter() {
                walk_collect_lambdas(arm, None, out);
            }
        }
        _ => {}
    }
}

fn format_compact_name(n: &ArcStr) -> String {
    // "iterate" -> "FusedIterate"; non-alphanumeric becomes underscore
    // so we don't produce invalid Rust idents for weird names.
    let mut out = String::from("Fused_");
    let mut upper = true;
    for c in n.chars() {
        if c.is_ascii_alphanumeric() {
            if upper {
                out.extend(c.to_uppercase());
                upper = false;
            } else {
                out.push(c);
            }
        } else {
            out.push('_');
            upper = true;
        }
    }
    out
}

/// Walk a `LambdaExpr`, build a FusionCtx from its (type-annotated)
/// args, and attempt to emit a fused kernel for its body. Returns
/// `None` if any arg lacks a primitive type annotation, or if the
/// body isn't fusable as a single expression.
pub fn try_fuse_lambda(
    lambda: &crate::expr::LambdaExpr,
    struct_name: &str,
) -> Option<String> {
    use netidx::utils::Either;
    let body = match &lambda.body {
        Either::Left(e) => e,
        Either::Right(_) => return None,
    };
    let mut ctx = FusionCtx::default();
    for arg in lambda.args.iter() {
        let typ = arg.constraint.as_ref()?;
        let prim = PrimType::from_type(typ)?;
        let name = arg.pattern.single_bind()?;
        ctx.inputs.push(Input {
            name: name.clone(),
            prim,
            bind_id: None,
            rust_name: name.to_string(),
        });
    }
    emit_kernel(struct_name, body, &ctx)
}

// ─── `graphix compile`-style rewrite pipeline ────────────────────

#[derive(Debug, Clone)]
pub struct FusedKernel {
    pub struct_name: String,
    pub builtin_name: String,
    pub fn_name: String,
    pub rust_source: String,
}

pub fn rewrite_program(
    expr: &mut Expr,
    package_prefix: &str,
) -> anyhow::Result<Vec<FusedKernel>> {
    let mut state = RewriteState::new();
    rewrite_program_with_state(expr, package_prefix, &mut state)?;
    Ok(state.kernels)
}

#[derive(Debug, Default)]
pub struct RewriteState {
    pub kernels: Vec<FusedKernel>,
    pub known: std::collections::BTreeMap<ArcStr, KnownFusedFn>,
    pub consts: std::collections::BTreeMap<ArcStr, KnownConst>,
    pub counter: u32,
    /// Typechecker-derived function types for every Lambda and Apply
    /// in the program, keyed by ExprId. Populated by the compile()
    /// pipeline and consulted by the rewrite pass to drive codegen
    /// against real typechecker output (arg types, return types, HOF
    /// callback signatures). Empty when `rewrite_program` is called
    /// from a test or a caller that hasn't wired up the full
    /// typecheck — in which case unannotated callback lambdas stay
    /// unannotated and can't be fused.
    pub fn_types: nohash::IntMap<crate::expr::ExprId, crate::typ::FnType>,
}

impl RewriteState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_fn_types(
        fn_types: nohash::IntMap<crate::expr::ExprId, crate::typ::FnType>,
    ) -> Self {
        Self { fn_types, ..Self::default() }
    }
}

pub fn rewrite_program_with_state(
    expr: &mut Expr,
    package_prefix: &str,
    state: &mut RewriteState,
) -> anyhow::Result<()> {
    let fn_types = state.fn_types.clone();
    rewrite_walk(
        expr,
        package_prefix,
        &mut state.kernels,
        &mut state.known,
        &mut state.consts,
        &mut state.counter,
        &fn_types,
    )
}

fn rewrite_walk(
    expr: &mut Expr,
    prefix: &str,
    kernels: &mut Vec<FusedKernel>,
    known: &mut std::collections::BTreeMap<ArcStr, KnownFusedFn>,
    consts: &mut std::collections::BTreeMap<ArcStr, KnownConst>,
    counter: &mut u32,
    fn_types: &nohash::IntMap<crate::expr::ExprId, crate::typ::FnType>,
) -> anyhow::Result<()> {
    fn try_rewrite_bind(
        name: &ArcStr,
        lambda_expr: &mut Expr,
        prefix: &str,
        kernels: &mut Vec<FusedKernel>,
        known: &mut std::collections::BTreeMap<ArcStr, KnownFusedFn>,
        consts: &std::collections::BTreeMap<ArcStr, KnownConst>,
        counter: &mut u32,
        fn_types: &nohash::IntMap<crate::expr::ExprId, crate::typ::FnType>,
    ) -> anyhow::Result<()> {
        use netidx::utils::Either;
        let ExprKind::Lambda(lambda_arc) = &mut lambda_expr.kind else {
            return Ok(());
        };
        if matches!(&lambda_arc.body, Either::Right(_)) {
            return Ok(());
        }
        *counter += 1;
        let n = *counter;
        let struct_name = format!(
            "Fused{}{}",
            format_compact_name(name).trim_start_matches("Fused_"),
            n
        );
        let builtin_name = format!("{prefix}_{}_{}", sanitize(name), n);
        if let Some(ft) = fn_types.get(&lambda_expr.id) {
            apply_fntype_to_lambda(lambda_arc, ft);
        }
        match emit_function_kernel_with_known_and_consts(
            &struct_name,
            &builtin_name,
            name,
            lambda_arc,
            known,
            consts,
        ) {
            Some((src, signature)) => {
                let new_lambda = crate::expr::LambdaExpr {
                    args: lambda_arc.args.clone(),
                    vargs: lambda_arc.vargs.clone(),
                    rtype: lambda_arc.rtype.clone(),
                    constraints: lambda_arc.constraints.clone(),
                    throws: lambda_arc.throws.clone(),
                    body: Either::Right(ArcStr::from(builtin_name.as_str())),
                };
                *lambda_arc = triomphe::Arc::new(new_lambda);
                known.insert(name.clone(), signature);
                kernels.push(FusedKernel {
                    struct_name,
                    builtin_name,
                    fn_name: name.to_string(),
                    rust_source: src,
                });
                Ok(())
            }
            None => {
                let pretty = if name.starts_with("anon") {
                    compact_str::format_compact!("anonymous lambda")
                } else {
                    compact_str::format_compact!("lambda `{name}`")
                };
                if let Some(ft) = fn_types.get(&lambda_expr.id) {
                    if let Some(pos) = fntype_first_unresolved(ft) {
                        let src_pos = lambda_expr.pos;
                        anyhow::bail!(
                            "fusion: {pretty} at line {}, column {} can't be \
                             compiled to native code — {pos} is an unresolved type \
                             variable. Add an explicit type annotation so the AOT \
                             emitter can pin the type.",
                            src_pos.line,
                            src_pos.column
                        );
                    }
                }
                log::info!(
                    "fusion: skipping {pretty} at line {}, column {} — body \
                     uses constructs the emitter doesn't handle; it will run in the \
                     interpreter",
                    lambda_expr.pos.line,
                    lambda_expr.pos.column
                );
                Ok(())
            }
        }
    }

    match &mut expr.kind {
        ExprKind::Do { exprs } => {
            let arc = exprs;
            let mut tmp: Vec<Expr> = arc.iter().cloned().collect();
            for e in &mut tmp {
                rewrite_walk(e, prefix, kernels, known, consts, counter, fn_types)?;
            }
            *arc = triomphe::Arc::from_iter(tmp);
        }
        ExprKind::Bind(b) => {
            let bind_inner = triomphe::Arc::make_mut(b);
            if let Some(name) = bind_inner.pattern.single_bind().cloned() {
                record_const_binding(&name, &bind_inner.value, consts);
                try_rewrite_bind(
                    &name,
                    &mut bind_inner.value,
                    prefix,
                    kernels,
                    known,
                    consts,
                    counter,
                    fn_types,
                )?;
            }
            rewrite_walk(
                &mut bind_inner.value,
                prefix,
                kernels,
                known,
                consts,
                counter,
                fn_types,
            )?;
        }
        ExprKind::Module { value, .. } => match value {
            crate::expr::ModuleKind::Resolved { exprs, .. } => {
                let mut tmp: Vec<Expr> = exprs.iter().cloned().collect();
                for e in &mut tmp {
                    rewrite_walk(e, prefix, kernels, known, consts, counter, fn_types)?;
                }
                *exprs = triomphe::Arc::from_iter(tmp);
            }
            crate::expr::ModuleKind::Dynamic { source, .. } => {
                let source_mut = triomphe::Arc::make_mut(source);
                rewrite_walk(source_mut, prefix, kernels, known, consts, counter, fn_types)?;
            }
            crate::expr::ModuleKind::Unresolved { .. } => {}
        },
        ExprKind::Apply(a) => {
            let apply_id = expr.id;
            let apply_fn_type = fn_types.get(&apply_id).cloned();
            if std::env::var_os("GRAPHIX_FUSION_DEBUG").is_some() {
                if let ExprKind::Ref { name } = &a.function.kind {
                    eprintln!(
                        "fusion: Apply to {name} (id={apply_id:?}) — fn_type from map: {}",
                        apply_fn_type.is_some()
                    );
                    if let Some(ft) = &apply_fn_type {
                        crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
                            for (i, arg) in ft.args.iter().enumerate() {
                                eprintln!("  arg[{i}]: {}", arg.typ);
                            }
                        });
                    }
                }
            }
            let mut new_args: Vec<(Option<ArcStr>, Expr)> =
                a.args.iter().cloned().collect();
            for (idx, (_label, arg)) in new_args.iter_mut().enumerate() {
                if let ExprKind::Lambda(_) = &arg.kind {
                    if let Some(ft) = &apply_fn_type {
                        inject_lambda_arg_types_from_fntype(arg, idx, ft);
                    }
                    let synth_name =
                        ArcStr::from(format!("anon{}", arg.id.inner()).as_str());
                    try_rewrite_bind(
                        &synth_name,
                        arg,
                        prefix,
                        kernels,
                        known,
                        consts,
                        counter,
                        fn_types,
                    )?;
                }
                rewrite_walk(arg, prefix, kernels, known, consts, counter, fn_types)?;
            }
            a.args = triomphe::Arc::from_iter(new_args);
            let fn_mut = triomphe::Arc::make_mut(&mut a.function);
            rewrite_walk(fn_mut, prefix, kernels, known, consts, counter, fn_types)?;
        }
        _ => {}
    }
    Ok(())
}

fn sanitize(s: &str) -> String {
    s.chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect()
}

/// Inject argument types from a real typechecker-derived FnType into a
/// callback lambda missing type annotations. Reads the outer HOF's arg
/// type at `arg_position` (which should itself be a function type) and
/// copies its arg types onto the lambda's argspec wherever the user
/// didn't annotate.
fn inject_lambda_arg_types_from_fntype(
    lambda_expr: &mut Expr,
    arg_position: usize,
    hof_fntype: &crate::typ::FnType,
) {
    let Some(outer_arg) = hof_fntype.args.get(arg_position) else {
        return;
    };
    outer_arg.typ.with_deref(|resolved| {
        let Some(resolved) = resolved else { return };
        let stripped = strip_byref(resolved);
        let crate::typ::Type::Fn(cb_fntype) = stripped else {
            return;
        };
        apply_fntype_args_to_lambda(lambda_expr, cb_fntype);
    });
}

/// Patch a lambda's argspec / rtype with primitive types resolved
/// from a `FnType` (typically the call-site resolved type from
/// CallSite::typecheck). Where the user wrote no annotation, fill
/// in from `ft`; user-provided annotations stay. Used both by the
/// AOT rewrite pass (post-typecheck patching driven by fn_types)
/// and by the runtime deferred-fusion path (Lambda's InitFn at
/// first call uses the call site's resolved FnType to give
/// previously-unfusable callbacks like `|idx| ...` the typed
/// argspec they need to fuse).
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

fn fntype_first_unresolved(ft: &crate::typ::FnType) -> Option<compact_str::CompactString> {
    use compact_str::format_compact;
    for (i, a) in ft.args.iter().enumerate() {
        if a.typ.with_deref(|r| r.is_none()) {
            return Some(format_compact!("argument {i}"));
        }
    }
    if ft.rtype.with_deref(|r| r.is_none()) {
        return Some(format_compact!("the return type"));
    }
    None
}

fn apply_fntype_args_to_lambda(lambda_expr: &mut Expr, cb_fntype: &crate::typ::FnType) {
    let ExprKind::Lambda(lambda_arc) = &mut lambda_expr.kind else {
        return;
    };
    apply_fntype_to_lambda(lambda_arc, cb_fntype);
}

fn strip_byref(t: &crate::typ::Type) -> &crate::typ::Type {
    match t {
        crate::typ::Type::ByRef(inner) => strip_byref(inner),
        _ => t,
    }
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
/// expression — both backends fold it as appropriate at lowering.
///
/// Used by both the AOT rewrite pass and `Bind::compile` (the
/// runtime path), since the runtime fusion attempts in
/// `Lambda::compile` need the same const visibility the AOT pass
/// already had.
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
        known_fns: std::collections::BTreeMap::new(),
        known_consts: consts.clone(),
    };
    if let Some(e) = emit_expr(value, &probe) {
        consts.insert(name.clone(), KnownConst { expr: e });
    }
}

// ─── Package emission (AOT-only) ─────────────────────────────────

#[derive(Debug, Clone)]
pub struct EmittedPackage {
    pub short_name: String,
    pub kernel_sources: Vec<FusedKernel>,
    pub main_gx: String,
}

pub fn emit_package(
    short_name: &str,
    kernels: Vec<FusedKernel>,
    main_gx: String,
) -> EmittedPackage {
    EmittedPackage {
        short_name: short_name.to_string(),
        kernel_sources: kernels,
        main_gx,
    }
}

pub fn render_lib_rs(pkg: &EmittedPackage) -> String {
    let mut out = String::new();
    writeln!(
        out,
        "// AUTO-GENERATED package for {}. Do not edit by hand.",
        pkg.short_name
    )
    .ok();
    writeln!(out).ok();
    for k in &pkg.kernel_sources {
        out.push_str(&k.rust_source);
        writeln!(out).ok();
    }
    writeln!(out, "::graphix_derive::defpackage! {{").ok();
    write!(out, "    builtins => [").ok();
    for (i, k) in pkg.kernel_sources.iter().enumerate() {
        if i > 0 {
            write!(out, ", ").ok();
        }
        write!(out, "{}", k.struct_name).ok();
    }
    writeln!(out, "],").ok();
    writeln!(out, "}}").ok();
    out
}

pub fn render_mod_gxi(_pkg: &EmittedPackage) -> &'static str {
    ""
}

pub fn render_mod_gx(_pkg: &EmittedPackage) -> &'static str {
    ""
}

pub fn render_cargo_toml(pkg: &EmittedPackage, graphix_src_root: &str) -> String {
    format!(
        r#"[package]
name = "graphix-package-{short}"
version = "0.1.0"
edition = "2024"
description = "Auto-generated fused package produced by graphix compile."
license = "MIT"

[dependencies]
anyhow = "1"
arcstr = "1"
nohash = "0.2"
ahash = "0.8"
graphix-compiler = {{ path = "{root}/graphix-compiler" }}
graphix-derive    = {{ path = "{root}/graphix-derive" }}
graphix-package   = {{ path = "{root}/graphix-package" }}
graphix-package-core = {{ path = "{root}/stdlib/graphix-package-core" }}
graphix-rt        = {{ path = "{root}/graphix-rt" }}
netidx            = {{ path = "{root}/../netidx/netidx" }}
netidx-core       = {{ path = "{root}/../netidx/netidx-core" }}
netidx-value      = {{ path = "{root}/../netidx/netidx-value" }}
tokio             = {{ version = "1", features = ["rt-multi-thread", "net", "time", "io-util", "fs", "sync", "process", "macros", "signal", "io-std"] }}
triomphe          = "0.1"
async-trait       = "0.1"

[features]
default = []
standalone = []
"#,
        short = pkg.short_name,
        root = graphix_src_root,
    )
}

pub fn write_package(
    pkg: &EmittedPackage,
    target_dir: &std::path::Path,
    graphix_src_root: &str,
) -> anyhow::Result<()> {
    use std::fs;
    let src = target_dir.join("src");
    let gx_dir = src.join("graphix");
    fs::create_dir_all(&gx_dir)?;
    fs::write(src.join("lib.rs"), render_lib_rs(pkg))?;
    fs::write(gx_dir.join("mod.gx"), render_mod_gx(pkg))?;
    fs::write(gx_dir.join("mod.gxi"), render_mod_gxi(pkg))?;
    fs::write(gx_dir.join("main.gx"), &pkg.main_gx)?;
    fs::write(
        target_dir.join("Cargo.toml"),
        render_cargo_toml(pkg, graphix_src_root),
    )?;
    Ok(())
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
            rust_name: name.to_string(),
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

    /// Render a fused expression to its Rust source — what tests want
    /// to assert on. The fusion pass produces KIR; the AOT path runs
    /// it through `kir_to_rust_expr` to get the final string.
    fn rust(e: &KirExpr) -> String {
        kir_to_rust_expr(e)
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
        assert_eq!(e.typ, PrimType::Bool);
        assert_eq!(rust(&e), "(((zr * zr) + (zi * zi)) > 4f64)");
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

    #[test]
    fn emit_full_kernel() {
        let ctx = FusionCtx { inputs: vec![input("zr", PrimType::F64), input("zi", PrimType::F64)], ..Default::default() };
        let zr_sq = bin(ref_expr("zr"), ref_expr("zr"), |l, r| ExprKind::Mul { lhs: l, rhs: r });
        let zi_sq = bin(ref_expr("zi"), ref_expr("zi"), |l, r| ExprKind::Mul { lhs: l, rhs: r });
        let sum = bin(zr_sq, zi_sq, |l, r| ExprKind::Add { lhs: l, rhs: r });
        let src = emit_kernel("FusedEscaped", &sum, &ctx).expect("should emit");
        assert!(src.contains("pub struct FusedEscaped"));
        assert!(src.contains("zr_id"));
        assert!(src.contains("get_as_unchecked::<f64>"));
        assert!(src.contains("(zr * zr) + (zi * zi)"));
        assert!(src.contains("Value::F64"));
        println!("{src}");
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
        assert_eq!(e.typ, PrimType::I64);
        assert_eq!(rust(&e), "(a + (b * c))");
    }

    #[test]
    fn parser_mandelbrot_escape_test() {
        let ctx = FusionCtx { inputs: vec![input("zr", PrimType::F64), input("zi", PrimType::F64)], ..Default::default() };
        let e = parse_fuse("zr * zr + zi * zi > 4.0", &ctx).expect("should fuse");
        assert_eq!(e.typ, PrimType::Bool);
        assert_eq!(rust(&e), "(((zr * zr) + (zi * zi)) > 4f64)");
    }

    #[test]
    fn parser_lambda_full_kernel() {
        let e = parse_one("|zr: f64, zi: f64, cr: f64| zr * zr - zi * zi + cr")
            .expect("parse");
        let lambda = match &e.kind {
            ExprKind::Lambda(l) => l.clone(),
            _ => panic!("expected lambda"),
        };
        let src = try_fuse_lambda(&lambda, "FusedNewZr").expect("should fuse");
        assert!(src.contains("pub struct FusedNewZr"));
        assert!(src.contains("zr_id"));
        assert!(src.contains("cr_id"));
        assert!(src.contains("get_as_unchecked::<f64>"));
        assert!(src.contains("Value::F64"));
        println!("{src}");
    }

    #[test]
    fn parser_lambda_with_let_now_fuses() {
        let e = parse_one("|a: f64, b: f64| -> f64 { let c = a + b; c * 2.0 }")
            .expect("parse");
        let lambda = match &e.kind {
            ExprKind::Lambda(l) => l.clone(),
            _ => panic!("expected lambda"),
        };
        let src = emit_function_kernel("FusedScaled", "pkg_scaled", "scaled", &lambda)
            .expect("should fuse via emit_function_kernel");
        assert!(src.contains("let mut c = (a + b)"), "source: {src}");
        assert!(src.contains("return (c * 2f64);"), "source: {src}");
        println!("{src}");
    }

    #[test]
    fn parser_lambda_without_annotation_is_rejected() {
        let e = parse_one("|a, b: f64| a + b").expect("parse");
        let lambda = match &e.kind {
            ExprKind::Lambda(l) => l.clone(),
            _ => panic!("expected lambda"),
        };
        assert!(try_fuse_lambda(&lambda, "WontFuse").is_none());
    }

    #[test]
    fn mandelbrot_iterate_fuses_end_to_end() {
        let src = r#"
            |zr: f64, zi: f64, cr: f64, ci: f64, i: i64| -> i64
                select i {
                    0 => 0,
                    _ if zr * zr + zi * zi > 4.0 => i,
                    _ => iterate(zr * zr - zi * zi + cr, 2.0 * zr * zi + ci, cr, ci, i - 1)
                }
        "#;
        let e = parse_one(src).expect("parse");
        let lambda = match &e.kind {
            ExprKind::Lambda(l) => l.clone(),
            _ => panic!("expected lambda"),
        };
        let out = emit_function_kernel(
            "FusedIterateKernel",
            "pkg_iterate",
            "iterate",
            &lambda,
        )
        .expect("should fuse mandelbrot iterate");
        assert!(out.contains("fused_iterate_body"), "{out}");
        assert!(out.contains("loop {"), "{out}");
        assert!(out.contains("(i == 0i64)"), "{out}");
        assert!(out.contains("return 0i64;"), "{out}");
        assert!(out.contains("((zr * zr) + (zi * zi)) > 4f64"), "{out}");
        assert!(out.contains("return i;"), "{out}");
        assert!(out.contains("let __tmp_zr: f64"), "{out}");
        assert!(out.contains("let __tmp_i: i64"), "{out}");
        assert!(out.contains("zr = __tmp_zr;"), "{out}");
        assert!(out.contains("i = __tmp_i;"), "{out}");
        assert!(out.contains("continue;"), "{out}");
        assert!(out.contains("pub struct FusedIterateKernel"), "{out}");
        assert!(out.contains("impl<R: ::graphix_compiler::Rt"), "{out}");
        assert!(out.contains("const NAME: &'static str = \"pkg_iterate\";"), "{out}");
        assert!(out.contains("get_as_unchecked::<f64>"), "{out}");
        assert!(out.contains("get_as_unchecked::<i64>"), "{out}");
        assert!(out.contains("::netidx::subscriber::Value::I64(__r)"), "{out}");
        println!("{out}");
    }

    #[test]
    fn walk_mandelbrot_and_report() {
        let src = r#"
            let rec iterate = |zr: f64, zi: f64, cr: f64, ci: f64, i: i64| -> i64
                select i {
                    0 => 0,
                    _ if zr * zr + zi * zi > 4.0 => i,
                    _ => iterate(zr * zr - zi * zi + cr, 2.0 * zr * zi + ci, cr, ci, i - 1)
                }
        "#;
        let e = parse_one(src).expect("parse");
        let reports = walk_and_fuse(&e);
        assert_eq!(reports.len(), 1, "should see exactly the iterate lambda");
        let r = &reports[0];
        assert_eq!(r.name.as_ref().map(|s| s.as_str()), Some("iterate"));
        match &r.outcome {
            FusionOutcome::Rejected(_) => {}
            FusionOutcome::Fused(_) => {
                panic!("try_fuse_lambda uses the single-expression path");
            }
        }
    }

    #[test]
    fn walk_finds_fusable_siblings() {
        let src = r#"{
            let hyp2 = |a: f64, b: f64| a * a + b * b;
            let scaled = |a: f64, b: f64| -> f64 { let c = a + b; c * 2.0 };
            null
        }"#;
        let e = parse_one(src).expect("parse");
        let reports = walk_and_fuse(&e);
        assert_eq!(reports.len(), 2);
        let named = |reports: &[FusionReport], n: &str| {
            reports
                .iter()
                .find(|r| r.name.as_ref().map(|s| s.as_str()) == Some(n))
                .cloned()
                .expect("missing report")
        };
        match &named(&reports, "hyp2").outcome {
            FusionOutcome::Fused(src) => {
                assert!(src.contains("pub struct Fused_Hyp2"));
                assert!(src.contains("a * a"));
                assert!(src.contains("b * b"));
            }
            _ => panic!("hyp2 should fuse"),
        }
        match &named(&reports, "scaled").outcome {
            FusionOutcome::Fused(src) => {
                assert!(src.contains("pub struct Fused_Scaled"), "{src}");
                assert!(src.contains("let mut c"), "{src}");
            }
            _ => panic!("scaled should fuse via the single-expr path now"),
        }
    }

    #[test]
    fn clamp_via_cli_parse_path_fuses() {
        use crate::expr::parser as p;
        use crate::expr::{Origin, Source};
        use arcstr::ArcStr;
        let src = "let clamp = |x: i64, lo: i64, hi: i64| -> i64 {
            let lo_clamped = select x < lo { true => lo, false => x };
            select lo_clamped > hi { true => hi, false => lo_clamped }
        }";
        let ori = Origin {
            parent: None,
            source: Source::Unspecified,
            text: ArcStr::from(src),
        };
        let exprs = p::parse(ori).expect("parse");
        let mut found = false;
        for e in exprs.iter() {
            if let ExprKind::Bind(b) = &e.kind {
                if let ExprKind::Lambda(l) = &b.value.kind {
                    let out = emit_function_kernel("T", "t_clamp", "clamp", l);
                    assert!(
                        out.is_some(),
                        "clamp parsed via parser::parse should fuse"
                    );
                    found = true;
                }
            }
        }
        assert!(found, "didn't find the clamp lambda in parsed exprs");
    }

    #[test]
    fn select_as_expression_fuses() {
        let e = parse_one(
            "|x: i64, lo: i64, hi: i64| -> i64 {
                let lo_clamped = select x < lo { true => lo, false => x };
                select lo_clamped > hi { true => hi, false => lo_clamped }
            }",
        )
        .expect("parse");
        let lambda = match &e.kind {
            ExprKind::Lambda(l) => l.clone(),
            _ => panic!("expected lambda"),
        };
        let src = emit_function_kernel("FusedClamp", "pkg_clamp", "clamp", &lambda)
            .expect("clamp should fuse");
        assert!(src.contains("let mut lo_clamped"), "{src}");
        assert!(src.contains("if (x < lo)"), "{src}");
        assert!(src.contains("if (lo_clamped > hi)"), "{src}");
        println!("{src}");
    }

    #[test]
    fn select_bool_literal_arms_fuse_cleanly() {
        let ctx = FusionCtx { inputs: vec![input("x", PrimType::F64)], ..Default::default() };
        let e = parse_one("select x > 0.0 { true => x, false => 0.0 - x }")
            .expect("parse");
        let e = emit_expr(&e, &ctx).expect("should fuse");
        assert_eq!(e.typ, PrimType::F64);
        let s = rust(&e);
        assert!(s.contains("if (x > 0f64)"), "{s}");
        assert!(s.contains("else if (!(x > 0f64))"), "{s}");
    }

    #[test]
    fn annotated_callback_in_apply_fuses() {
        use crate::expr::parser as p;
        use crate::expr::{Origin, Source};
        use arcstr::ArcStr;
        let src = "let cb = array::init(10, |idx: i64| idx * 2);";
        let ori = Origin {
            parent: None,
            source: Source::Unspecified,
            text: ArcStr::from(src),
        };
        let exprs = p::parse(ori).expect("parse");
        let mut state = RewriteState::new();
        for e in exprs.iter() {
            let mut e = e.clone();
            rewrite_program_with_state(&mut e, "ua", &mut state).expect("rewrite");
        }
        let anon = state
            .kernels
            .iter()
            .find(|k| k.fn_name.starts_with("anon"))
            .expect("annotated callback should fuse");
        assert!(
            anon.rust_source.contains("(idx * 2i64)"),
            "expected native `idx * 2` in fused body: {}",
            anon.rust_source
        );
        assert!(
            anon.rust_source.contains("mut idx: i64"),
            "expected `idx: i64` from user annotation: {}",
            anon.rust_source
        );
    }

    #[test]
    fn return_type_inferred_when_annotation_missing() {
        let e = parse_one("|a: i64, b: i64| a + b").expect("parse");
        let lambda = match &e.kind {
            ExprKind::Lambda(l) => l.clone(),
            _ => panic!("expected lambda"),
        };
        let src = emit_function_kernel("FusedSum", "pkg_sum", "sum", &lambda)
            .expect("unannotated rtype should still fuse");
        assert!(src.contains("Value::I64(__r)"), "{src}");
        assert!(src.contains("-> i64"), "{src}");
    }

    #[test]
    fn anonymous_callback_lambda_fuses() {
        use crate::expr::parser as p;
        use crate::expr::{Origin, Source};
        use arcstr::ArcStr;
        let src = "let scale = 2.0; \
                   let result = some_hof(10, |x: f64| -> f64 x * scale);";
        let ori = Origin {
            parent: None,
            source: Source::Unspecified,
            text: ArcStr::from(src),
        };
        let exprs = p::parse(ori).expect("parse");
        let mut state = RewriteState::new();
        for e in exprs.iter() {
            let mut e = e.clone();
            rewrite_program_with_state(&mut e, "anon", &mut state).expect("rewrite");
        }
        let fused = state
            .kernels
            .iter()
            .find(|k| k.fn_name.starts_with("anon"))
            .expect("anonymous lambda should fuse");
        assert!(
            fused.rust_source.contains("(x * 2f64)")
                || fused.rust_source.contains("x * 2f64"),
            "scale should inline: {}",
            fused.rust_source
        );
    }

    #[test]
    fn outer_scope_constant_is_inlined() {
        use crate::expr::parser as p;
        use crate::expr::{Origin, Source};
        use arcstr::ArcStr;
        let src = "let max_iter = 64;
                   let escape = |cr: f64, ci: f64| -> i64 max_iter;";
        let ori = Origin {
            parent: None,
            source: Source::Unspecified,
            text: ArcStr::from(src),
        };
        let exprs = p::parse(ori).expect("parse");
        let mut state = RewriteState::new();
        for e in exprs.iter() {
            let mut e = e.clone();
            rewrite_program_with_state(&mut e, "ok", &mut state).expect("rewrite");
        }
        assert!(
            state.consts.contains_key(&ArcStr::from("max_iter")),
            "max_iter should be registered as a compile-time constant"
        );
        let escape = state
            .kernels
            .iter()
            .find(|k| k.fn_name == "escape")
            .expect("escape must fuse");
        assert!(
            escape.rust_source.contains("return 64i64;"),
            "expected max_iter to inline as 64i64, got:\n{}",
            escape.rust_source
        );
    }

    #[test]
    fn type_cast_and_qop_fuse() {
        let ctx = FusionCtx {
            inputs: vec![input("px", PrimType::I64), input("dx", PrimType::F64)],
            ..Default::default()
        };
        let e = parse_one("cast<f64>(px)$ * dx").expect("parse");
        let out = emit_expr(&e, &ctx).expect("should fuse");
        assert_eq!(out.typ, PrimType::F64);
        let s = rust(&out);
        assert!(s.contains("(px as f64)"), "{s}");
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

    #[test]
    fn cross_function_fusion_state_threads_across_top_level() {
        use crate::expr::parser as p;
        use crate::expr::{Origin, Source};
        use arcstr::ArcStr;
        let src = "let helper = |a: f64, b: f64| -> f64 a + b; \
                   let caller = |x: f64, y: f64| -> f64 helper(x, y) * 2.0;";
        let ori = Origin {
            parent: None,
            source: Source::Unspecified,
            text: ArcStr::from(src),
        };
        let exprs = p::parse(ori).expect("parse");
        let mut state = RewriteState::new();
        for e in exprs.iter() {
            let mut e = e.clone();
            rewrite_program_with_state(&mut e, "ts", &mut state).expect("rewrite");
        }
        assert_eq!(state.kernels.len(), 2, "both lambdas should fuse");
        let caller = state
            .kernels
            .iter()
            .find(|k| k.fn_name == "caller")
            .expect("caller kernel missing");
        assert!(
            caller.rust_source.contains("fused_helper_body"),
            "caller should inline helper: {}",
            caller.rust_source
        );
    }

    #[test]
    fn non_tail_self_recursion_fuses_as_rust_recursion() {
        let e = parse_one(
            "|n: i64| -> i64 select n {
                0 => 0,
                1 => 1,
                _ => fib(n - 1) + fib(n - 2)
            }",
        )
        .expect("parse");
        let lambda = match &e.kind {
            ExprKind::Lambda(l) => l.clone(),
            _ => panic!("expected lambda"),
        };
        let src = emit_function_kernel("FusedFib", "pkg_fib", "fib", &lambda)
            .expect("naive fib should fuse as recursive Rust");
        assert!(src.contains("fused_fib_body"), "{src}");
        let call_count = src.matches("fused_fib_body(").count();
        assert!(call_count >= 2, "expected 2+ recursive calls, got {call_count}");
        assert!(!src.contains("loop {"), "non-tail fib shouldn't use loop: {src}");
        println!("{src}");
    }

    #[test]
    fn cross_function_fusion_inlines_known_callee() {
        let src = r#"{
            let helper = |a: f64, b: f64| -> f64 a + b;
            let caller = |x: f64, y: f64| -> f64 helper(x, y) * 2.0;
            null
        }"#;
        let mut e = parse_one(src).expect("parse");
        let kernels = rewrite_program(&mut e, "cross").expect("rewrite");
        assert_eq!(kernels.len(), 2, "both lambdas should fuse, got: {kernels:?}");
        let caller = kernels
            .iter()
            .find(|k| k.fn_name == "caller")
            .expect("caller kernel missing");
        assert!(
            caller.rust_source.contains("fused_helper_body"),
            "caller should inline helper: {}",
            caller.rust_source
        );
    }

    #[test]
    fn rewrite_mandelbrot_produces_builtin_ref() {
        let src = r#"{
            let rec iterate = |zr: f64, zi: f64, cr: f64, ci: f64, i: i64| -> i64
                select i {
                    0 => 0,
                    _ if zr * zr + zi * zi > 4.0 => i,
                    _ => iterate(zr * zr - zi * zi + cr, 2.0 * zr * zi + ci, cr, ci, i - 1)
                };
            42
        }"#;
        let mut e = parse_one(src).expect("parse");
        let kernels = rewrite_program(&mut e, "fused").expect("rewrite");
        assert_eq!(kernels.len(), 1, "exactly one fusable lambda");
        let k = &kernels[0];
        assert_eq!(k.fn_name, "iterate");
        assert!(
            k.builtin_name.starts_with("fused_iterate_"),
            "{}",
            k.builtin_name
        );
        assert!(k.rust_source.contains("fused_iterate_body"));
        assert!(k.rust_source.contains("continue;"));
        let printed = format!("{}", e.kind);
        assert!(
            printed.contains(&format!("'{}", k.builtin_name)),
            "expected rewritten source to contain 'builtin ref, got:\n{printed}"
        );
        assert!(
            !printed.contains("_ if zr * zr + zi * zi > 4"),
            "iterate body was not replaced:\n{printed}"
        );
    }

    #[test]
    fn render_lib_rs_has_defpackage_and_kernel() {
        let src = r#"{
            let rec iterate = |zr: f64, zi: f64, cr: f64, ci: f64, i: i64| -> i64
                select i {
                    0 => 0,
                    _ if zr * zr + zi * zi > 4.0 => i,
                    _ => iterate(zr * zr - zi * zi + cr, 2.0 * zr * zi + ci, cr, ci, i - 1)
                };
            42
        }"#;
        let mut e = parse_one(src).expect("parse");
        let kernels = rewrite_program(&mut e, "fused").expect("rewrite");
        let pkg = emit_package("fused", kernels, format!("{}", e.kind));
        let lib_rs = render_lib_rs(&pkg);
        assert!(lib_rs.contains("fused_iterate_body"), "{lib_rs}");
        assert!(lib_rs.contains("defpackage!"), "{lib_rs}");
        assert!(
            lib_rs.contains("FusedIterate1") || lib_rs.contains("FusedIterate"),
            "{lib_rs}"
        );
        assert_eq!(render_mod_gxi(&pkg), "");
        assert_eq!(render_mod_gx(&pkg), "");
    }

    #[test]
    fn unresolved_tvar_is_refused() {
        use crate::typ::{FnArgType, FnType, Type};
        use triomphe::Arc as TArc;
        let src = "let rec f = |x| x";
        let mut e = parse_one(src).expect("parse");
        let lambda_id = match &e.kind {
            ExprKind::Bind(b) => match &b.value.kind {
                ExprKind::Lambda(_) => b.value.id,
                _ => panic!("expected lambda in bind"),
            },
            _ => panic!("expected bind"),
        };
        let ft = FnType {
            args: TArc::from_iter([FnArgType {
                label: None,
                typ: Type::empty_tvar(),
            }]),
            rtype: Type::empty_tvar(),
            ..Default::default()
        };
        let mut fn_types = nohash::IntMap::default();
        fn_types.insert(lambda_id, ft);
        let mut state = RewriteState::with_fn_types(fn_types);
        let err = rewrite_program_with_state(&mut e, "ur", &mut state)
            .expect_err("unresolved tvar should be refused");
        let msg = format!("{err:#}");
        assert!(
            msg.contains("unresolved type variable"),
            "bail message should name the cause, got: {msg}"
        );
        assert!(
            msg.contains("argument 0"),
            "bail message should point at the offending position, got: {msg}"
        );
        assert!(
            msg.contains("line") && msg.contains("column"),
            "bail message should include source position so anon callbacks \
             are findable, got: {msg}"
        );
    }
}
