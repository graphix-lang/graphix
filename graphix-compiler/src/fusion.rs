//! Node fusion: emit Rust source for pure-expression subtrees so
//! rustc can specialize them instead of leaving them as a tree of
//! `Box<dyn Update>` interpreter nodes.
//!
//! This is Step 3 of the plan at
//! `~/.claude/plans/ethereal-humming-summit.md`. The emitter starts
//! with pure arithmetic / comparison / bool subtrees over primitive
//! Graphix types, and grows outward:
//!
//! - `emit_expr` — a pure expression subtree → Rust expression.
//! - `emit_body` — a function body with `select` arms, `let`-bindings,
//!   and self-recursive tail calls → a Rust statement block that uses
//!   `return` / `continue` appropriately.
//! - `emit_function_kernel` — takes a whole fully-typed lambda and
//!   emits a standalone Rust function plus an `Apply<R, E>` builtin
//!   shim that can be registered like any other package builtin.
//!
//! The mandelbrot `iterate` function is the driving example: primitive
//! arg types, a `select` on an integer, a guard using arithmetic, and
//! a self-recursive tail call with updated args — the classic
//! "pure-function loop" pattern. Successfully emitting a loop-form
//! kernel for that is the gating test for the whole pipeline.

use crate::{
    env::Env,
    expr::{Expr, ExprKind, ModPath, Pattern, StructurePattern},
    typ::Type,
    BindId, Scope,
};
use arcstr::ArcStr;
use netidx_value::{Typ, Value};
use std::fmt::Write;

/// Every primitive Graphix type whose payload the fusion emitter knows
/// how to extract from a `Value` with `get_as_unchecked`. Intentionally
/// narrow for the first slice — strings, decimals, datetimes, bytes
/// etc. require owned/refcounted accessors and different patterns.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
}

impl PrimType {
    /// Name of the corresponding Rust primitive.
    pub fn rust_name(self) -> &'static str {
        match self {
            PrimType::I8 => "i8",
            PrimType::I16 => "i16",
            PrimType::I32 => "i32",
            PrimType::I64 => "i64",
            PrimType::U8 => "u8",
            PrimType::U16 => "u16",
            PrimType::U32 => "u32",
            PrimType::U64 => "u64",
            PrimType::F32 => "f32",
            PrimType::F64 => "f64",
            PrimType::Bool => "bool",
        }
    }

    /// Name of the `Value` variant that carries this primitive.
    pub fn value_variant(self) -> &'static str {
        match self {
            PrimType::I8 => "I8",
            PrimType::I16 => "I16",
            PrimType::I32 => "I32",
            PrimType::I64 => "I64",
            PrimType::U8 => "U8",
            PrimType::U16 => "U16",
            PrimType::U32 => "U32",
            PrimType::U64 => "U64",
            PrimType::F32 => "F32",
            PrimType::F64 => "F64",
            PrimType::Bool => "Bool",
        }
    }

    fn from_typ(t: Typ) -> Option<PrimType> {
        Some(match t {
            Typ::I8 => PrimType::I8,
            Typ::I16 => PrimType::I16,
            Typ::I32 => PrimType::I32,
            Typ::I64 => PrimType::I64,
            Typ::U8 => PrimType::U8,
            Typ::U16 => PrimType::U16,
            Typ::U32 => PrimType::U32,
            Typ::U64 => PrimType::U64,
            Typ::F32 => PrimType::F32,
            Typ::F64 => PrimType::F64,
            Typ::Bool => PrimType::Bool,
            _ => return None,
        })
    }

    /// Derive a [`PrimType`] from a fully-resolved Graphix [`Type`].
    /// Returns `None` for anything that isn't a single-variant numeric
    /// or bool primitive — union of variants, unbound TVar, or
    /// non-primitive types all disqualify the subtree from fusion.
    /// Bound TVars are transparently deref'd: after typecheck, a
    /// TVar bound to `i64` looks like a plain `i64` to the fusion
    /// emitter. Graphix's scripting mode allows TVars to remain
    /// unbound at the end of typecheck (inferred from context only
    /// as the program runs), in which case we can't emit specialized
    /// Rust for that position and the caller should either annotate
    /// or accept falling back to the interpreter.
    pub fn from_type(t: &Type) -> Option<PrimType> {
        t.with_deref(|resolved| match resolved? {
            Type::Primitive(flags) => {
                let mut iter = flags.iter();
                let first = iter.next()?;
                if iter.next().is_some() {
                    return None;
                }
                Self::from_typ(first)
            }
            _ => None,
        })
    }

    pub fn is_numeric(self) -> bool {
        !matches!(self, PrimType::Bool)
    }

    pub fn is_integer(self) -> bool {
        matches!(
            self,
            PrimType::I8
                | PrimType::I16
                | PrimType::I32
                | PrimType::I64
                | PrimType::U8
                | PrimType::U16
                | PrimType::U32
                | PrimType::U64
        )
    }

    pub fn is_float(self) -> bool {
        matches!(self, PrimType::F32 | PrimType::F64)
    }
}

/// One input to a fused kernel — a bound variable read by one or more
/// `Ref` nodes inside the subtree. `rust_name` is used in the emitted
/// body so generated identifiers are deterministic and don't collide
/// with Rust keywords.
#[derive(Debug, Clone)]
pub struct Input {
    pub name: ArcStr,
    pub prim: PrimType,
    pub bind_id: Option<BindId>,
    pub rust_name: String,
}

/// Signature of a function the emitter has already seen fuse
/// successfully. Used when one fused kernel calls another — the call
/// site lowers to a direct Rust call to the target's `body_fn_name`.
#[derive(Debug, Clone)]
pub struct KnownFusedFn {
    /// Rust name of the free fn (e.g. "fused_iterate_body"). Must be
    /// visible from the emitted kernel (same crate).
    pub body_fn_name: String,
    /// Positional argument types in declaration order. Currently only
    /// all-positional primitive-typed args are supported.
    pub arg_types: Vec<PrimType>,
    /// Return primitive type.
    pub return_type: PrimType,
}

/// A compile-time-known primitive value bound to a Graphix-level
/// name. Used to inline references to outer-scope `let x = <literal>`
/// bindings as Rust literals, so a fused kernel can reach variables
/// outside its immediate arg list.
#[derive(Debug, Clone)]
pub struct KnownConst {
    pub prim: PrimType,
    /// Rust source form — e.g. "100i64", "-2.5f64", "true".
    pub rust_src: String,
}

/// Lookup table the emitter consults whenever it sees a `Ref` — tells
/// us the variable's primitive type and the name to use in generated
/// Rust. Also carries a small registry of already-fused functions so a
/// kernel body can direct-call another fused kernel instead of
/// round-tripping through the interpreted CallSite.
#[derive(Debug, Clone, Default)]
pub struct FusionCtx {
    pub inputs: Vec<Input>,
    /// Other kernels already fused in the current pass. Used by
    /// `emit_expr` to lower `Apply { fn: Ref(name) }` to a direct Rust
    /// call when `name` is in this map. The map is keyed by the
    /// Graphix-level name (e.g. "iterate") so rewrites done earlier in
    /// the walk are visible to fusions done later.
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
}

/// Emitted Rust for a sub-expression, plus the primitive type it
/// evaluates to. The caller uses the type to decide whether a parent
/// operator is legal (e.g. `+` requires numeric operands of the same
/// type) and to choose the right `Value` variant when wrapping the
/// kernel's final result.
#[derive(Debug, Clone)]
pub struct EmittedExpr {
    pub src: String,
    pub typ: PrimType,
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

fn constant_prim(v: &Value) -> Option<(PrimType, String)> {
    use std::fmt::Display;
    fn lit<T: Display>(v: T, suffix: &str) -> String {
        format!("{v}{suffix}")
    }
    Some(match v {
        Value::I8(x) => (PrimType::I8, lit(x, "i8")),
        Value::I16(x) => (PrimType::I16, lit(x, "i16")),
        Value::I32(x) | Value::Z32(x) => (PrimType::I32, lit(x, "i32")),
        Value::I64(x) | Value::Z64(x) => (PrimType::I64, lit(x, "i64")),
        Value::U8(x) => (PrimType::U8, lit(x, "u8")),
        Value::U16(x) => (PrimType::U16, lit(x, "u16")),
        Value::U32(x) | Value::V32(x) => (PrimType::U32, lit(x, "u32")),
        Value::U64(x) | Value::V64(x) => (PrimType::U64, lit(x, "u64")),
        Value::F32(x) => (PrimType::F32, format!("{x}f32")),
        Value::F64(x) => (PrimType::F64, format!("{x}f64")),
        Value::Bool(b) => (PrimType::Bool, format!("{b}")),
        _ => return None,
    })
}

fn arith(lhs: &EmittedExpr, rhs: &EmittedExpr, op: &str) -> Option<EmittedExpr> {
    if lhs.typ != rhs.typ || !lhs.typ.is_numeric() {
        return None;
    }
    Some(EmittedExpr {
        src: format!("({} {} {})", lhs.src, op, rhs.src),
        typ: lhs.typ,
    })
}

fn cmp(lhs: &EmittedExpr, rhs: &EmittedExpr, op: &str) -> Option<EmittedExpr> {
    if lhs.typ != rhs.typ {
        return None;
    }
    Some(EmittedExpr {
        src: format!("({} {} {})", lhs.src, op, rhs.src),
        typ: PrimType::Bool,
    })
}

fn bool_op(lhs: &EmittedExpr, rhs: &EmittedExpr, op: &str) -> Option<EmittedExpr> {
    if lhs.typ != PrimType::Bool || rhs.typ != PrimType::Bool {
        return None;
    }
    Some(EmittedExpr {
        src: format!("({} {} {})", lhs.src, op, rhs.src),
        typ: PrimType::Bool,
    })
}

/// Emit a `select` at expression position as a Rust `if/else if/else`
/// chain. Every arm body must itself emit as a primitive expression,
/// and every arm body must have the same primitive type (Rust's if-
/// expression requires consistent branch types). The final arm, if
/// unconditional, becomes a bare `else { ... }`; otherwise we close
/// with `else { unreachable!() }` — Graphix typecheck enforces
/// exhaustiveness so this is a safety-net, not a hot path.
fn emit_select_as_expr(
    s: &crate::expr::SelectExpr,
    ctx: &FusionCtx,
) -> Option<EmittedExpr> {
    let scrut = emit_expr(&s.arg, ctx)?;
    let n = s.arms.len();
    if n == 0 {
        return None;
    }
    let mut out = String::new();
    let mut unified_typ: Option<PrimType> = None;
    for (i, (pat, body)) in s.arms.iter().enumerate() {
        let is_last = i == n - 1;
        let mut arm_ctx = ctx.clone();
        let cond =
            emit_arm_condition(&scrut, &pat.structure_predicate, &mut arm_ctx)?;
        let guard_src = match &pat.guard {
            None => None,
            Some(g) => {
                let g = emit_expr(g, &arm_ctx)?;
                if g.typ != PrimType::Bool {
                    return None;
                }
                Some(g.src)
            }
        };
        let combined = match (cond, guard_src) {
            (None, None) => None,
            (Some(c), None) => Some(c),
            (None, Some(g)) => Some(g),
            (Some(c), Some(g)) => Some(format!("({c}) && ({g})")),
        };
        let body_ev = emit_expr(body, &arm_ctx)?;
        match unified_typ {
            Some(t) if t != body_ev.typ => return None,
            None => unified_typ = Some(body_ev.typ),
            _ => (),
        }
        match (i, &combined) {
            (0, Some(c)) => {
                out.push_str(&format!("if {c} {{ {} }}", body_ev.src));
            }
            (0, None) => {
                // First and only or first unconditional — just emit
                // the body, wrapped in a block for uniformity.
                out.push_str(&format!("{{ {} }}", body_ev.src));
            }
            (_, Some(c)) if !is_last => {
                out.push_str(&format!(" else if {c} {{ {} }}", body_ev.src));
            }
            (_, Some(c)) => {
                // Last arm conditional — keep the condition and close
                // with an `else { unreachable!() }` for exhaustiveness.
                out.push_str(&format!(" else if {c} {{ {} }}", body_ev.src));
                out.push_str(" else { unreachable!(\"select fallthrough\") }");
            }
            (_, None) => {
                // Unconditional non-first arm — becomes `else { ... }`.
                out.push_str(&format!(" else {{ {} }}", body_ev.src));
            }
        }
    }
    let typ = unified_typ?;
    Some(EmittedExpr { src: format!("({out})"), typ })
}

/// Emit an `Apply` call whose target is a Ref to a function already
/// in `ctx.known_fns`. The call lowers to a direct Rust call against
/// the target's `body_fn_name`. Returns `None` if the target isn't
/// known, the arg arity/typing doesn't match, or any arg expression
/// can't itself be fused.
fn emit_known_fused_call(
    a: &crate::expr::ApplyExpr,
    ctx: &FusionCtx,
) -> Option<EmittedExpr> {
    let name = match &a.function.kind {
        ExprKind::Ref { name } => ident_of(name)?,
        _ => return None,
    };
    let fn_info = ctx.find_fn(name)?.clone();
    // All-positional, matching arity.
    if a.args.len() != fn_info.arg_types.len() {
        return None;
    }
    if a.args.iter().any(|(label, _)| label.is_some()) {
        return None;
    }
    let mut parts = Vec::with_capacity(a.args.len());
    for ((_, expr), expected) in a.args.iter().zip(&fn_info.arg_types) {
        let e = emit_expr(expr, ctx)?;
        if e.typ != *expected {
            return None;
        }
        parts.push(e.src);
    }
    Some(EmittedExpr {
        src: format!("{}({})", fn_info.body_fn_name, parts.join(", ")),
        typ: fn_info.return_type,
    })
}

/// Emit a Graphix `{ let x = ...; let y = ...; body }` block as a
/// Rust block expression. Each non-last statement must be a
/// `let`-binding whose value emits as a primitive expression; the
/// final statement provides the block's value.
fn emit_do_as_expr(exprs: &[Expr], ctx: &FusionCtx) -> Option<EmittedExpr> {
    if exprs.is_empty() {
        return None;
    }
    let mut local_ctx = ctx.clone();
    let last = exprs.len() - 1;
    let mut lets = String::new();
    for (i, e) in exprs.iter().enumerate() {
        if i == last {
            let body = emit_expr(e, &local_ctx)?;
            return Some(EmittedExpr {
                src: format!("{{ {}{} }}", lets, body.src),
                typ: body.typ,
            });
        }
        match &e.kind {
            ExprKind::Bind(b) => {
                if b.rec {
                    return None;
                }
                let name = b.pattern.single_bind()?;
                let value = emit_expr(&b.value, &local_ctx)?;
                lets.push_str(&format!(
                    "let mut {}: {} = {}; ",
                    name,
                    value.typ.rust_name(),
                    value.src
                ));
                local_ctx.inputs.push(Input {
                    name: name.clone(),
                    prim: value.typ,
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

/// Emit Rust source for a single sub-expression. Returns `None` if
/// any sub-tree is something the emitter doesn't handle yet — that
/// short-circuits the whole parent, so the caller falls back to the
/// interpreted path.
pub fn emit_expr(expr: &Expr, ctx: &FusionCtx) -> Option<EmittedExpr> {
    match &expr.kind {
        ExprKind::Constant(v) => {
            let (typ, src) = constant_prim(v)?;
            Some(EmittedExpr { src, typ })
        }
        ExprKind::Ref { name } => {
            let ident = ident_of(name)?;
            // Prefer lambda-arg / let-bound locals, then fall back to
            // the known-constants registry (outer-scope `let x = <lit>;`
            // bindings inlined at compile time).
            if let Some(input) = ctx.find(ident) {
                return Some(EmittedExpr {
                    src: input.rust_name.clone(),
                    typ: input.prim,
                });
            }
            if let Some(c) = ctx.find_const(ident) {
                return Some(EmittedExpr {
                    src: c.rust_src.clone(),
                    typ: c.prim,
                });
            }
            None
        }
        ExprKind::ExplicitParens(inner) => emit_expr(inner, ctx),
        ExprKind::Add { lhs, rhs } => arith(&emit_expr(lhs, ctx)?, &emit_expr(rhs, ctx)?, "+"),
        ExprKind::Sub { lhs, rhs } => arith(&emit_expr(lhs, ctx)?, &emit_expr(rhs, ctx)?, "-"),
        ExprKind::Mul { lhs, rhs } => arith(&emit_expr(lhs, ctx)?, &emit_expr(rhs, ctx)?, "*"),
        ExprKind::Div { lhs, rhs } => arith(&emit_expr(lhs, ctx)?, &emit_expr(rhs, ctx)?, "/"),
        ExprKind::Mod { lhs, rhs } => arith(&emit_expr(lhs, ctx)?, &emit_expr(rhs, ctx)?, "%"),
        ExprKind::Eq { lhs, rhs } => cmp(&emit_expr(lhs, ctx)?, &emit_expr(rhs, ctx)?, "=="),
        ExprKind::Ne { lhs, rhs } => cmp(&emit_expr(lhs, ctx)?, &emit_expr(rhs, ctx)?, "!="),
        ExprKind::Lt { lhs, rhs } => cmp(&emit_expr(lhs, ctx)?, &emit_expr(rhs, ctx)?, "<"),
        ExprKind::Gt { lhs, rhs } => cmp(&emit_expr(lhs, ctx)?, &emit_expr(rhs, ctx)?, ">"),
        ExprKind::Lte { lhs, rhs } => cmp(&emit_expr(lhs, ctx)?, &emit_expr(rhs, ctx)?, "<="),
        ExprKind::Gte { lhs, rhs } => cmp(&emit_expr(lhs, ctx)?, &emit_expr(rhs, ctx)?, ">="),
        ExprKind::And { lhs, rhs } => {
            bool_op(&emit_expr(lhs, ctx)?, &emit_expr(rhs, ctx)?, "&&")
        }
        ExprKind::Or { lhs, rhs } => {
            bool_op(&emit_expr(lhs, ctx)?, &emit_expr(rhs, ctx)?, "||")
        }
        ExprKind::Not { expr } => {
            let inner = emit_expr(expr, ctx)?;
            if inner.typ != PrimType::Bool {
                return None;
            }
            Some(EmittedExpr { src: format!("(!{})", inner.src), typ: PrimType::Bool })
        }
        // A Select at an expression position lowers to a Rust
        // `if/else-if/else` chain, with the final arm collapsed into
        // an unconditional `else` when it is `_`/`name`/unconditional
        // + no guard. Each arm body must itself emit as a primitive
        // expression of the same type.
        ExprKind::Select(s) => emit_select_as_expr(s, ctx),
        // A Do block at an expression position lowers to a Rust block
        // expression: each leading `let` is a `let`, and the final
        // expression provides the value. Non-Bind non-last statements
        // abort fusion.
        ExprKind::Do { exprs } => emit_do_as_expr(exprs, ctx),
        // Direct call to an already-fused function becomes a Rust
        // function call against the kernel's `_body` symbol. This is
        // what lets one fused kernel compose with another — the shim
        // roundtrip (Value pack → CachedVals → match → Value unpack)
        // is bypassed entirely inside fused code.
        ExprKind::Apply(a) => emit_known_fused_call(a, ctx),
        // `cast<T>(expr)` between primitives is a Rust `(expr as T)`.
        // Non-primitive casts (e.g. to string, variant) can't fuse.
        ExprKind::TypeCast { expr, typ } => {
            let target = PrimType::from_type(typ)?;
            let inner = emit_expr(expr, ctx)?;
            // Bool casts in Rust need a match rather than `as`; skip
            // that case until we meet it in practice.
            if target == PrimType::Bool || inner.typ == PrimType::Bool {
                return None;
            }
            Some(EmittedExpr {
                src: format!("({} as {})", inner.src, target.rust_name()),
                typ: target,
            })
        }
        // `$` (or-never) and `?` (qop) on a post-typecheck expression
        // assert that the inner value succeeded. For a fused kernel
        // the types were proven at compile time, so we just emit the
        // inner expression. If the runtime would actually error (e.g.
        // an array index out of bounds), our fusion candidate-selection
        // will reject the expression upstream — we only emit fused
        // code for operations that can't fail.
        ExprKind::OrNever(inner) | ExprKind::Qop(inner) => emit_expr(inner, ctx),
        // Deliberately not-yet-supported: Bind (at non-statement
        // position), Lambda, checked arithmetic, Sample, and anything
        // reactive. Presence of those inside a fusion candidate
        // aborts the attempt.
        _ => None,
    }
}

/// Information about a self-recursive function, used by `emit_body` to
/// detect tail calls and lower them to a loop `continue`.
///
/// When `self_info` is `Some`, the caller wraps the emitted body in a
/// `loop { ... }` and every `continue` transfers control back to the
/// loop head with the args mutated in place. When it is `None`, tail
/// positions simply `return` their computed value.
#[derive(Debug, Clone)]
pub struct SelfInfo {
    /// The graphix name being bound to the lambda (e.g. "iterate").
    pub name: ArcStr,
    /// Arg names in signature order. Must match the lambda's argspec.
    /// These are the mutable loop variables updated by each tail call.
    pub params: Vec<Input>,
}

/// Emit a sequence of Rust statements that evaluate `expr` as the body
/// of a function. Handles:
///
/// - Pure expressions: wrapped in `return <expr>;` (the "tail" emit).
/// - Self-recursive tail calls (when `self_info` is `Some`): lowered
///   to temp-assign + direct-assign + `continue`.
/// - `select` over primitive scrutinees: lowered to a sequence of
///   `if`-guards, one per arm, each ending in a return or continue.
/// - `let`-style bindings inside a Do block: lowered to Rust `let`s
///   that introduce new primitive-typed inputs visible to the tail.
///
/// Returns `None` if any sub-expression isn't in the supported subset.
///
/// The emitted source does NOT include the outer `fn` signature or
/// `loop { ... }` wrapper — the caller is responsible for that.
pub fn emit_body(
    expr: &Expr,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
) -> Option<String> {
    let mut out = String::new();
    emit_body_into(&mut out, expr, ctx, self_info, 0)?;
    Some(out)
}

fn indent_str(out: &mut String, indent: usize) {
    for _ in 0..indent {
        out.push_str("    ");
    }
}

fn emit_body_into(
    out: &mut String,
    expr: &Expr,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
    indent: usize,
) -> Option<()> {
    match &expr.kind {
        ExprKind::Do { exprs } => emit_do(out, exprs, ctx, self_info, indent),
        ExprKind::ExplicitParens(inner) => {
            emit_body_into(out, inner, ctx, self_info, indent)
        }
        ExprKind::Select(s) => emit_select(out, s, ctx, self_info, indent),
        _ => emit_tail(out, expr, ctx, self_info, indent),
    }
}

/// Emit a Do block as function-body statements: each non-last expr
/// becomes a Rust statement (Bind, or value discarded); the last one
/// is a tail expression.
fn emit_do(
    out: &mut String,
    exprs: &[Expr],
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
    indent: usize,
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
            emit_body_into(out, e, &local_ctx, self_info, indent)?;
        } else {
            // Everything before the last must be a binding (we can't
            // fuse arbitrary statement sequences — Graphix blocks only
            // return the last expr's value). A plain expression with a
            // side-effect would be unfusable anyway.
            match &e.kind {
                ExprKind::Bind(b) => {
                    emit_bind_stmt(out, b, &mut local_ctx, indent)?;
                }
                // Tolerate a no-op filler (e.g. a trailing `;` quirk).
                ExprKind::NoOp => {}
                _ => return None,
            }
        }
    }
    Some(())
}

/// Emit a `let`-style binding as a Rust `let <name> = <expr>;` and
/// extend the ctx so later emissions can see the new input.
fn emit_bind_stmt(
    out: &mut String,
    b: &crate::expr::BindExpr,
    ctx: &mut FusionCtx,
    indent: usize,
) -> Option<()> {
    // Only simple single-name non-recursive bindings in the fusable
    // subset — recursive lets, destructuring, and anything with a
    // non-primitive value all fall back to the interpreter.
    if b.rec {
        return None;
    }
    let name = b.pattern.single_bind()?;
    let value = emit_expr(&b.value, ctx)?;
    let rust_name = name.to_string();
    indent_str(out, indent);
    writeln!(out, "let mut {} = {};", rust_name, value.src).ok()?;
    ctx.inputs.push(Input {
        name: name.clone(),
        prim: value.typ,
        bind_id: None,
        rust_name,
    });
    Some(())
}

/// Emit a `select` expression as a series of `if` / `if let` guards.
/// Each arm's body becomes a statement that either returns or
/// continues a surrounding loop; the caller wraps the whole thing in
/// that loop if `self_info` is set.
///
/// Exhaustiveness: we don't prove it here. If the scrutinee doesn't
/// match any arm, we fall out of the select and emit
/// `unreachable!()` as a belt-and-braces guard — the Graphix
/// typechecker already requires exhaustiveness, so this should never
/// fire at runtime, but fused code is unsafe and we'd rather fail
/// loudly than UB.
fn emit_select(
    out: &mut String,
    s: &crate::expr::SelectExpr,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
    indent: usize,
) -> Option<()> {
    let scrut = emit_expr(&s.arg, ctx)?;
    indent_str(out, indent);
    writeln!(out, "// select {}", s.arg.kind).ok()?;
    // Emit arm by arm. Most arms are `if <cond> { body }`. The last
    // arm, if unconditional (Ignore or Bind with no guard), is emitted
    // inline — no wrapping `if { ... }` — so the terminating
    // `unreachable!()` comes up correctly typed and the generated code
    // doesn't drown in warnings. All other arms (conditional or not-
    // last-unconditional) get a wrapping `if`.
    let n = s.arms.len();
    let mut last_was_unconditional = false;
    for (i, (pat, arm_body)) in s.arms.iter().enumerate() {
        let is_last = i == n - 1;
        let unconditional =
            emit_arm(out, &scrut, pat, arm_body, ctx, self_info, indent, is_last)?;
        if is_last {
            last_was_unconditional = unconditional;
        }
    }
    // If the final arm isn't an unconditional one, fall back to the
    // typecheck-should-forbid guard. If it IS unconditional, the body
    // above already returns or continues, so no guard is needed.
    if !last_was_unconditional {
        indent_str(out, indent);
        writeln!(
            out,
            "unreachable!(\"select fell through — typecheck should forbid\");"
        )
        .ok()?;
    }
    Some(())
}

/// Emit one `select` arm. Returns `Some(true)` if the arm is
/// unconditional (i.e. its body runs whenever control reaches it).
/// Returns `Some(false)` if the arm is conditional (wrapped in `if`).
/// Returns `None` if emission failed.
fn emit_arm(
    out: &mut String,
    scrut: &EmittedExpr,
    pat: &Pattern,
    arm_body: &Expr,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
    indent: usize,
    is_last: bool,
) -> Option<bool> {
    // Type-predicate can be ignored for the kinds of patterns we
    // currently support (all operate on a single-variant primitive
    // scrutinee). If a type_predicate is present it must match the
    // scrutinee's type — but since the scrutinee itself carries one
    // primitive type, we've already satisfied any reasonable
    // predicate. This is consistent with how StructPatternNode sees it.
    let mut arm_ctx = ctx.clone();
    let cond = emit_arm_condition(scrut, &pat.structure_predicate, &mut arm_ctx)?;
    // Guards are arbitrary pure expressions over the scrutinee and any
    // names bound by the pattern.
    let guard_src = match &pat.guard {
        None => None,
        Some(g) => {
            let g = emit_expr(g, &arm_ctx)?;
            if g.typ != PrimType::Bool {
                return None;
            }
            Some(g.src)
        }
    };
    let combined = match (cond, guard_src) {
        (None, None) => None,
        (Some(c), None) => Some(c),
        (None, Some(g)) => Some(g),
        (Some(c), Some(g)) => Some(format!("({c}) && ({g})")),
    };
    match combined {
        None if is_last => {
            // Unconditional last arm — inline the body, skip the `if`.
            emit_body_into(out, arm_body, &arm_ctx, self_info, indent)?;
            Some(true)
        }
        None => {
            // Unconditional non-last arm — still uses `if true { ... }`
            // so any subsequent (guarded) arms retain their semantics.
            // This is unusual in practice because an unconditional
            // middle arm shadows anything after it, but keep the
            // straightforward lowering for now.
            indent_str(out, indent);
            writeln!(out, "{{").ok()?;
            emit_body_into(out, arm_body, &arm_ctx, self_info, indent + 1)?;
            indent_str(out, indent);
            writeln!(out, "}}").ok()?;
            Some(true)
        }
        Some(cond) => {
            indent_str(out, indent);
            writeln!(out, "if {cond} {{").ok()?;
            emit_body_into(out, arm_body, &arm_ctx, self_info, indent + 1)?;
            indent_str(out, indent);
            writeln!(out, "}}").ok()?;
            Some(false)
        }
    }
}

/// Emit the boolean test for an arm's structure predicate. Returns
/// `None` if the emitter can't express the predicate (e.g. variants,
/// tuples, arrays — not supported yet). Returns `Ok(None)` (packed as
/// `Some(None)` below) if the test always succeeds (Ignore or Bind).
/// Mutates `arm_ctx` to add any bindings introduced by the pattern.
fn emit_arm_condition(
    scrut: &EmittedExpr,
    pat: &StructurePattern,
    arm_ctx: &mut FusionCtx,
) -> Option<Option<String>> {
    match pat {
        StructurePattern::Ignore => Some(None),
        StructurePattern::Bind(name) => {
            // Introduce a let-like binding for this pattern's name.
            let rust_name = name.to_string();
            arm_ctx.inputs.push(Input {
                name: name.clone(),
                prim: scrut.typ,
                bind_id: None,
                rust_name,
            });
            Some(None)
        }
        StructurePattern::Literal(v) => {
            let (lit_typ, lit_src) = constant_prim(v)?;
            if lit_typ != scrut.typ {
                return None;
            }
            // Small simplification for bool literals: lowering
            // `scrut == true` / `scrut == false` to `scrut` / `!scrut`
            // keeps the emitted code tidy (and rustc emits the same
            // machine code either way).
            if lit_typ == PrimType::Bool {
                match v {
                    Value::Bool(true) => return Some(Some(scrut.src.clone())),
                    Value::Bool(false) => {
                        return Some(Some(format!("(!{})", scrut.src)));
                    }
                    _ => {}
                }
            }
            Some(Some(format!("({} == {})", scrut.src, lit_src)))
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

/// Emit a "tail" expression — either a pure expression that returns a
/// value, or a self-recursive tail call that continues the loop. Also
/// supports `ExprKind::Bind` followed by another tail (via Do block),
/// but that path is handled by `emit_do`; this function is for the
/// leaf position in a select arm.
fn emit_tail(
    out: &mut String,
    expr: &Expr,
    ctx: &FusionCtx,
    self_info: Option<&SelfInfo>,
    indent: usize,
) -> Option<()> {
    // Self-recursive tail call: only recognize it if we know we're
    // emitting a function body that can `continue`.
    if let Some(self_info) = self_info {
        if let Some(()) = try_emit_tail_call(out, expr, ctx, self_info, indent) {
            return Some(());
        }
    }
    // Nested select inside an arm body — descend.
    if let ExprKind::Select(s) = &expr.kind {
        return emit_select(out, s, ctx, self_info, indent);
    }
    // Nested Do — descend. emit_do handles its own tail.
    if let ExprKind::Do { exprs } = &expr.kind {
        return emit_do(out, exprs, ctx, self_info, indent);
    }
    if let ExprKind::ExplicitParens(inner) = &expr.kind {
        return emit_tail(out, inner, ctx, self_info, indent);
    }
    // Plain expression: evaluate and return.
    let v = emit_expr(expr, ctx)?;
    indent_str(out, indent);
    writeln!(out, "return {};", v.src).ok()?;
    Some(())
}

/// Try to emit `expr` as a tail call to `self_info.name`. Returns
/// `None` if it isn't a self-call, or if the args can't all be fused.
fn try_emit_tail_call(
    out: &mut String,
    expr: &Expr,
    ctx: &FusionCtx,
    self_info: &SelfInfo,
    indent: usize,
) -> Option<()> {
    let apply = match &expr.kind {
        ExprKind::Apply(a) => a,
        _ => return None,
    };
    // The function must be a bare Ref to self_info.name.
    let fn_ref = match &apply.function.kind {
        ExprKind::Ref { name } => name,
        _ => return None,
    };
    let fn_ident = ident_of(fn_ref)?;
    if fn_ident != self_info.name.as_str() {
        return None;
    }
    // All args must be positional (no labeled args) and match arity.
    if apply.args.len() != self_info.params.len() {
        return None;
    }
    if apply.args.iter().any(|(label, _)| label.is_some()) {
        return None;
    }
    // Evaluate each new arg into a temp so we don't clobber a param we
    // still need to read for a later arg.
    let mut evaluated: Vec<(String, EmittedExpr)> = Vec::with_capacity(apply.args.len());
    for ((_, arg_expr), param) in apply.args.iter().zip(&self_info.params) {
        let e = emit_expr(arg_expr, ctx)?;
        if e.typ != param.prim {
            return None;
        }
        let tmp = format!("__tmp_{}", param.rust_name);
        evaluated.push((tmp, e));
    }
    for ((tmp_name, e), param) in evaluated.iter().zip(&self_info.params) {
        indent_str(out, indent);
        writeln!(out, "let {}: {} = {};", tmp_name, param.prim.rust_name(), e.src)
            .ok()?;
    }
    for ((tmp_name, _), param) in evaluated.iter().zip(&self_info.params) {
        indent_str(out, indent);
        writeln!(out, "{} = {};", param.rust_name, tmp_name).ok()?;
    }
    indent_str(out, indent);
    writeln!(out, "continue;").ok()?;
    Some(())
}

/// Emit a complete fused-kernel struct as Rust source. The struct
/// implements `Update<R, E>` and evaluates `body` by pulling each
/// input via `get_as_unchecked`, running the emitted expression, and
/// wrapping the result in the appropriate `Value` variant.
///
/// The caller supplies `struct_name` and the inputs; later, the
/// fusion pass will derive both by walking the subtree.
pub fn emit_kernel(
    struct_name: &str,
    body: &Expr,
    ctx: &FusionCtx,
) -> Option<String> {
    let result = emit_expr(body, ctx)?;
    let result_variant = result.typ.value_variant();
    let result_rust = result.typ.rust_name();

    let mut out = String::new();
    writeln!(out, "// AUTO-GENERATED by graphix fusion pass. Do not edit by hand.").ok()?;
    writeln!(out, "// Fused body: {}", body.kind).ok()?;
    writeln!(out).ok()?;
    writeln!(out, "#[derive(Debug)]").ok()?;
    writeln!(out, "pub struct {struct_name} {{").ok()?;
    writeln!(out, "    pub spec: ::graphix_compiler::expr::Expr,").ok()?;
    writeln!(out, "    pub typ: ::graphix_compiler::typ::Type,").ok()?;
    for input in &ctx.inputs {
        writeln!(out, "    pub {}_id: ::graphix_compiler::BindId,", input.rust_name).ok()?;
    }
    writeln!(out, "}}").ok()?;
    writeln!(out).ok()?;
    writeln!(
        out,
        "impl<R: ::graphix_compiler::Rt, E: ::graphix_compiler::UserEvent>"
    ).ok()?;
    writeln!(out, "    ::graphix_compiler::Update<R, E> for {struct_name}").ok()?;
    writeln!(out, "{{").ok()?;
    writeln!(
        out,
        "    fn update("
    ).ok()?;
    writeln!(out, "        &mut self,").ok()?;
    writeln!(out, "        _ctx: &mut ::graphix_compiler::ExecCtx<R, E>,").ok()?;
    writeln!(out, "        event: &mut ::graphix_compiler::Event<E>,").ok()?;
    writeln!(out, "    ) -> ::std::option::Option<::netidx::subscriber::Value> {{").ok()?;
    // Touch: on init, read everything from ctx.cached; otherwise just
    // from event.variables. For this first slice we keep the simpler
    // read and the caller enforces that all inputs have a value.
    for input in &ctx.inputs {
        writeln!(
            out,
            "        let {} = unsafe {{",
            input.rust_name,
        ).ok()?;
        writeln!(
            out,
            "            *event.variables.get(&self.{}_id)?.get_as_unchecked::<{}>()",
            input.rust_name,
            input.prim.rust_name(),
        ).ok()?;
        writeln!(out, "        }};").ok()?;
    }
    writeln!(
        out,
        "        let result: {result_rust} = {};",
        result.src,
    ).ok()?;
    writeln!(
        out,
        "        ::std::option::Option::Some(::netidx::subscriber::Value::{result_variant}(result))",
    ).ok()?;
    writeln!(out, "    }}").ok()?;
    writeln!(out).ok()?;
    writeln!(out, "    fn spec(&self) -> &::graphix_compiler::expr::Expr {{ &self.spec }}").ok()?;
    writeln!(out, "    fn typ(&self) -> &::graphix_compiler::typ::Type {{ &self.typ }}").ok()?;
    writeln!(out, "    fn refs(&self, refs: &mut ::graphix_compiler::Refs) {{").ok()?;
    for input in &ctx.inputs {
        writeln!(out, "        refs.refed.insert(self.{}_id);", input.rust_name).ok()?;
    }
    writeln!(out, "    }}").ok()?;
    writeln!(out, "    fn delete(&mut self, _ctx: &mut ::graphix_compiler::ExecCtx<R, E>) {{}}").ok()?;
    writeln!(out, "    fn sleep(&mut self, _ctx: &mut ::graphix_compiler::ExecCtx<R, E>) {{}}").ok()?;
    writeln!(out, "    fn typecheck(&mut self, _ctx: &mut ::graphix_compiler::ExecCtx<R, E>) -> ::anyhow::Result<()> {{").ok()?;
    writeln!(out, "        Ok(())").ok()?;
    writeln!(out, "    }}").ok()?;
    writeln!(out, "}}").ok()?;
    Some(out)
}

/// Check whether a (parsed, not-yet-lowered) expression contains at
/// least one tail call to `name`. Used to decide whether to wrap an
/// emitted body in a `loop { ... }`.
/// Try to determine the primitive return type of a function body
/// without requiring an explicit `-> T` annotation. Walks the body
/// structurally, looking at the tail position:
/// - a Do block's type is its last expression's
/// - a Select's type is the first arm whose body we can type
/// - an Apply to a known-fused function gives its declared return
/// - self-recursive calls contribute nothing (the type we'd be
///   inferring *is* the rtype — it's circular), so they're skipped
/// - any other pure expression's type is what `emit_expr` reports
///
/// Used when a lambda lacks an `-> T` annotation — the fusion pass
/// falls back to this rather than bailing. If all paths return
/// `None`, fusion can't proceed.
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

/// Emit a complete "function-shaped" fused kernel as Rust source. This
/// produces three pieces of code, ready to paste into a generated
/// module:
///
/// 1. A free `fn fused_<name>_body(...)` whose arguments match the
///    lambda's argspec and whose body is the emitted loop / select /
///    arithmetic.
/// 2. A struct `struct_name` with a `CachedVals` field.
/// 3. `BuiltIn<R, E>` and `Apply<R, E>` impls that pull primitives out
///    of the cached values and call the free function.
///
/// The caller picks `struct_name`, `builtin_name` (which must start
/// with the hosting package's prefix — see graphix-derive's
/// `defpackage!`), and `fn_name` (the graphix-level function name, used
/// for self-recursion detection).
///
/// Returns `None` if any argument lacks a primitive type annotation or
/// the body isn't entirely fusable.
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

/// Same as `emit_function_kernel`, but also accepts a registry of
/// already-fused functions (so this kernel can inline-call them) and
/// returns the fused-function signature alongside the source. The
/// rewrite pass uses this form to chain-fuse dependent kernels.
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

/// Like `emit_function_kernel_with_known`, but additionally accepts a
/// registry of compile-time-known primitive constants. A `Ref` in the
/// lambda body that doesn't match a lambda arg falls back to this
/// registry and emits as the corresponding Rust literal.
pub fn emit_function_kernel_with_known_and_consts(
    struct_name: &str,
    builtin_name: &str,
    fn_name: &str,
    lambda: &crate::expr::LambdaExpr,
    known: &std::collections::BTreeMap<ArcStr, KnownFusedFn>,
    consts: &std::collections::BTreeMap<ArcStr, KnownConst>,
) -> Option<(String, KnownFusedFn)> {
    use netidx::utils::Either;
    let body = match &lambda.body {
        Either::Left(e) => e,
        Either::Right(_) => return None,
    };
    // Build the input list from the lambda's args. Every arg must have
    // a primitive type annotation and a single-bind pattern.
    let mut ctx = FusionCtx {
        inputs: vec![],
        known_fns: known.clone(),
        known_consts: consts.clone(),
    };
    let mut params: Vec<Input> = Vec::new();
    let mut arg_types: Vec<PrimType> = Vec::new();
    for arg in lambda.args.iter() {
        if arg.labeled.is_some() {
            // Labeled args complicate the tail-call shape; skip for the
            // first slice.
            return None;
        }
        let typ = arg.constraint.as_ref()?;
        let prim = PrimType::from_type(typ)?;
        let name = arg.pattern.single_bind()?;
        let input = Input {
            name: name.clone(),
            prim,
            bind_id: None,
            rust_name: name.to_string(),
        };
        params.push(input.clone());
        ctx.inputs.push(input);
        arg_types.push(prim);
    }
    let self_info = SelfInfo { name: ArcStr::from(fn_name), params: params.clone() };
    // Resolve the return type: explicit annotation first, otherwise
    // infer by walking the body's tail. Inference covers typical
    // graphix patterns — last-let value, select arms, direct calls to
    // already-fused functions.
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
    // calls in the body lower as direct Rust recursive calls rather
    // than tripping the "unknown function" path in emit_expr. Tail
    // calls still go through the `loop { continue; }` lowering via
    // SelfInfo — `emit_tail` checks that path first.
    ctx.known_fns.insert(ArcStr::from(fn_name), signature.clone());
    // Emit the body at the indent level it will sit at inside the
    // generated function: indent 2 (8 spaces) when wrapped in `loop`,
    // indent 1 (4 spaces) when not.
    let body_indent = if has_tail { 2 } else { 1 };
    let mut body_src = String::new();
    emit_body_into(&mut body_src, body, &ctx, Some(&self_info), body_indent)?;

    let mut out = String::new();
    writeln!(out, "// AUTO-GENERATED by graphix fusion pass. Do not edit by hand.").ok()?;
    writeln!(out, "// Source lambda: {}", fn_name).ok()?;
    writeln!(out).ok()?;

    // Free function — the real workhorse. The `allow` block covers
    // parens/unreachable/mut spam from the straightforward lowering;
    // they don't signal bugs, they're just noise rustc would shout
    // about on a clean-style codebase.
    writeln!(
        out,
        "#[allow(unused_parens, unreachable_code, unused_mut, clippy::too_many_arguments)]"
    )
    .ok()?;
    write!(out, "#[inline]\npub fn fused_{fn_name}_body(").ok()?;
    let mut first = true;
    for p in &params {
        if !first {
            out.push_str(", ");
        }
        first = false;
        write!(out, "mut {}: {}", p.rust_name, p.prim.rust_name()).ok()?;
    }
    writeln!(out, ") -> {} {{", rprim.rust_name()).ok()?;
    if has_tail {
        writeln!(out, "    loop {{").ok()?;
    }
    // body_src is already indented to the right column by emit_body_into
    // (indent=2 inside loop, indent=1 without), so emit verbatim.
    out.push_str(&body_src);
    if has_tail {
        writeln!(out, "    }}").ok()?;
    }
    writeln!(out, "}}").ok()?;
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
    // kernel was picked up by the fusion pass (graphix-shell runs
    // --check first) and proved every arg's type, which is what
    // drove the arg-type choices in params[]. get_as_unchecked::<T>
    // is sound precisely when the Value's tag matches T's variant,
    // and the typechecker enforces exactly that invariant — so we
    // can skip the tag match at runtime.
    //
    // If the typechecker invariant is ever violated (e.g. we wired a
    // kernel into a call site whose arg types disagree with params),
    // the resulting UB would manifest as wrong output, not a crash —
    // which is why (a) the typechecker is the type-source-of-truth
    // and (b) the rewrite pass refuses to fuse a kernel whose
    // PrimType::from_type lookup doesn't agree with the Apply's
    // resolved FnType.
    writeln!(out, "        unsafe {{").ok()?;
    for (i, p) in params.iter().enumerate() {
        writeln!(
            out,
            "            let __a{i}: {rust} = *self.args.0.get_unchecked({i}).as_ref().unwrap_unchecked().get_as_unchecked::<{rust}>();",
            i = i,
            rust = p.prim.rust_name(),
        ).ok()?;
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

/// Walk an `Expr` and build a `FusionCtx` by resolving every bare-
/// identifier `Ref` against the env. Returns `None` if the subtree
/// contains anything the emitter can't handle — any compound `ModPath`
/// (`a::b`), any non-primitive bound type, or any unresolvable name.
///
/// This is the "input discovery" half of the fusion pass: emit_expr
/// won't see a Ref it doesn't already know about, so we must find
/// every Ref first, learn its `(BindId, Type)`, and stash that in the
/// ctx before calling the emitter.
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
                // Dedup: one Ref entry per unique source name.
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
            // Everything else aborts the whole discovery: we don't
            // want to silently fuse over a sub-expression the emitter
            // can't handle.
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
    /// Name the lambda was bound to (if any), useful for generated
    /// struct names and for logs.
    pub name: Option<ArcStr>,
    /// ExprId of the Lambda expression.
    pub id: crate::expr::ExprId,
    /// The emitted Rust source, or an explanation of why fusion
    /// failed.
    pub outcome: FusionOutcome,
}

#[derive(Debug, Clone)]
pub enum FusionOutcome {
    /// Lambda is fully fusable; here's the Rust source.
    Fused(String),
    /// Fusion was attempted but rejected. The reason is deliberately
    /// coarse right now — we only know whether arg typing or body
    /// fusion is what failed. The caller can refine as the pass grows.
    Rejected(&'static str),
}

/// Walk a whole program (parsed but not necessarily typechecked) and
/// attempt fusion for every lambda we encounter — both top-level
/// definitions and nested let-bound ones. Non-lambda sub-expressions
/// are traversed for their children but not themselves reported.
///
/// This is the "probe the program and tell me what would fuse" entry
/// point. In the full AOT pipeline it would feed a code-generator
/// that emits one kernel per positive outcome.
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
            // Also descend into the lambda body so we find nested
            // let-bound lambdas inside it.
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
        // For the rest: recurse into every Arc<Expr> subfield we care
        // about. We only need enough coverage to find lambdas — we
        // don't have to visit every leaf. This list is deliberately
        // conservative; missing variants just mean we don't look
        // inside them for nested lambdas.
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
    // "iterate" -> "FusedIterate"; anything non-alphanumeric becomes
    // an underscore so we don't produce invalid Rust idents for
    // weird names (they basically can't reach this path in practice
    // because the parser won't let you bind such names, but be safe).
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
/// body isn't entirely fusable. This is the natural entry point for
/// the future pass that walks top-level definitions and fuses the
/// pure ones.
pub fn try_fuse_lambda(
    lambda: &crate::expr::LambdaExpr,
    struct_name: &str,
) -> Option<String> {
    use netidx::utils::Either;
    let body = match &lambda.body {
        Either::Left(e) => e,
        Either::Right(_) => return None, // builtin shims aren't fused
    };
    let mut ctx = FusionCtx::default();
    for arg in lambda.args.iter() {
        let typ = arg.constraint.as_ref()?;
        let prim = PrimType::from_type(typ)?;
        // Only simple single-bind patterns for the first slice.
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

// ── `graphix compile`-style rewrite pipeline ────────────────────────
//
// `FusedKernel` holds the output of a single successful fusion: the
// emitted Rust source plus the struct name and builtin name. `rewrite`
// walks a whole program, attempts function-kernel fusion on every
// top-level `let [rec] iterate = |...| ...` binding, and — for each
// lambda that fuses — mutates the binding so its body becomes a
// builtin-reference `Either::Right(builtin_name)` instead of the
// original Graphix expression. The rewritten Expr + the collected
// kernels is everything `graphix compile` needs to emit a package.

#[derive(Debug, Clone)]
pub struct FusedKernel {
    /// Name of the struct emitted into Rust (`FusedIterateAuto`).
    pub struct_name: String,
    /// Value of the `NAME` const — must start with `package_prefix`.
    pub builtin_name: String,
    /// The original graphix function name (for logs / comments).
    pub fn_name: String,
    /// Emitted Rust source, ready to drop into the generated crate.
    pub rust_source: String,
}

/// Walk a program and rewrite every fusable `let name = <Lambda>`
/// binding into `let name = |...| 'builtin_prefix_name_N`. Mutates the
/// Expr in place and returns the kernels it emitted. `package_prefix`
/// is prepended to every builtin name; `defpackage!` requires that.
///
/// Only top-level and immediately-nested lambda-binding rewrites are
/// performed. Nested Lambda-inside-arbitrary-expr cases are left
/// alone — the benefit of fusing a lambda depends on how it's called,
/// and surfacing that here would duplicate work better done later.
pub fn rewrite_program(
    expr: &mut Expr,
    package_prefix: &str,
) -> anyhow::Result<Vec<FusedKernel>> {
    let mut state = RewriteState::new();
    rewrite_program_with_state(expr, package_prefix, &mut state)?;
    Ok(state.kernels)
}

/// State carried across multiple `rewrite_program_with_state` calls so
/// that later top-level expressions can see kernels fused from earlier
/// top-level expressions. The `graphix compile` CLI uses this when the
/// input file is a sequence of top-level `let`s: helper + caller.
#[derive(Debug, Default)]
pub struct RewriteState {
    pub kernels: Vec<FusedKernel>,
    pub known: std::collections::BTreeMap<ArcStr, KnownFusedFn>,
    /// Compile-time-known primitive constants accumulated as the
    /// rewrite pass descends. Used by emit_expr to inline references
    /// to outer-scope `let <name> = <literal>;` bindings.
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
    pub fn_types: fxhash::FxHashMap<crate::expr::ExprId, crate::typ::FnType>,
}

impl RewriteState {
    pub fn new() -> Self {
        Self::default()
    }

    /// Build a RewriteState seeded with typechecker-derived function
    /// types. Prefer this over `new()` whenever you have the types
    /// from a real compile() pass — the rewrite pass can then reject
    /// programs where it would have fused something the typechecker
    /// would have rejected, and emit Rust whose types exactly match
    /// what the interpreter sees.
    pub fn with_fn_types(
        fn_types: fxhash::FxHashMap<crate::expr::ExprId, crate::typ::FnType>,
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
    fn_types: &fxhash::FxHashMap<crate::expr::ExprId, crate::typ::FnType>,
) -> anyhow::Result<()> {
    // Helper: try to fuse a lambda bound to `name`. On success, mutate
    // the lambda in-place so its body becomes an Either::Right builtin
    // reference and push a FusedKernel into `kernels`. On failure, we
    // distinguish two cases:
    //
    // - The typechecker left an unresolved TVar somewhere in the
    //   lambda's FnType — the user can fix this by adding an explicit
    //   type annotation. In AOT mode we bail so the user finds out;
    //   silently running their "fast" program in the interpreter is
    //   exactly the surprise `compile` is supposed to eliminate.
    //
    // - The body has concrete but non-primitive types, reactive
    //   constructs, or other constructs the emitter doesn't handle.
    //   Not user-fixable — log::info and move on.
    fn try_rewrite_bind(
        name: &ArcStr,
        lambda_expr: &mut Expr,
        prefix: &str,
        kernels: &mut Vec<FusedKernel>,
        known: &mut std::collections::BTreeMap<ArcStr, KnownFusedFn>,
        consts: &std::collections::BTreeMap<ArcStr, KnownConst>,
        counter: &mut u32,
        fn_types: &fxhash::FxHashMap<crate::expr::ExprId, crate::typ::FnType>,
    ) -> anyhow::Result<()> {
        use netidx::utils::Either;
        let ExprKind::Lambda(lambda_arc) = &mut lambda_expr.kind else {
            return Ok(());
        };
        // Already a builtin shim — nothing to do.
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
        // If the typechecker gave us a resolved FnType for this
        // lambda, inject its arg types / rtype onto the LambdaExpr
        // *before* running the emitter — so the emitter's existing
        // code reads them straight off the argspec without needing
        // its own inference machinery.
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
                // Replace the body with the builtin reference. Since
                // LambdaExpr is inside a triomphe::Arc, we need to
                // clone-modify-swap.
                let new_lambda = crate::expr::LambdaExpr {
                    args: lambda_arc.args.clone(),
                    vargs: lambda_arc.vargs.clone(),
                    rtype: lambda_arc.rtype.clone(),
                    constraints: lambda_arc.constraints.clone(),
                    throws: lambda_arc.throws.clone(),
                    body: Either::Right(ArcStr::from(builtin_name.as_str())),
                };
                *lambda_arc = triomphe::Arc::new(new_lambda);
                // Register so subsequent fusions can inline a call.
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
                // For anonymous callback lambdas the rewriter synthesizes
                // `anon<ExprId>` as the name — useless for a user-facing
                // error. Strip it so source position does all the talking.
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
            // Descend into call args to find anonymous callback
            // lambdas (e.g. `array::init(n, |idx| <body>)`). The
            // only type source is the typechecker-derived `fn_types`
            // map: look up this Apply's ExprId to get the callee's
            // resolved FnType, then read the expected arg type of
            // any position that holds a lambda. If no fn_types entry
            // exists (caller didn't populate the map), the lambda
            // stays unannotated and will fail to fuse later.
            let apply_id = expr.id;
            let apply_fn_type = fn_types.get(&apply_id).cloned();
            // (Optional trace: set GRAPHIX_FUSION_DEBUG=1 to see
            // which Apply sites have fn_types entries. Useful when
            // an expected fusion doesn't happen — the typechecker
            // may have left a tvar unbound, in which case the user
            // needs to add a type annotation.)
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
        _ => {
            // We don't descend into every ExprKind variant. The
            // typical shape we want to fuse is a Bind at a Do/Module
            // level; everything else can wait.
        }
    }
    Ok(())
}

fn sanitize(s: &str) -> String {
    s.chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect()
}

/// Inject argument types from a real typechecker-derived FnType into
/// a callback lambda that was missing type annotations. Reads the
/// outer HOF's arg type at `arg_position` — which should itself be a
/// function type — and copies its arg types onto the lambda's argspec
/// wherever the user didn't already annotate.
///
/// `arg_position` is the lambda's position in the outer Apply's arg
/// list; the FnType's Nth arg is our Lambda, and that FnType has its
/// OWN callback-signature `Type::Fn(inner)` giving us the types we
/// need to inject.
fn inject_lambda_arg_types_from_fntype(
    lambda_expr: &mut Expr,
    arg_position: usize,
    hof_fntype: &crate::typ::FnType,
) {
    let Some(outer_arg) = hof_fntype.args.get(arg_position) else {
        return;
    };
    // The outer arg's declared type should itself be a function
    // type — we only care about positions where the HOF expects a
    // callback. Follow Type::ByRef wrappers and TVar indirections.
    outer_arg.typ.with_deref(|resolved| {
        let Some(resolved) = resolved else { return };
        let stripped = strip_byref(resolved);
        let crate::typ::Type::Fn(cb_fntype) = stripped else {
            return;
        };
        apply_fntype_args_to_lambda(lambda_expr, cb_fntype);
    });
}

/// Apply arg types AND return type from a FnType onto the Lambda's
/// argspec / rtype, filling in anything the user omitted. Does not
/// overwrite user-provided annotations — those win even if the
/// typechecker saw something more specific via unification (the
/// conflict will have surfaced during check already).
fn apply_fntype_to_lambda(
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
        // Use a primitive representation when the typechecker's type
        // reduces to one. Non-primitive types fall back to whatever
        // the emitter does today (fail fusion) — that's the correct
        // behaviour for a primitive-only emitter subset.
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

/// If any arg type or the return type of `ft` is an unresolved TVar,
/// return a short description of the first such position. This is
/// used to decide whether a failed fusion attempt should bail
/// (annotation missing → user-fixable) or skip silently (everything
/// else). Only the top level is inspected; nested types (e.g. a
/// callback argument's own args) can stay polymorphic without
/// blocking fusion of the outer lambda, since we only ever emit
/// primitive-shaped lambdas and won't try to fuse a lambda whose arg
/// is itself a function type.
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

/// Like `apply_fntype_to_lambda` but takes `&mut Expr` — callers
/// holding an Expr that wraps a Lambda can just pass it in. No-op
/// when the Expr's kind isn't Lambda.
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
        PrimType::I8 => Typ::I8,
        PrimType::I16 => Typ::I16,
        PrimType::I32 => Typ::I32,
        PrimType::I64 => Typ::I64,
        PrimType::U8 => Typ::U8,
        PrimType::U16 => Typ::U16,
        PrimType::U32 => Typ::U32,
        PrimType::U64 => Typ::U64,
        PrimType::F32 => Typ::F32,
        PrimType::F64 => Typ::F64,
        PrimType::Bool => Typ::Bool,
    };
    Type::Primitive(typ.into())
}

/// If `value` is a compile-time-computable primitive expression in
/// terms of already-known constants, record `(name, KnownConst)` into
/// `consts`. The Rust source stored on the KnownConst is the full
/// emitted expression — rustc's constant folding collapses it to a
/// literal at build time, so every inline use pays zero runtime cost.
fn record_const_binding(
    name: &ArcStr,
    value: &Expr,
    consts: &mut std::collections::BTreeMap<ArcStr, KnownConst>,
) {
    // Direct Constant is trivially constant.
    if let ExprKind::Constant(v) = &value.kind {
        if let Some((prim, rust_src)) = constant_prim(v) {
            consts.insert(name.clone(), KnownConst { prim, rust_src });
            return;
        }
    }
    // Expression-valued bindings: try to emit them as an expression
    // using only the known-const registry (empty inputs list). If
    // emit_expr succeeds, every Ref inside resolved to a constant —
    // we can freeze the whole expression as a Rust source string.
    let probe = FusionCtx {
        inputs: vec![],
        known_fns: std::collections::BTreeMap::new(),
        known_consts: consts.clone(),
    };
    if let Some(e) = emit_expr(value, &probe) {
        consts.insert(name.clone(), KnownConst { prim: e.typ, rust_src: e.src });
    }
}

/// Sources for a complete generated graphix-package crate. Each field
/// is the textual contents of one file. The `Cargo.toml` is a
/// template: `{workspace_root}` is the placeholder for the
/// absolute path needed to point path-deps at the in-workspace
/// graphix crates.
#[derive(Debug, Clone)]
pub struct EmittedPackage {
    /// Short package name (no `graphix-package-` prefix).
    pub short_name: String,
    /// Kernel sources concatenated, ready to go into `src/lib.rs`
    /// after the standard header / `defpackage!` block.
    pub kernel_sources: Vec<FusedKernel>,
    /// The rewritten main program, suitable for `src/graphix/main.gx`.
    pub main_gx: String,
}

/// Bundle the rewritten program + emitted kernels into the textual
/// sources of a graphix-package crate. The caller is responsible for
/// writing those to disk and invoking `graphix-package`'s
/// `build_standalone`.
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

/// Build the full `src/lib.rs` contents for an emitted package.
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

/// Build the `src/graphix/mod.gxi` and `src/graphix/mod.gx` contents.
/// For a compile-only package the module exports nothing (the main
/// program stands on its own), so both files are empty. We return
/// explicit empty strings so callers always write both files.
pub fn render_mod_gxi(_pkg: &EmittedPackage) -> &'static str {
    ""
}

pub fn render_mod_gx(_pkg: &EmittedPackage) -> &'static str {
    ""
}

/// Build the `Cargo.toml` for an emitted package. `graphix_src_root`
/// is the absolute path to the workspace that contains graphix-compiler
/// (etc.), because the emitted crate needs `path = ...` deps to match.
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
fxhash = "0.2"
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

/// Write all generated files to a directory matching graphix-package
/// layout. Creates `src/lib.rs`, `src/graphix/main.gx`, `src/graphix/
/// mod.gx`, `src/graphix/mod.gxi`, and `Cargo.toml`. Intended to be
/// followed by `graphix-package::GraphixPM::build_standalone`.
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
        assert_eq!(e.src, "(((zr * zr) + (zi * zi)) > 4f64)");
    }

    #[test]
    fn reject_type_mismatch() {
        // Adding i64 + f64 is not fusable: emitter refuses to insert a
        // cast because that's a language-level question, not a
        // codegen one.
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
        // Sanity: the generated source has the struct, the unsafe
        // extraction, the expression, and the Value wrapping.
        assert!(src.contains("pub struct FusedEscaped"));
        assert!(src.contains("zr_id"));
        assert!(src.contains("get_as_unchecked::<f64>"));
        assert!(src.contains("(zr * zr) + (zi * zi)"));
        assert!(src.contains("Value::F64"));
        // Echo it for inspection — this test deliberately prints so
        // `cargo test -- --nocapture emit_full_kernel` shows the
        // emitter output.
        println!("{src}");
    }

    // ── Tests against the real parser ──────────────────────────

    fn parse_fuse(source: &str, ctx: &FusionCtx) -> Option<EmittedExpr> {
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
        // The source-level precedence becomes `a + (b * c)` after parsing;
        // our emitter keeps that structure.
        assert_eq!(e.src, "(a + (b * c))");
    }

    #[test]
    fn parser_mandelbrot_escape_test() {
        // The fusable inner part of mandelbrot's iterate guard.
        let ctx = FusionCtx { inputs: vec![input("zr", PrimType::F64), input("zi", PrimType::F64)], ..Default::default() };
        let e = parse_fuse("zr * zr + zi * zi > 4.0", &ctx).expect("should fuse");
        assert_eq!(e.typ, PrimType::Bool);
        assert_eq!(e.src, "(((zr * zr) + (zi * zi)) > 4f64)");
    }

    #[test]
    fn parser_lambda_full_kernel() {
        // `|zr: f64, zi: f64, cr: f64| zr * zr - zi * zi + cr` — the
        // new-zr step of mandelbrot's iterate recurrence. It's a pure
        // f64 lambda and should fully fuse.
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
        // Let-bindings inside a lambda body are fusable; the emitter
        // treats each `let x = ...` as a Rust `let x = ...` and makes
        // `x` visible to the tail.
        let e = parse_one("|a: f64, b: f64| -> f64 { let c = a + b; c * 2.0 }")
            .expect("parse");
        let lambda = match &e.kind {
            ExprKind::Lambda(l) => l.clone(),
            _ => panic!("expected lambda"),
        };
        let src = emit_function_kernel("FusedScaled", "pkg_scaled", "scaled", &lambda)
            .expect("should fuse via emit_function_kernel");
        // The emitted function body: `let mut c = ...; return c * 2.0;`
        assert!(src.contains("let mut c = (a + b)"), "source: {src}");
        assert!(src.contains("return (c * 2f64);"), "source: {src}");
        println!("{src}");
    }

    #[test]
    fn parser_lambda_without_annotation_is_rejected() {
        // Without an `: f64` annotation on `a`, we don't know the
        // primitive type and can't pick a Value variant — refuse.
        let e = parse_one("|a, b: f64| a + b").expect("parse");
        let lambda = match &e.kind {
            ExprKind::Lambda(l) => l.clone(),
            _ => panic!("expected lambda"),
        };
        assert!(try_fuse_lambda(&lambda, "WontFuse").is_none());
    }

    #[test]
    fn mandelbrot_iterate_fuses_end_to_end() {
        // The real target of this session's work: produce a complete
        // Rust kernel for mandelbrot's iterate function and sanity-
        // check that it has the expected overall shape (loop, tail
        // call with tmp vars, escape guard, base case).
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
        // Structural expectations:
        assert!(out.contains("fused_iterate_body"), "{out}");
        assert!(out.contains("loop {"), "{out}");
        // Base case: i == 0 → return 0
        assert!(out.contains("(i == 0i64)"), "{out}");
        assert!(out.contains("return 0i64;"), "{out}");
        // Escape guard: zr*zr + zi*zi > 4.0 → return i
        assert!(out.contains("((zr * zr) + (zi * zi)) > 4f64"), "{out}");
        assert!(out.contains("return i;"), "{out}");
        // Tail call: assign tmps then update loop vars then continue
        assert!(out.contains("let __tmp_zr: f64"), "{out}");
        assert!(out.contains("let __tmp_i: i64"), "{out}");
        assert!(out.contains("zr = __tmp_zr;"), "{out}");
        assert!(out.contains("i = __tmp_i;"), "{out}");
        assert!(out.contains("continue;"), "{out}");
        // Apply shim
        assert!(out.contains("pub struct FusedIterateKernel"), "{out}");
        assert!(out.contains("impl<R: ::graphix_compiler::Rt"), "{out}");
        assert!(out.contains("const NAME: &'static str = \"pkg_iterate\";"), "{out}");
        // Invocation bits: unsafe fast-path extracts primitives
        // directly via get_as_unchecked::<T> after args.update returns
        // true (which guarantees every slot is Some).
        assert!(out.contains("get_as_unchecked::<f64>"), "{out}");
        assert!(out.contains("get_as_unchecked::<i64>"), "{out}");
        assert!(out.contains("::netidx::subscriber::Value::I64(__r)"), "{out}");
        println!("{out}");
    }

    #[test]
    fn walk_mandelbrot_and_report() {
        // `iterate` from the mandelbrot benchmark — walker should find
        // it. But note: `walk_and_fuse` currently goes through
        // `try_fuse_lambda` which uses `emit_kernel` (the
        // single-expression shape), not `emit_function_kernel` (the
        // function-shaped one). The Select-based body isn't a single
        // expression, so walk_and_fuse still rejects it via that path —
        // the function-shaped emission is exercised directly in the
        // "end-to-end" test above.
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
        // The old emit_kernel-based try_fuse_lambda only handles single-
        // expression bodies; the function-shape emit_function_kernel
        // is what handles Select.
        match &r.outcome {
            FusionOutcome::Rejected(_) => {}
            FusionOutcome::Fused(_) => {
                panic!("try_fuse_lambda uses the single-expression path");
            }
        }
    }

    #[test]
    fn walk_finds_fusable_siblings() {
        // Two top-level lambdas — both have primitive arg types, but
        // one body is a single expression (fuses via try_fuse_lambda
        // and emit_kernel) and the other has a let-binding (requires
        // emit_function_kernel). walk_and_fuse currently routes through
        // try_fuse_lambda, so only the first one is "fused" from its
        // perspective.
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
        // `scaled` with a let-binding now fuses too — emit_expr learned
        // to lower `Do` blocks to Rust block expressions, so even the
        // single-expression path through try_fuse_lambda handles it.
        match &named(&reports, "scaled").outcome {
            FusionOutcome::Fused(src) => {
                assert!(src.contains("pub struct Fused_Scaled"), "{src}");
                // Do-block lowered to a Rust block with a let
                assert!(src.contains("let mut c"), "{src}");
            }
            _ => panic!("scaled should fuse via the single-expr path now"),
        }
    }

    #[test]
    fn clamp_via_cli_parse_path_fuses() {
        // Reproduce the exact CLI path: parse a top-level `let clamp
        // = ...;` and feed through the same fusion pipeline the
        // `graphix compile` subcommand uses.
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
                    let out =
                        emit_function_kernel("T", "t_clamp", "clamp", l);
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
        // `select` nested inside a `let`-bound value must lower to a
        // Rust if/else-if/else expression so the outer `let` still
        // works. This is the `clamp` shape.
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
        // let-bound select becomes a Rust let with an if-expression.
        // Bool literal patterns collapse from `(cond == true)` /
        // `(cond == false)` to `cond` / `(!cond)`.
        assert!(src.contains("let mut lo_clamped"), "{src}");
        assert!(src.contains("if (x < lo)"), "{src}");
        // The returning select at the end becomes a chain of
        // if-statements (not an if-expression, since this is at body
        // position).
        assert!(src.contains("if (lo_clamped > hi)"), "{src}");
        println!("{src}");
    }

    #[test]
    fn select_bool_literal_arms_fuse_cleanly() {
        // A 2-arm Select over a bool scrutinee should produce valid,
        // compilable Rust. This is the `abs` shape: `select x > 0.0
        // { true => x, false => 0.0 - x }`.
        let ctx = FusionCtx { inputs: vec![input("x", PrimType::F64)], ..Default::default() };
        let e = parse_one("select x > 0.0 { true => x, false => 0.0 - x }")
            .expect("parse");
        let e = emit_expr(&e, &ctx).expect("should fuse");
        assert_eq!(e.typ, PrimType::F64);
        // The `true`-arm pattern simplifies to the raw bool expression;
        // the `false`-arm to its negation.
        assert!(e.src.contains("if (x > 0f64)"), "{}", e.src);
        assert!(e.src.contains("else if (!(x > 0f64))"), "{}", e.src);
    }

    #[test]
    fn annotated_callback_in_apply_fuses() {
        // An annotated callback passed as an Apply arg should fuse
        // via the anon-lambda descent path. (The unannotated-in-HOF
        // case requires typechecker-derived fn_types and is covered
        // end-to-end by the differential corpus.)
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
        // A named lambda with no `-> T`: rtype is inferred from the
        // body's tail expression.
        let e = parse_one("|a: i64, b: i64| a + b").expect("parse");
        let lambda = match &e.kind {
            ExprKind::Lambda(l) => l.clone(),
            _ => panic!("expected lambda"),
        };
        let src = emit_function_kernel("FusedSum", "pkg_sum", "sum", &lambda)
            .expect("unannotated rtype should still fuse");
        // Return type inferred as i64; the Apply shim wraps with I64.
        assert!(src.contains("Value::I64(__r)"), "{src}");
        assert!(src.contains("-> i64"), "{src}");
    }

    #[test]
    fn anonymous_callback_lambda_fuses() {
        // An anonymous `|idx|` passed as an argument to some HOF
        // should still be fused — the rewrite pass synthesizes a
        // name from the lambda's ExprId. The outer HOF call sees a
        // shim lambda that dispatches to native code.
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
        // The anon lambda should have fused (uses scale from outer).
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
        // `max_iter` is a top-level `let max_iter = 64;`. The lambda
        // below references it but doesn't take it as an arg. With
        // const-inlining wired up, the fusion pass recognizes the
        // constant and emits it as a Rust literal `64i64` at the Ref
        // site.
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
        // `cast<f64>(px)$` lowers to `(px as f64)` — the cast is a
        // Rust primitive cast, and `$` is a typecheck-proven assertion
        // that the value exists, stripped by the emitter.
        let ctx = FusionCtx {
            inputs: vec![input("px", PrimType::I64), input("dx", PrimType::F64)],
            ..Default::default()
        };
        let e = parse_one("cast<f64>(px)$ * dx").expect("parse");
        let out = emit_expr(&e, &ctx).expect("should fuse");
        assert_eq!(out.typ, PrimType::F64);
        assert!(out.src.contains("(px as f64)"), "{}", out.src);
    }

    #[test]
    fn cast_non_primitive_rejected() {
        // Cast to/from bool uses a match in Rust, not `as`. Until we
        // add that, we refuse to fuse bool casts.
        let ctx = FusionCtx {
            inputs: vec![input("x", PrimType::Bool)],
            ..Default::default()
        };
        let e = parse_one("cast<i64>(x)").expect("parse");
        assert!(emit_expr(&e, &ctx).is_none());
    }

    #[test]
    fn cross_function_fusion_state_threads_across_top_level() {
        // Across TWO top-level `let`s (not inside one Do block) — as
        // the `graphix compile` CLI sees them after parse(). The
        // shared RewriteState must carry the first fusion's
        // signature through to the second's emission so the second
        // can inline a direct call.
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
        // Naive fibonacci — the recursion sits in the middle of an
        // Add, not in tail position. With self registered in
        // known_fns inside emit_function_kernel, emit_expr lowers the
        // self-calls as direct Rust recursive calls.
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
        // Contains the recursive call as a direct Rust function call,
        // not an interpreter-dispatched Apply.
        assert!(src.contains("fused_fib_body"), "{src}");
        // The body has TWO recursive calls (n-1, n-2) in a single Add.
        let call_count = src.matches("fused_fib_body(").count();
        assert!(call_count >= 2, "expected 2+ recursive calls, got {call_count}");
        // AND we should NOT have wrapped in loop{} (since no tail call).
        // NB: the emitted call-in-match-arm looks like `return (...);`
        //     so we check for `return (fused_fib_body(...)`.
        assert!(!src.contains("loop {"), "non-tail fib shouldn't use loop: {src}");
        println!("{src}");
    }

    #[test]
    fn cross_function_fusion_inlines_known_callee() {
        // `helper` fuses first; then `caller` sees `helper` in the
        // known-fns registry and can inline a direct Rust call to
        // `fused_helper_body` rather than bailing on the Apply.
        let src = r#"{
            let helper = |a: f64, b: f64| -> f64 a + b;
            let caller = |x: f64, y: f64| -> f64 helper(x, y) * 2.0;
            null
        }"#;
        let mut e = parse_one(src).expect("parse");
        let kernels = rewrite_program(&mut e, "cross").expect("rewrite");
        assert_eq!(kernels.len(), 2, "both lambdas should fuse, got: {kernels:?}");
        // The `caller` kernel must contain a direct call to
        // `fused_helper_body` (not a fallback/interpreted path).
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
        // Feed rewrite_program the exact mandelbrot top-level. The
        // iterate binding should come out as a lambda whose body is
        // `Either::Right(<builtin name>)`, and we should get one
        // FusedKernel back with Rust source that references it.
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
        // Verify the rewritten expression pretty-prints using the
        // `'builtin_name` form for iterate's body.
        let printed = format!("{}", e.kind);
        assert!(
            printed.contains(&format!("'{}", k.builtin_name)),
            "expected rewritten source to contain 'builtin ref, got:\n{printed}"
        );
        // And it should NOT still contain the original select-based
        // body for iterate.
        assert!(
            !printed.contains("_ if zr * zr + zi * zi > 4"),
            "iterate body was not replaced:\n{printed}"
        );
    }

    #[test]
    fn render_lib_rs_has_defpackage_and_kernel() {
        // Full pipeline: parse → rewrite → render Rust package.
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
        // mod.gxi and mod.gx are empty for compile-only packages
        assert_eq!(render_mod_gxi(&pkg), "");
        assert_eq!(render_mod_gx(&pkg), "");
    }

    #[test]
    fn unresolved_tvar_is_refused() {
        // A lambda whose FnType has an unresolved TVar in arg
        // position can't be lowered to native code. AOT mode treats
        // this as an error so the user gets a signal — silently
        // running in the interpreter would defeat the point of
        // `graphix compile`.
        use crate::typ::{FnArgType, FnType, Type};
        use triomphe::Arc as TArc;
        let src = "let rec f = |x| x";
        let mut e = parse_one(src).expect("parse");
        // Find the lambda's ExprId inside the Bind.
        let lambda_id = match &e.kind {
            ExprKind::Bind(b) => match &b.value.kind {
                ExprKind::Lambda(_) => b.value.id,
                _ => panic!("expected lambda in bind"),
            },
            _ => panic!("expected bind"),
        };
        // Construct a FnType whose arg type and return type are both
        // unbound TVars — exactly what the typechecker would leave
        // behind for `|x| x` in a module that never calls it.
        let ft = FnType {
            args: TArc::from_iter([FnArgType {
                label: None,
                typ: Type::empty_tvar(),
            }]),
            rtype: Type::empty_tvar(),
            ..Default::default()
        };
        let mut fn_types = fxhash::FxHashMap::default();
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
