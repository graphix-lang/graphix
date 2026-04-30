//! Typed kernel intermediate representation.
//!
//! This is the shared form between the fusion analysis (which decides
//! what's fusable and produces a KIR tree) and the backends that lower
//! KIR to executable code:
//!
//! - `kir_to_rust_*` — produces Rust source for the AOT path
//!   (`graphix compile`). The emitted source goes into a generated
//!   `graphix-package-<name>` crate and is compiled by rustc.
//!
//! - `kir_to_clif` (in `crate::jit`, future) — produces Cranelift IR
//!   for the JIT path. The compiled function pointer lives inside a
//!   `CraneliftNode<R, E>` wrapped around the would-be interpreter
//!   apply, with lazy compile + atomic swap.
//!
//! Both backends share the fusion-side analysis. Anything KIR can't
//! represent is by definition not fusable; the fusion pass falls back
//! to the interpreter for that subtree.
//!
//! ## Shape
//!
//! - [`KirExpr`] is a typed expression node — every expression carries
//!   its [`PrimType`]. The `op` field is a [`KirOp`] enum spanning all
//!   supported expression forms.
//!
//! - [`KirStmt`] is a statement that appears in a function body —
//!   `let`-bindings, `return`, statement-form `select` chains, and
//!   self-tail-calls. Function bodies are `Vec<KirStmt>`.
//!
//! - [`KirKernel`] is a complete function: name, params, return type,
//!   `has_tail_loop` flag (whether the body ends with a self-recursive
//!   tail call and thus needs a surrounding `loop {}`), and body.
//!
//! Expression-form blocks (`{ let a = ..; let b = ..; tail }`) live
//! inside [`KirOp::Block`] with a flat list of [`Let`]s and a tail
//! [`KirExpr`]. Expression-form `select` lowers to [`KirOp::IfChain`]
//! — a sequence of (cond, value) pairs where the last entry's `cond`
//! may be `None` for an unconditional `else`.

use crate::{typ::Type, BindId};
use arcstr::ArcStr;
use netidx_value::{Typ, Value};
use std::fmt::Write;

// ─── Primitive types ─────────────────────────────────────────────

/// Every primitive Graphix type whose payload the kernel emitter knows
/// how to extract from a `Value` with `get_as_unchecked`. Intentionally
/// narrow: strings, decimals, datetimes, bytes etc. require owned /
/// refcounted accessors and different patterns.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

    pub fn from_typ(t: Typ) -> Option<PrimType> {
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
    /// Bound TVars are transparently deref'd: after typecheck, a TVar
    /// bound to `i64` looks like a plain `i64` here. Graphix's scripting
    /// mode allows TVars to remain unbound at the end of typecheck, in
    /// which case we can't emit specialized code for that position and
    /// the caller should annotate or fall back to the interpreter.
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

    pub fn is_signed(self) -> bool {
        matches!(
            self,
            PrimType::I8
                | PrimType::I16
                | PrimType::I32
                | PrimType::I64
                | PrimType::F32
                | PrimType::F64
        )
    }
}

// ─── Operators ───────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl BinOp {
    pub fn rust_op(self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Lte,
    Gte,
}

impl CmpOp {
    pub fn rust_op(self) -> &'static str {
        match self {
            CmpOp::Eq => "==",
            CmpOp::Ne => "!=",
            CmpOp::Lt => "<",
            CmpOp::Gt => ">",
            CmpOp::Lte => "<=",
            CmpOp::Gte => ">=",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BoolOp {
    And,
    Or,
}

impl BoolOp {
    pub fn rust_op(self) -> &'static str {
        match self {
            BoolOp::And => "&&",
            BoolOp::Or => "||",
        }
    }
}

// ─── Constants ───────────────────────────────────────────────────

/// A primitive-typed compile-time constant. Used by `KirOp::Const` and
/// when we recognise an outer-scope `let x = <literal>` for inlining.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ConstVal {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    Bool(bool),
}

impl ConstVal {
    pub fn typ(&self) -> PrimType {
        match self {
            ConstVal::I8(_) => PrimType::I8,
            ConstVal::I16(_) => PrimType::I16,
            ConstVal::I32(_) => PrimType::I32,
            ConstVal::I64(_) => PrimType::I64,
            ConstVal::U8(_) => PrimType::U8,
            ConstVal::U16(_) => PrimType::U16,
            ConstVal::U32(_) => PrimType::U32,
            ConstVal::U64(_) => PrimType::U64,
            ConstVal::F32(_) => PrimType::F32,
            ConstVal::F64(_) => PrimType::F64,
            ConstVal::Bool(_) => PrimType::Bool,
        }
    }

    /// Try to lift a netidx [`Value`] to a primitive constant. Returns
    /// `None` for any non-primitive variant (string, bytes, datetime,
    /// arrays, decimals, …).
    pub fn from_value(v: &Value) -> Option<ConstVal> {
        Some(match v {
            Value::I8(x) => ConstVal::I8(*x),
            Value::I16(x) => ConstVal::I16(*x),
            Value::I32(x) | Value::Z32(x) => ConstVal::I32(*x),
            Value::I64(x) | Value::Z64(x) => ConstVal::I64(*x),
            Value::U8(x) => ConstVal::U8(*x),
            Value::U16(x) => ConstVal::U16(*x),
            Value::U32(x) | Value::V32(x) => ConstVal::U32(*x),
            Value::U64(x) | Value::V64(x) => ConstVal::U64(*x),
            Value::F32(x) => ConstVal::F32(*x),
            Value::F64(x) => ConstVal::F64(*x),
            Value::Bool(b) => ConstVal::Bool(*b),
            _ => return None,
        })
    }

    /// Render as a Rust source literal — `42i64`, `3.14f64`, `true`.
    pub fn rust_src(&self) -> String {
        match self {
            ConstVal::I8(x) => format!("{x}i8"),
            ConstVal::I16(x) => format!("{x}i16"),
            ConstVal::I32(x) => format!("{x}i32"),
            ConstVal::I64(x) => format!("{x}i64"),
            ConstVal::U8(x) => format!("{x}u8"),
            ConstVal::U16(x) => format!("{x}u16"),
            ConstVal::U32(x) => format!("{x}u32"),
            ConstVal::U64(x) => format!("{x}u64"),
            ConstVal::F32(x) => format!("{x}f32"),
            ConstVal::F64(x) => format!("{x}f64"),
            ConstVal::Bool(b) => format!("{b}"),
        }
    }
}

// ─── Inputs and known fns/consts ─────────────────────────────────

/// One input to a fused kernel — a binding the kernel reads. `name` is
/// the source-level identifier; `rust_name` is the identifier the Rust
/// backend emits (deterministic, won't collide with Rust keywords);
/// `bind_id` is set when the input came from `discover_inputs` walking
/// a real graph, `None` for synthetic inputs (lambda args, let-bindings
/// introduced by the body emitter).
#[derive(Debug, Clone)]
pub struct Input {
    pub name: ArcStr,
    pub prim: PrimType,
    pub bind_id: Option<BindId>,
    pub rust_name: String,
}

/// Signature of a function the emitter has already seen fuse
/// successfully. Used when one fused kernel calls another — the call
/// site lowers to a direct call against the target's `body_fn_name`.
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

/// A compile-time-known primitive expression bound to a Graphix-level
/// name. Used to inline references to outer-scope bindings whose value
/// is itself a constant or an expression over already-known constants.
/// Stored as a [`KirExpr`] (rather than a precomputed string) so each
/// inline pays its own way through both backends; rustc constant-folds
/// the inlined Rust source, and the CLIF backend evaluates the same
/// SSA tree.
#[derive(Debug, Clone)]
pub struct KnownConst {
    pub expr: KirExpr,
}

impl KnownConst {
    pub fn typ(&self) -> PrimType {
        self.expr.typ
    }
}

// ─── KIR core ────────────────────────────────────────────────────

/// A typed expression node. `typ` is the primitive Graphix type the
/// expression evaluates to; `op` is the actual operation.
#[derive(Debug, Clone)]
pub struct KirExpr {
    pub op: KirOp,
    pub typ: PrimType,
}

#[derive(Debug, Clone)]
pub enum KirOp {
    /// A primitive constant.
    Const(ConstVal),
    /// Reference to a function arg or let-bound local. Identified by
    /// name; the Rust backend uses the name directly, the CLIF backend
    /// looks it up in its `Variable` table.
    Local(ArcStr),
    /// Binary arithmetic. Constructor enforces `lhs.typ == rhs.typ`
    /// and `lhs.typ.is_numeric()`; result type is the operand type.
    Bin {
        op: BinOp,
        lhs: Box<KirExpr>,
        rhs: Box<KirExpr>,
    },
    /// Comparison. `lhs.typ == rhs.typ`; result type is `Bool`.
    Cmp {
        op: CmpOp,
        lhs: Box<KirExpr>,
        rhs: Box<KirExpr>,
    },
    /// Boolean and/or. Both operands and the result are `Bool`.
    BoolBin {
        op: BoolOp,
        lhs: Box<KirExpr>,
        rhs: Box<KirExpr>,
    },
    /// Boolean negation. Operand and result are `Bool`.
    Not(Box<KirExpr>),
    /// Primitive cast (Rust `as`). Excludes bool↔integer (not
    /// supported yet — Rust uses match for those, not `as`).
    Cast {
        inner: Box<KirExpr>,
        target: PrimType,
    },
    /// Direct call to an already-fused function (looked up by name).
    /// All-positional arguments, types must match the function's
    /// declared signature.
    Call {
        fn_name: ArcStr,
        args: Vec<KirExpr>,
    },
    /// Expression-form block: `{ let a = ..; let b = ..; tail }`.
    /// All non-tail items are let-bindings; the tail provides the
    /// block's value.
    Block {
        lets: Vec<Let>,
        tail: Box<KirExpr>,
    },
    /// Expression-form if-chain. Each entry is `(condition, value)`;
    /// the last entry's condition may be `None` for an unconditional
    /// `else`. If no entry is unconditional, lowering inserts an
    /// `unreachable!()` tail (typecheck should make this unreachable
    /// in practice).
    IfChain {
        arms: Vec<(Option<KirExpr>, KirExpr)>,
    },
}

/// A single let-binding. Used in both [`KirOp::Block`] (expression-form
/// blocks) and [`KirStmt::Let`] (function bodies).
#[derive(Debug, Clone)]
pub struct Let {
    pub local: ArcStr,
    pub value: KirExpr,
}

// ─── Statement-form (function bodies) ────────────────────────────

#[derive(Debug, Clone)]
pub enum KirStmt {
    /// `let mut <local> = <value>;` — introduces a new local visible
    /// to subsequent statements.
    Let(Let),
    /// Function exit with a value.
    Return(KirExpr),
    /// Self-tail-call: assign new arg values to the loop variables and
    /// continue the surrounding `loop {}`. Only legal when the kernel
    /// has `has_tail_loop = true`. The backend chooses how to spell
    /// the temp-then-assign sequence (Rust uses temps; CLIF uses SSA).
    /// Args are stored in declaration order.
    TailCall { args: Vec<KirExpr> },
    /// Statement-form select chain. Each arm is conditional or
    /// unconditional; arms after an unconditional arm are dead. If
    /// no arm is unconditional, lowering inserts a fallthrough
    /// `unreachable!()` — typecheck should forbid this in practice.
    Select { arms: Vec<SelectArm> },
}

#[derive(Debug, Clone)]
pub struct SelectArm {
    /// `None` means unconditional (final `else`).
    pub cond: Option<KirExpr>,
    pub body: Vec<KirStmt>,
}

/// A complete kernel — one function's worth.
#[derive(Debug, Clone)]
pub struct KirKernel {
    /// Graphix-level function name (used for self-recursion detection
    /// and for naming the emitted Rust free function).
    pub fn_name: ArcStr,
    /// Parameters in declaration order. Each is also visible as a
    /// local in the body.
    pub params: Vec<Input>,
    pub return_type: PrimType,
    /// True iff the body contains a self-tail-call. Backends wrap the
    /// body in `loop { ... }` (Rust) or a back-edge to the entry block
    /// (CLIF) accordingly.
    pub has_tail_loop: bool,
    pub body: Vec<KirStmt>,
}

// ─── Constructors that enforce KIR invariants ────────────────────
//
// These are the canonical way to build typed nodes — they enforce the
// type invariants every backend assumes (matching operand types for
// arithmetic, both operands bool for boolean ops, etc.) so a malformed
// node can't sneak in. Returning `Option<KirExpr>` mirrors the existing
// fusion-pass shape: a None constructor result aborts the whole parent
// fusion attempt.

pub fn arith(lhs: KirExpr, rhs: KirExpr, op: BinOp) -> Option<KirExpr> {
    if lhs.typ != rhs.typ || !lhs.typ.is_numeric() {
        return None;
    }
    let typ = lhs.typ;
    Some(KirExpr {
        op: KirOp::Bin { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
        typ,
    })
}

pub fn cmp(lhs: KirExpr, rhs: KirExpr, op: CmpOp) -> Option<KirExpr> {
    if lhs.typ != rhs.typ {
        return None;
    }
    Some(KirExpr {
        op: KirOp::Cmp { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
        typ: PrimType::Bool,
    })
}

pub fn bool_op(lhs: KirExpr, rhs: KirExpr, op: BoolOp) -> Option<KirExpr> {
    if lhs.typ != PrimType::Bool || rhs.typ != PrimType::Bool {
        return None;
    }
    Some(KirExpr {
        op: KirOp::BoolBin { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
        typ: PrimType::Bool,
    })
}

pub fn not(inner: KirExpr) -> Option<KirExpr> {
    if inner.typ != PrimType::Bool {
        return None;
    }
    Some(KirExpr { op: KirOp::Not(Box::new(inner)), typ: PrimType::Bool })
}

pub fn cast(inner: KirExpr, target: PrimType) -> Option<KirExpr> {
    // Rust's `as` doesn't accept bool↔integer in either direction;
    // those need a `match`. Until the emitter grows that, refuse.
    if target == PrimType::Bool || inner.typ == PrimType::Bool {
        return None;
    }
    Some(KirExpr {
        op: KirOp::Cast { inner: Box::new(inner), target },
        typ: target,
    })
}

pub fn const_expr(c: ConstVal) -> KirExpr {
    let typ = c.typ();
    KirExpr { op: KirOp::Const(c), typ }
}

pub fn local(name: ArcStr, prim: PrimType) -> KirExpr {
    KirExpr { op: KirOp::Local(name), typ: prim }
}

/// True if the kernel contains a [`KirOp::Call`] anywhere — i.e.
/// non-tail self-recursion or cross-kernel calls. The AOT backend
/// handles these (rustc emits real recursion / direct calls); the
/// interpreter and JIT v1 do not. Callers wiring KIR through the
/// runtime path use this to refuse to instantiate a `KirNode` that
/// would panic on first call. Lifts in M4-followups when the kernel
/// registry is in place.
pub fn kernel_contains_call(kernel: &KirKernel) -> bool {
    kernel.body.iter().any(stmt_has_call)
}

fn stmt_has_call(stmt: &KirStmt) -> bool {
    match stmt {
        KirStmt::Let(l) => expr_has_call(&l.value),
        KirStmt::Return(e) => expr_has_call(e),
        KirStmt::TailCall { args } => args.iter().any(expr_has_call),
        KirStmt::Select { arms } => arms.iter().any(|a| {
            a.cond.as_ref().is_some_and(expr_has_call)
                || a.body.iter().any(stmt_has_call)
        }),
    }
}

fn expr_has_call(e: &KirExpr) -> bool {
    match &e.op {
        KirOp::Call { .. } => true,
        KirOp::Const(_) | KirOp::Local(_) => false,
        KirOp::Bin { lhs, rhs, .. }
        | KirOp::Cmp { lhs, rhs, .. }
        | KirOp::BoolBin { lhs, rhs, .. } => {
            expr_has_call(lhs) || expr_has_call(rhs)
        }
        KirOp::Not(inner) | KirOp::Cast { inner, .. } => expr_has_call(inner),
        KirOp::Block { lets, tail } => {
            lets.iter().any(|l| expr_has_call(&l.value)) || expr_has_call(tail)
        }
        KirOp::IfChain { arms } => arms
            .iter()
            .any(|(c, v)| c.as_ref().is_some_and(expr_has_call) || expr_has_call(v)),
    }
}

// ─── Rust backend ────────────────────────────────────────────────
//
// Lowers KIR to Rust source. The output is byte-equivalent to what the
// pre-KIR fusion emitter produced — every existing fusion test asserts
// against specific substrings, and we preserve them.
//
// Format conventions:
// - Binary / unary / cast / cmp / bool ops always wrap in parens.
// - Constants render via `ConstVal::rust_src` (`42i64`, `3.14f64`, …).
// - Block expressions: `{ <let>; <let>; <tail> }` — single-line, single
//   space separators, no trailing `;` after tail.
// - If-chain expressions wrap in parens: `(if <c1> { <e1> } else if ...)`.
// - Bodies are emitted as multi-line Rust statements with indented
//   `<indent>` levels at 4 spaces per level. Each statement ends with
//   `\n`. The caller is responsible for the surrounding `fn { … }`
//   shape and for any `loop { … }` wrapper.

/// Render a KIR expression to Rust source. Always wraps in parens
/// (consistent with the existing emitter).
pub fn kir_to_rust_expr(e: &KirExpr) -> String {
    let mut out = String::new();
    write_rust_expr(&mut out, e);
    out
}

fn write_rust_expr(out: &mut String, e: &KirExpr) {
    match &e.op {
        KirOp::Const(c) => {
            out.push_str(&c.rust_src());
        }
        KirOp::Local(name) => {
            out.push_str(name.as_str());
        }
        KirOp::Bin { op, lhs, rhs } => {
            out.push('(');
            write_rust_expr(out, lhs);
            write!(out, " {} ", op.rust_op()).ok();
            write_rust_expr(out, rhs);
            out.push(')');
        }
        KirOp::Cmp { op, lhs, rhs } => {
            out.push('(');
            write_rust_expr(out, lhs);
            write!(out, " {} ", op.rust_op()).ok();
            write_rust_expr(out, rhs);
            out.push(')');
        }
        KirOp::BoolBin { op, lhs, rhs } => {
            out.push('(');
            write_rust_expr(out, lhs);
            write!(out, " {} ", op.rust_op()).ok();
            write_rust_expr(out, rhs);
            out.push(')');
        }
        KirOp::Not(inner) => {
            out.push_str("(!");
            write_rust_expr(out, inner);
            out.push(')');
        }
        KirOp::Cast { inner, target } => {
            out.push('(');
            write_rust_expr(out, inner);
            write!(out, " as {})", target.rust_name()).ok();
        }
        KirOp::Call { fn_name, args } => {
            // Direct call to a known fused-fn body. The function lives
            // in the same generated crate so we can use the bare name.
            out.push_str(&format!("fused_{}_body(", fn_name));
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write_rust_expr(out, a);
            }
            out.push(')');
        }
        KirOp::Block { lets, tail } => {
            out.push_str("{ ");
            for l in lets {
                // Expression-form let needs a type annotation so
                // rustc's inference doesn't get tripped up when the
                // RHS is a polymorphic numeric literal.
                write!(
                    out,
                    "let mut {}: {} = ",
                    l.local,
                    l.value.typ.rust_name()
                )
                .ok();
                write_rust_expr(out, &l.value);
                out.push_str("; ");
            }
            write_rust_expr(out, tail);
            out.push_str(" }");
        }
        KirOp::IfChain { arms } => {
            out.push('(');
            let n = arms.len();
            for (i, (cond, body)) in arms.iter().enumerate() {
                let is_last = i == n - 1;
                match (i, cond) {
                    (0, Some(c)) => {
                        out.push_str("if ");
                        write_rust_expr(out, c);
                        out.push_str(" { ");
                        write_rust_expr(out, body);
                        out.push_str(" }");
                    }
                    (0, None) => {
                        // First arm unconditional — just a wrapped
                        // block, no `if`. Matches the existing emitter.
                        out.push_str("{ ");
                        write_rust_expr(out, body);
                        out.push_str(" }");
                    }
                    (_, Some(c)) if !is_last => {
                        out.push_str(" else if ");
                        write_rust_expr(out, c);
                        out.push_str(" { ");
                        write_rust_expr(out, body);
                        out.push_str(" }");
                    }
                    (_, Some(c)) => {
                        // Last arm conditional — keep the condition and
                        // close with an `else { unreachable!() }` so the
                        // expression is well-typed.
                        out.push_str(" else if ");
                        write_rust_expr(out, c);
                        out.push_str(" { ");
                        write_rust_expr(out, body);
                        out.push_str(" } else { unreachable!(\"select fallthrough\") }");
                    }
                    (_, None) => {
                        // Unconditional non-first arm becomes the `else`.
                        out.push_str(" else { ");
                        write_rust_expr(out, body);
                        out.push_str(" }");
                    }
                }
            }
            out.push(')');
        }
    }
}

fn indent_into(out: &mut String, indent: usize) {
    for _ in 0..indent {
        out.push_str("    ");
    }
}

/// Render a body (sequence of statements) to Rust source. Indented at
/// `indent` levels of 4-space indent. Used both for top-level kernel
/// bodies and (recursively) for select-arm sub-bodies.
pub fn kir_to_rust_body(stmts: &[KirStmt], indent: usize) -> String {
    let mut out = String::new();
    write_rust_body(&mut out, stmts, indent);
    out
}

fn write_rust_body(out: &mut String, stmts: &[KirStmt], indent: usize) {
    for stmt in stmts {
        write_rust_stmt(out, stmt, indent);
    }
}

fn write_rust_stmt(out: &mut String, stmt: &KirStmt, indent: usize) {
    match stmt {
        KirStmt::Let(l) => {
            indent_into(out, indent);
            // Body-position `let` matches the existing emitter: no
            // type annotation; rustc infers from the RHS, which is
            // already monomorphic in our IR. `mut` is defensive but
            // costs nothing.
            write!(out, "let mut {} = ", l.local).ok();
            write_rust_expr(out, &l.value);
            out.push_str(";\n");
        }
        KirStmt::Return(e) => {
            indent_into(out, indent);
            out.push_str("return ");
            write_rust_expr(out, e);
            out.push_str(";\n");
        }
        KirStmt::TailCall { args } => {
            // Evaluate each new arg into a temp first (so that an arg
            // expression that reads an old param value sees the old
            // value, not one we already overwrote). Then assign each
            // temp into its destination param and `continue`.
            //
            // The arg→param mapping is positional and the kernel
            // emitter's responsibility — the caller must supply the
            // KirKernel's params in `params` so this lowering can name
            // the destinations. We carry the names by stashing the
            // current kernel's params in a thread-local, but that's
            // ugly; instead we encode the destination names directly
            // by requiring the caller to attach them. For simplicity
            // here, we synthesize names from the parameter index — but
            // that loses the original names the existing tests check
            // for (`__tmp_zr`, `zr = __tmp_zr;`).
            //
            // To preserve the existing format, this function relies on
            // the caller having already split the work: in practice
            // the kernel-emit pipeline passes per-arg destination
            // names separately. See `write_rust_tail_call_with_params`
            // below, which is what the kernel emitter actually calls.
            //
            // This unparameterised form is a fallback that uses
            // numeric placeholders; it's only here so that
            // `kir_to_rust_body` is callable without extra context.
            // The existing tests go through `kir_to_rust_kernel`,
            // which calls the parameterised form.
            for (i, a) in args.iter().enumerate() {
                indent_into(out, indent);
                write!(out, "let __tmp_arg{i}: {} = ", a.typ.rust_name()).ok();
                write_rust_expr(out, a);
                out.push_str(";\n");
            }
            for (i, _) in args.iter().enumerate() {
                indent_into(out, indent);
                write!(out, "__arg{i} = __tmp_arg{i};\n").ok();
            }
            indent_into(out, indent);
            out.push_str("continue;\n");
        }
        KirStmt::Select { arms } => {
            write_rust_select(out, arms, indent);
        }
    }
}

/// Emit a tail call with caller-supplied destination param names and
/// types. This is what `kir_to_rust_kernel` uses; the unparameterised
/// `KirStmt::TailCall` lowering above is a fallback.
fn write_rust_tail_call_with_params(
    out: &mut String,
    args: &[KirExpr],
    params: &[Input],
    indent: usize,
) {
    debug_assert_eq!(args.len(), params.len(), "tail-call arity mismatch");
    for (a, p) in args.iter().zip(params.iter()) {
        indent_into(out, indent);
        write!(
            out,
            "let __tmp_{}: {} = ",
            p.rust_name,
            p.prim.rust_name()
        )
        .ok();
        write_rust_expr(out, a);
        out.push_str(";\n");
    }
    for p in params.iter() {
        indent_into(out, indent);
        writeln!(out, "{} = __tmp_{};", p.rust_name, p.rust_name).ok();
    }
    indent_into(out, indent);
    out.push_str("continue;\n");
}

fn write_rust_select(out: &mut String, arms: &[SelectArm], indent: usize) {
    let n = arms.len();
    let mut last_was_unconditional = false;
    for (i, arm) in arms.iter().enumerate() {
        let is_last = i == n - 1;
        match (&arm.cond, is_last) {
            (None, true) => {
                // Unconditional last arm — inline body, no wrapping `if`.
                write_rust_body(out, &arm.body, indent);
                last_was_unconditional = true;
            }
            (None, false) => {
                // Unconditional non-last arm — emit as a bare block so
                // any subsequent guarded arms still parse; but in
                // practice the body's tail (return/continue) ends the
                // function for that path. Subsequent arms are dead.
                indent_into(out, indent);
                out.push_str("{\n");
                write_rust_body(out, &arm.body, indent + 1);
                indent_into(out, indent);
                out.push_str("}\n");
            }
            (Some(c), _) => {
                indent_into(out, indent);
                out.push_str("if ");
                write_rust_expr(out, c);
                out.push_str(" {\n");
                write_rust_body(out, &arm.body, indent + 1);
                indent_into(out, indent);
                out.push_str("}\n");
            }
        }
    }
    if !last_was_unconditional {
        indent_into(out, indent);
        out.push_str("unreachable!(\"select fell through — typecheck should forbid\");\n");
    }
}

/// Lower a complete kernel to its Rust free-function source. Produces
/// just the `fn fused_<name>_body(...) -> T { ... }` form — the
/// surrounding `Apply<R, E>` shim and `BuiltIn<R, E>` impl are still
/// generated by `fusion::emit_function_kernel*` since they are AOT-
/// only packaging infrastructure (the JIT path uses a `CraneliftNode`
/// instead).
///
/// The output mirrors the existing emitter: an `#[allow(...)]`
/// attribute to silence parens/unreachable/mut warnings, an `#[inline]`
/// hint, the signature with `mut` params, and a `loop { … }` wrapper
/// when `has_tail_loop` is set.
pub fn kir_to_rust_kernel(kernel: &KirKernel) -> String {
    let mut out = String::new();
    writeln!(
        out,
        "#[allow(unused_parens, unreachable_code, unused_mut, clippy::too_many_arguments)]"
    )
    .ok();
    write!(out, "#[inline]\npub fn fused_{}_body(", kernel.fn_name).ok();
    let mut first = true;
    for p in &kernel.params {
        if !first {
            out.push_str(", ");
        }
        first = false;
        write!(out, "mut {}: {}", p.rust_name, p.prim.rust_name()).ok();
    }
    writeln!(out, ") -> {} {{", kernel.return_type.rust_name()).ok();
    let body_indent = if kernel.has_tail_loop { 2 } else { 1 };
    if kernel.has_tail_loop {
        writeln!(out, "    loop {{").ok();
    }
    write_rust_kernel_body(&mut out, &kernel.body, &kernel.params, body_indent);
    if kernel.has_tail_loop {
        writeln!(out, "    }}").ok();
    }
    writeln!(out, "}}").ok();
    out
}

/// Render a kernel body, threading the kernel's params through so that
/// `KirStmt::TailCall` can spell its destination assignments using the
/// real param names.
fn write_rust_kernel_body(
    out: &mut String,
    stmts: &[KirStmt],
    params: &[Input],
    indent: usize,
) {
    for stmt in stmts {
        write_rust_kernel_stmt(out, stmt, params, indent);
    }
}

fn write_rust_kernel_stmt(
    out: &mut String,
    stmt: &KirStmt,
    params: &[Input],
    indent: usize,
) {
    match stmt {
        KirStmt::Let(l) => {
            indent_into(out, indent);
            write!(out, "let mut {} = ", l.local).ok();
            write_rust_expr(out, &l.value);
            out.push_str(";\n");
        }
        KirStmt::Return(e) => {
            indent_into(out, indent);
            out.push_str("return ");
            write_rust_expr(out, e);
            out.push_str(";\n");
        }
        KirStmt::TailCall { args } => {
            write_rust_tail_call_with_params(out, args, params, indent);
        }
        KirStmt::Select { arms } => {
            write_rust_kernel_select(out, arms, params, indent);
        }
    }
}

fn write_rust_kernel_select(
    out: &mut String,
    arms: &[SelectArm],
    params: &[Input],
    indent: usize,
) {
    let n = arms.len();
    let mut last_was_unconditional = false;
    for (i, arm) in arms.iter().enumerate() {
        let is_last = i == n - 1;
        match (&arm.cond, is_last) {
            (None, true) => {
                write_rust_kernel_body(out, &arm.body, params, indent);
                last_was_unconditional = true;
            }
            (None, false) => {
                indent_into(out, indent);
                out.push_str("{\n");
                write_rust_kernel_body(out, &arm.body, params, indent + 1);
                indent_into(out, indent);
                out.push_str("}\n");
            }
            (Some(c), _) => {
                indent_into(out, indent);
                out.push_str("if ");
                write_rust_expr(out, c);
                out.push_str(" {\n");
                write_rust_kernel_body(out, &arm.body, params, indent + 1);
                indent_into(out, indent);
                out.push_str("}\n");
            }
        }
    }
    if !last_was_unconditional {
        indent_into(out, indent);
        out.push_str("unreachable!(\"select fell through — typecheck should forbid\");\n");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn i64c(x: i64) -> KirExpr {
        const_expr(ConstVal::I64(x))
    }

    fn f64c(x: f64) -> KirExpr {
        const_expr(ConstVal::F64(x))
    }

    fn loc(name: &str, prim: PrimType) -> KirExpr {
        local(ArcStr::from(name), prim)
    }

    #[test]
    fn arith_constructors_enforce_types() {
        // Same numeric type: ok.
        assert!(arith(i64c(1), i64c(2), BinOp::Add).is_some());
        assert!(arith(f64c(1.0), f64c(2.0), BinOp::Mul).is_some());
        // Mismatched types: rejected.
        assert!(arith(i64c(1), f64c(2.0), BinOp::Add).is_none());
        // Bool isn't numeric.
        let t = const_expr(ConstVal::Bool(true));
        let f = const_expr(ConstVal::Bool(false));
        assert!(arith(t, f, BinOp::Add).is_none());
    }

    #[test]
    fn rust_renders_arith_with_parens_and_suffix() {
        // `zr * zr + zi * zi > 4.0` style.
        let zr = loc("zr", PrimType::F64);
        let zi = loc("zi", PrimType::F64);
        let zr_sq = arith(zr.clone(), zr, BinOp::Mul).unwrap();
        let zi_sq = arith(zi.clone(), zi, BinOp::Mul).unwrap();
        let sum = arith(zr_sq, zi_sq, BinOp::Add).unwrap();
        let escaped = cmp(sum, f64c(4.0), CmpOp::Gt).unwrap();
        let src = kir_to_rust_expr(&escaped);
        assert_eq!(src, "(((zr * zr) + (zi * zi)) > 4f64)");
    }

    #[test]
    fn block_lets_get_type_annotations() {
        // `{ let c = a + b; c * 2.0 }` style.
        let a = loc("a", PrimType::F64);
        let b = loc("b", PrimType::F64);
        let sum = arith(a, b, BinOp::Add).unwrap();
        let c_ref = loc("c", PrimType::F64);
        let scaled = arith(c_ref, f64c(2.0), BinOp::Mul).unwrap();
        let block = KirExpr {
            op: KirOp::Block {
                lets: vec![Let { local: ArcStr::from("c"), value: sum }],
                tail: Box::new(scaled),
            },
            typ: PrimType::F64,
        };
        let src = kir_to_rust_expr(&block);
        assert!(src.contains("let mut c: f64 = (a + b);"), "{src}");
        assert!(src.contains("(c * 2f64)"), "{src}");
    }

    #[test]
    fn ifchain_unconditional_last_arm() {
        // `if x { 1 } else { 2 }` style.
        let x = loc("x", PrimType::Bool);
        let chain = KirExpr {
            op: KirOp::IfChain {
                arms: vec![
                    (Some(x), i64c(1)),
                    (None, i64c(2)),
                ],
            },
            typ: PrimType::I64,
        };
        let src = kir_to_rust_expr(&chain);
        assert_eq!(src, "(if x { 1i64 } else { 2i64 })");
    }

    #[test]
    fn ifchain_all_conditional_inserts_unreachable() {
        let x = loc("x", PrimType::Bool);
        let y = loc("y", PrimType::Bool);
        let chain = KirExpr {
            op: KirOp::IfChain {
                arms: vec![
                    (Some(x), i64c(1)),
                    (Some(y), i64c(2)),
                ],
            },
            typ: PrimType::I64,
        };
        let src = kir_to_rust_expr(&chain);
        assert!(src.contains("unreachable!"), "{src}");
        assert!(src.starts_with("(if x { 1i64 }"), "{src}");
    }

    #[test]
    fn cast_uses_as() {
        let px = loc("px", PrimType::I64);
        let casted = cast(px, PrimType::F64).unwrap();
        let src = kir_to_rust_expr(&casted);
        assert_eq!(src, "(px as f64)");
    }

    #[test]
    fn cast_bool_rejected() {
        let x = loc("x", PrimType::Bool);
        assert!(cast(x, PrimType::I64).is_none());
        let y = loc("y", PrimType::I64);
        assert!(cast(y, PrimType::Bool).is_none());
    }

    #[test]
    fn kernel_with_tail_call_wraps_loop() {
        // Mini-mandelbrot shape: one param, body is `select { 0 => 0,
        // _ => iterate(i - 1) }` style — return-or-tail-call.
        let i = Input {
            name: ArcStr::from("i"),
            prim: PrimType::I64,
            bind_id: None,
            rust_name: "i".to_string(),
        };
        let i_ref = || loc("i", PrimType::I64);
        let is_zero = cmp(i_ref(), i64c(0), CmpOp::Eq).unwrap();
        let next_i = arith(i_ref(), i64c(1), BinOp::Sub).unwrap();
        let body = vec![KirStmt::Select {
            arms: vec![
                SelectArm {
                    cond: Some(is_zero),
                    body: vec![KirStmt::Return(i64c(0))],
                },
                SelectArm {
                    cond: None,
                    body: vec![KirStmt::TailCall { args: vec![next_i] }],
                },
            ],
        }];
        let kernel = KirKernel {
            fn_name: ArcStr::from("countdown"),
            params: vec![i],
            return_type: PrimType::I64,
            has_tail_loop: true,
            body,
        };
        let src = kir_to_rust_kernel(&kernel);
        assert!(src.contains("loop {"), "{src}");
        assert!(src.contains("if (i == 0i64) {"), "{src}");
        assert!(src.contains("return 0i64;"), "{src}");
        assert!(src.contains("let __tmp_i: i64 = (i - 1i64);"), "{src}");
        assert!(src.contains("i = __tmp_i;"), "{src}");
        assert!(src.contains("continue;"), "{src}");
        assert!(src.contains("fn fused_countdown_body(mut i: i64) -> i64"), "{src}");
    }
}
