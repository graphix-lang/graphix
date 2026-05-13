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

// ─── Composite types ────────────────────────────────────────────

/// Type of a value that can flow through a fused kernel.
///
/// Variants:
/// - `Prim(p)`: scalar primitive.
/// - `Array(p)`: flat array of one primitive type. Nested arrays
///   aren't supported in v1.
/// - `Tuple(elems)`: heterogeneous fixed-arity ordered values like
///   `(i64, f64, bool)`. Same `ValArray` runtime layout as `Array`,
///   different per-slot types. Field access is by compile-time
///   index.
/// - `Struct(fields)`: heterogeneous fixed-arity values keyed by
///   alphabetically-sorted field name. Same `ValArray` layout. Field
///   access is by name → sorted-index.
///
/// Both Tuple and Struct reuse the array boundary plumbing
/// (`get_unchecked<T>(i)` per slot, `from_iter_exact` for the output);
/// they're really arrays-with-per-slot-types. Fusion lowering
/// recognises `(a, b, c)` / `{x: a, y: b}` literal forms as producer
/// ops and `t.0` / `s.field` as accessor ops.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum KirType {
    Prim(PrimType),
    Array(PrimType),
    /// `(T0, T1, T2, ...)` — order matters; per-slot types in source order.
    Tuple(Vec<PrimType>),
    /// `{field0: T0, field1: T1, ...}` — sorted alphabetically by
    /// field name (matching graphix's canonical struct layout).
    Struct(Vec<(ArcStr, PrimType)>),
    /// `` `Foo(T0, T1) | `Bar(U0) `` — tagged union of cases. Each
    /// case carries its tag (an `ArcStr`) and zero-or-more payload
    /// primitive types. At runtime, a variant value with payloads
    /// is `Value::Array([Value::String("tag"), payload0, ...])`;
    /// nullary cases (no payloads) are `Value::String("tag")` and
    /// fall outside this fusion path (the v0 emitter rejects them).
    /// The cases list is the set of all cases that can flow into
    /// this position — typecheck-derived.
    Variant(Vec<(ArcStr, Vec<PrimType>)>),
}

impl KirType {
    /// Convenience for `KirType::Prim(_)` — when callers know they have
    /// a scalar.
    pub fn prim(p: PrimType) -> Self {
        KirType::Prim(p)
    }

    /// Convenience for `KirType::Array(_)`.
    pub fn array(elem: PrimType) -> Self {
        KirType::Array(elem)
    }

    /// Returns the inner `PrimType` if this is a scalar; `None`
    /// otherwise. Use to gate ops that only make sense on scalars
    /// (binary arithmetic, casts, etc.).
    pub fn as_prim(&self) -> Option<PrimType> {
        match self {
            KirType::Prim(p) => Some(*p),
            _ => None,
        }
    }

    /// Returns the element `PrimType` if this is an array; `None` for
    /// scalars / tuples / structs.
    pub fn as_array_elem(&self) -> Option<PrimType> {
        match self {
            KirType::Array(p) => Some(*p),
            _ => None,
        }
    }

    /// Returns the per-slot types if this is a tuple; `None` otherwise.
    pub fn as_tuple_elems(&self) -> Option<&[PrimType]> {
        match self {
            KirType::Tuple(es) => Some(es),
            _ => None,
        }
    }

    /// Returns the (sorted) field list if this is a struct; `None`
    /// otherwise.
    pub fn as_struct_fields(&self) -> Option<&[(ArcStr, PrimType)]> {
        match self {
            KirType::Struct(fs) => Some(fs),
            _ => None,
        }
    }

    /// Returns the case list if this is a variant; `None` otherwise.
    pub fn as_variant_cases(&self) -> Option<&[(ArcStr, Vec<PrimType>)]> {
        match self {
            KirType::Variant(cs) => Some(cs),
            _ => None,
        }
    }

    /// Rust source for this type — used by the AOT emitter when
    /// rendering parameter and return types. Tuple and struct values
    /// share the array runtime layout (all `ValArray` underneath), so
    /// they all render the same way at the kernel boundary.
    pub fn rust_name(&self) -> std::borrow::Cow<'static, str> {
        match self {
            KirType::Prim(p) => p.rust_name().into(),
            // Array, tuple, struct: always-`Value::Array(ValArray)`
            // at runtime — kernel boundary takes the `ValArray`
            // directly, skipping the outer Value wrapper.
            KirType::Array(_)
            | KirType::Tuple(_)
            | KirType::Struct(_) => "::netidx_value::ValArray".into(),
            // Variant: either `Value::String(tag)` (nullary) or
            // `Value::Array([tag, ...])` (with-payload). Kernel
            // boundary takes the full `Value` so VariantTagEq /
            // VariantPayload can dispatch on the discriminant.
            KirType::Variant(_) => "::netidx_value::Value".into(),
        }
    }

    /// Try to derive a [`KirType`] from a fully-resolved Graphix
    /// [`Type`]. Recognises:
    /// - `Type::Primitive` (one variant) → `Prim(_)`
    /// - `Type::Array(elem)` → `Array(_)`
    /// - `Type::Tuple(elems)` → `Tuple(_)` (each elem must be primitive)
    /// - `Type::Struct(fields)` → `Struct(_)` (each field must be primitive)
    /// Returns `None` for any other shape — callers fall back to
    /// non-fused execution.
    pub fn from_type(t: &Type) -> Option<KirType> {
        // Helper: pull out a single `Type::Variant(tag, payloads)`
        // case where every payload is a primitive. Nullary cases
        // (empty payloads) are accepted — they flow at runtime as
        // `Value::String(tag)`, distinct from `Value::Array([tag,
        // ...payloads])` for the with-payload case; the kernel
        // boundary takes `&Value` and dispatches at access sites.
        fn variant_case(
            t: &Type,
        ) -> Option<(ArcStr, Vec<PrimType>)> {
            t.with_deref(|resolved| match resolved? {
                Type::Variant(tag, ts) => {
                    let payloads: Option<Vec<PrimType>> = ts
                        .iter()
                        .map(|p| PrimType::from_type(p))
                        .collect();
                    payloads.map(|p| (tag.clone(), p))
                }
                _ => None,
            })
        }
        t.with_deref(|resolved| match resolved? {
            Type::Array(inner) => PrimType::from_type(inner).map(KirType::Array),
            Type::Tuple(elems) => {
                let elems: Option<Vec<PrimType>> = elems
                    .iter()
                    .map(|e| PrimType::from_type(e))
                    .collect();
                elems.map(KirType::Tuple)
            }
            Type::Variant(_, _) => {
                // Single-case variant (uncommon but legal).
                variant_case(resolved?).map(|c| KirType::Variant(vec![c]))
            }
            Type::Set(members) => {
                // Variant unions: `[ \`Foo(...), \`Bar(...) ]` parses
                // as a Set of single-Variant types. Match this
                // shape; reject Sets with non-variant members
                // (those are general unions — different fusion
                // story).
                let cases: Option<Vec<(ArcStr, Vec<PrimType>)>> = members
                    .iter()
                    .map(|m| variant_case(m))
                    .collect();
                cases.map(KirType::Variant)
            }
            Type::Struct(fields) => {
                let fs: Option<Vec<(ArcStr, PrimType)>> = fields
                    .iter()
                    .map(|(n, t)| PrimType::from_type(t).map(|p| (n.clone(), p)))
                    .collect();
                fs.map(KirType::Struct)
            }
            other => PrimType::from_type(other).map(KirType::Prim),
        })
    }
}

impl From<PrimType> for KirType {
    fn from(p: PrimType) -> Self {
        KirType::Prim(p)
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

/// A flat array of primitives passed as a kernel parameter.
///
/// Sibling to [`Input`] — kept separate so the existing scalar-only
/// pipeline (constructors, emitters, etc.) keeps compiling unchanged
/// while array support is added incrementally. `elem` is the element
/// `PrimType`; the runtime value is a `&ValArray` whose every slot
/// carries that variant. Inside the kernel, elements are loaded via
/// `unsafe { arr.get_unchecked::<T>(i) }` (see
/// `netidx_value::ValArray::get_unchecked`) — no allocation, no
/// per-element variant match.
#[derive(Debug, Clone)]
pub struct ArrayInput {
    pub name: ArcStr,
    pub elem: PrimType,
    pub bind_id: Option<BindId>,
    pub rust_name: String,
}

/// A tuple value passed as a kernel parameter.
///
/// Same `&ValArray` boundary as [`ArrayInput`] (graphix tuples are
/// stored as `Value::Array(ValArray)` at the runtime layer); the
/// difference is per-slot types. `elems[i]` is the `PrimType` of the
/// `i`th tuple position.
#[derive(Debug, Clone)]
pub struct TupleInput {
    pub name: ArcStr,
    pub elems: Vec<PrimType>,
    pub bind_id: Option<BindId>,
    pub rust_name: String,
}

/// A struct value passed as a kernel parameter.
///
/// Same `&ValArray` boundary, with fields stored at compile-time
/// known sorted-by-name positions (matching graphix's canonical
/// struct layout).
#[derive(Debug, Clone)]
pub struct StructInput {
    pub name: ArcStr,
    pub fields: Vec<(ArcStr, PrimType)>,
    pub bind_id: Option<BindId>,
    pub rust_name: String,
}

/// A variant value passed as a kernel parameter. Same `&ValArray`
/// boundary as tuples/structs, but the slot at index 0 is the tag
/// string (an interned `ArcStr`) and payloads start at index 1.
/// `cases` enumerates the legal `(tag, payload_types)` shapes — at
/// runtime exactly one of these is active per value.
#[derive(Debug, Clone)]
pub struct VariantInput {
    pub name: ArcStr,
    pub cases: Vec<(ArcStr, Vec<PrimType>)>,
    pub bind_id: Option<BindId>,
    pub rust_name: String,
}

/// Per-source-arg metadata used by the tail-call renderer to assign
/// each new value to the right destination. Populated by
/// `build_kir_kernel` in lambda argspec order — so
/// `tail_call_slots[i]` describes the destination of the `i`th
/// tail-call arg.
#[derive(Debug, Clone)]
pub struct TailCallSlot {
    pub name: ArcStr,
    pub rust_name: String,
    pub kind: TailCallSlotKind,
}

#[derive(Debug, Clone, Copy)]
pub enum TailCallSlotKind {
    Scalar(PrimType),
    /// Array / Tuple / Struct — all share the `ValArray` runtime
    /// representation. Body signature receives `&ValArray`; tail-call
    /// rebinding clones an owned `ValArray` into the shadowed local.
    ValArray,
    /// Variant — runtime representation is `Value` (`String` for
    /// nullary, `Array` for with-payload). Body signature receives
    /// `&Value`; tail-call rebinding clones an owned `Value`.
    Variant,
}

/// Signature of a function the emitter has already seen fuse
/// successfully. Used when one fused kernel calls another — the call
/// site lowers to a direct call against the target's `body_fn_name`.
#[derive(Debug, Clone)]
pub struct KnownFusedFn {
    /// Rust name of the free fn (e.g. "fused_iterate_body"). Must be
    /// visible from the emitted kernel (same crate).
    pub body_fn_name: String,
    /// Positional argument types in declaration order. Either scalar
    /// primitives or flat arrays of primitives.
    pub arg_types: Vec<KirType>,
    /// Return type — scalar or array.
    pub return_type: KirType,
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
    pub fn typ(&self) -> KirType {
        self.expr.typ.clone()
    }
}

// ─── KIR core ────────────────────────────────────────────────────

/// A typed expression node. `typ` is the Graphix type the expression
/// evaluates to (scalar primitive or flat array of primitives); `op`
/// is the actual operation.
#[derive(Debug, Clone)]
pub struct KirExpr {
    pub op: KirOp,
    pub typ: KirType,
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
    /// Late-bound call into a function value the kernel doesn't know
    /// statically. Resolved at runtime by the interpreter, which
    /// reads the `fn_index`-th slot of the kernel's fn-args table
    /// (populated by [`crate::kir_interp::KirNode`] from its incoming
    /// args), invokes the resulting `LambdaDef`'s `Apply`, and
    /// converts the result back to a `RegValue`.
    ///
    /// The JIT path doesn't support `DynCall` (kernels containing it
    /// fall back to the interpreter). Lifts in M4g v3 if the perf
    /// signal warrants it; for now the interpreter is fast enough
    /// for the common HOF-callback patterns.
    ///
    /// Type info is carried on the op rather than re-derived at
    /// runtime so the interpreter doesn't have to look up
    /// `kernel.fn_params[fn_index]` on every call.
    DynCall {
        fn_index: u32,
        args: Vec<KirExpr>,
        /// Argument types parallel to `args` — used to convert each
        /// `RegValue` into a `netidx::Value` before the dispatch.
        arg_types: Vec<PrimType>,
        /// Return type. Equals the wrapping `KirExpr.typ`; carried
        /// here for RegValue conversion symmetry with `arg_types`.
        return_type: PrimType,
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
    /// Length of a flat array parameter, as `u64`. `name` must
    /// resolve in `KirKernel.array_params`. Lowers to `arr.len() as
    /// u64` in the Rust backend; the KIR interpreter reads
    /// `array_inputs[i].len()`.
    ArrayLen {
        name: ArcStr,
    },
    /// Indexed read of a flat array parameter. `name` resolves in
    /// `array_params`; `idx` evaluates to an integer index. Result
    /// type is the wrapping `KirExpr.typ`, which must equal the
    /// referenced array's `elem` type. Lowers to
    /// `unsafe { arr.get_unchecked::<T>(idx as usize) }` in the Rust
    /// backend; the interpreter does the same call.
    ///
    /// No bounds check at runtime — the constructor is unsafe in the
    /// effective sense that it relies on the caller (fusion's lowering
    /// pass) to only emit indices the typechecker has proven in
    /// range. Out-of-range graphix-level array reads bail to
    /// non-fused execution before we ever build a kernel that would
    /// hit this op (see the Qop / `$` handling in fusion).
    ArrayGet {
        name: ArcStr,
        idx: Box<KirExpr>,
    },
    /// Reduce a flat array to a scalar via a same-cycle fold. Lowers
    /// `array::fold(arr, init, |acc, x| body)` when the callback
    /// fuses to scalar KIR. `array` resolves in `array_params`;
    /// `acc_local` and `elem_local` are introduced as locals visible
    /// only inside `body`. The result type equals `body.typ` (which
    /// the constructor has pinned to `init.typ`). Lowering emits a
    /// Rust `for i in 0..arr.len()` loop with `acc` mutated in
    /// place — should auto-vectorize when the body is pure
    /// arithmetic over a primitive accumulator.
    ///
    /// Errors inside the body propagate the same way the surrounding
    /// kernel handles them (currently: bail to non-fused if the body
    /// can't be lowered cleanly). Per-element error semantics match
    /// the runtime's existing `array::fold` behavior — see the
    /// callback-error notes in `design/whole_graph_fusion.md`.
    ArrayFold {
        array: ArcStr,
        /// Element type of `array` — copied here at construction time
        /// so the backend doesn't need to walk the body looking for an
        /// `ArrayGet` against the same name (the body usually reads
        /// the bound `elem_local`, not the array directly).
        elem_typ: PrimType,
        init: Box<KirExpr>,
        acc_local: ArcStr,
        elem_local: ArcStr,
        body: Box<KirExpr>,
    },
    /// Build a flat array of `n` elements by applying a fused body to
    /// each index. Lowers `array::init(n, |idx| body)`. The body sees
    /// `idx_local: i64` as a local; its result type becomes the
    /// array's element type. Result `KirExpr.typ` is
    /// `KirType::Array(elem_typ)`.
    ///
    /// Rust emit:
    /// ```ignore
    /// {
    ///     let __n = n as usize;
    ///     ::netidx_value::ValArray::from_iter_exact(
    ///         (0..__n).map(|__i| {
    ///             let idx: i64 = __i as i64;
    ///             ::netidx_value::Value::F64(body)
    ///         })
    ///     )
    /// }
    /// ```
    /// One pooled allocation per invocation (the result array
    /// itself); no per-element Vec.
    ArrayInit {
        n: Box<KirExpr>,
        idx_local: ArcStr,
        elem_typ: PrimType,
        body: Box<KirExpr>,
    },
    /// Build a flat array by applying a fused body to each element of
    /// an input array. Lowers `array::map(arr, |x| body)`. Result
    /// `KirExpr.typ` is `KirType::Array(result_elem)` where
    /// `result_elem` is `body.typ`'s primitive variant.
    ///
    /// Same one-allocation cost profile as `ArrayInit`; element loads
    /// go through the unsafe `ValArray::get_unchecked` fast path.
    ArrayMap {
        array: ArcStr,
        /// Element type of the *input* `array`.
        in_elem: PrimType,
        elem_local: ArcStr,
        /// Element type of the *output* — equals `body.typ.as_prim()`.
        out_elem: PrimType,
        body: Box<KirExpr>,
    },
    /// Build a flat array by retaining each element of an input array
    /// for which a fused predicate body returns true. Lowers
    /// `array::filter(arr, |x| pred)`. Result `KirExpr.typ` is
    /// `KirType::Array(elem)` — same element type as the input
    /// (filter never changes the element shape).
    ///
    /// Output length is dynamic, so the lowering uses
    /// `iter.collect::<ValArray>()` which goes through the
    /// `FromIterator` impl that internally takes an `LPooled<Vec>`
    /// as scratch and feeds `from_iter_exact`. After warmup the
    /// pool returns the same buffer each call — no per-invocation
    /// allocation in the hot path.
    ArrayFilter {
        array: ArcStr,
        elem: PrimType,
        elem_local: ArcStr,
        /// Predicate body — must emit as `KirType::Prim(Bool)`.
        predicate: Box<KirExpr>,
    },
    /// Read tuple slot `idx` of `name` (a tuple kernel parameter).
    /// Result `KirExpr.typ` is `KirType::Prim(elem_typ)` — the type
    /// of that specific slot. Lowers byte-identically to
    /// `KirOp::ArrayGet` using `ValArray::get_unchecked`.
    TupleGet {
        name: ArcStr,
        idx: usize,
        elem_typ: PrimType,
    },
    /// Build a tuple from per-slot expressions. Each `fields[i]`
    /// emits to its own scalar KIR; the renderer wraps each in
    /// `Value::<variant>(x)` and feeds them through
    /// `ValArray::from_iter_exact`. Result type is
    /// `KirType::Tuple(elem_types)` where `elem_types[i] ==
    /// fields[i].typ.as_prim().unwrap()`.
    TupleNew {
        fields: Vec<KirExpr>,
        elem_types: Vec<PrimType>,
    },
    /// Read struct field `field` from `name` (a struct kernel
    /// parameter), at the sorted index `sorted_idx`. Result type is
    /// `KirType::Prim(elem_typ)`. Same lowering as `TupleGet`.
    StructGet {
        name: ArcStr,
        field: ArcStr,
        sorted_idx: usize,
        elem_typ: PrimType,
    },
    /// Build a struct from per-field expressions. `sorted_fields` is
    /// the canonical alphabetical order (graphix's runtime layout).
    /// Lowers identically to `TupleNew` — the runtime doesn't
    /// distinguish tuples from structs at the ValArray level.
    StructNew {
        sorted_fields: Vec<(ArcStr, KirExpr)>,
        sorted_types: Vec<(ArcStr, PrimType)>,
    },
    /// Test whether a variant param's runtime tag matches a
    /// compile-time-known tag string. Reads `name`'s slot 0 (the
    /// interned tag `ArcStr`), compares against `expected_tag` via
    /// byte equality. Result type is `KirType::Prim(Bool)`. Used
    /// as the condition for select arms matching a variant pattern.
    VariantTagEq {
        name: ArcStr,
        expected_tag: ArcStr,
    },
    /// Read a payload slot from a variant param. `payload_idx` is
    /// the 0-based payload position (slot index = payload_idx + 1
    /// at runtime — the tag occupies slot 0). `elem_typ` is the
    /// payload's primitive type. Only safe inside a select arm
    /// gated by a matching `VariantTagEq`.
    VariantPayload {
        name: ArcStr,
        payload_idx: usize,
        elem_typ: PrimType,
    },
    /// Build a variant value: `` `Tag(p0, p1, ...) ``. Lowers to
    /// `ValArray::from_iter_exact([Value::String("Tag"), Value::T0(p0), ...])`.
    /// Result `KirExpr.typ` is `KirType::Variant([(tag, payload_types)])`
    /// — exactly one case (the one being constructed).
    VariantNew {
        tag: ArcStr,
        payloads: Vec<KirExpr>,
        payload_types: Vec<PrimType>,
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
    /// Primitive parameters in declaration order. Each is also visible
    /// as a local in the body.
    pub params: Vec<Input>,
    /// Function-typed parameters in declaration order. Distinct from
    /// `params` because the interpreter holds them in a separate
    /// fn-args table (the value is a `LambdaDef`, not a primitive).
    /// `KirOp::DynCall { fn_index }` indexes into this table.
    pub fn_params: Vec<FnParam>,
    /// Array-typed parameters. Sibling to `params`; kept separate so
    /// the scalar pipeline doesn't need to know about array types.
    /// `KirOp::ArrayLen` / `KirOp::ArrayGet` reference these by name.
    pub array_params: Vec<ArrayInput>,
    /// Tuple-typed parameters. `KirOp::TupleGet` references these by
    /// name + sorted index.
    pub tuple_params: Vec<TupleInput>,
    /// Struct-typed parameters. `KirOp::StructGet` references these
    /// by name + field-name (resolved to sorted index at lowering).
    pub struct_params: Vec<StructInput>,
    /// Variant-typed parameters. `KirOp::VariantTagEq` / VariantPayload
    /// reference these by name; the runtime layout puts the tag string
    /// at slot 0 and payloads at slots 1..N.
    pub variant_params: Vec<VariantInput>,
    /// Per-source-position metadata so the tail-call renderer can
    /// assign each new arg value to the right destination
    /// regardless of which slot list (scalar / array / tuple /
    /// struct) the destination lives in. Length matches the
    /// lambda's source argspec order. Only populated when
    /// `has_tail_loop` is true; otherwise empty.
    pub tail_call_slots: Vec<TailCallSlot>,
    pub return_type: KirType,
    /// True iff the body contains a self-tail-call. Backends wrap the
    /// body in `loop { ... }` (Rust) or a back-edge to the entry block
    /// (CLIF) accordingly.
    pub has_tail_loop: bool,
    pub body: Vec<KirStmt>,
}

/// A function-typed kernel "parameter" — really a slot in the
/// kernel's fn-args table. Each slot resolves to a `LambdaDef` at
/// DynCall time; how that resolution happens depends on the
/// [`FnSource`] tag.
#[derive(Debug, Clone)]
pub struct FnParam {
    /// Graphix-level name. The fusion emitter looks up
    /// `Apply{Ref(name)}` references against this list.
    pub name: ArcStr,
    /// Where to find the `LambdaDef` for this slot at runtime.
    pub source: FnSource,
    /// Argument prim types of the *callee* function — used to
    /// convert RegValue→Value at DynCall sites.
    pub arg_types: Vec<PrimType>,
    /// Return prim type of the callee.
    pub return_type: PrimType,
}

/// How a [`FnParam`]'s `LambdaDef` is sourced at DynCall time.
#[derive(Debug, Clone)]
pub enum FnSource {
    /// HOF argument: the kernel's caller passes a `LambdaDef` value
    /// at position `arg_pos` (zero-based, in the lambda's source-
    /// order argument list, mixed with primitive args). KirNode's
    /// runtime extracts it from the incoming `from` slice.
    Param { arg_pos: u32 },
    /// Statically-resolved binding: the `LambdaDef` lives in
    /// `ctx.cached[bind_id]` (or, for unstable bindings,
    /// `event.variables[bind_id]`). Set when fusion can't fuse the
    /// callee inline (its body uses unsupported constructs) but can
    /// still call it via Apply::update.
    Binding { bind_id: crate::BindId },
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
    let lp = lhs.typ.as_prim()?;
    let rp = rhs.typ.as_prim()?;
    if lp != rp || !lp.is_numeric() {
        return None;
    }
    Some(KirExpr {
        op: KirOp::Bin { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
        typ: KirType::Prim(lp),
    })
}

pub fn cmp(lhs: KirExpr, rhs: KirExpr, op: CmpOp) -> Option<KirExpr> {
    if lhs.typ != rhs.typ || lhs.typ.as_prim().is_none() {
        return None;
    }
    Some(KirExpr {
        op: KirOp::Cmp { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
        typ: KirType::Prim(PrimType::Bool),
    })
}

pub fn bool_op(lhs: KirExpr, rhs: KirExpr, op: BoolOp) -> Option<KirExpr> {
    let bool_t = KirType::Prim(PrimType::Bool);
    if lhs.typ != bool_t || rhs.typ != bool_t {
        return None;
    }
    Some(KirExpr {
        op: KirOp::BoolBin { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
        typ: bool_t,
    })
}

pub fn not(inner: KirExpr) -> Option<KirExpr> {
    let bool_t = KirType::Prim(PrimType::Bool);
    if inner.typ != bool_t {
        return None;
    }
    Some(KirExpr { op: KirOp::Not(Box::new(inner)), typ: bool_t })
}

pub fn cast(inner: KirExpr, target: PrimType) -> Option<KirExpr> {
    // Rust's `as` doesn't accept bool↔integer in either direction;
    // those need a `match`. Until the emitter grows that, refuse.
    let inner_p = inner.typ.as_prim()?;
    if target == PrimType::Bool || inner_p == PrimType::Bool {
        return None;
    }
    Some(KirExpr {
        op: KirOp::Cast { inner: Box::new(inner), target },
        typ: KirType::Prim(target),
    })
}

pub fn const_expr(c: ConstVal) -> KirExpr {
    let typ = KirType::Prim(c.typ());
    KirExpr { op: KirOp::Const(c), typ }
}

pub fn local(name: ArcStr, prim: PrimType) -> KirExpr {
    KirExpr { op: KirOp::Local(name), typ: KirType::Prim(prim) }
}

/// Construct a [`KirExpr`] referring to an array-typed local. The
/// element type goes on `KirType::Array(elem)` so downstream ops
/// (ArrayMap, ArrayFold) can read it back without a separate sidecar.
pub fn local_array(name: ArcStr, elem: PrimType) -> KirExpr {
    KirExpr { op: KirOp::Local(name), typ: KirType::Array(elem) }
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
        KirOp::Call { .. } | KirOp::DynCall { .. } => true,
        KirOp::Const(_) | KirOp::Local(_) | KirOp::ArrayLen { .. } => false,
        KirOp::Bin { lhs, rhs, .. }
        | KirOp::Cmp { lhs, rhs, .. }
        | KirOp::BoolBin { lhs, rhs, .. } => {
            expr_has_call(lhs) || expr_has_call(rhs)
        }
        KirOp::Not(inner) | KirOp::Cast { inner, .. } => expr_has_call(inner),
        KirOp::ArrayGet { idx, .. } => expr_has_call(idx),
        KirOp::ArrayFold { init, body, .. } => {
            expr_has_call(init) || expr_has_call(body)
        }
        KirOp::ArrayInit { n, body, .. } => expr_has_call(n) || expr_has_call(body),
        KirOp::ArrayMap { body, .. } => expr_has_call(body),
        KirOp::ArrayFilter { predicate, .. } => expr_has_call(predicate),
        KirOp::TupleGet { .. } | KirOp::StructGet { .. } => false,
        KirOp::TupleNew { fields, .. } => fields.iter().any(expr_has_call),
        KirOp::StructNew { sorted_fields, .. } => {
            sorted_fields.iter().any(|(_, e)| expr_has_call(e))
        }
        KirOp::VariantTagEq { .. } | KirOp::VariantPayload { .. } => false,
        KirOp::VariantNew { payloads, .. } => payloads.iter().any(expr_has_call),
        KirOp::Block { lets, tail } => {
            lets.iter().any(|l| expr_has_call(&l.value)) || expr_has_call(tail)
        }
        KirOp::IfChain { arms } => arms
            .iter()
            .any(|(c, v)| c.as_ref().is_some_and(expr_has_call) || expr_has_call(v)),
    }
}

/// True if the kernel's body contains any [`KirOp::DynCall`] —
/// distinguishes "needs to dispatch through `Apply::update` at
/// runtime" from "needs static cross-kernel calls" (which the JIT
/// already handles). Kernels containing DynCall fall back to the
/// interpreter; the JIT lowering returns Err.
pub fn kernel_contains_dyncall(kernel: &KirKernel) -> bool {
    kernel.body.iter().any(stmt_has_dyncall)
}

fn stmt_has_dyncall(s: &KirStmt) -> bool {
    match s {
        KirStmt::Let(l) => expr_has_dyncall(&l.value),
        KirStmt::Return(e) => expr_has_dyncall(e),
        KirStmt::TailCall { args } => args.iter().any(expr_has_dyncall),
        KirStmt::Select { arms } => arms.iter().any(|a| {
            a.cond.as_ref().is_some_and(expr_has_dyncall)
                || a.body.iter().any(stmt_has_dyncall)
        }),
    }
}

fn expr_has_dyncall(e: &KirExpr) -> bool {
    match &e.op {
        KirOp::DynCall { .. } => true,
        KirOp::Call { args, .. } => args.iter().any(expr_has_dyncall),
        KirOp::Const(_) | KirOp::Local(_) | KirOp::ArrayLen { .. } => false,
        KirOp::Bin { lhs, rhs, .. }
        | KirOp::Cmp { lhs, rhs, .. }
        | KirOp::BoolBin { lhs, rhs, .. } => {
            expr_has_dyncall(lhs) || expr_has_dyncall(rhs)
        }
        KirOp::Not(inner) | KirOp::Cast { inner, .. } => expr_has_dyncall(inner),
        KirOp::ArrayGet { idx, .. } => expr_has_dyncall(idx),
        KirOp::ArrayFold { init, body, .. } => {
            expr_has_dyncall(init) || expr_has_dyncall(body)
        }
        KirOp::ArrayInit { n, body, .. } => {
            expr_has_dyncall(n) || expr_has_dyncall(body)
        }
        KirOp::ArrayMap { body, .. } => expr_has_dyncall(body),
        KirOp::ArrayFilter { predicate, .. } => expr_has_dyncall(predicate),
        KirOp::TupleGet { .. } | KirOp::StructGet { .. } => false,
        KirOp::TupleNew { fields, .. } => fields.iter().any(expr_has_dyncall),
        KirOp::StructNew { sorted_fields, .. } => {
            sorted_fields.iter().any(|(_, e)| expr_has_dyncall(e))
        }
        KirOp::VariantTagEq { .. } | KirOp::VariantPayload { .. } => false,
        KirOp::VariantNew { payloads, .. } => {
            payloads.iter().any(expr_has_dyncall)
        }
        KirOp::Block { lets, tail } => {
            lets.iter().any(|l| expr_has_dyncall(&l.value))
                || expr_has_dyncall(tail)
        }
        KirOp::IfChain { arms } => arms.iter().any(|(c, v)| {
            c.as_ref().is_some_and(expr_has_dyncall) || expr_has_dyncall(v)
        }),
    }
}

/// Walk a kernel body collecting the names of every `KirOp::Call` it
/// contains. Used by the JIT path (to declare callee `FuncRef`s
/// before lowering) and by the lazy-fusion path (to discover
/// transitive callees of an already-built kernel).
pub fn collect_call_sites(kernel: &KirKernel) -> std::collections::BTreeSet<ArcStr> {
    let mut out = std::collections::BTreeSet::new();
    for s in &kernel.body {
        walk_call_sites_stmt(s, &mut out);
    }
    out
}

fn walk_call_sites_stmt(s: &KirStmt, out: &mut std::collections::BTreeSet<ArcStr>) {
    match s {
        KirStmt::Let(l) => walk_call_sites_expr(&l.value, out),
        KirStmt::Return(e) => walk_call_sites_expr(e, out),
        KirStmt::TailCall { args } => {
            for a in args {
                walk_call_sites_expr(a, out);
            }
        }
        KirStmt::Select { arms } => {
            for a in arms {
                if let Some(c) = &a.cond {
                    walk_call_sites_expr(c, out);
                }
                for s in &a.body {
                    walk_call_sites_stmt(s, out);
                }
            }
        }
    }
}

fn walk_call_sites_expr(e: &KirExpr, out: &mut std::collections::BTreeSet<ArcStr>) {
    match &e.op {
        KirOp::Call { fn_name, args } => {
            out.insert(fn_name.clone());
            for a in args {
                walk_call_sites_expr(a, out);
            }
        }
        KirOp::DynCall { args, .. } => {
            // No static name to record — DynCall resolves through the
            // kernel's fn-args table at runtime, not the static-call
            // FuncRef table. Just recurse into the arg expressions.
            for a in args {
                walk_call_sites_expr(a, out);
            }
        }
        KirOp::Const(_) | KirOp::Local(_) | KirOp::ArrayLen { .. } => {}
        KirOp::Bin { lhs, rhs, .. }
        | KirOp::Cmp { lhs, rhs, .. }
        | KirOp::BoolBin { lhs, rhs, .. } => {
            walk_call_sites_expr(lhs, out);
            walk_call_sites_expr(rhs, out);
        }
        KirOp::Not(inner) | KirOp::Cast { inner, .. } => walk_call_sites_expr(inner, out),
        KirOp::ArrayGet { idx, .. } => walk_call_sites_expr(idx, out),
        KirOp::ArrayFold { init, body, .. } => {
            walk_call_sites_expr(init, out);
            walk_call_sites_expr(body, out);
        }
        KirOp::ArrayInit { n, body, .. } => {
            walk_call_sites_expr(n, out);
            walk_call_sites_expr(body, out);
        }
        KirOp::ArrayMap { body, .. } => walk_call_sites_expr(body, out),
        KirOp::ArrayFilter { predicate, .. } => walk_call_sites_expr(predicate, out),
        KirOp::TupleGet { .. } | KirOp::StructGet { .. } => {}
        KirOp::TupleNew { fields, .. } => {
            for f in fields {
                walk_call_sites_expr(f, out);
            }
        }
        KirOp::StructNew { sorted_fields, .. } => {
            for (_, f) in sorted_fields {
                walk_call_sites_expr(f, out);
            }
        }
        KirOp::VariantTagEq { .. } | KirOp::VariantPayload { .. } => {}
        KirOp::VariantNew { payloads, .. } => {
            for p in payloads {
                walk_call_sites_expr(p, out);
            }
        }
        KirOp::Block { lets, tail } => {
            for l in lets {
                walk_call_sites_expr(&l.value, out);
            }
            walk_call_sites_expr(tail, out);
        }
        KirOp::IfChain { arms } => {
            for (c, v) in arms {
                if let Some(c) = c {
                    walk_call_sites_expr(c, out);
                }
                walk_call_sites_expr(v, out);
            }
        }
    }
}

// (Helper `find_array_elem_type_in_body` removed — the element type
// is now baked into `KirOp::ArrayFold` directly via `elem_typ`, so
// neither backend needs to walk the body to recover it.)

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
        KirOp::ArrayLen { name } => {
            // The kernel signature passes array params as `&ValArray`,
            // which derefs to `&[Value]`. `.len() as u64` matches the
            // KIR-declared `KirExpr.typ = U64`.
            write!(out, "({}.len() as u64)", name).ok();
        }
        KirOp::ArrayGet { name, idx } => {
            // Unsafe element load — relies on `ValArray::get_unchecked<T>`
            // (added in netidx-value). The `T` comes from `e.typ`,
            // which the constructor pinned to the array's element
            // type. `idx` is the user-typed index expression; cast to
            // `usize` because `get_unchecked` takes `usize`.
            write!(
                out,
                "(unsafe {{ {}.get_unchecked::<{}>((",
                name,
                e.typ.rust_name()
            )
            .ok();
            write_rust_expr(out, idx);
            out.push_str(") as usize) })");
        }
        KirOp::ArrayInit { n, idx_local, elem_typ, body } => {
            // Render as a self-contained expression that yields a
            // `ValArray`. Uses `ValArray::from_iter_exact` so the
            // result array is one pooled allocation; per-element work
            // happens inside `.map`'s closure with no intermediate
            // Vec.
            //
            //   {
            //       let __n = (n) as usize;
            //       ::netidx_value::ValArray::from_iter_exact(
            //           (0..__n).map(|__i| {
            //               let idx: i64 = __i as i64;
            //               ::netidx_value::Value::F64(body)
            //           })
            //       )
            //   }
            let elem_v = elem_typ.value_variant();
            out.push_str("{ let __n: usize = (");
            write_rust_expr(out, n);
            out.push_str(") as usize; ::netidx_value::ValArray::from_iter_exact(");
            write!(
                out,
                "(0..__n).map(|__i| {{ let {idx}: i64 = __i as i64; \
                 ::netidx_value::Value::{elem_v}(",
                idx = idx_local,
            )
            .ok();
            write_rust_expr(out, body);
            out.push_str(") })) }");
        }
        KirOp::ArrayMap { array, in_elem, elem_local, out_elem, body } => {
            // Same shape as ArrayInit but iterates over an existing
            // `ValArray` input rather than a 0..n range. Element
            // loads via the unsafe `get_unchecked` fast path; output
            // built via `from_iter_exact` to share the pooled
            // ValArray allocator.
            let in_t = in_elem.rust_name();
            let out_v = out_elem.value_variant();
            write!(
                out,
                "{{ let __n: usize = {arr}.len(); \
                 ::netidx_value::ValArray::from_iter_exact( \
                 (0..__n).map(|__i| {{ \
                 let {elem}: {in_t} = unsafe {{ {arr}.get_unchecked::<{in_t}>(__i) }}; \
                 ::netidx_value::Value::{out_v}(",
                arr = array,
                elem = elem_local,
                in_t = in_t,
                out_v = out_v,
            )
            .ok();
            write_rust_expr(out, body);
            out.push_str(") })) }");
        }
        KirOp::TupleGet { name, idx, elem_typ } => {
            // Tuple slot read — same as ArrayGet but the index is a
            // compile-time literal and the element type is encoded
            // on the op (no need to walk siblings).
            write!(
                out,
                "(unsafe {{ {}.get_unchecked::<{}>({}usize) }})",
                name,
                elem_typ.rust_name(),
                idx,
            )
            .ok();
        }
        KirOp::TupleNew { fields, elem_types } => {
            // Build a fixed-size ValArray from the literal fields.
            // Emit as `from_iter_exact([Value::T0(f0), Value::T1(f1), ...].into_iter())`
            // — one pooled allocation, no Vec.
            out.push_str(
                "::netidx_value::ValArray::from_iter_exact([",
            );
            for (i, (f, t)) in fields.iter().zip(elem_types.iter()).enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write!(out, "::netidx_value::Value::{}(", t.value_variant()).ok();
                write_rust_expr(out, f);
                out.push(')');
            }
            out.push_str("].into_iter())");
        }
        KirOp::StructGet { name, sorted_idx, elem_typ, .. } => {
            // Struct field read by sorted index — same byte-shape as
            // TupleGet. The compiler resolves the source-side field
            // name to its sorted-index at lowering time.
            write!(
                out,
                "(unsafe {{ {}.get_unchecked::<{}>({}usize) }})",
                name,
                elem_typ.rust_name(),
                sorted_idx,
            )
            .ok();
        }
        KirOp::StructNew { sorted_fields, sorted_types } => {
            // Identical to TupleNew at the runtime layer — graphix's
            // canonical struct layout is a sorted ValArray.
            out.push_str(
                "::netidx_value::ValArray::from_iter_exact([",
            );
            for (i, ((_, f), (_, t))) in
                sorted_fields.iter().zip(sorted_types.iter()).enumerate()
            {
                if i > 0 {
                    out.push_str(", ");
                }
                write!(out, "::netidx_value::Value::{}(", t.value_variant()).ok();
                write_rust_expr(out, f);
                out.push(')');
            }
            out.push_str("].into_iter())");
        }
        KirOp::VariantTagEq { name, expected_tag } => {
            // The variant param is `&Value`. Match the discriminant:
            //   - nullary case: `Value::String(s)` — tag is `s`
            //   - with-payload: `Value::Array(arr)` — tag is at slot 0
            // The `_ =>` unreachable branch lets the optimizer drop
            // the comparison path for the unmatched discriminant.
            write!(
                out,
                "(match {} {{ \
                    ::netidx_value::Value::String(__t) => \
                        __t.as_str() == \"{}\", \
                    ::netidx_value::Value::Array(__a) => unsafe {{ \
                        __a.get_ref_unchecked::<::arcstr::ArcStr>(0usize) \
                            .as_str() == \"{}\" \
                    }}, \
                    _ => unsafe {{ ::std::hint::unreachable_unchecked() }} \
                }})",
                name,
                expected_tag.as_str().escape_default(),
                expected_tag.as_str().escape_default(),
            )
            .ok();
        }
        KirOp::VariantPayload { name, payload_idx, elem_typ } => {
            // Only valid inside an arm matched by a non-nullary
            // case — so the discriminant is `Value::Array(arr)` and
            // the payload sits at slot `payload_idx + 1` (slot 0
            // holds the tag string).
            write!(
                out,
                "(match {name} {{ \
                    ::netidx_value::Value::Array(__a) => unsafe {{ \
                        __a.get_unchecked::<{rust}>({slot}usize) \
                    }}, \
                    _ => unsafe {{ ::std::hint::unreachable_unchecked() }} \
                }})",
                name = name,
                rust = elem_typ.rust_name(),
                slot = payload_idx + 1,
            )
            .ok();
        }
        KirOp::VariantNew { tag, payloads, payload_types } => {
            // Nullary case (payloads empty) → `Value::String(literal!("tag"))`.
            // With-payload → `Value::Array(ValArray::from_iter_exact([
            //     Value::String(literal!("tag")), Value::T0(p0), ...
            // ]))`. Tag is interned via `literal!` so downstream
            // VariantTagEq can take the ArcStr ptr fast-path when
            // both sides are interned.
            if payloads.is_empty() {
                write!(
                    out,
                    "::netidx_value::Value::String(::arcstr::literal!(\"{}\"))",
                    tag.as_str().escape_default(),
                )
                .ok();
            } else {
                out.push_str(
                    "::netidx_value::Value::Array(\
                     ::netidx_value::ValArray::from_iter_exact([\
                     ::netidx_value::Value::String(::arcstr::literal!(\"",
                );
                write!(out, "{}", tag.as_str().escape_default()).ok();
                out.push_str("\"))");
                for (p, t) in payloads.iter().zip(payload_types.iter()) {
                    out.push_str(", ");
                    write!(out, "::netidx_value::Value::{}(", t.value_variant()).ok();
                    write_rust_expr(out, p);
                    out.push(')');
                }
                out.push_str("].into_iter()))");
            }
        }
        KirOp::ArrayFilter { array, elem, elem_local, predicate } => {
            // Output length is dynamic, so we go through the
            // `FromIterator<Value>` impl on `ValArray` (which
            // internally stages into an `LPooled<Vec>` then feeds
            // `from_iter_exact`). After thread warmup the pool
            // returns the same scratch buffer each call — net zero
            // alloc on the hot path.
            let in_t = elem.rust_name();
            let out_v = elem.value_variant();
            write!(
                out,
                "{{ let __n: usize = {arr}.len(); \
                 (0..__n).filter_map(|__i| {{ \
                 let {elem_l}: {in_t} = unsafe {{ {arr}.get_unchecked::<{in_t}>(__i) }}; \
                 if (",
                arr = array,
                elem_l = elem_local,
                in_t = in_t,
            )
            .ok();
            write_rust_expr(out, predicate);
            write!(
                out,
                ") {{ ::std::option::Option::Some(::netidx_value::Value::{out_v}({elem_l})) }} \
                 else {{ ::std::option::Option::None }} \
                 }}).collect::<::netidx_value::ValArray>() }}",
                out_v = out_v,
                elem_l = elem_local,
            )
            .ok();
        }
        KirOp::ArrayFold { array, elem_typ, init, acc_local, elem_local, body } => {
            // Renders as a self-contained expression block:
            //   { let mut acc: T = init;
            //     let n = arr.len();
            //     let mut i = 0usize;
            //     while i < n {
            //         let elem: E = unsafe { arr.get_unchecked::<E>(i) };
            //         acc = body;
            //         i += 1;
            //     }
            //     acc }
            // We use `while` (not `for ... in 0..n`) because `for`
            // borrows the range and rustc's vectorizer is happier
            // with the explicit-counter form. `acc` and `elem` are
            // the user-given local names; rustc enforces uniqueness
            // because they're shadowed locals in this block.
            let acc_t = e.typ.rust_name();
            let elem_t = elem_typ.rust_name();
            write!(
                out,
                "{{ let mut {acc}: {acc_t} = ",
                acc = acc_local
            )
            .ok();
            write_rust_expr(out, init);
            write!(
                out,
                "; let __n: usize = {arr}.len(); let mut __i: usize = 0; \
                 while __i < __n {{ let {elem}: {elem_t} = unsafe {{ \
                 {arr}.get_unchecked::<{elem_t}>(__i) }}; {acc} = ",
                arr = array,
                elem = elem_local,
                acc = acc_local,
                elem_t = elem_t
            )
            .ok();
            write_rust_expr(out, body);
            write!(out, "; __i += 1; }} {acc} }}", acc = acc_local).ok();
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
        KirOp::DynCall { .. } => {
            // The Rust-source AOT backend doesn't support DynCall yet;
            // fusion's classifier is supposed to refuse to lower a
            // late-bound call into KIR for the AOT path. If we get
            // here, that classifier was bypassed — emit a clearly
            // wrong placeholder so the Rust compile fails loudly.
            out.push_str("compile_error!(\"DynCall in AOT backend\")");
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
    // Emit parameters in source-declared order so the apply shim
    // (which extracts args in source order) can call the body
    // positionally. `tail_call_slots` is populated for every kernel
    // and carries the source-order mapping. Scalar slots take their
    // primitive type by value; ValArray slots (Array / Tuple /
    // Struct) take `&ValArray`; Variant slots take `&Value`.
    for (i, slot) in kernel.tail_call_slots.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        match slot.kind {
            TailCallSlotKind::Scalar(p) => write!(
                out,
                "mut {}: {}",
                slot.rust_name,
                p.rust_name(),
            )
            .ok(),
            TailCallSlotKind::ValArray => write!(
                out,
                "{}: &::netidx_value::ValArray",
                slot.rust_name,
            )
            .ok(),
            TailCallSlotKind::Variant => write!(
                out,
                "{}: &::netidx_value::Value",
                slot.rust_name,
            )
            .ok(),
        };
    }
    let return_t = match &kernel.return_type {
        KirType::Prim(p) => p.rust_name().to_string(),
        // Array, tuple, struct: body produces an owned ValArray via
        // from_iter_exact / collect.
        KirType::Array(_)
        | KirType::Tuple(_)
        | KirType::Struct(_) => "::netidx_value::ValArray".to_string(),
        // Variant: body produces a `Value` (String for nullary,
        // Array for with-payload).
        KirType::Variant(_) => "::netidx_value::Value".to_string(),
    };
    writeln!(out, ") -> {} {{", return_t).ok();
    let body_indent = if kernel.has_tail_loop { 2 } else { 1 };
    // When the body tail-calls itself, composite params (which enter
    // as `&ValArray`) need a mutable owned local to be reassignable.
    // Insert a `let mut <name>: ValArray = <name>.clone();` shadow
    // for each. The clone is a refcount bump (ValArray is Arc-
    // backed) — cheap, and necessary so the loop can reassign the
    // local to a freshly-built ValArray each iteration.
    //
    // Scalar params stay `mut <T>` in the signature, which works
    // directly for tail-call reassignment.
    if kernel.has_tail_loop {
        for p in &kernel.array_params {
            writeln!(
                out,
                "    let mut {}: ::netidx_value::ValArray = {}.clone();",
                p.rust_name, p.rust_name,
            )
            .ok();
        }
        for p in &kernel.tuple_params {
            writeln!(
                out,
                "    let mut {}: ::netidx_value::ValArray = {}.clone();",
                p.rust_name, p.rust_name,
            )
            .ok();
        }
        for p in &kernel.struct_params {
            writeln!(
                out,
                "    let mut {}: ::netidx_value::ValArray = {}.clone();",
                p.rust_name, p.rust_name,
            )
            .ok();
        }
        for p in &kernel.variant_params {
            // Owned `Value` shadow — `Value::clone()` is a refcount
            // bump for the heap-backed variants (Array, String,
            // Bytes, …) and a Copy for the scalar variants.
            writeln!(
                out,
                "    let mut {}: ::netidx_value::Value = {}.clone();",
                p.rust_name, p.rust_name,
            )
            .ok();
        }
        writeln!(out, "    loop {{").ok();
    }
    write_rust_kernel_body(
        &mut out,
        &kernel.body,
        &kernel.params,
        &kernel.tail_call_slots,
        body_indent,
    );
    if kernel.has_tail_loop {
        writeln!(out, "    }}").ok();
    }
    writeln!(out, "}}").ok();
    out
}

/// Render a kernel body, threading the scalar params (for legacy
/// emitter paths that still want a plain `[Input]`) and the full
/// per-source-position tail-call slot list (so `KirStmt::TailCall`
/// can resolve each arg to its destination slot regardless of
/// category).
fn write_rust_kernel_body(
    out: &mut String,
    stmts: &[KirStmt],
    params: &[Input],
    tail_slots: &[TailCallSlot],
    indent: usize,
) {
    for stmt in stmts {
        write_rust_kernel_stmt(out, stmt, params, tail_slots, indent);
    }
}

fn write_rust_kernel_stmt(
    out: &mut String,
    stmt: &KirStmt,
    params: &[Input],
    tail_slots: &[TailCallSlot],
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
            write_rust_tail_call_with_slots(out, args, tail_slots, indent);
        }
        KirStmt::Select { arms } => {
            write_rust_kernel_select(out, arms, params, tail_slots, indent);
        }
    }
}

fn write_rust_kernel_select(
    out: &mut String,
    arms: &[SelectArm],
    params: &[Input],
    tail_slots: &[TailCallSlot],
    indent: usize,
) {
    let n = arms.len();
    let mut last_was_unconditional = false;
    for (i, arm) in arms.iter().enumerate() {
        let is_last = i == n - 1;
        match (&arm.cond, is_last) {
            (None, true) => {
                write_rust_kernel_body(
                    out, &arm.body, params, tail_slots, indent,
                );
                last_was_unconditional = true;
            }
            (None, false) => {
                indent_into(out, indent);
                out.push_str("{\n");
                write_rust_kernel_body(
                    out, &arm.body, params, tail_slots, indent + 1,
                );
                indent_into(out, indent);
                out.push_str("}\n");
            }
            (Some(c), _) => {
                indent_into(out, indent);
                out.push_str("if ");
                write_rust_expr(out, c);
                out.push_str(" {\n");
                write_rust_kernel_body(
                    out, &arm.body, params, tail_slots, indent + 1,
                );
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

/// Emit a tail call using the per-source-position slot list, so each
/// new arg goes to its right destination regardless of category.
/// Scalars get a `__tmp_<name>: <T>` temp; composites get an owned
/// `ValArray` temp (the source expression already produces an owned
/// value when the body is a `TupleNew` / `ArrayInit` /
/// `ValArray::from_iter_exact` call; `Ref(name)` to a composite
/// renders as a bare identifier that we explicitly `.clone()` to
/// avoid moving the shadowed mutable local).
fn write_rust_tail_call_with_slots(
    out: &mut String,
    args: &[KirExpr],
    slots: &[TailCallSlot],
    indent: usize,
) {
    debug_assert_eq!(args.len(), slots.len(), "tail-call arity mismatch");
    for (a, slot) in args.iter().zip(slots.iter()) {
        indent_into(out, indent);
        match slot.kind {
            TailCallSlotKind::Scalar(p) => {
                write!(
                    out,
                    "let __tmp_{}: {} = ",
                    slot.rust_name,
                    p.rust_name(),
                )
                .ok();
                write_rust_expr(out, a);
                out.push_str(";\n");
            }
            TailCallSlotKind::ValArray => {
                // The arg expression's runtime type is owned
                // `ValArray` regardless of how the body produced it
                // (TupleNew / StructNew / ArrayInit / ArrayMap /
                // ArrayFilter / a bare composite Local). For a bare
                // Local that shadows the loop's owned slot, we'd
                // otherwise move out of the slot mid-tail-call; the
                // explicit `.clone()` (refcount bump) keeps the
                // slot's value alive until the assignment below.
                write!(
                    out,
                    "let __tmp_{}: ::netidx_value::ValArray = (",
                    slot.rust_name,
                )
                .ok();
                write_rust_expr(out, a);
                out.push_str(").clone();\n");
            }
            TailCallSlotKind::Variant => {
                // Variant runtime type is `Value` (String for nullary,
                // Array for with-payload). VariantNew produces an
                // owned Value; a bare Local shadows the loop slot, so
                // explicit clone keeps the slot alive across the
                // assignment.
                write!(
                    out,
                    "let __tmp_{}: ::netidx_value::Value = (",
                    slot.rust_name,
                )
                .ok();
                write_rust_expr(out, a);
                out.push_str(").clone();\n");
            }
        }
    }
    for slot in slots.iter() {
        indent_into(out, indent);
        writeln!(out, "{} = __tmp_{};", slot.rust_name, slot.rust_name).ok();
    }
    indent_into(out, indent);
    out.push_str("continue;\n");
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
            typ: KirType::Prim(PrimType::F64),
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
            typ: KirType::Prim(PrimType::I64),
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
            typ: KirType::Prim(PrimType::I64),
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
            params: vec![i.clone()],
            fn_params: vec![],
            array_params: vec![],
            tuple_params: vec![],
            struct_params: vec![],
            variant_params: vec![],
            tail_call_slots: vec![TailCallSlot {
                name: i.name.clone(),
                rust_name: i.rust_name.clone(),
                kind: TailCallSlotKind::Scalar(i.prim),
            }],
            return_type: KirType::Prim(PrimType::I64),
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
