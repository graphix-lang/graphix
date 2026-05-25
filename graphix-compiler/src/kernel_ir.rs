//! Typed kernel intermediate representation.
//!
//! This is the shared form between the fusion analysis (which decides
//! what's fusable and produces a KIR tree) and the JIT backend that
//! lowers KIR to executable code:
//!
//! - `kir_to_clif` (in `crate::kir_jit`) — produces Cranelift IR
//!   for the JIT path. The compiled function pointer lives inside a
//!   `CraneliftNode<R, E>` wrapped around the would-be interpreter
//!   apply, with lazy compile + atomic swap.
//!
//! The JIT shares the fusion-side analysis. Anything KIR can't
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
    /// `Array<T>` — element type is any `KirType`, including nested
    /// composites (`Array<Tuple<i64, string>>`, `Array<Array<i64>>`,
    /// etc.). The runtime stores arrays as `ValArray<Value>` and
    /// `Value` is already nested-capable, so no runtime layout change
    /// is needed; only the static KIR type needs to track nesting.
    Array(Box<KirType>),
    /// `(T0, T1, T2, ...)` — order matters; per-slot types in source
    /// order. Slots can be any `KirType` (nested tuples / arrays /
    /// structs / variants allowed).
    Tuple(Vec<KirType>),
    /// `{field0: T0, field1: T1, ...}` — sorted alphabetically by
    /// field name (matching graphix's canonical struct layout). Field
    /// types can be any `KirType`.
    Struct(Vec<(ArcStr, KirType)>),
    /// `` `Foo(T0, T1) | `Bar(U0) `` — tagged union of cases. Each
    /// case carries its tag (an `ArcStr`) and zero-or-more payload
    /// types (any `KirType`). At runtime, a variant value with
    /// payloads is `Value::Array([Value::String("tag"), payload0,
    /// ...])`; nullary cases (no payloads) are
    /// `Value::String("tag")` and fall outside this fusion path (the
    /// v0 emitter rejects them). The cases list is the set of all
    /// cases that can flow into this position — typecheck-derived.
    Variant(Vec<(ArcStr, Vec<KirType>)>),
    /// "No useful value" — the return shape of side-effect-only
    /// builtins like `println`, `dbg`, `log`. Graphix surfaces it as
    /// `_` in val sigs, which the parser resolves to `Type::Bottom`.
    /// Kernels never *produce* a Unit-typed value that downstream
    /// code reads: `KirOp::DynCall { return_type: Unit }` discards
    /// the runtime `Value` returned by `Apply::update`. `Unit` is
    /// a **leaf type** — composing it (Array<Unit>, Tuple<[Unit,
    /// …]>, …) is rejected by the constructors. Adding it lets
    /// `extract_fn_signature` accept Bottom-returning callees,
    /// which is what `discover_builtin_fn_inputs` needs to register
    /// sync side-effect builtins as `FnSource::Builtin` slots.
    Unit,
    /// `ArcStr`-backed string. Lives outside [`PrimType`] because
    /// `ArcStr` isn't `Copy` (a refcounted thin pointer). The
    /// interpreter holds strings via
    /// [`crate::kir_interp::EvalResult::String`]; the JIT lowers
    /// String SSA values as a single `i64` (ArcStr's thin pointer)
    /// with explicit clone/drop helpers. Produced by
    /// [`KirOp::ConstStr`] and [`KirOp::Concat`]; consumed by
    /// `KirOp::DynCall` arg-marshalling to push a `Value::String`.
    /// Like `Unit`, treated as a **leaf type** — `Array<String>`,
    /// `Tuple<[String, _]>`, etc. aren't expressible at this layer.
    String,
    /// The single-value `null` type — graphix's `Type::Primitive(Typ::Null)`.
    /// Runtime representation is `Value::Null`. Used as the None case
    /// of [`Nullable`](`KirType::Nullable`) and to type the literal
    /// expression `null`. Like other leaves (`Unit`, `String`), Null
    /// is not composable directly into Tuple/Struct/Variant payload
    /// positions — those want a real T; nullability is expressed via
    /// `Nullable(Box<T>)`.
    Null,
    /// `[T, null]` — graphix's option shape. The runtime value is
    /// either `Value::Null` or `T`'s runtime form (no discriminator
    /// tag in between). Produced by builtins like
    /// `array::filter_map`'s callback return, struct fields typed
    /// `[T, null]`, etc. Consumed by `select x { null as _ => …, t as
    /// v => … }` patterns. Nesting (`Array<Nullable<T>>`,
    /// `Tuple<[Nullable<T>, _]>`, etc.) is expressible — the runtime
    /// just sees a slot whose value may be Null.
    Nullable(Box<KirType>),
}

impl KirType {
    /// Convenience for `KirType::Prim(_)` — when callers know they have
    /// a scalar.
    pub fn prim(p: PrimType) -> Self {
        KirType::Prim(p)
    }

    /// Convenience for `KirType::Array(_)` with a primitive element.
    /// For composite elements construct directly via
    /// `KirType::Array(Box::new(inner))`.
    pub fn array(elem: PrimType) -> Self {
        KirType::Array(Box::new(KirType::Prim(elem)))
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

    /// Returns the element type if this is an array; `None` for
    /// scalars / tuples / structs.
    pub fn as_array_elem(&self) -> Option<&KirType> {
        match self {
            KirType::Array(t) => Some(t),
            _ => None,
        }
    }

    /// Backwards-compatible helper: returns the element `PrimType` if
    /// this is `Array<Prim(P)>`; `None` if the element is composite or
    /// the value isn't an array. Use [`Self::as_array_elem`] for the
    /// fully-general accessor; this exists for sites that genuinely
    /// only handle the scalar case (e.g. legacy JIT helper selection).
    pub fn as_array_prim(&self) -> Option<PrimType> {
        self.as_array_elem().and_then(KirType::as_prim)
    }

    /// Returns the per-slot types if this is a tuple; `None` otherwise.
    pub fn as_tuple_elems(&self) -> Option<&[KirType]> {
        match self {
            KirType::Tuple(es) => Some(es),
            _ => None,
        }
    }

    /// Returns the (sorted) field list if this is a struct; `None`
    /// otherwise.
    pub fn as_struct_fields(&self) -> Option<&[(ArcStr, KirType)]> {
        match self {
            KirType::Struct(fs) => Some(fs),
            _ => None,
        }
    }

    /// Returns the case list if this is a variant; `None` otherwise.
    pub fn as_variant_cases(&self) -> Option<&[(ArcStr, Vec<KirType>)]> {
        match self {
            KirType::Variant(cs) => Some(cs),
            _ => None,
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
        // case where every payload converts to a KirType (now
        // possibly composite). Nullary cases (empty payloads) are
        // accepted — they flow at runtime as `Value::String(tag)`,
        // distinct from `Value::Array([tag, ...payloads])` for the
        // with-payload case; the kernel boundary takes `&Value` and
        // dispatches at access sites.
        fn variant_case(
            t: &Type,
        ) -> Option<(ArcStr, Vec<KirType>)> {
            t.with_deref(|resolved| match resolved? {
                Type::Variant(tag, ts) => {
                    let payloads: Option<Vec<KirType>> = ts
                        .iter()
                        .map(|p| KirType::from_type(p))
                        .collect();
                    payloads.map(|p| (tag.clone(), p))
                }
                _ => None,
            })
        }
        // Helper: identify a `Type::Primitive` whose only variant is
        // `Typ::Null`. Used both as a top-level lowering (→
        // `KirType::Null`) and as the marker arm of a
        // `[T, null]` Set (→ `KirType::Nullable(T)`).
        fn is_null_primitive(t: &Type) -> bool {
            t.with_deref(|resolved| match resolved {
                Some(Type::Primitive(p)) => {
                    p.contains(netidx_value::Typ::Null) && p.iter().count() == 1
                }
                _ => false,
            })
        }
        t.with_deref(|resolved| match resolved? {
            // `_` (Bottom) is the return type of side-effect-only
            // builtins. Lower it to `KirType::Unit` so dispatch can
            // register them — the value is never consumed downstream.
            Type::Bottom => Some(KirType::Unit),
            // String primitives lower to `KirType::String` (lives
            // outside `PrimType` because ArcStr doesn't fit a
            // register).
            Type::Primitive(p)
                if p.contains(netidx_value::Typ::String)
                    && p.iter().count() == 1 =>
            {
                Some(KirType::String)
            }
            // Null primitive lowers to `KirType::Null` — the
            // single-value null shape. (Multi-flag primitives with
            // Null + other variants fall through to `PrimType::from_type`,
            // which rejects them.)
            Type::Primitive(p)
                if p.contains(netidx_value::Typ::Null) && p.iter().count() == 1 =>
            {
                Some(KirType::Null)
            }
            Type::Array(inner) => {
                KirType::from_type(inner).map(|t| KirType::Array(Box::new(t)))
            }
            Type::Tuple(elems) => {
                let elems: Option<Vec<KirType>> = elems
                    .iter()
                    .map(|e| KirType::from_type(e))
                    .collect();
                elems.map(KirType::Tuple)
            }
            Type::Variant(_, _) => {
                // Single-case variant (uncommon but legal).
                variant_case(resolved?).map(|c| KirType::Variant(vec![c]))
            }
            Type::Set(members) => {
                // First try `[T, null]` shape (graphix's `Option<T>`):
                // exactly two members, one of which is the null
                // primitive. Order-independent — `[T, null]` and
                // `[null, T]` both lower to `KirType::Nullable(T)`.
                if members.len() == 2 {
                    let (null_idx, t_idx) =
                        match (is_null_primitive(&members[0]), is_null_primitive(&members[1])) {
                            (true, false) => (0, 1),
                            (false, true) => (1, 0),
                            _ => (usize::MAX, usize::MAX),
                        };
                    if null_idx != usize::MAX {
                        if let Some(inner) = KirType::from_type(&members[t_idx]) {
                            return Some(KirType::Nullable(Box::new(inner)));
                        }
                        return None;
                    }
                    // `[T, Error<X>]` shape (graphix's `Result<T, X>`):
                    // exactly two members, one a `Type::Error`. Lowered
                    // to `KirType::Nullable(T)` — the wire shape is
                    // identical (Value-shape two-register) and the
                    // boundary marshals the inner `Value` through
                    // opaquely, so a `Value::Error(...)` flows through
                    // as-is to downstream consumers that store it
                    // (Bind) or read it (Ref). Pattern-matching the
                    // Error case won't dispatch correctly — that's a
                    // documented cliff requiring a real `KirType::Result`
                    // variant.
                    let is_err = |t: &Type| -> bool {
                        t.with_deref(|r| matches!(r, Some(Type::Error(_))))
                    };
                    let (err_idx, t_idx) =
                        match (is_err(&members[0]), is_err(&members[1])) {
                            (true, false) => (0, 1),
                            (false, true) => (1, 0),
                            _ => (usize::MAX, usize::MAX),
                        };
                    if err_idx != usize::MAX {
                        if let Some(inner) = KirType::from_type(&members[t_idx]) {
                            return Some(KirType::Nullable(Box::new(inner)));
                        }
                        return None;
                    }
                }
                // Variant unions: `[ \`Foo(...), \`Bar(...) ]` parses
                // as a Set of single-Variant types. Match this
                // shape; reject Sets with non-variant members
                // (those are general unions — different fusion
                // story).
                let cases: Option<Vec<(ArcStr, Vec<KirType>)>> = members
                    .iter()
                    .map(|m| variant_case(m))
                    .collect();
                cases.map(KirType::Variant)
            }
            Type::Struct(fields) => {
                let fs: Option<Vec<(ArcStr, KirType)>> = fields
                    .iter()
                    .map(|(n, t)| KirType::from_type(t).map(|p| (n.clone(), p)))
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Lte,
    Gte,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BoolOp {
    And,
    Or,
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

}

// ─── Inputs and known fns/consts ─────────────────────────────────

/// One input to a fused kernel — a binding the kernel reads. `name` is
/// the source-level identifier; `bind_id` is set when the input came
/// from `discover_inputs` walking a real graph, `None` for synthetic
/// inputs (lambda args, let-bindings introduced by the body emitter).
#[derive(Debug, Clone)]
pub struct Input {
    pub name: ArcStr,
    pub prim: PrimType,
    pub bind_id: Option<BindId>,
}

/// An array value passed as a kernel parameter. The element type is
/// any [`KirType`] — primitive or nested composite. Runtime value is
/// `&ValArray`; for primitive elements the JIT loads via
/// `arr.get_unchecked::<T>(i)`, for composite elements via a
/// `*mut Value` accessor.
#[derive(Debug, Clone)]
pub struct ArrayInput {
    pub name: ArcStr,
    pub elem: KirType,
    pub bind_id: Option<BindId>,
}

/// A tuple value passed as a kernel parameter. Per-slot types can be
/// any [`KirType`] (nested tuples / structs / variants / arrays
/// allowed). Same `&ValArray` runtime boundary as [`ArrayInput`].
#[derive(Debug, Clone)]
pub struct TupleInput {
    pub name: ArcStr,
    pub elems: Vec<KirType>,
    pub bind_id: Option<BindId>,
}

/// A struct value passed as a kernel parameter. Field types can be
/// any [`KirType`]. Same `&ValArray` boundary, with fields stored at
/// compile-time-known sorted-by-name positions.
#[derive(Debug, Clone)]
pub struct StructInput {
    pub name: ArcStr,
    pub fields: Vec<(ArcStr, KirType)>,
    pub bind_id: Option<BindId>,
}

/// A variant value passed as a kernel parameter. Payload types per
/// case can be any [`KirType`]. Same `&ValArray` boundary as
/// tuples/structs, but the slot at index 0 is the tag string (an
/// interned `ArcStr`) and payloads start at index 1. `cases`
/// enumerates the legal `(tag, payload_types)` shapes — at runtime
/// exactly one of these is active per value.
#[derive(Debug, Clone)]
pub struct VariantInput {
    pub name: ArcStr,
    pub cases: Vec<(ArcStr, Vec<KirType>)>,
    pub bind_id: Option<BindId>,
}

/// A nullable-typed kernel input — graphix's `[T, null]` option
/// shape. Runtime representation is a `Value` that is either
/// `Value::Null` or `T`'s runtime form. `elem` is the inner type
/// (always non-null; the nullability is implicit in this slot
/// list's identity).
#[derive(Debug, Clone)]
pub struct NullableInput {
    pub name: ArcStr,
    pub elem: KirType,
    pub bind_id: Option<BindId>,
}

/// A string-typed kernel let-binding. Strings only appear as
/// in-kernel locals — never as kernel params (no string args on
/// either backend) and never as `RegionInput` (no string region-
/// input shape). Runtime representation is an owned `ArcStr` slot
/// in the env's string table.
#[derive(Debug, Clone)]
pub struct StringInput {
    pub name: ArcStr,
    pub bind_id: Option<BindId>,
}

/// Per-source-arg metadata used by the tail-call renderer to assign
/// each new value to the right destination. Populated by
/// `build_kir_kernel` in lambda argspec order — so
/// `tail_call_slots[i]` describes the destination of the `i`th
/// tail-call arg.
#[derive(Debug, Clone)]
pub struct TailCallSlot {
    pub name: ArcStr,
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
    /// `[T, null]` option shape — runtime representation is `Value`
    /// (either `Value::Null` or `T`'s form). Tail-call rebinding
    /// clones an owned `Value` into the shadowed local via
    /// `rebind_nullable`. Same wire format as `Variant` but routed
    /// to `env.nullables` rather than `env.variants`.
    Nullable,
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
    /// A string-typed constant. Used by `emit_expr`'s
    /// `ExprKind::Constant(Value::String)` arm and by `Concat`'s
    /// literal segments. Result type is [`KirType::String`].
    /// Lives outside [`ConstVal`] because [`ConstVal`] is `Copy` and
    /// `ArcStr` isn't — keeping `ConstVal` `Copy` matters for the
    /// `kir_interp` register paths.
    ConstStr(ArcStr),
    /// Concatenation of string-renderable parts — the KIR form of
    /// `ExprKind::StringInterpolate` (`"x=[x]"`). Each child must
    /// be [`KirType::String`] or [`KirType::Prim`]; prim children
    /// are formatted via the netidx `Value` `Display` (the same
    /// renderer `StringInterpolate` uses at non-fusion time).
    /// Result type is [`KirType::String`].
    Concat(Vec<KirExpr>),
    /// The literal `null`. Result type is [`KirType::Null`]. Runtime
    /// representation is `Value::Null`. The JIT can't lower this op
    /// today — kernels containing it route to the interpreter via
    /// `kernel_contains_null` (same fallback shape as `String`).
    ConstNull,
    /// Test whether a nullable value is `null`. The operand must be
    /// [`KirType::Null`] or [`KirType::Nullable`]; result is
    /// `Bool`. Emitted as the condition for `select` arms whose
    /// type-predicate is `null`. JIT-gated by `kernel_contains_null`.
    IsNull(Box<KirExpr>),
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
    /// The cranelift JIT supports `DynCall` end-to-end (scalar /
    /// composite args, scalar / composite / Value-shape returns,
    /// pending-path correctness). See the JIT codegen notes in
    /// `CLAUDE.md` for the dispatch ABI and pending-path discipline.
    ///
    /// Type info is carried on the op rather than re-derived at
    /// runtime so the interpreter doesn't have to look up
    /// `kernel.fn_params[fn_index]` on every call.
    DynCall {
        fn_index: u32,
        args: Vec<KirExpr>,
        /// Argument types parallel to `args`. Used by the dispatch
        /// machinery to encode each arg into a `netidx::Value` and
        /// (in the JIT path) to pick the right buf-push helper —
        /// scalars marshal as primitives, composites pass as
        /// refcount-bumped clones of the caller's owned pointer.
        arg_types: Vec<KirType>,
        /// Return type. Equals the wrapping `KirExpr.typ`; carried
        /// here for symmetry with `arg_types` and so the JIT can
        /// pick the right `cast_u64_to_*` / `Box::from_raw` decode
        /// path without re-walking `KirExpr.typ`.
        return_type: KirType,
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
    /// Result `KirExpr.typ` matches `elem_typ`. For `Prim` slots the
    /// runtime extracts a scalar via `ValArray::get_unchecked`; for
    /// composite slots (nested array/tuple/struct/variant) the runtime
    /// returns the slot's `Value` wrapped in an `EvalResult::ValArray`
    /// or `Variant`. Kernels containing composite-slot accesses route
    /// to the interpreter via `kernel_contains_composite_element_op`
    /// (the JIT's primitive-only extraction path can't represent the
    /// composite return).
    TupleGet {
        name: ArcStr,
        idx: usize,
        elem_typ: KirType,
    },
    /// Build a tuple from per-slot expressions. Each `fields[i]`
    /// emits to a KIR expression of type `elem_types[i]`; the
    /// interpreter / JIT wrap primitives in `Value::<variant>(x)`
    /// and feed composite values (tuples, structs, variants, arrays,
    /// nullables, strings) through the appropriate boundary helper.
    /// Result type is `KirType::Tuple(elem_types)`.
    TupleNew {
        fields: Vec<KirExpr>,
        elem_types: Vec<KirType>,
    },
    /// Read struct field `field` from `name` (a struct kernel
    /// parameter), at the sorted index `sorted_idx`. Result type
    /// matches `elem_typ` — composite-typed fields produce
    /// composite `EvalResult`s in the interpreter; the JIT routes
    /// kernels with composite-field access to the interpreter via
    /// `kernel_contains_composite_element_op`.
    StructGet {
        name: ArcStr,
        field: ArcStr,
        sorted_idx: usize,
        elem_typ: KirType,
    },
    /// Build a struct from per-field expressions. `sorted_fields` is
    /// the canonical alphabetical order (graphix's runtime layout).
    /// Lowers identically to `TupleNew` — the runtime doesn't
    /// distinguish tuples from structs at the ValArray level. Field
    /// types in `sorted_types` may be composite (mirroring TupleNew's
    /// generalisation).
    StructNew {
        sorted_fields: Vec<(ArcStr, KirExpr)>,
        sorted_types: Vec<(ArcStr, KirType)>,
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
    /// — exactly one case (the one being constructed). Payload types
    /// may be composite (mirroring TupleNew/StructNew's
    /// generalisation).
    VariantNew {
        tag: ArcStr,
        payloads: Vec<KirExpr>,
        payload_types: Vec<KirType>,
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
    /// Evaluate `expr` for its side effect, discard the result.
    /// Used for `KirType::Unit` calls (`println`, `dbg`, `log`, …)
    /// and any other expression whose value the program doesn't
    /// consume. The interpreter / JIT evaluates `expr` (firing any
    /// `KirOp::DynCall`s or producer-op effects inside it) and
    /// throws away the `EvalResult`.
    Discard(KirExpr),
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
    /// `[T, null]` option-shape parameters. `KirOp::IsNull` and Local
    /// reads against names here yield the slot's `Value` (either
    /// `Value::Null` or `T`'s runtime form). Stored separately from
    /// `variant_params` even though both use `Value` at runtime —
    /// the consumer ops and the env slot list differ.
    pub nullable_params: Vec<NullableInput>,
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
    /// Argument types of the *callee* function. Carries scalar
    /// primitives plus composite (Array/Tuple/Struct/Variant)
    /// shapes — the JIT marshals each arg into a `netidx::Value`
    /// per its declared kind before dispatch.
    pub arg_types: Vec<KirType>,
    /// Return type of the callee. May be scalar or composite; the
    /// dispatcher and JIT codegen branch on the kind to pick the
    /// right encode/decode path.
    pub return_type: KirType,
}

/// How a [`FnParam`]'s callable is sourced at dispatch time.
#[derive(Debug, Clone)]
pub enum FnSource {
    /// HOF argument: the kernel's caller passes a `LambdaDef` value
    /// at position `arg_pos` (zero-based, in the lambda's source-
    /// order argument list, mixed with primitive args). KirNode's
    /// runtime extracts it from the incoming `from` slice.
    Param { arg_pos: u32 },
    /// Statically-resolved user binding: the `LambdaDef` lives in
    /// `ctx.cached[bind_id]` (or, for unstable bindings,
    /// `event.variables[bind_id]`). Set when fusion can't fuse the
    /// callee inline (its body uses unsupported constructs) but can
    /// still call it via Apply::update.
    Binding { bind_id: crate::BindId },
    /// Sync builtin call. Resolved at `KirNode::new` time by looking
    /// up `name` in `ctx.builtins`, constructing the builtin's
    /// `Apply<R, E>` via the registered init fn, and stashing it in
    /// the per-kernel slot. The slot is pre-bound — `dispatch` skips
    /// the `LambdaDef`-rebind check entirely.
    ///
    /// `typ` is the resolved FnType at the call site (read by
    /// fusion off `a.function.typ.get()` — the typed-AST cell on
    /// the function expression of the Apply), needed by builtin
    /// init fns.
    ///
    /// `layout` describes the callee's full formal-arg list (one
    /// entry per `typ.args` slot, in declaration order — same order
    /// `Apply::update` reads `from[]`). `Positional` slots are fed
    /// by the kernel-marshalled call args (indexed by their position
    /// in `FnParam.arg_types`). `LabeledDefault` slots get the
    /// captured default expression compiled once at
    /// `pre_bind_builtin` time and never updated again (mirrors
    /// `CallSite::bind`'s `compile_default!` macro, but resolved
    /// once because the call site is fixed in a fused kernel).
    Builtin {
        name: ArcStr,
        typ: std::sync::Arc<crate::typ::FnType>,
        layout: std::sync::Arc<[BuiltinSlot]>,
        /// Lambda ID of the binding this call resolves to (when the
        /// fusion discovery pass could identify it). Used by
        /// `KirNode::pre_bind_builtin` to look up the lambda's
        /// env+scope so a `BuiltinSlot::LabeledDefault` whose
        /// expression references free variables visible only in
        /// the lambda's original module scope (e.g. `default_escape`
        /// in `str::escape`'s `#esc = default_escape`) compiles
        /// correctly. `None` is safe — pre_bind_builtin falls back
        /// to compiling defaults in the kernel's own scope.
        lambda_id: Option<crate::LambdaId>,
    },
}

/// Per-formal-arg routing for a [`FnSource::Builtin`] slot —
/// one entry per arg in the callee's declared signature, in
/// declaration order. See [`FnSource::Builtin`].
#[derive(Debug, Clone)]
pub enum BuiltinSlot {
    /// Fed by the kernel — `Positional(call_idx)` reads value
    /// `call_idx` from the kernel's marshalled call-arg list
    /// (i.e. the `FnParam.arg_types[call_idx]`-typed value).
    Positional(usize),
    /// Filled at `pre_bind_builtin` time by compiling the captured
    /// default expression. The result `Node` lives in the slot's
    /// `arg_refs[i]` for the life of the kernel; no per-dispatch
    /// work.
    LabeledDefault(crate::expr::Expr),
    /// One slot representing the variadic tail. At dispatch time,
    /// the kernel's call args `[from_call_idx, from_call_idx + count)`
    /// are forwarded as additional positional refs to the inner
    /// Apply — the Apply's own vargs handling collects them into the
    /// expected `Array<T>` per its declared FnType. `count` is fixed
    /// at fusion time because each call site's arity is captured in
    /// its own `FnParam` (different arities → different FnParams,
    /// keyed by `(name, arity)` in `find_fn_input`).
    Variadic { from_call_idx: usize, count: usize },
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
    KirExpr {
        op: KirOp::Local(name),
        typ: KirType::Array(Box::new(KirType::Prim(elem))),
    }
}

/// True if the kernel contains a [`KirOp::Call`] anywhere — i.e.
/// non-tail self-recursion or cross-kernel calls. The interpreter and
/// JIT v1 do not handle these. Callers wiring KIR through the runtime
/// path use this to refuse to instantiate a `KirNode` that would panic
/// on first call. Lifts in M4-followups when the kernel registry is in
/// place.
pub fn kernel_contains_call(kernel: &KirKernel) -> bool {
    kernel.body.iter().any(stmt_has_call)
}

fn stmt_has_call(stmt: &KirStmt) -> bool {
    match stmt {
        KirStmt::Let(l) => expr_has_call(&l.value),
        KirStmt::Return(e) => expr_has_call(e),
        KirStmt::Discard(e) => expr_has_call(e),
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
        KirOp::Const(_)
        | KirOp::ConstStr(_)
        | KirOp::ConstNull
        | KirOp::Local(_)
        | KirOp::ArrayLen { .. } => false,
        KirOp::Concat(parts) => parts.iter().any(expr_has_call),
        KirOp::IsNull(inner) => expr_has_call(inner),
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

/// True if the kernel contains a `TupleGet` / `StructGet` (and
/// eventually array / variant accessors) whose `elem_typ` is a
/// composite (non-`Prim`) `KirType`. The JIT's primitive-only
/// scalar extraction can't handle these — callers gate on this
/// helper to route the whole kernel to the interpreter (where
/// `extract_composite_or_scalar` handles both cases).
pub fn kernel_contains_composite_element_op(kernel: &KirKernel) -> bool {
    kernel.body.iter().any(stmt_has_composite_element_op)
}

/// True if the kernel references [`KirType::Null`] or
/// [`KirType::Nullable`] anywhere — via [`KirOp::ConstNull`],
/// [`KirOp::IsNull`], a Null/Nullable arg/return on a `DynCall`,
/// or any `KirExpr.typ` of that shape. The JIT can't lower these
/// today, so such kernels route through the interpreter — same
/// fallback shape as `kernel_contains_string`.
pub fn kernel_contains_null(kernel: &KirKernel) -> bool {
    if matches!(kernel.return_type, KirType::Null | KirType::Nullable(_)) {
        return true;
    }
    kernel.body.iter().any(stmt_has_null)
}

fn stmt_has_null(s: &KirStmt) -> bool {
    match s {
        KirStmt::Let(l) => expr_has_null(&l.value),
        KirStmt::Return(e) => expr_has_null(e),
        KirStmt::Discard(e) => expr_has_null(e),
        KirStmt::TailCall { args } => args.iter().any(expr_has_null),
        KirStmt::Select { arms } => arms.iter().any(|a| {
            a.cond.as_ref().is_some_and(expr_has_null)
                || a.body.iter().any(stmt_has_null)
        }),
    }
}

fn expr_has_null(e: &KirExpr) -> bool {
    if matches!(e.typ, KirType::Null | KirType::Nullable(_)) {
        return true;
    }
    match &e.op {
        KirOp::ConstNull | KirOp::IsNull(_) => true,
        KirOp::Const(_) | KirOp::ConstStr(_) | KirOp::Local(_)
        | KirOp::ArrayLen { .. } | KirOp::TupleGet { .. }
        | KirOp::StructGet { .. } | KirOp::VariantTagEq { .. }
        | KirOp::VariantPayload { .. } => false,
        KirOp::Concat(parts) => parts.iter().any(expr_has_null),
        KirOp::Bin { lhs, rhs, .. }
        | KirOp::Cmp { lhs, rhs, .. }
        | KirOp::BoolBin { lhs, rhs, .. } => {
            expr_has_null(lhs) || expr_has_null(rhs)
        }
        KirOp::Not(inner) | KirOp::Cast { inner, .. } => expr_has_null(inner),
        KirOp::ArrayGet { idx, .. } => expr_has_null(idx),
        KirOp::ArrayFold { init, body, .. } => {
            expr_has_null(init) || expr_has_null(body)
        }
        KirOp::ArrayInit { n, body, .. } => expr_has_null(n) || expr_has_null(body),
        KirOp::ArrayMap { body, .. } => expr_has_null(body),
        KirOp::ArrayFilter { predicate, .. } => expr_has_null(predicate),
        KirOp::TupleNew { fields, .. } => fields.iter().any(expr_has_null),
        KirOp::StructNew { sorted_fields, .. } => {
            sorted_fields.iter().any(|(_, e)| expr_has_null(e))
        }
        KirOp::VariantNew { payloads, .. } => payloads.iter().any(expr_has_null),
        KirOp::Block { lets, tail } => {
            lets.iter().any(|l| expr_has_null(&l.value)) || expr_has_null(tail)
        }
        KirOp::IfChain { arms } => arms.iter().any(|(c, v)| {
            c.as_ref().is_some_and(expr_has_null) || expr_has_null(v)
        }),
        KirOp::Call { args, .. } => args.iter().any(expr_has_null),
        KirOp::DynCall { args, arg_types, return_type, .. } => {
            matches!(return_type, KirType::Null | KirType::Nullable(_))
                || arg_types.iter().any(|t| {
                    matches!(t, KirType::Null | KirType::Nullable(_))
                })
                || args.iter().any(expr_has_null)
        }
    }
}

fn stmt_has_composite_element_op(s: &KirStmt) -> bool {
    match s {
        KirStmt::Let(l) => expr_has_composite_element_op(&l.value),
        KirStmt::Return(e) => expr_has_composite_element_op(e),
        KirStmt::Discard(e) => expr_has_composite_element_op(e),
        KirStmt::TailCall { args } => {
            args.iter().any(expr_has_composite_element_op)
        }
        KirStmt::Select { arms } => arms.iter().any(|a| {
            a.cond.as_ref().is_some_and(expr_has_composite_element_op)
                || a.body.iter().any(stmt_has_composite_element_op)
        }),
    }
}

fn expr_has_composite_element_op(e: &KirExpr) -> bool {
    match &e.op {
        KirOp::TupleGet { elem_typ, .. } | KirOp::StructGet { elem_typ, .. } => {
            !matches!(elem_typ, KirType::Prim(_))
        }
        // ArrayGet's element type lives on `e.typ` (no separate
        // elem_typ field). Composite-result ArrayGet routes to interp.
        KirOp::ArrayGet { idx, .. } => {
            !matches!(e.typ, KirType::Prim(_))
                || expr_has_composite_element_op(idx)
        }
        KirOp::Const(_) | KirOp::ConstStr(_) | KirOp::ConstNull
        | KirOp::Local(_) | KirOp::ArrayLen { .. }
        | KirOp::VariantTagEq { .. } | KirOp::VariantPayload { .. } => false,
        KirOp::Concat(parts) => parts.iter().any(expr_has_composite_element_op),
        KirOp::IsNull(inner) => expr_has_composite_element_op(inner),
        KirOp::Bin { lhs, rhs, .. }
        | KirOp::Cmp { lhs, rhs, .. }
        | KirOp::BoolBin { lhs, rhs, .. } => {
            expr_has_composite_element_op(lhs)
                || expr_has_composite_element_op(rhs)
        }
        KirOp::Not(inner) | KirOp::Cast { inner, .. } => {
            expr_has_composite_element_op(inner)
        }
        KirOp::ArrayFold { init, body, .. } => {
            expr_has_composite_element_op(init)
                || expr_has_composite_element_op(body)
        }
        KirOp::ArrayInit { n, body, .. } => {
            expr_has_composite_element_op(n)
                || expr_has_composite_element_op(body)
        }
        KirOp::ArrayMap { body, .. } => expr_has_composite_element_op(body),
        KirOp::ArrayFilter { predicate, .. } => {
            expr_has_composite_element_op(predicate)
        }
        KirOp::TupleNew { fields, .. } => {
            fields.iter().any(expr_has_composite_element_op)
        }
        KirOp::StructNew { sorted_fields, .. } => sorted_fields
            .iter()
            .any(|(_, e)| expr_has_composite_element_op(e)),
        KirOp::VariantNew { payloads, .. } => {
            payloads.iter().any(expr_has_composite_element_op)
        }
        KirOp::Block { lets, tail } => {
            lets.iter().any(|l| expr_has_composite_element_op(&l.value))
                || expr_has_composite_element_op(tail)
        }
        KirOp::IfChain { arms } => arms.iter().any(|(c, v)| {
            c.as_ref().is_some_and(expr_has_composite_element_op)
                || expr_has_composite_element_op(v)
        }),
        KirOp::Call { args, .. } | KirOp::DynCall { args, .. } => {
            args.iter().any(expr_has_composite_element_op)
        }
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
        KirStmt::Discard(e) => walk_call_sites_expr(e, out),
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
        KirOp::Concat(parts) => {
            for p in parts {
                walk_call_sites_expr(p, out);
            }
        }
        KirOp::Const(_)
        | KirOp::ConstStr(_)
        | KirOp::ConstNull
        | KirOp::Local(_)
        | KirOp::ArrayLen { .. } => {}
        KirOp::Bin { lhs, rhs, .. }
        | KirOp::Cmp { lhs, rhs, .. }
        | KirOp::BoolBin { lhs, rhs, .. } => {
            walk_call_sites_expr(lhs, out);
            walk_call_sites_expr(rhs, out);
        }
        KirOp::Not(inner) | KirOp::Cast { inner, .. } => walk_call_sites_expr(inner, out),
        KirOp::IsNull(inner) => walk_call_sites_expr(inner, out),
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
    fn cast_bool_rejected() {
        let x = loc("x", PrimType::Bool);
        assert!(cast(x, PrimType::I64).is_none());
        let y = loc("y", PrimType::I64);
        assert!(cast(y, PrimType::Bool).is_none());
    }
}
