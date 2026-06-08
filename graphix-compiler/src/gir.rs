//! Typed kernel intermediate representation.
//!
//! This is the shared form between the fusion analysis (which decides
//! what's fusable and produces a GIR tree) and the JIT backend that
//! lowers GIR to executable code:
//!
//! - `gir_to_clif` (in `crate::gir_jit`) — produces Cranelift IR
//!   for the JIT path. The compiled function pointer lives inside a
//!   `CraneliftNode<R, E>` wrapped around the would-be interpreter
//!   apply, with lazy compile + atomic swap.
//!
//! The JIT shares the fusion-side analysis. Anything GIR can't
//! represent is by definition not fusable; the fusion pass falls back
//! to the interpreter for that subtree.
//!
//! ## Shape
//!
//! - [`GirExpr`] is a typed expression node — every expression carries
//!   its [`PrimType`]. The `op` field is a [`GirOp`] enum spanning all
//!   supported expression forms.
//!
//! - [`GirStmt`] is a statement that appears in a function body —
//!   `let`-bindings, `return`, statement-form `select` chains, and
//!   self-tail-calls. Function bodies are `Vec<GirStmt>`.
//!
//! - [`GirKernel`] is a complete function: name, params, return type,
//!   `has_tail_loop` flag (whether the body ends with a self-recursive
//!   tail call and thus needs a surrounding `loop {}`), and body.
//!
//! Expression-form blocks (`{ let a = ..; let b = ..; tail }`) live
//! inside [`GirOp::Block`] with a flat list of [`Let`]s and a tail
//! [`GirExpr`]. Expression-form `select` lowers to [`GirOp::IfChain`]
//! — a sequence of (cond, value) pairs where the last entry's `cond`
//! may be `None` for an unconditional `else`.

use crate::{
    typ::{AbstractId, Type},
    BindId,
};
use arcstr::ArcStr;
use netidx_value::{Typ, Value};
use std::sync::LazyLock;

/// Fusion-time registry mapping each abstract type's [`AbstractId`] to its
/// concrete implementation type. Populated by
/// `node::module::check_sig` when a signed module's `type X;` is matched
/// to its impl `type X = …`. [`GirType::from_type`] consults it to lower
/// an abstract-typed value to its concrete `GirType` — the optimizer peeks
/// through the abstraction, but only here (the type system keeps it opaque
/// everywhere else, and `from_type` is only ever called during fusion).
/// Keyed by the globally-unique `AbstractId`, so it is safe to share
/// across concurrently-compiling `ExecCtx`s.
pub static ABSTRACT_REGISTRY: LazyLock<
    parking_lot::RwLock<nohash::IntMap<AbstractId, Type>>,
> = LazyLock::new(|| parking_lot::RwLock::new(nohash::IntMap::default()));

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
pub enum GirType {
    Prim(PrimType),
    /// `Array<T>` — element type is any `GirType`, including nested
    /// composites (`Array<Tuple<i64, string>>`, `Array<Array<i64>>`,
    /// etc.). The runtime stores arrays as `ValArray<Value>` and
    /// `Value` is already nested-capable, so no runtime layout change
    /// is needed; only the static GIR type needs to track nesting.
    Array(Box<GirType>),
    /// `(T0, T1, T2, ...)` — order matters; per-slot types in source
    /// order. Slots can be any `GirType` (nested tuples / arrays /
    /// structs / variants allowed).
    Tuple(Vec<GirType>),
    /// `{field0: T0, field1: T1, ...}` — sorted alphabetically by
    /// field name (matching graphix's canonical struct layout). Field
    /// types can be any `GirType`.
    Struct(Vec<(ArcStr, GirType)>),
    /// `` `Foo(T0, T1) | `Bar(U0) `` — tagged union of cases. Each
    /// case carries its tag (an `ArcStr`) and zero-or-more payload
    /// types (any `GirType`). At runtime, a variant value with
    /// payloads is `Value::Array([Value::String("tag"), payload0,
    /// ...])`; nullary cases (no payloads) are
    /// `Value::String("tag")` and fall outside this fusion path (the
    /// v0 emitter rejects them). The cases list is the set of all
    /// cases that can flow into this position — typecheck-derived.
    Variant(Vec<(ArcStr, Vec<GirType>)>),
    /// "No useful value" — the return shape of side-effect-only
    /// builtins like `println`, `dbg`, `log`. Graphix surfaces it as
    /// `_` in val sigs, which the parser resolves to `Type::Bottom`.
    /// Kernels never *produce* a Unit-typed value that downstream
    /// code reads: `GirOp::DynCall { return_type: Unit }` discards
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
    /// [`crate::gir_interp::EvalResult::String`]; the JIT lowers
    /// String SSA values as a single `i64` (ArcStr's thin pointer)
    /// with explicit clone/drop helpers. Produced by
    /// [`GirOp::ConstStr`] and [`GirOp::Concat`]; consumed by
    /// `GirOp::DynCall` arg-marshalling to push a `Value::String`.
    /// Like `Unit`, treated as a **leaf type** — `Array<String>`,
    /// `Tuple<[String, _]>`, etc. aren't expressible at this layer.
    String,
    /// The single-value `null` type — graphix's `Type::Primitive(Typ::Null)`.
    /// Runtime representation is `Value::Null`. Used as the None case
    /// of [`Nullable`](`GirType::Nullable`) and to type the literal
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
    Nullable(Box<GirType>),
    /// `datetime` — graphix's `Type::Primitive(Typ::DateTime)`.
    /// Runtime representation is `Value::DateTime(Arc<DateTime<Utc>>)`
    /// — a thin `Arc` pointer, so it flows through the JIT as a
    /// two-register `Value` (disc + payload), exactly like
    /// [`Variant`](GirType::Variant) / [`Nullable`](GirType::Nullable).
    /// Arithmetic (`datetime ± duration`) lowers to [`GirOp::ValueArith`]
    /// which reuses netidx's `impl {Add,Sub} for Value`.
    DateTime,
    /// `duration` — graphix's `Type::Primitive(Typ::Duration)`.
    /// Runtime representation is `Value::Duration(Arc<Duration>)`;
    /// Value-shape like [`DateTime`](GirType::DateTime). Arithmetic
    /// (`duration ± duration`, `duration {*,/} <number>`,
    /// `<number> * duration`) lowers to [`GirOp::ValueArith`].
    Duration,
    /// `bytes` — graphix's `Type::Primitive(Typ::Bytes)`. Runtime
    /// representation is `Value::Bytes(PBytes)` (a refcounted thin
    /// pointer). Treated as a **Value-shape** type (two-register
    /// `Value`, like [`DateTime`](GirType::DateTime)) rather than a
    /// single-register leaf like [`String`](GirType::String): a `bytes`
    /// literal lowers to [`GirOp::ConstValue`] and rides the same
    /// `graphix_value_clone`/`drop` + `value_inputs` machinery, so no
    /// bytes-specific JIT helpers / slot lists are needed. (The
    /// redundant disc word vs. String's single-register form is a
    /// negligible cost for a much smaller, DRY implementation.)
    Bytes,
    /// `Map<K, V>` — graphix's `Type::Map`. Runtime representation is
    /// `Value::Map(CMap)` where `CMap = immutable_chunkmap::Map` is a
    /// refcounted thin pointer, so — like [`Bytes`](GirType::Bytes) —
    /// it's a **Value-shape** type (two-register `Value`) reusing the
    /// `graphix_value_clone`/`drop` + `value_inputs` machinery. A
    /// constant map literal (`{"a" => 1, ...}` with all-constant
    /// keys/values) folds to [`GirOp::ConstValue`]; map builtins
    /// (`map::len`, `map::get`, …) consume/produce it via `DynCall`.
    /// (Dynamic map literals and `m{key}` access aren't lowered yet.)
    Map,
    /// `Error<T>` — graphix's `Type::Error`. Runtime representation is
    /// `Value::Error(Arc<Value>)`, a refcounted thin pointer, so — like
    /// [`Bytes`](GirType::Bytes) / [`Map`](GirType::Map) — it's a
    /// **Value-shape** type (two-register `Value`) reusing the
    /// `graphix_value_clone`/`drop` + `value_inputs` machinery. The
    /// `error(v)` builtin produces one via `DynCall`; consumers store
    /// and pass it opaquely. Note this is a BARE `Error<T>` — the
    /// `[T, Error]` Result/Option shape still lowers to
    /// [`Nullable`](GirType::Nullable), not here.
    Error,
}

impl GirType {
    /// True for the "Value-shape" types — those whose JIT/runtime
    /// representation is a two-register `Value` (disc + payload), not
    /// a bare scalar / pointer / arcstr. These share the variant ABI:
    /// passed/returned as two `I64`s, cloned via `graphix_value_clone`,
    /// dropped via `graphix_value_drop`.
    pub fn is_value_shape(&self) -> bool {
        matches!(
            self,
            GirType::Variant(_)
                | GirType::Nullable(_)
                | GirType::DateTime
                | GirType::Duration
                | GirType::Bytes
                | GirType::Map
                | GirType::Error
        )
    }
    /// Convenience for `GirType::Prim(_)` — when callers know they have
    /// a scalar.
    pub fn prim(p: PrimType) -> Self {
        GirType::Prim(p)
    }

    /// Convenience for `GirType::Array(_)` with a primitive element.
    /// For composite elements construct directly via
    /// `GirType::Array(Box::new(inner))`.
    pub fn array(elem: PrimType) -> Self {
        GirType::Array(Box::new(GirType::Prim(elem)))
    }

    /// Returns the inner `PrimType` if this is a scalar; `None`
    /// otherwise. Use to gate ops that only make sense on scalars
    /// (binary arithmetic, casts, etc.).
    pub fn as_prim(&self) -> Option<PrimType> {
        match self {
            GirType::Prim(p) => Some(*p),
            _ => None,
        }
    }

    /// Returns the element type if this is an array; `None` for
    /// scalars / tuples / structs.
    pub fn as_array_elem(&self) -> Option<&GirType> {
        match self {
            GirType::Array(t) => Some(t),
            _ => None,
        }
    }

    /// Backwards-compatible helper: returns the element `PrimType` if
    /// this is `Array<Prim(P)>`; `None` if the element is composite or
    /// the value isn't an array. Use [`Self::as_array_elem`] for the
    /// fully-general accessor; this exists for sites that genuinely
    /// only handle the scalar case (e.g. legacy JIT helper selection).
    pub fn as_array_prim(&self) -> Option<PrimType> {
        self.as_array_elem().and_then(GirType::as_prim)
    }

    /// Returns the per-slot types if this is a tuple; `None` otherwise.
    pub fn as_tuple_elems(&self) -> Option<&[GirType]> {
        match self {
            GirType::Tuple(es) => Some(es),
            _ => None,
        }
    }

    /// Returns the (sorted) field list if this is a struct; `None`
    /// otherwise.
    pub fn as_struct_fields(&self) -> Option<&[(ArcStr, GirType)]> {
        match self {
            GirType::Struct(fs) => Some(fs),
            _ => None,
        }
    }

    /// Returns the case list if this is a variant; `None` otherwise.
    pub fn as_variant_cases(&self) -> Option<&[(ArcStr, Vec<GirType>)]> {
        match self {
            GirType::Variant(cs) => Some(cs),
            _ => None,
        }
    }

    /// Try to derive a [`GirType`] from a fully-resolved Graphix
    /// [`Type`]. Recognises:
    /// - `Type::Primitive` (one variant) → `Prim(_)`
    /// - `Type::Array(elem)` → `Array(_)`
    /// - `Type::Tuple(elems)` → `Tuple(_)` (each elem must be primitive)
    /// - `Type::Struct(fields)` → `Struct(_)` (each field must be primitive)
    /// Returns `None` for any other shape — callers fall back to
    /// non-fused execution.
    pub fn from_type(t: &Type) -> Option<GirType> {
        // Helper: pull out a single `Type::Variant(tag, payloads)`
        // case where every payload converts to a GirType (now
        // possibly composite). Nullary cases (empty payloads) are
        // accepted — they flow at runtime as `Value::String(tag)`,
        // distinct from `Value::Array([tag, ...payloads])` for the
        // with-payload case; the kernel boundary takes `&Value` and
        // dispatches at access sites.
        fn variant_case(
            t: &Type,
        ) -> Option<(ArcStr, Vec<GirType>)> {
            t.with_deref(|resolved| match resolved? {
                Type::Variant(tag, ts) => {
                    let payloads: Option<Vec<GirType>> = ts
                        .iter()
                        .map(|p| GirType::from_type(p))
                        .collect();
                    payloads.map(|p| (tag.clone(), p))
                }
                _ => None,
            })
        }
        // Helper: identify a `Type::Primitive` whose only variant is
        // `Typ::Null`. Used both as a top-level lowering (→
        // `GirType::Null`) and as the marker arm of a
        // `[T, null]` Set (→ `GirType::Nullable(T)`).
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
            // builtins. Lower it to `GirType::Unit` so dispatch can
            // register them — the value is never consumed downstream.
            Type::Bottom => Some(GirType::Unit),
            // String primitives lower to `GirType::String` (lives
            // outside `PrimType` because ArcStr doesn't fit a
            // register).
            Type::Primitive(p)
                if p.contains(netidx_value::Typ::String)
                    && p.iter().count() == 1 =>
            {
                Some(GirType::String)
            }
            // Null primitive lowers to `GirType::Null` — the
            // single-value null shape.
            Type::Primitive(p)
                if p.contains(netidx_value::Typ::Null) && p.iter().count() == 1 =>
            {
                Some(GirType::Null)
            }
            // `datetime` / `duration` — Value-shape types (runtime
            // `Value::DateTime`/`Value::Duration`, both `Arc`-payload).
            Type::Primitive(p)
                if p.contains(netidx_value::Typ::DateTime)
                    && p.iter().count() == 1 =>
            {
                Some(GirType::DateTime)
            }
            Type::Primitive(p)
                if p.contains(netidx_value::Typ::Duration)
                    && p.iter().count() == 1 =>
            {
                Some(GirType::Duration)
            }
            // `bytes` — Value-shape (runtime `Value::Bytes(PBytes)`).
            Type::Primitive(p)
                if p.contains(netidx_value::Typ::Bytes)
                    && p.iter().count() == 1 =>
            {
                Some(GirType::Bytes)
            }
            // `Map<K, V>` — Value-shape (runtime `Value::Map(CMap)`).
            Type::Map { .. } => Some(GirType::Map),
            // Bare `Error<T>` — Value-shape (runtime
            // `Value::Error(Arc<Value>)`), produced by `error(v)`. The
            // `[T, Error]` Result shape is handled by the `Type::Set`
            // arm below (→ `Nullable`); this is only a standalone
            // `Type::Error` (e.g. `error`'s `-> Error<'a>` return).
            Type::Error(_) => Some(GirType::Error),
            // `T | null` collapsed-primitive option shape: a multi-flag
            // primitive carrying `Null` plus exactly one other primitive
            // bit. The typechecker represents `[T, null]` this way
            // (rather than as a `Type::Set`) when `T` is itself a
            // primitive bitflag — e.g. `select x { 0 => null, n => n }`
            // infers `i64 | null`, and a block whose tail has that type
            // surfaces the collapsed form. Same `GirType::Nullable`
            // shape as the `[T, null]` `Set` arm below. Without this,
            // such a value misses the typed-AST fast path in
            // `infer_body_rtype` and falls back to walking the body,
            // which silently de-fuses any block that produces an option
            // value (a predictable-performance cliff).
            Type::Primitive(p)
                if p.contains(netidx_value::Typ::Null) && p.iter().count() == 2 =>
            {
                let other =
                    p.iter().find(|f| *f != netidx_value::Typ::Null)?;
                // The non-null bit becomes the Nullable's inner GirType
                // — a scalar prim, or `String` (which lives outside
                // `PrimType`). Mirrors the `[T, null]` `Set` arm's
                // recursion so both representations agree.
                let inner = if other == netidx_value::Typ::String {
                    GirType::String
                } else {
                    GirType::Prim(PrimType::from_typ(other)?)
                };
                Some(GirType::Nullable(Box::new(inner)))
            }
            Type::Array(inner) => {
                GirType::from_type(inner).map(|t| GirType::Array(Box::new(t)))
            }
            Type::Tuple(elems) => {
                let elems: Option<Vec<GirType>> = elems
                    .iter()
                    .map(|e| GirType::from_type(e))
                    .collect();
                elems.map(GirType::Tuple)
            }
            Type::Variant(_, _) => {
                // Single-case variant (uncommon but legal).
                variant_case(resolved?).map(|c| GirType::Variant(vec![c]))
            }
            Type::Set(members) => {
                // First try `[T, null]` shape (graphix's `Option<T>`):
                // exactly two members, one of which is the null
                // primitive. Order-independent — `[T, null]` and
                // `[null, T]` both lower to `GirType::Nullable(T)`.
                if members.len() == 2 {
                    let (null_idx, t_idx) =
                        match (is_null_primitive(&members[0]), is_null_primitive(&members[1])) {
                            (true, false) => (0, 1),
                            (false, true) => (1, 0),
                            _ => (usize::MAX, usize::MAX),
                        };
                    if null_idx != usize::MAX {
                        if let Some(inner) = GirType::from_type(&members[t_idx]) {
                            return Some(GirType::Nullable(Box::new(inner)));
                        }
                        return None;
                    }
                    // `[T, Error<X>]` shape (graphix's `Result<T, X>`):
                    // exactly two members, one a `Type::Error`. Lowered
                    // to `GirType::Nullable(T)` — the wire shape is
                    // identical (Value-shape two-register) and the
                    // boundary marshals the inner `Value` through
                    // opaquely, so a `Value::Error(...)` flows through
                    // as-is to downstream consumers that store it
                    // (Bind) or read it (Ref). Pattern-matching the
                    // Error case won't dispatch correctly — that's a
                    // documented cliff requiring a real `GirType::Result`
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
                        if let Some(inner) = GirType::from_type(&members[t_idx]) {
                            return Some(GirType::Nullable(Box::new(inner)));
                        }
                        return None;
                    }
                }
                // Variant unions: `[ \`Foo(...), \`Bar(...) ]` parses
                // as a Set of single-Variant types. Match this
                // shape; reject Sets with non-variant members
                // (those are general unions — different fusion
                // story).
                let cases: Option<Vec<(ArcStr, Vec<GirType>)>> = members
                    .iter()
                    .map(|m| variant_case(m))
                    .collect();
                cases.map(GirType::Variant)
            }
            Type::Struct(fields) => {
                let fs: Option<Vec<(ArcStr, GirType)>> = fields
                    .iter()
                    .map(|(n, t)| GirType::from_type(t).map(|p| (n.clone(), p)))
                    .collect();
                fs.map(GirType::Struct)
            }
            // Abstract (opaque) types lower to their concrete impl
            // representation, registered by `node::module::check_sig` in
            // `ABSTRACT_REGISTRY`. The type system keeps them opaque
            // everywhere else; fusion peeks so abstract-typed values flow
            // through kernels. Parameterized abstracts (`Box<'a>`) would
            // need param substitution into the concrete — not handled
            // yet, so they fall through to `None`.
            Type::Abstract { id, params } if params.is_empty() => {
                let concrete = ABSTRACT_REGISTRY.read().get(id).cloned();
                concrete.and_then(|c| GirType::from_type(&c))
            }
            other => PrimType::from_type(other).map(GirType::Prim),
        })
    }
}

impl From<PrimType> for GirType {
    fn from(p: PrimType) -> Self {
        GirType::Prim(p)
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

/// A primitive-typed compile-time constant. Used by `GirOp::Const` and
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
/// any [`GirType`] — primitive or nested composite. Runtime value is
/// `&ValArray`; for primitive elements the JIT loads via
/// `arr.get_unchecked::<T>(i)`, for composite elements via a
/// `*mut Value` accessor.
#[derive(Debug, Clone)]
pub struct ArrayInput {
    pub name: ArcStr,
    pub elem: GirType,
    pub bind_id: Option<BindId>,
}

/// A tuple value passed as a kernel parameter. Per-slot types can be
/// any [`GirType`] (nested tuples / structs / variants / arrays
/// allowed). Same `&ValArray` runtime boundary as [`ArrayInput`].
#[derive(Debug, Clone)]
pub struct TupleInput {
    pub name: ArcStr,
    pub elems: Vec<GirType>,
    pub bind_id: Option<BindId>,
}

/// A struct value passed as a kernel parameter. Field types can be
/// any [`GirType`]. Same `&ValArray` boundary, with fields stored at
/// compile-time-known sorted-by-name positions.
#[derive(Debug, Clone)]
pub struct StructInput {
    pub name: ArcStr,
    pub fields: Vec<(ArcStr, GirType)>,
    pub bind_id: Option<BindId>,
}

/// A variant value passed as a kernel parameter. Payload types per
/// case can be any [`GirType`]. Same `&ValArray` boundary as
/// tuples/structs, but the slot at index 0 is the tag string (an
/// interned `ArcStr`) and payloads start at index 1. `cases`
/// enumerates the legal `(tag, payload_types)` shapes — at runtime
/// exactly one of these is active per value.
#[derive(Debug, Clone)]
pub struct VariantInput {
    pub name: ArcStr,
    pub cases: Vec<(ArcStr, Vec<GirType>)>,
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
    pub elem: GirType,
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

/// A Value-shape kernel let-binding whose type is `DateTime` or
/// `Duration`. Unlike `NullableInput` (which stores only the inner
/// `T` of `[T, null]`), this carries the full `GirType` so a `Ref`
/// read resolves to the correct `DateTime`/`Duration` type. Runtime
/// representation rides the interpreter's `nullables` Value slot
/// (a name→Value map); the JIT uses the `nullables` `ValueVar` slot.
#[derive(Debug, Clone)]
pub struct ValueInput {
    pub name: ArcStr,
    pub typ: GirType,
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
    /// String — runtime representation is an owned `ArcStr` (one
    /// machine word). Body signature receives the ArcStr; tail-call
    /// rebinding clones (refcount-bump) into the shadowed local.
    String,
    /// Bare value-shape (`DateTime`/`Duration`/`Bytes`) — runtime
    /// representation is `Value` (two words). Routed to
    /// `env.nullables` like `Nullable`, but re-wrapped to the slot's
    /// declared type on read.
    Value,
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
    pub arg_types: Vec<GirType>,
    /// Return type — scalar or array.
    pub return_type: GirType,
}

/// A compile-time-known primitive expression bound to a Graphix-level
/// name. Used to inline references to outer-scope bindings whose value
/// is itself a constant or an expression over already-known constants.
/// Stored as a [`GirExpr`] (rather than a precomputed string) so each
/// inline pays its own way through both backends; rustc constant-folds
/// the inlined Rust source, and the CLIF backend evaluates the same
/// SSA tree.
#[derive(Debug, Clone)]
pub struct KnownConst {
    pub expr: GirExpr,
}

impl KnownConst {
    pub fn typ(&self) -> GirType {
        self.expr.typ.clone()
    }
}

// ─── GIR core ────────────────────────────────────────────────────

/// A typed expression node. `typ` is the Graphix type the expression
/// evaluates to (scalar primitive or flat array of primitives); `op`
/// is the actual operation.
#[derive(Debug, Clone)]
pub struct GirExpr {
    pub op: GirOp,
    pub typ: GirType,
}

#[derive(Debug, Clone)]
pub enum GirOp {
    /// A primitive constant.
    Const(ConstVal),
    /// A Value-shape constant — a `datetime`/`duration` literal whose
    /// runtime form is `Value::DateTime`/`Value::Duration` (an `Arc`
    /// payload that doesn't fit a `ConstVal` register). Result type is
    /// [`GirType::DateTime`] / [`GirType::Duration`]. The interpreter
    /// clones the `Value`; the JIT interns it in a per-kernel value-
    /// constants table (like `ConstStr`'s string table) and emits the
    /// `(disc, payload)` words, cloning the `Arc` on use.
    ConstValue(Value),
    /// Arithmetic on Value-shape operands — `datetime ± duration`,
    /// `duration ± duration`, `duration {*,/} <number>`,
    /// `<number> * duration`. Either operand may be scalar (promoted
    /// to its `Value` form) or already Value-shape; the result is
    /// always Value-shape ([`GirType::DateTime`]/[`GirType::Duration`]).
    /// Both backends compute via netidx's `impl {Add,Sub,Mul,Div} for
    /// Value`, so the result is byte-identical to the non-fused arith
    /// node. Distinct from [`GirOp::Bin`] (scalar, register math).
    ValueArith {
        op: BinOp,
        lhs: Box<GirExpr>,
        rhs: Box<GirExpr>,
    },
    /// Equality (`==` / `!=`) on two same-typed Value-shape operands
    /// (Map/Variant/Nullable/Bytes/DateTime/Duration), where neither is
    /// a primitive (those use the register-`Cmp` path). Both backends
    /// compare via netidx's `impl PartialEq for Value`. Result `Bool`.
    ValueEq {
        /// `true` for `!=`, `false` for `==`.
        ne: bool,
        lhs: Box<GirExpr>,
        rhs: Box<GirExpr>,
    },
    /// A string-typed constant. Used by `emit_expr`'s
    /// `ExprKind::Constant(Value::String)` arm and by `Concat`'s
    /// literal segments. Result type is [`GirType::String`].
    /// Lives outside [`ConstVal`] because [`ConstVal`] is `Copy` and
    /// `ArcStr` isn't — keeping `ConstVal` `Copy` matters for the
    /// `gir_interp` register paths.
    ConstStr(ArcStr),
    /// Concatenation of string-renderable parts — the GIR form of
    /// `ExprKind::StringInterpolate` (`"x=[x]"`). Each child must
    /// be [`GirType::String`] or [`GirType::Prim`]; prim children
    /// are formatted via the netidx `Value` `Display` (the same
    /// renderer `StringInterpolate` uses at non-fusion time).
    /// Result type is [`GirType::String`].
    Concat(Vec<GirExpr>),
    /// The literal `null`. Result type is [`GirType::Null`]. Runtime
    /// representation is `Value::Null`. The JIT can't lower this op
    /// today — kernels containing it route to the interpreter via
    /// `kernel_contains_null` (same fallback shape as `String`).
    ConstNull,
    /// Test whether a nullable value is `null`. The operand must be
    /// [`GirType::Null`] or [`GirType::Nullable`]; result is
    /// `Bool`. Emitted as the condition for `select` arms whose
    /// type-predicate is `null`. JIT-gated by `kernel_contains_null`.
    IsNull(Box<GirExpr>),
    /// `?` (Qop) applied to a Nullable value. The operand must be
    /// [`GirType::Nullable<T>`]. At runtime:
    ///   * If the inner Value is `Value::Error(...)` (Result error
    ///     case), signal pending — the wrapper's pending check
    ///     short-circuits the kernel and returns `None`, mirroring
    ///     the non-fused `Qop::update`'s "return None on Error" path.
    ///     A future iteration will additionally route the error to
    ///     the nearest catch handler's BindId; today it just drops.
    ///   * Otherwise extract `T` from the Value.
    ///
    /// Result type is the inner `T` (whatever `Nullable<T>`'s inner
    /// is). The JIT codegen reads the operand's `(disc, payload)`
    /// pair, branches on the error-disc check, and extracts `T` via
    /// the same payload-to-prim/string/composite path the kernel
    /// boundary uses.
    QopUnwrap {
        inner: Box<GirExpr>,
        success_typ: GirType,
    },
    /// Reference to a function arg or let-bound local. Identified by
    /// name; the Rust backend uses the name directly, the CLIF backend
    /// looks it up in its `Variable` table.
    Local(ArcStr),
    /// Binary arithmetic. Constructor enforces `lhs.typ == rhs.typ`
    /// and `lhs.typ.is_numeric()`; result type is the operand type.
    Bin {
        op: BinOp,
        lhs: Box<GirExpr>,
        rhs: Box<GirExpr>,
    },
    /// Comparison. `lhs.typ == rhs.typ`; result type is `Bool`.
    Cmp {
        op: CmpOp,
        lhs: Box<GirExpr>,
        rhs: Box<GirExpr>,
    },
    /// Boolean and/or. Both operands and the result are `Bool`.
    BoolBin {
        op: BoolOp,
        lhs: Box<GirExpr>,
        rhs: Box<GirExpr>,
    },
    /// Boolean negation. Operand and result are `Bool`.
    Not(Box<GirExpr>),
    /// Primitive cast (Rust `as`). Excludes bool↔integer (not
    /// supported yet — Rust uses match for those, not `as`).
    Cast {
        inner: Box<GirExpr>,
        target: PrimType,
    },
    /// Direct call to an already-fused function (looked up by name).
    /// All-positional arguments, types must match the function's
    /// declared signature.
    Call {
        fn_name: ArcStr,
        args: Vec<GirExpr>,
    },
    /// Late-bound call into a function value the kernel doesn't know
    /// statically. Resolved at runtime by the interpreter, which
    /// reads the `fn_index`-th slot of the kernel's fn-args table
    /// (populated by [`crate::gir_interp::GirNode`] from its incoming
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
        args: Vec<GirExpr>,
        /// Argument types parallel to `args`. Used by the dispatch
        /// machinery to encode each arg into a `netidx::Value` and
        /// (in the JIT path) to pick the right buf-push helper —
        /// scalars marshal as primitives, composites pass as
        /// refcount-bumped clones of the caller's owned pointer.
        arg_types: Vec<GirType>,
        /// Return type. Equals the wrapping `GirExpr.typ`; carried
        /// here for symmetry with `arg_types` and so the JIT can
        /// pick the right `cast_u64_to_*` / `Box::from_raw` decode
        /// path without re-walking `GirExpr.typ`.
        return_type: GirType,
    },
    /// Expression-form block: `{ let a = ..; let b = ..; tail }`.
    /// All non-tail items are let-bindings; the tail provides the
    /// block's value.
    Block {
        lets: Vec<Let>,
        tail: Box<GirExpr>,
    },
    /// Expression-form if-chain. Each entry is `(condition, value)`;
    /// the last entry's condition may be `None` for an unconditional
    /// `else`. If no entry is unconditional, lowering inserts an
    /// `unreachable!()` tail (typecheck should make this unreachable
    /// in practice).
    IfChain {
        arms: Vec<(Option<GirExpr>, GirExpr)>,
    },
    /// Length of a flat array parameter, as `u64`. `name` must
    /// resolve in `GirKernel.array_params`. Lowers to `arr.len() as
    /// u64` in the Rust backend; the GIR interpreter reads
    /// `array_inputs[i].len()`.
    ArrayLen {
        name: ArcStr,
    },
    /// Indexed read of a flat array parameter. `name` resolves in
    /// `array_params`; `idx` evaluates to an integer index. Result
    /// type is the wrapping `GirExpr.typ`, which must equal the
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
        idx: Box<GirExpr>,
    },
    /// `bytes[i]` — index a `bytes` value, bounds-checked + negative-from-
    /// end (shared `node::array::bytes_index`). Operand is value-shape
    /// (Bytes); result is `Nullable<u8>` (`[u8, Error<…>]`).
    BytesIndex {
        bytes: Box<GirExpr>,
        idx: Box<GirExpr>,
    },
    /// Map access `m{key}`. `map` is a `GirType::Map` value-shape
    /// operand, `key` any value-bearing operand; the result is
    /// `Nullable<V>` carrying the looked-up value or a
    /// `map key not found` error (the same `[V, Error]` shape the
    /// node-walk `MapRef` produces). Both operands are consumed by
    /// the lookup.
    MapRef {
        map: Box<GirExpr>,
        key: Box<GirExpr>,
    },
    /// Array/bytes slice `a[i..j]` / `a[i..]` / `a[..j]` / `a[..]`.
    /// `source` is a `GirType::Array`/`Bytes` value-shape operand;
    /// `start`/`end` are optional scalar (integer) bounds. The result
    /// is `Nullable<source>` — the sub-array/sub-bytes or an
    /// out-of-bounds / negative-bound error (the `[T, Error]` shape the
    /// node-walk `ArraySlice` produces).
    ArraySlice {
        source: Box<GirExpr>,
        start: Option<Box<GirExpr>>,
        end: Option<Box<GirExpr>>,
    },
    /// Reduce a flat array to a scalar via a same-cycle fold. Lowers
    /// `array::fold(arr, init, |acc, x| body)` when the callback
    /// fuses to scalar GIR. `array` resolves in `array_params`;
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
        /// Element type of `array` — `Prim` for scalar elements, or a
        /// composite (`Tuple`/…) for `Array<(k,v)>`. Copied here at
        /// construction time so the backend doesn't need to walk the
        /// body looking for an `ArrayGet` against the same name (the
        /// body usually reads the bound `elem_local`). The loop binds
        /// `elem_local` as a scalar local for `Prim`, a composite
        /// otherwise (same split as `ArrayMap`).
        elem_typ: GirType,
        init: Box<GirExpr>,
        acc_local: ArcStr,
        elem_local: ArcStr,
        body: Box<GirExpr>,
    },
    /// Build a flat array of `n` elements by applying a fused body to
    /// each index. Lowers `array::init(n, |idx| body)`. The body sees
    /// `idx_local: i64` as a local; its result type becomes the
    /// array's element type. Result `GirExpr.typ` is
    /// `GirType::Array(elem_typ)`.
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
        n: Box<GirExpr>,
        idx_local: ArcStr,
        // The output element type is `body.typ` (any GirType — prim or
        // composite); no separate field needed.
        body: Box<GirExpr>,
    },
    /// Build a flat array by applying a fused body to each element of
    /// an input array. Lowers `array::map(arr, |x| body)`. Result
    /// `GirExpr.typ` is `GirType::Array(result_elem)` where
    /// `result_elem` is `body.typ`'s primitive variant.
    ///
    /// Same one-allocation cost profile as `ArrayInit`; element loads
    /// go through the unsafe `ValArray::get_unchecked` fast path.
    ArrayMap {
        array: ArcStr,
        /// Element type of the *input* `array` — `Prim` for scalar
        /// elements, or a composite (`Tuple`/`Struct`/…) for
        /// `Array<(k, v)>`-style elements. The loop binds `elem_local`
        /// as a scalar local for `Prim` and a composite local
        /// otherwise.
        in_elem: GirType,
        elem_local: ArcStr,
        // Output element type is `body.typ` (any GirType — prim or
        // composite); no separate field needed.
        body: Box<GirExpr>,
    },
    /// Build a flat array by retaining each element of an input array
    /// for which a fused predicate body returns true. Lowers
    /// `array::filter(arr, |x| pred)`. Result `GirExpr.typ` is
    /// `GirType::Array(elem)` — same element type as the input
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
        /// Element type — `Prim` for scalar elements, composite for
        /// `Array<(k,v)>`. The loop binds `elem_local` scalar/composite
        /// (same split as `ArrayMap`); on a `keep`, the *original*
        /// element is pushed to the output.
        elem: GirType,
        elem_local: ArcStr,
        /// Predicate body — must emit as `GirType::Prim(Bool)`.
        predicate: Box<GirExpr>,
    },
    /// Return the first element of `array` for which a fused predicate
    /// body returns true, as an option (`null` when none match). Lowers
    /// `array::find(arr, |x| pred)`. Result `GirExpr.typ` is
    /// `GirType::Nullable(elem)` — the matched element or `null`.
    ArrayFind {
        array: ArcStr,
        /// `Prim` for scalar elements, composite for `Array<(k,v)>`;
        /// bound the same way as `ArrayMap`. The result is
        /// `Nullable<elem>` — the matched element (scalar or composite)
        /// or `null`.
        elem: GirType,
        elem_local: ArcStr,
        /// Predicate body — must emit as `GirType::Prim(Bool)`.
        predicate: Box<GirExpr>,
    },
    /// Build a flat array from the non-`null` results of a fused body
    /// returning `[out_elem, null]` per element. Lowers
    /// `array::filter_map(arr, |x| body)`. Result `GirExpr.typ` is
    /// `GirType::Array(out_elem)`; like `ArrayFilter`, output length is
    /// dynamic so the lowering collects into a `ValArray`.
    ArrayFilterMap {
        array: ArcStr,
        in_elem: PrimType,
        elem_local: ArcStr,
        /// Element type of the *output* — the inner type of the body's
        /// `Nullable` result.
        out_elem: PrimType,
        /// Body — must emit as `GirType::Nullable(out_elem)`.
        body: Box<GirExpr>,
    },
    /// `array::find_map` — like `ArrayFilterMap` (body yields
    /// `Nullable<out>`) but **early-exits** on the first non-null body
    /// result and returns it (the whole op's result is that
    /// `Nullable<out>`, or `null` if no element mapped non-null).
    /// `in_elem` may be composite (`Array<(k,v)>`), bound the same way
    /// as `ArrayMap` (scalar local vs `arrays` slot).
    ArrayFindMap {
        array: ArcStr,
        in_elem: GirType,
        elem_local: ArcStr,
        /// Body — must emit as `GirType::Nullable(out)`; the op's
        /// result type is the same `Nullable<out>`.
        body: Box<GirExpr>,
    },
    /// Build a flat array by concatenating the per-element `Array<out_elem>`
    /// results of a fused body. Lowers `array::flat_map(arr, |x| body)`
    /// where the body produces an array. Result `GirExpr.typ` is
    /// `GirType::Array(out_elem)`; output length is dynamic.
    ArrayFlatMap {
        array: ArcStr,
        /// Element type — `Prim` for scalar elements, composite for
        /// `Array<(k,v)>`; bound the same way as `ArrayMap`.
        in_elem: GirType,
        elem_local: ArcStr,
        /// Element type of the *output* — the element type of the body's
        /// `Array` result.
        out_elem: PrimType,
        /// Body — must emit as `GirType::Array(out_elem)`.
        body: Box<GirExpr>,
    },
    /// Read tuple slot `idx` of `name` (a tuple kernel parameter).
    /// Result `GirExpr.typ` matches `elem_typ`. For `Prim` slots the
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
        elem_typ: GirType,
    },
    /// Build a tuple from per-slot expressions. Each `fields[i]`
    /// emits to a GIR expression of type `elem_types[i]`; the
    /// interpreter / JIT wrap primitives in `Value::<variant>(x)`
    /// and feed composite values (tuples, structs, variants, arrays,
    /// nullables, strings) through the appropriate boundary helper.
    /// Result type is `GirType::Tuple(elem_types)`.
    TupleNew {
        fields: Vec<GirExpr>,
        elem_types: Vec<GirType>,
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
        elem_typ: GirType,
    },
    /// Build a struct from per-field expressions. `sorted_fields` is
    /// the canonical alphabetical order (graphix's runtime layout).
    /// Lowers identically to `TupleNew` — the runtime doesn't
    /// distinguish tuples from structs at the ValArray level. Field
    /// types in `sorted_types` may be composite (mirroring TupleNew's
    /// generalisation).
    StructNew {
        sorted_fields: Vec<(ArcStr, GirExpr)>,
        sorted_types: Vec<(ArcStr, GirType)>,
    },
    /// Test whether a variant param's runtime tag matches a
    /// compile-time-known tag string. Reads `name`'s slot 0 (the
    /// interned tag `ArcStr`), compares against `expected_tag` via
    /// byte equality. Result type is `GirType::Prim(Bool)`. Used
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
    /// Result `GirExpr.typ` is `GirType::Variant([(tag, payload_types)])`
    /// — exactly one case (the one being constructed). Payload types
    /// may be composite (mirroring TupleNew/StructNew's
    /// generalisation).
    VariantNew {
        tag: ArcStr,
        payloads: Vec<GirExpr>,
        payload_types: Vec<GirType>,
    },
}

/// A single let-binding. Used in both [`GirOp::Block`] (expression-form
/// blocks) and [`GirStmt::Let`] (function bodies).
#[derive(Debug, Clone)]
pub struct Let {
    pub local: ArcStr,
    pub value: GirExpr,
}

// ─── Statement-form (function bodies) ────────────────────────────

#[derive(Debug, Clone)]
pub enum GirStmt {
    /// `let mut <local> = <value>;` — introduces a new local visible
    /// to subsequent statements.
    Let(Let),
    /// Function exit with a value.
    Return(GirExpr),
    /// Self-tail-call: assign new arg values to the loop variables and
    /// continue the surrounding `loop {}`. Only legal when the kernel
    /// has `has_tail_loop = true`. The backend chooses how to spell
    /// the temp-then-assign sequence (Rust uses temps; CLIF uses SSA).
    /// Args are stored in declaration order.
    TailCall { args: Vec<GirExpr> },
    /// Statement-form select chain. Each arm is conditional or
    /// unconditional; arms after an unconditional arm are dead. If
    /// no arm is unconditional, lowering inserts a fallthrough
    /// `unreachable!()` — typecheck should forbid this in practice.
    Select { arms: Vec<SelectArm> },
    /// Evaluate `expr` for its side effect, discard the result.
    /// Used for `GirType::Unit` calls (`println`, `dbg`, `log`, …)
    /// and any other expression whose value the program doesn't
    /// consume. The interpreter / JIT evaluates `expr` (firing any
    /// `GirOp::DynCall`s or producer-op effects inside it) and
    /// throws away the `EvalResult`.
    Discard(GirExpr),
}

#[derive(Debug, Clone)]
pub struct SelectArm {
    /// `None` means unconditional (final `else`).
    pub cond: Option<GirExpr>,
    pub body: Vec<GirStmt>,
}

/// A complete kernel — one function's worth.
#[derive(Debug, Clone)]
pub struct GirKernel {
    /// Graphix-level function name (used for self-recursion detection
    /// and for naming the emitted Rust free function).
    pub fn_name: ArcStr,
    /// Primitive parameters in declaration order. Each is also visible
    /// as a local in the body.
    pub params: Vec<Input>,
    /// Function-typed parameters in declaration order. Distinct from
    /// `params` because the interpreter holds them in a separate
    /// fn-args table (the value is a `LambdaDef`, not a primitive).
    /// `GirOp::DynCall { fn_index }` indexes into this table.
    pub fn_params: Vec<FnParam>,
    /// Array-typed parameters. Sibling to `params`; kept separate so
    /// the scalar pipeline doesn't need to know about array types.
    /// `GirOp::ArrayLen` / `GirOp::ArrayGet` reference these by name.
    pub array_params: Vec<ArrayInput>,
    /// Tuple-typed parameters. `GirOp::TupleGet` references these by
    /// name + sorted index.
    pub tuple_params: Vec<TupleInput>,
    /// Struct-typed parameters. `GirOp::StructGet` references these
    /// by name + field-name (resolved to sorted index at lowering).
    pub struct_params: Vec<StructInput>,
    /// Variant-typed parameters. `GirOp::VariantTagEq` / VariantPayload
    /// reference these by name; the runtime layout puts the tag string
    /// at slot 0 and payloads at slots 1..N.
    pub variant_params: Vec<VariantInput>,
    /// `[T, null]` option-shape parameters. `GirOp::IsNull` and Local
    /// reads against names here yield the slot's `Value` (either
    /// `Value::Null` or `T`'s runtime form). Stored separately from
    /// `variant_params` even though both use `Value` at runtime —
    /// the consumer ops and the env slot list differ.
    pub nullable_params: Vec<NullableInput>,
    /// String-typed parameters. Runtime representation is a single
    /// machine word holding an `ArcStr` thin pointer; the kernel
    /// clones (refcount-bump) on entry and reads via `env.strings`.
    pub string_params: Vec<StringInput>,
    /// Bare value-shape parameters — `DateTime` / `Duration` /
    /// `Bytes`. Two-word `Value` wire shape like variant/nullable,
    /// but carries the full `GirType` (no inner `elem` indirection)
    /// so a `Local` read re-wraps to the right type. Rides the same
    /// `env.nullables` Value slot as those types' locals.
    pub value_params: Vec<ValueInput>,
    /// Per-source-position metadata so the tail-call renderer can
    /// assign each new arg value to the right destination
    /// regardless of which slot list (scalar / array / tuple /
    /// struct) the destination lives in. Length matches the
    /// lambda's source argspec order. Only populated when
    /// `has_tail_loop` is true; otherwise empty.
    pub tail_call_slots: Vec<TailCallSlot>,
    pub return_type: GirType,
    /// True iff the body contains a self-tail-call. Backends wrap the
    /// body in `loop { ... }` (Rust) or a back-edge to the entry block
    /// (CLIF) accordingly.
    pub has_tail_loop: bool,
    pub body: Vec<GirStmt>,
}

// ─── Kernel ABI layout — single source of truth ──────────────────
//
// The kernel calling convention is "kind-grouped": parameters are
// laid out at the JIT/runtime boundary as all `Scalar` params first
// (each one machine word at the prim's CLIF type), then all
// pointer-shaped composite params (array/tuple/struct, each one
// machine word holding a `*ValArray`), then all two-word `Value`
// params (variant/nullable, each two words: disc + payload). Returns
// follow an analogous rule.
//
// Every ABI site — the two JIT signature builders, the uniform
// wrapper's slot unpacker, the kernel entry-block binder, and the
// runtime arg packer — derives its ordering and wire footprint from
// `GirKernel::abi_params` / `abi_return` / `abi_param_wire_slots`
// rather than hand-walking the per-kind `*_params` lists. Keeping the
// order in one place is what makes a mistake a compile error (a new
// `AbiParamKind` variant forces every match) instead of a silent
// bit-reinterpretation at one drifted site. Kind-grouped (rather than
// source-positional) is retained deliberately: it matches the
// kernel's existing entry ABI and keeps the per-kind clone-on-entry /
// drop-on-exit codegen a clean per-list walk. Foreign callers, if
// ever wanted, get a separate generated C-ABI wrapper — they don't
// constrain this internal convention.

/// The kind of a single kernel parameter, in enough detail to drive
/// both the wire encoding (how many machine words, what CLIF type)
/// and the env-slot binding (which `bind_*` the entry code calls).
#[derive(Debug, Clone, Copy)]
pub enum AbiParamKind {
    /// One machine word at the prim's CLIF type. Binds via `env.bind`.
    Scalar(PrimType),
    /// One machine word holding a `*ValArray`; binds via
    /// `env.bind_composite`. Array/Tuple/Struct share this wire shape
    /// but stay distinct so the runtime packer can pull each from the
    /// right `*_args` vector.
    Array,
    Tuple,
    Struct,
    /// Two machine words (disc, payload) of a `repr(u64)` `Value`.
    /// Binds via `env.bind_variant`.
    Variant,
    /// Two machine words (disc, payload). Binds via `env.bind_nullable`
    /// — same wire shape as `Variant`, different env slot list.
    Nullable,
    /// One machine word holding an `ArcStr` thin pointer. Binds via
    /// `env.bind_string` (after a refcount-bump clone on entry).
    String,
    /// Two machine words (disc, payload) of a bare value-shape
    /// `Value` — `DateTime` / `Duration` / `Bytes`. Same wire shape
    /// as `Nullable`/`Variant`; binds into the `env.nullables` Value
    /// slot (re-wrapped to the right type at `Local`-read time).
    Value,
}

impl AbiParamKind {
    /// Number of machine-word (`u64`) slots this param occupies on the
    /// wire. The single home of the "Variant/Nullable/Value are two
    /// words, everything else is one" rule.
    pub fn wire_words(self) -> usize {
        match self {
            AbiParamKind::Variant
            | AbiParamKind::Nullable
            | AbiParamKind::Value => 2,
            _ => 1,
        }
    }
}

/// One kernel parameter at the ABI boundary, in kind-grouped order.
/// `wire_slot` is the starting `u64` slot index in the flat boundary
/// layout; the param spans `kind.wire_words()` consecutive slots.
#[derive(Debug, Clone, Copy)]
pub struct AbiParamDesc<'a> {
    pub name: &'a ArcStr,
    pub kind: AbiParamKind,
    pub wire_slot: usize,
}

/// The wire shape of a kernel's return value.
#[derive(Debug, Clone, Copy)]
pub enum AbiReturn {
    /// One return value. `prim` is set for scalar returns (so the
    /// signature picks the narrow CLIF type) and `None` for
    /// pointer/string/unit returns (one machine word).
    One { prim: Option<PrimType> },
    /// Two return values (disc, payload) — variant/nullable `Value`.
    Two,
}

impl GirKernel {
    /// Iterate the kernel's parameters in canonical kind-grouped ABI
    /// order, each tagged with its wire-slot offset. This is THE
    /// definition of the parameter calling convention — every ABI site
    /// consumes it rather than re-deriving the order.
    pub fn abi_params(&self) -> impl Iterator<Item = AbiParamDesc<'_>> {
        let scalars =
            self.params.iter().map(|p| (&p.name, AbiParamKind::Scalar(p.prim)));
        let arrays =
            self.array_params.iter().map(|p| (&p.name, AbiParamKind::Array));
        let tuples =
            self.tuple_params.iter().map(|p| (&p.name, AbiParamKind::Tuple));
        let structs =
            self.struct_params.iter().map(|p| (&p.name, AbiParamKind::Struct));
        let variants = self
            .variant_params
            .iter()
            .map(|p| (&p.name, AbiParamKind::Variant));
        let nullables = self
            .nullable_params
            .iter()
            .map(|p| (&p.name, AbiParamKind::Nullable));
        let strings =
            self.string_params.iter().map(|p| (&p.name, AbiParamKind::String));
        let values =
            self.value_params.iter().map(|p| (&p.name, AbiParamKind::Value));
        scalars
            .chain(arrays)
            .chain(tuples)
            .chain(structs)
            .chain(strings)
            .chain(variants)
            .chain(nullables)
            .chain(values)
            .scan(0usize, |off, (name, kind)| {
                let wire_slot = *off;
                *off += kind.wire_words();
                Some(AbiParamDesc { name, kind, wire_slot })
            })
    }

    /// Total number of `u64` wire slots the parameter list occupies.
    /// The single home for the `n_pointer_slots + n_value_slots`
    /// arithmetic that used to be copy-pasted at every ABI site.
    pub fn abi_param_wire_slots(&self) -> usize {
        self.params.len()
            + self.array_params.len()
            + self.tuple_params.len()
            + self.struct_params.len()
            + self.string_params.len()
            + 2 * (self.variant_params.len()
                + self.nullable_params.len()
                + self.value_params.len())
    }

    /// True if this kernel does no computation: its body is a single
    /// `Return` of a bare `Local` read. Such a kernel just forwards one
    /// of its inputs (a region binding / lifted async value) unchanged,
    /// so fusing it wraps a zero-compute kernel in dispatch overhead —
    /// the runtime `Ref` feeder already produces that value. `fuse()`
    /// skips the splice for these (the original nodes stay live and
    /// carry the value). A body with any actual op — even one extra
    /// `Let` before the `Return` — is not trivial and fuses normally.
    pub fn is_identity_passthrough(&self) -> bool {
        self.body.len() == 1
            && matches!(
                &self.body[0],
                GirStmt::Return(GirExpr { op: GirOp::Local(_), .. })
            )
    }

    /// The wire shape of this kernel's return value, or `None` for the
    /// invalid bare-`Null` return (fusion must widen to `Nullable<T>`
    /// before producing). Callers translate `None` into their own
    /// error with context.
    pub fn abi_return(&self) -> Option<AbiReturn> {
        match &self.return_type {
            GirType::Prim(p) => Some(AbiReturn::One { prim: Some(*p) }),
            GirType::Array(_)
            | GirType::Tuple(_)
            | GirType::Struct(_)
            | GirType::Unit
            | GirType::String => Some(AbiReturn::One { prim: None }),
            GirType::Variant(_)
            | GirType::Nullable(_)
            | GirType::DateTime
            | GirType::Duration
            | GirType::Bytes | GirType::Map | GirType::Error => Some(AbiReturn::Two),
            GirType::Null => None,
        }
    }
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
    pub arg_types: Vec<GirType>,
    /// Return type of the callee. May be scalar or composite; the
    /// dispatcher and JIT codegen branch on the kind to pick the
    /// right encode/decode path.
    pub return_type: GirType,
}

/// How a [`FnParam`]'s callable is sourced at dispatch time.
#[derive(Debug, Clone)]
pub enum FnSource {
    /// HOF argument: the kernel's caller passes a `LambdaDef` value
    /// at position `arg_pos` (zero-based, in the lambda's source-
    /// order argument list, mixed with primitive args). GirNode's
    /// runtime extracts it from the incoming `from` slice.
    Param { arg_pos: u32 },
    /// Statically-resolved user binding: the `LambdaDef` lives in
    /// `ctx.cached[bind_id]` (or, for unstable bindings,
    /// `event.variables[bind_id]`). Set when fusion can't fuse the
    /// callee inline (its body uses unsupported constructs) but can
    /// still call it via Apply::update.
    Binding { bind_id: crate::BindId },
    /// Sync builtin call. Resolved at `GirNode::new` time by looking
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
        /// `GirNode::pre_bind_builtin` to look up the lambda's
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

// ─── Constructors that enforce GIR invariants ────────────────────
//
// These are the canonical way to build typed nodes — they enforce the
// type invariants every backend assumes (matching operand types for
// arithmetic, both operands bool for boolean ops, etc.) so a malformed
// node can't sneak in. Returning `Option<GirExpr>` mirrors the existing
// fusion-pass shape: a None constructor result aborts the whole parent
// fusion attempt.

pub fn arith(lhs: GirExpr, rhs: GirExpr, op: BinOp) -> Option<GirExpr> {
    let lp = lhs.typ.as_prim()?;
    let rp = rhs.typ.as_prim()?;
    if lp != rp || !lp.is_numeric() {
        return None;
    }
    Some(GirExpr {
        op: GirOp::Bin { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
        typ: GirType::Prim(lp),
    })
}

pub fn cmp(lhs: GirExpr, rhs: GirExpr, op: CmpOp) -> Option<GirExpr> {
    if lhs.typ != rhs.typ {
        return None;
    }
    if lhs.typ.as_prim().is_some() {
        return Some(GirExpr {
            op: GirOp::Cmp { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
            typ: GirType::Prim(PrimType::Bool),
        });
    }
    // Non-primitive `==` / `!=` (String, composite Array/Tuple/Struct, or
    // value-shape Map/Variant/Nullable/Bytes/DateTime/Duration) compares
    // via `Value`'s PartialEq. Ordering operators on non-prim types
    // aren't lowered, and Unit/Null have no comparable runtime form.
    if matches!(op, CmpOp::Eq | CmpOp::Ne)
        && !matches!(lhs.typ, GirType::Unit | GirType::Null)
    {
        return Some(GirExpr {
            op: GirOp::ValueEq {
                ne: matches!(op, CmpOp::Ne),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            typ: GirType::Prim(PrimType::Bool),
        });
    }
    None
}

pub fn bool_op(lhs: GirExpr, rhs: GirExpr, op: BoolOp) -> Option<GirExpr> {
    let bool_t = GirType::Prim(PrimType::Bool);
    if lhs.typ != bool_t || rhs.typ != bool_t {
        return None;
    }
    Some(GirExpr {
        op: GirOp::BoolBin { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
        typ: bool_t,
    })
}

pub fn not(inner: GirExpr) -> Option<GirExpr> {
    let bool_t = GirType::Prim(PrimType::Bool);
    if inner.typ != bool_t {
        return None;
    }
    Some(GirExpr { op: GirOp::Not(Box::new(inner)), typ: bool_t })
}

pub fn cast(inner: GirExpr, target: PrimType) -> Option<GirExpr> {
    // Rust's `as` doesn't accept bool↔integer in either direction;
    // those need a `match`. Until the emitter grows that, refuse.
    let inner_p = inner.typ.as_prim()?;
    if target == PrimType::Bool || inner_p == PrimType::Bool {
        return None;
    }
    Some(GirExpr {
        op: GirOp::Cast { inner: Box::new(inner), target },
        typ: GirType::Prim(target),
    })
}

pub fn const_expr(c: ConstVal) -> GirExpr {
    let typ = GirType::Prim(c.typ());
    GirExpr { op: GirOp::Const(c), typ }
}

pub fn local(name: ArcStr, prim: PrimType) -> GirExpr {
    GirExpr { op: GirOp::Local(name), typ: GirType::Prim(prim) }
}

/// Construct a [`GirExpr`] referring to an array-typed local. The
/// element type goes on `GirType::Array(elem)` so downstream ops
/// (ArrayMap, ArrayFold) can read it back without a separate sidecar.
pub fn local_array(name: ArcStr, elem: PrimType) -> GirExpr {
    GirExpr {
        op: GirOp::Local(name),
        typ: GirType::Array(Box::new(GirType::Prim(elem))),
    }
}

/// True if the kernel contains a [`GirOp::Call`] anywhere — i.e.
/// non-tail self-recursion or cross-kernel calls. The interpreter and
/// JIT v1 do not handle these. Callers wiring GIR through the runtime
/// path use this to refuse to instantiate a `GirNode` that would panic
/// on first call. Lifts in M4-followups when the kernel registry is in
/// place.
pub fn kernel_contains_call(kernel: &GirKernel) -> bool {
    kernel.body.iter().any(stmt_has_call)
}

fn stmt_has_call(stmt: &GirStmt) -> bool {
    match stmt {
        GirStmt::Let(l) => expr_has_call(&l.value),
        GirStmt::Return(e) => expr_has_call(e),
        GirStmt::Discard(e) => expr_has_call(e),
        GirStmt::TailCall { args } => args.iter().any(expr_has_call),
        GirStmt::Select { arms } => arms.iter().any(|a| {
            a.cond.as_ref().is_some_and(expr_has_call)
                || a.body.iter().any(stmt_has_call)
        }),
    }
}

pub(crate) fn expr_has_call(e: &GirExpr) -> bool {
    match &e.op {
        GirOp::Call { .. } | GirOp::DynCall { .. } => true,
        GirOp::Const(_)
        | GirOp::ConstStr(_)
        | GirOp::ConstValue(_)
        | GirOp::ConstNull
        | GirOp::Local(_)
        | GirOp::ArrayLen { .. } => false,
        GirOp::Concat(parts) => parts.iter().any(expr_has_call),
        GirOp::IsNull(inner) => expr_has_call(inner),
        GirOp::QopUnwrap { inner, .. } => expr_has_call(inner),
        GirOp::ValueArith { lhs, rhs, .. }
        | GirOp::ValueEq { lhs, rhs, .. } => {
            expr_has_call(lhs) || expr_has_call(rhs)
        }
        GirOp::Bin { lhs, rhs, .. }
        | GirOp::Cmp { lhs, rhs, .. }
        | GirOp::BoolBin { lhs, rhs, .. } => {
            expr_has_call(lhs) || expr_has_call(rhs)
        }
        GirOp::Not(inner) | GirOp::Cast { inner, .. } => expr_has_call(inner),
        GirOp::ArrayGet { idx, .. } => expr_has_call(idx),
        GirOp::BytesIndex { bytes, idx } => {
            expr_has_call(bytes) || expr_has_call(idx)
        }
        GirOp::MapRef { map, key } => {
            expr_has_call(map) || expr_has_call(key)
        }
        GirOp::ArraySlice { source, start, end } => {
            expr_has_call(source)
                || start.as_ref().is_some_and(|e| expr_has_call(e))
                || end.as_ref().is_some_and(|e| expr_has_call(e))
        }
        GirOp::ArrayFold { init, body, .. } => {
            expr_has_call(init) || expr_has_call(body)
        }
        GirOp::ArrayInit { n, body, .. } => expr_has_call(n) || expr_has_call(body),
        GirOp::ArrayMap { body, .. } => expr_has_call(body),
        GirOp::ArrayFilterMap { body, .. } => expr_has_call(body),
        GirOp::ArrayFindMap { body, .. } => expr_has_call(body),
        GirOp::ArrayFlatMap { body, .. } => expr_has_call(body),
        GirOp::ArrayFilter { predicate, .. } => expr_has_call(predicate),
        GirOp::ArrayFind { predicate, .. } => expr_has_call(predicate),
        GirOp::TupleGet { .. } | GirOp::StructGet { .. } => false,
        GirOp::TupleNew { fields, .. } => fields.iter().any(expr_has_call),
        GirOp::StructNew { sorted_fields, .. } => {
            sorted_fields.iter().any(|(_, e)| expr_has_call(e))
        }
        GirOp::VariantTagEq { .. } | GirOp::VariantPayload { .. } => false,
        GirOp::VariantNew { payloads, .. } => payloads.iter().any(expr_has_call),
        GirOp::Block { lets, tail } => {
            lets.iter().any(|l| expr_has_call(&l.value)) || expr_has_call(tail)
        }
        GirOp::IfChain { arms } => arms
            .iter()
            .any(|(c, v)| c.as_ref().is_some_and(expr_has_call) || expr_has_call(v)),
    }
}

/// True if the kernel contains a `TupleGet` / `StructGet` (and
/// eventually array / variant accessors) whose `elem_typ` is a
/// composite (non-`Prim`) `GirType`. The JIT's primitive-only
/// scalar extraction can't handle these — callers gate on this
/// helper to route the whole kernel to the interpreter (where
/// `extract_composite_or_scalar` handles both cases).
pub fn kernel_contains_composite_element_op(kernel: &GirKernel) -> bool {
    kernel.body.iter().any(stmt_has_composite_element_op)
}

/// True if the kernel references [`GirType::Null`] or
/// [`GirType::Nullable`] anywhere — via [`GirOp::ConstNull`],
/// [`GirOp::IsNull`], a Null/Nullable arg/return on a `DynCall`,
/// or any `GirExpr.typ` of that shape. The JIT can't lower these
/// today, so such kernels route through the interpreter — same
/// fallback shape as `kernel_contains_string`.
pub fn kernel_contains_null(kernel: &GirKernel) -> bool {
    if matches!(kernel.return_type, GirType::Null | GirType::Nullable(_)) {
        return true;
    }
    kernel.body.iter().any(stmt_has_null)
}

fn stmt_has_null(s: &GirStmt) -> bool {
    match s {
        GirStmt::Let(l) => expr_has_null(&l.value),
        GirStmt::Return(e) => expr_has_null(e),
        GirStmt::Discard(e) => expr_has_null(e),
        GirStmt::TailCall { args } => args.iter().any(expr_has_null),
        GirStmt::Select { arms } => arms.iter().any(|a| {
            a.cond.as_ref().is_some_and(expr_has_null)
                || a.body.iter().any(stmt_has_null)
        }),
    }
}

fn expr_has_null(e: &GirExpr) -> bool {
    if matches!(e.typ, GirType::Null | GirType::Nullable(_)) {
        return true;
    }
    match &e.op {
        GirOp::ConstNull | GirOp::IsNull(_) => true,
        // QopUnwrap operates on a Nullable inner — counts as touching null.
        GirOp::QopUnwrap { .. } => true,
        GirOp::Const(_) | GirOp::ConstStr(_) | GirOp::ConstValue(_)
        | GirOp::Local(_)
        | GirOp::ArrayLen { .. } | GirOp::TupleGet { .. }
        | GirOp::StructGet { .. } | GirOp::VariantTagEq { .. }
        | GirOp::VariantPayload { .. } => false,
        GirOp::Concat(parts) => parts.iter().any(expr_has_null),
        GirOp::Bin { lhs, rhs, .. }
        | GirOp::Cmp { lhs, rhs, .. }
        | GirOp::BoolBin { lhs, rhs, .. }
        | GirOp::ValueArith { lhs, rhs, .. }
        | GirOp::ValueEq { lhs, rhs, .. } => {
            expr_has_null(lhs) || expr_has_null(rhs)
        }
        GirOp::Not(inner) | GirOp::Cast { inner, .. } => expr_has_null(inner),
        GirOp::ArrayGet { idx, .. } => expr_has_null(idx),
        GirOp::BytesIndex { bytes, idx } => {
            expr_has_null(bytes) || expr_has_null(idx)
        }
        GirOp::MapRef { map, key } => {
            expr_has_null(map) || expr_has_null(key)
        }
        GirOp::ArraySlice { source, start, end } => {
            expr_has_null(source)
                || start.as_ref().is_some_and(|e| expr_has_null(e))
                || end.as_ref().is_some_and(|e| expr_has_null(e))
        }
        GirOp::ArrayFold { init, body, .. } => {
            expr_has_null(init) || expr_has_null(body)
        }
        GirOp::ArrayInit { n, body, .. } => expr_has_null(n) || expr_has_null(body),
        GirOp::ArrayMap { body, .. } => expr_has_null(body),
        GirOp::ArrayFilterMap { body, .. } => expr_has_null(body),
        GirOp::ArrayFindMap { body, .. } => expr_has_null(body),
        GirOp::ArrayFlatMap { body, .. } => expr_has_null(body),
        GirOp::ArrayFilter { predicate, .. } => expr_has_null(predicate),
        // ArrayFind *produces* a Nullable result, so it always counts as
        // null-touching regardless of the predicate.
        GirOp::ArrayFind { .. } => true,
        GirOp::TupleNew { fields, .. } => fields.iter().any(expr_has_null),
        GirOp::StructNew { sorted_fields, .. } => {
            sorted_fields.iter().any(|(_, e)| expr_has_null(e))
        }
        GirOp::VariantNew { payloads, .. } => payloads.iter().any(expr_has_null),
        GirOp::Block { lets, tail } => {
            lets.iter().any(|l| expr_has_null(&l.value)) || expr_has_null(tail)
        }
        GirOp::IfChain { arms } => arms.iter().any(|(c, v)| {
            c.as_ref().is_some_and(expr_has_null) || expr_has_null(v)
        }),
        GirOp::Call { args, .. } => args.iter().any(expr_has_null),
        GirOp::DynCall { args, arg_types, return_type, .. } => {
            matches!(return_type, GirType::Null | GirType::Nullable(_))
                || arg_types.iter().any(|t| {
                    matches!(t, GirType::Null | GirType::Nullable(_))
                })
                || args.iter().any(expr_has_null)
        }
    }
}

fn stmt_has_composite_element_op(s: &GirStmt) -> bool {
    match s {
        GirStmt::Let(l) => expr_has_composite_element_op(&l.value),
        GirStmt::Return(e) => expr_has_composite_element_op(e),
        GirStmt::Discard(e) => expr_has_composite_element_op(e),
        GirStmt::TailCall { args } => {
            args.iter().any(expr_has_composite_element_op)
        }
        GirStmt::Select { arms } => arms.iter().any(|a| {
            a.cond.as_ref().is_some_and(expr_has_composite_element_op)
                || a.body.iter().any(stmt_has_composite_element_op)
        }),
    }
}

fn expr_has_composite_element_op(e: &GirExpr) -> bool {
    match &e.op {
        GirOp::TupleGet { elem_typ, .. } | GirOp::StructGet { elem_typ, .. } => {
            !matches!(elem_typ, GirType::Prim(_))
        }
        // ArrayGet's element type lives on `e.typ` (no separate
        // elem_typ field). Composite-result ArrayGet routes to interp.
        GirOp::ArrayGet { idx, .. } => {
            !matches!(e.typ, GirType::Prim(_))
                || expr_has_composite_element_op(idx)
        }
        GirOp::BytesIndex { bytes, idx } => {
            expr_has_composite_element_op(bytes)
                || expr_has_composite_element_op(idx)
        }
        GirOp::MapRef { map, key } => {
            expr_has_composite_element_op(map)
                || expr_has_composite_element_op(key)
        }
        GirOp::ArraySlice { source, start, end } => {
            expr_has_composite_element_op(source)
                || start.as_ref().is_some_and(|e| expr_has_composite_element_op(e))
                || end.as_ref().is_some_and(|e| expr_has_composite_element_op(e))
        }
        GirOp::Const(_) | GirOp::ConstStr(_) | GirOp::ConstValue(_)
        | GirOp::ConstNull
        | GirOp::Local(_) | GirOp::ArrayLen { .. }
        | GirOp::VariantTagEq { .. } | GirOp::VariantPayload { .. } => false,
        GirOp::Concat(parts) => parts.iter().any(expr_has_composite_element_op),
        GirOp::IsNull(inner) => expr_has_composite_element_op(inner),
        GirOp::QopUnwrap { inner, .. } => expr_has_composite_element_op(inner),
        GirOp::Bin { lhs, rhs, .. }
        | GirOp::Cmp { lhs, rhs, .. }
        | GirOp::BoolBin { lhs, rhs, .. }
        | GirOp::ValueArith { lhs, rhs, .. }
        | GirOp::ValueEq { lhs, rhs, .. } => {
            expr_has_composite_element_op(lhs)
                || expr_has_composite_element_op(rhs)
        }
        GirOp::Not(inner) | GirOp::Cast { inner, .. } => {
            expr_has_composite_element_op(inner)
        }
        GirOp::ArrayFold { init, body, .. } => {
            expr_has_composite_element_op(init)
                || expr_has_composite_element_op(body)
        }
        GirOp::ArrayInit { n, body, .. } => {
            expr_has_composite_element_op(n)
                || expr_has_composite_element_op(body)
        }
        GirOp::ArrayMap { body, .. } => expr_has_composite_element_op(body),
        GirOp::ArrayFilterMap { body, .. } => expr_has_composite_element_op(body),
        GirOp::ArrayFindMap { body, .. } => expr_has_composite_element_op(body),
        GirOp::ArrayFlatMap { body, .. } => expr_has_composite_element_op(body),
        GirOp::ArrayFilter { predicate, .. } => {
            expr_has_composite_element_op(predicate)
        }
        GirOp::ArrayFind { predicate, .. } => {
            expr_has_composite_element_op(predicate)
        }
        GirOp::TupleNew { fields, .. } => {
            fields.iter().any(expr_has_composite_element_op)
        }
        GirOp::StructNew { sorted_fields, .. } => sorted_fields
            .iter()
            .any(|(_, e)| expr_has_composite_element_op(e)),
        GirOp::VariantNew { payloads, .. } => {
            payloads.iter().any(expr_has_composite_element_op)
        }
        GirOp::Block { lets, tail } => {
            lets.iter().any(|l| expr_has_composite_element_op(&l.value))
                || expr_has_composite_element_op(tail)
        }
        GirOp::IfChain { arms } => arms.iter().any(|(c, v)| {
            c.as_ref().is_some_and(expr_has_composite_element_op)
                || expr_has_composite_element_op(v)
        }),
        GirOp::Call { args, .. } | GirOp::DynCall { args, .. } => {
            args.iter().any(expr_has_composite_element_op)
        }
    }
}

/// Walk a kernel body collecting the names of every `GirOp::Call` it
/// contains. Used by the JIT path (to declare callee `FuncRef`s
/// before lowering) and by the lazy-fusion path (to discover
/// transitive callees of an already-built kernel).
pub fn collect_call_sites(kernel: &GirKernel) -> std::collections::BTreeSet<ArcStr> {
    let mut out = std::collections::BTreeSet::new();
    for s in &kernel.body {
        walk_call_sites_stmt(s, &mut out);
    }
    out
}

fn walk_call_sites_stmt(s: &GirStmt, out: &mut std::collections::BTreeSet<ArcStr>) {
    match s {
        GirStmt::Let(l) => walk_call_sites_expr(&l.value, out),
        GirStmt::Return(e) => walk_call_sites_expr(e, out),
        GirStmt::Discard(e) => walk_call_sites_expr(e, out),
        GirStmt::TailCall { args } => {
            for a in args {
                walk_call_sites_expr(a, out);
            }
        }
        GirStmt::Select { arms } => {
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

fn walk_call_sites_expr(e: &GirExpr, out: &mut std::collections::BTreeSet<ArcStr>) {
    match &e.op {
        GirOp::Call { fn_name, args } => {
            out.insert(fn_name.clone());
            for a in args {
                walk_call_sites_expr(a, out);
            }
        }
        GirOp::DynCall { args, .. } => {
            // No static name to record — DynCall resolves through the
            // kernel's fn-args table at runtime, not the static-call
            // FuncRef table. Just recurse into the arg expressions.
            for a in args {
                walk_call_sites_expr(a, out);
            }
        }
        GirOp::Concat(parts) => {
            for p in parts {
                walk_call_sites_expr(p, out);
            }
        }
        GirOp::Const(_)
        | GirOp::ConstStr(_)
        | GirOp::ConstValue(_)
        | GirOp::ConstNull
        | GirOp::Local(_)
        | GirOp::ArrayLen { .. } => {}
        GirOp::Bin { lhs, rhs, .. }
        | GirOp::Cmp { lhs, rhs, .. }
        | GirOp::BoolBin { lhs, rhs, .. }
        | GirOp::ValueArith { lhs, rhs, .. }
        | GirOp::ValueEq { lhs, rhs, .. } => {
            walk_call_sites_expr(lhs, out);
            walk_call_sites_expr(rhs, out);
        }
        GirOp::Not(inner) | GirOp::Cast { inner, .. } => walk_call_sites_expr(inner, out),
        GirOp::IsNull(inner) => walk_call_sites_expr(inner, out),
        GirOp::QopUnwrap { inner, .. } => walk_call_sites_expr(inner, out),
        GirOp::ArrayGet { idx, .. } => walk_call_sites_expr(idx, out),
        GirOp::BytesIndex { bytes, idx } => {
            walk_call_sites_expr(bytes, out);
            walk_call_sites_expr(idx, out);
        }
        GirOp::MapRef { map, key } => {
            walk_call_sites_expr(map, out);
            walk_call_sites_expr(key, out);
        }
        GirOp::ArraySlice { source, start, end } => {
            walk_call_sites_expr(source, out);
            if let Some(e) = start {
                walk_call_sites_expr(e, out);
            }
            if let Some(e) = end {
                walk_call_sites_expr(e, out);
            }
        }
        GirOp::ArrayFold { init, body, .. } => {
            walk_call_sites_expr(init, out);
            walk_call_sites_expr(body, out);
        }
        GirOp::ArrayInit { n, body, .. } => {
            walk_call_sites_expr(n, out);
            walk_call_sites_expr(body, out);
        }
        GirOp::ArrayMap { body, .. } => walk_call_sites_expr(body, out),
        GirOp::ArrayFilterMap { body, .. } => walk_call_sites_expr(body, out),
        GirOp::ArrayFindMap { body, .. } => walk_call_sites_expr(body, out),
        GirOp::ArrayFlatMap { body, .. } => walk_call_sites_expr(body, out),
        GirOp::ArrayFilter { predicate, .. } => walk_call_sites_expr(predicate, out),
        GirOp::ArrayFind { predicate, .. } => walk_call_sites_expr(predicate, out),
        GirOp::TupleGet { .. } | GirOp::StructGet { .. } => {}
        GirOp::TupleNew { fields, .. } => {
            for f in fields {
                walk_call_sites_expr(f, out);
            }
        }
        GirOp::StructNew { sorted_fields, .. } => {
            for (_, f) in sorted_fields {
                walk_call_sites_expr(f, out);
            }
        }
        GirOp::VariantTagEq { .. } | GirOp::VariantPayload { .. } => {}
        GirOp::VariantNew { payloads, .. } => {
            for p in payloads {
                walk_call_sites_expr(p, out);
            }
        }
        GirOp::Block { lets, tail } => {
            for l in lets {
                walk_call_sites_expr(&l.value, out);
            }
            walk_call_sites_expr(tail, out);
        }
        GirOp::IfChain { arms } => {
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
// is now baked into `GirOp::ArrayFold` directly via `elem_typ`, so
// neither backend needs to walk the body to recover it.)


#[cfg(test)]
mod tests {
    use super::*;

    fn i64c(x: i64) -> GirExpr {
        const_expr(ConstVal::I64(x))
    }

    fn f64c(x: f64) -> GirExpr {
        const_expr(ConstVal::F64(x))
    }

    fn loc(name: &str, prim: PrimType) -> GirExpr {
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
