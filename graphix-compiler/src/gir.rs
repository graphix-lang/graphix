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
use triomphe::Arc;

/// Fusion-time registry mapping each abstract type's [`AbstractId`] to its
/// concrete implementation type. Populated by
/// `node::module::check_sig` when a signed module's `type X;` is matched
/// to its impl `type X = …`. The fusion classifier ([`abi_kind`] /
/// [`freeze_concrete`]) consults it to resolve an abstract-typed value
/// to its concrete shape — the optimizer peeks through the abstraction,
/// but only here (the type system keeps it opaque everywhere else, and
/// the registry is only ever read during fusion).
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

    /// The netidx [`Typ`] for this primitive. The inverse of
    /// [`from_typ`](PrimType::from_typ) on the eleven prim variants.
    /// Used to drive [`Value::cast`] so the fused cast matches the
    /// node-walk (which casts through the same `Value::cast`).
    pub fn to_typ(self) -> Typ {
        match self {
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
        }
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

// ─── Composite types: see the Type-based classifier below ────────
//
// The fused-kernel type lattice is plain netidx [`Type`]; shape
// classification + the structure accessors live in the
// classifier section (`abi_kind`, `freeze_concrete`, `array_elem`,
// `tuple_slots`, `struct_fields`, `variant_cases`, `nullable_inner`,
// `scalar_prim`, `is_value_shape`).


// ─── Type-based classifier (additive foundation for the GirType→Type
//     refactor) ─────────────────────────────────────────────────────
//
// These three pieces — [`AbiKind`], [`abi_kind`], and
// [`freeze_concrete`] — reproduce the *decisions* of
// [`GirType::from_type`] while carrying the structure in a plain
// netidx [`Type`] instead of the parallel [`GirType`] enum. The
// differential test at the bottom of this file proves the
// correspondence shape-by-shape. None of this touches `GirType`,
// `from_type`, or any existing usage; it is purely additive groundwork
// before the larger cut.
//
// **Important parity note.** `from_type` is `Env`-free and does NOT
// resolve `Type::Ref` aliases (a bare `Type::Ref` reaching it falls
// through to `PrimType::from_type` → `None`); the fusion callers
// pre-resolve Refs via `resolve_abstract` before calling `from_type`.
// To keep the invariant `freeze_concrete(t).is_some() ==
// from_type(t).is_some()` *exact* (and the signature `Env`-free, like
// `from_type`), `freeze_concrete` mirrors `from_type`'s traversal
// precisely: it derefs TVars (`with_deref`) and expands nullary
// `Type::Abstract` via [`ABSTRACT_REGISTRY`], but it does NOT resolve
// `Type::Ref`. A frozen `Type` is therefore TVar-free and (nullary-)
// abstract-free over the fusable subset; Ref-resolution, if needed,
// happens in the caller exactly as it does for `from_type` today.

/// The flat, top-level runtime-shape classifier — the `Type`-based
/// twin of the leaf shapes [`GirType::from_type`] produces. Unlike
/// `GirType`, this is *not* nested: any structure (element / field /
/// payload types) is carried by the classified [`Type`] itself and
/// read back out via the structure accessors ([`array_elem`],
/// [`tuple_slots`], [`struct_fields`], [`variant_cases`],
/// [`nullable_inner`]).
///
/// Mirrors [`AbiParamKind`] plus the two non-param leaf shapes
/// `from_type` can yield (`Unit` from `Type::Bottom`, `Null` from the
/// null primitive). `Value` here is the *bare* value-shape group —
/// `DateTime` / `Duration` / `Bytes` / `Map` / `Error` — which all
/// share the two-register `Value` wire form and which `GirType`
/// distinguishes only to pick the right runtime carrier.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AbiKind {
    Scalar(PrimType),
    Array,
    Tuple,
    Struct,
    String,
    Variant,
    Nullable,
    /// Bare value-shape: `DateTime` / `Duration` / `Bytes` / `Map` /
    /// `Error`. Two-register `Value` at the ABI; the carried `Type`
    /// says which.
    Value,
    /// `Type::Bottom` — side-effect-only return shape. Not a valid
    /// kernel parameter.
    Unit,
    /// The single-value `null` type. Not a valid kernel parameter.
    Null,
}

impl AbiKind {
    /// Map a top-level shape to the [`AbiParamKind`] used by the param
    /// path. `Unit`/`Null` are not valid parameter shapes (they aren't
    /// produced as kernel params), so they map to `None`. The bare
    /// `Value` group all binds via the `Value` param kind (its carried
    /// `Type` selects the runtime carrier at access sites).
    pub fn to_abi_param_kind(self) -> Option<AbiParamKind> {
        Some(match self {
            AbiKind::Scalar(p) => AbiParamKind::Scalar(p),
            AbiKind::Array => AbiParamKind::Array,
            AbiKind::Tuple => AbiParamKind::Tuple,
            AbiKind::Struct => AbiParamKind::Struct,
            AbiKind::String => AbiParamKind::String,
            AbiKind::Variant => AbiParamKind::Variant,
            AbiKind::Nullable => AbiParamKind::Nullable,
            AbiKind::Value => AbiParamKind::Value,
            AbiKind::Unit | AbiKind::Null => return None,
        })
    }
}

/// True for a `Type::Primitive` carrying exactly the single bit `which`.
/// Derefs TVars, like `from_type`'s own primitive arms.
fn is_single_prim(t: &Type, which: Typ) -> bool {
    t.with_deref(|r| match r {
        Some(Type::Primitive(p)) => {
            p.contains(which) && p.iter().count() == 1
        }
        _ => false,
    })
}

/// Classify the TOP-LEVEL runtime shape of a `Type`, mirroring the
/// arms of [`GirType::from_type`] but returning the flat [`AbiKind`]
/// (it does NOT recurse to verify element fusability — that's
/// [`freeze_concrete`]'s job). Derefs TVars via `with_deref`; expands
/// nullary `Type::Abstract` via [`ABSTRACT_REGISTRY`]. Returns `None`
/// for the same top-level shapes `from_type` rejects (unbound TVar,
/// `Type::Fn`, multi-member non-option/non-variant `Set`,
/// parameterized abstract, `Type::Any`, `Type::Ref`, `Type::ByRef`).
pub fn abi_kind(t: &Type) -> Option<AbiKind> {
    t.with_deref(|resolved| {
        let resolved = resolved?;
        // Order matches `from_type`: Bottom, then the single-bit
        // primitive special cases, then the value-shape composites,
        // then the collapsed-option primitive, then the structural
        // composites and Set, then the abstract registry.
        match resolved {
            Type::Bottom => return Some(AbiKind::Unit),
            Type::Map { .. } => return Some(AbiKind::Value),
            Type::Error(_) => return Some(AbiKind::Value),
            Type::Array(_) => return Some(AbiKind::Array),
            Type::Tuple(_) => return Some(AbiKind::Tuple),
            Type::Struct(_) => return Some(AbiKind::Struct),
            Type::Variant(_, _) => return Some(AbiKind::Variant),
            Type::Abstract { id, params } if params.is_empty() => {
                let concrete = ABSTRACT_REGISTRY.read().get(id).cloned();
                return concrete.as_ref().and_then(abi_kind);
            }
            _ => {}
        }
        // Single-bit primitives.
        if is_single_prim(resolved, Typ::String) {
            return Some(AbiKind::String);
        }
        if is_single_prim(resolved, Typ::Null) {
            return Some(AbiKind::Null);
        }
        if is_single_prim(resolved, Typ::DateTime)
            || is_single_prim(resolved, Typ::Duration)
            || is_single_prim(resolved, Typ::Bytes)
        {
            return Some(AbiKind::Value);
        }
        if let Type::Primitive(p) = resolved {
            // Collapsed `T | null` (exactly 2 bits, one of them Null).
            if p.contains(Typ::Null) && p.iter().count() == 2 {
                return Some(AbiKind::Nullable);
            }
            // Plain single-register scalar.
            if let Some(prim) = PrimType::from_type(resolved) {
                return Some(AbiKind::Scalar(prim));
            }
            return None;
        }
        // `Type::Set`: option (`[T, null]`) and result (`[T, Error]`)
        // both classify as Nullable; a set of single-Variant types is
        // Variant; anything else is non-fusable.
        if let Type::Set(members) = resolved {
            // Mirror `from_type`'s exact two-stage priority for a
            // 2-member set: NULL-marker first (the other member is the
            // success T, even if that T is itself an Error/value-shape),
            // then ERROR-marker, only if neither member is null. The
            // success member must itself be classifiable, else the whole
            // shape is non-fusable.
            if let Some(succ) = option_result_success(members) {
                return succ.and_then(|s| abi_kind(s)).map(|_| AbiKind::Nullable);
            }
            // Variant union: every member must be a single Variant.
            let all_variants = members.iter().all(|m| {
                m.with_deref(|r| matches!(r, Some(Type::Variant(_, _))))
            });
            if all_variants {
                // Note: an EMPTY set classifies as a variant union here
                // (zero members vacuously all-variant) — matching
                // `from_type`, whose variant-union collect over zero
                // members yields `Some(vec![])` → `GirType::Variant`.
                return Some(AbiKind::Variant);
            }
            return None;
        }
        None
    })
}

/// If `members` (a `Type::Set`'s members) is the option/result shape —
/// a 2-member set with exactly one null/Error marker — returns
/// `Some(Some(success_member))`. NULL-marker takes precedence over
/// Error (so `[null, Error<T>]` is `Nullable<Error<T>>`, with null as
/// the marker), exactly as `from_type` resolves it. Returns `None` if
/// it isn't the option/result shape at all (so the caller falls to the
/// variant-union path). The inner `Option` lets the caller distinguish
/// "is the option shape, here's the success member" from "isn't".
fn option_result_success(members: &[Type]) -> Option<Option<&Type>> {
    if members.len() != 2 {
        return None;
    }
    let is_null = |m: &Type| is_single_prim(m, Typ::Null);
    let is_err =
        |m: &Type| m.with_deref(|r| matches!(r, Some(Type::Error(_))));
    // Stage 1: `[T, null]` — exactly one member is the null primitive;
    // the OTHER is the success T (regardless of what T is).
    match (is_null(&members[0]), is_null(&members[1])) {
        (true, false) => return Some(Some(&members[1])),
        (false, true) => return Some(Some(&members[0])),
        // (true, true) → `[null, null]`: not option; falls through to
        // the Error stage (neither is Error) and then variant-union.
        // (false, false) → no null; try Error.
        _ => {}
    }
    // Stage 2: `[T, Error]` — only reached when NEITHER member is null.
    match (is_err(&members[0]), is_err(&members[1])) {
        (true, false) => Some(Some(&members[1])),
        (false, true) => Some(Some(&members[0])),
        // (true, true) → `[Error, Error]`: not option/result.
        // (false, false) → no marker at all.
        _ => None,
    }
}

/// Produce a fully-concrete `Type` (no live TVars, no nullary
/// abstracts) over the fusable subset, mirroring
/// [`GirType::from_type`]'s traversal and accept/reject decisions
/// exactly. Returns `None` iff `from_type` returns `None` for the same
/// input — that is the load-bearing invariant the differential test
/// asserts.
///
/// A `depth` cap (16, matching `resolve_abstract`) terminates on
/// recursive abstract types: when the cap trips we return `None`,
/// which is also what `from_type` does for such types (it would
/// recurse until the registry yields a shape it can't represent, or
/// stack-overflow — here we simply cap and reject, which is the safe
/// matching behavior since recursive types aren't fusable anyway).
pub fn freeze_concrete(t: &Type) -> Option<Type> {
    freeze_concrete_d(t, 0)
}

fn freeze_concrete_d(t: &Type, depth: usize) -> Option<Type> {
    if depth > 16 {
        return None;
    }
    t.with_deref(|resolved| {
        let resolved = resolved?;
        match resolved {
            // Leaves that `from_type` accepts as-is.
            Type::Bottom => Some(Type::Bottom),
            Type::Map { .. } => Some(resolved.clone()),
            Type::Error(_) => Some(resolved.clone()),
            // Single-bit special primitives + collapsed option +
            // plain scalar all just need their top-level shape
            // recognized; the primitive carries no further structure to
            // freeze, so clone the resolved (TVar-free) form.
            Type::Primitive(p) => {
                if is_single_prim(resolved, Typ::String)
                    || is_single_prim(resolved, Typ::Null)
                    || is_single_prim(resolved, Typ::DateTime)
                    || is_single_prim(resolved, Typ::Duration)
                    || is_single_prim(resolved, Typ::Bytes)
                {
                    return Some(Type::Primitive(*p));
                }
                // Collapsed `T | null` (2 bits incl. Null): the inner
                // bit must be String or a register prim (matching
                // `from_type`'s collapsed-option arm).
                if p.contains(Typ::Null) && p.iter().count() == 2 {
                    let other = p.iter().find(|f| *f != Typ::Null)?;
                    if other == Typ::String
                        || PrimType::from_typ(other).is_some()
                    {
                        return Some(Type::Primitive(*p));
                    }
                    return None;
                }
                // Plain single-register scalar.
                PrimType::from_type(resolved).map(|_| Type::Primitive(*p))
            }
            Type::Array(inner) => {
                let inner = freeze_concrete_d(inner, depth + 1)?;
                Some(Type::Array(Arc::new(inner)))
            }
            Type::Tuple(elems) => {
                let frozen: Option<Vec<Type>> = elems
                    .iter()
                    .map(|e| freeze_concrete_d(e, depth + 1))
                    .collect();
                Some(Type::Tuple(Arc::from_iter(frozen?)))
            }
            Type::Struct(fields) => {
                let frozen: Option<Vec<(ArcStr, Type)>> = fields
                    .iter()
                    .map(|(n, ft)| {
                        freeze_concrete_d(ft, depth + 1).map(|t| (n.clone(), t))
                    })
                    .collect();
                Some(Type::Struct(Arc::from_iter(frozen?)))
            }
            Type::Variant(tag, payloads) => {
                let frozen: Option<Vec<Type>> = payloads
                    .iter()
                    .map(|p| freeze_concrete_d(p, depth + 1))
                    .collect();
                Some(Type::Variant(tag.clone(), Arc::from_iter(frozen?)))
            }
            Type::Set(members) => {
                // `[T, null]` / `[T, Error]` → freeze the success
                // member (the marker stays as-is). Uses the SAME
                // null-first priority as `from_type` (via
                // `option_result_success`): `[null, Error<T>]` is the
                // option of `Error<T>`, not a degenerate.
                if let Some(succ_opt) = option_result_success(members) {
                    let succ = succ_opt?;
                    // The success member is `members[0]` or `members[1]`;
                    // the other is the marker (kept as-is). Freeze the
                    // success in place, preserving member order.
                    let succ_idx =
                        if std::ptr::eq(&members[0], succ) { 0 } else { 1 };
                    let frozen_succ = freeze_concrete_d(succ, depth + 1)?;
                    let m0 = if succ_idx == 0 {
                        frozen_succ.clone()
                    } else {
                        members[0].clone()
                    };
                    let m1 = if succ_idx == 1 {
                        frozen_succ
                    } else {
                        members[1].clone()
                    };
                    return Some(Type::Set(Arc::from_iter([m0, m1])));
                }
                // Variant union: every member a single Variant; freeze
                // each.
                let frozen: Option<Vec<Type>> = members
                    .iter()
                    .map(|m| {
                        m.with_deref(|r| match r {
                            Some(Type::Variant(tag, payloads)) => {
                                let fp: Option<Vec<Type>> = payloads
                                    .iter()
                                    .map(|p| freeze_concrete_d(p, depth + 1))
                                    .collect();
                                Some(Type::Variant(
                                    tag.clone(),
                                    Arc::from_iter(fp?),
                                ))
                            }
                            _ => None,
                        })
                    })
                    .collect();
                Some(Type::Set(Arc::from_iter(frozen?)))
            }
            Type::Abstract { id, params } if params.is_empty() => {
                let concrete = ABSTRACT_REGISTRY.read().get(id).cloned()?;
                freeze_concrete_d(&concrete, depth + 1)
            }
            // Everything else: Any, Fn, Ref, ByRef, parameterized
            // Abstract, multi-member non-option/non-variant Set
            // (handled above), unbound TVar (None via with_deref).
            _ => None,
        }
    })
}

// ─── Structure accessors over a (frozen) `Type` ──────────────────────
//
// These read nesting out of a `Type` the way the `GirType::as_*`
// accessors read it out of a `GirType`. They expect a frozen (concrete)
// `Type` but tolerate live TVars by deref'ing; they do not resolve
// Refs (parity with `from_type`).

/// Element type of a `Type::Array`; `None` otherwise.
pub fn array_elem(t: &Type) -> Option<&Type> {
    match t {
        Type::Array(e) => Some(e),
        _ => None,
    }
}

/// Per-slot types of a `Type::Tuple`; `None` otherwise.
pub fn tuple_slots(t: &Type) -> Option<&[Type]> {
    match t {
        Type::Tuple(es) => Some(es),
        _ => None,
    }
}

/// Sorted field list of a `Type::Struct`; `None` otherwise.
pub fn struct_fields(t: &Type) -> Option<&[(ArcStr, Type)]> {
    match t {
        Type::Struct(fs) => Some(fs),
        _ => None,
    }
}

/// The variant cases of a `Type`: a single `Type::Variant` yields one
/// case; a `Type::Set` of single-Variant members yields the case list
/// (mirroring `GirType::from_type`'s Variant-union handling). `None`
/// for any other shape. Each case is `(tag, payload-types)`.
pub fn variant_cases(t: &Type) -> Option<Vec<(ArcStr, Vec<Type>)>> {
    fn one(t: &Type) -> Option<(ArcStr, Vec<Type>)> {
        t.with_deref(|r| match r {
            Some(Type::Variant(tag, payloads)) => {
                Some((tag.clone(), payloads.iter().cloned().collect()))
            }
            _ => None,
        })
    }
    match t {
        Type::Variant(_, _) => one(t).map(|c| vec![c]),
        Type::Set(members) => members.iter().map(one).collect(),
        _ => None,
    }
}

/// Normalizes ALL THREE option-shaped forms `GirType::Nullable`
/// collapses and returns the (frozen) success type `T`:
/// - `Type::Set([T, null])` (the explicit option Set),
/// - `Type::Set([T, Error])` (the result Set),
/// - the collapsed 2-bit primitive `T | null`.
/// Returns `None` for any non-option shape. The returned `T` is frozen
/// (run through [`freeze_concrete`]) so callers get a concrete inner.
pub fn nullable_inner(t: &Type) -> Option<Type> {
    t.with_deref(|resolved| {
        let resolved = resolved?;
        match resolved {
            // Collapsed `T | null` primitive.
            Type::Primitive(p)
                if p.contains(Typ::Null) && p.iter().count() == 2 =>
            {
                let other = p.iter().find(|f| *f != Typ::Null)?;
                if other == Typ::String {
                    return Some(Type::Primitive(Typ::String.into()));
                }
                PrimType::from_typ(other)
                    .map(|pt| Type::Primitive(pt.to_typ().into()))
            }
            Type::Set(members) => {
                // Same null-first priority as `from_type` /
                // `freeze_concrete`. `[null, Error<T>]` → success is
                // `Error<T>` (null is the marker).
                let succ = option_result_success(members)??;
                freeze_concrete(succ)
            }
            _ => None,
        }
    })
}

/// The scalar [`PrimType`] of a `Type` whose top-level shape is a plain
/// register scalar; `None` for any composite / string / value-shape /
/// option type. Replaces `GirType::as_prim`.
pub fn scalar_prim(t: &Type) -> Option<PrimType> {
    match abi_kind(t) {
        Some(AbiKind::Scalar(p)) => Some(p),
        _ => None,
    }
}

/// If `t` is `Array<P>` with a plain scalar element, the element
/// `PrimType`; `None` if the element is composite or `t` isn't an
/// array. Replaces `GirType::as_array_prim`.
pub fn array_scalar_prim(t: &Type) -> Option<PrimType> {
    array_elem(t).and_then(scalar_prim)
}

/// True for the "Value-shape" types — those whose JIT/runtime
/// representation is a two-register `Value` (disc + payload):
/// `Variant`, `Nullable`/option/result, `DateTime`, `Duration`,
/// `Bytes`, `Map`, `Error`. Replaces `GirType::is_value_shape`.
pub fn is_value_shape(t: &Type) -> bool {
    matches!(
        abi_kind(t),
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value)
    )
}

// ─── Concrete-`Type` constructors for the leaf shapes ────────────────
//
// Carrying `Type` in the GIR means the emitter sometimes needs to mint
// a concrete `Type` for a shape it knows by hand (a `string` let, a
// `null` literal, a scalar result). These build the frozen `Type` the
// classifier recognizes.

/// A plain scalar `Type` for a [`PrimType`] (`Type::Primitive`).
pub fn prim_type(p: PrimType) -> Type {
    Type::Primitive(p.to_typ().into())
}

/// The `string` type (`Type::Primitive(Typ::String)`).
pub fn string_type() -> Type {
    Type::Primitive(Typ::String.into())
}

/// The `null` type (`Type::Primitive(Typ::Null)`).
pub fn null_type() -> Type {
    Type::Primitive(Typ::Null.into())
}

/// The unit / side-effect-only type (`Type::Bottom`).
pub fn unit_type() -> Type {
    Type::Bottom
}

/// The `bytes` type (`Type::Primitive(Typ::Bytes)`).
pub fn bytes_type() -> Type {
    Type::Primitive(Typ::Bytes.into())
}

/// The `datetime` type (`Type::Primitive(Typ::DateTime)`).
pub fn datetime_type() -> Type {
    Type::Primitive(Typ::DateTime.into())
}

/// The `duration` type (`Type::Primitive(Typ::Duration)`).
pub fn duration_type() -> Type {
    Type::Primitive(Typ::Duration.into())
}

/// A generic `Map<_, _>` type. Only the top-level `AbiKind::Value`
/// classification matters for the fused kernel (the runtime value is a
/// thin-pointer `Value::Map`); the key/value structure isn't inspected
/// by codegen, so a placeholder `Null`-keyed/`Null`-valued map suffices
/// for the leaf-shape classification (`abi_kind` → `Value`).
pub fn map_type() -> Type {
    Type::Map {
        key: triomphe::Arc::new(null_type()),
        value: triomphe::Arc::new(null_type()),
    }
}

/// `Array<elem>`.
pub fn array_type(elem: Type) -> Type {
    Type::Array(triomphe::Arc::new(elem))
}

/// `(T0, T1, ...)` from per-slot element types.
pub fn tuple_type(elems: Vec<Type>) -> Type {
    Type::Tuple(triomphe::Arc::from_iter(elems))
}

/// `{f0: T0, f1: T1, ...}` from a sorted field list.
pub fn struct_type(fields: Vec<(ArcStr, Type)>) -> Type {
    Type::Struct(triomphe::Arc::from_iter(fields))
}

/// Reconstruct a variant `Type` from a `(tag, payload-types)` case
/// list — a single case is a `Type::Variant`, multiple cases a
/// `Type::Set` of single-Variant members (the inverse of
/// [`variant_cases`]).
pub fn variant_type_from_cases(cases: &[(ArcStr, Vec<Type>)]) -> Type {
    let mk = |(tag, payloads): &(ArcStr, Vec<Type>)| {
        Type::Variant(tag.clone(), triomphe::Arc::from_iter(payloads.clone()))
    };
    if cases.len() == 1 {
        mk(&cases[0])
    } else {
        Type::Set(triomphe::Arc::from_iter(cases.iter().map(mk)))
    }
}

/// `[inner, null]` option shape — the canonical `Nullable<inner>`
/// representation (a 2-member Set with a null marker). Used when the
/// emitter knows it has produced an option value but only has the
/// success inner type in hand (HOF callback bodies, etc.).
pub fn nullable_type(inner: Type) -> Type {
    Type::Set(triomphe::Arc::from_iter([inner, null_type()]))
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
    pub elem: Type,
    pub bind_id: Option<BindId>,
}

/// A tuple value passed as a kernel parameter. Per-slot types can be
/// any [`GirType`] (nested tuples / structs / variants / arrays
/// allowed). Same `&ValArray` runtime boundary as [`ArrayInput`].
#[derive(Debug, Clone)]
pub struct TupleInput {
    pub name: ArcStr,
    pub elems: Vec<Type>,
    pub bind_id: Option<BindId>,
}

/// A struct value passed as a kernel parameter. Field types can be
/// any [`GirType`]. Same `&ValArray` boundary, with fields stored at
/// compile-time-known sorted-by-name positions.
#[derive(Debug, Clone)]
pub struct StructInput {
    pub name: ArcStr,
    pub fields: Vec<(ArcStr, Type)>,
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
    pub cases: Vec<(ArcStr, Vec<Type>)>,
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
    pub elem: Type,
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
    pub typ: Type,
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
    pub arg_types: Vec<Type>,
    /// Return type — scalar or array.
    pub return_type: Type,
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
    pub fn typ(&self) -> Type {
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
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub enum GirOp {
    /// A compile-time constant carrying a netidx [`Value`]. The
    /// interpreter clones the `Value`. The JIT dispatches on the
    /// value's shape: a `Value::is_copy()` scalar (`i64`/`f64`/`bool`/
    /// …) lowers inline as an `iconst`/`f64const` (extracting the
    /// scalar for the `PrimType` from the expression's `typ`), while a
    /// non-copy value-shape constant (datetime/duration/bytes/map) is
    /// interned in a per-kernel value-constants table (like
    /// `ConstStr`'s string table) and emitted as the `(disc, payload)`
    /// words, cloning the `Arc` on use. Result type matches the
    /// value's shape via the expression's `typ` ([`GirType::Prim`] for
    /// scalars, [`GirType::DateTime`]/`Duration`/`Bytes`/`Map` for
    /// value-shape).
    Const(Value),
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
    /// literal segments. Result type is [`GirType::String`]. A distinct
    /// op from [`GirOp::Const`] because String SSA is a single-register
    /// thin pointer (`ArcStr`), not the two-register `Value` shape the
    /// JIT uses for [`GirOp::Const`]'s value-shape constants.
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
        success_typ: Type,
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
    /// returns the resulting `Value`.
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
        arg_types: Vec<Type>,
        /// Return type. Equals the wrapping `GirExpr.typ`; carried
        /// here for symmetry with `arg_types` and so the JIT can
        /// pick the right `cast_u64_to_*` / `Box::from_raw` decode
        /// path without re-walking `GirExpr.typ`.
        return_type: Type,
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
    ///
    /// `scrut` is the select's scrutinee (the dispatch value). It is
    /// evaluated ONCE up front and bottom-checked: a bottom (`None`)
    /// scrutinee poisons the whole chain (it produces bottom), matching
    /// the node-walk where a `select` node with no scrutinee value
    /// never fires. This is distinct from a bottom *arm condition*
    /// (e.g. a guard), which just makes that arm fail to match and
    /// falls through. `scrut` is `None` only for selects with no
    /// representable scrutinee gate (none today — lowering always sets
    /// it); a `None` here means "no up-front bottom-check".
    IfChain {
        scrut: Option<Box<GirExpr>>,
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
        elem_typ: Type,
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
        in_elem: Type,
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
        elem: Type,
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
        elem: Type,
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
        in_elem: Type,
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
        in_elem: Type,
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
        elem_typ: Type,
    },
    /// Build a tuple from per-slot expressions. Each `fields[i]`
    /// emits to a GIR expression of type `elem_types[i]`; the
    /// interpreter / JIT wrap primitives in `Value::<variant>(x)`
    /// and feed composite values (tuples, structs, variants, arrays,
    /// nullables, strings) through the appropriate boundary helper.
    /// Result type is `GirType::Tuple(elem_types)`.
    TupleNew {
        fields: Vec<GirExpr>,
        elem_types: Vec<Type>,
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
        elem_typ: Type,
    },
    /// Build a struct from per-field expressions. `sorted_fields` is
    /// the canonical alphabetical order (graphix's runtime layout).
    /// Lowers identically to `TupleNew` — the runtime doesn't
    /// distinguish tuples from structs at the ValArray level. Field
    /// types in `sorted_types` may be composite (mirroring TupleNew's
    /// generalisation).
    StructNew {
        sorted_fields: Vec<(ArcStr, GirExpr)>,
        sorted_types: Vec<(ArcStr, Type)>,
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
        payload_types: Vec<Type>,
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
    ///
    /// `scrut` is the scrutinee, evaluated once up front and
    /// bottom-checked: a bottom scrutinee poisons the select (the
    /// kernel returns bottom), mirroring the node-walk's "Select fires
    /// iff the scrutinee has a value." See [`GirOp::IfChain`] for the
    /// scrutinee-vs-guard distinction. `None` means no up-front
    /// bottom-check (none today).
    Select { scrut: Option<GirExpr>, arms: Vec<SelectArm> },
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
    pub return_type: Type,
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
        match abi_kind(&self.return_type)? {
            AbiKind::Scalar(p) => Some(AbiReturn::One { prim: Some(p) }),
            AbiKind::Array
            | AbiKind::Tuple
            | AbiKind::Struct
            | AbiKind::Unit
            | AbiKind::String => Some(AbiReturn::One { prim: None }),
            AbiKind::Variant | AbiKind::Nullable | AbiKind::Value => {
                Some(AbiReturn::Two)
            }
            AbiKind::Null => None,
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
    pub arg_types: Vec<Type>,
    /// Return type of the callee. May be scalar or composite; the
    /// dispatcher and JIT codegen branch on the kind to pick the
    /// right encode/decode path.
    pub return_type: Type,
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
    let lp = scalar_prim(&lhs.typ)?;
    let rp = scalar_prim(&rhs.typ)?;
    if lp != rp || !lp.is_numeric() {
        return None;
    }
    Some(GirExpr {
        op: GirOp::Bin { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
        typ: prim_type(lp),
    })
}

pub fn cmp(lhs: GirExpr, rhs: GirExpr, op: CmpOp) -> Option<GirExpr> {
    if lhs.typ != rhs.typ {
        return None;
    }
    if scalar_prim(&lhs.typ).is_some() {
        return Some(GirExpr {
            op: GirOp::Cmp { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
            typ: prim_type(PrimType::Bool),
        });
    }
    // Non-primitive `==` / `!=` (String, composite Array/Tuple/Struct, or
    // value-shape Map/Variant/Nullable/Bytes/DateTime/Duration) compares
    // via `Value`'s PartialEq. Ordering operators on non-prim types
    // aren't lowered, and Unit/Null have no comparable runtime form.
    if matches!(op, CmpOp::Eq | CmpOp::Ne)
        && !matches!(abi_kind(&lhs.typ), Some(AbiKind::Unit | AbiKind::Null))
    {
        return Some(GirExpr {
            op: GirOp::ValueEq {
                ne: matches!(op, CmpOp::Ne),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            typ: prim_type(PrimType::Bool),
        });
    }
    None
}

pub fn bool_op(lhs: GirExpr, rhs: GirExpr, op: BoolOp) -> Option<GirExpr> {
    let bool_t = prim_type(PrimType::Bool);
    if lhs.typ != bool_t || rhs.typ != bool_t {
        return None;
    }
    Some(GirExpr {
        op: GirOp::BoolBin { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
        typ: bool_t,
    })
}

pub fn not(inner: GirExpr) -> Option<GirExpr> {
    let bool_t = prim_type(PrimType::Bool);
    if inner.typ != bool_t {
        return None;
    }
    Some(GirExpr { op: GirOp::Not(Box::new(inner)), typ: bool_t })
}

pub fn cast(inner: GirExpr, target: PrimType) -> Option<GirExpr> {
    // Rust's `as` doesn't accept bool↔integer in either direction;
    // those need a `match`. Until the emitter grows that, refuse.
    let inner_p = scalar_prim(&inner.typ)?;
    if target == PrimType::Bool || inner_p == PrimType::Bool {
        return None;
    }
    Some(GirExpr {
        op: GirOp::Cast { inner: Box::new(inner), target },
        typ: prim_type(target),
    })
}

/// Build a primitive-typed [`GirOp::Const`] from a scalar [`Value`].
/// The lowered [`PrimType`] is derived from the value's variant via
/// [`scalar_prim_of_value`] (collapsing `Z32`/`V64`/… to their fixed-
/// width form). Panics on a non-scalar `Value` — a GIR-malformed
/// condition; value-shape constants (datetime/duration/bytes/map) are
/// emitted as `GirExpr { op: GirOp::Const(v), typ: <value-shape> }`
/// directly by the lowering code, which knows the [`GirType`].
pub fn const_expr(value: Value) -> GirExpr {
    let prim = scalar_prim_of_value(&value).unwrap_or_else(|| {
        panic!("const_expr: non-scalar Value {value:?} — GIR malformed")
    });
    GirExpr { op: GirOp::Const(value), typ: prim_type(prim) }
}

/// Map a scalar [`Value`] to the [`PrimType`] the kernel should treat
/// it as. Variable-width variants (`Z32`/`Z64`/`V32`/`V64`) collapse
/// to their fixed-width form (`I32`/`I64`/`U32`/`U64`), matching how
/// the interpreter and JIT extract the scalar. Returns `None` for any
/// non-scalar `Value` (string, bytes, array, datetime, decimal, …).
pub fn scalar_prim_of_value(v: &Value) -> Option<PrimType> {
    Some(match v {
        Value::I8(_) => PrimType::I8,
        Value::I16(_) => PrimType::I16,
        Value::I32(_) | Value::Z32(_) => PrimType::I32,
        Value::I64(_) | Value::Z64(_) => PrimType::I64,
        Value::U8(_) => PrimType::U8,
        Value::U16(_) => PrimType::U16,
        Value::U32(_) | Value::V32(_) => PrimType::U32,
        Value::U64(_) | Value::V64(_) => PrimType::U64,
        Value::F32(_) => PrimType::F32,
        Value::F64(_) => PrimType::F64,
        Value::Bool(_) => PrimType::Bool,
        _ => return None,
    })
}

pub fn local(name: ArcStr, prim: PrimType) -> GirExpr {
    GirExpr { op: GirOp::Local(name), typ: prim_type(prim) }
}

/// Construct a [`GirExpr`] referring to an array-typed local. The
/// element type goes on `Type::Array(elem)` so downstream ops
/// (ArrayMap, ArrayFold) can read it back without a separate sidecar.
pub fn local_array(name: ArcStr, elem: PrimType) -> GirExpr {
    GirExpr {
        op: GirOp::Local(name),
        typ: array_type(prim_type(elem)),
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
        GirStmt::Select { scrut, arms } => {
            scrut.as_ref().is_some_and(expr_has_call)
                || arms.iter().any(|a| {
                    a.cond.as_ref().is_some_and(expr_has_call)
                        || a.body.iter().any(stmt_has_call)
                })
        }
    }
}

/// Extract the integer value of a `GirOp::Const(Value::<int>)` as an
/// `i128` (covers every signed/unsigned width). `None` for non-constant
/// or non-integer exprs.
pub(crate) fn const_int(e: &GirExpr) -> Option<i128> {
    match &e.op {
        GirOp::Const(v) => match v {
            Value::I8(n) => Some(*n as i128),
            Value::I16(n) => Some(*n as i128),
            Value::I32(n) => Some(*n as i128),
            Value::I64(n) | Value::Z64(n) => Some(*n as i128),
            Value::U8(n) => Some(*n as i128),
            Value::U16(n) => Some(*n as i128),
            Value::U32(n) | Value::V32(n) => Some(*n as i128),
            Value::U64(n) | Value::V64(n) => Some(*n as i128),
            _ => None,
        },
        _ => None,
    }
}

/// Whether an integer div/mod (`lhs op rhs`) could be a value-bottom
/// (div/mod-by-zero, or signed MIN/-1 overflow). Returns `false` only
/// when both the divisor is a known-nonzero constant AND the overflow
/// case is provably absent — i.e. the op provably can't bottom (so it
/// stays a non-tainted `Single`, the fast path). A non-constant divisor
/// conservatively returns `true`.
pub(crate) fn int_div_may_bottom(lhs: &GirExpr, rhs: &GirExpr) -> bool {
    match const_int(rhs) {
        // Non-constant divisor: could be zero (or -1 with a MIN
        // dividend) at runtime.
        None => true,
        Some(0) => true,
        // Divisor -1 can overflow only with a signed MIN dividend.
        // Stay conservative unless the dividend is a non-MIN constant.
        Some(-1) => {
            let signed = scalar_prim(&lhs.typ)
                .is_some_and(|p| p.is_signed());
            if !signed {
                false
            } else {
                match (const_int(lhs), scalar_prim(&lhs.typ)) {
                    (Some(l), Some(p)) => l == signed_min_i128(p),
                    _ => true,
                }
            }
        }
        // Any other nonzero constant divisor can't bottom.
        Some(_) => false,
    }
}

fn signed_min_i128(p: PrimType) -> i128 {
    match p {
        PrimType::I8 => i8::MIN as i128,
        PrimType::I16 => i16::MIN as i128,
        PrimType::I32 => i32::MIN as i128,
        _ => i64::MIN as i128,
    }
}

/// Whether a SCALAR `GirExpr` could compile to a value-bottom
/// (`CompiledExpr::Scalar2` in the JIT) — i.e. produce or propagate an
/// integer div/mod-by-zero, signed MIN/-1, or scalar `?`-on-error.
/// Used by the JIT's `compile_ifchain` to decide whether the scalar
/// merge block needs a validity-bit phi. MUST over-approximate (never
/// under-report) — a missed taint would silently drop a bottom.
///
/// Returns `true` for any op that produces taint (int div/mod, scalar
/// QopUnwrap) or propagates it from a sub-expression (the pure scalar
/// consumers Bin/Cmp/BoolBin/Not/Cast/IfChain), AND for a bare scalar
/// `Local` (which may read a tainted let-bound local — invisible from
/// the expression alone, so conservatively assumed taintable). Non-
/// taintable: composite/value-shape producers, DynCalls/Calls,
/// accessors, constants.
pub(crate) fn expr_may_value_bottom(e: &GirExpr) -> bool {
    match &e.op {
        // Producers.
        GirOp::Bin { op, lhs, rhs } => {
            (matches!(op, BinOp::Div | BinOp::Mod)
                && scalar_prim(&lhs.typ).is_some_and(|p| p.is_integer())
                && int_div_may_bottom(lhs, rhs))
                || expr_may_value_bottom(lhs)
                || expr_may_value_bottom(rhs)
        }
        GirOp::QopUnwrap { .. } => true,
        // Propagating scalar consumers.
        GirOp::Cmp { lhs, rhs, .. } | GirOp::BoolBin { lhs, rhs, .. } => {
            expr_may_value_bottom(lhs) || expr_may_value_bottom(rhs)
        }
        GirOp::Not(inner) | GirOp::Cast { inner, .. } => {
            expr_may_value_bottom(inner)
        }
        GirOp::IfChain { scrut, arms } => {
            scrut.as_ref().is_some_and(|s| expr_may_value_bottom(s))
                || arms
                    .iter()
                    .any(|(_, body)| expr_may_value_bottom(body))
        }
        // A scalar Local may read a tainted let-bound local.
        GirOp::Local(_) => {
            matches!(abi_kind(&e.typ), Some(AbiKind::Scalar(_)))
        }
        // Non-taintable in scalar position.
        _ => false,
    }
}

pub(crate) fn expr_has_call(e: &GirExpr) -> bool {
    match &e.op {
        GirOp::Call { .. } | GirOp::DynCall { .. } => true,
        GirOp::Const(_)
        | GirOp::ConstStr(_)
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
        GirOp::IfChain { scrut, arms } => {
            scrut.as_ref().is_some_and(|s| expr_has_call(s))
                || arms.iter().any(|(c, v)| {
                    c.as_ref().is_some_and(expr_has_call) || expr_has_call(v)
                })
        }
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
    if is_null_or_nullable(&kernel.return_type) {
        return true;
    }
    kernel.body.iter().any(stmt_has_null)
}

/// True if `t`'s top-level shape is the `null` type or a
/// `Nullable`/option/result shape.
fn is_null_or_nullable(t: &Type) -> bool {
    matches!(abi_kind(t), Some(AbiKind::Null | AbiKind::Nullable))
}

fn stmt_has_null(s: &GirStmt) -> bool {
    match s {
        GirStmt::Let(l) => expr_has_null(&l.value),
        GirStmt::Return(e) => expr_has_null(e),
        GirStmt::Discard(e) => expr_has_null(e),
        GirStmt::TailCall { args } => args.iter().any(expr_has_null),
        GirStmt::Select { scrut, arms } => {
            scrut.as_ref().is_some_and(expr_has_null)
                || arms.iter().any(|a| {
                    a.cond.as_ref().is_some_and(expr_has_null)
                        || a.body.iter().any(stmt_has_null)
                })
        }
    }
}

fn expr_has_null(e: &GirExpr) -> bool {
    if is_null_or_nullable(&e.typ) {
        return true;
    }
    match &e.op {
        GirOp::ConstNull | GirOp::IsNull(_) => true,
        // QopUnwrap operates on a Nullable inner — counts as touching null.
        GirOp::QopUnwrap { .. } => true,
        GirOp::Const(_) | GirOp::ConstStr(_)
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
        GirOp::IfChain { scrut, arms } => {
            scrut.as_ref().is_some_and(|s| expr_has_null(s))
                || arms.iter().any(|(c, v)| {
                    c.as_ref().is_some_and(expr_has_null) || expr_has_null(v)
                })
        }
        GirOp::Call { args, .. } => args.iter().any(expr_has_null),
        GirOp::DynCall { args, arg_types, return_type, .. } => {
            is_null_or_nullable(return_type)
                || arg_types.iter().any(is_null_or_nullable)
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
        GirStmt::Select { scrut, arms } => {
            scrut.as_ref().is_some_and(expr_has_composite_element_op)
                || arms.iter().any(|a| {
                    a.cond.as_ref().is_some_and(expr_has_composite_element_op)
                        || a.body.iter().any(stmt_has_composite_element_op)
                })
        }
    }
}

fn expr_has_composite_element_op(e: &GirExpr) -> bool {
    match &e.op {
        GirOp::TupleGet { elem_typ, .. } | GirOp::StructGet { elem_typ, .. } => {
            scalar_prim(elem_typ).is_none()
        }
        // ArrayGet's element type lives on `e.typ` (no separate
        // elem_typ field). Composite-result ArrayGet routes to interp.
        GirOp::ArrayGet { idx, .. } => {
            scalar_prim(&e.typ).is_none()
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
        GirOp::Const(_) | GirOp::ConstStr(_)
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
        GirOp::IfChain { scrut, arms } => {
            scrut.as_ref().is_some_and(|s| expr_has_composite_element_op(s))
                || arms.iter().any(|(c, v)| {
                    c.as_ref().is_some_and(expr_has_composite_element_op)
                        || expr_has_composite_element_op(v)
                })
        }
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
        GirStmt::Select { scrut, arms } => {
            if let Some(s) = scrut {
                walk_call_sites_expr(s, out);
            }
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
        GirOp::IfChain { scrut, arms } => {
            if let Some(s) = scrut {
                walk_call_sites_expr(s, out);
            }
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
        const_expr(Value::I64(x))
    }

    fn f64c(x: f64) -> GirExpr {
        const_expr(Value::F64(x))
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
        let t = const_expr(Value::Bool(true));
        let f = const_expr(Value::Bool(false));
        assert!(arith(t, f, BinOp::Add).is_none());
    }

    #[test]
    fn cast_bool_rejected() {
        let x = loc("x", PrimType::Bool);
        assert!(cast(x, PrimType::I64).is_none());
        let y = loc("y", PrimType::I64);
        assert!(cast(y, PrimType::Bool).is_none());
    }

    #[test]
    fn abi_kind_to_param_kind_round_trips() {
        // Unit / Null aren't valid params; everything else maps.
        assert!(AbiKind::Unit.to_abi_param_kind().is_none());
        assert!(AbiKind::Null.to_abi_param_kind().is_none());
        assert!(matches!(
            AbiKind::Scalar(PrimType::I64).to_abi_param_kind(),
            Some(AbiParamKind::Scalar(PrimType::I64))
        ));
        for k in [
            AbiKind::Array,
            AbiKind::Tuple,
            AbiKind::Struct,
            AbiKind::String,
            AbiKind::Variant,
            AbiKind::Nullable,
            AbiKind::Value,
        ] {
            assert!(k.to_abi_param_kind().is_some(), "{k:?} should be a param");
        }
    }
}
