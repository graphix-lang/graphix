//! The kernel ABI contract — the durable, body-free half of the
//! fusion/JIT boundary.
//!
//! Everything here is derived from netidx [`Type`]s: the closed
//! register-scalar set ([`PrimType`]), the runtime shape classifier
//! ([`abi_kind`] / [`AbiKind`]), type freezing ([`freeze_for_abi`]),
//! the typed kernel input slots, and [`KernelSig`] — the single
//! source-ordered parameter list every ABI site derives its wire
//! layout from (the unified Value ABI). The JIT emits CLIF straight
//! from the node graph; this module holds the durable, body-free
//! contract.
//!
//! [`KnownFusedFn`] (the caller-side cross-kernel call signature) rides
//! along here too — it's ABI proper. The scalar operator taxonomy
//! (`BinOp` / `CmpOp` / `BoolOp`) lives in `node::op` with the node-walk
//! that defines those operators' semantics; the JIT imports it from there.

use crate::{
    BindId,
    expr::ModPath,
    typ::{AbstractId, Type, TypeRef},
};
use arcstr::ArcStr;
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use triomphe::Arc;

/// Per-`ExecCtx` registry mapping each abstract type's [`AbstractId`] to
/// its concrete implementation type. Populated by
/// `node::module::check_sig` when a signed module's `type X;` is matched
/// to its impl `type X = …`. The fusion classifiers ([`abi_kind`] /
/// [`freeze_for_abi`] / `resolve_abstract`) consult it to resolve an
/// abstract-typed value to its concrete shape. Static instance checking
/// may also resolve a representation, but only from within its defining
/// module.
///
/// Lives on the `ExecCtx` (a field, not a process-global) so it is
/// dropped with the context. `AbstractId`s are minted fresh on every
/// compile — each `ExecCtx` recompiles its stdlib from source and gets
/// new ids — so a global map would grow without bound across a
/// long-lived process that spins up many contexts. Per-context storage
/// is also correct: a `Type` (and its `AbstractId`s) never escapes its
/// owning context, so context A never needs to resolve an abstract that
/// context B registered.
///
/// No lock or interior mutability: registration holds `&mut ExecCtx`;
/// later instance checks and fusion read through a shared borrow. The
/// old process-global needed an `RwLock` to guard concurrent compiles of
/// different contexts; per-context storage removes that contention.
#[derive(Debug)]
struct AbstractDef {
    scope: ModPath,
    params: Arc<[ArcStr]>,
    typ: Type,
}

#[derive(Debug, Default)]
pub struct AbstractRegistry(nohash::IntMap<AbstractId, AbstractDef>);

impl AbstractRegistry {
    #[cfg(test)]
    fn insert(&mut self, id: AbstractId, typ: Type) {
        let params: Arc<[ArcStr]> = Arc::from_iter(std::iter::empty());
        self.insert_scoped(id, params, typ, ModPath::root());
    }

    pub fn insert_scoped(
        &mut self,
        id: AbstractId,
        params: Arc<[ArcStr]>,
        typ: Type,
        scope: ModPath,
    ) {
        self.0.insert(id, AbstractDef { scope, params, typ });
    }

    /// The concrete implementation type registered for `id`, cloned out
    /// (a few `Arc` bumps) so the classifier holds no borrow across its
    /// recursion. `None` for an unregistered id (non-fusable).
    pub fn resolve(&self, id: &AbstractId, params: &[Type]) -> Option<Type> {
        let def = self.0.get(id)?;
        if def.params.len() != params.len() {
            return None;
        }
        let known = def
            .params
            .iter()
            .cloned()
            .zip(params.iter().cloned())
            .collect::<LPooled<ahash::AHashMap<_, _>>>();
        Some(def.typ.replace_tvars(&known))
    }

    pub fn resolve_internal(
        &self,
        id: &AbstractId,
        params: &[Type],
        scope: &ModPath,
    ) -> Option<Type> {
        let def = self.0.get(id)?;
        if !self.visible_from(def, scope) {
            return None;
        }
        self.resolve(id, params)
    }

    /// The private definition TEMPLATE (formal param names +
    /// unsubstituted body) for `id`, scope-gated like
    /// [`Self::resolve_internal`]. The privatize walk uses it to
    /// re-point a public-view ref's resolution cell at the private
    /// body without expanding anything — the ref's own params keep
    /// substituting through `lookup_ref` as usual.
    pub(crate) fn internal_template(
        &self,
        id: &AbstractId,
        scope: &ModPath,
    ) -> Option<(Arc<[ArcStr]>, Type)> {
        let def = self.0.get(id)?;
        if !self.visible_from(def, scope) {
            return None;
        }
        Some((def.params.clone(), def.typ.clone()))
    }

    fn visible_from(&self, def: &AbstractDef, scope: &ModPath) -> bool {
        let mut candidate = netidx_core::path::Path::parts(&scope.0);
        netidx_core::path::Path::parts(&def.scope.0)
            .all(|part| candidate.next() == Some(part))
    }
}

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
// classifier section (`abi_kind`, `freeze_for_abi`, `array_elem`,
// `tuple_slots`, `struct_fields`, `variant_cases`, `nullable_inner`,
// `scalar_prim`, `is_value_shape`).

// ─── Type-based runtime-shape classifier ─────────────────────────────
//
// These three pieces — [`AbiKind`], [`abi_kind`], and
// [`freeze_for_abi`] — classify the runtime shape of a netidx
// [`Type`] over the fusable subset. [`abi_kind`] returns the flat,
// top-level [`AbiKind`] (it does not recurse to verify nested
// fusability); [`freeze_for_abi`] recurses, producing a fully-
// concrete `Type` (or `None` if any nested element is non-fusable).
//
// **Important parity note.** Neither resolves `Type::Ref` aliases (a
// bare `Type::Ref` reaching the scalar fallback yields `None`); the
// fusion callers pre-resolve Refs via `resolve_abstract` before
// classifying. Both deref TVars (`with_deref`) and expand nullary
// `Type::Abstract` via [`ABSTRACT_REGISTRY`], but neither resolves
// `Type::Ref`. A frozen `Type` is therefore TVar-free and (nullary-)
// abstract-free over the fusable subset; Ref-resolution, if needed,
// happens in the caller.

/// The flat, top-level runtime-shape classifier — the leaf shapes a
/// `Type` can take at the ABI boundary. This is *not* nested: any
/// structure (element / field / payload types) is carried by the
/// classified [`Type`] itself and read back out via the structure
/// accessors ([`array_elem`], [`tuple_slots`], [`struct_fields`],
/// [`variant_cases`], [`nullable_inner`]).
///
/// Mirrors [`AbiParamKind`] plus the two non-param leaf shapes
/// [`abi_kind`] can yield (`Unit` from `Type::Bottom`, `Null` from the
/// null primitive). `Value` here is the *bare* value-shape group —
/// `DateTime` / `Duration` / `Bytes` / `Map` / `Error` — which all
/// share the two-register `Value` wire form; the carried `Type`
/// selects the right runtime carrier.
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
        Some(Type::Primitive(p)) => p.contains(which) && p.iter().count() == 1,
        _ => false,
    })
}

/// Classify the TOP-LEVEL runtime shape of a `Type`, returning the
/// flat [`AbiKind`] (it does NOT recurse to verify element fusability
/// — that's [`freeze_for_abi`]'s job). Derefs TVars via `with_deref`;
/// expands nullary `Type::Abstract` via the [`AbstractRegistry`].
/// Returns `None` for the top-level shapes that aren't fusable (unbound
/// TVar, `Type::Fn`, multi-member non-option/non-variant `Set`,
/// parameterized abstract, `Type::Any`, `Type::Ref`, `Type::ByRef`).
///
/// Recursive abstract types (a registry rep that re-reaches its own id,
/// directly or through an option-of-self) terminate via [`Seen`] cycle
/// detection on the abstract-expansion recursion and return `None` (a
/// recursive type has no flat ABI kind) — the same mechanism
/// [`freeze_for_abi`] / `resolve_abstract` use.
pub fn abi_kind(reg: &AbstractRegistry, t: &Type) -> Option<AbiKind> {
    abi_kind_d(reg, t, None)
}

fn abi_kind_d(reg: &AbstractRegistry, t: &Type, seen: Option<&Seen>) -> Option<AbiKind> {
    // Clone the deref'd type OUT of `with_deref` so the TVar's read
    // guard is DROPPED before the body runs: the Set/Abstract arms
    // recurse (more TVar guards), and parking_lot's fair, non-reentrant
    // TVar lock can't be re-acquired while held. (The registry no longer
    // participates — it's a plain per-ctx borrow now, not a lock — but
    // the TVar deref-clone discipline still stands. Same as
    // `resolve_abstract`'s TVar arm and `freeze_for_abi_d`.)
    let resolved = t.with_deref(|r| r.cloned());
    {
        let resolved = resolved.as_ref()?;
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
            Type::Abstract { id, params } => {
                let key = ExpandKey::Abstract(*id);
                if Seen::contains(seen, &key) {
                    // Recursive abstract — no flat ABI kind.
                    return None;
                }
                let concrete = reg.resolve(id, params);
                let node = Seen::push(seen, key);
                return concrete.as_ref().and_then(|c| abi_kind_d(reg, c, Some(&node)));
            }
            // A ref reaches here through instance signatures, which
            // stay name-compressed since the privatize walk replaced
            // eager expansion. Its carried resolution cell makes it
            // expandable WITHOUT an env; an empty cell (or a recursive
            // named type — no flat ABI layout) is `None` = de-fuse,
            // exactly the pre-cell behavior.
            Type::Ref(tr) => {
                let key = ExpandKey::Ref(tr.clone());
                if Seen::contains(seen, &key) {
                    return None;
                }
                let expanded = tr.expand_cell();
                let node = Seen::push(seen, key);
                return expanded.as_ref().and_then(|c| abi_kind_d(reg, c, Some(&node)));
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
                // Pass `seen` through — the success member's abstract (if
                // any) must be checked against the accumulated path, so an
                // option-of-self (`type A = [A, null]`) terminates.
                return succ
                    .and_then(|s| abi_kind_d(reg, s, seen))
                    .map(|_| AbiKind::Nullable);
            }
            // Variant union: every member must be a single Variant.
            let all_variants = members
                .iter()
                .all(|m| m.with_deref(|r| matches!(r, Some(Type::Variant(_, _)))));
            if all_variants {
                // Note: an EMPTY set classifies as a variant union here
                // (zero members vacuously all-variant) — a variant-union
                // over zero members still yields `AbiKind::Variant`.
                return Some(AbiKind::Variant);
            }
            return None;
        }
        None
    }
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
    let is_err = |m: &Type| m.with_deref(|r| matches!(r, Some(Type::Error(_))));
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

/// One type EXPANSION on the path from the root, for cycle detection
/// during concretization: the identity of a resolved abstract or named
/// (`Type::Ref`) type. Structural nesting (array/tuple/struct) is NOT an
/// expansion — only following an abstract to its registry rep, or a
/// `Ref` to its definition, is. Shared by [`freeze_for_abi`] (abstracts
/// only — it rejects `Ref`) and `fusion::lowering::resolve_abstract`
/// (both).
#[derive(Clone, PartialEq)]
pub(crate) enum ExpandKey {
    Abstract(AbstractId),
    Ref(TypeRef),
}

/// Stack-allocated cons-list of the [`ExpandKey`]s on the current path
/// from the root — the basis for cycle detection while concretizing a
/// type. ONLY an actual abstract/`Ref` expansion extends the chain;
/// structural recursion passes it through unchanged. So it terminates on
/// true type recursion (a key that re-occurs) while letting arbitrarily-
/// deep FINITE types through. This replaces the old fixed depth cap,
/// which incremented on every recursion and so conflated "deeply nested"
/// with "recursive" — silently refusing to fuse deeply-nested but
/// non-recursive types (a predictable-performance violation). Membership
/// is an O(chain) walk; the chain length is the count of nested
/// expansions (tiny), so no heap.
pub(crate) struct Seen<'a> {
    key: ExpandKey,
    /// [`expand_key_fp`] of `key` — membership walks compare u64s and
    /// fall back to full equality only on a fingerprint match (the
    /// `TypeRef` string equality dominated dependency-validity checks
    /// at GUI scale).
    fp: u64,
    len: usize,
    prev: Option<&'a Seen<'a>>,
}

/// Cheap discriminating fingerprint of an [`ExpandKey`] — collisions
/// only cost a full-equality fallback.
pub(crate) fn expand_key_fp(key: &ExpandKey) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut h = ahash::AHasher::default();
    match key {
        ExpandKey::Abstract(id) => {
            0u8.hash(&mut h);
            id.hash(&mut h);
        }
        ExpandKey::Ref(tr) => {
            1u8.hash(&mut h);
            tr.scope.hash(&mut h);
            tr.name.hash(&mut h);
            tr.params.len().hash(&mut h);
        }
    }
    h.finish()
}

impl<'a> Seen<'a> {
    pub(crate) fn push(prev: Option<&'a Seen<'a>>, key: ExpandKey) -> Self {
        let fp = expand_key_fp(&key);
        Self { key, fp, len: Self::len(prev) + 1, prev }
    }

    pub(crate) fn contains(cur: Option<&Self>, key: &ExpandKey) -> bool {
        Self::contains_fp(cur, expand_key_fp(key), key)
    }

    pub(crate) fn contains_fp(mut cur: Option<&Self>, fp: u64, key: &ExpandKey) -> bool {
        while let Some(s) = cur {
            if s.fp == fp && &s.key == key {
                return true;
            }
            cur = s.prev;
        }
        false
    }

    /// Like [`Self::contains`], but yields the MATCHED key from the
    /// chain. The freeze's opaque-leaf rule wants the OUTER
    /// occurrence of a recursive ref — its resolution cell is
    /// provably filled (it just expanded to reach this recurrence),
    /// where the inner occurrence's cell may still be empty (`Seen`
    /// keys compare cell-blind).
    pub(crate) fn find<'b>(
        mut cur: Option<&'b Self>,
        key: &ExpandKey,
    ) -> Option<&'b ExpandKey> {
        let fp = expand_key_fp(key);
        while let Some(s) = cur {
            if s.fp == fp && &s.key == key {
                return Some(&s.key);
            }
            cur = s.prev;
        }
        None
    }

    pub(crate) fn len(this: Option<&Self>) -> usize {
        this.map_or(0, |s| s.len)
    }
}

/// Backstop on the freeze's expansion CHAIN for NON-regular recursion
/// (`type T<'a> = T<Array<'a>>` — the params grow each step, so
/// identity detection never fires). Mirrors `resolve_abstract_d`'s
/// per-path length guard; regular recursion never gets near it.
const MAX_FREEZE_EXPANSIONS: usize = 256;

/// The kernel-ABI encodability gate. Despite the "freeze" framing this
/// is not (only) a `Type → Type` normalization — concretizing the TVars
/// is the *means*; deciding whether the type can be encoded into the
/// kernel's register/`Value` layout, and handing back the concrete wire
/// shape when it can, is the *end*. The pure type-level concretization it
/// rests on already lives on `Type` (`with_deref` / `normalize`); what is
/// left here is the ABI delta, which is why this belongs in `kernel_abi`
/// and not on `Type`: the `None`s are perfectly valid types (`Fn`, `Ref`,
/// `decimal`, a bare function) that simply have no kernel encoding, the
/// register gate is `PrimType::from_typ`, and it peeks through
/// [`ABSTRACT_REGISTRY`] (which the type system keeps opaque everywhere
/// else).
///
/// Concretely: produce a fully-concrete `Type` (no live TVars, no nullary
/// abstracts) over the fusable subset, recursing through every nested
/// element. Returns `None` if any part of the type is non-fusable —
/// the accept/reject decisions match [`abi_kind`] at each level (e.g.
/// `Map`/`Error` stop the recursion: they are carried as an opaque
/// two-register `Value`, so their parameters never reach the wire).
///
/// Recursive abstract and recursive named types terminate by [`Seen`]
/// cycle detection on the two expansion arms (`Abstract` via the
/// registry; `Ref` via its carried resolution cell, env-free) and
/// freeze to an OPAQUE LEAF — the recurring node stays unexpanded, so
/// the frozen output is FINITE and the recursive value crosses the
/// kernel boundary as a 2-word opaque (`abi_kind` classifies a
/// variant-union like `List` as `Variant` without recursing payloads;
/// the List/Map collection lowering and list-typed DynCalls depend on
/// this). A recursive NON-variant shape (a recursive tuple) also
/// freezes finitely — such a type is uninhabited at runtime (no base
/// case), and every consumer classifies per-level, so the leaf is
/// shape-safe. Consumers that need flat payload structure (select
/// payload binds, element leaves) refuse the leaf per-node. NON-regular
/// recursion (`type T<'a> = T<Array<'a>>` — params grow per step, so
/// identity never matches) is cut by [`MAX_FREEZE_EXPANSIONS`] on the
/// expansion chain, mirroring `resolve_abstract_d`'s per-path guard.
/// Structural recursion (array/tuple/struct/variant nesting)
/// is intentionally unbounded: it terminates on a finite type, and any
/// type deep enough to overflow it here would already have overflowed
/// the parser (source nesting is stack-limited) or the typechecker's own
/// uncapped type walks (`normalize` / `contains` / `resolve_tvars`). The
/// bound is thus inherited upstream — consistent with those sibling
/// passes — not a fixed cap re-imposed here (the old `depth > 16` cap
/// was, and it rejected legitimate deeply-nested finite types).
pub fn freeze_for_abi(reg: &AbstractRegistry, t: &Type) -> Option<Type> {
    freeze_for_abi_d(reg, t, None)
}

fn freeze_for_abi_d(
    reg: &AbstractRegistry,
    t: &Type,
    seen: Option<&Seen>,
) -> Option<Type> {
    // Deref-clone hoisted out — see `abi_kind`'s note (most arms recurse
    // under the TVar lock).
    let resolved = t.with_deref(|r| r.cloned());
    {
        let resolved = resolved.as_ref()?;
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
                    if other == Typ::String || PrimType::from_typ(other).is_some() {
                        return Some(Type::Primitive(*p));
                    }
                    return None;
                }
                // Plain single-register scalar.
                PrimType::from_type(resolved).map(|_| Type::Primitive(*p))
            }
            Type::Array(inner) => {
                let inner = freeze_for_abi_d(reg, inner, seen)?;
                Some(Type::Array(Arc::new(inner)))
            }
            Type::Tuple(elems) => {
                let frozen: Option<LPooled<Vec<Type>>> =
                    elems.iter().map(|e| freeze_for_abi_d(reg, e, seen)).collect();
                let mut frozen = frozen?;
                Some(Type::Tuple(Arc::from_iter(frozen.drain(..))))
            }
            Type::Struct(fields) => {
                let frozen: Option<LPooled<Vec<(ArcStr, Type)>>> = fields
                    .iter()
                    .map(|(n, ft)| {
                        freeze_for_abi_d(reg, ft, seen).map(|t| (n.clone(), t))
                    })
                    .collect();
                let mut frozen = frozen?;
                Some(Type::Struct(Arc::from_iter(frozen.drain(..))))
            }
            Type::Variant(tag, payloads) => {
                let frozen: Option<LPooled<Vec<Type>>> =
                    payloads.iter().map(|p| freeze_for_abi_d(reg, p, seen)).collect();
                let mut frozen = frozen?;
                Some(Type::Variant(tag.clone(), Arc::from_iter(frozen.drain(..))))
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
                    let succ_idx = if std::ptr::eq(&members[0], succ) { 0 } else { 1 };
                    let frozen_succ = freeze_for_abi_d(reg, succ, seen)?;
                    let m0 = if succ_idx == 0 {
                        frozen_succ.clone()
                    } else {
                        members[0].clone()
                    };
                    let m1 = if succ_idx == 1 { frozen_succ } else { members[1].clone() };
                    return Some(Type::Set(Arc::from_iter([m0, m1])));
                }
                // Variant union: every member a single Variant; freeze
                // each.
                let frozen: Option<LPooled<Vec<Type>>> = members
                    .iter()
                    .map(|m| {
                        // Clone out, then recurse guard-free (see
                        // `abi_kind`'s lock-discipline note).
                        let m = m.with_deref(|r| r.cloned());
                        match m {
                            Some(Type::Variant(tag, payloads)) => {
                                let fp: Option<LPooled<Vec<Type>>> = payloads
                                    .iter()
                                    .map(|p| freeze_for_abi_d(reg, p, seen))
                                    .collect();
                                let mut fp = fp?;
                                Some(Type::Variant(
                                    tag.clone(),
                                    Arc::from_iter(fp.drain(..)),
                                ))
                            }
                            _ => None,
                        }
                    })
                    .collect();
                let mut frozen = frozen?;
                Some(Type::Set(Arc::from_iter(frozen.drain(..))))
            }
            Type::Abstract { id, params } => {
                let key = ExpandKey::Abstract(*id);
                if Seen::contains(seen, &key) {
                    // Recursive abstract: freeze to an OPAQUE LEAF
                    // (see the Ref arm below) — the registry id IS
                    // the identity; params freeze so the leaf is
                    // TVar-free.
                    let frozen: Option<LPooled<Vec<Type>>> =
                        params.iter().map(|p| freeze_for_abi_d(reg, p, seen)).collect();
                    let mut frozen = frozen?;
                    return Some(Type::Abstract {
                        id: *id,
                        params: Arc::from_iter(frozen.drain(..)),
                    });
                }
                if Seen::len(seen) > MAX_FREEZE_EXPANSIONS {
                    return None;
                }
                let concrete = reg.resolve(id, params)?;
                let node = Seen::push(seen, key);
                freeze_for_abi_d(reg, &concrete, Some(&node))
            }
            // Name-compressed instance-signature refs expand through
            // their carried resolution cell (env-free); an empty cell
            // is `None` = de-fuse. A RECURSIVE named type freezes to
            // an OPAQUE LEAF: the ref stays unexpanded, so the frozen
            // form is finite (`[`Cons(i64, Ref(List<i64>)), `Nil]`),
            // `abi_kind` classifies the whole as a 2-word opaque
            // (Variant — the Set-of-variants arm never recurses
            // payloads), and consumers that need flat payload
            // structure (select payload binds, elem leaves) refuse
            // per-node as they already do for non-scalar payloads.
            // The leaf reuses the MATCHED (outer) ref — provably
            // cell-filled — and freezes its params: an unbound-TVar
            // param would violate the frozen=TVar-free invariant and
            // alias identities (unbound cells compare equal).
            Type::Ref(tr) => {
                let key = ExpandKey::Ref(tr.clone());
                if let Some(matched) = Seen::find(seen, &key) {
                    let ExpandKey::Ref(outer) = matched else {
                        unreachable!("a Ref key can only match a Ref entry")
                    };
                    let frozen: Option<LPooled<Vec<Type>>> = tr
                        .params
                        .iter()
                        .map(|p| freeze_for_abi_d(reg, p, seen))
                        .collect();
                    let mut frozen = frozen?;
                    return Some(Type::Ref(
                        outer.with_params(Arc::from_iter(frozen.drain(..))),
                    ));
                }
                if Seen::len(seen) > MAX_FREEZE_EXPANSIONS {
                    return None;
                }
                let expanded = tr.expand_cell()?;
                let node = Seen::push(seen, key);
                freeze_for_abi_d(reg, &expanded, Some(&node))
            }
            // Everything else: Any, Fn, ByRef, multi-member
            // non-option/non-variant Set
            // (handled above), unbound TVar (None via with_deref).
            _ => None,
        }
    }
}

// ─── Structure accessors over a (frozen) `Type` ──────────────────────
//
// These read nesting out of a `Type`: element, slot, field, payload,
// and option-inner types. They expect a frozen (concrete) `Type` but
// tolerate live TVars by deref'ing; they do not resolve Refs (the
// callers pre-resolve via `resolve_abstract`).

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
/// (the same variant-union handling [`abi_kind`] uses). `None` for
/// any other shape. Each case is `(tag, payload-types)`.
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

/// [`freeze_for_abi`], retrying through [`Type::normalize`] and then
/// through [`Type::resolve_tvars`]`().normalize()` when the direct
/// freeze fails. Typecheck can leave a union un-flattened — a
/// `select`'s result type is the raw fold of its arm types, e.g.
/// `Set([i64, TVar→i64])` — which `freeze_for_abi` rejects (it
/// mirrors `from_type` shape-for-shape). Normalizing flattens and
/// merges the set (→ `i64`) without changing the denoted type. But
/// `normalize`'s merge compares members STRUCTURALLY, so it can't see
/// a TVar bound AFTER the union was built: a `never()` arm's fresh
/// TVar gets bound by a downstream consumer's unification (e.g. to
/// `Array<f64>`), leaving `Set([TVar→Array<TVar→f64>, Array<f64>])` —
/// structurally unequal members. The third rung snapshots every bound
/// TVar to its binding first, so the merge collapses the set. Each
/// rung runs only when the previous fails, so the common path pays
/// nothing and no rung rewrites TVar bindings (`resolve_tvars`
/// deep-clones).
pub fn freeze_for_abi_normalized(reg: &AbstractRegistry, t: &Type) -> Option<Type> {
    freeze_for_abi(reg, t)
        .or_else(|| freeze_for_abi(reg, &t.normalize()))
        .or_else(|| freeze_for_abi(reg, &t.resolve_tvars().normalize()))
}

/// Normalizes ALL THREE option-shaped forms that collapse to
/// [`AbiKind::Nullable`] and returns the (frozen) success type `T`:
/// - `Type::Set([T, null])` (the explicit option Set),
/// - `Type::Set([T, Error])` (the result Set),
/// - the collapsed 2-bit primitive `T | null`.
/// Returns `None` for any non-option shape. The returned `T` is frozen
/// (run through [`freeze_for_abi`]) so callers get a concrete inner.
pub fn nullable_inner(reg: &AbstractRegistry, t: &Type) -> Option<Type> {
    // Deref-clone hoisted out of the guard — see `abi_kind`'s note
    // (the Set arm freezes the success member, which recurses).
    let resolved = t.with_deref(|r| r.cloned());
    {
        let resolved = resolved.as_ref()?;
        match resolved {
            // Collapsed `T | null` primitive.
            Type::Primitive(p) if p.contains(Typ::Null) && p.iter().count() == 2 => {
                let other = p.iter().find(|f| *f != Typ::Null)?;
                if other == Typ::String {
                    return Some(Type::Primitive(Typ::String.into()));
                }
                PrimType::from_typ(other).map(|pt| Type::Primitive(pt.to_typ().into()))
            }
            Type::Set(members) => {
                // Same null-first priority as `from_type` /
                // `freeze_for_abi`. `[null, Error<T>]` → success is
                // `Error<T>` (null is the marker).
                let succ = option_result_success(members)??;
                freeze_for_abi(reg, succ)
            }
            _ => None,
        }
    }
}

/// The scalar [`PrimType`] of a `Type` whose top-level shape is a plain
/// register scalar; `None` for any composite / string / value-shape /
/// option type.
pub fn scalar_prim(reg: &AbstractRegistry, t: &Type) -> Option<PrimType> {
    match abi_kind(reg, t) {
        Some(AbiKind::Scalar(p)) => Some(p),
        _ => None,
    }
}

/// If `t` is `Array<P>` with a plain scalar element, the element
/// `PrimType`; `None` if the element is composite or `t` isn't an
/// array.
pub fn array_scalar_prim(reg: &AbstractRegistry, t: &Type) -> Option<PrimType> {
    array_elem(t).and_then(|e| scalar_prim(reg, e))
}

/// True for the "Value-shape" types — those whose JIT/runtime
/// representation is a two-register `Value` (disc + payload):
/// `Variant`, `Nullable`/option/result, `DateTime`, `Duration`,
/// `Bytes`, `Map`, `Error`.
pub fn is_value_shape(reg: &AbstractRegistry, t: &Type) -> bool {
    matches!(
        abi_kind(reg, t),
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value)
    )
}

// ─── Concrete-`Type` constructors for the leaf shapes ────────────────
//
// The emitter sometimes needs to mint a concrete `Type` for a shape it
// knows by hand (a `string` let, a `null` literal, a scalar result).
// These build the frozen `Type` the classifier recognizes.

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

/// One parameter of a fused kernel, in SOURCE order — the unified
/// Value ABI's single parameter list. The vec's order IS the ABI
/// order (wire slots assign sequentially), the runtime packer's arg
/// order, and the tail-rebind order; there is no kind grouping and no
/// parallel per-kind bookkeeping. `bind_id` is set when the input
/// came from `discover_inputs` walking a real graph, `None` for
/// synthetic inputs (lambda formals).
#[derive(Debug, Clone)]
pub struct KernelParam {
    pub name: ArcStr,
    pub kind: ParamKind,
    pub bind_id: Option<BindId>,
}

/// The shape of one kernel parameter, carrying the kind-specific
/// static metadata the body emitter and runtime packer need. Wire
/// shape is uniform regardless (a two-word Value pair); this drives
/// element/field reads, entry binding, and value validation.
#[derive(Debug, Clone)]
pub enum ParamKind {
    Scalar(PrimType),
    /// Array — `elem` is the element type (primitive or nested
    /// composite).
    Array {
        elem: Type,
    },
    /// Tuple — per-slot types (nesting allowed).
    Tuple {
        elems: Vec<Type>,
    },
    /// Struct — fields at compile-time-known sorted-by-name
    /// positions.
    Struct {
        fields: Vec<(ArcStr, Type)>,
    },
    /// Variant — the slot at runtime index 0 is the tag string,
    /// payloads start at index 1. `cases` enumerates the legal
    /// `(tag, payload_types)` shapes.
    Variant {
        cases: Vec<(ArcStr, Vec<Type>)>,
    },
    /// `[T, null]` option shape — `elem` is the inner (non-null)
    /// type.
    Nullable {
        elem: Type,
    },
    String,
    /// Bare value shape (`DateTime`/`Duration`/`Bytes`) — carries the
    /// full `Type` so a `Ref` read re-wraps correctly.
    Value {
        typ: Type,
    },
}

impl ParamKind {
    /// The wire-classification this parameter binds under.
    pub fn abi(&self) -> AbiParamKind {
        match self {
            ParamKind::Scalar(p) => AbiParamKind::Scalar(*p),
            ParamKind::Array { .. } => AbiParamKind::Array,
            ParamKind::Tuple { .. } => AbiParamKind::Tuple,
            ParamKind::Struct { .. } => AbiParamKind::Struct,
            ParamKind::Variant { .. } => AbiParamKind::Variant,
            ParamKind::Nullable { .. } => AbiParamKind::Nullable,
            ParamKind::String => AbiParamKind::String,
            ParamKind::Value { .. } => AbiParamKind::Value,
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
    /// order argument list, mixed with primitive args). Kernel's
    /// runtime extracts it from the incoming `from` slice.
    Param { arg_pos: u32 },
    /// Statically-resolved user binding: the `LambdaDef` lives in
    /// `ctx.cached[bind_id]` (or, for unstable bindings,
    /// `event.variables[bind_id]`). Set when fusion can't fuse the
    /// callee inline (its body uses unsupported constructs) but can
    /// still call it via Apply::update.
    Binding { bind_id: crate::BindId },
    /// Sync builtin call. Resolved at `Kernel::new` time by looking
    /// up `name` in `ctx.builtins`, constructing the builtin's
    /// `Apply<R, E>` via the registered init fn, and stashing it in
    /// the per-kernel slot. The slot is pre-bound — `dispatch` skips
    /// the `LambdaDef`-rebind check entirely.
    ///
    /// `typ` is the resolved FnType at the call site (read by
    /// fusion off the CallSite's resolved `ftype()`), needed by
    /// builtin init fns.
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
        /// `Kernel::pre_bind_builtin` to look up the lambda's
        /// env+scope so a `BuiltinSlot::LabeledDefault` whose
        /// expression references free variables visible only in
        /// the lambda's original module scope (e.g. `default_escape`
        /// in `str::escape`'s `#esc = default_escape`) compiles
        /// correctly. `None` is safe — pre_bind_builtin falls back
        /// to compiling defaults in the kernel's own scope.
        lambda_id: Option<crate::LambdaId>,
    },
    /// The `cast<T>(x)` operator, lowered as a one-argument DynCall to
    /// the cast machinery (it's an operator, not a callable, but the
    /// dispatch path is the same). `target` is the full structured
    /// destination `Type`; `Kernel::new` constructs a `CastApply` that
    /// runs `target.cast_value(&ctx.env, v)` — the SAME function the
    /// node-walk uses, so interp/jit agree by construction. Pre-bound:
    /// dispatch ignores the (Null placeholder) fn-arg value. Only
    /// casts the inline emitter can't handle (a non-scalar source or
    /// target) are registered here; scalar→scalar casts stay inline.
    Cast { target: crate::typ::Type },
    /// A handler-ful `?` (a `?` caught by an enclosing `try`). On the
    /// error path the operator delivers its error by WRITING the catch
    /// handler's variable — `wrap_error(&ctx.env, spec, e)` then
    /// `set_var(handler_id, ..)` — exactly what `Qop::update` does. The
    /// catch handler that READS that variable is always a separate
    /// kernel (next cycle), so there's no read-after-write hazard.
    /// `Kernel::new` constructs a `QopDeliverApply` carrying the handler
    /// BindId + the `?`'s spec (for the error's position/origin).
    QopDeliver { handler_id: crate::BindId, spec: crate::expr::Expr },
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

// ─── Kernel ABI layout — single source of truth ──────────────────
//
// The kernel calling convention is the unified Value ABI
// (`design/unified_value_abi.md`): parameters are laid out at the
// JIT/runtime boundary in SOURCE order — `KernelSig::params`' vec
// order IS the ABI order — with a uniform two-word `(disc, payload)`
// footprint per param, both words the genuine netidx `Value`
// encoding (TAINT/STALE riding the disc's tag byte). Returns are the
// same two-word pair.
//
// Every ABI site — the JIT signature builder, the uniform wrapper's
// slot unpacker, the kernel entry-block binder, and the runtime arg
// packer — derives its ordering and wire footprint from
// `KernelSig::abi_params` / `abi_return` / `abi_param_wire_slots`
// rather than re-deriving an order. Keeping it in one place is what
// makes a mistake a compile error (a new `AbiParamKind` variant
// forces every match) instead of a silent bit-reinterpretation at
// one drifted site. Foreign callers, if ever wanted, get a separate
// generated C-ABI wrapper — they don't constrain this internal
// convention.

/// The kind of a single kernel parameter, in enough detail to drive
/// both the wire encoding (how many machine words, what CLIF type)
/// and the env-slot binding (which `bind_*` the entry code calls).
/// Wire shape is uniform — every kind is a two-word `(disc, payload)`
/// pair whose payload word is the genuine `Value` encoding (a
/// scalar's widened bits, `ValArray` bits, `ArcStr` bits, a
/// value-shape's payload word). The kinds stay distinct because entry
/// binding and body emission differ per kind (clone helper, local
/// kind, element accessors), not because the wire differs.
#[derive(Debug, Clone, Copy)]
pub enum AbiParamKind {
    Scalar(PrimType),
    Array,
    Tuple,
    Struct,
    Variant,
    Nullable,
    String,
    /// Bare value shape — `DateTime` / `Duration` / `Bytes` / `Map` /
    /// `Error`.
    Value,
}

impl AbiParamKind {
    /// Number of machine-word (`u64`) slots this param occupies on the
    /// wire. Every kind is now two words — `(disc, payload)` — so the
    /// disc carries #219 taint uniformly across the kernel boundary
    /// (scalars/composites/strings gained a leading disc word that the
    /// dispatch sets to `value.disc | TAINT?`).
    pub fn wire_words(self) -> usize {
        2
    }
}

/// One kernel parameter at the ABI boundary, in SOURCE order (the
/// unified single list — vec order is ABI order). `wire_slot` is the
/// starting `u64` slot index in the flat boundary layout; the param
/// spans `kind.wire_words()` consecutive slots.
#[derive(Debug, Clone, Copy)]
pub struct AbiParamDesc<'a> {
    pub name: &'a ArcStr,
    pub kind: AbiParamKind,
    pub wire_slot: usize,
    /// The source binding this param carries, when the input came from
    /// a real graph binding (region free-vars). `None` for synthetic
    /// inputs (lambda args, lifted async values). The JIT entry binder
    /// registers it on the env slot so `Ref` emission can resolve
    /// BindId-first — exact under shadowing, where basenames alias.
    pub bind_id: Option<BindId>,
}

/// The wire shape of a kernel's return value — the unified Value ABI:
/// every kernel returns the two-word genuine `(disc, payload)` Value
/// pair (a scalar's payload widened per `pack_value_to_u64`'s rules,
/// a composite's ValArray bits, a string's ArcStr bits, a value
/// shape's payload word).
#[derive(Debug, Clone, Copy)]
pub enum AbiReturn {
    Pair,
}

/// A kernel's identity: the address of its shared [`KernelSig`]
/// allocation. Every map that resolves "which kernel" — the JIT's
/// `by_kernel` cache, discovery's `callees`/`bodies`, the declare
/// phase's `funcids`, a function's `callee_refs` — keys on THIS, never
/// on the kernel's source name. Names shadow, and one polymorphic
/// lambda mints one kernel PER monomorphization, so distinct kernels
/// legitimately share a name; resolving by name binds call sites to
/// the wrong FuncId (a silent wrong answer, or a CLIF signature
/// mismatch — the audit-jul2026 findings). `fn_name` is a label for
/// symbols and diagnostics only.
pub(crate) fn kernel_key(k: &std::sync::Arc<KernelSig>) -> usize {
    std::sync::Arc::as_ptr(k) as usize
}

/// A kernel's ABI contract — everything the boundary sites (the JIT
/// signature builders, the uniform wrapper's slot unpacker, the
/// kernel entry binder, and the runtime arg packer) need to agree
/// on, with no body attached. Built once per kernel and shared by
/// `Arc`: the runtime dispatch node and the JIT cache key off the
/// same allocation.
#[derive(Debug, Clone)]
pub struct KernelSig {
    /// Graphix-level function name. A LABEL for emitted symbols and
    /// diagnostics only — never resolve calls by it (see
    /// [`kernel_key`]).
    pub fn_name: ArcStr,
    /// The kernel's parameters in SOURCE order — the unified single
    /// list. Vec order IS the ABI order (see [`KernelParam`]). Each
    /// is also visible as a local in the body.
    pub params: Vec<KernelParam>,
    /// Function-typed parameters in declaration order. Distinct from
    /// `params` because the interpreter holds them in a separate
    /// fn-args table (the value is a `LambdaDef`, not a primitive).
    /// a DynCall's `fn_index` indexes into this table.
    pub fn_params: Vec<FnParam>,
    /// LIFTED connect-target inputs (sorted by `BindId`): let-bound
    /// reactive accumulators routed in as feeders whose IDENTITY is
    /// per-INSTANCE data, not code. The first `lifted.len()` words of
    /// the per-instance state buffer hold each target's `BindId`
    /// (written at construction). Emission reserves these words
    /// (`state_next` starts past them) and `emit_connect_node` loads
    /// the write target from them.
    pub lifted: Vec<BindId>,
    pub return_type: Type,
    /// True iff the body contains a self-tail-call. Backends wrap the
    /// body in `loop { ... }` (Rust) or a back-edge to the entry block
    /// (CLIF) accordingly.
    pub has_tail_loop: bool,
}

/// One registered owner of a per-slot state CHAIN (see
/// `BodyCx::open_slot_tables` / `emit_site_block`): the word at `rel`
/// (an absolute index into the instance state buffer, or a
/// block-relative index inside a call-site block / [`SiteLeaf`]) owns
/// a boxed `Vec<u64>` with `own_levels` directory levels below it;
/// `leaf` describes the chain's BOTTOM table when its entries are
/// per-slot call-site BLOCKS rather than plain selection words. The
/// runtime free (`emit_helpers::free_slot_chain`) and the resize
/// helpers walk exactly this structure, recursively.
#[derive(Debug, Clone)]
pub struct SiteAnchor {
    pub rel: u32,
    pub own_levels: u32,
    pub leaf: Option<std::sync::Arc<SiteLeaf>>,
    /// REPLAY-kind chain: `Kernel::reset_replay`/`sleep` free the
    /// chain and null the anchor (the emitted code rebuilds fresh —
    /// zero = no history), so a per-slot interior-bottom cache can't
    /// bridge evaluation frames — the chain twin of
    /// `replay_state_words`. Selection-memory chains stay `false`
    /// (semantic state, survives per ruling; only `Drop` frees them).
    pub reset: bool,
}

/// A chain leaf whose entries are per-slot CALL-SITE BLOCKS: `stride`
/// words per slot ordinal, `anchors` naming the in-block words that
/// own further chains (a callee's own loop-select tables and
/// sub-call-site blocks — the recursion that lets
/// callee-in-loop-in-callee compositions free exactly).
#[derive(Debug, Clone)]
pub struct SiteLeaf {
    pub stride: u32,
    pub anchors: std::sync::Arc<[SiteAnchor]>,
}

/// The number of leading `u64` wire slots reserved before the parameter
/// list, carrying per-kernel cycle context. Every kernel — even a
/// zero-param constant-only one — carries them, so the ABI is uniform.
/// The single source of truth for the offset: [`KernelSig::abi_params`]
/// starts its wire-slot scan here and every load/pack site reserves
/// this many leading words.
///
/// Slot 0 is the `event.init` flag (1 = the kernel's init cycle), read
/// by every fused constant to gate its
/// [`emit::STALE`](crate::fusion::emit) bit (a constant fires only at
/// init).
///
/// Slot 1 is the per-kernel-INSTANCE state pointer (`*mut u64`, 0 when
/// the kernel claimed no state — `WrappedKernel::state_words == 0`): a
/// zero-initialized buffer owned by the invoking runtime `Kernel` node,
/// giving root-body emission sites one word each of cross-invocation
/// memory (exact HOF resize detection, select selection memory —
/// `design/kernel_instance_state.md`). Cross-kernel
/// calls forward the caller's pointer for signature uniformity, but
/// only the region parent's ROOT body may claim words (a callee is
/// reached from arbitrarily many call sites, whose claims would alias).
///
/// Slot 2 is the PER-CALL-SITE state block pointer (`*mut u64`): a
/// CALLEE body's cross-invocation memory (select selection memory,
/// loop slot-table anchors), sized by the callee's own claims
/// (`site_layout`) and supplied by each CALLER from its own storage —
/// static instance words at a root call site, per-slot chain-leaf
/// blocks at an in-loop call site — so one compiled body gets
/// per-call-site instance state, matching the node-walk's
/// per-CallSite Apply instances (`design/kernel_instance_state.md`,
/// "Per-call-site state blocks"). `0` for region parents (the wrapper
/// packs it), for callees that claim nothing, and on RECURSIVE
/// back-edges (a fresh transient activation in the node-walk; for a
/// single-shot activation fresh memory ≡ no memory) — every
/// site-block consumer null-guards its base.
pub(crate) const CTX_WIRE_SLOTS: usize = 3;

impl KernelSig {
    /// Iterate the kernel's parameters in ABI order — the `params`
    /// vec's SOURCE order, each tagged with its wire-slot offset. This
    /// is THE definition of the parameter calling convention — every
    /// ABI site consumes it rather than re-deriving an order. Wire
    /// slots start after the [`CTX_WIRE_SLOTS`] leading cycle-context
    /// words.
    pub fn abi_params(&self) -> impl Iterator<Item = AbiParamDesc<'_>> {
        self.params.iter().enumerate().map(|(i, p)| AbiParamDesc {
            name: &p.name,
            kind: p.kind.abi(),
            wire_slot: CTX_WIRE_SLOTS + 2 * i,
            bind_id: p.bind_id,
        })
    }

    /// Total number of `u64` wire slots the boundary buffer occupies —
    /// the [`CTX_WIRE_SLOTS`] leading cycle-context words plus two per
    /// param (`disc`, `payload`).
    pub fn abi_param_wire_slots(&self) -> usize {
        CTX_WIRE_SLOTS + 2 * self.params.len()
    }

    /// Total wire slots the dispatch packs and the wrapper unpacks.
    /// Equals [`Self::abi_param_wire_slots`] — the validity bitmask is
    /// gone (taint rides in each param's disc word, #219).
    pub fn abi_wire_slots_total(&self) -> usize {
        self.abi_param_wire_slots()
    }

    /// The wire shape of this kernel's return value, or `None` for the
    /// invalid bare-`Null` return (fusion must widen to `Nullable<T>`
    /// before producing). Callers translate `None` into their own
    /// error with context.
    pub fn abi_return(&self, reg: &AbstractRegistry) -> Option<AbiReturn> {
        match abi_kind(reg, &self.return_type)? {
            AbiKind::Null => None,
            _ => Some(AbiReturn::Pair),
        }
    }
}

// ─── Cross-kernel call signature ─────────────────────────────────

/// Caller-side signature of a successfully-built lambda kernel. When
/// one fused kernel calls another, the call site marshals its args
/// against this (the types here were resolved + frozen at BUILD time,
/// so they are the authority — freezing caller-side node types
/// re-rejects abstract Refs, #218).
#[derive(Debug, Clone)]
pub struct KnownFusedFn {
    /// Flat per-input types in slot order: formal args first, then
    /// closure-converted captures.
    pub arg_types: Vec<Type>,
    /// Return type.
    pub return_type: Type,
    /// The `let` binding this kernel was built from, when known —
    /// names shadow, ids don't, so a name-resolved call must carry a
    /// matching fnode `Ref` id. Without the check, a body call to a
    /// shadowed same-name outer lambda (`let f = …; let f = |n|
    /// f(n) * 2`) resolves against the kernel ITSELF (#206: infinite
    /// native self-call, stack overflow).
    pub self_bind: Option<crate::BindId>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use triomphe::Arc;

    fn i64_t() -> Type {
        Type::Primitive(Typ::I64.into())
    }

    /// A deeply-nested but FINITE, non-recursive type must freeze. The old
    /// fixed depth cap (16) incremented on every structural level and so
    /// silently rejected it — a predictable-performance bug. Cycle
    /// detection keys on expansion identity, so structural depth no longer
    /// trips it.
    #[test]
    fn deep_finite_type_freezes() {
        let reg = AbstractRegistry::default();
        let mut t = i64_t();
        for _ in 0..40 {
            t = Type::Array(Arc::new(t));
        }
        assert!(
            freeze_for_abi(&reg, &t).is_some(),
            "a 40-deep nested array is finite and should freeze"
        );
    }

    /// A self-referential abstract type must (a) TERMINATE — not hang or
    /// stack-overflow (the test hanging would itself be the failure) — and
    /// (b) freeze to a FINITE form whose recurrence is an OPAQUE LEAF:
    /// `(Abstract(id), i64)` with the inner occurrence unexpanded.
    #[test]
    fn recursive_abstract_freezes_to_opaque_leaf() {
        let mut reg = AbstractRegistry::default();
        let id = AbstractId::new();
        // Concrete impl references its own abstract id: `(Self, i64)`.
        let rec_impl = Type::Tuple(Arc::from_iter([
            Type::Abstract { id, params: Arc::from_iter(Vec::<Type>::new()) },
            i64_t(),
        ]));
        reg.insert(id, rec_impl);
        let t = Type::Abstract { id, params: Arc::from_iter(Vec::<Type>::new()) };
        let frozen = freeze_for_abi(&reg, &t)
            .expect("a recursive abstract freezes finitely (and must not loop)");
        match &frozen {
            Type::Tuple(slots) => {
                assert!(
                    matches!(&slots[0], Type::Abstract { id: leaf, .. } if *leaf == id),
                    "the recurrence must stay an unexpanded opaque leaf, got {frozen}"
                );
                assert_eq!(slots[1], i64_t());
            }
            other => panic!("expected the expanded tuple, got {other}"),
        }
    }

    /// MUTUAL recursion (abstract A's rep contains B, B's contains A) must
    /// also terminate — the cons-list carries BOTH ids on the path — and
    /// freeze with A's re-occurrence as the opaque leaf.
    #[test]
    fn mutually_recursive_abstracts_terminate() {
        let mut reg = AbstractRegistry::default();
        let a = AbstractId::new();
        let b = AbstractId::new();
        let abst = |id| Type::Abstract { id, params: Arc::from_iter(Vec::<Type>::new()) };
        reg.insert(a, Type::Tuple(Arc::from_iter([abst(b), i64_t()])));
        reg.insert(b, Type::Tuple(Arc::from_iter([abst(a), i64_t()])));
        let frozen = freeze_for_abi(&reg, &abst(a))
            .expect("mutually-recursive abstracts freeze finitely (and must terminate)");
        // A → (B, i64) → ((A-leaf, i64), i64)
        match &frozen {
            Type::Tuple(slots) => match &slots[0] {
                Type::Tuple(inner) => {
                    assert!(
                        matches!(&inner[0], Type::Abstract { id, .. } if *id == a),
                        "A must recur as an opaque leaf, got {frozen}"
                    );
                }
                other => panic!("expected B's expansion, got {other}"),
            },
            other => panic!("expected A's expansion, got {other}"),
        }
    }

    /// NON-regular recursion with a params-BLIND key: `ExpandKey::
    /// Abstract` carries only the id, so a params-growing abstract
    /// recurrence is caught at the FIRST re-occurrence and freezes to
    /// a leaf carrying the GROWN (frozen) params — terminating, never
    /// chasing the growth. (The chain backstop exists for Ref keys,
    /// which carry params and so never self-match under growth.)
    #[test]
    fn non_regular_recursive_abstract_freezes_at_first_recurrence() {
        let mut reg = AbstractRegistry::default();
        let id = AbstractId::new();
        // type T<'a> = (T<Array<'a>>, i64) — the registry substitutes 'a.
        let tv = crate::typ::TVar::empty_named(arcstr::literal!("a"));
        let grown = Type::Abstract {
            id,
            params: Arc::from_iter([Type::Array(Arc::new(Type::TVar(tv.clone())))]),
        };
        let body = Type::Tuple(Arc::from_iter([grown, i64_t()]));
        reg.insert_scoped(
            id,
            Arc::from_iter([arcstr::literal!("a")]),
            body,
            crate::expr::ModPath::root(),
        );
        let t = Type::Abstract { id, params: Arc::from_iter([i64_t()]) };
        let frozen = freeze_for_abi(&reg, &t)
            .expect("params-blind abstract recursion terminates at first recurrence");
        match &frozen {
            Type::Tuple(slots) => match &slots[0] {
                Type::Abstract { id: leaf, params } => {
                    assert_eq!(*leaf, id);
                    assert!(
                        matches!(&params[0], Type::Array(e) if **e == i64_t()),
                        "the leaf carries the grown, frozen params, got {frozen}"
                    );
                }
                other => panic!("expected the opaque leaf, got {other}"),
            },
            other => panic!("expected the expanded tuple, got {other}"),
        }
    }

    /// `abi_kind` is the third concretizer with abstract-expansion
    /// recursion: an option-of-self (`type A = [A, null]`) must terminate
    /// (the new cycle guard) and yield `None` — without the guard this
    /// loops forever.
    #[test]
    fn abi_kind_option_of_self_terminates() {
        let mut reg = AbstractRegistry::default();
        let a = AbstractId::new();
        let abst = Type::Abstract { id: a, params: Arc::from_iter(Vec::<Type>::new()) };
        let opt_self =
            Type::Set(Arc::from_iter([abst.clone(), Type::Primitive(Typ::Null.into())]));
        reg.insert(a, opt_self);
        assert!(
            abi_kind(&reg, &abst).is_none(),
            "option-of-self abstract has no flat ABI kind (and must terminate)"
        );
    }
}
