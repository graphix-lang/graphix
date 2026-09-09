//! The kernel ABI contract: the body-free half of the fusion/JIT boundary.
//!
//! Derived from netidx [`Type`]s: the register-scalar set ([`PrimType`]),
//! the runtime shape classifier ([`abi_kind`] / [`AbiKind`]), the
//! encodability gate ([`freeze_for_abi`]), and [`KernelSig`], the
//! source-ordered parameter list every ABI site derives its wire layout
//! from. [`KnownFusedFn`] is the caller-side cross-kernel call signature.

use crate::{
    BindId,
    typ::{Type, TypeRef},
};
use arcstr::ArcStr;
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use triomphe::Arc;

/// The primitive types a kernel holds in a register: the numerics and bool.
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

    /// The netidx [`Typ`] for this primitive; the inverse of [`PrimType::from_typ`].
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

    /// The [`PrimType`] of a single-bit numeric or bool primitive type,
    /// dereferencing bound TVars; `None` for anything else.
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

/// The top-level runtime shape of a `Type` at the ABI boundary. Not
/// nested: element, field and payload types are read back out of the
/// classified [`Type`] by the structure accessors ([`array_elem`],
/// [`tuple_slots`], [`struct_fields`], [`variant_cases`], [`nullable_inner`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AbiKind {
    Scalar(PrimType),
    Array,
    Tuple,
    Struct,
    String,
    Variant,
    Nullable,
    /// An opaque two-word `Value`: `DateTime`, `Duration`, `Bytes`, `Map`,
    /// `Error`, `List`, and abstract types. The carried `Type` says which.
    Value,
    /// `Type::Bottom`. Not a valid kernel parameter.
    Unit,
    /// The `null` type. Not a valid kernel parameter.
    Null,
}

impl AbiKind {
    /// The [`AbiParamKind`] for this shape; `None` for `Unit`/`Null`.
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
fn is_single_prim(t: &Type, which: Typ) -> bool {
    t.with_deref(|r| match r {
        Some(Type::Primitive(p)) => p.contains(which) && p.iter().count() == 1,
        _ => false,
    })
}

/// The top-level [`AbiKind`] of a `Type`; nested elements are not
/// checked (see [`freeze_for_abi`]). `None` for shapes with no kernel
/// encoding: an unbound TVar, `Fn`, `Any`, `ByRef`, a `Set` that is
/// neither option/result nor all-variant, a `Ref` with an empty
/// resolution cell, or a recursive named type.
pub fn abi_kind(t: &Type) -> Option<AbiKind> {
    abi_kind_d(t, None)
}

fn abi_kind_d(t: &Type, seen: Option<&Seen>) -> Option<AbiKind> {
    // The TVar lock is not reentrant: clone out of the guard before recursing.
    let resolved = t.with_deref(|r| r.cloned());
    {
        let resolved = resolved.as_ref()?;
        match resolved {
            Type::Bottom => return Some(AbiKind::Unit),
            Type::Map { .. } => return Some(AbiKind::Value),
            Type::Error(_) => return Some(AbiKind::Value),
            Type::Array(_) => return Some(AbiKind::Array),
            Type::List(_) => return Some(AbiKind::Value),
            Type::Tuple(_) => return Some(AbiKind::Tuple),
            Type::Struct(_) => return Some(AbiKind::Struct),
            Type::Variant(_, _) => return Some(AbiKind::Variant),
            Type::Abstract { .. } => return Some(AbiKind::Value),
            // A constructor application whose ctor is a bound TVar is
            // unreduced by typecheck; classify its filled form.
            Type::App(c, a) => {
                let cd = c.with_deref(|t| t.cloned());
                let ad = a.with_deref(|t| t.cloned());
                return match (cd, ad) {
                    (Some(cd), Some(ad)) => {
                        cd.fill_hole(&ad).and_then(|r| abi_kind_d(&r, seen))
                    }
                    _ => None,
                };
            }
            Type::Ref(tr) => {
                let key = ExpandKey::Ref(tr.clone());
                if Seen::contains(seen, &key) {
                    return None;
                }
                let expanded = tr.expand_cell();
                let node = Seen::push(seen, key);
                return expanded.as_ref().and_then(|c| abi_kind_d(c, Some(&node)));
            }
            _ => {}
        }
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
            if p.contains(Typ::Null) && p.iter().count() == 2 {
                return Some(AbiKind::Nullable);
            }
            if let Some(prim) = PrimType::from_type(resolved) {
                return Some(AbiKind::Scalar(prim));
            }
            return None;
        }
        if let Type::Set(members) = resolved {
            if let Some(succ) = option_result_success(members) {
                return succ.and_then(|s| abi_kind_d(s, seen)).map(|_| AbiKind::Nullable);
            }
            let all_variants = members
                .iter()
                .all(|m| m.with_deref(|r| matches!(r, Some(Type::Variant(_, _)))));
            if all_variants {
                return Some(AbiKind::Variant);
            }
            return None;
        }
        None
    }
}

/// The success member of an option (`[T, null]`) or result (`[T, Error]`)
/// set; `None` for any other member list. A null marker takes precedence:
/// `[null, Error<T>]` is the option of `Error<T>`.
fn option_result_success(members: &[Type]) -> Option<Option<&Type>> {
    if members.len() != 2 {
        return None;
    }
    let is_null = |m: &Type| is_single_prim(m, Typ::Null);
    let is_err = |m: &Type| m.with_deref(|r| matches!(r, Some(Type::Error(_))));
    match (is_null(&members[0]), is_null(&members[1])) {
        (true, false) => return Some(Some(&members[1])),
        (false, true) => return Some(Some(&members[0])),
        _ => {}
    }
    match (is_err(&members[0]), is_err(&members[1])) {
        (true, false) => Some(Some(&members[1])),
        (false, true) => Some(Some(&members[0])),
        _ => None,
    }
}

/// Which marker a [`AbiKind::Nullable`] shape carries: `Some(false)` for
/// the option forms (the non-success value is null), `Some(true)` for the
/// result form (the non-success value is an error, whose disc is not
/// null, so a success test must be positive against T's own disc).
/// `None` for a non-Nullable shape.
pub fn nullable_error_marked(t: &Type) -> Option<bool> {
    let resolved = t.with_deref(|r| r.cloned())?;
    match &resolved {
        Type::Primitive(p) if p.contains(Typ::Null) && p.iter().count() == 2 => {
            Some(false)
        }
        Type::Set(members) => {
            option_result_success(members)?;
            let is_null = |m: &Type| is_single_prim(m, Typ::Null);
            Some(!is_null(&members[0]) && !is_null(&members[1]))
        }
        _ => None,
    }
}

/// One named-type expansion on the path from the root, for cycle
/// detection while concretizing a type. Structural nesting is not an
/// expansion; only following a `Ref` to its definition is.
#[derive(Clone, PartialEq)]
pub(crate) enum ExpandKey {
    Ref(TypeRef),
}

/// Stack-allocated cons-list of the [`ExpandKey`]s on the current path
/// from the root. Only a `Ref` expansion extends it, so a recurring key
/// means true type recursion, not structural depth.
pub(crate) struct Seen<'a> {
    key: ExpandKey,
    /// [`expand_key_fp`] of `key`; membership compares it before full equality.
    fp: u64,
    len: usize,
    prev: Option<&'a Seen<'a>>,
}

/// Fingerprint of an [`ExpandKey`]; a collision only costs a full-equality check.
pub(crate) fn expand_key_fp(key: &ExpandKey) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut h = ahash::AHasher::default();
    let ExpandKey::Ref(tr) = key;
    tr.scope.hash(&mut h);
    tr.name.hash(&mut h);
    tr.params.len().hash(&mut h);
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

    /// Like [`Self::contains`], but yields the matched key: the outer
    /// occurrence of a recursive ref, whose resolution cell is filled
    /// where the inner occurrence's may not be (keys compare cell-blind).
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

/// Bounds the expansion chain for non-regular recursion
/// (`type T<'a> = T<Array<'a>>`), whose keys never repeat.
const MAX_FREEZE_EXPANSIONS: usize = 256;

/// The kernel-ABI encodability gate: the fully concrete (TVar-free)
/// form of `t` over the fusable subset, or `None` if any part has no
/// kernel encoding. Accept/reject matches [`abi_kind`] at each level;
/// `Map`/`Error` stop the recursion (opaque `Value` on the wire).
///
/// A recursive named type freezes to an opaque leaf: the recurring
/// `Ref` stays unexpanded, so the output is finite and the value
/// crosses the boundary as a 2-word opaque. Consumers needing flat
/// payload structure refuse the leaf per node. Non-regular recursion
/// is cut by [`MAX_FREEZE_EXPANSIONS`]; structural depth is unbounded.
pub fn freeze_for_abi(t: &Type) -> Option<Type> {
    freeze_for_abi_d(t, None)
}

fn freeze_for_abi_d(t: &Type, seen: Option<&Seen>) -> Option<Type> {
    crate::stack::ensure_sufficient(|| freeze_for_abi_d_inner(t, seen))
}

fn freeze_for_abi_d_inner(t: &Type, seen: Option<&Seen>) -> Option<Type> {
    // The TVar lock is not reentrant: clone out of the guard before recursing.
    let resolved = t.with_deref(|r| r.cloned());
    {
        let resolved = resolved.as_ref()?;
        match resolved {
            Type::Bottom => Some(Type::Bottom),
            Type::Map { .. } => Some(resolved.clone()),
            Type::Error(_) => Some(resolved.clone()),
            Type::Primitive(p) => {
                if is_single_prim(resolved, Typ::String)
                    || is_single_prim(resolved, Typ::Null)
                    || is_single_prim(resolved, Typ::DateTime)
                    || is_single_prim(resolved, Typ::Duration)
                    || is_single_prim(resolved, Typ::Bytes)
                {
                    return Some(Type::Primitive(*p));
                }
                if p.contains(Typ::Null) && p.iter().count() == 2 {
                    let other = p.iter().find(|f| *f != Typ::Null)?;
                    if other == Typ::String || PrimType::from_typ(other).is_some() {
                        return Some(Type::Primitive(*p));
                    }
                    return None;
                }
                PrimType::from_type(resolved).map(|_| Type::Primitive(*p))
            }
            Type::Array(inner) => {
                let inner = freeze_for_abi_d(inner, seen)?;
                Some(Type::Array(Arc::new(inner)))
            }
            Type::List(inner) => {
                let inner = freeze_for_abi_d(inner, seen)?;
                Some(Type::List(Arc::new(inner)))
            }
            Type::Tuple(elems) => {
                let frozen: Option<LPooled<Vec<Type>>> =
                    elems.iter().map(|e| freeze_for_abi_d(e, seen)).collect();
                let mut frozen = frozen?;
                Some(Type::Tuple(Arc::from_iter(frozen.drain(..))))
            }
            Type::Struct(fields) => {
                let frozen: Option<LPooled<Vec<(ArcStr, Type)>>> = fields
                    .iter()
                    .map(|(n, ft)| freeze_for_abi_d(ft, seen).map(|t| (n.clone(), t)))
                    .collect();
                let mut frozen = frozen?;
                Some(Type::Struct(Arc::from_iter(frozen.drain(..))))
            }
            Type::Variant(tag, payloads) => {
                let frozen: Option<LPooled<Vec<Type>>> =
                    payloads.iter().map(|p| freeze_for_abi_d(p, seen)).collect();
                let mut frozen = frozen?;
                Some(Type::Variant(tag.clone(), Arc::from_iter(frozen.drain(..))))
            }
            Type::Set(members) => {
                if let Some(succ_opt) = option_result_success(members) {
                    let succ = succ_opt?;
                    let succ_idx = if std::ptr::eq(&members[0], succ) { 0 } else { 1 };
                    let frozen_succ = freeze_for_abi_d(succ, seen)?;
                    let m0 = if succ_idx == 0 {
                        frozen_succ.clone()
                    } else {
                        members[0].clone()
                    };
                    let m1 = if succ_idx == 1 { frozen_succ } else { members[1].clone() };
                    return Some(Type::Set(Arc::from_iter([m0, m1])));
                }
                let frozen: Option<LPooled<Vec<Type>>> = members
                    .iter()
                    .map(|m| {
                        let m = m.with_deref(|r| r.cloned());
                        match m {
                            Some(Type::Variant(tag, payloads)) => {
                                let fp: Option<LPooled<Vec<Type>>> = payloads
                                    .iter()
                                    .map(|p| freeze_for_abi_d(p, seen))
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
                let frozen: Option<LPooled<Vec<Type>>> =
                    params.iter().map(|p| freeze_for_abi_d(p, seen)).collect();
                let mut frozen = frozen?;
                Some(Type::Abstract { id: *id, params: Arc::from_iter(frozen.drain(..)) })
            }
            // A constructor application whose ctor is a bound TVar is
            // unreduced by typecheck; freeze its filled form.
            Type::App(c, a) => {
                let cd = c.with_deref(|t| t.cloned());
                let ad = a.with_deref(|t| t.cloned());
                match (cd, ad) {
                    (Some(cd), Some(ad)) => {
                        let r = cd.fill_hole(&ad)?;
                        freeze_for_abi_d(&r, seen)
                    }
                    _ => None,
                }
            }
            // A recurring ref becomes the opaque leaf, built on the outer
            // (cell-filled) occurrence with its params frozen so the
            // leaf stays TVar-free.
            Type::Ref(tr) => {
                let key = ExpandKey::Ref(tr.clone());
                if let Some(matched) = Seen::find(seen, &key) {
                    let ExpandKey::Ref(outer) = matched;
                    let frozen: Option<LPooled<Vec<Type>>> =
                        tr.params.iter().map(|p| freeze_for_abi_d(p, seen)).collect();
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
                freeze_for_abi_d(&expanded, Some(&node))
            }
            _ => None,
        }
    }
}

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

/// The `(tag, payload-types)` cases of a `Type::Variant` or a `Type::Set`
/// of single-Variant members; `None` otherwise.
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

/// [`freeze_for_abi`], retrying on an un-flattened union through
/// `resolve_tvars().normalize()`. The retry must normalize a deep clone,
/// never `t`: `normalize` writes bindings back into shared TVar cells,
/// and attempting fusion must not change the program's static types.
pub fn freeze_for_abi_normalized(t: &Type) -> Option<Type> {
    freeze_for_abi(t).or_else(|| freeze_for_abi(&t.resolve_tvars().normalize()))
}

/// The frozen success type `T` of a [`AbiKind::Nullable`] shape
/// (`[T, null]`, `[T, Error]`, or the collapsed `T | null` primitive);
/// `None` for any other shape.
pub fn nullable_inner(t: &Type) -> Option<Type> {
    let resolved = t.with_deref(|r| r.cloned());
    {
        let resolved = resolved.as_ref()?;
        match resolved {
            Type::Primitive(p) if p.contains(Typ::Null) && p.iter().count() == 2 => {
                let other = p.iter().find(|f| *f != Typ::Null)?;
                if other == Typ::String {
                    return Some(Type::Primitive(Typ::String.into()));
                }
                PrimType::from_typ(other).map(|pt| Type::Primitive(pt.to_typ().into()))
            }
            Type::Set(members) => {
                let succ = option_result_success(members)??;
                freeze_for_abi(succ)
            }
            _ => None,
        }
    }
}

/// The [`PrimType`] of a register-scalar `Type`; `None` otherwise.
pub fn scalar_prim(t: &Type) -> Option<PrimType> {
    match abi_kind(t) {
        Some(AbiKind::Scalar(p)) => Some(p),
        _ => None,
    }
}

/// The element [`PrimType`] of an `Array<P>` with a scalar element; `None` otherwise.
pub fn array_scalar_prim(t: &Type) -> Option<PrimType> {
    array_elem(t).and_then(|e| scalar_prim(e))
}

/// True for types whose kernel representation is a two-register `Value`:
/// `Variant`, `Nullable`, and the [`AbiKind::Value`] group.
pub fn is_value_shape(t: &Type) -> bool {
    matches!(abi_kind(t), Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value))
}

/// A plain scalar `Type` for a [`PrimType`].
pub fn prim_type(p: PrimType) -> Type {
    Type::Primitive(p.to_typ().into())
}

/// The `string` type.
pub fn string_type() -> Type {
    Type::Primitive(Typ::String.into())
}

/// The `null` type.
pub fn null_type() -> Type {
    Type::Primitive(Typ::Null.into())
}

/// The unit type, `Type::Bottom`.
pub fn unit_type() -> Type {
    Type::Bottom
}

/// The `bytes` type.
pub fn bytes_type() -> Type {
    Type::Primitive(Typ::Bytes.into())
}

/// The `datetime` type.
pub fn datetime_type() -> Type {
    Type::Primitive(Typ::DateTime.into())
}

/// The `duration` type.
pub fn duration_type() -> Type {
    Type::Primitive(Typ::Duration.into())
}

/// A placeholder `Map<null, null>` type; codegen only reads its
/// [`AbiKind::Value`] classification, never its key/value types.
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

/// A variant `Type` from a `(tag, payload-types)` case list; the inverse
/// of [`variant_cases`].
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

/// The `[inner, null]` option type.
pub fn nullable_type(inner: Type) -> Type {
    Type::Set(triomphe::Arc::from_iter([inner, null_type()]))
}

/// One parameter of a fused kernel. The `KernelSig::params` order is
/// the ABI order, the packer's arg order and the tail-rebind order.
/// `bind_id` is `None` for synthetic inputs (lambda formals).
#[derive(Debug, Clone)]
pub struct KernelParam {
    pub name: ArcStr,
    pub kind: ParamKind,
    pub bind_id: Option<BindId>,
}

/// The shape of one kernel parameter with the static metadata the body
/// emitter and packer need. The wire shape is a two-word Value pair
/// for every kind.
#[derive(Debug, Clone)]
pub enum ParamKind {
    Scalar(PrimType),
    Array {
        elem: Type,
    },
    Tuple {
        elems: Vec<Type>,
    },
    /// Fields sorted by name.
    Struct {
        fields: Vec<(ArcStr, Type)>,
    },
    /// Runtime index 0 is the tag string; payloads start at index 1.
    Variant {
        cases: Vec<(ArcStr, Vec<Type>)>,
    },
    /// `elem` is the non-null inner type.
    Nullable {
        elem: Type,
    },
    String,
    /// Carries the full `Type` so a `Ref` read re-wraps correctly.
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

/// The [`PrimType`] of a scalar [`Value`]; variable-width integers
/// collapse to their fixed-width form. `None` for a non-scalar.
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

/// The kind of a kernel parameter at the wire. Every kind is a two-word
/// `(disc, payload)` pair in the netidx `Value` encoding (TAINT/STALE in
/// the disc's tag byte); the kinds differ in entry binding and body
/// emission, not on the wire.
#[derive(Debug, Clone, Copy)]
pub enum AbiParamKind {
    Scalar(PrimType),
    Array,
    Tuple,
    Struct,
    Variant,
    Nullable,
    String,
    Value,
}

impl AbiParamKind {
    /// Number of `u64` wire slots this param occupies.
    pub fn wire_words(self) -> usize {
        2
    }
}

/// One kernel parameter at the ABI boundary. `wire_slot` is its first
/// `u64` slot; it spans `kind.wire_words()` consecutive slots.
#[derive(Debug, Clone, Copy)]
pub struct AbiParamDesc<'a> {
    pub name: &'a ArcStr,
    pub kind: AbiParamKind,
    pub wire_slot: usize,
    /// The source binding, `None` for synthetic inputs. `Ref` emission
    /// resolves by it first, since basenames alias under shadowing.
    pub bind_id: Option<BindId>,
}

/// The wire shape of a kernel's return value: a two-word `(disc, payload)` pair.
#[derive(Debug, Clone, Copy)]
pub enum AbiReturn {
    Pair,
}

/// A kernel's identity: the address of its shared [`KernelSig`]. Every
/// "which kernel" map keys on this, never on `fn_name`: names shadow, and
/// a polymorphic lambda mints one kernel per monomorphization.
pub(crate) fn kernel_key(k: &std::sync::Arc<KernelSig>) -> usize {
    std::sync::Arc::as_ptr(k) as usize
}

/// A kernel's ABI contract, shared by `Arc` between the runtime dispatch
/// node and the JIT cache. The address is the kernel's identity
/// ([`kernel_key`]).
#[derive(Debug)]
pub struct KernelSig {
    /// A label for emitted symbols and diagnostics; never resolve calls by it.
    pub fn_name: ArcStr,
    /// The parameters in source order, which is the ABI order.
    pub params: Vec<KernelParam>,
    pub return_type: Type,
    /// True iff the body contains a self-tail-call.
    pub has_tail_loop: bool,
    /// Formal positions with no param slot: fn-typed formals whose every
    /// use is a statically-resolved call and which every self-call
    /// forwards unchanged. Callers skip the arg entirely.
    pub skipped_args: Vec<u32>,
    /// Formal positions every self-call forwards unchanged; the tail
    /// loop never rebinds them, so their kind is not loop-gated.
    pub tail_invariant: Vec<u32>,
    /// Set once the body's CLIF define completes. Callees define before
    /// callers, so `false` at a call site means a self/back-edge call.
    pub defined: std::sync::atomic::AtomicBool,
    /// This body's call-site block size in words, filled once the layout
    /// is final. A self-call reads it at run time because the size is
    /// unknown while the body is still being emitted.
    pub site_desc: std::sync::atomic::AtomicU64,
}

impl Clone for KernelSig {
    fn clone(&self) -> Self {
        use std::sync::atomic::{AtomicBool, Ordering::Relaxed};
        KernelSig {
            fn_name: self.fn_name.clone(),
            params: self.params.clone(),
            return_type: self.return_type.clone(),
            has_tail_loop: self.has_tail_loop,
            skipped_args: self.skipped_args.clone(),
            tail_invariant: self.tail_invariant.clone(),
            defined: AtomicBool::new(self.defined.load(Relaxed)),
            site_desc: std::sync::atomic::AtomicU64::new(self.site_desc.load(Relaxed)),
        }
    }
}

/// The word at `rel` holds the root of a per-activation block tree: a
/// self-call's activation depth is a run-time fact, so its callee block
/// is a lazily allocated child (`graphix_site_child_block`) rather than
/// a static carve-out. Keyed by call site, never by depth (siblings at
/// one depth are distinct activations). `words`/`slots` describe the
/// child blocks, identical at every level.
#[derive(Debug, Clone)]
pub struct SelfBlock {
    pub rel: u32,
    pub words: u32,
    pub slots: std::sync::Arc<[u32]>,
}

/// One owner of a per-slot state chain: the word at `rel` (absolute in
/// the instance buffer, or block-relative inside a call-site block) owns
/// a boxed `Vec<u64>` with `own_levels` directory levels below it;
/// `leaf` describes the bottom table when its entries are call-site
/// blocks. The chain is per-position state: it survives frames and
/// sleep, is prefix-retained across resizes, and is freed by `Drop`.
#[derive(Debug, Clone)]
pub struct SiteAnchor {
    pub rel: u32,
    pub own_levels: u32,
    pub leaf: Option<std::sync::Arc<SiteLeaf>>,
}

/// A chain leaf whose entries are per-slot call-site blocks: `stride`
/// words per slot, `anchors` naming the in-block words owning further chains.
#[derive(Debug, Clone)]
pub struct SiteLeaf {
    pub stride: u32,
    pub anchors: std::sync::Arc<[SiteAnchor]>,
}

/// Leading `u64` wire slots before the parameter list, present in every
/// kernel.
///
/// Slot 0 is the cycle-context word: bit 0 = init (the forced view,
/// wakes included), bit 1 = quiet (a re-derivation inside a frame or
/// tail loop that is not its own init; callees inherit it), bit 2 =
/// wake (an arm's `wake_init` or the kernel's own slept bit). Genuine
/// init is `bit0 & !bit2`.
///
/// Slot 1 is the per-instance state pointer (`*mut u64`, 0 when the
/// kernel claimed no words); only the region's root body may claim
/// words, since a callee's claims would alias across call sites.
///
/// Slot 2 is the per-call-site state block pointer, sized by the callee
/// and supplied by each caller from its own storage; 0 for region
/// parents, callees that claim nothing, and recursive back-edges, so
/// every consumer null-guards it.
pub(crate) const CTX_WIRE_SLOTS: usize = 3;

impl KernelSig {
    /// The parameters in ABI order with their wire-slot offsets; every
    /// ABI site derives its layout from this, never its own order.
    pub fn abi_params(&self) -> impl Iterator<Item = AbiParamDesc<'_>> {
        self.params.iter().enumerate().map(|(i, p)| AbiParamDesc {
            name: &p.name,
            kind: p.kind.abi(),
            wire_slot: CTX_WIRE_SLOTS + 2 * i,
            bind_id: p.bind_id,
        })
    }

    /// Total `u64` wire slots: [`CTX_WIRE_SLOTS`] plus two per param.
    pub fn abi_param_wire_slots(&self) -> usize {
        CTX_WIRE_SLOTS + 2 * self.params.len()
    }

    /// Total wire slots the dispatch packs and the wrapper unpacks.
    pub fn abi_wire_slots_total(&self) -> usize {
        self.abi_param_wire_slots()
    }

    /// The wire shape of the return value; `None` for a bare `null`
    /// return, which fusion must widen to `Nullable<T>`.
    pub fn abi_return(&self) -> Option<AbiReturn> {
        match abi_kind(&self.return_type)? {
            AbiKind::Null => None,
            _ => Some(AbiReturn::Pair),
        }
    }
}

/// Caller-side signature of a built lambda kernel; a cross-kernel call
/// site marshals its args against these build-time frozen types.
#[derive(Debug, Clone)]
pub struct KnownFusedFn {
    /// Flat per-input types in slot order: formal args first, then
    /// closure-converted captures.
    pub arg_types: Vec<Type>,
    /// Return type.
    pub return_type: Type,
    /// The `let` binding this kernel was built from, when known. A call
    /// resolves as a self-call only when its `Ref` id matches; a
    /// shadowed same-name outer lambda must not.
    pub self_bind: Option<crate::BindId>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use triomphe::Arc;

    fn i64_t() -> Type {
        Type::Primitive(Typ::I64.into())
    }

    /// A deeply nested finite type freezes; only expansion identity, not
    /// structural depth, is cycle-checked.
    #[test]
    fn deep_finite_type_freezes() {
        let mut t = i64_t();
        for _ in 0..40 {
            t = Type::Array(Arc::new(t));
        }
        assert!(
            freeze_for_abi(&t).is_some(),
            "a 40-deep nested array is finite and should freeze"
        );
    }
}
