//! The kernel ABI contract: the body-free half of the fusion/JIT boundary.
//!
//! Derived from netidx [`Type`]s: the register-scalar set ([`PrimType`]),
//! the runtime shape classifier ([`abi_kind`] / [`AbiKind`]), the
//! encodability gate ([`freeze_for_abi`]), and [`KernelSig`], the
//! source-ordered parameter list every ABI site derives its wire layout
//! from.

use crate::{
    BindId,
    expr::WrittenAt,
    profile::{self, Phase},
    typ::{Type, TypeRef},
};
use ahash::AHashMap;
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use enumflags2::BitFlags;
use netidx_core::pack::{Pack as PackTrait, PackError};
use netidx_derive::Pack;
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::cell::{Cell, RefCell};
use triomphe::Arc;

/// The primitive types a kernel holds in a register: the numerics and bool.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Pack)]
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

/// True for a `Type::Primitive` carrying exactly the single bit `which`.
fn is_single_prim(t: &Type, which: Typ) -> bool {
    t.with_deref(|r| match r {
        Some(Type::Primitive(p)) => p.contains(which) && p.iter().count() == 1,
        _ => false,
    })
}

/// A union with its primitives, unions of them and typedefs naming them
/// (`Number = [Int, Real]`) folded into the one primitive set they are,
/// beside its other members; `None` when that changes nothing.
fn primitive_union(members: &[Type]) -> Option<Type> {
    fn bits(t: &Type, depth: usize, acc: &mut BitFlags<Typ>) -> bool {
        if depth > MAX_FREEZE_EXPANSIONS {
            return false;
        }
        // a member with no bits (⊥, an empty set) is no primitive
        match &t.deref_cloned() {
            Some(Type::Primitive(p)) if !p.is_empty() => {
                *acc |= *p;
                true
            }
            Some(Type::Set(ms)) if !ms.is_empty() => {
                let mut part = *acc;
                let all = ms.iter().all(|m| bits(m, depth + 1, &mut part));
                if all {
                    *acc = part
                }
                all
            }
            Some(Type::Ref(tr)) => {
                let mut part = *acc;
                let all =
                    tr.expand_cell().is_some_and(|x| bits(&x, depth + 1, &mut part));
                if all {
                    *acc = part
                }
                all
            }
            _ => false,
        }
    }
    let mut acc = BitFlags::empty();
    let mut folded = 0;
    let mut plain = true;
    let mut rest: LPooled<Vec<Type>> = LPooled::take();
    for m in members {
        if bits(m, 0, &mut acc) {
            folded += 1;
            plain &= matches!(m.deref_cloned(), Some(Type::Primitive(_)));
        } else {
            rest.push(m.clone())
        }
    }
    if folded == 0 || (folded == 1 && plain) {
        return None;
    }
    Some(match rest.is_empty() {
        true => Type::Primitive(acc),
        false => Type::Set(Arc::from_iter(
            std::iter::once(Type::Primitive(acc)).chain(rest.drain(..)),
        )),
    })
}

/// The top-level [`AbiKind`] of a `Type`; nested elements are not
/// checked (see [`freeze_for_abi`]). A `Set` is one of: a union that
/// folds to one primitive set ([`primitive_union`]), an option or a
/// result (`Nullable`), all variants (`Variant`), or all tuples, structs
/// and arrays (`Value`). `None` for shapes with no kernel encoding: an
/// unbound TVar, `Fn`, `Any`, `ByRef`, any other `Set`, a `Ref` with an
/// empty resolution cell, or a recursive named type.
pub fn abi_kind(t: &Type) -> Option<AbiKind> {
    abi_kind_d(t, None)
}

/// Whether a value of `t` crosses a kernel's boundary, as a call's
/// argument or a region's return: every encoded shape but `Unit`.
pub(crate) fn crosses_boundary(t: &Type) -> bool {
    match abi_kind(t) {
        Some(
            AbiKind::Scalar(_)
            | AbiKind::Array
            | AbiKind::Tuple
            | AbiKind::Struct
            | AbiKind::Variant
            | AbiKind::Nullable
            | AbiKind::Value
            | AbiKind::String
            | AbiKind::Null,
        ) => true,
        Some(AbiKind::Unit) | None => false,
    }
}

fn abi_kind_d(t: &Type, seen: Option<&Seen>) -> Option<AbiKind> {
    // The TVar lock is not reentrant: clone out of the guard before recursing.
    let resolved = t.deref_cloned();
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
            Type::Variant(_, _, _) => return Some(AbiKind::Variant),
            Type::Abstract { .. } => return Some(AbiKind::Value),
            // A constructor application whose ctor is a bound TVar is
            // unreduced by typecheck; classify its filled form.
            Type::App(c, a) => {
                let cd = c.deref_cloned();
                let ad = a.deref_cloned();
                return match (cd, ad) {
                    (Some(cd), Some(ad)) => {
                        cd.fill_hole(&ad).and_then(|r| abi_kind_d(&r, seen))
                    }
                    _ => None,
                };
            }
            Type::Ref(tr) => {
                if Seen::find(seen, tr).is_some() {
                    return None;
                }
                let expanded = tr.expand_cell();
                let node = Seen::push(seen, tr.clone());
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
            if nullable_prim(p).is_some() {
                return Some(AbiKind::Nullable);
            }
            if let Some(prim) = PrimType::from_type(resolved) {
                return Some(AbiKind::Scalar(prim));
            }
            if p.is_empty() {
                return None;
            }
            return Some(AbiKind::Value);
        }
        if let Type::Set(members) = resolved {
            if let Some(p) = primitive_union(members) {
                return abi_kind_d(&p, seen);
            }
            if let Some(succ) = option_result_success(members) {
                return abi_kind_d(succ, seen).map(|_| AbiKind::Nullable);
            }
            // CR claude for claude: [perf/medium] A union not covered by these three
            // rules has no kernel encoding: [`A, `B, null] (an optional enum), [`A,
            // `B(i64), null], [{x: i64}, `N], [List<i64>, Array<i64>]. Every function
            // returning or taking one node-walks. With kind markers the value tells all
            // these members apart (graphix_value_is_a), so: let a null or error member
            // beside any encodable rest be Nullable, and let a union of any marked
            // kinds and arrays ride as Value, with #[native] pins. probe:
            // design/review-2026-10-19/repro/x-kind-markers-optional-enum-no-kernel.gx
            // (x-kind-markers-06)
            // 2026-10-20 claude: Tried widening the Value rule to any members a value
            // tells apart (lists, maps, tag unions, primitives beside composites):
            // every probe in x-kind-markers-06-union-formals/ that then fused DIVERGED
            // (interp != jit), so the select lowering of type tests over a Value
            // scrutinee is not ready for those kinds; reverted. The order is: make the
            // select lowering test each member kind on a Value (graphix_value_is_a),
            // pin it, then widen this rule.
            // 2026-10-20 claude: Correction to the note above: the DIVERGENCE verdicts
            // were the probes own #[native] refusing, not wrong values. With #[native]
            // stripped and the rule widened, every probe agrees between the engines,
            // but nothing new fuses: the formal still has no kernel encoding and the
            // select refuses its scrutinee (type .. does not freeze concrete), so the
            // freeze step, this rule and the select lowering over a Value scrutinee all
            // need the change together. Reverted.
            // 2026-10-21 claude: A union of composites and null, [(i64, i64),
            // Array<i64>, null], is a case of this one: f-select-02 made the select
            // lowering destructure a composite member of any value scrutinee
            // (emit_member_cond in emit/select.rs: a tag test for an option's or a
            // result's composite, graphix_value_is_a for a written member test, the
            // node-walk's shape test for an inferred one), but that union does not
            // freeze, so its select de-fuses before any pattern is lowered. Of the
            // three changes this CR needs together, the select lowering's member tests
            // now exist for composites; the freeze step and the abi_kind rule remain.
            let all_variants = members
                .iter()
                .all(|m| m.with_deref(|r| matches!(r, Some(Type::Variant(_, _, _)))));
            if all_variants {
                return Some(AbiKind::Variant);
            }
            // a union of composites rides as the Value it is; a select
            // tests a member's shape before reading it as that member
            if members.iter().all(|m| {
                m.with_deref(|r| {
                    matches!(r, Some(Type::Tuple(_) | Type::Struct(_) | Type::Array(_)))
                })
            }) {
                return Some(AbiKind::Value);
            }
            return None;
        }
        None
    }
}

/// The member `T` of a `T | null` primitive union when `T` has a
/// register or string form: such a union is [`AbiKind::Nullable`], any
/// other a plain two-word Value.
fn nullable_prim(p: &BitFlags<Typ>) -> Option<Typ> {
    if !p.contains(Typ::Null) || p.iter().count() != 2 {
        return None;
    }
    let other = p.iter().find(|t| *t != Typ::Null)?;
    (other == Typ::String || PrimType::from_typ(other).is_some()).then_some(other)
}

/// The success member of an option (`[T, null]`) or result (`[T, Error]`)
/// set; `None` for any other member list. A null marker takes precedence:
/// `[null, Error<T>]` is the option of `Error<T>`.
fn option_result_success(members: &[Type]) -> Option<&Type> {
    if members.len() != 2 {
        return None;
    }
    let is_null = |m: &Type| is_single_prim(m, Typ::Null);
    let is_err = |m: &Type| m.with_deref(|r| matches!(r, Some(Type::Error(_))));
    match (is_null(&members[0]), is_null(&members[1])) {
        (true, false) => return Some(&members[1]),
        (false, true) => return Some(&members[0]),
        _ => {}
    }
    match (is_err(&members[0]), is_err(&members[1])) {
        (true, false) => Some(&members[1]),
        (false, true) => Some(&members[0]),
        _ => None,
    }
}

/// Which marker a [`AbiKind::Nullable`] shape carries: `Some(false)` for
/// the option forms (the non-success value is null), `Some(true)` for the
/// result form (the non-success value is an error, whose disc is not
/// null, so a success test must be positive against T's own disc).
/// `None` for a non-Nullable shape.
pub fn nullable_error_marked(t: &Type) -> Option<bool> {
    let resolved = t.deref_cloned()?;
    match &resolved {
        Type::Primitive(p) => nullable_prim(p).map(|_| false),
        Type::Set(members) => {
            option_result_success(members)?;
            let is_null = |m: &Type| is_single_prim(m, Typ::Null);
            Some(!is_null(&members[0]) && !is_null(&members[1]))
        }
        _ => None,
    }
}

/// Stack-allocated cons-list of the named-type expansions on the
/// current path from the root, for cycle detection while concretizing
/// a type. Only following a `Ref` to its definition extends it, so a
/// recurring key means true type recursion, not structural depth.
pub(crate) struct Seen<'a> {
    key: Arc<TypeRef>,
    /// [`expand_key_fp`] of `key`; membership compares it before full equality.
    fp: u64,
    len: usize,
    prev: Option<&'a Seen<'a>>,
    /// The refs a freeze expanded directly below this one that the
    /// freeze's own memo ([`Freeze::memo`]) does not hold, and what they
    /// froze to: a ref's freeze depends only on it and the path, so a
    /// sibling occurrence takes it.
    frozen: RefCell<SmallVec<[(u64, Arc<TypeRef>, Type); 2]>>,
}

/// Fingerprint of an expansion key; a collision only costs a
/// full-equality check.
pub(crate) fn expand_key_fp(key: &TypeRef) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut h = ahash::AHasher::default();
    key.scope.hash(&mut h);
    key.name.hash(&mut h);
    key.params.len().hash(&mut h);
    h.finish()
}

/// Whether no param of `key` holds a type variable: an unbound cell
/// compares equal to any other, so only a closed key names one type.
pub(crate) fn key_closed(key: &TypeRef) -> bool {
    key.params.iter().all(|t| t.tvar_free())
}

impl<'a> Seen<'a> {
    pub(crate) fn push(prev: Option<&'a Seen<'a>>, key: Arc<TypeRef>) -> Self {
        let fp = expand_key_fp(&key);
        Self { key, fp, len: Self::len(prev) + 1, prev, frozen: RefCell::default() }
    }

    /// The occurrence of `key` on the path. Keys compare cell-blind, so
    /// the one found may have its resolution cell filled where `key`'s
    /// is not; a key never repeats on one path.
    pub(crate) fn find<'b>(cur: Option<&'b Self>, key: &TypeRef) -> Option<&'b TypeRef> {
        Self::find_fp(cur, expand_key_fp(key), key)
    }

    /// [`Self::find`] with the key's fingerprint in hand.
    pub(crate) fn find_fp<'b>(
        mut cur: Option<&'b Self>,
        fp: u64,
        key: &TypeRef,
    ) -> Option<&'b TypeRef> {
        while let Some(s) = cur {
            if s.fp == fp && &*s.key == key {
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
pub(crate) const MAX_FREEZE_EXPANSIONS: usize = 256;

/// The kernel-ABI encodability gate: the concrete form of `t` over the
/// fusable subset, or `None` if any part has no kernel encoding.
/// Accept/reject matches [`abi_kind`] at each level. Every part the
/// kernel reads structurally is TVar-free; a `Map`, an `Error` and an
/// option/result's non-success member are kept as found (they cross
/// as an opaque `Value`), so a TVar may survive inside them.
///
/// A recursive named type freezes to an opaque leaf: the recurring
/// `Ref` stays unexpanded, so the output is finite and the value
/// crosses the boundary as a 2-word opaque. Consumers needing flat
/// payload structure refuse the leaf per node. Non-regular recursion
/// is cut by [`MAX_FREEZE_EXPANSIONS`]; structural depth is unbounded.
pub fn freeze_for_abi(t: &Type) -> Option<Type> {
    try_freeze_for_abi(t).ok()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FreezeError {
    Unsupported,
    NonCanonical,
    Unresolved,
}

fn try_freeze_for_abi(t: &Type) -> Result<Type, FreezeError> {
    let _profile = profile::phase(Phase::Freeze);
    Freeze::default().freeze(t, None)
}

/// A hash of a frozen node's own content and its parts' identities (a
/// part with none, a leaf, by content): parts interned first, two equal
/// nodes key alike. `None` for a node not interned.
pub(crate) fn shallow_key(t: &Type) -> Option<u64> {
    use std::hash::{Hash, Hasher};
    fn part(t: &Type, h: &mut ahash::AHasher) {
        match t.shared_identity() {
            Some(id) => id.hash(h),
            None => t.hash(h),
        }
    }
    let mut h = ahash::AHasher::default();
    std::mem::discriminant(t).hash(&mut h);
    match t {
        Type::Array(e) | Type::List(e) => part(e, &mut h),
        Type::Tuple(ts) | Type::Set(ts) => ts.iter().for_each(|t| part(t, &mut h)),
        Type::Struct(fs) => fs.iter().for_each(|(n, t, _)| {
            n.hash(&mut h);
            part(t, &mut h)
        }),
        Type::Variant(tag, ps, _) => {
            tag.hash(&mut h);
            ps.iter().for_each(|t| part(t, &mut h))
        }
        Type::Abstract { id, params } => {
            id.hash(&mut h);
            params.iter().for_each(|t| part(t, &mut h))
        }
        _ => return None,
    }
    Some(h.finish())
}

/// Whether two nodes [`shallow_key`] keys alike are equal: their own
/// content equal and their parts the same nodes (leaves equal).
pub(crate) fn shallow_same(a: &Type, b: &Type) -> bool {
    fn part(a: &Type, b: &Type) -> bool {
        match (a.shared_identity(), b.shared_identity()) {
            (Some(x), Some(y)) => x == y,
            (None, None) => a == b,
            _ => false,
        }
    }
    fn parts(a: &[Type], b: &[Type]) -> bool {
        a.len() == b.len() && a.iter().zip(b).all(|(a, b)| part(a, b))
    }
    match (a, b) {
        (Type::Array(x), Type::Array(y)) | (Type::List(x), Type::List(y)) => part(x, y),
        (Type::Tuple(x), Type::Tuple(y)) | (Type::Set(x), Type::Set(y)) => parts(x, y),
        (Type::Struct(x), Type::Struct(y)) => {
            x.len() == y.len()
                && x.iter()
                    .zip(y.iter())
                    .all(|((n0, t0, _), (n1, t1, _))| n0 == n1 && part(t0, t1))
        }
        (Type::Variant(t0, x, _), Type::Variant(t1, y, _)) => t0 == t1 && parts(x, y),
        (Type::Abstract { id: i0, params: x }, Type::Abstract { id: i1, params: y }) => {
            i0 == i1 && parts(x, y)
        }
        _ => false,
    }
}

/// One freeze's walk.
#[derive(Default)]
struct Freeze {
    /// The refs whose expansion met no recurring ref, by
    /// [`expand_key_fp`]: such a ref freezes alike on every path, so any
    /// later occurrence takes it, and a typedef DAG freezes once per
    /// definition. Each entry is the key, its frozen form and how many
    /// expansions deep its own went.
    memo: RefCell<LPooled<AHashMap<u64, SmallVec<[(Arc<TypeRef>, Type, usize); 1]>>>>,
    /// The expansion in progress met a recurring ref.
    recurred: Cell<bool>,
    /// The longest path the expansion in progress reached.
    deepest: Cell<usize>,
}

// XCR claude for claude: [perf/medium] Freezing walks a type's typedefs once per
// path, so a typedef DAG (`type T0 = [null, (T1, T1)]`, `type T1 = [null, (T2,
// T2)]`, ..) is 2^depth: a block `{ <T0..T30>; let v: T0 = null; select v { null =>
// 1, (a, b) => 2 } }` does not finish fusing in 170 s, and depth 21 / 23 take 3.6 /
// 6.2 s (debug, fused; --no-fusion 1 s). Found beside x-rerun-5-01, whose
// shapes_collide half is fixed. Freeze each (typedef, params) once a walk, sharing
// the frozen node. probe: the block above as a .gx file, run with fusion
// (fuse-freeze-dag-01)
// 2026-10-26 claude: Fixed by the freeze memo of f-kernel-01: the block at 21
// levels runs in 1-2.5 s fused (10.9 s with the memo off), 30 levels in 1.1 s. Pin:
// kernel_abi::tests::typedef_chain_named_twice_at_the_top_freezes_once (freezing
// (T1, T1) over this chain; fails its 2 s bound with the memo off).
impl Freeze {
    fn freeze(&self, t: &Type, seen: Option<&Seen>) -> Result<Type, FreezeError> {
        crate::stack::ensure_sufficient(|| {
            self.freeze_inner(t, seen).map(super::TypeMemo::intern_frozen)
        })
    }

    /// The memo's frozen form of `tr`, met at a path `len` long, when
    /// its expansion stays within [`MAX_FREEZE_EXPANSIONS`] there.
    fn memoized(&self, fp: u64, tr: &TypeRef, len: usize) -> Option<Type> {
        let memo = self.memo.borrow();
        let (_, t, below) = memo.get(&fp)?.iter().find(|(k, _, _)| **k == *tr)?;
        let deepest = len + 1 + below;
        if deepest > MAX_FREEZE_EXPANSIONS {
            return None;
        }
        self.deepest.set(self.deepest.get().max(deepest));
        Some(t.clone())
    }

    fn freeze_inner(&self, t: &Type, seen: Option<&Seen>) -> Result<Type, FreezeError> {
        use FreezeError::{NonCanonical, Unresolved, Unsupported};
        // The TVar lock is not reentrant: clone out of the guard before recursing.
        let resolved = t.deref_cloned();
        {
            let resolved = resolved.as_ref().ok_or(Unsupported)?;
            match resolved {
                Type::Bottom => Ok(Type::Bottom),
                Type::Map { .. } => Ok(resolved.clone()),
                Type::Error(_) => Ok(resolved.clone()),
                Type::Primitive(p) => {
                    // A primitive with no register form (a varint, a decimal,
                    // an error) and a union of several are one two-word Value.
                    if p.is_empty() {
                        return Err(Unsupported);
                    }
                    Ok(Type::Primitive(*p))
                }
                Type::Array(inner) => {
                    let inner = self.freeze(inner, seen)?;
                    Ok(Type::Array(Arc::new(inner)))
                }
                Type::List(inner) => {
                    let inner = self.freeze(inner, seen)?;
                    Ok(Type::List(Arc::new(inner)))
                }
                Type::Tuple(elems) => {
                    let frozen: Result<LPooled<Vec<Type>>, FreezeError> =
                        elems.iter().map(|e| self.freeze(e, seen)).collect();
                    let mut frozen = frozen?;
                    Ok(Type::Tuple(Arc::from_iter(frozen.drain(..))))
                }
                Type::Struct(fields) => {
                    let frozen: Result<
                        LPooled<Vec<(ArcStr, Type, WrittenAt)>>,
                        FreezeError,
                    > = fields
                        .iter()
                        .map(|(n, ft, at)| {
                            self.freeze(ft, seen).map(|t| (n.clone(), t, *at))
                        })
                        .collect();
                    let mut frozen = frozen?;
                    Ok(Type::Struct(Arc::from_iter(frozen.drain(..))))
                }
                Type::Variant(tag, payloads, at) => {
                    let frozen: Result<LPooled<Vec<Type>>, FreezeError> =
                        payloads.iter().map(|p| self.freeze(p, seen)).collect();
                    let mut frozen = frozen?;
                    Ok(Type::Variant(tag.clone(), Arc::from_iter(frozen.drain(..)), *at))
                }
                Type::Set(members) => {
                    if let Some(p) = primitive_union(members) {
                        return self.freeze(&p, seen);
                    }
                    if let Some(succ) = option_result_success(members) {
                        let succ_idx =
                            if std::ptr::eq(&members[0], succ) { 0 } else { 1 };
                        let frozen_succ = self.freeze(succ, seen)?;
                        let m0 = if succ_idx == 0 {
                            frozen_succ.clone()
                        } else {
                            members[0].clone()
                        };
                        let m1 =
                            if succ_idx == 1 { frozen_succ } else { members[1].clone() };
                        return Ok(Type::Set(Arc::from_iter([m0, m1])));
                    }
                    let composite = |m: &Type| {
                        m.with_deref(|r| {
                            matches!(
                                r,
                                Some(Type::Tuple(_) | Type::Struct(_) | Type::Array(_))
                            )
                        })
                    };
                    if members.iter().all(composite) {
                        // one the normalizer would collapse freezes as what it
                        // collapses to
                        // XCR claude for claude: [risk/low] [risk/low] This normalizes
                        // `resolved`, a shallow clone (deref_cloned) whose members share
                        // the program's TVar cells. TVar::normalize_int binds a non-normal
                        // binding back into the shared cell, so freezing a composite union
                        // can rewrite a settled cell during fusion.
                        // freeze_for_abi_normalized's doc forbids exactly that ("never t:
                        // normalize writes bindings back into shared TVar cells").
                        // Normalize a deep copy (`resolved.resolve_tvars().normalize()`),
                        // or return NonCanonical and let the normalized retry, which
                        // already works on a resolve_tvars() copy, decide. Found by
                        // reading; not run. (f-kernel-02)
                        // 2026-10-26 claude: The composite-union canonicity test
                        // normalizes resolved.resolve_tvars(), a deep copy, so a member's
                        // TVar cell is never rebound. Pin:
                        // kernel_abi::tests::composite_union_freeze_preserves_shared_cells
                        // (the cell's tuple-of-a-non-normal-set binding is rewritten
                        // without the fix).
                        let canonical = matches!(
                            resolved.resolve_tvars().normalize(),
                            Type::Set(ref ms) if ms.len() == members.len()
                        );
                        if !canonical {
                            return Err(NonCanonical);
                        }
                        let frozen: Result<LPooled<Vec<Type>>, FreezeError> =
                            members.iter().map(|m| self.freeze(m, seen)).collect();
                        let mut frozen = frozen?;
                        return Ok(Type::Set(Arc::from_iter(frozen.drain(..))));
                    }
                    let frozen: Result<LPooled<Vec<Type>>, FreezeError> = members
                        .iter()
                        .map(|m| {
                            let m = m.deref_cloned();
                            match &m {
                                Some(Type::Variant(tag, payloads, at)) => {
                                    let fp: Result<LPooled<Vec<Type>>, FreezeError> =
                                        payloads
                                            .iter()
                                            .map(|p| self.freeze(p, seen))
                                            .collect();
                                    let mut fp = fp?;
                                    Ok(Type::Variant(
                                        tag.clone(),
                                        Arc::from_iter(fp.drain(..)),
                                        *at,
                                    ))
                                }
                                _ => Err(NonCanonical),
                            }
                        })
                        .collect();
                    let mut frozen = frozen?;
                    Ok(Type::Set(Arc::from_iter(frozen.drain(..))))
                }
                Type::Abstract { id, params } => {
                    let frozen: Result<LPooled<Vec<Type>>, FreezeError> =
                        params.iter().map(|p| self.freeze(p, seen)).collect();
                    let mut frozen = frozen?;
                    Ok(Type::Abstract {
                        id: *id,
                        params: Arc::from_iter(frozen.drain(..)),
                    })
                }
                // A constructor application whose ctor is a bound TVar is
                // unreduced by typecheck; freeze its filled form.
                Type::App(c, a) => {
                    let cd = c.deref_cloned();
                    let ad = a.deref_cloned();
                    match (cd, ad) {
                        (Some(cd), Some(ad)) => {
                            let r = cd.fill_hole(&ad).ok_or(NonCanonical)?;
                            self.freeze(&r, seen)
                        }
                        _ => Err(NonCanonical),
                    }
                }
                // A recurring ref becomes the opaque leaf, built on the outer
                // (cell-filled) occurrence with its params frozen so the
                // leaf stays TVar-free.
                Type::Ref(tr) => {
                    if let Some(outer) = Seen::find(seen, tr) {
                        self.recurred.set(true);
                        let frozen: Result<LPooled<Vec<Type>>, FreezeError> =
                            tr.params.iter().map(|p| self.freeze(p, seen)).collect();
                        let mut frozen = frozen?;
                        return Ok(Type::Ref(Arc::new(
                            outer.with_params(Arc::from_iter(frozen.drain(..))),
                        )));
                    }
                    let len = Seen::len(seen);
                    if len > MAX_FREEZE_EXPANSIONS {
                        return Err(NonCanonical);
                    }
                    // XCR claude for claude: [perf/medium] [perf/medium] The freeze is still
                    // exponential when a typedef DAG is two wide. The memo lives in the
                    // parent Seen node, so it only shares a ref's frozen form between
                    // siblings under the same expansion. When A_i and B_i each name both
                    // A_{i+1} and B_{i+1}, every node is reached through two parents and
                    // the work doubles with each layer. 20 layers: 5.2 s of phase=Freeze,
                    // against 0.17 s with --no-fusion and 46 ms for --check. Fix: keep the
                    // results of refs whose expansion met no recurring ref
                    // (path-independent) in one map per top-level freeze keyed by the
                    // TypeRef. Or count expansions and refuse with Unsupported past
                    // lowering::FUSION_SIZE_CAP, as the structural-sharing case does.
                    // probe: design/review-2026-10-26/repro/f-kernel-freeze-layered-dag.gx
                    // (f-kernel-01)
                    // 2026-10-26 claude: Freeze is now one walk (kernel_abi::Freeze) with a
                    // memo per top-level freeze: a closed ref whose expansion met no
                    // recurring ref is frozen once and reused on any path (within
                    // MAX_FREEZE_EXPANSIONS, tracked per entry); the sibling memo in
                    // Seen::frozen keeps the rest. The probe takes 1.8 s, the --no-fusion
                    // time (7.1 s before). A two-wide DAG that recurs (A_i, B_i naming the
                    // next and, at the bottom, A0) is still path-dependent and unshared.
                    // Pins:
                    // kernel_abi::tests::two_wide_typedef_dag_freezes_once_per_definition
                    // and typedef_chain_named_twice_at_the_top_freezes_once (each fails its
                    // 2 s bound, about 5 s at depth 22, with the memo off).
                    let fp = expand_key_fp(tr);
                    if let Some(t) = self.memoized(fp, tr, len) {
                        return Ok(t);
                    }
                    if let Some(parent) = seen
                        && let Some((_, _, t)) = parent
                            .frozen
                            .borrow()
                            .iter()
                            .find(|(f, k, _)| *f == fp && **k == **tr)
                    {
                        self.recurred.set(true);
                        return Ok(t.clone());
                    }
                    let expanded = tr.expand_cell().ok_or(Unresolved)?;
                    let node = Seen::push(seen, tr.clone());
                    let recurred = self.recurred.replace(false);
                    let deepest = self.deepest.replace(node.len);
                    let t = self.freeze(&expanded, Some(&node))?;
                    if !self.recurred.get() && key_closed(tr) {
                        let below = self.deepest.get() - node.len;
                        let mut memo = self.memo.borrow_mut();
                        memo.entry(fp).or_default().push((tr.clone(), t.clone(), below));
                    } else if let Some(parent) = seen {
                        parent.frozen.borrow_mut().push((fp, tr.clone(), t.clone()));
                    }
                    self.recurred.set(recurred || self.recurred.get());
                    self.deepest.set(deepest.max(self.deepest.get()));
                    Ok(t)
                }
                _ => Err(Unsupported),
            }
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
pub fn struct_fields(t: &Type) -> Option<&[(ArcStr, Type, WrittenAt)]> {
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
            Some(Type::Variant(tag, payloads, _)) => {
                Some((tag.clone(), payloads.iter().cloned().collect()))
            }
            _ => None,
        })
    }
    match t {
        Type::Variant(_, _, _) => one(t).map(|c| vec![c]),
        Type::Set(members) => members.iter().map(one).collect(),
        _ => None,
    }
}

/// [`freeze_for_abi`], retrying on an un-flattened union through
/// `resolve_tvars().normalize()`. The retry must normalize a deep clone,
/// never `t`: `normalize` writes bindings back into shared TVar cells,
/// and attempting fusion must not change the program's static types.
pub fn freeze_for_abi_normalized(t: &Type) -> Option<Type> {
    try_freeze_for_abi_normalized(t).ok()
}

pub(crate) fn try_freeze_for_abi_normalized(t: &Type) -> Result<Type, FreezeError> {
    super::TypeMemo::frozen(t, || match try_freeze_for_abi(t) {
        Err(FreezeError::NonCanonical) => {
            let p = profile::phase(Phase::Normalize);
            let normalized = t.resolve_tvars().normalize();
            drop(p);
            try_freeze_for_abi(&normalized)
        }
        result => result,
    })
}

/// The success type `T` of a frozen [`AbiKind::Nullable`] shape
/// (`[T, null]`, `[T, Error]`, or the collapsed `T | null` primitive);
/// `None` for any other shape.
pub fn nullable_inner(t: &Type) -> Option<Type> {
    t.with_deref(|r| match r? {
        Type::Primitive(p) => nullable_prim(p).map(|t| Type::Primitive(t.into())),
        Type::Set(members) => option_result_success(members).cloned(),
        _ => None,
    })
}

/// The [`PrimType`] of a register-scalar `Type`; `None` otherwise.
pub fn scalar_prim(t: &Type) -> Option<PrimType> {
    match abi_kind(t) {
        Some(AbiKind::Scalar(p)) => Some(p),
        _ => None,
    }
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

/// The `null` type.
pub fn null_type() -> Type {
    Type::Primitive(Typ::Null.into())
}

/// A placeholder `Map<null, null>` type; codegen only reads its
/// [`AbiKind::Value`] classification, never its key/value types.
pub fn map_type() -> Type {
    Type::Map {
        key: triomphe::Arc::new(null_type()),
        value: triomphe::Arc::new(null_type()),
    }
}

/// One parameter of a fused kernel. The `KernelSig::params` order is
/// the ABI order, the packer's arg order and the tail-rebind order.
/// `bind_id` is `None` for synthetic inputs (lambda formals).
#[derive(Debug, Clone, Pack)]
#[pack(unwrapped)]
pub struct KernelParam {
    pub name: ArcStr,
    pub kind: ParamKind,
    pub bind_id: Option<BindId>,
}

/// The shape of one kernel parameter with the static metadata the body
/// emitter and packer need. The wire shape is a two-word Value pair
/// for every kind.
#[derive(Debug, Clone, PartialEq, Pack)]
#[pack(unwrapped)]
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
    /// Its runtime form is `rep`'s marked variant (`rep::variant`).
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

/// One kernel parameter at the ABI boundary. `wire_slot` is the first
/// of its two `u64` slots.
#[derive(Debug, Clone, Copy)]
pub struct AbiParamDesc<'a> {
    pub name: &'a ArcStr,
    pub kind: &'a ParamKind,
    pub wire_slot: usize,
    /// The source binding, `None` for synthetic inputs. `Ref` emission
    /// resolves by it first, since basenames alias under shadowing.
    pub bind_id: Option<BindId>,
}

/// A kernel's identity: the address of its shared [`KernelSig`]. Every
/// "which kernel" map keys on this, never on `fn_name`: names shadow, and
/// a polymorphic lambda mints one kernel per monomorphization.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct KernelKey(usize);

pub(crate) fn kernel_key(k: &Arc<KernelSig>) -> KernelKey {
    KernelKey(Arc::as_ptr(k) as usize)
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
    /// This body's call-site block size in words, filled once the layout
    /// is final. A self-call reads it at run time because the size is
    /// unknown while the body is still being emitted.
    pub site_block_words: std::sync::atomic::AtomicU64,
}

/// The word at `rel` holds the root of a per-activation block tree: a
/// self-call's activation depth is a run-time fact, so its callee block
/// is a lazily allocated child (`graphix_site_child_block`) rather than
/// a static carve-out. Keyed by call site, never by depth (siblings at
/// one depth are distinct activations).
#[derive(Debug, Clone)]
pub struct SelfBlock {
    pub rel: u32,
    pub layout: Arc<ActivationLayout>,
}

/// What every block of an activation tree holds, identical at every
/// level: `words` words and the reach stamp after them, the child
/// activations' roots at `slots`, the chains `anchors` own and the
/// other kernels' activation trees rooted at `nested`.
#[derive(Debug)]
pub struct ActivationLayout {
    pub words: u32,
    pub slots: Box<[u32]>,
    pub anchors: Box<[SiteAnchor]>,
    pub nested: Box<[SelfBlock]>,
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
    pub leaf: Option<Arc<SiteLeaf>>,
}

impl SiteAnchor {
    /// Whether the chain's call-site blocks root activation trees, its
    /// own or a nested chain's.
    pub fn roots_trees(&self) -> bool {
        self.leaf.as_deref().is_some_and(|l| {
            !l.self_blocks.is_empty() || l.anchors.iter().any(Self::roots_trees)
        })
    }
}

/// A chain leaf whose entries are per-slot call-site blocks: `stride`
/// words per slot, `anchors` naming the in-block words owning further
/// chains and `self_blocks` those rooting activation trees.
#[derive(Debug, Clone)]
pub struct SiteLeaf {
    pub stride: u32,
    pub anchors: Arc<[SiteAnchor]>,
    pub self_blocks: Arc<[SelfBlock]>,
}

/// Leading `u64` wire slots before the parameter list, present in every
/// kernel.
///
/// Slot 0 is the cycle-context word: bit 0 = born (`View::born`, a
/// Birth or a Step: constants fire), bit 1 = standing values read stale
/// (a Wake, a Step, or the kernel's own slept bit). A Birth is
/// `bit0 & !bit1`.
///
/// Slot 1 is the per-instance state pointer (`*mut u64`, 0 when the
/// kernel claimed no words); only the region's root body may claim
/// words, since a callee's claims would alias across call sites.
///
/// Slot 2 is the per-call-site state block pointer, sized by the callee
/// and supplied by each caller from its own storage (a self-call's is a
/// per-activation child block); 0 for region parents and callees that
/// claim nothing; a site-channel claim traps on a null one.
pub(crate) const CTX_WIRE_SLOTS: usize = 3;

/// The context word's born bit (slot 0).
pub(crate) const CTX_BORN: i64 = 1;
/// The context word's wake bit (slot 0).
pub(crate) const CTX_WAKE: i64 = 2;

/// The context word of a run, `born` or not, reading standing values
/// stale or not (`wake`).
pub(crate) fn ctx_word(born: bool, wake: bool) -> u64 {
    (born as u64 * CTX_BORN as u64) | (wake as u64 * CTX_WAKE as u64)
}

impl KernelSig {
    /// The parameters in ABI order with their wire-slot offsets; every
    /// ABI site derives its layout from this, never its own order.
    pub fn abi_params(&self) -> impl Iterator<Item = AbiParamDesc<'_>> {
        self.params.iter().enumerate().map(|(i, p)| AbiParamDesc {
            name: &p.name,
            kind: &p.kind,
            wire_slot: CTX_WIRE_SLOTS + 2 * i,
            bind_id: p.bind_id,
        })
    }

    /// Total `u64` wire slots the dispatch packs and the wrapper unpacks:
    /// [`CTX_WIRE_SLOTS`] plus two per param.
    pub fn abi_wire_slots_total(&self) -> usize {
        CTX_WIRE_SLOTS + 2 * self.params.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        expr::ModPath,
        typ::{AbstractId, FnType, TVar, TypeRef},
    };
    use arcstr::literal;
    use proptest::prelude::*;
    use triomphe::Arc;

    fn i64_t() -> Type {
        Type::Primitive(Typ::I64.into())
    }

    /// A type that shares its parts unfolds exponentially: one 40 levels
    /// of `(t, t)` has 2^40 leaves, past any kernel. The freeze refuses
    /// it at once instead of walking it as a tree.
    #[test]
    fn shared_type_refused_without_unfolding() {
        let mut t = i64_t();
        for _ in 0..40 {
            t = Type::Tuple(Arc::from_iter([t.clone(), t.clone()]));
        }
        assert_eq!(try_freeze_for_abi_normalized(&t), Err(FreezeError::Unsupported));
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

    #[test]
    fn normalized_freeze_preserves_shared_cells() {
        let raw = Type::Set(Arc::from_iter([i64_t(), i64_t()]));
        let cell = TVar::named(literal!("payload"), raw.clone());
        let typ = Type::Variant(
            literal!("V"),
            Arc::from_iter([Type::TVar(cell.clone())]),
            WrittenAt::NOWHERE,
        );
        assert_eq!(
            freeze_for_abi_normalized(&typ),
            Some(Type::Variant(
                literal!("V"),
                Arc::from_iter([i64_t()]),
                WrittenAt::NOWHERE,
            ))
        );
        assert_eq!(Type::TVar(cell).deref_cloned(), Some(raw));
    }

    /// Freezing a union of composites reads whether the normalizer would
    /// collapse it without normalizing the cells its members bind.
    #[test]
    fn composite_union_freeze_preserves_shared_cells() {
        let raw =
            Type::Tuple(Arc::from_iter([Type::Set(Arc::from_iter([i64_t(), i64_t()]))]));
        let cell = TVar::named(literal!("payload"), raw.clone());
        let pair = Type::Tuple(Arc::from_iter([i64_t(), i64_t()]));
        let typ = Type::Set(Arc::from_iter([Type::TVar(cell.clone()), pair]));
        assert!(freeze_for_abi(&typ).is_some());
        assert_eq!(Type::TVar(cell).deref_cloned(), Some(raw));
    }

    /// The type `name` in an env holding `defs` at the root.
    fn typed(defs: &[(String, String)], name: &str) -> (crate::env::Env, Type) {
        use crate::expr::{TypeDefBody, parser::parse_type};
        let mut env = crate::env::Env::default();
        for (name, body) in defs {
            env.deftype(
                &ModPath::root(),
                name,
                Arc::from_iter([]),
                &TypeDefBody::Alias(parse_type(body).unwrap()),
                true,
                None,
                Default::default(),
                Default::default(),
                Arc::new(Default::default()),
            )
            .unwrap();
        }
        env.seed_typedef_refs();
        let t = parse_type(name).unwrap().scope_refs(&ModPath::root());
        t.seed_refs(&env);
        (env, t)
    }

    /// A typedef DAG two wide (`A_i` and `B_i` each name `A_{i+1}` and
    /// `B_{i+1}`) freezes each definition once, not once per path.
    #[test]
    fn two_wide_typedef_dag_freezes_once_per_definition() {
        let depth = 22;
        let mut defs = vec![(format!("A{depth}"), "i64".to_string())];
        defs.push((format!("B{depth}"), "f64".into()));
        for i in 0..depth {
            let (a, b) = (format!("A{}", i + 1), format!("B{}", i + 1));
            defs.push((format!("A{i}"), format!("({a}, {b})")));
            defs.push((format!("B{i}"), format!("{{x: {a}, y: {b}}}")));
        }
        let (_env, t) = typed(&defs, "A0");
        let started = std::time::Instant::now();
        let frozen = freeze_for_abi(&t).expect("a finite typedef DAG freezes");
        assert!(started.elapsed() < std::time::Duration::from_secs(2));
        assert!(matches!(frozen, Type::Tuple(ref ts) if ts.len() == 2));
    }

    /// A ref named twice where no ref encloses it, `(T1, T1)` over
    /// `type T_i = [null, (T_{i+1}, T_{i+1})]`, freezes once too.
    #[test]
    fn typedef_chain_named_twice_at_the_top_freezes_once() {
        let depth = 22;
        let mut defs = vec![(format!("T{depth}"), "i64".to_string())];
        for i in 0..depth {
            defs.push((format!("T{i}"), format!("[null, (T{}, T{})]", i + 1, i + 1)));
        }
        let (_env, t) = typed(&defs, "(T1, T1)");
        let started = std::time::Instant::now();
        assert!(freeze_for_abi_normalized(&t).is_some());
        assert!(started.elapsed() < std::time::Duration::from_secs(2));
    }

    #[test]
    fn constructor_union_normalizes() {
        let typ = Type::App(
            Arc::new(Type::Set(Arc::from_iter([Type::Hole, Type::Hole]))),
            Arc::new(i64_t()),
        );
        assert_eq!(freeze_for_abi(&typ), Some(i64_t()));
        assert_eq!(freeze_for_abi_normalized(&typ), Some(i64_t()));
    }

    fn unresolved_ref(params: Arc<[Type]>) -> Type {
        Type::Ref(Arc::new(TypeRef::synthetic(
            ModPath::root(),
            ModPath::from(["T"]),
            params,
        )))
    }

    fn abstract_type(params: Arc<[Type]>) -> Type {
        Type::Abstract { id: AbstractId::of(&ModPath::root(), "A"), params }
    }

    fn abi_types() -> impl Strategy<Value = Type> {
        prop_oneof![
            Just(unresolved_ref(Arc::from_iter([]))),
            Just(abstract_type(Arc::from_iter([]))),
            Just(i64_t()),
            Just(Type::Primitive((Typ::I64 | Typ::Null).into())),
            Just(Type::Primitive(Typ::Null.into())),
            Just(Type::Primitive(Typ::String.into())),
            Just(Type::Any),
            Just(Type::Bottom),
            Just(Type::Hole),
            Just(Type::empty_tvar()),
            Just(Type::Fn(Arc::new(FnType::default()))),
        ]
        .prop_recursive(4, 64, 4, |inner| {
            prop_oneof![
                inner.clone().prop_map(|t| Type::Array(Arc::new(t))),
                inner.clone().prop_map(|t| Type::ByRef(
                    crate::typ::Mutability::Shared,
                    Arc::new(t)
                )),
                inner.clone().prop_map(|t| Type::Error(Arc::new(t))),
                inner.clone().prop_map(|t| Type::TVar(TVar::named(literal!("a"), t))),
                inner.clone().prop_map(|t| unresolved_ref(Arc::from_iter([t]))),
                inner.clone().prop_map(|t| abstract_type(Arc::from_iter([t]))),
                inner.clone().prop_map(|t| {
                    Type::Struct(Arc::from_iter([(literal!("f"), t, WrittenAt::NOWHERE)]))
                }),
                inner.clone().prop_map(|t| {
                    Type::Variant(literal!("V"), Arc::from_iter([t]), WrittenAt::NOWHERE)
                }),
                (inner.clone(), inner.clone()).prop_map(|(a, b)| {
                    Type::Map { key: Arc::new(a), value: Arc::new(b) }
                }),
                (inner.clone(), inner.clone())
                    .prop_map(|(a, b)| Type::Tuple(Arc::from_iter([a, b]))),
                (inner.clone(), inner.clone())
                    .prop_map(|(a, b)| Type::App(Arc::new(a), Arc::new(b))),
                proptest::collection::vec(inner, 0..4)
                    .prop_map(|ts| Type::Set(Arc::from_iter(ts))),
            ]
        })
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(512))]
        #[test]
        fn selective_retry_matches_unconditional_retry(typ in abi_types()) {
            let expected = freeze_for_abi(&typ).or_else(|| {
                freeze_for_abi(&typ.resolve_tvars().normalize())
            });
            prop_assert_eq!(freeze_for_abi_normalized(&typ), expected);
        }
    }
}

/// A self block's layout is an image object: the callers that copied a
/// callee's blocks share its layouts, a DAG the image keeps one.
impl PackTrait for SelfBlock {
    fn encoded_len(&self) -> usize {
        let SelfBlock { rel, layout } = self;
        rel.encoded_len()
            + crate::image::object_len(
                &(Arc::as_ptr(layout) as usize),
                |k| (*k, layout.clone()),
                |e| &mut e.ext::<crate::image::Compiled>().layouts,
            )
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let SelfBlock { rel, layout } = self;
        rel.encode(buf)?;
        crate::image::object_encode(
            &(Arc::as_ptr(layout) as usize),
            |k| (*k, layout.clone()),
            |e| &mut e.ext::<crate::image::Compiled>().layouts,
            buf,
            |buf| {
                crate::stack::ensure_sufficient(|| {
                    let ActivationLayout { words, slots, anchors, nested } = &**layout;
                    words.encode(buf)?;
                    crate::image::slice_encode(slots, buf)?;
                    crate::image::slice_encode(anchors, buf)?;
                    crate::image::slice_encode(nested, buf)
                })
            },
        )
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let rel = u32::decode(buf)?;
        Ok(SelfBlock { rel, layout: activation_layout_decode(buf)? })
    }
}

fn activation_layout_decode(
    buf: &mut impl Buf,
) -> Result<Arc<ActivationLayout>, PackError> {
    crate::image::foreign_decode(
        buf,
        |buf| {
            crate::stack::ensure_sufficient(|| {
                let words = u32::decode(buf)?;
                let slots: Vec<u32> = PackTrait::decode(buf)?;
                let anchors: Vec<SiteAnchor> = PackTrait::decode(buf)?;
                let nested: Vec<SelfBlock> = PackTrait::decode(buf)?;
                Ok(Arc::new(ActivationLayout {
                    words,
                    slots: slots.into(),
                    anchors: anchors.into(),
                    nested: nested.into(),
                }))
            })
        },
        |b| activation_layout_decode(b),
    )
}

impl PackTrait for SiteAnchor {
    fn encoded_len(&self) -> usize {
        let SiteAnchor { rel, own_levels, leaf } = self;
        rel.encoded_len()
            + own_levels.encoded_len()
            + 1
            + leaf.as_ref().map_or(0, site_leaf_len)
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let SiteAnchor { rel, own_levels, leaf } = self;
        rel.encode(buf)?;
        own_levels.encode(buf)?;
        match leaf {
            None => Ok(buf.put_u8(0)),
            Some(l) => {
                buf.put_u8(1);
                site_leaf_encode(l, buf)
            }
        }
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let rel = u32::decode(buf)?;
        let own_levels = u32::decode(buf)?;
        let leaf = match u8::decode(buf)? {
            0 => None,
            1 => Some(site_leaf_decode(buf)?),
            _ => return Err(PackError::UnknownTag),
        };
        Ok(SiteAnchor { rel, own_levels, leaf })
    }
}

/// A slot-chain leaf is an image object: the anchors that own it and
/// the code that names it share one.
pub(crate) fn site_leaf_len(l: &Arc<SiteLeaf>) -> usize {
    crate::image::object_len(
        &(Arc::as_ptr(l) as usize),
        |k| (*k, l.clone()),
        |e| &mut e.ext::<crate::image::Compiled>().site_leaves,
    )
}

pub(crate) fn site_leaf_encode(
    l: &Arc<SiteLeaf>,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    crate::image::object_encode(
        &(Arc::as_ptr(l) as usize),
        |k| (*k, l.clone()),
        |e| &mut e.ext::<crate::image::Compiled>().site_leaves,
        buf,
        |buf| {
            l.stride.encode(buf)?;
            crate::image::slice_encode(&l.anchors, buf)?;
            crate::image::slice_encode(&l.self_blocks, buf)
        },
    )
}

pub(crate) fn site_leaf_decode(buf: &mut impl Buf) -> Result<Arc<SiteLeaf>, PackError> {
    crate::image::foreign_decode(
        buf,
        |buf| {
            let stride = u32::decode(buf)?;
            let anchors: Vec<SiteAnchor> = PackTrait::decode(buf)?;
            let self_blocks: Vec<SelfBlock> = PackTrait::decode(buf)?;
            Ok(Arc::new(SiteLeaf {
                stride,
                anchors: anchors.into(),
                self_blocks: self_blocks.into(),
            }))
        },
        |b| site_leaf_decode(b),
    )
}

/// A kernel signature is an image object shared by the node that
/// dispatches it and the records of its bodies.
pub(crate) fn kernel_sig_encode(
    k: &Arc<KernelSig>,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    use std::sync::atomic::Ordering::Relaxed;
    crate::image::object_encode(
        &(Arc::as_ptr(k) as usize),
        |_| (Arc::as_ptr(k) as usize, k.clone()),
        |e| &mut e.ext::<crate::image::Compiled>().kernel_sigs,
        buf,
        |buf| {
            k.fn_name.encode(buf)?;
            k.params.encode(buf)?;
            k.return_type.encode(buf)?;
            k.has_tail_loop.encode(buf)?;
            k.skipped_args.encode(buf)?;
            k.tail_invariant.encode(buf)?;
            Ok(netidx_core::pack::encode_varint(k.site_block_words.load(Relaxed), buf))
        },
    )
}

pub(crate) fn kernel_sig_decode(buf: &mut impl Buf) -> Result<Arc<KernelSig>, PackError> {
    use std::sync::atomic::AtomicU64;
    crate::image::foreign_decode(
        buf,
        |buf| {
            let fn_name = ArcStr::decode(buf)?;
            let params = Vec::<KernelParam>::decode(buf)?;
            let return_type = Type::decode(buf)?;
            let has_tail_loop = bool::decode(buf)?;
            let skipped_args = Vec::<u32>::decode(buf)?;
            let tail_invariant = Vec::<u32>::decode(buf)?;
            let site_block_words = netidx_core::pack::decode_varint(buf)?;
            Ok(Arc::new(KernelSig {
                fn_name,
                params,
                return_type,
                has_tail_loop,
                skipped_args,
                tail_invariant,
                site_block_words: AtomicU64::new(site_block_words),
            }))
        },
        |b| kernel_sig_decode(b),
    )
}
