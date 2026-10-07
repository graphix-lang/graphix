use crate::{
    PRINT_FLAGS, PrintFlag, SourcePosition,
    dbgenv::{graphix_dbg_bind, gxdbg_typeref},
    env::Env,
    expr::{ModPath, Origin, Source, WrittenAt},
    format_with_flags,
    image::{self, KeyedNode},
    stack::ensure_sufficient,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack as PackTrait, PackError, encode_varint};
use netidx_value::Typ;
use nohash::{IntMap, IntSet};
use parking_lot::Mutex;
use poolshark::{IsoPoolable, local::LPooled};
use smallvec::SmallVec;
use std::{
    cmp::Ordering,
    fmt::Debug,
    hash::{Hash, Hasher},
    iter, mem,
    ops::{ControlFlow, Deref, DerefMut},
    sync::{self, LazyLock, Weak},
};
use triomphe::Arc;

mod cast;
pub use cast::{Indiscernible, IsAFlags, Open};
mod contains;
pub use contains::ContainsFlags;
pub use contains::TypeMismatch;
pub mod fntyp;
mod matches;
mod normalize;
pub use normalize::{NormKey, norm_key};
mod print;
mod setops;
#[doc(hidden)]
pub mod settle;
pub(crate) mod tval;
#[doc(hidden)]
pub mod tvar;

pub use fntyp::{FnArgKind, FnArgType, FnType};
pub use tval::TVal;
pub use tvar::TVar;

struct AndAc(bool);

impl FromIterator<bool> for AndAc {
    fn from_iter<T: IntoIterator<Item = bool>>(iter: T) -> Self {
        AndAc(iter.into_iter().all(|b| b))
    }
}

/// The address of a composite's content allocation, for walks that
/// visit each shared node once; `None` for leaves and for `Map`, whose
/// content is two allocations.
pub(super) fn node_addr(t: &Type) -> Option<usize> {
    match t {
        Type::Set(a) | Type::Tuple(a) | Type::Variant(_, a, _) => {
            Some((**a).as_ptr().addr())
        }
        Type::Abstract { params: a, .. } => Some((**a).as_ptr().addr()),
        Type::Struct(a) => Some((**a).as_ptr().addr()),
        Type::Fn(f) => Some((&**f as *const FnType).addr()),
        Type::Array(a) | Type::List(a) | Type::Error(a) | Type::ByRef(_, a) => {
            Some((&**a as *const Type).addr())
        }
        Type::Map { .. }
        | Type::App(..)
        | Type::Hole
        | Type::Concrete
        | Type::Function
        | Type::Singleton
        | Type::OneNumber
        | Type::Discernible
        | Type::Ordered
        | Type::Primitive(_)
        | Type::Any
        | Type::Bottom
        | Type::Ref(_)
        | Type::TVar(_) => None,
    }
}

/// A relation's question about two types, by their [`RefHist::ref_id`]s.
type RefPair = (Option<usize>, Option<usize>);

/// [`norm_key`] extended with `Variant`: a key may include the tag's
/// allocation identity.
fn probe_key(t: &Type) -> Option<NormKey> {
    match t {
        Type::Variant(tag, ts, _) => {
            Some((mem::discriminant(t), (**ts).as_ptr() as usize, tag.as_ptr() as usize))
        }
        t => norm_key(t),
    }
}

/// A relation's cycle memo: ids for the types a walk meets, and
/// `inner`, the relation's own record of the pairs in progress.
/// A pooled container taken at its first write: most walks never
/// write theirs.
struct Lazy<T: IsoPoolable>(Option<LPooled<T>>);

impl<T: IsoPoolable> Lazy<T> {
    fn new() -> Self {
        Self(None)
    }

    fn get(&self) -> Option<&T> {
        self.0.as_deref()
    }

    fn get_mut(&mut self) -> &mut T {
        self.0.get_or_insert_with(LPooled::take)
    }
}

struct RefHist<H: IsoPoolable> {
    inner: LPooled<H>,
    /// Definition key → (the resolution it came from, pinned so the key
    /// is not reused; the param lists seen with their ids).
    ref_ids: Lazy<
        IntMap<usize, (sync::Arc<ResolvedRef>, SmallVec<[(Arc<[Type]>, usize); 2]>)>,
    >,
    /// Content identity → id for non-Ref types, so the cycle memo does
    /// not conflate distinct finite sub-problems.
    content_ids: Lazy<AHashMap<NormKey, usize>>,
    /// Structure → id for composite non-Ref types, open cells by their
    /// cell: an expansion rebuilt at every visit meets its earlier self.
    /// A hash bucket holds the types themselves, matched by
    /// `union_identical`.
    shape_ids: Lazy<IntMap<u64, SmallVec<[(Type, usize); 1]>>>,
    next_id: usize,
}

/// A hash of `t`'s structure consistent with `union_identical`: a bound
/// cell is its binding, an open cell its cell, a union's members in any
/// order, a function only its shape (its equality is loose).
fn shape_hash(t: &Type) -> u64 {
    ensure_sufficient(|| {
        let mut h = ahash::AHasher::default();
        match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => return shape_hash(&b),
                None => (0u8, tv.cell_addr()).hash(&mut h),
            },
            Type::Set(ts) => {
                let members = ts.iter().fold(0u64, |a, t| a.wrapping_add(shape_hash(t)));
                (1u8, members).hash(&mut h)
            }
            Type::Fn(f) => (2u8, f.args.len(), f.vargs.is_some()).hash(&mut h),
            Type::Primitive(p) => (3u8, p.bits()).hash(&mut h),
            Type::Ref(tr) => (4u8, &tr.scope, &tr.name).hash(&mut h),
            Type::Variant(tag, _, _) => (5u8, tag).hash(&mut h),
            Type::Struct(fs) => {
                6u8.hash(&mut h);
                fs.iter().for_each(|(n, _, _)| n.hash(&mut h))
            }
            Type::Abstract { id, .. } => (7u8, id).hash(&mut h),
            Type::ByRef(m, _) => (8u8, m.tag()).hash(&mut h),
            t => (9u8, mem::discriminant(t)).hash(&mut h),
        }
        if !matches!(t, Type::Set(_) | Type::Fn(_)) {
            t.for_each_child(&mut |c| shape_hash(c).hash(&mut h));
        }
        h.finish()
    })
}

impl<H: IsoPoolable> Deref for RefHist<H> {
    type Target = H;

    fn deref(&self) -> &H {
        &*self.inner
    }
}

impl<H: IsoPoolable> DerefMut for RefHist<H> {
    fn deref_mut(&mut self) -> &mut H {
        &mut *self.inner
    }
}

impl<H: IsoPoolable> RefHist<H> {
    fn new() -> Self {
        RefHist {
            inner: LPooled::take(),
            ref_ids: Lazy::new(),
            content_ids: Lazy::new(),
            shape_ids: Lazy::new(),
            next_id: 0,
        }
    }

    fn next(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    /// A stable id for a type: a Ref keys on its definition and params
    /// (the cell is filled first, so one ref has one id in a walk); a
    /// bound cell is its binding; an open cell keys on the cell, a
    /// primitive on its bits, anything else on its content. `None` only
    /// for an unresolvable name, a constructor application or a hole.
    fn ref_id(&mut self, t: &Type, env: &Env) -> Option<usize> {
        let d = mem::discriminant(t);
        let k = match t {
            Type::Ref(tr) => {
                let r = tr.resolve_in(env)?;
                let key = r.def_key();
                let entry = self
                    .ref_ids
                    .get_mut()
                    .entry(key)
                    .or_insert_with(|| (r, SmallVec::new()));
                let found = entry.1.iter().find(|(p, _)| {
                    Arc::ptr_eq(p, &tr.params)
                        || p.len() == tr.params.len()
                            && p.iter()
                                .zip(tr.params.iter())
                                .all(|(a, b)| setops::union_identical(a, b))
                });
                if let Some((_, id)) = found {
                    return Some(*id);
                }
                let id = self.next();
                self.ref_ids
                    .get_mut()
                    .get_mut(&key)
                    .expect("inserted")
                    .1
                    .push((tr.params.clone(), id));
                return Some(id);
            }
            Type::TVar(tv) => match tv.binding() {
                Some(b) => return self.ref_id(&b, env),
                None => (d, tv.cell_addr(), 0),
            },
            Type::Primitive(p) => (d, p.bits() as usize, 0),
            Type::Abstract { id, params } => {
                (d, (**params).as_ptr().addr(), id.0 as usize)
            }
            Type::Any | Type::Bottom => (d, 0, 0),
            Type::App(..) | Type::Hole => return None,
            t => {
                let h = shape_hash(t);
                let found = self.shape_ids.get().and_then(|m| m.get(&h)).and_then(|b| {
                    b.iter()
                        .find(|(s, _)| setops::union_identical(s, t))
                        .map(|(_, id)| *id)
                });
                if let Some(id) = found {
                    return Some(id);
                }
                let id = self.next();
                self.shape_ids.get_mut().entry(h).or_default().push((t.clone(), id));
                return Some(id);
            }
        };
        if let Some(&id) = self.content_ids.get().and_then(|m| m.get(&k)) {
            return Some(id);
        }
        let id = self.next();
        self.content_ids.get_mut().insert(k, id);
        Some(id)
    }
}

/// Whether a typedef's body returns to the definition with no
/// constructor between ([`Type::reaches_unguarded`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Unguarded {
    Reaches,
    Guarded,
    /// A name on the way is not defined yet.
    Unknown,
}

impl Unguarded {
    fn or(self, other: Self) -> Self {
        match (self, other) {
            (Self::Reaches, _) | (_, Self::Reaches) => Self::Reaches,
            (Self::Unknown, _) | (_, Self::Unknown) => Self::Unknown,
            _ => Self::Guarded,
        }
    }
}

/// The identity of an abstract type: the low 64 bits of
/// [`abstract_uuid`] of its canonical path. Its `Pack` impl is
/// `uuid_id_codec!`'s.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct AbstractId(u64);

impl nohash::IsEnabled for AbstractId {}

/// The v5 UUID namespace abstract type identities are derived from,
/// so the compile-time [`AbstractId`] and a value's runtime tag agree
/// across processes and builds.
const ABSTRACT_NAMESPACE: uuid::Uuid = uuid::Uuid::from_bytes([
    0x1f, 0x64, 0x9a, 0x2e, 0x7b, 0xd5, 0x4c, 0x8a, 0x9f, 0x3e, 0x21, 0xb7, 0x5c, 0x0d,
    0xe6, 0x42,
]);

/// The UUID of the abstract type at `path` (`package::module::Name`).
/// Rust-backed abstract types register their wrapper under it.
pub fn abstract_uuid(path: &str) -> uuid::Uuid {
    uuid::Uuid::new_v5(&ABSTRACT_NAMESPACE, path.as_bytes())
}

/// The names of every abstract type minted in this process, for
/// diagnostics (`Type::Abstract` carries only the id).
static ABSTRACT_NAMES: LazyLock<Mutex<IntMap<AbstractId, ArcStr>>> =
    LazyLock::new(|| Mutex::new(IntMap::default()));

impl AbstractId {
    /// The identity of the abstract type `name` defined in `scope`:
    /// the low 64 bits of [`abstract_uuid`] of its canonical path.
    pub fn of(scope: &ModPath, name: &str) -> Self {
        let path = format_compact!("{scope}::{name}");
        let (_, lo) = abstract_uuid(&path).as_u64_pair();
        let id = AbstractId(lo);
        ABSTRACT_NAMES.lock().entry(id).or_insert_with(|| ArcStr::from(name));
        id
    }

    /// The identity of the abstract type `name` declared at `pos` in
    /// `ori`, in `scope`. A component a function body or block mints
    /// (`#fn..`, `#do..`) differs per instance and per process, so it
    /// takes no part: a module's type is its path without them (an
    /// interface's declaration and its implementation's are one type),
    /// and a type declared in a body is its place there, so every
    /// instance of the enclosing function, cold or warm, declares one.
    pub fn declared(
        scope: &ModPath,
        name: &str,
        pos: SourcePosition,
        ori: &Origin,
    ) -> Self {
        let parts = || netidx_core::path::Path::parts(&scope.0);
        if !parts().any(|p| p.starts_with('#')) {
            return Self::of(scope, name);
        }
        let mut stable = compact_str::CompactString::new("");
        for p in parts().filter(|p| !p.starts_with('#')) {
            stable.push('/');
            stable.push_str(p);
        }
        let in_body = parts().last().is_some_and(|p| p.starts_with('#'));
        let place = match in_body {
            false => format_compact!("{stable}::{name}"),
            true => format_compact!("{stable}::#{}:{pos}::{name}", ori.source),
        };
        let (_, lo) = abstract_uuid(&place).as_u64_pair();
        let id = AbstractId(lo);
        ABSTRACT_NAMES.lock().entry(id).or_insert_with(|| ArcStr::from(name));
        id
    }

    /// The type's declared name, if this process has minted the id.
    pub fn name(&self) -> Option<ArcStr> {
        ABSTRACT_NAMES.lock().get(self).cloned()
    }

    pub fn inner(&self) -> u64 {
        self.0
    }

    pub fn from_inner(i: u64) -> Self {
        AbstractId(i)
    }
}

/// The identity of a trait: the low 64 bits of a v5 UUID of its
/// canonical path, so an interface's declaration and the
/// implementation's re-declaration name one trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitId(u64);

const TRAIT_NAMESPACE: uuid::Uuid = uuid::Uuid::from_bytes([
    0x8c, 0x2b, 0x41, 0x9d, 0xe3, 0x07, 0x4f, 0x6a, 0xb1, 0x5d, 0x77, 0x0a, 0x2e, 0x93,
    0xc8, 0x15,
]);

impl TraitId {
    pub fn of(scope: &ModPath, name: &str) -> Self {
        let path = format_compact!("{scope}::{name}");
        let (_, lo) = uuid::Uuid::new_v5(&TRAIT_NAMESPACE, path.as_bytes()).as_u64_pair();
        TraitId(lo)
    }

    pub fn inner(&self) -> u64 {
        self.0
    }

    pub fn from_inner(i: u64) -> Self {
        TraitId(i)
    }
}

impl nohash::IsEnabled for TraitId {}

/// The core traits `Eq`, `Ord` and `Display`, which ride the value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoreTrait {
    Eq,
    Ord,
    Display,
}

static CORE_IDS: LazyLock<[TraitId; 3]> = LazyLock::new(|| {
    let core = ModPath::from(["core"]);
    [TraitId::of(&core, "Eq"), TraitId::of(&core, "Ord"), TraitId::of(&core, "Display")]
});

impl CoreTrait {
    pub fn id(self) -> TraitId {
        CORE_IDS[self as usize]
    }

    pub fn of_id(id: TraitId) -> Option<Self> {
        [Self::Eq, Self::Ord, Self::Display].into_iter().find(|t| t.id() == id)
    }

    #[doc(hidden)]
    pub fn method(self) -> &'static str {
        match self {
            Self::Eq => "eq",
            Self::Ord => "cmp",
            Self::Display => "fmt",
        }
    }

    #[doc(hidden)]
    pub fn arity(self) -> usize {
        match self {
            Self::Eq | Self::Ord => 2,
            Self::Display => 1,
        }
    }
}

/// What a `TypeRef`'s name means: the definition [`Type::lookup_ref`]
/// reads, held by its `TypeDef` and weakly by the ref's write-once
/// `resolved` cell, so a ref resolved once in its native env is
/// env-independent after. A recursive definition's body reaches its own
/// cell, so a strong cell would be a cycle.
#[derive(Debug)]
#[doc(hidden)]
pub struct ResolvedRef {
    canonical_scope: ModPath,
    pos: SourcePosition,
    ori: Arc<Origin>,
    params: Arc<[(TVar, Option<Type>)]>,
    typ: Type,
}

impl ResolvedRef {
    pub(crate) fn new(
        canonical_scope: ModPath,
        pos: SourcePosition,
        ori: Arc<Origin>,
        params: Arc<[(TVar, Option<Type>)]>,
        typ: Type,
    ) -> Self {
        Self { canonical_scope, pos, ori, params, typ }
    }

    pub(crate) fn pos(&self) -> SourcePosition {
        self.pos
    }

    pub(crate) fn ori(&self) -> &Arc<Origin> {
        &self.ori
    }

    pub(crate) fn params(&self) -> &Arc<[(TVar, Option<Type>)]> {
        &self.params
    }

    pub(crate) fn typ(&self) -> &Type {
        &self.typ
    }

    /// The definition's parameters by name, bound to `args`; `None` when
    /// the arity differs.
    pub(crate) fn bindings(
        &self,
        args: &[Type],
    ) -> Option<LPooled<AHashMap<ArcStr, Type>>> {
        if self.params.len() != args.len() {
            return None;
        }
        let mut known: LPooled<AHashMap<ArcStr, Type>> = LPooled::take();
        for ((tv, _), arg) in self.params.iter().zip(args.iter()) {
            known.insert(tv.name.clone(), arg.clone());
        }
        Some(known)
    }

    /// The definition's identity: every cell filled from one `TypeDef`
    /// shares its params allocation.
    pub(crate) fn def_key(&self) -> usize {
        Arc::as_ptr(&self.params) as *const () as usize
    }

    #[doc(hidden)]
    pub fn canonical_scope(&self) -> &ModPath {
        &self.canonical_scope
    }
}

/// A reference to a named typedef, e.g. `Foo` or `Result<i64, string>`.
/// `pos`/`ori` are IDE metadata and `resolved` is the write-once name
/// resolution cell ([`ResolvedRef`]); neither is part of type identity
/// or of the syntax codec's packed form, while an image session writes
/// all three (and keys them), so a restored ref keeps its resolution and
/// never resolves again. The cell depends on (scope, name, env) but not
/// `params`: [`TypeRef::with_params`] shares it, [`TypeRef::with_scope`]
/// mints a new one. Never overwrite a filled cell — clones share it.
#[derive(Debug, Clone)]
pub struct TypeRef {
    pub scope: ModPath,
    pub name: ModPath,
    pub params: Arc<[Type]>,
    pub pos: Option<SourcePosition>,
    pub ori: Option<Arc<Origin>>,
    pub(in crate::typ) resolved: Arc<Mutex<Option<Weak<ResolvedRef>>>>,
}

pub(crate) fn resolved_encode(
    r: &ResolvedRef,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    let ResolvedRef { canonical_scope, pos, ori, params, typ } = r;
    canonical_scope.encode(buf)?;
    image::pos_encode(pos, buf)?;
    image::origin_encode(ori, buf)?;
    params.encode(buf)?;
    typ.encode(buf)
}

pub(crate) fn resolved_decode(buf: &mut impl Buf) -> Result<ResolvedRef, PackError> {
    Ok(ResolvedRef {
        canonical_scope: PackTrait::decode(buf)?,
        pos: image::pos_decode(buf)?,
        ori: image::origin_decode(buf)?,
        params: PackTrait::decode(buf)?,
        typ: PackTrait::decode(buf)?,
    })
}

/// The syntax codec writes the name and parameters and mints a fresh
/// cell; under an image the position, origin and the shared resolution
/// cell travel too, so a restored session never re-resolves.
impl PackTrait for TypeRef {
    fn encoded_len(&self) -> usize {
        let TypeRef { scope, name, params, pos, ori, resolved } = self;
        let base = scope.encoded_len() + name.encoded_len() + params.encoded_len();
        if !image::is_encoding() {
            return base;
        }
        let pos = 1 + pos.as_ref().map_or(0, image::pos_len);
        let ori = 1 + ori.as_ref().map_or(0, image::origin_len);
        base + pos + ori + image::refcell_len(resolved)
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let TypeRef { scope, name, params, pos, ori, resolved } = self;
        scope.encode(buf)?;
        name.encode(buf)?;
        params.encode(buf)?;
        if !image::is_encoding() {
            return Ok(());
        }
        match pos {
            None => buf.put_u8(0),
            Some(p) => {
                buf.put_u8(1);
                image::pos_encode(p, buf)?;
            }
        }
        match ori {
            None => buf.put_u8(0),
            Some(o) => {
                buf.put_u8(1);
                image::origin_encode(o, buf)?;
            }
        }
        image::refcell_encode(resolved, buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let scope = PackTrait::decode(buf)?;
        let name = PackTrait::decode(buf)?;
        let params = PackTrait::decode(buf)?;
        if !image::is_decoding() {
            return Ok(TypeRef::new(scope, name, params, None, None));
        }
        let pos = match u8::decode(buf)? {
            0 => None,
            1 => Some(image::pos_decode(buf)?),
            _ => return Err(PackError::UnknownTag),
        };
        let ori = match u8::decode(buf)? {
            0 => None,
            1 => Some(image::origin_decode(buf)?),
            _ => return Err(PackError::UnknownTag),
        };
        let resolved = image::refcell_decode(buf)?;
        Ok(TypeRef { scope, name, params, pos, ori, resolved })
    }
}

impl TypeRef {
    pub fn new(
        scope: ModPath,
        name: ModPath,
        params: Arc<[Type]>,
        pos: Option<SourcePosition>,
        ori: Option<Arc<Origin>>,
    ) -> Self {
        Self { scope, name, params, pos, ori, resolved: Arc::default() }
    }

    /// A `TypeRef` with no source-position info.
    pub fn synthetic(scope: ModPath, name: ModPath, params: Arc<[Type]>) -> Self {
        Self::new(scope, name, params, None, None)
    }

    /// This ref with different `params`, sharing the resolution cell.
    #[doc(hidden)]
    pub fn with_params(&self, params: Arc<[Type]>) -> Self {
        Self { params, ..self.clone() }
    }

    /// This ref re-scoped, with a new resolution cell pre-filled from
    /// this ref's cell when that is resolved (a filled cell is the
    /// name's final target; a new scope may not even reach it). An
    /// unfilled cell stays fresh.
    pub(crate) fn with_scope(&self, scope: ModPath, params: Arc<[Type]>) -> Self {
        Self {
            scope,
            name: self.name.clone(),
            params,
            pos: self.pos,
            ori: self.ori.clone(),
            resolved: Arc::new(Mutex::new(self.resolved.lock().clone())),
        }
    }

    /// Expand this ref through its filled cell, env-free, substituting
    /// params as `lookup_ref` would. `None` when the cell is empty or
    /// the arity mismatches. No constraint checks.
    pub fn expand_cell(&self) -> Option<Type> {
        let r = self.resolved()?;
        Some(r.typ.replace_tvars(&*r.bindings(&self.params)?))
    }

    /// The definition the cell holds; `None` when it is empty, or when
    /// the definition is gone (see [`Self::resolve_in`]).
    pub(crate) fn resolved(&self) -> Option<sync::Arc<ResolvedRef>> {
        self.resolved.lock().as_ref().and_then(Weak::upgrade)
    }

    /// Identity of the resolution cell: shared by the rebuilds of one ref.
    #[doc(hidden)]
    pub fn cell_addr(&self) -> usize {
        Arc::as_ptr(&self.resolved) as *const () as usize
    }

    /// [`ResolvedRef::def_key`] of the filled cell.
    #[doc(hidden)]
    pub fn def_key(&self) -> Option<usize> {
        self.resolved().map(|r| r.def_key())
    }

    /// Do two refs of one scope and name mean one definition, as far as
    /// their cells prove? Filled cells by the definition's identity, two
    /// empty ones alike; one of each may differ (a def's thrown types keep
    /// their cells when they move into its scope), and a disagreement
    /// only sends the pair to the expansion arm.
    pub(crate) fn cells_agree(&self, other: &Self) -> bool {
        match (self.resolved(), other.resolved()) {
            (Some(a), Some(b)) => a.def_key() == b.def_key(),
            (None, None) => true,
            _ => false,
        }
    }

    /// Does the name mean something: its filled cell, a typedef visible
    /// in `env`, or a trait (a bound)? Never writes the cell: a fill
    /// before every name is registered can capture a shadowed target.
    #[doc(hidden)]
    pub fn names_something(&self, env: &Env) -> bool {
        self.resolved().is_some()
            || self.resolve_pure(env).is_some()
            || env.trait_of_ref(self).is_some()
    }

    /// What this ref's name means in `env`; never reads or writes the
    /// cell.
    pub(crate) fn resolve_pure(&self, env: &Env) -> Option<sync::Arc<ResolvedRef>> {
        env.resolve_type_name(&self.scope, &self.name)
            .map(|hit| match hit {
                Some(crate::env::TypeName::Def(def)) => Some(def),
                Some(crate::env::TypeName::Trait(_)) | None => None,
            })
            .ok()
            .flatten()
    }

    /// Why the name does not resolve, when that is a mistake in the path
    /// (an ambiguous glob, `super` past the root, a missing module) and
    /// not an absent name: the error a report names in place of
    /// [`UnresolvableRef`].
    pub fn resolve_error(&self, env: &Env) -> Option<anyhow::Error> {
        env.resolve_type_name(&self.scope, &self.name).err()
    }

    /// Resolve this ref's name in `env` and fill the cell if empty;
    /// `None` iff the name is not visible and the cell is empty. An
    /// existing resolution wins. The snapshot is computed without the
    /// cell lock held (resolution can re-enter). Fills only this ref,
    /// not the snapshot's nested refs: mid-compile the env is
    /// incomplete, and a nested name can resolve to an outer shadow.
    /// A cell whose definition is gone (a type that outlived the env
    /// entry that defined it) is `None` too, and logged: the name may
    /// mean something else now, so it is never re-resolved.
    #[doc(hidden)]
    pub fn resolve_in(&self, env: &Env) -> Option<sync::Arc<ResolvedRef>> {
        let dead = || {
            log::error!(
                "type `{}` outlived its definition in `{}`",
                self.name,
                self.scope
            );
            None
        };
        if let Some(w) = &*self.resolved.lock() {
            return w.upgrade().or_else(dead);
        }
        let r = self.resolve_pure(env)?;
        let mut cell = self.resolved.lock();
        match &*cell {
            Some(w) => w.upgrade().or_else(dead),
            None => {
                *cell = Some(sync::Arc::downgrade(&r));
                Some(r)
            }
        }
    }
}

impl Default for TypeRef {
    fn default() -> Self {
        Self {
            scope: ModPath::root(),
            name: ModPath::root(),
            params: Arc::from(Vec::<Type>::new()),
            pos: None,
            ori: None,
            resolved: Arc::default(),
        }
    }
}

impl PartialEq for TypeRef {
    fn eq(&self, other: &Self) -> bool {
        self.scope == other.scope
            && self.name == other.name
            && self.params == other.params
    }
}

impl Eq for TypeRef {}

impl PartialOrd for TypeRef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::hash::Hash for TypeRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.scope.hash(state);
        self.name.hash(state);
        self.params.hash(state);
    }
}

impl Ord for TypeRef {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.scope
            .cmp(&other.scope)
            .then_with(|| self.name.cmp(&other.name))
            .then_with(|| self.params.cmp(&other.params))
    }
}

/// Type depth is not bounded by source nesting (`let x1 = [x0]; let x2
/// = [x1]; ..` builds a type as deep as the program is long), so every
/// recursive walk, `Eq`, `Ord` and `Hash` included, runs under
/// [`ensure_sufficient`].
#[derive(Debug, Clone)]
pub enum Type {
    Bottom,
    Any,
    Primitive(BitFlags<Typ>),
    /// Boxed: inline, a ref (two paths, params, position, origin and
    /// cell) would make every type half again as large.
    Ref(Arc<TypeRef>),
    Fn(Arc<FnType>),
    Set(Arc<[Type]>),
    TVar(TVar),
    Error(Arc<Type>),
    Array(Arc<Type>),
    /// The native linked list. The runtime rep is private to
    /// `node::list`.
    List(Arc<Type>),
    ByRef(Mutability, Arc<Type>),
    Tuple(Arc<[Type]>),
    Struct(Arc<[(ArcStr, Type, WrittenAt)]>),
    Variant(ArcStr, Arc<[Type]>, WrittenAt),
    Map {
        key: Arc<Type>,
        value: Arc<Type>,
    },
    Abstract {
        id: AbstractId,
        params: Arc<[Type]>,
    },
    /// A type constructor applied to one argument (`self<'a>`,
    /// `'c<i64>`). The constructor is a type variable that binds to a
    /// type with a [`Type::Hole`] in its last parameter; once bound the
    /// application is its filled type ([`Type::app`]).
    App(Arc<Type>, Arc<Type>),
    /// The hole in a type constructor, written `'_` (`impl Collection
    /// for Array<'_>`). Legal nowhere else.
    Hole,
    /// The conjunct `'a: Concrete`: whatever binds the cell is fully known,
    /// with no open cell and no ⊥ in it, so a type-directed builtin can act
    /// on it. Legal only as a constraint.
    Concrete,
    /// The conjunct `'a: Function`: whatever binds the cell is a function
    /// type, so a builtin wrapping a function can read its signature.
    /// Legal only as a constraint.
    Function,
    /// The conjunct `'a: Singleton`: whatever binds the cell is one type,
    /// not a union of several (arithmetic is `fn<'a: Number + Singleton>`).
    /// Legal only as a constraint.
    Singleton,
    /// The conjunct `'a: OneNumber`: whatever binds the cell holds at
    /// most one numeric type (`[i64, null]`, not `[i64, f64, null]`).
    /// Legal only as a constraint.
    OneNumber,
    /// The conjunct `'a: Discernible`: no union anywhere in whatever binds
    /// the cell holds two members with one runtime form
    /// ([`Type::rep_ambiguity`]), so its values compare and hash as their
    /// types do. Legal only as a constraint.
    Discernible,
    /// The conjunct `'a: Ordered`: `Discernible`, and no reference where a
    /// comparison looks ([`Type::ref_in`]): a reference's value is its
    /// cell, which neither orders nor names what it points to, so values
    /// of the type order, hash and compare by value alone. Legal only as
    /// a constraint.
    Ordered,
}

/// Whether a reference may be written through: `&T` reads, `&mut T`
/// also writes.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, netidx_derive::Pack,
)]
pub enum Mutability {
    Shared,
    Mut,
}

impl Mutability {
    fn tag(self) -> u8 {
        match self {
            Mutability::Shared => tag::BYREF,
            Mutability::Mut => tag::BYREF_MUT,
        }
    }

    /// The type and expression prefix: `&` or `&mut `.
    pub fn prefix(self) -> &'static str {
        match self {
            Mutability::Shared => "&",
            Mutability::Mut => "&mut ",
        }
    }
}

mod tag {
    pub const BOTTOM: u8 = 0;
    pub const ANY: u8 = 1;
    pub const PRIMITIVE: u8 = 2;
    pub const REF: u8 = 3;
    pub const FN: u8 = 4;
    pub const SET: u8 = 5;
    pub const TVAR: u8 = 6;
    pub const ERROR: u8 = 7;
    pub const ARRAY: u8 = 8;
    pub const LIST: u8 = 9;
    pub const BYREF: u8 = 10;
    pub const TUPLE: u8 = 11;
    pub const STRUCT: u8 = 12;
    pub const VARIANT: u8 = 13;
    pub const MAP: u8 = 14;
    pub const ABSTRACT: u8 = 15;
    pub const APP: u8 = 16;
    pub const HOLE: u8 = 17;
    pub const CONCRETE: u8 = 18;
    pub const FUNCTION: u8 = 19;
    pub const SINGLETON: u8 = 20;
    pub const ONE_NUMBER: u8 = 21;
    pub const BYREF_MUT: u8 = 22;
    pub const DISCERNIBLE: u8 = 23;
    pub const ORDERED: u8 = 24;
}

pub(super) fn key_text(s: &str, out: &mut Vec<u8>) {
    encode_varint(s.len() as u64, out);
    out.put_slice(s.as_bytes());
}

fn key_list(ts: &Arc<[Type]>, out: &mut Vec<u8>) {
    let keep = || KeyedNode::Types(ts.clone());
    image::shared_key(<[Type]>::as_ptr(ts) as usize, keep, out, |out| {
        encode_varint(ts.len() as u64, out);
        for t in ts.iter() {
            t.content_key(out);
        }
    })
}

fn key_one(t: &Arc<Type>, out: &mut Vec<u8>) {
    let keep = || KeyedNode::Type(t.clone());
    image::shared_key(Arc::as_ptr(t) as usize, keep, out, |out| t.content_key(out))
}

impl TypeRef {
    fn content_key(&self, out: &mut Vec<u8>) {
        let TypeRef { scope, name, params, pos, ori, resolved } = self;
        key_text(scope, out);
        key_text(name, out);
        key_list(params, out);
        match pos {
            None => out.put_u8(0),
            Some(p) => {
                out.put_u8(1);
                out.put_i32_le(p.line);
                out.put_i32_le(p.column);
            }
        }
        out.put_u64_le(ori.as_ref().map_or(0, |o| Arc::as_ptr(o) as usize as u64));
        out.put_u64_le(Arc::as_ptr(resolved) as *const () as usize as u64);
    }
}

impl Type {
    /// The canonical bytes the image keys this type by: the structure,
    /// with every shared leaf (a variable, a resolution cell, an
    /// origin, a lambda ids cell) by identity.
    pub(crate) fn content_key(&self, out: &mut Vec<u8>) {
        ensure_sufficient(|| self.content_key_inner(out))
    }

    fn content_key_inner(&self, out: &mut Vec<u8>) {
        match self {
            Type::Bottom => out.put_u8(tag::BOTTOM),
            Type::Any => out.put_u8(tag::ANY),
            Type::Hole => out.put_u8(tag::HOLE),
            Type::Concrete => out.put_u8(tag::CONCRETE),
            Type::Function => out.put_u8(tag::FUNCTION),
            Type::Singleton => out.put_u8(tag::SINGLETON),
            Type::OneNumber => out.put_u8(tag::ONE_NUMBER),
            Type::Discernible => out.put_u8(tag::DISCERNIBLE),
            Type::Ordered => out.put_u8(tag::ORDERED),
            Type::Primitive(p) => {
                out.put_u8(tag::PRIMITIVE);
                out.put_u64_le(p.bits() as u64);
            }
            Type::Ref(r) => {
                out.put_u8(tag::REF);
                r.content_key(out);
            }
            Type::Fn(f) => {
                out.put_u8(tag::FN);
                let keep = || KeyedNode::Fn(f.clone());
                image::shared_key(Arc::as_ptr(f) as usize, keep, out, |out| {
                    f.content_key(out)
                });
            }
            Type::TVar(tv) => {
                out.put_u8(tag::TVAR);
                out.put_u64_le(tv.wrapper_addr() as u64);
            }
            Type::Set(ts) => {
                out.put_u8(tag::SET);
                key_list(ts, out);
            }
            Type::Tuple(ts) => {
                out.put_u8(tag::TUPLE);
                key_list(ts, out);
            }
            Type::Error(t) => {
                out.put_u8(tag::ERROR);
                key_one(t, out);
            }
            Type::Array(t) => {
                out.put_u8(tag::ARRAY);
                key_one(t, out);
            }
            Type::List(t) => {
                out.put_u8(tag::LIST);
                key_one(t, out);
            }
            Type::ByRef(m, t) => {
                out.put_u8(m.tag());
                key_one(t, out);
            }
            Type::Struct(fs) => {
                out.put_u8(tag::STRUCT);
                image::shared_key(
                    <[(ArcStr, Type, WrittenAt)]>::as_ptr(fs) as usize,
                    || KeyedNode::Fields(fs.clone()),
                    out,
                    |out| {
                        encode_varint(fs.len() as u64, out);
                        for (n, t, _) in fs.iter() {
                            key_text(n, out);
                            t.content_key(out);
                        }
                    },
                );
            }
            Type::Variant(name, ts, _) => {
                out.put_u8(tag::VARIANT);
                key_text(name, out);
                key_list(ts, out);
            }
            Type::Map { key, value } => {
                out.put_u8(tag::MAP);
                key_one(key, out);
                key_one(value, out);
            }
            Type::Abstract { id, params } => {
                out.put_u8(tag::ABSTRACT);
                out.put_u64_le(id.0);
                key_list(params, out);
            }
            Type::App(c, a) => {
                out.put_u8(tag::APP);
                key_one(c, out);
                key_one(a, out);
            }
        }
    }

    fn shape_len(&self) -> usize {
        ensure_sufficient(|| self.shape_len_inner())
    }

    fn shape_len_inner(&self) -> usize {
        1 + match self {
            Type::Bottom
            | Type::Any
            | Type::Hole
            | Type::Concrete
            | Type::Function
            | Type::OneNumber
            | Type::Discernible
            | Type::Ordered
            | Type::Singleton => 0,
            Type::Primitive(p) => p.encoded_len(),
            Type::Ref(r) => r.encoded_len(),
            Type::Fn(f) => f.encoded_len(),
            Type::TVar(tv) => tv.encoded_len(),
            Type::Set(ts) | Type::Tuple(ts) => ts.encoded_len(),
            Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(_, t) => {
                t.encoded_len()
            }
            Type::Struct(fs) => fs.encoded_len(),
            Type::Variant(name, ts, _) => name.encoded_len() + ts.encoded_len(),
            Type::Map { key, value } => key.encoded_len() + value.encoded_len(),
            Type::Abstract { id, params } => id.encoded_len() + params.encoded_len(),
            Type::App(c, a) => c.encoded_len() + a.encoded_len(),
        }
    }

    fn shape_encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        ensure_sufficient(|| self.shape_encode_inner(buf))
    }

    fn shape_encode_inner(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        match self {
            Type::Bottom => Ok(buf.put_u8(tag::BOTTOM)),
            Type::Any => Ok(buf.put_u8(tag::ANY)),
            Type::Hole => Ok(buf.put_u8(tag::HOLE)),
            Type::Concrete => Ok(buf.put_u8(tag::CONCRETE)),
            Type::Function => Ok(buf.put_u8(tag::FUNCTION)),
            Type::Singleton => Ok(buf.put_u8(tag::SINGLETON)),
            Type::OneNumber => Ok(buf.put_u8(tag::ONE_NUMBER)),
            Type::Discernible => Ok(buf.put_u8(tag::DISCERNIBLE)),
            Type::Ordered => Ok(buf.put_u8(tag::ORDERED)),
            Type::Primitive(p) => {
                buf.put_u8(tag::PRIMITIVE);
                p.encode(buf)
            }
            Type::Ref(r) => {
                buf.put_u8(tag::REF);
                r.encode(buf)
            }
            Type::Fn(f) => {
                buf.put_u8(tag::FN);
                f.encode(buf)
            }
            Type::TVar(tv) => {
                buf.put_u8(tag::TVAR);
                tv.encode(buf)
            }
            Type::Set(ts) => {
                buf.put_u8(tag::SET);
                ts.encode(buf)
            }
            Type::Tuple(ts) => {
                buf.put_u8(tag::TUPLE);
                ts.encode(buf)
            }
            Type::Error(t) => {
                buf.put_u8(tag::ERROR);
                t.encode(buf)
            }
            Type::Array(t) => {
                buf.put_u8(tag::ARRAY);
                t.encode(buf)
            }
            Type::List(t) => {
                buf.put_u8(tag::LIST);
                t.encode(buf)
            }
            Type::ByRef(m, t) => {
                buf.put_u8(m.tag());
                t.encode(buf)
            }
            Type::Struct(fs) => {
                buf.put_u8(tag::STRUCT);
                fs.encode(buf)
            }
            Type::Variant(name, ts, _) => {
                buf.put_u8(tag::VARIANT);
                name.encode(buf)?;
                ts.encode(buf)
            }
            Type::Map { key, value } => {
                buf.put_u8(tag::MAP);
                key.encode(buf)?;
                value.encode(buf)
            }
            Type::Abstract { id, params } => {
                buf.put_u8(tag::ABSTRACT);
                id.encode(buf)?;
                params.encode(buf)
            }
            Type::App(c, a) => {
                buf.put_u8(tag::APP);
                c.encode(buf)?;
                a.encode(buf)
            }
        }
    }

    fn shape_decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if !buf.has_remaining() {
            return Err(PackError::BufferShort);
        }
        Ok(match buf.get_u8() {
            tag::BOTTOM => Type::Bottom,
            tag::ANY => Type::Any,
            tag::HOLE => Type::Hole,
            tag::CONCRETE => Type::Concrete,
            tag::FUNCTION => Type::Function,
            tag::SINGLETON => Type::Singleton,
            tag::ONE_NUMBER => Type::OneNumber,
            tag::DISCERNIBLE => Type::Discernible,
            tag::ORDERED => Type::Ordered,
            tag::PRIMITIVE => Type::Primitive(PackTrait::decode(buf)?),
            tag::REF => Type::Ref(PackTrait::decode(buf)?),
            tag::FN => Type::Fn(PackTrait::decode(buf)?),
            tag::TVAR => Type::TVar(PackTrait::decode(buf)?),
            tag::SET => Type::Set(PackTrait::decode(buf)?),
            tag::TUPLE => Type::Tuple(PackTrait::decode(buf)?),
            tag::ERROR => Type::Error(PackTrait::decode(buf)?),
            tag::ARRAY => Type::Array(PackTrait::decode(buf)?),
            tag::LIST => Type::List(PackTrait::decode(buf)?),
            tag::BYREF => Type::ByRef(Mutability::Shared, PackTrait::decode(buf)?),
            tag::BYREF_MUT => Type::ByRef(Mutability::Mut, PackTrait::decode(buf)?),
            tag::STRUCT => Type::Struct(PackTrait::decode(buf)?),
            tag::VARIANT => {
                let name = PackTrait::decode(buf)?;
                Type::Variant(name, PackTrait::decode(buf)?, WrittenAt::NOWHERE)
            }
            tag::MAP => {
                let key = PackTrait::decode(buf)?;
                Type::Map { key, value: PackTrait::decode(buf)? }
            }
            tag::ABSTRACT => {
                let id = PackTrait::decode(buf)?;
                Type::Abstract { id, params: PackTrait::decode(buf)? }
            }
            tag::APP => {
                let c = PackTrait::decode(buf)?;
                Type::App(c, PackTrait::decode(buf)?)
            }
            _ => return Err(PackError::UnknownTag),
        })
    }
}

/// Under an image session a type is an object keyed by its content,
/// written once and referenced afterwards.
impl PackTrait for Type {
    fn encoded_len(&self) -> usize {
        if image::is_encoding() { image::type_len(self) } else { self.shape_len() }
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        if image::is_encoding() {
            image::type_encode(self, buf, |b| self.shape_encode(b))
        } else {
            self.shape_encode(buf)
        }
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        // outside bytes reach this (a value's abstract params): one level
        // per byte
        ensure_sufficient(|| {
            if image::is_decoding() {
                image::object_decode(buf, |b| Self::shape_decode(b), |b| Self::decode(b))
            } else {
                Self::shape_decode(buf)
            }
        })
    }
}

/// Structural equality with content-Arc pointer shortcuts (the
/// copy-on-write walks share aggressively). Exhaustive on `self` so a
/// new variant fails to compile.
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Bottom, _) => matches!(other, Type::Bottom),
            (Type::Any, _) => matches!(other, Type::Any),
            (Type::Hole, _) => matches!(other, Type::Hole),
            (Type::Concrete, _) => matches!(other, Type::Concrete),
            (Type::Function, _) => matches!(other, Type::Function),
            (Type::Singleton, _) => matches!(other, Type::Singleton),
            (Type::OneNumber, _) => matches!(other, Type::OneNumber),
            (Type::Discernible, _) => matches!(other, Type::Discernible),
            (Type::Ordered, _) => matches!(other, Type::Ordered),
            (Type::Primitive(a), _) => matches!(other, Type::Primitive(b) if a == b),
            _ => ensure_sufficient(|| self.eq_composite(other)),
        }
    }
}

impl Eq for Type {}

impl Type {
    fn eq_composite(&self, other: &Self) -> bool {
        fn slice_eq(a: &Arc<[Type]>, b: &Arc<[Type]>) -> bool {
            (**a).as_ptr() == (**b).as_ptr() || **a == **b
        }
        fn one_eq(a: &Arc<Type>, b: &Arc<Type>) -> bool {
            Arc::ptr_eq(a, b) || a == b
        }
        match self {
            Type::Bottom => matches!(other, Type::Bottom),
            Type::Any => matches!(other, Type::Any),
            Type::Hole => matches!(other, Type::Hole),
            Type::Concrete => matches!(other, Type::Concrete),
            Type::Function => matches!(other, Type::Function),
            Type::Singleton => matches!(other, Type::Singleton),
            Type::OneNumber => matches!(other, Type::OneNumber),
            Type::Discernible => matches!(other, Type::Discernible),
            Type::Ordered => matches!(other, Type::Ordered),
            Type::Primitive(a) => matches!(other, Type::Primitive(b) if a == b),
            Type::Ref(a) => matches!(other, Type::Ref(b) if a == b),
            Type::Fn(a) => {
                matches!(other, Type::Fn(b) if Arc::ptr_eq(a, b) || a == b)
            }
            Type::Set(a) => matches!(other, Type::Set(b) if slice_eq(a, b)),
            Type::TVar(a) => matches!(other, Type::TVar(b) if a == b),
            Type::Error(a) => matches!(other, Type::Error(b) if one_eq(a, b)),
            Type::Array(a) => matches!(other, Type::Array(b) if one_eq(a, b)),
            Type::List(a) => matches!(other, Type::List(b) if one_eq(a, b)),
            Type::ByRef(m, a) => {
                matches!(other, Type::ByRef(n, b) if m == n && one_eq(a, b))
            }
            Type::Tuple(a) => matches!(other, Type::Tuple(b) if slice_eq(a, b)),
            Type::Struct(a) => matches!(
                other,
                Type::Struct(b) if (**a).as_ptr() == (**b).as_ptr() || **a == **b
            ),
            Type::Variant(t0, a, _) => {
                matches!(other, Type::Variant(t1, b, _) if t0 == t1 && slice_eq(a, b))
            }
            Type::Map { key: k0, value: v0 } => matches!(
                other,
                Type::Map { key: k1, value: v1 } if one_eq(k0, k1) && one_eq(v0, v1)
            ),
            Type::Abstract { id: i0, params: p0 } => matches!(
                other,
                Type::Abstract { id: i1, params: p1 } if i0 == i1 && slice_eq(p0, p1)
            ),
            Type::App(c0, a0) => matches!(
                other,
                Type::App(c1, a1) if one_eq(c0, c1) && one_eq(a0, a1)
            ),
        }
    }

    /// The variant's position in declaration order (its image tag).
    fn rank(&self) -> u8 {
        match self {
            Type::Bottom => tag::BOTTOM,
            Type::Any => tag::ANY,
            Type::Primitive(_) => tag::PRIMITIVE,
            Type::Ref(_) => tag::REF,
            Type::Fn(_) => tag::FN,
            Type::Set(_) => tag::SET,
            Type::TVar(_) => tag::TVAR,
            Type::Error(_) => tag::ERROR,
            Type::Array(_) => tag::ARRAY,
            Type::List(_) => tag::LIST,
            Type::ByRef(m, _) => m.tag(),
            Type::Tuple(_) => tag::TUPLE,
            Type::Struct(_) => tag::STRUCT,
            Type::Variant(..) => tag::VARIANT,
            Type::Map { .. } => tag::MAP,
            Type::Abstract { .. } => tag::ABSTRACT,
            Type::App(..) => tag::APP,
            Type::Hole => tag::HOLE,
            Type::Concrete => tag::CONCRETE,
            Type::Function => tag::FUNCTION,
            Type::Singleton => tag::SINGLETON,
            Type::OneNumber => tag::ONE_NUMBER,
            Type::Discernible => tag::DISCERNIBLE,
            Type::Ordered => tag::ORDERED,
        }
    }

    /// Two types of one variant, field by field in declaration order.
    fn cmp_fields(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Type::Primitive(a), Type::Primitive(b)) => a.cmp(b),
            (Type::Ref(a), Type::Ref(b)) => a.cmp(b),
            (Type::Fn(a), Type::Fn(b)) => a.cmp(b),
            (Type::TVar(a), Type::TVar(b)) => a.cmp(b),
            (Type::Set(a), Type::Set(b)) | (Type::Tuple(a), Type::Tuple(b)) => a.cmp(b),
            (Type::Error(a), Type::Error(b))
            | (Type::Array(a), Type::Array(b))
            | (Type::List(a), Type::List(b))
            | (Type::ByRef(_, a), Type::ByRef(_, b)) => a.cmp(b),
            (Type::Struct(a), Type::Struct(b)) => a.cmp(b),
            (Type::Variant(t0, a, w0), Type::Variant(t1, b, w1)) => {
                t0.cmp(t1).then_with(|| a.cmp(b)).then_with(|| w0.cmp(w1))
            }
            (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
                k0.cmp(k1).then_with(|| v0.cmp(v1))
            }
            (
                Type::Abstract { id: i0, params: p0 },
                Type::Abstract { id: i1, params: p1 },
            ) => i0.cmp(i1).then_with(|| p0.cmp(p1)),
            (Type::App(c0, a0), Type::App(c1, a1)) => c0.cmp(c1).then_with(|| a0.cmp(a1)),
            _ => Ordering::Equal,
        }
    }
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Type {
    fn cmp(&self, other: &Self) -> Ordering {
        self.rank().cmp(&other.rank()).then_with(|| match self {
            Type::Bottom
            | Type::Any
            | Type::Hole
            | Type::Concrete
            | Type::Function
            | Type::Singleton
            | Type::OneNumber
            | Type::Discernible
            | Type::Ordered
            | Type::Primitive(_) => self.cmp_fields(other),
            _ => ensure_sufficient(|| self.cmp_fields(other)),
        })
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.rank().hash(state);
        match self {
            Type::Bottom
            | Type::Any
            | Type::Hole
            | Type::Concrete
            | Type::Function
            | Type::OneNumber
            | Type::Discernible
            | Type::Ordered
            | Type::Singleton => (),
            Type::Primitive(p) => p.hash(state),
            t => ensure_sufficient(|| match t {
                Type::Ref(r) => r.hash(state),
                Type::Fn(f) => f.hash(state),
                Type::TVar(tv) => tv.hash(state),
                Type::Set(ts) | Type::Tuple(ts) => ts.hash(state),
                Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(_, t) => {
                    t.hash(state)
                }
                Type::Struct(fs) => fs.hash(state),
                Type::Variant(tag, ts, at) => {
                    tag.hash(state);
                    ts.hash(state);
                    at.hash(state)
                }
                Type::Map { key, value } => {
                    key.hash(state);
                    value.hash(state)
                }
                Type::Abstract { id, params } => {
                    id.hash(state);
                    params.hash(state)
                }
                Type::App(c, a) => {
                    c.hash(state);
                    a.hash(state)
                }
                Type::Bottom
                | Type::Any
                | Type::Hole
                | Type::Concrete
                | Type::Function
                | Type::Singleton
                | Type::OneNumber
                | Type::Discernible
                | Type::Ordered
                | Type::Primitive(_) => (),
            }),
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::Bottom
    }
}

impl Type {
    /// A `'static` ⊥ for a node whose type is bottom: with drop glue,
    /// `&Type::Bottom` is not promoted.
    pub const BOTTOM: &'static Type = &Type::Bottom;
}

/// Field drop glue runs after `drop` returns and cannot be guarded, so
/// a composite that owns the last reference to its content drops its
/// fields here, under the guard, out of a `ManuallyDrop` copy; the glue
/// then drops the leaf left behind. A shared content only decrements.
impl Drop for Type {
    fn drop(&mut self) {
        use std::{mem::ManuallyDrop, ptr};
        let last = match self {
            Type::Bottom
            | Type::Any
            | Type::Hole
            | Type::Concrete
            | Type::Function
            | Type::Singleton
            | Type::OneNumber
            | Type::Discernible
            | Type::Ordered
            | Type::Primitive(_)
            | Type::TVar(_) => false,
            Type::Ref(tr) => tr.params.is_unique() || tr.resolved.is_unique(),
            Type::Fn(f) => f.is_unique(),
            Type::Set(ts) | Type::Tuple(ts) | Type::Variant(_, ts, _) => ts.is_unique(),
            Type::Abstract { params, .. } => params.is_unique(),
            Type::Error(a) | Type::Array(a) | Type::List(a) | Type::ByRef(_, a) => {
                a.is_unique()
            }
            Type::Struct(fs) => fs.is_unique(),
            Type::Map { key: a, value: b } | Type::App(a, b) => {
                a.is_unique() || b.is_unique()
            }
        };
        if !last {
            return;
        }
        let t = ManuallyDrop::new(mem::take(self));
        // SAFETY: each field is read out once and `t` is never dropped.
        ensure_sufficient(|| unsafe {
            match &*t {
                Type::Bottom
                | Type::Any
                | Type::Hole
                | Type::Concrete
                | Type::Function
                | Type::Singleton
                | Type::OneNumber
                | Type::Discernible
                | Type::Ordered
                | Type::Primitive(_) => (),
                Type::Ref(r) => drop(ptr::read(r)),
                Type::Fn(f) => drop(ptr::read(f)),
                Type::TVar(tv) => drop(ptr::read(tv)),
                Type::Set(ts) | Type::Tuple(ts) => drop(ptr::read(ts)),
                Type::Error(a) | Type::Array(a) | Type::List(a) | Type::ByRef(_, a) => {
                    drop(ptr::read(a))
                }
                Type::Struct(fs) => drop(ptr::read(fs)),
                Type::Variant(tag, ts, _) => {
                    drop(ptr::read(tag));
                    drop(ptr::read(ts))
                }
                Type::Map { key, value } => {
                    drop(ptr::read(key));
                    drop(ptr::read(value))
                }
                Type::Abstract { id: _, params } => drop(ptr::read(params)),
                Type::App(c, a) => {
                    drop(ptr::read(c));
                    drop(ptr::read(a))
                }
            }
        })
    }
}

/// A classifiable resolution failure from [`Type::lookup_ref`]: the
/// name, and where it is written.
#[derive(Debug)]
pub struct UnresolvableRef {
    pub name: ModPath,
    pub scope: ModPath,
    pub pos: Option<SourcePosition>,
    pub ori: Option<Arc<Origin>>,
}

impl UnresolvableRef {
    pub fn of(tr: &TypeRef) -> Self {
        Self {
            name: tr.name.clone(),
            scope: tr.scope.clone(),
            pos: tr.pos,
            ori: tr.ori.clone(),
        }
    }

    /// The error a report names: a mistake in the path when there is
    /// one, else this.
    pub fn error(tr: &TypeRef, env: &Env) -> anyhow::Error {
        tr.resolve_error(env).unwrap_or_else(|| anyhow::Error::new(Self::of(tr)))
    }
}

/// `undefined type T[ in m][ at <pos>[ in <source>]]`: the scope only when
/// it is a module a program names (a block's or function's is minted).
impl std::fmt::Display for UnresolvableRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "undefined type {}", self.name)?;
        let minted =
            netidx_core::path::Path::parts(&self.scope.0).any(|p| p.starts_with('#'));
        if !minted && &*self.scope.0 != "/" {
            write!(f, " in {}", self.scope)?;
        }
        if let Some(pos) = self.pos {
            write!(f, " at {pos}")?;
            match self.ori.as_ref().map(|o| &o.source) {
                None | Some(Source::Internal(_) | Source::Unspecified) => (),
                Some(source) => write!(f, " in {source}")?,
            }
        }
        Ok(())
    }
}

impl std::error::Error for UnresolvableRef {}

impl Type {
    /// Read-only walk over this type's immediate structural children.
    /// `TVar` is a leaf: cell contents are per-walk policy. A recursive
    /// walk matches its interesting arms and routes the rest here; see
    /// [`Self::cow_children`] for rebuild walks.
    pub(crate) fn try_for_each_child<B>(
        &self,
        f: &mut impl FnMut(&Type) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        match self {
            Type::Bottom
            | Type::Any
            | Type::Primitive(_)
            | Type::TVar(_)
            | Type::Hole
            | Type::Concrete
            | Type::Singleton
            | Type::OneNumber
            | Type::Discernible
            | Type::Ordered
            | Type::Function => ControlFlow::Continue(()),
            Type::App(c, a) => {
                f(c)?;
                f(a)
            }
            Type::Ref(tr) => {
                for t in tr.params.iter() {
                    f(t)?;
                }
                ControlFlow::Continue(())
            }
            Type::Abstract { id: _, params } => {
                for t in params.iter() {
                    f(t)?;
                }
                ControlFlow::Continue(())
            }
            Type::Set(ts) | Type::Tuple(ts) | Type::Variant(_, ts, _) => {
                for t in ts.iter() {
                    f(t)?;
                }
                ControlFlow::Continue(())
            }
            Type::Struct(fs) => {
                for (_, t, _) in fs.iter() {
                    f(t)?;
                }
                ControlFlow::Continue(())
            }
            Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(_, t) => f(t),
            Type::Map { key, value } => {
                f(key)?;
                f(value)
            }
            Type::Fn(ft) => ft.try_for_each_type(f),
        }
    }

    /// The definitions this type's filled cells name, and theirs, by
    /// [`ResolvedRef::def_key`]: through bindings and conjuncts.
    #[doc(hidden)]
    pub fn named_defs(
        &self,
        cells: &mut AHashSet<usize>,
        out: &mut AHashMap<usize, sync::Arc<ResolvedRef>>,
    ) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                if cells.insert(tv.cell_addr()) {
                    if let Some(b) = tv.binding() {
                        b.named_defs(cells, out)
                    }
                    for c in tv.cell_constraints() {
                        c.named_defs(cells, out)
                    }
                }
            }
            Type::Ref(tr) => {
                if let Some(r) = tr.resolved()
                    && !out.contains_key(&r.def_key())
                {
                    out.insert(r.def_key(), r.clone());
                    r.typ.named_defs(cells, out);
                    for c in r.params.iter().filter_map(|(_, c)| c.as_ref()) {
                        c.named_defs(cells, out)
                    }
                }
                self.for_each_child(&mut |c| c.named_defs(cells, out))
            }
            Type::Fn(ft) => ft.for_each_part(&mut |t, _| t.named_defs(cells, out)),
            t => t.for_each_child(&mut |c| c.named_defs(cells, out)),
        })
    }

    /// [`Self::try_for_each_child`] without early exit.
    #[doc(hidden)]
    pub fn for_each_child(&self, f: &mut impl FnMut(&Type)) {
        let _ = self.try_for_each_child::<()>(&mut |t| {
            f(t);
            ControlFlow::Continue(())
        });
    }

    /// Rebuild this type's immediate structural children through `f`
    /// (`None` from `f` means unchanged); `None` when nothing changed.
    /// Leaves, `TVar` included, return `None`. `Ref` params rebuild
    /// through [`TypeRef::with_params`], sharing the resolution cell.
    #[doc(hidden)]
    pub fn cow_children(
        &self,
        f: &mut impl FnMut(&Type) -> Option<Type>,
    ) -> Option<Type> {
        match self {
            Type::Bottom
            | Type::Any
            | Type::Primitive(_)
            | Type::TVar(_)
            | Type::Hole
            | Type::Concrete
            | Type::Singleton
            | Type::OneNumber
            | Type::Discernible
            | Type::Ordered
            | Type::Function => None,
            Type::App(c, a) => match (f(c), f(a)) {
                (None, None) => None,
                (c2, a2) => Some(Type::app(
                    c2.unwrap_or_else(|| (**c).clone()),
                    a2.unwrap_or_else(|| (**a).clone()),
                )),
            },
            Type::Ref(tr) => Type::cow_slice(&tr.params, |t| f(t))
                .map(|params| Type::Ref(Arc::new(tr.with_params(params)))),
            Type::Abstract { id, params } => Type::cow_slice(params, |t| f(t))
                .map(|params| Type::Abstract { id: *id, params }),
            Type::Error(t) => f(t).map(|t| Type::Error(Arc::new(t))),
            Type::Array(t) => f(t).map(|t| Type::Array(Arc::new(t))),
            Type::List(t) => f(t).map(|t| Type::List(Arc::new(t))),
            Type::ByRef(m, t) => f(t).map(|t| Type::ByRef(*m, Arc::new(t))),
            Type::Map { key, value } => match (f(key), f(value)) {
                (None, None) => None,
                (k, v) => Some(Type::Map {
                    key: k.map(Arc::new).unwrap_or_else(|| key.clone()),
                    value: v.map(Arc::new).unwrap_or_else(|| value.clone()),
                }),
            },
            Type::Tuple(ts) => Type::cow_slice(ts, |t| f(t)).map(Type::Tuple),
            Type::Variant(tag, ts, at) => Type::cow_slice(ts, |t| f(t))
                .map(|ts| Type::Variant(tag.clone(), ts, *at)),
            Type::Set(ts) => Type::cow_slice(ts, |t| f(t)).map(Type::Set),
            Type::Struct(fs) => {
                Type::cow_slice(fs, |(n, t, at)| f(t).map(|t| (n.clone(), t, *at)))
                    .map(Type::Struct)
            }
            Type::Fn(ft) => ft.cow_walk(|t| f(t)).map(|ft| Type::Fn(Arc::new(ft))),
        }
    }

    pub fn empty_tvar() -> Self {
        Type::TVar(TVar::default())
    }

    /// Apply a constructor to an argument: a concrete constructor (a
    /// type with a hole) is filled, a variable stays an application
    /// until it binds.
    pub fn app(ctor: Type, arg: Type) -> Type {
        match &ctor {
            Type::TVar(_) => Type::App(Arc::new(ctor), Arc::new(arg)),
            c => c
                .fill_hole(&arg)
                .unwrap_or_else(|| Type::App(Arc::new(ctor), Arc::new(arg))),
        }
    }

    /// A bound constructor's application, filled: the constructor
    /// dereferenced through its cell and its hole replaced by `arg`.
    /// `None` while the constructor is an open variable.
    pub(crate) fn app_filled(ctor: &Type, arg: &Type) -> Option<Type> {
        ctor.with_deref(|c| match c {
            None | Some(Type::TVar(_)) => None,
            Some(c) => c.fill_hole(arg),
        })
    }

    /// The reference behind a type variable: a cell bound (through
    /// other cells) to a reference or to a filled constructor
    /// application.
    pub(crate) fn ref_behind(&self) -> Option<Type> {
        match self {
            Type::TVar(_) => self.with_deref(|t| match t {
                Some(t @ Type::Ref(_)) => Some(t.clone()),
                _ => None,
            }),
            _ => None,
        }
    }

    /// The other side of a constructor application, dereferenced and
    /// [`Self::decompose`]d: a reference by name, a bare alias through
    /// its expansion.
    #[doc(hidden)]
    pub fn app_split(t: &Type, env: &Env) -> Result<Option<(Type, Type)>> {
        let Some(t) = t.deref_cloned() else { return Ok(None) };
        if let Some(parts) = t.decompose() {
            return Ok(Some(parts));
        }
        match &t {
            Type::Ref(tr) if tr.params.is_empty() => Ok(t.lookup_ref(env)?.decompose()),
            _ => Ok(None),
        }
    }

    /// [`Self::app_split`] for a receiver that lost its name (a cell
    /// holding a typedef's expansion): each registered head of the
    /// constructor variable's trait bounds is tried, and the one that
    /// contains the receiver and determines the element is the
    /// constructor. Only a `commit` binds the receiver's open cells; a
    /// probe decides on a copy.
    pub(crate) fn app_split_for(
        ctor: &Type,
        t: &Type,
        env: &Env,
        commit: bool,
    ) -> Result<Option<(Type, Type)>> {
        if let Some(parts) = Self::app_split(t, env)? {
            return Ok(Some(parts));
        }
        let Some(t) = t.deref_cloned() else { return Ok(None) };
        let Type::TVar(cv) = ctor else { return Ok(None) };
        // A head is kept when it contains the receiver and determines the
        // element; a proper subtype (`[`Nil]` under `List<'_>`) leaves the
        // element open and is not this constructor. Each head is tried on
        // a private copy of the receiver, so a rejected one binds nothing.
        let fits = |head: &Type, t: &Type| -> Result<Option<(Type, Type)>> {
            let head = head.reset_tvars();
            let elem = Type::empty_tvar();
            let Some(filled) = head.fill_hole(&elem) else { return Ok(None) };
            Ok((filled.contains(env, t)? && elem.with_deref(|e| e.is_some()))
                .then(|| (head.resolve_tvars(), elem.resolve_tvars())))
        };
        for c in cv.cell_constraints().iter() {
            let Type::Ref(tr) = c else { continue };
            let Some(tid) = env.trait_of_ref(tr) else { continue };
            let Some(heads) = env.impls_of(tid) else { continue };
            for im in heads.iter() {
                if !matches!(im.target, Type::Ref(_)) {
                    continue;
                }
                let Some(copy) = fits(&im.target, &t.reset_tvars())? else { continue };
                if !commit {
                    return Ok(Some(copy));
                }
                if let Some(r) = fits(&im.target, &t)? {
                    if graphix_dbg_bind() {
                        eprintln!("APP-SPLIT recovered ctor={:?} elem={:?}", r.0, r.1);
                    }
                    return Ok(Some(r));
                }
            }
        }
        Ok(None)
    }

    /// How a trait signature spells its receiver: `applied` if `self`
    /// occurs as a constructor (`self<'a>`), `bare` if it occurs as a
    /// type. A trait uses one form throughout.
    pub(crate) fn self_shape(&self, applied: &mut bool, bare: &mut bool) {
        ensure_sufficient(|| match self {
            Type::App(c, a) if matches!(&**c, Type::TVar(tv) if &*tv.name == "self") => {
                *applied = true;
                a.self_shape(applied, bare)
            }
            Type::TVar(tv) if &*tv.name == "self" => *bare = true,
            t => t.for_each_child(&mut |c| c.self_shape(applied, bare)),
        })
    }

    /// The number of holes in this type.
    #[doc(hidden)]
    pub fn holes(&self) -> usize {
        ensure_sufficient(|| match self {
            Type::Hole => 1,
            t => {
                let mut n = 0;
                t.for_each_child(&mut |c| n += c.holes());
                n
            }
        })
    }

    /// Pre-unify a declared parameter type with an argument's type
    /// before the argument typechecks, so an unannotated callback's
    /// parameters take the declared types. A function-typed argument
    /// unifies its parameter positions only; anything else whole.
    #[doc(hidden)]
    pub fn pre_unify_arg(env: &Env, declared: &Type, actual: &Type) -> Result<()> {
        let d = declared.deref_cloned();
        let a = actual.deref_cloned();
        match (&d, &a) {
            (Some(Type::Fn(d)), Some(Type::Fn(a))) => d.pre_unify_params(env, a),
            // a union formal two of whose members admit the argument leaves
            // the choice to the argument's own check
            (Some(Type::Set(ms)), _) => {
                let mut admit = 0;
                for m in ms.iter() {
                    if m.contains_with_flags(BitFlags::empty(), env, actual)? {
                        admit += 1;
                    }
                }
                match admit {
                    0 | 1 => declared.contains(env, actual).map(|_| ()),
                    _ => Ok(()),
                }
            }
            _ => declared.contains(env, actual).map(|_| ()),
        }
    }

    /// The name of the element a constructor quantifier `q` is applied
    /// to; a fn type lists it among its quantifiers, so a call copies it.
    pub fn elem_name(q: &str) -> ArcStr {
        format_compact!("{q}#elem").as_str().into()
    }

    /// The type of a parameter whose written type is the trait `tr`:
    /// the fresh bounded quantifier `tv`, applied to a fresh element
    /// when the trait is a constructor trait (`|c: Collection|` ≡
    /// `'c: Collection, c: 'c<'e>`).
    #[doc(hidden)]
    pub fn trait_param(env: &Env, tv: TVar, tr: &TypeRef) -> Type {
        let hole = env
            .trait_of_ref(tr)
            .and_then(|tid| env.trait_def(tid))
            .is_some_and(|d| d.hole);
        if hole {
            Type::App(Arc::new(Type::TVar(tv)), Arc::new(Type::empty_tvar()))
        } else {
            Type::TVar(tv)
        }
    }

    /// Whether `tc` bounds by a constructor trait (`Collection`), whose
    /// implementations are constructors (`Array<'_>`).
    #[doc(hidden)]
    pub fn is_ctor_trait_bound(env: &Env, tc: &Type) -> bool {
        match tc {
            Type::Ref(tr) => env
                .trait_of_ref(tr)
                .and_then(|tid| env.trait_def(tid))
                .is_some_and(|d| d.hole),
            _ => false,
        }
    }

    /// This type with each occurrence of a quantifier `ctors` names
    /// applied to that quantifier's element: a variable bounded by a
    /// constructor trait (`'c: Collection`) stands for a constructor
    /// applied (`'c<'e>`), as a trait-typed parameter does
    /// ([`Self::trait_param`]).
    #[doc(hidden)]
    pub fn apply_ctor_quantifiers(&self, ctors: &[(ArcStr, Type)]) -> Type {
        self.apply_ctor_quantifiers_int(ctors).unwrap_or_else(|| self.clone())
    }

    fn apply_ctor_quantifiers_int(&self, ctors: &[(ArcStr, Type)]) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                ctors.iter().find(|(name, _)| *name == tv.name).map(|(_, elem)| {
                    Type::App(Arc::new(self.clone()), Arc::new(elem.clone()))
                })
            }
            Type::App(c, a) => a
                .apply_ctor_quantifiers_int(ctors)
                .map(|a| Type::App(c.clone(), Arc::new(a))),
            t => t.cow_children(&mut |c| c.apply_ctor_quantifiers_int(ctors)),
        })
    }

    /// This type with its hole replaced by `arg`; `None` if it has no
    /// hole (it is not a constructor).
    pub fn fill_hole(&self, arg: &Type) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::Hole => Some(arg.clone()),
            t => t.cow_children(&mut |c| c.fill_hole(arg)),
        })
    }

    /// The constructor form of this type (last parameter replaced by a
    /// hole) with that parameter; `None` if the outermost form has no
    /// parameters. Syntactic: a reference is taken by name.
    pub fn decompose(&self) -> Option<(Type, Type)> {
        match self {
            Type::Array(t) => Some((Type::Array(Arc::new(Type::Hole)), (**t).clone())),
            Type::List(t) => Some((Type::List(Arc::new(Type::Hole)), (**t).clone())),
            Type::Map { key, value } => Some((
                Type::Map { key: key.clone(), value: Arc::new(Type::Hole) },
                (**value).clone(),
            )),
            Type::Ref(tr) if !tr.params.is_empty() => {
                let n = tr.params.len() - 1;
                let params = Arc::from_iter(
                    tr.params.iter().take(n).cloned().chain(iter::once(Type::Hole)),
                );
                Some((Type::Ref(Arc::new(tr.with_params(params))), tr.params[n].clone()))
            }
            Type::Abstract { id, params } if !params.is_empty() => {
                let n = params.len() - 1;
                let ps = Arc::from_iter(
                    params.iter().take(n).cloned().chain(iter::once(Type::Hole)),
                );
                Some((Type::Abstract { id: *id, params: ps }, params[n].clone()))
            }
            _ => None,
        }
    }

    /// No TVar anywhere beneath (Ref params, not expansions). A
    /// tvar-free type's identity is stable, so it can key a cache.
    pub fn tvar_free(&self) -> bool {
        ensure_sufficient(|| match self {
            Type::TVar(_) => false,
            t => t
                .try_for_each_child(&mut |c| {
                    if c.tvar_free() {
                        ControlFlow::Continue(())
                    } else {
                        ControlFlow::Break(())
                    }
                })
                .is_continue(),
        })
    }

    /// Every name this written type holds that names nothing in `env`
    /// now ([`TypeRef::names_something`]), into `out`: a name's
    /// parameters and a variable's bounds included, a variable's binding
    /// (inferred, not written) not.
    #[doc(hidden)]
    pub fn unresolved_names(&self, env: &Env, out: &mut Vec<TypeRef>) {
        fn go(t: &Type, env: &Env, seen: &mut IntSet<usize>, out: &mut Vec<TypeRef>) {
            ensure_sufficient(|| match t {
                Type::Ref(tr) => {
                    if !tr.names_something(env) {
                        out.push((**tr).clone())
                    }
                    tr.params.iter().for_each(|p| go(p, env, seen, out))
                }
                Type::TVar(tv) if seen.insert(tv.cell_addr()) => {
                    tv.cell_constraints().iter().for_each(|c| go(c, env, seen, out))
                }
                Type::TVar(_) => (),
                Type::Fn(ft) => ft.for_each_type(&mut |t| go(t, env, seen, out)),
                t => t.for_each_child(&mut |c| go(c, env, seen, out)),
            })
        }
        go(self, env, &mut LPooled::take(), out)
    }

    /// Fill the resolution cell of every `Type::Ref` reachable from
    /// this type against `env`, for a type about to outlive the env
    /// that gives its names meaning. Names not visible are skipped
    /// (they fill at their first in-context lookup) and make the
    /// result false. Recurses through filled snapshot bodies and
    /// through cells' bindings and conjuncts.
    pub fn seed_refs(&self, env: &Env) -> bool {
        self.seed_refs_seen(env, &mut LPooled::take())
    }

    /// [`Self::seed_refs`] over types that share `seen`, the cells and
    /// nodes already walked.
    #[doc(hidden)]
    pub fn seed_refs_seen(&self, env: &Env, seen: &mut IntSet<usize>) -> bool {
        fn go(t: &Type, env: &Env, seen: &mut IntSet<usize>) -> bool {
            ensure_sufficient(|| {
                if let Some(node) = node_addr(t)
                    && !seen.insert(node)
                {
                    return true;
                }
                let mut all = true;
                match t {
                    // Keyed on the cell: with_params clones share it.
                    Type::Ref(tr) if seen.insert(Arc::as_ptr(&tr.resolved).addr()) => {
                        match tr.resolve_in(env) {
                            None => all = false,
                            Some(r) => {
                                for (_, c) in r.params.iter() {
                                    if let Some(c) = c {
                                        all &= go(c, env, seen);
                                    }
                                }
                                all &= go(&r.typ, env, seen);
                            }
                        }
                    }
                    Type::TVar(tv) => {
                        let cell = tv.cell();
                        if !seen.insert(Arc::as_ptr(&cell).addr()) {
                            return true;
                        }
                        let (binding, cons) = {
                            let cell = cell.read();
                            (cell.binding.clone(), cell.constraints.clone())
                        };
                        for t in binding.iter().chain(cons.iter()) {
                            all &= go(t, env, seen);
                        }
                    }
                    _ => (),
                }
                t.for_each_child(&mut |c| all &= go(c, env, seen));
                all
            })
        }
        go(self, env, seen)
    }

    /// Whether this type reaches the definition `def` (a
    /// [`ResolvedRef::def_key`]) again through unions, aliases (expanded
    /// with their params) and bound cells alone, with no constructor
    /// between: compared by the definition, so a renaming import is the
    /// same type. A name not defined yet leaves the answer
    /// [`Unguarded::Unknown`]. The walk ends: a cycle among other
    /// definitions was refused when its last member was defined.
    pub(crate) fn reaches_unguarded(&self, env: &Env, def: usize) -> Unguarded {
        fn go(
            t: &Type,
            env: &Env,
            def: usize,
            path: &mut SmallVec<[usize; 8]>,
        ) -> Unguarded {
            let any = |ts: &mut dyn Iterator<Item = Unguarded>| {
                ts.fold(Unguarded::Guarded, |acc, u| acc.or(u))
            };
            ensure_sufficient(|| match t {
                Type::Set(ts) => any(&mut ts.iter().map(|t| go(t, env, def, path))),
                Type::TVar(tv) => match tv.binding() {
                    Some(b) => go(&b, env, def, path),
                    None => Unguarded::Guarded,
                },
                Type::App(c, a) => match Type::app_filled(c, a) {
                    Some(f) => go(&f, env, def, path),
                    None => Unguarded::Guarded,
                },
                Type::Ref(tr) => {
                    let Some(r) = tr.resolve_pure(env) else { return Unguarded::Unknown };
                    let key = r.def_key();
                    if key == def {
                        return Unguarded::Reaches;
                    }
                    let Some(known) = r.bindings(&tr.params) else {
                        return Unguarded::Guarded;
                    };
                    path.push(key);
                    let u = go(&r.typ.replace_tvars(&known), env, def, path);
                    path.pop();
                    u
                }
                _ => Unguarded::Guarded,
            })
        }
        go(self, env, def, &mut SmallVec::new())
    }

    pub fn lookup_ref(&self, env: &Env) -> Result<Type> {
        Ok(self
            .lookup_ref_with(env, true)?
            .expect("a committing lookup refuses by error"))
    }

    /// [`Self::lookup_ref`] for a relation's walk. Committing, a violated
    /// parameter bound is an error and an open argument takes the bound
    /// as a conjunct; a probe (`!commit`) binds nothing and answers a
    /// violated bound with `None`.
    #[doc(hidden)]
    pub fn lookup_ref_with(&self, env: &Env, commit: bool) -> Result<Option<Type>> {
        match self {
            Self::Ref(tr) => {
                let TypeRef { scope, name, params, pos, ori, resolved: _ } = &**tr;
                let resolved = tr.resolve_in(env).ok_or_else(|| {
                    if gxdbg_typeref() {
                        eprintln!(
                            "TYPEREF-MISS {name} in {scope}; typedef scopes with the name:"
                        );
                        for (s, m) in env.typedefs.into_iter() {
                            if m.into_iter().any(|(n, _)| name.ends_with(n.as_str())) {
                                eprintln!("  {s}");
                            }
                        }
                    }
                    UnresolvableRef::error(tr, env)
                })?;
                let ResolvedRef {
                    canonical_scope,
                    pos: def_pos,
                    ori: def_ori,
                    params: def_params,
                    typ: def_typ,
                } = &*resolved;
                if def_params.len() != params.len() {
                    bail!("{} expects {} type parameters", name, def_params.len());
                }
                if env.ide.is_lsp() {
                    if let (Some(pos), Some(ori)) = (pos, ori) {
                        env.push_type_ref(crate::ide::TypeRefSite {
                            pos: *pos,
                            ori: ori.clone(),
                            name: name.clone(),
                            canonical_scope: canonical_scope.clone(),
                            def_pos: *def_pos,
                            def_ori: def_ori.clone(),
                        });
                    }
                }
                let known = resolved.bindings(params).expect("arity checked");
                for ((_, constraint), arg) in def_params.iter().zip(params.iter()) {
                    let Some(constraint) = constraint else {
                        continue;
                    };
                    let constraint = constraint.replace_tvars(&known);
                    match arg {
                        Type::TVar(tv) if !tv.is_bound() => {
                            if commit {
                                tv.add_cell_constraint(constraint)
                            }
                        }
                        _ if commit => constraint.check_contains(env, arg)?,
                        _ => {
                            if !constraint.contains_with_flags(
                                BitFlags::empty(),
                                env,
                                arg,
                            )? {
                                return Ok(None);
                            }
                        }
                    }
                }
                Ok(Some(def_typ.replace_tvars(&known)))
            }
            t => Ok(Some(t.clone())),
        }
    }

    /// Push a `TypeRefSite` for every `Type::Ref` beneath that carries
    /// a source position. The caller gates on `env.ide.is_lsp()`.
    pub fn record_ide_refs(&self, env: &Env, fallback_scope: &ModPath) {
        ensure_sufficient(|| match self {
            Type::Ref(tr) => {
                if let (Some(pos), Some(ori)) = (tr.pos, &tr.ori) {
                    let (canonical_scope, def_pos, def_ori) = match tr.resolve_pure(env) {
                        Some(r) => (r.canonical_scope.clone(), r.pos, r.ori.clone()),
                        None => (
                            fallback_scope.clone(),
                            SourcePosition::default(),
                            ori.clone(),
                        ),
                    };
                    env.push_type_ref(crate::ide::TypeRefSite {
                        pos,
                        ori: ori.clone(),
                        name: tr.name.clone(),
                        canonical_scope,
                        def_pos,
                        def_ori,
                    });
                }
                for p in tr.params.iter() {
                    p.record_ide_refs(env, fallback_scope);
                }
            }
            Type::TVar(tv) => {
                if let Some(t) = tv.binding() {
                    t.record_ide_refs(env, fallback_scope);
                }
            }
            t => t.for_each_child(&mut |c| c.record_ide_refs(env, fallback_scope)),
        })
    }

    pub fn boolean() -> Self {
        Self::Primitive(Typ::Bool.into())
    }

    /// `Bottom`, or a union whose every member (through bound tvars)
    /// is. An unbound tvar is not provably bottom.
    pub fn all_bottom(&self) -> bool {
        ensure_sufficient(|| {
            self.with_deref(|t| match t {
                Some(Type::Bottom) => true,
                Some(Type::Set(s)) => s.iter().all(|t| t.all_bottom()),
                _ => false,
            })
        })
    }

    /// A `Bottom` anywhere in the type's own structure (fn signatures
    /// and references are leaves): the diagnostic for `_` written in
    /// type position where a wildcard was meant.
    pub fn has_bottom(&self) -> bool {
        ensure_sufficient(|| {
            self.with_deref(|t| match t {
                Some(Type::Bottom) => true,
                Some(
                    Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(_, t),
                ) => t.has_bottom(),
                Some(Type::Map { key, value }) => key.has_bottom() || value.has_bottom(),
                Some(Type::Tuple(ts) | Type::Variant(_, ts, _) | Type::Set(ts)) => {
                    ts.iter().any(|t| t.has_bottom())
                }
                Some(Type::Struct(fs)) => fs.iter().any(|(_, t, _)| t.has_bottom()),
                _ => false,
            })
        })
    }

    /// The dereferenced type, cloned; `None` for an unbound cell.
    pub fn deref_cloned(&self) -> Option<Self> {
        self.with_deref(|t| t.cloned())
    }

    /// `f` over this type with bound cells and filled applications
    /// looked through; `None` for an open cell. `f` runs while the read
    /// guard of every cell on the chain is held: an `f` (or a walk it
    /// starts) that writes one of those cells deadlocks.
    pub fn with_deref<R, F: FnOnce(Option<&Self>) -> R>(&self, f: F) -> R {
        match self {
            // A filled application is its filled type to every walk.
            Self::App(c, a) => match Self::app_filled(c, a) {
                Some(filled) => ensure_sufficient(|| filled.with_deref(f)),
                None => f(Some(self)),
            },
            Self::Hole
            | Self::Concrete
            | Self::Function
            | Self::Singleton
            | Self::Discernible
            | Self::Ordered
            | Self::OneNumber => f(Some(self)),
            Self::Bottom
            | Self::Abstract { .. }
            | Self::Any
            | Self::Primitive(_)
            | Self::Fn(_)
            | Self::Set(_)
            | Self::Error(_)
            | Self::Array(_)
            | Self::List(_)
            | Self::ByRef(..)
            | Self::Tuple(_)
            | Self::Struct(_)
            | Self::Variant(_, _, _)
            | Self::Ref(_)
            | Self::Map { .. } => f(Some(self)),
            Self::TVar(tv) => match tv.read().cell.read().binding.as_ref() {
                Some(t) => ensure_sufficient(|| t.with_deref(f)),
                None => f(None),
            },
        }
    }

    /// A trait named as a parameter's type (`fn(s: Read)`) becomes a
    /// fresh bounded quantifier `fn<'s: Read>(s: 's)` named `#s`; a
    /// trait anywhere else is an error. Returns the rewritten type.
    /// A bound's conjunct: a trait or a predicate itself, or a type
    /// holding neither a hole nor a trait, which only a parameter's type
    /// may be.
    pub fn check_bound(&self, env: &Env) -> Result<()> {
        match self {
            Type::Ref(tr) if env.trait_of_ref(tr).is_some() => Ok(()),
            Type::Concrete
            | Type::Function
            | Type::Singleton
            | Type::OneNumber
            | Type::Discernible
            | Type::Ordered => Ok(()),
            t => t.rewrite_trait_args(env).map(|_| ()),
        }
    }

    pub fn rewrite_trait_args(&self, env: &Env) -> Result<Type> {
        if self.holes() > 0 {
            bail!(
                "'_ is the hole of a constructor trait's implementation target \
                 (`impl Collection for Array<'_>`); it is not a type"
            )
        }
        Ok(self.rewrite_trait_args_int(env)?.unwrap_or_else(|| self.clone()))
    }

    /// The rewrite of what changed; `None` for a type it leaves alone.
    fn rewrite_trait_args_int(&self, env: &Env) -> Result<Option<Type>> {
        ensure_sufficient(|| self.rewrite_trait_args_inner(env))
    }

    fn rewrite_trait_args_inner(&self, env: &Env) -> Result<Option<Type>> {
        match self {
            Type::Ref(tr) if env.trait_of_ref(tr).is_some() => bail!(
                "trait {} used as a type: a trait is a bound — write it as a \
                 parameter's type (`fn(x: {})`) or a quantifier's (`fn<'a: {}>`)",
                tr.name,
                tr.name,
                tr.name
            ),
            Type::Fn(ft) => {
                for (_, c) in ft.constraint_view().iter() {
                    c.check_bound(env)?;
                }
                let mut quantifiers: LPooled<Vec<ArcStr>> =
                    ft.quantifiers.iter().cloned().collect();
                let mut changed = false;
                let mut part = |t: &Type| -> Result<Type> {
                    Ok(match t.rewrite_trait_args_int(env)? {
                        Some(r) => {
                            changed = true;
                            r
                        }
                        None => t.clone(),
                    })
                };
                let mut args: LPooled<Vec<FnArgType>> = LPooled::take();
                let mut params_changed = false;
                for (i, a) in ft.args.iter().enumerate() {
                    let typ = match &a.typ {
                        Type::Ref(tr) if env.trait_of_ref(tr).is_some() => {
                            let name: ArcStr = match a.name() {
                                Some(n) => format_compact!("#{n}").as_str().into(),
                                None => format_compact!("#arg{i}").as_str().into(),
                            };
                            let tv = TVar::empty_generic(name.clone());
                            tv.add_cell_constraint(a.typ.clone());
                            // a fn type's element is a quantifier, copied per call
                            let t = match &Type::trait_param(env, tv, tr) {
                                Type::App(c, _) => {
                                    let elem = Type::elem_name(&name);
                                    quantifiers.push(elem.clone());
                                    let elem = Type::TVar(TVar::empty_generic(elem));
                                    Type::App(c.clone(), Arc::new(elem))
                                }
                                t => t.clone(),
                            };
                            if !quantifiers.contains(&name) {
                                quantifiers.push(name);
                            }
                            params_changed = true;
                            t
                        }
                        t => part(t)?,
                    };
                    args.push(FnArgType { kind: a.kind.clone(), typ });
                }
                let vargs = ft.vargs.as_ref().map(|t| part(t)).transpose()?;
                let rtype = part(&ft.rtype)?;
                let throws = part(&ft.throws)?;
                let ctors = ctor_quantifiers(ft, env);
                if !changed && !params_changed && ctors.is_empty() {
                    return Ok(None);
                }
                quantifiers.extend(ctors.iter().map(|(q, _)| Type::elem_name(q)));
                let ft = FnType {
                    args: Arc::from_iter(args.drain(..)),
                    vargs,
                    rtype,
                    throws,
                    explicit_throws: ft.explicit_throws,
                    quantifiers: Arc::from_iter(quantifiers.drain(..)),
                    lambda_ids: ft.lambda_ids.clone(),
                };
                if ctors.is_empty() {
                    return Ok(Some(Type::Fn(Arc::new(ft))));
                }
                let ft =
                    ft.cow_walk(|t| t.apply_ctor_quantifiers_int(&ctors)).unwrap_or(ft);
                Ok(Some(Type::Fn(Arc::new(ft))))
            }
            t => {
                let mut err = None;
                let r = t.cow_children(&mut |c| match c.rewrite_trait_args_int(env) {
                    Ok(r) => r,
                    Err(e) => {
                        err = Some(e);
                        None
                    }
                });
                match err {
                    Some(e) => Err(e),
                    None => Ok(r),
                }
            }
        }
    }

    pub fn scope_refs(&self, scope: &ModPath) -> Type {
        let mut copies: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.scope_refs_with(scope, &mut copies)
    }

    /// [`Self::scope_refs`] over `copies`, shared with the other parts of
    /// one type.
    pub(crate) fn scope_refs_with(
        &self,
        scope: &ModPath,
        copies: &mut AHashMap<usize, TVar>,
    ) -> Type {
        self.scope_refs_int(scope, copies).unwrap_or_else(|| self.clone())
    }

    /// `None` when no `Ref` or `TVar` is beneath. `copies` maps each
    /// cell re-minted so far to its copy: every occurrence of one cell
    /// is one copy, so what was one variable stays one.
    fn scope_refs_int(
        &self,
        scope: &ModPath,
        copies: &mut AHashMap<usize, TVar>,
    ) -> Option<Type> {
        ensure_sufficient(|| self.scope_refs_int_inner(scope, copies))
    }

    fn scope_refs_int_inner(
        &self,
        scope: &ModPath,
        copies: &mut AHashMap<usize, TVar>,
    ) -> Option<Type> {
        match self {
            Type::TVar(tv) => {
                let addr = tv.cell_addr();
                if let Some(copy) = copies.get(&addr) {
                    return Some(Type::TVar(copy.clone()));
                }
                let (bound, cons) = {
                    let cell = tv.cell();
                    let cell = cell.read();
                    (cell.binding.clone(), cell.constraints.clone())
                };
                let fresh = tv.fresh_copy();
                copies.insert(addr, fresh.clone());
                if let Some(typ) = bound {
                    fresh.bind(typ.scope_refs_int(scope, copies).unwrap_or(typ))
                }
                // The re-minted cell keeps the conjunction (an annotated
                // bound lives only there).
                for c in cons.iter() {
                    let c = c.scope_refs_int(scope, copies).unwrap_or_else(|| c.clone());
                    fresh.add_cell_constraint(c);
                }
                Some(Type::TVar(fresh))
            }
            Type::Ref(tr) => {
                let params = Arc::from_iter(tr.params.iter().map(|t| {
                    t.scope_refs_int(scope, copies).unwrap_or_else(|| t.clone())
                }));
                Some(Type::Ref(Arc::new(tr.with_scope(scope.clone(), params))))
            }
            t => t.cow_children(&mut |c| c.scope_refs_int(scope, copies)),
        }
    }

    /// A unification view of this type with every `Any` leaf replaced
    /// by a throwaway TVar, sharing the existing cells so bindings made
    /// through the view land in the original. Select's arm typecheck
    /// unifies pattern predicates through it: `T.contains(Any)` is
    /// false, which would stop the walk at a `_` slot before later
    /// slots' binds narrowed.
    pub fn any_as_tvar(&self) -> Type {
        self.any_as_tvar_int().unwrap_or_else(|| self.clone())
    }

    /// `None` when no `Any` is beneath. `Ref`/`Abstract` params and
    /// `Fn` signatures are leaves: the select arm walk never descends
    /// them.
    fn any_as_tvar_int(&self) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::Any => Some(Type::empty_tvar()),
            Type::Ref(_) | Type::Fn(_) | Type::Abstract { .. } => None,
            t => t.cow_children(&mut |c| c.any_as_tvar_int()),
        })
    }
}

/// An id that is the low 64 bits of a uuid: a fixed eight bytes, where a
/// varint would take ten.
macro_rules! uuid_id_codec {
    ($id:ty) => {
        impl PackTrait for $id {
            fn encoded_len(&self) -> usize {
                self.inner().encoded_len()
            }

            fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
                self.inner().encode(buf)
            }

            fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
                Ok(<$id>::from_inner(u64::decode(buf)?))
            }
        }
    };
}

uuid_id_codec!(AbstractId);
uuid_id_codec!(TraitId);

/// The declared quantifiers of `ft` bounded by constructor traits alone,
/// each with an element of its own ([`Type::apply_ctor_quantifiers`]).
fn ctor_quantifiers(ft: &FnType, env: &Env) -> LPooled<Vec<(ArcStr, Type)>> {
    let mut out: LPooled<Vec<(ArcStr, Type)>> = LPooled::take();
    if ft.quantifiers.is_empty() {
        return out;
    }
    let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    ft.collect_tvars(&mut named);
    for q in ft.quantifiers.iter() {
        let Some(tv) = named.get(q) else { continue };
        let cons = tv.cell_constraints();
        if !tv.is_bound()
            && !cons.is_empty()
            && cons.iter().all(|c| Type::is_ctor_trait_bound(env, c))
        {
            out.push((q.clone(), Type::TVar(TVar::empty_generic(Type::elem_name(q)))));
        }
    }
    out
}
