use crate::{
    PRINT_FLAGS, PrintFlag, SourcePosition,
    dbgenv::{graphix_dbg_bind, gxdbg_typeref},
    env::Env,
    expr::{ModPath, Origin, WrittenAt},
    format_with_flags,
    image::{self, KeyedNode},
    stack::ensure_sufficient,
};
use ahash::AHashMap;
use anyhow::{Result, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_core::{
    pack::{Pack as PackTrait, PackError, encode_varint},
    utils::Either,
};
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
    sync::LazyLock,
};
use triomphe::Arc;

mod cast;
pub use cast::IsAFlags;
mod contains;
pub use contains::ContainsFlags;
pub mod fntyp;
mod matches;
mod normalize;
pub(crate) use normalize::{NormKey, norm_key};
mod print;
mod setops;
mod settle;
pub(crate) mod tval;
pub(crate) mod tvar;

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
        Type::Array(a) | Type::List(a) | Type::Error(a) | Type::ByRef(a) => {
            Some((&**a as *const Type).addr())
        }
        Type::Map { .. }
        | Type::App(..)
        | Type::Hole
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
struct RefHist<H: IsoPoolable> {
    inner: LPooled<H>,
    /// Definition key → (the resolution it came from, pinned so the key
    /// is not reused; the param lists seen with their ids).
    ref_ids:
        LPooled<IntMap<usize, (Arc<ResolvedRef>, SmallVec<[(Arc<[Type]>, usize); 2]>)>>,
    /// Content identity → id for non-Ref types, so the cycle memo does
    /// not conflate distinct finite sub-problems.
    content_ids: LPooled<AHashMap<NormKey, usize>>,
    next_id: usize,
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
            ref_ids: LPooled::take(),
            content_ids: LPooled::take(),
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
                let entry =
                    self.ref_ids.entry(key).or_insert_with(|| (r, SmallVec::new()));
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
            t => probe_key(t)?,
        };
        if let Some(&id) = self.content_ids.get(&k) {
            return Some(id);
        }
        let id = self.next();
        self.content_ids.insert(k, id);
        Some(id)
    }
}

/// The identity of an abstract type: the low 64 bits of
/// [`abstract_uuid`] of its canonical path. Its `Pack` impl lives in
/// [`crate::expr::serialize`].
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

/// What a `TypeRef`'s name means: the snapshot [`Type::lookup_ref`]
/// reads from the env, held in the ref's write-once `resolved` cell so
/// a ref resolved once in its native env is env-independent after.
#[derive(Debug)]
pub(crate) struct ResolvedRef {
    canonical_scope: ModPath,
    pos: SourcePosition,
    ori: Arc<Origin>,
    params: Arc<[(TVar, Option<Type>)]>,
    typ: Type,
}

impl ResolvedRef {
    /// The definition's identity: every cell filled from one `TypeDef`
    /// shares its params allocation.
    pub(crate) fn def_key(&self) -> usize {
        Arc::as_ptr(&self.params) as *const () as usize
    }

    /// Same definition? Cells filled from one `TypeDef` share its
    /// content Arcs, so this is usually a pointer comparison.
    pub(crate) fn same_def(&self, other: &Self) -> bool {
        (Arc::ptr_eq(&self.params, &other.params) || self.params == other.params)
            && self.typ == other.typ
    }

    pub(crate) fn canonical_scope(&self) -> &ModPath {
        &self.canonical_scope
    }
}

/// A reference to a named typedef, e.g. `Foo` or `Result<i64, string>`.
/// `pos`/`ori` are IDE metadata and `resolved` is the write-once name
/// resolution cell ([`ResolvedRef`]); neither is part of type identity
/// or the packed form. The cell depends on (scope, name, env) but not
/// `params`: [`TypeRef::with_params`] shares it, [`TypeRef::with_scope`]
/// mints a new one. Never overwrite a filled cell — clones share it.
#[derive(Debug, Clone)]
pub struct TypeRef {
    pub scope: ModPath,
    pub name: ModPath,
    pub params: Arc<[Type]>,
    pub pos: Option<SourcePosition>,
    pub ori: Option<Arc<Origin>>,
    pub(in crate::typ) resolved: Arc<Mutex<Option<Arc<ResolvedRef>>>>,
}

fn resolved_len(r: &ResolvedRef) -> usize {
    let ResolvedRef { canonical_scope, pos, ori, params, typ } = r;
    canonical_scope.encoded_len()
        + image::pos_len(pos)
        + image::origin_len(ori)
        + params.encoded_len()
        + typ.encoded_len()
}

fn resolved_encode(r: &ResolvedRef, buf: &mut impl BufMut) -> Result<(), PackError> {
    let ResolvedRef { canonical_scope, pos, ori, params, typ } = r;
    canonical_scope.encode(buf)?;
    image::pos_encode(pos, buf)?;
    image::origin_encode(ori, buf)?;
    params.encode(buf)?;
    typ.encode(buf)
}

fn resolved_decode(buf: &mut impl Buf) -> Result<ResolvedRef, PackError> {
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
        base + pos + ori + image::refcell_len(resolved, resolved_len)
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
        image::refcell_encode(resolved, buf, resolved_encode)
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
        let resolved = image::refcell_decode(buf, |b| resolved_decode(b))?;
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
    pub(crate) fn with_params(&self, params: Arc<[Type]>) -> Self {
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
        if r.params.len() != self.params.len() {
            return None;
        }
        let mut known: LPooled<AHashMap<ArcStr, Type>> = LPooled::take();
        for ((tv, _), arg) in r.params.iter().zip(self.params.iter()) {
            known.insert(tv.name.clone(), arg.clone());
        }
        Some(r.typ.replace_tvars(&known))
    }

    pub(crate) fn resolved(&self) -> Option<Arc<ResolvedRef>> {
        self.resolved.lock().clone()
    }

    /// [`ResolvedRef::def_key`] of the filled cell.
    pub(crate) fn def_key(&self) -> Option<usize> {
        self.resolved.lock().as_ref().map(|r| r.def_key())
    }

    /// Do two same-named refs mean the same definition? True unless
    /// both cells are filled with different definitions, in which case
    /// the name-equality fast paths must fall through to expansion.
    pub(crate) fn cells_agree(&self, other: &Self) -> bool {
        match (self.resolved(), other.resolved()) {
            (Some(a), Some(b)) => a.same_def(&b),
            _ => true,
        }
    }

    /// What this ref's name means in `env`; never reads or writes the
    /// cell.
    pub(crate) fn resolve_pure(&self, env: &Env) -> Option<Arc<ResolvedRef>> {
        env.resolve_visible(&self.scope, &self.name, crate::env::NameNs::Type, |s, n| {
            env.typedefs.get(s).and_then(|m| m.get(n)).map(|d| {
                Arc::new(ResolvedRef {
                    canonical_scope: ModPath(netidx_core::path::Path::from(
                        ArcStr::from(s),
                    )),
                    pos: d.pos,
                    ori: d.ori.clone(),
                    params: d.params.clone(),
                    typ: d.typ.clone(),
                })
            })
        })
        .map_err(|e| {
            // Logged so an ambiguous glob does not read as "undefined type".
            log::warn!("resolving type `{}` in `{}`: {e:#}", self.name, self.scope)
        })
        .ok()
        .flatten()
    }

    /// Resolve this ref's name in `env` and fill the cell if empty;
    /// `None` iff the name is not visible and the cell is empty. An
    /// existing resolution wins. The snapshot is computed without the
    /// cell lock held (resolution can re-enter). Fills only this ref,
    /// not the snapshot's nested refs: mid-compile the env is
    /// incomplete, and a nested name can resolve to an outer shadow.
    pub(crate) fn resolve_in(&self, env: &Env) -> Option<Arc<ResolvedRef>> {
        if let Some(r) = self.resolved() {
            return Some(r);
        }
        let r = self.resolve_pure(env)?;
        Some(self.resolved.lock().get_or_insert(r).clone())
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
    Ref(TypeRef),
    Fn(Arc<FnType>),
    Set(Arc<[Type]>),
    TVar(TVar),
    Error(Arc<Type>),
    Array(Arc<Type>),
    /// The native linked list. The runtime rep is private to
    /// `node::list`.
    List(Arc<Type>),
    ByRef(Arc<Type>),
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
            Type::ByRef(t) => {
                out.put_u8(tag::BYREF);
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
            Type::Bottom | Type::Any | Type::Hole => 0,
            Type::Primitive(p) => p.encoded_len(),
            Type::Ref(r) => r.encoded_len(),
            Type::Fn(f) => f.encoded_len(),
            Type::TVar(tv) => tv.encoded_len(),
            Type::Set(ts) | Type::Tuple(ts) => ts.encoded_len(),
            Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(t) => {
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
            Type::ByRef(t) => {
                buf.put_u8(tag::BYREF);
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
            tag::PRIMITIVE => Type::Primitive(PackTrait::decode(buf)?),
            tag::REF => Type::Ref(PackTrait::decode(buf)?),
            tag::FN => Type::Fn(PackTrait::decode(buf)?),
            tag::TVAR => Type::TVar(PackTrait::decode(buf)?),
            tag::SET => Type::Set(PackTrait::decode(buf)?),
            tag::TUPLE => Type::Tuple(PackTrait::decode(buf)?),
            tag::ERROR => Type::Error(PackTrait::decode(buf)?),
            tag::ARRAY => Type::Array(PackTrait::decode(buf)?),
            tag::LIST => Type::List(PackTrait::decode(buf)?),
            tag::BYREF => Type::ByRef(PackTrait::decode(buf)?),
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
        if image::is_encoding() {
            image::type_len(self, || self.shape_len())
        } else {
            self.shape_len()
        }
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        if image::is_encoding() {
            image::type_encode(self, buf, |b| self.shape_encode(b))
        } else {
            self.shape_encode(buf)
        }
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if image::is_decoding() {
            image::object_decode(
                buf,
                |d| &mut d.types,
                |b| Self::shape_decode(b),
                |b| Self::decode(b),
            )
        } else {
            Self::shape_decode(buf)
        }
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
            Type::ByRef(a) => matches!(other, Type::ByRef(b) if one_eq(a, b)),
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
            Type::ByRef(_) => tag::BYREF,
            Type::Tuple(_) => tag::TUPLE,
            Type::Struct(_) => tag::STRUCT,
            Type::Variant(..) => tag::VARIANT,
            Type::Map { .. } => tag::MAP,
            Type::Abstract { .. } => tag::ABSTRACT,
            Type::App(..) => tag::APP,
            Type::Hole => tag::HOLE,
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
            | (Type::ByRef(a), Type::ByRef(b)) => a.cmp(b),
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
            Type::Bottom | Type::Any | Type::Hole | Type::Primitive(_) => {
                self.cmp_fields(other)
            }
            _ => ensure_sufficient(|| self.cmp_fields(other)),
        })
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.rank().hash(state);
        match self {
            Type::Bottom | Type::Any | Type::Hole => (),
            Type::Primitive(p) => p.hash(state),
            t => ensure_sufficient(|| match t {
                Type::Ref(r) => r.hash(state),
                Type::Fn(f) => f.hash(state),
                Type::TVar(tv) => tv.hash(state),
                Type::Set(ts) | Type::Tuple(ts) => ts.hash(state),
                Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(t) => {
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
                Type::Bottom | Type::Any | Type::Hole | Type::Primitive(_) => (),
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
            | Type::Primitive(_)
            | Type::TVar(_) => false,
            Type::Ref(tr) => tr.params.is_unique() || tr.resolved.is_unique(),
            Type::Fn(f) => f.is_unique(),
            Type::Set(ts) | Type::Tuple(ts) | Type::Variant(_, ts, _) => ts.is_unique(),
            Type::Abstract { params, .. } => params.is_unique(),
            Type::Error(a) | Type::Array(a) | Type::List(a) | Type::ByRef(a) => {
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
                Type::Bottom | Type::Any | Type::Hole | Type::Primitive(_) => (),
                Type::Ref(r) => drop(ptr::read(r)),
                Type::Fn(f) => drop(ptr::read(f)),
                Type::TVar(tv) => drop(ptr::read(tv)),
                Type::Set(ts) | Type::Tuple(ts) => drop(ptr::read(ts)),
                Type::Error(a) | Type::Array(a) | Type::List(a) | Type::ByRef(a) => {
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

/// A classifiable resolution failure from [`Type::lookup_ref`].
#[derive(Debug)]
pub struct UnresolvableRef {
    pub name: ModPath,
    pub scope: ModPath,
}

impl std::fmt::Display for UnresolvableRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "undefined type {} in {}", self.name, self.scope)
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
            | Type::Hole => ControlFlow::Continue(()),
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
            Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(t) => f(t),
            Type::Map { key, value } => {
                f(key)?;
                f(value)
            }
            Type::Fn(ft) => ft.try_for_each_type(f),
        }
    }

    /// [`Self::try_for_each_child`] without early exit.
    pub(crate) fn for_each_child(&self, f: &mut impl FnMut(&Type)) {
        let _ = self.try_for_each_child::<()>(&mut |t| {
            f(t);
            ControlFlow::Continue(())
        });
    }

    /// Rebuild this type's immediate structural children through `f`
    /// (`None` from `f` means unchanged); `None` when nothing changed.
    /// Leaves, `TVar` included, return `None`. `Ref` params rebuild
    /// through [`TypeRef::with_params`], sharing the resolution cell.
    pub(crate) fn cow_children(
        &self,
        f: &mut impl FnMut(&Type) -> Option<Type>,
    ) -> Option<Type> {
        match self {
            Type::Bottom
            | Type::Any
            | Type::Primitive(_)
            | Type::TVar(_)
            | Type::Hole => None,
            Type::App(c, a) => match (f(c), f(a)) {
                (None, None) => None,
                (c2, a2) => Some(Type::app(
                    c2.unwrap_or_else(|| (**c).clone()),
                    a2.unwrap_or_else(|| (**a).clone()),
                )),
            },
            Type::Ref(tr) => Type::cow_slice(&tr.params, |t| f(t))
                .map(|params| Type::Ref(tr.with_params(params))),
            Type::Abstract { id, params } => Type::cow_slice(params, |t| f(t))
                .map(|params| Type::Abstract { id: *id, params }),
            Type::Error(t) => f(t).map(|t| Type::Error(Arc::new(t))),
            Type::Array(t) => f(t).map(|t| Type::Array(Arc::new(t))),
            Type::List(t) => f(t).map(|t| Type::List(Arc::new(t))),
            Type::ByRef(t) => f(t).map(|t| Type::ByRef(Arc::new(t))),
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
    pub(crate) fn app_split(t: &Type, env: &Env) -> Result<Option<(Type, Type)>> {
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
    /// constructor.
    pub(crate) fn app_split_for(
        ctor: &Type,
        t: &Type,
        env: &Env,
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
            let Some(heads) = env.impls.get(&tid) else { continue };
            for im in heads.iter() {
                if !matches!(im.target, Type::Ref(_))
                    || fits(&im.target, &t.reset_tvars())?.is_none()
                {
                    continue;
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
    pub(crate) fn holes(&self) -> usize {
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
    pub(crate) fn pre_unify_arg(env: &Env, declared: &Type, actual: &Type) -> Result<()> {
        let d = declared.deref_cloned();
        let a = actual.deref_cloned();
        match (&d, &a) {
            (Some(Type::Fn(d)), Some(Type::Fn(a))) => d.pre_unify_params(env, a),
            _ => declared.contains(env, actual).map(|_| ()),
        }
    }

    /// The type of a parameter whose written type is the trait `tr`:
    /// the fresh bounded quantifier `tv`, applied to a fresh element
    /// when the trait is a constructor trait (`|c: Collection|` ≡
    /// `'c: Collection, c: 'c<'e>`).
    pub(crate) fn trait_param(env: &Env, tv: TVar, tr: &TypeRef) -> Type {
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
                Some((Type::Ref(tr.with_params(params)), tr.params[n].clone()))
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

    fn iter_prims(&self) -> impl Iterator<Item = Self> {
        match self {
            Self::Primitive(p) => {
                Either::Left(p.iter().map(|t| Type::Primitive(t.into())))
            }
            t => Either::Right(iter::once(t.clone())),
        }
    }

    /// No TVar anywhere beneath (Ref params, not expansions). A
    /// tvar-free type's identity is stable, so it can key a cache.
    pub(crate) fn tvar_free(&self) -> bool {
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

    /// Fill the resolution cell of every `Type::Ref` reachable from
    /// this type against `env`, for a type about to outlive the env
    /// that gives its names meaning. Names not visible are skipped
    /// (they fill at their first in-context lookup) and make the
    /// result false. Recurses through filled snapshot bodies and
    /// through cells' bindings and conjuncts.
    pub fn seed_refs(&self, env: &Env) -> bool {
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
        go(self, env, &mut LPooled::take())
    }

    /// Whether this type reaches the definition `name` in `scope` again
    /// through unions, aliases (expanded with their params) and bound
    /// cells alone, with no constructor between. Every definition
    /// registered before is contractive, so an unguarded path that does
    /// not return is short; one longer than `UNGUARDED_DEPTH` names is
    /// taken as not returning.
    pub(crate) fn reaches_unguarded(&self, env: &Env, scope: &str, name: &str) -> bool {
        const UNGUARDED_DEPTH: usize = 256;
        fn go(t: &Type, env: &Env, def: (&str, &str), depth: usize) -> bool {
            ensure_sufficient(|| match t {
                Type::Set(ts) => ts.iter().any(|t| go(t, env, def, depth)),
                Type::TVar(tv) => tv.binding().is_some_and(|b| go(&b, env, def, depth)),
                Type::App(c, a) => {
                    Type::app_filled(c, a).is_some_and(|f| go(&f, env, def, depth))
                }
                Type::Ref(tr) if depth < UNGUARDED_DEPTH => {
                    let Some(r) = tr.resolve_pure(env) else { return false };
                    let base =
                        netidx_core::path::Path::basename(&*tr.name).unwrap_or(&tr.name);
                    let canon: &str = r.canonical_scope();
                    if (canon, base) == def {
                        return true;
                    }
                    let mut known: LPooled<AHashMap<ArcStr, Type>> = LPooled::take();
                    for ((tv, _), arg) in r.params.iter().zip(tr.params.iter()) {
                        known.insert(tv.name.clone(), arg.clone());
                    }
                    go(&r.typ.replace_tvars(&known), env, def, depth + 1)
                }
                _ => false,
            })
        }
        go(self, env, (scope, name), 0)
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
    pub(crate) fn lookup_ref_with(
        &self,
        env: &Env,
        commit: bool,
    ) -> Result<Option<Type>> {
        match self {
            Self::Ref(tr) => {
                let TypeRef { scope, name, params, pos, ori, resolved: _ } = tr;
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
                    anyhow::Error::new(UnresolvableRef {
                        name: name.clone(),
                        scope: scope.clone(),
                    })
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
                // XCR claude for eric: every expansion pushes a site, and
                // this is the only recorder of annotation refs (record_ide_refs
                // covers typedef bodies and interface sigs), so moving it means a
                // record_ide_refs at every annotation compile site in node/*.
                // Queries dedup their output, so the cost is sink memory and lock
                // traffic under lsp_mode only; the cheap fix is an (origin, pos)
                // seen-set beside `Ide::type_refs` checked in `Env::push_type_ref`
                // (ide.rs / env.rs, other packages).
                if env.lsp_mode {
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
                let mut known: LPooled<AHashMap<ArcStr, Type>> = LPooled::take();
                for ((tv, _), arg) in def_params.iter().zip(params.iter()) {
                    known.insert(tv.name.clone(), arg.clone());
                }
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
    /// a source position. The caller gates on `env.lsp_mode`.
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
                    Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(t),
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
            Self::Hole => f(Some(self)),
            Self::Bottom
            | Self::Abstract { .. }
            | Self::Any
            | Self::Primitive(_)
            | Self::Fn(_)
            | Self::Set(_)
            | Self::Error(_)
            | Self::Array(_)
            | Self::List(_)
            | Self::ByRef(_)
            | Self::Tuple(_)
            | Self::Struct(_)
            | Self::Variant(_, _, _)
            | Self::Ref(TypeRef { .. })
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
    pub fn rewrite_trait_args(&self, env: &Env) -> Result<Type> {
        if self.holes() > 0 {
            bail!(
                "'_ is the hole of a constructor trait's implementation target \
                 (`impl Collection for Array<'_>`); it is not a type"
            )
        }
        self.rewrite_trait_args_int(env)
    }

    fn rewrite_trait_args_int(&self, env: &Env) -> Result<Type> {
        ensure_sufficient(|| self.rewrite_trait_args_inner(env))
    }

    fn rewrite_trait_args_inner(&self, env: &Env) -> Result<Type> {
        match self {
            Type::Ref(tr) if env.trait_of_ref(tr).is_some() => bail!(
                "trait {} used as a type: a trait is a bound — write it as a \
                 parameter's type (`fn(x: {})`) or a quantifier's (`fn<'a: {}>`)",
                tr.name,
                tr.name,
                tr.name
            ),
            Type::Fn(ft) => {
                let mut quantifiers: LPooled<Vec<ArcStr>> =
                    ft.quantifiers.iter().cloned().collect();
                let mut changed = false;
                let mut args: LPooled<Vec<FnArgType>> = LPooled::take();
                for (i, a) in ft.args.iter().enumerate() {
                    let typ = match &a.typ {
                        Type::Ref(tr) if env.trait_of_ref(tr).is_some() => {
                            let name: ArcStr = match a.name() {
                                Some(n) => format_compact!("#{n}").as_str().into(),
                                None => format_compact!("#arg{i}").as_str().into(),
                            };
                            let tv = TVar::empty_named(name.clone());
                            tv.add_cell_constraint(a.typ.clone());
                            if !quantifiers.contains(&name) {
                                quantifiers.push(name);
                            }
                            changed = true;
                            Type::trait_param(env, tv, tr)
                        }
                        t => {
                            let r = t.rewrite_trait_args_int(env)?;
                            changed |= !r.ptr_eq_shallow(t);
                            r
                        }
                    };
                    args.push(FnArgType { kind: a.kind.clone(), typ });
                }
                let vargs = match &ft.vargs {
                    None => None,
                    Some(t) => {
                        let r = t.rewrite_trait_args_int(env)?;
                        changed |= !r.ptr_eq_shallow(t);
                        Some(r)
                    }
                };
                let rtype = ft.rtype.rewrite_trait_args_int(env)?;
                changed |= !rtype.ptr_eq_shallow(&ft.rtype);
                let throws = ft.throws.rewrite_trait_args_int(env)?;
                changed |= !throws.ptr_eq_shallow(&ft.throws);
                if !changed {
                    return Ok(self.clone());
                }
                Ok(Type::Fn(Arc::new(FnType {
                    args: Arc::from_iter(args.drain(..)),
                    vargs,
                    rtype,
                    throws,
                    explicit_throws: ft.explicit_throws,
                    quantifiers: Arc::from_iter(quantifiers.drain(..)),
                    lambda_ids: ft.lambda_ids.clone(),
                })))
            }
            t => {
                let mut err = None;
                let r = t.cow_children(&mut |c| match c.rewrite_trait_args_int(env) {
                    Ok(r) if r.ptr_eq_shallow(c) => None,
                    Ok(r) => Some(r),
                    Err(e) => {
                        err = Some(e);
                        None
                    }
                });
                match err {
                    Some(e) => Err(e),
                    None => Ok(r.unwrap_or_else(|| self.clone())),
                }
            }
        }
    }

    /// Same allocation or same leaf — the "unchanged" test for walks
    /// that return `self.clone()` when nothing moved.
    fn ptr_eq_shallow(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Fn(a), Type::Fn(b)) => Arc::ptr_eq(a, b),
            (Type::TVar(a), Type::TVar(b)) => a == b,
            (a, b) => a == b,
        }
    }

    pub fn scope_refs(&self, scope: &ModPath) -> Type {
        self.scope_refs_int(scope).unwrap_or_else(|| self.clone())
    }

    /// `None` when no `Ref` or `TVar` is beneath.
    fn scope_refs_int(&self, scope: &ModPath) -> Option<Type> {
        ensure_sufficient(|| self.scope_refs_int_inner(scope))
    }

    fn scope_refs_int_inner(&self, scope: &ModPath) -> Option<Type> {
        match self {
            Type::TVar(tv) => {
                let (bound, cons) = {
                    let cell = tv.cell();
                    let cell = cell.read();
                    (cell.binding.clone(), cell.constraints.clone())
                };
                let fresh = match bound {
                    None => TVar::empty_named(tv.name.clone()),
                    Some(typ) => TVar::named(tv.name.clone(), typ.scope_refs(scope)),
                };
                // The re-minted cell keeps the conjunction (an annotated
                // bound lives only there). A conjunct reaching this very
                // cell is copied unscoped, or re-minting never ends.
                let addr = tv.cell_addr();
                for c in cons.iter() {
                    let c = if tvar::would_cycle_inner(addr, c) {
                        c.clone()
                    } else {
                        c.scope_refs(scope)
                    };
                    fresh.add_cell_constraint(c);
                }
                Some(Type::TVar(fresh))
            }
            Type::Ref(tr) => {
                let params =
                    Arc::from_iter(tr.params.iter().map(|t| t.scope_refs(scope)));
                Some(Type::Ref(tr.with_scope(scope.clone(), params)))
            }
            t => t.cow_children(&mut |c| c.scope_refs_int(scope)),
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
