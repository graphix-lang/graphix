use crate::{
    PRINT_FLAGS, PrintFlag,
    env::{Env, TypeDef},
    expr::ModPath,
    format_with_flags,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack as PackTrait, PackError};
use netidx_core::utils::Either;
use netidx_derive::Pack;
use netidx_value::Typ;
use nohash::IntMap;
use parking_lot::Mutex;
use poolshark::{IsoPoolable, local::LPooled};
use smallvec::SmallVec;
use std::{
    cmp::{Eq, PartialEq},
    fmt::Debug,
    iter,
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

struct RefHist<H: IsoPoolable> {
    inner: LPooled<H>,
    ref_ids: LPooled<IntMap<usize, SmallVec<[(Arc<[Type]>, usize); 2]>>>,
    /// Per-call ref-expansion cache (ref_id → raw `lookup_ref` result).
    /// Committing consumers take `reset_tvars()` copies; the concrete
    /// mass stays Arc-shared so repeated pairs are pruned by identity.
    expansions: LPooled<IntMap<usize, Type>>,
    /// Pure-probe pair memo: `contains_int` verdicts for empty-flag
    /// calls, keyed by both sides' content-Arc identities. Each entry
    /// pins both types so an address cannot be recycled under its key,
    /// and carries the `epoch` at insert: a committing call may bind a
    /// cell a verdict read, so the epoch bumps there.
    probe_pairs: LPooled<AHashMap<(NormKey, NormKey), (u64, bool)>>,
    probe_pins: LPooled<Vec<Type>>,
    /// Content identity → id for non-Ref types with a content key, so
    /// the cycle memo does not conflate distinct finite sub-problems.
    /// Content-less types (Any, primitives, tvars) keep `None`, which
    /// preserves their cycle break.
    content_ids: LPooled<AHashMap<NormKey, usize>>,
    /// A probe that depends on its own verdict claims nothing.
    distribution_probes_in_progress: SmallVec<[usize; 4]>,
    epoch: u64,
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
    fn new(inner: LPooled<H>) -> Self {
        RefHist {
            inner,
            ref_ids: LPooled::take(),
            expansions: LPooled::take(),
            probe_pairs: LPooled::take(),
            probe_pins: LPooled::take(),
            content_ids: LPooled::take(),
            distribution_probes_in_progress: SmallVec::new(),
            epoch: 0,
            next_id: 0,
        }
    }

    /// A committing call ran; prior probe verdicts may be stale.
    fn note_commit(&mut self) {
        self.epoch += 1;
    }

    /// [`norm_key`] extended with `Variant`: a verdict key may include
    /// the tag's allocation identity.
    fn probe_key(t: &Type) -> Option<NormKey> {
        match t {
            Type::Variant(tag, ts) => Some((
                std::mem::discriminant(t),
                (**ts).as_ptr() as usize,
                tag.as_ptr() as usize,
            )),
            t => norm_key(t),
        }
    }

    /// Cached pure-probe verdict for `(t0, t1)`, if current.
    fn probe_get(&self, t0: &Type, t1: &Type) -> Option<bool> {
        let k = (Self::probe_key(t0)?, Self::probe_key(t1)?);
        let (epoch, r) = self.probe_pairs.get(&k).copied()?;
        (epoch == self.epoch).then_some(r)
    }

    fn probe_put(&mut self, t0: &Type, t1: &Type, r: bool) {
        if let (Some(k0), Some(k1)) = (Self::probe_key(t0), Self::probe_key(t1)) {
            if self.probe_pairs.insert((k0, k1), (self.epoch, r)).is_none() {
                self.probe_pins.push(t0.clone());
                self.probe_pins.push(t1.clone());
            }
        }
    }

    /// [`Type::lookup_ref`] through the expansion cache. A non-Ref, an
    /// unresolvable ref, or a ref with TVar params (its expansion embeds
    /// the caller's live cells) goes uncached. `raw` (pure probes only)
    /// hands back the cached expansion itself; committing calls take
    /// `reset_tvars()` copies.
    fn expand_ref(
        &mut self,
        t: &Type,
        id: Option<usize>,
        env: &Env,
        raw: bool,
    ) -> Result<Type> {
        // A non-Ref has a content id for the cycle memo, but caching its
        // expansion would sever its live inference cells.
        if !matches!(t, Type::Ref(_)) {
            return t.lookup_ref(env);
        }
        let Some(id) = id else { return t.lookup_ref(env) };
        let closed = match t {
            Type::Ref(tr) => tr.params.iter().all(|p| p.tvar_free()),
            _ => true,
        };
        if !closed {
            return t.lookup_ref(env);
        }
        if let Some(e) = self.expansions.get(&id) {
            return Ok(if raw { e.clone() } else { e.reset_tvars() });
        }
        let e = t.lookup_ref(env)?;
        self.expansions.insert(id, e.clone());
        Ok(if raw { e } else { e.reset_tvars() })
    }

    /// A stable id for a type: a Ref keys on (definition identity,
    /// params) — the filled resolution cell when present, else the
    /// env-resolved `TypeDef` address; a non-Ref with content keys on
    /// its content; anything else is `None`.
    fn ref_id(&mut self, t: &Type, env: &Env) -> Option<usize> {
        match t {
            Type::Ref(tr) => {
                let def_addr = match tr.resolved() {
                    Some(r) => Arc::as_ptr(&r).addr(),
                    None => {
                        match env.lookup_typedef(&tr.scope, &tr.name).ok().flatten() {
                            Some(def) => (def as *const TypeDef).addr(),
                            None => return None,
                        }
                    }
                };
                let params = &tr.params;
                let entries = self.ref_ids.entry(def_addr).or_default();
                for &(ref p, id) in entries.iter() {
                    if p.len() == params.len()
                        && p.iter()
                            .zip(params.iter())
                            .all(|(a, b)| setops::union_identical(a, b))
                    {
                        return Some(id);
                    }
                }
                let id = self.next_id;
                self.next_id += 1;
                entries.push((params.clone(), id));
                Some(id)
            }
            _ => {
                let k = Self::probe_key(t)?;
                if let Some(&id) = self.content_ids.get(&k) {
                    return Some(id);
                }
                let id = self.next_id;
                self.next_id += 1;
                self.content_ids.insert(k, id);
                Some(id)
            }
        }
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
    pos: crate::SourcePosition,
    ori: Arc<crate::expr::Origin>,
    params: Arc<[(TVar, Option<Type>)]>,
    typ: Type,
}

impl ResolvedRef {
    /// Same definition? Cells filled from one `TypeDef` share its
    /// content Arcs, so this is usually a pointer comparison.
    pub(crate) fn same_def(&self, other: &Self) -> bool {
        (Arc::ptr_eq(&self.params, &other.params) || self.params == other.params)
            && self.typ == other.typ
    }

    pub(crate) fn typ(&self) -> &Type {
        &self.typ
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
/// mints fresh. Never overwrite a filled cell — clones share it.
#[derive(Debug, Clone)]
pub struct TypeRef {
    pub scope: ModPath,
    pub name: ModPath,
    pub params: Arc<[Type]>,
    pub pos: Option<crate::SourcePosition>,
    pub ori: Option<Arc<crate::expr::Origin>>,
    pub(in crate::typ) resolved: Arc<Mutex<Option<Arc<ResolvedRef>>>>,
}

fn resolved_len(r: &ResolvedRef) -> usize {
    let ResolvedRef { canonical_scope, pos, ori, params, typ } = r;
    canonical_scope.encoded_len()
        + crate::image::pos_len(pos)
        + crate::image::origin_len(ori)
        + params.encoded_len()
        + typ.encoded_len()
}

fn resolved_encode(r: &ResolvedRef, buf: &mut impl BufMut) -> Result<(), PackError> {
    let ResolvedRef { canonical_scope, pos, ori, params, typ } = r;
    canonical_scope.encode(buf)?;
    crate::image::pos_encode(pos, buf)?;
    crate::image::origin_encode(ori, buf)?;
    params.encode(buf)?;
    typ.encode(buf)
}

fn resolved_decode(buf: &mut impl Buf) -> Result<ResolvedRef, PackError> {
    Ok(ResolvedRef {
        canonical_scope: PackTrait::decode(buf)?,
        pos: crate::image::pos_decode(buf)?,
        ori: crate::image::origin_decode(buf)?,
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
        if !crate::image::is_encoding() {
            return base;
        }
        let pos = 1 + pos.as_ref().map_or(0, crate::image::pos_len);
        let ori = 1 + ori.as_ref().map_or(0, crate::image::origin_len);
        base + pos + ori + crate::image::refcell_len(resolved, resolved_len)
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let TypeRef { scope, name, params, pos, ori, resolved } = self;
        scope.encode(buf)?;
        name.encode(buf)?;
        params.encode(buf)?;
        if !crate::image::is_encoding() {
            return Ok(());
        }
        match pos {
            None => buf.put_u8(0),
            Some(p) => {
                buf.put_u8(1);
                crate::image::pos_encode(p, buf)?;
            }
        }
        match ori {
            None => buf.put_u8(0),
            Some(o) => {
                buf.put_u8(1);
                crate::image::origin_encode(o, buf)?;
            }
        }
        crate::image::refcell_encode(resolved, buf, resolved_encode)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let scope = PackTrait::decode(buf)?;
        let name = PackTrait::decode(buf)?;
        let params = PackTrait::decode(buf)?;
        if !crate::image::is_decoding() {
            return Ok(TypeRef::new(scope, name, params, None, None));
        }
        let pos = match u8::decode(buf)? {
            0 => None,
            1 => Some(crate::image::pos_decode(buf)?),
            _ => return Err(PackError::UnknownTag),
        };
        let ori = match u8::decode(buf)? {
            0 => None,
            1 => Some(crate::image::origin_decode(buf)?),
            _ => return Err(PackError::UnknownTag),
        };
        let resolved = crate::image::refcell_decode(buf, resolved_decode)?;
        Ok(TypeRef { scope, name, params, pos, ori, resolved })
    }
}

impl TypeRef {
    pub fn new(
        scope: ModPath,
        name: ModPath,
        params: Arc<[Type]>,
        pos: Option<crate::SourcePosition>,
        ori: Option<Arc<crate::expr::Origin>>,
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

    /// This ref re-scoped, with a fresh resolution cell pre-filled from
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
                        arcstr::ArcStr::from(s),
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
    /// cell lock held (resolution can re-enter). Returns whether this
    /// call filled the cell.
    fn resolve_in_raw(&self, env: &Env) -> Option<(Arc<ResolvedRef>, bool)> {
        if let Some(r) = self.resolved() {
            return Some((r, false));
        }
        let r = self.resolve_pure(env)?;
        let mut guard = self.resolved.lock();
        match &*guard {
            Some(r) => Some((r.clone(), false)),
            None => {
                *guard = Some(r.clone());
                Some((r, true))
            }
        }
    }

    /// [`Self::resolve_in_raw`] without the fill flag. Fills only this
    /// ref, not the snapshot's nested refs: mid-compile the env is
    /// incomplete, and a nested name can resolve to an outer shadow.
    pub(crate) fn resolve_in(&self, env: &Env) -> Option<Arc<ResolvedRef>> {
        self.resolve_in_raw(env).map(|(r, _)| r)
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

#[derive(Debug, Clone, Eq, PartialOrd, Ord, Hash, Pack)]
#[pack(unwrapped)]
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
    /// `node::collection::list`.
    List(Arc<Type>),
    ByRef(Arc<Type>),
    Tuple(Arc<[Type]>),
    Struct(Arc<[(ArcStr, Type)]>),
    Variant(ArcStr, Arc<[Type]>),
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

/// Structural equality with content-Arc pointer shortcuts (the
/// copy-on-write walks share aggressively). Exhaustive on `self` so a
/// new variant fails to compile.
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        fn slice_eq(a: &Arc<[Type]>, b: &Arc<[Type]>) -> bool {
            (**a).as_ptr() == (**b).as_ptr() || **a == **b
        }
        match self {
            Type::Bottom => matches!(other, Type::Bottom),
            Type::Any => matches!(other, Type::Any),
            Type::Primitive(a) => matches!(other, Type::Primitive(b) if a == b),
            Type::Ref(a) => matches!(other, Type::Ref(b) if a == b),
            Type::Fn(a) => {
                matches!(other, Type::Fn(b) if Arc::ptr_eq(a, b) || a == b)
            }
            Type::Set(a) => matches!(other, Type::Set(b) if slice_eq(a, b)),
            Type::TVar(a) => matches!(other, Type::TVar(b) if a == b),
            Type::Error(a) => {
                matches!(other, Type::Error(b) if Arc::ptr_eq(a, b) || a == b)
            }
            Type::Array(a) => {
                matches!(other, Type::Array(b) if Arc::ptr_eq(a, b) || a == b)
            }
            Type::List(a) => {
                matches!(other, Type::List(b) if Arc::ptr_eq(a, b) || a == b)
            }
            Type::ByRef(a) => {
                matches!(other, Type::ByRef(b) if Arc::ptr_eq(a, b) || a == b)
            }
            Type::Tuple(a) => matches!(other, Type::Tuple(b) if slice_eq(a, b)),
            Type::Struct(a) => matches!(
                other,
                Type::Struct(b) if (**a).as_ptr() == (**b).as_ptr() || **a == **b
            ),
            Type::Variant(t0, a) => {
                matches!(other, Type::Variant(t1, b) if t0 == t1 && slice_eq(a, b))
            }
            Type::Map { key: k0, value: v0 } => matches!(
                other,
                Type::Map { key: k1, value: v1 }
                    if (Arc::ptr_eq(k0, k1) || k0 == k1)
                        && (Arc::ptr_eq(v0, v1) || v0 == v1)
            ),
            Type::Abstract { id: i0, params: p0 } => matches!(
                other,
                Type::Abstract { id: i1, params: p1 } if i0 == i1 && slice_eq(p0, p1)
            ),
            Type::App(c0, a0) => matches!(
                other,
                Type::App(c1, a1)
                    if (Arc::ptr_eq(c0, c1) || c0 == c1)
                        && (Arc::ptr_eq(a0, a1) || a0 == a1)
            ),
            Type::Hole => matches!(other, Type::Hole),
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::Bottom
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
            Type::Set(ts) | Type::Tuple(ts) | Type::Variant(_, ts) => {
                for t in ts.iter() {
                    f(t)?;
                }
                ControlFlow::Continue(())
            }
            Type::Struct(fs) => {
                for (_, t) in fs.iter() {
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
            Type::Variant(tag, ts) => {
                Type::cow_slice(ts, |t| f(t)).map(|ts| Type::Variant(tag.clone(), ts))
            }
            Type::Set(ts) => Type::cow_slice(ts, |t| f(t)).map(Type::Set),
            Type::Struct(fs) => {
                Type::cow_slice(fs, |(n, t)| f(t).map(|t| (n.clone(), t)))
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
        let cons = cv.read().typ.read().constraints.clone();
        for c in cons.iter() {
            let Type::Ref(tr) = c else { continue };
            let Some(tid) = env.trait_of_ref(tr) else { continue };
            let Some(heads) = env.impls.get(&tid) else { continue };
            for im in heads.iter() {
                if !matches!(im.target, Type::Ref(_)) {
                    continue;
                }
                let head = im.target.reset_tvars();
                let elem = Type::empty_tvar();
                let Some(filled) = head.fill_hole(&elem) else { continue };
                // A proper subtype (`[`Nil]` under `List<'_>`) leaves the
                // element open and is not this constructor.
                if filled.contains(env, &t)? && elem.with_deref(|e| e.is_some()) {
                    let r = (head.resolve_tvars(), elem.resolve_tvars());
                    if crate::dbgenv::graphix_dbg_bind() {
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
        match self {
            Type::App(c, a) if matches!(&**c, Type::TVar(tv) if &*tv.name == "self") => {
                *applied = true;
                a.self_shape(applied, bare)
            }
            Type::TVar(tv) if &*tv.name == "self" => *bare = true,
            t => t.for_each_child(&mut |c| c.self_shape(applied, bare)),
        }
    }

    /// The number of holes in this type.
    pub(crate) fn holes(&self) -> usize {
        match self {
            Type::Hole => 1,
            t => {
                let mut n = 0;
                t.for_each_child(&mut |c| n += c.holes());
                n
            }
        }
    }

    /// Pre-unify a declared parameter type with an argument's type
    /// before the argument typechecks, so an unannotated callback's
    /// parameters take the declared types. A function-typed argument
    /// unifies its parameter positions only; anything else whole.
    pub(crate) fn pre_unify_arg(env: &Env, declared: &Type, actual: &Type) -> Result<()> {
        let d = declared.deref_cloned();
        let a = actual.deref_cloned();
        match (d, a) {
            (Some(Type::Fn(d)), Some(Type::Fn(a))) => d.pre_unify_params(env, &a),
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
        match self {
            Type::Hole => Some(arg.clone()),
            t => t.cow_children(&mut |c| c.fill_hole(arg)),
        }
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

    pub fn is_defined(&self) -> bool {
        match self {
            Self::App(c, a) => c.is_defined() && a.is_defined(),
            Self::Hole => true,
            Self::Bottom
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
            | Self::Variant(_, _)
            | Self::Ref(TypeRef { .. })
            | Self::Map { .. }
            | Self::Abstract { .. } => true,
            Self::TVar(tv) => tv.read().typ.read().typ.is_some(),
        }
    }

    /// No TVar anywhere beneath (Ref params, not expansions). A
    /// tvar-free type's identity is stable, so it can key a cache.
    pub(crate) fn tvar_free(&self) -> bool {
        match self {
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
        }
    }

    /// Fill the resolution cell of every `Type::Ref` reachable from
    /// this type against `env`, for a type about to outlive the env
    /// that gives its names meaning. Names not visible are skipped
    /// (they fill at their first in-context lookup). Recurses through
    /// filled snapshot bodies.
    pub fn seed_refs(&self, env: &Env) {
        struct Seen {
            cells: poolshark::local::LPooled<AHashSet<usize>>,
            nodes: poolshark::local::LPooled<AHashSet<usize>>,
        }
        fn go(t: &Type, env: &Env, seen: &mut Seen) {
            let node = match t {
                Type::Set(a) | Type::Tuple(a) | Type::Variant(_, a) => {
                    Some((**a).as_ptr().addr())
                }
                Type::Struct(a) => Some((**a).as_ptr().addr()),
                Type::Fn(f) => Some((&**f as *const FnType).addr()),
                Type::Error(a) | Type::Array(a) | Type::List(a) | Type::ByRef(a) => {
                    Some((&**a as *const Type).addr())
                }
                _ => None,
            };
            if let Some(node) = node
                && !seen.nodes.insert(node)
            {
                return;
            }
            match t {
                Type::Bottom
                | Type::Any
                | Type::Primitive(_)
                | Type::Abstract { .. }
                | Type::Hole => (),
                Type::App(c, a) => {
                    go(c, env, seen);
                    go(a, env, seen)
                }
                Type::Ref(tr) => {
                    for p in tr.params.iter() {
                        go(p, env, seen);
                    }
                    // Keyed on the cell: with_params clones share it.
                    if !seen.cells.insert(Arc::as_ptr(&tr.resolved).addr()) {
                        return;
                    }
                    if let Some((r, _)) = tr.resolve_in_raw(env) {
                        for (_, constraint) in r.params.iter() {
                            if let Some(c) = constraint {
                                go(c, env, seen);
                            }
                        }
                        go(&r.typ, env, seen);
                    }
                }
                Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(t) => {
                    go(t, env, seen)
                }
                Type::Map { key, value } => {
                    go(key, env, seen);
                    go(value, env, seen);
                }
                Type::Tuple(ts) | Type::Variant(_, ts) | Type::Set(ts) => {
                    for t in ts.iter() {
                        go(t, env, seen);
                    }
                }
                Type::Struct(ts) => {
                    for (_, t) in ts.iter() {
                        go(t, env, seen);
                    }
                }
                Type::TVar(tv) => {
                    let cell = tv.read().typ.clone();
                    if !seen.cells.insert(triomphe::Arc::as_ptr(&cell).addr()) {
                        return;
                    }
                    let bound = cell.read().typ.clone();
                    if let Some(t) = bound {
                        go(&t, env, seen);
                    }
                }
                Type::Fn(f) => {
                    for a in f.args.iter() {
                        go(&a.typ, env, seen);
                    }
                    if let Some(t) = f.vargs.as_ref() {
                        go(t, env, seen);
                    }
                    go(&f.rtype, env, seen);
                    go(&f.throws, env, seen);
                }
            }
        }
        let mut seen = Seen {
            cells: poolshark::local::LPooled::take(),
            nodes: poolshark::local::LPooled::take(),
        };
        go(self, env, &mut seen)
    }

    pub fn lookup_ref(&self, env: &Env) -> Result<Type> {
        match self {
            Self::Ref(tr) => {
                let TypeRef { scope, name, params, pos, ori, resolved: _ } = tr;
                let resolved = tr.resolve_in(env).ok_or_else(|| {
                    if std::env::var_os("GXDBG_TYPEREF").is_some() {
                        eprintln!(
                            "TYPEREF-MISS {name} in {scope}; typedef scopes with the name:"
                        );
                        for (s, m) in env.typedefs.into_iter() {
                            if m.into_iter().any(|(n, _)| {
                                name.ends_with(n.as_str())
                            }) {
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
                        Type::TVar(tv) if tv.read().typ.read().typ.is_none() => {
                            tv.add_cell_constraint(constraint)
                        }
                        _ => constraint.check_contains(env, arg)?,
                    }
                }
                Ok(def_typ.replace_tvars(&known))
            }
            t => Ok(t.clone()),
        }
    }

    /// Push a `TypeRefSite` for every `Type::Ref` beneath that carries
    /// a source position. The caller gates on `env.lsp_mode`.
    pub fn record_ide_refs(&self, env: &Env, fallback_scope: &ModPath) {
        match self {
            Type::Ref(tr) => {
                if let (Some(pos), Some(ori)) = (tr.pos, &tr.ori) {
                    let resolved = env
                        .resolve_visible(
                            &tr.scope,
                            &tr.name,
                            crate::env::NameNs::Type,
                            |s, n| {
                                env.typedefs.get(s).and_then(|m| m.get(n)).map(|d| {
                                    let canonical =
                                        ModPath(netidx_core::path::Path::from(
                                            arcstr::ArcStr::from(s),
                                        ));
                                    (canonical, d.pos, d.ori.clone())
                                })
                            },
                        )
                        .ok()
                        .flatten();
                    let (canonical_scope, def_pos, def_ori) = match resolved {
                        Some((s, dp, do_)) => (s, dp, do_),
                        None => (
                            fallback_scope.clone(),
                            crate::SourcePosition::default(),
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
                if let Some(t) = tv.read().typ.read().typ.as_ref() {
                    t.record_ide_refs(env, fallback_scope);
                }
            }
            t => t.for_each_child(&mut |c| c.record_ide_refs(env, fallback_scope)),
        }
    }

    pub fn any() -> Self {
        Self::Any
    }

    pub fn boolean() -> Self {
        Self::Primitive(Typ::Bool.into())
    }

    pub fn number() -> Self {
        Self::Primitive(Typ::number())
    }

    pub fn int() -> Self {
        Self::Primitive(Typ::integer())
    }

    pub fn uint() -> Self {
        Self::Primitive(Typ::unsigned_integer())
    }

    fn strip_error_int(
        &self,
        env: &Env,
        hist: &mut RefHist<AHashSet<Option<usize>>>,
    ) -> Option<Type> {
        match self {
            Type::App(..) | Type::Hole => None,
            Type::Error(t) => match t.strip_error_int(env, hist) {
                Some(t) => Some(t),
                None => Some((**t).clone()),
            },
            Type::TVar(tv) => tv
                .read()
                .typ
                .read()
                .typ
                .as_ref()
                .and_then(|t| t.strip_error_int(env, hist)),
            Type::Primitive(p) => {
                if *p == BitFlags::from(Typ::Error) {
                    Some(Type::Any)
                } else {
                    None
                }
            }
            Type::Ref(TypeRef { .. }) => {
                let id = hist.ref_id(self, env);
                let t = self.lookup_ref(env).ok()?;
                if hist.insert(id) { t.strip_error_int(env, hist) } else { None }
            }
            Type::Set(s) => {
                let r = Self::flatten_set(
                    s.iter().filter_map(|t| t.strip_error_int(env, hist)),
                );
                match r {
                    Type::Primitive(p) if p.is_empty() => None,
                    t => Some(t),
                }
            }
            Type::Array(_)
            | Type::List(_)
            | Type::Map { .. }
            | Type::ByRef(_)
            | Type::Tuple(_)
            | Type::Struct(_)
            | Type::Variant(_, _)
            | Type::Fn(_)
            | Type::Any
            | Type::Bottom
            | Type::Abstract { .. } => None,
        }
    }

    /// The payload of the outer error type; `None` if self is not an
    /// error or contains non-error members.
    pub fn strip_error(&self, env: &Env) -> Option<Self> {
        self.strip_error_int(
            env,
            &mut RefHist::<AHashSet<Option<usize>>>::new(LPooled::take()),
        )
    }

    pub fn is_bot(&self) -> bool {
        match self {
            Type::Bottom => true,
            Type::App(..) | Type::Hole => false,
            Type::Any
            | Type::Abstract { .. }
            | Type::TVar(_)
            | Type::Primitive(_)
            | Type::Ref(TypeRef { .. })
            | Type::Fn(_)
            | Type::Error(_)
            | Type::Array(_)
            | Type::List(_)
            | Type::ByRef(_)
            | Type::Tuple(_)
            | Type::Struct(_)
            | Type::Variant(_, _)
            | Type::Set(_)
            | Type::Map { .. } => false,
        }
    }

    /// `Bottom`, or a union whose every member (through bound tvars)
    /// is. An unbound tvar is not provably bottom.
    pub fn all_bottom(&self) -> bool {
        crate::stack::ensure_sufficient(|| {
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
        crate::stack::ensure_sufficient(|| {
            self.with_deref(|t| match t {
                Some(Type::Bottom) => true,
                Some(
                    Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(t),
                ) => t.has_bottom(),
                Some(Type::Map { key, value }) => key.has_bottom() || value.has_bottom(),
                Some(Type::Tuple(ts) | Type::Variant(_, ts) | Type::Set(ts)) => {
                    ts.iter().any(|t| t.has_bottom())
                }
                Some(Type::Struct(fs)) => fs.iter().any(|(_, t)| t.has_bottom()),
                _ => false,
            })
        })
    }

    /// The dereferenced type, cloned; `None` for an unbound cell.
    pub fn deref_cloned(&self) -> Option<Self> {
        self.with_deref(|t| t.cloned())
    }

    pub fn with_deref<R, F: FnOnce(Option<&Self>) -> R>(&self, f: F) -> R {
        match self {
            // A filled application is its filled type to every walk.
            Self::App(c, a) => match Self::app_filled(c, a) {
                Some(filled) => filled.with_deref(f),
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
            | Self::Variant(_, _)
            | Self::Ref(TypeRef { .. })
            | Self::Map { .. } => f(Some(self)),
            Self::TVar(tv) => match tv.read().typ.read().typ.as_ref() {
                Some(t) => t.with_deref(f),
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
                            let r = t.rewrite_trait_args(env)?;
                            changed |= !r.ptr_eq_shallow(t);
                            r
                        }
                    };
                    args.push(FnArgType { kind: a.kind.clone(), typ });
                }
                let vargs = match &ft.vargs {
                    None => None,
                    Some(t) => {
                        let r = t.rewrite_trait_args(env)?;
                        changed |= !r.ptr_eq_shallow(t);
                        Some(r)
                    }
                };
                let rtype = ft.rtype.rewrite_trait_args(env)?;
                changed |= !rtype.ptr_eq_shallow(&ft.rtype);
                let throws = ft.throws.rewrite_trait_args(env)?;
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
                let r = t.cow_children(&mut |c| match c.rewrite_trait_args(env) {
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
        crate::stack::ensure_sufficient(|| self.scope_refs_int_inner(scope))
    }

    fn scope_refs_int_inner(&self, scope: &ModPath) -> Option<Type> {
        match self {
            Type::TVar(tv) => {
                let (bound, cons) = {
                    let cell = tv.read().typ.clone();
                    let cell = cell.read();
                    (cell.typ.clone(), cell.constraints.clone())
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
                    let c = if crate::typ::tvar::would_cycle_inner(addr, c) {
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
        match self {
            Type::Any => Some(Type::empty_tvar()),
            Type::Ref(_) | Type::Fn(_) | Type::Abstract { .. } => None,
            t => t.cow_children(&mut |c| c.any_as_tvar_int()),
        }
    }
}
