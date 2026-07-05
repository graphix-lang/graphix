use crate::{
    PRINT_FLAGS, PrintFlag,
    env::{Env, TypeDef},
    expr::ModPath,
    format_with_flags,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx::{publisher::Typ, utils::Either};
use netidx_derive::Pack;
use nohash::IntMap;
use poolshark::{IsoPoolable, local::LPooled};
use smallvec::SmallVec;
use std::{
    cmp::{Eq, PartialEq},
    fmt::Debug,
    iter,
    ops::{Deref, DerefMut},
};
use triomphe::Arc;

mod cast;
mod contains;
pub mod fntyp;
mod matches;
mod normalize;
mod print;
mod setops;
mod tval;
mod tvar;

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
        RefHist { inner, ref_ids: LPooled::take(), next_id: 0 }
    }

    /// Return a stable ID for a Ref type based on (typedef identity, params).
    /// Returns None for non-Ref types — cycle detection is driven by the
    /// Ref side, and None collapses all non-Ref types to the same key.
    fn ref_id(&mut self, t: &Type, env: &Env) -> Option<usize> {
        match t {
            Type::Ref(TypeRef { scope, name, params, .. }) => {
                match env.lookup_typedef(scope, name) {
                    Some(def) => {
                        let def_addr = (def as *const TypeDef).addr();
                        let entries = self.ref_ids.entry(def_addr).or_default();
                        for &(ref p, id) in entries.iter() {
                            if **p == **params {
                                return Some(id);
                            }
                        }
                        let id = self.next_id;
                        self.next_id += 1;
                        entries.push((params.clone(), id));
                        Some(id)
                    }
                    None => None,
                }
            }
            _ => None,
        }
    }
}

/// A unique id for an abstract type. Like the `atomic_id!` types, but with a
/// custom `Pack` impl (in [`crate::expr::serialize`]) that remaps a packed id
/// to a fresh one per decode unit — abstract ids from different packed modules
/// are each numbered from 0, so raw decode would collide. The rest of the API
/// mirrors `atomic_id!`.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct AbstractId(u64);

impl nohash::IsEnabled for AbstractId {}

impl AbstractId {
    pub fn new() -> Self {
        use std::sync::atomic::{AtomicU64, Ordering};
        static NEXT: AtomicU64 = AtomicU64::new(0);
        AbstractId(NEXT.fetch_add(1, Ordering::Relaxed))
    }

    pub fn inner(&self) -> u64 {
        self.0
    }

    pub fn from_inner(i: u64) -> Self {
        AbstractId(i)
    }
}

impl Default for AbstractId {
    fn default() -> Self {
        Self::new()
    }
}

/// A reference to a named typedef, e.g. `Foo` or `Result<i64, string>`.
/// `pos` and `ori` are IDE metadata recording where this reference
/// was written in source — they're populated by the parser and
/// ignored for type-system equality, ordering and hashing so they
/// don't affect type identity.
#[derive(Debug, Clone, Pack)]
#[pack(unwrapped)]
pub struct TypeRef {
    pub scope: ModPath,
    pub name: ModPath,
    pub params: Arc<[Type]>,
    // pos/ori are IDE metadata, excluded from type identity and dropped from
    // the packed form (decode to None).
    #[pack(skip)]
    pub pos: Option<crate::SourcePosition>,
    #[pack(skip)]
    pub ori: Option<Arc<crate::expr::Origin>>,
}

impl TypeRef {
    /// Build a `TypeRef` with no source-position info — for synthetic
    /// type references created during type inference, set operations,
    /// stdlib type literals, etc.
    pub fn synthetic(scope: ModPath, name: ModPath, params: Arc<[Type]>) -> Self {
        Self { scope, name, params, pos: None, ori: None }
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
        // Mirror PartialEq — skip pos/ori (they're source-position
        // metadata, not part of type identity).
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Pack)]
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
    ByRef(Arc<Type>),
    Tuple(Arc<[Type]>),
    Struct(Arc<[(ArcStr, Type)]>),
    Variant(ArcStr, Arc<[Type]>),
    Map { key: Arc<Type>, value: Arc<Type> },
    Abstract { id: AbstractId, params: Arc<[Type]> },
}

impl Default for Type {
    fn default() -> Self {
        Self::Bottom
    }
}

impl Type {
    pub fn empty_tvar() -> Self {
        Type::TVar(TVar::default())
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
            Self::Bottom
            | Self::Any
            | Self::Primitive(_)
            | Self::Fn(_)
            | Self::Set(_)
            | Self::Error(_)
            | Self::Array(_)
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

    pub fn lookup_ref(&self, env: &Env) -> Result<Type> {
        match self {
            Self::Ref(TypeRef { scope, name, params, pos, ori }) => {
                let resolved = env
                    .find_visible(scope, name, |s, n| {
                        env.typedefs.get(s).and_then(|m| m.get(n)).map(|d| {
                            let canonical = ModPath(netidx::path::Path::from(
                                arcstr::ArcStr::from(s),
                            ));
                            (
                                canonical,
                                d.pos,
                                d.ori.clone(),
                                d.params.clone(),
                                d.typ.clone(),
                            )
                        })
                    })
                    .ok_or_else(|| anyhow!("undefined type {name} in {scope}"))?;
                let (canonical_scope, def_pos, def_ori, def_params, def_typ) = resolved;
                if def_params.len() != params.len() {
                    bail!("{} expects {} type parameters", name, def_params.len());
                }
                if env.lsp_mode {
                    if let (Some(pos), Some(ori)) = (pos, ori) {
                        env.push_type_ref(crate::ide::TypeRefSite {
                            pos: *pos,
                            ori: ori.clone(),
                            name: name.clone(),
                            canonical_scope,
                            def_pos,
                            def_ori,
                        });
                    }
                }
                let mut known: LPooled<AHashMap<ArcStr, Type>> = LPooled::take();
                for ((tv, ct), arg) in def_params.iter().zip(params.iter()) {
                    if let Some(ct) = ct {
                        ct.check_contains(env, arg)?;
                    }
                    known.insert(tv.name.clone(), arg.clone());
                }
                Ok(def_typ.replace_tvars(&known))
            }
            t => Ok(t.clone()),
        }
    }

    /// Walk this type tree and, for every `Type::Ref` carrying
    /// parser-populated `pos`/`ori`, push a `TypeRefSite` to the
    /// IDE side-channel. Used at typedef-registration time so
    /// references inside typedef bodies (which the type system
    /// never auto-derefs) still show up in find-references results.
    /// Caller is responsible for gating on `env.lsp_mode`; this
    /// method recurses unconditionally once entered.
    pub fn record_ide_refs(&self, env: &Env, fallback_scope: &ModPath) {
        match self {
            Type::Ref(tr) => {
                if let (Some(pos), Some(ori)) = (tr.pos, &tr.ori) {
                    let resolved = env.find_visible(&tr.scope, &tr.name, |s, n| {
                        env.typedefs.get(s).and_then(|m| m.get(n)).map(|d| {
                            let canonical = ModPath(netidx::path::Path::from(
                                arcstr::ArcStr::from(s),
                            ));
                            (canonical, d.pos, d.ori.clone())
                        })
                    });
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
            Type::Set(ts) | Type::Tuple(ts) | Type::Variant(_, ts) => {
                for t in ts.iter() {
                    t.record_ide_refs(env, fallback_scope);
                }
            }
            Type::Array(t) | Type::Error(t) | Type::ByRef(t) => {
                t.record_ide_refs(env, fallback_scope)
            }
            Type::Map { key, value } => {
                key.record_ide_refs(env, fallback_scope);
                value.record_ide_refs(env, fallback_scope);
            }
            Type::Struct(fields) => {
                for (_, t) in fields.iter() {
                    t.record_ide_refs(env, fallback_scope);
                }
            }
            Type::Fn(ft) => {
                for arg in ft.args.iter() {
                    arg.typ.record_ide_refs(env, fallback_scope);
                }
                ft.rtype.record_ide_refs(env, fallback_scope);
                ft.throws.record_ide_refs(env, fallback_scope);
            }
            Type::Abstract { params, .. } => {
                for p in params.iter() {
                    p.record_ide_refs(env, fallback_scope);
                }
            }
            Type::TVar(tv) => {
                if let Some(t) = tv.read().typ.read().typ.as_ref() {
                    t.record_ide_refs(env, fallback_scope);
                }
            }
            Type::Bottom | Type::Any | Type::Primitive(_) => (),
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

    /// remove the outer error type and return the inner payload, fail if self
    /// isn't an error or contains non error types
    pub fn strip_error(&self, env: &Env) -> Option<Self> {
        self.strip_error_int(
            env,
            &mut RefHist::<AHashSet<Option<usize>>>::new(LPooled::take()),
        )
    }

    pub fn is_bot(&self) -> bool {
        match self {
            Type::Bottom => true,
            Type::Any
            | Type::Abstract { .. }
            | Type::TVar(_)
            | Type::Primitive(_)
            | Type::Ref(TypeRef { .. })
            | Type::Fn(_)
            | Type::Error(_)
            | Type::Array(_)
            | Type::ByRef(_)
            | Type::Tuple(_)
            | Type::Struct(_)
            | Type::Variant(_, _)
            | Type::Set(_)
            | Type::Map { .. } => false,
        }
    }

    pub fn with_deref<R, F: FnOnce(Option<&Self>) -> R>(&self, f: F) -> R {
        match self {
            Self::Bottom
            | Self::Abstract { .. }
            | Self::Any
            | Self::Primitive(_)
            | Self::Fn(_)
            | Self::Set(_)
            | Self::Error(_)
            | Self::Array(_)
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

    pub fn scope_refs(&self, scope: &ModPath) -> Type {
        match self {
            Type::Bottom => Type::Bottom,
            Type::Any => Type::Any,
            Type::Primitive(s) => Type::Primitive(*s),
            Type::Abstract { id, params } => Type::Abstract {
                id: *id,
                params: Arc::from_iter(params.iter().map(|t| t.scope_refs(scope))),
            },
            Type::Error(t0) => Type::Error(Arc::new(t0.scope_refs(scope))),
            Type::Array(t0) => Type::Array(Arc::new(t0.scope_refs(scope))),
            Type::Map { key, value } => {
                let key = Arc::new(key.scope_refs(scope));
                let value = Arc::new(value.scope_refs(scope));
                Type::Map { key, value }
            }
            Type::ByRef(t) => Type::ByRef(Arc::new(t.scope_refs(scope))),
            Type::Tuple(ts) => {
                let i = ts.iter().map(|t| t.scope_refs(scope));
                Type::Tuple(Arc::from_iter(i))
            }
            Type::Variant(tag, ts) => {
                let i = ts.iter().map(|t| t.scope_refs(scope));
                Type::Variant(tag.clone(), Arc::from_iter(i))
            }
            Type::Struct(ts) => {
                let i = ts.iter().map(|(n, t)| (n.clone(), t.scope_refs(scope)));
                Type::Struct(Arc::from_iter(i))
            }
            Type::TVar(tv) => match tv.read().typ.read().typ.as_ref() {
                None => Type::TVar(TVar::empty_named(tv.name.clone())),
                Some(typ) => {
                    let typ = typ.scope_refs(scope);
                    Type::TVar(TVar::named(tv.name.clone(), typ))
                }
            },
            Type::Ref(tr) => {
                let params =
                    Arc::from_iter(tr.params.iter().map(|t| t.scope_refs(scope)));
                Type::Ref(TypeRef { scope: scope.clone(), params, ..tr.clone() })
            }
            Type::Set(ts) => {
                Type::Set(Arc::from_iter(ts.iter().map(|t| t.scope_refs(scope))))
            }
            Type::Fn(f) => Type::Fn(Arc::new(f.scope_refs(scope))),
        }
    }

    /// A unification VIEW of this type with every `Any` leaf replaced by a
    /// fresh (throwaway) TVar, sharing everything else — in particular the
    /// existing TVar CELLS, so bindings made through the view land in the
    /// original type.
    ///
    /// Select's arm typecheck unifies each pattern predicate against the
    /// scrutinee with a bool-discarding `contains` walk whose composite
    /// arms short-circuit on the first false pair. A pattern `_` infers
    /// `Type::Any` (load-bearing for exhaustiveness / dead-arm analysis /
    /// runtime dispatch — a catch-all must match everything), but
    /// `T.contains(Any)` is false, so the walk stopped at a `_` slot and
    /// every LATER slot's bind TVars never narrowed to the scrutinee's
    /// slot types (which also kept those selects from fusing: their arm
    /// types carried unbound TVars that `freeze_region_return` refuses).
    /// Unifying through this view instead makes the `_` slot bind its
    /// throwaway TVar (→ true) and the walk continue, without changing
    /// what the stored predicate means anywhere else.
    pub fn any_as_tvar(&self) -> Type {
        match self {
            Type::Any => Type::empty_tvar(),
            Type::Bottom
            | Type::Primitive(_)
            | Type::TVar(_)
            | Type::Ref(_)
            | Type::Fn(_)
            | Type::Abstract { .. } => self.clone(),
            Type::Error(t) => Type::Error(Arc::new(t.any_as_tvar())),
            Type::Array(t) => Type::Array(Arc::new(t.any_as_tvar())),
            Type::ByRef(t) => Type::ByRef(Arc::new(t.any_as_tvar())),
            Type::Map { key, value } => Type::Map {
                key: Arc::new(key.any_as_tvar()),
                value: Arc::new(value.any_as_tvar()),
            },
            Type::Tuple(ts) => {
                Type::Tuple(Arc::from_iter(ts.iter().map(|t| t.any_as_tvar())))
            }
            Type::Variant(tag, ts) => Type::Variant(
                tag.clone(),
                Arc::from_iter(ts.iter().map(|t| t.any_as_tvar())),
            ),
            Type::Struct(ts) => Type::Struct(Arc::from_iter(
                ts.iter().map(|(n, t)| (n.clone(), t.any_as_tvar())),
            )),
            Type::Set(ts) => {
                Type::Set(Arc::from_iter(ts.iter().map(|t| t.any_as_tvar())))
            }
        }
    }
}
