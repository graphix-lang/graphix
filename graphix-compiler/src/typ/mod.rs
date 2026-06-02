use crate::{
    env::{Env, TypeDef},
    expr::ModPath,
    format_with_flags, PrintFlag, PRINT_FLAGS,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{anyhow, bail, Result};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx::{publisher::Typ, utils::Either};
use nohash::IntMap;
use poolshark::{local::LPooled, IsoPoolable};
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
mod fntyp;
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

atomic_id!(AbstractId);

/// A reference to a named typedef, e.g. `Foo` or `Result<i64, string>`.
/// `pos` and `ori` are IDE metadata recording where this reference
/// was written in source — they're populated by the parser and
/// ignored for type-system equality, ordering and hashing so they
/// don't affect type identity.
#[derive(Debug, Clone)]
pub struct TypeRef {
    pub scope: ModPath,
    pub name: ModPath,
    pub params: Arc<[Type]>,
    pub pos: Option<crate::SourcePosition>,
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
            Self::TVar(tv) => tv.read().typ.read().is_some(),
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
                        env.push_type_ref(crate::TypeRefSite {
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
                    env.push_type_ref(crate::TypeRefSite {
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
                if let Some(t) = tv.read().typ.read().as_ref() {
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
            Type::TVar(tv) => {
                tv.read().typ.read().as_ref().and_then(|t| t.strip_error_int(env, hist))
            }
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
                if hist.insert(id) {
                    t.strip_error_int(env, hist)
                } else {
                    None
                }
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
            Self::TVar(tv) => match tv.read().typ.read().as_ref() {
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
            Type::TVar(tv) => match tv.read().typ.read().as_ref() {
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
}
