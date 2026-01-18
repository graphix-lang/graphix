use crate::{env::Env, expr::ModPath, format_with_flags, PrintFlag, PRINT_FLAGS};
use anyhow::{anyhow, bail, Result};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use fxhash::FxHashSet;
use netidx::{publisher::Typ, utils::Either};
use poolshark::local::LPooled;
use std::{
    cmp::{Eq, PartialEq},
    fmt::Debug,
    iter,
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

pub use fntyp::{FnArgType, FnType};
pub use tval::TVal;
pub use tvar::TVar;

struct AndAc(bool);

impl FromIterator<bool> for AndAc {
    fn from_iter<T: IntoIterator<Item = bool>>(iter: T) -> Self {
        AndAc(iter.into_iter().all(|b| b))
    }
}

atomic_id!(AbstractId);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Bottom,
    Any,
    Primitive(BitFlags<Typ>),
    Ref { scope: ModPath, name: ModPath, params: Arc<[Type]> },
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
            | Self::Ref { .. }
            | Self::Map { .. }
            | Self::Abstract { .. } => true,
            Self::TVar(tv) => tv.read().typ.read().is_some(),
        }
    }

    pub fn lookup_ref<'a>(&'a self, env: &'a Env) -> Result<&'a Type> {
        match self {
            Self::Ref { scope, name, params } => {
                let def = env
                    .lookup_typedef(scope, name)
                    .ok_or_else(|| anyhow!("undefined type {name} in {scope}"))?;
                if def.params.len() != params.len() {
                    bail!("{} expects {} type parameters", name, def.params.len());
                }
                def.typ.unbind_tvars();
                for ((tv, ct), arg) in def.params.iter().zip(params.iter()) {
                    if let Some(ct) = ct {
                        ct.check_contains(env, arg)?;
                    }
                    if !tv.would_cycle(arg) {
                        match arg {
                            Type::TVar(arg_tv) => match &*arg_tv.read().typ.read() {
                                None => *tv.read().typ.write() = Some(arg.clone()),
                                Some(t) => *tv.read().typ.write() = Some(t.clone()),
                            },
                            _ => {
                                *tv.read().typ.write() = Some(arg.clone());
                            }
                        }
                    }
                }
                Ok(&def.typ)
            }
            t => Ok(t),
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

    fn strip_error_int(&self, env: &Env, hist: &mut FxHashSet<usize>) -> Option<Type> {
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
            Type::Ref { .. } => {
                let t = self.lookup_ref(env).ok()?;
                let addr = t as *const Type as usize;
                if hist.insert(addr) {
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
        self.strip_error_int(env, &mut LPooled::take())
    }

    pub fn is_bot(&self) -> bool {
        match self {
            Type::Bottom => true,
            Type::Any
            | Type::Abstract { .. }
            | Type::TVar(_)
            | Type::Primitive(_)
            | Type::Ref { .. }
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
            | Self::Ref { .. }
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
            Type::Ref { scope: _, name, params } => {
                let params = Arc::from_iter(params.iter().map(|t| t.scope_refs(scope)));
                Type::Ref { scope: scope.clone(), name: name.clone(), params }
            }
            Type::Set(ts) => {
                Type::Set(Arc::from_iter(ts.iter().map(|t| t.scope_refs(scope))))
            }
            Type::Fn(f) => Type::Fn(Arc::new(f.scope_refs(scope))),
        }
    }
}
