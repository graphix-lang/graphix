use crate::{
    expr::ModPath,
    typ::{FnType, PrintFlag, Type, PRINT_FLAGS},
};
use anyhow::{bail, Result};
use arcstr::ArcStr;
use compact_str::format_compact;
use fxhash::{FxHashMap, FxHashSet};
use parking_lot::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::{
    cmp::{Eq, PartialEq},
    collections::hash_map::Entry,
    fmt::{self, Debug},
    hash::Hash,
    ops::Deref,
};
use triomphe::Arc;

atomic_id!(TVarId);

pub(super) fn would_cycle_inner(addr: usize, t: &Type) -> bool {
    match t {
        Type::Primitive(_) | Type::Any | Type::Bottom | Type::Ref { .. } => false,
        Type::TVar(t) => {
            Arc::as_ptr(&t.read().typ).addr() == addr
                || match &*t.read().typ.read() {
                    None => false,
                    Some(t) => would_cycle_inner(addr, t),
                }
        }
        Type::Abstract { id: _, params } => {
            params.iter().any(|t| would_cycle_inner(addr, t))
        }
        Type::Error(t) => would_cycle_inner(addr, t),
        Type::Array(a) => would_cycle_inner(addr, &**a),
        Type::Map { key, value } => {
            would_cycle_inner(addr, &**key) || would_cycle_inner(addr, &**value)
        }
        Type::ByRef(t) => would_cycle_inner(addr, t),
        Type::Tuple(ts) => ts.iter().any(|t| would_cycle_inner(addr, t)),
        Type::Variant(_, ts) => ts.iter().any(|t| would_cycle_inner(addr, t)),
        Type::Struct(ts) => ts.iter().any(|(_, t)| would_cycle_inner(addr, t)),
        Type::Set(s) => s.iter().any(|t| would_cycle_inner(addr, t)),
        Type::Fn(f) => {
            let FnType { args, vargs, rtype, constraints, throws, explicit_throws: _ } =
                &**f;
            args.iter().any(|t| would_cycle_inner(addr, &t.typ))
                || match vargs {
                    None => false,
                    Some(t) => would_cycle_inner(addr, t),
                }
                || would_cycle_inner(addr, rtype)
                || constraints.read().iter().any(|a| {
                    Arc::as_ptr(&a.0.read().typ).addr() == addr
                        || would_cycle_inner(addr, &a.1)
                })
                || would_cycle_inner(addr, &throws)
        }
    }
}

#[derive(Debug)]
pub struct TVarInnerInner {
    pub(crate) id: TVarId,
    pub(crate) frozen: bool,
    pub(crate) typ: Arc<RwLock<Option<Type>>>,
}

#[derive(Debug)]
pub struct TVarInner {
    pub name: ArcStr,
    pub(crate) typ: RwLock<TVarInnerInner>,
}

#[derive(Debug, Clone)]
pub struct TVar(Arc<TVarInner>);

impl fmt::Display for TVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !PRINT_FLAGS.get().contains(PrintFlag::DerefTVars) {
            write!(f, "'{}", self.name)
        } else {
            write!(f, "'{}: ", self.name)?;
            match &*self.read().typ.read() {
                Some(t) => write!(f, "{t}"),
                None => write!(f, "unbound"),
            }
        }
    }
}

impl Default for TVar {
    fn default() -> Self {
        Self::empty_named(ArcStr::from(format_compact!("_{}", TVarId::new().0).as_str()))
    }
}

impl Deref for TVar {
    type Target = TVarInner;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl PartialEq for TVar {
    fn eq(&self, other: &Self) -> bool {
        let t0 = self.read();
        let t1 = other.read();
        t0.typ.as_ptr().addr() == t1.typ.as_ptr().addr() || {
            let t0 = t0.typ.read();
            let t1 = t1.typ.read();
            *t0 == *t1
        }
    }
}

impl Eq for TVar {}

impl PartialOrd for TVar {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let t0 = self.read();
        let t1 = other.read();
        if t0.typ.as_ptr().addr() == t1.typ.as_ptr().addr() {
            Some(std::cmp::Ordering::Equal)
        } else {
            let t0 = t0.typ.read();
            let t1 = t1.typ.read();
            t0.partial_cmp(&*t1)
        }
    }
}

impl Ord for TVar {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let t0 = self.read();
        let t1 = other.read();
        if t0.typ.as_ptr().addr() == t1.typ.as_ptr().addr() {
            std::cmp::Ordering::Equal
        } else {
            let t0 = t0.typ.read();
            let t1 = t1.typ.read();
            t0.cmp(&*t1)
        }
    }
}

impl TVar {
    pub fn scope_refs(&self, scope: &ModPath) -> Self {
        match Type::TVar(self.clone()).scope_refs(scope) {
            Type::TVar(tv) => tv,
            _ => unreachable!(),
        }
    }

    pub fn empty_named(name: ArcStr) -> Self {
        Self(Arc::new(TVarInner {
            name,
            typ: RwLock::new(TVarInnerInner {
                id: TVarId::new(),
                frozen: false,
                typ: Arc::new(RwLock::new(None)),
            }),
        }))
    }

    pub fn named(name: ArcStr, typ: Type) -> Self {
        Self(Arc::new(TVarInner {
            name,
            typ: RwLock::new(TVarInnerInner {
                id: TVarId::new(),
                frozen: false,
                typ: Arc::new(RwLock::new(Some(typ))),
            }),
        }))
    }

    pub fn read<'a>(&'a self) -> RwLockReadGuard<'a, TVarInnerInner> {
        self.typ.read()
    }

    pub fn write<'a>(&'a self) -> RwLockWriteGuard<'a, TVarInnerInner> {
        self.typ.write()
    }

    /// make self an alias for other
    pub fn alias(&self, other: &Self) {
        let mut s = self.write();
        if !s.frozen {
            s.frozen = true;
            let o = other.read();
            s.id = o.id;
            s.typ = Arc::clone(&o.typ);
        }
    }

    pub fn freeze(&self) {
        self.write().frozen = true;
    }

    /// copy self from other
    pub fn copy(&self, other: &Self) {
        let s = self.read();
        let o = other.read();
        *s.typ.write() = o.typ.read().clone();
    }

    pub fn normalize(&self) -> Self {
        match &mut *self.read().typ.write() {
            None => (),
            Some(t) => {
                *t = t.normalize();
            }
        }
        self.clone()
    }

    pub fn unbind(&self) {
        *self.read().typ.write() = None
    }

    pub(super) fn would_cycle(&self, t: &Type) -> bool {
        let addr = Arc::as_ptr(&self.read().typ).addr();
        would_cycle_inner(addr, t)
    }

    pub(super) fn addr(&self) -> usize {
        Arc::as_ptr(&self.0).addr()
    }

    pub(super) fn inner_addr(&self) -> usize {
        Arc::as_ptr(&self.read().typ).addr()
    }
}

impl Type {
    pub fn unfreeze_tvars(&self) {
        match self {
            Type::Bottom | Type::Any | Type::Primitive(_) => (),
            Type::Ref { params, .. } => {
                for t in params.iter() {
                    t.unfreeze_tvars();
                }
            }
            Type::Error(t) => t.unfreeze_tvars(),
            Type::Array(t) => t.unfreeze_tvars(),
            Type::Map { key, value } => {
                key.unfreeze_tvars();
                value.unfreeze_tvars();
            }
            Type::ByRef(t) => t.unfreeze_tvars(),
            Type::Tuple(ts) => {
                for t in ts.iter() {
                    t.unfreeze_tvars()
                }
            }
            Type::Struct(ts) => {
                for (_, t) in ts.iter() {
                    t.unfreeze_tvars()
                }
            }
            Type::Variant(_, ts) => {
                for t in ts.iter() {
                    t.unfreeze_tvars()
                }
            }
            Type::TVar(tv) => tv.write().frozen = false,
            Type::Fn(ft) => ft.unfreeze_tvars(),
            Type::Set(s) => {
                for typ in s.iter() {
                    typ.unfreeze_tvars()
                }
            }
            Type::Abstract { id: _, params } => {
                for typ in params.iter() {
                    typ.unfreeze_tvars()
                }
            }
        }
    }

    /// alias type variables with the same name to each other
    pub fn alias_tvars(&self, known: &mut FxHashMap<ArcStr, TVar>) {
        match self {
            Type::Bottom | Type::Any | Type::Primitive(_) => (),
            Type::Ref { params, .. } => {
                for t in params.iter() {
                    t.alias_tvars(known);
                }
            }
            Type::Error(t) => t.alias_tvars(known),
            Type::Array(t) => t.alias_tvars(known),
            Type::Map { key, value } => {
                key.alias_tvars(known);
                value.alias_tvars(known);
            }
            Type::ByRef(t) => t.alias_tvars(known),
            Type::Tuple(ts) => {
                for t in ts.iter() {
                    t.alias_tvars(known)
                }
            }
            Type::Struct(ts) => {
                for (_, t) in ts.iter() {
                    t.alias_tvars(known)
                }
            }
            Type::Variant(_, ts) => {
                for t in ts.iter() {
                    t.alias_tvars(known)
                }
            }
            Type::TVar(tv) => match known.entry(tv.name.clone()) {
                Entry::Occupied(e) => {
                    let v = e.get();
                    tv.alias(v);
                }
                Entry::Vacant(e) => {
                    e.insert(tv.clone());
                    ()
                }
            },
            Type::Fn(ft) => ft.alias_tvars(known),
            Type::Set(s) => {
                for typ in s.iter() {
                    typ.alias_tvars(known)
                }
            }
            Type::Abstract { id: _, params } => {
                for typ in params.iter() {
                    typ.alias_tvars(known)
                }
            }
        }
    }

    pub fn collect_tvars(&self, known: &mut FxHashMap<ArcStr, TVar>) {
        match self {
            Type::Bottom | Type::Any | Type::Primitive(_) => (),
            Type::Ref { params, .. } => {
                for t in params.iter() {
                    t.collect_tvars(known);
                }
            }
            Type::Error(t) => t.collect_tvars(known),
            Type::Array(t) => t.collect_tvars(known),
            Type::Map { key, value } => {
                key.collect_tvars(known);
                value.collect_tvars(known);
            }
            Type::ByRef(t) => t.collect_tvars(known),
            Type::Tuple(ts) => {
                for t in ts.iter() {
                    t.collect_tvars(known)
                }
            }
            Type::Struct(ts) => {
                for (_, t) in ts.iter() {
                    t.collect_tvars(known)
                }
            }
            Type::Variant(_, ts) => {
                for t in ts.iter() {
                    t.collect_tvars(known)
                }
            }
            Type::TVar(tv) => match known.entry(tv.name.clone()) {
                Entry::Occupied(_) => (),
                Entry::Vacant(e) => {
                    e.insert(tv.clone());
                    ()
                }
            },
            Type::Fn(ft) => ft.collect_tvars(known),
            Type::Set(s) => {
                for typ in s.iter() {
                    typ.collect_tvars(known)
                }
            }
            Type::Abstract { id: _, params } => {
                for typ in params.iter() {
                    typ.collect_tvars(known)
                }
            }
        }
    }

    pub fn check_tvars_declared(&self, declared: &FxHashSet<ArcStr>) -> Result<()> {
        match self {
            Type::Bottom | Type::Any | Type::Primitive(_) => Ok(()),
            Type::Ref { params, .. } => {
                params.iter().try_for_each(|t| t.check_tvars_declared(declared))
            }
            Type::Error(t) => t.check_tvars_declared(declared),
            Type::Array(t) => t.check_tvars_declared(declared),
            Type::Map { key, value } => {
                key.check_tvars_declared(declared)?;
                value.check_tvars_declared(declared)
            }
            Type::ByRef(t) => t.check_tvars_declared(declared),
            Type::Tuple(ts) => {
                ts.iter().try_for_each(|t| t.check_tvars_declared(declared))
            }
            Type::Struct(ts) => {
                ts.iter().try_for_each(|(_, t)| t.check_tvars_declared(declared))
            }
            Type::Variant(_, ts) => {
                ts.iter().try_for_each(|t| t.check_tvars_declared(declared))
            }
            Type::TVar(tv) => {
                if !declared.contains(&tv.name) {
                    bail!("undeclared type variable '{}'", tv.name)
                } else {
                    Ok(())
                }
            }
            Type::Set(s) => s.iter().try_for_each(|t| t.check_tvars_declared(declared)),
            Type::Abstract { id: _, params } => {
                params.iter().try_for_each(|t| t.check_tvars_declared(declared))
            }
            Type::Fn(_) => Ok(()),
        }
    }

    pub fn has_unbound(&self) -> bool {
        match self {
            Type::Bottom | Type::Any | Type::Primitive(_) => false,
            Type::Ref { .. } => false,
            Type::Error(e) => e.has_unbound(),
            Type::Array(t0) => t0.has_unbound(),
            Type::Map { key, value } => key.has_unbound() || value.has_unbound(),
            Type::ByRef(t0) => t0.has_unbound(),
            Type::Tuple(ts) => ts.iter().any(|t| t.has_unbound()),
            Type::Struct(ts) => ts.iter().any(|(_, t)| t.has_unbound()),
            Type::Variant(_, ts) => ts.iter().any(|t| t.has_unbound()),
            Type::TVar(tv) => tv.read().typ.read().is_some(),
            Type::Set(s) => s.iter().any(|t| t.has_unbound()),
            Type::Abstract { id: _, params } => params.iter().any(|t| t.has_unbound()),
            Type::Fn(ft) => ft.has_unbound(),
        }
    }

    /// bind all unbound type variables to the specified type
    pub fn bind_as(&self, t: &Self) {
        match self {
            Type::Bottom | Type::Any | Type::Primitive(_) => (),
            Type::Ref { .. } => (),
            Type::Error(t0) => t0.bind_as(t),
            Type::Array(t0) => t0.bind_as(t),
            Type::Map { key, value } => {
                key.bind_as(t);
                value.bind_as(t);
            }
            Type::ByRef(t0) => t0.bind_as(t),
            Type::Tuple(ts) => {
                for elt in ts.iter() {
                    elt.bind_as(t)
                }
            }
            Type::Struct(ts) => {
                for (_, elt) in ts.iter() {
                    elt.bind_as(t)
                }
            }
            Type::Variant(_, ts) => {
                for elt in ts.iter() {
                    elt.bind_as(t)
                }
            }
            Type::TVar(tv) => {
                let tv = tv.read();
                let mut tv = tv.typ.write();
                if tv.is_none() {
                    *tv = Some(t.clone());
                }
            }
            Type::Set(s) => {
                for elt in s.iter() {
                    elt.bind_as(t)
                }
            }
            Type::Fn(ft) => ft.bind_as(t),
            Type::Abstract { id: _, params } => {
                for typ in params.iter() {
                    typ.bind_as(t)
                }
            }
        }
    }

    /// return a copy of self with all type variables unbound and
    /// unaliased. self will not be modified
    pub fn reset_tvars(&self) -> Type {
        match self {
            Type::Bottom => Type::Bottom,
            Type::Any => Type::Any,
            Type::Primitive(p) => Type::Primitive(*p),
            Type::Ref { scope, name, params } => Type::Ref {
                scope: scope.clone(),
                name: name.clone(),
                params: Arc::from_iter(params.iter().map(|t| t.reset_tvars())),
            },
            Type::Error(t0) => Type::Error(Arc::new(t0.reset_tvars())),
            Type::Array(t0) => Type::Array(Arc::new(t0.reset_tvars())),
            Type::Map { key, value } => {
                let key = Arc::new(key.reset_tvars());
                let value = Arc::new(value.reset_tvars());
                Type::Map { key, value }
            }
            Type::ByRef(t0) => Type::ByRef(Arc::new(t0.reset_tvars())),
            Type::Tuple(ts) => {
                Type::Tuple(Arc::from_iter(ts.iter().map(|t| t.reset_tvars())))
            }
            Type::Struct(ts) => Type::Struct(Arc::from_iter(
                ts.iter().map(|(n, t)| (n.clone(), t.reset_tvars())),
            )),
            Type::Variant(tag, ts) => Type::Variant(
                tag.clone(),
                Arc::from_iter(ts.iter().map(|t| t.reset_tvars())),
            ),
            Type::TVar(tv) => Type::TVar(TVar::empty_named(tv.name.clone())),
            Type::Set(s) => Type::Set(Arc::from_iter(s.iter().map(|t| t.reset_tvars()))),
            Type::Fn(fntyp) => Type::Fn(Arc::new(fntyp.reset_tvars())),
            Type::Abstract { id, params } => Type::Abstract {
                id: *id,
                params: Arc::from_iter(params.iter().map(|t| t.reset_tvars())),
            },
        }
    }

    /// return a copy of self with every TVar named in known replaced
    /// with the corresponding type
    pub fn replace_tvars(&self, known: &FxHashMap<ArcStr, Self>) -> Type {
        match self {
            Type::TVar(tv) => match known.get(&tv.name) {
                Some(t) => t.clone(),
                None => Type::TVar(tv.clone()),
            },
            Type::Bottom => Type::Bottom,
            Type::Any => Type::Any,
            Type::Primitive(p) => Type::Primitive(*p),
            Type::Ref { scope, name, params } => Type::Ref {
                scope: scope.clone(),
                name: name.clone(),
                params: Arc::from_iter(params.iter().map(|t| t.replace_tvars(known))),
            },
            Type::Error(t0) => Type::Error(Arc::new(t0.replace_tvars(known))),
            Type::Array(t0) => Type::Array(Arc::new(t0.replace_tvars(known))),
            Type::Map { key, value } => {
                let key = Arc::new(key.replace_tvars(known));
                let value = Arc::new(value.replace_tvars(known));
                Type::Map { key, value }
            }
            Type::ByRef(t0) => Type::ByRef(Arc::new(t0.replace_tvars(known))),
            Type::Tuple(ts) => {
                Type::Tuple(Arc::from_iter(ts.iter().map(|t| t.replace_tvars(known))))
            }
            Type::Struct(ts) => Type::Struct(Arc::from_iter(
                ts.iter().map(|(n, t)| (n.clone(), t.replace_tvars(known))),
            )),
            Type::Variant(tag, ts) => Type::Variant(
                tag.clone(),
                Arc::from_iter(ts.iter().map(|t| t.replace_tvars(known))),
            ),
            Type::Set(s) => {
                Type::Set(Arc::from_iter(s.iter().map(|t| t.replace_tvars(known))))
            }
            Type::Fn(fntyp) => Type::Fn(Arc::new(fntyp.replace_tvars(known))),
            Type::Abstract { id, params } => Type::Abstract {
                id: *id,
                params: Arc::from_iter(params.iter().map(|t| t.replace_tvars(known))),
            },
        }
    }

    /// Unbind any bound tvars, but do not unalias them.
    pub(crate) fn unbind_tvars(&self) {
        match self {
            Type::Bottom | Type::Any | Type::Primitive(_) | Type::Ref { .. } => (),
            Type::Error(t0) => t0.unbind_tvars(),
            Type::Array(t0) => t0.unbind_tvars(),
            Type::Map { key, value } => {
                key.unbind_tvars();
                value.unbind_tvars();
            }
            Type::ByRef(t0) => t0.unbind_tvars(),
            Type::Tuple(ts)
            | Type::Variant(_, ts)
            | Type::Set(ts)
            | Type::Abstract { id: _, params: ts } => {
                for t in ts.iter() {
                    t.unbind_tvars()
                }
            }
            Type::Struct(ts) => {
                for (_, t) in ts.iter() {
                    t.unbind_tvars()
                }
            }
            Type::TVar(tv) => tv.unbind(),
            Type::Fn(fntyp) => fntyp.unbind_tvars(),
        }
    }
}
