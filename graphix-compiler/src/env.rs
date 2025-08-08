use crate::{
    expr::{Arg, ModPath, Sandbox},
    typ::{FnType, TVar, Type},
    BindId, InitFn, LambdaId, Rt, UserEvent,
};
use anyhow::{anyhow, bail, Result};
use arcstr::ArcStr;
use compact_str::CompactString;
use fxhash::{FxHashMap, FxHashSet};
use immutable_chunkmap::{map::MapS as Map, set::SetS as Set};
use netidx::path::Path;
use std::{cell::RefCell, fmt, iter, ops::Bound, sync::Weak};
use triomphe::Arc;

pub struct LambdaDef<R: Rt, E: UserEvent> {
    pub id: LambdaId,
    pub env: Env<R, E>,
    pub scope: ModPath,
    pub argspec: Arc<[Arg]>,
    pub typ: Arc<FnType>,
    pub init: InitFn<R, E>,
}

impl<R: Rt, E: UserEvent> fmt::Debug for LambdaDef<R, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "LambdaDef({:?})", self.id)
    }
}

pub struct Bind {
    pub id: BindId,
    pub export: bool,
    pub typ: Type,
    pub doc: Option<ArcStr>,
    pub scope: ModPath,
    pub name: CompactString,
}

impl fmt::Debug for Bind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Bind {{ id: {:?}, export: {} }}", self.id, self.export,)
    }
}

impl Clone for Bind {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            scope: self.scope.clone(),
            name: self.name.clone(),
            doc: self.doc.clone(),
            export: self.export,
            typ: self.typ.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub params: Arc<[(TVar, Option<Type>)]>,
    pub typ: Type,
}

#[derive(Debug)]
pub struct Env<R: Rt, E: UserEvent> {
    pub by_id: Map<BindId, Bind>,
    pub lambdas: Map<LambdaId, Weak<LambdaDef<R, E>>>,
    pub byref_chain: Map<BindId, BindId>,
    pub binds: Map<ModPath, Map<CompactString, BindId>>,
    pub used: Map<ModPath, Arc<Vec<ModPath>>>,
    pub modules: Set<ModPath>,
    pub typedefs: Map<ModPath, Map<CompactString, TypeDef>>,
}

impl<R: Rt, E: UserEvent> Clone for Env<R, E> {
    fn clone(&self) -> Self {
        Self {
            by_id: self.by_id.clone(),
            binds: self.binds.clone(),
            byref_chain: self.byref_chain.clone(),
            used: self.used.clone(),
            modules: self.modules.clone(),
            typedefs: self.typedefs.clone(),
            lambdas: self.lambdas.clone(),
        }
    }
}

impl<R: Rt, E: UserEvent> Env<R, E> {
    pub(super) fn new() -> Self {
        Self {
            by_id: Map::new(),
            binds: Map::new(),
            byref_chain: Map::new(),
            used: Map::new(),
            modules: Set::new(),
            typedefs: Map::new(),
            lambdas: Map::new(),
        }
    }

    pub(super) fn clear(&mut self) {
        let Self { by_id, binds, byref_chain, used, modules, typedefs, lambdas } = self;
        *by_id = Map::new();
        *binds = Map::new();
        *byref_chain = Map::new();
        *used = Map::new();
        *modules = Set::new();
        *typedefs = Map::new();
        *lambdas = Map::new();
    }

    // restore the lexical environment to the state it was in at the
    // snapshot `other`, but leave the bind and type environment
    // alone.
    pub(super) fn restore_lexical_env(&self, other: Self) -> Self {
        Self {
            binds: other.binds,
            used: other.used,
            modules: other.modules,
            typedefs: other.typedefs,
            by_id: self.by_id.clone(),
            lambdas: self.lambdas.clone(),
            byref_chain: self.byref_chain.clone(),
        }
    }

    pub fn apply_sandbox(&self, spec: &Sandbox) -> Result<Self> {
        fn get_bind_name(n: &ModPath) -> Result<(&str, &str)> {
            let dir = Path::dirname(&**n).ok_or_else(|| anyhow!("unknown module {n}"))?;
            let k = Path::basename(&**n).ok_or_else(|| anyhow!("unknown module {n}"))?;
            Ok((dir, k))
        }
        match spec {
            Sandbox::Unrestricted => Ok(self.clone()),
            Sandbox::Blacklist(bl) => {
                let mut t = self.clone();
                for n in bl.iter() {
                    if t.modules.remove_cow(n) {
                        t.binds.remove_cow(n);
                        t.typedefs.remove_cow(n);
                    } else {
                        let (dir, k) = get_bind_name(n)?;
                        let vals = t.binds.get_mut_cow(dir).ok_or_else(|| {
                            anyhow!("no value {k} in module {dir} and no module {n}")
                        })?;
                        if let None = vals.remove_cow(&CompactString::from(k)) {
                            bail!("no value {k} in module {dir} and no module {n}")
                        }
                    }
                }
                Ok(t)
            }
            Sandbox::Whitelist(wl) => {
                let mut t = self.clone();
                let mut modules = FxHashSet::default();
                let mut names: FxHashMap<_, FxHashSet<_>> = FxHashMap::default();
                for w in wl.iter() {
                    if t.modules.contains(w) {
                        modules.insert(w.clone());
                    } else {
                        let (dir, n) = get_bind_name(w)?;
                        let dir = ModPath(Path::from(ArcStr::from(dir)));
                        let n = CompactString::from(n);
                        t.binds.get(&dir).and_then(|v| v.get(&n)).ok_or_else(|| {
                            anyhow!("no value {n} in module {dir} and no module {w}")
                        })?;
                        names.entry(dir).or_default().insert(n);
                    }
                }
                t.typedefs = t.typedefs.update_many(
                    t.typedefs.into_iter().map(|(k, v)| (k.clone(), v.clone())),
                    |k, v, _| {
                        if modules.contains(&k) || names.contains_key(&k) {
                            Some((k, v))
                        } else {
                            None
                        }
                    },
                );
                t.modules =
                    t.modules.update_many(t.modules.into_iter().cloned(), |k, _| {
                        if modules.contains(&k) || names.contains_key(&k) {
                            Some(k)
                        } else {
                            None
                        }
                    });
                t.binds = t.binds.update_many(
                    t.binds.into_iter().map(|(k, v)| (k.clone(), v.clone())),
                    |k, v, _| {
                        if modules.contains(&k) {
                            Some((k, v))
                        } else if let Some(names) = names.get(&k) {
                            let v = v.update_many(
                                v.into_iter().map(|(k, v)| (k.clone(), v.clone())),
                                |kn, vn, _| {
                                    if names.contains(&kn) {
                                        Some((kn, vn))
                                    } else {
                                        None
                                    }
                                },
                            );
                            Some((k, v))
                        } else {
                            None
                        }
                    },
                );
                Ok(t)
            }
        }
    }

    pub fn find_visible<T, F: FnMut(&str, &str) -> Option<T>>(
        &self,
        scope: &ModPath,
        name: &ModPath,
        mut f: F,
    ) -> Option<T> {
        let mut buf = CompactString::from("");
        let name_scope = Path::dirname(&**name);
        let name = Path::basename(&**name).unwrap_or("");
        for scope in Path::dirnames(&**scope).rev() {
            let used = self.used.get(scope);
            let used = iter::once(scope)
                .chain(used.iter().flat_map(|s| s.iter().map(|p| &***p)));
            for scope in used {
                let scope = name_scope
                    .map(|ns| {
                        buf.clear();
                        buf.push_str(scope);
                        if let Some(Path::SEP) = buf.chars().next_back() {
                            buf.pop();
                        }
                        buf.push_str(ns);
                        buf.as_str()
                    })
                    .unwrap_or(scope);
                if let Some(res) = f(scope, name) {
                    return Some(res);
                }
            }
        }
        None
    }

    pub fn lookup_bind(
        &self,
        scope: &ModPath,
        name: &ModPath,
    ) -> Option<(&ModPath, &Bind)> {
        self.find_visible(scope, name, |scope, name| {
            self.binds.get_full(scope).and_then(|(scope, vars)| {
                vars.get(name)
                    .and_then(|bid| self.by_id.get(bid).map(|bind| (scope, bind)))
            })
        })
    }

    pub fn lookup_typedef(&self, scope: &ModPath, name: &ModPath) -> Option<&TypeDef> {
        self.find_visible(scope, name, |scope, name| {
            self.typedefs.get(scope).and_then(|m| m.get(name))
        })
    }

    /// lookup binds in scope that match the specified partial
    /// name. This is intended to be used for IDEs and interactive
    /// shells, and is not used by the compiler.
    pub fn lookup_matching(
        &self,
        scope: &ModPath,
        part: &ModPath,
    ) -> Vec<(CompactString, BindId)> {
        let mut res = vec![];
        self.find_visible(scope, part, |scope, part| {
            if let Some(vars) = self.binds.get(scope) {
                let r = vars.range::<str, _>((Bound::Included(part), Bound::Unbounded));
                for (name, bind) in r {
                    if name.starts_with(part) {
                        res.push((name.clone(), *bind));
                    }
                }
            }
            None::<()>
        });
        res
    }

    /// lookup modules in scope that match the specified partial
    /// name. This is intended to be used for IDEs and interactive
    /// shells, and is not used by the compiler.
    pub fn lookup_matching_modules(
        &self,
        scope: &ModPath,
        part: &ModPath,
    ) -> Vec<ModPath> {
        let mut res = vec![];
        self.find_visible(scope, part, |scope, part| {
            let p = ModPath(Path::from(ArcStr::from(scope)).append(part));
            for m in self.modules.range((Bound::Included(p.clone()), Bound::Unbounded)) {
                if m.0.starts_with(&*p.0) {
                    if let Some(m) = m.strip_prefix(scope) {
                        if !m.trim().is_empty() {
                            res.push(ModPath(Path::from(ArcStr::from(m))));
                        }
                    }
                }
            }
            None::<()>
        });
        res
    }

    pub fn canonical_modpath(&self, scope: &ModPath, name: &ModPath) -> Option<ModPath> {
        self.find_visible(scope, name, |scope, name| {
            let p = ModPath(Path::from(ArcStr::from(scope)).append(name));
            if self.modules.contains(&p) {
                Some(p)
            } else {
                None
            }
        })
    }

    pub fn deftype(
        &mut self,
        scope: &ModPath,
        name: &str,
        params: Arc<[(TVar, Option<Type>)]>,
        typ: Type,
    ) -> Result<()> {
        let defs = self.typedefs.get_or_default_cow(scope.clone());
        if defs.get(name).is_some() {
            bail!("{name} is already defined in scope {scope}")
        } else {
            thread_local! {
                static KNOWN: RefCell<FxHashMap<ArcStr, TVar>> = RefCell::new(FxHashMap::default());
                static DECLARED: RefCell<FxHashSet<ArcStr>> = RefCell::new(FxHashSet::default());
            }
            KNOWN.with_borrow_mut(|known| {
                known.clear();
                for (tv, tc) in params.iter() {
                    Type::TVar(tv.clone()).alias_tvars(known);
                    if let Some(tc) = tc {
                        tc.alias_tvars(known);
                    }
                }
                typ.alias_tvars(known);
            });
            DECLARED.with_borrow_mut(|declared| {
                declared.clear();
                for (tv, _) in params.iter() {
                    if !declared.insert(tv.name.clone()) {
                        bail!("duplicate type variable {tv} in definition of {name}");
                    }
                }
                typ.check_tvars_declared(declared)?;
                for (_, t) in params.iter() {
                    if let Some(t) = t {
                        t.check_tvars_declared(declared)?;
                    }
                }
                Ok::<_, anyhow::Error>(())
            })?;
            KNOWN.with_borrow(|known| {
                DECLARED.with_borrow(|declared| {
                    for dec in declared {
                        if !known.contains_key(dec) {
                            bail!("unused type parameter {dec} in definition of {name}")
                        }
                    }
                    Ok(())
                })
            })?;
            defs.insert_cow(name.into(), TypeDef { params, typ });
            Ok(())
        }
    }

    pub fn undeftype(&mut self, scope: &ModPath, name: &str) {
        if let Some(defs) = self.typedefs.get_mut_cow(scope) {
            defs.remove_cow(&CompactString::from(name));
            if defs.len() == 0 {
                self.typedefs.remove_cow(scope);
            }
        }
    }

    // create a new binding. If an existing bind exists in the same
    // scope shadow it.
    pub fn bind_variable(&mut self, scope: &ModPath, name: &str, typ: Type) -> &mut Bind {
        let binds = self.binds.get_or_default_cow(scope.clone());
        let mut existing = true;
        let id = binds.get_or_insert_cow(CompactString::from(name), || {
            existing = false;
            BindId::new()
        });
        if existing {
            *id = BindId::new();
        }
        self.by_id.get_or_insert_cow(*id, || Bind {
            export: true,
            id: *id,
            scope: scope.clone(),
            doc: None,
            name: CompactString::from(name),
            typ,
        })
    }

    pub fn unbind_variable(&mut self, id: BindId) {
        if let Some(b) = self.by_id.remove_cow(&id) {
            if let Some(binds) = self.binds.get_mut_cow(&b.scope) {
                binds.remove_cow(&b.name);
                if binds.len() == 0 {
                    self.binds.remove_cow(&b.scope);
                }
            }
        }
    }
}
