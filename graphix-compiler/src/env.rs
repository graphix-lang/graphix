use crate::{
    expr::{ModPath, Origin, Sandbox},
    ide::{
        Ide, ModuleInternalView, ModuleRefSite, ReferenceSite, ScopeMapEntry,
        SigImplLink, TypeRefSite,
    },
    typ::{TVar, Type},
    BindId, Scope,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{anyhow, bail, Result};
use arcstr::ArcStr;
use combine::stream::position::SourcePosition;
use compact_str::CompactString;
use immutable_chunkmap::{map::MapS as Map, set::SetS as Set};
use netidx::path::Path;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use std::{fmt, iter, mem, ops::Bound};
use triomphe::Arc;

pub struct Bind {
    pub id: BindId,
    pub export: bool,
    pub typ: Type,
    pub doc: Option<ArcStr>,
    pub scope: ModPath,
    pub name: CompactString,
    /// Source position where the binding was introduced. Used by IDE
    /// tooling for go-to-definition; not consulted by the compiler.
    pub pos: SourcePosition,
    /// Source origin (file/buffer) where the binding was introduced.
    pub ori: Arc<Origin>,
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
            pos: self.pos,
            ori: self.ori.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub params: Arc<[(TVar, Option<Type>)]>,
    pub typ: Type,
    pub doc: Option<ArcStr>,
    /// Source position where this typedef was declared. Used by IDE
    /// tooling for go-to-definition; the compiler doesn't read it.
    pub pos: SourcePosition,
    pub ori: Arc<Origin>,
}

#[derive(Clone, Debug, Default)]
pub struct Env {
    pub by_id: Map<BindId, Bind>,
    pub byref_chain: Map<BindId, BindId>,
    pub binds: Map<ModPath, Map<CompactString, BindId>>,
    pub used: Map<ModPath, Arc<Vec<ModPath>>>,
    pub modules: Set<ModPath>,
    pub typedefs: Map<ModPath, Map<CompactString, TypeDef>>,
    pub catch: Map<ModPath, BindId>,
    /// Append-only mirror of every `(scope, name) → BindId` ever
    /// created via `bind_variable`. Used by IDE tooling for cursor
    /// → scope completion: it exposes lambda parameters and other
    /// short-lived bindings that `binds` drops at scope teardown
    /// and `unbind_variable` removes from `by_id`. Not consulted by
    /// the compiler. Only populated when `lsp_mode` is set.
    pub ide_binds: Map<ModPath, Map<CompactString, Bind>>,
    /// True iff the compiler should populate IDE side-channels
    /// (`ide_binds`, the `ide` sink, etc.). Toggled by the LSP
    /// runtime; normal compiles leave it unset and pay no IDE cost.
    pub lsp_mode: bool,
    /// Every IDE side-channel ([`Ide`]): name/module/type references,
    /// the scope map, sig→impl links, and per-module env snapshots.
    /// `Some(_)` only when running under an LSP-style check; clones
    /// share the inner `Arc<Mutex>` so reentrant or concurrent compiles
    /// within a single check all drain into the same buffer. The runtime
    /// swaps this in/out at each check boundary. Sites that hold `&mut
    /// ExecCtx` push the first three tables via [`Env::push_reference`] /
    /// [`Env::push_module_reference`] / [`Env::push_scope_map_entry`];
    /// sites that hold only `&Env` push the rest via [`Env::push_type_ref`]
    /// / [`Env::push_sig_link`] / [`Env::push_module_internal_view`].
    ///
    /// Named `ide` rather than `lsp` because the sink is general IDE
    /// tooling state, not specific to the language server — other
    /// consumers (e.g. atlas) may read it too.
    pub ide: Option<Arc<Mutex<Ide>>>,
}

impl Env {
    pub(super) fn clear(&mut self) {
        let Self {
            by_id,
            binds,
            byref_chain,
            used,
            modules,
            typedefs,
            catch,
            ide_binds,
            lsp_mode: _,
            ide: _,
        } = self;
        *by_id = Map::new();
        *binds = Map::new();
        *byref_chain = Map::new();
        *used = Map::new();
        *modules = Set::new();
        *typedefs = Map::new();
        *catch = Map::new();
        *ide_binds = Map::new();
    }

    // restore the lexical environment to the state it was in at the
    // snapshot `other`, but leave the bind and type environment
    // alone. `ide_binds` is preserved across restoration so IDE
    // tooling sees lambda parameters / let bindings that were
    // introduced inside the restored region. The `ide` sink is
    // preserved on `self` so any pushes that happened inside the
    // restored region accumulate alongside the rest of the check.
    pub(super) fn restore_lexical_env(&self, other: Self) -> Self {
        Self {
            binds: other.binds,
            used: other.used,
            modules: other.modules,
            typedefs: other.typedefs,
            by_id: self.by_id.clone(),
            catch: self.catch.clone(),
            byref_chain: self.byref_chain.clone(),
            ide_binds: self.ide_binds.clone(),
            lsp_mode: self.lsp_mode,
            ide: self.ide.clone(),
        }
    }

    pub(super) fn restore_lexical_env_mut(&self, other: &mut Self) -> Self {
        Self {
            binds: mem::take(&mut other.binds),
            used: mem::take(&mut other.used),
            modules: mem::take(&mut other.modules),
            typedefs: mem::take(&mut other.typedefs),
            by_id: self.by_id.clone(),
            catch: self.catch.clone(),
            ide_binds: self.ide_binds.clone(),
            byref_chain: self.byref_chain.clone(),
            lsp_mode: self.lsp_mode,
            ide: self.ide.clone(),
        }
    }

    /// Push a `ReferenceSite` into the active IDE sink, if any. No-op
    /// when `self.ide` is `None` (every non-LSP compile).
    pub fn push_reference(&self, site: ReferenceSite) {
        if let Some(ide) = &self.ide {
            ide.lock().references.push(site);
        }
    }

    /// Push a `ModuleRefSite` into the active IDE sink, if any.
    pub fn push_module_reference(&self, site: ModuleRefSite) {
        if let Some(ide) = &self.ide {
            ide.lock().module_references.push(site);
        }
    }

    /// Push a `ScopeMapEntry` into the active IDE sink, if any.
    pub fn push_scope_map_entry(&self, entry: ScopeMapEntry) {
        if let Some(ide) = &self.ide {
            ide.lock().scope_map.push(entry);
        }
    }

    /// Push a `TypeRefSite` into the active IDE sink, if any. No-op
    /// when `self.ide` is `None` (every non-LSP compile).
    pub fn push_type_ref(&self, site: TypeRefSite) {
        if let Some(ide) = &self.ide {
            ide.lock().type_refs.push(site);
        }
    }

    /// Push a `SigImplLink` into the active IDE sink, if any.
    pub fn push_sig_link(&self, link: SigImplLink) {
        if let Some(ide) = &self.ide {
            ide.lock().sig_links.push(link);
        }
    }

    /// Push a per-module internal-view snapshot into the active IDE
    /// sink, if any.
    pub fn push_module_internal_view(&self, view: ModuleInternalView) {
        if let Some(ide) = &self.ide {
            ide.lock().module_internals.push(view);
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
                let mut modules = AHashSet::default();
                let mut names: AHashMap<_, AHashSet<_>> = AHashMap::default();
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

    /// lookup the bind id of the nearest catch handler in this scope
    pub fn lookup_catch(&self, scope: &ModPath) -> Result<BindId> {
        match Path::dirnames(&scope.0).rev().find_map(|scope| self.catch.get(scope)) {
            Some(id) => Ok(*id),
            None => bail!("there is no catch visible in {scope}"),
        }
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
        doc: Option<ArcStr>,
        pos: SourcePosition,
        ori: Arc<Origin>,
    ) -> Result<()> {
        if self.typedefs.get(scope).and_then(|m| m.get(name)).is_some() {
            bail!("{name} is already defined in scope {scope}")
        }
        let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        let mut declared: LPooled<AHashSet<ArcStr>> = LPooled::take();
        for (tv, tc) in params.iter() {
            Type::TVar(tv.clone()).alias_tvars(&mut known);
            if let Some(tc) = tc {
                tc.alias_tvars(&mut known);
            }
        }
        typ.alias_tvars(&mut known);
        for (tv, _) in params.iter() {
            if !declared.insert(tv.name.clone()) {
                bail!("duplicate type variable {tv} in definition of {name}");
            }
        }
        for (_, t) in params.iter() {
            if let Some(t) = t {
                t.check_tvars_declared(&mut declared)?;
            }
        }
        for dec in declared.iter() {
            if !known.contains_key(dec) {
                bail!("unused type parameter {dec} in definition of {name}")
            }
        }
        if self.lsp_mode {
            // Capture every type-name occurrence inside the typedef
            // body for IDE find-references. This catches uses that
            // never go through `Type::lookup_ref` directly (e.g.
            // `Foo` inside `type Pair = (Foo, Foo)` — typedef bodies
            // are stored, not type-checked against anything). Done
            // before we mutably borrow `self.typedefs` below.
            typ.record_ide_refs(self, scope);
        }
        let defs = self.typedefs.get_or_default_cow(scope.clone());
        defs.insert_cow(name.into(), TypeDef { params, typ, doc, pos, ori });
        Ok(())
    }

    pub fn undeftype(&mut self, scope: &ModPath, name: &str) {
        if let Some(defs) = self.typedefs.get_mut_cow(scope) {
            defs.remove_cow(&CompactString::from(name));
            if defs.len() == 0 {
                self.typedefs.remove_cow(scope);
            }
        }
    }

    /// Drop everything registered at `scope` or any descendant. Used by
    /// the LSP when re-typechecking a stdlib (or third-party graphix)
    /// package crate's own source: the runtime's env was pre-loaded
    /// with that package at startup, but the live edits need to
    /// register fresh under the same scope. Without scrubbing first,
    /// re-registration trips the duplicate-module / duplicate-type
    /// guards.
    ///
    /// Returns the number of (scope, name) entries removed across binds
    /// and typedefs.
    pub fn unbind_scope_subtree(&mut self, scope: &ModPath) -> usize {
        fn is_under(s: &ModPath, prefix: &ModPath) -> bool {
            // Both come from netidx Path. A scope is under `prefix`
            // if it equals prefix or starts with `prefix + "/"`.
            let s: &str = s;
            let p: &str = prefix;
            if s == p {
                return true;
            }
            if !s.starts_with(p) {
                return false;
            }
            // Avoid matching e.g. `/tu` as a prefix of `/tui`.
            s.as_bytes().get(p.len()).copied() == Some(b'/')
        }
        let mut removed = 0;
        let bind_scopes: LPooled<Vec<ModPath>> = (&self.binds)
            .into_iter()
            .filter(|(s, _)| is_under(s, scope))
            .map(|(s, _)| s.clone())
            .collect();
        for s in &*bind_scopes {
            if let Some(defs) = self.binds.get(s) {
                let ids: Vec<BindId> = defs.into_iter().map(|(_, id)| *id).collect();
                removed += ids.len();
                for id in &ids {
                    self.by_id.remove_cow(id);
                }
            }
            self.binds.remove_cow(s);
            self.ide_binds.remove_cow(s);
        }
        let type_scopes: LPooled<Vec<ModPath>> = (&self.typedefs)
            .into_iter()
            .filter(|(s, _)| is_under(s, scope))
            .map(|(s, _)| s.clone())
            .collect();
        for s in &*type_scopes {
            if let Some(defs) = self.typedefs.get(s) {
                removed += defs.len();
            }
            self.typedefs.remove_cow(s);
        }
        let used_scopes: LPooled<Vec<ModPath>> = (&self.used)
            .into_iter()
            .filter(|(s, _)| is_under(s, scope))
            .map(|(s, _)| s.clone())
            .collect();
        for s in &*used_scopes {
            self.used.remove_cow(s);
        }
        let mod_scopes: LPooled<Vec<ModPath>> =
            (&self.modules).into_iter().filter(|s| is_under(s, scope)).cloned().collect();
        for s in &*mod_scopes {
            self.modules.remove_cow(s);
        }
        let catch_scopes: LPooled<Vec<ModPath>> = (&self.catch)
            .into_iter()
            .filter(|(s, _)| is_under(s, scope))
            .map(|(s, _)| s.clone())
            .collect();
        for s in &*catch_scopes {
            self.catch.remove_cow(s);
        }
        removed
    }

    /// create a new binding. If an existing bind exists in the same
    /// scope shadow it.
    pub fn bind_variable(
        &mut self,
        scope: &ModPath,
        name: &str,
        typ: Type,
        pos: SourcePosition,
        ori: Arc<Origin>,
    ) -> &mut Bind {
        let binds = self.binds.get_or_default_cow(scope.clone());
        let mut existing = true;
        let id = binds.get_or_insert_cow(CompactString::from(name), || {
            existing = false;
            BindId::new()
        });
        if existing {
            *id = BindId::new();
        }
        let bind = self.by_id.get_or_insert_cow(*id, || Bind {
            export: true,
            id: *id,
            scope: scope.clone(),
            doc: None,
            name: CompactString::from(name),
            typ,
            pos,
            ori,
        });
        if self.lsp_mode {
            let ide_clone = bind.clone();
            let ide_defs = self.ide_binds.get_or_default_cow(scope.clone());
            ide_defs.insert_cow(CompactString::from(name), ide_clone);
        }
        self.by_id.get_mut_cow(id).unwrap()
    }

    /// make the specified name an alias for `id`
    pub fn alias_variable(&mut self, scope: &ModPath, name: &str, id: BindId) {
        let binds = self.binds.get_or_default_cow(scope.clone());
        binds.insert_cow(CompactString::from(name), id);
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

    pub fn use_in_scope(&mut self, scope: &Scope, name: &ModPath) -> Result<()> {
        match self.canonical_modpath(&scope.lexical, name) {
            None => bail!("use {name}: no such module {name} in scope {}", scope.lexical),
            Some(_) => {
                let used = self.used.get_or_default_cow(scope.lexical.clone());
                Ok(Arc::make_mut(used).push(name.clone()))
            }
        }
    }

    pub fn stop_use_in_scope(&mut self, scope: &Scope, name: &ModPath) {
        if let Some(used) = self.used.get_mut_cow(&scope.lexical) {
            Arc::make_mut(used).retain(|n| n != name);
            if used.is_empty() {
                self.used.remove_cow(&scope.lexical);
            }
        }
    }
}
