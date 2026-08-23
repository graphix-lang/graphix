use crate::{
    BindId,
    expr::{ExprId, ModPath, Origin, Sandbox, TypeDefBody},
    ide::{
        Ide, ModuleInternalView, ModuleRefSite, ReferenceSite, ScopeMapEntry,
        SigImplLink, TypeRefSite,
    },
    mod_root,
    typ::{AbstractId, FnType, TVar, TraitId, Type},
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use combine::stream::position::SourcePosition;
use compact_str::CompactString;
// SIZE 16, not MapS (=256): the env maps are WRITE-heavy at compile
// time (one insert_cow per bind/typedef while the whole stdlib
// compiles), and a COW insert clones the touched chunk — at 256
// entries of ~100-byte `Bind` that was ~25KB of memcpy plus ~1k
// refcount bumps PER INSERT, ~20% of total compile time in the
// jul22 samply profile; measured knee: 256→74ms/16→57ms per stdlib
// compile, regress suite −10%. Small chunks trade slightly deeper trees
// (reads stay O(log n)) for 16x cheaper writes.
pub type Map<K, V> = immutable_chunkmap::map::Map<K, V, 16>;
pub type Set<K> = immutable_chunkmap::set::Set<K, 16>;
use netidx_core::path::Path;
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
/// The representation of a Graphix-minted abstract type (`type T =
/// Abstract<rep>`), registered globally like [`Env::names`] but
/// consulted only from inside the defining scope — which is what gates
/// `T(v)`, `x.0` and the pattern `T(x)` to where the definition is
/// visible (`design/nominal_abstract_types.md`).
pub struct AbstractRep {
    pub scope: ModPath,
    pub name: ArcStr,
    pub params: Arc<[TVar]>,
    pub rep: Type,
    /// The definition is EXPORTED (an interface's `type T =
    /// Abstract<rep>`, or a module with no interface), so the
    /// constructor is usable from anywhere the type is; otherwise only
    /// from inside `scope`.
    pub public: bool,
}

impl AbstractRep {
    /// A fresh instance of the type: `(T<'a..>, rep['a..])` with the
    /// formals replaced by fresh type variables shared between the two.
    pub fn instantiate(&self, id: AbstractId) -> (Type, Type) {
        let fresh: LPooled<Vec<Type>> =
            self.params.iter().map(|_| Type::empty_tvar()).collect();
        let rep = self.instantiate_with(&fresh);
        (Type::Abstract { id, params: Arc::from_iter(fresh.iter().cloned()) }, rep)
    }

    /// The representation with the formals replaced by `params`.
    pub fn instantiate_with(&self, params: &[Type]) -> Type {
        let known: LPooled<AHashMap<ArcStr, Type>> = self
            .params
            .iter()
            .map(|tv| tv.name.clone())
            .zip(params.iter().cloned())
            .collect();
        self.rep.replace_tvars(&known)
    }
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub params: Arc<[(TVar, Option<Type>)]>,
    pub typ: Type,
    /// For a Graphix-minted abstract type (`type T = Abstract<rep>`),
    /// the representation its constructor wraps — present exactly
    /// where the definition is visible, which is what gates `T(v)`,
    /// `x.0` and the pattern `T(x)` (`design/nominal_abstract_types.md`).
    pub rep: Option<Type>,
    pub doc: Option<ArcStr>,
    /// Source position where this typedef was declared. Used by IDE
    /// tooling for go-to-definition; the compiler doesn't read it.
    pub pos: SourcePosition,
    pub ori: Arc<Origin>,
}

/// One explicit import: the imported name (the map key in
/// [`ScopeNames::imports`], which differs from `name` under `as`)
/// resolves to `name` in the module at `scope`.
#[derive(Debug, Clone)]
pub struct ImportEntry {
    /// canonical scope the item was imported from
    pub scope: ModPath,
    /// the item's own name there
    pub name: CompactString,
    /// the import's anchor is a keyword root (`self`/`super`): the
    /// redirect walks `scope` up to its module root instead of
    /// consulting `scope` alone, because the anchor of a `super`
    /// import may be a block level (a script file's top level) whose
    /// items live across the block chain.
    pub chain: bool,
    /// Position/origin of the `use`, for diagnostics and IDE tooling.
    pub pos: SourcePosition,
    pub ori: Arc<Origin>,
}

/// A scope's explicit namespace: what its `use` declarations
/// imported. Lives in [`Env::names`], keyed by the scope path.
#[derive(Debug, Clone, Default)]
pub struct ScopeNames {
    pub imports: Map<CompactString, ImportEntry>,
    /// Glob (`use m::*`) source modules, in declaration order.
    pub globs: Arc<Vec<ModPath>>,
}

/// A declared trait (`design/traits.md`): its identity, where it was
/// declared, and its methods. Lives in [`Env::trait_defs`], a GLOBAL
/// registry keyed by [`TraitId`] — the interface's declaration and
/// the implementation's re-declaration mint the same id, and the
/// later registration (the implementation's, which carries the
/// default-method bindings) replaces the earlier.
#[derive(Debug, Clone)]
pub struct TraitDef {
    pub id: TraitId,
    pub name: ArcStr,
    /// The declaring module scope.
    pub scope: ModPath,
    /// `scope::Name` — the trait's own module-like scope, where its
    /// method dispatchers are bound (`Trait::method`, `use Trait::*`).
    pub path: ModPath,
    pub methods: Arc<[TraitMethodDef]>,
    pub doc: Option<ArcStr>,
    pub pos: SourcePosition,
    pub ori: Arc<Origin>,
}

#[derive(Debug, Clone)]
pub struct TraitMethodDef {
    pub name: ArcStr,
    /// The declared signature with the receiver `self` constrained by
    /// the trait: `fn<self: Trait>(self, ..) -> T`.
    pub typ: Arc<FnType>,
    /// Index of the `self` parameter in `typ.args`.
    pub self_index: usize,
    /// The declaration supplies a default body (an implementor may
    /// omit the method).
    pub has_default: bool,
    /// The dispatcher binding at `path::name` — what a call names; a
    /// call site resolves it to an implementation by its self
    /// argument's type (`CallSite::resolve_trait_call`).
    pub dispatcher: BindId,
    /// The default body's binding, when the method has one and the
    /// declaring implementation has compiled it.
    pub default: Option<BindId>,
}

/// Which trait method a dispatcher binding stands for
/// ([`Env::trait_methods`]).
#[derive(Debug, Clone, Copy)]
pub struct TraitMethodRef {
    pub trait_id: TraitId,
    pub index: usize,
}

/// One `impl Trait for Target` ([`Env::impls`], global — impls are
/// facts, not names; `design/traits.md` §4).
#[derive(Debug, Clone)]
pub struct ImplDef {
    pub trait_id: TraitId,
    /// The target type, scoped; may mention `params`.
    pub target: Type,
    /// The head's declared type variables (`impl<'a: C> T for P<'a>`);
    /// their bounds live on the cells. Lookup instantiates them fresh
    /// and unifies the head with the use site's type.
    pub params: Arc<[TVar]>,
    /// The scope whose bindings are the implementation's methods.
    pub scope: ModPath,
    /// Method name → the binding a resolved call references.
    pub methods: Map<CompactString, BindId>,
    /// The impl came from an interface declaration (`impl T for X;`):
    /// its method bindings were minted by the signature and the
    /// implementation's methods proxy to them. An implementation's
    /// own registration of the same (trait, target) replaces it.
    pub declared: bool,
    pub pos: SourcePosition,
    pub ori: Arc<Origin>,
}

/// Which namespace a resolution serves. Path INTERIORS are always
/// module-kind; the terminal name's kind decides which preludes
/// apply: values and types get the core prelude, modules get the
/// package prelude (registered package names as path roots) and then
/// the core prelude (core's public submodules).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NameNs {
    Value,
    Type,
    Module,
}

/// True if scope `s` is `prefix` itself or a path descendant of it.
pub(crate) fn scope_is_under(s: &str, prefix: &str) -> bool {
    if prefix == "/" || s == prefix {
        return true;
    }
    if !s.starts_with(prefix) {
        return false;
    }
    // Avoid matching e.g. `/tu` as a prefix of `/tui`.
    s.as_bytes().get(prefix.len()).copied() == Some(b'/')
}

/// Iterate the lexical levels of `from` from innermost to the
/// enclosing module root, inclusive.
pub(crate) fn chain_levels(from: &str) -> impl Iterator<Item = &str> {
    let root = mod_root(from);
    let mut cur = Some(from);
    iter::from_fn(move || {
        let c = cur?;
        cur = if c == root { None } else { Some(Path::dirname(c).unwrap_or("/")) };
        Some(c)
    })
}

const MAX_IMPORT_DEPTH: usize = 32;

#[derive(Clone, Debug, Default)]
pub struct Env {
    pub by_id: Map<BindId, Bind>,
    pub byref_chain: Map<BindId, BindId>,
    pub binds: Map<ModPath, Map<CompactString, BindId>>,
    pub modules: Set<ModPath>,
    pub typedefs: Map<ModPath, Map<CompactString, TypeDef>>,
    /// Every scope's explicit namespace (imports + globs), keyed by
    /// the scope path. NOT part of the lexical env: scope paths are
    /// globally unique, so this is a per-context registry of every
    /// module and block's import table — it survives the module
    /// privacy swap (`restore_lexical_env` keeps it from `self`),
    /// which is what lets deferred resolution consult the DEFINING
    /// module's table long after that module finished compiling.
    pub names: Map<ModPath, ScopeNames>,
    /// Every Graphix-minted abstract type's representation, keyed by
    /// its identity — a global registry like `names` (see
    /// [`AbstractRep`]); visibility is decided at lookup.
    pub abstract_reps: Map<AbstractId, Arc<AbstractRep>>,
    /// Trait NAMES by declaring scope — lexical, like `typedefs`
    /// (a trait is in scope by declaration, `use`, or prelude).
    pub traits: Map<ModPath, Map<CompactString, TraitId>>,
    /// Every trait's definition by identity — a global registry like
    /// `names` (see [`TraitDef`]).
    pub trait_defs: Map<TraitId, Arc<TraitDef>>,
    /// Dispatcher binding → the trait method it names, for every
    /// registration (an interface's and its implementation's
    /// dispatchers both map here). Global.
    pub trait_methods: Map<BindId, TraitMethodRef>,
    /// Every trait's implementations. Global: an impl applies
    /// wherever its trait is used, scope governs only the trait's
    /// NAME (`design/traits.md` §4).
    pub impls: Map<TraitId, Arc<Vec<Arc<ImplDef>>>>,
    /// GENERALIZED bindings — let-bound lambdas, interface `val`s and
    /// trait dispatchers — whose signature a VALUE occurrence
    /// instantiates afresh, exactly as a call site does
    /// (`Ref::typecheck0`). A lambda parameter is never here: it is
    /// monomorphic within its body. Global like `names`.
    pub poly_binds: Set<BindId>,
    /// Registered package names — the package prelude: usable as
    /// module path roots from anywhere. Populated by package
    /// registration; survives the lexical swap like `names`.
    pub package_roots: Set<ArcStr>,
    /// Installed catch handlers by DYNAMIC scope path: the handler's
    /// error-variable bind and the TOP the handler node lives under
    /// (cross-top deliveries must take the `set_var` path).
    pub catch: Map<ModPath, (BindId, ExprId)>,
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
            names,
            abstract_reps,
            traits,
            trait_defs,
            trait_methods,
            impls,
            poly_binds,
            package_roots: _,
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
        *names = Map::new();
        *abstract_reps = Map::new();
        *traits = Map::new();
        *trait_defs = Map::new();
        *trait_methods = Map::new();
        *impls = Map::new();
        *poly_binds = Set::new();
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
    // `names` and `package_roots` are global registries keyed by
    // globally-unique scope paths, not lexical state — always kept
    // from `self`.
    pub(super) fn restore_lexical_env(&self, other: Self) -> Self {
        Self {
            binds: other.binds,
            modules: other.modules,
            typedefs: other.typedefs,
            traits: other.traits,
            by_id: self.by_id.clone(),
            catch: self.catch.clone(),
            byref_chain: self.byref_chain.clone(),
            names: self.names.clone(),
            abstract_reps: self.abstract_reps.clone(),
            trait_defs: self.trait_defs.clone(),
            trait_methods: self.trait_methods.clone(),
            impls: self.impls.clone(),
            poly_binds: self.poly_binds.clone(),
            package_roots: self.package_roots.clone(),
            ide_binds: self.ide_binds.clone(),
            lsp_mode: self.lsp_mode,
            ide: self.ide.clone(),
        }
    }

    pub(super) fn restore_lexical_env_mut(&self, other: &mut Self) -> Self {
        Self {
            binds: mem::take(&mut other.binds),
            modules: mem::take(&mut other.modules),
            typedefs: mem::take(&mut other.typedefs),
            traits: mem::take(&mut other.traits),
            by_id: self.by_id.clone(),
            catch: self.catch.clone(),
            byref_chain: self.byref_chain.clone(),
            names: self.names.clone(),
            abstract_reps: self.abstract_reps.clone(),
            trait_defs: self.trait_defs.clone(),
            trait_methods: self.trait_methods.clone(),
            impls: self.impls.clone(),
            poly_binds: self.poly_binds.clone(),
            package_roots: self.package_roots.clone(),
            ide_binds: self.ide_binds.clone(),
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

    /// The current package root of `scope`: `/pkg` when the first
    /// path component names a registered package, else `/` (user
    /// programs — the program is the package).
    pub fn package_root<'a>(&self, scope: &'a str) -> &'a str {
        match Path::parts(scope).next() {
            Some(first) if self.package_roots.contains(first) => {
                &scope[..1 + first.len()]
            }
            _ => "/",
        }
    }

    /// The scope `k` levels of `super` above the module enclosing
    /// `scope`. One `super` from module M is the SCOPE surrounding
    /// M's declaration (which may be a block level — a script file's
    /// top level); further `super`s iterate dirname∘mod_root. Errors
    /// when a step would climb above the package root — the root
    /// scope for user programs (the program is the package), `/pkg`
    /// for registered packages.
    pub fn super_anchor<'a>(&self, scope: &'a str, k: usize) -> Result<&'a str> {
        let mut anchor = scope;
        for _ in 0..k {
            let m = mod_root(anchor);
            let at_root = m == "/"
                || (Path::dirname(m).is_none()
                    && Path::basename(m)
                        .map(|b| self.package_roots.contains(b))
                        .unwrap_or(false));
            if at_root {
                bail!("`super` goes above the package root")
            }
            anchor = Path::dirname(m).unwrap_or("/");
        }
        Ok(anchor)
    }

    /// Consult one lexical level for `n`: the level's own
    /// declarations (via `f`), then — iff `origin` is inside the
    /// level's module (imports are private to their module and its
    /// descendants) — its explicit imports (redirected, following
    /// the target level's own imports in turn) and its globs (a glob
    /// provides the source module's OWN names only; two globs
    /// providing the same name is an error at use).
    fn lookup_at<T>(
        &self,
        origin: &str,
        level: &str,
        n: &str,
        depth: usize,
        f: &mut impl FnMut(&str, &str) -> Option<T>,
    ) -> Result<Option<T>> {
        if let Some(t) = f(level, n) {
            return Ok(Some(t));
        }
        if !scope_is_under(origin, mod_root(level)) {
            return Ok(None);
        }
        let Some(sn) = self.names.get(level) else { return Ok(None) };
        if depth > MAX_IMPORT_DEPTH {
            bail!("import chain too deep resolving `{n}` (import cycle?)")
        }
        if let Some(e) = sn.imports.get(n) {
            let hit = if e.chain {
                self.chain_lookup(origin, &e.scope, &e.name, depth + 1, f)?
            } else {
                self.lookup_at(origin, &e.scope, &e.name, depth + 1, f)?
            };
            if let Some(t) = hit {
                return Ok(Some(t));
            }
            // an import covers only the kinds its target has; a
            // kind-miss falls through to globs (the widget-module
            // pattern: `use gui::text::{self, *}` imports the module
            // name AND glob-provides the same-named val inside it)
        }
        let mut found: Option<(usize, T)> = None;
        for (i, g) in sn.globs.iter().enumerate() {
            if let Some(t) = f(g, n) {
                match &found {
                    None => found = Some((i, t)),
                    Some((j, _)) => bail!(
                        "`{n}` is ambiguous: both `{}` and `{g}` provide it; \
                         import one explicitly",
                        sn.globs[*j]
                    ),
                }
            }
        }
        Ok(found.map(|(_, t)| t))
    }

    /// [`Self::lookup_at`] over every level from `from` up to its
    /// module root, inclusive.
    fn chain_lookup<T>(
        &self,
        origin: &str,
        from: &str,
        n: &str,
        depth: usize,
        f: &mut impl FnMut(&str, &str) -> Option<T>,
    ) -> Result<Option<T>> {
        for level in chain_levels(from) {
            if let Some(t) = self.lookup_at(origin, level, n, depth, f)? {
                return Ok(Some(t));
            }
        }
        Ok(None)
    }

    /// Resolve the single segment `seg` as a MODULE from `scope`:
    /// the lexical chain (own submodules, module imports, glob-
    /// provided modules), then the package prelude, then the core
    /// prelude. Returns the canonical module scope.
    fn resolve_module_seg(&self, scope: &str, seg: &str) -> Result<Option<ModPath>> {
        let mut f = |lvl: &str, n: &str| {
            let p = ModPath(Path::from(ArcStr::from(lvl)).append(n));
            if self.modules.contains(&p) { Some(p) } else { None }
        };
        if let Some(p) = self.chain_lookup(scope, scope, seg, 0, &mut f)? {
            return Ok(Some(p));
        }
        // package_roots alone answers here — a sandboxed env may keep
        // `/sys/net` without `/sys`, so the DESCENT is what gates,
        // not the root
        if self.package_roots.contains(seg) {
            return Ok(Some(ModPath(Path::root().append(seg))));
        }
        Ok(f("/core", seg))
    }

    /// One qualified-path descent step: resolve `seg` as a module
    /// within the module at `cur` — its own submodules always, its
    /// module imports/globs iff `origin` is inside it.
    fn descend_step(
        &self,
        origin: &str,
        cur: &str,
        seg: &str,
    ) -> Result<Option<ModPath>> {
        let mut f = |lvl: &str, n: &str| {
            let p = ModPath(Path::from(ArcStr::from(lvl)).append(n));
            if self.modules.contains(&p) { Some(p) } else { None }
        };
        self.lookup_at(origin, cur, seg, 0, &mut f)
    }

    /// The resolution core: resolve `name`, written at `scope`, per
    /// the explicit-import rules — `f` is consulted with candidate
    /// `(module_scope, base_name)` pairs in precedence order and the
    /// first `Some` wins; a `None` from `f` means "no item of my
    /// kind there" and resolution continues (so an import whose
    /// target lacks the wanted kind falls through). Errors are
    /// structural: an ambiguous glob name, a `super` past the root,
    /// a missing interior module, a keyword in a non-leading
    /// position.
    pub fn resolve_visible<T>(
        &self,
        scope: &ModPath,
        name: &ModPath,
        ns: NameNs,
        mut f: impl FnMut(&str, &str) -> Option<T>,
    ) -> Result<Option<T>> {
        let parts: LPooled<Vec<&str>> = Path::parts(&**name).collect();
        let Some((&base, _)) = parts.split_last() else { return Ok(None) };
        let n_super = parts.iter().take_while(|s| **s == "super").count();
        // the bare receiver name of an impl method is an ordinary
        // value binding (`self.0`, `read(self, n)`); only `self::x`
        // is the path keyword
        if parts.len() == 1 && base == "self" && ns == NameNs::Value {
            return self.chain_lookup(scope, scope, base, 0, &mut f);
        }
        let lead = match parts[0] {
            "self" | "package" => 1,
            "super" => n_super,
            _ => 0,
        };
        if lead == parts.len() {
            bail!("a path must name something below self/super/package")
        }
        if let Some(kw) =
            parts[lead..].iter().find(|s| matches!(**s, "self" | "super" | "package"))
        {
            bail!("`{kw}` is only legal leading a path")
        }
        let interior = &parts[lead..parts.len() - 1];
        // keyword-rooted or qualified: resolve the module context,
        // then the terminal name at that module (with imports/globs
        // visible iff we are inside it — lookup_at gates that)
        let anchor: &str = match parts[0] {
            "self" => mod_root(scope),
            "super" => self.super_anchor(scope, n_super)?,
            "package" => self.package_root(scope),
            _ if parts.len() == 1 => {
                // bare name: the lexical chain, then the preludes
                if let Some(t) = self.chain_lookup(scope, scope, base, 0, &mut f)? {
                    return Ok(Some(t));
                }
                if ns == NameNs::Module && self.package_roots.contains(base) {
                    if let Some(t) = f("/", base) {
                        return Ok(Some(t));
                    }
                }
                return Ok(f("/core", base));
            }
            first => {
                // qualified: the first segment resolves as a module
                // through the chain and preludes
                match self.resolve_module_seg(scope, first)? {
                    Some(m) => {
                        let m = self.descend(scope, m, &interior[1..])?;
                        return self.lookup_at(scope, &m, base, 0, &mut f);
                    }
                    None => return Ok(None),
                }
            }
        };
        // keyword-rooted path: `super::x` resolves along the anchor's
        // own chain (a super anchor may be a block level); self and
        // package anchors are module roots, where the chain is one
        // level. No preludes — keyword roots are explicit.
        if interior.is_empty() {
            return self.chain_lookup(scope, anchor, base, 0, &mut f);
        }
        let first = match self.chain_lookup(
            scope,
            anchor,
            interior[0],
            0,
            &mut |lvl: &str, n: &str| {
                let p = ModPath(Path::from(ArcStr::from(lvl)).append(n));
                if self.modules.contains(&p) { Some(p) } else { None }
            },
        )? {
            Some(m) => m,
            None => bail!("no module `{}` in `{anchor}`", interior[0]),
        };
        let m = self.descend(scope, first, &interior[1..])?;
        self.lookup_at(scope, &m, base, 0, &mut f)
    }

    /// Walk `segs` down from module `cur`, erroring on a missing
    /// step.
    fn descend(&self, origin: &ModPath, cur: ModPath, segs: &[&str]) -> Result<ModPath> {
        let mut m = cur;
        for seg in segs {
            match self.descend_step(origin, &m, seg)? {
                Some(next) => m = next,
                None => bail!("no module `{seg}` in `{m}`"),
            }
        }
        Ok(m)
    }

    pub fn lookup_bind(
        &self,
        scope: &ModPath,
        name: &ModPath,
    ) -> Result<Option<(&ModPath, &Bind)>> {
        self.resolve_visible(scope, name, NameNs::Value, |scope, name| {
            self.binds.get_full(scope).and_then(|(scope, vars)| {
                vars.get(name)
                    .and_then(|bid| self.by_id.get(bid).map(|bind| (scope, bind)))
            })
        })
    }

    pub fn lookup_typedef(
        &self,
        scope: &ModPath,
        name: &ModPath,
    ) -> Result<Option<&TypeDef>> {
        self.resolve_visible(scope, name, NameNs::Type, |scope, name| {
            self.typedefs.get(scope).and_then(|m| m.get(name))
        })
    }

    /// Resolve a trait NAME written at `scope` — declaration, import,
    /// or prelude, like a type name.
    pub fn lookup_trait(
        &self,
        scope: &ModPath,
        name: &ModPath,
    ) -> Result<Option<TraitId>> {
        self.resolve_visible(scope, name, NameNs::Type, |scope, name| {
            self.traits.get(scope).and_then(|m| m.get(name)).copied()
        })
    }

    /// The trait a type reference names, if it names one rather than
    /// a typedef. A reference whose resolution cell is filled is a
    /// typedef (traits never fill it), so the table walk runs only for
    /// still-unresolved refs — trait conjuncts on constrained cells.
    pub fn trait_of_ref(&self, tr: &crate::typ::TypeRef) -> Option<TraitId> {
        if tr.resolved().is_some() {
            return None;
        }
        self.lookup_trait(&tr.scope, &tr.name).ok().flatten()
    }

    /// Declare trait `name` in `scope`. Binds one dispatcher per
    /// method at `scope::name::method` (registering `scope::name` as
    /// a module-like scope so `Trait::m` paths and `use Trait::m`
    /// resolve), records the dispatchers in `trait_methods`, and
    /// registers the definition globally — replacing an earlier
    /// registration of the same identity (an interface's), whose
    /// dispatchers stay valid through `trait_methods`.
    pub fn deftrait(
        &mut self,
        scope: &ModPath,
        name: &ArcStr,
        methods: impl Iterator<Item = (ArcStr, Arc<FnType>, usize, bool)>,
        doc: Option<ArcStr>,
        pos: SourcePosition,
        ori: Arc<Origin>,
    ) -> Result<Arc<TraitDef>> {
        if self.traits.get(scope).and_then(|m| m.get(name.as_str())).is_some() {
            bail!("trait {name} is already defined in scope {scope}")
        }
        if self.typedefs.get(scope).and_then(|m| m.get(name.as_str())).is_some() {
            bail!("{name} is already defined as a type in scope {scope}")
        }
        let id = TraitId::of(scope, name);
        let path = ModPath(scope.append(name));
        if self.modules.contains(&path) {
            bail!("{name} is already defined as a module in scope {scope}")
        }
        self.modules.insert_cow(path.clone());
        let mut defs: LPooled<Vec<TraitMethodDef>> = LPooled::take();
        for (mname, typ, self_index, has_default) in methods {
            let bind = self.bind_variable(
                &path,
                &mname,
                Type::Fn(typ.clone()),
                pos,
                ori.clone(),
            );
            let dispatcher = bind.id;
            let index = defs.len();
            self.trait_methods
                .insert_cow(dispatcher, TraitMethodRef { trait_id: id, index });
            self.poly_binds.insert_cow(dispatcher);
            defs.push(TraitMethodDef {
                name: mname,
                typ,
                self_index,
                dispatcher,
                has_default,
                default: None,
            });
        }
        let def = Arc::new(TraitDef {
            id,
            name: name.clone(),
            scope: scope.clone(),
            path,
            methods: Arc::from_iter(defs.drain(..)),
            doc,
            pos,
            ori,
        });
        self.traits
            .get_or_default_cow(scope.clone())
            .insert_cow(name.as_str().into(), id);
        // the FIRST registration (an interface's, when there is one)
        // is the definition of record; a re-declaration contributes
        // its default bodies through `set_trait_defaults`
        if self.trait_defs.get(&id).is_none() {
            self.trait_defs.insert_cow(id, def.clone());
        }
        Ok(def)
    }

    /// Record the compiled default-method bindings of trait `id` on
    /// its definition of record. Returns that definition.
    pub fn set_trait_defaults(
        &mut self,
        id: TraitId,
        defaults: impl Iterator<Item = (CompactString, BindId)>,
    ) -> Arc<TraitDef> {
        let mut by_name: LPooled<AHashMap<CompactString, BindId>> = LPooled::take();
        for (n, b) in defaults {
            by_name.insert(n, b);
        }
        let cur =
            self.trait_defs.get(&id).expect("set_trait_defaults on an unknown trait");
        let methods: Arc<[TraitMethodDef]> =
            Arc::from_iter(cur.methods.iter().map(|m| {
                let default = by_name.get(m.name.as_str()).copied().or(m.default);
                TraitMethodDef { default, ..m.clone() }
            }));
        let def = Arc::new(TraitDef { methods, ..(**cur).clone() });
        self.trait_defs.insert_cow(id, def.clone());
        def
    }

    pub fn undeftrait(&mut self, def: &Arc<TraitDef>) {
        if let Some(m) = self.traits.get_mut_cow(&def.scope) {
            m.remove_cow(&CompactString::from(def.name.as_str()));
            if m.len() == 0 {
                self.traits.remove_cow(&def.scope);
            }
        }
        for m in def.methods.iter() {
            self.trait_methods.remove_cow(&m.dispatcher);
            self.poly_binds.remove_cow(&m.dispatcher);
            self.unbind_variable(m.dispatcher);
        }
        self.modules.remove_cow(&def.path);
        if self.trait_defs.get(&def.id).map(|d| Arc::ptr_eq(d, def)) == Some(true) {
            self.trait_defs.remove_cow(&def.id);
        }
    }

    pub fn trait_def(&self, id: TraitId) -> Option<&Arc<TraitDef>> {
        self.trait_defs.get(&id)
    }

    /// Register an implementation. One impl per (trait, target): a
    /// registration whose head unifies with an existing one is a
    /// conflict, except that an implementation may replace the
    /// interface declaration of the same (trait, target) it fulfils.
    pub fn register_impl(&mut self, im: Arc<ImplDef>) -> Result<()> {
        let mut list: Vec<Arc<ImplDef>> =
            self.impls.get(&im.trait_id).map(|l| (**l).clone()).unwrap_or_default();
        let mut replace = None;
        for (i, other) in list.iter().enumerate() {
            if self.heads_overlap(&other.target, &im.target)? {
                if other.declared && !im.declared {
                    replace = Some(i);
                    break;
                }
                bail!(
                    "conflicting implementation: {} is already implemented for {} at {}",
                    self.trait_defs
                        .get(&im.trait_id)
                        .map(|d| d.name.clone())
                        .unwrap_or_else(|| arcstr::literal!("?")),
                    other.target,
                    other.pos
                )
            }
        }
        let trait_id = im.trait_id;
        match replace {
            Some(i) => list[i] = im,
            None => list.push(im),
        }
        self.impls.insert_cow(trait_id, Arc::new(list));
        Ok(())
    }

    pub fn unregister_impl(&mut self, im: &Arc<ImplDef>) {
        let Some(list) = self.impls.get(&im.trait_id) else { return };
        let list: Vec<Arc<ImplDef>> =
            list.iter().filter(|o| !Arc::ptr_eq(o, im)).cloned().collect();
        if list.is_empty() {
            self.impls.remove_cow(&im.trait_id);
        } else {
            self.impls.insert_cow(im.trait_id, Arc::new(list));
        }
    }

    /// Do two impl heads name a common type? Each side's head
    /// variables are instantiated fresh, so the probe binds nothing
    /// that outlives it.
    fn heads_overlap(&self, a: &Type, b: &Type) -> Result<bool> {
        let a = a.reset_tvars();
        let b = b.reset_tvars();
        Ok(a.contains(self, &b)? || b.contains(self, &a)?)
    }

    /// The registered impl whose head names the same types as
    /// `target` (an interface's `impl T for X;` pairing with the
    /// implementation's), parameterized heads included.
    pub fn impl_entry(
        &self,
        trait_id: TraitId,
        target: &Type,
    ) -> Result<Option<Arc<ImplDef>>> {
        let Some(list) = self.impls.get(&trait_id) else { return Ok(None) };
        for im in list.iter() {
            if self.heads_overlap(&im.target, target)? {
                return Ok(Some(im.clone()));
            }
        }
        Ok(None)
    }

    /// The implementation of `trait_id` for `t`, which must already be
    /// dereferenced and expanded to a structural type (no `TVar`,
    /// `Ref`, or `Set` at the top — the caller decides those). An
    /// abstract target matches by identity; any other head matches by
    /// unification against a fresh instantiation (binding its
    /// variables, whose bounds discharge through the cells — `impl<'a:
    /// T> T for P<'a>`), then equivalence.
    pub fn find_impl(&self, trait_id: TraitId, t: &Type) -> Result<Option<Arc<ImplDef>>> {
        let Some(list) = self.impls.get(&trait_id) else { return Ok(None) };
        // an open cell inside `t` could still become anything: no
        // head is known to apply, and a probe must not bind it
        if t.has_unbound() {
            return Ok(None);
        }
        for im in list.iter() {
            if let (Type::Abstract { id: a, .. }, Type::Abstract { id: b, .. }) =
                (&im.target, t)
                && a != b
            {
                continue;
            }
            let head = if im.params.is_empty() {
                im.target.clone()
            } else {
                im.target.reset_tvars()
            };
            if head.contains(self, t)? && t.contains(self, &head)? {
                return Ok(Some(im.clone()));
            }
        }
        Ok(None)
    }

    pub fn canonical_modpath(
        &self,
        scope: &ModPath,
        name: &ModPath,
    ) -> Result<Option<ModPath>> {
        self.resolve_visible(scope, name, NameNs::Module, |scope, name| {
            let p = ModPath(Path::from(ArcStr::from(scope)).append(name));
            if self.modules.contains(&p) { Some(p) } else { None }
        })
    }

    /// lookup the nearest catch handler in this scope: the handler's
    /// error-variable bind id and its top
    pub fn lookup_catch(&self, scope: &ModPath) -> Result<(BindId, ExprId)> {
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
        let scan = |res: &mut Vec<(CompactString, BindId)>, level: &str, part: &str| {
            if let Some(vars) = self.binds.get(level) {
                let r = vars.range::<str, _>((Bound::Included(part), Bound::Unbounded));
                for (name, bind) in r {
                    if name.starts_with(part) {
                        res.push((name.clone(), *bind));
                    }
                }
            }
        };
        match Path::dirname(&**part) {
            None => {
                let part = Path::basename(&**part).unwrap_or("");
                for level in chain_levels(scope) {
                    scan(&mut res, level, part);
                    if let Some(sn) = self.names.get(level) {
                        for (name, e) in &sn.imports {
                            if name.starts_with(part) {
                                let find = |lvl: &str| {
                                    self.binds
                                        .get(lvl)
                                        .and_then(|v| v.get(&e.name))
                                        .copied()
                                };
                                let id = if e.chain {
                                    chain_levels(&e.scope).find_map(find)
                                } else {
                                    find(&e.scope)
                                };
                                if let Some(id) = id {
                                    res.push((name.clone(), id));
                                }
                            }
                        }
                        for g in sn.globs.iter() {
                            scan(&mut res, g, part);
                        }
                    }
                }
                scan(&mut res, "/core", part);
            }
            Some(_) => {
                // qualified partial: resolve the module prefix, scan
                // its own names
                let part_base = Path::basename(&**part).unwrap_or("");
                let prefix = ModPath(Path::from(ArcStr::from(
                    Path::dirname(&**part).unwrap_or("/"),
                )));
                if let Ok(Some(m)) = self.canonical_modpath(scope, &prefix) {
                    scan(&mut res, &m, part_base);
                }
            }
        }
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
        let scan = |res: &mut Vec<ModPath>, level: &str, part: &str| {
            let p = ModPath(Path::from(ArcStr::from(level)).append(part));
            for m in self.modules.range((Bound::Included(p.clone()), Bound::Unbounded)) {
                if m.0.starts_with(&*p.0) {
                    if let Some(m) = m.strip_prefix(level) {
                        if !m.trim().is_empty() {
                            res.push(ModPath(Path::from(ArcStr::from(m))));
                        }
                    }
                }
            }
        };
        match Path::dirname(&**part) {
            None => {
                let part = Path::basename(&**part).unwrap_or("");
                for level in chain_levels(scope) {
                    scan(&mut res, level, part);
                    if let Some(sn) = self.names.get(level) {
                        for (name, _) in &sn.imports {
                            if name.starts_with(part) {
                                res.push(ModPath(Path::root().append(name)));
                            }
                        }
                        for g in sn.globs.iter() {
                            scan(&mut res, g, part);
                        }
                    }
                }
                for p in self.package_roots.into_iter() {
                    if p.starts_with(part) {
                        res.push(ModPath(Path::root().append(p.as_str())));
                    }
                }
                scan(&mut res, "/core", part);
            }
            Some(dir) => {
                let part_base = Path::basename(&**part).unwrap_or("");
                let prefix = ModPath(Path::from(ArcStr::from(dir)));
                if let Ok(Some(m)) = self.canonical_modpath(scope, &prefix) {
                    scan(&mut res, &m, part_base);
                }
            }
        }
        res
    }

    /// Install one explicit import at `scope`. Errors on a duplicate
    /// import or a same-scope declaration of the same name, unless
    /// `replace` (the REPL: a re-`use` shadows). Identical re-imports
    /// are idempotent (a `.gxi` use applies to the impl too, and the
    /// impl may spell it again).
    pub fn import(
        &mut self,
        scope: &ModPath,
        key: &str,
        entry: ImportEntry,
        replace: bool,
    ) -> Result<()> {
        if !replace {
            if let Some(e) = self.names.get(scope).and_then(|sn| sn.imports.get(key)) {
                if e.scope == entry.scope && e.name == entry.name {
                    return Ok(());
                }
                bail!(
                    "`{key}` is already imported here (from `{}`); \
                     rename one (`use ... as ...`)",
                    e.scope
                )
            }
            let declared = self
                .binds
                .get(scope)
                .map(|v| v.get(key).is_some())
                .unwrap_or(false)
                || self
                    .typedefs
                    .get(scope)
                    .map(|v| v.get(key).is_some())
                    .unwrap_or(false)
                || self.traits.get(scope).map(|v| v.get(key).is_some()).unwrap_or(false);
            if declared {
                bail!("`{key}` is already defined in this scope; use `as` to rename")
            }
        }
        let sn = self.names.get_or_default_cow(scope.clone());
        sn.imports.insert_cow(key.into(), entry);
        Ok(())
    }

    /// Register a glob (`use m::*`) source module at `scope`.
    /// Idempotent.
    pub fn import_glob(&mut self, scope: &ModPath, src: ModPath) {
        let sn = self.names.get_or_default_cow(scope.clone());
        let globs = Arc::make_mut(&mut sn.globs);
        if !globs.contains(&src) {
            globs.push(src)
        }
    }

    /// True iff an import target currently names something (any
    /// kind), following the chain rule for keyword-anchored entries.
    pub fn import_target_exists(&self, e: &ImportEntry) -> bool {
        let check = |lvl: &str| {
            self.binds.get(lvl).map(|v| v.get(&e.name).is_some()).unwrap_or(false)
                || self
                    .typedefs
                    .get(lvl)
                    .map(|v| v.get(&e.name).is_some())
                    .unwrap_or(false)
                || self.traits.get(lvl).map(|v| v.get(&e.name).is_some()).unwrap_or(false)
                || self
                    .modules
                    .contains(&ModPath(Path::from(ArcStr::from(lvl)).append(&e.name)))
        };
        if e.chain { chain_levels(&e.scope).any(check) } else { check(&e.scope) }
    }

    /// Drop every import table at `scope` or any descendant. Used
    /// when a dynamic module recompiles (its `use`s re-register from
    /// the fresh source) and by the LSP scope scrub.
    pub fn clear_names_under(&mut self, scope: &ModPath) {
        let stale: LPooled<Vec<ModPath>> = (&self.names)
            .into_iter()
            .filter(|(s, _)| scope_is_under(s, scope))
            .map(|(s, _)| s.clone())
            .collect();
        for s in &*stale {
            self.names.remove_cow(s);
        }
    }

    pub fn deftype(
        &mut self,
        scope: &ModPath,
        name: &str,
        params: Arc<[(TVar, Option<Type>)]>,
        body: &TypeDefBody,
        public: bool,
        doc: Option<ArcStr>,
        pos: SourcePosition,
        ori: Arc<Origin>,
    ) -> Result<()> {
        if self.typedefs.get(scope).and_then(|m| m.get(name)).is_some() {
            bail!("{name} is already defined in scope {scope}")
        }
        let (typ, rep) = match body {
            TypeDefBody::Alias(typ) => {
                (typ.scope_refs(scope).rewrite_trait_args(self)?, None)
            }
            TypeDefBody::Abstract(rep) => {
                let formals =
                    Arc::from_iter(params.iter().map(|(tv, _)| Type::TVar(tv.clone())));
                let typ =
                    Type::Abstract { id: AbstractId::of(scope, name), params: formals };
                let rep = match rep {
                    None => None,
                    Some(r) => Some(r.scope_refs(scope).rewrite_trait_args(self)?),
                };
                (typ, rep)
            }
        };
        let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        let mut declared: LPooled<AHashSet<ArcStr>> = LPooled::take();
        for (tv, tc) in params.iter() {
            Type::TVar(tv.clone()).alias_tvars(&mut known);
            if let Some(tc) = tc {
                tc.alias_tvars(&mut known);
            }
        }
        typ.alias_tvars(&mut known);
        if let Some(rep) = &rep {
            rep.alias_tvars(&mut known);
        }
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
        if let (Type::Abstract { id, .. }, Some(rep)) = (&typ, &rep) {
            // an interface's typedefs are compiled again inside the
            // implementation: a re-registration never hides a
            // published definition
            let public =
                public || self.abstract_reps.get(id).map(|r| r.public).unwrap_or(false);
            let formals = Arc::from_iter(params.iter().map(|(tv, _)| tv.clone()));
            let r = AbstractRep {
                scope: scope.clone(),
                name: ArcStr::from(name),
                params: formals,
                rep: rep.clone(),
                public,
            };
            self.abstract_reps.insert_cow(*id, Arc::new(r));
        }
        let defs = self.typedefs.get_or_default_cow(scope.clone());
        defs.insert_cow(
            name.into(),
            TypeDef { params, typ: typ.clone(), rep, doc, pos, ori },
        );
        // A chain of BARE aliases must not close a cycle: `type A = B;
        // type B = A` names nothing, and contains' coinductive ref-pair
        // memo answers true for (cycle, T) before any structure is
        // compared — a binding annotated with the cycle would typecheck
        // against everything. Recursion through a structural body
        // (variant, union, tuple, struct, ...) is untouched: the walk
        // follows only bodies that are bare `Type::Ref`s, and it runs
        // at the def that closes the loop (earlier legs stop at the
        // then-unresolvable forward name). `resolve_pure` because a
        // def-gate probe must not fill resolution cells at a
        // mid-compile registration horizon.
        {
            let mut seen: LPooled<AHashSet<(CompactString, CompactString)>> =
                LPooled::take();
            let scope_str: &str = scope;
            seen.insert((scope_str.into(), name.into()));
            let mut cur = typ;
            while let Type::Ref(tr) = &cur {
                let Some(r) = tr.resolve_pure(self) else { break };
                let refname: &str = &tr.name;
                let base = Path::basename(&refname).unwrap_or(refname);
                let canon: &str = r.canonical_scope();
                if !seen.insert((canon.into(), base.into())) {
                    self.undeftype(scope, name);
                    bail!(
                        "circular type alias: {name} refers back to itself \
                         through a chain of bare aliases; a recursive type \
                         must recurse through a structural body (variant, \
                         union, tuple, struct, ...)"
                    );
                }
                let next = r.typ().clone();
                cur = next;
            }
        }
        Ok(())
    }

    /// The representation of the Graphix-minted abstract type `id`, if
    /// its definition is visible from `from` (the defining scope and
    /// its subtree).
    pub fn abstract_rep(&self, id: AbstractId, from: &ModPath) -> Option<&AbstractRep> {
        let r = self.abstract_reps.get(&id)?;
        let mut from_parts = Path::parts(&from.0);
        let inside = Path::parts(&r.scope.0).all(|part| from_parts.next() == Some(part));
        (r.public || inside).then_some(&**r)
    }

    /// Fill the resolution cell of every `Type::Ref` reachable from a
    /// registered typedef body — the closure-conversion moment for
    /// bodies fusion will expand env-free (`TypeRef::expand_cell`): a
    /// recursive type's inner occurrence is reached by no typecheck
    /// walk (the Ref×Ref name fast path answers without expanding), so
    /// only this pass fills it. Runs after typecheck, when every name's
    /// FINAL target is registered — the one moment eager seeding is
    /// order-correct.
    pub fn seed_typedef_refs(&self) {
        for (_, defs) in self.typedefs.into_iter() {
            for (_, td) in defs.into_iter() {
                td.typ.seed_refs(self);
                if let Some(rep) = &td.rep {
                    rep.seed_refs(self);
                }
            }
        }
    }

    /// Mark the abstract type `id`'s definition exported: its
    /// interface (or interface-less module) published the body.
    pub fn publish_abstract_rep(&mut self, id: AbstractId) {
        if let Some(r) = self.abstract_reps.get(&id)
            && !r.public
        {
            let r = AbstractRep {
                scope: r.scope.clone(),
                name: r.name.clone(),
                params: r.params.clone(),
                rep: r.rep.clone(),
                public: true,
            };
            self.abstract_reps.insert_cow(id, Arc::new(r));
        }
    }

    /// Is `id` a Graphix-minted abstract type (as opposed to a
    /// Rust-backed one)? Visible from everywhere: the TAG is public,
    /// only the representation is scoped.
    pub fn abstract_minted(&self, id: AbstractId) -> bool {
        self.abstract_reps.get(&id).is_some()
    }

    pub fn undeftype(&mut self, scope: &ModPath, name: &str) {
        self.abstract_reps.remove_cow(&AbstractId::of(scope, name));
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
        let mut removed = 0;
        let bind_scopes: LPooled<Vec<ModPath>> = (&self.binds)
            .into_iter()
            .filter(|(s, _)| scope_is_under(s, scope))
            .map(|(s, _)| s.clone())
            .collect();
        for s in &*bind_scopes {
            if let Some(defs) = self.binds.get(s) {
                let ids: LPooled<Vec<BindId>> =
                    defs.into_iter().map(|(_, id)| *id).collect();
                removed += ids.len();
                for id in &*ids {
                    self.by_id.remove_cow(id);
                }
            }
            self.binds.remove_cow(s);
            self.ide_binds.remove_cow(s);
        }
        let type_scopes: LPooled<Vec<ModPath>> = (&self.typedefs)
            .into_iter()
            .filter(|(s, _)| scope_is_under(s, scope))
            .map(|(s, _)| s.clone())
            .collect();
        for s in &*type_scopes {
            if let Some(defs) = self.typedefs.get(s) {
                removed += defs.len();
            }
            self.typedefs.remove_cow(s);
        }
        self.clear_names_under(scope);
        let mod_scopes: LPooled<Vec<ModPath>> = (&self.modules)
            .into_iter()
            .filter(|s| scope_is_under(s, scope))
            .cloned()
            .collect();
        for s in &*mod_scopes {
            self.modules.remove_cow(s);
        }
        let catch_scopes: LPooled<Vec<ModPath>> = (&self.catch)
            .into_iter()
            .filter(|(s, _)| scope_is_under(s, scope))
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::is_block_component;

    #[test]
    fn mod_root_strips_marked() {
        assert_eq!(mod_root("/m/tui/#do1/#fn7"), "/m/tui");
        assert_eq!(mod_root("/#do1"), "/");
        assert_eq!(mod_root("/#do1/foo"), "/#do1/foo");
        assert_eq!(mod_root("/"), "/");
        assert_eq!(mod_root("/a/b"), "/a/b");
        assert!(is_block_component("#do1"));
        assert!(!is_block_component("do1"));
    }

    #[test]
    fn chain_stops_at_module_root() {
        let levels: Vec<&str> = chain_levels("/m/tui/#do1/#fn7").collect();
        assert_eq!(levels, vec!["/m/tui/#do1/#fn7", "/m/tui/#do1", "/m/tui"]);
        let levels: Vec<&str> = chain_levels("/#do1").collect();
        assert_eq!(levels, vec!["/#do1", "/"]);
        let levels: Vec<&str> = chain_levels("/").collect();
        assert_eq!(levels, vec!["/"]);
    }

    #[test]
    fn super_anchor_walks() {
        let mut env = Env::default();
        assert_eq!(env.super_anchor("/a/b/c", 1).unwrap(), "/a/b");
        assert_eq!(env.super_anchor("/a/b/c", 2).unwrap(), "/a");
        // a user program's depth-1 module: the parent is the root
        // scope, which IS the user package's root
        assert_eq!(env.super_anchor("/a", 1).unwrap(), "/");
        assert!(env.super_anchor("/a", 2).is_err());
        assert_eq!(env.super_anchor("/#do1/foo", 1).unwrap(), "/#do1");
        assert!(env.super_anchor("/#do1", 1).is_err());
        // a registered package's root refuses super
        env.package_roots.insert_cow(ArcStr::from("pkg"));
        assert!(env.super_anchor("/pkg", 1).is_err());
        assert_eq!(env.super_anchor("/pkg/sub", 1).unwrap(), "/pkg");
    }

    #[test]
    fn scope_under() {
        assert!(scope_is_under("/a/b", "/a"));
        assert!(scope_is_under("/a", "/a"));
        assert!(!scope_is_under("/ab", "/a"));
        assert!(scope_is_under("/anything", "/"));
    }
}
