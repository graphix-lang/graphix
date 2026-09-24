use crate::{
    BindId, CFlag,
    dbgenv::graphix_dbg_bind,
    expr::{At, Expr, ModPath, Origin, Sandbox, TypeDefBody},
    ide::{
        FieldRefSite, Ide, ModuleInternalView, ModuleRefSite, ReferenceSite,
        ScopeMapEntry, SigImplLink, TypeRefSite, Warning,
    },
    is_do_block, mod_root,
    profile::{self, Phase},
    typ::{AbstractId, FnType, TVar, TraitId, Type, TypeRef},
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use combine::stream::position::SourcePosition;
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use netidx_core::path::Path;
use netidx_derive::Pack;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use std::{
    fmt, iter, mem,
    ops::Bound,
    sync::atomic::{AtomicBool, Ordering},
};
use triomphe::Arc;

/// The chunk size of the environment's maps: they are write-heavy at
/// compile time and a COW insert clones the touched chunk.
pub const CHUNK: usize = 16;
pub type Map<K, V> = immutable_chunkmap::map::Map<K, V, CHUNK>;
pub type Set<K> = immutable_chunkmap::set::Set<K, CHUNK>;

#[derive(Clone)]
pub struct Bind {
    pub id: BindId,
    pub export: bool,
    pub typ: Type,
    pub doc: Option<ArcStr>,
    pub scope: ModPath,
    pub name: CompactString,
    /// Where the binding was introduced (IDE tooling only).
    pub pos: SourcePosition,
    /// Source origin (file/buffer) where the binding was introduced.
    pub ori: Arc<Origin>,
    /// What wake catch-up tracks the binding as part of, if anything.
    pub facet: Option<Facet>,
}

/// A binding that is a facet of other inputs for wake catch-up.
#[derive(Debug, Clone, Pack)]
pub enum Facet {
    /// Bound by a select arm's pattern, with the inputs whose fires
    /// reach that select's scrutinee (closed over enclosing pattern
    /// binds): a facet of the scrutinee delivery, so no nested select
    /// tracks it for wake catch-up, and an arm that reads it consumes
    /// those inputs' fires.
    Pattern(Arc<[BindId]>),
    /// Bound by a destructuring `let`: the group's representative bind,
    /// which wake catch-up tracks as one input for all siblings.
    Let(BindId),
}

impl fmt::Debug for Bind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Bind {{ id: {:?}, export: {} }}", self.id, self.export,)
    }
}

#[derive(Debug, Clone, Pack)]
#[pack(unwrapped)]
/// The representation of a Graphix-minted abstract type (`type T =
/// Abstract<rep>`), registered globally but consulted only where the
/// definition is visible, which gates `T(v)`, `x.0` and `T(x)`.
pub struct AbstractRep {
    pub scope: ModPath,
    pub name: ArcStr,
    pub params: Arc<[TVar]>,
    pub rep: Type,
    /// The definition is exported, so the constructor is usable
    /// wherever the type is; otherwise only inside `scope`.
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
    /// For a Graphix-minted abstract type, the representation its
    /// constructor wraps; present only where the definition is visible.
    pub rep: Option<Type>,
    pub doc: Option<ArcStr>,
    /// Where the typedef was declared (IDE tooling only).
    pub pos: SourcePosition,
    pub ori: Arc<Origin>,
    /// Every `Type::Ref` reachable from `typ` and `rep` has its
    /// resolution cell filled; cells are write-once, so this never
    /// clears.
    pub seeded: Arc<AtomicBool>,
}

/// One explicit import: the imported name (the map key in
/// [`ScopeNames::imports`], which differs from `name` under `as`)
/// resolves to `name` in the module at `scope`.
#[derive(Debug, Clone)]
pub struct ImportEntry {
    /// Canonical scope the item was imported from.
    pub scope: ModPath,
    /// The item's own name there.
    pub name: CompactString,
    /// The redirect walks `scope` up to its module root, since a
    /// `super` anchor may be a block level whose items live across the
    /// block chain.
    pub keyword_anchored: bool,
    /// Position/origin of the `use`, for diagnostics and IDE tooling.
    pub pos: SourcePosition,
    pub ori: Arc<Origin>,
}

/// A scope's explicit namespace: what its `use` declarations
/// imported. Lives in [`Env::names`], keyed by the scope path.
#[derive(Debug, Clone, Default, Pack)]
#[pack(unwrapped)]
pub struct ScopeNames {
    pub imports: Map<CompactString, ImportEntry>,
    /// Glob (`use m::*`) source modules, in declaration order.
    pub globs: Arc<Vec<ModPath>>,
}

/// A declared trait: its identity, declaring scope and methods. Lives
/// in the global [`Env::trait_defs`]; an interface's declaration and
/// the implementation's re-declaration mint the same id.
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
    /// A constructor trait: every signature applies `self` (`self<'a>`),
    /// so an impl head names a constructor (`Array<'_>`) and a call
    /// selects by the receiver's outermost form.
    pub hole: bool,
    pub doc: Option<ArcStr>,
    pub pos: SourcePosition,
    pub ori: Arc<Origin>,
}

#[derive(Debug, Clone, Pack)]
#[pack(unwrapped)]
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
    /// The dispatcher binding at `path::name`, resolved to an
    /// implementation by the self argument's type.
    pub dispatcher: BindId,
    /// The default body's binding, when the method has one and the
    /// declaring implementation has compiled it.
    pub default: Option<BindId>,
}

/// Which trait method a dispatcher binding stands for
/// ([`Env::trait_methods`]).
#[derive(Debug, Clone, Copy, Pack)]
#[pack(unwrapped)]
pub struct TraitMethodRef {
    pub trait_id: TraitId,
    pub index: usize,
}

/// One `impl Trait for Target` ([`Env::impls`], global).
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
    /// From an interface declaration (`impl T for X;`): its method
    /// bindings were minted by the signature and the implementation's
    /// methods proxy to them.
    pub declared: bool,
    pub pos: SourcePosition,
    pub ori: Arc<Origin>,
}

/// Which namespace a resolution serves. Path interiors are always
/// modules; the terminal name's kind decides which preludes apply.
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

/// What the prefix of a `use` path names. A bare keyword anchor
/// (`self`, `super::super`, `package`) resolves along the lexical chain
/// of the scope it is written in, and a `super` anchor may be a block
/// level; everything else is a canonical module.
pub enum UseAnchor<'a> {
    Chain(&'a str),
    Module(ModPath),
}

impl UseAnchor<'_> {
    /// The anchor as a scope path.
    pub fn path(&self) -> ModPath {
        match self {
            Self::Chain(a) => ModPath(Path::from(ArcStr::from(*a))),
            Self::Module(m) => m.clone(),
        }
    }
}

const MAX_IMPORT_DEPTH: usize = 32;

/// One step of completion's search ([`Env::completion_levels`]).
enum Visit<'a> {
    Level(&'a str),
    Import(&'a CompactString, &'a ImportEntry),
}

/// A typedef's parameters with their constraints' type references
/// scoped to `scope`, where the definition is.
pub(crate) fn scope_params(
    params: &[(TVar, Option<Type>)],
    scope: &ModPath,
) -> Arc<[(TVar, Option<Type>)]> {
    Arc::from_iter(
        params
            .iter()
            .map(|(tv, tc)| (tv.clone(), tc.as_ref().map(|t| t.scope_refs(scope)))),
    )
}

/// `m` without the entries `keep` refuses.
fn retain<K: Ord + Clone, V: Clone>(
    m: &Map<K, V>,
    mut keep: impl FnMut(&K, &V) -> bool,
) -> Map<K, V> {
    let gone: LPooled<Vec<K>> =
        m.into_iter().filter(|(k, v)| !keep(k, v)).map(|(k, _)| k.clone()).collect();
    if gone.is_empty() { m.clone() } else { m.remove_many(gone.iter().cloned()) }
}

/// `s` without the members `keep` refuses.
fn retain_set<K: Ord + Clone>(s: &Set<K>, mut keep: impl FnMut(&K) -> bool) -> Set<K> {
    let gone: LPooled<Vec<K>> = s.into_iter().filter(|k| !keep(k)).cloned().collect();
    if gone.is_empty() { s.clone() } else { s.remove_many(gone.iter().cloned()) }
}

#[derive(Clone, Debug, Default)]
pub struct Env {
    pub by_id: Map<BindId, Bind>,
    pub byref_chain: Map<BindId, BindId>,
    pub binds: Map<ModPath, Map<CompactString, BindId>>,
    pub modules: Set<ModPath>,
    pub typedefs: Map<ModPath, Map<CompactString, TypeDef>>,
    /// Every scope's explicit namespace (imports + globs), keyed by
    /// scope path. A global registry, not lexical state: it survives
    /// `restore_lexical_env`, so deferred resolution can consult the
    /// defining module's table.
    pub names: Map<ModPath, ScopeNames>,
    /// Every Graphix-minted abstract type's representation, global;
    /// visibility is decided at lookup.
    pub abstract_reps: Map<AbstractId, Arc<AbstractRep>>,
    /// Trait names by declaring scope; lexical, like `typedefs`.
    pub traits: Map<ModPath, Map<CompactString, TraitId>>,
    /// Every trait's definition by identity; global.
    pub trait_defs: Map<TraitId, Arc<TraitDef>>,
    /// Dispatcher binding → the trait method it names; global.
    pub trait_methods: Map<BindId, TraitMethodRef>,
    /// Every trait's implementations; global (scope governs only the
    /// trait's name).
    pub impls: Map<TraitId, Arc<Vec<Arc<ImplDef>>>>,
    /// Generalized bindings (let-bound lambdas, interface `val`s, trait
    /// dispatchers) whose signature a value occurrence instantiates
    /// afresh. A lambda parameter is never here. Global.
    pub poly_binds: Set<BindId>,
    /// Registered package names, usable as module path roots from
    /// anywhere. Global.
    pub package_roots: Set<ArcStr>,
    // XCR claude for eric: three states exist: off; lsp between checks (true,
    // None: registration records into nothing); a check (true, Some). Only
    // GXRt::check installs a sink, under lsp_mode. Recommend `ide: IdeMode {Off,
    // Lsp(Option<sink>)}` after the merge: 44 `lsp_mode` reads in every package.
    /// Populate the IDE side-channels (the `ide` sink).
    pub lsp_mode: bool,
    /// The IDE side-channels ([`Ide`]); `Some` only under an LSP-style
    /// check. Clones share the `Arc<Mutex>` so every compile within one
    /// check drains into the same buffer.
    pub ide: Option<Arc<Mutex<Ide>>>,
}

impl Env {
    // XCR claude for eric: the two restores are one now, and `clear` is gone. A
    // `Lexical` sub-struct would rename 43 `env.binds`/`modules`/`typedefs`/
    // `traits` reads across every package; worth it as its own change after
    // the merge (the image's lexical codec would take the struct too).
    /// Restore the lexical environment to the snapshot `other`; the
    /// global registries and IDE sinks stay as they are on `self`.
    pub(super) fn restore_lexical_env(&self, other: Self) -> Self {
        let Self { binds, modules, typedefs, traits, .. } = other;
        Self { binds, modules, typedefs, traits, ..self.clone() }
    }

    /// [`Self::restore_lexical_env`] taking the lexical maps out of
    /// `other`, so the restored env holds them alone and updates them
    /// in place.
    pub(super) fn restore_lexical_env_mut(&self, other: &mut Self) -> Self {
        self.restore_lexical_env(Self {
            binds: mem::take(&mut other.binds),
            modules: mem::take(&mut other.modules),
            typedefs: mem::take(&mut other.typedefs),
            traits: mem::take(&mut other.traits),
            ..Self::default()
        })
    }

    /// Run `f` on the active IDE sink, if any.
    pub fn with_ide(&self, f: impl FnOnce(&mut Ide)) {
        if let Some(ide) = &self.ide {
            f(&mut ide.lock())
        }
    }

    pub fn push_reference(&self, site: ReferenceSite) {
        self.with_ide(|ide| ide.references.push(site))
    }

    pub fn push_module_reference(&self, site: ModuleRefSite) {
        self.with_ide(|ide| ide.module_references.push(site))
    }

    // XCR claude for eric: done for WarningsAreErrors: the flag is read here and the
    // error carries the site. The stderr branch stays: without `--log-dir` the shell
    // installs no logger, so `log::warn!` would silence every script's warnings. The
    // fix is a warning sink the embedder installs (the Ide's, generalized).
    /// Warn about the text `[pos, end)` of `spec`: an error under
    /// `WarningsAreErrors`, else to the IDE sink under a check that has
    /// one, else to stderr.
    pub fn warn(
        &self,
        flags: BitFlags<CFlag>,
        spec: &Expr,
        pos: SourcePosition,
        end: SourcePosition,
        message: impl fmt::Display,
    ) -> Result<()> {
        if flags.contains(CFlag::WarningsAreErrors) {
            return Err(anyhow!("{message}").at(spec));
        }
        match &self.ide {
            None => eprintln!("WARNING: {} at {pos} {message}", spec.ori),
            Some(ide) => ide.lock().warnings.push(Warning {
                pos,
                end,
                ori: spec.ori.clone(),
                message: format_compact!("{message}").as_str().into(),
            }),
        }
        Ok(())
    }

    pub fn push_field_ref(&self, site: FieldRefSite) {
        self.with_ide(|ide| ide.field_refs.push(site))
    }

    pub fn push_scope_map_entry(&self, entry: ScopeMapEntry) {
        self.with_ide(|ide| ide.scope_map.push(entry))
    }

    pub fn push_type_ref(&self, site: TypeRefSite) {
        self.with_ide(|ide| ide.type_refs.push(site))
    }

    pub fn push_sig_link(&self, link: SigImplLink) {
        self.with_ide(|ide| ide.sig_links.push(link))
    }

    /// A module's internal view, for the IDE.
    pub fn push_module_internal_view(&self, view: ModuleInternalView) {
        self.with_ide(|ide| ide.module_internals.push(view))
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
                    if t.modules.contains(n) {
                        t.unbind_lexical_under(n);
                    } else {
                        let (dir, k) = get_bind_name(n)?;
                        let removed = t
                            .binds
                            .get_mut_cow(dir)
                            .and_then(|vals| vals.remove_cow(&CompactString::from(k)));
                        if removed.is_none() {
                            bail!("no value {k} in module {dir} and no module {n}")
                        }
                    }
                }
                Ok(t)
            }
            Sandbox::Whitelist(wl) => {
                let mut modules: LPooled<AHashSet<ModPath>> = LPooled::take();
                let mut names: LPooled<
                    AHashMap<ModPath, LPooled<AHashSet<CompactString>>>,
                > = LPooled::take();
                for w in wl.iter() {
                    if self.modules.contains(w) {
                        modules.insert(w.clone());
                    } else {
                        let (dir, n) = get_bind_name(w)?;
                        let dir = ModPath(Path::from(ArcStr::from(dir)));
                        let n = CompactString::from(n);
                        self.binds.get(&dir).and_then(|v| v.get(&n)).ok_or_else(
                            || anyhow!("no value {n} in module {dir} and no module {w}"),
                        )?;
                        names.entry(dir).or_default().insert(n);
                    }
                }
                let kept = |k: &ModPath| modules.contains(k) || names.contains_key(k);
                let mut t = self.clone();
                t.typedefs = retain(&self.typedefs, |k, _| kept(k));
                t.modules = retain_set(&self.modules, kept);
                t.binds = retain(&self.binds, |k, _| kept(k));
                for (dir, ns) in names.iter().filter(|(dir, _)| !modules.contains(*dir)) {
                    if let Some(vals) = t.binds.get_mut_cow(dir) {
                        *vals = retain(vals, |n, _| ns.contains(n));
                    }
                }
                Ok(t)
            }
        }
    }

    /// The current package root of `scope`: `/pkg` when the first
    /// path component names a registered package; a loaded script's
    /// own top level (its `#do` block under the root) when the first
    /// component is one; else `/` (the program is the package).
    pub fn package_root<'a>(&self, scope: &'a str) -> &'a str {
        match Path::parts(scope).next() {
            Some(first) if self.package_roots.contains(first) || is_do_block(first) => {
                &scope[..1 + first.len()]
            }
            _ => "/",
        }
    }

    /// The scope `k` levels of `super` above the module enclosing
    /// `scope`: one `super` is the scope surrounding the module's
    /// declaration (possibly a block level). Errors on a step above
    /// the package root.
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

    /// Consult one lexical level for `n`: its own declarations (via
    /// `f`), then, iff `origin` is inside the level's module, its
    /// explicit imports and its globs (two globs providing one name is
    /// an error at use).
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
            let hit = if e.keyword_anchored {
                self.chain_lookup(origin, &e.scope, &e.name, depth + 1, f)?
            } else {
                self.lookup_at(origin, &e.scope, &e.name, depth + 1, f)?
            };
            if let Some(t) = hit {
                return Ok(Some(t));
            }
            // A kind-miss on an import falls through to globs
            // (`use gui::text::{self, *}`).
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

    /// The module `lvl/n`, if there is one.
    fn module_at(&self, lvl: &str, n: &str) -> Option<ModPath> {
        let mut p: LPooled<String> = LPooled::take();
        p.push_str(lvl);
        if !n.is_empty() {
            if !lvl.ends_with('/') {
                p.push('/');
            }
            p.push_str(n);
        }
        self.modules.get(p.as_str()).cloned()
    }

    /// Resolve the single segment `seg` as a module from `scope`: the
    /// lexical chain, then the package prelude, then the core prelude.
    fn resolve_module_seg(&self, scope: &str, seg: &str) -> Result<Option<ModPath>> {
        let mut f = |lvl: &str, n: &str| self.module_at(lvl, n);
        if let Some(p) = self.chain_lookup(scope, scope, seg, 0, &mut f)? {
            return Ok(Some(p));
        }
        // A sandboxed env may keep `/sys/net` without `/sys`: the
        // descent gates, not the root.
        if self.package_roots.contains(seg) {
            return Ok(Some(ModPath(Path::root().append(seg))));
        }
        Ok(self.module_at("/core", seg))
    }

    /// One qualified-path descent step: `seg` as a module within `cur`
    /// (its imports/globs visible iff `origin` is inside it).
    fn descend_step(
        &self,
        origin: &str,
        cur: &str,
        seg: &str,
    ) -> Result<Option<ModPath>> {
        self.lookup_at(origin, cur, seg, 0, &mut |lvl, n| self.module_at(lvl, n))
    }

    /// Resolve `name`, written at `scope`: `f` is consulted with
    /// candidate `(module_scope, base_name)` pairs in precedence order
    /// and the first `Some` wins. Errors are structural: an ambiguous
    /// glob name, a `super` past the root, a missing interior module.
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
        // A bare `self` in value position is the receiver binding;
        // only `self::x` is the path keyword.
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
        let anchor: &str = match parts[0] {
            "self" => mod_root(scope),
            "super" => self.super_anchor(scope, n_super)?,
            "package" => self.package_root(scope),
            _ if parts.len() == 1 => {
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
            first => match self.resolve_module_seg(scope, first)? {
                Some(m) => {
                    let m = self.descend(scope, m, &interior[1..])?;
                    return self.lookup_at(scope, &m, base, 0, &mut f);
                }
                None => return Ok(None),
            },
        };
        // `super::x` resolves along the anchor's own chain (a super
        // anchor may be a block level). No preludes for keyword roots.
        if interior.is_empty() {
            return self.chain_lookup(scope, anchor, base, 0, &mut f);
        }
        let first =
            match self.chain_lookup(scope, anchor, interior[0], 0, &mut |lvl, n| {
                self.module_at(lvl, n)
            })? {
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

    /// The trait a type reference names, if it names one rather than a
    /// typedef (a filled resolution cell is always a typedef).
    pub fn trait_of_ref(&self, tr: &TypeRef) -> Option<TraitId> {
        if tr.resolved().is_some() {
            return None;
        }
        self.lookup_trait(&tr.scope, &tr.name).ok().flatten()
    }

    /// Declare trait `name` in `scope`: one dispatcher per method at
    /// `scope::name::method`, recorded in `trait_methods`, and the
    /// definition registered globally. The first registration of an
    /// identity is the definition of record.
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
        let (mut applied, mut bare) = (false, false);
        for (mname, typ, self_index, has_default) in methods {
            Type::Fn(typ.clone()).self_shape(&mut applied, &mut bare);
            if applied && bare {
                bail!(
                    "trait {name}: `self` is a type constructor (`self<'a>`) in one \
                     signature and a type in another; a trait spells its receiver one \
                     way throughout"
                )
            }
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
            hole: applied,
            doc,
            pos,
            ori,
        });
        self.traits
            .get_or_default_cow(scope.clone())
            .insert_cow(name.as_str().into(), id);
        // A re-declaration contributes only its default bodies.
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

    /// Register an implementation. One impl per (trait, target): an
    /// overlapping head is a conflict unless it is the same module's
    /// interface declaration, which the implementation fulfils — the
    /// declaration stays the entry of record and the implementation's
    /// methods proxy to it. Returns the fulfilled declaration.
    pub fn register_impl(&mut self, im: Arc<ImplDef>) -> Result<Option<Arc<ImplDef>>> {
        let mut list: Vec<Arc<ImplDef>> =
            self.impls.get(&im.trait_id).map(|l| (**l).clone()).unwrap_or_default();
        for other in list.iter() {
            if self.heads_overlap(&other.target, &im.target)? {
                if other.declared
                    && !im.declared
                    && Path::dirname(&*other.scope) == Path::dirname(&*im.scope)
                {
                    return Ok(Some(other.clone()));
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
        list.push(im);
        self.impls.insert_cow(trait_id, Arc::new(list));
        Ok(None)
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
    /// dereferenced and expanded to a structural type. An abstract
    /// target matches by identity; any other head by unification
    /// against a fresh instantiation, then equivalence.
    pub fn find_impl(&self, trait_id: TraitId, t: &Type) -> Result<Option<Arc<ImplDef>>> {
        let Some(list) = self.impls.get(&trait_id) else { return Ok(None) };
        // An open cell inside `t` could still become anything.
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
            let (hc, tc) = (head.contains(self, t)?, t.contains(self, &head)?);
            if graphix_dbg_bind() {
                eprintln!("FIND-IMPL head={head:?} t={t:?} head>=t={hc} t>=head={tc}");
            }
            if hc && tc {
                return Ok(Some(im.clone()));
            }
        }
        if graphix_dbg_bind() {
            eprintln!("FIND-IMPL none for {t:?}");
        }
        Ok(None)
    }

    /// Resolve the segments before a `use` item's last one, written in
    /// `scope`; `None` for no prefix.
    pub fn use_anchor<'a>(
        &self,
        scope: &'a ModPath,
        prefix: &[&str],
    ) -> Result<Option<UseAnchor<'a>>> {
        let n_super = prefix.iter().take_while(|s| **s == "super").count();
        Ok(match prefix.first() {
            None => None,
            Some(&"self") if prefix.len() == 1 => Some(UseAnchor::Chain(mod_root(scope))),
            Some(&"super") if n_super == prefix.len() => {
                Some(UseAnchor::Chain(self.super_anchor(scope, n_super)?))
            }
            Some(&"package") if prefix.len() == 1 => {
                Some(UseAnchor::Chain(self.package_root(scope)))
            }
            Some(_) => {
                let p = ModPath(Path::from_iter(prefix.iter().copied()));
                match self.canonical_modpath(scope, &p)? {
                    Some(m) => Some(UseAnchor::Module(m)),
                    None => bail!("use: no module `{p}` in scope"),
                }
            }
        })
    }

    pub fn canonical_modpath(
        &self,
        scope: &ModPath,
        name: &ModPath,
    ) -> Result<Option<ModPath>> {
        self.resolve_visible(scope, name, NameNs::Module, |lvl, n| self.module_at(lvl, n))
    }

    /// Completion's search from `scope` for a partial name: every scope
    /// to scan (each lexical level, its glob sources, then `/core`) and
    /// every explicit import of each level.
    fn completion_levels<'a>(&'a self, scope: &'a str, mut f: impl FnMut(Visit<'a>)) {
        for lvl in chain_levels(scope) {
            f(Visit::Level(lvl));
            if let Some(sn) = self.names.get(lvl) {
                for (name, e) in &sn.imports {
                    f(Visit::Import(name, e));
                }
                for g in sn.globs.iter() {
                    f(Visit::Level(g));
                }
            }
        }
        f(Visit::Level("/core"));
    }

    /// Where an import's target lives, following the chain rule for a
    /// keyword-anchored entry, as `find` sees it.
    fn import_target<T>(e: &ImportEntry, find: impl Fn(&str) -> Option<T>) -> Option<T> {
        if e.keyword_anchored {
            chain_levels(&e.scope).find_map(find)
        } else {
            find(&e.scope)
        }
    }

    /// Binds in scope matching a partial name (IDE/shell completion).
    pub fn lookup_matching(
        &self,
        scope: &ModPath,
        part: &ModPath,
    ) -> Vec<(CompactString, BindId)> {
        let mut res = vec![];
        let scan = |res: &mut Vec<(CompactString, BindId)>, level: &str, part: &str| {
            if let Some(vars) = self.binds.get(level) {
                let r = vars.range::<str, _>((Bound::Included(part), Bound::Unbounded));
                let r = r.take_while(|(name, _)| name.starts_with(part));
                res.extend(r.map(|(name, bind)| (name.clone(), *bind)));
            }
        };
        match Path::dirname(&**part) {
            None => {
                let part = Path::basename(&**part).unwrap_or("");
                self.completion_levels(scope, |v| match v {
                    Visit::Level(level) => scan(&mut res, level, part),
                    Visit::Import(name, e) if name.starts_with(part) => {
                        let find = |lvl: &str| {
                            self.binds.get(lvl).and_then(|v| v.get(&e.name)).copied()
                        };
                        if let Some(id) = Self::import_target(e, find) {
                            res.push((name.clone(), id));
                        }
                    }
                    Visit::Import(..) => (),
                });
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

    /// Modules in scope matching a partial name (IDE/shell completion),
    /// each relative to the level it was found under.
    pub fn lookup_matching_modules(
        &self,
        scope: &ModPath,
        part: &ModPath,
    ) -> Vec<ModPath> {
        let mut res = vec![];
        let scan = |res: &mut Vec<ModPath>, level: &str, part: &str| {
            let p = ModPath(Path::from(ArcStr::from(level)).append(part));
            let r = self.modules.range((Bound::Included(p.clone()), Bound::Unbounded));
            for m in r.take_while(|m| m.0.starts_with(&*p.0)) {
                let rel = m.strip_prefix(level).map(|m| m.trim_start_matches('/'));
                if let Some(rel) = rel.filter(|m| !m.trim().is_empty()) {
                    res.push(ModPath(Path::from(ArcStr::from(rel))));
                }
            }
        };
        match Path::dirname(&**part) {
            None => {
                let part = Path::basename(&**part).unwrap_or("");
                self.completion_levels(scope, |v| match v {
                    Visit::Level(level) => scan(&mut res, level, part),
                    Visit::Import(name, e)
                        if name.starts_with(part)
                            && Self::import_target(e, |lvl| {
                                self.module_at(lvl, &e.name)
                            })
                            .is_some() =>
                    {
                        res.push(ModPath(Path::root().append(name)))
                    }
                    Visit::Import(..) => (),
                });
                for p in self.package_roots.into_iter() {
                    if p.starts_with(part) {
                        res.push(ModPath(Path::root().append(p.as_str())));
                    }
                }
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
    /// import or a same-scope declaration of the name unless `replace`;
    /// identical re-imports are idempotent.
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
            let declared = self.declares(scope, key);
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

    /// Whether `level` itself declares `name`: a binding, a type or a
    /// trait.
    fn declares(&self, level: &str, name: &str) -> bool {
        self.binds.get(level).is_some_and(|v| v.get(name).is_some())
            || self.typedefs.get(level).is_some_and(|v| v.get(name).is_some())
            || self.traits.get(level).is_some_and(|v| v.get(name).is_some())
    }

    /// True iff an import target currently names something (any
    /// kind), following the chain rule for keyword-anchored entries.
    pub fn import_target_exists(&self, e: &ImportEntry) -> bool {
        let check = |lvl: &str| {
            self.declares(lvl, &e.name) || self.module_at(lvl, &e.name).is_some()
        };
        if e.keyword_anchored {
            chain_levels(&e.scope).any(check)
        } else {
            check(&e.scope)
        }
    }

    /// Drop every import table at `scope` or any descendant.
    pub fn clear_names_under(&mut self, scope: &ModPath) {
        self.names = retain(&self.names, |s, _| !scope_is_under(s, scope));
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
        if self.traits.get(scope).and_then(|m| m.get(name)).is_some() {
            bail!("{name} is already defined as a trait in scope {scope}")
        }
        // CR claude for eric: [bug] a typedef that reaches itself only through unions
        // and refs is accepted: `type T = [i64, T]; let v: T = "hello"` checks and
        // prints hello, and a select over T counts as exhaustive (the root of the CRs in
        // typ/contains.rs on the in-progress `true` and trait_contains). Refuse a
        // definition whose body is not contractive (every self-reference must sit under
        // a constructor).
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
        let params = scope_params(&params, scope);
        let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        let mut declared: LPooled<AHashSet<ArcStr>> = LPooled::take();
        let mut used: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        typ.collect_tvars(&mut used);
        for t in rep.iter().chain(params.iter().filter_map(|(_, tc)| tc.as_ref())) {
            t.collect_tvars(&mut used);
        }
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
            if !used.contains_key(dec) {
                bail!("unused type parameter {dec} in definition of {name}")
            }
        }
        if self.lsp_mode {
            // Typedef bodies are stored, not checked, so their type
            // references are recorded here for the IDE.
            typ.record_ide_refs(self, scope);
        }
        if let (Type::Abstract { id, .. }, Some(rep)) = (&typ, &rep) {
            // A re-registration never hides a published definition.
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
            TypeDef {
                params,
                typ: typ.clone(),
                rep,
                doc,
                pos,
                ori,
                seeded: Arc::new(AtomicBool::new(false)),
            },
        );
        // A chain of bare aliases must not close a cycle: `type A = B;
        // type B = A` names nothing, and contains' coinductive memo
        // would accept it against everything.
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
        (r.public || scope_is_under(from, &r.scope)).then_some(&**r)
    }

    /// Fill the resolution cell of every `Type::Ref` reachable from a
    /// registered typedef body, for env-free expansion
    /// (`TypeRef::expand_cell`). Must run after typecheck, when every
    /// name's final target is registered.
    pub fn seed_typedef_refs(&self) {
        let _profile = profile::phase(Phase::SeedTypes);
        for (_, defs) in self.typedefs.into_iter() {
            for (_, td) in defs.into_iter() {
                if td.seeded.load(Ordering::Relaxed) {
                    continue;
                }
                let complete = td.typ.seed_refs(self)
                    & td.rep.as_ref().is_none_or(|rep| rep.seed_refs(self));
                if complete {
                    td.seeded.store(true, Ordering::Relaxed);
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
            let r = AbstractRep { public: true, ..(**r).clone() };
            self.abstract_reps.insert_cow(id, Arc::new(r));
        }
    }

    /// Is `id` a Graphix-minted (not Rust-backed) abstract type?
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

    /// Drop the lexical entries (modules, binds, typedefs, traits) at
    /// `scope` or below; the number of names they held.
    fn unbind_lexical_under(&mut self, scope: &str) -> usize {
        fn drop_scopes<V: Clone>(
            m: &mut Map<ModPath, Map<CompactString, V>>,
            scope: &str,
        ) -> usize {
            let mut n = 0;
            *m = retain(m, |s, names| {
                let under = scope_is_under(s, scope);
                if under {
                    n += names.len();
                }
                !under
            });
            n
        }
        self.modules = retain_set(&self.modules, |s| !scope_is_under(s, scope));
        drop_scopes(&mut self.binds, scope)
            + drop_scopes(&mut self.typedefs, scope)
            + drop_scopes(&mut self.traits, scope)
    }

    /// Drop everything registered at `scope` or any descendant, so a
    /// package's source can re-register under the same scope: the
    /// lexical entries, the imports, and every global entry declared
    /// there (bindings the lexical maps no longer name included).
    /// Returns the number of bind, typedef and trait names removed.
    pub fn unbind_scope_subtree(&mut self, scope: &ModPath) -> usize {
        let under = |s: &ModPath| scope_is_under(s, scope);
        let removed = self.unbind_lexical_under(scope);
        self.clear_names_under(scope);
        let binds: LPooled<Vec<BindId>> = (&self.by_id)
            .into_iter()
            .filter(|(_, b)| under(&b.scope))
            .map(|(id, _)| *id)
            .collect();
        self.by_id = self.by_id.remove_many(binds.iter().copied());
        self.trait_methods = self.trait_methods.remove_many(binds.iter().copied());
        self.poly_binds = self.poly_binds.remove_many(binds.iter().copied());
        self.byref_chain = self.byref_chain.remove_many(binds.iter().copied());
        self.abstract_reps = retain(&self.abstract_reps, |_, r| !under(&r.scope));
        let traits: LPooled<Vec<TraitId>> = (&self.trait_defs)
            .into_iter()
            .filter(|(_, d)| under(&d.scope))
            .map(|(id, _)| *id)
            .collect();
        self.trait_defs = self.trait_defs.remove_many(traits.iter().copied());
        self.impls = self.impls.remove_many(traits.iter().copied());
        let impls: LPooled<Vec<Arc<ImplDef>>> = (&self.impls)
            .into_iter()
            .flat_map(|(_, l)| l.iter())
            .filter(|im| under(&im.scope))
            .cloned()
            .collect();
        for im in &*impls {
            self.unregister_impl(im);
        }
        removed
    }

    /// Create a new binding, shadowing an existing one in the same scope.
    pub fn bind_variable(
        &mut self,
        scope: &ModPath,
        name: &str,
        typ: Type,
        pos: SourcePosition,
        ori: Arc<Origin>,
    ) -> &mut Bind {
        let id = BindId::new();
        self.binds
            .get_or_default_cow(scope.clone())
            .insert_cow(CompactString::from(name), id);
        let bind = Bind {
            export: true,
            id,
            scope: scope.clone(),
            doc: None,
            name: CompactString::from(name),
            typ,
            pos,
            ori,
            facet: None,
        };
        self.with_ide(|ide| ide.binds.push(bind.clone()));
        self.by_id.insert_cow(id, bind);
        self.by_id.get_mut_cow(&id).expect("just inserted")
    }

    /// Give the binding `id` the type `typ`. Every reference compiled
    /// afterwards reads it; the IDE mirror gets the binding again, and
    /// its latest entry wins.
    pub fn retype(&mut self, id: BindId, typ: Type) {
        if let Some(b) = self.by_id.get_mut_cow(&id) {
            b.typ = typ;
            let b = b.clone();
            self.with_ide(|ide| ide.binds.push(b));
        }
    }

    /// Record that `id` is bound by a select arm's pattern, over a
    /// scrutinee whose fires come from `inputs`.
    pub fn mark_pattern_bind(&mut self, id: BindId, inputs: Arc<[BindId]>) {
        if let Some(b) = self.by_id.get_mut_cow(&id) {
            b.facet = Some(Facet::Pattern(inputs));
        }
    }

    pub fn is_pattern_bind(&self, id: BindId) -> bool {
        self.pattern_inputs(id).is_some()
    }

    /// The inputs a pattern bind is a facet of: those whose fires reach
    /// its select's scrutinee, closed over enclosing pattern binds.
    /// `None` for any other bind.
    pub fn pattern_inputs(&self, id: BindId) -> Option<&[BindId]> {
        match self.by_id.get(&id).and_then(|b| b.facet.as_ref()) {
            Some(Facet::Pattern(inputs)) => Some(inputs),
            Some(Facet::Let(_)) | None => None,
        }
    }

    /// Record that `id` is one of a destructuring `let`'s siblings,
    /// represented by `rep` for wake catch-up.
    pub fn mark_facet(&mut self, id: BindId, rep: BindId) {
        if let Some(b) = self.by_id.get_mut_cow(&id) {
            b.facet = Some(Facet::Let(rep));
        }
    }

    /// The bind wake catch-up tracks `id` under: its `let`
    /// destructuring group's representative, else itself.
    pub fn facet_of(&self, id: BindId) -> BindId {
        match self.by_id.get(&id).and_then(|b| b.facet.as_ref()) {
            Some(Facet::Let(rep)) => *rep,
            Some(Facet::Pattern(_)) | None => id,
        }
    }

    pub fn unbind_variable(&mut self, id: BindId) {
        if let Some(b) = self.by_id.remove_cow(&id) {
            if let Some(binds) = self.binds.get_mut_cow(&b.scope) {
                if binds.get(&b.name) == Some(&id) {
                    binds.remove_cow(&b.name);
                }
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
    fn package_root_is_the_scripts_do_block() {
        let mut env = Env::default();
        assert_eq!(env.package_root("/"), "/");
        assert_eq!(env.package_root("/a/b"), "/");
        assert_eq!(env.package_root("/#do1"), "/#do1");
        assert_eq!(env.package_root("/#do1/#block3/test"), "/#do1");
        assert_eq!(env.package_root("/#fn7/m"), "/");
        env.package_roots.insert_cow(ArcStr::from("pkg"));
        assert_eq!(env.package_root("/pkg/#do1/sub"), "/pkg");
    }

    /// Removing a scope drops the abstract representation its typedef
    /// minted, so a type re-registered at that path is a new one.
    #[test]
    fn unbind_scope_drops_abstract_reps() {
        let mut env = Env::default();
        let scope = ModPath::from(["pkg"]);
        let ori = Arc::new(Origin::default());
        let register = |env: &mut Env, body: &TypeDefBody| {
            env.deftype(
                &scope,
                "Key",
                Arc::from_iter([]),
                body,
                true,
                None,
                SourcePosition::default(),
                ori.clone(),
            )
            .unwrap()
        };
        register(&mut env, &TypeDefBody::Abstract(Some(Type::Any)));
        let id = AbstractId::of(&scope, "Key");
        assert!(env.abstract_minted(id));
        assert!(env.unbind_scope_subtree(&scope) > 0);
        assert!(!env.abstract_minted(id));
        register(&mut env, &TypeDefBody::Abstract(None));
        assert!(!env.abstract_minted(id));
    }

    fn at(env: &mut Env, scope: &ModPath, name: &str) -> BindId {
        let ori = Arc::new(Origin::default());
        env.bind_variable(scope, name, Type::Any, SourcePosition::default(), ori).id
    }

    /// A blacklisted module takes its submodules with it.
    #[test]
    fn blacklist_removes_the_subtree() {
        let mut env = Env::default();
        let (sys, net) = (ModPath::from(["sys"]), ModPath::from(["sys", "net"]));
        env.modules.insert_cow(sys.clone());
        env.modules.insert_cow(net.clone());
        at(&mut env, &net, "publish");
        let t = env.apply_sandbox(&Sandbox::Blacklist(Arc::from_iter([sys]))).unwrap();
        assert!(!t.modules.contains(&net));
        assert!(t.binds.get(&net).is_none());
        assert!(env.modules.contains(&net), "the original is untouched");
    }

    /// Everything bound under a package goes, including a shadowed
    /// binding and one the lexical maps no longer name.
    #[test]
    fn unbind_scope_drops_what_the_lexical_maps_lost() {
        let mut env = Env::default();
        let pkg = ModPath::from(["pkg"]);
        let shadowed = at(&mut env, &pkg, "x");
        let live = at(&mut env, &pkg, "x");
        let body = ModPath::from(["pkg", "#fn7"]);
        let local = at(&mut env, &body, "y");
        env.binds.remove_cow(&body);
        env.poly_binds.insert_cow(local);
        let other = at(&mut env, &ModPath::from(["other"]), "z");
        env.unbind_scope_subtree(&pkg);
        for id in [shadowed, live, local] {
            assert!(env.by_id.get(&id).is_none(), "{id:?}");
        }
        assert!(!env.poly_binds.contains(&local));
        assert!(env.by_id.get(&other).is_some());
    }

    /// Deleting a shadowed binding leaves the name to the one that
    /// shadowed it.
    #[test]
    fn unbinding_a_shadowed_binding_keeps_the_name() {
        let mut env = Env::default();
        let m = ModPath::from(["m"]);
        let old = at(&mut env, &m, "x");
        let new = at(&mut env, &m, "x");
        env.unbind_variable(old);
        assert_eq!(env.binds.get(&m).and_then(|b| b.get("x")), Some(&new));
    }

    /// Module completion offers an import only when it names a module.
    #[test]
    fn module_completion_skips_value_imports() {
        let mut env = Env::default();
        let (m, sub) = (ModPath::from(["m"]), ModPath::from(["m", "sub"]));
        env.modules.insert_cow(m.clone());
        env.modules.insert_cow(sub);
        at(&mut env, &m, "val");
        let import = |name: &str| ImportEntry {
            scope: m.clone(),
            name: name.into(),
            keyword_anchored: false,
            pos: SourcePosition::default(),
            ori: Arc::new(Origin::default()),
        };
        let root = ModPath::root();
        env.import(&root, "mval", import("val"), false).unwrap();
        env.import(&root, "msub", import("sub"), false).unwrap();
        let found = env.lookup_matching_modules(&root, &ModPath::from(["m"]));
        let found: Vec<String> = found.iter().map(|m| m.to_string()).collect();
        assert!(found.contains(&"msub".to_string()), "{found:?}");
        assert!(!found.contains(&"mval".to_string()), "{found:?}");
        assert!(found.contains(&"m".to_string()), "{found:?}");
    }

    #[test]
    fn super_anchor_walks() {
        let mut env = Env::default();
        assert_eq!(env.super_anchor("/a/b/c", 1).unwrap(), "/a/b");
        assert_eq!(env.super_anchor("/a/b/c", 2).unwrap(), "/a");
        assert_eq!(env.super_anchor("/a", 1).unwrap(), "/");
        assert!(env.super_anchor("/a", 2).is_err());
        assert_eq!(env.super_anchor("/#do1/foo", 1).unwrap(), "/#do1");
        assert!(env.super_anchor("/#do1", 1).is_err());
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
