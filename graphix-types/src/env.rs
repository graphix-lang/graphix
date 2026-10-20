use crate::{
    BindId, CFlag,
    expr::{At, Expr, ModPath, Origin, Sandbox, TypeDefBody},
    ide::{
        FieldRefSite, Ide, IdeMode, ModuleRefSite, ReferenceSite, ScopeMapEntry,
        SigImplLink, TypeRefSite, Warning,
    },
    mod_root,
    profile::{self, Phase},
    tracked::{TrackedMap, TrackedSet},
    typ::{
        AbstractId, CoreTrait, FnType, ResolvedRef, TVar, TraitId, Type, TypeRef,
        Unguarded,
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use combine::stream::position::SourcePosition;
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use netidx_core::{
    pack::{Pack, PackError},
    path::Path,
};
use netidx_derive::Pack;
use poolshark::local::LPooled;
use std::{
    fmt,
    hash::Hash,
    iter, mem,
    sync::atomic::{AtomicBool, Ordering},
};
use triomphe::Arc;

/// The environment's hasher. An image's maps decode only where it
/// hashes as it did where they were written, so a change of hasher is a
/// change of image format (`image::REGISTRATION_FORMAT`).
pub type Hasher = imhm::FxBuildHasher;
pub type Map<K, V> = imhm::Map<K, V, Hasher>;
pub type Set<K> = imhm::Set<K, Hasher>;

#[derive(Clone)]
pub struct Bind {
    pub id: BindId,
    pub typ: Type,
    pub doc: Option<ArcStr>,
    pub scope: ModPath,
    pub name: CompactString,
    /// Where the binding was introduced (IDE tooling only).
    pub pos: SourcePosition,
    /// Source origin (file/buffer) where the binding was introduced.
    pub ori: Arc<Origin>,
    /// The pattern that bound it, where that matters past its compile.
    pub binder: Option<Binder>,
}

/// A pattern that bound a binding.
#[derive(Debug, Clone, Pack)]
pub enum Binder {
    /// A select arm's pattern, with the inputs whose fires reach that
    /// select's scrutinee (closed over enclosing pattern binds): a facet
    /// of the scrutinee delivery, so no nested select tracks it for wake
    /// catch-up, and an arm that reads it consumes those inputs' fires.
    SelectPattern(Arc<[BindId]>),
    /// A destructuring `let`'s pattern: the type a wider writer needs is
    /// written on the pattern, not the name.
    LetPattern,
}

impl fmt::Debug for Bind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Bind {{ id: {:?} }}", self.id)
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
    /// The formals and their bounds, as the definition declares them.
    pub params: Arc<[(TVar, Option<Type>)]>,
    pub rep: Type,
    /// The definition is exported, so the constructor is usable
    /// wherever the type is; otherwise only inside `scope`.
    pub public: bool,
}

impl AbstractRep {
    /// A fresh instance of the type: `(T<'a..>, rep['a..])` with the
    /// formals replaced by fresh type variables shared between the two,
    /// each bound as its formal is.
    pub fn instantiate(&self, id: AbstractId) -> (Type, Type) {
        let fresh: LPooled<Vec<Type>> =
            self.params.iter().map(|_| Type::empty_tvar()).collect();
        let known = self.formals(&fresh);
        for ((_, bound), cell) in self.params.iter().zip(fresh.iter()) {
            if let (Some(bound), Type::TVar(tv)) = (bound, cell) {
                tv.add_cell_constraint(bound.replace_tvars(&known))
            }
        }
        let rep = self.rep.replace_tvars(&known);
        (Type::Abstract { id, params: Arc::from_iter(fresh.iter().cloned()) }, rep)
    }

    /// The representation with the formals replaced by `params`.
    pub fn instantiate_with(&self, params: &[Type]) -> Type {
        self.rep.replace_tvars(&self.formals(params))
    }

    fn formals(&self, params: &[Type]) -> LPooled<AHashMap<ArcStr, Type>> {
        self.params
            .iter()
            .map(|(tv, _)| tv.name.clone())
            .zip(params.iter().cloned())
            .collect()
    }
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    /// The parameters, body, scope and site. The only strong owner:
    /// every resolution cell naming this definition holds it weakly.
    pub(crate) def: std::sync::Arc<ResolvedRef>,
    /// For a Graphix-minted abstract type, the representation its
    /// constructor wraps; present only where the definition is visible.
    pub rep: Option<Type>,
    pub doc: Option<ArcStr>,
    /// Every `Type::Ref` reachable from `typ` and `rep` has its
    /// resolution cell filled; cells are write-once, so this never
    /// clears.
    pub seeded: Arc<AtomicBool>,
}

impl TypeDef {
    /// Record that this definition and `other` are one type (an
    /// interface's alias and its implementation's): a reference to either
    /// is then one definition wherever identity decides.
    pub fn link_same(&self, other: &TypeDef) {
        self.def.link_same(&other.def)
    }

    pub fn params(&self) -> &Arc<[(TVar, Option<Type>)]> {
        self.def.params()
    }

    pub fn typ(&self) -> &Type {
        self.def.typ()
    }

    /// Where the typedef was declared (IDE tooling only).
    pub fn pos(&self) -> SourcePosition {
        self.def.pos()
    }

    pub fn ori(&self) -> &Arc<Origin> {
        self.def.ori()
    }
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
    /// Where the imported name is written in the use tree.
    pub name_at: SourcePosition,
    /// Set by the first lookup that goes through the import.
    pub used: Arc<AtomicBool>,
}

/// A scope's explicit namespace: what its `use` declarations
/// imported. Lives in [`Env::names`], keyed by the scope path.
#[derive(Debug, Clone, Default)]
pub struct ScopeNames {
    pub imports: Map<CompactString, ImportEntry>,
    /// Glob (`use m::*`) sources, in declaration order.
    pub globs: Arc<Vec<Glob>>,
}

impl crate::tracked::SameValue for ScopeNames {
    fn same_value(&self, other: &Self) -> bool {
        self.imports.ptr_eq(&other.imports) && Arc::ptr_eq(&self.globs, &other.globs)
    }
}

impl crate::tracked::SameValue for TraitMethodRef {
    fn same_value(&self, other: &Self) -> bool {
        self == other
    }
}

/// Where a glob import finds its names.
#[derive(Debug, Clone, PartialEq, Eq, Pack)]
pub enum Glob {
    /// One module's own names.
    Module(ModPath),
    /// A `super::*` anchored at a block level: each level from it up to
    /// its module, the innermost name first, as `use super::x` takes it.
    Chain(ModPath),
}

impl Glob {
    fn find<T>(&self, n: &str, f: &mut impl FnMut(&str, &str) -> Option<T>) -> Option<T> {
        match self {
            Self::Module(m) => f(m, n),
            Self::Chain(from) => chain_levels(from).find_map(|l| f(l, n)),
        }
    }

    fn source(&self) -> &ModPath {
        match self {
            Self::Module(m) | Self::Chain(m) => m,
        }
    }
}

impl Pack for ScopeNames {
    fn encoded_len(&self) -> usize {
        crate::shared_map::map_len(&self.imports) + self.globs.encoded_len()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        crate::shared_map::map_encode(&self.imports, buf, &mut |k, v, buf| {
            k.encode(buf)?;
            v.encode(buf)
        })?;
        self.globs.encode(buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let imports = crate::shared_map::map_decode(buf, &mut |buf| {
            Ok((CompactString::decode(buf)?, ImportEntry::decode(buf)?))
        })?;
        Ok(Self { imports, globs: Pack::decode(buf)? })
    }
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

impl TraitDef {
    /// Whether a method takes `self` beyond its receiver: a union cannot
    /// implement such a trait, since a call narrows only the receiver.
    pub fn takes_self_beyond_receiver(&self) -> bool {
        self.methods.iter().any(|m| {
            m.typ
                .args
                .iter()
                .enumerate()
                .any(|(i, a)| Some(i) != m.self_index && a.typ.mentions_self())
                || m.typ.vargs.as_ref().is_some_and(|t| t.mentions_self())
        })
    }
}

#[derive(Debug, Clone, Pack)]
#[pack(unwrapped)]
pub struct TraitMethodDef {
    pub name: ArcStr,
    /// The declared signature with the receiver `self` constrained by
    /// the trait: `fn<self: Trait>(self, ..) -> T`.
    pub typ: Arc<FnType>,
    /// Index of the `self` parameter in `typ.args`; `None` when `self` is
    /// only the result.
    pub self_index: Option<usize>,
    /// The dispatcher binding at `path::name`, resolved to an
    /// implementation by the self argument's type.
    pub dispatcher: BindId,
    /// The default body's binding, when the method has one and the
    /// declaring implementation has compiled it.
    pub default: Option<BindId>,
}

/// Which trait method a dispatcher binding stands for
/// ([`Env::trait_methods`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Pack)]
#[pack(unwrapped)]
pub struct TraitMethodRef {
    pub trait_id: TraitId,
    pub index: usize,
}

fn non_contractive(name: &str) -> String {
    format!(
        "recursive type {name} refers back to itself through unions and aliases \
         alone, so it names no value; a recursive type must recurse through a \
         structural body (variant, tuple, struct, array, ...)"
    )
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

impl TraitDef {
    /// One declaration, whether or not both sides were decoded from an
    /// image (an image carries a def by value): its dispatchers are
    /// minted per declaration.
    pub(crate) fn same_definition(&self, other: &TraitDef) -> bool {
        self.id == other.id
            && self.pos == other.pos
            && self
                .methods
                .iter()
                .map(|m| m.dispatcher)
                .eq(other.methods.iter().map(|m| m.dispatcher))
    }
}

impl ImplDef {
    /// One implementation, whether or not both sides were decoded from
    /// an image: an implementation's scope is its own block.
    pub(crate) fn same_definition(&self, other: &ImplDef) -> bool {
        self.trait_id == other.trait_id
            && self.scope == other.scope
            && self.declared == other.declared
            && self.pos == other.pos
    }
}

/// Which namespace a resolution serves. Path interiors are always
/// modules; the terminal name's kind decides which preludes apply.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NameNs {
    Value,
    Type,
    Module,
}

/// What a name in type position names.
pub(crate) enum TypeName {
    Def(std::sync::Arc<ResolvedRef>),
    Trait(TraitId),
}

/// Where a scope is, for a message: the root is the top level.
fn in_scope(scope: &ModPath) -> CompactString {
    match &*scope.0 {
        "/" | "" => CompactString::const_new("at the top level"),
        _ => format_compact!("in scope {scope}"),
    }
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

/// [`scope_is_under`] where a component a body or block mints (`#fn..`,
/// `#do..`) stands for every instance of it: an abstract type a body
/// declares is one type in every instance ([`AbstractId::declared`]),
/// and each instance's body sees its definition.
fn under_some_instance(s: &str, prefix: &str) -> bool {
    let (mut s, mut prefix) = (Path::parts(s), Path::parts(prefix));
    loop {
        match (prefix.next(), s.next()) {
            (None, _) => return true,
            (Some(_), None) => return false,
            (Some(p), Some(c))
                if p == c || (p.starts_with('#') && c.starts_with('#')) => {}
            (Some(_), Some(_)) => return false,
        }
    }
}

/// Iterate the lexical levels of `from` from innermost to the
/// enclosing module root, inclusive.
#[doc(hidden)]
pub fn chain_levels(from: &str) -> impl Iterator<Item = &str> {
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
#[doc(hidden)]
pub fn scope_params(
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
fn retain<K: Hash + Eq + Clone, V: Clone>(
    m: &Map<K, V>,
    mut keep: impl FnMut(&K, &V) -> bool,
) -> Map<K, V> {
    let gone: LPooled<Vec<K>> =
        m.into_iter().filter(|(k, v)| !keep(k, v)).map(|(k, _)| k.clone()).collect();
    if gone.is_empty() { m.clone() } else { m.remove_many(gone.iter().cloned()) }
}

/// `s` without the members `keep` refuses.
fn retain_set<K: Hash + Eq + Clone>(
    s: &Set<K>,
    mut keep: impl FnMut(&K) -> bool,
) -> Set<K> {
    let gone: LPooled<Vec<K>> = s.into_iter().filter(|k| !keep(k)).cloned().collect();
    if gone.is_empty() { s.clone() } else { s.remove_many(gone.iter().cloned()) }
}

#[derive(Clone, Debug, Default)]
pub struct Env {
    pub by_id: TrackedMap<BindId, Arc<Bind>>,
    pub byref_chain: TrackedMap<BindId, BindId>,
    pub binds: Map<ModPath, Map<CompactString, BindId>>,
    pub modules: Set<ModPath>,
    pub typedefs: Map<ModPath, Map<CompactString, TypeDef>>,
    /// Every scope's explicit namespace (imports + globs), keyed by
    /// scope path. A global registry, not lexical state: it survives
    /// `swap_lexical`, so deferred resolution can consult the
    /// defining module's table.
    pub names: TrackedMap<ModPath, ScopeNames>,
    /// Every Graphix-minted abstract type's representation, global;
    /// visibility is decided at lookup.
    pub abstract_reps: TrackedMap<AbstractId, Arc<AbstractRep>>,
    /// Trait names by declaring scope; lexical, like `typedefs`.
    pub traits: Map<ModPath, Map<CompactString, TraitId>>,
    /// Every trait's definition by identity; global.
    pub trait_defs: TrackedMap<TraitId, Arc<TraitDef>>,
    /// Dispatcher binding → the trait method it names; global.
    pub trait_methods: TrackedMap<BindId, TraitMethodRef>,
    /// Every trait's implementations; global (scope governs only the
    /// trait's name).
    pub impls: TrackedMap<TraitId, Arc<Vec<Arc<ImplDef>>>>,
    /// Generalized bindings (let-bound lambdas, interface `val`s, trait
    /// dispatchers) whose signature a value occurrence instantiates
    /// afresh. A lambda parameter is never here. Global.
    pub poly_binds: TrackedSet<BindId>,
    /// Registered package names, usable as module path roots from
    /// anywhere. Global.
    pub package_roots: TrackedSet<ArcStr>,
    /// Whether compiles serve an editor, and the IDE side-channels
    /// ([`Ide`]) a check records into.
    pub ide: IdeMode,
    /// The modules whose undeclared impls this compile task does not
    /// see: its siblings in a run of module checks, which reach each
    /// other only through their interfaces.
    #[doc(hidden)]
    pub hidden_impls: Arc<Vec<ModPath>>,
}

impl Env {
    /// A copy for a compile task: the global registries record what the
    /// task writes, for [`Self::join`]; the lexical maps are the task's
    /// own, as `with_restored` leaves them.
    #[doc(hidden)]
    pub fn fork(&self) -> Self {
        Self {
            by_id: self.by_id.fork(),
            byref_chain: self.byref_chain.fork(),
            names: self.names.fork(),
            abstract_reps: self.abstract_reps.fork(),
            trait_defs: self.trait_defs.fork(),
            trait_methods: self.trait_methods.fork(),
            impls: self.impls.fork(),
            poly_binds: self.poly_binds.fork(),
            package_roots: self.package_roots.fork(),
            ide: self.ide.fork(),
            binds: self.binds.clone(),
            modules: self.modules.clone(),
            typedefs: self.typedefs.clone(),
            traits: self.traits.clone(),
            hidden_impls: self.hidden_impls.clone(),
        }
    }

    /// Write back what the task `fork` wrote to the global registries.
    #[doc(hidden)]
    pub fn join(&mut self, fork: Self) {
        let Self {
            by_id,
            byref_chain,
            binds: _,
            modules: _,
            typedefs: _,
            names,
            abstract_reps,
            traits: _,
            trait_defs,
            trait_methods,
            impls,
            poly_binds,
            package_roots,
            ide,
            hidden_impls: _,
        } = fork;
        self.by_id.join(by_id);
        self.byref_chain.join(byref_chain);
        self.names.join(names);
        self.abstract_reps.join(abstract_reps);
        self.trait_defs.join(trait_defs);
        self.trait_methods.join(trait_methods);
        self.impls.join(impls);
        self.poly_binds.join(poly_binds);
        self.package_roots.join(package_roots);
        self.ide.join(ide);
    }

    /// A snapshot of the lexical fields alone, the only ones
    /// [`Self::swap_lexical`] exchanges: a definition keeps one,
    /// and holding the global tables too would make every later write
    /// to them copy what the snapshot shares.
    #[doc(hidden)]
    pub fn lexical(&self) -> Self {
        Self {
            binds: self.binds.clone(),
            modules: self.modules.clone(),
            typedefs: self.typedefs.clone(),
            traits: self.traits.clone(),
            ..Self::default()
        }
    }

    /// Exchange the lexical environment with `other`'s; the global
    /// registries and IDE sinks stay as they are on `self`.
    #[doc(hidden)]
    pub fn swap_lexical(&mut self, other: &mut Self) {
        let Self {
            binds,
            modules,
            typedefs,
            traits,
            by_id: _,
            byref_chain: _,
            names: _,
            abstract_reps: _,
            trait_defs: _,
            trait_methods: _,
            impls: _,
            poly_binds: _,
            package_roots: _,
            ide: _,
            hidden_impls: _,
        } = other;
        mem::swap(&mut self.binds, binds);
        mem::swap(&mut self.modules, modules);
        mem::swap(&mut self.typedefs, typedefs);
        mem::swap(&mut self.traits, traits);
    }

    /// Run `f` on the active IDE sink, if any.
    pub fn with_ide(&self, f: impl FnOnce(&mut Ide)) {
        if let Some(ide) = self.ide.sink() {
            f(&mut ide.lock())
        }
    }

    pub fn push_reference(&self, site: ReferenceSite) {
        self.with_ide(|ide| ide.references.push(site))
    }

    pub fn push_module_reference(&self, site: ModuleRefSite) {
        self.with_ide(|ide| ide.module_references.push(site))
    }

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
        // CR claude for eric: [risk/low] Warnings are published only while compiling,
        // and a program restored from its session image compiles nothing. So a script
        // prints 'WARNING: .. error raised by ? will not be caught' on its first run
        // and never again, which is when a user reruns it to read the warning (probe:
        // design/review-2026-10-12/repro/x-errors-09.gx, three runs without
        // --no-cache). This makes a cold start and a warm start visibly different.
        // Either record a program compile's warnings in the program image entry and
        // replay them through `publish_warning` on restore, or rule that warnings come
        // from `--check`/the LSP and say so. (x-errors-09)
        self.publish_warning(&spec.ori, pos, end, message);
        Ok(())
    }

    /// [`Self::warn`] about a position no expression stands at.
    pub fn warn_at(
        &self,
        flags: BitFlags<CFlag>,
        ori: &Arc<Origin>,
        pos: SourcePosition,
        end: SourcePosition,
        message: impl fmt::Display,
    ) -> Result<()> {
        if flags.contains(CFlag::WarningsAreErrors) {
            return Err(anyhow!("{message}")
                .context(crate::expr::ParserContext { ori: ori.clone(), pos }));
        }
        self.publish_warning(ori, pos, end, message);
        Ok(())
    }

    fn publish_warning(
        &self,
        ori: &Arc<Origin>,
        pos: SourcePosition,
        end: SourcePosition,
        message: impl fmt::Display,
    ) {
        match self.ide.sink() {
            None => self
                .ide
                .print_warning(format_compact!("WARNING at {pos} {ori}: {message}")),
            Some(ide) => ide.lock().warnings.push(Warning {
                pos,
                end,
                ori: ori.clone(),
                message: format_compact!("{message}").as_str().into(),
            }),
        }
    }

    pub fn push_field_ref(&self, site: FieldRefSite) {
        self.with_ide(|ide| ide.field_refs.push(site))
    }

    pub fn push_scope_map_entry(&self, entry: ScopeMapEntry) {
        self.with_ide(|ide| ide.scope_map.push(entry))
    }

    pub fn push_type_ref(&self, site: TypeRefSite) {
        self.with_ide(|ide| ide.push_type_ref(site))
    }

    pub fn push_sig_link(&self, link: SigImplLink) {
        self.with_ide(|ide| ide.sig_links.push(link))
    }

    /// Keep the trait `tid` nameable where it is declared: a sandboxed
    /// module implements the traits its interface declares impls of.
    pub fn grant_trait(&mut self, tid: TraitId) {
        if let Some(d) = self.trait_defs.get(&tid) {
            let (scope, name) = (d.scope.clone(), CompactString::from(d.name.as_str()));
            self.keep_module_path(&scope, true);
            self.traits.get_or_default_cow(scope).insert_cow(name, tid);
        }
    }

    /// Keep `path`'s enclosing modules, and `path` itself when `inclusive`,
    /// as modules a qualified path can descend through, without their
    /// contents.
    fn keep_module_path(&mut self, path: &str, inclusive: bool) {
        let mut cur = match inclusive {
            true => Some(path),
            false => Path::dirname(path),
        };
        while let Some(p) = cur
            && p != "/"
        {
            self.modules.insert_cow(ModPath(Path::from(ArcStr::from(p))));
            cur = Path::dirname(p);
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
                // a listed module keeps its whole subtree: its submodules and
                // its traits' scopes
                let under_listed =
                    |k: &ModPath| modules.iter().any(|m| scope_is_under(&k.0, &m.0));
                let kept = |k: &ModPath| under_listed(k) || names.contains_key(k);
                let mut t = self.clone();
                t.typedefs = retain(&self.typedefs, |k, _| kept(k));
                t.modules = retain_set(&self.modules, kept);
                t.binds = retain(&self.binds, |k, _| kept(k));
                t.traits = retain(&self.traits, |k, _| kept(k));
                for (dir, ns) in names.iter().filter(|(dir, _)| !under_listed(dir)) {
                    if let Some(vals) = t.binds.get_mut_cow(dir) {
                        *vals = retain(vals, |n, _| ns.contains(n));
                    }
                }
                for m in modules.iter() {
                    t.keep_module_path(m, false);
                }
                for dir in names.keys() {
                    t.keep_module_path(dir, true);
                }
                Ok(t)
            }
        }
    }

    /// The current package root of `scope`: `/pkg` when the first
    /// path component names a registered package, else `/` (the program
    /// is the package).
    pub fn package_root<'a>(&self, scope: &'a str) -> &'a str {
        match Path::parts(scope).next() {
            Some(first) if self.package_roots.contains(first) => {
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
                e.used.store(true, Ordering::Relaxed);
                return Ok(Some(t));
            }
            // A kind-miss on an import falls through to globs
            // (`use gui::text::{self, *}`).
        }
        let mut found: Option<(usize, T)> = None;
        for (i, g) in sn.globs.iter().enumerate() {
            if let Some(t) = g.find(n, f) {
                match &found {
                    None => found = Some((i, t)),
                    Some((j, _)) => bail!(
                        "`{n}` is ambiguous: both `{}` and `{}` provide it; \
                         import one explicitly",
                        sn.globs[*j].source(),
                        g.source()
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
        // CR claude for eric: [perf/low] A reference to a name bound d block levels
        // out runs `lookup_at` at every level. Each level hashes and copies a scope
        // prefix of length O(d) (`names.get`, `module_at`, and the caller's `binds`
        // lookup), and each call also recomputes `mod_root`. That makes one reference
        // O(d^2), and every instance build pays it again. With 1000 parameter reads 160
        // blocks deep, InstanceGraph takes 2.3 s, against 0.04 s for reads of an inner
        // local. Fix: hash each level once, e.g. carry interned level ids or prehashed
        // prefixes in the Scope, built at block entry, and take `mod_root` from the
        // Scope. Alternatively, memoize `(scope, name) -> BindId` per compile. probe:
        // design/review-2026-10-12/repro/t-misc-02-outer.gx vs t-misc-02-local.gx
        // (t-misc-02) (t-misc-02)
        // 2026-10-16 claude: Measured on the quick build: the probe's outer-name
        // version checks in 0.45 s against 0.14 s for the local one, about 0.3 ms per
        // reference at depth 160. Each level of chain_lookup hashes the level's whole
        // path three times (the caller's binds lookup, names.get, module_at) and scans
        // it once for mod_root, so a constant-factor cut (one mod_root per chain)
        // leaves the O(d^2). The real fix is prehashed scope keys across binds, names
        // and modules (a key-type change to the env's maps) or a per-compile (scope,
        // name) memo that every bind must invalidate. Real programs nest a few blocks,
        // so this is a cost-versus-churn call: readdressed to eric.
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
                    .and_then(|bid| self.by_id.get(bid).map(|bind| (scope, &**bind)))
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

    /// Whether `r` is core's `ErrChain`, under whatever name it was
    /// reached.
    pub(crate) fn is_core_errchain(&self, r: &ResolvedRef) -> bool {
        static CORE: std::sync::LazyLock<ModPath> =
            std::sync::LazyLock::new(|| ModPath::from(["core"]));
        self.typedefs
            .get(&*CORE)
            .and_then(|defs| defs.get("ErrChain"))
            .is_some_and(|td| td.def.def_key() == r.def_key())
    }

    /// The trait a type reference names, if it names one rather than a
    /// typedef (a filled resolution cell is always a typedef).
    pub fn trait_of_ref(&self, tr: &TypeRef) -> Option<TraitId> {
        if let Some(id) = tr.trait_id() {
            return Some(id);
        }
        if tr.resolved().is_some() {
            return None;
        }
        match self.resolve_type_name(&tr.scope, &tr.name) {
            Ok(Some(TypeName::Trait(id))) => Some(id),
            _ => None,
        }
    }

    /// What a name in type position means at `scope`: the first typedef
    /// or trait of that name along the lookup chain, so a closer one of
    /// either kind shadows a farther one of the other.
    pub(crate) fn resolve_type_name(
        &self,
        scope: &ModPath,
        name: &ModPath,
    ) -> Result<Option<TypeName>> {
        self.resolve_visible(scope, name, NameNs::Type, |s, n| {
            match self.typedefs.get(s).and_then(|m| m.get(n)) {
                Some(d) => Some(TypeName::Def(d.def.clone())),
                None => self
                    .traits
                    .get(s)
                    .and_then(|m| m.get(n))
                    .map(|id| TypeName::Trait(*id)),
            }
        })
    }

    /// Declare trait `name` in `scope`: one dispatcher per method at
    /// `scope::name::method`, recorded in `trait_methods`, and the
    /// definition registered globally. The first registration of an
    /// identity is the definition of record.
    pub fn deftrait(
        &mut self,
        scope: &ModPath,
        name: &ArcStr,
        methods: impl Iterator<Item = (ArcStr, SourcePosition, Arc<FnType>, Option<usize>)>,
        doc: Option<ArcStr>,
        pos: SourcePosition,
        ori: Arc<Origin>,
    ) -> Result<Arc<TraitDef>> {
        if self.traits.get(scope).and_then(|m| m.get(name.as_str())).is_some() {
            bail!("trait {name} is already defined {}", in_scope(scope))
        }
        if self.typedefs.get(scope).and_then(|m| m.get(name.as_str())).is_some() {
            bail!("{name} is already defined as a type {}", in_scope(scope))
        }
        let id = TraitId::of(scope, name);
        let path = ModPath(scope.append(name));
        if self.modules.contains(&path) {
            bail!("{name} is already defined as a module {}", in_scope(scope))
        }
        self.modules.insert_cow(path.clone());
        let mut defs: LPooled<Vec<TraitMethodDef>> = LPooled::take();
        let (mut applied, mut bare) = (false, false);
        for (mname, mpos, typ, self_index) in methods {
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
                mpos,
                ori.clone(),
            );
            let dispatcher = bind.id;
            let index = defs.len();
            self.trait_methods.insert(dispatcher, TraitMethodRef { trait_id: id, index });
            self.poly_binds.insert(dispatcher);
            defs.push(TraitMethodDef {
                name: mname,
                typ,
                self_index,
                dispatcher,
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
            self.trait_defs.insert(id, def.clone());
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
        self.trait_defs.insert(id, def.clone());
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
            self.trait_methods.remove(&m.dispatcher);
            self.poly_binds.remove(&m.dispatcher);
            self.unbind_variable(m.dispatcher);
        }
        self.modules.remove_cow(&def.path);
        if self.trait_defs.get(&def.id).is_some_and(|d| d.same_definition(def)) {
            self.trait_defs.remove(&def.id);
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
        let empty = Arc::new(Vec::new());
        let list = self.impls.get(&im.trait_id).cloned().unwrap_or(empty);
        let id = self.head_abstract(&im.target);
        for other in list.iter() {
            let distinct_abstracts = id.is_some_and(|id| {
                self.head_abstract(&other.target).is_some_and(|o| o != id)
            });
            if !distinct_abstracts && self.heads_overlap(&other.target, &im.target)? {
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
        let mut list = list;
        Arc::make_mut(&mut list).push(im);
        self.impls.insert(trait_id, list);
        Ok(None)
    }

    pub fn unregister_impl(&mut self, im: &Arc<ImplDef>) {
        let Some(list) = self.impls.get(&im.trait_id) else { return };
        let list: Vec<Arc<ImplDef>> =
            list.iter().filter(|o| !o.same_definition(im)).cloned().collect();
        if list.is_empty() {
            self.impls.remove(&im.trait_id);
        } else {
            self.impls.insert(im.trait_id, Arc::new(list));
        }
    }

    /// Whether the abstract type `id` crosses data: it has a `Cast`
    /// implementation.
    pub fn casts(&self, id: AbstractId) -> bool {
        self.impls_of(CoreTrait::Cast.id()).is_some_and(|l| {
            l.iter().any(|im| self.head_abstract(&im.target) == Some(id))
        })
    }

    /// Whether the abstract type `id` is written as its representation:
    /// its `Cast` implementation leaves `to_value` out.
    pub fn written_as_representation(&self, id: AbstractId) -> bool {
        self.impls_of(CoreTrait::Cast.id()).is_some_and(|l| {
            l.iter().any(|im| {
                self.head_abstract(&im.target) == Some(id)
                    && !im.methods.contains_key("to_value")
            })
        })
    }

    /// The abstract type an impl head names, through its typedef: two
    /// heads of different abstract types never overlap.
    fn head_abstract(&self, t: &Type) -> Option<AbstractId> {
        match t {
            Type::Abstract { id, .. } => Some(*id),
            Type::Ref(_) => match t.lookup_ref(self).ok()? {
                Type::Abstract { id, .. } => Some(id),
                _ => None,
            },
            _ => None,
        }
    }

    /// Do two impl heads name a common type? Each side's head
    /// variables are instantiated fresh, so the probe binds nothing
    /// that outlives it.
    pub(crate) fn heads_overlap(&self, a: &Type, b: &Type) -> Result<bool> {
        let a = a.reset_tvars();
        let b = b.reset_tvars();
        a.drop_bounds();
        b.drop_bounds();
        Ok(a.contains(self, &b)? || b.contains(self, &a)?)
    }

    /// The implementations of `trait_id` this compile task sees: those
    /// registered, less a sibling's undeclared ones ([`Self::hidden_impls`]).
    #[doc(hidden)]
    pub fn impls_of(&self, trait_id: TraitId) -> Option<Arc<Vec<Arc<ImplDef>>>> {
        let list = self.impls.get(&trait_id)?;
        if !list.iter().any(|im| self.impl_hidden(im)) {
            return Some(list.clone());
        }
        Some(Arc::new(list.iter().filter(|im| !self.impl_hidden(im)).cloned().collect()))
    }

    /// Every impl of `trait_id`, hidden ones included (see
    /// [`Self::impl_hidden`]).
    pub(crate) fn impls_all(&self, trait_id: TraitId) -> Option<&Arc<Vec<Arc<ImplDef>>>> {
        self.impls.get(&trait_id)
    }

    /// An impl a sibling module's body added undeclared, hidden from the
    /// other siblings' checks.
    pub(crate) fn impl_hidden(&self, im: &ImplDef) -> bool {
        !im.declared
            && self.hidden_impls.iter().any(|m| scope_is_under(&im.scope.0, &m.0))
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
        crate::typ::find_impl(self, trait_id, t)
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
                    match g {
                        Glob::Module(m) => f(Visit::Level(m)),
                        Glob::Chain(from) => {
                            chain_levels(from).for_each(|l| f(Visit::Level(l)))
                        }
                    }
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
                let at = res.len();
                res.extend(
                    vars.iter()
                        .filter(|(name, _)| name.starts_with(part))
                        .map(|(name, bind)| (name.clone(), *bind)),
                );
                res[at..].sort_unstable_by(|(a, _), (b, _)| a.cmp(b));
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
            let mut found: LPooled<Vec<&ModPath>> =
                self.modules.iter().filter(|m| m.0.starts_with(&*p.0)).collect();
            found.sort_unstable();
            for m in found.drain(..) {
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
                for p in self.package_roots.iter() {
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
        let sn = self.names.get_or_default(scope.clone());
        sn.imports.insert_cow(key.into(), entry);
        Ok(())
    }

    /// The imports this env holds that `before` does not and that no
    /// lookup went through, with their keys.
    pub fn unused_imports_since<'a>(
        &'a self,
        before: &'a Env,
    ) -> impl Iterator<Item = (&'a CompactString, &'a ImportEntry)> {
        self.names.iter().flat_map(move |(scope, sn)| {
            let old = before.names.get(scope);
            sn.imports.iter().filter(move |(key, e)| {
                !e.used.load(Ordering::Relaxed)
                    && !old
                        .and_then(|o| o.imports.get(key.as_str()))
                        .is_some_and(|o| Arc::ptr_eq(&o.used, &e.used))
            })
        })
    }

    /// Register a glob (`use m::*`) source module at `scope`.
    /// Idempotent.
    pub fn import_glob(&mut self, scope: &ModPath, src: Glob) {
        let sn = self.names.get_or_default(scope.clone());
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
        self.names.retain(|s, _| !scope_is_under(s, scope));
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
        decl: SourcePosition,
        ori: Arc<Origin>,
    ) -> Result<Unguarded> {
        if self.typedefs.get(scope).and_then(|m| m.get(name)).is_some() {
            bail!("{name} is already defined {}", in_scope(scope))
        }
        if self.traits.get(scope).and_then(|m| m.get(name)).is_some() {
            bail!("{name} is already defined as a trait {}", in_scope(scope))
        }
        let (typ, rep) = match body {
            TypeDefBody::Alias(typ) => {
                (typ.scope_refs(scope).rewrite_trait_args(self)?, None)
            }
            TypeDefBody::Abstract(rep) => {
                let formals =
                    Arc::from_iter(params.iter().map(|(tv, _)| Type::TVar(tv.clone())));
                let id = AbstractId::declared(scope, name, decl, &ori);
                let typ = Type::Abstract { id, params: formals };
                let rep = match rep {
                    None => None,
                    Some(r) => Some(r.scope_refs(scope).rewrite_trait_args(self)?),
                };
                (typ, rep)
            }
        };
        let params = scope_params(&params, scope);
        for b in params.iter().filter_map(|(_, b)| b.as_ref()) {
            b.check_bound(self)?;
        }
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
        // a variable is a parameter or a quantifier of a `fn<..>` around it
        typ.check_tvars_declared(&declared)?;
        if let Some(rep) = &rep {
            rep.check_tvars_declared(&declared)?;
        }
        for dec in declared.iter() {
            if !used.contains_key(dec) {
                bail!("unused type parameter {dec} in definition of {name}")
            }
        }
        if self.ide.is_lsp() {
            // Typedef bodies are stored, not checked, so their type
            // references are recorded here for the IDE.
            typ.record_ide_refs(self, scope);
        }
        if let (Type::Abstract { id, .. }, Some(rep)) = (&typ, &rep)
            && let Some(old) = self.abstract_reps.get(id)
        {
            // the formals by position, then the body
            let shape = |params: &[(TVar, Option<Type>)], rep: &Type| {
                Type::Tuple(Arc::from_iter(
                    params
                        .iter()
                        .map(|(tv, _)| Type::TVar(tv.clone()))
                        .chain([rep.clone()]),
                ))
            };
            let (was, now) = (shape(&old.params, &old.rep), shape(&params, rep));
            if old.params.len() != params.len()
                || was.sig_matches(self, &now).is_err()
                || now.sig_matches(self, &was).is_err()
            {
                bail!(
                    "{name} is already defined as Abstract<{}>: a value of it may still \
                     be held, so its representation cannot change",
                    old.rep
                )
            }
        }
        if let (Type::Abstract { id, .. }, Some(rep)) = (&typ, &rep) {
            // A re-registration never hides a published definition.
            let public =
                public || self.abstract_reps.get(id).map(|r| r.public).unwrap_or(false);
            let r = AbstractRep {
                scope: scope.clone(),
                name: ArcStr::from(name),
                params: params.clone(),
                rep: rep.clone(),
                public,
            };
            // CR claude for eric: [bug/medium] Every instance of a function body that
            // declares an abstract type re-inserts its AbstractRep with a fresh Arc,
            // even when the representation is unchanged. Two statements that call such
            // a function elaborate in parallel compile tasks, and Env::join then sees
            // two writes to one id: the debug build panics in TrackedMap::join with "a
            // fork's write to AbstractId(..) would replace a write made since the
            // fork". `--check` passes, and `cargo test` runs with these asserts on.
            // Keep the existing entry when the check above found the representation
            // unchanged (and publicity unchanged), so the second write is a no-op. A
            // trait declared in a function body hits the same assert on Env::modules
            // (design/review-2026-10-19/repro/x-typecheck-patterns-local-trait-fork-panic.gx);
            // refuse a trait in a function body, as an impl there is refused. probe:
            // design/review-2026-10-19/repro/x-typecheck-patterns-local-abstract-fork-panic.gx
            // (x-typecheck-patterns-04)
            // 2026-10-20 claude: The abstract half is fixed: deftype keeps an existing
            // AbstractRep of the same publicity (the check above it refused a changed
            // representation), so instances in parallel statements write nothing, and
            // Env::abstract_rep judges visibility with a minted component (#fn.., #do..)
            // standing for every instance of it (under_some_instance), so an instance whose
            // scope differs from the registering one still sees its own type, cold and warm
            // (lang::image::function_body_abstract_type_is_one_type). Pin:
            // lang::modules::body_abstract_type_from_two_statements. The trait half still
            // panics (x-typecheck-patterns-local-trait-fork-panic.gx); refusing a trait in a
            // function body is the open ruling c-module-traits-06, so re-addressed.

            // the check above refused a changed representation, so an
            // entry of the same publicity is this one: writing it again would
            // be a second write a parallel task's join refuses
            if self.abstract_reps.get(id).is_none_or(|old| old.public != public) {
                self.abstract_reps.insert(*id, Arc::new(r));
            }
        }
        let def = std::sync::Arc::new(ResolvedRef::new(
            scope.clone(),
            pos,
            ori,
            params,
            typ.clone(),
        ));
        let key = def.def_key();
        let defs = self.typedefs.get_or_default_cow(scope.clone());
        defs.insert_cow(
            name.into(),
            TypeDef { def, rep, doc, seeded: Arc::new(AtomicBool::new(false)) },
        );
        // Every self-reference must sit under a constructor: `type T =
        // [i64, T]` (or `type A = B; type B = A`) names no value, and
        // contains' coinductive memo would accept it against anything.
        match typ.reaches_unguarded(self, key) {
            Unguarded::Reaches => {
                if let Type::Abstract { id, .. } = &typ {
                    self.abstract_reps.remove(id);
                }
                self.undeftype(scope, name);
                bail!("{}", non_contractive(name))
            }
            u => Ok(u),
        }
    }

    /// [`Self::deftype`]'s contractiveness again, for a definition whose
    /// body named something not defined yet: at the end of the check
    /// every name it reaches is. `Unknown` while a name it reaches is
    /// still undefined.
    pub fn check_contractive(&self, scope: &ModPath, name: &str) -> Result<Unguarded> {
        let Some(td) = self.typedefs.get(scope).and_then(|m| m.get(name)) else {
            return Ok(Unguarded::Guarded);
        };
        match td.typ().reaches_unguarded(self, td.def.def_key()) {
            Unguarded::Reaches => bail!("{}", non_contractive(name)),
            u => Ok(u),
        }
    }

    /// The representation of the Graphix-minted abstract type `id`, if
    /// its definition is visible from `from` (the defining scope and
    /// its subtree).
    pub fn abstract_rep(&self, id: AbstractId, from: &ModPath) -> Option<&AbstractRep> {
        let r = self.abstract_reps.get(&id)?;
        (r.public || under_some_instance(from, &r.scope)).then_some(&**r)
    }

    /// Fill the resolution cell of every `Type::Ref` reachable from a
    /// registered typedef body, for env-free expansion
    /// (`TypeRef::expand_cell`). Must run after the check, when every
    /// name's final target is registered: before elaboration, whose
    /// instances expand their callers' types under their definitions'
    /// envs, and again before fusion for what elaboration registered.
    pub fn seed_typedef_refs(&self) {
        let _profile = profile::phase(Phase::SeedTypes);
        for (_, defs) in self.typedefs.into_iter() {
            for (_, td) in defs.into_iter() {
                if td.seeded.load(Ordering::Relaxed) {
                    continue;
                }
                let complete = td.typ().seed_refs(self)
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
            self.abstract_reps.insert(id, Arc::new(r));
        }
    }

    /// Mark exported the representation of every abstract type defined
    /// at `scope` itself.
    pub fn publish_abstract_reps_at(&mut self, scope: &ModPath) {
        let ids: LPooled<Vec<AbstractId>> = match self.typedefs.get(scope) {
            None => LPooled::take(),
            Some(defs) => defs
                .into_iter()
                .filter_map(|(_, td)| match (td.typ(), &td.rep) {
                    (Type::Abstract { id, .. }, Some(_)) => Some(*id),
                    _ => None,
                })
                .collect(),
        };
        for id in ids.iter() {
            self.publish_abstract_rep(*id);
        }
    }

    /// Is `id` a Graphix-minted (not Rust-backed) abstract type?
    pub fn abstract_minted(&self, id: AbstractId) -> bool {
        self.abstract_reps.get(&id).is_some()
    }

    /// Drop the name `name` at `scope`. An abstract type's
    /// representation stays: every compile of one source typedef
    /// registers the same one, and a live compile still constructs it.
    pub fn undeftype(&mut self, scope: &ModPath, name: &str) {
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
        // every registry by name: one a new field adds is cleared here, or
        // named exempt
        let Env {
            by_id,
            byref_chain,
            // the lexical maps, which unbind_lexical_under cleared
            binds: _,
            modules: _,
            typedefs: _,
            traits: _,
            names,
            abstract_reps,
            trait_defs,
            trait_methods,
            impls,
            poly_binds,
            // a package stays registered across its recheck
            package_roots: _,
            ide: _,
            hidden_impls: _,
        } = self;
        names.retain(|s, _| !under(s));
        let binds: LPooled<Vec<BindId>> = by_id
            .into_iter()
            .filter(|(_, b)| under(&b.scope))
            .map(|(id, _)| *id)
            .collect();
        by_id.remove_many(binds.iter().copied());
        trait_methods.remove_many(binds.iter().copied());
        poly_binds.remove_many(binds.iter().copied());
        byref_chain.remove_many(binds.iter().copied());
        abstract_reps.retain(|_, r| !under(&r.scope));
        let traits: LPooled<Vec<TraitId>> = (&*trait_defs)
            .into_iter()
            .filter(|(_, d)| under(&d.scope))
            .map(|(id, _)| *id)
            .collect();
        trait_defs.remove_many(traits.iter().copied());
        impls.remove_many(traits.iter().copied());
        let impls: LPooled<Vec<Arc<ImplDef>>> = (&*impls)
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
            id,
            scope: scope.clone(),
            doc: None,
            name: CompactString::from(name),
            typ,
            pos,
            ori,
            binder: None,
        };
        self.with_ide(|ide| ide.binds.push(bind.clone()));
        self.by_id.insert(id, Arc::new(bind));
        Arc::make_mut(self.by_id.get_mut(&id).expect("just inserted"))
    }

    /// Give the binding `id` the type `typ`. Every reference compiled
    /// afterwards reads it; the IDE mirror gets the binding again, and
    /// its latest entry wins.
    pub fn retype(&mut self, id: BindId, typ: Type) {
        if let Some(b) = self.by_id.get_mut(&id) {
            Arc::make_mut(b).typ = typ;
            let b = (**b).clone();
            self.with_ide(|ide| ide.binds.push(b));
        }
    }

    /// Record that `id` is bound by a select arm's pattern, over a
    /// scrutinee whose fires come from `inputs`.
    pub fn mark_pattern_bind(&mut self, id: BindId, inputs: Arc<[BindId]>) {
        if let Some(b) = self.by_id.get_mut(&id) {
            Arc::make_mut(b).binder = Some(Binder::SelectPattern(inputs));
        }
    }

    /// The inputs a pattern bind is a facet of: those whose fires reach
    /// its select's scrutinee, closed over enclosing pattern binds.
    /// `None` for any other bind.
    pub fn pattern_inputs(&self, id: BindId) -> Option<&[BindId]> {
        match self.by_id.get(&id).and_then(|b| b.binder.as_ref()) {
            Some(Binder::SelectPattern(inputs)) => Some(inputs),
            Some(Binder::LetPattern) | None => None,
        }
    }

    /// Record that `id` is bound by a destructuring `let`'s pattern.
    pub fn mark_let_pattern(&mut self, id: BindId) {
        if let Some(b) = self.by_id.get_mut(&id) {
            Arc::make_mut(b).binder = Some(Binder::LetPattern);
        }
    }

    pub fn unbind_variable(&mut self, id: BindId) {
        self.poly_binds.remove(&id);
        if let Some(b) = self.by_id.remove(&id) {
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
    fn package_root_is_a_package_or_the_root() {
        let mut env = Env::default();
        assert_eq!(env.package_root("/"), "/");
        assert_eq!(env.package_root("/a/b"), "/");
        assert_eq!(env.package_root("/#do1"), "/");
        assert_eq!(env.package_root("/#do1/#block3/test"), "/");
        assert_eq!(env.package_root("/#fn7/m"), "/");
        env.package_roots.insert(ArcStr::from("pkg"));
        assert_eq!(env.package_root("/pkg/#do1/sub"), "/pkg");
    }

    /// A recursive typedef's body reaches its own resolution cell once
    /// seeded; the cell is weak, so the definition goes with the env.
    #[test]
    fn recursive_typedefs_free_with_their_env() {
        let mut env = Env::default();
        let scope = ModPath::root();
        for src in [
            "type L = [`Nil, `Cons(i64, L)]",
            "type A = [`End, `A(B)]",
            "type B = [`End, `B(A)]",
        ] {
            let expr = crate::expr::parser::parse_one(src).unwrap();
            let crate::expr::ExprKind::TypeDef(td) = &expr.kind else { unreachable!() };
            env.deftype(
                &scope,
                &td.name,
                td.params.clone(),
                &td.body,
                true,
                None,
                expr.pos,
                expr.pos,
                expr.ori.clone(),
            )
            .unwrap();
        }
        env.seed_typedef_refs();
        let defs = env.typedefs.get(&scope).unwrap();
        fn names_itself(t: &Type, def: &std::sync::Arc<ResolvedRef>) -> bool {
            let mut found = matches!(t, Type::Ref(tr)
                if tr.resolved().is_some_and(|r| std::sync::Arc::ptr_eq(&r, def)));
            t.for_each_child(&mut |c| found |= names_itself(c, def));
            found
        }
        let l = defs.get("L").unwrap();
        assert!(names_itself(l.typ(), &l.def), "the seeded body must reach its own cell");
        let held: LPooled<Vec<std::sync::Weak<ResolvedRef>>> = ["L", "A", "B"]
            .iter()
            .map(|n| std::sync::Arc::downgrade(&defs.get(*n).unwrap().def))
            .collect();
        drop(env);
        for w in held.iter() {
            assert!(w.upgrade().is_none(), "a definition outlived its env");
        }
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
        env.poly_binds.insert(local);
        let other = at(&mut env, &ModPath::from(["other"]), "z");
        let ori = Arc::new(Origin::default());
        let pos = SourcePosition::default();
        // a trait of the package, an impl of a trait outside it, an import
        // and a by-ref chain
        let own = env
            .deftrait(
                &pkg,
                &arcstr::literal!("Own"),
                std::iter::empty(),
                None,
                pos,
                ori.clone(),
            )
            .unwrap();
        let outside = env
            .deftrait(
                &ModPath::from(["other"]),
                &arcstr::literal!("Out"),
                std::iter::empty(),
                None,
                pos,
                ori.clone(),
            )
            .unwrap();
        let im = Arc::new(ImplDef {
            trait_id: outside.id,
            target: Type::Primitive(netidx_value::Typ::I64.into()),
            params: Arc::from_iter([]),
            scope: pkg.clone(),
            methods: Map::default(),
            declared: false,
            pos,
            ori: ori.clone(),
        });
        env.register_impl(im).unwrap();
        env.import_glob(&pkg, Glob::Module(ModPath::from(["other"])));
        env.byref_chain.insert(live, other);
        env.unbind_scope_subtree(&pkg);
        for id in [shadowed, live, local] {
            assert!(env.by_id.get(&id).is_none(), "{id:?}");
        }
        assert!(!env.poly_binds.contains(&local));
        assert!(env.by_id.get(&other).is_some());
        assert!(env.trait_defs.get(&own.id).is_none());
        assert!(env.trait_defs.get(&outside.id).is_some());
        assert!(env.impls.get(&outside.id).is_none_or(|l| l.is_empty()));
        assert!(env.names.get(&pkg).is_none());
        assert!(env.byref_chain.get(&live).is_none());
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
            name_at: SourcePosition::default(),
            used: Default::default(),
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
        env.package_roots.insert(ArcStr::from("pkg"));
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
