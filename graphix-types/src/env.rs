use crate::{
    AddrSet, BindId, CFlag, SourcePosition,
    expr::{At, Expr, ModPath, Origin, Sandbox, TypeDefBody},
    ide::{
        FieldRefSite, Ide, IdeMode, Lowered, ModuleRefSite, ReferenceSite, ScopeMapEntry,
        SigImplLink, TypeRefSite, Warning, WarningRecord,
    },
    is_lowered, mod_root,
    profile::{self, Phase},
    reference::Lives,
    tracked::{TrackedMap, TrackedSet},
    typ::{
        AbstractId, CoreTrait, FnType, ResolvedRef, TVar, TraitId, Type, TypeRef,
        Unguarded,
        expansive::{Blind, Expansive, through_aliases},
        variance,
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use imhm::{Contents, NodeRef, SlotRef};
use netidx_core::{
    pack::{Pack, PackError},
    path::Path,
};
use netidx_derive::Pack;
use parking_lot::RwLock;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    fmt,
    hash::Hash,
    iter, mem, ptr,
    sync::{
        OnceLock, Weak,
        atomic::{AtomicBool, Ordering},
    },
};
use triomphe::Arc;

/// The environment's hasher. An image's maps decode only where it
/// hashes as it did where they were written, so a change of hasher is a
/// change of image format (`image::REGISTRATION_FORMAT`).
pub type Hasher = imhm::FxBuildHasher;
pub type Map<K, V> = imhm::Map<K, V, Hasher>;
pub type Set<K> = imhm::Set<K, Hasher>;

/// Each pair of `of` outside the subtrees its tree shares with `with`'s
/// that the node of `with` in its place does not hold: every pair `of`
/// holds that `with` lacks or holds another value for is among them, and
/// no pair of a shared subtree is.
pub fn each_unshared<K: Eq, V: Eq>(
    of: &Map<K, V>,
    with: &Map<K, V>,
    mut f: impl FnMut(&K, &V),
) {
    if let Some(root) = of.root() {
        unshared(root, with.root(), &mut f)
    }
}

/// [`each_unshared`] under `of`, a node at the depth of `with`. An inner
/// node's children are in fragment order and a leaf's pairs in hash
/// order, so the children of `of` that `with` lacks between two it shares
/// are compared, in order, with the children of `with` between those two:
/// their counterparts, unless a fragment there was filled or emptied.
fn unshared<'a, K: Eq, V: Eq>(
    of: NodeRef<'a, K, V>,
    with: Option<NodeRef<'_, K, V>>,
    f: &mut impl FnMut(&'a K, &'a V),
) {
    if with.is_some_and(|w| w.identity() == of.identity()) {
        return;
    }
    let mut nodes: SmallVec<[NodeRef<'_, K, V>; 32]> = SmallVec::new();
    let mut pairs: SmallVec<[(&K, &V); 32]> = SmallVec::new();
    match with.map(|w| w.contents()) {
        Some(Contents::Leaf(theirs)) => pairs.extend(theirs),
        Some(Contents::Inner(theirs)) => {
            for slot in theirs {
                match slot {
                    SlotRef::Entry(k, v) => pairs.push((k, v)),
                    SlotRef::Node(n) => nodes.push(n),
                }
            }
        }
        None => (),
    }
    let mut held = 0;
    match of.contents() {
        Contents::Leaf(mine) => {
            for (k, v) in mine {
                if !holds(&pairs, &mut held, k, v) {
                    f(k, v)
                }
            }
        }
        Contents::Inner(slots) => {
            let (mut next, mut counterpart) = (0, 0);
            for slot in slots {
                match slot {
                    SlotRef::Entry(k, v) => {
                        if !holds(&pairs, &mut held, k, v) {
                            f(k, v)
                        }
                    }
                    SlotRef::Node(n) => {
                        match nodes[next..]
                            .iter()
                            .position(|t| t.identity() == n.identity())
                        {
                            Some(at) => {
                                next += at + 1;
                                counterpart = next
                            }
                            None => {
                                unshared(n, nodes.get(counterpart).copied(), f);
                                counterpart += 1
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Whether `pairs`, from `next` on, hold `k` at `v`, then past it.
fn holds<K: Eq, V: Eq>(pairs: &[(&K, &V)], next: &mut usize, k: &K, v: &V) -> bool {
    match pairs[*next..].iter().position(|(tk, tv)| *tk == k && *tv == v) {
        Some(at) => {
            *next += at + 1;
            true
        }
        None => false,
    }
}

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
    /// What bound it, where that matters past its compile.
    pub binder: Option<Binder>,
}

/// What bound a binding, when it is not a plain name.
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
    /// A reference to an expression no binding names: its referent's
    /// cell, whose type no source can write.
    Referent,
}

impl fmt::Debug for Bind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Bind {{ id: {:?} }}", self.id)
    }
}

#[derive(Debug, Clone)]
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
    /// The typedefs `rep` and the bounds name, owned from the check of
    /// the definition that registered it ([`Env::own_abstract_rep`]).
    pub(crate) named: Arc<OnceLock<Box<[std::sync::Arc<ResolvedRef>]>>>,
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

    /// The parameters of the widest instance: each formal's bound, a
    /// formal it names at `Any`, or `Any`. Every instance's parameters are
    /// inside them.
    pub fn widest(&self) -> SmallVec<[Type; 4]> {
        let anys: SmallVec<[Type; 4]> = self.params.iter().map(|_| Type::Any).collect();
        let known = self.formals(&anys);
        self.params
            .iter()
            .map(|(_, bound)| match bound {
                Some(bound) => bound.replace_tvars(&known),
                None => Type::Any,
            })
            .collect()
    }

    /// Whether the representation at the widest instance holds it at
    /// every instance: it meets each parameter covariantly or not at all,
    /// never under a writable reference, a function's argument or an
    /// abstract type's parameter.
    pub fn widest_holds_each(&self, env: &Env) -> bool {
        let formals: SmallVec<[ArcStr; 4]> =
            self.params.iter().map(|(tv, _)| tv.name.clone()).collect();
        variance::covariant_in(env, &self.rep, &formals)
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
    /// The parameters, body, scope and site. Every resolution cell
    /// naming this definition holds it weakly; its owners are this entry,
    /// the node that declared it, and what outlives both (a check table).
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

/// The typedefs blocks declare, findable after their block
/// (`design/env_independent_typerefs.md`, Block types): a block's typedef
/// is in the lexical maps only while the block compiles, and a name
/// naming it may be looked up after. A block's scope names one source
/// occurrence, so an entry means one thing from wherever it is reached.
/// Each definition is held weakly. Global like the import tables: every
/// copy of the env, a compile task's included, shares it.
#[derive(Clone, Debug, Default)]
pub struct BlockTypes(Arc<RwLock<BlockRegistry>>);

#[derive(Debug, Default)]
struct BlockRegistry {
    by_name: AHashMap<CompactString, SmallVec<[BlockType; 1]>>,
    entries: usize,
    /// `entries` after the last sweep of dead entries.
    swept_at: usize,
    /// The loads numbered so far ([`BlockTypes::next_load`]).
    loads: u32,
}

#[derive(Clone, Debug)]
pub struct BlockType {
    pub scope: ModPath,
    pub def: std::sync::Weak<ResolvedRef>,
    pub seeded: Arc<AtomicBool>,
}

/// What the registry holds for a name at a scope.
pub(crate) enum BlockHit {
    Live(std::sync::Arc<ResolvedRef>),
    Dead,
}

fn same_origin(a: &Arc<Origin>, b: &Arc<Origin>) -> bool {
    Arc::ptr_eq(a, b) || a == b
}

/// Whether `scope` is a block's own: its last component is generated.
/// A module a block declares is not: its scope ends in its name, which
/// a reload declares again with other text.
pub(crate) fn is_block_scope(scope: &str) -> bool {
    Path::basename(scope).is_some_and(crate::is_block_component)
}

impl BlockTypes {
    fn entry<T>(
        &self,
        scope: &str,
        name: &str,
        f: impl FnOnce(&BlockType) -> T,
    ) -> Option<T> {
        let reg = self.0.read();
        reg.by_name.get(name)?.iter().find(|e| &*e.scope.0 == scope).map(f)
    }

    pub(crate) fn find(&self, scope: &str, name: &str) -> Option<BlockHit> {
        self.entry(scope, name, |e| match e.def.upgrade() {
            Some(def) => BlockHit::Live(def),
            None => BlockHit::Dead,
        })
    }

    /// The live definition of `name` at `scope`, with its seeded flag,
    /// else the one `new` makes, recorded there in place of a dead entry:
    /// one step under the lock, so compiles of one block declare one
    /// definition.
    fn declare(
        &self,
        scope: &ModPath,
        name: &str,
        new: impl FnOnce() -> std::sync::Arc<ResolvedRef>,
    ) -> (std::sync::Arc<ResolvedRef>, Arc<AtomicBool>) {
        let mut reg = self.0.write();
        let reg = &mut *reg;
        let es = reg.by_name.entry(CompactString::from(name)).or_default();
        let live = |e: &BlockType| Some((e.def.upgrade()?, e.seeded.clone()));
        let at = es.iter().position(|e| e.scope == *scope);
        if let Some(live) = at.and_then(|i| live(&es[i])) {
            return live;
        }
        let def = new();
        let seeded = Arc::new(AtomicBool::new(false));
        let entry = BlockType {
            scope: scope.clone(),
            def: std::sync::Arc::downgrade(&def),
            seeded: seeded.clone(),
        };
        match at {
            Some(i) => es[i] = entry,
            None => {
                es.push(entry);
                reg.entries += 1;
            }
        }
        if reg.entries >= 2 * reg.swept_at.max(16) {
            reg.by_name.retain(|_, es| {
                es.retain(|e| e.def.strong_count() > 0);
                !es.is_empty()
            });
            reg.entries = reg.by_name.values().map(|es| es.len()).sum();
            reg.swept_at = reg.entries;
        }
        (def, seeded)
    }

    /// The number of a load of a source the session may load again (a
    /// REPL input, a `load`, a check, a dynamic module's source): its
    /// origin's [`crate::expr::Origin::load`], so the scopes it opens are
    /// named apart from every other load's, and a block it declares is
    /// never an earlier load's live definition.
    pub fn next_load(&self) -> u32 {
        let mut reg = self.0.write();
        reg.loads += 1;
        reg.loads
    }

    /// Every live entry at `scope` or below, with its name.
    pub fn live_under(&self, scope: &str) -> Vec<(CompactString, BlockType)> {
        let reg = self.0.read();
        let mut out = Vec::new();
        for (n, es) in reg.by_name.iter() {
            for e in es.iter() {
                if e.def.strong_count() > 0 && scope_is_under(&e.scope, scope) {
                    out.push((n.clone(), e.clone()))
                }
            }
        }
        out
    }

    pub fn from_entries(
        entries: impl IntoIterator<Item = (CompactString, BlockType)>,
    ) -> Self {
        let mut reg = BlockRegistry::default();
        for (n, e) in entries {
            reg.by_name.entry(n).or_default().push(e);
            reg.entries += 1;
        }
        reg.swept_at = reg.entries;
        Self(Arc::new(RwLock::new(reg)))
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
    /// Where the dynamic module at this scope is declared, for one whose
    /// path crosses a body: its source is the statement, so this tells its
    /// abstract types from a same-named module's in another body
    /// ([`AbstractId::declared`]); a file module's source is its file.
    pub site: Option<ArcStr>,
    /// The input whose bodies name the scope, where the scope crosses a
    /// body: the table goes when no lexical view holds the input
    /// ([`Env::sweep_names`]).
    pub input: Option<Weak<()>>,
}

/// A top-level input: a REPL input, a script, a statement an embedder
/// compiles. Every lexical view made while it compiles holds it, and so
/// does every view made under one of those, so it lives as long as
/// anything can still compile in a scope one of its bodies names.
#[derive(Clone, Debug, Default)]
pub struct InputHold(std::sync::Arc<()>);

impl ScopeNames {
    /// Whether a view made in `input` holds the table's input. A view
    /// made in none (an image's, a tool's copy of the env after a
    /// compile) reads what it finds.
    fn held_by(&self, input: Option<&InputHold>) -> bool {
        match (&self.input, input) {
            (Some(owner), Some(i)) => Weak::as_ptr(owner) == std::sync::Arc::as_ptr(&i.0),
            (None, _) | (_, None) => true,
        }
    }
}

impl crate::tracked::SameValue for ScopeNames {
    fn same_value(&self, other: &Self) -> bool {
        self.imports.ptr_eq(&other.imports)
            && Arc::ptr_eq(&self.globs, &other.globs)
            && self.site == other.site
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

/// A lexical level as the source names it: a module, or a block in one.
fn written_level(level: &str) -> CompactString {
    let module = crate::written_scope(level);
    if mod_root(level) == level {
        format_compact!("`{module}`")
    } else {
        format_compact!("a block in `{module}`")
    }
}

impl Glob {
    /// What `f` finds for `n` and the level it found it at, which
    /// identifies the item.
    fn find<T>(
        &self,
        n: &str,
        f: &mut impl FnMut(&str, &str) -> Option<T>,
    ) -> Option<(&str, T)> {
        match self {
            Self::Module(m) => f(m, n).map(|t| (&***m, t)),
            Self::Chain(from) => chain_levels(from).find_map(|l| f(l, n).map(|t| (l, t))),
        }
    }
}

impl Pack for ScopeNames {
    fn encoded_len(&self) -> usize {
        crate::shared_map::map_len(&self.imports)
            + self.globs.encoded_len()
            + self.site.encoded_len()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        crate::shared_map::map_encode(&self.imports, buf, &mut |k, v, buf| {
            k.encode(buf)?;
            v.encode(buf)
        })?;
        self.globs.encode(buf)?;
        self.site.encode(buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let imports = crate::shared_map::map_decode(buf, &mut |buf| {
            Ok((CompactString::decode(buf)?, ImportEntry::decode(buf)?))
        })?;
        Ok(Self {
            imports,
            globs: Pack::decode(buf)?,
            site: Pack::decode(buf)?,
            input: None,
        })
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
    /// The first method that takes `self` beyond its receiver, in another
    /// parameter's type, in the bound of a variable its signature holds, or
    /// where its result takes a value of it in (under a function's
    /// parameter, behind a writable reference): a union cannot implement
    /// such a trait, since a call narrows only the receiver.
    pub fn self_beyond_receiver(&self, env: &Env) -> Option<&TraitMethodDef> {
        self.methods.iter().find(|m| {
            m.typ
                .args
                .iter()
                .enumerate()
                .any(|(i, a)| Some(i) != m.self_index && a.typ.mentions_self())
                || m.typ.vargs.as_ref().is_some_and(|t| t.mentions_self())
                || m.typ.bound_mentions_self()
                || m.typ.rtype.mentions_self_taken_in(env)
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
    /// The definition assertions every body of the method carries, an
    /// implementation's and the default's (`pure`, `sync`).
    pub requires: Arc<[ArcStr]>,
    /// The dispatcher binding at `path::name`, resolved to an
    /// implementation by the self argument's type.
    pub dispatcher: BindId,
    /// The default body's binding, when the method has one and the
    /// declaring implementation has compiled it.
    pub default: Option<BindId>,
}

/// A method as [`Env::deftrait`] declares it.
pub struct TraitMethodSig {
    pub name: ArcStr,
    pub pos: SourcePosition,
    pub typ: Arc<FnType>,
    pub self_index: Option<usize>,
    pub requires: Arc<[ArcStr]>,
}

/// Which trait method a dispatcher binding stands for
/// ([`Env::trait_methods`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Pack)]
#[pack(unwrapped)]
pub struct TraitMethodRef {
    pub trait_id: TraitId,
    pub index: usize,
}

/// Where and how a typedef is declared, beside what it declares.
struct TypeDefSite {
    public: bool,
    doc: Option<ArcStr>,
    pos: SourcePosition,
    decl: SourcePosition,
    ori: Arc<Origin>,
}

fn rep_changed(name: &str, old: &Type) -> String {
    format!(
        "{name} is already defined as Abstract<{old}>: a value of it may still be \
         held, so its representation cannot change"
    )
}

/// Whether a definition's formals are the cells a compile wrote: a block
/// compiled again declares its live definition, whose formals are the
/// first compile's.
fn same_formals(def: &[(TVar, Option<Type>)], written: &[(TVar, Option<Type>)]) -> bool {
    def.len() == written.len()
        && def.iter().zip(written).all(|((a, _), (b, _))| a.same_cell(b))
}

fn non_contractive(name: &str) -> String {
    format!(
        "recursive type {name} refers back to itself through unions and aliases \
         alone, so it names no value; a recursive type must recurse through a \
         structural body (variant, tuple, struct, array, ...)"
    )
}

fn expansive(name: &str, param: &TVar) -> String {
    format!(
        "recursive type {name} grows its parameter {param} as it recurses: {param} \
         comes back wrapped in a constructor, so {name} unfolds to ever larger types \
         and no check over it ends; a recursive type must pass its parameters on \
         unwrapped"
    )
}

fn expansive_through(name: &str, param: &TVar, alias: &ModPath, why: Blind) -> String {
    let why = match why {
        Blind::Late => {
            format!("is not defined above the type that applies it: define {alias} first")
        }
        Blind::Bounded => format!(
            "bounds a parameter of its own, as only an application of {alias} \
             can: write its body out there"
        ),
    };
    format!(
        "recursive type {name} passes its parameter {param} back to itself through \
         {alias}, which {why}"
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
    /// an image: its scope is its own block, or, declared by an
    /// interface, a block every load names alike, so its load's.
    pub(crate) fn same_definition(&self, other: &ImplDef) -> bool {
        self.trait_id == other.trait_id
            && self.scope == other.scope
            && self.declared == other.declared
            && self.pos == other.pos
            && same_origin(&self.ori, &other.ori)
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

/// Why a type name its scope declares has no definition where it is
/// looked up.
enum Unseen {
    /// Its definition is gone.
    Dead,
    /// Its block or module declares it in a statement not compiled yet.
    Later,
}

/// What the statements a list has still to compile may change of the
/// names read through its level, each by its statement's index: a name
/// an explicit `use` imports, a glob, and a module the list declares,
/// whose names a `use` above it may import.
#[derive(Debug, Default)]
pub struct Unsettled {
    pub uses: SmallVec<[(usize, CompactString); 8]>,
    pub globs: SmallVec<[usize; 2]>,
    pub modules: SmallVec<[(usize, CompactString); 8]>,
}

impl Unsettled {
    pub fn is_empty(&self) -> bool {
        self.uses.is_empty() && self.globs.is_empty() && self.modules.is_empty()
    }
}

/// A list's [`Unsettled`] and the index of the first statement that has
/// not begun to compile.
pub type OpenList = (Arc<Unsettled>, usize);

/// The refusal of a name read through a level whose list may still
/// change it ([`Env::unsettled`]).
#[derive(Debug)]
pub struct ChangesLater(CompactString);

impl fmt::Display for ChangesLater {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for ChangesLater {}

/// Where a scope is, for a message: the root is the top level.
fn in_scope(scope: &ModPath) -> CompactString {
    match &*scope.0 {
        "/" | "" => CompactString::const_new("at the top level"),
        _ => format_compact!("in scope {scope}"),
    }
}

/// What the statement list at `scope` is, for a message.
fn scope_kind(scope: &str) -> &'static str {
    if is_block_scope(scope) { "block" } else { "module" }
}

/// Whether `scope` names a body: a path with a generated component, which
/// one compile names and no other.
fn crosses_body(scope: &str) -> bool {
    Path::parts(scope).any(crate::is_block_component)
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

/// What a name means where it is written: the bindings, modules, types
/// and traits each scope declares. A definition and a module keep a
/// snapshot, which [`Env::swap_lexical`] puts in place to compile under.
#[derive(Clone, Debug, Default)]
pub struct Lexical {
    pub binds: Map<ModPath, Map<CompactString, BindId>>,
    pub modules: Set<ModPath>,
    pub typedefs: Map<ModPath, Map<CompactString, TypeDef>>,
    pub traits: Map<ModPath, Map<CompactString, TraitId>>,
    /// The type names each statement list declares (a block, a module's
    /// body, a script), by its scope. A name there is its type before its
    /// declaration too: a block's through `Env::block_types` where an
    /// earlier compile declared it, else it is refused, never an outer
    /// type of the name or one a `use` or a prelude brings. A module's and
    /// a script's are held only while their statements compile.
    pub block_names: Map<ModPath, Map<CompactString, ()>>,
    /// The load of a dynamic module's source this view is inside, whose
    /// own traits, impls and imports it reads before the registries'.
    pub body: Option<Arc<LoadedBody>>,
    /// The top-level input this view was made in.
    pub input: Option<InputHold>,
    /// What the statement lists open when the view was made had still to
    /// declare ([`Env::unsettled`]): a definition checked under it once
    /// its list has compiled reads no module in it as holding what it
    /// holds now.
    pub open: Map<ModPath, OpenList>,
}

impl Lexical {
    /// Whether the two are one snapshot, every name meaning in one what
    /// it means in the other; false says nothing.
    pub fn same_snapshot(&self, other: &Self) -> bool {
        let body = match (&self.body, &other.body) {
            (Some(a), Some(b)) => Arc::ptr_eq(a, b),
            (a, b) => a.is_none() && b.is_none(),
        };
        body && self.binds.ptr_eq(&other.binds)
            && self.modules.ptr_eq(&other.modules)
            && self.typedefs.ptr_eq(&other.typedefs)
            && self.traits.ptr_eq(&other.traits)
            && self.block_names.ptr_eq(&other.block_names)
    }

    /// This view, a definition's, for an instance a call in the load
    /// `site` makes. The instance dispatches as its caller does: it is
    /// inside `site` unless the definition was made in a load `site` is
    /// not inside, where it dispatches by both and names by its own
    /// ([`LoadedBody::join`]).
    pub fn called_in(&self, site: Option<&Arc<LoadedBody>>) -> Self {
        let mut view = self.clone();
        if let Some(site) = site {
            view.body = Some(match &self.body {
                None => site.clone(),
                Some(own) => LoadedBody::join(own, site),
            })
        }
        view
    }
}

/// What one load of a dynamic module's source registered, shared by the
/// lexical view of every definition made in it. The registries name one
/// trait record, one impl of a trait for a type and one import table a
/// scope; a reload gives each to the source that replaces this one, and
/// a load of another instance of the module's function takes the import
/// tables. A function made here still compiles its body against these,
/// whenever it is first called.
#[derive(Debug)]
pub struct LoadedBody {
    /// The load around the module, when it is declared in one.
    outer: Option<Arc<LoadedBody>>,
    /// A join's other loads: a load made for one load's function called
    /// from another that is not inside it, which names as the one and
    /// dispatches as both. It declares nothing of its own, and its
    /// `beside` holds every other load a dispatch in it reads, each once
    /// and none of them a join, however many calls made it.
    beside: Box<[Arc<LoadedBody>]>,
    /// The module's scope: the load's import tables are the ones there
    /// and below.
    scope: ModPath,
    /// Never held across a call into the env: a name looked up under it
    /// reads it again.
    state: RwLock<LoadedState>,
}

#[derive(Debug, Default)]
struct LoadedState {
    /// Each trait it declares, with its own dispatchers and defaults.
    traits: SmallVec<[Arc<TraitDef>; 1]>,
    /// Each impl it writes, by the bindings of the methods it writes.
    impls: SmallVec<[Arc<ImplDef>; 2]>,
    /// The import tables at its module's scope and below as its compile
    /// left them.
    names: Option<Arc<Map<ModPath, ScopeNames>>>,
    /// The dispatchers and impl methods the interfaces of the modules
    /// it declares minted. Their traits and impls stand in the
    /// registries until a reload replaces the load.
    declared: SmallVec<[BindId; 4]>,
    /// Those traits and impls as the registries gave them back, once a
    /// reload replaced it.
    given: Option<Given>,
}

#[derive(Debug, Default)]
struct Given {
    traits: SmallVec<[Arc<TraitDef>; 1]>,
    impls: SmallVec<[Arc<ImplDef>; 2]>,
}

impl LoadedBody {
    pub fn under(outer: Option<Arc<LoadedBody>>, scope: ModPath) -> Arc<Self> {
        Arc::new(Self { outer, beside: Box::default(), scope, state: RwLock::default() })
    }

    /// The load of an instance of a function made in `own`, for a call in
    /// `site`. It is `site` where that is inside `own`, and `own` where
    /// `own` already dispatches as `site` does; else `own` joined with
    /// the loads `site` dispatches by.
    pub fn join(own: &Arc<LoadedBody>, site: &Arc<LoadedBody>) -> Arc<Self> {
        if site.is_within(own) {
            return site.clone();
        }
        if own.dispatches_as(site) {
            return own.clone();
        }
        let base = match own.beside.is_empty() {
            true => own,
            false => own.outer.as_ref().unwrap_or(own),
        };
        let mut beside: Vec<Arc<LoadedBody>> = own.beside.to_vec();
        let site_loads = iter::successors(Some(site), |b| b.outer.as_ref())
            .flat_map(|b| iter::once(b).chain(b.beside.iter()));
        for b in site_loads {
            let known = base.nodes().any(|o| ptr::eq(o, &**b))
                || beside.iter().any(|o| Arc::ptr_eq(o, b));
            if b.beside.is_empty() && !known {
                beside.push(b.clone())
            }
        }
        Arc::new(Self {
            outer: Some(base.clone()),
            beside: beside.into(),
            scope: own.scope.clone(),
            state: RwLock::default(),
        })
    }

    fn chain(&self) -> impl Iterator<Item = &LoadedBody> {
        iter::successors(Some(self), |b| b.outer.as_deref())
    }

    /// The loads a dispatch here reads: the chain of loads, then the ones
    /// joined to it.
    fn nodes(&self) -> impl Iterator<Item = &LoadedBody> {
        self.chain().chain(self.chain().flat_map(|b| b.beside.iter().map(|a| &**a)))
    }

    /// Whether this load is `other` or is declared inside it.
    fn is_within(&self, other: &LoadedBody) -> bool {
        self.chain().any(|b| ptr::eq(b, other))
    }

    /// The first `f` answers over the loads a dispatch here reads.
    fn find_dispatch<T>(
        &self,
        f: &mut impl FnMut(&LoadedBody) -> Option<T>,
    ) -> Option<T> {
        self.nodes().find_map(f)
    }

    /// Whether every load a dispatch in `other` reads is one a dispatch
    /// here reads.
    fn dispatches_as(&self, other: &LoadedBody) -> bool {
        other
            .nodes()
            .filter(|o| o.beside.is_empty())
            .all(|o| self.nodes().any(|b| ptr::eq(b, o)))
    }

    pub fn declare_trait(&self, def: Arc<TraitDef>) {
        self.state.write().traits.push(def)
    }

    pub fn implement(&self, im: Arc<ImplDef>) {
        self.state.write().impls.push(im)
    }

    /// The interface of a module the load declares minted `ids` for
    /// dispatch: its traits' dispatchers and its impls' methods.
    pub fn declare(&self, ids: impl Iterator<Item = BindId>) {
        self.state.write().declared.extend(ids)
    }

    /// A reload is replacing the load, and the registries give back what
    /// the interface of a module it declares registered.
    pub fn give(
        &self,
        traits: impl Iterator<Item = Arc<TraitDef>>,
        impls: impl Iterator<Item = Arc<ImplDef>>,
    ) {
        let new = Given { traits: traits.collect(), impls: impls.collect() };
        let mut st = self.state.write();
        let given = st.given.get_or_insert_default();
        given.traits.extend(new.traits);
        given.impls.extend(new.impls)
    }

    /// The load's source compiled: `names` are its import tables.
    pub fn keep_names(&self, names: Map<ModPath, ScopeNames>) {
        self.state.write().names = Some(Arc::new(names))
    }

    /// The reload that would have replaced the load failed: what its
    /// modules' interfaces declared is the registry's again.
    pub fn revive(&self) {
        self.state.write().given = None
    }

    fn trait_def(&self, id: TraitId) -> Option<Arc<TraitDef>> {
        self.find_dispatch(&mut |b| {
            let st = b.state.read();
            let given = st.given.iter().flat_map(|g| g.traits.iter());
            st.traits.iter().chain(given).find(|t| t.id == id).cloned()
        })
    }

    fn impls(&self, trait_id: TraitId, own: &mut Vec<Arc<ImplDef>>) {
        self.find_dispatch::<()>(&mut |b| {
            let st = b.state.read();
            let given = st.given.iter().flat_map(|g| g.impls.iter());
            let all = st.impls.iter().chain(given);
            for im in all.filter(|im| im.trait_id == trait_id) {
                if !own.iter().any(|o| Arc::ptr_eq(o, im)) {
                    own.push(im.clone())
                }
            }
            None
        });
    }

    /// The table at `level` of the innermost load around it, once that
    /// load's compile has ended.
    fn names_at(&self, level: &str) -> Option<ScopeNames> {
        let own = self
            .chain()
            .find(|b| b.beside.is_empty() && scope_is_under(level, &b.scope))?;
        own.state.read().names.as_ref()?.get(level).cloned()
    }

    /// The bindings a dispatch in this load can reach: its traits'
    /// dispatchers and defaults and its impls' methods. A function made
    /// here keeps them as it keeps what it captures.
    pub fn dispatch_bindings(&self, f: &mut impl FnMut(BindId)) {
        self.find_dispatch::<()>(&mut |b| {
            let st = b.state.read();
            for t in st.traits.iter() {
                for m in t.methods.iter() {
                    f(m.dispatcher);
                    m.default.into_iter().for_each(&mut *f)
                }
            }
            for im in st.impls.iter() {
                im.methods.into_iter().for_each(|(_, id)| f(*id))
            }
            st.declared.iter().copied().for_each(&mut *f);
            None
        });
    }
}

/// A trait declaration deleted while a function value named one of its
/// dispatchers, as the registries held it then, for the view that names
/// those dispatchers ([`Env::kept_trait`]). It goes with the last of them.
#[derive(Debug)]
pub struct KeptTrait {
    /// The record, with the defaults its implementation compiled.
    def: Arc<TraitDef>,
    /// An interface's, and the ones its splice into the body minted.
    dispatchers: SmallVec<[BindId; 4]>,
    /// The impl entries the registry held for the record.
    impls: SmallVec<[Arc<ImplDef>; 2]>,
    /// The typedefs the signatures and the heads name, which a
    /// resolution cell holds only weakly.
    _named: Vec<std::sync::Arc<ResolvedRef>>,
}

impl KeptTrait {
    fn new(
        def: Arc<TraitDef>,
        dispatchers: &[BindId],
        impls: SmallVec<[Arc<ImplDef>; 2]>,
    ) -> Self {
        let mut cells: LPooled<AHashSet<usize>> = LPooled::take();
        let mut defs: LPooled<AHashMap<usize, std::sync::Arc<ResolvedRef>>> =
            LPooled::take();
        for m in def.methods.iter() {
            Type::Fn(m.typ.clone()).named_defs(&mut cells, &mut defs)
        }
        for im in impls.iter() {
            im.target.named_defs(&mut cells, &mut defs);
            for tv in im.params.iter() {
                Type::TVar(tv.clone()).named_defs(&mut cells, &mut defs)
            }
        }
        let _named = defs.drain().map(|(_, d)| d).collect();
        Self { def, dispatchers: SmallVec::from_slice(dispatchers), impls, _named }
    }

    /// The bindings a call through one of the dispatchers reaches: the
    /// defaults and the impls' methods.
    fn reached(&self) -> impl Iterator<Item = BindId> {
        let defaults = self.def.methods.iter().filter_map(|m| m.default);
        let methods = self.impls.iter().flat_map(|im| im.methods.into_iter());
        defaults.chain(methods.map(|(_, id)| *id))
    }
}

#[derive(Clone, Debug, Default)]
pub struct Env {
    pub by_id: TrackedMap<BindId, Arc<Bind>>,
    pub byref_chain: TrackedMap<BindId, BindId>,
    pub lexical: Lexical,
    /// Every scope's explicit namespace (imports + globs), keyed by
    /// scope path. A global registry, not lexical state: it survives
    /// `swap_lexical`, so deferred resolution can consult the
    /// defining module's table. A table at a scope that crosses a body
    /// is read only by a view that holds its input
    /// ([`ScopeNames::input`]).
    pub names: TrackedMap<ModPath, ScopeNames>,
    /// Every Graphix-minted abstract type's representation, global;
    /// visibility is decided at lookup.
    pub abstract_reps: TrackedMap<AbstractId, Arc<AbstractRep>>,
    /// Every trait's definition by identity; global.
    pub trait_defs: TrackedMap<TraitId, Arc<TraitDef>>,
    /// Dispatcher binding → the trait method it names; global.
    pub trait_methods: TrackedMap<BindId, TraitMethodRef>,
    /// Every trait's implementations; global (scope governs only the
    /// trait's name).
    pub impls: TrackedMap<TraitId, Arc<Vec<Arc<ImplDef>>>>,
    /// The deleted trait declarations function values still dispatch
    /// through, by each dispatcher one names.
    pub kept_traits: TrackedMap<BindId, Arc<KeptTrait>>,
    /// Generalized bindings (let-bound lambdas, interface `val`s, trait
    /// dispatchers, and the lets nothing writes that hold a lambda in a
    /// data literal) whose signatures a value occurrence instantiates
    /// afresh. A lambda parameter is never here. Global.
    pub poly_binds: TrackedSet<BindId>,
    /// Registered package names, usable as module path roots from
    /// anywhere. Global.
    pub package_roots: TrackedSet<ArcStr>,
    /// Whether compiles serve an editor, and the IDE side-channels
    /// ([`Ide`]) a check records into.
    pub ide: IdeMode,
    /// The typedefs blocks declare, past their blocks.
    pub block_types: BlockTypes,
    /// What each statement list being compiled has still to bring, by
    /// its scope: a type name is not read through a level a later `use`
    /// or a module declared below may change. Not lexical: a definition
    /// checked under its snapshot once its list has compiled reads what
    /// the list brought.
    #[doc(hidden)]
    pub unsettled: Map<ModPath, OpenList>,
    /// The modules whose undeclared impls this compile task does not
    /// see: its siblings in a run of module checks, which reach each
    /// other only through their interfaces.
    #[doc(hidden)]
    pub hidden_impls: Arc<Vec<ModPath>>,
    /// While recording, each warning printed, as it reaches stderr (a
    /// task's at its join; tasks share their parent's record): a program
    /// image carries its compile's warnings, which a restore prints again
    /// ([`Self::record_warnings`]).
    #[doc(hidden)]
    pub recorded: Arc<WarningRecord>,
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
            kept_traits: self.kept_traits.fork(),
            poly_binds: self.poly_binds.fork(),
            package_roots: self.package_roots.fork(),
            block_types: self.block_types.clone(),
            ide: self.ide.fork(),
            lexical: self.lexical.clone(),
            unsettled: self.unsettled.clone(),
            hidden_impls: self.hidden_impls.clone(),
            recorded: self.recorded.clone(),
        }
    }

    /// Write back what the task `fork` wrote to the global registries.
    #[doc(hidden)]
    pub fn join(&mut self, fork: Self) {
        let Self {
            by_id,
            byref_chain,
            lexical: _,
            names,
            abstract_reps,
            trait_defs,
            trait_methods,
            impls,
            kept_traits,
            poly_binds,
            package_roots,
            block_types: _,
            ide,
            unsettled: _,
            hidden_impls: _,
            recorded: _,
        } = fork;
        self.by_id.join(by_id);
        self.byref_chain.join(byref_chain);
        self.names.join(names);
        self.abstract_reps.join(abstract_reps);
        self.trait_defs.join(trait_defs);
        self.trait_methods.join(trait_methods);
        self.impls.join(impls);
        self.kept_traits.join(kept_traits);
        self.poly_binds.join(poly_binds);
        self.package_roots.join(package_roots);
        self.ide.join(ide, &self.recorded);
    }

    /// A snapshot of the lexical environment, which a definition keeps.
    #[doc(hidden)]
    pub fn lexical(&self) -> Lexical {
        let mut lexical = self.lexical.clone();
        for (level, list) in self.unsettled.iter() {
            lexical.open.insert_cow(level.clone(), list.clone());
        }
        lexical
    }

    /// Exchange the lexical environment with `other`; the global
    /// registries and IDE sinks stay as they are.
    #[doc(hidden)]
    pub fn swap_lexical(&mut self, other: &mut Lexical) {
        mem::swap(&mut self.lexical, other)
    }

    /// Run `f` on the active IDE sink, if any.
    pub fn with_ide(&self, f: impl FnOnce(&mut Ide)) {
        if let Some(ide) = self.ide.sink() {
            f(&mut ide.lock())
        }
    }

    /// A reference written in the source. One a lowering redirected to a
    /// cell of its own is told under the name written there: the binding
    /// that name resolves to, or the cell, when the seq declared the name.
    pub fn push_reference(&self, mut site: ReferenceSite) {
        let Some(sink) = self.ide.sink() else { return };
        if let Some(base) = Path::basename(&site.name.0).filter(|n| is_lowered(n)) {
            let Some(l) = sink.lock().lowered.get(base).cloned() else { return };
            if !l.sites.contains(&site.pos) {
                return;
            }
            site.name = ModPath::from([l.written.as_str()]);
            if l.decl.is_none() {
                match self.lookup_bind(&site.scope, &site.name) {
                    Ok(Some((_, b))) => site.bind_id = b.id,
                    _ => return,
                }
            }
        }
        sink.lock().references.push(site)
    }

    /// The cell `cell` a seq's lowering made stands for `lowered`.
    pub fn note_lowered(&self, cell: ArcStr, lowered: Lowered) {
        self.with_ide(|ide| crate::ide::merge_lowered(&mut ide.lowered, cell, lowered))
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
            None => {
                let warning = format_compact!("WARNING at {pos} {ori}: {message}");
                self.ide.print_warning(warning, &self.recorded)
            }
            Some(ide) => ide.lock().warnings.push(Warning {
                pos,
                end,
                ori: ori.clone(),
                message: format_compact!("{message}").as_str().into(),
            }),
        }
    }

    /// Record each warning printed from now until
    /// [`Self::recorded_warnings`].
    pub fn record_warnings(&self) {
        *self.recorded.lock() = Some(vec![])
    }

    /// The warnings printed since [`Self::record_warnings`], ending the
    /// record.
    pub fn recorded_warnings(&self) -> Vec<ArcStr> {
        self.recorded.lock().take().unwrap_or_default()
    }

    /// Print again warnings a compile printed ([`Self::recorded_warnings`]).
    pub fn replay_warnings(&self, warnings: &[ArcStr]) {
        for w in warnings {
            self.ide.print_warning(CompactString::from(w.as_str()), &self.recorded)
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

    /// Record, for the IDE, that `name`, written at `pos` in `scope`,
    /// names `td`.
    pub fn push_typedef_ref(
        &self,
        td: &ResolvedRef,
        name: &ModPath,
        scope: &ModPath,
        pos: SourcePosition,
        ori: &Arc<Origin>,
    ) {
        if self.ide.is_lsp() {
            self.push_type_ref(TypeRefSite {
                pos,
                ori: ori.clone(),
                scope: scope.clone(),
                name: name.clone(),
                canonical_scope: td.canonical_scope().clone(),
            })
        }
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
            self.lexical.traits.get_or_default_cow(scope).insert_cow(name, tid);
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
            self.lexical.modules.insert_cow(ModPath(Path::from(ArcStr::from(p))));
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
                    if self.lexical.modules.contains(n) {
                        t.unbind_lexical_under(n);
                    } else {
                        let (dir, k) = get_bind_name(n)?;
                        let k = CompactString::from(k);
                        if !self
                            .lexical
                            .binds
                            .get(dir)
                            .is_some_and(|vals| vals.contains_key(&k))
                        {
                            bail!("no value {k} in module {dir} and no module {n}")
                        }
                        if let Some(vals) = t.lexical.binds.get_mut_cow(dir) {
                            vals.remove_cow(&k);
                        }
                    }
                }
                Ok(t)
            }
            // CR claude for eric: [bug/low] What a whitelist leaves of a name a
            // dynamic module's sig imports depends on where the type is declared,
            // and the check accepts each. Under `sandbox whitelist [core]` and `sig {
            // use super::Foo; val f: fn(x: Foo) -> i64 }` the body `let f = |x: Foo|
            // x.a` loads where Foo is a block's type (`let r = { type Foo = ..; let
            // st = mod foo dynamic .. }`, 7). Where Foo is the script's own, every
            // load is `DynamicLoadError("Foo names the type a later statement of
            // this module declares: declare it before its first use")`, though Foo
            // stands above the module: this arm drops the level's
            // `lexical.typedefs` and leaves its `block_names`, which
            // `Env::type_name_at` reads as a type declared below. Where Foo is a
            // function body's, the sig's own spliced `use` fails every load,
            // whatever the body writes: "use: no `Foo` in `#fn..::#do..`". So a
            // sig's `impl Show for C;` over a type of the script cannot be
            // implemented under a whitelist (the body's `impl Show for C` is
            // refused by that message), and `--check` passes all three; `sandbox
            // unrestricted` loads all three. The message is wrong whatever the
            // rule. Whether a sig's import grants the body the name under a
            // whitelist, as a declared impl grants its trait (`Env::grant_trait`),
            // is the sandbox's policy, so Eric's unless a ruling covers it; then the
            // check refuses what no load can take. base-r24 alike. probes:
            // ~/ptmp/cr29/scratch/r24/24e/dynamic-sig-names-at-check-01/{e1_use_above,e4_trait_impl,g2_fn_once,i2_block_whitelist_named,f1_unrestricted}.gx
            // (whitelist-sig-import-01)
            // 2026-11-13 claude: Review of dynamic-sig-names-at-check-01: a sig's import from a package the whitelist leaves out is one more: `sandbox
            // whitelist [core]; sig { use sys::fs::Mode; val f: fn(x: Mode) -> i64; val k: i64 }` checks and every load is "use: no module `sys::fs` in
            // scope" (base-r24 alike), while the path with no import, `val f: fn(x: sys::fs::Mode)`, loads (e9_sandboxed_pkg.gx). The function case fails
            // without a whitelist too where the type stands below the module (sig-import-fn-body-below-01). probe:
            // ~/ptmp/cr29/scratch/r24/24e/dynamic-sig-names-at-check-01/review/r43_wl_import.gx
            // 2026-11-14 claude: Round 25 (25g): base-r25 reproduces all of it (probes copied to ~/ptmp/cr29/scratch/r25/25g/wsi/). Done: the wrong
            // message. Env::apply_sandbox's whitelist arm now drops a dropped scope's block_names with its typedefs (as the blacklist's
            // unbind_lexical_under does), so a body naming a script's type under a whitelist is told "undefined type super::Foo in foo", not "a later
            // statement of this module declares" (pins lang::modules::a_whitelist_hides_the_loaders_type, fails without; twin
            // a_blacklist_shows_the_loaders_type). Not done, and why: the check cannot refuse what no load can take. A refusal at the module's declaration
            // (replaying the sig's use items against the sandboxed view in compile_dynamic) was built and dropped, because the failures come from three
            // places and only some are knowable there: (1) a sig use whose module prefix the whitelist drops (r43_wl_import: use sys::fs::Mode) fails every
            // load in resolve_use_item; (2) a sig use of a name declared in a function body's block (g2_fn_once) fails every load in check_pending_imports,
            // whatever the sandbox (that part is sig-import-fn-body-below-01); (3) a script's type (e1, e4) fails only where the BODY writes the name, and
            // a body that never does (let f = |x| 1 under val f: fn(x: Foo) -> i64) loads today, so a declaration-site refusal loses valid loads; and a
            // block's type (i2) loads only because block types resolve through the global Env::block_types, outside the whitelist. So the verdict is the
            // sandbox's policy, which no ruling covers: should a sig's use of a type grant the body that name under a whitelist, as a declared impl grants
            // its trait (Env::grant_trait, called in compile_dynamic)? Recommendation: yes, grant the typedef (not values, not a module's contents) that
            // the sig imports, keeping its module path nameable without its contents: the loader writes both the sandbox and the sig, a type is inert, e1
            // and e4 (impl Show for C over a script's type) become implementable, and the path form val f: fn(x: sys::fs::Mode) already loads; the
            // alternative leaves a sig's own impl Show for C over a script type unusable under every whitelist. Readdressed to eric.
            Sandbox::Whitelist(wl) => {
                let mut modules: LPooled<AHashSet<ModPath>> = LPooled::take();
                let mut names: LPooled<
                    AHashMap<ModPath, LPooled<AHashSet<CompactString>>>,
                > = LPooled::take();
                for w in wl.iter() {
                    if self.lexical.modules.contains(w) {
                        modules.insert(w.clone());
                    } else {
                        let (dir, n) = get_bind_name(w)?;
                        let dir = ModPath(Path::from(ArcStr::from(dir)));
                        let n = CompactString::from(n);
                        self.lexical.binds.get(&dir).and_then(|v| v.get(&n)).ok_or_else(
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
                t.lexical.typedefs = retain(&self.lexical.typedefs, |k, _| kept(k));
                t.lexical.block_names = retain(&self.lexical.block_names, |k, _| kept(k));
                t.lexical.modules = retain_set(&self.lexical.modules, kept);
                t.lexical.binds = retain(&self.lexical.binds, |k, _| kept(k));
                t.lexical.traits = retain(&self.lexical.traits, |k, _| kept(k));
                for (dir, ns) in names.iter().filter(|(dir, _)| !under_listed(dir)) {
                    if let Some(vals) = t.lexical.binds.get_mut_cow(dir) {
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

    /// The scope a path's leading keywords name from `scope`: `self`,
    /// `package` or a run of `super`; `None` unless `lead` is exactly one.
    pub fn path_anchor<'a>(
        &self,
        scope: &'a str,
        lead: &[&str],
    ) -> Result<Option<&'a str>> {
        Ok(match lead {
            ["self"] => Some(mod_root(scope)),
            ["package"] => Some(self.package_root(scope)),
            [_, ..] if lead.iter().all(|s| *s == "super") => {
                Some(self.super_anchor(scope, lead.len())?)
            }
            _ => None,
        })
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
    /// an error at use). Where `settle`, a name the level does not hold
    /// yet is refused while a statement its list has still to compile may
    /// bring it there ([`Self::unsettled`]): it is never read from a
    /// glob such a statement may shadow, or from outside the level.
    fn lookup_at<T>(
        &self,
        origin: &str,
        level: &str,
        n: &str,
        depth: usize,
        settle: bool,
        f: &mut impl FnMut(&str, &str) -> Option<T>,
    ) -> Result<Option<T>> {
        let inside = scope_is_under(origin, mod_root(level));
        self.lookup_in(origin, level, inside, n, depth, settle, f)
    }

    // CR claude for claude: [risk/medium] A definition's snapshot holds nothing of a
    // module declared below the definition, so in a function body above `mod o` a
    // name a `use` imports from o finds nothing and falls through to an outer type or
    // the prelude's, for good: with `let f = |a: [f64, string]| { let x: Option<f64>
    // = a; x }; mod o; use o::Option` over o's `type Option<'a> = ['a, string]` the
    // body's Option is the prelude's ("Option<f64> does not contain [f64, string]")
    // while f's signature and every statement outside read the import. A typedef in
    // such a body settles its shape on the outer type, and the check's end fills the
    // name to the import (`Env::seed_typedef_refs` runs under the final env): under
    // an outer `type Foo<'a> = Array<'a>`, `{ let f = |a: i64| { type T = [i64,
    // Foo<T>]; let x: T = a; x }; use o::Foo; f(1) }; mod o` over o's `type Foo<'a> =
    // ['a, string]` is accepted and prints 1 with T left `[i64, T, string]`:
    // typedef-use-below-01's door through the snapshot (no hang found; base-r24
    // alike). While the list compiles such a name is refused (`ChangesLater`), but a
    // snapshot has no record that o was to come. An import whose target names nothing
    // in the view (`import_target_exists`) could refuse in place of falling through,
    // once (fn-typedef-unjudged-01) is closed: alone it leaves that T deferred and
    // never judged, and the run does not return. probes:
    // ~/ptmp/cr29/scratch/r24/24e/typedef-use-below-01/{fn-body-import,fn-body-typedef,fn-body-typedef-glob}/
    // (import-snapshot-fallthrough-01)
    // 2026-11-13 claude: Reachable as a wrong value, not only a risk: `let f = |x: Option<f64>| x; mod o; use
    // o::Option; let r = f("s"); let y = select r { null as _ => 0.0, f64 as v => v + 1.0 }; println(y)` over
    // o's `type Option<'a> = ['a, string]` passes `--check` (the call reads the import, the body and its
    // result the prelude's Option), prints 1.0 fused and logs "no arm matches "s": a coverage hole in the
    // check" under `--no-fusion`; base-r24 alike. With `mod o` above f the select is refused as it should be
    // (lang::types::lambda_parameter_above_the_use_reads_the_import pins that order). probe:
    // ~/ptmp/cr29/scratch/r24/24e/typedef-use-below-01/review/fn-above-mod-wrong-value/
    // 2026-11-14 claude: r25 lane 25g: a definition's snapshot now keeps the lists open when it was made
    // (Lexical::open, set by Env::lexical; Env::unsettled_module reads it beside Env::unsettled), so a name a
    // use or a glob imports from a module declared below the definition is refused with the list's own
    // ChangesLater message instead of falling through to the outer or prelude type. Closed: fn-body-import,
    // review/fn-above-mod-wrong-value (the wrong value), the glob twin. Pins:
    // lang::types::lambda_parameter_above_the_module_it_imports_from_refused,
    // lambda_body_annotation_above_the_module_it_imports_from_refused,
    // lambda_parameter_above_the_module_a_glob_imports_from_refused (12 runs fail without the fix), twins
    // lambda_parameter_above_the_module_of_a_glob_reads_the_import and
    // lambda_above_a_module_imports_nothing_it_names. STILL OPEN: fn-body-typedef and fn-body-typedef-glob.
    // The name in a typedef a function body declares is now ChangesLater, T is deferred and never judged
    // (fn-typedef-unjudged-01), and the run does not return (killed by the memory cap; --check passes):
    // before this change it printed 1 over a wrong T. Closing fn-typedef-unjudged-01 closes them; re-run both
    // probes then. probes: ~/ptmp/cr29/scratch/r25/25g/import-snapshot/
    // 2026-11-14 claude: r25 lane 25g review: the fix holds. Pins fail without Env::lexical's fill (12 of 28
    // runs of lang::types::lambda_ fail); census base vs lane 0 changes of 2967 files; guicheck 39.724G
    // base-r25, 39.833G lane. Probes one step over refuse as the list's own rule does: a block above the mod,
    // a nested closure, a path through a sibling module declared below (module a's 'use package::o::Option'
    // with 'mod o' after 'mod a' in the root: base accepted and printed the right value only because the
    // signature read the import and the body's annotation did not), 'use o::{self, Option}', 'use o as
    // Alias'; each had a body reading the prelude's Option over a signature reading the import. The remaining
    // part closes with fn-typedef-unjudged-01's fix (lane 25f, faf1c656c): applied to this lane in a scratch
    // tree, fn-body-typedef and fn-body-typedef-glob are both refused as non-contractive (T = [i64, Foo<T>]
    // over o's ['a, string]) and fn-body-import is still refused as above. Until 25f lands, those two probes
    // hang (base-r25 printed 1). Delete this CR once both lanes are on r25-staging and the two probes are
    // re-run (a pin for the pair belongs in whichever lane lands second). Probes:
    // ~/ptmp/cr29/scratch/r25/25g/review/.
    /// [`Self::lookup_at`], with whether `origin` is inside the level's
    /// module already known: one chain's levels share their module.
    fn lookup_in<T>(
        &self,
        origin: &str,
        level: &str,
        inside: bool,
        n: &str,
        depth: usize,
        settle: bool,
        f: &mut impl FnMut(&str, &str) -> Option<T>,
    ) -> Result<Option<T>> {
        if let Some(t) = f(level, n) {
            return Ok(Some(t));
        }
        if !inside {
            return Ok(None);
        }
        let later = match settle && !self.unsettled.is_empty() {
            true => self.unsettled.get(level).map(|(l, from)| (&**l, *from)),
            false => None,
        };
        let kind = || scope_kind(level);
        if let Some((l, from)) = later
            && l.uses.iter().any(|(i, k)| *i >= from && k.as_str() == n)
        {
            return Err(anyhow!(ChangesLater(format_compact!(
                "{n} names what a later `use` of this {kind} imports: write the \
                 `use` before the name's first use",
                kind = kind()
            ))));
        }
        let kept = self.lexical.body.as_ref().and_then(|b| b.names_at(level));
        let mut later_module = None;
        if let Some(sn) = kept.as_ref().or_else(|| self.names.get(level)) {
            debug_assert!(
                sn.held_by(self.lexical.input.as_ref()),
                "{n} is looked up from {origin} through the import table at \
                 {level} by a view made in another input"
            );
            if depth > MAX_IMPORT_DEPTH {
                bail!("import chain too deep resolving `{n}` (import cycle?)")
            }
            if let Some(e) = sn.imports.get(n) {
                let hit = if e.keyword_anchored {
                    self.chain_lookup(origin, &e.scope, &e.name, depth + 1, settle, f)?
                } else {
                    self.lookup_at(origin, &e.scope, &e.name, depth + 1, settle, f)?
                };
                if let Some(t) = hit {
                    e.used.store(true, Ordering::Relaxed);
                    return Ok(Some(t));
                }
                if settle
                    && !e.keyword_anchored
                    && let Some(m) = self.unsettled_module(&e.scope)
                {
                    return Err(anyhow!(ChangesLater(format_compact!(
                        "{n} names what a `use` of this {kind} imports from module \
                         {}, which a later statement declares: declare the module \
                         before the name's first use",
                        written_level(m),
                        kind = kind()
                    ))));
                }
                // A kind-miss on an import falls through to globs
                // (`use gui::text::{self, *}`).
            }
            let mut found: Option<(&str, T)> = None;
            for g in sn.globs.iter() {
                if settle
                    && let Glob::Module(m) = g
                    && let Some(m) = self.unsettled_module(m)
                {
                    later_module = Some(m);
                    continue;
                }
                if let Some((at, t)) = g.find(n, f) {
                    match &found {
                        None => found = Some((at, t)),
                        Some((first, _)) if *first == at => (),
                        Some((first, _)) => bail!(
                            "`{n}` is ambiguous: both {} and {} provide it; \
                             import one explicitly",
                            written_level(first),
                            written_level(at)
                        ),
                    }
                }
            }
            if let Some((_, t)) = found {
                return Ok(Some(t));
            }
        }
        if let Some((l, from)) = later
            && l.globs.iter().any(|i| *i >= from)
        {
            return Err(anyhow!(ChangesLater(format_compact!(
                "{n} may name what a later glob `use` of this {kind} imports: write \
                 the `use` before the name's first use",
                kind = kind()
            ))));
        }
        if let Some(m) = later_module {
            return Err(anyhow!(ChangesLater(format_compact!(
                "{n} may name what a glob `use` of this {kind} imports from module \
                 {}, which a later statement declares: declare the module before \
                 the name's first use",
                written_level(m),
                kind = kind()
            ))));
        }
        Ok(None)
    }

    /// Whether the list being compiled at `level` declares a module named
    /// `name`, and registered its path before its statements compiled.
    pub fn list_declares_module(&self, level: &str, name: &str) -> bool {
        self.unsettled
            .get(level)
            .is_some_and(|(l, _)| l.modules.iter().any(|(_, k)| k.as_str() == name))
    }

    /// The module at or around `m` that a list being compiled declares
    /// in a statement it has not reached: nothing it holds is known yet.
    fn unsettled_module<'a>(&self, m: &'a str) -> Option<&'a str> {
        if self.unsettled.is_empty() && self.lexical.open.is_empty() {
            return None;
        }
        let mut at = m;
        while at != "/" {
            let parent = Path::dirname(at).unwrap_or("/");
            let name = Path::basename(at)?;
            if let Some((l, from)) =
                self.unsettled.get(parent).or_else(|| self.lexical.open.get(parent))
                && l.modules.iter().any(|(i, k)| i >= from && k.as_str() == name)
            {
                return Some(at);
            }
            at = parent;
        }
        None
    }

    /// [`Self::lookup_at`] over every level from `from` up to its
    /// module root, inclusive.
    fn chain_lookup<T>(
        &self,
        origin: &str,
        from: &str,
        n: &str,
        depth: usize,
        settle: bool,
        f: &mut impl FnMut(&str, &str) -> Option<T>,
    ) -> Result<Option<T>> {
        let inside = scope_is_under(origin, mod_root(from));
        for level in chain_levels(from) {
            if let Some(t) = self.lookup_in(origin, level, inside, n, depth, settle, f)? {
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
        self.lexical.modules.get(p.as_str()).cloned()
    }

    /// Resolve the single segment `seg` as a module from `scope`: the
    /// lexical chain, then the package prelude, then the core prelude.
    fn resolve_module_seg(
        &self,
        scope: &str,
        seg: &str,
        settle: bool,
    ) -> Result<Option<ModPath>> {
        let mut f = |lvl: &str, n: &str| self.module_at(lvl, n);
        if let Some(p) = self.chain_lookup(scope, scope, seg, 0, settle, &mut f)? {
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
        settle: bool,
    ) -> Result<Option<ModPath>> {
        self.lookup_at(origin, cur, seg, 0, settle, &mut |lvl, n| self.module_at(lvl, n))
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
        f: impl FnMut(&str, &str) -> Option<T>,
    ) -> Result<Option<T>> {
        self.resolve_path(scope, name, ns, false, f)
    }

    /// [`Self::resolve_visible`]; where `settle`, a name a statement not
    /// compiled yet may change at a level on the way is refused there
    /// ([`Self::lookup_at`]).
    fn resolve_path<T>(
        &self,
        scope: &ModPath,
        name: &ModPath,
        ns: NameNs,
        settle: bool,
        mut f: impl FnMut(&str, &str) -> Option<T>,
    ) -> Result<Option<T>> {
        let parts: LPooled<Vec<&str>> = Path::parts(&**name).collect();
        let Some((&base, _)) = parts.split_last() else { return Ok(None) };
        let n_super = parts.iter().take_while(|s| **s == "super").count();
        // A bare `self` in value position is the receiver binding;
        // only `self::x` is the path keyword.
        if parts.len() == 1 && base == "self" && ns == NameNs::Value {
            return self.chain_lookup(scope, scope, base, 0, settle, &mut f);
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
        let anchor: &str = match self.path_anchor(scope, &parts[..lead])? {
            Some(anchor) => anchor,
            None if parts.len() == 1 => {
                if let Some(t) =
                    self.chain_lookup(scope, scope, base, 0, settle, &mut f)?
                {
                    return Ok(Some(t));
                }
                if ns == NameNs::Module && self.package_roots.contains(base) {
                    if let Some(t) = f("/", base) {
                        return Ok(Some(t));
                    }
                }
                return Ok(f("/core", base));
            }
            None => match self.resolve_module_seg(scope, parts[0], settle)? {
                Some(m) => {
                    let m = self.descend(scope, m, &interior[1..], settle)?;
                    return self.lookup_at(scope, &m, base, 0, settle, &mut f);
                }
                None => return Ok(None),
            },
        };
        // `super::x` resolves along the anchor's own chain (a super
        // anchor may be a block level). No preludes for keyword roots.
        if interior.is_empty() {
            return self.chain_lookup(scope, anchor, base, 0, settle, &mut f);
        }
        let first = match self.chain_lookup(
            scope,
            anchor,
            interior[0],
            0,
            settle,
            &mut |lvl, n| self.module_at(lvl, n),
        )? {
            Some(m) => m,
            None => bail!("no module `{}` in `{anchor}`", interior[0]),
        };
        let m = self.descend(scope, first, &interior[1..], settle)?;
        self.lookup_at(scope, &m, base, 0, settle, &mut f)
    }

    /// Walk `segs` down from module `cur`, erroring on a missing
    /// step.
    fn descend(
        &self,
        origin: &ModPath,
        cur: ModPath,
        segs: &[&str],
        settle: bool,
    ) -> Result<ModPath> {
        let mut m = cur;
        for seg in segs {
            match self.descend_step(origin, &m, seg, settle)? {
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
            self.lexical.binds.get_full(scope).and_then(|(scope, vars)| {
                vars.get(name)
                    .and_then(|bid| self.by_id.get(bid).map(|bind| (scope, &**bind)))
            })
        })
    }

    /// The typedef a name in type position means at `scope`, as
    /// [`Self::resolve_type_name`] reads it; `None` where it names a trait
    /// or nothing.
    pub fn lookup_typedef(
        &self,
        scope: &ModPath,
        name: &ModPath,
    ) -> Result<Option<std::sync::Arc<ResolvedRef>>> {
        Ok(match self.resolve_type_name(scope, name)? {
            Some(TypeName::Def(d)) => Some(d),
            Some(TypeName::Trait(_)) | None => None,
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
            self.lexical.traits.get(scope).and_then(|m| m.get(name)).copied()
        })
    }

    /// Whether `r` is core's `ErrChain`, under whatever name it was
    /// reached.
    pub(crate) fn is_core_errchain(&self, r: &ResolvedRef) -> bool {
        static CORE: std::sync::LazyLock<ModPath> =
            std::sync::LazyLock::new(|| ModPath::from(["core"]));
        self.lexical
            .typedefs
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

    /// [`Self::trait_of_ref`] for a reader that takes a trait for a bound
    /// as it compiles, where a type waits for the check: a name that means
    /// a trait as the scopes stand and that a statement not compiled yet
    /// may change is refused ([`Self::open_list`]).
    pub fn settled_trait_of_ref(&self, tr: &TypeRef) -> Result<Option<TraitId>> {
        if let Some(id) = tr.trait_id() {
            return Ok(Some(id));
        }
        if tr.resolved().is_some() {
            return Ok(None);
        }
        let e = match self.resolve_type_name(&tr.scope, &tr.name) {
            Ok(Some(TypeName::Trait(id))) => return Ok(Some(id)),
            Ok(_) => return Ok(None),
            Err(e) if e.is::<ChangesLater>() => e,
            Err(_) => return Ok(None),
        };
        let stands =
            self.resolve_path(&tr.scope, &tr.name, NameNs::Type, false, |s, n| {
                self.type_name_at(s, n)
            });
        match stands {
            Ok(Some(Ok(TypeName::Trait(_)))) => Err(e),
            _ => Ok(None),
        }
    }

    /// What a name in type position means at `scope`: the first typedef
    /// or trait of that name along the lookup chain, so a closer one of
    /// either kind shadows a farther one of the other. A name a statement
    /// not compiled yet may change is refused: one its list declares
    /// below, and one a later `use` or a module declared below may bring
    /// to a level on the way ([`Self::open_list`]).
    pub(crate) fn resolve_type_name(
        &self,
        scope: &ModPath,
        name: &ModPath,
    ) -> Result<Option<TypeName>> {
        let mut kind = "";
        let hit = self.resolve_path(scope, name, NameNs::Type, true, |s, n| {
            let hit = self.type_name_at(s, n)?;
            kind = scope_kind(s);
            Some(hit)
        })?;
        match hit {
            None => Ok(None),
            Some(Ok(t)) => Ok(Some(t)),
            Some(Err(Unseen::Dead)) => {
                bail!("type `{name}` outlived its definition in `{scope}`")
            }
            Some(Err(Unseen::Later)) => bail!(
                "{name} names the type a later statement of this {kind} declares: \
                 declare it before its first use"
            ),
        }
    }

    /// The typedef or trait `n` names at the one level `s`.
    fn type_name_at(&self, s: &str, n: &str) -> Option<Result<TypeName, Unseen>> {
        if let Some(d) = self.lexical.typedefs.get(s).and_then(|m| m.get(n)) {
            return Some(Ok(TypeName::Def(d.def.clone())));
        }
        if let Some(id) = self.lexical.traits.get(s).and_then(|m| m.get(n)) {
            return Some(Ok(TypeName::Trait(*id)));
        }
        let later = match self.lexical.block_names.get(s) {
            Some(names) if names.get(n).is_none() => return None,
            Some(_) => Some(Err(Unseen::Later)),
            None => None,
        };
        if !is_block_scope(s) {
            return later;
        }
        match self.block_types.find(s, n) {
            Some(BlockHit::Live(def)) => Some(Ok(TypeName::Def(def))),
            Some(BlockHit::Dead) => Some(Err(Unseen::Dead)),
            None => later,
        }
    }

    /// Where the written name `tr` means a type its block or module
    /// declares below it while the name means something else at that
    /// scope's door (an outer type or trait, an import, a prelude's): what
    /// its reader is told. A name already resolved to a trait or to
    /// another definition means that one: no note.
    pub fn later_block_type_note(&self, tr: &TypeRef) -> Option<CompactString> {
        let (Some(pos), Some(ori)) = (tr.pos, &tr.ori) else { return None };
        if tr.trait_id().is_some() {
            return None;
        }
        let mut level = CompactString::default();
        let hit = self
            .resolve_visible(&tr.scope, &tr.name, NameNs::Type, |s, n| {
                let hit = self.type_name_at(s, n)?;
                level = CompactString::from(s);
                Some(hit)
            })
            .ok()??;
        let named = tr.def_key();
        let declared = match hit {
            Err(Unseen::Later) if named.is_none() => None,
            Ok(TypeName::Def(d))
                if same_origin(d.ori(), ori)
                    && d.pos() > pos
                    && named.is_none_or(|k| k == d.def_key()) =>
            {
                Some(d.pos())
            }
            Ok(_) | Err(Unseen::Later | Unseen::Dead) => return None,
        };
        let other = self
            .resolve_visible(&tr.scope, &tr.name, NameNs::Type, |s, n| {
                if s == level { None } else { self.type_name_at(s, n)?.ok() }
            })
            .ok()??;
        let (opos, oori) = match &other {
            TypeName::Def(o) => (o.pos(), o.ori().clone()),
            TypeName::Trait(id) => {
                let t = self.trait_def(*id)?;
                (t.pos, t.ori.clone())
            }
        };
        let at = declared.map(|p| format_compact!(", at {p}")).unwrap_or_default();
        let elsewhere = match same_origin(&oori, ori) {
            true => CompactString::default(),
            false => format_compact!(" in {}", oori.source),
        };
        let name = &tr.name;
        let kind = scope_kind(&level);
        Some(format_compact!(
            "`{name}` here names the type this {kind} declares below it{at}, not the \
             `{name}` declared at {opos}{elsewhere}: a name a {kind} declares is its \
             type throughout the {kind}"
        ))
    }

    /// Declare trait `name` in `scope`: one dispatcher per method at
    /// `scope::name::method`, recorded in `trait_methods`, and the
    /// definition registered globally. The first registration of an
    /// identity is the definition of record.
    pub fn deftrait(
        &mut self,
        scope: &ModPath,
        name: &ArcStr,
        methods: impl Iterator<Item = TraitMethodSig>,
        doc: Option<ArcStr>,
        pos: SourcePosition,
        ori: Arc<Origin>,
    ) -> Result<Arc<TraitDef>> {
        if self.lexical.traits.get(scope).and_then(|m| m.get(name.as_str())).is_some() {
            bail!("trait {name} is already defined {}", in_scope(scope))
        }
        if self.lexical.typedefs.get(scope).and_then(|m| m.get(name.as_str())).is_some() {
            bail!("{name} is already defined as a type {}", in_scope(scope))
        }
        let id = TraitId::of(scope, name);
        let path = ModPath(scope.append(name));
        if self.lexical.modules.contains(&path) {
            bail!("{name} is already defined as a module {}", in_scope(scope))
        }
        self.lexical.modules.insert_cow(path.clone());
        let mut defs: LPooled<Vec<TraitMethodDef>> = LPooled::take();
        let (mut applied, mut bare) = (false, false);
        for TraitMethodSig { name: mname, pos: mpos, typ, self_index, requires } in
            methods
        {
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
                requires,
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
        self.lexical
            .traits
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

    /// Take the declaration `def` out of scope and out of the record.
    /// Its dispatchers stay bound: whoever deletes the declaration
    /// decides whether a value still names them.
    pub fn undeftrait(&mut self, def: &Arc<TraitDef>) {
        if let Some(m) = self.lexical.traits.get_mut_cow(&def.scope) {
            m.remove_cow(&CompactString::from(def.name.as_str()));
            if m.len() == 0 {
                self.lexical.traits.remove_cow(&def.scope);
            }
        }
        self.lexical.modules.remove_cow(&def.path);
        self.unrecord_trait(def)
    }

    /// Give the definition of record back where `def` is it, so another
    /// declaration of the trait registers as the first did.
    pub fn unrecord_trait(&mut self, def: &Arc<TraitDef>) {
        if self.trait_defs.get(&def.id).is_some_and(|d| d.same_definition(def)) {
            self.trait_defs.remove(&def.id);
        }
    }

    /// `def` as the record holds it, with the defaults its implementation
    /// compiled, where `def` is the definition of record.
    pub fn trait_of_record(&self, def: &Arc<TraitDef>) -> Arc<TraitDef> {
        match self.trait_defs.get(&def.id) {
            Some(rec) if rec.same_definition(def) => rec.clone(),
            Some(_) | None => def.clone(),
        }
    }

    /// The trait `id` as this view sees it: the declaration of the load
    /// it is inside, else the deleted declaration it names, else the
    /// definition of record.
    pub fn trait_def(&self, id: TraitId) -> Option<Arc<TraitDef>> {
        let own = self.lexical.body.as_ref().and_then(|b| b.trait_def(id));
        own.or_else(|| self.kept_trait(id).map(|k| k.def.clone()))
            .or_else(|| self.trait_defs.get(&id).cloned())
    }

    /// The deleted declaration of the trait `id` this view names: the
    /// one whose dispatchers its bindings at the trait's path are.
    fn kept_trait(&self, id: TraitId) -> Option<&Arc<KeptTrait>> {
        if self.kept_traits.len() == 0 {
            return None;
        }
        let named = |k: &KeptTrait| {
            k.def.methods.first().is_some_and(|m| {
                let binds = self.lexical.binds.get(&k.def.path);
                let own = binds.and_then(|b| b.get(m.name.as_str()));
                own.is_some_and(|id| k.dispatchers.contains(id))
            })
        };
        let mut kept = self.kept_traits.iter().map(|(_, k)| k);
        kept.find(|k| k.def.id == id && named(k))
    }

    // CR claude for claude: [bug/low] A function that outlives the declaration of a
    // trait it names only as a bound binds at no new call site. A declaration is
    // kept under the dispatchers a function value names (`Env::keep_trait`,
    // escaped-fn-dropped-trait-01), and a function that writes the trait as a
    // parameter's type or a variable's bound and calls no method names none, as
    // does every function over a trait with no methods. Only an embedder reaches
    // it: over a root `let saved: fn(x: i64) -> i64 = never()`, compile `{ type C =
    // Abstract<i64>; trait Sh { val sh: fn(self) -> i64 }; impl Sh for C { let sh =
    // |c: C| c.0 * 10 }; saved <- |x: i64| { let f = |v: Sh| 30; f(C(x)) }; 1 }`,
    // drop it, then compile `saved(3)`: it logs "binding the callee failed: .. an
    // instance at fn(v: C) -> i64 of a definition typed fn(v: '#v: unbound within
    // Sh) -> i64" and is bottom (30 while the block lives). The same with `'a: Sh
    // |v: 'a| 30`, and with `trait Mk {}; impl Mk for C {}` and `|v: Mk| 30`. To
    // fix: a function value keeps the declarations of the traits its types name,
    // not only of the dispatchers its body names. Its definition's check resolves
    // every such bound, so the definition can record the traits and pin a
    // dispatcher of each; a trait with no method has no dispatcher, so it needs a
    // holder of another kind (the declaration kept under the function's
    // definition, as a load keeps its own in `LoadedBody`).
    // escaped-fn-dropped-impl-01 wants such a holder too: fix them together.
    // probe: `rprobe_bound_only`, `rprobe_bound_tvar` and `rprobe2_marker` in
    // ~/ptmp/cr29/scratch/r24/24e/escaped-fn-dropped-trait-01/review/probes_rev.rs
    // (escaped-fn-dropped-bound-01)
    /// The declaration `def` is about to be deleted and function values
    /// name `named` among its `dispatchers`: each of those keeps the
    /// record and the impl entries the registry holds for it, and holds
    /// the bindings a call through it reaches ([`Lives::hold`]), until
    /// [`Self::release_dispatcher`]. A declaration that is not the record
    /// is kept elsewhere: a load's, given back at its reload, by the load
    /// ([`LoadedBody`]), and an interface's splice into its body with the
    /// interface's.
    pub fn keep_trait(
        &mut self,
        lives: &Lives,
        def: &Arc<TraitDef>,
        dispatchers: &[BindId],
        named: &[BindId],
    ) {
        if named.is_empty() {
            return;
        }
        let kept = match named.iter().find_map(|id| self.kept_traits.get(id)) {
            Some(k) => k.clone(),
            None => {
                let record = self.trait_defs.get(&def.id);
                let Some(record) = record.filter(|r| r.same_definition(def)) else {
                    return;
                };
                let registered = self.impls.get(&def.id);
                let impls = registered.iter().flat_map(|l| l.iter()).cloned().collect();
                Arc::new(KeptTrait::new(record.clone(), dispatchers, impls))
            }
        };
        for id in named {
            if self.kept_traits.get(id).is_none() {
                lives.hold(*id, kept.reached());
                self.kept_traits.insert(*id, kept.clone());
            }
        }
    }

    /// The dispatcher `id` is gone, and no value names it: what it kept
    /// of its declaration goes when the last of that declaration's does.
    pub fn release_dispatcher(&mut self, id: BindId) {
        self.trait_methods.remove(&id);
        self.kept_traits.remove(&id);
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

    /// Put `new` in the place of the registered `old`, the same
    /// definition.
    pub fn replace_impl(&mut self, old: &Arc<ImplDef>, new: Arc<ImplDef>) {
        let Some(mut list) = self.impls.get(&old.trait_id).cloned() else { return };
        for o in Arc::make_mut(&mut list).iter_mut().filter(|o| o.same_definition(old)) {
            *o = new.clone()
        }
        self.impls.insert(old.trait_id, list);
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

    /// Whether the abstract type `id` has an implementation of `t`.
    pub fn implements(&self, t: CoreTrait, id: AbstractId) -> bool {
        self.impls_of(t.id()).is_some_and(|l| {
            l.iter().any(|im| self.head_abstract(&im.target) == Some(id))
        })
    }

    /// Whether the abstract type `id` crosses data: it has a `Cast`
    /// implementation.
    pub fn casts(&self, id: AbstractId) -> bool {
        self.implements(CoreTrait::Cast, id)
    }

    /// Whether the abstract type `id` is written as its representation:
    /// its `Cast` implementation leaves `to_value` out.
    pub fn written_as_representation(&self, id: AbstractId) -> bool {
        self.cast_leaves_out(id, "to_value")
    }

    /// Whether the abstract type `id` is read as its representation: its
    /// `Cast` implementation leaves `from_value` out.
    pub fn read_as_representation(&self, id: AbstractId) -> bool {
        self.cast_leaves_out(id, "from_value")
    }

    fn cast_leaves_out(&self, id: AbstractId, method: &str) -> bool {
        self.impls_of(CoreTrait::Cast.id()).is_some_and(|l| {
            l.iter().any(|im| {
                self.head_abstract(&im.target) == Some(id)
                    && !im.methods.contains_key(method)
            })
        })
    }

    /// The abstract type an impl head names, through its typedef: two
    /// heads of different abstract types never overlap.
    fn head_abstract(&self, t: &Type) -> Option<AbstractId> {
        if let Some(id) = t.resolved_abstract() {
            return Some(id);
        }
        match t {
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
        let list = self.impls_all(trait_id)?;
        if !list.iter().any(|im| self.impl_hidden(im)) {
            return Some(list);
        }
        Some(Arc::new(list.iter().filter(|im| !self.impl_hidden(im)).cloned().collect()))
    }

    // CR claude for eric: [bug/low] A retired load's own impls reach only the
    // calls its own text writes. A function outside the module that a retired
    // body's function calls is instantiated in its definition's view, which
    // carries no load, so its dispatch reads the registry. `Sh::e(C(x))` in the
    // first body, where `Sh` is declared outside, `e` is its default `|s| sh(s)
    // + 5` and each body implements `sh` for its own `C`, runs the SECOND
    // body's `sh` where it is first called after the reload: h1.gx prints
    // `alive=20` and `late=[2019, 2020]` at ep 14, the instance alive across the
    // reload on the first body's `sh` and the ones `late` binds on the second's.
    // An outside `let app = |x: Sh| Sh::sh(x) + 5` called from the body does the
    // same (h2.gx, the same two lines). The core traits are the fault by another
    // road: the runtime consults Eq, Ord, Display and Cast by a value's abstract
    // id, which the two bodies' `C` share, so the first body's `impl Eq for C`
    // stops deciding its own `C(x) == C(x + 1)` at the reload, in the instance
    // alive across it too (p1.gx: `alive` is ep + 1 to ep 6 and ep + 5000 from
    // 7, the second body declaring no `C`). base-r23 alike, all three. Ruling
    // D33 (a) is that a reload does not change what a saved function means. To
    // fix: an instance made for a call in a loaded view takes that view's load
    // where its definition has none (first check what shares an instance by
    // definition and signature), and the core hooks either are kept by load or
    // a load's abstract types are its own, which may need a ruling. probes:
    // ~/ptmp/cr29/scratch/r23/23e/retired-body-trait-dispatch-01/rev/{h1,h2,p1}.gx,
    // `graphix --no-cache` (retired-body-outside-dispatch-01)
    // 2026-11-14 claude: The dispatch half is fixed: an instance is made in the
    // load of its call (Lexical::called_in; the point of a call carries its load in
    // Scope::load, for the binds, slots and builtin callbacks made in a later
    // cycle), so h1.gx and h2.gx print late=[20, 21] at ep 14. Pins:
    // lang::modules::a_function_saved_across_a_reload_dispatches_an_outside_function_to_its_own_impl
    // and the ten beside it (default, i64 head, run-time bind, shared kernels,
    // collection callback, builtin callback, labeled default, union self, escaping
    // closure, nested reload),
    // lib_tests::leaks::a_saved_function_calling_an_outside_function_keeps_nothing_once_dropped;
    // 36 runs of the first nine fail with called_in undone. A function made in
    // another module's load is cross-load-dispatch-01. What remains is the core
    // traits (p1.gx, unchanged: alive is ep + 5000 from ep 7), and it wants Eric's
    // ruling, since it is a rule about a type's identity across reloads. The
    // runtime finds Eq, Ord, Display and Cast by a value's abstract id alone
    // (Value's own eq and cmp, sorts, map keys, printing and casts reach no view),
    // and the two bodies' C are one id (AbstractId::declared is the module's path
    // and the name). Options: (a) leave it, a value compares and prints by the impl
    // registered now as design/module_system.md says, and D33 (a) covers bindings
    // and user traits only; (b) an abstract type a dynamic module's body declares
    // and its interface does not is its load's own (the id takes the load), its
    // core impls stay registered while a function of that load lives, and a type
    // the interface declares stays one type across reloads, compared by the impl
    // registered now; (c) lower ==, the orderings and printing to the view's impl
    // where the type is written, which leaves sorts, map keys and nested values on
    // the registry, so one body would compare one type two ways. Recommendation:
    // (b), the only one under which a retired body means what it meant; a type the
    // interface does not declare has no name outside its load. Readdressed to eric;
    // not yet an item in ~/ptmp/cr-discuss.md.
    // 2026-11-14 claude: Reviewed: the dispatch half holds. Probed one step over (a seq step and a
    // seqq, a queuefn wrapper, an outside recursion, an explicit bound, a file module's function, an
    // outside generic impl's method, a struct of functions, a second body that implements nothing),
    // fused, --no-fusion, forked, NO_SUBST, ELAB_AUDIT and from a cached image: every one right.
    // Three more pins in lang::modules
    // (.._binds_an_outside_function_the_next_body_implements_nothing_for,
    // .._an_outside_impls_method_to_its_own_impl, .._a_seq_steps_call_to_its_own_impl); with the
    // fixer's eleven, 56 runs fail with called_in undone. Found beside it:
    // orphan-fn-late-bind-defuse-01, reload-same-fn-impl-01, callable-scope-root-01. The core traits
    // half still waits for Eric.
    // 2026-11-14 claude: The core-traits half is item D38 in ~/ptmp/cr-discuss.md (Open).
    /// Every impl of `trait_id`, hidden ones included (see
    /// [`Self::impl_hidden`]). Inside a load of a dynamic module's
    /// source the impls it writes come first, in place of the registered
    /// ones of their heads: its own entry, the interface's declaration it
    /// fulfils, or the entry of the source that replaced it. So do the
    /// ones kept with a deleted declaration the view names.
    pub(crate) fn impls_all(&self, trait_id: TraitId) -> Option<Arc<Vec<Arc<ImplDef>>>> {
        let registered = self.impls.get(&trait_id);
        let kept = self.kept_trait(trait_id);
        if self.lexical.body.is_none() && kept.is_none() {
            return registered.cloned();
        }
        let mut own: LPooled<Vec<Arc<ImplDef>>> = LPooled::take();
        if let Some(body) = &self.lexical.body {
            body.impls(trait_id, &mut own)
        }
        if let Some(k) = kept {
            own.extend(k.impls.iter().cloned())
        }
        if own.is_empty() {
            return registered.cloned();
        }
        let heads: LPooled<Vec<Option<AbstractId>>> =
            own.iter().map(|im| self.head_abstract(&im.target)).collect();
        let stands_for = |r: &Arc<ImplDef>| {
            let head = self.head_abstract(&r.target);
            own.iter().any(|o| Arc::ptr_eq(o, r))
                || (head.is_some() && heads.contains(&head))
        };
        let rest =
            registered.into_iter().flat_map(|l| l.iter()).filter(|r| !stands_for(r));
        Some(Arc::new(own.iter().chain(rest).cloned().collect()))
    }

    /// An impl a sibling module's body added undeclared, hidden from the
    /// other siblings' checks.
    pub(crate) fn impl_hidden(&self, im: &ImplDef) -> bool {
        !im.declared && self.in_sibling(&im.scope)
    }

    /// Whether `scope` is in a sibling module of the one this task checks
    /// ([`Self::hidden_impls`]), whose check this one does not read.
    pub fn in_sibling(&self, scope: &ModPath) -> bool {
        self.hidden_impls.iter().any(|m| scope_is_under(&scope.0, &m.0))
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

    /// The implementation of `trait_id` for `t`. An abstract head, written
    /// or named by its typedef, matches by identity; any other head by
    /// unification against a fresh instantiation, then equivalence.
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
            if let Some(vars) = self.lexical.binds.get(level) {
                let at = res.len();
                res.extend(
                    vars.iter()
                        .filter(|(name, _)| name.starts_with(part) && !is_lowered(name))
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
                            self.lexical
                                .binds
                                .get(lvl)
                                .and_then(|v| v.get(&e.name))
                                .copied()
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
                self.lexical.modules.iter().filter(|m| m.0.starts_with(&*p.0)).collect();
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
        self.names_mut(scope).imports.insert_cow(key.into(), entry);
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

    /// This env with only what an interface states at `scope`: the
    /// imports keyed `import`, the globs `glob` keeps and the types and
    /// traits `declared` names.
    pub fn interface_view(
        &self,
        scope: &ModPath,
        import: impl Fn(&str) -> bool,
        glob: impl Fn(&Glob) -> bool,
        declared: impl Fn(&str) -> bool,
    ) -> Env {
        let mut view = self.clone();
        if let Some(sn) = self.names.get(scope) {
            let sn = ScopeNames {
                imports: retain(&sn.imports, |k, _| import(k)),
                globs: Arc::new(sn.globs.iter().filter(|g| glob(g)).cloned().collect()),
                site: sn.site.clone(),
                input: sn.input.clone(),
            };
            view.names.insert(scope.clone(), sn);
        }
        if let Some(defs) = view.lexical.typedefs.get_mut_cow(scope) {
            *defs = retain(defs, |k, _| declared(k));
        }
        if let Some(traits) = view.lexical.traits.get_mut_cow(scope) {
            *traits = retain(traits, |k, _| declared(k));
        }
        view
    }

    /// Record where the dynamic module at `scope` is declared, when its
    /// path crosses a body (`#`) component.
    pub fn declare_module_site(
        &mut self,
        scope: &ModPath,
        ori: &Origin,
        pos: SourcePosition,
    ) {
        if crosses_body(scope) {
            let place = crate::typ::declaration_place(ori, pos);
            self.names_mut(scope).site = Some(place.as_str().into());
        }
    }

    /// Where the innermost module around `scope` whose path crosses a
    /// body is declared ([`Self::declare_module_site`]).
    fn module_site(&self, scope: &ModPath) -> Option<ArcStr> {
        let mut at: &str = &scope.0;
        loop {
            if let Some(site) = self
                .names
                .get(&ModPath(Path::from(ArcStr::from(at))))
                .and_then(|sn| sn.site.clone())
            {
                return Some(site);
            }
            at = Path::dirname(at)?;
        }
    }

    /// Register a glob (`use m::*`) source module at `scope`. A glob
    /// already there is no write: two compile tasks registering it again
    /// would each replace the entry, which their joins refuse.
    pub fn import_glob(&mut self, scope: &ModPath, src: Glob) {
        if self.names.get(scope).is_some_and(|sn| sn.globs.contains(&src)) {
            return;
        }
        Arc::make_mut(&mut self.names_mut(scope).globs).push(src)
    }

    /// The table at `scope` to write, made where there is none: one at a
    /// scope that crosses a body is the input's being compiled.
    fn names_mut(&mut self, scope: &ModPath) -> &mut ScopeNames {
        let input = match (&self.lexical.input, self.names.get(scope)) {
            (Some(input), None) if crosses_body(scope) => {
                Some(std::sync::Arc::downgrade(&input.0))
            }
            _ => None,
        };
        let sn = self.names.get_or_default(scope.clone());
        if input.is_some() {
            sn.input = input
        }
        sn
    }

    /// Begin a top-level input: the views made from here on hold it,
    /// until [`Self::leave_input`] takes what it returns.
    pub fn enter_input(&mut self) -> Option<InputHold> {
        mem::replace(&mut self.lexical.input, Some(InputHold::default()))
    }

    /// End the input begun where [`Self::enter_input`] returned `outer`.
    pub fn leave_input(&mut self, outer: Option<InputHold>) {
        self.lexical.input = outer
    }

    /// Drop the import tables of every input no lexical view holds: a
    /// definition and a module keep a view, so nothing can compile in
    /// the scopes such an input's bodies name.
    pub fn sweep_names(&mut self) {
        self.names.retain(|_, sn| sn.input.as_ref().is_none_or(|i| i.strong_count() > 0))
    }

    /// Whether `level` itself declares `name`: a binding, a type or a
    /// trait.
    fn declares(&self, level: &str, name: &str) -> bool {
        self.lexical.binds.get(level).is_some_and(|v| v.get(name).is_some())
            || self.lexical.typedefs.get(level).is_some_and(|v| v.get(name).is_some())
            || self.lexical.traits.get(level).is_some_and(|v| v.get(name).is_some())
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

    /// Every import table at `scope` or any descendant, and one at
    /// `scope` itself whether or not it imports anything.
    pub fn names_under(&self, scope: &ModPath) -> Map<ModPath, ScopeNames> {
        let mut names = Map::new();
        names.insert_cow(scope.clone(), ScopeNames::default());
        for (s, sn) in self.names.iter().filter(|(s, _)| scope_is_under(s, scope)) {
            names.insert_cow(s.clone(), sn.clone());
        }
        names
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
        let site = TypeDefSite { public, doc, pos, decl, ori };
        self.deftype_inner(scope, name, params, body, site, false).map(|(u, _)| u)
    }

    /// [`Self::deftype`] for a typedef a block declares among others: an
    /// abstract type registered already is compared with the registered
    /// representation once the block's typedefs are declared, by
    /// [`Self::check_abstract_rep`] over the registered one this returns.
    /// A comparison here would fill the cells of the names it writes
    /// while a later typedef of the block may yet shadow one.
    pub fn deftype_in_block(
        &mut self,
        scope: &ModPath,
        name: &str,
        params: Arc<[(TVar, Option<Type>)]>,
        body: &TypeDefBody,
        pos: SourcePosition,
        decl: SourcePosition,
        ori: Arc<Origin>,
    ) -> Result<(Unguarded, Option<Arc<AbstractRep>>)> {
        let site = TypeDefSite { public: false, doc: None, pos, decl, ori };
        self.deftype_inner(scope, name, params, body, site, true)
    }

    fn deftype_inner(
        &mut self,
        scope: &ModPath,
        name: &str,
        params: Arc<[(TVar, Option<Type>)]>,
        body: &TypeDefBody,
        site: TypeDefSite,
        defer: bool,
    ) -> Result<(Unguarded, Option<Arc<AbstractRep>>)> {
        let TypeDefSite { public, doc, pos, decl, ori } = site;
        if self.lexical.typedefs.get(scope).and_then(|m| m.get(name)).is_some() {
            bail!("{name} is already defined {}", in_scope(scope))
        }
        if self.lexical.traits.get(scope).and_then(|m| m.get(name)).is_some() {
            bail!("{name} is already defined as a trait {}", in_scope(scope))
        }
        let (typ, rep) = match body {
            TypeDefBody::Alias(typ) => {
                (typ.scope_refs(scope).rewrite_trait_args(self)?, None)
            }
            TypeDefBody::Abstract(rep) => {
                let formals =
                    Arc::from_iter(params.iter().map(|(tv, _)| Type::TVar(tv.clone())));
                let site = self.module_site(scope);
                let id = AbstractId::declared(scope, name, decl, &ori, site.as_deref());
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
        for (tv, _) in params.iter() {
            if !used.contains_key(&tv.name) {
                bail!("unused type parameter {tv} in definition of {name}")
            }
        }
        if self.ide.is_lsp() {
            // Typedef bodies are stored, not checked, so their type
            // references are recorded here for the IDE.
            typ.record_ide_refs(self, scope);
        }
        let typ = through_aliases(self, name, &params, &typ).unwrap_or(typ);
        let rep = rep.map(|r| through_aliases(self, name, &params, &r).unwrap_or(r));
        let new = || {
            std::sync::Arc::new(ResolvedRef::new(
                scope.clone(),
                pos,
                ori,
                params.clone(),
                typ.clone(),
            ))
        };
        let (def, seeded) = if defer && is_block_scope(scope) {
            self.block_types.declare(scope, name, new)
        } else {
            (new(), Arc::new(AtomicBool::new(false)))
        };
        let written = match &rep {
            Some(r) if !same_formals(def.params(), &params) => {
                let formals: LPooled<AHashMap<ArcStr, Type>> = def
                    .params()
                    .iter()
                    .map(|(tv, _)| (tv.name.clone(), Type::TVar(tv.clone())))
                    .collect();
                Some(r.replace_tvars(&formals))
            }
            r => r.clone(),
        };
        let td_def = def.clone();
        let td = TypeDef { def, rep: written, doc, seeded };
        let defs = self.lexical.typedefs.get_or_default_cow(scope.clone());
        defs.insert_cow(name.into(), td);
        // compared with the definition in scope, where a self-reference in
        // the representation resolves
        let mut deferred = None;
        if let (Type::Abstract { id, .. }, Some(rep)) = (&typ, &rep)
            && let Some(old) = self.abstract_reps.get(id).cloned()
        {
            if defer {
                deferred = Some(old);
            } else if !self.rep_unchanged(&old, &params, rep) {
                self.undeftype(scope, name);
                bail!("{}", rep_changed(name, &old.rep))
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
                named: Arc::default(),
            };

            // the check above refused a changed representation, or left it
            // to `check_abstract_rep`, so an entry of the same publicity is
            // this one: writing it again would be a second write a parallel
            // task's join refuses
            if self.abstract_reps.get(id).is_none_or(|old| old.public != public) {
                self.abstract_reps.insert(*id, Arc::new(r));
            }
        }
        match self.typedef_shape(name, &typ, &td_def) {
            Err(e) => {
                self.undeftype(scope, name);
                Err(e)
            }
            Ok(u) => Ok((u, deferred)),
        }
    }

    /// Refuse a typedef `name`, whose body is `typ`, that names no value
    /// or unfolds without end. Every self-reference must sit under a
    /// constructor: `type T = [i64, T]` (or `type A = B; type B = A`)
    /// names no value, and contains' coinductive memo would accept it
    /// against anything. And no parameter may come back to its definition
    /// wrapped in a constructor ([`crate::typ::expansive`]): every walk
    /// over a type ends because its unfolding repeats.
    fn typedef_shape(
        &self,
        name: &str,
        typ: &Type,
        def: &std::sync::Arc<ResolvedRef>,
    ) -> Result<Unguarded> {
        let guarded = self.typedef_guard(name, typ, def)?;
        match crate::typ::expansive::expansive(self, def) {
            Expansive::Grows(param) => bail!("{}", expansive(name, &param)),
            Expansive::Through(param, alias, why) => {
                bail!("{}", expansive_through(name, &param, &alias, why))
            }
            Expansive::Unknown => Ok(guarded.or(Unguarded::Unknown)),
            Expansive::Regular => Ok(guarded),
        }
    }

    /// [`Self::typedef_shape`]'s refusal of a self-reference outside every
    /// constructor.
    fn typedef_guard(
        &self,
        name: &str,
        typ: &Type,
        def: &ResolvedRef,
    ) -> Result<Unguarded> {
        match typ.reaches_unguarded(self, def.def_key()) {
            Unguarded::Reaches => bail!("{}", non_contractive(name)),
            u => Ok(u),
        }
    }

    /// Whether `params` and `rep` declare what `old` does: the formals by
    /// position, then the body.
    fn rep_unchanged(
        &self,
        old: &AbstractRep,
        params: &[(TVar, Option<Type>)],
        rep: &Type,
    ) -> bool {
        let shape = |params: &[(TVar, Option<Type>)], rep: &Type| {
            Type::Tuple(Arc::from_iter(
                params.iter().map(|(tv, _)| Type::TVar(tv.clone())).chain([rep.clone()]),
            ))
        };
        let (was, now) = (shape(&old.params, &old.rep), shape(params, rep));
        old.params.len() == params.len()
            && was.sig_matches(self, &now).is_ok()
            && now.sig_matches(self, &was).is_ok()
    }

    /// Resolve the names the abstract typedef `name` at `scope` writes in
    /// its representation and bounds, and have the registration it made
    /// own the typedefs they name: a body's typedefs leave the env once
    /// the body has compiled, and the registration outlives them. Called
    /// once every typedef its block declares is.
    pub fn own_abstract_rep(&self, scope: &ModPath, name: &str) {
        let Some(td) = self.lexical.typedefs.get(scope).and_then(|m| m.get(name)) else {
            return;
        };
        let Type::Abstract { id, .. } = td.typ() else { return };
        let Some(r) = self.abstract_reps.get(id) else { return };
        if r.named.get().is_some() {
            return;
        }
        let bounds = r.params.iter().filter_map(|(_, b)| b.as_ref());
        let written = || iter::once(&r.rep).chain(bounds.clone());
        let mut seen: LPooled<AddrSet> = LPooled::take();
        for t in written() {
            t.seed_refs_seen(self, &mut seen);
        }
        let mut cells: LPooled<AHashSet<usize>> = LPooled::take();
        let mut defs: LPooled<AHashMap<usize, std::sync::Arc<ResolvedRef>>> =
            LPooled::take();
        for t in written() {
            t.named_defs(&mut cells, &mut defs)
        }
        let _ = r.named.set(defs.drain().map(|(_, d)| d).collect());
    }

    /// Refuse the abstract typedef `name` at `scope` if its representation
    /// is not `old`'s: [`Self::deftype_in_block`]'s comparison, once the
    /// typedefs it names are declared. One that still names nothing is
    /// the check's end's to refuse.
    pub fn check_abstract_rep(
        &self,
        scope: &ModPath,
        name: &str,
        old: &AbstractRep,
    ) -> Result<()> {
        let Some(td) = self.lexical.typedefs.get(scope).and_then(|m| m.get(name)) else {
            return Ok(());
        };
        let Some(rep) = &td.rep else { return Ok(()) };
        let mut unresolved = Vec::new();
        let _ = rep.unresolved_names(self, &mut unresolved, None);
        if unresolved.is_empty() && !self.rep_unchanged(old, td.def.params(), rep) {
            bail!("{}", rep_changed(name, &old.rep))
        }
        Ok(())
    }

    /// [`Self::deftype`]'s check of a typedef's shape again, for a
    /// definition whose body named something not defined yet: at the end
    /// of the check every name it reaches is. `Unknown` while a name it
    /// reaches is still undefined.
    pub fn check_typedef(&self, scope: &ModPath, name: &str) -> Result<Unguarded> {
        let Some(def) = self.declared_def(scope, name) else {
            return Ok(Unguarded::Guarded);
        };
        self.typedef_shape(name, def.typ(), &def)
    }

    /// The definition of the typedef `name` declares at `scope`: its
    /// lexical entry, or a block's, which outlives the block's compile
    /// in [`Self::block_types`].
    fn declared_def(
        &self,
        scope: &ModPath,
        name: &str,
    ) -> Option<std::sync::Arc<ResolvedRef>> {
        if let Some(td) = self.lexical.typedefs.get(scope).and_then(|m| m.get(name)) {
            return Some(td.def.clone());
        }
        match self.block_types.find(scope, name) {
            Some(BlockHit::Live(def)) if is_block_scope(scope) => Some(def),
            _ => None,
        }
    }

    /// [`Self::check_typedef`] of the self-references alone, and whether
    /// the definition is known not to be expansive: a walk found it, or
    /// another definition's walk that reached it, regular.
    pub fn check_typedef_guard(
        &self,
        scope: &ModPath,
        name: &str,
    ) -> Result<(Unguarded, bool)> {
        let Some(def) = self.declared_def(scope, name) else {
            return Ok((Unguarded::Guarded, true));
        };
        let regular = def.params().is_empty() || def.regular.get().is_some();
        Ok((self.typedef_guard(name, def.typ(), &def)?, regular))
    }

    /// The representation of the Graphix-minted abstract type `id`, if
    /// its definition is visible from `from` (the defining scope and
    /// its subtree).
    pub fn abstract_rep(&self, id: AbstractId, from: &ModPath) -> Option<&AbstractRep> {
        let r = self.abstract_reps.get(&id)?;
        let declares = |t: &Type| matches!(t, Type::Abstract { id: d, .. } if *d == id);
        let visible = r.public
            || chain_levels(from).any(|scope| {
                match self
                    .lexical
                    .typedefs
                    .get(scope)
                    .and_then(|m| m.get(r.name.as_str()))
                {
                    Some(def) => def.rep.is_some() && declares(def.typ()),
                    None => {
                        is_block_scope(scope)
                            && matches!(self.block_types.find(scope, &r.name),
                                Some(BlockHit::Live(def)) if declares(def.typ()))
                    }
                }
            });
        visible.then_some(&**r)
    }

    /// Fill the resolution cell of every `Type::Ref` reachable from a
    /// registered typedef body, for env-free expansion
    /// (`TypeRef::expand_cell`). Must run after the check, when every
    /// name's final target is registered: before elaboration, whose
    /// instances expand their callers' types under their definitions'
    /// envs, and again before fusion for what elaboration registered.
    pub fn seed_typedef_refs(&self) {
        self.seed_typedef_refs_under(&ModPath::root())
    }

    /// [`Self::seed_typedef_refs`] for the typedefs at `scope` and below:
    /// a module's own, in the module's env.
    pub fn seed_typedef_refs_under(&self, scope: &ModPath) {
        let _profile = profile::phase(Phase::SeedTypes);
        let under =
            self.lexical.typedefs.into_iter().filter(|(s, _)| scope_is_under(s, scope));
        for (_, defs) in under {
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
        for (_, bt) in self.block_types.live_under(scope) {
            if bt.seeded.load(Ordering::Relaxed) {
                continue;
            }
            if let Some(def) = bt.def.upgrade()
                && def.typ().seed_refs(self)
            {
                bt.seeded.store(true, Ordering::Relaxed);
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
        let ids: LPooled<Vec<AbstractId>> = match self.lexical.typedefs.get(scope) {
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

    /// The definition `name` names at `scope`, for a holder that must
    /// keep it alive past the env entry (the node that declared it).
    #[doc(hidden)]
    pub fn typedef_definition(
        &self,
        scope: &ModPath,
        name: &str,
    ) -> Option<std::sync::Arc<ResolvedRef>> {
        self.lexical
            .typedefs
            .get(scope)
            .and_then(|m| m.get(name))
            .map(|td| td.def.clone())
    }

    /// Record the type names the statement list at `scope` (a block, a
    /// module's body, a script) declares before its statements compile: a
    /// name it declares means its type throughout it, so before its
    /// declaration it is never an outer type of the name or one a `use`
    /// or a prelude brings ([`Lexical::block_names`]).
    pub fn open_block<'a>(
        &mut self,
        scope: &ModPath,
        names: impl IntoIterator<Item = &'a str>,
    ) {
        let mut names = names.into_iter().peekable();
        if names.peek().is_some() {
            let m = self.lexical.block_names.get_or_default_cow(scope.clone());
            for n in names {
                m.insert_cow(CompactString::from(n), ());
            }
        }
    }

    /// The statement list at `scope` begins to compile: record what its
    /// statements may change of the names read through its level
    /// ([`Unsettled`]). The entry it replaces, for [`Self::close_list`].
    pub fn open_list(&mut self, scope: &ModPath, later: Unsettled) -> Option<OpenList> {
        match later.is_empty() {
            true if self.unsettled.is_empty() => None,
            true => self.unsettled.remove_cow(scope),
            false => self.unsettled.insert_cow(scope.clone(), (Arc::new(later), 0)),
        }
    }

    /// Statement `stmt` of the list at `scope`, a `use` or a module,
    /// begins to compile: what it brings is no longer to come.
    pub fn settle_statement(&mut self, scope: &ModPath, stmt: usize) {
        if let Some((_, from)) = self.unsettled.get_mut_cow(scope) {
            *from = stmt + 1
        }
    }

    /// The list at `scope` has compiled: `prev` is what
    /// [`Self::open_list`] replaced.
    pub fn close_list(&mut self, scope: &ModPath, prev: Option<OpenList>) {
        match prev {
            Some(prev) => drop(self.unsettled.insert_cow(scope.clone(), prev)),
            None if self.unsettled.is_empty() => (),
            None => drop(self.unsettled.remove_cow(scope)),
        }
    }

    /// Forget the names [`Self::open_block`] recorded for the module's
    /// body or script at `scope`, whose statements have compiled: the
    /// lexical maps hold what it declares, and a name they lose (one its
    /// interface hides, one deleted) is no longer its own. A block's stay.
    pub fn close_block(&mut self, scope: &ModPath) {
        if !is_block_scope(scope) {
            self.lexical.block_names.remove_cow(scope);
        }
    }

    /// Drop the name `name` at `scope`. An abstract type's
    /// representation stays: every compile of one source typedef
    /// registers the same one, and a live compile still constructs it.
    pub fn undeftype(&mut self, scope: &ModPath, name: &str) {
        if let Some(defs) = self.lexical.typedefs.get_mut_cow(scope) {
            defs.remove_cow(&CompactString::from(name));
            if defs.len() == 0 {
                self.lexical.typedefs.remove_cow(scope);
            }
        }
    }

    /// Drop the lexical entries (modules, binds, typedefs, traits) at
    /// `scope` or below; the number of names they held. The bindings
    /// themselves stay registered.
    pub fn unbind_lexical_under(&mut self, scope: &str) -> usize {
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
        self.lexical.modules =
            retain_set(&self.lexical.modules, |s| !scope_is_under(s, scope));
        drop_scopes(&mut self.lexical.block_names, scope);
        drop_scopes(&mut self.lexical.binds, scope)
            + drop_scopes(&mut self.lexical.typedefs, scope)
            + drop_scopes(&mut self.lexical.traits, scope)
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
            lexical: _,
            names,
            abstract_reps,
            trait_defs,
            trait_methods,
            impls,
            kept_traits,
            poly_binds,
            // a package stays registered across its recheck
            package_roots: _,
            // each load names its blocks apart
            block_types: _,
            ide: _,
            // a list's, while it compiles
            unsettled: _,
            hidden_impls: _,
            recorded: _,
        } = self;
        names.retain(|s, _| !under(s));
        let binds: LPooled<Vec<BindId>> = by_id
            .into_iter()
            .filter(|(_, b)| under(&b.scope))
            .map(|(id, _)| *id)
            .collect();
        by_id.remove_many(binds.iter().copied());
        trait_methods.remove_many(binds.iter().copied());
        kept_traits.remove_many(binds.iter().copied());
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
        self.lexical
            .binds
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
        self.mirror_bind(&bind);
        self.by_id.insert(id, Arc::new(bind));
        Arc::make_mut(self.by_id.get_mut(&id).expect("just inserted"))
    }

    /// Give the binding `id` the type `typ`. Every reference compiled
    /// afterwards reads it; the IDE mirror gets the binding again, and
    /// its latest entry wins.
    pub fn retype(&mut self, id: BindId, typ: Type) {
        if let Some(b) = self.by_id.get_mut(&id) {
            Arc::make_mut(b).typ = typ;
            let b = b.clone();
            self.mirror_bind(&b);
        }
    }

    /// Give the IDE the binding as it stands. One a lowering named is
    /// given only as the name the seq declared it by, where it did: no
    /// source wrote any other, so no query or completion may answer it.
    fn mirror_bind(&self, b: &Bind) {
        self.with_ide(|ide| {
            if !is_lowered(&b.name) {
                return ide.binds.push(b.clone());
            }
            if let Some(Lowered { written, decl: Some((pos, ori)), .. }) =
                ide.lowered.get(b.name.as_str())
            {
                let mut b = b.clone();
                b.name = written.as_str().into();
                b.pos = *pos;
                b.ori = ori.clone();
                ide.binds.push(b)
            }
        })
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
            Some(Binder::LetPattern | Binder::Referent) | None => None,
        }
    }

    /// Record that `id` is bound by a destructuring `let`'s pattern.
    pub fn mark_let_pattern(&mut self, id: BindId) {
        if let Some(b) = self.by_id.get_mut(&id) {
            Arc::make_mut(b).binder = Some(Binder::LetPattern);
        }
    }

    pub fn unbind_variable(&mut self, id: BindId) {
        self.unbind_name(id);
        self.poly_binds.remove(&id);
        self.by_id.remove(&id);
    }

    pub fn unbind_name(&mut self, id: BindId) {
        if let Some(b) = self.by_id.get(&id) {
            if let Some(binds) = self.lexical.binds.get_mut_cow(&b.scope) {
                if binds.get(&b.name) == Some(&id) {
                    binds.remove_cow(&b.name);
                }
                if binds.len() == 0 {
                    self.lexical.binds.remove_cow(&b.scope);
                }
            }
        }
    }

    /// An env holding the typedefs `defs` at the root, each named `Name`
    /// or `Name<'a, ..>` (a body `Abstract<rep>` declares an abstract
    /// type), and `t` there, every reference seeded. For tests.
    #[cfg(test)]
    pub fn typed_at_root(defs: &[(&str, &str)], t: &str) -> Result<(Env, Type)> {
        use crate::expr::parser::parse_type;
        let root = ModPath::root();
        let mut env = Env::default();
        for (name, body) in defs {
            let (name, params) = name.split_once('<').unwrap_or((name, ""));
            let params = params.trim_end_matches('>').split(',').map(str::trim);
            let params = Arc::from_iter(params.filter(|p| !p.is_empty()).map(|p| {
                (TVar::empty_named(ArcStr::from(p.trim_start_matches('\''))), None)
            }));
            env.deftype(
                &root,
                name,
                params,
                &match body.strip_prefix("Abstract<").and_then(|b| b.strip_suffix('>')) {
                    Some(rep) => TypeDefBody::Abstract(Some(parse_type(rep)?)),
                    None => TypeDefBody::Alias(parse_type(body)?),
                },
                true,
                None,
                Default::default(),
                Default::default(),
                Arc::new(Default::default()),
            )?;
        }
        env.seed_typedef_refs();
        let t = parse_type(t)?.scope_refs(&root);
        t.seed_refs(&env);
        Ok((env, t))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::is_block_component;

    #[test]
    fn unshared_pairs_hold_every_difference_and_little_else() {
        let mut seed = 0x2545_f491_4f6c_dd1du64;
        let mut below = move |n: usize| {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            (seed % n as u64) as usize
        };
        for n in [16, 64, 4096] {
            let keys: Vec<BindId> = (0..n).map(|_| BindId::new()).collect();
            let mut base: Map<BindId, BindId> = Map::default();
            for k in keys.iter() {
                base.insert_cow(*k, BindId::new());
            }
            for _ in 0..20 {
                let [mut first, mut second] = [base.clone(), base.clone()];
                let mut changes = 0;
                for map in [&mut first, &mut second] {
                    for _ in 0..=below(6) {
                        changes += 1;
                        match below(3) {
                            0 => drop(map.remove_cow(&keys[below(n)])),
                            1 => drop(map.insert_cow(BindId::new(), BindId::new())),
                            _ => drop(map.insert_cow(keys[below(n)], BindId::new())),
                        }
                    }
                }
                for (of, with) in [(&first, &second), (&second, &first), (&base, &first)]
                {
                    let mut seen: Vec<(BindId, BindId)> = vec![];
                    each_unshared(of, with, |k, v| seen.push((*k, *v)));
                    for (k, v) in of.iter() {
                        if with.get(k) != Some(v) {
                            assert!(seen.contains(&(*k, *v)), "{k:?} missed");
                        }
                    }
                    assert!(seen.len() <= (changes * 64).min(of.len()));
                }
            }
            let rebuilt: Map<BindId, BindId> =
                base.iter().map(|(k, v)| (*k, *v)).collect();
            for (of, with) in [(&rebuilt, &base), (&base, &base.clone())] {
                each_unshared(of, with, |k, _| panic!("{k:?} named"));
            }
        }
    }

    #[test]
    fn two_forks_registering_one_glob_join() {
        let mut env = Env::default();
        let scope = ModPath::from(["blk"]);
        let str = || Glob::Module(ModPath::from(["str"]));
        env.import_glob(&scope, str());
        let (mut a, mut b) = (env.fork(), env.fork());
        a.import_glob(&scope, str());
        b.import_glob(&scope, str());
        env.join(a);
        env.join(b);
    }

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
        let defs = env.lexical.typedefs.get(&scope).unwrap();
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

    /// Unbinding a package from a clone of the env (a check, rolled back
    /// by restoring the original) leaves the original's block types.
    #[test]
    fn unbinding_a_clone_keeps_the_block_types() {
        let mut env = Env::default();
        let pkg = ModPath::from(["pkg"]);
        let block = ModPath(pkg.0.append("#do1.1~0"));
        let i64 = Type::Primitive(netidx_value::Typ::I64.into());
        let pos = SourcePosition::default();
        let ori = Arc::new(Origin::default());
        env.deftype_in_block(
            &block,
            "L",
            Arc::from_iter([]),
            &TypeDefBody::Alias(i64),
            pos,
            pos,
            ori,
        )
        .unwrap();
        let live = |env: &Env| {
            matches!(env.block_types.find(&block.0, "L"), Some(BlockHit::Live(_)))
        };
        assert!(live(&env));
        let mut check = env.clone();
        check.unbind_scope_subtree(&pkg);
        assert!(live(&env));
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
        env.lexical.modules.insert_cow(sys.clone());
        env.lexical.modules.insert_cow(net.clone());
        at(&mut env, &net, "publish");
        let t = env.apply_sandbox(&Sandbox::Blacklist(Arc::from_iter([sys]))).unwrap();
        assert!(!t.lexical.modules.contains(&net));
        assert!(t.lexical.binds.get(&net).is_none());
        assert!(env.lexical.modules.contains(&net), "the original is untouched");
    }

    /// A blacklist's entries name the unsandboxed env, so an entry under
    /// a module an earlier entry removed is no error, and a name that is
    /// nowhere still is.
    #[test]
    fn blacklist_entries_are_independent_of_order() {
        let mut env = Env::default();
        let (sys, net) = (ModPath::from(["sys"]), ModPath::from(["sys", "net"]));
        env.lexical.modules.insert_cow(sys.clone());
        env.lexical.modules.insert_cow(net.clone());
        at(&mut env, &net, "publish");
        let publish = ModPath::from(["sys", "net", "publish"]);
        for bl in [
            [sys.clone(), net.clone()],
            [net.clone(), sys.clone()],
            [sys.clone(), publish.clone()],
            [publish.clone(), sys.clone()],
        ] {
            let t = env.apply_sandbox(&Sandbox::Blacklist(Arc::from_iter(bl))).unwrap();
            assert!(!t.lexical.modules.contains(&net));
            assert!(t.lexical.binds.get(&net).is_none());
        }
        let nowhere = ModPath::from(["sys", "net", "nowhere"]);
        assert!(
            env.apply_sandbox(&Sandbox::Blacklist(Arc::from_iter([sys, nowhere])))
                .is_err()
        );
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
        env.lexical.binds.remove_cow(&body);
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
        assert_eq!(env.lexical.binds.get(&m).and_then(|b| b.get("x")), Some(&new));
    }

    /// Module completion offers an import only when it names a module.
    #[test]
    fn module_completion_skips_value_imports() {
        let mut env = Env::default();
        let (m, sub) = (ModPath::from(["m"]), ModPath::from(["m", "sub"]));
        env.lexical.modules.insert_cow(m.clone());
        env.lexical.modules.insert_cow(sub);
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

    #[test]
    fn warning_record_follows_join_order() {
        let mut env = Env::default();
        env.record_warnings();
        let ori = Arc::new(Origin::default());
        let pos = SourcePosition::default();
        let (first, second) = (env.fork(), env.fork());
        second.warn_at(BitFlags::empty(), &ori, pos, pos, "second").unwrap();
        first.warn_at(BitFlags::empty(), &ori, pos, pos, "first").unwrap();
        env.join(first);
        env.join(second);
        let recorded = env.recorded_warnings();
        let messages: Vec<_> =
            recorded.iter().map(|w| w.rsplit(": ").next().unwrap()).collect();
        assert_eq!(messages, ["first", "second"]);
    }

    #[test]
    fn a_join_names_as_its_own_load_and_dispatches_as_both() {
        let root = LoadedBody::under(None, ModPath::root());
        let a = LoadedBody::under(Some(root.clone()), ModPath::from(["a"]));
        let d = LoadedBody::under(Some(root.clone()), ModPath::from(["d"]));
        let inner = LoadedBody::under(Some(a.clone()), ModPath::from(["a", "i"]));
        assert!(Arc::ptr_eq(&LoadedBody::join(&a, &inner), &inner));
        assert!(Arc::ptr_eq(&LoadedBody::join(&inner, &a), &inner));
        assert!(Arc::ptr_eq(&LoadedBody::join(&inner, &root), &inner));
        let j = LoadedBody::join(&a, &d);
        assert!(!Arc::ptr_eq(&j, &a) && !Arc::ptr_eq(&j, &d));
        assert!(j.is_within(&a) && !j.is_within(&d));
        assert!(j.dispatches_as(&a) && j.dispatches_as(&d) && j.dispatches_as(&root));
        assert!(!a.dispatches_as(&d));
        assert!(Arc::ptr_eq(&LoadedBody::join(&a, &j), &j));
        assert!(Arc::ptr_eq(&LoadedBody::join(&j, &d), &j));
        let back = LoadedBody::join(&d, &j);
        assert!(back.is_within(&d) && !back.is_within(&a));
        assert!(back.dispatches_as(&a) && back.dispatches_as(&d));
    }

    #[test]
    fn joins_made_down_a_cross_load_recursion_stay_flat() {
        let root = LoadedBody::under(None, ModPath::root());
        let a = LoadedBody::under(Some(root.clone()), ModPath::from(["a"]));
        let d = LoadedBody::under(Some(root.clone()), ModPath::from(["d"]));
        let mut cur = LoadedBody::join(&a, &d);
        for _ in 0..200_000 {
            let through_d = LoadedBody::join(&d, &cur);
            cur = LoadedBody::join(&a, &through_d);
        }
        assert_eq!(cur.beside.len(), 1);
        assert!(cur.chain().count() <= 3);
        assert!(cur.dispatches_as(&a) && cur.dispatches_as(&d));
        assert!(cur.is_within(&a) && !cur.is_within(&d));
    }
}
