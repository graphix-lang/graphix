use crate::{
    PRINT_FLAGS, PrintFlag,
    env::{Env, TypeDef},
    expr::ModPath,
    format_with_flags,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_core::utils::Either;
use netidx_derive::Pack;
use netidx_value::Typ;
use nohash::IntMap;
use parking_lot::Mutex;
use poolshark::{IsoPoolable, local::LPooled};
use smallvec::SmallVec;
use std::{
    cmp::{Eq, PartialEq},
    fmt::Debug,
    iter,
    ops::{ControlFlow, Deref, DerefMut},
    sync::LazyLock,
};
use triomphe::Arc;

mod cast;
pub use cast::IsAFlags;
mod contains;
pub use contains::ContainsFlags;
pub mod fntyp;
mod matches;
mod normalize;
pub(crate) use normalize::{NormKey, norm_key};
mod print;
mod setops;
pub(crate) mod tval;
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
    /// Per-call ref-expansion cache (ref_id → raw `lookup_ref` result).
    /// Consumers take `reset_tvars()` copies, so cells stay fresh per
    /// crossing exactly as per-crossing `lookup_ref` behaved, while the
    /// CONCRETE mass is Arc-shared across crossings — `contains`'
    /// content-identity fast path then prunes repeated pairs instead of
    /// re-walking the expansion per crossing (the 2026-07-13 widget-type
    /// wedge's unification leg).
    expansions: LPooled<IntMap<usize, Type>>,
    /// Pure-PROBE pair memo: `contains_int` results for empty-flag
    /// calls (no binding, no aliasing — side-effect free by the probe
    /// contract), keyed by both sides' content-Arc identities
    /// ([`norm_key`]). The general Set arms run O(|lhs|·|rhs|) probe
    /// walks PER NESTING LEVEL; over widget-scale unions the probe tree
    /// is astronomically large while the DISTINCT pair set is small.
    /// Each entry PINS both compared types (`probe_pins`) so an address
    /// can't be recycled into a different type while its key lives.
    /// Entries carry the `epoch` at insert: a probe verdict reads cell
    /// BINDINGS, and any flagged (committing) call may bind — the epoch
    /// bumps there, invalidating prior verdicts conservatively.
    probe_pairs: LPooled<AHashMap<(NormKey, NormKey), (u64, bool)>>,
    probe_pins: LPooled<Vec<Type>>,
    /// Content-identity → id for NON-Ref types with a content key
    /// (`probe_key`: Variant/Fn/Array/Set/Struct/Map). The cycle memo
    /// (`contains_int`'s Ref arm) keys `(t0_id, t1_id)`; collapsing every
    /// non-Ref to `None` conflated DISTINCT finite sub-problems — a
    /// recursive `List<'a> ⊇ Cons(i64, Cons(i64, Fn))` inserted the outer
    /// pair, then the inner `List<'a> ⊇ Cons(i64, Fn)` (a smaller,
    /// unrelated RHS) hit the same `(_, None)` key and was assumed part
    /// of the cycle, so the deep `Fn`-vs-`List` mismatch was never
    /// checked (aug27a ryouko). A finite RHS shrinks and terminates on
    /// its own — it never needed the memo — so distinguishing these ids
    /// only removes false hits; the genuinely non-shrinking RHS types
    /// (Any, primitives, tvars) have no content key and keep `None`,
    /// preserving their cycle break.
    content_ids: LPooled<AHashMap<NormKey, usize>>,
    epoch: u64,
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
        RefHist {
            inner,
            ref_ids: LPooled::take(),
            expansions: LPooled::take(),
            probe_pairs: LPooled::take(),
            probe_pins: LPooled::take(),
            content_ids: LPooled::take(),
            epoch: 0,
            next_id: 0,
        }
    }

    /// A flagged (possibly binding) call ran — prior probe verdicts may
    /// be stale. See `probe_pairs`.
    fn note_commit(&mut self) {
        self.epoch += 1;
    }

    /// [`norm_key`] extended with `Variant`: a pair-VERDICT key may
    /// include the tag's allocation identity (unlike the rebuild memos,
    /// where the un-keyed tag made slice-only keys unsound).
    fn probe_key(t: &Type) -> Option<NormKey> {
        match t {
            Type::Variant(tag, ts) => Some((
                std::mem::discriminant(t),
                (**ts).as_ptr() as usize,
                tag.as_ptr() as usize,
            )),
            t => norm_key(t),
        }
    }

    /// Cached pure-probe verdict for `(t0, t1)`, when both sides have
    /// content identities and the entry is current. See `probe_pairs`.
    fn probe_get(&self, t0: &Type, t1: &Type) -> Option<bool> {
        let k = (Self::probe_key(t0)?, Self::probe_key(t1)?);
        let (epoch, r) = self.probe_pairs.get(&k).copied()?;
        (epoch == self.epoch).then_some(r)
    }

    fn probe_put(&mut self, t0: &Type, t1: &Type, r: bool) {
        if let (Some(k0), Some(k1)) = (Self::probe_key(t0), Self::probe_key(t1)) {
            if self.probe_pairs.insert((k0, k1), (self.epoch, r)).is_none() {
                self.probe_pins.push(t0.clone());
                self.probe_pins.push(t1.clone());
            }
        }
    }

    /// [`Type::lookup_ref`] through the per-call expansion cache — see
    /// the `expansions` field. `id` is the type's [`Self::ref_id`];
    /// `None` (non-Ref, or unresolvable) falls through uncached, as does
    /// a ref with TVar params: its expansion embeds the CALLER's live
    /// cells (the inference channel `lookup_ref`'s substitution wires
    /// up), which a cached/reset copy would sever.
    ///
    /// `raw` (pure PROBES only): hand back the cached expansion ITSELF
    /// — probes never bind, so per-crossing cell freshness buys nothing,
    /// and the stable addresses are what lets `probe_pairs` recognize a
    /// repeated pair. Flagged (committing) calls take `reset_tvars()`
    /// copies so one crossing's bindings can't infect another's.
    fn expand_ref(
        &mut self,
        t: &Type,
        id: Option<usize>,
        env: &Env,
        raw: bool,
    ) -> Result<Type> {
        // Only a Ref expands. A non-Ref now carries a content id (for the
        // cycle memo — see `content_ids`), but its expansion is uncached
        // `lookup_ref`, exactly as before content ids existed: routing it
        // through the id-keyed cache below would hand back a
        // `reset_tvars()` copy and sever the live inference cells.
        if !matches!(t, Type::Ref(_)) {
            return t.lookup_ref(env);
        }
        let Some(id) = id else { return t.lookup_ref(env) };
        let closed = match t {
            Type::Ref(tr) => tr.params.iter().all(|p| p.tvar_free()),
            _ => true,
        };
        if !closed {
            return t.lookup_ref(env);
        }
        if let Some(e) = self.expansions.get(&id) {
            return Ok(if raw { e.clone() } else { e.reset_tvars() });
        }
        let e = t.lookup_ref(env)?;
        self.expansions.insert(id, e.clone());
        Ok(if raw { e } else { e.reset_tvars() })
    }

    /// Return a stable ID for a Ref type based on (typedef identity, params).
    /// Returns None for non-Ref types — cycle detection is driven by the
    /// Ref side, and None collapses all non-Ref types to the same key.
    /// Identity comes from the ref's FILLED resolution cell when
    /// present (a cell-carried ref may be unresolvable in the ambient
    /// env — keying it `None` would collapse distinct escaped
    /// recursive types onto one cycle key — and after a redefinition
    /// an old-cell ref must not share the new def's identity), else
    /// from the env-resolved `TypeDef` address as before. Both are
    /// per-call-live allocations, so the two address spaces can't
    /// collide.
    fn ref_id(&mut self, t: &Type, env: &Env) -> Option<usize> {
        match t {
            Type::Ref(tr) => {
                let def_addr = match tr.resolved() {
                    Some(r) => Arc::as_ptr(&r).addr(),
                    None => {
                        match env.lookup_typedef(&tr.scope, &tr.name).ok().flatten() {
                            Some(def) => (def as *const TypeDef).addr(),
                            None => return None,
                        }
                    }
                };
                let params = &tr.params;
                let entries = self.ref_ids.entry(def_addr).or_default();
                for &(ref p, id) in entries.iter() {
                    if p.len() == params.len()
                        && p.iter()
                            .zip(params.iter())
                            .all(|(a, b)| setops::union_identical(a, b))
                    {
                        return Some(id);
                    }
                }
                let id = self.next_id;
                self.next_id += 1;
                entries.push((params.clone(), id));
                Some(id)
            }
            // A non-Ref with a CONTENT identity (Variant/Fn/Array/…) gets
            // its own id so the cycle memo doesn't conflate distinct finite
            // sub-problems (see `content_ids`). A content-less type
            // (Any/primitive/tvar) stays `None` — it never has a stable
            // shrinking structure, so collapsing it is both harmless and
            // required for the Any-style cycle break.
            _ => {
                let k = Self::probe_key(t)?;
                if let Some(&id) = self.content_ids.get(&k) {
                    return Some(id);
                }
                let id = self.next_id;
                self.next_id += 1;
                self.content_ids.insert(k, id);
                Some(id)
            }
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

/// The UUID namespace every abstract type's identity is derived from:
/// `abstract_uuid(path)` is the v5 UUID of the type's canonical path
/// (`package::module::Name`) in this namespace — deterministic across
/// parses, processes and builds, so the compile-time [`AbstractId`]
/// and the runtime tag of a value agree everywhere
/// (`design/nominal_abstract_types.md`).
const ABSTRACT_NAMESPACE: uuid::Uuid = uuid::Uuid::from_bytes([
    0x1f, 0x64, 0x9a, 0x2e, 0x7b, 0xd5, 0x4c, 0x8a, 0x9f, 0x3e, 0x21, 0xb7, 0x5c, 0x0d,
    0xe6, 0x42,
]);

/// The runtime UUID of the abstract type at `path`. Rust-backed abstract
/// types register their `AbstractWrapper` under this so that a type test
/// (`T as t`) can recognize their values by the type's path alone.
pub fn abstract_uuid(path: &str) -> uuid::Uuid {
    uuid::Uuid::new_v5(&ABSTRACT_NAMESPACE, path.as_bytes())
}

/// The names of every abstract type minted in this process, for
/// diagnostics: `Type::Abstract` carries only the id, and the id is a
/// path hash, so without this a type error prints the word "abstract"
/// instead of the type's name. Ids are path-deterministic, so two
/// mints of one id always record the same name.
static ABSTRACT_NAMES: LazyLock<Mutex<IntMap<AbstractId, ArcStr>>> =
    LazyLock::new(|| Mutex::new(IntMap::default()));

impl AbstractId {
    /// The identity of the abstract type `name` defined in `scope`:
    /// the low 64 bits of [`abstract_uuid`] of its canonical path.
    pub fn of(scope: &ModPath, name: &str) -> Self {
        let path = format_compact!("{scope}::{name}");
        let (_, lo) = abstract_uuid(&path).as_u64_pair();
        let id = AbstractId(lo);
        ABSTRACT_NAMES.lock().entry(id).or_insert_with(|| ArcStr::from(name));
        id
    }

    /// The type's declared name, if this process has minted the id.
    pub fn name(&self) -> Option<ArcStr> {
        ABSTRACT_NAMES.lock().get(self).cloned()
    }

    pub fn inner(&self) -> u64 {
        self.0
    }

    pub fn from_inner(i: u64) -> Self {
        AbstractId(i)
    }
}

/// The identity of a trait: the low 64 bits of a v5 UUID of its
/// canonical path (`package::module::Name`), minted at declaration
/// like [`AbstractId`] — so an interface's declaration and its
/// implementation's re-declaration name ONE trait, and the global impl
/// table keys on it (`design/traits.md` §4).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitId(u64);

const TRAIT_NAMESPACE: uuid::Uuid = uuid::Uuid::from_bytes([
    0x8c, 0x2b, 0x41, 0x9d, 0xe3, 0x07, 0x4f, 0x6a, 0xb1, 0x5d, 0x77, 0x0a, 0x2e, 0x93,
    0xc8, 0x15,
]);

impl TraitId {
    pub fn of(scope: &ModPath, name: &str) -> Self {
        let path = format_compact!("{scope}::{name}");
        let (_, lo) = uuid::Uuid::new_v5(&TRAIT_NAMESPACE, path.as_bytes()).as_u64_pair();
        TraitId(lo)
    }

    pub fn inner(&self) -> u64 {
        self.0
    }
}

impl nohash::IsEnabled for TraitId {}

/// What a `TypeRef`'s name means: the snapshot of everything
/// [`Type::lookup_ref`] reads from the env via `find_visible`. Held in
/// the ref's write-once `resolved` cell so a ref first resolved in its
/// NATIVE env becomes an env-independent value — later consumers get
/// the same answer regardless of which env they hold (the def env's
/// resolution survives past the def env). Substitution of the ref's
/// `params` into `typ` stays per-call (pure given this snapshot).
#[derive(Debug)]
pub(crate) struct ResolvedRef {
    canonical_scope: ModPath,
    pos: crate::SourcePosition,
    ori: Arc<crate::expr::Origin>,
    params: Arc<[(TVar, Option<Type>)]>,
    typ: Type,
}

impl ResolvedRef {
    /// Same definition? Independently-filled cells for one `TypeDef`
    /// hold distinct `Arc<ResolvedRef>` allocations but SHARE the
    /// def's content Arcs (`d.typ.clone()`/`d.params.clone()`), so
    /// the equality walks shortcut to pointer comparisons; only
    /// genuinely different definitions (cross-env views of an
    /// interface name, REPL redefinition) walk further, and those
    /// differ near the top.
    pub(crate) fn same_def(&self, other: &Self) -> bool {
        (Arc::ptr_eq(&self.params, &other.params) || self.params == other.params)
            && self.typ == other.typ
    }

    pub(crate) fn typ(&self) -> &Type {
        &self.typ
    }

    pub(crate) fn canonical_scope(&self) -> &ModPath {
        &self.canonical_scope
    }
}

/// A reference to a named typedef, e.g. `Foo` or `Result<i64, string>`.
/// `pos` and `ori` are IDE metadata recording where this reference
/// was written in source — they're populated by the parser and
/// ignored for type-system equality, ordering and hashing so they
/// don't affect type identity. `resolved` is the write-once name
/// resolution cell ([`ResolvedRef`]) — also identity-excluded and
/// dropped from the packed form (a decoded ref re-resolves in the
/// loading env). The cell is a function of (scope, name, resolving
/// env) only — NOT of `params` — so param-substituting rebuilds share
/// it ([`TypeRef::with_params`]) while a scope change must mint fresh
/// ([`TypeRef::with_scope`]). It is never overwritten in place: clones
/// share the cell, so refilling would leak one context's view into
/// another's type.
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
    #[pack(skip)]
    pub(in crate::typ) resolved: Arc<Mutex<Option<Arc<ResolvedRef>>>>,
}

impl TypeRef {
    pub fn new(
        scope: ModPath,
        name: ModPath,
        params: Arc<[Type]>,
        pos: Option<crate::SourcePosition>,
        ori: Option<Arc<crate::expr::Origin>>,
    ) -> Self {
        Self { scope, name, params, pos, ori, resolved: Arc::default() }
    }

    /// Build a `TypeRef` with no source-position info — for synthetic
    /// type references created during type inference, set operations,
    /// stdlib type literals, etc.
    pub fn synthetic(scope: ModPath, name: ModPath, params: Arc<[Type]>) -> Self {
        Self::new(scope, name, params, None, None)
    }

    /// This ref with different `params`, SHARING the resolution cell:
    /// the cell caches the name resolution, which does not depend on
    /// params (substitution happens per lookup).
    pub(crate) fn with_params(&self, params: Arc<[Type]>) -> Self {
        Self { params, ..self.clone() }
    }

    /// This ref re-scoped, with a FRESH resolution cell: the name can
    /// resolve differently from the new scope.
    pub(crate) fn with_scope(&self, scope: ModPath, params: Arc<[Type]>) -> Self {
        Self {
            scope,
            name: self.name.clone(),
            params,
            pos: self.pos,
            ori: self.ori.clone(),
            resolved: Arc::default(),
        }
    }

    /// Expand this ref through its FILLED cell — env-free (the whole
    /// point of the cell), substituting the ref's params into the
    /// snapshot body exactly as `lookup_ref` would. `None` when the
    /// cell is empty or the arity mismatches. No constraint checks —
    /// those ran at typecheck; this exists for fusion-side shape
    /// classification (`abi_kind`/`freeze_for_abi`), which is sizing,
    /// not checking.
    pub fn expand_cell(&self) -> Option<Type> {
        let r = self.resolved()?;
        if r.params.len() != self.params.len() {
            return None;
        }
        let mut known: LPooled<AHashMap<ArcStr, Type>> = LPooled::take();
        for ((tv, _), arg) in r.params.iter().zip(self.params.iter()) {
            known.insert(tv.name.clone(), arg.clone());
        }
        Some(r.typ.replace_tvars(&known))
    }

    pub(crate) fn resolved(&self) -> Option<Arc<ResolvedRef>> {
        self.resolved.lock().clone()
    }

    /// Do two same-named refs demonstrably mean the same definition?
    /// True unless both cells are filled with DIFFERENT definitions
    /// (cross-env views of an interface name, REPL redefinition) —
    /// the name-equality fast paths in `contains`/`union`/`diff`/
    /// `could_match` must fall through to the expansion arms there,
    /// or their verdict would contradict what the expansions say.
    pub(crate) fn cells_agree(&self, other: &Self) -> bool {
        match (self.resolved(), other.resolved()) {
            (Some(a), Some(b)) => a.same_def(&b),
            _ => true,
        }
    }

    /// PURE compute of what this ref's name means in `env` — never
    /// reads or writes the cell (a def-gate probe must not fill a
    /// cell at a mid-compile registration horizon).
    pub(crate) fn resolve_pure(&self, env: &Env) -> Option<Arc<ResolvedRef>> {
        env.resolve_visible(&self.scope, &self.name, crate::env::NameNs::Type, |s, n| {
            env.typedefs.get(s).and_then(|m| m.get(n)).map(|d| {
                Arc::new(ResolvedRef {
                    canonical_scope: ModPath(netidx_core::path::Path::from(
                        arcstr::ArcStr::from(s),
                    )),
                    pos: d.pos,
                    ori: d.ori.clone(),
                    params: d.params.clone(),
                    typ: d.typ.clone(),
                })
            })
        })
        .map_err(|e| {
            // resolution failures surface as UnresolvableRef at the
            // consumer; without this, a structural error (ambiguous
            // glob) would masquerade as "undefined type"
            log::warn!("resolving type `{}` in `{}`: {e:#}", self.name, self.scope)
        })
        .ok()
        .flatten()
    }

    /// Resolve this ref's name in `env` and fill the cell (write-once)
    /// if it is empty; `None` iff the name is not visible AND the cell
    /// is empty. An existing resolution always wins — the snapshot is
    /// computed WITHOUT the cell lock held (resolution can re-enter
    /// through constraint checking, and callers may hold TVar guards).
    /// Returns whether THIS call performed the fill.
    fn resolve_in_raw(&self, env: &Env) -> Option<(Arc<ResolvedRef>, bool)> {
        if let Some(r) = self.resolved() {
            return Some((r, false));
        }
        let r = self.resolve_pure(env)?;
        let mut guard = self.resolved.lock();
        match &*guard {
            Some(r) => Some((r.clone(), false)),
            None => {
                *guard = Some(r.clone());
                Some((r, true))
            }
        }
    }

    /// [`Self::resolve_in_raw`] without the fill flag. Fills ONLY
    /// this ref — deliberately NOT transitive: an eager seed of the
    /// snapshot's nested refs resolves names at the TOUCHING walk's
    /// time, and mid-compile the registration horizon is incomplete
    /// (a sibling submodule's type referenced from a union body
    /// resolves to an outer shadow — tui's `list::List` captured the
    /// list PACKAGE's type during an earlier sibling's def gate).
    /// Lazy expansion is order-correct: nested refs fill when a walk
    /// genuinely needs them, which happens at typecheck time under
    /// the full env.
    pub(crate) fn resolve_in(&self, env: &Env) -> Option<Arc<ResolvedRef>> {
        self.resolve_in_raw(env).map(|(r, _)| r)
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
            resolved: Arc::default(),
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

#[derive(Debug, Clone, Eq, PartialOrd, Ord, Hash, Pack)]
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
    /// The native linked list — a compiler-known constructor like
    /// `Array` (`design/list_native.md`). The runtime rep is private
    /// to `node::collection::list`: cons = `Value::Array([head,
    /// tail])`, nil = the static empty `ValArray`.
    List(Arc<Type>),
    ByRef(Arc<Type>),
    Tuple(Arc<[Type]>),
    Struct(Arc<[(ArcStr, Type)]>),
    Variant(ArcStr, Arc<[Type]>),
    Map {
        key: Arc<Type>,
        value: Arc<Type>,
    },
    Abstract {
        id: AbstractId,
        params: Arc<[Type]>,
    },
    /// A type constructor applied to one argument — `self<'a>` in a
    /// trait signature, `'c<i64>` in generic code. The constructor is a
    /// type variable that binds to a type with a [`Type::Hole`] in its
    /// last parameter; once it is bound the application normalizes to
    /// the filled type ([`Type::app`]). `design/recursive_activations.md`
    /// §7.
    App(Arc<Type>, Arc<Type>),
    /// The hole in a type constructor, written `'_`: the last parameter
    /// of a constructor trait's impl head (`impl Collection for
    /// Array<'_>`), and what a constructor variable binds to. Legal
    /// nowhere else.
    Hole,
}

/// Structural equality (the derived relation), with content-Arc pointer
/// SHORTCUTS: the copy-on-write type walks share aggressively, so equal
/// types are routinely pointer-identical — the derived tree walk paid
/// full structural comparison (and `FnType`'s constraint-view machinery)
/// per shared occurrence, which went super-linear over widget-scale
/// unions (2026-07-13). Same relation, exhaustive on `self` so a new
/// variant fails to compile rather than silently comparing unequal.
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        fn slice_eq(a: &Arc<[Type]>, b: &Arc<[Type]>) -> bool {
            (**a).as_ptr() == (**b).as_ptr() || **a == **b
        }
        match self {
            Type::Bottom => matches!(other, Type::Bottom),
            Type::Any => matches!(other, Type::Any),
            Type::Primitive(a) => matches!(other, Type::Primitive(b) if a == b),
            Type::Ref(a) => matches!(other, Type::Ref(b) if a == b),
            Type::Fn(a) => {
                matches!(other, Type::Fn(b) if Arc::ptr_eq(a, b) || a == b)
            }
            Type::Set(a) => matches!(other, Type::Set(b) if slice_eq(a, b)),
            Type::TVar(a) => matches!(other, Type::TVar(b) if a == b),
            Type::Error(a) => {
                matches!(other, Type::Error(b) if Arc::ptr_eq(a, b) || a == b)
            }
            Type::Array(a) => {
                matches!(other, Type::Array(b) if Arc::ptr_eq(a, b) || a == b)
            }
            Type::List(a) => {
                matches!(other, Type::List(b) if Arc::ptr_eq(a, b) || a == b)
            }
            Type::ByRef(a) => {
                matches!(other, Type::ByRef(b) if Arc::ptr_eq(a, b) || a == b)
            }
            Type::Tuple(a) => matches!(other, Type::Tuple(b) if slice_eq(a, b)),
            Type::Struct(a) => matches!(
                other,
                Type::Struct(b) if (**a).as_ptr() == (**b).as_ptr() || **a == **b
            ),
            Type::Variant(t0, a) => {
                matches!(other, Type::Variant(t1, b) if t0 == t1 && slice_eq(a, b))
            }
            Type::Map { key: k0, value: v0 } => matches!(
                other,
                Type::Map { key: k1, value: v1 }
                    if (Arc::ptr_eq(k0, k1) || k0 == k1)
                        && (Arc::ptr_eq(v0, v1) || v0 == v1)
            ),
            Type::Abstract { id: i0, params: p0 } => matches!(
                other,
                Type::Abstract { id: i1, params: p1 } if i0 == i1 && slice_eq(p0, p1)
            ),
            Type::App(c0, a0) => matches!(
                other,
                Type::App(c1, a1)
                    if (Arc::ptr_eq(c0, c1) || c0 == c1)
                        && (Arc::ptr_eq(a0, a1) || a0 == a1)
            ),
            Type::Hole => matches!(other, Type::Hole),
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::Bottom
    }
}

/// See [`Type::lookup_ref`] — the classifiable resolution failure.
#[derive(Debug)]
pub struct UnresolvableRef {
    pub name: ModPath,
    pub scope: ModPath,
}

impl std::fmt::Display for UnresolvableRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "undefined type {} in {}", self.name, self.scope)
    }
}

impl std::error::Error for UnresolvableRef {}

impl Type {
    /// Read-only walk over this type's IMMEDIATE structural children:
    /// `Ref`/`Abstract` params, collection element types, struct field
    /// types, and (via [`FnType::try_for_each_type`]) fn signature
    /// components. `TVar` is a LEAF — cell contents (binding,
    /// constraints) are per-walk policy, never walked here. This is
    /// the single exhaustive child enumeration for query walks: a
    /// recursive walk matches its interesting arms (TVar, and any arm
    /// whose traversal policy differs — e.g. skipping `Ref` params)
    /// and routes everything else through this. See also
    /// [`Self::cow_children`] for rebuild walks, and the "Invariants
    /// for future type walks" section of
    /// `design/type_operation_scaling.md`.
    pub(crate) fn try_for_each_child<B>(
        &self,
        f: &mut impl FnMut(&Type) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        match self {
            Type::Bottom
            | Type::Any
            | Type::Primitive(_)
            | Type::TVar(_)
            | Type::Hole => ControlFlow::Continue(()),
            Type::App(c, a) => {
                f(c)?;
                f(a)
            }
            Type::Ref(tr) => {
                for t in tr.params.iter() {
                    f(t)?;
                }
                ControlFlow::Continue(())
            }
            Type::Abstract { id: _, params } => {
                for t in params.iter() {
                    f(t)?;
                }
                ControlFlow::Continue(())
            }
            Type::Set(ts) | Type::Tuple(ts) | Type::Variant(_, ts) => {
                for t in ts.iter() {
                    f(t)?;
                }
                ControlFlow::Continue(())
            }
            Type::Struct(fs) => {
                for (_, t) in fs.iter() {
                    f(t)?;
                }
                ControlFlow::Continue(())
            }
            Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(t) => f(t),
            Type::Map { key, value } => {
                f(key)?;
                f(value)
            }
            Type::Fn(ft) => ft.try_for_each_type(f),
        }
    }

    /// [`Self::try_for_each_child`] without early exit.
    pub(crate) fn for_each_child(&self, f: &mut impl FnMut(&Type)) {
        let _ = self.try_for_each_child::<()>(&mut |t| {
            f(t);
            ControlFlow::Continue(())
        });
    }

    /// Rebuild this type's IMMEDIATE structural children through `f`
    /// (`None` from `f` = child unchanged); `None` = nothing changed,
    /// keep the original (shared) — the COW discipline every rebuild
    /// walk must follow (`design/type_operation_scaling.md`). Leaves
    /// (including `TVar` — cell handling is per-walk) return `None`.
    /// `Ref` params rebuild through [`TypeRef::with_params`], SHARING
    /// the resolution cell: a params-only rewrite does not change what
    /// the name means (load-bearing for `reset_tvars` — expand_ref's
    /// commit copies must keep their seeded resolutions). A walk that
    /// re-scopes or rebinds refs overrides the `Ref` arm.
    pub(crate) fn cow_children(
        &self,
        f: &mut impl FnMut(&Type) -> Option<Type>,
    ) -> Option<Type> {
        match self {
            Type::Bottom
            | Type::Any
            | Type::Primitive(_)
            | Type::TVar(_)
            | Type::Hole => None,
            Type::App(c, a) => match (f(c), f(a)) {
                (None, None) => None,
                (c2, a2) => Some(Type::app(
                    c2.unwrap_or_else(|| (**c).clone()),
                    a2.unwrap_or_else(|| (**a).clone()),
                )),
            },
            Type::Ref(tr) => Type::cow_slice(&tr.params, |t| f(t))
                .map(|params| Type::Ref(tr.with_params(params))),
            Type::Abstract { id, params } => Type::cow_slice(params, |t| f(t))
                .map(|params| Type::Abstract { id: *id, params }),
            Type::Error(t) => f(t).map(|t| Type::Error(Arc::new(t))),
            Type::Array(t) => f(t).map(|t| Type::Array(Arc::new(t))),
            Type::List(t) => f(t).map(|t| Type::List(Arc::new(t))),
            Type::ByRef(t) => f(t).map(|t| Type::ByRef(Arc::new(t))),
            Type::Map { key, value } => match (f(key), f(value)) {
                (None, None) => None,
                (k, v) => Some(Type::Map {
                    key: k.map(Arc::new).unwrap_or_else(|| key.clone()),
                    value: v.map(Arc::new).unwrap_or_else(|| value.clone()),
                }),
            },
            Type::Tuple(ts) => Type::cow_slice(ts, |t| f(t)).map(Type::Tuple),
            Type::Variant(tag, ts) => {
                Type::cow_slice(ts, |t| f(t)).map(|ts| Type::Variant(tag.clone(), ts))
            }
            Type::Set(ts) => Type::cow_slice(ts, |t| f(t)).map(Type::Set),
            Type::Struct(fs) => {
                Type::cow_slice(fs, |(n, t)| f(t).map(|t| (n.clone(), t)))
                    .map(Type::Struct)
            }
            Type::Fn(ft) => ft.cow_walk(|t| f(t)).map(|ft| Type::Fn(Arc::new(ft))),
        }
    }

    pub fn empty_tvar() -> Self {
        Type::TVar(TVar::default())
    }

    /// Apply a constructor to an argument: a concrete constructor (a
    /// type with a hole) is filled, a variable stays an application
    /// until it binds.
    pub fn app(ctor: Type, arg: Type) -> Type {
        match &ctor {
            Type::TVar(_) => Type::App(Arc::new(ctor), Arc::new(arg)),
            c => c
                .fill_hole(&arg)
                .unwrap_or_else(|| Type::App(Arc::new(ctor), Arc::new(arg))),
        }
    }

    /// A bound constructor's application, filled: the constructor
    /// dereferenced through its cell and its hole replaced by `arg`.
    /// `None` while the constructor is an open variable.
    pub(crate) fn app_filled(ctor: &Type, arg: &Type) -> Option<Type> {
        ctor.with_deref(|c| match c {
            None | Some(Type::TVar(_)) => None,
            Some(c) => c.fill_hole(arg),
        })
    }

    /// The reference behind a type variable: a cell bound (through
    /// other cells) to a reference, or to a constructor application
    /// whose constructor has since bound — `with_deref` reports that
    /// as its filled reference.
    pub(crate) fn ref_behind(&self) -> Option<Type> {
        match self {
            Type::TVar(_) => self.with_deref(|t| match t {
                Some(t @ Type::Ref(_)) => Some(t.clone()),
                _ => None,
            }),
            _ => None,
        }
    }

    /// The other side of a constructor application: dereferenced, then
    /// [`Self::decompose`]d — a reference with parameters by name, a
    /// bare alias through its expansion.
    pub(crate) fn app_split(t: &Type, env: &Env) -> Result<Option<(Type, Type)>> {
        let Some(t) = t.with_deref(|t| t.cloned()) else { return Ok(None) };
        if let Some(parts) = t.decompose() {
            return Ok(Some(parts));
        }
        match &t {
            Type::Ref(tr) if tr.params.is_empty() => Ok(t.lookup_ref(env)?.decompose()),
            _ => Ok(None),
        }
    }

    /// [`Self::app_split`] for a receiver that lost its name: a cell
    /// bound through `contains` holds a typedef's EXPANSION (a list's
    /// union), which decomposes to nothing. The constructor variable's
    /// trait bounds name the candidates — each registered head of such
    /// a trait, filled with a fresh element, is unified against the
    /// receiver on a fresh instantiation, and the one that contains it
    /// and thereby determines the element is the constructor.
    pub(crate) fn app_split_for(
        ctor: &Type,
        t: &Type,
        env: &Env,
    ) -> Result<Option<(Type, Type)>> {
        if let Some(parts) = Self::app_split(t, env)? {
            return Ok(Some(parts));
        }
        let Some(t) = t.with_deref(|t| t.cloned()) else { return Ok(None) };
        let Type::TVar(cv) = ctor else { return Ok(None) };
        let cons = cv.read().typ.read().constraints.clone();
        for c in cons.iter() {
            let Type::Ref(tr) = c else { continue };
            let Some(tid) = env.trait_of_ref(tr) else { continue };
            let Some(heads) = env.impls.get(&tid) else { continue };
            for im in heads.iter() {
                if !matches!(im.target, Type::Ref(_)) {
                    continue;
                }
                let head = im.target.reset_tvars();
                let elem = Type::empty_tvar();
                let Some(filled) = head.fill_hole(&elem) else { continue };
                // the head contains the receiver AND that determined the
                // element: a proper subtype (`[`Nil]` under `List<'_>`)
                // leaves the element open and is not this constructor
                if filled.contains(env, &t)? && elem.with_deref(|e| e.is_some()) {
                    let r = (head.resolve_tvars(), elem.resolve_tvars());
                    if crate::dbgenv::graphix_dbg_bind() {
                        eprintln!("APP-SPLIT recovered ctor={:?} elem={:?}", r.0, r.1);
                    }
                    return Ok(Some(r));
                }
            }
        }
        Ok(None)
    }

    /// How a trait signature spells its receiver: `applied` if `self`
    /// occurs as a constructor (`self<'a>`), `bare` if it occurs as a
    /// type. A trait uses one form throughout.
    pub(crate) fn self_shape(&self, applied: &mut bool, bare: &mut bool) {
        match self {
            Type::App(c, a) if matches!(&**c, Type::TVar(tv) if &*tv.name == "self") => {
                *applied = true;
                a.self_shape(applied, bare)
            }
            Type::TVar(tv) if &*tv.name == "self" => *bare = true,
            t => t.for_each_child(&mut |c| c.self_shape(applied, bare)),
        }
    }

    /// The number of holes in this type.
    pub(crate) fn holes(&self) -> usize {
        match self {
            Type::Hole => 1,
            t => {
                let mut n = 0;
                t.for_each_child(&mut |c| n += c.holes());
                n
            }
        }
    }

    /// A call site's pre-unification of a declared parameter type with
    /// an argument's type, run BEFORE the argument typechecks so an
    /// unannotated callback's parameters take the declared types. A
    /// function-typed argument unifies its parameter positions only
    /// ([`FnType::pre_unify_params`]); anything else unifies whole.
    pub(crate) fn pre_unify_arg(env: &Env, declared: &Type, actual: &Type) -> Result<()> {
        let d = declared.with_deref(|t| t.cloned());
        let a = actual.with_deref(|t| t.cloned());
        match (d, a) {
            (Some(Type::Fn(d)), Some(Type::Fn(a))) => d.pre_unify_params(env, &a),
            _ => declared.contains(env, actual).map(|_| ()),
        }
    }

    /// The type of a parameter whose written type is the trait `tr`:
    /// the fresh bounded quantifier `tv`, applied to a fresh element
    /// when the trait is a constructor trait (`|c: Collection|` ≡
    /// `'c: Collection, c: 'c<'e>`).
    pub(crate) fn trait_param(env: &Env, tv: TVar, tr: &TypeRef) -> Type {
        let hole = env
            .trait_of_ref(tr)
            .and_then(|tid| env.trait_def(tid))
            .is_some_and(|d| d.hole);
        if hole {
            Type::App(Arc::new(Type::TVar(tv)), Arc::new(Type::empty_tvar()))
        } else {
            Type::TVar(tv)
        }
    }

    /// This type with its hole replaced by `arg`; `None` if it has no
    /// hole (it is not a constructor).
    pub fn fill_hole(&self, arg: &Type) -> Option<Type> {
        match self {
            Type::Hole => Some(arg.clone()),
            t => t.cow_children(&mut |c| c.fill_hole(arg)),
        }
    }

    /// The constructor form of this type — its last parameter replaced
    /// by a hole — with that parameter; `None` if the outermost form
    /// has no parameters (it is not a constructor). Decomposition is
    /// syntactic, on the outermost form only: a reference is taken by
    /// name, never expanded.
    pub fn decompose(&self) -> Option<(Type, Type)> {
        match self {
            Type::Array(t) => Some((Type::Array(Arc::new(Type::Hole)), (**t).clone())),
            Type::List(t) => Some((Type::List(Arc::new(Type::Hole)), (**t).clone())),
            Type::Map { key, value } => Some((
                Type::Map { key: key.clone(), value: Arc::new(Type::Hole) },
                (**value).clone(),
            )),
            Type::Ref(tr) if !tr.params.is_empty() => {
                let n = tr.params.len() - 1;
                let params = Arc::from_iter(
                    tr.params.iter().take(n).cloned().chain(iter::once(Type::Hole)),
                );
                Some((Type::Ref(tr.with_params(params)), tr.params[n].clone()))
            }
            Type::Abstract { id, params } if !params.is_empty() => {
                let n = params.len() - 1;
                let ps = Arc::from_iter(
                    params.iter().take(n).cloned().chain(iter::once(Type::Hole)),
                );
                Some((Type::Abstract { id: *id, params: ps }, params[n].clone()))
            }
            _ => None,
        }
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
            Self::App(c, a) => c.is_defined() && a.is_defined(),
            Self::Hole => true,
            Self::Bottom
            | Self::Any
            | Self::Primitive(_)
            | Self::Fn(_)
            | Self::Set(_)
            | Self::Error(_)
            | Self::Array(_)
            | Self::List(_)
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

    /// No TVar anywhere beneath (not following Refs — a ref's own
    /// expansion embeds params, so param tvar-freedom is what callers
    /// gate on; the walker's Ref arm yields exactly the params). A
    /// tvar-free type's identity is stable under `PartialEq`, so it
    /// can key a cache. Cheap short-circuiting walk.
    pub(crate) fn tvar_free(&self) -> bool {
        match self {
            Type::TVar(_) => false,
            t => t
                .try_for_each_child(&mut |c| {
                    if c.tvar_free() {
                        ControlFlow::Continue(())
                    } else {
                        ControlFlow::Break(())
                    }
                })
                .is_continue(),
        }
    }

    /// Deterministically fill the resolution cell of every `Type::Ref`
    /// reachable from this type against `env` — the closure-conversion
    /// moment for a type about to outlive the env that gives its names
    /// meaning (LambdaDef signatures, a sig'd module's private typedef
    /// store, the abstract registry's private bodies). Names not
    /// visible in `env` are skipped silently (forward references fill
    /// later at their first in-context lookup). Recurses through
    /// filled cells' snapshot bodies so nested named types seed
    /// transitively; the permanent visited set (composite addresses +
    /// ref/tvar cell addresses) makes recursive typedefs terminate.
    pub fn seed_refs(&self, env: &Env) {
        struct Seen {
            cells: poolshark::local::LPooled<AHashSet<usize>>,
            nodes: poolshark::local::LPooled<AHashSet<usize>>,
        }
        fn go(t: &Type, env: &Env, seen: &mut Seen) {
            let node = match t {
                Type::Set(a) | Type::Tuple(a) | Type::Variant(_, a) => {
                    Some((**a).as_ptr().addr())
                }
                Type::Struct(a) => Some((**a).as_ptr().addr()),
                Type::Fn(f) => Some((&**f as *const FnType).addr()),
                Type::Error(a) | Type::Array(a) | Type::List(a) | Type::ByRef(a) => {
                    Some((&**a as *const Type).addr())
                }
                _ => None,
            };
            if let Some(node) = node
                && !seen.nodes.insert(node)
            {
                return;
            }
            match t {
                Type::Bottom
                | Type::Any
                | Type::Primitive(_)
                | Type::Abstract { .. }
                | Type::Hole => (),
                Type::App(c, a) => {
                    go(c, env, seen);
                    go(a, env, seen)
                }
                Type::Ref(tr) => {
                    for p in tr.params.iter() {
                        go(p, env, seen);
                    }
                    // Keyed on the CELL, not the ref: with_params
                    // clones share the cell, and the cell (not the
                    // params) is what seeding fills.
                    if !seen.cells.insert(Arc::as_ptr(&tr.resolved).addr()) {
                        return;
                    }
                    if let Some((r, _)) = tr.resolve_in_raw(env) {
                        for (_, constraint) in r.params.iter() {
                            if let Some(c) = constraint {
                                go(c, env, seen);
                            }
                        }
                        go(&r.typ, env, seen);
                    }
                }
                Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(t) => {
                    go(t, env, seen)
                }
                Type::Map { key, value } => {
                    go(key, env, seen);
                    go(value, env, seen);
                }
                Type::Tuple(ts) | Type::Variant(_, ts) | Type::Set(ts) => {
                    for t in ts.iter() {
                        go(t, env, seen);
                    }
                }
                Type::Struct(ts) => {
                    for (_, t) in ts.iter() {
                        go(t, env, seen);
                    }
                }
                Type::TVar(tv) => {
                    let cell = tv.read().typ.clone();
                    if !seen.cells.insert(triomphe::Arc::as_ptr(&cell).addr()) {
                        return;
                    }
                    let bound = cell.read().typ.clone();
                    if let Some(t) = bound {
                        go(&t, env, seen);
                    }
                }
                Type::Fn(f) => {
                    for a in f.args.iter() {
                        go(&a.typ, env, seen);
                    }
                    if let Some(t) = f.vargs.as_ref() {
                        go(t, env, seen);
                    }
                    go(&f.rtype, env, seen);
                    go(&f.throws, env, seen);
                }
            }
        }
        let mut seen = Seen {
            cells: poolshark::local::LPooled::take(),
            nodes: poolshark::local::LPooled::take(),
        };
        go(self, env, &mut seen)
    }

    pub fn lookup_ref(&self, env: &Env) -> Result<Type> {
        match self {
            Self::Ref(tr) => {
                let TypeRef { scope, name, params, pos, ori, resolved: _ } = tr;
                let resolved = tr.resolve_in(env).ok_or_else(|| {
                    anyhow::Error::new(UnresolvableRef {
                        name: name.clone(),
                        scope: scope.clone(),
                    })
                })?;
                let ResolvedRef {
                    canonical_scope,
                    pos: def_pos,
                    ori: def_ori,
                    params: def_params,
                    typ: def_typ,
                } = &*resolved;
                if def_params.len() != params.len() {
                    bail!("{} expects {} type parameters", name, def_params.len());
                }
                if env.lsp_mode {
                    if let (Some(pos), Some(ori)) = (pos, ori) {
                        env.push_type_ref(crate::ide::TypeRefSite {
                            pos: *pos,
                            ori: ori.clone(),
                            name: name.clone(),
                            canonical_scope: canonical_scope.clone(),
                            def_pos: *def_pos,
                            def_ori: def_ori.clone(),
                        });
                    }
                }
                let mut known: LPooled<AHashMap<ArcStr, Type>> = LPooled::take();
                for ((tv, _), arg) in def_params.iter().zip(params.iter()) {
                    known.insert(tv.name.clone(), arg.clone());
                }
                for ((_, constraint), arg) in def_params.iter().zip(params.iter()) {
                    let Some(constraint) = constraint else {
                        continue;
                    };
                    let constraint = constraint.replace_tvars(&known);
                    match arg {
                        Type::TVar(tv) if tv.read().typ.read().typ.is_none() => {
                            tv.add_cell_constraint(constraint)
                        }
                        _ => constraint.check_contains(env, arg)?,
                    }
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
                    let resolved = env
                        .resolve_visible(
                            &tr.scope,
                            &tr.name,
                            crate::env::NameNs::Type,
                            |s, n| {
                                env.typedefs.get(s).and_then(|m| m.get(n)).map(|d| {
                                    let canonical =
                                        ModPath(netidx_core::path::Path::from(
                                            arcstr::ArcStr::from(s),
                                        ));
                                    (canonical, d.pos, d.ori.clone())
                                })
                            },
                        )
                        .ok()
                        .flatten();
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
            Type::TVar(tv) => {
                if let Some(t) = tv.read().typ.read().typ.as_ref() {
                    t.record_ide_refs(env, fallback_scope);
                }
            }
            t => t.for_each_child(&mut |c| c.record_ide_refs(env, fallback_scope)),
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
            Type::App(..) | Type::Hole => None,
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
            | Type::List(_)
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
            Type::App(..) | Type::Hole => false,
            Type::Any
            | Type::Abstract { .. }
            | Type::TVar(_)
            | Type::Primitive(_)
            | Type::Ref(TypeRef { .. })
            | Type::Fn(_)
            | Type::Error(_)
            | Type::Array(_)
            | Type::List(_)
            | Type::ByRef(_)
            | Type::Tuple(_)
            | Type::Struct(_)
            | Type::Variant(_, _)
            | Type::Set(_)
            | Type::Map { .. } => false,
        }
    }

    /// True when the type can never produce a value: `Bottom`, or a
    /// union whose every member (through bound tvar chains) is. An
    /// unbound tvar is NOT provably bottom. Used by display claiming
    /// (`is_custom`) — a never-producing expression vacuously unifies
    /// with any display type, so `contains` alone over-claims it.
    pub fn all_bottom(&self) -> bool {
        crate::stack::ensure_sufficient(|| {
            self.with_deref(|t| match t {
                Some(Type::Bottom) => true,
                Some(Type::Set(s)) => s.iter().all(|t| t.all_bottom()),
                _ => false,
            })
        })
    }

    pub fn with_deref<R, F: FnOnce(Option<&Self>) -> R>(&self, f: F) -> R {
        match self {
            // A constructor application whose constructor has bound IS
            // its filled type (`app_filled`) — every walk sees `self<'b>`
            // with `self := Array` as `Array<'b>`; only an open
            // constructor stays an application.
            Self::App(c, a) => match Self::app_filled(c, a) {
                Some(filled) => filled.with_deref(f),
                None => f(Some(self)),
            },
            Self::Hole => f(Some(self)),
            Self::Bottom
            | Self::Abstract { .. }
            | Self::Any
            | Self::Primitive(_)
            | Self::Fn(_)
            | Self::Set(_)
            | Self::Error(_)
            | Self::Array(_)
            | Self::List(_)
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

    /// Apply the trait-in-type-position rule (`design/traits.md` §1):
    /// a trait named as a PARAMETER's type (`fn(s: Read)`) is a fresh
    /// bounded quantifier — `fn<'s: Read>(s: 's)`, one variable per
    /// parameter, named `#s` so it cannot collide with a written name
    /// and prints back as the trait — and a trait anywhere else (a
    /// return type, a field, an element) is an error, because no value
    /// has a trait as its type. Returns the rewritten type; the same
    /// type when nothing changed.
    pub fn rewrite_trait_args(&self, env: &Env) -> Result<Type> {
        if self.holes() > 0 {
            bail!(
                "'_ is the hole of a constructor trait's implementation target \
                 (`impl Collection for Array<'_>`); it is not a type"
            )
        }
        match self {
            Type::Ref(tr) if env.trait_of_ref(tr).is_some() => bail!(
                "trait {} used as a type: a trait is a bound — write it as a \
                 parameter's type (`fn(x: {})`) or a quantifier's (`fn<'a: {}>`)",
                tr.name,
                tr.name,
                tr.name
            ),
            Type::Fn(ft) => {
                let mut quantifiers: LPooled<Vec<ArcStr>> =
                    ft.quantifiers.iter().cloned().collect();
                let mut changed = false;
                let mut args: LPooled<Vec<FnArgType>> = LPooled::take();
                for (i, a) in ft.args.iter().enumerate() {
                    let typ = match &a.typ {
                        Type::Ref(tr) if env.trait_of_ref(tr).is_some() => {
                            let name: ArcStr = match a.name() {
                                Some(n) => format_compact!("#{n}").as_str().into(),
                                None => format_compact!("#arg{i}").as_str().into(),
                            };
                            let tv = TVar::empty_named(name.clone());
                            tv.add_cell_constraint(a.typ.clone());
                            if !quantifiers.contains(&name) {
                                quantifiers.push(name);
                            }
                            changed = true;
                            Type::trait_param(env, tv, tr)
                        }
                        t => {
                            let r = t.rewrite_trait_args(env)?;
                            changed |= !r.ptr_eq_shallow(t);
                            r
                        }
                    };
                    args.push(FnArgType { kind: a.kind.clone(), typ });
                }
                let vargs = match &ft.vargs {
                    None => None,
                    Some(t) => {
                        let r = t.rewrite_trait_args(env)?;
                        changed |= !r.ptr_eq_shallow(t);
                        Some(r)
                    }
                };
                let rtype = ft.rtype.rewrite_trait_args(env)?;
                changed |= !rtype.ptr_eq_shallow(&ft.rtype);
                let throws = ft.throws.rewrite_trait_args(env)?;
                changed |= !throws.ptr_eq_shallow(&ft.throws);
                if !changed {
                    return Ok(self.clone());
                }
                Ok(Type::Fn(Arc::new(FnType {
                    args: Arc::from_iter(args.drain(..)),
                    vargs,
                    rtype,
                    throws,
                    explicit_throws: ft.explicit_throws,
                    quantifiers: Arc::from_iter(quantifiers.drain(..)),
                    lambda_ids: ft.lambda_ids.clone(),
                })))
            }
            t => {
                let mut err = None;
                let r = t.cow_children(&mut |c| match c.rewrite_trait_args(env) {
                    Ok(r) if r.ptr_eq_shallow(c) => None,
                    Ok(r) => Some(r),
                    Err(e) => {
                        err = Some(e);
                        None
                    }
                });
                match err {
                    Some(e) => Err(e),
                    None => Ok(r.unwrap_or_else(|| self.clone())),
                }
            }
        }
    }

    /// Same allocation or same leaf — the "unchanged" test for walks
    /// that return `self.clone()` when nothing moved.
    fn ptr_eq_shallow(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Fn(a), Type::Fn(b)) => Arc::ptr_eq(a, b),
            (Type::TVar(a), Type::TVar(b)) => a == b,
            (a, b) => a == b,
        }
    }

    pub fn scope_refs(&self, scope: &ModPath) -> Type {
        self.scope_refs_int(scope).unwrap_or_else(|| self.clone())
    }

    /// `None` = no `Ref` or `TVar` anywhere beneath — the caller keeps
    /// the original (shared); cell-free ref-free structure has nothing
    /// to re-scope or re-mint.
    fn scope_refs_int(&self, scope: &ModPath) -> Option<Type> {
        crate::stack::ensure_sufficient(|| self.scope_refs_int_inner(scope))
    }

    fn scope_refs_int_inner(&self, scope: &ModPath) -> Option<Type> {
        match self {
            Type::TVar(tv) => {
                let (bound, cons) = {
                    let cell = tv.read().typ.clone();
                    let cell = cell.read();
                    (cell.typ.clone(), cell.constraints.clone())
                };
                let fresh = match bound {
                    None => TVar::empty_named(tv.name.clone()),
                    Some(typ) => TVar::named(tv.name.clone(), typ.scope_refs(scope)),
                };
                // The re-minted cell keeps the conjunction: a
                // quantifier bound written in an annotation
                // (`fn<'a: Number>(x: 'a)`) lives only on the cell, and
                // dropping it here made every annotated bound vacuous
                // (2026-08-22). A conjunct that reaches this very cell
                // is copied unscoped — re-scoping it would re-mint the
                // cell inside it without end.
                let addr = tv.cell_addr();
                for c in cons.iter() {
                    let c = if crate::typ::tvar::would_cycle_inner(addr, c) {
                        c.clone()
                    } else {
                        c.scope_refs(scope)
                    };
                    fresh.add_cell_constraint(c);
                }
                Some(Type::TVar(fresh))
            }
            Type::Ref(tr) => {
                let params =
                    Arc::from_iter(tr.params.iter().map(|t| t.scope_refs(scope)));
                Some(Type::Ref(tr.with_scope(scope.clone(), params)))
            }
            t => t.cow_children(&mut |c| c.scope_refs_int(scope)),
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
        self.any_as_tvar_int().unwrap_or_else(|| self.clone())
    }

    /// `None` = no `Any` beneath — keep the original (shared). `Ref`
    /// params, `Abstract` params, and `Fn` signatures are LEAVES here
    /// (preserved from the pre-walker code): the unification view
    /// exists for the select arm walk's structural pairs, which never
    /// descend those.
    fn any_as_tvar_int(&self) -> Option<Type> {
        match self {
            Type::Any => Some(Type::empty_tvar()),
            Type::Ref(_) | Type::Fn(_) | Type::Abstract { .. } => None,
            t => t.cow_children(&mut |c| c.any_as_tvar_int()),
        }
    }
}
