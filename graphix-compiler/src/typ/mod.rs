use crate::{
    PRINT_FLAGS, PrintFlag,
    env::{Env, TypeDef},
    expr::ModPath,
    format_with_flags,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx::{publisher::Typ, utils::Either};
use netidx_derive::Pack;
use nohash::IntMap;
use parking_lot::Mutex;
use poolshark::{IsoPoolable, local::LPooled};
use smallvec::SmallVec;
use std::{
    cmp::{Eq, PartialEq},
    fmt::Debug,
    iter,
    ops::{ControlFlow, Deref, DerefMut},
};
use triomphe::Arc;

mod cast;
mod contains;
pub use contains::ContainsFlags;
pub mod fntyp;
mod matches;
mod normalize;
pub(crate) use normalize::{NormKey, norm_key};
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
    epoch: u64,
    next_id: usize,
    /// An Abstract-PAIR comparison returned false somewhere in this
    /// walk — the verdict might flip if the abstract were seen through
    /// its defining module, so a failure is classifiable
    /// [`AbstractOpaque`] (retry via `privatize_type`). Sticky for the
    /// walk: a set-member probe's abstract false can over-tag an
    /// unrelated failure, which only costs a retry that fails honestly.
    /// A failure with the flag CLEAR is final — no name resolution can
    /// change it (the tree-wide `mentions_abstract` classification this
    /// replaces silently dropped genuine mismatches back to dynamic
    /// binding, soak-jul13b cluster A).
    abstract_false: bool,
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
            epoch: 0,
            next_id: 0,
            abstract_false: false,
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
                    None => match env.lookup_typedef(&tr.scope, &tr.name) {
                        Some(def) => (def as *const TypeDef).addr(),
                        None => return None,
                    },
                };
                let params = &tr.params;
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
            _ => None,
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

impl AbstractId {
    pub fn new() -> Self {
        use std::sync::atomic::{AtomicU64, Ordering};
        static NEXT: AtomicU64 = AtomicU64::new(0);
        AbstractId(NEXT.fetch_add(1, Ordering::Relaxed))
    }

    pub fn inner(&self) -> u64 {
        self.0
    }

    pub fn from_inner(i: u64) -> Self {
        AbstractId(i)
    }
}

impl Default for AbstractId {
    fn default() -> Self {
        Self::new()
    }
}

/// What a `TypeRef`'s name means: the snapshot of everything
/// [`Type::lookup_ref`] reads from the env via `find_visible`. Held in
/// the ref's write-once `resolved` cell so a ref first resolved in its
/// NATIVE env becomes an env-independent value — later consumers get
/// the same answer regardless of which env they hold (the def env's
/// private view survives past the def env). Substitution of the ref's
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

    /// Same VIEW, not just same definition: body ALLOCATION identity.
    /// [`Self::same_def`]'s structural equality is blind to nested
    /// resolution cells — an interface typedef body is registered
    /// twice (`bind_sig` into the outer env; the resolver-injected
    /// copy into the module env), and the two are structurally equal
    /// yet carry different views: their nested refs fill from
    /// different envs (abstract outside, concrete inside). The
    /// privatize walk rebinds on view divergence, so it needs this
    /// stronger test. Allocation-free bodies (primitives etc.) have
    /// nothing nested to diverge — structural equality suffices there.
    pub(crate) fn same_view(&self, other: &Self) -> bool {
        if !Arc::ptr_eq(&self.params, &other.params) && self.params != other.params {
            return false;
        }
        match (&self.typ, &other.typ) {
            (Type::Bottom, Type::Bottom) | (Type::Any, Type::Any) => true,
            (Type::Primitive(a), Type::Primitive(b)) => a == b,
            (
                Type::Abstract { id: a, params: pa },
                Type::Abstract { id: b, params: pb },
            ) => a == b && (Arc::ptr_eq(pa, pb) || pa == pb),
            (Type::Ref(a), Type::Ref(b)) => {
                Arc::ptr_eq(&a.resolved, &b.resolved) && a == b
            }
            (Type::Set(x), Type::Set(y))
            | (Type::Tuple(x), Type::Tuple(y))
            | (Type::Variant(_, x), Type::Variant(_, y)) => {
                (**x).as_ptr() == (**y).as_ptr() && self.typ == other.typ
            }
            (Type::Struct(x), Type::Struct(y)) => (**x).as_ptr() == (**y).as_ptr(),
            (Type::Fn(x), Type::Fn(y)) => Arc::ptr_eq(x, y),
            (Type::Error(x), Type::Error(y))
            | (Type::Array(x), Type::Array(y))
            | (Type::ByRef(x), Type::ByRef(y)) => Arc::ptr_eq(x, y),
            (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
                Arc::ptr_eq(k0, k1) && Arc::ptr_eq(v0, v1)
            }
            _ => false,
        }
    }

    /// This resolution re-pointed at an abstract's PRIVATE body
    /// template — the privatize bridge. `formals` are the registry's
    /// positional param names; the ref's own params then substitute
    /// through `lookup_ref` exactly as they did against the public
    /// definition.
    pub(crate) fn private_view(&self, formals: &[ArcStr], body: Type) -> Self {
        ResolvedRef {
            canonical_scope: self.canonical_scope.clone(),
            pos: self.pos,
            ori: self.ori.clone(),
            params: Arc::from_iter(
                formals.iter().map(|n| (TVar::empty_named(n.clone()), None)),
            ),
            typ: body,
        }
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

    /// This ref rebound to an explicit resolution in a FRESH,
    /// pre-filled cell — the original (shared) cell is never
    /// overwritten; that would leak this context's view into every
    /// type aliasing the cell.
    pub(crate) fn rebind_resolution(
        &self,
        params: Arc<[Type]>,
        r: Arc<ResolvedRef>,
    ) -> Self {
        Self {
            scope: self.scope.clone(),
            name: self.name.clone(),
            params,
            pos: self.pos,
            ori: self.ori.clone(),
            resolved: Arc::new(Mutex::new(Some(r))),
        }
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
    /// reads or writes the cell. The privatize walk uses it to detect
    /// a cell/env view divergence without disturbing the shared cell.
    pub(crate) fn resolve_pure(&self, env: &Env) -> Option<Arc<ResolvedRef>> {
        env.find_visible(&self.scope, &self.name, |s, n| {
            env.typedefs.get(s).and_then(|m| m.get(n)).map(|d| {
                Arc::new(ResolvedRef {
                    canonical_scope: ModPath(netidx::path::Path::from(
                        arcstr::ArcStr::from(s),
                    )),
                    pos: d.pos,
                    ori: d.ori.clone(),
                    params: d.params.clone(),
                    typ: d.typ.clone(),
                })
            })
        })
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
    /// the full env. Transitive seeding exists only as the EXPLICIT
    /// [`Type::seed_refs`] walk, invoked where the timing is provably
    /// safe (check_sig's registry copy — after the module body; the
    /// privatize walk's rebinds — at program typecheck).
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
    ByRef(Arc<Type>),
    Tuple(Arc<[Type]>),
    Struct(Arc<[(ArcStr, Type)]>),
    Variant(ArcStr, Arc<[Type]>),
    Map { key: Arc<Type>, value: Arc<Type> },
    Abstract { id: AbstractId, params: Arc<[Type]> },
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
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::Bottom
    }
}

/// Attached (as anyhow context) to `check_contains` failures where
/// either side mentions a [`Type::Abstract`]: abstraction is opacity —
/// the private↔public equivalence exists only through name resolution
/// inside the defining module, so a recheck comparing across the
/// boundary can fail without any semantic contradiction. Static call
/// resolution uses this marker to discard an instance it cannot prove
/// without crossing that boundary; other failures remain fatal.
#[derive(Debug, Clone, Copy)]
pub struct AbstractOpaque;

impl std::fmt::Display for AbstractOpaque {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "the check crossed an abstract-type boundary")
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
            Type::Bottom | Type::Any | Type::Primitive(_) | Type::TVar(_) => {
                ControlFlow::Continue(())
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
            Type::Error(t) | Type::Array(t) | Type::ByRef(t) => f(t),
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
            Type::Bottom | Type::Any | Type::Primitive(_) | Type::TVar(_) => None,
            Type::Ref(tr) => Type::cow_slice(&tr.params, |t| f(t))
                .map(|params| Type::Ref(tr.with_params(params))),
            Type::Abstract { id, params } => Type::cow_slice(params, |t| f(t))
                .map(|params| Type::Abstract { id: *id, params }),
            Type::Error(t) => f(t).map(|t| Type::Error(Arc::new(t))),
            Type::Array(t) => f(t).map(|t| Type::Array(Arc::new(t))),
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
                Type::Error(a) | Type::Array(a) | Type::ByRef(a) => {
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
                Type::Bottom | Type::Any | Type::Primitive(_) | Type::Abstract { .. } => {
                    ()
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
                Type::Error(t) | Type::Array(t) | Type::ByRef(t) => go(t, env, seen),
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
            Self::TVar(tv) => match tv.read().typ.read().typ.as_ref() {
                Some(t) => t.with_deref(f),
                None => f(None),
            },
        }
    }

    pub fn scope_refs(&self, scope: &ModPath) -> Type {
        self.scope_refs_int(scope).unwrap_or_else(|| self.clone())
    }

    /// `None` = no `Ref` or `TVar` anywhere beneath — the caller keeps
    /// the original (shared); cell-free ref-free structure has nothing
    /// to re-scope or re-mint.
    fn scope_refs_int(&self, scope: &ModPath) -> Option<Type> {
        match self {
            Type::TVar(tv) => Some(match tv.read().typ.read().typ.as_ref() {
                None => Type::TVar(TVar::empty_named(tv.name.clone())),
                Some(typ) => {
                    let typ = typ.scope_refs(scope);
                    Type::TVar(TVar::named(tv.name.clone(), typ))
                }
            }),
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
