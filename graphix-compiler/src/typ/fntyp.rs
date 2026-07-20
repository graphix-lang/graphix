use super::AndAc;
use crate::{
    LambdaId,
    env::Env,
    expr::{
        ModPath,
        print::{PrettyBuf, PrettyDisplay},
    },
    typ::{AbstractId, RefHist, TVar, Type, contains::ContainsFlags},
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_derive::Pack;
use nohash::{IntMap, IntSet};
use parking_lot::RwLock;
use poolshark::local::LPooled;
use std::{
    cmp::{Eq, Ordering, PartialEq},
    fmt::{self, Debug, Write},
    hash::{Hash, Hasher},
    ops::ControlFlow,
    sync::{Arc as SArc, Weak},
};
use triomphe::Arc;

/// Position vs label distinction for a function argument.
///
/// Positional args carry an optional source-level name (used for IDE
/// hover/completion; positional names do not contribute to type
/// identity). Labeled args always carry a name — the label IS the
/// call-site key — plus a flag for whether the lambda definition
/// supplied a default value.
#[derive(Debug, Clone, Pack)]
#[pack(unwrapped)]
pub enum FnArgKind {
    Positional { name: Option<ArcStr> },
    Labeled { name: ArcStr, has_default: bool },
}

impl FnArgKind {
    pub fn name(&self) -> Option<&ArcStr> {
        match self {
            FnArgKind::Positional { name } => name.as_ref(),
            FnArgKind::Labeled { name, .. } => Some(name),
        }
    }

    pub fn label(&self) -> Option<&ArcStr> {
        match self {
            FnArgKind::Labeled { name, .. } => Some(name),
            FnArgKind::Positional { .. } => None,
        }
    }

    pub fn is_labeled(&self) -> bool {
        matches!(self, FnArgKind::Labeled { .. })
    }

    pub fn is_positional(&self) -> bool {
        matches!(self, FnArgKind::Positional { .. })
    }

    pub fn has_default(&self) -> bool {
        matches!(self, FnArgKind::Labeled { has_default: true, .. })
    }
}

// Positional names are documentation; only the discriminator matters
// for positional. Labeled args participate fully in equality/ordering
// since the label is the call-site key and `has_default` is part of
// the type's shape (it determines whether callers can omit the arg).
impl PartialEq for FnArgKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (FnArgKind::Positional { .. }, FnArgKind::Positional { .. }) => true,
            (
                FnArgKind::Labeled { name: n0, has_default: d0 },
                FnArgKind::Labeled { name: n1, has_default: d1 },
            ) => n0 == n1 && d0 == d1,
            _ => false,
        }
    }
}

impl Eq for FnArgKind {}

impl PartialOrd for FnArgKind {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FnArgKind {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (FnArgKind::Positional { .. }, FnArgKind::Positional { .. }) => {
                Ordering::Equal
            }
            (FnArgKind::Positional { .. }, FnArgKind::Labeled { .. }) => Ordering::Less,
            (FnArgKind::Labeled { .. }, FnArgKind::Positional { .. }) => {
                Ordering::Greater
            }
            (
                FnArgKind::Labeled { name: n0, has_default: d0 },
                FnArgKind::Labeled { name: n1, has_default: d1 },
            ) => n0.cmp(n1).then_with(|| d0.cmp(d1)),
        }
    }
}

impl std::hash::Hash for FnArgKind {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Mirror PartialEq: Positional ignores the documentation name;
        // Labeled hashes name + has_default.
        match self {
            FnArgKind::Positional { .. } => 0u8.hash(state),
            FnArgKind::Labeled { name, has_default } => {
                1u8.hash(state);
                name.hash(state);
                has_default.hash(state);
            }
        }
    }
}

#[derive(Debug, Clone, Pack)]
#[pack(unwrapped)]
pub struct FnArgType {
    pub kind: FnArgKind,
    pub typ: Type,
}

impl FnArgType {
    pub fn name(&self) -> Option<&ArcStr> {
        self.kind.name()
    }

    pub fn label(&self) -> Option<&ArcStr> {
        self.kind.label()
    }

    pub fn is_labeled(&self) -> bool {
        self.kind.is_labeled()
    }

    pub fn is_positional(&self) -> bool {
        self.kind.is_positional()
    }

    pub fn has_default(&self) -> bool {
        self.kind.has_default()
    }
}

impl PartialEq for FnArgType {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.typ == other.typ
    }
}

impl Eq for FnArgType {}

impl PartialOrd for FnArgType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FnArgType {
    fn cmp(&self, other: &Self) -> Ordering {
        self.kind.cmp(&other.kind).then_with(|| self.typ.cmp(&other.typ))
    }
}

impl std::hash::Hash for FnArgType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        self.typ.hash(state);
    }
}

#[derive(Debug)]
struct Link(Weak<RwLock<LambdaIdsInner>>);

impl Hash for Link {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (Weak::as_ptr(&self.0) as usize).hash(state)
    }
}

impl nohash::IsEnabled for Link {}

impl PartialEq for Link {
    fn eq(&self, other: &Self) -> bool {
        Weak::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Link {}

impl Link {
    fn upgrade(&self) -> Option<LambdaIds> {
        Weak::upgrade(&self.0).map(LambdaIds)
    }

    fn addr(&self) -> usize {
        Weak::as_ptr(&self.0) as usize
    }
}

#[derive(Debug, Default)]
struct LambdaIdsInner {
    own: Option<LambdaId>,
    links: IntSet<Link>,
}

impl LambdaIdsInner {
    fn walk<F: FnMut(LambdaId)>(&self, visited: &mut IntSet<usize>, f: &mut F) {
        if let Some(id) = self.own {
            f(id)
        }
        for link in &self.links {
            if visited.insert(link.addr())
                && let Some(link) = link.upgrade()
            {
                link.0.read().walk(visited, f)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct LambdaIds(SArc<RwLock<LambdaIdsInner>>);

impl Default for LambdaIds {
    fn default() -> Self {
        Self(SArc::new(RwLock::new(LambdaIdsInner::default())))
    }
}

impl LambdaIds {
    pub fn set_id(&self, id: LambdaId) {
        self.0.write().own = Some(id)
    }

    pub fn ids(&self) -> LPooled<IntSet<LambdaId>> {
        let mut visited: LPooled<IntSet<usize>> = LPooled::take();
        let mut ids: LPooled<IntSet<LambdaId>> = LPooled::take();
        visited.insert(SArc::as_ptr(&self.0) as usize);
        self.0.read().walk(&mut visited, &mut |id| {
            ids.insert(id);
        });
        ids
    }

    pub(crate) fn own(&self) -> Option<LambdaId> {
        self.0.read().own
    }

    pub fn link(&self, other: &LambdaIds) {
        self.0.write().links.insert(other.as_link());
        other.0.write().links.insert(self.as_link());
    }

    fn as_link(&self) -> Link {
        Link(SArc::downgrade(&self.0))
    }
}

/// A function signature. Since tvar-constraints phase C the CELLS are
/// the only constraint store: a quantifier like `fn<'a: Number>`
/// seeds `'a`'s cell conjunction at construction, and every consumer
/// that used to read the retired `constraints` LIST derives its view
/// from the signature's reachable cells ([`FnType::constraint_view`]).
#[derive(Debug, Clone)]
pub struct FnType {
    pub args: Arc<[FnArgType]>,
    pub vargs: Option<Type>,
    pub rtype: Type,
    pub throws: Type,
    pub explicit_throws: bool,
    /// The quantifier NAMES this fn type's `fn<...>` header declared,
    /// in source order. Syntax, not semantics — the constraint TYPES
    /// live in the named cells' conjunctions. Recorded because the
    /// declaration SITE is not derivable from cells: an inner fn that
    /// merely mentions a quantifier (`fn<'a: fn(x: 'a) -> _>`'s
    /// constraint) reaches the same cell and conjunct as the declaring
    /// header, and a view that can't tell them apart re-prints and
    /// re-compares the header at every occurrence — infinite regress
    /// on self-referential constraints. Excluded from Eq/Ord/Hash:
    /// type identity is [`FnType::constraint_view`] (declared names ∩
    /// reachable single-conjunct cells).
    pub quantifiers: Arc<[ArcStr]>,
    /// accumulated set of all LambdaIds this type might represent
    pub lambda_ids: LambdaIds,
}

impl FnType {
    /// The tvar cells reachable from the SIGNATURE components (args /
    /// vargs / rtype / throws), by name. The retired constraints
    /// list's tvars shared these cells by construction (phase B), so
    /// this is the complete cell set every list walker used to reach.
    fn sig_tvars(&self) -> LPooled<AHashMap<ArcStr, TVar>> {
        let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        for arg in self.args.iter() {
            arg.typ.collect_tvars(&mut known);
        }
        if let Some(t) = &self.vargs {
            t.collect_tvars(&mut known);
        }
        self.rtype.collect_tvars(&mut known);
        self.throws.collect_tvars(&mut known);
        known
    }

    /// The display/interface view the retired list used to hold:
    /// name-sorted `(tvar, constraint)` pairs for every reachable
    /// cell carrying EXACTLY ONE conjunct. Multi-conjunct cells stay
    /// unlisted (the old listing rule — one type per var; an
    /// approximation could leak into interface matching), and their
    /// conjunction still prints at use sites under `DerefTVars`
    /// ("'a: unbound within A & B").
    pub fn constraint_view(&self) -> LPooled<Vec<(TVar, Type)>> {
        // DECLARED-driven: only names this fn's own header quantified
        // produce pairs. An inner fn mentioning the quantifier reaches
        // the same cell, but its `quantifiers` is empty — that is what
        // terminates the view → conjunct-Fn → view regress on
        // self-referential constraints (`fn<'a: fn(x: 'a) -> _>`), and
        // with exact print fidelity where an extent guard could only
        // elide. The WALKING guard stays as a backstop for cell graphs
        // no surface syntax produces.
        let key = self as *const Self as usize;
        if !Self::walking(|w| w.insert(key)) {
            return LPooled::take();
        }
        let r = (|| {
            let known = self.sig_tvars();
            let mut view: LPooled<Vec<(TVar, Type)>> = LPooled::take();
            for name in self.quantifiers.iter() {
                let Some(tv) = known.get(name) else { continue };
                let cons = tv.cell_constraints();
                if let [tc] = &cons[..] {
                    // NORMALIZED, like the retired list's
                    // `normalize_int` entries: the raw stored conjunct
                    // and its print-then-reparse twin can differ in
                    // Set member form (`_`/Bottom members, ordering)
                    // while denoting the same type — equality and
                    // display must agree on the canonical form
                    // (expr_round_trip4 at 50k cases).
                    view.push((tv.clone(), tc.normalize()));
                }
            }
            view.sort_by(|(a, _), (b, _)| a.name.cmp(&b.name));
            view
        })();
        Self::walking(|w| w.remove(&key));
        r
    }

    /// EVERY reachable single-conjunct cell as (tvar, conjunct)
    /// pairs, declared or not. The IMPL side of [`FnType::sig_matches`]
    /// needs this: an inferred implementation carries its constraints
    /// on auto `'_N` cells that no `fn<...>` header declared, so the
    /// declared-driven `constraint_view` can't see them.
    pub(crate) fn cell_constraint_pairs(&self) -> LPooled<Vec<(TVar, Type)>> {
        let known = self.sig_tvars();
        let mut view: LPooled<Vec<(TVar, Type)>> = LPooled::take();
        for tv in known.values() {
            let cons = tv.cell_constraints();
            if let [tc] = &cons[..] {
                view.push((tv.clone(), tc.normalize()));
            }
        }
        view.sort_by(|(a, _), (b, _)| a.name.cmp(&b.name));
        view
    }

    fn walking<R>(f: impl FnOnce(&mut nohash::IntSet<usize>) -> R) -> R {
        thread_local! {
            static WALKING: std::cell::RefCell<nohash::IntSet<usize>> =
                std::cell::RefCell::new(nohash::IntSet::default());
        }
        WALKING.with_borrow_mut(f)
    }
}

impl PartialEq for FnType {
    fn eq(&self, other: &Self) -> bool {
        let Self {
            args: args0,
            vargs: vargs0,
            rtype: rtype0,
            throws: th0,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = self;
        let Self {
            args: args1,
            vargs: vargs1,
            rtype: rtype1,
            throws: th1,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = other;
        args0 == args1
            && vargs0 == vargs1
            && rtype0 == rtype1
            && th0 == th1
            && *self.constraint_view() == *other.constraint_view()
    }
}

impl Eq for FnType {}

impl PartialOrd for FnType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use std::cmp::Ordering;
        let Self {
            args: args0,
            vargs: vargs0,
            rtype: rtype0,
            throws: th0,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = self;
        let Self {
            args: args1,
            vargs: vargs1,
            rtype: rtype1,
            throws: th1,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = other;
        match args0.partial_cmp(&args1) {
            Some(Ordering::Equal) => match vargs0.partial_cmp(vargs1) {
                Some(Ordering::Equal) => match rtype0.partial_cmp(rtype1) {
                    Some(Ordering::Equal) => {
                        match (*self.constraint_view())
                            .partial_cmp(&*other.constraint_view())
                        {
                            Some(Ordering::Equal) => th0.partial_cmp(th1),
                            r => r,
                        }
                    }
                    r => r,
                },
                r => r,
            },
            r => r,
        }
    }
}

impl Ord for FnType {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl std::hash::Hash for FnType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Mirror PartialEq: include args, vargs, rtype, the
        // cell-derived constraint view, throws. Skip lambda_ids
        // (provenance) and explicit_throws (the pretty-printer
        // doesn't preserve it through round-trip when
        // `throws == Bottom`, so equality would break parser
        // round-trip tests; for fusion's monomorphization cache,
        // explicit_throws is folded into the key separately).
        let Self {
            args,
            vargs,
            rtype,
            throws,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = self;
        args.hash(state);
        vargs.hash(state);
        rtype.hash(state);
        self.constraint_view().hash(state);
        throws.hash(state);
    }
}

impl Default for FnType {
    fn default() -> Self {
        Self {
            args: Arc::from_iter([]),
            vargs: None,
            rtype: Default::default(),
            throws: Default::default(),
            explicit_throws: false,
            quantifiers: Arc::from_iter([]),
            lambda_ids: LambdaIds::default(),
        }
    }
}

impl FnType {
    /// `None` = already normal — the caller keeps the original (shared).
    pub(super) fn normalize_int(
        &self,
        cx: &mut super::normalize::NormCx,
    ) -> Option<Self> {
        self.cow_walk(|t| t.normalize_int(cx))
    }

    /// Snapshot with all bound TVars replaced by their concrete types.
    /// TVar-free parts are returned SHARED — see [`Type::resolve_tvars`].
    pub fn resolve_tvars(&self) -> Self {
        self.resolve_tvars_seen_int(&mut super::normalize::ResolveTvarsCx::take())
            .unwrap_or_else(|| self.clone())
    }

    /// `None` = no TVar anywhere beneath — the caller keeps the original.
    pub(super) fn resolve_tvars_seen_int(
        &self,
        cx: &mut super::normalize::ResolveTvarsCx,
    ) -> Option<Self> {
        self.cow_walk(|t| t.resolve_tvars_seen_int(cx))
    }

    /// Rewrite every type position through `f` (`None` from `f` =
    /// unchanged), rebuilding only if something changed.
    pub(crate) fn cow_walk(
        &self,
        mut f: impl FnMut(&Type) -> Option<Type>,
    ) -> Option<Self> {
        let Self { args, vargs, rtype, throws, explicit_throws, quantifiers, lambda_ids } =
            self;
        let new_args = Type::cow_slice(args, |a| {
            f(&a.typ).map(|typ| FnArgType { kind: a.kind.clone(), typ })
        });
        let new_vargs = vargs.as_ref().and_then(&mut f);
        let new_rtype = f(rtype);
        let new_throws = f(throws);
        if new_args.is_none()
            && new_vargs.is_none()
            && new_rtype.is_none()
            && new_throws.is_none()
        {
            return None;
        }
        Some(FnType {
            args: new_args.unwrap_or_else(|| args.clone()),
            vargs: match new_vargs {
                Some(t) => Some(t),
                None => vargs.clone(),
            },
            rtype: new_rtype.unwrap_or_else(|| rtype.clone()),
            throws: new_throws.unwrap_or_else(|| throws.clone()),
            explicit_throws: *explicit_throws,
            quantifiers: quantifiers.clone(),
            lambda_ids: lambda_ids.clone(),
        })
    }

    /// Read-only walk over the signature's component types, in
    /// signature order: args, vargs, rtype, throws. Sig-cell
    /// CONSTRAINTS are not visited — walks that need them use
    /// [`Self::for_each_sig_constraint`]. This is the single component
    /// enumeration behind both the FnType query walks and
    /// [`Type::try_for_each_child`]'s `Fn` arm.
    pub(crate) fn try_for_each_type<B>(
        &self,
        f: &mut impl FnMut(&Type) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        let FnType {
            args,
            vargs,
            rtype,
            throws,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = self;
        for arg in args.iter() {
            f(&arg.typ)?;
        }
        if let Some(t) = vargs {
            f(t)?;
        }
        f(rtype)?;
        f(throws)
    }

    /// [`Self::try_for_each_type`] without early exit.
    pub(crate) fn for_each_type(&self, f: &mut impl FnMut(&Type)) {
        let _ = self.try_for_each_type::<()>(&mut |t| {
            f(t);
            ControlFlow::Continue(())
        });
    }

    /// Visit each signature cell's constraint conjuncts — the walk
    /// `alias_tvars`/`unfreeze_tvars`/`collect_tvars` need beyond the
    /// component types (constraint TYPES live in the cells since phase
    /// C). Guarded per `FnType` address: a conjunct Fn reaching back
    /// here would recurse forever (see `constraint_view`).
    fn for_each_sig_constraint(&self, f: &mut impl FnMut(&Type)) {
        let key = self as *const Self as usize;
        if Self::walking(|w| w.insert(key)) {
            for tv in self.sig_tvars().values() {
                for tc in tv.cell_constraints() {
                    f(&tc);
                }
            }
            Self::walking(|w| w.remove(&key));
        }
    }

    pub fn unbind_tvars(&self) {
        self.for_each_type(&mut |t| t.unbind_tvars())
    }

    /// [`Type::unbind_open_tvars`] over the signature — closed
    /// def-body facts stay bound.
    pub fn unbind_open_tvars(&self) {
        self.for_each_type(&mut |t| t.unbind_open_tvars())
    }

    /// Record the def gate's inferred facts. `closed_only` is the
    /// NESTED-gate mode (def_gate_depth > 1): a nested lambda's cells
    /// can still be entangled with the enclosing lambda's in-flight
    /// inference, so only bindings with NO open interior cells are
    /// recorded — a fully-closed fact (`'n := i64` from `n == i64:3`)
    /// is true regardless of how the enclosing solve finishes, while a
    /// partial one (`Array<'b-unbound>`) snapshots a mid-solve state
    /// the enclosing gate may still revise (the 8630436f scoping
    /// concern; recording those regressed firing-jul2026/03).
    pub fn constrain_known(&self, closed_only: bool) {
        let mut known = LPooled::take();
        self.collect_tvars(&mut known);
        for (_, tv) in known.drain() {
            // clone the binding OUT of the cell guards before acting —
            // add_cell_constraint write-locks the same cell (lock
            // discipline, see CLAUDE.md emit contracts)
            let bound = tv.read().typ.read().typ.clone();
            if closed_only {
                match &bound {
                    Some(t)
                        if *t != Type::Bottom && *t != Type::Any && !t.has_unbound() => {}
                    _ => continue,
                }
            }
            if let Some(t) = bound
                && t != Type::Bottom
                && t != Type::Any
            {
                // Snapshot with PRIVATE cells (`reset_tvars`), and
                // leave still-open leaves OPEN: a binding whose
                // interior cell is unbound is a PARTIAL fact, and
                // closing the leaf (the old `bind_as(Any)`) turned
                // "an array of something not yet solved" into the
                // false fact `Array<Any>` — `settle` then
                // materialized it as a real binding and every
                // instance of a nested generic def inherited an
                // element type the body never delivers. The fresh open
                // leaf still carries the source cell's constraint
                // conjunction, so the obligation survives without
                // the lie.
                let t = t.reset_tvars();
                let tc = t.normalize();
                if std::env::var("GRAPHIX_DBG_BIND").is_ok() {
                    eprintln!(
                        "CONSTRAIN-KNOWN '{}({:x}) += {tc:?}",
                        tv.name,
                        tv.cell_addr()
                    );
                }
                // The def-time binding is a FACT every instance must
                // honor (observation #4): a cell conjunct — the cell
                // is the ONLY store (phase C). It survives the
                // def-time unbind and per-site freshening, and
                // argument unification checks it AT THE BINDING ARG;
                // display/interface listings derive from the cells
                // (`constraint_view`).
                tv.add_cell_constraint(tc);
            }
        }
    }

    pub fn reset_tvars(&self) -> Self {
        self.reset_tvars_int(&mut LPooled::take()).unwrap_or_else(|| self.clone())
    }

    /// One cell-identity freshening map across the whole signature —
    /// see [`Type::reset_tvars_int`]. Cell constraint conjunctions
    /// travel with the cells (the TVar-level reset copies them), so
    /// nothing beyond the signature components needs freshening.
    /// `None` = no TVar anywhere beneath — keep the original (shared).
    pub(super) fn reset_tvars_int(
        &self,
        known: &mut AHashMap<usize, TVar>,
    ) -> Option<Self> {
        self.cow_walk(|t| t.reset_tvars_int(known))
    }

    pub fn replace_tvars(&self, known: &AHashMap<ArcStr, Type>) -> Self {
        self.replace_tvars_int(known, &mut LPooled::take())
            .unwrap_or_else(|| self.clone())
    }

    /// `None` = no TVar anywhere beneath — keep the original (shared).
    pub(super) fn replace_tvars_int(
        &self,
        known: &AHashMap<ArcStr, Type>,
        renamed: &mut AHashMap<ArcStr, TVar>,
    ) -> Option<Self> {
        self.cow_walk(|t| t.replace_tvars_int(known, renamed))
    }

    /// Replace automatically constrained type variables (those with
    /// underscore-prefixed names like `'_23`) with their constraint type.
    /// This is only useful for making nicer display types in IDEs and
    /// shells.
    ///
    /// Ordering: when combining with `Type::resolve_tvars` to fully
    /// pretty a function signature, call `replace_auto_constrained`
    /// FIRST and `resolve_tvars` SECOND. `resolve_tvars` empties the
    /// constraint table, so reversing the order leaves the auto
    /// constraints with nothing to fold against.
    pub fn replace_auto_constrained(&self) -> Self {
        let mut known: LPooled<AHashMap<ArcStr, Type>> = LPooled::take();
        let Self { args, vargs, rtype, throws, explicit_throws, quantifiers, lambda_ids } =
            self;
        // Auto ('_N) single-conjunct cells fold to their constraint
        // type for display; named quantifiers keep their cells (the
        // constraint stays visible via `constraint_view`). Read the
        // CELLS directly, not `constraint_view`: auto names come from
        // INFERENCE, never from a declared `fn<...>` header, so the
        // declared-driven view can't see them.
        for (name, tv) in self.sig_tvars().drain() {
            if name.starts_with('_')
                && let [tc] = &tv.cell_constraints()[..]
            {
                known.insert(name, tc.clone());
            }
        }
        let mut all_tvars: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        self.collect_tvars(&mut all_tvars);
        for (name, tv) in all_tvars.drain() {
            if !known.contains_key(&name) {
                known.insert(name, Type::TVar(tv));
            }
        }
        let args = Arc::from_iter(args.iter().map(|FnArgType { kind, typ }| FnArgType {
            kind: kind.clone(),
            typ: typ.replace_tvars(&known),
        }));
        let vargs = vargs.as_ref().map(|t| t.replace_tvars(&known));
        let rtype = rtype.replace_tvars(&known);
        let throws = throws.replace_tvars(&known);
        let explicit_throws = *explicit_throws;
        let lambda_ids = lambda_ids.clone();
        Self {
            args,
            vargs,
            rtype,
            throws,
            explicit_throws,
            quantifiers: quantifiers.clone(),
            lambda_ids,
        }
    }

    pub fn has_unbound(&self) -> bool {
        self.try_for_each_type(&mut |t| {
            if t.has_unbound() {
                ControlFlow::Break(())
            } else {
                ControlFlow::Continue(())
            }
        })
        .is_break()
    }

    pub fn bind_as(&self, t: &Type) {
        self.for_each_type(&mut |x| x.bind_as(t))
    }

    // The three walks below visit the sig-cell constraints BETWEEN
    // rtype and throws — the pre-walker component order, preserved
    // exactly: for `alias_tvars` the first-seen occurrence of a name
    // becomes the surviving cell, so component order is observable.
    // Hence the explicit sequence instead of `for_each_type`.

    pub fn alias_tvars(&self, known: &mut AHashMap<ArcStr, TVar>) {
        let FnType {
            args,
            vargs,
            rtype,
            throws,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = self;
        for arg in args.iter() {
            arg.typ.alias_tvars(known)
        }
        if let Some(vargs) = vargs {
            vargs.alias_tvars(known)
        }
        rtype.alias_tvars(known);
        self.for_each_sig_constraint(&mut |tc| tc.alias_tvars(known));
        throws.alias_tvars(known);
    }

    pub fn unfreeze_tvars(&self) {
        let FnType {
            args,
            vargs,
            rtype,
            throws,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = self;
        for arg in args.iter() {
            arg.typ.unfreeze_tvars()
        }
        if let Some(vargs) = vargs {
            vargs.unfreeze_tvars()
        }
        rtype.unfreeze_tvars();
        self.for_each_sig_constraint(&mut |tc| tc.unfreeze_tvars());
        throws.unfreeze_tvars();
    }

    pub fn collect_tvars(&self, known: &mut AHashMap<ArcStr, TVar>) {
        let FnType {
            args,
            vargs,
            rtype,
            throws,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = self;
        for arg in args.iter() {
            arg.typ.collect_tvars(known)
        }
        if let Some(vargs) = vargs {
            vargs.collect_tvars(known)
        }
        rtype.collect_tvars(known);
        self.for_each_sig_constraint(&mut |tc| tc.collect_tvars(known));
        throws.collect_tvars(known);
    }

    pub fn contains(&self, env: &Env, t: &Self) -> Result<bool> {
        self.contains_int(
            ContainsFlags::AliasTVars | ContainsFlags::InitTVars,
            env,
            &mut RefHist::new(LPooled::take()),
            t,
        )
    }

    pub(super) fn contains_int(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut RefHist<AHashMap<(Option<usize>, Option<usize>), bool>>,
        t: &Self,
    ) -> Result<bool> {
        let mut sul = 0;
        let mut tul = 0;
        for (i, a) in self.args.iter().enumerate() {
            sul = i;
            match &a.kind {
                FnArgKind::Positional { .. } => {
                    break;
                }
                FnArgKind::Labeled { name: l, .. } => {
                    match t.args.iter().find(|a| a.label() == Some(l)) {
                        None => return Ok(false),
                        Some(o) => {
                            if !o.typ.contains_int(flags, env, hist, &a.typ)? {
                                return Ok(false);
                            }
                        }
                    }
                }
            }
        }
        for (i, a) in t.args.iter().enumerate() {
            tul = i;
            match &a.kind {
                FnArgKind::Positional { .. } => {
                    break;
                }
                FnArgKind::Labeled { name: l, has_default } => {
                    match self.args.iter().find(|a| a.label() == Some(l)) {
                        Some(_) => (),
                        None => {
                            if !*has_default {
                                return Ok(false);
                            }
                        }
                    }
                }
            }
        }
        let slen = self.args.len() - sul;
        let tlen = t.args.len() - tul;
        Ok(slen == tlen
            && t.args[tul..]
                .iter()
                .zip(self.args[sul..].iter())
                .map(|(t, s)| t.typ.contains_int(flags, env, hist, &s.typ))
                .collect::<Result<AndAc>>()?
                .0
            && match (&t.vargs, &self.vargs) {
                (Some(tv), Some(sv)) => tv.contains_int(flags, env, hist, sv)?,
                (None, None) => true,
                (_, _) => false,
            }
            && self.rtype.contains_int(flags, env, hist, &t.rtype)?
            && self
                .constraint_view()
                .iter()
                .map(|(tv, tc)| {
                    tc.contains_int(flags, env, hist, &Type::TVar(tv.clone()))
                })
                .collect::<Result<AndAc>>()?
                .0
            && t.constraint_view()
                .iter()
                .map(|(tv, tc)| {
                    tc.contains_int(flags, env, hist, &Type::TVar(tv.clone()))
                })
                .collect::<Result<AndAc>>()?
                .0
            && self.throws.contains_int(flags, env, hist, &t.throws)?)
    }

    pub fn check_contains(&self, env: &Env, other: &Self) -> Result<()> {
        if !self.contains(env, other)? {
            bail!("Fn type mismatch {self} does not contain {other}")
        }
        Ok(())
    }

    /// Return true if function signatures are contained. This is contains,
    /// but does not allow labeled argument subtyping.
    pub fn sig_contains(&self, env: &Env, other: &Self) -> Result<bool> {
        let Self {
            args: args0,
            vargs: vargs0,
            rtype: rtype0,
            throws: tr0,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = self;
        let Self {
            args: args1,
            vargs: vargs1,
            rtype: rtype1,
            throws: tr1,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = other;
        Ok(args0.len() == args1.len()
            && args0
                .iter()
                .zip(args1.iter())
                .map(|(a0, a1)| Ok(a0.kind == a1.kind && a0.typ.contains(env, &a1.typ)?))
                .collect::<Result<AndAc>>()?
                .0
            && match (vargs0, vargs1) {
                (None, None) => true,
                (None, _) | (_, None) => false,
                (Some(t0), Some(t1)) => t0.contains(env, t1)?,
            }
            && rtype0.contains(env, rtype1)?
            && self
                .constraint_view()
                .iter()
                .map(|(tv, tc)| tc.contains(env, &Type::TVar(tv.clone())))
                .collect::<Result<AndAc>>()?
                .0
            && other
                .constraint_view()
                .iter()
                .map(|(tv, tc)| tc.contains(env, &Type::TVar(tv.clone())))
                .collect::<Result<AndAc>>()?
                .0
            && tr0.contains(env, tr1)?)
    }

    pub fn check_sig_contains(&self, env: &Env, other: &Self) -> Result<()> {
        if !self.sig_contains(env, other)? {
            bail!("Fn signature {self} does not contain {other}")
        }
        Ok(())
    }

    pub fn sig_matches(
        &self,
        env: &Env,
        impl_fn: &Self,
        adts: &mut IntMap<AbstractId, Type>,
    ) -> Result<()> {
        self.sig_matches_int(
            env,
            impl_fn,
            &mut LPooled::take(),
            &mut RefHist::new(LPooled::take()),
            adts,
        )
    }

    pub(super) fn sig_matches_int(
        &self,
        env: &Env,
        impl_fn: &Self,
        tvar_map: &mut IntMap<usize, Type>,
        hist: &mut RefHist<AHashSet<(Option<usize>, Option<usize>)>>,
        adts: &IntMap<AbstractId, Type>,
    ) -> Result<()> {
        let Self {
            args: sig_args,
            vargs: sig_vargs,
            rtype: sig_rtype,
            throws: sig_throws,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = self;
        let Self {
            args: impl_args,
            vargs: impl_vargs,
            rtype: impl_rtype,
            throws: impl_throws,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = impl_fn;
        if sig_args.len() != impl_args.len() {
            bail!(
                "argument count mismatch: signature has {}, implementation has {}",
                sig_args.len(),
                impl_args.len()
            );
        }
        for (i, (sig_arg, impl_arg)) in sig_args.iter().zip(impl_args.iter()).enumerate()
        {
            if sig_arg.kind != impl_arg.kind {
                bail!(
                    "argument {} kind mismatch: signature has {:?}, implementation has {:?}",
                    i,
                    sig_arg.kind,
                    impl_arg.kind
                );
            }
            sig_arg
                .typ
                .sig_matches_int(env, &impl_arg.typ, tvar_map, hist, adts)
                .with_context(|| format!("in argument {i}"))?;
        }
        match (sig_vargs, impl_vargs) {
            (None, None) => (),
            (Some(sig_va), Some(impl_va)) => {
                sig_va
                    .sig_matches_int(env, impl_va, tvar_map, hist, adts)
                    .context("in variadic argument")?;
            }
            (None, Some(_)) => {
                bail!("signature has no variadic args but implementation does")
            }
            (Some(_), None) => {
                bail!("signature has variadic args but implementation does not")
            }
        }
        sig_rtype
            .sig_matches_int(env, impl_rtype, tvar_map, hist, adts)
            .context("in return type")?;
        sig_throws
            .sig_matches_int(env, impl_throws, tvar_map, hist, adts)
            .context("in throws clause")?;
        let sig_cons = self.constraint_view();
        let impl_cons = impl_fn.cell_constraint_pairs();
        for (sig_tv, sig_tc) in sig_cons.iter() {
            if !impl_cons
                .iter()
                .any(|(impl_tv, impl_tc)| sig_tv == impl_tv && sig_tc == impl_tc)
            {
                bail!("missing constraint {sig_tv}: {sig_tc} in implementation")
            }
        }
        // SATISFACTION against the WHOLE conjunction, read from the
        // CELLS: `cell_constraint_pairs`' single-conjunct rule is the
        // display listing rule, and an inference cell routinely holds
        // several conjuncts (the homogeneous-arith Number + the
        // def-gate's constrain_known binding snapshot) — the old
        // pair-list walk skipped exactly those cells, so a sandboxed
        // impl inferred at f64 slipped under a fn(i64) -> i64 sig
        // (dynamic_module1, 2026-07-12). Every conjunct must admit
        // the signature's concrete choice.
        for tv in impl_fn.sig_tvars().values() {
            match tvar_map.get(&tv.inner_addr()).cloned() {
                None | Some(Type::TVar(_)) => (),
                Some(sig_type) => {
                    for impl_tc in tv.cell_constraints() {
                        let ok = impl_tc
                            .contains_int(
                                enumflags2::BitFlags::empty(),
                                env,
                                &mut RefHist::new(LPooled::take()),
                                &sig_type,
                            )
                            .unwrap_or(false);
                        if !ok {
                            bail!(
                                "signature has concrete type {sig_type}, which the \
                                 implementation constraint {impl_tc} does not admit"
                            )
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn map_argpos(
        &self,
        other: &Self,
    ) -> LPooled<AHashMap<ArcStr, (Option<usize>, Option<usize>)>> {
        let mut tbl: LPooled<AHashMap<ArcStr, (Option<usize>, Option<usize>)>> =
            LPooled::take();
        for (i, a) in self.args.iter().enumerate() {
            match &a.kind {
                FnArgKind::Positional { .. } => break,
                FnArgKind::Labeled { name, .. } => {
                    tbl.entry(name.clone()).or_default().0 = Some(i)
                }
            }
        }
        for (i, a) in other.args.iter().enumerate() {
            match &a.kind {
                FnArgKind::Positional { .. } => break,
                FnArgKind::Labeled { name, .. } => {
                    tbl.entry(name.clone()).or_default().1 = Some(i)
                }
            }
        }
        tbl
    }

    pub fn scope_refs(&self, scope: &ModPath) -> Self {
        let vargs = self.vargs.as_ref().map(|t| t.scope_refs(scope));
        let rtype = self.rtype.scope_refs(scope);
        let args =
            Arc::from_iter(self.args.iter().map(|a| FnArgType {
                kind: a.kind.clone(),
                typ: a.typ.scope_refs(scope),
            }));
        let throws = self.throws.scope_refs(scope);
        FnType {
            args,
            rtype,
            vargs,
            throws,
            explicit_throws: self.explicit_throws,
            quantifiers: self.quantifiers.clone(),
            lambda_ids: self.lambda_ids.clone(),
        }
    }
}

impl FnType {
    /// Should the `throws` clause be SUPPRESSED when printing? True
    /// only for the IMPLICIT no-throw shapes — an inferred `Bottom`
    /// ("the body observed nothing"), a TVar bound to `Bottom`, or an
    /// unbound auto-allocated (`'_N`) inference cell — and NEVER when
    /// the user wrote an explicit `throws` clause (`explicit_throws`
    /// tracks intent; see the `explicit_throws_always_shown` and
    /// `unbound_auto_throws_is_hidden` tests). The single predicate
    /// both `Display` and `PrettyDisplay` consult — they used to
    /// disagree (Display suppressed `Bottom` unconditionally).
    fn suppress_throws(&self) -> bool {
        !self.explicit_throws
            && match &self.throws {
                Type::Bottom => true,
                Type::TVar(tv) => {
                    let bound = tv.read().typ.read().typ.clone();
                    match bound {
                        Some(Type::Bottom) => true,
                        None => tv.name.starts_with('_'),
                        Some(_) => false,
                    }
                }
                _ => false,
            }
    }
}

impl fmt::Display for FnType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let constraints = self.constraint_view();
        if constraints.len() == 0 {
            write!(f, "fn(")?;
        } else {
            write!(f, "fn<")?;
            for (i, (tv, t)) in constraints.iter().enumerate() {
                write!(f, "{tv}: {t}")?;
                if i < constraints.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ">(")?;
        }
        for (i, a) in self.args.iter().enumerate() {
            match &a.kind {
                FnArgKind::Labeled { name, has_default: true } => {
                    write!(f, "?#{name}: ")?
                }
                FnArgKind::Labeled { name, has_default: false } => {
                    write!(f, "#{name}: ")?
                }
                FnArgKind::Positional { name: Some(n) } => write!(f, "{n}: ")?,
                FnArgKind::Positional { name: None } => (),
            }
            write!(f, "{}", a.typ)?;
            if i < self.args.len() - 1 || self.vargs.is_some() {
                write!(f, ", ")?;
            }
        }
        if let Some(vargs) = &self.vargs {
            write!(f, "@args: {}", vargs)?;
        }
        match &self.rtype {
            Type::Fn(ft) => write!(f, ") -> ({ft})")?,
            Type::ByRef(t) => match &**t {
                Type::Fn(ft) => write!(f, ") -> &({ft})")?,
                t => write!(f, ") -> &{t}")?,
            },
            t => write!(f, ") -> {t}")?,
        }
        if self.suppress_throws() {
            Ok(())
        } else {
            write!(f, " throws {}", &self.throws)
        }
    }
}

impl PrettyDisplay for FnType {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let constraints = self.constraint_view();
        if constraints.is_empty() {
            writeln!(buf, "fn(")?;
        } else {
            writeln!(buf, "fn<")?;
            buf.with_indent(2, |buf| {
                for (i, (tv, t)) in constraints.iter().enumerate() {
                    write!(buf, "{tv}: ")?;
                    buf.with_indent(2, |buf| t.fmt_pretty(buf))?;
                    if i < constraints.len() - 1 {
                        buf.kill_newline();
                        writeln!(buf, ",")?;
                    }
                }
                Ok(())
            })?;
            writeln!(buf, ">(")?;
        }
        buf.with_indent(2, |buf| {
            for (i, a) in self.args.iter().enumerate() {
                match &a.kind {
                    FnArgKind::Labeled { name, has_default: true } => {
                        write!(buf, "?#{name}: ")?
                    }
                    FnArgKind::Labeled { name, has_default: false } => {
                        write!(buf, "#{name}: ")?
                    }
                    FnArgKind::Positional { name: Some(n) } => write!(buf, "{n}: ")?,
                    FnArgKind::Positional { name: None } => (),
                }
                buf.with_indent(2, |buf| a.typ.fmt_pretty(buf))?;
                if i < self.args.len() - 1 || self.vargs.is_some() {
                    buf.kill_newline();
                    writeln!(buf, ",")?;
                }
            }
            if let Some(vargs) = &self.vargs {
                write!(buf, "@args: ")?;
                buf.with_indent(2, |buf| vargs.fmt_pretty(buf))?;
            }
            Ok(())
        })?;
        match &self.rtype {
            Type::Fn(ft) => {
                write!(buf, ") -> (")?;
                ft.fmt_pretty(buf)?;
                buf.kill_newline();
                writeln!(buf, ")")?;
            }
            Type::ByRef(t) => match &**t {
                Type::Fn(ft) => {
                    write!(buf, ") -> &(")?;
                    ft.fmt_pretty(buf)?;
                    buf.kill_newline();
                    writeln!(buf, ")")?;
                }
                t => {
                    write!(buf, ") -> &")?;
                    t.fmt_pretty(buf)?;
                }
            },
            t => {
                write!(buf, ") -> ")?;
                t.fmt_pretty(buf)?;
            }
        }
        if self.suppress_throws() {
            Ok(())
        } else {
            buf.kill_newline();
            write!(buf, " throws ")?;
            self.throws.fmt_pretty(buf)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::expr::parser::parse_fn_type;
    use poolshark::local::LPooled;

    /// IDE display path: parse a polymorphic sig (`val push_front: …`),
    /// alias same-named TVars together (what the module loader does
    /// for sig binds), then run the same pretty pipeline the LSP
    /// hover uses. The user-written name `'a` must survive — the
    /// folding pass used to call `replace_tvars`, which renamed
    /// unrelated TVars to anonymous `'_<id>` placeholders, hiding the
    /// relationship between the two `'a` occurrences.
    #[test]
    fn polymorphic_sig_preserves_tvar_names() {
        let ft = parse_fn_type("fn(a: Array<'a>, @args: 'a) -> Array<'a>").unwrap();
        ft.alias_tvars(&mut LPooled::take());
        let folded = ft.replace_auto_constrained();
        let resolved = folded.resolve_tvars();
        let s = format!("{}", crate::typ::Type::Fn(triomphe::Arc::new(resolved)));
        assert!(
            s.contains("'a"),
            "named tvar 'a should survive pretty-printing, got: {s}"
        );
        assert!(
            !s.contains("'_"),
            "auto tvars should not leak into pretty output, got: {s}"
        );
    }

    /// Function types with no explicit `throws` clause carry an
    /// auto-allocated unbound TVar in their throws slot — that's how
    /// the typechecker leaves room for call-site inference. The
    /// printer must suppress it; otherwise hover on something like
    /// `array::len` reads `fn(a: Array<'a>) -> i64 throws '_42`,
    /// implying it might raise.
    #[test]
    fn unbound_auto_throws_is_hidden() {
        let ft = parse_fn_type("fn(a: Array<'a>) -> i64").unwrap();
        ft.alias_tvars(&mut LPooled::take());
        let folded = ft.replace_auto_constrained();
        let resolved = folded.resolve_tvars();
        let s = format!("{}", crate::typ::Type::Fn(triomphe::Arc::new(resolved)));
        assert!(!s.contains("throws"), "unbound auto throws should not appear, got: {s}");
    }

    /// An *explicit* `throws T` written by the user must always be
    /// shown, even when `T` happens to be `Bottom` or an auto TVar.
    /// The `explicit_throws` flag tracks user intent; only the
    /// implicit-Bottom / implicit-auto cases get suppressed.
    #[test]
    fn explicit_throws_always_shown() {
        let ft = parse_fn_type("fn(x: 'a) -> 'a throws `Boom").unwrap();
        ft.alias_tvars(&mut LPooled::take());
        let s = format!("{}", crate::typ::Type::Fn(triomphe::Arc::new(ft)));
        assert!(s.contains("throws"), "explicit throws should be shown, got: {s}");
        assert!(s.contains("`Boom"), "throws variant should be printed, got: {s}");
    }
}
