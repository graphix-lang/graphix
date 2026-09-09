use super::AndAc;
use crate::{
    LambdaId,
    env::Env,
    expr::{
        ModPath,
        print::{PrettyBuf, PrettyDisplay},
    },
    typ::{RefHist, TVar, Type, contains::ContainsFlags},
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

/// Positional or labeled function argument. A positional name is
/// documentation only (not part of type identity); a label is the
/// call-site key, and `has_default` is part of the type's shape.
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

#[derive(Debug, Clone)]
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

    /// The node of a fresh instantiation: carries this node's `own` id
    /// and a one-way snapshot of its links, so what later unifies with
    /// the instance lands on the copy and never reaches the def's node.
    pub(crate) fn instantiate(&self) -> LambdaIds {
        let inner = self.0.read();
        Self(SArc::new(RwLock::new(LambdaIdsInner {
            own: inner.own,
            links: inner.links.clone(),
        })))
    }

    /// Every live linked id; dead links are pruned as the walk meets
    /// them. Locks one node at a time. O(live linked nodes).
    pub fn ids(&self) -> LPooled<IntSet<LambdaId>> {
        let mut visited: LPooled<IntSet<usize>> = LPooled::take();
        let mut ids: LPooled<IntSet<LambdaId>> = LPooled::take();
        let mut work: LPooled<Vec<LambdaIds>> = LPooled::take();
        visited.insert(SArc::as_ptr(&self.0) as usize);
        work.push(self.clone());
        while let Some(node) = work.pop() {
            let mut inner = node.0.write();
            if let Some(id) = inner.own {
                ids.insert(id);
            }
            inner.links.retain(|link| match link.upgrade() {
                Some(next) => {
                    if visited.insert(link.addr()) {
                        work.push(next);
                    }
                    true
                }
                None => false,
            });
        }
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

/// A function signature. Constraints live only in the tvar cells: a
/// quantifier like `fn<'a: Number>` seeds `'a`'s cell conjunction, and
/// [`FnType::constraint_view`] derives the listing from reachable cells.
#[derive(Debug, Clone)]
pub struct FnType {
    pub args: Arc<[FnArgType]>,
    pub vargs: Option<Type>,
    pub rtype: Type,
    pub throws: Type,
    pub explicit_throws: bool,
    /// The quantifier names the `fn<...>` header declared, in source
    /// order. Syntax only (the constraint types live in the cells), but
    /// the declaration site is what stops a self-referential constraint
    /// from regressing. Excluded from Eq/Ord/Hash.
    pub quantifiers: Arc<[ArcStr]>,
    /// Every LambdaId this type might represent.
    pub lambda_ids: LambdaIds,
}

impl FnType {
    /// The tvar cells reachable from args / vargs / rtype / throws,
    /// by name.
    pub(crate) fn sig_tvars(&self) -> LPooled<AHashMap<ArcStr, TVar>> {
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

    /// Name-sorted `(tvar, constraint)` pairs for every declared
    /// quantifier whose cell carries exactly one conjunct.
    /// Multi-conjunct cells are unlisted.
    pub fn constraint_view(&self) -> LPooled<Vec<(TVar, Type)>> {
        // Only declared names produce pairs: an inner fn mentioning a
        // quantifier has an empty `quantifiers`, which terminates the
        // regress on self-referential constraints.
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
                    // Normalized so equality and display agree on the
                    // canonical form.
                    view.push((tv.clone(), tc.normalize()));
                }
            }
            view.sort_by(|(a, _), (b, _)| a.name.cmp(&b.name));
            view
        })();
        Self::walking(|w| w.remove(&key));
        r
    }

    /// Every reachable single-conjunct cell as (tvar, conjunct) pairs,
    /// declared or not (an inferred impl's constraints sit on auto
    /// `'_N` cells).
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
        // Like PartialEq: lambda_ids is provenance and explicit_throws
        // does not survive a print round-trip when throws is Bottom.
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
    /// `None` when already normal.
    pub(super) fn normalize_int(
        &self,
        cx: &mut super::normalize::NormCx,
    ) -> Option<Self> {
        self.cow_walk(|t| t.normalize_int(cx))
    }

    /// See [`Type::resolve_tvars`].
    pub fn resolve_tvars(&self) -> Self {
        self.resolve_tvars_seen_int(&mut super::normalize::ResolveTvarsCx::take())
            .unwrap_or_else(|| self.clone())
    }

    /// `None` when no TVar is beneath.
    pub(super) fn resolve_tvars_seen_int(
        &self,
        cx: &mut super::normalize::ResolveTvarsCx,
    ) -> Option<Self> {
        self.cow_walk(|t| t.resolve_tvars_seen_int(cx))
    }

    /// Rewrite every type position through `f` (`None` means
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

    /// Read-only walk over args, vargs, rtype, throws in that order.
    /// Cell constraints are not visited; see
    /// [`Self::for_each_sig_constraint`].
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

    /// Visit each signature cell's constraint conjuncts. Guarded per
    /// `FnType` address: a conjunct Fn can reach back here.
    pub(crate) fn for_each_sig_constraint(&self, f: &mut impl FnMut(&Type)) {
        let key = self as *const Self as usize;
        if Self::walking(|w| w.insert(key)) {
            let mut tvs: LPooled<Vec<(ArcStr, TVar)>> =
                self.sig_tvars().drain().collect();
            tvs.sort_by(|a, b| {
                a.0.cmp(&b.0).then_with(|| a.1.read().id.cmp(&b.1.read().id))
            });
            for (_, tv) in tvs.drain(..) {
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

    /// [`Type::unbind_open_tvars`] over the signature.
    pub fn unbind_open_tvars(&self) {
        self.for_each_type(&mut |t| t.unbind_open_tvars())
    }

    /// Record the def gate's inferred facts as cell constraints. With
    /// `closed_only` (a nested gate) only bindings with no open
    /// interior are recorded: a partial one snapshots mid-solve state
    /// the enclosing gate may still revise.
    pub fn constrain_known(&self, closed_only: bool) {
        let mut known = LPooled::take();
        self.collect_tvars(&mut known);
        // Recording order lands in constraint lists and diagnostics,
        // so it must be deterministic.
        let mut known: LPooled<Vec<(ArcStr, TVar)>> = known.drain().collect();
        known.sort_by(|a, b| {
            a.0.cmp(&b.0).then_with(|| a.1.read().id.cmp(&b.1.read().id))
        });
        for (_, tv) in known.drain(..) {
            // Cloned out through the alias chain: add_cell_constraint
            // write-locks the same cell.
            let bound = Type::TVar(tv.clone()).deref_cloned();
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
                // Private cells, open leaves left open: a partial fact
                // must not be closed to `Array<Any>`.
                let t = t.reset_tvars();
                let tc = t.normalize();
                if crate::dbgenv::graphix_dbg_bind() {
                    eprintln!(
                        "CONSTRAIN-KNOWN '{}({:x}) += {tc:?}",
                        tv.name,
                        tv.cell_addr()
                    );
                }
                tv.add_cell_constraint(tc);
            }
        }
    }

    pub fn reset_tvars(&self) -> Self {
        self.reset_tvars_int(&mut LPooled::take())
    }

    /// One cell-identity freshening map across the whole signature
    /// (see [`Type::reset_tvars_int`]). Always a fresh signature: an
    /// instantiation's `lambda_ids` is its own.
    pub(super) fn reset_tvars_int(&self, known: &mut AHashMap<usize, TVar>) -> Self {
        let mut fresh =
            self.cow_walk(|t| t.reset_tvars_int(known)).unwrap_or_else(|| self.clone());
        fresh.lambda_ids = self.lambda_ids.instantiate();
        fresh
    }

    pub fn replace_tvars(&self, known: &AHashMap<ArcStr, Type>) -> Self {
        self.replace_tvars_int(known, &mut LPooled::take())
            .unwrap_or_else(|| self.clone())
    }

    /// `None` when no TVar is beneath.
    pub(super) fn replace_tvars_int(
        &self,
        known: &AHashMap<ArcStr, Type>,
        renamed: &mut AHashMap<ArcStr, TVar>,
    ) -> Option<Self> {
        self.cow_walk(|t| t.replace_tvars_int(known, renamed))
    }

    /// Replace auto type variables (`'_23`) that carry one constraint
    /// with that constraint, for display. Call before
    /// `Type::resolve_tvars`, which discards the cells.
    pub fn replace_auto_constrained(&self) -> Self {
        let mut known: LPooled<AHashMap<ArcStr, Type>> = LPooled::take();
        let Self { args, vargs, rtype, throws, explicit_throws, quantifiers, lambda_ids } =
            self;
        // Read the cells directly: auto names are never declared, so
        // `constraint_view` cannot see them.
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

    // The three walks below visit the cell constraints between rtype
    // and throws; for `alias_tvars` the first-seen occurrence of a
    // name becomes the surviving cell, so the order is observable.

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
        // Over the canonical form: a tvar in a portion that normalizes
        // away carries no constraint force, and identity must not
        // depend on the stored form.
        self.for_each_sig_constraint(&mut |tc| tc.normalize().collect_tvars(known));
        throws.collect_tvars(known);
    }

    /// Index of the first positional parameter (`args.len()` when all
    /// are labeled); `args[..first_positional()]` is the labeled prefix.
    pub fn first_positional(&self) -> usize {
        self.args.iter().position(|a| a.is_positional()).unwrap_or(self.args.len())
    }

    /// Whether a value of type `t` could match a pattern typed `self`:
    /// same arity and labels, every component could match; nothing is
    /// unified.
    pub(super) fn could_match_int(
        &self,
        env: &Env,
        hist: &mut RefHist<AHashMap<(Option<usize>, Option<usize>), bool>>,
        t: &Self,
    ) -> Result<bool> {
        let sul = self.first_positional();
        let tul = t.first_positional();
        for a in &self.args[..sul] {
            if let FnArgKind::Labeled { name: l, .. } = &a.kind {
                match t.args.iter().find(|b| b.label() == Some(l)) {
                    None => return Ok(false),
                    Some(o) => {
                        if !a.typ.could_match_int(env, hist, &o.typ)? {
                            return Ok(false);
                        }
                    }
                }
            }
        }
        for a in &t.args[..tul] {
            if let FnArgKind::Labeled { name: l, has_default } = &a.kind
                && !*has_default
                && !self.args.iter().any(|b| b.label() == Some(l))
            {
                return Ok(false);
            }
        }
        if self.args.len() - sul != t.args.len() - tul {
            return Ok(false);
        }
        for (a, b) in self.args[sul..].iter().zip(t.args[tul..].iter()) {
            if !a.typ.could_match_int(env, hist, &b.typ)? {
                return Ok(false);
            }
        }
        match (&self.vargs, &t.vargs) {
            (None, None) => (),
            (Some(a), Some(b)) => {
                if !a.could_match_int(env, hist, b)? {
                    return Ok(false);
                }
            }
            _ => return Ok(false),
        }
        Ok(self.rtype.could_match_int(env, hist, &t.rtype)?
            && self.throws.could_match_int(env, hist, &t.throws)?)
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
        let sul = self.first_positional();
        let tul = t.first_positional();
        for a in &self.args[..sul] {
            if let FnArgKind::Labeled { name: l, .. } = &a.kind {
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
        for a in &t.args[..tul] {
            if let FnArgKind::Labeled { name: l, has_default } = &a.kind {
                if !*has_default && !self.args.iter().any(|a| a.label() == Some(l)) {
                    return Ok(false);
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

    /// The parameter positions of [`Self::contains`] alone: pushes a
    /// declared signature's parameter types into an argument function
    /// before its body is typechecked. Return and throws are what the
    /// body determines and are left open.
    pub fn pre_unify_params(&self, env: &Env, t: &Self) -> Result<()> {
        let flags = ContainsFlags::AliasTVars | ContainsFlags::InitTVars;
        let mut hist = RefHist::new(LPooled::take());
        let sul = self.first_positional();
        let tul = t.first_positional();
        for a in &self.args[..sul] {
            if let FnArgKind::Labeled { name: l, .. } = &a.kind
                && let Some(o) = t.args.iter().find(|a| a.label() == Some(l))
            {
                o.typ.contains_int(flags, env, &mut hist, &a.typ)?;
            }
        }
        for (t, s) in t.args[tul..].iter().zip(self.args[sul..].iter()) {
            t.typ.contains_int(flags, env, &mut hist, &s.typ)?;
        }
        Ok(())
    }

    /// [`Self::contains`] without labeled argument subtyping.
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

    pub fn sig_matches(&self, env: &Env, impl_fn: &Self) -> Result<()> {
        self.sig_matches_int(
            env,
            impl_fn,
            &mut LPooled::take(),
            &mut RefHist::new(LPooled::take()),
        )
    }

    pub(super) fn sig_matches_int(
        &self,
        env: &Env,
        impl_fn: &Self,
        tvar_map: &mut IntMap<usize, Type>,
        hist: &mut RefHist<AHashSet<(Option<usize>, Option<usize>)>>,
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
                .sig_matches_int(env, &impl_arg.typ, tvar_map, hist)
                .with_context(|| format!("in argument {i}"))?;
        }
        match (sig_vargs, impl_vargs) {
            (None, None) => (),
            (Some(sig_va), Some(impl_va)) => {
                sig_va
                    .sig_matches_int(env, impl_va, tvar_map, hist)
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
            .sig_matches_int(env, impl_rtype, tvar_map, hist)
            .context("in return type")?;
        sig_throws
            .sig_matches_int(env, impl_throws, tvar_map, hist)
            .context("in throws clause")?;
        // Every declared bound must be among the impl cell's whole
        // conjunction, compared by meaning (refs are scoped
        // independently on each side).
        let sig_cons = self.constraint_view();
        let impl_tvs = impl_fn.sig_tvars();
        for (sig_tv, sig_tc) in sig_cons.iter() {
            let mut found = false;
            if let Some(tv) = impl_tvs.get(&sig_tv.name) {
                for c in tv.cell_constraints().iter() {
                    if c == sig_tc
                        || (c.contains_with_flags(BitFlags::empty(), env, sig_tc)?
                            && sig_tc.contains_with_flags(BitFlags::empty(), env, c)?)
                    {
                        found = true;
                        break;
                    }
                }
            }
            if !found {
                bail!("missing constraint {sig_tv}: {sig_tc} in implementation")
            }
        }
        // Every conjunct of every impl cell must admit the
        // signature's concrete choice.
        let mut impl_tvs: LPooled<Vec<(ArcStr, TVar)>> =
            impl_fn.sig_tvars().drain().collect();
        impl_tvs.sort_by(|a, b| {
            a.0.cmp(&b.0).then_with(|| a.1.read().id.cmp(&b.1.read().id))
        });
        for (_, tv) in impl_tvs.drain(..) {
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
    /// Suppress the `throws` clause when printing: only for the
    /// implicit no-throw shapes (inferred `Bottom`, a TVar bound to
    /// `Bottom`, an unbound auto cell), never for an explicit clause.
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

/// Is this positional parameter a trait method's receiver (`self`
/// typed by the `self` variable)? It prints bare.
fn is_self_param(a: &FnArgType) -> bool {
    matches!(&a.kind, FnArgKind::Positional { name: Some(n) } if &**n == "self")
        && matches!(&a.typ, Type::TVar(tv) if &*tv.name == "self")
}

/// The quantifiers a signature prints: the declared ones, minus the
/// receiver `self` (implied by its trait) and the compiler-minted
/// `#arg` quantifiers of traits written in argument position (those
/// print as the trait at the argument).
fn printed_quantifiers(ft: &FnType) -> LPooled<Vec<(TVar, Type)>> {
    let mut v = ft.constraint_view();
    v.retain(|(tv, _)| &*tv.name != "self" && !tv.name.starts_with('#'));
    v
}

impl fmt::Display for FnType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let constraints = printed_quantifiers(self);
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
            if is_self_param(a) {
                write!(f, "self")?;
            } else {
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
            }
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
        let constraints = printed_quantifiers(self);
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
                if is_self_param(a) {
                    writeln!(buf, "self")?;
                } else {
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
                }
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

    /// The pretty pipeline keeps the user-written `'a` on both
    /// occurrences of a polymorphic sig.
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

    /// An implicit throws slot (an unbound auto TVar) is not printed.
    #[test]
    fn unbound_auto_throws_is_hidden() {
        let ft = parse_fn_type("fn(a: Array<'a>) -> i64").unwrap();
        ft.alias_tvars(&mut LPooled::take());
        let folded = ft.replace_auto_constrained();
        let resolved = folded.resolve_tvars();
        let s = format!("{}", crate::typ::Type::Fn(triomphe::Arc::new(resolved)));
        assert!(!s.contains("throws"), "unbound auto throws should not appear, got: {s}");
    }

    /// An explicit `throws T` is always printed, even for `Bottom` or
    /// an auto TVar.
    #[test]
    fn explicit_throws_always_shown() {
        let ft = parse_fn_type("fn(x: 'a) -> 'a throws `Boom").unwrap();
        ft.alias_tvars(&mut LPooled::take());
        let s = format!("{}", crate::typ::Type::Fn(triomphe::Arc::new(ft)));
        assert!(s.contains("throws"), "explicit throws should be shown, got: {s}");
        assert!(s.contains("`Boom"), "throws variant should be printed, got: {s}");
    }
}
