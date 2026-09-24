use crate::{
    LambdaId,
    dbgenv::graphix_dbg_bind,
    env::Env,
    expr::{
        ModPath,
        print::{PrettyBuf, PrettyDisplay},
    },
    typ::{
        TVar, Type,
        contains::{ContainsFlags, ContainsHist},
        key_text,
        matches::MatchHist,
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use bytes::BufMut;
use enumflags2::BitFlags;
use netidx_core::pack::encode_varint;
use netidx_derive::Pack;
use nohash::{IntMap, IntSet};
use parking_lot::RwLock;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    cell::RefCell,
    cmp::Ordering,
    fmt::{self, Debug, Write},
    hash::{Hash, Hasher},
    ops::ControlFlow,
    sync::{Arc as SArc, Weak},
    thread::LocalKey,
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

impl Hash for FnArgKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
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

#[derive(Debug, Clone, Pack, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    pub(crate) fn addr(&self) -> usize {
        SArc::as_ptr(&self.0) as *const () as usize
    }

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

/// Signature vars in their deterministic order, by (name, TVarId):
/// recording and settle order land in constraint lists and diagnostics.
pub(super) fn sorted_tvars(
    tvs: impl IntoIterator<Item = (ArcStr, TVar)>,
) -> LPooled<Vec<(ArcStr, TVar)>> {
    let mut v: LPooled<Vec<(ArcStr, TVar)>> = tvs.into_iter().collect();
    v.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.read().id.cmp(&b.1.read().id)));
    v
}

thread_local! {
    static VIEWING: RefCell<IntSet<usize>> = RefCell::new(IntSet::default());
    static CONSTRAINING: RefCell<IntSet<usize>> = RefCell::new(IntSet::default());
}

/// One walk's reentrancy guard on a signature: a conjunct can reach the
/// signature it constrains. The key is released on drop, unwind too.
struct Walking {
    set: &'static LocalKey<RefCell<IntSet<usize>>>,
    key: usize,
}

impl Walking {
    fn enter(
        set: &'static LocalKey<RefCell<IntSet<usize>>>,
        ft: &FnType,
    ) -> Option<Self> {
        let key = ft as *const FnType as usize;
        set.with_borrow_mut(|s| s.insert(key)).then_some(Walking { set, key })
    }
}

impl Drop for Walking {
    fn drop(&mut self) {
        self.set.with_borrow_mut(|s| s.remove(&self.key));
    }
}

impl FnType {
    /// The tvar cells reachable from args / vargs / rtype / throws,
    /// by name.
    pub(crate) fn sig_tvars(&self) -> LPooled<AHashMap<ArcStr, TVar>> {
        let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        self.for_each_type(&mut |t| t.collect_tvars(&mut known));
        known
    }

    /// `(tvar, conjunct)` for every conjunct of every signature cell
    /// `keep` selects, normalized (so equality and display agree on the
    /// canonical form), sorted by name then conjunct.
    fn constraint_pairs(
        &self,
        keep: impl Fn(&TVar) -> bool,
    ) -> LPooled<Vec<(TVar, Type)>> {
        let mut view: LPooled<Vec<(TVar, Type)>> = LPooled::take();
        let Some(_guard) = Walking::enter(&VIEWING, self) else { return view };
        for (_, tv) in self.sig_tvars().drain() {
            if keep(&tv) {
                for tc in tv.cell_constraints() {
                    view.push((tv.clone(), tc.normalize()));
                }
            }
        }
        view.sort_by(|(a, x), (b, y)| a.name.cmp(&b.name).then_with(|| x.cmp(y)));
        view.dedup_by(|(a, x), (b, y)| a.name == b.name && x == y);
        view
    }

    /// The declared quantifiers' conjuncts, as [`Self::constraint_pairs`]:
    /// the part of the constraints that is the signature's identity.
    pub fn constraint_view(&self) -> LPooled<Vec<(TVar, Type)>> {
        self.constraint_pairs(|tv| self.quantifiers.contains(&tv.name))
    }

    /// Every reachable cell's conjuncts, declared or not (an inferred
    /// impl's constraints sit on auto `'_N` cells).
    pub(crate) fn cell_constraint_pairs(&self) -> LPooled<Vec<(TVar, Type)>> {
        self.constraint_pairs(|_| true)
    }

    /// The canonical bytes the image keys this type by: the shape with
    /// every variable by identity, and the lambda ids cell by identity.
    pub(crate) fn content_key(&self, out: &mut Vec<u8>) {
        let Self {
            args,
            vargs,
            rtype,
            throws,
            explicit_throws,
            quantifiers: _,
            lambda_ids,
        } = self;
        encode_varint(args.len() as u64, out);
        for a in args.iter() {
            match &a.kind {
                FnArgKind::Positional { name } => {
                    out.put_u8(0);
                    match name {
                        None => out.put_u8(0),
                        Some(n) => {
                            out.put_u8(1);
                            key_text(n, out);
                        }
                    }
                }
                FnArgKind::Labeled { name, has_default } => {
                    out.put_u8(1);
                    key_text(name, out);
                    out.put_u8(*has_default as u8);
                }
            }
            a.typ.content_key(out);
        }
        match vargs {
            None => out.put_u8(0),
            Some(t) => {
                out.put_u8(1);
                t.content_key(out);
            }
        }
        rtype.content_key(out);
        throws.content_key(out);
        out.put_u8(*explicit_throws as u8);
        out.put_u64_le(lambda_ids.addr() as u64);
    }
}

/// Structural: `lambda_ids` is provenance, `explicit_throws` does not
/// survive a print round-trip when throws is Bottom, and the declared
/// constraints compare only once the shape is equal.
impl PartialEq for FnType {
    fn eq(&self, other: &Self) -> bool {
        self.args == other.args
            && self.vargs == other.vargs
            && self.rtype == other.rtype
            && self.throws == other.throws
            && *self.constraint_view() == *other.constraint_view()
    }
}

impl Eq for FnType {}

impl PartialOrd for FnType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FnType {
    fn cmp(&self, other: &Self) -> Ordering {
        self.args
            .cmp(&other.args)
            .then_with(|| self.vargs.cmp(&other.vargs))
            .then_with(|| self.rtype.cmp(&other.rtype))
            .then_with(|| (*self.constraint_view()).cmp(&*other.constraint_view()))
            .then_with(|| self.throws.cmp(&other.throws))
    }
}

/// The shape only: equal signatures have equal shapes.
impl Hash for FnType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.args.hash(state);
        self.vargs.hash(state);
        self.rtype.hash(state);
        self.throws.hash(state);
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

    /// Every type position of the signature in its one order: args,
    /// vargs, rtype, each signature cell's conjuncts (flagged `true`),
    /// throws. [`Self::alias_tvars`] makes the first-seen occurrence of a
    /// name the surviving cell, so the order is observable. The
    /// conjuncts are guarded per `FnType` address: one can reach back.
    pub(crate) fn for_each_part(&self, f: &mut impl FnMut(&Type, bool)) {
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
            f(&arg.typ, false)
        }
        if let Some(vargs) = vargs {
            f(vargs, false)
        }
        f(rtype, false);
        if let Some(_guard) = Walking::enter(&CONSTRAINING, self) {
            for (_, tv) in sorted_tvars(self.sig_tvars().drain()).drain(..) {
                for tc in tv.cell_constraints() {
                    f(&tc, true);
                }
            }
        }
        f(throws, false);
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
        for (_, tv) in sorted_tvars(known.drain()).drain(..) {
            // Cloned out through the alias chain: add_cell_constraint
            // write-locks the same cell.
            let bound = Type::TVar(tv.clone()).deref_cloned();
            let Some(t) = bound.filter(|t| *t != Type::Bottom && *t != Type::Any) else {
                continue;
            };
            if closed_only && t.has_unbound() {
                continue;
            }
            // Private cells, open leaves left open: a partial fact
            // must not be closed to `Array<Any>`.
            let tc = t.reset_tvars().normalize();
            if graphix_dbg_bind() {
                eprintln!("CONSTRAIN-KNOWN '{}({:x}) += {tc:?}", tv.name, tv.cell_addr());
            }
            tv.add_cell_constraint(tc);
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
        fresh: &mut AHashMap<usize, TVar>,
    ) -> Option<Self> {
        self.cow_walk(|t| t.replace_tvars_int(known, fresh))
    }

    /// Replace auto type variables (`'_23`) that carry one constraint
    /// with that constraint, for display. Call before
    /// `Type::resolve_tvars`, which discards the cells.
    pub fn replace_auto_constrained(&self) -> Self {
        let mut known: LPooled<AHashMap<ArcStr, Type>> = LPooled::take();
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
            known.entry(name).or_insert(Type::TVar(tv));
        }
        self.replace_tvars(&known)
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

    pub fn alias_tvars(&self, known: &mut AHashMap<ArcStr, TVar>) {
        self.for_each_part(&mut |t, _| t.alias_tvars(known))
    }

    pub fn unfreeze_tvars(&self) {
        self.for_each_part(&mut |t, _| t.unfreeze_tvars())
    }

    /// Conjuncts are visited in their canonical form: a tvar in a
    /// portion that normalizes away carries no constraint force, and
    /// identity must not depend on the stored form.
    pub fn collect_tvars(&self, known: &mut AHashMap<ArcStr, TVar>) {
        self.for_each_part(&mut |t, constraint| {
            if constraint {
                t.normalize().collect_tvars(known)
            } else {
                t.collect_tvars(known)
            }
        })
    }

    /// Index of the first positional parameter (`args.len()` when all
    /// are labeled); `args[..first_positional()]` is the labeled prefix.
    pub fn first_positional(&self) -> usize {
        self.args.iter().position(|a| a.is_positional()).unwrap_or(self.args.len())
    }

    /// `self`'s arguments paired with `t`'s: labeled by name, positional
    /// by index. The pairs cover what aligns; the flag says whether the
    /// shapes align in full: no label of `self` absent from `t`, no
    /// required label of `t` absent from `self`, as many positionals.
    fn align<'a>(
        &'a self,
        t: &'a Self,
    ) -> (SmallVec<[(&'a FnArgType, &'a FnArgType); 8]>, bool) {
        let (sul, tul) = (self.first_positional(), t.first_positional());
        let mut pairs: SmallVec<[(&FnArgType, &FnArgType); 8]> = SmallVec::new();
        let mut ok = self.args.len() - sul == t.args.len() - tul;
        for a in &self.args[..sul] {
            match t.args[..tul].iter().find(|b| b.label() == a.label()) {
                Some(b) => pairs.push((a, b)),
                None => ok = false,
            }
        }
        ok &= t.args[..tul].iter().all(|b| {
            b.has_default() || self.args[..sul].iter().any(|a| a.label() == b.label())
        });
        pairs.extend(self.args[sul..].iter().zip(t.args[tul..].iter()));
        (pairs, ok)
    }

    /// Whether a value of type `t` could match a pattern typed `self`:
    /// same arity and labels, every component could match; nothing is
    /// unified.
    pub(super) fn could_match_int(
        &self,
        env: &Env,
        hist: &mut MatchHist,
        t: &Self,
    ) -> Result<bool> {
        let (pairs, ok) = self.align(t);
        if !ok {
            return Ok(false);
        }
        for (a, b) in pairs {
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
        self.contains_int(ContainsFlags::Commit.into(), env, &mut ContainsHist::new(), t)
    }

    /// Arguments are contravariant: each of `t`'s parameters must admit
    /// what `self`'s callers pass, and a label `self` lets a caller omit
    /// must be one `t` defaults.
    pub(super) fn contains_int(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Self,
    ) -> Result<bool> {
        let (pairs, ok) = self.align(t);
        if !ok || pairs.iter().any(|(s, t)| s.has_default() && !t.has_default()) {
            return Ok(false);
        }
        for (s, t) in pairs {
            if !t.typ.contains_int(flags, env, hist, &s.typ)? {
                return Ok(false);
            }
        }
        Ok(match (&t.vargs, &self.vargs) {
            (Some(tv), Some(sv)) => tv.contains_int(flags, env, hist, sv)?,
            (None, None) => true,
            (_, _) => false,
        } && self.rtype.contains_int(flags, env, hist, &t.rtype)?
            && self.bounds_hold(flags, env, hist)?
            && t.bounds_hold(flags, env, hist)?
            && self.throws.contains_int(flags, env, hist, &t.throws)?)
    }

    /// Every declared bound holds for its variable. A variable with one
    /// bound meets it (an open one binds to it); with several, only a
    /// bound variable is checked: an open conjunction settles by a
    /// witness, and meanwhile the cell enforces it at every binding.
    fn bounds_hold(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
    ) -> Result<bool> {
        let view = self.constraint_view();
        for (tv, tc) in view.iter() {
            let alone = view.iter().filter(|(v, _)| v.name == tv.name).count() == 1;
            if (alone || tv.is_bound())
                && !tc.contains_int(flags, env, hist, &Type::TVar(tv.clone()))?
            {
                return Ok(false);
            }
        }
        Ok(true)
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
        let mut hist = ContainsHist::new();
        for (s, t) in self.align(t).0 {
            t.typ.contains_int(ContainsFlags::Commit.into(), env, &mut hist, &s.typ)?;
        }
        Ok(())
    }

    pub fn sig_matches(&self, env: &Env, impl_fn: &Self) -> Result<()> {
        self.sig_matches_int(
            env,
            impl_fn,
            &mut LPooled::take(),
            &mut super::RefHist::new(),
        )
    }

    pub(super) fn sig_matches_int(
        &self,
        env: &Env,
        impl_fn: &Self,
        tvar_map: &mut IntMap<usize, Type>,
        hist: &mut super::RefHist<AHashSet<super::RefPair>>,
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
        let impl_tvs = sorted_tvars(impl_fn.sig_tvars().drain());
        let probe = BitFlags::empty();
        for (sig_tv, sig_tc) in self.constraint_view().iter() {
            let mut found = false;
            if let Ok(i) = impl_tvs.binary_search_by(|(n, _)| n.cmp(&sig_tv.name)) {
                for c in impl_tvs[i].1.cell_constraints().iter() {
                    if c == sig_tc
                        || (c.contains_with_flags(probe, env, sig_tc)?
                            && sig_tc.contains_with_flags(probe, env, c)?)
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
        for (_, tv) in impl_tvs.iter() {
            match tvar_map.get(&tv.cell_addr()) {
                None | Some(Type::TVar(_)) => (),
                Some(sig_type) => {
                    for impl_tc in tv.cell_constraints() {
                        let ok = impl_tc
                            .contains_with_flags(probe, env, sig_type)
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
                Type::TVar(tv) => match tv.binding() {
                    Some(Type::Bottom) => true,
                    None => tv.name.starts_with('_'),
                    Some(_) => false,
                },
                _ => false,
            }
    }
}

/// Is this positional parameter a trait method's receiver (`self`
/// typed by the `self` variable or its application)? It prints as its
/// type alone.
fn is_self_param(a: &FnArgType) -> bool {
    let is_self = |t: &Type| matches!(t, Type::TVar(tv) if &*tv.name == "self");
    matches!(&a.kind, FnArgKind::Positional { name: Some(n) } if &**n == "self")
        && match &a.typ {
            Type::App(c, _) => is_self(c),
            t => is_self(t),
        }
}

/// The quantifiers a signature prints: the declared ones, minus the
/// receiver `self` (implied by its trait) and the compiler-minted
/// `#arg` quantifiers of traits written in argument position (those
/// print as the trait at the argument). One entry per conjunct, a
/// variable's conjuncts adjacent.
fn printed_quantifiers(ft: &FnType) -> LPooled<Vec<(TVar, Type)>> {
    let mut v = ft.constraint_view();
    v.retain(|(tv, _)| &*tv.name != "self" && !tv.name.starts_with('#'));
    v
}

/// How an argument's name is written before its type.
struct ArgPrefix<'a>(&'a FnArgKind);

impl fmt::Display for ArgPrefix<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            FnArgKind::Labeled { name, has_default: true } => write!(f, "?#{name}: "),
            FnArgKind::Labeled { name, has_default: false } => write!(f, "#{name}: "),
            FnArgKind::Positional { name: Some(n) } => write!(f, "{n}: "),
            FnArgKind::Positional { name: None } => Ok(()),
        }
    }
}

/// A return type as written after `->`: a function type is
/// parenthesized, bare or behind a reference.
enum Ret<'a> {
    Fn(&'a FnType),
    RefFn(&'a FnType),
    Ref(&'a Type),
    Plain(&'a Type),
}

impl<'a> Ret<'a> {
    fn of(t: &'a Type) -> Self {
        match t {
            Type::Fn(ft) => Ret::Fn(ft),
            Type::ByRef(t) => match &**t {
                Type::Fn(ft) => Ret::RefFn(ft),
                t => Ret::Ref(t),
            },
            t => Ret::Plain(t),
        }
    }
}

impl fmt::Display for FnType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let constraints = printed_quantifiers(self);
        if constraints.len() == 0 {
            write!(f, "fn(")?;
        } else {
            write!(f, "fn<")?;
            for (i, (tv, t)) in constraints.iter().enumerate() {
                match i.checked_sub(1).map(|j| &constraints[j].0) {
                    Some(prev) if prev.name == tv.name => write!(f, " + {t}")?,
                    Some(_) => write!(f, ", '{}: {t}", tv.name)?,
                    None => write!(f, "'{}: {t}", tv.name)?,
                }
            }
            write!(f, ">(")?;
        }
        for (i, a) in self.args.iter().enumerate() {
            if is_self_param(a) {
                write!(f, "{}", a.typ)?;
            } else {
                write!(f, "{}{}", ArgPrefix(&a.kind), a.typ)?;
            }
            if i < self.args.len() - 1 || self.vargs.is_some() {
                write!(f, ", ")?;
            }
        }
        if let Some(vargs) = &self.vargs {
            write!(f, "@args: {}", vargs)?;
        }
        match Ret::of(&self.rtype) {
            Ret::Fn(ft) => write!(f, ") -> ({ft})")?,
            Ret::RefFn(ft) => write!(f, ") -> &({ft})")?,
            Ret::Ref(t) => write!(f, ") -> &{t}")?,
            Ret::Plain(t) => write!(f, ") -> {t}")?,
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
            buf.nested(|buf| {
                for (i, (tv, t)) in constraints.iter().enumerate() {
                    match i.checked_sub(1).map(|j| &constraints[j].0) {
                        Some(prev) if prev.name == tv.name => {
                            buf.kill_newline();
                            write!(buf, " + ")?;
                        }
                        Some(_) => {
                            buf.kill_newline();
                            writeln!(buf, ",")?;
                            write!(buf, "'{}: ", tv.name)?;
                        }
                        None => write!(buf, "'{}: ", tv.name)?,
                    }
                    buf.nested(|buf| t.fmt_pretty(buf))?;
                }
                Ok(())
            })?;
            writeln!(buf, ">(")?;
        }
        buf.nested(|buf| {
            for (i, a) in self.args.iter().enumerate() {
                if is_self_param(a) {
                    writeln!(buf, "{}", a.typ)?;
                } else {
                    write!(buf, "{}", ArgPrefix(&a.kind))?;
                    buf.nested(|buf| a.typ.fmt_pretty(buf))?;
                }
                if i < self.args.len() - 1 || self.vargs.is_some() {
                    buf.kill_newline();
                    writeln!(buf, ",")?;
                }
            }
            if let Some(vargs) = &self.vargs {
                write!(buf, "@args: ")?;
                buf.nested(|buf| vargs.fmt_pretty(buf))?;
            }
            Ok(())
        })?;
        match Ret::of(&self.rtype) {
            Ret::Fn(ft) => {
                write!(buf, ") -> (")?;
                ft.fmt_pretty(buf)?;
                buf.kill_newline();
                writeln!(buf, ")")?;
            }
            Ret::RefFn(ft) => {
                write!(buf, ") -> &(")?;
                ft.fmt_pretty(buf)?;
                buf.kill_newline();
                writeln!(buf, ")")?;
            }
            Ret::Ref(t) => {
                write!(buf, ") -> &")?;
                t.fmt_pretty(buf)?;
            }
            Ret::Plain(t) => {
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
