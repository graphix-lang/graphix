use crate::{
    LambdaId,
    env::Env,
    expr::{
        ModPath,
        print::{PrettyBuf, PrettyDisplay},
    },
    image,
    typ::{
        Mutability, TVar, Type,
        contains::{ContainsFlags, ContainsHist},
        key_text,
        matches::MatchHist,
        tvar::{Fresh, Level},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, encode_varint};
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

    #[doc(hidden)]
    pub fn own(&self) -> Option<LambdaId> {
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
    /// order (the constraint types live in the cells). They decide which
    /// cells a call freshens or holds rigid, and which conjuncts
    /// `constraint_view`, and so Eq and Ord, see; the image keys them.
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
        let Self { args, vargs, rtype, throws, explicit_throws, quantifiers, lambda_ids } =
            self;
        encode_varint(quantifiers.len() as u64, out);
        for q in quantifiers.iter() {
            key_text(q, out);
        }
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
    /// Cell constraints are not visited; [`Self::for_each_part`] visits
    /// the signature cells' conjuncts too, and which of the two a walk
    /// takes decides whether it sees quantifier bounds.
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
    #[doc(hidden)]
    pub fn for_each_type(&self, f: &mut impl FnMut(&Type)) {
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
    #[doc(hidden)]
    pub fn for_each_part(&self, f: &mut impl FnMut(&Type, bool)) {
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

    /// [`Type::unbind_vacuous_tvars`] over the signature.
    pub fn unbind_vacuous_tvars(&self) {
        self.for_each_type(&mut |t| t.unbind_vacuous_tvars())
    }

    pub fn reset_tvars(&self) -> Self {
        self.reset_tvars_int(&mut LPooled::take(), Fresh::Copy)
    }

    /// [`Type::instantiate_with`] over the signature.
    #[doc(hidden)]
    pub fn instantiate_with(
        &self,
        known: &mut AHashMap<usize, TVar>,
        open: &IntSet<LambdaId>,
    ) -> Self {
        let mut fresh = self
            .cow_walk(|t| t.instantiate_int(known, open))
            .unwrap_or_else(|| self.clone());
        fresh.lambda_ids = self.lambda_ids.instantiate();
        fresh
    }

    /// One cell-identity freshening map across the whole signature
    /// (see [`Type::reset_tvars_int`]). Always a fresh signature: an
    /// instantiation's `lambda_ids` is its own.
    pub(super) fn reset_tvars_int(
        &self,
        known: &mut AHashMap<usize, TVar>,
        how: Fresh<'_>,
    ) -> Self {
        let mut fresh = self
            .cow_walk(|t| t.reset_tvars_int(known, how))
            .unwrap_or_else(|| self.clone());
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

    /// A reference's copy of the scheme (`Fresh::Scheme`) under the open
    /// gates `open`.
    #[doc(hidden)]
    pub fn scheme(&self, open: &IntSet<LambdaId>) -> Self {
        self.reset_tvars_int(&mut LPooled::take(), Fresh::Scheme(open))
    }

    /// A quantifier of this signature no call has picked: the type is a
    /// scheme, not the type of a call.
    pub fn has_open_quantifier(&self) -> bool {
        if self.quantifiers.is_empty() {
            return false;
        }
        let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        self.collect_tvars(&mut named);
        named
            .iter()
            .any(|(name, tv)| self.quantifiers.contains(name) && tv.open_cell().is_some())
    }

    /// Map each of the signature's own open quantifiers to a fresh cell
    /// in `known`, its conjuncts copied through `walk`: every call picks
    /// them anew, whatever gate owns them.
    fn fresh_quantifiers(
        &self,
        known: &mut AHashMap<usize, TVar>,
        walk: impl Fn(&Type, &mut AHashMap<usize, TVar>) -> Type,
    ) {
        if self.quantifiers.is_empty() {
            return;
        }
        let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        self.collect_tvars(&mut named);
        named.retain(|name, tv| {
            let open = self.quantifiers.contains(name).then(|| tv.open_cell()).flatten();
            open.map(|o| *tv = o).is_some()
        });
        for tv in named.values() {
            known
                .entry(tv.cell_addr())
                .or_insert_with(|| TVar::empty_named(tv.name.clone()));
        }
        for tv in named.values() {
            let f = known[&tv.cell_addr()].clone();
            for c in tv.cell_constraints() {
                f.add_cell_constraint(walk(&c, known));
            }
        }
    }

    /// Map each open quantifier of a function type the signature holds
    /// (a rank-2 formal's `fn<'b: C>`) that a call copies to a fresh
    /// generic cell, its conjuncts copied through `walk`: the formal's
    /// callers pick it, never the site that passes the argument.
    fn generic_inner_quantifiers(
        &self,
        known: &mut AHashMap<usize, TVar>,
        open: &IntSet<LambdaId>,
        walk: impl Fn(&Type, &mut AHashMap<usize, TVar>) -> Type,
    ) {
        let mut inner: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.inner_quantifiers(&mut inner);
        inner.retain(|addr, tv| !known.contains_key(addr) && tv.level().copied_by(open));
        for (addr, tv) in inner.iter() {
            known.insert(*addr, TVar::empty_generic(tv.name.clone()));
        }
        for (addr, tv) in inner.iter() {
            let f = known[addr].clone();
            for c in tv.cell_constraints() {
                f.add_cell_constraint(walk(&c, known));
            }
        }
    }

    /// The open quantifiers of every function type the signature holds
    /// (a rank-2 formal's `fn<'b: C>`), by cell.
    pub(super) fn inner_quantifiers(&self, out: &mut AHashMap<usize, TVar>) {
        fn go(t: &Type, out: &mut AHashMap<usize, TVar>, seen: &mut AHashSet<usize>) {
            crate::stack::ensure_sufficient(|| match t {
                Type::TVar(tv) => {
                    if seen.insert(tv.cell_addr())
                        && let Some(b) = tv.binding()
                    {
                        go(&b, out, seen)
                    }
                }
                Type::Fn(ft) => {
                    if !ft.quantifiers.is_empty() {
                        let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
                        ft.collect_tvars(&mut named);
                        for (name, tv) in named.drain() {
                            if ft.quantifiers.contains(&name)
                                && let Some(open) = tv.open_cell()
                            {
                                out.insert(open.cell_addr(), open);
                            }
                        }
                    }
                    ft.for_each_part(&mut |t, _| go(t, out, seen))
                }
                t => t.for_each_child(&mut |c| go(c, out, seen)),
            })
        }
        let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
        self.for_each_part(&mut |t, _| go(t, out, &mut seen));
    }

    /// A call's copy of a signature whose cells it shares (a parameter
    /// called in its definition's body): only the quantifiers are fresh.
    #[doc(hidden)]
    pub fn shared_call(&self) -> Self {
        let mut known: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.fresh_quantifiers(&mut known, |c, known| c.swap_cells(known));
        if known.is_empty() {
            return self.clone();
        }
        self.cow_walk(|t| Some(t.swap_cells(&known))).unwrap_or_else(|| self.clone())
    }

    /// A call's copy of the signature under the open gates `open`
    /// (`Fresh::Instantiate`), its quantifiers always fresh, each fresh
    /// cell the copy holds more than once frozen, so it keeps its cell
    /// when it unifies with an unfrozen one.
    pub fn instantiate(&self, open: &IntSet<LambdaId>) -> Self {
        let mut known: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        let how = Fresh::Instantiate(open);
        let walk = |c: &Type, known: &mut AHashMap<usize, TVar>| {
            c.reset_tvars_int(known, how).unwrap_or_else(|| c.clone())
        };
        self.fresh_quantifiers(&mut known, walk);
        self.generic_inner_quantifiers(&mut known, open, walk);
        let fresh = self.reset_tvars_int(&mut known, how);
        let copies: LPooled<AHashSet<usize>> =
            known.values().map(|tv| tv.cell_addr()).collect();
        let mut occurrences: LPooled<Vec<TVar>> = LPooled::take();
        fresh.for_each_part(&mut |t, _| t.tvar_occurrences(&mut occurrences));
        let mut count: LPooled<AHashMap<usize, usize>> = LPooled::take();
        for tv in occurrences.iter() {
            *count.entry(tv.cell_addr()).or_default() += 1;
        }
        for tv in occurrences.iter() {
            let addr = tv.cell_addr();
            if count[&addr] > 1 && copies.contains(&addr) {
                tv.freeze()
            }
        }
        fresh
    }

    /// Mark generic every cell the signature reaches at `depth` or
    /// deeper: the definition at `depth` closed over them. A cell a
    /// binding lowered above it is its environment's.
    #[doc(hidden)]
    pub fn generalize(&self, depth: u32) {
        let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.reached_cells(&mut cells);
        for tv in cells.values() {
            if tv.level().depth() >= depth {
                tv.generalize()
            }
        }
    }

    /// Claim every cell the signature reaches for `level`
    /// ([`TVar::claim`]).
    #[doc(hidden)]
    pub fn claim(&self, level: Level) {
        let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.reached_cells(&mut cells);
        for tv in cells.values() {
            tv.claim(level)
        }
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
        // CR claude for claude: [bug] An open quantifier of `self` (`fn<'b: Number>`)
        // binds here like any cell. Only `quantified_formal` (node/callsite.rs:244)
        // holds it rigid, and only for a formal whose whole type is the quantified
        // function. Every other position accepts a monomorphic function, yet each call
        // through the value picks `'b` afresh. That covers an annotation through a
        // typedef (`let h: F = g`, `Array<F>`: the check runs on a fresh expansion, so
        // `'b := i64` is lost) and a struct field, tuple element, union member or array
        // element of a formal (a call copies `'b` generic). `--check` passes, `h(1.5)`
        // runs `g = |x: i64| ..` on 1.5, and the JIT panics at fusion/kernel.rs:243 (a
        // runtime I64 in a compiled F64 slot); the inline `let h: fn<'b: Number>(x: 'b)
        // -> 'b = g` instead binds `'b` to i64 for good, so the same type means two
        // things depending on how it is written. probe:
        // design/review-2026-10-05/repro/t-fntyp-03.gx (t-fntyp-03)
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

    /// Every declared bound holds for its bound variable. An open one
    /// stays open: it stands for one type the bound admits, and its cell
    /// enforces the bound at every binding.
    fn bounds_hold(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
    ) -> Result<bool> {
        for (tv, tc) in self.constraint_view().iter() {
            if tv.is_bound()
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
        // labeled arguments pair by label, whatever order each side writes
        let (pairs, aligned) = self.align(impl_fn);
        if !aligned {
            bail!("the signature's and the implementation's labeled arguments differ")
        }
        for (i, (sig_arg, impl_arg)) in pairs.into_iter().enumerate() {
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
        // CR claude for claude: [bug] impl_tvs holds only the tvars written at the top of
        // the implementation's type: sig_tvars collects by name and never enters a
        // binding. An unannotated parameter's cell is bound (`x: '_1 := Array<'_3>`)
        // and tvar_map keys '_3, so neither loop below sees the Number + Singleton
        // conjuncts on '_3. `val f: fn(x: Array<'a>) -> Array<'a>` over `|x| { let y =
        // x[0]$; [y + y] }` passes --check, and then the build refuses `f(["a", "b"])`
        // at elaboration; a dynamic module loads it and adds the strings at run time.
        // The correct `fn<'a: Number + Singleton>(..)` is refused with "missing
        // constraint 'a: Number in implementation". Both loops need the cells the walk
        // mapped, reached through bindings and keyed by cell (as settle::position_cells
        // collects them). probe: design/review-2026-10-05/repro/t-fntyp-05.gx
        // (t-fntyp-05)
        let impl_tvs = sorted_tvars(impl_fn.sig_tvars().drain());
        let probe = BitFlags::empty();
        for (sig_tv, sig_tc) in self.constraint_view().iter() {
            let mut found = false;
            // the impl variable the signature's was matched with, else the
            // same-named one: either form of a bound (`'c: C`, `c: C`)
            // names its variable its own way
            let sig_addr = sig_tv.cell_addr();
            let matched = impl_tvs
                .iter()
                .find(|(_, tv)| {
                    matches!(tvar_map.get(&tv.cell_addr()), Some(Type::TVar(s)) if s.cell_addr() == sig_addr)
                })
                .or_else(|| {
                    impl_tvs
                        .binary_search_by(|(n, _)| n.cmp(&sig_tv.name))
                        .ok()
                        .map(|i| &impl_tvs[i])
                });
            if let Some((_, impl_tv)) = matched {
                for c in impl_tv.cell_constraints().iter() {
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
        let mut param_bounds = LPooled::take();
        self.for_each_part(&mut |t, _| param_bounds_of(env, t, &mut param_bounds));
        // A call through the signature is checked by the signature alone:
        // every conjunct of every impl cell must admit the signature's
        // concrete choice, or follow from the conjuncts of the signature
        // variable it stands for.
        for (_, tv) in impl_tvs.iter() {
            match tvar_map.get(&tv.cell_addr()) {
                None => (),
                Some(Type::TVar(sig_tv)) => {
                    let mut sig_tcs = sig_tv.cell_constraints();
                    if let Some(bounds) = param_bounds.get(&sig_tv.cell_addr()) {
                        sig_tcs.extend(bounds.iter().cloned());
                    }
                    for impl_tc in tv.cell_constraints() {
                        let mut implied = false;
                        for s in sig_tcs.iter() {
                            if s == &impl_tc
                                || impl_tc.contains_with_flags(probe, env, s)?
                            {
                                implied = true;
                                break;
                            }
                        }
                        if !implied {
                            bail!(
                                "the implementation requires {sig_tv}: {impl_tc}, which \
                                 the signature does not declare"
                            )
                        }
                    }
                }
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
        let mut copies: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.cow_walk(|t| t.scope_refs_int(scope, &mut copies))
            .unwrap_or_else(|| self.clone())
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
pub(crate) struct Ret<'a> {
    pub(crate) open: &'static str,
    pub(crate) typ: &'a dyn PrettyDisplay,
    pub(crate) close: &'static str,
}

impl<'a> Ret<'a> {
    pub(crate) fn of(t: &'a Type) -> Self {
        let (open, typ, close): (_, &dyn PrettyDisplay, _) = match t {
            Type::Fn(ft) => ("(", &**ft, ")"),
            Type::ByRef(m, t) => match (&**t, m) {
                (Type::Fn(ft), Mutability::Shared) => ("&(", &**ft, ")"),
                (Type::Fn(ft), Mutability::Mut) => ("&mut (", &**ft, ")"),
                (t, m) => (m.prefix(), t, ""),
            },
            t => ("", t, ""),
        };
        Ret { open, typ, close }
    }

    /// Laid out over lines, a newline after.
    pub(crate) fn fmt_pretty(&self, buf: &mut PrettyBuf) -> fmt::Result {
        write!(buf, "{}", self.open)?;
        self.typ.fmt_pretty(buf)?;
        if !self.close.is_empty() {
            buf.kill_newline();
            writeln!(buf, "{}", self.close)?;
        }
        Ok(())
    }
}

impl fmt::Display for Ret<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}", self.open, self.typ, self.close)
    }
}

/// Bounds as written: a variable's adjacent bounds joined with ` + `.
pub(crate) fn write_bounds(
    f: &mut impl fmt::Write,
    bounds: &[(TVar, Type)],
) -> fmt::Result {
    for (i, (tv, t)) in bounds.iter().enumerate() {
        match i.checked_sub(1).map(|j| &bounds[j].0) {
            Some(prev) if prev.name == tv.name => write!(f, " + {t}")?,
            Some(_) => write!(f, ", '{}: {t}", tv.name)?,
            None => write!(f, "'{}: {t}", tv.name)?,
        }
    }
    Ok(())
}

impl fmt::Display for FnType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let constraints = printed_quantifiers(self);
        if constraints.len() == 0 {
            write!(f, "fn(")?;
        } else {
            write!(f, "fn<")?;
            write_bounds(f, &constraints)?;
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
        write!(f, ") -> {}", Ret::of(&self.rtype))?;
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
        write!(buf, ") -> ")?;
        Ret::of(&self.rtype).fmt_pretty(buf)?;
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

impl FnType {
    // The constraints wire slot is a derived view of the cells: decode
    // re-seeds its entries onto the cells (`add_cell_constraint` dedups).
    // CR claude for claude: [dead] The constraints slot carries nothing either decoder
    // needs. Under an image, each cell is a shared object whose definition already
    // holds its conjuncts (image/mod.rs cell_encode). Under the syntax codec, each TVar
    // occurrence writes its cell's conjuncts inline, and the slot's own TVars decode to
    // fresh cells nothing references, so shape_decode's add_cell_constraint only
    // touches orphans. The slot costs a cell_constraint_pairs walk (normalize, sort and
    // dedup over every reachable cell) in shape_encode, a second one in shape_len under
    // the syntax codec, and on an image decode a re-add of each conjunct to a cell that
    // already holds it. Drop the slot and cell_constraint_pairs (bump the image and AST
    // pack formats), and the sentence in design/tvar_constraints.md that calls it the
    // Pack wire slot. (t-fntyp-11)
    fn shape_len(&self) -> usize {
        // The full cell pairs, not the declared-quantifier view: anonymous
        // cells carry inference facts that must cross the wire.
        let constraints = self.cell_constraint_pairs();
        self.args.encoded_len()
            + self.vargs.encoded_len()
            + self.rtype.encoded_len()
            + <Vec<(TVar, Type)> as Pack>::encoded_len(&constraints)
            + self.throws.encoded_len()
            + self.explicit_throws.encoded_len()
            + self.quantifiers.encoded_len()
    }

    fn shape_encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        self.args.encode(buf)?;
        self.vargs.encode(buf)?;
        self.rtype.encode(buf)?;
        let constraints = self.cell_constraint_pairs();
        <Vec<(TVar, Type)> as Pack>::encode(&constraints, buf)?;
        self.throws.encode(buf)?;
        self.explicit_throws.encode(buf)?;
        self.quantifiers.encode(buf)
    }

    fn shape_decode(
        buf: &mut impl Buf,
        own: Option<LambdaId>,
    ) -> Result<Self, PackError> {
        let args = <Arc<[FnArgType]> as Pack>::decode(buf)?;
        let vargs = <Option<Type> as Pack>::decode(buf)?;
        let rtype = <Type as Pack>::decode(buf)?;
        let constraints = <Vec<(TVar, Type)> as Pack>::decode(buf)?;
        let throws = <Type as Pack>::decode(buf)?;
        let explicit_throws = <bool as Pack>::decode(buf)?;
        let quantifiers = <Arc<[ArcStr]> as Pack>::decode(buf)?;
        for (tv, tc) in constraints {
            tv.add_cell_constraint(tc);
        }
        // Provenance only; excluded from FnType identity.
        let lambda_ids = LambdaIds::default();
        if let Some(id) = own {
            lambda_ids.set_id(id);
        }
        Ok(FnType {
            args,
            vargs,
            rtype,
            throws,
            explicit_throws,
            quantifiers,
            lambda_ids,
        })
    }
}

/// Under an image session a function type is an object carrying its
/// own lambda id, written once and referenced afterwards (a binding's
/// type and its definition share one).
impl Pack for FnType {
    fn encoded_len(&self) -> usize {
        if image::is_encoding() { image::fntype_len(self) } else { self.shape_len() }
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        if image::is_encoding() {
            image::fntype_encode(self, buf, |buf| {
                self.lambda_ids.own().encode(buf)?;
                self.shape_encode(buf)
            })
        } else {
            self.shape_encode(buf)
        }
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if image::is_decoding() {
            image::object_decode(
                buf,
                |buf| {
                    let own = <Option<LambdaId> as Pack>::decode(buf)?;
                    Self::shape_decode(buf, own)
                },
                |b| Self::decode(b),
            )
        } else {
            Self::shape_decode(buf, None)
        }
    }
}

/// The bounds the type references in `t` give the open variables passed
/// as their parameters, by cell: a call checks them where it expands the
/// reference, as it checks a variable's own conjuncts.
fn param_bounds_of(env: &Env, t: &Type, out: &mut IntMap<usize, SmallVec<[Type; 1]>>) {
    crate::stack::ensure_sufficient(|| match t {
        Type::Ref(tr) => {
            if let Some(resolved) = tr.resolve_in(env)
                && let Some(known) = resolved.bindings(&tr.params)
            {
                for ((_, bound), arg) in resolved.params().iter().zip(tr.params.iter()) {
                    if let (Some(bound), Type::TVar(tv)) = (bound, arg)
                        && !tv.is_bound()
                    {
                        out.entry(tv.cell_addr())
                            .or_default()
                            .push(bound.replace_tvars(&known))
                    }
                }
            }
            for p in tr.params.iter() {
                param_bounds_of(env, p, out)
            }
        }
        Type::Fn(ft) => ft.for_each_part(&mut |t, _| param_bounds_of(env, t, out)),
        t => t.for_each_child(&mut |c| param_bounds_of(env, c, out)),
    })
}
