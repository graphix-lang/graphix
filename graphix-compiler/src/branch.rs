//! What a branch of a cycle's update pass sees of the shared state, and
//! how a forked branch's view merges back (`design/parallel_eval.md` §4).
//!
//! A forked branch reads its parent's state frozen and writes deltas
//! and logs of its own; its parent links are raw pointers, valid because
//! a parent is suspended in the join until both of its branches return.

use crate::{
    BindId, CompileCtx, CustomBuiltinType, Event, ExecCtx, Rt, TagValue, UserEvent,
    cost::{CycleSite, ForkSite, Meter, Plan, ticks},
    expr::ExprId,
    node::place::Path,
};
use futures::channel::mpsc;
use graphix_types::stack::{Control, ParMode};
use netidx_value::Value;
use nohash::{IntMap, IntSet};
use poolshark::{global::GPooled, local::LPooled};
use rayon::iter::{
    IndexedParallelIterator, IntoParallelRefMutIterator, ParallelIterator,
};
use smallvec::SmallVec;
use std::{
    future::Future,
    ops::{Deref, DerefMut},
    pin::Pin,
    sync::LazyLock,
    time::Duration,
};

type BoxFuture<T> = Pin<Box<dyn Future<Output = T> + Send + 'static>>;

/// The value of a store entry; `None` for a bottom or no entry.
pub(crate) fn stored_value(e: Option<&(TagValue, u64)>) -> Option<Value> {
    e.and_then(
        |(tv, _)| if tv.tag().is_bottom() { None } else { Some(tv.value_cloned()) },
    )
}

/// A runtime call a forked branch made, replayed into its parent at the
/// merge in the order the branch made it.
enum RtOp {
    RefVar(BindId, ExprId),
    UnrefVar(BindId, ExprId),
    SetVar(BindId, Value),
    PatchVar(BindId, Path, Value),
    NotifySet(BindId),
    SetTimer(BindId, Duration),
    Spawn(BoxFuture<(BindId, Box<dyn CustomBuiltinType>)>),
    SpawnVar(BoxFuture<(BindId, Value)>),
    Watch(mpsc::Receiver<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>),
    WatchVar(mpsc::Receiver<GPooled<Vec<(BindId, Value)>>>),
}

/// A forked branch's runtime: deltas over its parent's store and
/// reference paths (`None` = removed), and the log of everything else.
pub struct ForkRt<R: Rt> {
    parent: *const RtView<'static, R>,
    cycle: u64,
    /// Under `GRAPHIX_PAR_AUDIT`, every variable this branch read.
    reads: Option<parking_lot::Mutex<IntSet<BindId>>>,
    store: IntMap<BindId, Option<(TagValue, u64)>>,
    ref_paths: IntMap<BindId, Option<(BindId, Path)>>,
    log: Vec<RtOp>,
}

// SAFETY: `parent` is only read, and only while the parent is suspended
// in the join that forked this branch. Its children read a `ForkRt`
// through `&` (the store and reference-path deltas, the cycle); the log
// is only reached through `&mut`.
unsafe impl<R: Rt + Sync> Send for ForkRt<R> {}
unsafe impl<R: Rt + Sync> Sync for ForkRt<R> {}

impl<R: Rt> ForkRt<R> {
    /// Under `GRAPHIX_PAR_AUDIT`, note that this branch read `id`.
    pub(crate) fn note_read(&self, id: BindId) {
        if let Some(reads) = &self.reads {
            reads.lock().insert(id);
        }
    }

    /// Queue a write of `v` to `id` before everything this branch did.
    pub(crate) fn queue_first(&mut self, id: BindId, v: Value) {
        self.log.insert(0, RtOp::SetVar(id, v));
    }

    /// A branch forked from `parent`, which must not be used until the
    /// branch has merged back.
    pub(crate) fn new(parent: &RtView<'_, R>) -> Self {
        Self {
            parent: parent as *const RtView<'_, R> as *const RtView<'static, R>,
            cycle: parent.cycle(),
            reads: crate::dbgenv::graphix_par_audit()
                .then(|| parking_lot::Mutex::new(IntSet::default())),
            store: IntMap::default(),
            ref_paths: IntMap::default(),
            log: Vec::new(),
        }
    }

    fn parent(&self) -> &RtView<'_, R> {
        // SAFETY: see the type.
        unsafe { &*(self.parent as *const RtView<'_, R>) }
    }
}

/// The runtime as a branch sees it: the runtime itself at the root, a
/// [`ForkRt`] in a forked branch. Its methods are the [`Rt`] calls a
/// node makes.
pub enum RtView<'a, R: Rt> {
    Root(&'a mut R),
    Fork(&'a mut ForkRt<R>),
}

macro_rules! logged {
    ($self:ident, $call:ident($($arg:expr),*), $op:expr) => {
        match $self {
            RtView::Root(r) => r.$call($($arg),*),
            RtView::Fork(f) => f.log.push($op),
        }
    };
}

impl<'a, R: Rt> RtView<'a, R> {
    /// The same view, borrowed for a shorter time.
    pub fn reborrow(&mut self) -> RtView<'_, R> {
        match self {
            Self::Root(r) => RtView::Root(r),
            Self::Fork(f) => RtView::Fork(f),
        }
    }

    #[inline]
    pub fn ref_var(&mut self, id: BindId, ref_by: ExprId) {
        logged!(self, ref_var(id, ref_by), RtOp::RefVar(id, ref_by))
    }

    pub fn unref_var(&mut self, id: BindId, ref_by: ExprId) {
        logged!(self, unref_var(id, ref_by), RtOp::UnrefVar(id, ref_by))
    }

    #[inline]
    pub fn set_var(&mut self, id: BindId, value: Value) {
        logged!(self, set_var(id, value), RtOp::SetVar(id, value))
    }

    pub fn patch_var(&mut self, id: BindId, path: Path, value: Value) {
        logged!(self, patch_var(id, path, value), RtOp::PatchVar(id, path, value))
    }

    #[inline]
    pub fn notify_set(&mut self, id: BindId) {
        logged!(self, notify_set(id), RtOp::NotifySet(id))
    }

    pub fn set_timer(&mut self, id: BindId, timeout: Duration) {
        logged!(self, set_timer(id, timeout), RtOp::SetTimer(id, timeout))
    }

    pub fn spawn<F>(&mut self, f: F)
    where
        F: Future<Output = (BindId, Box<dyn CustomBuiltinType>)> + Send + 'static,
    {
        logged!(self, spawn(f), RtOp::Spawn(Box::pin(f)))
    }

    pub fn spawn_var<F>(&mut self, f: F)
    where
        F: Future<Output = (BindId, Value)> + Send + 'static,
    {
        logged!(self, spawn_var(f), RtOp::SpawnVar(Box::pin(f)))
    }

    pub fn watch(
        &mut self,
        s: mpsc::Receiver<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
    ) {
        logged!(self, watch(s), RtOp::Watch(s))
    }

    pub fn watch_var(&mut self, s: mpsc::Receiver<GPooled<Vec<(BindId, Value)>>>) {
        logged!(self, watch_var(s), RtOp::WatchVar(s))
    }

    #[inline]
    pub fn cycle(&self) -> u64 {
        match self {
            Self::Root(r) => r.cycle(),
            Self::Fork(f) => f.cycle,
        }
    }

    /// The (production, cycle stamp) of `id`'s last delivery; see
    /// [`Rt::store_get`].
    #[inline]
    pub fn store_get(&self, id: &BindId) -> Option<&(TagValue, u64)> {
        let mut view: &RtView<'_, R> = self;
        loop {
            match view {
                RtView::Root(r) => return r.store_get(id),
                RtView::Fork(f) => match f.store.get(id) {
                    Some(e) => return e.as_ref(),
                    None => view = f.parent(),
                },
            }
        }
    }

    /// See [`Rt::store_value`].
    #[inline]
    pub fn store_value(&self, id: &BindId) -> Option<Value> {
        stored_value(self.store_get(id))
    }

    #[inline]
    pub fn store_insert(&mut self, id: BindId, tv: TagValue) {
        match self {
            Self::Root(r) => r.store_insert(id, tv),
            Self::Fork(f) => {
                f.store.insert(id, Some((tv, f.cycle)));
            }
        }
    }

    pub fn store_insert_standing(&mut self, id: BindId, tv: TagValue) {
        match self {
            Self::Root(r) => r.store_insert_standing(id, tv),
            Self::Fork(f) => {
                f.store.insert(id, Some((tv, f.cycle.wrapping_sub(1))));
            }
        }
    }

    #[inline]
    pub fn store_remove(&mut self, id: &BindId) {
        match self {
            Self::Root(r) => r.store_remove(id),
            Self::Fork(f) => {
                f.store.insert(*id, None);
            }
        }
    }

    pub fn ref_path(&self, cell: &BindId) -> Option<&(BindId, Path)> {
        let mut view: &RtView<'_, R> = self;
        loop {
            match view {
                RtView::Root(r) => return r.ref_path(cell),
                RtView::Fork(f) => match f.ref_paths.get(cell) {
                    Some(e) => return e.as_ref(),
                    None => view = f.parent(),
                },
            }
        }
    }

    pub fn set_ref_path(&mut self, cell: BindId, root: BindId, path: Path) {
        match self {
            Self::Root(r) => r.set_ref_path(cell, root, path),
            Self::Fork(f) => {
                f.ref_paths.insert(cell, Some((root, path)));
            }
        }
    }

    pub fn clear_ref_path(&mut self, cell: &BindId) {
        match self {
            Self::Root(r) => r.clear_ref_path(cell),
            Self::Fork(f) => {
                f.ref_paths.insert(*cell, None);
            }
        }
    }

    /// Apply what the forked branch `child` did, after everything this
    /// view did before the fork and before anything it does after.
    pub(crate) fn merge(&mut self, child: ForkRt<R>) {
        let ForkRt { parent: _, cycle: _, reads, store, ref_paths, log } = child;
        match self {
            Self::Fork(f) => {
                if let (Some(mine), Some(theirs)) = (&f.reads, reads) {
                    mine.lock().extend(theirs.into_inner());
                }
                f.store.extend(store);
                f.ref_paths.extend(ref_paths);
                f.log.extend(log);
            }
            Self::Root(r) => {
                let cycle = r.cycle();
                for (id, e) in store {
                    match e {
                        None => r.store_remove(&id),
                        Some((tv, stamp)) if stamp == cycle => r.store_insert(id, tv),
                        Some((tv, _)) => r.store_insert_standing(id, tv),
                    }
                }
                for (cell, e) in ref_paths {
                    match e {
                        None => r.clear_ref_path(&cell),
                        Some((root, path)) => r.set_ref_path(cell, root, path),
                    }
                }
                for op in log {
                    match op {
                        RtOp::RefVar(id, by) => r.ref_var(id, by),
                        RtOp::UnrefVar(id, by) => r.unref_var(id, by),
                        RtOp::SetVar(id, v) => r.set_var(id, v),
                        RtOp::PatchVar(id, p, v) => r.patch_var(id, p, v),
                        RtOp::NotifySet(id) => r.notify_set(id),
                        RtOp::SetTimer(id, d) => r.set_timer(id, d),
                        RtOp::Spawn(f) => r.spawn(f),
                        RtOp::SpawnVar(f) => r.spawn_var(f),
                        RtOp::Watch(s) => r.watch(s),
                        RtOp::WatchVar(s) => r.watch_var(s),
                    }
                }
            }
        }
    }
}

/// A map a branch writes over its parent's: the parent's entries show
/// through unless the branch wrote them or removed them (`removed`). A
/// root map has no parent and no removals.
pub struct Layered<V> {
    map: IntMap<BindId, V>,
    removed: IntSet<BindId>,
    parent: *const Layered<V>,
}

// SAFETY: `parent` is only read, and only while the parent is suspended
// in the join that forked this branch.
unsafe impl<V: Send + Sync> Send for Layered<V> {}
unsafe impl<V: Send + Sync> Sync for Layered<V> {}

impl<V> Default for Layered<V> {
    fn default() -> Self {
        Self {
            map: IntMap::default(),
            removed: IntSet::default(),
            parent: std::ptr::null(),
        }
    }
}

impl<V: std::fmt::Debug> std::fmt::Debug for Layered<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.map.iter()).finish()
    }
}

impl<V: Clone> Layered<V> {
    #[inline]
    fn parent(&self) -> Option<&Layered<V>> {
        // SAFETY: see the type.
        unsafe { self.parent.as_ref() }
    }

    #[inline]
    pub fn get(&self, id: &BindId) -> Option<&V> {
        let mut layer = self;
        loop {
            if let Some(v) = layer.map.get(id) {
                return Some(v);
            }
            let parent = layer.parent()?;
            if layer.removed.contains(id) {
                return None;
            }
            layer = parent;
        }
    }

    #[inline]
    pub fn contains_key(&self, id: &BindId) -> bool {
        self.get(id).is_some()
    }

    /// Set `id`, returning what it held.
    #[inline]
    pub fn insert(&mut self, id: BindId, v: V) -> Option<V> {
        match self.map.insert(id, v) {
            Some(prev) => Some(prev),
            None if self.parent.is_null() || self.removed.remove(&id) => None,
            None => self.parent().and_then(|p| p.get(&id).cloned()),
        }
    }

    /// Set `id` if it holds nothing, else hand `v` back.
    #[inline]
    pub fn try_insert(&mut self, id: BindId, v: V) -> Result<(), V> {
        if self.contains_key(&id) {
            return Err(v);
        }
        self.insert(id, v);
        Ok(())
    }

    /// Remove `id`, returning what it held.
    #[inline]
    pub fn remove(&mut self, id: &BindId) -> Option<V> {
        let own = self.map.remove(id);
        if self.parent.is_null() || self.removed.contains(id) {
            return own;
        }
        let from_parent = self.parent().and_then(|p| p.get(id).cloned());
        if from_parent.is_some() {
            self.removed.insert(*id);
        }
        own.or(from_parent)
    }

    /// The ids this layer delivered itself.
    pub(crate) fn own_ids(&self) -> impl Iterator<Item = BindId> + '_ {
        self.map.keys().copied()
    }

    /// The ids this layer and `other` both delivered themselves.
    pub(crate) fn delivered_in_both(&self, other: &Self) -> LPooled<Vec<BindId>> {
        self.map.keys().filter(|id| other.map.contains_key(*id)).copied().collect()
    }

    /// Take this layer's own entry for `id`, leaving what its parent
    /// holds visible.
    pub(crate) fn take_own(&mut self, id: &BindId) -> Option<V> {
        self.map.remove(id)
    }

    /// The entries this layer holds.
    pub fn len(&self) -> usize {
        self.map.len()
    }

    /// Whether this layer holds nothing.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty() && self.removed.is_empty()
    }

    pub fn clear(&mut self) {
        self.map.clear();
        self.removed.clear();
    }

    /// A layer over this one, which must not be used until the layer has
    /// merged back.
    pub(crate) fn fork(&self) -> Self {
        Self { map: IntMap::default(), removed: IntSet::default(), parent: self }
    }

    /// Apply the forked layer `child` over this one.
    pub(crate) fn merge(&mut self, child: Self) {
        for id in child.removed {
            self.remove(&id);
        }
        for (id, v) in child.map {
            self.insert(id, v);
        }
    }
}

/// A forked branch's compile state: its parent's, read through, until
/// the branch first writes; then a compile fork of its own
/// ([`CompileCtx::fork`]) that joins its parent's at the merge.
pub struct ForkCx<R: Rt, E: UserEvent> {
    parent: *const CxView<'static, R, E>,
    own: Option<Box<CompileCtx<R, E>>>,
}

// SAFETY: `parent` is only read, and only while the parent is suspended
// in the join that forked this branch.
unsafe impl<R: Rt + Sync, E: UserEvent + Sync> Send for ForkCx<R, E> {}

impl<R: Rt, E: UserEvent> ForkCx<R, E> {
    /// A branch forked from `parent`, which must not be used until the
    /// branch has merged back.
    pub(crate) fn new(parent: &CxView<'_, R, E>) -> Self {
        Self {
            parent: parent as *const CxView<'_, R, E> as *const CxView<'static, R, E>,
            own: None,
        }
    }

    fn parent(&self) -> &CxView<'_, R, E> {
        // SAFETY: see the type.
        unsafe { &*(self.parent as *const CxView<'_, R, E>) }
    }
}

/// The compile state as a branch sees it.
pub enum CxView<'a, R: Rt, E: UserEvent> {
    Root(&'a mut CompileCtx<R, E>),
    Fork(&'a mut ForkCx<R, E>),
}

impl<'a, R: Rt, E: UserEvent> CxView<'a, R, E> {
    /// The same view, borrowed for a shorter time.
    pub fn reborrow(&mut self) -> CxView<'_, R, E> {
        match self {
            Self::Root(c) => CxView::Root(c),
            Self::Fork(f) => CxView::Fork(f),
        }
    }

    /// Take back what the forked branch `child` compiled.
    pub(crate) fn merge(&mut self, child: ForkCx<R, E>) {
        if let Some(cx) = child.own {
            self.join(*cx)
        }
    }
}

impl<'a, R: Rt, E: UserEvent> Deref for CxView<'a, R, E> {
    type Target = CompileCtx<R, E>;

    #[inline]
    fn deref(&self) -> &CompileCtx<R, E> {
        let mut view: &CxView<'_, R, E> = self;
        loop {
            match view {
                CxView::Root(c) => return c,
                CxView::Fork(f) => match &f.own {
                    Some(c) => return c,
                    None => view = f.parent(),
                },
            }
        }
    }
}

impl<'a, R: Rt, E: UserEvent> DerefMut for CxView<'a, R, E> {
    #[inline]
    fn deref_mut(&mut self) -> &mut CompileCtx<R, E> {
        match self {
            Self::Root(c) => c,
            Self::Fork(f) => {
                if f.own.is_none() {
                    let parent: &CompileCtx<R, E> = f.parent();
                    let mut own = Box::new(parent.fork());
                    own.def_assertions = parent.def_assertions.clone();
                    f.own = Some(own);
                }
                f.own.as_mut().unwrap()
            }
        }
    }
}

/// `xs` cut at `ranges`, which cover it in order.
pub(crate) fn cut<'a, T>(
    mut xs: &'a mut [T],
    ranges: &[(usize, usize)],
) -> SmallVec<[&'a mut [T]; 16]> {
    ranges
        .iter()
        .map(|&(lo, hi)| {
            let (part, rest) = std::mem::take(&mut xs).split_at_mut(hi - lo);
            xs = rest;
            part
        })
        .collect()
}

/// One part of a [`fork_each`]: its branch's views and its result.
struct Part<R: Rt, E: UserEvent, P, T> {
    cx: ForkCx<R, E>,
    rt: ForkRt<R>,
    event: Event<E>,
    part: Option<P>,
    out: Option<T>,
}

/// Run `f` over each of `parts` on a branch of its own, every branch
/// forked from `ctx` (siblings, one level down), and merge them back in
/// order: no part sees what another did, and `ctx` ends as the serial
/// evaluation of the parts in order would leave it. The results, in
/// order.
pub fn fork_each<R, E, P, T, F>(
    ctx: &mut ExecCtx<'_, R, E>,
    parts: impl IntoIterator<Item = P>,
    f: F,
) -> Vec<T>
where
    R: Rt,
    E: UserEvent,
    P: Send,
    T: Send,
    F: Fn(&mut ExecCtx<'_, R, E>, P) -> T + Sync,
{
    debug_assert!(!ctx.deferred_pending(), "a fork over unapplied compile work");
    let mut branches: Vec<Part<R, E, P, T>> = parts
        .into_iter()
        .map(|p| Part {
            cx: ForkCx::new(&ctx.cx),
            rt: ForkRt::new(&ctx.rt),
            event: ctx.event.fork(),
            part: Some(p),
            out: None,
        })
        .collect();
    let (libstate, hooks, control, decoder) =
        (ctx.libstate, ctx.core_hook_sites, ctx.control, ctx.image_decoder);
    let (fork_depth, par) = (ctx.fork_depth + 1, ctx.par);
    control.forked();
    let tokio = tokio::runtime::Handle::try_current().ok();
    branches.par_iter_mut().with_max_len(1).for_each(|b| {
        // a part may run on any thread of the pool
        let _interrupt = graphix_types::stack::InterruptScope::new(&**control);
        let _tokio = tokio.as_ref().map(|h| h.enter());
        let mut c = ExecCtx {
            cx: CxView::Fork(&mut b.cx),
            image_decoder: decoder,
            libstate,
            rt: RtView::Fork(&mut b.rt),
            core_hook_sites: hooks,
            control,
            event: &mut b.event,
            fork_depth,
            par,
            serial: false,
        };
        b.out = Some(f(&mut c, b.part.take().expect("a part")));
    });
    if branches.first().is_some_and(|b| b.rt.reads.is_some()) {
        for (i, b) in branches.iter().enumerate() {
            let reads = b.rt.reads.as_ref().expect("audited").lock();
            for a in &branches[..i] {
                audit(&reads, &a.event, &a.rt, &b.event, &b.rt);
            }
        }
    }
    // what an earlier part delivered first: a later part's delivery of
    // it waits a cycle, queued where serial evaluation would queue it
    let mut delivered: LPooled<IntSet<BindId>> = LPooled::take();
    let mut out = Vec::with_capacity(branches.len());
    for Part { cx, mut rt, mut event, part: _, out: o } in branches.drain(..) {
        let again: LPooled<Vec<BindId>> =
            event.variables.own_ids().filter(|id| delivered.contains(id)).collect();
        for id in again.iter() {
            let tv = event.variables.take_own(id).expect("delivered");
            rt.queue_first(*id, tv.value());
        }
        delivered.extend(event.variables.own_ids());
        ctx.cx.merge(cx);
        ctx.rt.merge(rt);
        ctx.event.merge(event);
        out.push(o.expect("every part ran"));
    }
    out
}

/// Run `a` and `b` as two branches forked from `ctx` and merge them back,
/// `a`'s first: neither sees what the other did, and `ctx` ends as the
/// serial evaluation of `a` then `b` would leave it.
pub fn fork_join<R, E, A, B, RA, RB>(ctx: &mut ExecCtx<'_, R, E>, a: A, b: B) -> (RA, RB)
where
    R: Rt,
    E: UserEvent,
    A: FnOnce(&mut ExecCtx<'_, R, E>) -> RA + Send,
    B: FnOnce(&mut ExecCtx<'_, R, E>) -> RB + Send,
    RA: Send,
    RB: Send,
{
    debug_assert!(!ctx.deferred_pending(), "a fork over unapplied compile work");
    let (mut rt_a, mut rt_b) = (ForkRt::new(&ctx.rt), ForkRt::new(&ctx.rt));
    let (mut cx_a, mut cx_b) = (ForkCx::new(&ctx.cx), ForkCx::new(&ctx.cx));
    let (mut ev_a, mut ev_b) = (ctx.event.fork(), ctx.event.fork());
    let (libstate, hooks, control, decoder) =
        (ctx.libstate, ctx.core_hook_sites, ctx.control, ctx.image_decoder);
    let (fork_depth, par) = (ctx.fork_depth + 1, ctx.par);
    control.forked();
    let branch = |cx, rt, event| ExecCtx {
        cx: CxView::Fork(cx),
        image_decoder: decoder,
        libstate,
        rt: RtView::Fork(rt),
        core_hook_sites: hooks,
        control,
        event,
        fork_depth,
        par,
        serial: false,
    };
    let tokio = tokio::runtime::Handle::try_current().ok();
    let (ra, rb) = rayon::join(
        || a(&mut branch(&mut cx_a, &mut rt_a, &mut ev_a)),
        || {
            // the stolen side runs on a thread of its own
            let _interrupt = graphix_types::stack::InterruptScope::new(&**control);
            let _tokio = tokio.as_ref().map(|h| h.enter());
            b(&mut branch(&mut cx_b, &mut rt_b, &mut ev_b))
        },
    );
    if let Some(reads) = &rt_b.reads {
        audit(&reads.lock(), &ev_a, &rt_a, &ev_b, &rt_b);
    }
    // what both delivered, the left delivered first: the right's waits a
    // cycle, queued where serial evaluation would have queued it
    for id in ev_b.variables.delivered_in_both(&ev_a.variables).drain(..) {
        let tv = ev_b.variables.take_own(&id).expect("delivered");
        rt_b.queue_first(id, tv.value());
    }
    ctx.cx.merge(cx_a);
    ctx.rt.merge(rt_a);
    ctx.event.merge(ev_a);
    ctx.cx.merge(cx_b);
    ctx.rt.merge(rt_b);
    ctx.event.merge(ev_b);
    (ra, rb)
}

/// `GRAPHIX_PAR_AUDIT`: the right branch read nothing the left one
/// published this cycle, which serial evaluation would have shown it.
fn audit<R: Rt, E: UserEvent>(
    right_reads: &IntSet<BindId>,
    left: &Event<E>,
    left_rt: &ForkRt<R>,
    right: &Event<E>,
    right_rt: &ForkRt<R>,
) {
    let own = |ev: &Event<E>, rt: &ForkRt<R>, id: &BindId| {
        ev.variables.map.contains_key(id) || rt.store.contains_key(id)
    };
    for id in right_reads {
        if own(left, left_rt, id) && !own(right, right_rt, id) {
            panic!(
                "GRAPHIX_PAR_AUDIT: a forked branch read {id:?}, which its \
                 left sibling published in the same cycle"
            )
        }
    }
}

/// The process's evaluation pool: `GRAPHIX_EVAL_THREADS` threads (default
/// one per core), shared by every runtime in the process.
pub fn eval_pool() -> &'static rayon::ThreadPool {
    static POOL: LazyLock<rayon::ThreadPool> = LazyLock::new(|| {
        let threads = std::env::var("GRAPHIX_EVAL_THREADS")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(0);
        rayon::ThreadPoolBuilder::new()
            .num_threads(threads)
            .stack_size(16 << 20)
            .thread_name(|i| format!("graphix-eval-{i}"))
            .build()
            .expect("the evaluation pool")
    });
    &POOL
}

/// Run `f`, a cycle's updates, on the evaluation pool when the cycle may
/// fork (always when forced, by `site` under `Auto`), so its joins fork
/// onto the pool; inline otherwise.
pub fn run_cycle<T: Send>(
    control: &Control,
    site: &mut CycleSite,
    f: impl FnOnce() -> T + Send,
) -> T {
    let pooled = match control.par_mode() {
        ParMode::Off => false,
        ParMode::Force => true,
        ParMode::Auto => site.enter(),
    };
    let (t0, forks) = (ticks(), control.forks());
    let r = if pooled {
        let tokio = tokio::runtime::Handle::try_current().ok();
        eval_pool().install(|| {
            let _interrupt = graphix_types::stack::InterruptScope::new(control);
            let _tokio = tokio.as_ref().map(|h| h.enter());
            f()
        })
    } else {
        f()
    };
    if control.par_mode() == ParMode::Auto {
        site.record(ticks().wrapping_sub(t0), pooled, control.forks() - forks);
    }
    r
}

/// Whether a view made on this thread may fork: only on the evaluation
/// pool, where [`run_cycle`] runs a cycle that may.
pub(crate) fn view_mode(control: &Control) -> ParMode {
    match control.par_mode() {
        ParMode::Off => ParMode::Off,
        m if eval_pool().current_thread_index().is_some() => m,
        _ => ParMode::Off,
    }
}

/// Forks nest at most this deep: a branch this deep runs its fork
/// points serially. Every lookup a branch makes walks at most this many
/// layers, and binary splits this deep already outnumber any machine's
/// cores many times over.
pub const MAX_FORK_DEPTH: u8 = 16;

/// Run `a` then `b`, the two children of `site`, or fork them where the
/// site's plan says.
#[inline]
pub fn join2<R, E, A, B, RA, RB>(
    site: &mut ForkSite,
    ctx: &mut ExecCtx<'_, R, E>,
    a: A,
    b: B,
) -> (RA, RB)
where
    R: Rt,
    E: UserEvent,
    A: FnOnce(&mut ExecCtx<'_, R, E>) -> RA + Send,
    B: FnOnce(&mut ExecCtx<'_, R, E>) -> RB + Send,
    RA: Send,
    RB: Send,
{
    match site.plan(ctx, 2) {
        Plan::Serial => (a(ctx), b(ctx)),
        Plan::Measure(mut m) => {
            let ra = m.time(0, || a(ctx));
            let rb = m.time(1, || b(ctx));
            m.done(2);
            (ra, rb)
        }
        Plan::Fork(s) => match s.fork(ctx, 0, 2) {
            Some(_) => fork_join(ctx, a, b),
            None => (a(ctx), b(ctx)),
        },
    }
}

/// Run `f` timed as child `i` when measuring.
#[inline]
pub fn timed<T>(
    meter: &mut Option<&mut Meter<'_>>,
    i: usize,
    f: impl FnOnce() -> T,
) -> T {
    match meter {
        Some(m) => m.time(i, f),
        None => f(),
    }
}

/// What forked branches share through their parent links is safe to read
/// from several threads at once: the raw links bypass the compiler's own
/// check, so it is made here.
#[allow(dead_code)]
fn shared_across_branches<R: Rt, E: UserEvent>() {
    fn sync<T: Sync>() {}
    sync::<CompileCtx<R, E>>();
    sync::<Layered<TagValue>>();
    sync::<RtView<'static, R>>();
    sync::<Event<E>>();
}
