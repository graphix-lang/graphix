//! What a branch of a cycle's update pass sees of the shared state, and
//! how a forked branch's view merges back (`design/parallel_eval.md` §4).
//!
//! A forked branch reads its parent's state frozen and writes deltas
//! and logs of its own; its parent links are raw pointers, valid because
//! a parent is suspended in the join until both of its branches return.

use crate::{
    BindId, CompileCtx, CustomBuiltinType, Event, ExecCtx, Node, Rt, TagValue,
    analysis::Sites,
    cost::{ForkSite, Meter, Plan, Siblings},
    expr::Expr,
    node::{
        callsite::CallNode,
        place::{Path, RefPath},
    },
    prune::{BoundaryRef, Reader},
    typ::tvar::TaskContext,
};
use futures::channel::mpsc;
use graphix_types::stack::{
    Control, ParMode, eval_threads, with_control, with_current_control,
};
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
    sync::atomic::{AtomicUsize, Ordering},
    time::Duration,
};
use triomphe::Arc;

type BoxFuture<T> = Pin<Box<dyn Future<Output = T> + Send + 'static>>;

/// The value of a store entry; `None` for a bottom or no entry.
pub(crate) fn stored_value(e: Option<&(TagValue, u64)>) -> Option<Value> {
    e.and_then(
        |(tv, _)| if tv.tag().is_bottom() { None } else { Some(tv.value_cloned()) },
    )
}

/// The stamp of a standing entry replacing `e` in `cycle`: a standing
/// entry is no delivery, so it keeps the stamp of the last one from an
/// earlier cycle, else an earlier cycle's, which no same-cycle reader
/// sees as delivered.
pub(crate) fn standing_stamp(e: Option<&(TagValue, u64)>, cycle: u64) -> u64 {
    match e {
        Some((_, stamp)) if *stamp < cycle => *stamp,
        _ => cycle.wrapping_sub(1),
    }
}

/// A runtime call a forked branch made, replayed into its parent at the
/// merge in the order the branch made it.
enum RtOp {
    RefVar(BindId, Reader),
    UnrefVar(BindId, Reader),
    SetVar(BindId, Value),
    SetLevel(BindId, Value),
    PatchVar(BindId, Path, Value, Option<Arc<Expr>>),
    PatchLevel(BindId, Path, Value),
    NotifySet(BindId),
    ForgetWrites(BindId),
    SetTimer(BindId, Duration),
    CancelTimer(BindId),
    Spawn(BoxFuture<(BindId, Box<dyn CustomBuiltinType>)>),
    SpawnVar(BoxFuture<(BindId, Value)>),
    Watch(mpsc::Receiver<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>),
    WatchVar(mpsc::Receiver<GPooled<Vec<(BindId, Value)>>>),
}

impl RtOp {
    /// The runtime call this op makes, unless it only registers or
    /// drops a reader.
    fn effect(&self) -> Option<&'static str> {
        Some(match self {
            RtOp::RefVar(..) | RtOp::UnrefVar(..) => return None,
            RtOp::SetVar(..) => "set_var",
            RtOp::SetLevel(..) => "set_level",
            RtOp::PatchVar(..) => "patch_var",
            RtOp::PatchLevel(..) => "patch_level",
            RtOp::NotifySet(..) => "notify_set",
            RtOp::ForgetWrites(..) => "forget_writes",
            RtOp::SetTimer(..) => "set_timer",
            RtOp::CancelTimer(..) => "cancel_timer",
            RtOp::Spawn(..) => "spawn",
            RtOp::SpawnVar(..) => "spawn_var",
            RtOp::Watch(..) => "watch",
            RtOp::WatchVar(..) => "watch_var",
        })
    }
}

/// A forked branch's runtime: deltas over its parent's store and
/// reference paths (`None` = removed), and the log of everything else.
pub struct ForkRt<R: Rt> {
    parent: *const RtView<'static, R>,
    cycle: u64,
    /// Under `GRAPHIX_PAR_AUDIT`, every variable this branch read.
    reads: Option<parking_lot::Mutex<IntSet<BindId>>>,
    store: IntMap<BindId, Option<(TagValue, u64)>>,
    ref_paths: IntMap<BindId, Option<RefPath>>,
    /// The readers this branch registered, which the runtime learns only
    /// at the merge: a delivery this cycle marks them too.
    readers: IntMap<BindId, SmallVec<[Reader; 1]>>,
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

    /// A branch forked from `parent`.
    ///
    /// # Safety
    ///
    /// The parent is read through a pointer whose lifetime is erased: it
    /// must outlive this branch and must not be used, by its owner or
    /// another branch's merge, until this branch has merged back.
    pub(crate) unsafe fn new(parent: &RtView<'_, R>) -> Self {
        Self {
            parent: parent as *const RtView<'_, R> as *const RtView<'static, R>,
            cycle: parent.cycle(),
            reads: crate::dbgenv::graphix_par_audit()
                .then(|| parking_lot::Mutex::new(IntSet::default())),
            store: IntMap::default(),
            ref_paths: IntMap::default(),
            readers: IntMap::default(),
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
    pub fn ref_var(&mut self, id: BindId, by: Reader) {
        if let Self::Fork(f) = self {
            f.readers.entry(id).or_default().push(by.clone());
        }
        logged!(self, ref_var(id, by), RtOp::RefVar(id, by))
    }

    /// Each reader of `id`: the runtime's and those the branches above
    /// registered this cycle.
    fn each_reader(&self, id: &BindId, f: &mut dyn FnMut(&Reader)) {
        let mut view: &RtView<'_, R> = self;
        loop {
            match view {
                RtView::Root(r) => return r.readers(id, f),
                RtView::Fork(fk) => {
                    if let Some(rs) = fk.readers.get(id) {
                        rs.iter().for_each(&mut *f)
                    }
                    view = fk.parent();
                }
            }
        }
    }

    pub fn unref_var(&mut self, id: BindId, by: Reader) {
        logged!(self, unref_var(id, by), RtOp::UnrefVar(id, by))
    }

    /// Mark every boundary that reads `id`: it was delivered this cycle.
    pub fn mark_readers(&mut self, id: BindId) {
        let cycle = self.cycle();
        self.each_reader(&id, &mut |r| r.mark(cycle))
    }

    /// Whether `b` was marked this cycle, or is held dirty.
    pub fn dirty(&self, b: &BoundaryRef) -> bool {
        b.marked(self.cycle())
    }

    /// The writes to `id` queued and not delivered yet: the runtime's,
    /// and those the branches above made this cycle.
    pub fn waiting_writes(&self, id: &BindId) -> usize {
        let mut n = 0;
        let mut view: &RtView<'_, R> = self;
        loop {
            match view {
                RtView::Root(r) => return n + r.waiting_writes(id),
                RtView::Fork(f) => {
                    n += f
                        .log
                        .iter()
                        .filter(|op| matches!(op, RtOp::SetVar(i, _) if i == id))
                        .count();
                    view = f.parent();
                }
            }
        }
    }

    #[inline]
    pub fn set_var(&mut self, id: BindId, value: Value) {
        logged!(self, set_var(id, value), RtOp::SetVar(id, value))
    }

    pub fn set_level(&mut self, id: BindId, value: Value) {
        logged!(self, set_level(id, value), RtOp::SetLevel(id, value))
    }

    pub fn patch_var(
        &mut self,
        id: BindId,
        path: Path,
        value: Value,
        w: Option<Arc<Expr>>,
    ) {
        logged!(self, patch_var(id, path, value, w), RtOp::PatchVar(id, path, value, w))
    }

    pub fn patch_level(&mut self, id: BindId, path: Path, value: Value) {
        logged!(self, patch_level(id, path, value), RtOp::PatchLevel(id, path, value))
    }

    #[inline]
    pub fn notify_set(&mut self, id: BindId) {
        self.mark_readers(id);
        logged!(self, notify_set(id), RtOp::NotifySet(id))
    }

    pub fn set_timer(&mut self, id: BindId, timeout: Duration) {
        logged!(self, set_timer(id, timeout), RtOp::SetTimer(id, timeout))
    }

    pub fn cancel_timer(&mut self, id: BindId) {
        logged!(self, cancel_timer(id), RtOp::CancelTimer(id))
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
        self.mark_readers(id)
    }

    /// See [`Rt::store_insert_standing`].
    pub fn store_insert_standing(&mut self, id: BindId, tv: TagValue) {
        let stamp = standing_stamp(self.store_get(&id), self.cycle());
        self.store_put(id, Some((tv, stamp)));
        self.mark_readers(id)
    }

    /// See [`Rt::store_put`].
    pub fn store_put(&mut self, id: BindId, entry: Option<(TagValue, u64)>) {
        match self {
            Self::Root(r) => r.store_put(id, entry),
            Self::Fork(f) => {
                f.store.insert(id, entry);
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

    /// A binding released or deleted: its stored value and the writes
    /// still waiting for it go.
    pub fn store_release(&mut self, id: &BindId) {
        logged!(self, forget_writes(*id), RtOp::ForgetWrites(*id));
        self.store_remove(id)
    }

    pub fn ref_path(&self, cell: &BindId) -> Option<&RefPath> {
        if let RtView::Fork(f) = self {
            f.note_read(*cell)
        }
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

    pub fn set_ref_path(&mut self, cell: BindId, to: RefPath) {
        match self {
            Self::Root(r) => r.set_ref_path(cell, to),
            Self::Fork(f) => {
                f.ref_paths.insert(cell, Some(to));
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
        let ForkRt { parent: _, cycle: _, reads, store, ref_paths, readers, log } = child;
        match self {
            Self::Fork(f) => {
                if let (Some(mine), Some(theirs)) = (&f.reads, reads) {
                    mine.lock().extend(theirs.into_inner());
                }
                f.store.extend(store);
                f.ref_paths.extend(ref_paths);
                for (id, rs) in readers {
                    f.readers.entry(id).or_default().extend(rs);
                }
                f.log.extend(log);
            }
            Self::Root(r) => {
                for (id, e) in store {
                    r.store_put(id, e);
                }
                for (cell, e) in ref_paths {
                    match e {
                        None => r.clear_ref_path(&cell),
                        Some(to) => r.set_ref_path(cell, to),
                    }
                }
                for op in log {
                    match op {
                        RtOp::RefVar(id, by) => r.ref_var(id, by),
                        RtOp::UnrefVar(id, by) => r.unref_var(id, by),
                        RtOp::SetVar(id, v) => r.set_var(id, v),
                        RtOp::SetLevel(id, v) => r.set_level(id, v),
                        RtOp::PatchVar(id, p, v, w) => r.patch_var(id, p, v, w),
                        RtOp::PatchLevel(id, p, v) => r.patch_level(id, p, v),
                        RtOp::NotifySet(id) => r.notify_set(id),
                        RtOp::ForgetWrites(id) => r.forget_writes(id),
                        RtOp::SetTimer(id, d) => r.set_timer(id, d),
                        RtOp::CancelTimer(id) => r.cancel_timer(id),
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

/// A layer's capacity worth keeping whatever a cycle used.
const SHRINK_FLOOR: usize = 1024;

/// What a layer held for an id before a [`Layered::put`].
pub(crate) enum Prior<V> {
    /// Its own entry.
    Own(V),
    /// Its parent's, which it sees through.
    Inherited,
    /// Nothing it sees.
    Absent,
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

    /// Set `id`, returning this layer's own entry for it.
    #[inline]
    pub fn insert(&mut self, id: BindId, v: V) -> Option<V> {
        let prev = self.map.insert(id, v);
        if prev.is_none() && !self.parent.is_null() {
            self.removed.remove(&id);
        }
        prev
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

    /// Set `id` for a while: what [`Self::put_back`] needs to leave the
    /// layer as it was.
    pub(crate) fn put(&mut self, id: BindId, v: V) -> Prior<V> {
        let inherited = !self.map.contains_key(&id)
            && !self.removed.contains(&id)
            && self.parent().is_some_and(|p| p.contains_key(&id));
        match self.insert(id, v) {
            Some(prev) => Prior::Own(prev),
            None if inherited => Prior::Inherited,
            None => Prior::Absent,
        }
    }

    /// Undo a [`Self::put`].
    pub(crate) fn put_back(&mut self, id: BindId, prior: Prior<V>) {
        match prior {
            Prior::Own(v) => {
                self.map.insert(id, v);
            }
            Prior::Inherited => {
                self.map.remove(&id);
            }
            Prior::Absent => {
                self.remove(&id);
            }
        }
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

    /// Empty this layer. A table whose capacity far exceeds what this
    /// cycle held shrinks: a clear scans every bucket, so one big cycle
    /// would otherwise tax every later one.
    pub fn clear(&mut self) {
        let used = self.map.len();
        self.map.clear();
        if self.map.capacity() > SHRINK_FLOOR.max(used * 4) {
            self.map.shrink_to(used * 2);
        }
        self.removed.clear();
    }

    /// A layer over this one.
    ///
    /// # Safety
    ///
    /// The parent is read through a pointer whose lifetime is erased: it
    /// must outlive this branch and must not be used, by its owner or
    /// another branch's merge, until this branch has merged back.
    pub(crate) unsafe fn fork(&self) -> Self {
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
pub struct ForkCx<R: Rt> {
    parent: *const CxView<'static, R>,
    own: Option<Box<CompileCtx<R>>>,
}

// SAFETY: `parent` is only read, and only while the parent is suspended
// in the join that forked this branch.
unsafe impl<R: Rt + Sync> Send for ForkCx<R> {}

impl<R: Rt> ForkCx<R> {
    /// A branch forked from `parent`.
    ///
    /// # Safety
    ///
    /// The parent is read through a pointer whose lifetime is erased: it
    /// must outlive this branch and must not be used, by its owner or
    /// another branch's merge, until this branch has merged back.
    pub(crate) unsafe fn new(parent: &CxView<'_, R>) -> Self {
        Self {
            parent: parent as *const CxView<'_, R> as *const CxView<'static, R>,
            own: None,
        }
    }

    fn parent(&self) -> &CxView<'_, R> {
        // SAFETY: see the type.
        unsafe { &*(self.parent as *const CxView<'_, R>) }
    }
}

/// The compile state as a branch sees it.
pub enum CxView<'a, R: Rt> {
    Root(&'a mut CompileCtx<R>),
    Fork(&'a mut ForkCx<R>),
}

impl<'a, R: Rt> CxView<'a, R> {
    /// The same view, borrowed for a shorter time.
    pub fn reborrow(&mut self) -> CxView<'_, R> {
        match self {
            Self::Root(c) => CxView::Root(c),
            Self::Fork(f) => CxView::Fork(f),
        }
    }

    /// Take back what the forked branch `child` compiled.
    pub(crate) fn merge(&mut self, child: ForkCx<R>) {
        if let Some(cx) = child.own {
            self.join(*cx)
        }
    }
}

impl<'a, R: Rt> Deref for CxView<'a, R> {
    type Target = CompileCtx<R>;

    #[inline]
    fn deref(&self) -> &CompileCtx<R> {
        let mut view: &CxView<'_, R> = self;
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

impl<'a, R: Rt> DerefMut for CxView<'a, R> {
    #[inline]
    fn deref_mut(&mut self) -> &mut CompileCtx<R> {
        match self {
            Self::Root(c) => c,
            Self::Fork(f) => {
                if f.own.is_none() {
                    let parent: &CompileCtx<R> = f.parent();
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
pub(crate) fn cut<S: Run>(xs: S, ranges: &[(usize, usize)]) -> SmallVec<[S; 16]> {
    let mut rest = Some(xs);
    ranges
        .iter()
        .map(|&(lo, hi)| {
            let (part, r) = rest.take().expect("a range past the end").split_at(hi - lo);
            rest = Some(r);
            part
        })
        .collect()
}

/// A run of siblings a fork point may cut into parts.
pub(crate) trait Run: Sized + Send {
    fn len(&self) -> usize;
    fn split_at(self, i: usize) -> (Self, Self);
}

impl<T: Send> Run for &mut [T] {
    fn len(&self) -> usize {
        <[T]>::len(self)
    }

    fn split_at(self, i: usize) -> (Self, Self) {
        self.split_at_mut(i)
    }
}

impl<K: Send, V: Send> Run for &mut indexmap::map::Slice<K, V> {
    fn len(&self) -> usize {
        indexmap::map::Slice::len(self)
    }

    fn split_at(self, i: usize) -> (Self, Self) {
        self.split_at_mut(i)
    }
}

/// Update `items`, the children of the fork point `site`, as its plan
/// says: in order, timed when it measures, or cut into the ranges it
/// forks, each updated in order on a branch of its own, their results
/// folded in order by `merge`.
pub(crate) fn fork_point<R, S, T>(
    ctx: &mut ExecCtx<'_, R>,
    items: S,
    site: &mut ForkSite,
    in_order: impl Fn(&mut ExecCtx<'_, R>, S, Option<&mut Meter<'_>>) -> T + Sync,
    merge: impl FnMut(T, T) -> T,
) -> T
where
    R: Rt,
    S: Run,
    T: Send,
{
    let n = items.len();
    match site.plan(ctx, n) {
        Plan::Serial => in_order(ctx, items, None),
        Plan::Measure(mut m) => {
            let r = in_order(ctx, items, Some(&mut m));
            m.done(n);
            r
        }
        Plan::Fork(s) => {
            let mut ranges: LPooled<Vec<(usize, usize)>> = LPooled::take();
            s.ranges(0, n, &mut ranges);
            if ranges.len() < 2 {
                return in_order(ctx, items, None);
            }
            let parts = cut(items, &ranges);
            fork_each(ctx, parts, |c, p| in_order(c, p, None))
                .into_iter()
                .reduce(merge)
                .expect("a part")
        }
    }
}

/// What the code around a node says about forking it.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct ForkFlags {
    /// Inside a seq machine, outside any callee body. Its guards and
    /// raises meet through handler state no branch view isolates, in
    /// the order serial evaluation gives them.
    pub(crate) seq: bool,
    /// Under `#[serial]`, callees included.
    pub(crate) inhibit: bool,
    /// Under `#[parallel]`, outside any callee body: every fork point
    /// forks, a collection in ranges of this many slots (0: one range
    /// per worker).
    pub(crate) forced: Option<u32>,
}

impl ForkFlags {
    /// What a callee's body runs under: only `#[serial]` reaches it.
    pub(crate) fn body(self) -> Self {
        Self { seq: false, inhibit: self.inhibit, forced: None }
    }
}

/// A forked branch: its views of the compile state and the runtime, and
/// its event.
struct Branch<R: Rt> {
    cx: ForkCx<R>,
    rt: ForkRt<R>,
    event: Event,
}

impl<R: Rt> Branch<R> {
    /// A branch forked from `ctx`.
    ///
    /// # Safety
    ///
    /// The parent is read through a pointer whose lifetime is erased: it
    /// must outlive this branch and must not be used, by its owner or
    /// another branch's merge, until this branch has merged back.
    unsafe fn new(ctx: &ExecCtx<'_, R>) -> Self {
        // SAFETY: the caller's contract is each view's
        unsafe {
            Branch {
                cx: ForkCx::new(&ctx.cx),
                rt: ForkRt::new(&ctx.rt),
                event: ctx.event.fork(),
            }
        }
    }
}

/// What every branch of one fork shares with the view it forked from.
struct Shared<'a, R: Rt> {
    image_decoder: &'a std::sync::OnceLock<crate::image::SharedDecoder>,
    libstate: &'a crate::LibState,
    core_hook_sites: &'a parking_lot::Mutex<crate::node::coretraits::CoreHookSites<R>>,
    fork_depth: u8,
    par: graphix_types::stack::ParMode,
    fork: ForkFlags,
    boundary: Option<BoundaryRef>,
    prune: graphix_types::stack::PruneMode,
    catch_up: Option<triomphe::Arc<crate::node::wake::CatchUp>>,
}

impl<'a, R: Rt> Shared<'a, R> {
    /// What `ctx`'s branches share, one fork level down.
    fn of(ctx: &ExecCtx<'a, R>) -> Self {
        Shared {
            image_decoder: ctx.image_decoder,
            libstate: ctx.libstate,
            core_hook_sites: ctx.core_hook_sites,
            fork_depth: ctx.fork_depth + 1,
            par: ctx.par,
            fork: ctx.fork,
            boundary: ctx.boundary.clone(),
            prune: ctx.prune,
            catch_up: ctx.catch_up.clone(),
        }
    }

    /// The view the branch `b` runs under.
    fn ctx<'b>(&self, b: &'b mut Branch<R>) -> ExecCtx<'b, R>
    where
        'a: 'b,
    {
        ExecCtx {
            cx: CxView::Fork(&mut b.cx),
            image_decoder: self.image_decoder,
            libstate: self.libstate,
            rt: RtView::Fork(&mut b.rt),
            core_hook_sites: self.core_hook_sites,
            event: &mut b.event,
            fork_depth: self.fork_depth,
            par: self.par,
            fork: self.fork,
            boundary: self.boundary.clone(),
            prune: self.prune,
            catch_up: self.catch_up.clone(),
        }
    }
}

/// `GRAPHIX_PAR_AUDIT`: no branch read what an earlier one published in
/// the same cycle, which serial evaluation would have shown it.
fn audit_in_order<R: Rt>(branches: &[&Branch<R>]) {
    for (i, b) in branches.iter().enumerate() {
        let Some(reads) = b.rt.reads.as_ref() else { continue };
        let reads = reads.lock();
        for a in &branches[..i] {
            audit(&reads, &a.event, &a.rt, &b.event, &b.rt);
        }
    }
}

/// Merge `branches`, forked from `ctx`, back in order: `ctx` ends as the
/// serial evaluation of the branches would leave it. What an earlier
/// branch delivered first, a later one's delivery waits a cycle for,
/// queued where serial evaluation would queue it.
fn merge_in_order<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    branches: impl IntoIterator<Item = Branch<R>>,
) {
    let mut delivered: LPooled<IntSet<BindId>> = LPooled::take();
    for Branch { cx, mut rt, mut event } in branches {
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
    }
}

/// Run `f` on a branch of its own forked from `ctx`, on this thread, and
/// merge it back: the event `f` sees holds as its own entries only what
/// `f` published. With `f`'s result, every runtime call `f` queued that
/// does more than register or drop a reader, by name.
pub(crate) fn alone<R: Rt, T>(
    ctx: &mut ExecCtx<'_, R>,
    f: impl FnOnce(&mut ExecCtx<'_, R>) -> T,
) -> (T, SmallVec<[&'static str; 4]>) {
    // SAFETY: `ctx` is borrowed for this call and used again only after
    // the branch merged, below
    let mut b = unsafe { Branch::new(ctx) };
    // a branch alone is no fork level
    let shared = Shared { fork_depth: ctx.fork_depth, ..Shared::of(ctx) };
    let t = f(&mut shared.ctx(&mut b));
    let effects = b.rt.log.iter().filter_map(RtOp::effect).collect();
    merge_in_order(ctx, [b]);
    (t, effects)
}

/// Run `f`, one part of a fork, on a thread of the pool: under the
/// runtime's control, the caller's tokio runtime and the forking
/// thread's compile context, its share of `live` given back when it
/// ends, or unwinds.
pub(crate) fn as_part<T>(
    control: &Control,
    tokio: Option<&tokio::runtime::Handle>,
    live: &Live,
    f: impl FnOnce() -> T,
) -> T {
    with_control(control, || {
        let _tokio = tokio.map(|h| h.enter());
        let _cx = live.cx.enter();
        let _part = live.part();
        f()
    })
}

/// One part of a [`fork_each`]: its branch and its result.
struct Part<R: Rt, P, T> {
    branch: Branch<R>,
    part: Option<P>,
    out: Option<T>,
}

/// Run `f` over each of `parts` on a branch of its own, every branch
/// forked from `ctx` (siblings, one level down), and merge them back in
/// order: no part sees what another did, and `ctx` ends as the serial
/// evaluation of the parts in order would leave it. The results, in
/// order.
pub fn fork_each<R, P, T, F>(
    ctx: &mut ExecCtx<'_, R>,
    parts: impl IntoIterator<Item = P>,
    f: F,
) -> Vec<T>
where
    R: Rt,
    P: Send,
    T: Send,
    F: Fn(&mut ExecCtx<'_, R>, P) -> T + Sync,
{
    if ctx.control.eval_pool().is_none() {
        return parts.into_iter().map(|p| f(ctx, p)).collect();
    }
    debug_assert!(!ctx.deferred_pending(), "a fork over unapplied compile work");
    let mut branches: Vec<Part<R, P, T>> = parts
        .into_iter()
        // SAFETY: `ctx` is borrowed for this call and used again only after
        // every branch merged, in order, below
        .map(|p| Part { branch: unsafe { Branch::new(ctx) }, part: Some(p), out: None })
        .collect();
    let shared = Shared::of(ctx);
    let control = ctx.control.clone();
    let control = &*control;
    control.forked();
    let tokio = tokio::runtime::Handle::try_current().ok();
    let live = Live::start(control, branches.len());
    on_pool(control, || {
        branches.par_iter_mut().with_max_len(1).for_each(|b| {
            // a part may run on any thread of the pool
            as_part(control, tokio.as_ref(), &live, || {
                let part = b.part.take().expect("a part");
                b.out = Some(f(&mut shared.ctx(&mut b.branch), part));
            })
        })
    });
    if branches.first().is_some_and(|b| b.branch.rt.reads.is_some()) {
        let all: SmallVec<[&Branch<R>; 16]> =
            branches.iter().map(|b| &b.branch).collect();
        audit_in_order(&all);
    }
    let mut out = Vec::with_capacity(branches.len());
    let parts = branches.drain(..).map(|Part { branch, part: _, out: o }| {
        out.push(o.expect("every part ran"));
        branch
    });
    merge_in_order(ctx, parts);
    out
}

/// Update `items`, instances of functions the runtime started (a
/// server's requests), each on a branch of its own when there are two or
/// more and they are independent, else in order. `siblings` is the
/// caller's verdict on its instances: decided here
/// ([`Siblings::decide`]) and kept until the caller resets it, as
/// instances of one function stand for each other. A fresh instance's
/// call binds here first, so no branch compiles; `call` is an item's
/// call.
pub fn fork_instances<R, T, O, F>(
    ctx: &mut ExecCtx<'_, R>,
    siblings: &mut Siblings,
    items: &mut [T],
    call: for<'a> fn(&'a mut T) -> &'a mut Node<R>,
    f: F,
) -> Vec<O>
where
    R: Rt,
    T: Send,
    O: Send,
    F: Fn(&mut ExecCtx<'_, R>, &mut T) -> O + Sync,
{
    for item in items.iter_mut() {
        if let Some(cs) =
            call(item).downcast_mut::<CallNode<R>>().and_then(CallNode::call_mut)
        {
            cs.prebind(ctx)
        }
    }
    if ctx.deferred_pending() {
        ctx.apply_deferred();
    }
    if !siblings.decided() {
        let calls: LPooled<Vec<&Node<R>>> = items.iter_mut().map(|i| &*call(i)).collect();
        siblings.decide(ctx, calls.len(), calls.iter().copied(), Sites::Slots);
    }
    // CR claude for claude: [perf/medium] Every update forks all running instances, with
    // no cost estimate and no saturated() check. An instance's first update is costly,
    // but a waiting instance's later updates are a few nodes each, and each one still
    // pays a fork of every instance and wakes the whole pool. A server whose N
    // instances wait on a 1 ms tick used 5.92 s of CPU against 0.62 s serial at N=300,
    // and 3.20 s against 0.13 s at N=20, with the same answers and wall time. Proposed
    // fix: plan these forks through a cost::SlotSite held beside `siblings`, as
    // collections do, and update the 'whatever their cost' rule in CLAUDE.md. probe:
    // design/review-2026-10-26/repro/x-parallel-serve-fork-every-update.sh
    // (x-parallel-01)
    // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q66, A server
    // forks every instance on every update): yes, plan them (the recommendation: plan
    // them.)
    // 2026-10-28 claude: Not fixed. A rework covering this CR with c-cost-misc-01,
    // x-perf-runtime-02 and x-parallel-01 (handler instances planned as a SlotSite, a
    // fork's threshold raised by a decayed setup overhead, four strict losses before a
    // site goes serial, hysteresis on the sampling schedule) was built and measured, and
    // dropped: pinned to cores 0-11, --no-fusion, mixed.gx (a recursion whose depth
    // alternates 0 and 9 per cycle) took 1.53 s with the rework's first two rounds and
    // 1.42 s with all three, against 1.03 s on main; fade.gx gained 0.02-0.04 s. The
    // cause is the cost model, not the loss rule (which fired at most 40 times a run): a
    // sample taken while a child forked counts only as a lower bound, which inflates a
    // recursion's upper levels, so they fork about twice as often and the level below
    // them almost never. A next attempt should keep main's nested samples or measure
    // against them on mixed.gx. The patches and the benchmark scripts:
    // ~/tmp/grok-forkcost/ and ~/tmp/grok-forkcost/bench/ (run1.sh, batch.sh,
    // p/mixed.gx, p/fade.gx).
    let fork =
        items.len() >= 2 && ctx.fork_mode() != ParMode::Off && siblings.independent();
    match fork {
        true => fork_each(ctx, items.iter_mut(), |c, item| f(c, item)),
        false => items.iter_mut().map(|item| f(ctx, item)).collect(),
    }
}

/// Run `f` over each of `parts` in a compile task of its own, every task
/// forked from `ctx`'s compile state, on the evaluation pool, and join
/// the tasks back in order. A task compiles; it evaluates nothing.
pub(crate) fn compile_each<R, P, F>(
    ctx: &mut ExecCtx<'_, R>,
    parts: impl IntoIterator<Item = P>,
    f: F,
) where
    R: Rt,
    P: Send,
    F: Fn(&mut CompileCtx<R>, P) + Sync,
{
    let mut tasks: Vec<(CompileCtx<R>, Option<P>)> = {
        let cx: &mut CompileCtx<R> = ctx;
        parts.into_iter().map(|p| (cx.fork(), Some(p))).collect()
    };
    let control = ctx.control.clone();
    let control = &*control;
    control.build_forked();
    let tokio = tokio::runtime::Handle::try_current().ok();
    let live = Live::start(control, tasks.len());
    let run = |(task, p): &mut (CompileCtx<R>, Option<P>)| {
        as_part(control, tokio.as_ref(), &live, || {
            task.run_task(|task| f(task, p.take().expect("a part")));
        })
    };
    match control.eval_pool() {
        Some(_) => {
            on_pool(control, || tasks.par_iter_mut().with_max_len(1).for_each(run))
        }
        None => tasks.iter_mut().for_each(run),
    }
    for (task, _) in tasks.drain(..) {
        ctx.cx.join(task)
    }
}

/// Run `a` and `b` as two branches forked from `ctx` and merge them back,
/// `a`'s first: neither sees what the other did, and `ctx` ends as the
/// serial evaluation of `a` then `b` would leave it.
pub fn fork_join<R, A, B, RA, RB>(ctx: &mut ExecCtx<'_, R>, a: A, b: B) -> (RA, RB)
where
    R: Rt,
    A: FnOnce(&mut ExecCtx<'_, R>) -> RA + Send,
    B: FnOnce(&mut ExecCtx<'_, R>) -> RB + Send,
    RA: Send,
    RB: Send,
{
    if ctx.control.eval_pool().is_none() {
        return (a(ctx), b(ctx));
    }
    debug_assert!(!ctx.deferred_pending(), "a fork over unapplied compile work");
    // SAFETY: `ctx` is borrowed for this call and used again only after
    // both branches merged, in order, below
    let (mut left, mut right) = unsafe { (Branch::new(ctx), Branch::new(ctx)) };
    let shared = Shared::of(ctx);
    let control = ctx.control.clone();
    let control = &*control;
    control.forked();
    let tokio = tokio::runtime::Handle::try_current().ok();
    let live = Live::start(control, 2);
    let (ra, rb) = on_pool(control, || {
        rayon::join(
            || {
                let _part = live.part();
                a(&mut shared.ctx(&mut left))
            },
            // the stolen side runs on a thread of its own
            || as_part(control, tokio.as_ref(), &live, || b(&mut shared.ctx(&mut right))),
        )
    });
    if right.rt.reads.is_some() {
        audit_in_order(&[&left, &right]);
    }
    merge_in_order(ctx, [left, right]);
    (ra, rb)
}

/// `GRAPHIX_PAR_AUDIT`: the right branch read nothing the left one
/// published this cycle, which serial evaluation would have shown it.
fn audit<R: Rt>(
    right_reads: &IntSet<BindId>,
    left: &Event,
    left_rt: &ForkRt<R>,
    right: &Event,
    right_rt: &ForkRt<R>,
) {
    let own = |ev: &Event, rt: &ForkRt<R>, id: &BindId| {
        ev.variables.map.contains_key(id)
            || rt.store.contains_key(id)
            || rt.ref_paths.contains_key(id)
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

/// Whether every worker of the running cycle's evaluation pool has a
/// part to run. A fork made then only queues behind them, and a fork
/// costs its branches and merge whether or not another worker takes a
/// part.
pub(crate) fn saturated() -> bool {
    with_current_control(|c| c.is_some_and(Control::eval_saturated))
}

/// The workers of the running cycle's evaluation pool, else those a
/// pool is built with.
pub(crate) fn eval_workers() -> usize {
    with_current_control(|c| c.map_or_else(eval_threads, Control::eval_workers))
}

/// `n` parts forked: each gives its share back when it finishes. Made
/// on the forking thread, it carries that thread's compile context to
/// the parts.
pub(crate) struct Live<'a> {
    control: &'a Control,
    n: AtomicUsize,
    cx: TaskContext,
}

impl<'a> Live<'a> {
    pub(crate) fn start(control: &'a Control, n: usize) -> Self {
        control.add_live_parts(n as isize);
        Live { control, n: AtomicUsize::new(n), cx: TaskContext::current() }
    }

    /// One part's share, given back when it drops.
    pub(crate) fn part(&self) -> LivePart<'_, 'a> {
        LivePart(self)
    }
}

/// A part's share of a [`Live`].
pub(crate) struct LivePart<'b, 'a>(&'b Live<'a>);

impl Drop for LivePart<'_, '_> {
    fn drop(&mut self) {
        self.0.n.fetch_sub(1, Ordering::Relaxed);
        self.0.control.add_live_parts(-1);
    }
}

/// A part that unwound, or never ran, gives its count back here.
impl Drop for Live<'_> {
    fn drop(&mut self) {
        self.control.add_live_parts(-(*self.n.get_mut() as isize));
    }
}

/// Run `f`, a fork's parallel part, on the evaluation pool: entered here
/// from the runtime's own thread, so a cycle's serial work stays there,
/// and joined in place by a fork already on the pool.
pub(crate) fn on_pool<T: Send>(control: &Control, f: impl FnOnce() -> T + Send) -> T {
    let pool = control.eval_pool().expect("a pool before forking");
    if pool.current_thread_index().is_some() {
        return f();
    }
    let tokio = tokio::runtime::Handle::try_current().ok();
    let cx = TaskContext::current();
    pool.install(|| {
        with_control(control, || {
            let _tokio = tokio.as_ref().map(|h| h.enter());
            let _cx = cx.enter();
            f()
        })
    })
}

/// Forks nest at most this deep: a branch this deep runs its fork
/// points serially. Every lookup a branch makes walks at most this many
/// layers, and binary splits this deep already outnumber any machine's
/// cores many times over.
pub const MAX_FORK_DEPTH: u8 = 16;

/// Run `a` then `b`, the two children of `site`, or fork them where the
/// site's plan says.
#[inline]
pub fn join2<R, A, B, RA, RB>(
    site: &mut ForkSite,
    ctx: &mut ExecCtx<'_, R>,
    a: A,
    b: B,
) -> (RA, RB)
where
    R: Rt,
    A: FnOnce(&mut ExecCtx<'_, R>) -> RA + Send,
    B: FnOnce(&mut ExecCtx<'_, R>) -> RB + Send,
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
        Plan::Fork(_) => fork_join(ctx, a, b),
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
fn shared_across_branches<R: Rt>() {
    fn sync<T: Sync>() {}
    sync::<CompileCtx<R>>();
    sync::<Layered<TagValue>>();
    sync::<RtView<'static, R>>();
    sync::<Event>();
}
