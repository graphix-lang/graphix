//! What a branch of a cycle's update pass sees of the shared state, and
//! how a forked branch's view merges back (`design/parallel_eval.md` §4).
//!
//! A forked branch reads its parent's state frozen and writes deltas
//! and logs of its own; its parent links are raw pointers, valid because
//! a parent is suspended in the join until both of its branches return.

use crate::{BindId, CustomBuiltinType, Rt, TagValue, expr::ExprId, node::place::Path};
use futures::channel::mpsc;
use netidx_value::Value;
use nohash::IntMap;
use poolshark::global::GPooled;
use std::{future::Future, pin::Pin, time::Duration};

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
    store: IntMap<BindId, Option<(TagValue, u64)>>,
    ref_paths: IntMap<BindId, Option<(BindId, Path)>>,
    log: Vec<RtOp>,
}

// SAFETY: `parent` is only read, and only while the parent is suspended
// in the join that forked this branch.
unsafe impl<R: Rt + Sync> Send for ForkRt<R> {}

impl<R: Rt> ForkRt<R> {
    /// A branch forked from `parent`, which must not be used until the
    /// branch has merged back.
    pub(crate) fn new(parent: &RtView<'_, R>) -> Self {
        Self {
            parent: parent as *const RtView<'_, R> as *const RtView<'static, R>,
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

    pub fn ref_var(&mut self, id: BindId, ref_by: ExprId) {
        logged!(self, ref_var(id, ref_by), RtOp::RefVar(id, ref_by))
    }

    pub fn unref_var(&mut self, id: BindId, ref_by: ExprId) {
        logged!(self, unref_var(id, ref_by), RtOp::UnrefVar(id, ref_by))
    }

    pub fn set_var(&mut self, id: BindId, value: Value) {
        logged!(self, set_var(id, value), RtOp::SetVar(id, value))
    }

    pub fn patch_var(&mut self, id: BindId, path: Path, value: Value) {
        logged!(self, patch_var(id, path, value), RtOp::PatchVar(id, path, value))
    }

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

    pub fn cycle(&self) -> u64 {
        match self {
            Self::Root(r) => r.cycle(),
            Self::Fork(f) => f.parent().cycle(),
        }
    }

    /// The (production, cycle stamp) of `id`'s last delivery; see
    /// [`Rt::store_get`].
    pub fn store_get(&self, id: &BindId) -> Option<&(TagValue, u64)> {
        match self {
            Self::Root(r) => r.store_get(id),
            Self::Fork(f) => match f.store.get(id) {
                Some(e) => e.as_ref(),
                None => f.parent().store_get(id),
            },
        }
    }

    /// See [`Rt::store_value`].
    pub fn store_value(&self, id: &BindId) -> Option<Value> {
        stored_value(self.store_get(id))
    }

    pub fn store_insert(&mut self, id: BindId, tv: TagValue) {
        match self {
            Self::Root(r) => r.store_insert(id, tv),
            Self::Fork(f) => {
                let stamp = f.parent().cycle();
                f.store.insert(id, Some((tv, stamp)));
            }
        }
    }

    pub fn store_insert_standing(&mut self, id: BindId, tv: TagValue) {
        match self {
            Self::Root(r) => r.store_insert_standing(id, tv),
            Self::Fork(f) => {
                let stamp = f.parent().cycle().wrapping_sub(1);
                f.store.insert(id, Some((tv, stamp)));
            }
        }
    }

    pub fn store_remove(&mut self, id: &BindId) {
        match self {
            Self::Root(r) => r.store_remove(id),
            Self::Fork(f) => {
                f.store.insert(*id, None);
            }
        }
    }

    pub fn ref_path(&self, cell: &BindId) -> Option<&(BindId, Path)> {
        match self {
            Self::Root(r) => r.ref_path(cell),
            Self::Fork(f) => match f.ref_paths.get(cell) {
                Some(e) => e.as_ref(),
                None => f.parent().ref_path(cell),
            },
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
        let ForkRt { parent: _, store, ref_paths, log } = child;
        match self {
            Self::Fork(f) => {
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
/// through unless the branch wrote or removed them (`None`). A root
/// map has no parent and holds no removals.
pub struct Layered<V> {
    map: IntMap<BindId, Option<V>>,
    parent: *const Layered<V>,
}

// SAFETY: `parent` is only read, and only while the parent is suspended
// in the join that forked this branch.
unsafe impl<V: Send + Sync> Send for Layered<V> {}
unsafe impl<V: Send + Sync> Sync for Layered<V> {}

impl<V> Default for Layered<V> {
    fn default() -> Self {
        Self { map: IntMap::default(), parent: std::ptr::null() }
    }
}

impl<V: std::fmt::Debug> std::fmt::Debug for Layered<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.map.iter()).finish()
    }
}

impl<V: Clone> Layered<V> {
    fn parent(&self) -> Option<&Layered<V>> {
        // SAFETY: see the type.
        unsafe { self.parent.as_ref() }
    }

    pub fn get(&self, id: &BindId) -> Option<&V> {
        match self.map.get(id) {
            Some(e) => e.as_ref(),
            None => self.parent().and_then(|p| p.get(id)),
        }
    }

    pub fn contains_key(&self, id: &BindId) -> bool {
        self.get(id).is_some()
    }

    /// Set `id`, returning what it held.
    pub fn insert(&mut self, id: BindId, v: V) -> Option<V> {
        match self.map.insert(id, Some(v)) {
            Some(prev) => prev,
            None => self.parent().and_then(|p| p.get(&id).cloned()),
        }
    }

    /// Set `id` if it holds nothing, else hand `v` back.
    pub fn try_insert(&mut self, id: BindId, v: V) -> Result<(), V> {
        if self.contains_key(&id) {
            return Err(v);
        }
        self.map.insert(id, Some(v));
        Ok(())
    }

    /// Remove `id`, returning what it held.
    pub fn remove(&mut self, id: &BindId) -> Option<V> {
        match self.parent() {
            None => self.map.remove(id).flatten(),
            Some(p) => {
                let from_parent = p.get(id).cloned();
                let own = match from_parent.is_some() {
                    true => self.map.insert(*id, None),
                    false => self.map.remove(id),
                };
                match own {
                    Some(prev) => prev,
                    None => from_parent,
                }
            }
        }
    }

    /// The entries this layer holds, removals included.
    pub fn len(&self) -> usize {
        self.map.len()
    }

    /// Whether this layer holds nothing.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    pub fn clear(&mut self) {
        self.map.clear()
    }

    /// A layer over this one, which must not be used until the layer has
    /// merged back.
    pub(crate) fn fork(&self) -> Self {
        Self { map: IntMap::default(), parent: self }
    }

    /// Apply the forked layer `child` over this one.
    pub(crate) fn merge(&mut self, child: Self) {
        let root = self.parent.is_null();
        for (id, e) in child.map {
            match e {
                Some(v) => {
                    self.map.insert(id, Some(v));
                }
                None if root => {
                    self.map.remove(&id);
                }
                None => {
                    self.map.insert(id, None);
                }
            }
        }
    }
}
