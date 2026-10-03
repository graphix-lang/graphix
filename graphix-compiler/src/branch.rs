//! What a branch of a cycle's update pass sees of the shared state, and
//! how a forked branch's view merges back (`design/parallel_eval.md` §4).
//!
//! A forked branch reads its parent's state frozen and writes deltas
//! and logs of its own; its parent links are raw pointers, valid because
//! a parent is suspended in the join until both of its branches return.

use crate::{
    BindId, CompileCtx, CustomBuiltinType, ExecCtx, Rt, TagValue, UserEvent,
    expr::ExprId, node::place::Path,
};
use futures::channel::mpsc;
use graphix_types::stack::ParMode;
use netidx_value::Value;
use nohash::IntMap;
use poolshark::global::GPooled;
use std::{
    future::Future,
    ops::{Deref, DerefMut},
    pin::Pin,
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
            cycle: parent.cycle(),
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
            Self::Fork(f) => f.cycle,
        }
    }

    /// The (production, cycle stamp) of `id`'s last delivery; see
    /// [`Rt::store_get`].
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
    pub fn store_value(&self, id: &BindId) -> Option<Value> {
        stored_value(self.store_get(id))
    }

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
        let ForkRt { parent: _, cycle: _, store, ref_paths, log } = child;
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
        let mut layer = self;
        loop {
            match layer.map.get(id) {
                Some(e) => return e.as_ref(),
                None => layer = layer.parent()?,
            }
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

/// Run `a` and `b` as two branches forked from `ctx` and merge them back,
/// `a`'s first: neither sees what the other did, and `ctx` ends as the
/// serial evaluation of `a` then `b` would leave it.
pub fn fork_join<R, E, A, B, RA, RB>(ctx: &mut ExecCtx<'_, R, E>, a: A, b: B) -> (RA, RB)
where
    R: Rt,
    E: UserEvent,
    A: FnOnce(&mut ExecCtx<'_, R, E>) -> RA,
    B: FnOnce(&mut ExecCtx<'_, R, E>) -> RB,
{
    debug_assert!(!ctx.deferred_pending(), "a fork over unapplied compile work");
    let (mut rt_a, mut rt_b) = (ForkRt::new(&ctx.rt), ForkRt::new(&ctx.rt));
    let (mut cx_a, mut cx_b) = (ForkCx::new(&ctx.cx), ForkCx::new(&ctx.cx));
    let (mut ev_a, mut ev_b) = (ctx.event.fork(), ctx.event.fork());
    let (libstate, hooks, control, decoder) =
        (ctx.libstate, ctx.core_hook_sites, ctx.control, ctx.image_decoder);
    let fork_depth = ctx.fork_depth + 1;
    let branch = |cx, rt, event| ExecCtx {
        cx: CxView::Fork(cx),
        image_decoder: decoder,
        libstate,
        rt: RtView::Fork(rt),
        core_hook_sites: hooks,
        control,
        event,
        fork_depth,
    };
    let ra = a(&mut branch(&mut cx_a, &mut rt_a, &mut ev_a));
    let rb = b(&mut branch(&mut cx_b, &mut rt_b, &mut ev_b));
    ctx.cx.merge(cx_a);
    ctx.rt.merge(rt_a);
    ctx.event.merge(ev_a);
    ctx.cx.merge(cx_b);
    ctx.rt.merge(rt_b);
    ctx.event.merge(ev_b);
    (ra, rb)
}

/// Forks nest at most this deep: a branch this deep runs its fork
/// points serially. Every lookup a branch makes walks at most this many
/// layers, and binary splits this deep already outnumber any machine's
/// cores many times over.
pub const MAX_FORK_DEPTH: u8 = 16;

/// Whether a fork point forks. Until the cost model, only a forced
/// runtime does.
pub fn forks<R: Rt, E: UserEvent>(ctx: &ExecCtx<'_, R, E>) -> bool {
    ctx.fork_depth < MAX_FORK_DEPTH && ctx.control.par_mode() == ParMode::Force
}
