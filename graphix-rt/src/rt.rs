use crate::GXExt;
use chrono::prelude::*;
use futures::{FutureExt, channel::mpsc, stream::SelectAll};
use graphix_compiler::{
    BindId, CustomBuiltinType, ExecState, Rt, TagValue,
    expr::ExprId,
    node::place::{Path, VarUpdate},
};
use netidx_value::Value;
use nohash::{IntMap, IntSet};
use poolshark::global::GPooled;
use smallvec::SmallVec;
use std::{collections::VecDeque, fmt::Debug, time::Duration};
use tokio::{
    task::{AbortHandle, JoinSet},
    time,
};
use triomphe::Arc;

/// `GRAPHIX_DBG_VARS=1` prints every runtime variable event:
/// `REF_VAR`/`UNREF_VAR` (wake-interest refcounts), `SET_VAR` (queued
/// cross-cycle writes) and `NOTIFY_SET` (same-cycle bind delivery).
/// Checked once; set before launch.
fn dbg_vars() -> bool {
    static ON: std::sync::LazyLock<bool> =
        std::sync::LazyLock::new(|| std::env::var_os("GRAPHIX_DBG_VARS").is_some());
    *ON
}

/// How a waiting write came.
#[derive(Debug, Clone)]
pub(super) enum Via {
    /// The program's or the embedder's write.
    Write,
    /// A task's or a watch's reply, dropped when nothing references its
    /// variable any more.
    Reply,
    /// A member of a set that lands in one cycle (`set_many`, a
    /// callable's arguments): the set's variables.
    Set(Arc<[BindId]>),
}

/// Writes waiting for a cycle. A variable takes one delivery a cycle, so
/// each variable's writes wait in a FIFO of their own and land in order,
/// and a cycle costs the variables waiting, not the writes. A set's
/// writes land in the one cycle where each is first in its variable's
/// FIFO.
#[derive(Debug)]
pub(super) struct Waiting<T> {
    by_id: IntMap<BindId, VecDeque<(T, Via)>>,
    /// The variables with writes waiting, in the order they began to.
    ids: VecDeque<BindId>,
    taken: IntSet<BindId>,
}

impl<T> Default for Waiting<T> {
    fn default() -> Self {
        Self { by_id: IntMap::default(), ids: VecDeque::new(), taken: IntSet::default() }
    }
}

impl<T> Waiting<T> {
    pub(super) fn push(&mut self, id: BindId, t: T, via: Via) {
        let q = self.by_id.entry(id).or_default();
        if q.is_empty() {
            self.ids.push_back(id);
        }
        q.push_back((t, via));
    }

    pub(super) fn has(&self, id: &BindId) -> bool {
        self.by_id.contains_key(id)
    }

    pub(super) fn is_empty(&self) -> bool {
        self.by_id.is_empty()
    }

    fn pop(&mut self, id: &BindId) -> (T, Via) {
        let q = self.by_id.get_mut(id).expect("a waiting variable");
        let r = q.pop_front().expect("a waiting write");
        if q.is_empty() {
            self.by_id.remove(id);
        }
        r
    }

    /// This cycle's deliveries into `out`: each waiting variable's first
    /// write, a set's only when it is first for every member.
    pub(super) fn take(&mut self, out: &mut Vec<(BindId, T, Via)>) {
        for _ in 0..self.ids.len() {
            let id = self.ids.pop_front().expect("a waiting variable");
            let Some((_, via)) = self.by_id.get(&id).and_then(|q| q.front()) else {
                continue;
            };
            if !self.taken.contains(&id) {
                match via {
                    Via::Set(members) => {
                        let members = members.clone();
                        let first = |m: &BindId| {
                            !self.taken.contains(m)
                                && self.by_id.get(m).and_then(|q| q.front()).is_some_and(
                                    |(_, v)| matches!(v, Via::Set(s) if Arc::ptr_eq(s, &members)),
                                )
                        };
                        if members.iter().all(first) {
                            for m in members.iter() {
                                let (t, via) = self.pop(m);
                                self.taken.insert(*m);
                                out.push((*m, t, via));
                            }
                        }
                    }
                    Via::Write | Via::Reply => {
                        let (t, via) = self.pop(&id);
                        self.taken.insert(id);
                        out.push((id, t, via));
                    }
                }
            }
            if self.by_id.contains_key(&id) {
                self.ids.push_back(id);
            }
        }
        self.taken.clear();
    }
}

#[derive(Debug)]
pub struct GXRt<X: GXExt> {
    /// The (production, cycle-stamp) of every bound variable's last
    /// delivery; the cross-cycle read is [`Rt::store`].
    pub(super) store: IntMap<BindId, (TagValue, u64)>,
    /// Bumped after each cycle's nodes ran; also the trace
    /// recorder's cycle number.
    pub(super) cycle: u64,
    /// Each variable's readers and how many times each reads it: under a
    /// script one expression, inline.
    pub(super) by_ref: IntMap<BindId, SmallVec<[(ExprId, u32); 1]>>,
    pub(super) var_updates: Waiting<VarUpdate>,
    /// The place each place-reference cell stands for (`Rt::set_ref_path`).
    pub(super) ref_paths: IntMap<BindId, (BindId, Path)>,
    pub(super) custom_updates: Waiting<Box<dyn CustomBuiltinType>>,
    pub(super) tasks: JoinSet<(BindId, Value)>,
    /// The timers not fired yet, so a released one can be stopped.
    pub(super) timers: IntMap<BindId, AbortHandle>,
    pub(super) custom_tasks: JoinSet<(BindId, Box<dyn CustomBuiltinType>)>,
    pub(super) watches:
        SelectAll<mpsc::Receiver<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>>,
    pub(super) var_watches: SelectAll<mpsc::Receiver<GPooled<Vec<(BindId, Value)>>>>,
    // held so the watch streams never end
    _keepalive_watch_tx: mpsc::Sender<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
    _keepalive_var_watch_tx: mpsc::Sender<GPooled<Vec<(BindId, Value)>>>,
    pub(super) updated: IntMap<ExprId, bool>,
    pub ext: X,
}

impl<X: GXExt> GXRt<X> {
    /// The whole store.
    pub fn store(&self) -> &IntMap<BindId, (TagValue, u64)> {
        &self.store
    }

    fn previous_cycle(&self) -> u64 {
        self.cycle.wrapping_sub(1)
    }

    /// An execution state over a new runtime, its event's user part
    /// the extension's empty event.
    pub fn new_state() -> anyhow::Result<ExecState<Self, X::UserEvent>> {
        let mut rt = Self::new();
        let user = rt.ext.empty_event();
        ExecState::new(rt, user)
    }

    /// A runtime with no network; packages deliver external events
    /// through `watch`/`watch_var`/`spawn_var`.
    pub fn new() -> Self {
        let tasks = JoinSet::new();
        let custom_tasks = JoinSet::new();
        let (keepalive_watch_tx, dummy_rx) = mpsc::channel(1);
        let mut watches = SelectAll::new();
        watches.push(dummy_rx);
        let (keepalive_var_watch_tx, dummy_rx) = mpsc::channel(1);
        let mut var_watches = SelectAll::new();
        var_watches.push(dummy_rx);
        Self {
            store: IntMap::default(),
            cycle: 0,
            by_ref: IntMap::default(),
            var_updates: Waiting::default(),
            ref_paths: IntMap::default(),
            custom_updates: Waiting::default(),
            updated: IntMap::default(),
            ext: X::default(),
            tasks,
            timers: IntMap::default(),
            custom_tasks,
            watches,
            var_watches,
            _keepalive_watch_tx: keepalive_watch_tx,
            _keepalive_var_watch_tx: keepalive_var_watch_tx,
        }
    }
}

impl<X: GXExt> Default for GXRt<X> {
    fn default() -> Self {
        Self::new()
    }
}

impl<X: GXExt> Rt for GXRt<X> {
    fn store_get(&self, id: &BindId) -> Option<&(TagValue, u64)> {
        self.store.get(id)
    }

    fn store_insert(&mut self, id: BindId, tv: TagValue) {
        self.store.insert(id, (tv, self.cycle));
    }

    fn store_remove(&mut self, id: &BindId) {
        self.store.remove(id);
    }

    fn store_insert_standing(&mut self, id: BindId, tv: TagValue) {
        self.store.insert(id, (tv, self.previous_cycle()));
    }

    fn cycle(&self) -> u64 {
        self.cycle
    }

    fn set_timer(&mut self, id: BindId, timeout: Duration) {
        let h = self.tasks.spawn(
            time::sleep(timeout)
                .map(move |()| (id, Value::DateTime(Arc::new(Utc::now())))),
        );
        self.timers.insert(id, h);
    }

    fn cancel_timer(&mut self, id: BindId) {
        if let Some(h) = self.timers.remove(&id) {
            h.abort();
        }
    }

    fn ref_var(&mut self, id: BindId, ref_by: ExprId) {
        if dbg_vars() {
            eprintln!("REF_VAR {id:?} by {ref_by:?}");
        }
        let readers = self.by_ref.entry(id).or_default();
        match readers.iter_mut().find(|(e, _)| *e == ref_by) {
            Some((_, n)) => *n += 1,
            None => readers.push((ref_by, 1)),
        }
    }

    fn unref_var(&mut self, id: BindId, ref_by: ExprId) {
        if dbg_vars() {
            eprintln!("UNREF_VAR {id:?} by {ref_by:?}");
        }
        if let Some(readers) = self.by_ref.get_mut(&id) {
            if let Some(i) = readers.iter().position(|(e, _)| *e == ref_by) {
                readers[i].1 -= 1;
                if readers[i].1 == 0 {
                    readers.swap_remove(i);
                }
            }
            if readers.is_empty() {
                self.by_ref.remove(&id);
            }
        }
    }

    fn set_var(&mut self, id: BindId, value: Value) {
        if dbg_vars() {
            eprintln!("SET_VAR {id:?} = {value}");
        }
        self.var_updates.push(id, VarUpdate::Set(value), Via::Write);
    }

    fn patch_var(&mut self, id: BindId, path: Path, value: Value) {
        if dbg_vars() {
            eprintln!("PATCH_VAR {id:?} {path:?} = {value}");
        }
        self.var_updates.push(id, VarUpdate::Patch(path, value), Via::Write);
    }

    fn set_ref_path(&mut self, cell: BindId, root: BindId, path: Path) {
        self.ref_paths.insert(cell, (root, path));
    }

    fn ref_path(&self, cell: &BindId) -> Option<&(BindId, Path)> {
        self.ref_paths.get(cell)
    }

    fn clear_ref_path(&mut self, cell: &BindId) {
        self.ref_paths.remove(cell);
    }

    fn notify_set(&mut self, id: BindId) {
        if dbg_vars() {
            eprintln!("NOTIFY_SET {id:?} -> {:?}", self.by_ref.get(&id));
        }
        if let Some(refed) = self.by_ref.get(&id) {
            for (eid, _) in refed {
                self.updated.entry(*eid).or_default();
            }
        }
    }

    fn spawn<
        F: Future<Output = (BindId, Box<dyn CustomBuiltinType>)> + Send + 'static,
    >(
        &mut self,
        f: F,
    ) {
        self.custom_tasks.spawn(f);
    }

    fn spawn_var<F: Future<Output = (BindId, Value)> + Send + 'static>(&mut self, f: F) {
        self.tasks.spawn(f);
    }

    fn watch(
        &mut self,
        s: mpsc::Receiver<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
    ) {
        self.watches.push(s)
    }

    fn watch_var(&mut self, s: mpsc::Receiver<GPooled<Vec<(BindId, Value)>>>) {
        self.var_watches.push(s)
    }
}
