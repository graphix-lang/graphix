use crate::GXExt;
use ahash::AHashMap;
use chrono::prelude::*;
use futures::{FutureExt, channel::mpsc, stream::SelectAll};
use graphix_compiler::{
    BindId, CustomBuiltinType, ExecState, Rt, TagValue,
    expr::{Expr, ExprId},
    node::place::{Path, RefPath, VarUpdate},
    prune::Reader,
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

graphix_compiler::dbg_flag!(
    /// `GRAPHIX_DBG_VARS=1` prints every runtime variable event:
    /// `REF_VAR`/`UNREF_VAR` (wake-interest refcounts), `SET_VAR` (queued
    /// cross-cycle writes) and `NOTIFY_SET` (same-cycle bind delivery).
    dbg_vars,
    "GRAPHIX_DBG_VARS"
);

/// How a waiting write came.
#[derive(Debug, Clone)]
pub(super) enum Via {
    /// The program's or the embedder's write.
    Write,
    /// A task's or a watch's reply, dropped when nothing references its
    /// variable any more.
    Reply,
    /// A level's write ([`Rt::set_level`]): a later one replaces it while
    /// it waits.
    Level,
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
    by_id: IntMap<BindId, Fifo<T>>,
    /// The variables with writes waiting, in the order they began to.
    ids: VecDeque<BindId>,
    taken: IntSet<BindId>,
}

/// One variable's writes, oldest first; nearly always one, inline.
#[derive(Debug)]
struct Fifo<T> {
    /// The waiting writes are `q[head..]`, every one `Some`; the taken
    /// ones before them are `None` until they are half of `q`.
    q: SmallVec<[Option<(T, Via)>; 1]>,
    head: usize,
}

impl<T> Default for Fifo<T> {
    fn default() -> Self {
        Self { q: SmallVec::new(), head: 0 }
    }
}

impl<T> Fifo<T> {
    fn is_empty(&self) -> bool {
        self.head == self.q.len()
    }

    fn push(&mut self, w: (T, Via)) {
        self.q.push(Some(w))
    }

    fn front(&self) -> Option<&(T, Via)> {
        self.q.get(self.head).and_then(Option::as_ref)
    }

    fn back_mut(&mut self) -> Option<&mut (T, Via)> {
        match self.is_empty() {
            true => None,
            false => self.q.last_mut().and_then(Option::as_mut),
        }
    }

    fn pop_front(&mut self) -> Option<(T, Via)> {
        let w = self.q.get_mut(self.head)?.take();
        self.head += 1;
        if self.head * 2 >= self.q.len() {
            self.q.drain(..self.head);
            self.head = 0;
        }
        w
    }
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
        q.push((t, via));
    }

    /// Push a level's write, replacing the variable's last waiting write
    /// when that is a level's too and `replaces` it.
    pub(super) fn push_level(
        &mut self,
        id: BindId,
        t: T,
        replaces: impl FnOnce(&T) -> bool,
    ) {
        match self.by_id.get_mut(&id).and_then(|q| q.back_mut()) {
            Some((last, Via::Level)) if replaces(last) => *last = t,
            _ => self.push(id, t, Via::Level),
        }
    }

    /// Drop every write waiting for `id`; `take` passes over its place
    /// in line.
    pub(super) fn forget(&mut self, id: &BindId) {
        self.by_id.remove(id);
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
                        // a member whose writes were forgotten is not waited for
                        let first = |m: &BindId| {
                            !self.taken.contains(m)
                                && self.by_id.get(m).and_then(|q| q.front()).is_none_or(
                                    |(_, v)| matches!(v, Via::Set(s) if Arc::ptr_eq(s, &members)),
                                )
                        };
                        if members.iter().all(first) {
                            for m in members.iter() {
                                if !self.by_id.contains_key(m) {
                                    continue;
                                }
                                let (t, via) = self.pop(m);
                                self.taken.insert(*m);
                                out.push((*m, t, via));
                            }
                        }
                    }
                    Via::Write | Via::Reply | Via::Level => {
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

/// The most readers a variable keeps in a list before it hashes them.
const FEW_READERS: usize = 8;

/// A variable's readers and how many times each reads it: a list while
/// few, hashed past that, so a variable every slot of a collection reads
/// registers each in O(1).
#[derive(Debug)]
pub(super) enum Readers {
    Few(SmallVec<[(Reader, u32); 1]>),
    Many(AHashMap<Reader, u32>),
}

impl Default for Readers {
    fn default() -> Self {
        Self::Few(SmallVec::new())
    }
}

impl Readers {
    fn add(&mut self, r: Reader) {
        match self {
            Self::Few(few) => match few.iter().position(|(e, _)| *e == r) {
                Some(i) => few[i].1 += 1,
                None if few.len() < FEW_READERS => few.push((r, 1)),
                None => {
                    let mut many: AHashMap<Reader, u32> = few.drain(..).collect();
                    many.insert(r, 1);
                    *self = Self::Many(many)
                }
            },
            Self::Many(many) => *many.entry(r).or_insert(0) += 1,
        }
    }

    /// Drop one read by `r`; false when `r` reads nothing here.
    fn remove(&mut self, r: &Reader) -> bool {
        match self {
            Self::Few(few) => {
                let Some(i) = few.iter().position(|(e, _)| e == r) else { return false };
                few[i].1 -= 1;
                if few[i].1 == 0 {
                    few.swap_remove(i);
                }
            }
            Self::Many(many) => {
                let Some(n) = many.get_mut(r) else { return false };
                *n -= 1;
                if *n == 0 {
                    many.remove(r);
                }
            }
        }
        true
    }

    fn is_empty(&self) -> bool {
        match self {
            Self::Few(few) => few.is_empty(),
            Self::Many(many) => many.is_empty(),
        }
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = (&Reader, u32)> {
        let (few, many) = match self {
            Self::Few(few) => (Some(few.iter().map(|(r, n)| (r, *n))), None),
            Self::Many(many) => (None, Some(many.iter().map(|(r, n)| (r, *n)))),
        };
        few.into_iter().flatten().chain(many.into_iter().flatten())
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
    /// Each variable's readers (the root expression and the boundary
    /// around the read) and how many times each reads it.
    pub(super) by_ref: IntMap<BindId, Readers>,
    pub(super) var_updates: Waiting<VarUpdate>,
    /// The place each place-reference cell stands for (`Rt::set_ref_path`).
    pub(super) ref_paths: IntMap<BindId, RefPath>,
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

    /// Whether something may still deliver without new input: a spawned
    /// or custom task, an armed timer, or a watch besides the two the
    /// runtime holds open.
    pub(super) fn outstanding(&self) -> bool {
        !self.tasks.is_empty()
            || !self.custom_tasks.is_empty()
            || !self.timers.is_empty()
            || self.watches.len() > 1
            || self.var_watches.len() > 1
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

    fn forget_writes(&mut self, id: BindId) {
        self.var_updates.forget(&id);
        self.custom_updates.forget(&id);
    }

    fn store_put(&mut self, id: BindId, entry: Option<(TagValue, u64)>) {
        match entry {
            None => self.store.remove(&id),
            Some(e) => self.store.insert(id, e),
        };
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

    fn ref_var(&mut self, id: BindId, ref_by: Reader) {
        if dbg_vars() {
            eprintln!("REF_VAR {id:?} by {ref_by:?}");
        }
        let readers = self.by_ref.entry(id).or_default();
        readers.add(ref_by)
    }

    fn readers(&self, id: &BindId, f: &mut dyn FnMut(&Reader)) {
        if let Some(readers) = self.by_ref.get(id) {
            readers.iter().for_each(|(r, _)| f(r))
        }
    }

    fn unref_var(&mut self, id: BindId, ref_by: Reader) {
        if dbg_vars() {
            eprintln!("UNREF_VAR {id:?} by {ref_by:?}");
        }
        let Some(readers) = self.by_ref.get_mut(&id) else {
            debug_assert!(false, "unref_var of an unregistered {id:?} by {ref_by:?}");
            return;
        };
        let removed = readers.remove(&ref_by);
        debug_assert!(
            removed,
            "unref_var of an unregistered {id:?} by {ref_by:?}, registered by {readers:?}"
        );
        if readers.is_empty() {
            self.by_ref.remove(&id);
        }
    }

    fn set_var(&mut self, id: BindId, value: Value) {
        if dbg_vars() {
            eprintln!("SET_VAR {id:?} = {value}");
        }
        self.var_updates.push(id, VarUpdate::Set(value), Via::Write);
    }

    fn set_level(&mut self, id: BindId, value: Value) {
        if dbg_vars() {
            eprintln!("SET_LEVEL {id:?} = {value}");
        }
        self.var_updates.push_level(id, VarUpdate::Set(value), |_| true);
    }

    fn patch_var(&mut self, id: BindId, path: Path, value: Value, w: Option<Arc<Expr>>) {
        if dbg_vars() {
            eprintln!("PATCH_VAR {id:?} {path:?} = {value}");
        }
        self.var_updates.push(id, VarUpdate::Patch(path, value, w), Via::Write);
    }

    fn patch_level(&mut self, id: BindId, path: Path, value: Value) {
        if dbg_vars() {
            eprintln!("PATCH_LEVEL {id:?} {path:?} = {value}");
        }
        let same =
            |last: &VarUpdate| matches!(last, VarUpdate::Patch(p, ..) if *p == path);
        let update = VarUpdate::Patch(path.clone(), value, None);
        self.var_updates.push_level(id, update, same);
    }

    fn set_ref_path(&mut self, cell: BindId, to: RefPath) {
        self.ref_paths.insert(cell, to);
    }

    fn ref_path(&self, cell: &BindId) -> Option<&RefPath> {
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
            for (r, _) in refed.iter() {
                self.updated.entry(r.root).or_default();
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
