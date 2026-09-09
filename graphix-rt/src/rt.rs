use crate::GXExt;
use chrono::prelude::*;
use futures::{FutureExt, channel::mpsc, stream::SelectAll};
use graphix_compiler::{
    BindId, CustomBuiltinType, Rt, TagValue,
    expr::ExprId,
    node::place::{Path, VarUpdate},
};
use netidx_value::Value;
use nohash::IntMap;
use poolshark::global::GPooled;
use std::{collections::VecDeque, fmt::Debug, future, time::Duration};
use tokio::{
    task::{self, JoinSet},
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

#[derive(Debug)]
pub struct GXRt<X: GXExt> {
    /// The (production, cycle-stamp) of every bound variable's last
    /// delivery; the cross-cycle read is [`Rt::store`].
    pub(super) store: IntMap<BindId, (TagValue, u64)>,
    /// Bumped once at the top of each `do_cycle`; also the trace
    /// recorder's cycle number.
    pub(super) cycle: u64,
    pub(super) by_ref: IntMap<BindId, IntMap<ExprId, usize>>,
    pub(super) var_updates: VecDeque<(BindId, VarUpdate)>,
    /// The place each place-reference cell stands for (`Rt::set_ref_path`).
    pub(super) ref_paths: IntMap<BindId, (BindId, Path)>,
    pub(super) custom_updates: VecDeque<(BindId, Box<dyn CustomBuiltinType>)>,
    pub(super) tasks: JoinSet<(BindId, Value)>,
    pub(super) custom_tasks: JoinSet<(BindId, Box<dyn CustomBuiltinType>)>,
    pub(super) watches:
        SelectAll<mpsc::Receiver<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>>,
    pub(super) var_watches: SelectAll<mpsc::Receiver<GPooled<Vec<(BindId, Value)>>>>,
    // keeps the SelectAll from ever returning None
    dummy_watch_tx: mpsc::Sender<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
    // keeps the SelectAll from ever returning None
    var_dummy_watch_tx: mpsc::Sender<GPooled<Vec<(BindId, Value)>>>,
    pub(super) updated: IntMap<ExprId, bool>,
    pub ext: X,
}

impl<X: GXExt> GXRt<X> {
    /// A runtime with no network; packages deliver external events
    /// through `watch`/`watch_var`/`spawn_var`.
    pub fn new() -> Self {
        let mut tasks = JoinSet::new();
        tasks.spawn(async { future::pending().await });
        let mut custom_tasks = JoinSet::new();
        custom_tasks.spawn(async { future::pending().await });
        let (dummy_watch_tx, dummy_rx) = mpsc::channel(1);
        let mut watches = SelectAll::new();
        watches.push(dummy_rx);
        let (var_dummy_watch_tx, dummy_rx) = mpsc::channel(1);
        let mut var_watches = SelectAll::new();
        var_watches.push(dummy_rx);
        Self {
            store: IntMap::default(),
            cycle: 0,
            by_ref: IntMap::default(),
            var_updates: VecDeque::new(),
            ref_paths: IntMap::default(),
            custom_updates: VecDeque::new(),
            updated: IntMap::default(),
            ext: X::default(),
            tasks,
            custom_tasks,
            watches,
            var_watches,
            dummy_watch_tx,
            var_dummy_watch_tx,
        }
    }
}

impl<X: GXExt> Default for GXRt<X> {
    fn default() -> Self {
        Self::new()
    }
}

impl<X: GXExt> Rt for GXRt<X> {
    type AbortHandle = task::AbortHandle;

    fn store(&self) -> &IntMap<BindId, (TagValue, u64)> {
        &self.store
    }

    fn store_insert(&mut self, id: BindId, tv: TagValue) {
        self.store.insert(id, (tv, self.cycle));
    }

    fn store_remove(&mut self, id: &BindId) {
        self.store.remove(id);
    }

    fn store_insert_standing(&mut self, id: BindId, tv: TagValue) {
        // stamped one cycle back, so it reads Standing to every same-cycle reader
        self.store.insert(id, (tv, self.cycle.wrapping_sub(1)));
    }

    fn cycle(&self) -> u64 {
        self.cycle
    }

    fn clear(&mut self) {
        let Self {
            store,
            cycle,
            by_ref,
            var_updates,
            ref_paths,
            custom_updates,
            tasks,
            custom_tasks,
            watches,
            var_watches,
            dummy_watch_tx,
            var_dummy_watch_tx,
            updated,
            ext,
        } = self;
        ext.clear();
        updated.clear();
        store.clear();
        *cycle = 0;
        by_ref.clear();
        var_updates.clear();
        ref_paths.clear();
        custom_updates.clear();
        *tasks = JoinSet::new();
        tasks.spawn(async { future::pending().await });
        *custom_tasks = JoinSet::new();
        custom_tasks.spawn(async { future::pending().await });
        *watches = SelectAll::new();
        let (tx, rx) = mpsc::channel(1);
        *dummy_watch_tx = tx;
        watches.push(rx);
        *var_watches = SelectAll::new();
        let (tx, rx) = mpsc::channel(1);
        *var_dummy_watch_tx = tx;
        var_watches.push(rx);
    }

    fn set_timer(&mut self, id: BindId, timeout: Duration) {
        self.tasks.spawn(
            time::sleep(timeout)
                .map(move |()| (id, Value::DateTime(Arc::new(Utc::now())))),
        );
    }

    fn ref_var(&mut self, id: BindId, ref_by: ExprId) {
        if dbg_vars() {
            eprintln!("REF_VAR {id:?} by {ref_by:?}");
        }
        *self.by_ref.entry(id).or_default().entry(ref_by).or_default() += 1;
    }

    fn unref_var(&mut self, id: BindId, ref_by: ExprId) {
        if dbg_vars() {
            eprintln!("UNREF_VAR {id:?} by {ref_by:?}");
        }
        if let Some(refs) = self.by_ref.get_mut(&id) {
            if let Some(cn) = refs.get_mut(&ref_by) {
                *cn -= 1;
                if *cn == 0 {
                    refs.remove(&ref_by);
                }
            }
            if refs.is_empty() {
                self.by_ref.remove(&id);
            }
        }
    }

    fn set_var(&mut self, id: BindId, value: Value) {
        if dbg_vars() {
            eprintln!("SET_VAR {id:?} = {value}");
        }
        self.var_updates.push_back((id, VarUpdate::Set(value)));
    }

    fn patch_var(&mut self, id: BindId, path: Path, value: Value) {
        if dbg_vars() {
            eprintln!("PATCH_VAR {id:?} {path:?} = {value}");
        }
        self.var_updates.push_back((id, VarUpdate::Patch(path, value)));
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
            for eid in refed.keys() {
                self.updated.entry(*eid).or_default();
            }
        }
    }

    fn spawn<
        F: Future<Output = (BindId, Box<dyn CustomBuiltinType>)> + Send + 'static,
    >(
        &mut self,
        f: F,
    ) -> Self::AbortHandle {
        self.custom_tasks.spawn(f)
    }

    fn spawn_var<F: Future<Output = (BindId, Value)> + Send + 'static>(
        &mut self,
        f: F,
    ) -> Self::AbortHandle {
        self.tasks.spawn(f)
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
