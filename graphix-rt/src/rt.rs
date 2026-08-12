use crate::GXExt;
use chrono::prelude::*;
use futures::{FutureExt, channel::mpsc, stream::SelectAll};
use graphix_compiler::{BindId, CustomBuiltinType, Rt, TagValue, expr::ExprId};
use netidx_value::Value;
use nohash::IntMap;
use poolshark::global::GPooled;
use std::{collections::VecDeque, fmt::Debug, future, time::Duration};
use tokio::{
    task::{self, JoinSet},
    time,
};
use triomphe::Arc;

/// `GRAPHIX_DBG_VARS=1` — print every runtime variable event:
/// `REF_VAR`/`UNREF_VAR` (the (BindId, ExprId) wake-interest refcount),
/// `SET_VAR` (a queued cross-cycle write), and `NOTIFY_SET` (same-cycle
/// bind delivery with the current interest map). The tool for "who
/// publishes/wakes this bind" — found the dead-eliminated module
/// statement (a fused region waiting forever on a feeder whose producer
/// was spliced away, 2026-07-08). Checked once; set before launch.
fn dbg_vars() -> bool {
    static ON: std::sync::LazyLock<bool> =
        std::sync::LazyLock::new(|| std::env::var_os("GRAPHIX_DBG_VARS").is_some());
    *ON
}

#[derive(Debug)]
pub struct GXRt<X: GXExt> {
    /// The last DELIVERED value of every bound variable — see
    /// [`Rt::cached`]. Written by the cycle loop as each variable
    /// event lands in `event.variables`, and by the same-cycle
    /// publishers through [`Rt::cached_insert`].
    pub(super) cached: IntMap<BindId, Value>,
    /// Dense-delivery P3 SHADOW: the persistent tagged store that
    /// replaces BOTH `cached` and the per-cycle event map at the 5b
    /// flip (design/dense_delivery.md R3). Dual-written beside every
    /// `cached` write, stamped with [`Self::cycle`]; never read except
    /// by the gated agreement assert (`GRAPHIX_STORE_ASSERT=1` in
    /// `do_cycle`). Tags are placeholder FIRED until the flip —
    /// producers write their genuine tags at 5b.
    pub(super) store: IntMap<BindId, (TagValue, u64)>,
    /// The store's clock — bumped once at the top of each `do_cycle`.
    /// Also the trace recorder's cycle number (one clock).
    pub(super) cycle: u64,
    pub(super) by_ref: IntMap<BindId, IntMap<ExprId, usize>>,
    pub(super) var_updates: VecDeque<(BindId, Value)>,
    pub(super) custom_updates: VecDeque<(BindId, Box<dyn CustomBuiltinType>)>,
    pub(super) tasks: JoinSet<(BindId, Value)>,
    pub(super) custom_tasks: JoinSet<(BindId, Box<dyn CustomBuiltinType>)>,
    pub(super) watches:
        SelectAll<mpsc::Receiver<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>>,
    pub(super) var_watches: SelectAll<mpsc::Receiver<GPooled<Vec<(BindId, Value)>>>>,
    // so the selectall will never return None
    dummy_watch_tx: mpsc::Sender<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
    // so the selectall will never return None
    var_dummy_watch_tx: mpsc::Sender<GPooled<Vec<(BindId, Value)>>>,
    pub(super) updated: IntMap<ExprId, bool>,
    pub ext: X,
}

impl<X: GXExt> GXRt<X> {
    /// A runtime with no netidx (or any other network) — networking
    /// lives in packages since the netidx extraction
    /// (design/netidx_extraction.md); sys::net owns its handles in
    /// `ctx.libstate` and delivers through the generic
    /// `watch`/`watch_var`/`spawn_var` machinery.
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
            cached: IntMap::default(),
            store: IntMap::default(),
            cycle: 0,
            by_ref: IntMap::default(),
            var_updates: VecDeque::new(),
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

impl<X: GXExt> GXRt<X> {
    /// The P3 shadow-store agreement gate (`GRAPHIX_STORE_ASSERT=1`):
    /// the store is dual-written beside every `cached` write, so at
    /// every cycle end the two must agree exactly — equal key sets,
    /// equal values (tags/stamps are store-only). Called by `do_cycle`;
    /// a divergence here means a `cached` write path bypassed
    /// [`Rt::cached_insert`]/[`Rt::cached_remove`].
    pub(super) fn store_assert(&self) {
        static ON: std::sync::LazyLock<bool> = std::sync::LazyLock::new(|| {
            std::env::var_os("GRAPHIX_STORE_ASSERT").is_some()
        });
        if !*ON {
            return;
        }
        assert_eq!(
            self.store.len(),
            self.cached.len(),
            "shadow store/cached len divergence"
        );
        for (id, v) in self.cached.iter() {
            match self.store.get(id) {
                Some((tv, _)) => tv.with_value(|sv| {
                    assert!(
                        sv == v,
                        "shadow store/cached divergence at {id:?}: store {sv} cached {v}"
                    )
                }),
                None => panic!("shadow store missing {id:?} present in cached"),
            }
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

    fn cached(&self) -> &IntMap<BindId, Value> {
        &self.cached
    }

    fn cached_insert(&mut self, id: BindId, v: Value) {
        self.store.insert(id, (TagValue::fired(v.clone()), self.cycle));
        self.cached.insert(id, v);
    }

    fn cached_remove(&mut self, id: &BindId) {
        self.store.remove(id);
        self.cached.remove(id);
    }

    fn clear(&mut self) {
        let Self {
            cached,
            store,
            cycle,
            by_ref,
            var_updates,
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
        cached.clear();
        store.clear();
        *cycle = 0;
        by_ref.clear();
        var_updates.clear();
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
        self.var_updates.push_back((id, value.clone()));
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
