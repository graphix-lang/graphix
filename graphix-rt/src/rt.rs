use crate::{GXExt, UpdateBatch, WriteBatch};
use ahash::AHashMap;
use anyhow::{Result, bail};
use arcstr::{ArcStr, literal};
use chrono::prelude::*;
use compact_str::format_compact;
use futures::{FutureExt, channel::mpsc, stream::SelectAll};
use graphix_compiler::{BindId, CustomBuiltinType, Rt, expr::ExprId};
use netidx::{
    path::Path,
    protocol::valarray::ValArray,
    publisher::{self, Id, PublishFlags, Publisher, Val, Value, WriteRequest},
    resolver_client::ChangeTracker,
    subscriber::{self, Dval, SubId, Subscriber, UpdatesFlags},
};
use netidx_protocols::rpc::{
    self,
    server::{ArgSpec, RpcCall},
};
use nohash::IntMap;
use poolshark::global::GPooled;
use std::{
    collections::{VecDeque, hash_map::Entry},
    fmt::Debug,
    future,
    time::Duration,
};
use tokio::{
    sync::Mutex,
    task::{self, JoinSet},
    time::{self, Instant},
};
use triomphe::Arc;

#[derive(Debug)]
pub(super) struct RpcClient {
    proc: rpc::client::Proc,
    pub(super) last_used: Instant,
}

#[derive(Debug)]
pub struct GXRt<X: GXExt> {
    pub(super) by_ref: IntMap<BindId, IntMap<ExprId, usize>>,
    pub(super) subscribed: IntMap<SubId, IntMap<ExprId, usize>>,
    pub(super) published: IntMap<Id, IntMap<ExprId, usize>>,
    pub(super) var_updates: VecDeque<(BindId, Value)>,
    pub(super) custom_updates: VecDeque<(BindId, Box<dyn CustomBuiltinType>)>,
    pub(super) net_updates: VecDeque<(SubId, subscriber::Event)>,
    pub(super) net_writes: VecDeque<(Id, WriteRequest)>,
    pub(super) rpc_overflow: VecDeque<(BindId, RpcCall)>,
    pub(super) rpc_clients: AHashMap<Path, RpcClient>,
    pub(super) published_rpcs: AHashMap<Path, rpc::server::Proc>,
    pub(super) pending_unsubscribe: VecDeque<(Instant, Dval)>,
    pub(super) change_trackers: IntMap<BindId, Arc<Mutex<ChangeTracker>>>,
    pub(super) tasks: JoinSet<(BindId, Value)>,
    pub(super) custom_tasks: JoinSet<(BindId, Box<dyn CustomBuiltinType>)>,
    pub(super) watches:
        SelectAll<mpsc::Receiver<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>>,
    pub(super) var_watches: SelectAll<mpsc::Receiver<GPooled<Vec<(BindId, Value)>>>>,
    // so the selectall will never return None
    dummy_watch_tx: mpsc::Sender<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
    // so the selectall will never return None
    var_dummy_watch_tx: mpsc::Sender<GPooled<Vec<(BindId, Value)>>>,
    pub(super) batch: publisher::UpdateBatch,
    pub(super) publisher: Publisher,
    pub(super) subscriber: Subscriber,
    pub(super) updates_tx: mpsc::Sender<UpdateBatch>,
    pub(super) updates: mpsc::Receiver<UpdateBatch>,
    pub(super) writes_tx: mpsc::Sender<WriteBatch>,
    pub(super) writes: mpsc::Receiver<WriteBatch>,
    pub(super) rpcs_tx: mpsc::Sender<(BindId, RpcCall)>,
    pub(super) rpcs: mpsc::Receiver<(BindId, RpcCall)>,
    pub(super) updated: IntMap<ExprId, bool>,
    pub ext: X,
}

impl<X: GXExt> GXRt<X> {
    pub fn new(publisher: Publisher, subscriber: Subscriber) -> Self {
        let (updates_tx, updates) = mpsc::channel(100);
        let (writes_tx, writes) = mpsc::channel(100);
        let (rpcs_tx, rpcs) = mpsc::channel(100);
        let batch = publisher.start_batch();
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
            by_ref: IntMap::default(),
            var_updates: VecDeque::new(),
            custom_updates: VecDeque::new(),
            net_updates: VecDeque::new(),
            net_writes: VecDeque::new(),
            rpc_overflow: VecDeque::new(),
            rpc_clients: AHashMap::default(),
            subscribed: IntMap::default(),
            pending_unsubscribe: VecDeque::new(),
            published: IntMap::default(),
            change_trackers: IntMap::default(),
            published_rpcs: AHashMap::default(),
            updated: IntMap::default(),
            ext: X::default(),
            tasks,
            custom_tasks,
            watches,
            var_watches,
            dummy_watch_tx,
            var_dummy_watch_tx,
            batch,
            publisher,
            subscriber,
            updates,
            updates_tx,
            writes,
            writes_tx,
            rpcs_tx,
            rpcs,
        }
    }
}

macro_rules! or_err {
    ($bindid:expr, $e:expr) => {
        match $e {
            Ok(v) => v,
            Err(e) => {
                let e = ArcStr::from(format_compact!("{e:?}").as_str());
                let e = Value::Error(Arc::new(Value::String(e)));
                return ($bindid, e);
            }
        }
    };
}

macro_rules! check_changed {
    ($id:expr, $resolver:expr, $path:expr, $ct:expr) => {
        let mut ct = $ct.lock().await;
        if ct.path() != &$path {
            *ct = ChangeTracker::new($path.clone());
        }
        if !or_err!($id, $resolver.check_changed(&mut *ct).await) {
            return ($id, Value::Null);
        }
    };
}

impl<X: GXExt> Rt for GXRt<X> {
    type AbortHandle = task::AbortHandle;

    fn clear(&mut self) {
        let Self {
            by_ref,
            var_updates,
            custom_updates,
            net_updates,
            net_writes,
            rpc_clients,
            rpc_overflow,
            subscribed,
            published,
            published_rpcs,
            pending_unsubscribe,
            change_trackers,
            tasks,
            custom_tasks,
            watches,
            var_watches,
            dummy_watch_tx,
            var_dummy_watch_tx,
            batch,
            publisher,
            subscriber: _,
            updates_tx,
            updates,
            writes_tx,
            writes,
            rpcs,
            rpcs_tx,
            updated,
            ext,
        } = self;
        ext.clear();
        updated.clear();
        by_ref.clear();
        var_updates.clear();
        custom_updates.clear();
        net_updates.clear();
        net_writes.clear();
        rpc_overflow.clear();
        rpc_clients.clear();
        subscribed.clear();
        published.clear();
        published_rpcs.clear();
        pending_unsubscribe.clear();
        change_trackers.clear();
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
        *batch = publisher.start_batch();
        let (tx, rx) = mpsc::channel(3);
        *updates_tx = tx;
        *updates = rx;
        let (tx, rx) = mpsc::channel(100);
        *writes_tx = tx;
        *writes = rx;
        let (tx, rx) = mpsc::channel(100);
        *rpcs_tx = tx;
        *rpcs = rx
    }

    fn call_rpc(&mut self, name: Path, args: Vec<(ArcStr, Value)>, id: BindId) {
        let now = Instant::now();
        let proc = match self.rpc_clients.entry(name) {
            Entry::Occupied(mut e) => {
                let cl = e.get_mut();
                cl.last_used = now;
                Ok(cl.proc.clone())
            }
            Entry::Vacant(e) => {
                match rpc::client::Proc::new(&self.subscriber, e.key().clone()) {
                    Err(e) => Err(e),
                    Ok(proc) => {
                        let cl = RpcClient { last_used: now, proc: proc.clone() };
                        e.insert(cl);
                        Ok(proc)
                    }
                }
            }
        };
        self.tasks.spawn(async move {
            macro_rules! err {
                ($e:expr) => {{
                    let e = format_compact!("{:?}", $e);
                    (id, Value::error(e.as_str()))
                }};
            }
            match proc {
                Err(e) => err!(e),
                Ok(proc) => match proc.call(args).await {
                    Err(e) => err!(e),
                    Ok(res) => (id, res),
                },
            }
        });
    }

    fn publish_rpc(
        &mut self,
        name: Path,
        doc: Value,
        spec: Vec<ArgSpec>,
        id: BindId,
    ) -> Result<()> {
        use rpc::server::Proc;
        let e = match self.published_rpcs.entry(name) {
            Entry::Vacant(e) => e,
            Entry::Occupied(_) => bail!("already published"),
        };
        let proc = Proc::new(
            &self.publisher,
            e.key().clone(),
            doc,
            spec,
            move |c| Some((id, c)),
            Some(self.rpcs_tx.clone()),
        )?;
        e.insert(proc);
        Ok(())
    }

    fn unpublish_rpc(&mut self, name: Path) {
        self.published_rpcs.remove(&name);
    }

    fn subscribe(&mut self, flags: UpdatesFlags, path: Path, ref_by: ExprId) -> Dval {
        let dval =
            self.subscriber.subscribe_updates(path, [(flags, self.updates_tx.clone())]);
        *self.subscribed.entry(dval.id()).or_default().entry(ref_by).or_default() += 1;
        dval
    }

    fn unsubscribe(&mut self, _path: Path, dv: Dval, ref_by: ExprId) {
        if let Some(exprs) = self.subscribed.get_mut(&dv.id()) {
            if let Some(cn) = exprs.get_mut(&ref_by) {
                *cn -= 1;
                if *cn == 0 {
                    exprs.remove(&ref_by);
                }
            }
            if exprs.is_empty() {
                self.subscribed.remove(&dv.id());
            }
        }
        self.pending_unsubscribe.push_back((Instant::now(), dv));
    }

    fn list(&mut self, id: BindId, path: Path) {
        let ct = self
            .change_trackers
            .entry(id)
            .or_insert_with(|| Arc::new(Mutex::new(ChangeTracker::new(path.clone()))));
        let ct = Arc::clone(ct);
        let resolver = self.subscriber.resolver();
        self.tasks.spawn(async move {
            check_changed!(id, resolver, path, ct);
            let mut paths = or_err!(id, resolver.list(path).await);
            let paths = paths.drain(..).map(|p| Value::String(p.into()));
            (id, Value::Array(ValArray::from_iter_exact(paths)))
        });
    }

    fn list_table(&mut self, id: BindId, path: Path) {
        let ct = self
            .change_trackers
            .entry(id)
            .or_insert_with(|| Arc::new(Mutex::new(ChangeTracker::new(path.clone()))));
        let ct = Arc::clone(ct);
        let resolver = self.subscriber.resolver();
        self.tasks.spawn(async move {
            check_changed!(id, resolver, path, ct);
            let mut tbl = or_err!(id, resolver.table(path).await);
            let cols =
                tbl.cols.drain(..).map(|(name, _count)| Value::String(name.into()));
            let cols = Value::Array(ValArray::from_iter_exact(cols));
            let rows = tbl.rows.drain(..).map(|name| Value::String(name.into()));
            let rows = Value::Array(ValArray::from_iter_exact(rows));
            let tbl = Value::Array(ValArray::from([
                Value::Array(ValArray::from([Value::String(literal!("columns")), cols])),
                Value::Array(ValArray::from([Value::String(literal!("rows")), rows])),
            ]));
            (id, tbl)
        });
    }

    fn stop_list(&mut self, id: BindId) {
        self.change_trackers.remove(&id);
    }

    fn publish(&mut self, path: Path, value: Value, ref_by: ExprId) -> Result<Val> {
        let val = self.publisher.publish_with_flags_and_writes(
            PublishFlags::empty(),
            path,
            value,
            Some(self.writes_tx.clone()),
        )?;
        let id = val.id();
        *self.published.entry(id).or_default().entry(ref_by).or_default() += 1;
        Ok(val)
    }

    fn update(&mut self, val: &Val, value: Value) {
        val.update(&mut self.batch, value);
    }

    fn unpublish(&mut self, val: Val, ref_by: ExprId) {
        if let Some(refs) = self.published.get_mut(&val.id()) {
            if let Some(cn) = refs.get_mut(&ref_by) {
                *cn -= 1;
                if *cn == 0 {
                    refs.remove(&ref_by);
                }
            }
            if refs.is_empty() {
                self.published.remove(&val.id());
            }
        }
    }

    fn set_timer(&mut self, id: BindId, timeout: Duration) {
        self.tasks.spawn(
            time::sleep(timeout)
                .map(move |()| (id, Value::DateTime(Arc::new(Utc::now())))),
        );
    }

    fn ref_var(&mut self, id: BindId, ref_by: ExprId) {
        *self.by_ref.entry(id).or_default().entry(ref_by).or_default() += 1;
    }

    fn unref_var(&mut self, id: BindId, ref_by: ExprId) {
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
        self.var_updates.push_back((id, value.clone()));
    }

    fn notify_set(&mut self, id: BindId) {
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
