//! Package-owned netidx state. Since the netidx extraction
//! (design/netidx_extraction.md) the graphix core has no networking:
//! the `sys::net` builtins own their subscriber/publisher here,
//! delivered into the reactive graph through the generic machinery
//! every package uses (`Rt::watch_var` for subscription updates,
//! `Rt::watch` + [`graphix_compiler::CustomBuiltinType`] for
//! write-backs and rpc-server calls, `Rt::spawn_var` for one-shot
//! calls). Lives in `ctx.libstate`; materialized on first use from
//! the embedder-seeded [`NetConfig`] (absent → a process-internal
//! `InternalOnly`, created on a dedicated side thread so
//! materialization is legal from sync builtin code on any tokio
//! flavor).
use anyhow::{Error, Result, anyhow};
use arcstr::literal;
use futures::{StreamExt, channel::mpsc};
use graphix_compiler::{BindId, CustomBuiltinType, ExecCtx, Rt, UserEvent};
use graphix_package_core::{NetConfig, NetTimeouts};
use netidx::{
    config::Config,
    path::Path,
    publisher::{
        BindCfg, DesiredAuth, Id, PublishFlags, Publisher, PublisherBuilder, UpdateBatch,
        Val, WriteRequest,
    },
    resolver_client::ChangeTracker,
    subscriber::{
        Dval, Event as NEvent, SubId, Subscriber, SubscriberBuilder, UpdatesFlags,
    },
};
use netidx_value::Value;
use nohash::IntMap;
use parking_lot::Mutex;
use poolshark::global::{GPooled, Pool};
use std::{
    collections::VecDeque,
    sync::{Arc, LazyLock, OnceLock},
    time::Duration,
};
use tokio::{sync::oneshot, task, time};
use triomphe::Arc as TArc;

/// A subscription update routed to a builtin's BindId. Unsubscribed
/// becomes the same error value the pre-extraction runtime delivered.
fn translate(ev: NEvent) -> Value {
    match ev {
        NEvent::Update(v) => v,
        NEvent::Unsubscribed => Value::error(literal!("unsubscribed")),
    }
}

/// A write request on a published value, delivered to the publish
/// builtin as a custom event (carries the optional reply channel).
#[derive(Debug)]
pub(crate) struct NetWrite(pub(crate) WriteRequest);

impl CustomBuiltinType for NetWrite {}

/// An incoming rpc-server call, delivered to the publish_rpc builtin
/// as a custom event (carries the reply channel).
#[derive(Debug)]
pub(crate) struct NetRpcCall(pub(crate) Option<netidx_protocols::rpc::server::RpcCall>);

impl CustomBuiltinType for NetRpcCall {}

static VBATCH: LazyLock<Pool<Vec<(BindId, Value)>>> =
    LazyLock::new(|| Pool::new(32, 16384));
static CBATCH: LazyLock<Pool<Vec<(BindId, Box<dyn CustomBuiltinType>)>>> =
    LazyLock::new(|| Pool::new(32, 16384));

struct Handles {
    publisher: Publisher,
    subscriber: Subscriber,
    // Some when self-hosted: dropping shuts the side thread (and the
    // internal resolver) down.
    _own: Option<InternalGuard>,
}

struct InternalGuard {
    shutdown: Option<oneshot::Sender<()>>,
}

impl Drop for InternalGuard {
    fn drop(&mut self) {
        if let Some(tx) = self.shutdown.take() {
            let _ = tx.send(());
        }
    }
}

#[derive(Default)]
struct Routes {
    /// netidx SHARES Dvals by path: several builtins subscribing the
    /// same path get the same SubId, so routing is a fan-out list
    /// (the pre-extraction runtime's event.netidx map was shared by
    /// every reader of the SubId).
    subs: IntMap<SubId, smallvec::SmallVec<[BindId; 2]>>,
    writes: IntMap<Id, BindId>,
}

struct Inner {
    handles: OnceLock<Handles>,
    routes: Arc<Mutex<Routes>>,
    // channel ends handed to netidx; the pump translates into the
    // graph's watch channels
    updates_tx: mpsc::Sender<GPooled<Vec<(SubId, NEvent)>>>,
    writes_tx: mpsc::Sender<GPooled<Vec<WriteRequest>>>,
    rpcs_tx: mpsc::Sender<(BindId, netidx_protocols::rpc::server::RpcCall)>,
    // publish batching: builtins queue updates here; the flusher task
    // commits when pinged
    batch: Mutex<Option<UpdateBatch>>,
    flush_tx: mpsc::UnboundedSender<()>,
    // deferred unsubscribe (grace period, matches the pre-extraction
    // runtime's 60s Dval hold)
    graveyard_tx: mpsc::UnboundedSender<Dval>,
    // rpc client procs, GC'd on use
    rpc_clients: Mutex<Vec<(Path, netidx_protocols::rpc::client::Proc, time::Instant)>>,
    // per-list-builtin resolver change trackers
    change_trackers: Mutex<IntMap<BindId, Arc<tokio::sync::Mutex<ChangeTracker>>>>,
    publish_timeout: Option<Duration>,
    subscribe_timeout: Option<Duration>,
}

pub use graphix_package_core::NetConfig as NetConfigExport;

/// The libstate entry. Clone-cheap; one per ExecCtx.
#[derive(Clone)]
pub struct NetState(Arc<Inner>);

impl std::fmt::Debug for NetState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NetState")
    }
}

impl NetState {
    /// Get (or create) the net state for this runtime. On creation the
    /// pump/flusher/graveyard tasks spawn and their delivery channels
    /// register with the runtime — which is why this needs `ctx`.
    pub(crate) fn get<R: Rt, E: UserEvent>(ctx: &mut ExecCtx<R, E>) -> NetState {
        if let Some(st) = ctx.libstate.get::<NetState>() {
            return st.clone();
        }
        let timeouts = ctx
            .libstate
            .get::<NetTimeouts>()
            .cloned()
            .unwrap_or(NetTimeouts { publish: None, subscribe: None });
        let (updates_tx, mut updates_rx) = mpsc::channel(100);
        let (writes_tx, mut writes_rx) = mpsc::channel(100);
        let (rpcs_tx, mut rpcs_rx) = mpsc::channel(100);
        let (flush_tx, mut flush_rx) = mpsc::unbounded();
        let (graveyard_tx, mut graveyard_rx) = mpsc::unbounded::<Dval>();
        let routes: Arc<Mutex<Routes>> = Arc::default();
        // the graph-facing delivery channels
        let (mut var_tx, var_rx) = mpsc::channel(100);
        let (mut custom_tx, custom_rx) = mpsc::channel(100);
        ctx.rt.watch_var(var_rx);
        ctx.rt.watch(custom_rx);
        let st = NetState(Arc::new(Inner {
            handles: OnceLock::new(),
            routes: routes.clone(),
            updates_tx,
            writes_tx,
            rpcs_tx,
            batch: Mutex::new(None),
            flush_tx,
            graveyard_tx,
            rpc_clients: Mutex::new(Vec::new()),
            change_trackers: Mutex::new(IntMap::default()),
            publish_timeout: timeouts.publish,
            subscribe_timeout: timeouts.subscribe,
        }));
        // PUMP: netidx events -> graph events. Subscription updates
        // route SubId -> the subscribing builtin's BindId; writes route
        // the published Id -> the publish builtin's write BindId; rpc
        // calls are already keyed by the builtin's BindId.
        {
            let routes = routes.clone();
            task::spawn(async move {
                loop {
                    tokio::select! {
                        batch = updates_rx.next() => match batch {
                            None => break,
                            Some(mut batch) => {
                                let mut out = VBATCH.take();
                                {
                                    // coalesce per SubId (last wins) —
                                    // the same channel can be
                                    // registered on a shared Dval more
                                    // than once, and the old
                                    // event.netidx map coalesced —
                                    // then fan out to every registered
                                    // reader.
                                    let mut last: IntMap<SubId, NEvent> =
                                        IntMap::default();
                                    for (sub_id, ev) in batch.drain(..) {
                                        last.insert(sub_id, ev);
                                    }
                                    let routes = routes.lock();
                                    for (sub_id, ev) in last {
                                        if let Some(ids) = routes.subs.get(&sub_id) {
                                            for id in ids {
                                                out.push((*id, translate(ev.clone())));
                                            }
                                        }
                                    }
                                }
                                if !out.is_empty() {
                                    use futures::SinkExt;
                                    if var_tx.send(out).await.is_err() {
                                        break;
                                    }
                                }
                            }
                        },
                        batch = writes_rx.next() => match batch {
                            None => break,
                            Some(mut batch) => {
                                let mut out = CBATCH.take();
                                {
                                    let routes = routes.lock();
                                    for req in batch.drain(..) {
                                        if let Some(id) = routes.writes.get(&req.id) {
                                            out.push((
                                                *id,
                                                Box::new(NetWrite(req))
                                                    as Box<dyn CustomBuiltinType>,
                                            ));
                                        }
                                    }
                                }
                                if !out.is_empty() {
                                    use futures::SinkExt;
                                    if custom_tx.send(out).await.is_err() {
                                        break;
                                    }
                                }
                            }
                        },
                        call = rpcs_rx.next() => match call {
                            None => break,
                            Some((id, call)) => {
                                let mut out = CBATCH.take();
                                out.push((
                                    id,
                                    Box::new(NetRpcCall(Some(call)))
                                        as Box<dyn CustomBuiltinType>,
                                ));
                                use futures::SinkExt;
                                if custom_tx.send(out).await.is_err() {
                                    break;
                                }
                            }
                        },
                    }
                }
            });
        }
        // FLUSHER: commit the publish batch when pinged, coalescing
        // pings that arrive while a commit is in flight — the package
        // twin of the pre-extraction end-of-cycle commit.
        {
            let st2 = st.clone();
            task::spawn(async move {
                while let Some(()) = flush_rx.next().await {
                    let batch = st2.0.batch.lock().take();
                    if let Some(batch) = batch {
                        if batch.len() > 0 {
                            batch.commit(st2.0.publish_timeout).await;
                        }
                    }
                }
            });
        }
        // GRAVEYARD: hold unsubscribed Dvals for the grace period so
        // rapid resubscription reuses the connection.
        task::spawn(async move {
            while let Some(dv) = graveyard_rx.next().await {
                task::spawn(async move {
                    time::sleep(Duration::from_secs(60)).await;
                    drop(dv);
                });
            }
        });
        ctx.libstate.set(st.clone());
        st
    }

    /// The netidx handles, materializing on first touch per
    /// [`NetConfig`]. Self-hosted/config builds happen on a dedicated
    /// side thread running its own runtime, so this is legal from sync
    /// builtin code on any tokio flavor; the calling thread blocks for
    /// the spinup once per runtime.
    fn handles<R: Rt, E: UserEvent>(&self, ctx: &mut ExecCtx<R, E>) -> Result<&Handles> {
        if let Some(h) = self.0.handles.get() {
            return Ok(h);
        }
        let cfg = ctx.libstate.get::<NetConfig>().cloned().unwrap_or(NetConfig::Internal);
        let handles = match cfg {
            NetConfig::Ready { publisher, subscriber } => {
                Handles { publisher, subscriber, _own: None }
            }
            cfg => {
                let (tx, rx) = std::sync::mpsc::channel::<Result<_, Error>>();
                let (shutdown, shutdown_rx) = oneshot::channel::<()>();
                std::thread::Builder::new()
                    .name("gx-netidx".into())
                    .spawn(move || {
                        let rt = match tokio::runtime::Builder::new_current_thread()
                            .enable_all()
                            .build()
                        {
                            Ok(rt) => rt,
                            Err(e) => {
                                let _ = tx.send(Err(Error::from(e)));
                                return;
                            }
                        };
                        rt.block_on(async move {
                            let r = async {
                                match cfg {
                                    NetConfig::Ready { .. } => unreachable!(),
                                    NetConfig::Internal => {
                                        let env = netidx::InternalOnly::new().await?;
                                        let p = env.publisher().clone();
                                        let s = env.subscriber().clone();
                                        Ok::<_, Error>((
                                            p,
                                            s,
                                            Some(Box::new(env)
                                                as Box<dyn std::any::Any + Send>),
                                        ))
                                    }
                                    NetConfig::Config { config, auth, bind } => {
                                        let publisher =
                                            PublisherBuilder::new(config.clone())
                                                .desired_auth(auth.clone())
                                                .bind_cfg(bind)
                                                .build()
                                                .await?;
                                        let subscriber = SubscriberBuilder::new(config)
                                            .desired_auth(auth)
                                            .build()?;
                                        Ok((publisher, subscriber, None))
                                    }
                                }
                            }
                            .await;
                            match r {
                                Err(e) => {
                                    let _ = tx.send(Err(e));
                                }
                                Ok((p, s, own)) => {
                                    let _ = tx.send(Ok((p, s)));
                                    // hold the internal env (and this
                                    // runtime's tasks) until shutdown
                                    let _own = own;
                                    let _ = shutdown_rx.await;
                                }
                            }
                        })
                    })
                    .map_err(|e| anyhow!("spawning the netidx thread: {e:?}"))?;
                let (publisher, subscriber) =
                    rx.recv().map_err(|_| anyhow!("netidx thread died"))??;
                Handles {
                    publisher,
                    subscriber,
                    _own: Some(InternalGuard { shutdown: Some(shutdown) }),
                }
            }
        };
        let _ = self.0.handles.set(handles);
        Ok(self.0.handles.get().unwrap())
    }

    pub(crate) fn subscribe<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<R, E>,
        flags: UpdatesFlags,
        path: Path,
        id: BindId,
    ) -> Result<Dval> {
        let updates_tx = self.0.updates_tx.clone();
        let dv =
            self.handles(ctx)?.subscriber.subscribe_updates(path, [(flags, updates_tx)]);
        self.0.routes.lock().subs.entry(dv.id()).or_default().push(id);
        Ok(dv)
    }

    pub(crate) fn unsubscribe(&self, dv: Dval, id: BindId) {
        {
            let mut routes = self.0.routes.lock();
            if let Some(ids) = routes.subs.get_mut(&dv.id()) {
                if let Some(i) = ids.iter().position(|x| *x == id) {
                    ids.remove(i);
                }
                if ids.is_empty() {
                    routes.subs.remove(&dv.id());
                }
            }
        }
        let _ = self.0.graveyard_tx.unbounded_send(dv);
    }

    pub(crate) fn publish<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<R, E>,
        path: Path,
        value: Value,
        write_id: BindId,
    ) -> Result<Val> {
        let writes_tx = self.0.writes_tx.clone();
        let val = self.handles(ctx)?.publisher.publish_with_flags_and_writes(
            PublishFlags::empty(),
            path,
            value,
            Some(writes_tx),
        )?;
        self.0.routes.lock().writes.insert(val.id(), write_id);
        Ok(val)
    }

    pub(crate) fn update_val(&self, val: &Val, value: Value) {
        {
            let mut batch = self.0.batch.lock();
            let batch = match &mut *batch {
                Some(b) => b,
                None => {
                    // the publisher must exist — `val` came from publish
                    let h = self.0.handles.get().expect("publish before update");
                    *batch = Some(h.publisher.start_batch());
                    batch.as_mut().unwrap()
                }
            };
            val.update(batch, value);
        }
        let _ = self.0.flush_tx.unbounded_send(());
    }

    pub(crate) fn unpublish(&self, val: Val) {
        self.0.routes.lock().writes.remove(&val.id());
        drop(val)
    }

    pub(crate) fn call_rpc<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<R, E>,
        path: Path,
        args: Vec<(arcstr::ArcStr, Value)>,
        id: BindId,
    ) {
        use netidx_protocols::rpc::client::Proc;
        let proc = {
            let subscriber = match self.handles(ctx) {
                Ok(h) => h.subscriber.clone(),
                Err(e) => {
                    let e = compact_str::format_compact!("{e:?}");
                    ctx.rt.set_var(id, Value::error(e.as_str()));
                    return;
                }
            };
            let now = time::Instant::now();
            let mut clients = self.0.rpc_clients.lock();
            clients.retain(|(_, _, last)| {
                now.saturating_duration_since(*last) < Duration::from_secs(60)
            });
            match clients.iter_mut().find(|(p, _, _)| p == &path) {
                Some((_, proc, last)) => {
                    *last = now;
                    Ok(proc.clone())
                }
                None => match Proc::new(&subscriber, path.clone()) {
                    Err(e) => Err(e),
                    Ok(proc) => {
                        clients.push((path, proc.clone(), now));
                        Ok(proc)
                    }
                },
            }
        };
        match proc {
            Err(e) => {
                let e = compact_str::format_compact!("{e:?}");
                ctx.rt.set_var(id, Value::error(e.as_str()));
            }
            Ok(proc) => {
                ctx.rt.spawn_var(async move {
                    let r = match proc.call(args).await {
                        Ok(v) => v,
                        Err(e) => {
                            let e = compact_str::format_compact!("{e:?}");
                            Value::error(e.as_str())
                        }
                    };
                    (id, r)
                });
            }
        }
    }

    pub(crate) fn publish_rpc<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<R, E>,
        path: Path,
        doc: Value,
        spec: Vec<netidx_protocols::rpc::server::ArgSpec>,
        id: BindId,
    ) -> Result<netidx_protocols::rpc::server::Proc> {
        use netidx_protocols::rpc::server::Proc;
        let rpcs_tx = self.0.rpcs_tx.clone();
        let publisher = self.handles(ctx)?.publisher.clone();
        let proc = Proc::new(
            &publisher,
            path,
            doc,
            spec,
            move |c| Some((id, c)),
            Some(rpcs_tx),
        )?;
        Ok(proc)
    }

    /// One-shot resolver list/table. The `flushed` barrier is
    /// read-your-writes: this process's own publishes must be
    /// registered with the resolver before the list runs (the jul22
    /// lazy-netidx branch flaked 4/10 without it).
    pub(crate) fn list<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<R, E>,
        id: BindId,
        path: Path,
        table: bool,
    ) {
        let (publisher, subscriber) = match self.handles(ctx) {
            Ok(h) => (h.publisher.clone(), h.subscriber.clone()),
            Err(e) => {
                let e = compact_str::format_compact!("{e:?}");
                ctx.rt.set_var(id, Value::error(e.as_str()));
                return;
            }
        };
        let ct = {
            let mut cts = self.0.change_trackers.lock();
            let ct = cts.entry(id).or_insert_with(|| {
                Arc::new(tokio::sync::Mutex::new(ChangeTracker::new(path.clone())))
            });
            Arc::clone(ct)
        };
        ctx.rt.spawn_var(async move {
            publisher.flushed().await;
            let resolver = subscriber.resolver();
            {
                let mut ct = ct.lock().await;
                if ct.path() != &path {
                    *ct = ChangeTracker::new(path.clone());
                }
                match resolver.check_changed(&mut *ct).await {
                    Err(e) => {
                        let e = compact_str::format_compact!("{e:?}");
                        return (id, Value::error(e.as_str()));
                    }
                    Ok(false) => return (id, Value::Null),
                    Ok(true) => (),
                }
            }
            if table {
                match resolver.table(path).await {
                    Err(e) => {
                        let e = compact_str::format_compact!("{e:?}");
                        (id, Value::error(e.as_str()))
                    }
                    Ok(mut tbl) => {
                        use netidx::protocol::valarray::ValArray;
                        let cols = tbl
                            .cols
                            .drain(..)
                            .map(|(name, _)| Value::String(name.into()));
                        let cols = Value::Array(ValArray::from_iter_exact(cols));
                        let rows =
                            tbl.rows.drain(..).map(|name| Value::String(name.into()));
                        let rows = Value::Array(ValArray::from_iter_exact(rows));
                        let tbl = Value::Array(ValArray::from([
                            Value::Array(ValArray::from([
                                Value::String(literal!("columns")),
                                cols,
                            ])),
                            Value::Array(ValArray::from([
                                Value::String(literal!("rows")),
                                rows,
                            ])),
                        ]));
                        (id, tbl)
                    }
                }
            } else {
                match resolver.list(path).await {
                    Err(e) => {
                        let e = compact_str::format_compact!("{e:?}");
                        (id, Value::error(e.as_str()))
                    }
                    Ok(mut paths) => {
                        use netidx::protocol::valarray::ValArray;
                        let paths = paths.drain(..).map(|p| Value::String(p.into()));
                        (id, Value::Array(ValArray::from_iter_exact(paths)))
                    }
                }
            }
        });
    }

    pub(crate) fn stop_list(&self, id: BindId) {
        self.0.change_trackers.lock().remove(&id);
    }

    /// The raw subscriber for embedder-side consumers (the gui
    /// data_table). Materializes on first call.
    pub fn subscriber<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<R, E>,
    ) -> Result<Subscriber> {
        Ok(self.handles(ctx)?.subscriber.clone())
    }
}
