//! Package-owned netidx state for the `sys::net` builtins: the subscriber and
//! publisher, delivered into the graph through `Rt::watch_var`, `Rt::watch` +
//! [`graphix_compiler::CustomBuiltinType`] and `Rt::spawn_var`. Lives in
//! `ctx.libstate`; materialized on first use from the embedder-seeded
//! [`NetConfig`] (absent: a process-internal netidx on a dedicated side thread).
use anyhow::{Error, Result, anyhow};
use arcstr::literal;
use compact_str::format_compact;
use futures::{
    StreamExt,
    channel::mpsc,
    future::{AbortHandle, Abortable},
};
use graphix_compiler::{
    BindId, CustomBuiltinType, ExecCtx, Rt, UserEvent, branch::RtView,
};
use graphix_package_core::{Inbox, NetConfig, NetTimeouts};
use netidx::{
    path::Path,
    publisher::{
        Id, PublishFlags, Publisher, PublisherBuilder, UpdateBatch, Val, WriteRequest,
    },
    resolver_client::ChangeTracker,
    subscriber::{
        Dval, Event as NEvent, SubId, Subscriber, SubscriberBuilder, UpdatesFlags,
    },
};
use netidx_derive::IntoValue;
use netidx_value::Value;
use nohash::IntMap;
use parking_lot::Mutex;
use poolshark::global::{GPooled, Pool};
use std::{
    sync::{Arc, LazyLock, OnceLock},
    time::Duration,
};
use tokio::{
    sync::{Notify, oneshot},
    task, time,
};

graphix_compiler::dbg_flag!(rpc_dbg, "GXDBG_RPC");

fn error_value(error: Error) -> Value {
    Value::error(format_compact!("{error:#}").as_str())
}

/// A subscription update routed to a builtin's BindId. Unsubscribed
/// is delivered as an error value.
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

/// Calls are waiting in a publish_rpc builtin's inbox.
#[derive(Debug)]
pub(crate) struct NetRpcWake;

impl CustomBuiltinType for NetRpcWake {}

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

/// The raw netidx handles — a standalone libstate entry shared by
/// [`NetState`] and the `netidx:` module loader, so whichever touches
/// netidx first materializes the ONE universe both use.
#[derive(Clone, Default)]
pub struct NetHandles(Arc<Universe>);

#[derive(Default)]
struct Universe {
    handles: OnceLock<Handles>,
    /// held across a build, so concurrent first touches build one
    building: Mutex<()>,
}

impl std::fmt::Debug for NetHandles {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NetHandles")
    }
}

impl NetHandles {
    fn get(&self) -> Option<&Handles> {
        self.0.handles.get()
    }

    pub fn subscriber(&self, cfg: NetConfig) -> Result<Subscriber> {
        Ok(self.get_or_materialize(cfg)?.subscriber.clone())
    }
}

impl NetHandles {
    fn get_or_materialize(&self, cfg: NetConfig) -> Result<&Handles> {
        if let Some(h) = self.get() {
            return Ok(h);
        }
        let _building = self.0.building.lock();
        if let Some(h) = self.get() {
            return Ok(h);
        }
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
                        // a load blocks on a runtime of its own
                        let cfg = match cfg {
                            NetConfig::Load { load, bind } => match load() {
                                Ok((config, auth)) => {
                                    NetConfig::Config { config, auth, bind }
                                }
                                Err(e) => {
                                    let _ = tx.send(Err(e));
                                    return;
                                }
                            },
                            cfg => cfg,
                        };
                        rt.block_on(async move {
                            let r = async {
                                match cfg {
                                    NetConfig::Ready { .. } | NetConfig::Load { .. } => {
                                        unreachable!()
                                    }
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
                                        build(config, auth, bind).await
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
                    .map_err(|e| anyhow!("spawning the netidx thread: {e:#}"))?;
                let (publisher, subscriber) =
                    rx.recv().map_err(|_| anyhow!("netidx thread died"))??;
                Handles {
                    publisher,
                    subscriber,
                    _own: Some(InternalGuard { shutdown: Some(shutdown) }),
                }
            }
        };
        Ok(self.0.handles.get_or_init(|| handles))
    }
}

type Built = (Publisher, Subscriber, Option<Box<dyn std::any::Any + Send>>);

async fn build(
    config: netidx::config::Config,
    auth: netidx::publisher::DesiredAuth,
    bind: Option<netidx::publisher::BindCfg>,
) -> Result<Built> {
    let publisher = PublisherBuilder::new(config.clone())
        .desired_auth(auth.clone())
        .bind_cfg(bind)
        .build()
        .await?;
    let subscriber = SubscriberBuilder::new(config).desired_auth(auth).build()?;
    Ok((publisher, subscriber, None))
}

#[derive(Default)]
struct Routes {
    subs: IntMap<SubId, Route>,
    writes: IntMap<Id, BindId>,
}

/// One subscription's routing. netidx SHARES Dvals by path: several
/// builtins subscribing the same path get the same SubId, so routing is a
/// fan-out list. The route lives while any Dval this state handed out for
/// the SubId does (a reader's or a write target's), and so knows whether
/// the updates channel is registered on it:
/// netidx then sends a new reader nothing (`NO_SPURIOUS`), and the
/// subscribe replays the last value itself.
#[derive(Default)]
struct Route {
    ids: smallvec::SmallVec<[BindId; 2]>,
    held: usize,
    streamed: bool,
}

struct PendingRequest(AbortHandle);

impl Drop for PendingRequest {
    fn drop(&mut self) {
        self.0.abort();
    }
}

struct Inner {
    handles: NetHandles,
    routes: Arc<Mutex<Routes>>,
    netidx_updates_tx: mpsc::Sender<GPooled<Vec<(SubId, NEvent)>>>,
    writes_tx: mpsc::Sender<GPooled<Vec<WriteRequest>>>,
    rpcs_tx: mpsc::Sender<BindId>,
    /// The flusher holds this buffer without retaining the Inner.
    pending_publish_batch: Arc<Mutex<Option<UpdateBatch>>>,
    flush: Arc<Notify>,
    flusher: task::JoinHandle<()>,
    // rpc client procs, GC'd on use
    rpc_clients: Mutex<Vec<(Path, netidx_protocols::rpc::client::Proc, time::Instant)>>,
    requests: Mutex<IntMap<BindId, PendingRequest>>,
    /// A tracker is retained only after its listing completes successfully.
    change_trackers:
        Mutex<IntMap<BindId, Arc<tokio::sync::Mutex<Option<ChangeTracker>>>>>,
}

impl Drop for Inner {
    fn drop(&mut self) {
        self.flusher.abort();
    }
}

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
    /// pump and flusher tasks spawn and their delivery channels
    /// register with the runtime — which is why this needs `ctx`.
    /// Branches of a cycle may ask at once: one creates it.
    pub fn get<R: Rt, E: UserEvent>(ctx: &mut ExecCtx<'_, R, E>) -> NetState {
        if let Some(st) = ctx.libstate.get::<NetState>() {
            return st;
        }
        let timeouts =
            ctx.libstate.get::<NetTimeouts>().unwrap_or(NetTimeouts { publish: None });
        let handles = ctx.libstate.get_or_default::<NetHandles>();
        let rt = &mut ctx.rt;
        ctx.libstate.get_or_else(|| Self::create(rt, timeouts, handles))
    }

    fn create<R: Rt>(
        rt: &mut RtView<'_, R>,
        timeouts: NetTimeouts,
        handles: NetHandles,
    ) -> Self {
        let (updates_tx, mut updates_rx) = mpsc::channel(100);
        let (writes_tx, mut writes_rx) = mpsc::channel(100);
        let (rpcs_tx, mut rpcs_rx) = mpsc::channel(100);
        let routes: Arc<Mutex<Routes>> = Arc::default();
        // the graph-facing delivery channels
        let (mut var_tx, var_rx) = mpsc::channel(100);
        let (mut custom_tx, custom_rx) = mpsc::channel(100);
        rt.watch_var(var_rx);
        rt.watch(custom_rx);
        let pending_publish_batch: Arc<Mutex<Option<UpdateBatch>>> = Arc::default();
        let flush = Arc::new(Notify::new());
        let flusher = {
            let pending = pending_publish_batch.clone();
            let flush = flush.clone();
            let timeout = timeouts.publish;
            task::spawn(async move {
                loop {
                    flush.notified().await;
                    let batch = pending.lock().take();
                    if let Some(batch) = batch
                        && batch.len() > 0
                    {
                        batch.commit(timeout).await;
                    }
                }
            })
        };
        let st = NetState(Arc::new(Inner {
            handles,
            routes: routes.clone(),
            netidx_updates_tx: updates_tx,
            writes_tx,
            rpcs_tx,
            pending_publish_batch,
            flush,
            flusher,
            rpc_clients: Mutex::new(Vec::new()),
            requests: Mutex::new(IntMap::default()),
            change_trackers: Mutex::new(IntMap::default()),
        }));
        // PUMP: netidx events -> graph events. Subscription updates route
        // SubId -> the subscribing builtin's BindId; writes route the
        // published Id -> the publish builtin's write BindId.
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
                                    // every update, in order, to every reader
                                    let routes = routes.lock();
                                    for (sub_id, ev) in batch.drain(..) {
                                        if let Some(route) = routes.subs.get(&sub_id) {
                                            for id in &route.ids {
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
                            Some(id) => {
                                if rpc_dbg() {
                                    eprintln!("RPCDBG pump: calls for {id:?}");
                                }
                                let mut out = CBATCH.take();
                                out.push((
                                    id,
                                    Box::new(NetRpcWake) as Box<dyn CustomBuiltinType>,
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
        st
    }

    /// The netidx handles, materializing on first touch per [`NetConfig`].
    /// Self-hosted/config builds run on a dedicated side thread, so this is
    /// legal from sync builtin code; the caller blocks for the spinup once.
    fn handles<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<'_, R, E>,
    ) -> Result<&Handles> {
        if let Some(handles) = self.0.handles.get() {
            return Ok(handles);
        }
        let cfg = ctx.libstate.get::<NetConfig>().unwrap_or(NetConfig::Internal);
        self.0.handles.get_or_materialize(cfg)
    }

    pub(crate) fn subscribe<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<'_, R, E>,
        flags: UpdatesFlags,
        path: Path,
        id: BindId,
    ) -> Result<Dval> {
        let updates_tx = self.0.netidx_updates_tx.clone();
        // The shared channel is registered once per Dval: a later
        // subscriber of the path gets the path's last value alone, never a
        // re-send to every subscriber of it.
        let flags = flags | UpdatesFlags::NO_SPURIOUS;
        // CR claude for eric: [risk/low] The route is created and the id pushed only
        // after subscribe_updates has returned. Two consequences follow. (1) For a Dval
        // that is already live (one held by write_target), subscribe_updates makes the
        // connection task replay the last value into updates_tx on another thread. The
        // pump drops that replay if it reaches the routes lock before route.ids holds
        // this id. (2) drop_dval removes the route when `held` reaches 0, but netidx
        // keeps our updates_tx on a Dval that someone else still holds (the GUI
        // data_table through NetState::subscriber). A later subscribe makes a fresh
        // route with streamed=false, and the connection dedups the channel and skips
        // the replay (already registered + NO_SPURIOUS), so the new reader gets no
        // value until the path changes. Fix: push the id under the routes lock before
        // calling subscribe_updates, and replay dv.last() whenever a reader joins a
        // route that netidx already streams, which needs `streamed` to outlive held==0
        // while the Dval does. (x-concurrency-07)
        // 2026-10-13 codex: The suggested per-reader BEGIN_WITH_LAST channel needs an upstream
        // fix first: ../netidx/netidx/src/subscriber/connection.rs::handle_connect_stream
        // pushes its replay into stream_batch, and process_updates_batch broadcasts
        // that replay to every channel on the subscription. New readers would fire
        // existing readers too (contrary to net_second_subscriber), and several
        // channel registrations in one batch can replay duplicates to new readers.
        // Netidx must target the replay at the joining channel in stream order;
        // then sys can remove shared routing/manual last replay. Equality filtering
        // would discard legitimate repeated writes. Socket probes cannot run here;
        // the candidate sys rewrite was discarded. Upstream changes are outside scope.
        // 2026-10-16 claude: Same mechanism as sys-net-02 just below (a reader joining a route
        // netidx already streams gets its replay targeted at it), which Eric readdressed to
        // himself on 2026-10-14; readdressed with it.
        let dv =
            self.handles(ctx)?.subscriber.subscribe_updates(path, [(flags, updates_tx)]);
        let mut routes = self.0.routes.lock();
        let route = routes.subs.entry(dv.id()).or_default();
        // CR claude for claude: [bug/medium] `streamed` only says whether a Dval that
        // NetState handed out is still alive. It does not say whether netidx still has
        // the shared updates channel registered, and that registration lasts as long as
        // any Dval of the path. The rpc client cache in `call_rpc`, the gui
        // data_table's raw `subscriber`, or an embedder sharing the subscriber can each
        // keep one alive. When a subscribe arm sleeps, `drop_dval` removes the route.
        // On wake, the subscribe creates a new route with `streamed = false` and
        // replays nothing, and netidx sends no last value because NO_SPURIOUS sees the
        // channel is already registered. The reader then stays bottom until the
        // publisher's value changes. Fix: give each reader its own channel with plain
        // BEGIN_WITH_LAST so netidx decides when to send the last value (the same root
        // fix as the late-subscriber finding), or always replay `dv.last()` here and
        // drop the duplicate in the pump. probe:
        // design/review-2026-10-12/repro/sys-net-02.gx (sys-net-02)
        // 2026-10-13 codex: The suggested per-reader BEGIN_WITH_LAST channel needs an upstream
        // fix first: ../netidx/netidx/src/subscriber/connection.rs::handle_connect_stream
        // pushes its replay into stream_batch, and process_updates_batch broadcasts
        // that replay to every channel on the subscription. New readers would fire
        // existing readers too (contrary to net_second_subscriber), and several
        // channel registrations in one batch can replay duplicates to new readers.
        // Netidx must target the replay at the joining channel in stream order;
        // then sys can remove shared routing/manual last replay. Equality filtering
        // would discard legitimate repeated writes. Socket probes cannot run here;
        // the candidate sys rewrite was discarded. Upstream changes are outside scope.
        // CR claude for claude: [bug/high] A late second reader of a busy path sees the
        // same value twice, or newer then older values. This replay sends dv.last()
        // through set_var, and then the reader's BindId joins route.ids. The pump reads
        // route.ids when it drains a batch, not when netidx queued it, so updates
        // already waiting on the shared netidx_updates_tx channel (older than or equal
        // to last) also go to the new reader, after the replay. Any uniq, delta or
        // state machine over a subscription goes wrong. Probe:
        // design/review-2026-10-12/repro/sys-net-01.gx, run several times: "b#1=10473
        // b#2=10471" and "b#1=10968 b#2=10968". Fix: let netidx order the replay with
        // the stream. Give each Subscribe its own updates channel, registered with
        // BEGIN_WITH_LAST and without NO_SPURIOUS, and drop the set_var replay and
        // route.streamed. If one shared channel stays, store a join point per reader so
        // the pump skips batches queued before that reader joined. Add a pin with a
        // fast publisher and a late subscriber that asserts strictly increasing values.
        // The existing net_second_subscriber pin uses a static value, so it cannot
        // catch this. (sys-net-01)
        // 2026-10-13 codex: The suggested per-reader BEGIN_WITH_LAST channel needs an upstream
        // fix first: ../netidx/netidx/src/subscriber/connection.rs::handle_connect_stream
        // pushes its replay into stream_batch, and process_updates_batch broadcasts
        // that replay to every channel on the subscription. New readers would fire
        // existing readers too (contrary to net_second_subscriber), and several
        // channel registrations in one batch can replay duplicates to new readers.
        // Netidx must target the replay at the joining channel in stream order;
        // then sys can remove shared routing/manual last replay. Equality filtering
        // would discard legitimate repeated writes. Socket probes cannot run here;
        // the candidate sys rewrite was discarded. Upstream changes are outside scope.
        if route.streamed
            && flags.contains(UpdatesFlags::BEGIN_WITH_LAST)
            && let ev @ NEvent::Update(_) = dv.last()
        {
            ctx.rt.set_var(id, translate(ev));
        }
        route.streamed = true;
        route.held += 1;
        route.ids.push(id);
        Ok(dv)
    }

    /// The path's Dval for writing alone: no updates and no route.
    pub(crate) fn write_target<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<'_, R, E>,
        path: Path,
    ) -> Result<Dval> {
        let dv = self.handles(ctx)?.subscriber.subscribe(path);
        self.0.routes.lock().subs.entry(dv.id()).or_default().held += 1;
        Ok(dv)
    }

    /// Release a Dval [`Self::write_target`] gave.
    pub(crate) fn release(&self, dv: Dval) {
        self.drop_dval(dv)
    }

    /// Drop a Dval this state handed out: the subscription ends with its
    /// last holder, and its route with it.
    fn drop_dval(&self, dv: Dval) {
        let sub_id = dv.id();
        drop(dv);
        let mut routes = self.0.routes.lock();
        if let Some(route) = routes.subs.get_mut(&sub_id) {
            route.held -= 1;
            if route.held == 0 {
                routes.subs.remove(&sub_id);
            }
        }
    }

    pub(crate) fn unsubscribe(&self, dv: Dval, id: BindId) {
        {
            let mut routes = self.0.routes.lock();
            if let Some(route) = routes.subs.get_mut(&dv.id()) {
                if let Some(i) = route.ids.iter().position(|x| *x == id) {
                    route.ids.remove(i);
                }
            }
        }
        self.drop_dval(dv)
    }

    pub(crate) fn publish<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<'_, R, E>,
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
            let mut batch = self.0.pending_publish_batch.lock();
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
        self.0.flush.notify_one();
    }

    pub(crate) fn unpublish(&self, val: Val) {
        self.0.routes.lock().writes.remove(&val.id());
        drop(val)
    }

    fn spawn_request<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<'_, R, E>,
        id: BindId,
        future: impl Future<Output = Value> + Send + 'static,
    ) {
        let (abort, registration) = AbortHandle::new_pair();
        self.0.requests.lock().insert(id, PendingRequest(abort));
        ctx.rt.spawn_var(async move {
            (id, Abortable::new(future, registration).await.unwrap_or(Value::Null))
        });
    }

    pub(crate) fn stop_request(&self, id: BindId) {
        self.0.requests.lock().remove(&id);
    }

    pub(crate) fn call_rpc<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<'_, R, E>,
        path: Path,
        args: Vec<(arcstr::ArcStr, Value)>,
        id: BindId,
    ) {
        use netidx_protocols::rpc::client::Proc;
        let dbg_path = path.clone();
        let proc = {
            let subscriber = match self.handles(ctx) {
                Ok(h) => h.subscriber.clone(),
                Err(e) => {
                    ctx.rt.set_var(id, error_value(e));
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
                ctx.rt.set_var(id, error_value(e));
            }
            Ok(proc) => {
                if rpc_dbg() {
                    eprintln!("RPCDBG client: calling {dbg_path} for {id:?}");
                }
                self.spawn_request(ctx, id, async move {
                    let r = match proc.call(args).await {
                        Ok(v) => v,
                        Err(e) => error_value(e),
                    };
                    if rpc_dbg() {
                        eprintln!("RPCDBG client: reply from {dbg_path} for {id:?}: {r}");
                    }
                    r
                });
            }
        }
    }

    pub(crate) fn publish_rpc<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<'_, R, E>,
        path: Path,
        doc: Value,
        spec: Vec<netidx_protocols::rpc::server::ArgSpec>,
        id: BindId,
        inbox: Arc<Inbox<netidx_protocols::rpc::server::RpcCall>>,
    ) -> Result<netidx_protocols::rpc::server::Proc> {
        use netidx_protocols::rpc::server::Proc;
        if rpc_dbg() {
            eprintln!("RPCDBG server: publishing {path} for {id:?}");
        }
        let rpcs_tx = self.0.rpcs_tx.clone();
        let publisher = self.handles(ctx)?.publisher.clone();
        let proc = Proc::new(
            &publisher,
            path,
            doc,
            spec,
            move |c| inbox.post(c).then_some(id),
            Some(rpcs_tx),
        )?;
        Ok(proc)
    }

    /// One-shot resolver list/table. The `flushed` barrier is
    /// read-your-writes: this process's own publishes must be
    /// registered with the resolver before the list runs.
    pub(crate) fn list<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<'_, R, E>,
        id: BindId,
        path: Path,
        table: bool,
    ) {
        let (publisher, subscriber) = match self.handles(ctx) {
            Ok(h) => (h.publisher.clone(), h.subscriber.clone()),
            Err(e) => {
                ctx.rt.set_var(id, error_value(e));
                return;
            }
        };
        let ct = {
            let mut cts = self.0.change_trackers.lock();
            let ct =
                cts.entry(id).or_insert_with(|| Arc::new(tokio::sync::Mutex::new(None)));
            Arc::clone(ct)
        };
        self.spawn_request(ctx, id, async move {
            publisher.flushed().await;
            let resolver = subscriber.resolver();
            let mut saved = ct.lock().await;
            let mut tracker = saved
                .take()
                .filter(|t| t.path() == &path)
                .unwrap_or_else(|| ChangeTracker::new(path.clone()));
            match resolver.check_changed(&mut tracker).await {
                Err(e) => return error_value(e),
                Ok(false) => {
                    *saved = Some(tracker);
                    return Value::Null;
                }
                Ok(true) => (),
            }
            let result = if table {
                match resolver.table(path).await {
                    Err(e) => error_value(e),
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
                        #[derive(IntoValue)]
                        struct Table {
                            columns: Value,
                            rows: Value,
                        }
                        Table { columns: cols, rows }.into()
                    }
                }
            } else {
                match resolver.list(path).await {
                    Err(e) => error_value(e),
                    Ok(mut paths) => {
                        use netidx::protocol::valarray::ValArray;
                        let paths = paths.drain(..).map(|p| Value::String(p.into()));
                        Value::Array(ValArray::from_iter_exact(paths))
                    }
                }
            };
            if !matches!(result, Value::Error(_)) {
                *saved = Some(tracker);
            }
            result
        });
    }

    pub(crate) fn stop_list(&self, id: BindId) {
        self.stop_request(id);
        self.0.change_trackers.lock().remove(&id);
    }

    /// The raw subscriber for embedder-side consumers (the gui
    /// data_table). Materializes on first call.
    pub fn subscriber<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<'_, R, E>,
    ) -> Result<Subscriber> {
        Ok(self.handles(ctx)?.subscriber.clone())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use graphix_rt::{GXRt, NoExt};

    #[test]
    fn error_values_have_stable_context_text() {
        let error = anyhow!("inner").context("outer");
        assert_eq!(error_value(error), Value::error(literal!("outer: inner")));
    }

    #[tokio::test]
    async fn dropping_state_stops_idle_flusher() {
        let mut ctx = GXRt::<NoExt>::new_state().unwrap();
        let state = NetState::get(&mut ctx.view());
        let pending = Arc::downgrade(&state.0.pending_publish_batch);
        drop(state);
        drop(ctx);
        time::timeout(Duration::from_secs(1), async {
            while pending.upgrade().is_some() {
                task::yield_now().await;
            }
        })
        .await
        .expect("the flusher outlived its NetState");
    }

    #[tokio::test]
    async fn obsolete_requests_release_their_futures() {
        for replace in [false, true] {
            let mut ctx = GXRt::<NoExt>::new_state().unwrap();
            let state = NetState::get(&mut ctx.view());
            let id = BindId::new();
            let resource = Arc::new(());
            let held = Arc::downgrade(&resource);
            state.spawn_request(&mut ctx.view(), id, async move {
                let _resource = resource;
                std::future::pending::<Value>().await
            });
            if replace {
                state.spawn_request(&mut ctx.view(), id, std::future::ready(Value::Null));
            } else {
                state.stop_request(id);
            }
            time::timeout(Duration::from_secs(1), async {
                while held.upgrade().is_some() {
                    task::yield_now().await;
                }
            })
            .await
            .expect("obsolete request retained its resource");
        }
    }
}
