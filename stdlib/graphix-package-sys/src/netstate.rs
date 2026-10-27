//! Package-owned netidx state for the `sys::net` builtins: the subscriber and
//! publisher, delivered into the graph through `Rt::watch_var`, `Rt::watch` +
//! [`graphix_compiler::CustomBuiltinType`] and `Rt::spawn_var`. Lives in
//! `ctx.libstate`; materialized on first use from the embedder-seeded
//! [`NetConfig`] (absent: a process-internal netidx on a dedicated side thread).
use anyhow::{Error, Result, anyhow};
use arcstr::literal;
use compact_str::format_compact;
use futures::{
    FutureExt, StreamExt,
    channel::mpsc,
    future::{AbortHandle, Abortable},
    stream::{self, SelectAll},
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

/// How long a released write target that has not connected yet is kept
/// for the writes queued on it.
const QUEUED_WRITE_GRACE: Duration = Duration::from_secs(10);

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

type Updates = GPooled<Vec<(SubId, NEvent)>>;

/// A reader's updates channel, as the pump drains it.
type ReaderRx = Abortable<mpsc::Receiver<Updates>>;

/// One subscribe's reader: the subscription, and its own updates channel,
/// which netidx starts with the path's last value in order with the
/// stream. The pump drains it until the reader drops.
#[derive(Debug)]
pub(crate) struct Reader {
    _dv: Dval,
    stop: AbortHandle,
}

impl Drop for Reader {
    fn drop(&mut self) {
        self.stop.abort()
    }
}

struct PendingRequest(AbortHandle);

impl Drop for PendingRequest {
    fn drop(&mut self) {
        self.0.abort();
    }
}

struct Inner {
    handles: NetHandles,
    /// The publish builtin's write BindId of each published Id.
    writes: Arc<Mutex<IntMap<Id, BindId>>>,
    readers_tx: mpsc::UnboundedSender<(BindId, ReaderRx)>,
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
        let (readers_tx, mut readers_rx) = mpsc::unbounded();
        let (writes_tx, mut writes_rx) = mpsc::channel(100);
        let (rpcs_tx, mut rpcs_rx) = mpsc::channel(100);
        let writes: Arc<Mutex<IntMap<Id, BindId>>> = Arc::default();
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
            writes: writes.clone(),
            readers_tx,
            writes_tx,
            rpcs_tx,
            pending_publish_batch,
            flush,
            flusher,
            rpc_clients: Mutex::new(Vec::new()),
            requests: Mutex::new(IntMap::default()),
            change_trackers: Mutex::new(IntMap::default()),
        }));
        // PUMP: netidx events -> graph events. Each reader's updates go to
        // its BindId; writes route the published Id -> the publish
        // builtin's write BindId.
        {
            let writes = writes.clone();
            task::spawn(async move {
                let mut readers = SelectAll::new();
                loop {
                    tokio::select! {
                        reader = readers_rx.next() => match reader {
                            None => break,
                            Some((id, rx)) => readers.push(
                                rx.map(move |batch: Updates| (id, batch))
                            ),
                        },
                        Some((id, mut batch)) = readers.next(), if !readers.is_empty() => {
                            let mut out = VBATCH.take();
                            out.extend(batch.drain(..).map(|(_, ev)| (id, translate(ev))));
                            // the readers of one update share its cycle where
                            // their batches are already here
                            for _ in 0..readers.len() {
                                match readers.next().now_or_never() {
                                    Some(Some((id, mut batch))) => out.extend(
                                        batch.drain(..).map(|(_, ev)| (id, translate(ev)))
                                    ),
                                    Some(None) | None => break,
                                }
                            }
                            if !out.is_empty() {
                                use futures::SinkExt;
                                if var_tx.send(out).await.is_err() {
                                    break;
                                }
                            }
                        },
                        batch = writes_rx.next() => match batch {
                            None => break,
                            Some(mut batch) => {
                                let mut out = CBATCH.take();
                                {
                                    let writes = writes.lock();
                                    for req in batch.drain(..) {
                                        if let Some(id) = writes.get(&req.id) {
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

    /// Subscribe `path` for the reader `id`, on a channel of its own.
    // XCR claude for claude: [risk/low] The route is created and the id pushed only
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
    // 2026-10-17 claude: Point (2) is fixed with sys-net-02: Routes::register remembers,
    // per SubId and while the subscription lives (a DvalWeak), that the updates channel is
    // registered, so a reader subscribing after its route was removed gets the replay. Pin
    // lib_tests::net::net_rewoken_reader_of_a_held_path. Point (1), the replay racing the
    // route's creation, stands.
    // 2026-10-24 claude: Eric ruled (2026-10-24): fix the replay ordering upstream in
    // netidx (per-reader BEGIN_WITH_LAST targeted at the joining channel in stream order),
    // then simplify sys's routing.
    // 2026-10-27 claude: Point (1), the pump dropping a replay that beat the route's
    // creation, is gone with sys-net-01's fix: there is no route, each reader's channel
    // carries its own replay and the pump learns the channel before it can lose anything
    // on it (the channel buffers). Point (2) stays fixed without Routes::register: a
    // reader joining a subscription another holder keeps gets its own channel's replay
    // from netidx. Pins: lib_tests::net::net_rewoken_reader_of_a_held_path,
    // net_second_subscriber, net_late_reader_of_a_busy_path.
    // XCR claude for claude: [bug/high] A late second reader of a busy path sees the
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
    // 2026-10-24 claude: Eric ruled (2026-10-24): fix the replay ordering upstream in
    // netidx (per-reader BEGIN_WITH_LAST targeted at the joining channel in stream order),
    // then simplify sys's routing.
    // 2026-10-25 claude: The load flake in lib_tests::net::net_subscribe_arm_rewake is
    // this mechanism. Driving that fixture by its deliveries instead of the 150 ms timer
    // (x <- got ~ x + 1) makes each wake land before netidx has delivered the update the
    // arm slept through, and in the test harness about one run in four per mode fails: [0,
    // -1, 0, -1, 4, -1] and [0, -1, 2, -1, 2, -1] (the woken subscribe joined a
    // subscription netidx still held and replayed its pre-sleep dv.last()), and once
    // interp hung with no delivery after a wake (the replay lost, probably point (1) of
    // x-concurrency-07, unconfirmed). The timer usually gives netidx time to catch up;
    // under load it does not. The fixture keeps the timer until the upstream fix lands;
    // then switch it to delivery-driven, which pins this. Probe:
    // design/review-2026-10-12/repro/sys-net-01-rewake.gx (clean in 6 shell runs, the
    // harness reproduces it).
    // 2026-10-27 claude: Built per Eric's 10-24 ruling. Upstream (netidx connection.rs
    // handle_connect_stream) a joining channel's last-value replay now goes to that
    // channel alone, queued in by_chan at the point it joins, so it follows every update
    // sent before the join and precedes every update after (it was pushed into a batch
    // broadcast to every channel on the subscription, after the batch's updates). Here
    // each subscribe owns its updates channel, registered with BEGIN_WITH_LAST and no
    // NO_SPURIOUS, and the pump drains every reader's channel (a SelectAll; a reader
    // dropping aborts its stream). The set_var replay, Routes' subs/held/registered,
    // route.streamed and drop_dval are gone; Subscribe holds a Reader. One change to
    // review: two readers of one path now each get their own copy of an update; the pump
    // puts the copies that have already arrived into one batch, but one that arrives later
    // lands a cycle later. Pins: netidx
    // test::publisher::a_joining_channel_gets_its_replay_alone_in_order (reader out of
    // order on the old connection.rs, 3 of 3 runs);
    // lib_tests::net::net_late_reader_of_a_busy_path (late reader of a 30000-update path
    // sees each value newer than the last; fails in 3-4 of 4 modes on every one of 12 runs
    // of the old code). The 10-25 note's plan to drive net_subscribe_arm_rewake by its
    // deliveries does not hold: it then fails as [0, -1, 0, -1, 0, -1] on the fixed code,
    // because each On window ends at its first delivery, and that delivery is netidx's
    // last-known value of the path while the publisher's newer one is still in flight.
    // That is publish latency, not this mechanism, so the fixture keeps its timer.
    pub(crate) fn subscribe<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<'_, R, E>,
        flags: UpdatesFlags,
        path: Path,
        id: BindId,
    ) -> Result<Reader> {
        let (tx, rx) = mpsc::channel(100);
        let (rx, stop) = stream::abortable(rx);
        let dv = self.handles(ctx)?.subscriber.subscribe_updates(path, [(flags, tx)]);
        let _ = self.0.readers_tx.unbounded_send((id, rx));
        Ok(Reader { _dv: dv, stop })
    }

    /// The path's Dval for writing alone: no updates.
    pub(crate) fn write_target<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<'_, R, E>,
        path: Path,
    ) -> Result<Dval> {
        Ok(self.handles(ctx)?.subscriber.subscribe(path))
    }

    /// Release a Dval [`Self::write_target`] gave. One holding writes it
    /// queued while connecting is kept until it connects, which sends them,
    /// or for [`QUEUED_WRITE_GRACE`], where a runtime is there to wait.
    pub(crate) fn release(&self, dv: Dval) {
        if dv.queued_writes() > 0
            && let Ok(rt) = tokio::runtime::Handle::try_current()
        {
            rt.spawn(async move {
                let _ = time::timeout(QUEUED_WRITE_GRACE, dv.wait_subscribed()).await;
            });
        }
    }

    pub(crate) fn publish<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<'_, R, E>,
        path: Path,
        value: Value,
        write_id: Option<BindId>,
    ) -> Result<Val> {
        let writes_tx = write_id.map(|_| self.0.writes_tx.clone());
        let val = self.handles(ctx)?.publisher.publish_with_flags_and_writes(
            PublishFlags::empty(),
            path,
            value,
            writes_tx,
        )?;
        if let Some(id) = write_id {
            self.0.writes.lock().insert(val.id(), id);
        }
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
        self.0.writes.lock().remove(&val.id());
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
                        #[value(shape = "graphix_compiler::rep::GraphixShape")]
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
