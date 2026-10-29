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
use graphix_compiler::{BindId, CustomBuiltinType, ExecCtx, Rt, branch::RtView};
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
use serde_derive::{Deserialize, Serialize};
use std::{
    sync::{
        Arc, LazyLock, OnceLock,
        atomic::{AtomicU64, Ordering},
    },
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

/// What `sys::net::call_id` mints and a call is known by, unique in the
/// process. Its `Pack` refuses, so no peer can send one and it prints as
/// `abstract:`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct RpcId(u64);

impl nohash::IsEnabled for RpcId {}

impl RpcId {
    pub(crate) fn new() -> Self {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        Self(NEXT.fetch_add(1, Ordering::Relaxed))
    }
}

graphix_package_core::impl_no_pack!(RpcId);

netidx_core::atomic_id!(CallSerial);

/// A call in flight: the site it answers, and its request, which dropping
/// aborts.
struct InFlight {
    serial: CallSerial,
    site: BindId,
    _request: PendingRequest,
}

/// A call's one answer, delivered to its site: the server's reply, or
/// nothing where the request was aborted first.
#[derive(Debug)]
pub(crate) struct RpcAnswer {
    pub(crate) id: RpcId,
    serial: CallSerial,
    reply: Option<Value>,
}

impl CustomBuiltinType for RpcAnswer {}

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
    rpc_calls: Mutex<IntMap<RpcId, InFlight>>,
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
    pub fn get<R: Rt>(ctx: &mut ExecCtx<'_, R>) -> NetState {
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
            rpc_calls: Mutex::new(IntMap::default()),
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
    fn handles<R: Rt>(&self, ctx: &mut ExecCtx<'_, R>) -> Result<&Handles> {
        if let Some(handles) = self.0.handles.get() {
            return Ok(handles);
        }
        let cfg = ctx.libstate.get::<NetConfig>().unwrap_or(NetConfig::Internal);
        self.0.handles.get_or_materialize(cfg)
    }

    /// Subscribe `path` for the reader `id`, on a channel of its own.
    pub(crate) fn subscribe<R: Rt>(
        &self,
        ctx: &mut ExecCtx<'_, R>,
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
    pub(crate) fn write_target<R: Rt>(
        &self,
        ctx: &mut ExecCtx<'_, R>,
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

    pub(crate) fn publish<R: Rt>(
        &self,
        ctx: &mut ExecCtx<'_, R>,
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

    fn spawn_request<R: Rt>(
        &self,
        ctx: &mut ExecCtx<'_, R>,
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

    /// The client of the procedure at `path`, kept a minute past its last
    /// use.
    fn rpc_client<R: Rt>(
        &self,
        ctx: &mut ExecCtx<'_, R>,
        path: &Path,
    ) -> Result<netidx_protocols::rpc::client::Proc> {
        use netidx_protocols::rpc::client::Proc;
        let subscriber = self.handles(ctx)?.subscriber.clone();
        let now = time::Instant::now();
        let mut clients = self.0.rpc_clients.lock();
        clients.retain(|(_, _, last)| {
            now.saturating_duration_since(*last) < Duration::from_secs(60)
        });
        match clients.iter_mut().find(|(p, _, _)| p == path) {
            Some((_, proc, last)) => {
                *last = now;
                Ok(proc.clone())
            }
            None => {
                let proc = Proc::new(&subscriber, path.clone())?;
                clients.push((path.clone(), proc.clone(), now));
                Ok(proc)
            }
        }
    }

    /// Call the procedure `request` names as `id`, answering `site`: a
    /// call in flight under `id` is replaced, and answers aborted.
    pub(crate) fn call_rpc<R: Rt>(
        &self,
        ctx: &mut ExecCtx<'_, R>,
        site: BindId,
        id: RpcId,
        request: Result<(Path, Vec<(arcstr::ArcStr, Value)>)>,
    ) {
        let call = request
            .and_then(|(path, args)| Ok((self.rpc_client(ctx, &path)?, path, args)));
        match call {
            Err(e) => self.start_call(ctx, site, id, std::future::ready(error_value(e))),
            Ok((proc, path, args)) => {
                if rpc_dbg() {
                    eprintln!("RPCDBG client: calling {path} as {id:?}");
                }
                self.start_call(ctx, site, id, async move {
                    let r = match proc.call(args).await {
                        Ok(v) => v,
                        Err(e) => error_value(e),
                    };
                    if rpc_dbg() {
                        eprintln!("RPCDBG client: reply from {path} to {id:?}: {r}");
                    }
                    r
                })
            }
        }
    }

    fn start_call<R: Rt>(
        &self,
        ctx: &mut ExecCtx<'_, R>,
        site: BindId,
        id: RpcId,
        reply: impl Future<Output = Value> + Send + 'static,
    ) {
        let serial = CallSerial::new();
        let (abort, registration) = AbortHandle::new_pair();
        let call = InFlight { serial, site, _request: PendingRequest(abort) };
        self.0.rpc_calls.lock().insert(id, call);
        ctx.rt.spawn(async move {
            let reply = Abortable::new(reply, registration).await.ok();
            let answer = RpcAnswer { id, serial, reply };
            (site, Box::new(answer) as Box<dyn CustomBuiltinType>)
        });
    }

    /// The reply `answer` carries when it answers the call in flight under
    /// its id, which is in flight no more; `None` when that call was
    /// aborted or replaced.
    pub(crate) fn settle_call(&self, answer: RpcAnswer) -> Option<Value> {
        let mut calls = self.0.rpc_calls.lock();
        match calls.get(&answer.id) {
            Some(call) if call.serial == answer.serial => {
                calls.remove(&answer.id);
                answer.reply
            }
            Some(_) | None => None,
        }
    }

    /// Abort the call in flight under `id`, if there is one: it answers
    /// aborted.
    pub(crate) fn abort_call(&self, id: RpcId) {
        self.0.rpc_calls.lock().remove(&id);
    }

    /// Abort every call `site` has in flight. Their answers still come to
    /// `site`'s delivery id, which its caller remints or releases.
    pub(crate) fn abandon_calls(&self, site: BindId) {
        self.0.rpc_calls.lock().retain(|_, call| call.site != site);
    }

    pub(crate) fn publish_rpc<R: Rt>(
        &self,
        ctx: &mut ExecCtx<'_, R>,
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
    pub(crate) fn list<R: Rt>(
        &self,
        ctx: &mut ExecCtx<'_, R>,
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
    pub fn subscriber<R: Rt>(&self, ctx: &mut ExecCtx<'_, R>) -> Result<Subscriber> {
        Ok(self.handles(ctx)?.subscriber.clone())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use graphix_compiler::ExecState;
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

    /// A reply settles its call only while that call is in flight under
    /// its id: one replaced or aborted after its reply was sent answers
    /// aborted, an abort of an id not in flight does nothing, and a site
    /// abandons only its own calls.
    #[tokio::test]
    async fn an_answer_settles_only_its_own_call() {
        let mut ctx = GXRt::<NoExt>::new_state().unwrap();
        let state = NetState::get(&mut ctx.view());
        let (site, other) = (BindId::new(), BindId::new());
        let id = RpcId::new();
        let call = |ctx: &mut ExecState<GXRt<NoExt>>, site, id| {
            state.start_call(&mut ctx.view(), site, id, std::future::pending());
            state.0.rpc_calls.lock()[&id].serial
        };
        let answer = |id, serial, v| RpcAnswer { id, serial, reply: Some(Value::I64(v)) };
        let replaced = call(&mut ctx, site, id);
        let current = call(&mut ctx, site, id);
        assert_eq!(state.settle_call(answer(id, replaced, 1)), None);
        assert_eq!(state.settle_call(answer(id, current, 2)), Some(Value::I64(2)));
        assert_eq!(state.settle_call(answer(id, current, 2)), None);
        let aborted = call(&mut ctx, site, id);
        state.abort_call(id);
        state.abort_call(id);
        assert_eq!(state.settle_call(answer(id, aborted, 3)), None);
        let kept = RpcId::new();
        let mine = call(&mut ctx, site, id);
        let theirs = call(&mut ctx, other, kept);
        state.abandon_calls(site);
        assert_eq!(state.settle_call(answer(id, mine, 4)), None);
        assert_eq!(state.settle_call(answer(kept, theirs, 5)), Some(Value::I64(5)));
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
