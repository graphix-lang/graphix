use anyhow::Result;
use arcstr::{literal, ArcStr};
use enumflags2::BitFlags;
use extended_notify::{
    ArcPath, Event as NEvent, EventBatch, EventHandler, EventKind, Id, Interest, Watcher,
    WatcherConfigBuilder,
};
use futures::{channel::mpsc, SinkExt, TryFutureExt};
use fxhash::{FxHashMap, FxHashSet};
use graphix_compiler::{
    errf, expr::ExprId, typ::FnType, Apply, BindId, BuiltIn, CustomBuiltinType, Event,
    ExecCtx, Node, Rt, Scope, UserEvent, CBATCH_POOL,
};
use graphix_package_core::CachedVals;
use netidx_value::{
    abstract_type::AbstractWrapper, Abstract, FromValue, ValArray, Value,
};
use parking_lot::Mutex;
use poolshark::{global::GPooled, local::LPooled};
use std::{
    any::Any,
    cmp::Ordering,
    hash::{Hash, Hasher},
    ops::Deref,
    sync::{Arc, LazyLock},
    time::Duration,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
struct WInterest(Interest);

impl Deref for WInterest {
    type Target = Interest;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

macro_rules! impl_value_conv {
    ($enum:ident { $($variant:ident),* $(,)? }) => {
        impl FromValue for $enum {
            fn from_value(v: Value) -> anyhow::Result<Self> {
                match v {
                    Value::String(s) => match &*s {
                        $(stringify!($variant) => Ok(Self(Interest::$variant)),)*
                        _ => Err(anyhow::anyhow!("Invalid {} variant: {}", stringify!($enum), s)),
                    },
                    _ => Err(anyhow::anyhow!("Expected string value for {}, got: {:?}", stringify!($enum), v)),
                }
            }
        }

        impl Into<Value> for $enum {
            fn into(self) -> Value {
                match *self {
                    $(Interest::$variant => Value::String(literal!(stringify!($variant))),)*
                }
            }
        }
    };
}

impl_value_conv!(WInterest {
    Established,
    Any,
    Access,
    AccessOpen,
    AccessClose,
    AccessRead,
    AccessOther,
    Create,
    CreateFile,
    CreateFolder,
    CreateOther,
    Modify,
    ModifyData,
    ModifyDataSize,
    ModifyDataContent,
    ModifyDataOther,
    ModifyMetadata,
    ModifyMetadataAccessTime,
    ModifyMetadataWriteTime,
    ModifyMetadataPermissions,
    ModifyMetadataOwnership,
    ModifyMetadataExtended,
    ModifyMetadataOther,
    ModifyRename,
    ModifyRenameTo,
    ModifyRenameFrom,
    ModifyRenameBoth,
    ModifyRenameOther,
    ModifyOther,
    Delete,
    DeleteFile,
    DeleteFolder,
    DeleteOther,
    Other,
});

#[derive(Debug)]
struct WEvent(NEvent);

impl CustomBuiltinType for WEvent {}

#[derive(Debug, Clone)]
struct NotifyChan {
    tx: mpsc::Sender<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
    idmap: Arc<Mutex<FxHashMap<Id, BindId>>>,
}

impl EventHandler for NotifyChan {
    fn handle_event(
        &mut self,
        mut event: EventBatch,
    ) -> impl Future<Output = Result<()>> + Send {
        let mut batch = CBATCH_POOL.take();
        let idmap = self.idmap.lock();
        for (id, ev) in event.drain(..) {
            if let Some(id) = idmap.get(&id) {
                let wb: Box<dyn CustomBuiltinType> = Box::new(WEvent(ev));
                batch.push((*id, wb));
            }
        }
        drop(idmap);
        self.tx.send(batch).map_err(anyhow::Error::from)
    }
}

#[derive(Debug)]
struct Watched {
    w: extended_notify::Watched,
    idmap: Arc<Mutex<FxHashMap<Id, BindId>>>,
}

impl Drop for Watched {
    fn drop(&mut self) {
        self.idmap.lock().remove(&self.w.id());
    }
}

fn utf8_path(p: ArcPath) -> Value {
    Value::String(arcstr::format!("{}", p.display()))
}

// ── Abstract types ───────────────────────────────────────────────

#[derive(Debug, Clone)]
struct WatcherValue {
    watcher: Watcher,
    idmap: Arc<Mutex<FxHashMap<Id, BindId>>>,
}

impl PartialEq for WatcherValue {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.idmap, &other.idmap)
    }
}

impl Eq for WatcherValue {}

impl PartialOrd for WatcherValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WatcherValue {
    fn cmp(&self, other: &Self) -> Ordering {
        Arc::as_ptr(&self.idmap).cmp(&Arc::as_ptr(&other.idmap))
    }
}

impl Hash for WatcherValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.idmap).hash(state)
    }
}

graphix_package_core::impl_no_pack!(WatcherValue);

impl WatcherValue {
    fn add(
        &self,
        id: BindId,
        path: &str,
        interest: BitFlags<Interest>,
    ) -> Result<Watched> {
        let w = self.watcher.add(path.into(), interest)?;
        self.idmap.lock().insert(w.id(), id);
        Ok(Watched { w, idmap: Arc::clone(&self.idmap) })
    }
}

static WATCHER_WRAPPER: LazyLock<AbstractWrapper<WatcherValue>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0xb2, 0xc3, 0xd4, 0xe5, 0xf6, 0xa1, 0x47, 0x89, 0x9a, 0xbc, 0xde, 0xf0, 0x12,
        0x34, 0x56, 0x79,
    ]);
    Abstract::register::<WatcherValue>(id).expect("failed to register WatcherValue")
});

#[derive(Debug, Clone)]
struct WatchValue {
    _watched: Arc<Watched>,
    bind_id: BindId,
}

impl PartialEq for WatchValue {
    fn eq(&self, other: &Self) -> bool {
        self.bind_id == other.bind_id
    }
}

impl Eq for WatchValue {}

impl PartialOrd for WatchValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WatchValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.bind_id.cmp(&other.bind_id)
    }
}

impl Hash for WatchValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.bind_id.hash(state)
    }
}

graphix_package_core::impl_no_pack!(WatchValue);

static WATCH_VALUE_WRAPPER: LazyLock<AbstractWrapper<WatchValue>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0xc3, 0xd4, 0xe5, 0xf6, 0xa1, 0xb2, 0x47, 0x89, 0x9a, 0xbc, 0xde, 0xf0, 0x12,
        0x34, 0x56, 0x7a,
    ]);
    Abstract::register::<WatchValue>(id).expect("failed to register WatchValue")
});

// ── CreateWatcher ────────────────────────────────────────────────

#[derive(Debug)]
pub(crate) struct CreateWatcher {
    poll_interval: Option<Duration>,
    batch_size: Option<i64>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for CreateWatcher {
    const NAME: &str = "sys_watch_create";
    const NEEDS_CALLSITE: bool = false;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _fntyp: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _args: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(CreateWatcher { poll_interval: None, batch_size: None }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for CreateWatcher {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let poll_interval = from[0]
            .update(ctx, event)
            .and_then(|v| v.cast_to::<Option<Duration>>().ok().flatten());
        let batch_size = from[1]
            .update(ctx, event)
            .and_then(|v| v.cast_to::<Option<i64>>().ok().flatten());
        let trigger = from[2].update(ctx, event);
        match poll_interval {
            Some(poll_interval) if poll_interval < Duration::from_millis(100) => {
                return Some(errf!("WatchError", "poll_interval must be >= 100ms"))
            }
            Some(poll_interval) => self.poll_interval = Some(poll_interval),
            None => (),
        }
        match batch_size {
            Some(batch_size) if batch_size < 0 => {
                return Some(errf!("WatchError", "batch_size must be >= 0"))
            }
            Some(batch_size) => self.batch_size = Some(batch_size),
            None => (),
        }
        if trigger.is_some() {
            let idmap = Arc::new(Mutex::new(FxHashMap::default()));
            let (notify_tx, notify_rx) = mpsc::channel(10);
            let notify_tx = NotifyChan { tx: notify_tx, idmap: idmap.clone() };
            let mut builder = WatcherConfigBuilder::default();
            if let Some(pi) = &self.poll_interval {
                builder.poll_interval(*pi);
            }
            if let Some(bs) = &self.batch_size {
                builder.poll_batch(*bs as usize);
            }
            let watcher_result = builder
                .event_handler(notify_tx)
                .build()
                .map_err(|e| anyhow::anyhow!("{e:?}"))
                .and_then(|c| c.start());
            match watcher_result {
                Ok(watcher) => {
                    ctx.rt.watch(notify_rx);
                    Some(WATCHER_WRAPPER.wrap(WatcherValue { watcher, idmap }))
                }
                Err(e) => Some(errf!("WatchError", "failed to create watcher: {e:?}")),
            }
        } else {
            None
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

// ── WatchApply ───────────────────────────────────────────────────

#[derive(Debug)]
pub(crate) struct WatchApply {
    interest: Option<BitFlags<Interest>>,
    path: Option<ArcStr>,
    watcher_val: Option<Value>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for WatchApply {
    const NAME: &str = "sys_watch_watch";
    const NEEDS_CALLSITE: bool = false;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(WatchApply { interest: None, path: None, watcher_val: None }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for WatchApply {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let mut up = false;
        if let Some(Ok(mut int)) =
            from[0].update(ctx, event).map(|v| v.cast_to::<LPooled<Vec<WInterest>>>())
        {
            let int = int.drain(..).fold(BitFlags::empty(), |mut acc, fl| {
                acc.insert(fl.0);
                acc
            });
            up = true;
            self.interest = Some(int);
        }
        if let Some(watcher_val) = from[1].update(ctx, event) {
            up = true;
            self.watcher_val = Some(watcher_val);
        }
        if let Some(Ok(path)) = from[2].update(ctx, event).map(|v| v.cast_to::<ArcStr>())
        {
            up = true;
            self.path = Some(path);
        }
        if up
            && let Some(path) = &self.path
            && let Some(interest) = self.interest
            && let Some(Value::Abstract(ref a)) = self.watcher_val
        {
            if let Some(wv) = a.downcast_ref::<WatcherValue>() {
                let bind_id = BindId::new();
                match wv.add(bind_id, path, interest) {
                    Ok(watched) => {
                        return Some(
                            WATCH_VALUE_WRAPPER.wrap(WatchValue {
                                _watched: Arc::new(watched),
                                bind_id,
                            }),
                        );
                    }
                    Err(e) => {
                        return Some(errf!("WatchError", "{e:?}"));
                    }
                }
            }
        }
        None
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.interest = None;
        self.path = None;
        self.watcher_val = None;
    }
}

// ── Shared helpers for accessor functions ────────────────────────

fn extract_bind_ids(v: &Value, out: &mut FxHashSet<BindId>) {
    match v {
        Value::Abstract(a) => {
            if let Some(wv) = a.downcast_ref::<WatchValue>() {
                out.insert(wv.bind_id);
            }
        }
        Value::Array(arr) => {
            for elem in arr.iter() {
                if let Value::Abstract(a) = elem {
                    if let Some(wv) = a.downcast_ref::<WatchValue>() {
                        out.insert(wv.bind_id);
                    }
                }
            }
        }
        Value::Map(m) => {
            for (_, val) in m.clone().into_iter() {
                if let Value::Abstract(a) = &val {
                    if let Some(wv) = a.downcast_ref::<WatchValue>() {
                        out.insert(wv.bind_id);
                    }
                }
            }
        }
        _ => (),
    }
}

fn scan_watch_events<E: UserEvent>(
    bind_ids: &FxHashSet<BindId>,
    event: &mut Event<E>,
    convert: fn(&mut WEvent) -> Value,
) -> Option<Value> {
    for bid in bind_ids {
        if let Some(mut cbt) = event.custom.remove(bid) {
            if let Some(w) = (&mut *cbt as &mut dyn Any).downcast_mut::<WEvent>() {
                if let EventKind::Error(e) = &w.0.event {
                    return Some(errf!("WatchError", "{e:?}"));
                }
                return Some(convert(w));
            }
        }
    }
    None
}

fn convert_path(w: &mut WEvent) -> Value {
    w.0.paths.drain().next().map(utf8_path).unwrap_or(Value::Null)
}

fn convert_events(w: &mut WEvent) -> Value {
    let event: Value = match &w.0.event {
        EventKind::Event(int) => WInterest(*int).into(),
        EventKind::Error(_) => unreachable!(),
    };
    let paths = ValArray::from_iter_exact(w.0.paths.drain().map(utf8_path));
    ((literal!("event"), event), (literal!("paths"), Value::Array(paths))).into()
}

// ── WatchPath accessor ──────────────────────────────────────────

#[derive(Debug)]
pub(crate) struct WatchPath {
    top_id: ExprId,
    cached: CachedVals,
    bind_ids: FxHashSet<BindId>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for WatchPath {
    const NAME: &str = "sys_watch_path";
    const NEEDS_CALLSITE: bool = false;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(WatchPath {
            top_id,
            cached: CachedVals::new(from),
            bind_ids: FxHashSet::default(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for WatchPath {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if self.cached.update(ctx, from, event) {
            for bid in self.bind_ids.drain() {
                ctx.rt.unref_var(bid, self.top_id);
            }
            for v in self.cached.0.iter() {
                if let Some(v) = v {
                    extract_bind_ids(v, &mut self.bind_ids);
                }
            }
            for bid in &self.bind_ids {
                ctx.rt.ref_var(*bid, self.top_id);
            }
        }
        scan_watch_events(&self.bind_ids, event, convert_path)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        for bid in self.bind_ids.drain() {
            ctx.rt.unref_var(bid, self.top_id);
        }
        self.cached.clear();
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        for bid in &self.bind_ids {
            ctx.rt.unref_var(*bid, self.top_id);
        }
    }
}

// ── WatchEvents accessor ────────────────────────────────────────

#[derive(Debug)]
pub(crate) struct WatchEvents {
    top_id: ExprId,
    cached: CachedVals,
    bind_ids: FxHashSet<BindId>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for WatchEvents {
    const NAME: &str = "sys_watch_events";
    const NEEDS_CALLSITE: bool = false;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(WatchEvents {
            top_id,
            cached: CachedVals::new(from),
            bind_ids: FxHashSet::default(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for WatchEvents {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if self.cached.update(ctx, from, event) {
            for bid in self.bind_ids.drain() {
                ctx.rt.unref_var(bid, self.top_id);
            }
            for v in self.cached.0.iter() {
                if let Some(v) = v {
                    extract_bind_ids(v, &mut self.bind_ids);
                }
            }
            for bid in &self.bind_ids {
                ctx.rt.ref_var(*bid, self.top_id);
            }
        }
        scan_watch_events(&self.bind_ids, event, convert_events)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        for bid in self.bind_ids.drain() {
            ctx.rt.unref_var(bid, self.top_id);
        }
        self.cached.clear();
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        for bid in &self.bind_ids {
            ctx.rt.unref_var(*bid, self.top_id);
        }
    }
}
