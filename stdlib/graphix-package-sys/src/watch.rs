use ahash::AHashMap;
use anyhow::Result;
use arcstr::{ArcStr, literal};
use enumflags2::BitFlags;
use extended_notify::{
    ArcPath, Event as NEvent, EventBatch, EventHandler, EventKind, Id, Interest, Watcher,
    WatcherConfigBuilder,
};
use futures::{SinkExt, TryFutureExt, channel::mpsc};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CBATCH_POOL, CompileCtx, CustomBuiltinType, ExecCtx, Node,
    Rt, Scope, TagValue, UserEvent,
    effects::Effect,
    errf,
    expr::ExprId,
    image::{self, ImageBuf},
    typ::FnType,
};
use graphix_package_core::{CachedVals, seam_tick, seam_value};
use netidx_core::pack::{Pack, PackError};
use netidx_derive::IntoValue;
use netidx_value::{FromValue, ValArray, Value};
use nohash::IntSet;
use parking_lot::Mutex;
use poolshark::{global::GPooled, local::LPooled};
use std::{
    any::Any,
    cmp::Ordering,
    fmt::Debug,
    hash::{Hash, Hasher},
    marker::PhantomData,
    ops::Deref,
    sync::Arc,
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
pub(crate) struct WEvent(NEvent);

impl CustomBuiltinType for WEvent {}

#[derive(Debug, Clone)]
struct NotifyChan {
    tx: mpsc::Sender<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
    idmap: Arc<Mutex<AHashMap<Id, BindId>>>,
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
    idmap: Arc<Mutex<AHashMap<Id, BindId>>>,
}

impl Drop for Watched {
    fn drop(&mut self) {
        self.idmap.lock().remove(&self.w.id());
    }
}

fn utf8_path(p: ArcPath) -> Value {
    Value::String(arcstr::format!("{}", p.display()))
}

#[derive(Debug, Clone)]
struct WatcherValue {
    watcher: Watcher,
    idmap: Arc<Mutex<AHashMap<Id, BindId>>>,
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

graphix_package_core::abstract_wrapper!(
    WatcherValue,
    static WATCHER_WRAPPER = "sys::fs::watch::Watcher"
);

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

graphix_package_core::abstract_wrapper!(
    WatchValue,
    static WATCH_VALUE_WRAPPER = "sys::fs::watch::Watch"
);

#[derive(Debug)]
pub(crate) struct CreateWatcher {
    poll_interval: Option<Duration>,
    batch_size: Option<i64>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for CreateWatcher {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_watch_create";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
        _fntyp: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _args: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(CreateWatcher {
            poll_interval: None,
            batch_size: None,
            out: TagValue::phantom(),
        }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let poll_interval = Pack::decode(buf)?;
        let batch_size = Pack::decode(buf)?;
        Ok(Box::new(CreateWatcher {
            poll_interval,
            batch_size,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for CreateWatcher {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.poll_interval.encode(buf)?;
        self.batch_size.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let poll_interval = seam_value(from[0].update(ctx))
            .and_then(|v| v.value_cloned().cast_to::<Option<Duration>>().ok().flatten());
        let batch_size = seam_value(from[1].update(ctx))
            .and_then(|v| v.value_cloned().cast_to::<Option<i64>>().ok().flatten());
        let trigger = seam_tick(from[2].update(ctx)).is_some();
        match poll_interval {
            Some(poll_interval) if poll_interval < Duration::from_millis(100) => {
                return self.out.set(TagValue::fired(errf!(
                    "WatchError",
                    "poll_interval must be >= 100ms"
                )));
            }
            Some(poll_interval) => self.poll_interval = Some(poll_interval),
            None => (),
        }
        match batch_size {
            Some(batch_size) if batch_size < 0 => {
                return self.out.set(TagValue::fired(errf!(
                    "WatchError",
                    "batch_size must be >= 0"
                )));
            }
            Some(batch_size) => self.batch_size = Some(batch_size),
            None => (),
        }
        if trigger {
            let idmap = Arc::new(Mutex::new(AHashMap::default()));
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
                    self.out.set(TagValue::fired(
                        WATCHER_WRAPPER.wrap(WatcherValue { watcher, idmap }),
                    ))
                }
                Err(e) => self.out.set(TagValue::fired(errf!(
                    "WatchError",
                    "failed to create watcher: {e:?}"
                ))),
            }
        } else {
            self.out.ride()
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}
}

#[derive(Debug)]
pub(crate) struct WatchApply {
    interest: Option<BitFlags<Interest>>,
    path: Option<ArcStr>,
    watcher_val: Option<Value>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for WatchApply {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_watch_watch";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(WatchApply {
            interest: None,
            path: None,
            watcher_val: None,
            out: TagValue::phantom(),
        }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let interest = Pack::decode(buf)?;
        let path = Pack::decode(buf)?;
        Ok(Box::new(WatchApply {
            interest,
            path,
            watcher_val: None,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for WatchApply {
    /// `watcher_val` holds a live OS watcher.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.watcher_val.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.interest.encode(buf)?;
        self.path.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let mut up = false;
        if let Some(Ok(mut int)) = seam_tick(from[0].update(ctx))
            .map(|v| v.value_cloned().cast_to::<LPooled<Vec<WInterest>>>())
        {
            let int = int.drain(..).fold(BitFlags::empty(), |mut acc, fl| {
                acc.insert(fl.0);
                acc
            });
            up = true;
            self.interest = Some(int);
        }
        if let Some(watcher_val) = seam_tick(from[1].update(ctx)) {
            up = true;
            self.watcher_val = Some(watcher_val.value_cloned());
        }
        if let Some(Ok(path)) =
            seam_tick(from[2].update(ctx)).map(|tv| tv.value_cloned().cast_to::<ArcStr>())
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
                        return self.out.set(TagValue::fired(
                            WATCH_VALUE_WRAPPER.wrap(WatchValue {
                                _watched: Arc::new(watched),
                                bind_id,
                            }),
                        ));
                    }
                    Err(e) => {
                        return self
                            .out
                            .set(TagValue::fired(errf!("WatchError", "{e:?}")));
                    }
                }
            }
        }
        self.out.ride()
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.interest = None;
        self.path = None;
        self.watcher_val = None;
        self.out = TagValue::phantom();
    }
}

fn extract_bind_ids(v: &Value, out: &mut IntSet<BindId>) {
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

/// What a watch stream outputs per event.
pub(crate) trait WatchKind: Debug + Send + Sync + 'static {
    const NAME: &str;
    fn convert(w: &mut WEvent) -> Value;
}

#[derive(Debug)]
pub(crate) struct PathKind;

impl WatchKind for PathKind {
    const NAME: &str = "sys_watch_path";

    fn convert(w: &mut WEvent) -> Value {
        w.0.paths.drain().next().map(utf8_path).unwrap_or(Value::Null)
    }
}

#[derive(Debug)]
pub(crate) struct EventsKind;

impl WatchKind for EventsKind {
    const NAME: &str = "sys_watch_events";

    fn convert(w: &mut WEvent) -> Value {
        let event: Value = match &w.0.event {
            EventKind::Event(int) => WInterest(*int).into(),
            EventKind::Error(_) => unreachable!(),
        };
        #[derive(IntoValue)]
        struct Fields {
            event: Value,
            paths: ValArray,
        }
        let paths = ValArray::from_iter_exact(w.0.paths.drain().map(utf8_path));
        Fields { event, paths }.into()
    }
}

pub(crate) type WatchPath = WatchStream<PathKind>;
pub(crate) type WatchEvents = WatchStream<EventsKind>;

/// The events of a set of watches, one per cycle: every event of a cycle
/// is written to the stream's own variable, and the runtime delivers the
/// writes to one variable a cycle apart.
#[derive(Debug)]
pub(crate) struct WatchStream<K: WatchKind> {
    top_id: ExprId,
    cached: CachedVals,
    bind_ids: IntSet<BindId>,
    id: BindId,
    out: TagValue,
    kind: PhantomData<K>,
}

impl<R: Rt, E: UserEvent, K: WatchKind> BuiltIn<R, E> for WatchStream<K> {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = K::NAME;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.record_ref(id, top_id);
        Ok(Box::new(Self {
            top_id,
            cached: CachedVals::new(from),
            bind_ids: IntSet::default(),
            id,
            out: TagValue::phantom(),
            kind: PhantomData,
        }))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let top_id = ExprId::decode(buf)?;
        let id = BindId::decode(buf)?;
        let cached = CachedVals::image_decode(buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(Self {
            top_id,
            cached,
            bind_ids: IntSet::default(),
            id,
            out: TagValue::phantom(),
            kind: PhantomData,
        }))
    }
}

impl<K: WatchKind> WatchStream<K> {
    fn unwatch<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        for bid in self.bind_ids.drain() {
            ctx.unref_var(bid, self.top_id);
        }
    }
}

impl<R: Rt, E: UserEvent, K: WatchKind> Apply<R, E> for WatchStream<K> {
    /// `bind_ids` are registered watches on live OS watchers.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if !self.bind_ids.is_empty() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.top_id.encode(buf)?;
        self.id.encode(buf)?;
        self.cached.image_encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        if self.cached.update(ctx, from) {
            self.unwatch(ctx);
            for v in self.cached.0.iter().flatten() {
                extract_bind_ids(v, &mut self.bind_ids);
            }
            for bid in &self.bind_ids {
                ctx.rt.ref_var(*bid, self.top_id);
            }
        }
        for bid in &self.bind_ids {
            let Some(mut cbt) = ctx.event.custom.remove(bid) else { continue };
            let Some(w) = (&mut *cbt as &mut dyn Any).downcast_mut::<WEvent>() else {
                continue;
            };
            let v = match &w.0.event {
                EventKind::Error(e) => errf!("WatchError", "{e:?}"),
                EventKind::Event(_) => K::convert(w),
            };
            ctx.rt.set_var(self.id, v);
        }
        match ctx.event.variables.remove(&self.id) {
            Some(tv) => self.out.set(TagValue::fired(tv.value_cloned())),
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.unwatch(ctx);
        self.cached.clear();
        self.out = TagValue::phantom();
        ctx.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.unwatch(ctx);
        ctx.unref_var(self.id, self.top_id);
    }
}
