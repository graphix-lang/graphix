use crate::deftype;
use anyhow::{bail, Result};
use arcstr::{literal, ArcStr};
use enumflags2::BitFlags;
use extended_notify::{
    ArcPath, Event as NEvent, EventBatch, EventHandler, EventKind, Id, Interest, Watcher,
    WatcherConfigBuilder,
};
use futures::{channel::mpsc, SinkExt, TryFutureExt};
use fxhash::FxHashMap;
use graphix_compiler::{
    errf, expr::ExprId, Apply, BindId, BuiltIn, BuiltInInitFn, CustomBuiltinType, Event,
    ExecCtx, LibState, Node, Rt, UserEvent, CBATCH_POOL,
};
use netidx_value::{FromValue, ValArray, Value};
use parking_lot::Mutex;
use poolshark::{global::GPooled, local::LPooled};
use std::{any::Any, ops::Deref, sync::Arc, time::Duration};

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

struct WatchCtxInner {
    watcher: Watcher,
    idmap: Arc<Mutex<FxHashMap<Id, BindId>>>,
}

struct WatchCtx(Result<WatchCtxInner>);

macro_rules! or_err {
    ($t:expr) => {
        match &$t.0 {
            Ok(t) => t,
            Err(e) => bail!("watcher is not available {e:?}"),
        }
    };
}

impl WatchCtx {
    fn get<'a, R: Rt>(rt: &mut R, st: &'a mut LibState) -> &'a mut Self {
        fn start_watcher<R: Rt>(rt: &mut R) -> Result<WatchCtxInner> {
            let idmap = Arc::new(Mutex::new(FxHashMap::default()));
            let (notify_tx, notify_rx) = mpsc::channel(10);
            let notify_tx = NotifyChan { tx: notify_tx, idmap: idmap.clone() };
            let watcher = WatcherConfigBuilder::default()
                .event_handler(notify_tx)
                .build()?
                .start()?;
            rt.watch(notify_rx);
            Ok(WatchCtxInner { watcher, idmap })
        }
        st.get_or_else::<WatchCtx, _>(|| WatchCtx(start_watcher(rt)))
    }

    fn set_poll_interval(&self, dur: Duration) -> Result<()> {
        or_err!(self).watcher.set_poll_interval(dur)
    }

    fn set_poll_batch(&self, n: usize) -> Result<()> {
        or_err!(self).watcher.set_poll_batch(n)
    }

    fn add(
        &self,
        id: BindId,
        path: &str,
        interest: BitFlags<Interest>,
    ) -> Result<Watched> {
        let t = or_err!(self);
        let mut idmap = t.idmap.lock();
        let w = t.watcher.add(path.into(), interest)?;
        idmap.insert(w.id(), id);
        drop(idmap);
        Ok(Watched { w, idmap: Arc::clone(&t.idmap) })
    }
}

#[derive(Debug)]
pub(super) struct SetGlobals;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for SetGlobals {
    const NAME: &str = "fs_set_global_watch_parameters";
    deftype!(
        r#"fn(
            ?#poll_interval:[duration, null],
            ?#poll_batch_size:[i64, null]
        ) -> Result<null, `WatchError(string)>"#
    );

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _fntyp: &'a graphix_compiler::typ::FnType,
        _scope: &'b graphix_compiler::Scope,
        _args: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(SetGlobals))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for SetGlobals {
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
        match poll_interval {
            Some(poll_interval) if poll_interval < Duration::from_millis(100) => {
                return Some(errf!("WatchError", "poll_interval must be >= 100ms"))
            }
            None | Some(_) => (),
        }
        match batch_size {
            Some(batch_size) if batch_size < 0 => {
                return Some(errf!("WatchError", "batch_size must be >= 0"))
            }
            None | Some(_) => (),
        }
        let mut err = String::new();
        use std::fmt::Write;
        if let Some(poll_interval) = poll_interval {
            let wctx = WatchCtx::get(&mut ctx.rt, &mut ctx.libstate);
            if let Err(e) = wctx.set_poll_interval(poll_interval) {
                write!(err, "could not set poll interval: {e:?}").unwrap();
            }
        }
        if let Some(batch_size) = batch_size {
            let wctx = WatchCtx::get(&mut ctx.rt, &mut ctx.libstate);
            if let Err(e) = wctx.set_poll_batch(batch_size as usize) {
                if !err.is_empty() {
                    write!(err, ", ").unwrap()
                }
                write!(err, "could not set poll interval: {e:?}").unwrap()
            }
        }
        if poll_interval.is_some() || batch_size.is_some() {
            if err.is_empty() {
                Some(Value::Null)
            } else {
                Some(errf!("WatchError", "{err}"))
            }
        } else {
            None
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

macro_rules! watch {
    (
        type_name: $type_name:ident,
        builtin_name: $builtin_name:literal,
        graphix_type: $graphix_type:literal,
        handle_event: |$id:ident, $ctx:ident, $ev:ident| $handle_event:block
    ) => {
        #[derive(Debug)]
        pub(super) struct $type_name {
            id: BindId,
            top_id: ExprId,
            interest: Option<BitFlags<Interest>>,
            path: Option<ArcStr>,
            watch: Option<Watched>,
        }

        impl<R: Rt, E: UserEvent> BuiltIn<R, E> for $type_name {
            const NAME: &str = $builtin_name;
            deftype!($graphix_type);

            fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
                Arc::new(|ctx, _, _, _, top_id| {
                    let id = BindId::new();
                    ctx.rt.ref_var(id, top_id);
                    Ok(Box::new($type_name {
                        id,
                        top_id,
                        interest: None,
                        path: None,
                        watch: None,
                    }))
                })
            }
        }

        impl<R: Rt, E: UserEvent> Apply<R, E> for $type_name {
            fn update(
                &mut self,
                ctx: &mut ExecCtx<R, E>,
                from: &mut [Node<R, E>],
                event: &mut Event<E>,
            ) -> Option<Value> {
                let mut up = false;
                if let Some(Ok(mut int)) = from[0]
                    .update(ctx, event)
                    .map(|v| v.cast_to::<LPooled<Vec<WInterest>>>())
                {
                    let int = int.drain(..).fold(BitFlags::empty(), |mut acc, fl| {
                        acc.insert(fl.0);
                        acc
                    });
                    up |= self.interest != Some(int);
                    self.interest = Some(int);
                }
                if let Some(Ok(path)) =
                    from[1].update(ctx, event).map(|v| v.cast_to::<ArcStr>())
                {
                    let path = Some(path);
                    up = path != self.path;
                    self.path = path;
                }
                if up
                    && let Some(path) = &self.path
                    && let Some(interest) = self.interest
                {
                    let wctx = WatchCtx::get(&mut ctx.rt, &mut ctx.libstate);
                    match wctx.add(self.id, &*path, interest) {
                        Ok(w) => self.watch = Some(w),
                        Err(e) => {
                            ctx.rt.set_var(self.id, errf!("WatchError", "{e:?}"));
                        }
                    }
                }
                if let Some(mut w) = event.custom.remove(&self.id)
                    && let Some(w) = (&mut *w as &mut dyn Any).downcast_mut::<WEvent>()
                {
                    let $id = self.id;
                    let $ctx = ctx;
                    let $ev = w;
                    $handle_event;
                }
                event.variables.get(&self.id).cloned()
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
                self.path = None;
                self.watch = None;
                ctx.rt.unref_var(self.id, self.top_id);
                self.id = BindId::new();
                ctx.rt.ref_var(self.id, self.top_id);
            }

            fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
                ctx.rt.unref_var(self.id, self.top_id);
            }
        }
    };
}

fn utf8_path(p: ArcPath) -> Value {
    Value::String(arcstr::format!("{}", p.display()))
}

watch!(
    type_name: WatchBuiltIn,
    builtin_name: "fs_watch",
    graphix_type: "fn(?#interest:Array<Interest>, string) -> Result<string, `WatchError(string)>",
    handle_event: |id, ctx, w| {
        match &w.0.event {
            EventKind::Event(_) => {
                for p in w.0.paths.drain() {
                    ctx.rt.set_var(id, utf8_path(p))
                }
            }
            EventKind::Error(e) => ctx.rt.set_var(id, errf!("WatchError", "{e:?}")),
        }
    }
);

watch!(
    type_name: WatchFullBuiltIn,
    builtin_name: "fs_watch_full",
    graphix_type: "fn(?#interest:Array<Interest>, string) -> Result<WatchEvent, `WatchError(string)>",
    handle_event: |id, ctx, w| {
        let paths =
            Value::Array(ValArray::from_iter_exact(w.0.paths.drain().map(utf8_path)));
        match &w.0.event {
            EventKind::Error(e) => ctx.rt.set_var(id, errf!("WatchError", "{e:?}")),
            EventKind::Event(int) => {
                let e =
                    ((literal!("event"), Value::from(&WInterest(*int))), (literal!("paths"), paths));
                ctx.rt.set_var(id, e.into())
            }
        }
    }
);
