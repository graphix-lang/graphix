use anyhow::Result;
use futures::{SinkExt, channel::mpsc};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CBATCH_POOL, CompileCtx, CustomBuiltinType, Event, ExecCtx,
    Node, Rt, Scope, TagValue, UserEvent,
    effects::Effect,
    expr::ExprId,
    image::{self, ImageBuf},
    typ::FnType,
};
use graphix_package_core::{CachedVals, Invocation};
use netidx::publisher::Typ;
use netidx_core::pack::{Pack, PackError};
use netidx_derive::IntoValue;
use netidx_value::{ValArray, Value};
use poolshark::{
    global::{GPooled, Pool},
    local::LPooled,
};
use std::{
    any::Any,
    cmp::Ordering,
    hash::{Hash, Hasher},
    pin::Pin,
    sync::LazyLock,
    task::{Context, Poll, Waker},
};

use crate::{
    encoding::{decode_key, decode_value, encode_key},
    tree::get_tree_inner,
};

#[derive(Debug, Clone)]
struct SubscriptionValue {
    bind_id: BindId,
}

impl PartialEq for SubscriptionValue {
    fn eq(&self, other: &Self) -> bool {
        self.bind_id == other.bind_id
    }
}

impl Eq for SubscriptionValue {}

impl PartialOrd for SubscriptionValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SubscriptionValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.bind_id.cmp(&other.bind_id)
    }
}

impl Hash for SubscriptionValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.bind_id.hash(state)
    }
}

graphix_package_core::impl_no_pack!(SubscriptionValue);

graphix_package_core::abstract_wrapper!(
    SubscriptionValue,
    static SUBSCRIPTION_WRAPPER = "db::subscription::Subscription"
);

#[derive(Debug)]
enum DbEvent {
    Insert { key: Value, value: Value },
    Remove { key: Value },
}

static EVENT_POOL: LazyLock<Pool<Vec<DbEvent>>> = LazyLock::new(|| Pool::new(128, 4096));

#[derive(Debug)]
struct DbEvents(GPooled<Vec<DbEvent>>);

impl CustomBuiltinType for DbEvents {}

fn decode_sled_event(key_typ: Option<Typ>, event: sled::Event) -> Result<DbEvent> {
    Ok(match event {
        sled::Event::Insert { key, value } => DbEvent::Insert {
            key: decode_key(key_typ, &key)?,
            value: decode_value(&value)?,
        },
        sled::Event::Remove { key } => {
            DbEvent::Remove { key: decode_key(key_typ, &key)? }
        }
    })
}

fn push_event(key_typ: Option<Typ>, event: sled::Event, events: &mut Vec<DbEvent>) {
    match decode_sled_event(key_typ, event) {
        Ok(ev) => events.push(ev),
        Err(e) => log::warn!("db subscription: {e:#}"),
    }
}

fn drain_ready(
    subscriber: &mut sled::Subscriber,
    key_typ: Option<Typ>,
    events: &mut Vec<DbEvent>,
) {
    let waker = Waker::noop();
    let mut cx = Context::from_waker(&waker);
    while let Poll::Ready(Some(event)) = Pin::new(&mut *subscriber).poll(&mut cx) {
        push_event(key_typ, event, events)
    }
}

async fn watch(
    mut subscriber: sled::Subscriber,
    key_typ: Option<Typ>,
    bind_id: BindId,
    mut tx: mpsc::Sender<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
) {
    while let Some(first) = (&mut subscriber).await {
        let mut events = EVENT_POOL.take();
        push_event(key_typ, first, &mut events);
        // CR claude for eric: [bug] A chunk is whatever sled had ready when
        // this task woke, and the runtime delivers one chunk per bind id
        // per cycle. So one atomic db::batch or txn::commit reaches
        // on_insert spread over many cycles: a 2000-insert commit took 51
        // to 770 cycles, and the commit's reply arrived midway. A
        // subscriber that derives state from the events sees states the db
        // never held, such as a debit without its credit. on_insert and
        // on_remove also split each chunk into two arrays, so when an
        // insert and a remove of one key arrive in one cycle their order is
        // lost and a mirror cannot tell whether the key survived. sled's
        // watch events carry no commit boundary, so delivering a write as
        // one unit needs this package's own write paths to publish their
        // write sets. probe: design/review-2026-10-05/repro/db2-07.gx
        // (db2-07)
        // 2026-10-07 claude: re-addressed: sled is the process's alone (one handle per
        // path, tree.rs open_db), so the package's own write paths could publish each
        // write set as one event in place of sled's watch. Ordering a write set's
        // publication with the next write's needs a lock per db around apply and publish,
        // which serializes the db's writes: a trade for Eric. One ordered on_change
        // stream would also settle the insert/remove interleaving.
        drain_ready(&mut subscriber, key_typ, &mut events);
        if events.is_empty() {
            continue;
        }
        let mut batch = CBATCH_POOL.take();
        batch.push((bind_id, Box::new(DbEvents(events)) as Box<dyn CustomBuiltinType>));
        if tx.send(batch).await.is_err() {
            break;
        }
    }
}

/// A subscription to a tree's changes under a prefix. The watch is
/// registered in the cycle the subscription fires, so no write issued on
/// that fire is missed; it pauses while its arm sleeps and while an
/// argument is bottom.
#[derive(Debug)]
pub(crate) struct DbSubscribe {
    args: CachedVals,
    bind_id: BindId,
    abort: Option<tokio::task::AbortHandle>,
    slept: bool,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for DbSubscribe {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "db_subscription_new";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(DbSubscribe {
            args: CachedVals::new(from),
            bind_id: BindId::new(),
            abort: None,
            slept: false,
            out: TagValue::phantom(),
        }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        Ok(Box::new(DbSubscribe {
            args: CachedVals::image_decode(buf)?,
            bind_id: BindId::new(),
            abort: None,
            slept: false,
            out: TagValue::phantom(),
        }))
    }
}

impl DbSubscribe {
    fn stop(&mut self) {
        if let Some(abort) = self.abort.take() {
            abort.abort();
        }
    }

    /// Watch the cached tree under the cached prefix, reporting under the
    /// current bind id; a prefix no key can have watches nothing.
    fn watch<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.stop();
        let Some(tree) = get_tree_inner(&self.args, 1) else { return };
        let prefix = match self.args.0[0].as_ref() {
            None => return,
            Some(Value::Null) => Ok(GPooled::orphan(vec![])),
            Some(v) => encode_key(tree.key_typ, v),
        };
        let Ok(prefix) = prefix else { return };
        let subscriber = tree.tree.watch_prefix(&*prefix);
        let (tx, rx) = mpsc::channel(10);
        ctx.rt.watch(rx);
        let jh = tokio::task::spawn(watch(subscriber, tree.key_typ, self.bind_id, tx));
        self.abort = Some(jh.abort_handle());
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for DbSubscribe {
    /// A running watch exists only once a cycle has run.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.abort.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.args.image_encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        match self.args.update(ctx, from) {
            Invocation::Bottom { fresh } => {
                self.stop();
                self.out.set_bottom(fresh)
            }
            Invocation::Fired => {
                self.slept = false;
                self.bind_id = BindId::new();
                self.watch(ctx);
                let sub = SubscriptionValue { bind_id: self.bind_id };
                self.out.set(TagValue::fired(SUBSCRIPTION_WRAPPER.wrap(sub)))
            }
            Invocation::Quiet => {
                if std::mem::take(&mut self.slept) {
                    self.watch(ctx);
                }
                self.out.ride()
            }
        }
    }

    // XCR claude for claude: [bug] sleep aborts the watch task and forgets tree_val, and
    // the accessors' sleep below drops their bind id. Both re-establish only on a FIRED
    // argument, but at a wake the arm's arguments arrive stale. So a subscription, or
    // an on_insert/on_remove accessor, in a select arm that sleeps once never delivers
    // again: inserts after the arm comes back are silently lost. sys::net::subscribe
    // keeps a slept bit and resubscribes from the present path on the first update
    // after sleep. This needs the same: keep tree_val and the prefix and resubscribe
    // after a sleep, and have the accessor re-ref the bind id from its stale slot.
    // sys/watch.rs WatchStream has the same shape and goes deaf the same way. probe:
    // design/review-2026-10-05/repro/db2-05.gx (db2-05)
    // 2026-10-07 claude: the subscription keeps its arguments across a sleep and
    // watches again, under the same bind id, on its first update after; an accessor
    // keeps its argument and re-refs. Events while asleep are lost (a pause). No lib
    // test; design/review-2026-10-05/repro/db2-05.gx prints [{key: "b", value: 2}].
    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.stop();
        self.slept = true;
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.stop();
    }
}

fn extract_sub_bind_id(v: &Value) -> Option<BindId> {
    match v {
        Value::Abstract(a) => Some(a.downcast_ref::<SubscriptionValue>()?.bind_id),
        _ => None,
    }
}

fn scan_db_events<E: UserEvent>(
    bind_id: Option<BindId>,
    event: &Event<E>,
    convert: fn(&DbEvent) -> Option<Value>,
) -> Option<Value> {
    let bid = bind_id?;
    event.with_custom(&bid, |cbt| {
        let events = (cbt as &dyn Any).downcast_ref::<DbEvents>()?;
        let mut vals: LPooled<Vec<Value>> = events.0.iter().filter_map(convert).collect();
        if vals.is_empty() {
            return None;
        }
        Some(Value::Array(ValArray::from_iter_exact(vals.drain(..))))
    })
}

macro_rules! db_event_accessor {
    ($name:ident, $builtin_name:expr, $convert:expr) => {
        #[derive(Debug)]
        pub(crate) struct $name {
            top_id: ExprId,
            cached: CachedVals,
            bind_id: Option<BindId>,
            out: TagValue,
        }

        impl<R: Rt, E: UserEvent> BuiltIn<R, E> for $name {
            const EFFECT: Effect = Effect::Async;
            const NAME: &str = $builtin_name;

            fn init<'a, 'b, 'c, 'd>(
                _ctx: &'a mut CompileCtx<R, E>,
                _typ: &'a FnType,
                _resolved: Option<&'d FnType>,
                _scope: &'b Scope,
                from: &'c [Node<R, E>],
                top_id: ExprId,
            ) -> Result<Box<dyn Apply<R, E>>> {
                Ok(Box::new($name {
                    top_id,
                    cached: CachedVals::new(from),
                    bind_id: None,
                    out: TagValue::phantom(),
                }))
            }

            fn image_decode(
                ctx: &mut ExecCtx<'_, R, E>,
                _from: &[Node<R, E>],
                buf: &mut &[u8],
            ) -> Result<Box<dyn Apply<R, E>>, PackError> {
                let top_id = ExprId::decode(buf)?;
                let cached = CachedVals::image_decode(buf)?;
                let bind_id = <Option<BindId>>::decode(buf)?;
                if let Some(bid) = bind_id {
                    ctx.record_ref(bid, top_id);
                }
                Ok(Box::new($name { top_id, cached, bind_id, out: TagValue::phantom() }))
            }
        }

        impl<R: Rt, E: UserEvent> Apply<R, E> for $name {
            fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
                self.top_id.encode(buf)?;
                self.cached.image_encode(buf)?;
                self.bind_id.encode(buf)
            }

            fn update(
                &mut self,
                ctx: &mut ExecCtx<'_, R, E>,
                from: &mut [Node<R, E>],
            ) -> &TagValue {
                match self.cached.update(ctx, from) {
                    Invocation::Bottom { fresh } => {
                        if let Some(bid) = self.bind_id.take() {
                            ctx.unref_var(bid, self.top_id);
                        }
                        return self.out.set_bottom(fresh);
                    }
                    // after a sleep, the standing subscription again
                    Invocation::Quiet if self.bind_id.is_some() => (),
                    Invocation::Quiet | Invocation::Fired => {
                        if let Some(bid) = self.bind_id.take() {
                            ctx.unref_var(bid, self.top_id);
                        }
                        self.bind_id =
                            self.cached.0[0].as_ref().and_then(extract_sub_bind_id);
                        if let Some(bid) = self.bind_id {
                            ctx.rt.ref_var(bid, self.top_id);
                        }
                    }
                }
                match scan_db_events(self.bind_id, ctx.event, $convert) {
                    Some(v) => self.out.set(TagValue::fired(v)),
                    None => self.out.ride(),
                }
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                if let Some(bid) = self.bind_id.take() {
                    ctx.unref_var(bid, self.top_id);
                }
            }

            fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                if let Some(bid) = self.bind_id {
                    ctx.unref_var(bid, self.top_id);
                }
            }
        }
    };
}

db_event_accessor!(DbOnInsert, "db_subscription_on_insert", |se| match se {
    DbEvent::Insert { key, value } => {
        #[derive(IntoValue)]
        struct Fields {
            key: Value,
            value: Value,
        }
        Some(Fields { key: key.clone(), value: value.clone() }.into())
    }
    DbEvent::Remove { .. } => None,
});

db_event_accessor!(DbOnRemove, "db_subscription_on_remove", |se| match se {
    DbEvent::Remove { key } => {
        #[derive(IntoValue)]
        struct Fields {
            key: Value,
        }
        Some(Fields { key: key.clone() }.into())
    }
    DbEvent::Insert { .. } => None,
});
