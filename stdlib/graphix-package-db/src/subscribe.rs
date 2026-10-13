use anyhow::Result;
use arcstr::{ArcStr, literal};
use futures::{
    channel::mpsc,
    future::{self, Either},
};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CBATCH_POOL, CompileCtx, CustomBuiltinType, Event, ExecCtx,
    Node, Rt, Scope, TagValue, UserEvent,
    effects::Effect,
    errf,
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
    mem,
    pin::{Pin, pin},
    sync::LazyLock,
    task::{Context, Poll, Waker},
};

use crate::{
    encoding::{decode_key, decode_value, encode_prefix},
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

static EVENT_POOL: LazyLock<Pool<Vec<Result<DbEvent>>>> =
    LazyLock::new(|| Pool::new(128, 4096));

#[derive(Debug)]
struct DbEvents(GPooled<Vec<Result<DbEvent>>>);

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

fn push_event(
    key_typ: Option<Typ>,
    event: sled::Event,
    events: &mut Vec<Result<DbEvent>>,
) {
    events.push(decode_sled_event(key_typ, event));
}

fn drain_ready(
    subscriber: &mut sled::Subscriber,
    key_typ: Option<Typ>,
    events: &mut Vec<Result<DbEvent>>,
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
    let mut events = EVENT_POOL.take();
    let mut open = true;
    while open {
        if events.is_empty() {
            // CR claude for claude: [bug/critical] sled 0.34 reserves a subscriber slot
            // for every write, including a no-op write (the same value written again).
            // For a no-op it drops the reservation without waking the waker that
            // `Subscriber::poll` left in sled's senders map. This task therefore stays
            // parked here, and the dropped OneShots pile up in sled's 1024-slot
            // channel. After 1024 no-op writes, every writer to the tree blocks in
            // `Subscribers::reserve` while holding sled's `watched` read lock (and the
            // global concurrency-control lock for batches and commits). A later
            // `watch_prefix` in `DbSubscribe::watch` then blocks the runtime thread,
            // and dropping the Subscriber on abort deadlocks on `home.write()`. Fix:
            // drain the Subscriber from a blocking thread with `next_timeout` or its
            // blocking Iterator, which consume a dropped filler, and forward the events
            // into the mpsc. Also take `watch_prefix` off the runtime thread. Pin: more
            // than 1024 no-op inserts under a standing subscription must all answer.
            // Probe: design/review-2026-10-12/repro/db2-01.gx (stops at 1025 answered
            // inserts and hangs; without the subscription it runs to about 15000 and
            // exits). (db2-01)
            // 2026-10-13 codex: Reproduced: the probe stalls at 1026 inserts and needs SIGKILL.
            // The proposed next_timeout also wedges: sled 0.34.7 saves the dropped
            // OneShot back into Subscriber::existing on Disconnected, so one no-op
            // followed by a real insert returns Disconnected on every call.
            // Iterator drains fillers but has no cancellable wait for sleep/delete.
            // Fix sled::Subscribers::reserve / ReservedBroadcast drop to wake the
            // registered subscriber on cancellation, and Subscriber::next_timeout
            // to consume cancelled reservations. That dependency is outside this
            // worktree scope; retain the db2-01.gx probe for the dependency fix.
            match (&mut subscriber).await {
                Some(ev) => push_event(key_typ, ev, &mut events),
                None => break,
            }
        }
        // read sled while the runtime has no room: a subscriber that stops
        // reading fills sled's buffer, which blocks every writer to the tree
        let room = loop {
            drain_ready(&mut subscriber, key_typ, &mut events);
            let room = future::poll_fn(|cx| tx.poll_ready(cx));
            match future::select(pin!(room), &mut subscriber).await {
                Either::Left((room, _)) => break room,
                Either::Right((Some(ev), _)) => push_event(key_typ, ev, &mut events),
                Either::Right((None, _)) => {
                    open = false;
                    break future::poll_fn(|cx| tx.poll_ready(cx)).await;
                }
            }
        };
        if room.is_err() {
            break;
        }
        if events.is_empty() {
            continue;
        }
        let events = mem::replace(&mut events, EVENT_POOL.take());
        let mut batch = CBATCH_POOL.take();
        batch.push((bind_id, Box::new(DbEvents(events)) as Box<dyn CustomBuiltinType>));
        if tx.start_send(batch).is_err() {
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
            Some(v) => encode_prefix(tree.key_typ, v),
        };
        let Ok(prefix) = prefix else { return };
        let subscriber = tree.tree.watch_prefix(&*prefix);
        let (tx, rx) = mpsc::channel(1);
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
                // CR claude for claude: [bug/medium] Every fire of an argument mints a
                // new bind id and restarts the watch, even when the tree and prefix are
                // the values already cached. stop() aborts the old task, which drops
                // the events it was buffering. Batches already sent under the old id
                // are dropped too, because the accessors switch to the new id in the
                // same cycle. A write that completes just before a re-fire therefore
                // never reaches on_change. That breaks the subscription.gxi promise
                // that a copy applying the deliveries holds what the tree holds. probe:
                // design/review-2026-10-12/repro/db2-05.gx loses "k1" in about half its
                // runs when `#prefix` re-fires with the same null; without the re-fire
                // it loses nothing. Fix: on Invocation::Fired, compare the new (tree,
                // prefix) with the previous pair and keep the running watch and bind id
                // when they are equal; restart only on a real change, and say in the
                // doc that a change of tree or prefix discards the events still
                // pending. (db2-05)
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
        let mut vals: LPooled<Vec<Value>> = LPooled::take();
        for event in &*events.0 {
            match event {
                Err(e) => return Some(errf!("DbErr", "{e:#}")),
                Ok(event) => vals.extend(convert(event)),
            }
        }
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
                            ctx.release_var(bid, self.top_id);
                        }
                        return self.out.set_bottom(fresh);
                    }
                    // after a sleep, the standing subscription again
                    Invocation::Quiet if self.bind_id.is_some() => (),
                    Invocation::Quiet | Invocation::Fired => {
                        if let Some(bid) = self.bind_id.take() {
                            ctx.release_var(bid, self.top_id);
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

            // CR claude for claude: [bug/medium] The on_change/on_insert/on_remove
            // accessors release their listener in sleep but keep `out`. At the wake the
            // first update re-registers, finds no event, and `out.ride()` returns the
            // previous batch. A select whose scrutinee fired then emits it again as a
            // new event, so one insert is delivered twice: a screen that toasts or
            // appends on_insert events repeats the last one each time it is reselected.
            // design/async_sleep_outputs.md requires database event accessors to reset
            // to phantom in sleep, as the fs watch accessors do. Fix: set `self.out =
            // TagValue::phantom()` here, and add a pin to
            // stdlib/graphix-tests/src/lang/async_restart.rs. probe:
            // design/review-2026-10-12/repro/x-node-contract-02.gx Also reported as
            // (x-fix-stdlib-07), (x-rerun-2-02). (x-node-contract-02)
            fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                if let Some(bid) = self.bind_id.take() {
                    ctx.release_var(bid, self.top_id);
                }
            }

            fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                if let Some(bid) = self.bind_id {
                    ctx.release_var(bid, self.top_id);
                }
            }
        }
    };
}

db_event_accessor!(DbOnChange, "db_subscription_on_change", |se| Some(match se {
    DbEvent::Insert { key, value } => {
        Value::Array(ValArray::from([
            literal!("Insert").into(),
            key.clone(),
            value.clone(),
        ]))
    }
    DbEvent::Remove { key } => {
        Value::Array(ValArray::from([literal!("Remove").into(), key.clone()]))
    }
}));

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

#[cfg(test)]
mod test {
    use super::*;
    use crate::encoding::{encode_key, encode_value};
    use std::{sync::mpsc as std_mpsc, thread, time::Duration};

    #[test]
    fn undecodable_events_reach_accessors() {
        for event in [
            sled::Event::Insert {
                key: 1i64.to_be_bytes().as_slice().into(),
                value: b"".as_slice().into(),
            },
            sled::Event::Remove { key: b"".as_slice().into() },
        ] {
            let mut events = EVENT_POOL.take();
            push_event(Some(Typ::I64), event, &mut events);
            let id = BindId::new();
            let event = Event::new(graphix_compiler::NoUserEvent);
            event.custom.lock().insert(id, Box::new(DbEvents(events)));
            for convert in [
                (|_: &DbEvent| None) as fn(&DbEvent) -> Option<Value>,
                (|_: &DbEvent| Some(Value::Null)) as fn(&DbEvent) -> Option<Value>,
            ] {
                let v = scan_db_events(Some(id), &event, convert)
                    .expect("decode error delivered");
                let Value::Error(e) = v else { panic!("expected DbErr") };
                let Value::Array(fields) = &*e else { panic!("expected tagged error") };
                assert_eq!(fields[0], Value::from("DbErr"));
                let Value::String(msg) = &fields[1] else {
                    panic!("expected error text")
                };
                assert!(msg.contains("undecodable"));
            }
        }
    }

    /// The runtime never takes a delivery, yet writes to the tree finish:
    /// the watch keeps reading sled, whose full buffer would block them.
    /// The writer is a thread of its own, so a blocked one fails the test
    /// instead of holding the runtime's shutdown.
    #[tokio::test(flavor = "multi_thread")]
    async fn writes_pass_a_stalled_runtime() {
        let db = sled::Config::new().temporary(true).open().unwrap();
        let (tx, _never_read) = mpsc::channel(1);
        tokio::spawn(watch(db.watch_prefix(vec![]), None, BindId::new(), tx));
        let (done_tx, done) = std_mpsc::channel();
        thread::spawn(move || {
            let value = encode_value(&Value::Null).unwrap();
            for i in 0u64..3000 {
                let key = encode_key(None, &Value::U64(i)).unwrap();
                db.insert(key.as_slice(), value.as_slice()).unwrap();
            }
            let _ = done_tx.send(());
        });
        done.recv_timeout(Duration::from_secs(10))
            .expect("a writer blocked on the subscriber's buffer");
    }
}
