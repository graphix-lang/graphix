use anyhow::{Result, anyhow};
use arcstr::literal;
use futures::{channel::mpsc, future, prelude::*};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CBATCH_POOL, CompileCtx, CustomBuiltinType, Event, ExecCtx,
    Node, Rt, Scope, Tag, TagValue, UserEvent,
    effects::Effect,
    errf,
    expr::ExprId,
    image::{self, ImageBuf},
    node::WakeBit,
    rep,
    typ::{FnType, Type},
};
use graphix_package_core::{
    CachedVals, Invocation, data_target, read_back, write_marked_data,
};
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
    sync::LazyLock,
};

use crate::{
    encoding::{decode_key, decode_value, encode_prefix},
    tree::{TreeInner, get_tree_inner},
    watch::{Change, Registered},
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

fn push_event(key_typ: Option<Typ>, change: Change, events: &mut Vec<Result<DbEvent>>) {
    events.push((|| {
        let key = decode_key(key_typ, &change.key)?;
        Ok(match change.value {
            Some(value) => DbEvent::Insert { key, value: decode_value(&value)? },
            None => DbEvent::Remove { key },
        })
    })());
}

/// `events` as one delivery under `bind_id`.
fn delivery(
    bind_id: BindId,
    events: GPooled<Vec<Result<DbEvent>>>,
) -> GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>> {
    let mut batch = CBATCH_POOL.take();
    batch.push((bind_id, Box::new(DbEvents(events)) as Box<dyn CustomBuiltinType>));
    batch
}

/// Forward `changes` to the runtime, one batch per delivery the runtime
/// has room for, until the task ends, which ends the watch. A channel the
/// tree's drop closed ends it with an error.
async fn watch(
    _watch: Registered,
    mut changes: mpsc::UnboundedReceiver<Change>,
    key_typ: Option<Typ>,
    bind_id: BindId,
    mut tx: mpsc::Sender<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
) {
    loop {
        let next = changes.next().await;
        if future::poll_fn(|cx| tx.poll_ready(cx)).await.is_err() {
            break;
        }
        let mut batch_events = EVENT_POOL.take();
        let dropped = match next {
            Some(change) => {
                push_event(key_typ, change, &mut batch_events);
                while let Ok(change) = changes.try_recv() {
                    push_event(key_typ, change, &mut batch_events);
                }
                false
            }
            None => {
                batch_events.push(Err(anyhow!("the tree was dropped")));
                true
            }
        };
        if tx.start_send(delivery(bind_id, batch_events)).is_err() || dropped {
            break;
        }
    }
}

/// Watch `tree` under `prefix`, its changes delivered under `bind_id`;
/// aborting the returned task ends the watch.
fn spawn_watch(
    tree: &TreeInner,
    prefix: &[u8],
    bind_id: BindId,
    tx: mpsc::Sender<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
) -> tokio::task::AbortHandle {
    let (changes_tx, changes) = mpsc::unbounded();
    let registered = tree.watches.add(prefix, changes_tx);
    tokio::task::spawn(watch(registered, changes, tree.key_typ, bind_id, tx))
        .abort_handle()
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
    slept: WakeBit,
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
            slept: WakeBit::default(),
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
            slept: WakeBit::default(),
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
    /// current bind id; a prefix no key can have watches nothing, and one
    /// that cannot be written as data is the error.
    fn watch<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> Result<()> {
        self.stop();
        let Some(tree) = get_tree_inner(&self.args, 1) else { return Ok(()) };
        let prefix = match self.args.value(0).map(|v| write_marked_data(ctx, v)) {
            None => return Ok(()),
            Some(Err(e)) => return Err(e),
            Some(Ok(Value::Null)) => Ok(GPooled::orphan(vec![])),
            Some(Ok(v)) => encode_prefix(tree.key_typ, &v),
        };
        let Ok(prefix) = prefix else { return Ok(()) };
        let (tx, rx) = mpsc::channel(1);
        ctx.rt.watch(rx);
        self.abort = Some(spawn_watch(&tree, &prefix, self.bind_id, tx));
        Ok(())
    }

    /// `e` as the subscription's one event.
    fn report<R: Rt, E: UserEvent>(&self, ctx: &mut ExecCtx<'_, R, E>, e: anyhow::Error) {
        let (mut tx, rx) = mpsc::channel(1);
        let mut events = EVENT_POOL.take();
        events.push(Err(e));
        tx.try_send(delivery(self.bind_id, events)).expect("a new channel has room");
        ctx.rt.watch(rx);
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
        let previous = [self.args.value(0).cloned(), self.args.value(1).cloned()];
        match self.args.update(ctx, from) {
            Invocation::Bottom { fresh } => {
                self.stop();
                self.out.set_bottom(fresh)
            }
            Invocation::Fired => {
                self.slept = WakeBit::default();
                if self.abort.is_some()
                    && self.args.values().eq(previous.iter().map(Option::as_ref))
                {
                    return self.out.retag(Tag::FIRED);
                }
                self.bind_id = BindId::new();
                if let Err(e) = self.watch(ctx) {
                    self.report(ctx, e);
                }
                let sub = SubscriptionValue { bind_id: self.bind_id };
                self.out.set(TagValue::fired(SUBSCRIPTION_WRAPPER.wrap(sub)))
            }
            Invocation::Quiet => {
                // a prefix's error was reported when the subscription fired
                if self.slept.take() {
                    let _ = self.watch(ctx);
                }
                self.out.ride()
            }
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.stop();
        self.slept.set();
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
            /// what the events are read back as, where that needs `Cast`
            data: Option<Type>,
            out: TagValue,
        }

        impl<R: Rt, E: UserEvent> BuiltIn<R, E> for $name {
            const EFFECT: Effect = Effect::Async;
            const NAME: &str = $builtin_name;

            fn init<'a, 'b, 'c, 'd>(
                ctx: &'a mut CompileCtx<R, E>,
                _typ: &'a FnType,
                resolved: Option<&'d FnType>,
                _scope: &'b Scope,
                from: &'c [Node<R, E>],
                top_id: ExprId,
            ) -> Result<Box<dyn Apply<R, E>>> {
                Ok(Box::new($name {
                    top_id,
                    cached: CachedVals::new(from),
                    bind_id: None,
                    data: data_target(&ctx.env, resolved),
                    out: TagValue::phantom(),
                }))
            }

            fn image_decode(
                _ctx: &mut ExecCtx<'_, R, E>,
                _from: &[Node<R, E>],
                buf: &mut &[u8],
            ) -> Result<Box<dyn Apply<R, E>>, PackError> {
                let top_id = ExprId::decode(buf)?;
                let cached = CachedVals::image_decode(buf)?;
                let data = <Option<Type>>::decode(buf)?;
                Ok(Box::new($name {
                    top_id,
                    cached,
                    bind_id: None,
                    data,
                    out: TagValue::phantom(),
                }))
            }
        }

        impl<R: Rt, E: UserEvent> Apply<R, E> for $name {
            fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
                self.top_id.encode(buf)?;
                // a subscription value, which a bind id follows, has no
                // image: `cached` refuses it
                self.cached.image_encode(buf)?;
                self.data.encode(buf)
            }

            fn typecheck1(
                &mut self,
                ctx: &mut CompileCtx<R, E>,
                _from: &mut [Node<R, E>],
                resolved: &FnType,
            ) -> Result<()> {
                self.data = data_target(&ctx.env, Some(resolved));
                Ok(())
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
                        self.bind_id = self.cached.value(0).and_then(extract_sub_bind_id);
                        if let Some(bid) = self.bind_id {
                            ctx.ref_var(bid, self.top_id);
                        }
                    }
                }
                match scan_db_events(self.bind_id, ctx.event, $convert) {
                    Some(v @ Value::Error(_)) => self.out.set(TagValue::fired(v)),
                    Some(v) => {
                        let v = match &self.data {
                            Some(t) => read_back(ctx, t, &literal!("DbErr"), v),
                            None => v,
                        };
                        self.out.set(TagValue::fired(v))
                    }
                    None => self.out.ride(),
                }
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
                self.out = TagValue::phantom();
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
        rep::make_variant(literal!("Insert"), [key.clone(), value.clone()])
    }
    DbEvent::Remove { key } => {
        rep::make_variant(literal!("Remove"), [key.clone()])
    }
}));

db_event_accessor!(DbOnInsert, "db_subscription_on_insert", |se| match se {
    DbEvent::Insert { key, value } => {
        #[derive(IntoValue)]
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
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
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
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
    use crate::{
        encoding::{encode_key, encode_value},
        tree::{Tree, open_db, tree_inner},
    };
    use std::{thread, time::Duration};

    #[test]
    fn undecodable_events_reach_accessors() {
        for change in [
            Change {
                key: 1i64.to_be_bytes().as_slice().into(),
                value: Some(b"".as_slice().into()),
            },
            Change { key: b"".as_slice().into(), value: None },
        ] {
            let mut events = EVENT_POOL.take();
            push_event(Some(Typ::I64), change, &mut events);
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
                let Some((tag, [Value::String(msg)])) =
                    graphix_compiler::rep::variant(&e)
                else {
                    panic!("expected tagged error text")
                };
                assert_eq!(tag, "DbErr");
                assert!(msg.contains("undecodable"));
            }
        }
    }

    async fn temp_tree() -> (tempfile::TempDir, Tree) {
        let dir = tempfile::tempdir().unwrap();
        let db = open_db(dir.path().to_str().unwrap().into()).await.unwrap();
        let tree = db.open_tree("t").unwrap();
        (dir, tree_inner(db, tree, None))
    }

    fn key(i: u64) -> GPooled<Vec<u8>> {
        encode_key(None, &Value::U64(i)).unwrap()
    }

    fn value(v: Value) -> GPooled<Vec<u8>> {
        encode_value(&v).unwrap()
    }

    /// Every event of the deliveries `rx` receives until `done` says the
    /// last has arrived.
    async fn received(
        rx: &mut mpsc::Receiver<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
        done: impl Fn(&[DbEvent]) -> bool,
    ) -> Vec<DbEvent> {
        let mut events = vec![];
        while !done(&events) {
            let batch = tokio::time::timeout(Duration::from_secs(10), rx.next())
                .await
                .expect("a change was not delivered")
                .unwrap();
            for (_, batch) in batch.iter() {
                let batch = (&**batch as &dyn Any).downcast_ref::<DbEvents>().unwrap();
                for event in batch.0.iter() {
                    match event {
                        Ok(DbEvent::Insert { key, value }) => {
                            events.push(DbEvent::Insert {
                                key: key.clone(),
                                value: value.clone(),
                            })
                        }
                        Ok(DbEvent::Remove { key }) => {
                            events.push(DbEvent::Remove { key: key.clone() })
                        }
                        Err(e) => panic!("undecodable event {e:?}"),
                    }
                }
            }
        }
        events
    }

    /// The runtime never takes a delivery, yet writes to the tree finish.
    #[tokio::test(flavor = "multi_thread")]
    async fn writes_pass_a_stalled_runtime() {
        let (_dir, tree) = temp_tree().await;
        let (tx, _never_read) = mpsc::channel(1);
        let _watch = spawn_watch(&tree, &[], BindId::new(), tx);
        let writer = tokio::task::spawn_blocking(move || {
            let v = value(Value::Null);
            for i in 0u64..3000 {
                tree.insert(&key(i), &v).unwrap();
            }
        });
        tokio::time::timeout(Duration::from_secs(10), writer)
            .await
            .expect("a writer blocked on a watch")
            .unwrap();
    }

    /// A write that changes nothing is no change: thousands of them pass a
    /// standing watch, and the change after them is delivered.
    #[tokio::test(flavor = "multi_thread")]
    async fn no_op_writes_pass_a_watch() {
        let (_dir, tree) = temp_tree().await;
        let (tx, mut rx) = mpsc::channel(1);
        let _watch = spawn_watch(&tree, &[], BindId::new(), tx);
        tokio::task::spawn_blocking(move || {
            let same = value(Value::Null);
            for _ in 0..3000 {
                tree.insert(&key(0), &same).unwrap();
            }
            tree.insert(&key(0), &value(Value::U64(1))).unwrap();
        })
        .await
        .unwrap();
        let values: Vec<Value> = received(&mut rx, |e| {
            matches!(e.last(), Some(DbEvent::Insert { value: Value::U64(1), .. }))
        })
        .await
        .into_iter()
        .map(|e| match e {
            DbEvent::Insert { value, .. } => value,
            e => panic!("unexpected event {e:?}"),
        })
        .collect();
        assert_eq!(values, [Value::Null, Value::U64(1)]);
    }

    /// A watch registers while a write that found none is applying, and
    /// still hears what that write changed.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_watch_registers_past_a_write_in_flight() {
        let (_dir, tree) = temp_tree().await;
        let (applied_tx, applied) = std::sync::mpsc::channel();
        let (go_tx, go) = std::sync::mpsc::channel::<()>();
        let writer = {
            let tree = tree.clone();
            thread::spawn(move || {
                crate::watch::write(&[(&tree.watches, &tree.tree)], |changes| {
                    let v = value(Value::U64(7));
                    let old = tree.tree.insert(&*key(0), &**v)?;
                    changes.record(0, &key(0), old.as_deref(), Some(&v));
                    applied_tx.send(()).unwrap();
                    go.recv().unwrap();
                    anyhow::Ok(())
                })
                .unwrap()
            })
        };
        applied.recv().unwrap();
        let (tx, mut rx) = mpsc::channel(1);
        let _watch = spawn_watch(&tree, &[], BindId::new(), tx);
        go_tx.send(()).unwrap();
        writer.join().unwrap();
        let events = received(&mut rx, |e| !e.is_empty()).await;
        assert!(matches!(&events[..], [DbEvent::Insert { value: Value::U64(7), .. }]));
    }

    /// Ending a tree's watches delivers the changes sent before, then one
    /// error, then nothing.
    #[tokio::test(flavor = "multi_thread")]
    async fn ended_watches_deliver_an_error() {
        let (_dir, tree) = temp_tree().await;
        let (tx, mut rx) = mpsc::channel(1);
        let watch = spawn_watch(&tree, &[], BindId::new(), tx);
        let t = tree.clone();
        tokio::task::spawn_blocking(move || {
            t.insert(&key(0), &value(Value::Null)).unwrap();
            t.watches.end();
        })
        .await
        .unwrap();
        assert_eq!(until_error(&mut rx).await, [true, false]);
        for _ in 0..100 {
            if watch.is_finished() {
                break;
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
        assert!(watch.is_finished());
        assert!(tree.watches.is_empty());
    }

    /// The success of each event delivered until one fails.
    async fn until_error(
        rx: &mut mpsc::Receiver<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
    ) -> Vec<bool> {
        let mut events = vec![];
        while events.last() != Some(&false) {
            let batch = tokio::time::timeout(Duration::from_secs(10), rx.next())
                .await
                .expect("no error was delivered")
                .unwrap();
            for (_, batch) in batch.iter() {
                let batch = (&**batch as &dyn Any).downcast_ref::<DbEvents>().unwrap();
                events.extend(batch.0.iter().map(|e| e.is_ok()));
            }
        }
        events
    }

    /// A watch registered on a tree's ended watches delivers the error at
    /// once.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_watch_after_the_end_delivers_an_error() {
        let (_dir, tree) = temp_tree().await;
        tree.watches.end();
        let (tx, mut rx) = mpsc::channel(1);
        let _watch = spawn_watch(&tree, &[], BindId::new(), tx);
        assert_eq!(until_error(&mut rx).await, [false]);
        assert!(tree.watches.is_empty());
    }

    /// The end waits for a watched write in flight, whose change is
    /// delivered before the error.
    #[tokio::test(flavor = "multi_thread")]
    async fn the_end_waits_for_a_write_in_flight() {
        let (_dir, tree) = temp_tree().await;
        let (tx, mut rx) = mpsc::channel(1);
        let _watch = spawn_watch(&tree, &[], BindId::new(), tx);
        let (applied_tx, applied) = std::sync::mpsc::channel();
        let (go_tx, go) = std::sync::mpsc::channel::<()>();
        let writer = {
            let tree = tree.clone();
            thread::spawn(move || {
                crate::watch::write(&[(&tree.watches, &tree.tree)], |changes| {
                    let v = value(Value::U64(7));
                    let old = tree.tree.insert(&*key(0), &**v)?;
                    changes.record(0, &key(0), old.as_deref(), Some(&v));
                    applied_tx.send(()).unwrap();
                    go.recv().unwrap();
                    anyhow::Ok(())
                })
                .unwrap()
            })
        };
        applied.recv().unwrap();
        let ender = {
            let tree = tree.clone();
            thread::spawn(move || tree.watches.end())
        };
        thread::sleep(Duration::from_millis(100));
        assert!(!ender.is_finished(), "the end did not wait for the write");
        go_tx.send(()).unwrap();
        writer.join().unwrap();
        ender.join().unwrap();
        assert_eq!(until_error(&mut rx).await, [true, false]);
    }

    /// Ending the watch unregisters it.
    #[tokio::test(flavor = "multi_thread")]
    async fn an_aborted_watch_unregisters() {
        let (_dir, tree) = temp_tree().await;
        let (tx, _rx) = mpsc::channel(1);
        let (changes_tx, changes) = mpsc::unbounded();
        let registered = tree.watches.add(&[], changes_tx);
        assert!(!tree.watches.is_empty());
        let watch =
            tokio::task::spawn(watch(registered, changes, None, BindId::new(), tx));
        watch.abort();
        let _ = watch.await;
        assert!(tree.watches.is_empty());
    }

    /// Writers race to count on one key by compare and swap, so the tree
    /// takes the counts in order: the watch hears them in that order.
    #[tokio::test(flavor = "multi_thread")]
    async fn racing_writers_deliver_in_order() {
        const COUNT: u64 = 4000;
        let (_dir, tree) = temp_tree().await;
        let (tx, mut rx) = mpsc::channel(1);
        let _watch = spawn_watch(&tree, &[], BindId::new(), tx);
        tree.insert(&key(0), &value(Value::U64(0))).unwrap();
        let writers: Vec<_> = (0..8)
            .map(|_| {
                let tree = tree.clone();
                thread::spawn(move || {
                    loop {
                        let stored = tree.tree.get(key(0).as_slice()).unwrap().unwrap();
                        let Value::U64(n) = decode_value(&stored).unwrap() else {
                            panic!("not a count")
                        };
                        if n >= COUNT {
                            break;
                        }
                        let next = value(Value::U64(n + 1));
                        let _ =
                            tree.compare_and_swap(&key(0), Some(&stored), Some(&next));
                    }
                })
            })
            .collect();
        for w in writers {
            w.join().unwrap();
        }
        let events = received(&mut rx, |e| {
            matches!(e.last(), Some(DbEvent::Insert { value: Value::U64(n), .. }) if *n == COUNT)
        })
        .await;
        let heard: Vec<u64> = events
            .iter()
            .map(|e| match e {
                DbEvent::Insert { value: Value::U64(n), .. } => *n,
                e => panic!("unexpected event {e:?}"),
            })
            .collect();
        assert_eq!(heard, (0..=COUNT).collect::<Vec<_>>());
    }

    /// A batch delivers each change it makes, in order; a pop, a swap and a
    /// remove are changes too.
    #[tokio::test(flavor = "multi_thread")]
    async fn every_write_is_heard() {
        let (_dir, tree) = temp_tree().await;
        let (tx, mut rx) = mpsc::channel(1);
        let _watch = spawn_watch(&tree, &[], BindId::new(), tx);
        let (k, v) = (
            |i| sled::IVec::from(key(i).as_slice()),
            |x| Some(sled::IVec::from(value(Value::U64(x)).as_slice())),
        );
        tokio::task::spawn_blocking(move || {
            let mut writes: crate::tree::Writes = poolshark::local::LPooled::take();
            writes.extend([(k(0), v(1)), (k(0), v(1)), (k(1), v(2)), (k(0), None)]);
            tree.apply_batch(&writes).unwrap();
            tree.compare_and_swap(&key(1), v(2).as_deref(), v(3).as_deref())
                .unwrap()
                .unwrap();
            tree.pop(false).unwrap();
            tree.insert(&key(2), &value(Value::U64(4))).unwrap();
            tree.remove(&key(2)).unwrap();
        })
        .await
        .unwrap();
        let events = received(&mut rx, |e| e.len() >= 7).await;
        let shown: Vec<String> = events
            .iter()
            .map(|e| match e {
                DbEvent::Insert { key, value } => format!("+{key}={value}"),
                DbEvent::Remove { key } => format!("-{key}"),
            })
            .collect();
        assert_eq!(
            shown,
            [
                "+u64:0=u64:1",
                "+u64:1=u64:2",
                "-u64:0",
                "+u64:1=u64:3",
                "-u64:1",
                "+u64:2=u64:4",
                "-u64:2"
            ]
        );
    }
}
