use anyhow::Result;
use futures::{channel::mpsc, SinkExt};
use graphix_compiler::{
    expr::ExprId, typ::FnType, Apply, BindId, BuiltIn, CustomBuiltinType, Event, ExecCtx,
    Node, Rt, Scope, UserEvent, CBATCH_POOL,
};
use graphix_package_core::CachedVals;
use netidx::publisher::Typ;
use netidx_value::{abstract_type::AbstractWrapper, Abstract, ValArray, Value};
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

use crate::encoding::{decode_key, decode_value, encode_key, key_struct, kv_struct};
use crate::tree::TreeValue;

// ── Subscription types ────────────────────────────────────────────

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

static SUBSCRIPTION_WRAPPER: LazyLock<AbstractWrapper<SubscriptionValue>> =
    LazyLock::new(|| {
        let id = uuid::Uuid::from_bytes([
            0xd4, 0xe5, 0xf6, 0x07, 0x18, 0x29, 0x4a, 0x3b, 0x4c, 0x5d, 0x6e, 0x7f, 0x80,
            0xa1, 0xb2, 0xc3,
        ]);
        Abstract::register::<SubscriptionValue>(id)
            .expect("failed to register SubscriptionValue")
    });

// ── Custom event ──────────────────────────────────────────────────

#[derive(Debug)]
enum DbEvent {
    Insert { key: Value, value: Value },
    Remove { key: Value },
}

static EVENT_POOL: LazyLock<Pool<Vec<DbEvent>>> = LazyLock::new(|| Pool::new(128, 4096));

#[derive(Debug)]
struct DbEvents(GPooled<Vec<DbEvent>>);

impl CustomBuiltinType for DbEvents {}

fn decode_sled_event(key_typ: Option<Typ>, event: sled::Event) -> Option<DbEvent> {
    match event {
        sled::Event::Insert { key, value } => {
            let k = decode_key(key_typ, &key)?;
            let v = decode_value(&value)?;
            Some(DbEvent::Insert { key: k, value: v })
        }
        sled::Event::Remove { key } => {
            let k = decode_key(key_typ, &key)?;
            Some(DbEvent::Remove { key: k })
        }
    }
}

fn drain_ready(
    subscriber: &mut sled::Subscriber,
    key_typ: Option<Typ>,
    events: &mut Vec<DbEvent>,
) {
    let waker = Waker::noop();
    let mut cx = Context::from_waker(&waker);
    loop {
        match Pin::new(&mut *subscriber).poll(&mut cx) {
            Poll::Ready(Some(event)) => {
                if let Some(ev) = decode_sled_event(key_typ, event) {
                    events.push(ev);
                }
            }
            Poll::Ready(None) | Poll::Pending => break,
        }
    }
}

// ── Subscribe (with optional prefix) ──────────────────────────────

#[derive(Debug)]
pub(crate) struct DbSubscribe {
    tree_val: Option<Value>,
    abort: Option<tokio::task::AbortHandle>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for DbSubscribe {
    const NAME: &str = "db_subscription_new";
    const NEEDS_CALLSITE: bool = false;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(DbSubscribe { tree_val: None, abort: None }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for DbSubscribe {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        // from[0] = optional prefix (null = no prefix), from[1] = tree
        let prefix_val = from[0].update(ctx, event);
        let tree_changed = from[1].update(ctx, event);
        let tree_is_new = tree_changed.is_some();
        if let Some(v) = tree_changed {
            self.tree_val = Some(v);
        }
        if self.tree_val.is_none() || (prefix_val.is_none() && !tree_is_new) {
            return None;
        }
        if let Some(Value::Abstract(ref a)) = self.tree_val
            && let Some(tv) = a.downcast_ref::<TreeValue>()
        {
            let tree_inner = tv.inner.clone();
            let key_typ = tree_inner.key_typ;
            let prefix_bytes = match &prefix_val {
                Some(Value::Null) | None => poolshark::global::GPooled::orphan(vec![]),
                Some(pv) => match encode_key(key_typ, pv) {
                    Some(buf) => buf,
                    None => poolshark::global::GPooled::orphan(vec![]),
                },
            };
            let bind_id = BindId::new();
            let (mut tx, rx) = mpsc::channel(10);
            ctx.rt.watch(rx);
            if let Some(abort) = self.abort.take() {
                abort.abort();
            }
            let jh = tokio::task::spawn(async move {
                let mut subscriber = tree_inner.tree.watch_prefix(&*prefix_bytes);
                while let Some(first) = (&mut subscriber).await {
                    let mut events = EVENT_POOL.take();
                    if let Some(ev) = decode_sled_event(key_typ, first) {
                        events.push(ev);
                    }
                    // drain all immediately-ready events
                    drain_ready(&mut subscriber, key_typ, &mut events);
                    if events.is_empty() {
                        continue;
                    }
                    let mut batch: GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>> =
                        CBATCH_POOL.take();
                    batch.push((bind_id, Box::new(DbEvents(events))));
                    if tx.send(batch).await.is_err() {
                        break;
                    }
                }
            });
            self.abort = Some(jh.abort_handle());
            return Some(SUBSCRIPTION_WRAPPER.wrap(SubscriptionValue { bind_id }));
        }
        None
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        if let Some(abort) = self.abort.take() {
            abort.abort();
        }
        self.tree_val = None;
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {
        if let Some(abort) = self.abort.take() {
            abort.abort();
        }
    }
}

// ── Subscription accessors ────────────────────────────────────────

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
    let cbt = event.custom.get(&bid)?;
    let events = (&**cbt as &dyn Any).downcast_ref::<DbEvents>()?;
    let mut vals: LPooled<Vec<Value>> = events.0.iter().filter_map(convert).collect();
    if vals.is_empty() {
        return None;
    }
    Some(Value::Array(ValArray::from_iter_exact(vals.drain(..))))
}

macro_rules! db_event_accessor {
    ($name:ident, $builtin_name:expr, $convert:expr) => {
        #[derive(Debug)]
        pub(crate) struct $name {
            top_id: ExprId,
            cached: CachedVals,
            bind_id: Option<BindId>,
        }

        impl<R: Rt, E: UserEvent> BuiltIn<R, E> for $name {
            const NAME: &str = $builtin_name;
            const NEEDS_CALLSITE: bool = false;

            fn init<'a, 'b, 'c, 'd>(
                _ctx: &'a mut ExecCtx<R, E>,
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
                }))
            }
        }

        impl<R: Rt, E: UserEvent> Apply<R, E> for $name {
            fn update(
                &mut self,
                ctx: &mut ExecCtx<R, E>,
                from: &mut [Node<R, E>],
                event: &mut Event<E>,
            ) -> Option<Value> {
                if self.cached.update(ctx, from, event) {
                    if let Some(bid) = self.bind_id.take() {
                        ctx.rt.unref_var(bid, self.top_id);
                    }
                    let bid = extract_sub_bind_id(self.cached.0.first()?.as_ref()?);
                    if let Some(bid) = bid {
                        ctx.rt.ref_var(bid, self.top_id);
                    }
                    self.bind_id = bid;
                }
                scan_db_events(self.bind_id, event, $convert)
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
                if let Some(bid) = self.bind_id.take() {
                    ctx.rt.unref_var(bid, self.top_id);
                }
                self.cached.clear();
            }

            fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
                if let Some(bid) = self.bind_id {
                    ctx.rt.unref_var(bid, self.top_id);
                }
            }
        }
    };
}

db_event_accessor!(DbOnInsert, "db_subscription_on_insert", |se| match se {
    DbEvent::Insert { key, value } => Some(kv_struct(key.clone(), value.clone())),
    DbEvent::Remove { .. } => None,
});

db_event_accessor!(DbOnRemove, "db_subscription_on_remove", |se| match se {
    DbEvent::Remove { key } => Some(key_struct(key.clone())),
    DbEvent::Insert { .. } => None,
});
