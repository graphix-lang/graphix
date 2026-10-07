use crate::{
    encoding::{decode_key, decode_value, encode_key},
    tree::{Reaped, abstract_arg, blocking, get_tree_inner},
};
use anyhow::Result;
use graphix_compiler::{ExecCtx, Rt, UserEvent, effects::Effect};
use graphix_package_core::{
    CachedArgs, CachedArgsAsync, CachedVals, EvalCached, EvalCachedAsync,
};
use netidx::publisher::Typ;
use netidx_derive::FromValue;
use netidx_value::{ValArray, Value};
use parking_lot::Mutex;
use poolshark::local::LPooled;
use std::{fmt, ops::Bound, sync::Arc};

/// A cursor's iterator; `None` iterates nothing (a prefix no key can have).
pub(crate) struct CursorInner {
    iter: Reaped<Mutex<Option<sled::Iter>>>,
    key_typ: Option<Typ>,
}

impl CursorInner {
    fn next(&self) -> Result<Option<Value>> {
        match self.iter.lock().as_mut().and_then(|i| i.next()) {
            None => Ok(None),
            Some(r) => {
                let (k, v) = r?;
                let entry = [decode_key(self.key_typ, &k)?, decode_value(&v)?];
                Ok(Some(Value::Array(ValArray::from(entry))))
            }
        }
    }
}

impl fmt::Debug for CursorInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CursorInner").finish()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CursorValue {
    inner: Arc<CursorInner>,
}

graphix_package_core::impl_abstract_arc!(
    CursorValue,
    static CURSOR_WRAPPER = "db::cursor::Cursor"
);

fn wrap_cursor(iter: Option<sled::Iter>, key_typ: Option<Typ>) -> Value {
    let iter = Reaped::new(Mutex::new(iter));
    CURSOR_WRAPPER.wrap(CursorValue { inner: Arc::new(CursorInner { iter, key_typ }) })
}

fn get_cursor(cached: &CachedVals) -> Option<Arc<CursorInner>> {
    abstract_arg::<CursorValue>(cached, 0).map(|c| c.inner.clone())
}

#[derive(Debug, Default)]
pub(crate) struct DbCursorNewEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for DbCursorNewEv {
    const NAME: &str = "db_cursor_new";
    const EFFECT: Effect = Effect::Sync;

    fn eval(&mut self, _ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        let tree = get_tree_inner(from, 1)?;
        let iter = match from.0.first()?.as_ref()? {
            Value::Null => Some(tree.tree.iter()),
            v => encode_key(tree.key_typ, v).ok().map(|p| tree.tree.scan_prefix(&*p)),
        };
        Some(wrap_cursor(iter, tree.key_typ))
    }
}

pub(crate) type DbCursorNew = CachedArgs<DbCursorNewEv>;

/// A range bound's key encoded, `None` when no key can have it.
fn parse_bound(key_typ: Option<Typ>, v: &Value) -> Option<Option<Bound<Vec<u8>>>> {
    #[derive(FromValue)]
    enum Repr {
        Included(Value),
        Excluded(Value),
    }
    let encode = |k: &Value| encode_key(key_typ, k).ok().map(|k| k.to_vec());
    Some(match v.clone().cast_to::<Option<Repr>>().ok()? {
        None => Some(Bound::Unbounded),
        Some(Repr::Included(k)) => encode(&k).map(Bound::Included),
        Some(Repr::Excluded(k)) => encode(&k).map(Bound::Excluded),
    })
}

#[derive(Debug, Default)]
pub(crate) struct DbCursorRangeEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for DbCursorRangeEv {
    const NAME: &str = "db_cursor_range";
    const EFFECT: Effect = Effect::Sync;

    fn eval(&mut self, _ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        let tree = get_tree_inner(from, 2)?;
        let lo = parse_bound(tree.key_typ, from.0.first()?.as_ref()?)?;
        let hi = parse_bound(tree.key_typ, from.0.get(1)?.as_ref()?)?;
        let iter = lo.zip(hi).map(|r| tree.tree.range(r));
        Some(wrap_cursor(iter, tree.key_typ))
    }
}

pub(crate) type DbCursorRange = CachedArgs<DbCursorRangeEv>;

#[derive(Debug, Default)]
pub(crate) struct DbCursorReadEv;

impl EvalCachedAsync for DbCursorReadEv {
    type Args = Arc<CursorInner>;

    const NAME: &str = "db_cursor_read";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.0.get(1)?.as_ref()?;
        get_cursor(cached)
    }

    fn eval(c: Self::Args) -> impl Future<Output = Value> + Send {
        blocking(move || Ok(c.next()?.unwrap_or(Value::Null)))
    }
}

pub(crate) type DbCursorRead = CachedArgsAsync<DbCursorReadEv>;

#[derive(Debug, Default)]
pub(crate) struct DbCursorReadManyEv;

impl EvalCachedAsync for DbCursorReadManyEv {
    type Args = (Arc<CursorInner>, i64);

    const NAME: &str = "db_cursor_read_many";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((get_cursor(cached)?, cached.get::<i64>(1)?))
    }

    fn eval((c, n): Self::Args) -> impl Future<Output = Value> + Send {
        blocking(move || {
            let mut entries: LPooled<Vec<Value>> = LPooled::take();
            for _ in 0..n.max(0) {
                match c.next()? {
                    Some(e) => entries.push(e),
                    None => break,
                }
            }
            Ok(Value::Array(ValArray::from_iter_exact(entries.drain(..))))
        })
    }
}

pub(crate) type DbCursorReadMany = CachedArgsAsync<DbCursorReadManyEv>;

graphix_package_core::unit_image_state!(
    DbCursorNewEv,
    DbCursorReadEv,
    DbCursorReadManyEv,
    DbCursorRangeEv,
);
