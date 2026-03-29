use crate::encoding::{decode_key, decode_value, encode_key};
use crate::tree::{get_tree_inner, TreeInner};
use arcstr::ArcStr;
use graphix_compiler::errf;
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx::publisher::Typ;
use netidx_value::{ValArray, Value};
use poolshark::local::LPooled;
use std::{fmt, sync::Arc};

// ── Cursor types ──────────────────────────────────────────────────

pub(crate) struct CursorInner {
    iter: parking_lot::Mutex<sled::Iter>,
    key_typ: Option<Typ>,
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

graphix_package_core::impl_abstract_arc!(CursorValue, static CURSOR_WRAPPER = [
    0xd3, 0xe4, 0xf5, 0x06, 0x17, 0x28, 0x49, 0x3a,
    0x4b, 0x5c, 0x6d, 0x7e, 0x8f, 0xa0, 0xb1, 0xc2,
]);

// ── Builtins ──────────────────────────────────────────────────────

// -- DbCursorNew (with optional prefix) --

#[derive(Debug, Default)]
pub(crate) struct DbCursorNewEv;

impl EvalCachedAsync for DbCursorNewEv {
    const NAME: &str = "db_cursor_new";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Option<Value>, Arc<TreeInner>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let prefix_val = match cached.0.get(0)?.as_ref()? {
            Value::Null => None,
            v => Some(v.clone()),
        };
        let tree = get_tree_inner(cached, 1)?;
        Some((prefix_val, tree))
    }

    fn eval((prefix_val, tree): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let iter = match prefix_val {
                    Some(ref pv) => match encode_key(tree.key_typ, pv) {
                        Some(encoded) => tree.tree.scan_prefix(&*encoded),
                        None => tree.tree.iter(),
                    },
                    None => tree.tree.iter(),
                };
                CURSOR_WRAPPER.wrap(CursorValue {
                    inner: Arc::new(CursorInner {
                        iter: parking_lot::Mutex::new(iter),
                        key_typ: tree.key_typ,
                    }),
                })
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(v) => v,
            }
        }
    }
}

pub(crate) type DbCursorNew = CachedArgsAsync<DbCursorNewEv>;

// -- DbCursorRead --

#[derive(Debug, Default)]
pub(crate) struct DbCursorReadEv;

impl EvalCachedAsync for DbCursorReadEv {
    const NAME: &str = "db_cursor_read";
    const NEEDS_CALLSITE: bool = false;
    type Args = Arc<CursorInner>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.0.get(1)?.as_ref()?;
        match cached.0.get(0)?.as_ref()? {
            Value::Abstract(a) => {
                a.downcast_ref::<CursorValue>().map(|c| c.inner.clone())
            }
            _ => None,
        }
    }

    fn eval(inner: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let key_typ = inner.key_typ;
                inner.iter.lock().next().map(|r| {
                    r.map(|(k, v)| {
                        let key = decode_key(key_typ, &k);
                        let val = decode_value(&v);
                        (key, val)
                    })
                })
            })
            .await
            {
                Ok(Some(Ok((Some(k), Some(v))))) => Value::Array(ValArray::from([k, v])),
                Ok(Some(Ok(_))) => errf!("DbErr", "failed to decode entry"),
                Ok(Some(Err(e))) => errf!("DbErr", "{e}"),
                Ok(None) => Value::Null,
                Err(e) => errf!("DbErr", "task panicked: {e}"),
            }
        }
    }
}

pub(crate) type DbCursorRead = CachedArgsAsync<DbCursorReadEv>;

// -- DbCursorReadMany --

#[derive(Debug, Default)]
pub(crate) struct DbCursorReadManyEv;

impl EvalCachedAsync for DbCursorReadManyEv {
    const NAME: &str = "db_cursor_read_many";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Arc<CursorInner>, i64);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let n = match cached.0.get(1)?.as_ref()? {
            Value::I64(n) => *n,
            _ => return None,
        };
        match cached.0.get(0)?.as_ref()? {
            Value::Abstract(a) => {
                a.downcast_ref::<CursorValue>().map(|c| (c.inner.clone(), n))
            }
            _ => None,
        }
    }

    fn eval((inner, count): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let n = count.max(0) as usize;
            match tokio::task::spawn_blocking(move || {
                let key_typ = inner.key_typ;
                let mut results: LPooled<Vec<Value>> = LPooled::take();
                let mut iter = inner.iter.lock();
                for _ in 0..n {
                    match iter.next() {
                        None => break,
                        Some(Err(e)) => return Err(errf!("DbErr", "{e}")),
                        Some(Ok((k, v))) => {
                            match (decode_key(key_typ, &k), decode_value(&v)) {
                                (Some(k), Some(v)) => {
                                    results.push(Value::Array(ValArray::from([k, v])));
                                }
                                _ => {
                                    return Err(errf!("DbErr", "failed to decode entry"))
                                }
                            }
                        }
                    }
                }
                Ok(Value::Array(ValArray::from_iter_exact(results.drain(..))))
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => e,
                Ok(Ok(v)) => v,
            }
        }
    }
}

pub(crate) type DbCursorReadMany = CachedArgsAsync<DbCursorReadManyEv>;

// -- DbCursorRange --

fn parse_bound(key_typ: Option<Typ>, v: &Value) -> Option<std::ops::Bound<Vec<u8>>> {
    use std::ops::Bound;
    match v {
        Value::Null => Some(Bound::Unbounded),
        Value::Array(a) if a.len() >= 2 => match &a[0] {
            Value::String(tag) if &**tag == "Included" => {
                Some(Bound::Included(encode_key(key_typ, &a[1])?.drain(..).collect()))
            }
            Value::String(tag) if &**tag == "Excluded" => {
                Some(Bound::Excluded(encode_key(key_typ, &a[1])?.drain(..).collect()))
            }
            _ => None,
        },
        _ => None,
    }
}

#[derive(Debug)]
pub(crate) struct RangeArgs {
    lo: std::ops::Bound<Vec<u8>>,
    hi: std::ops::Bound<Vec<u8>>,
    tree: Arc<TreeInner>,
}

#[derive(Debug, Default)]
pub(crate) struct DbCursorRangeEv;

impl EvalCachedAsync for DbCursorRangeEv {
    const NAME: &str = "db_cursor_range";
    const NEEDS_CALLSITE: bool = false;
    type Args = RangeArgs;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tree = get_tree_inner(cached, 2)?;
        let lo = parse_bound(tree.key_typ, cached.0.get(0)?.as_ref()?)?;
        let hi = parse_bound(tree.key_typ, cached.0.get(1)?.as_ref()?)?;
        Some(RangeArgs { lo, hi, tree })
    }

    fn eval(args: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let RangeArgs { lo, hi, tree } = args;
            let key_typ = tree.key_typ;
            match tokio::task::spawn_blocking(move || {
                let iter = tree.tree.range((lo, hi));
                CURSOR_WRAPPER.wrap(CursorValue {
                    inner: Arc::new(CursorInner {
                        iter: parking_lot::Mutex::new(iter),
                        key_typ,
                    }),
                })
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(v) => v,
            }
        }
    }
}

pub(crate) type DbCursorRange = CachedArgsAsync<DbCursorRangeEv>;
