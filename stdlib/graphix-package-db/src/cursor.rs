use crate::{
    encoding::{decode_key, decode_value, encode_key, encode_prefix},
    tree::{Reaped, Tree, blocking, get_tree_inner},
};
use anyhow::{Result, bail};
use graphix_compiler::{ExecCtx, Rt, effects::Effect, rep};
use graphix_package_core::{
    CachedArgs, CachedArgsAsync, CachedVals, EvalCached, EvalCachedAsync,
    write_marked_data,
};
use netidx::publisher::Typ;
use netidx_derive::FromValue;
use netidx_value::{ValArray, Value};
use parking_lot::Mutex;
use poolshark::local::LPooled;
use std::{fmt, ops::Bound, sync::Arc};

/// A cursor's iterator; `None` iterates nothing (a prefix no key can
/// have), and an error, every read answers (a key that cannot be written
/// as data).
pub(crate) struct CursorInner {
    iter: Mutex<Result<Option<sled::Iter>>>,
    tree: Tree,
}

impl CursorInner {
    fn next(&self) -> Result<Option<Value>> {
        let next = match &mut *self.iter.lock() {
            Err(e) => bail!("{e:#}"),
            Ok(iter) => iter.as_mut().and_then(|i| i.next()),
        };
        match next {
            None => Ok(None),
            Some(r) => {
                let (k, v) = r?;
                let entry = [decode_key(self.tree.key_typ, &k)?, decode_value(&v)?];
                Ok(Some(rep::tuple(entry)))
            }
        }
    }
}

impl fmt::Debug for CursorInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CursorInner").finish()
    }
}

type Cursor = Arc<Reaped<CursorInner>>;

#[derive(Debug, Clone)]
pub(crate) struct CursorValue {
    inner: Cursor,
}

graphix_package_core::impl_abstract_arc!(
    CursorValue,
    static CURSOR_WRAPPER = "db::cursor::Cursor"
);

fn wrap_cursor(iter: Result<Option<sled::Iter>>, tree: Tree) -> Value {
    CURSOR_WRAPPER.wrap(CursorValue {
        inner: Arc::new(Reaped::new(CursorInner { iter: Mutex::new(iter), tree })),
    })
}

fn get_cursor(cached: &CachedVals) -> Option<Cursor> {
    cached.abstract_arg::<CursorValue>(0).map(|c| c.inner.clone())
}

#[derive(Debug, Default)]
pub(crate) struct DbCursorNewEv;

impl<R: Rt> EvalCached<R> for DbCursorNewEv {
    const NAME: &str = "db_cursor_new";
    const EFFECT: Effect = Effect::Sync;

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R>, from: &CachedVals) -> Option<Value> {
        let tree = get_tree_inner(from, 1)?;
        let iter = write_marked_data(ctx, from.value(0)?).map(|prefix| match prefix {
            Value::Null => Some(tree.tree.iter()),
            v => encode_prefix(tree.key_typ, &v).ok().map(|p| tree.tree.scan_prefix(&*p)),
        });
        Some(wrap_cursor(iter, tree))
    }
}

pub(crate) type DbCursorNew = CachedArgs<DbCursorNewEv>;

/// A range bound's key encoded, `None` when no key can have it.
fn parse_bound(key_typ: Option<Typ>, v: &Value) -> Option<Option<Bound<Vec<u8>>>> {
    #[derive(FromValue)]
    #[value(shape = "graphix_compiler::rep::GraphixShape")]
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

impl<R: Rt> EvalCached<R> for DbCursorRangeEv {
    const NAME: &str = "db_cursor_range";
    const EFFECT: Effect = Effect::Sync;

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R>, from: &CachedVals) -> Option<Value> {
        let tree = get_tree_inner(from, 2)?;
        let lo = write_marked_data(ctx, from.value(0)?);
        let hi = write_marked_data(ctx, from.value(1)?);
        let iter = match (lo, hi) {
            (Err(e), _) | (_, Err(e)) => Err(e),
            (Ok(lo), Ok(hi)) => {
                let lo = parse_bound(tree.key_typ, &lo)?;
                let hi = parse_bound(tree.key_typ, &hi)?;
                Ok(lo.zip(hi).map(|r| tree.tree.range(r)))
            }
        };
        Some(wrap_cursor(iter, tree))
    }
}

pub(crate) type DbCursorRange = CachedArgs<DbCursorRangeEv>;

#[derive(Debug, Default)]
pub(crate) struct DbCursorReadEv;

impl EvalCachedAsync for DbCursorReadEv {
    type Args = Cursor;

    const NAME: &str = "db_cursor_read";
    const CANCEL_SAFE: bool = false;
    const DATA_RESULT: bool = true;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.value(1)?;
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
    type Args = (Cursor, i64);

    const NAME: &str = "db_cursor_read_many";
    const CANCEL_SAFE: bool = false;
    const DATA_RESULT: bool = true;

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

#[cfg(test)]
mod test {
    use super::*;
    use crate::tree::{TreeValue, open_db, wrap_tree};

    #[tokio::test]
    async fn cursors_keep_the_database_registered() -> Result<()> {
        let dir = tempfile::tempdir()?;
        let path = dir.path().to_str().unwrap();
        let db = open_db(path.into()).await?;
        let weak = Arc::downgrade(&db);
        let value = wrap_tree(db.clone(), db.open_tree("t")?, Some(Typ::String));
        let Value::Abstract(value) = value else { panic!("expected tree") };
        let tree = value.downcast_ref::<TreeValue>().unwrap().inner.clone();
        let tree_weak = Arc::downgrade(&tree);
        let cursor = wrap_cursor(Ok(Some(tree.tree.iter())), tree);
        drop(value);
        assert!(tree_weak.upgrade().is_some(), "cursor must retain its tree");
        drop(db);
        let owner = weak.upgrade().expect("cursor must retain the registered database");
        assert!(Arc::ptr_eq(&owner, &open_db(path.into()).await?));
        drop(cursor);
        Ok(())
    }
}
