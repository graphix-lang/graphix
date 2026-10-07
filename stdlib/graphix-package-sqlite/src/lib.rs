#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::{ArcStr, literal};
use graphix_compiler::errf;
use graphix_package_core::{
    CachedArgsAsync, CachedVals, EvalCachedAsync, ReadFormat, TypedRead,
};
use netidx_value::{ValArray, Value};
use parking_lot::Mutex;
use poolshark::local::LPooled;
use std::sync::Arc;

#[derive(Debug)]
struct ConnectionValue {
    inner: Arc<Mutex<Option<rusqlite::Connection>>>,
}

graphix_package_core::impl_abstract_arc!(
    ConnectionValue,
    static CONNECTION_WRAPPER = "sqlite::Connection"
);

fn get_conn_arc(
    cached: &CachedVals,
    idx: usize,
) -> Option<Arc<Mutex<Option<rusqlite::Connection>>>> {
    match cached.0.get(idx)?.as_ref()? {
        Value::Abstract(a) => {
            let cv = a.downcast_ref::<ConnectionValue>()?;
            Some(cv.inner.clone())
        }
        _ => None,
    }
}

/// A TEXT that is not UTF-8 is refused, naming its column: read as "" it
/// would be lost on write back.
fn sqlite_to_value(
    v: rusqlite::types::ValueRef<'_>,
    col: &str,
) -> std::result::Result<Value, Value> {
    use rusqlite::types::ValueRef;
    Ok(match v {
        ValueRef::Null => Value::Null,
        ValueRef::Integer(i) => Value::I64(i),
        ValueRef::Real(f) => Value::F64(f),
        ValueRef::Text(s) => match std::str::from_utf8(s) {
            Ok(s) => Value::String(ArcStr::from(s)),
            Err(e) => return Err(errf!("SqliteError", "column {col}: {e}")),
        },
        ValueRef::Blob(b) => Value::Bytes(bytes::Bytes::copy_from_slice(b).into()),
    })
}

fn value_to_sqlite(v: &Value) -> rusqlite::types::Value {
    match v {
        Value::I64(i) => rusqlite::types::Value::Integer(*i),
        Value::F64(f) => rusqlite::types::Value::Real(*f),
        Value::String(s) => rusqlite::types::Value::Text(s.to_string()),
        Value::Bytes(b) => rusqlite::types::Value::Blob(b.to_vec()),
        Value::Null => rusqlite::types::Value::Null,
        _ => rusqlite::types::Value::Null,
    }
}

fn collect_params(params: &ValArray) -> LPooled<Vec<rusqlite::types::Value>> {
    params.iter().map(value_to_sqlite).collect()
}

async fn with_conn<F>(conn_arc: Arc<Mutex<Option<rusqlite::Connection>>>, f: F) -> Value
where
    F: FnOnce(&mut rusqlite::Connection) -> Value + Send + 'static,
{
    match tokio::task::spawn_blocking(move || {
        let mut guard = conn_arc.lock();
        match guard.as_mut() {
            Some(conn) => f(conn),
            None => errf!("SqliteError", "connection closed"),
        }
    })
    .await
    {
        Ok(v) => v,
        Err(e) => errf!("SqliteError", "spawn_blocking failed: {e}"),
    }
}

#[derive(Debug, Default)]
struct SqliteOpenEv;

impl EvalCachedAsync for SqliteOpenEv {
    type Args = ArcStr;

    const NAME: &str = "sqlite_open";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(path: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || rusqlite::Connection::open(&*path))
                .await
            {
                Err(e) => errf!("SqliteError", "spawn_blocking failed: {e}"),
                Ok(Err(e)) => errf!("SqliteError", "{e}"),
                Ok(Ok(conn)) => CONNECTION_WRAPPER
                    .wrap(ConnectionValue { inner: Arc::new(Mutex::new(Some(conn))) }),
            }
        }
    }
}

type SqliteOpen = CachedArgsAsync<SqliteOpenEv>;

#[derive(Debug, Default)]
struct SqliteExecEv;

impl EvalCachedAsync for SqliteExecEv {
    type Args = (Arc<Mutex<Option<rusqlite::Connection>>>, ArcStr, ValArray);

    const NAME: &str = "sqlite_exec";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let conn = get_conn_arc(cached, 0)?;
        let sql = cached.get::<ArcStr>(1)?;
        let params = cached.get::<ValArray>(2)?;
        Some((conn, sql, params))
    }

    fn eval((conn_arc, sql, params): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            with_conn(conn_arc, move |conn| {
                let params = collect_params(&params);
                let param_refs: LPooled<Vec<&dyn rusqlite::types::ToSql>> =
                    params.iter().map(|v| v as &dyn rusqlite::types::ToSql).collect();
                match conn.prepare_cached(&sql) {
                    Err(e) => errf!("SqliteError", "{e}"),
                    Ok(mut stmt) => match stmt.execute(param_refs.as_slice()) {
                        Err(e) => errf!("SqliteError", "{e}"),
                        Ok(n) => Value::U64(n as u64),
                    },
                }
            })
            .await
        }
    }
}

type SqliteExec = CachedArgsAsync<SqliteExecEv>;

#[derive(Debug, Default)]
struct SqliteExecBatchEv;

impl EvalCachedAsync for SqliteExecBatchEv {
    type Args = (Arc<Mutex<Option<rusqlite::Connection>>>, ArcStr);

    const NAME: &str = "sqlite_exec_batch";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let conn = get_conn_arc(cached, 0)?;
        let sql = cached.get::<ArcStr>(1)?;
        Some((conn, sql))
    }

    fn eval((conn_arc, sql): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            with_conn(conn_arc, move |conn| match conn.execute_batch(&sql) {
                Err(e) => errf!("SqliteError", "{e}"),
                Ok(()) => Value::Null,
            })
            .await
        }
    }
}

type SqliteExecBatch = CachedArgsAsync<SqliteExecBatchEv>;

#[derive(Debug)]
struct Query;

impl ReadFormat for Query {
    const NAME: &str = "sqlite_query";
    const TAG: ArcStr = literal!("SqliteError");
    type Args = (Arc<Mutex<Option<rusqlite::Connection>>>, ArcStr, ValArray);

    fn prepare_args(cached: &CachedVals) -> Option<Self::Args> {
        let conn = get_conn_arc(cached, 0)?;
        let sql = cached.get::<ArcStr>(1)?;
        let params = cached.get::<ValArray>(2)?;
        Some((conn, sql, params))
    }

    fn parse((conn_arc, sql, params): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            with_conn(conn_arc, move |conn| {
                let params = collect_params(&params);
                let param_refs: LPooled<Vec<&dyn rusqlite::types::ToSql>> =
                    params.iter().map(|v| v as &dyn rusqlite::types::ToSql).collect();
                let mut stmt = match conn.prepare_cached(&sql) {
                    Ok(s) => s,
                    Err(e) => return errf!("SqliteError", "{e}"),
                };
                let mut rows = match stmt.query(param_refs.as_slice()) {
                    Ok(r) => r,
                    Err(e) => return errf!("SqliteError", "{e}"),
                };
                // the columns of the statement as stepped: a schema change
                // re-prepares it on its first step
                let mut sorted_cols: LPooled<Vec<(usize, ArcStr)>> = LPooled::take();
                let mut result_rows: LPooled<Vec<Value>> = LPooled::take();
                loop {
                    let row = match rows.next() {
                        Err(e) => return errf!("SqliteError", "{e}"),
                        Ok(None) => break,
                        Ok(Some(row)) => row,
                    };
                    if result_rows.is_empty() {
                        let stmt = row.as_ref();
                        sorted_cols.extend((0..stmt.column_count()).map(|i| {
                            (i, ArcStr::from(stmt.column_name(i).unwrap_or("")))
                        }));
                        sorted_cols.sort_by(|a, b| a.1.cmp(&b.1));
                    }
                    let mut vals: LPooled<Vec<Value>> = LPooled::take();
                    for (idx, name) in sorted_cols.iter() {
                        let v = match row.get_ref(*idx) {
                            Ok(v) => v,
                            Err(e) => return errf!("SqliteError", "{e}"),
                        };
                        let v = match sqlite_to_value(v, name) {
                            Ok(v) => v,
                            Err(e) => return e,
                        };
                        vals.push(Value::Array([Value::String(name.clone()), v].into()));
                    }
                    result_rows
                        .push(Value::Array(ValArray::from_iter_exact(vals.drain(..))));
                }
                Value::Array(ValArray::from_iter_exact(result_rows.drain(..)))
            })
            .await
        }
    }
}

type SqliteQuery = CachedArgsAsync<TypedRead<Query>>;

macro_rules! simple_sql_builtin {
    ($ev_name:ident, $type_name:ident, $builtin_name:literal, $sql:literal) => {
        #[derive(Debug, Default)]
        struct $ev_name;

        impl EvalCachedAsync for $ev_name {
            type Args = Arc<Mutex<Option<rusqlite::Connection>>>;

            const NAME: &str = $builtin_name;

            fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
                get_conn_arc(cached, 0)
            }

            fn eval(conn_arc: Self::Args) -> impl Future<Output = Value> + Send {
                async move {
                    with_conn(conn_arc, |conn| match conn.execute_batch($sql) {
                        Err(e) => errf!("SqliteError", "{e}"),
                        Ok(()) => Value::Null,
                    })
                    .await
                }
            }
        }

        type $type_name = CachedArgsAsync<$ev_name>;

        graphix_package_core::unit_image_state!($ev_name);
    };
}

simple_sql_builtin!(SqliteBeginEv, SqliteBegin, "sqlite_begin", "BEGIN");
simple_sql_builtin!(SqliteCommitEv, SqliteCommit, "sqlite_commit", "COMMIT");
simple_sql_builtin!(SqliteRollbackEv, SqliteRollback, "sqlite_rollback", "ROLLBACK");

#[derive(Debug, Default)]
struct SqliteCloseEv;

impl EvalCachedAsync for SqliteCloseEv {
    type Args = Arc<Mutex<Option<rusqlite::Connection>>>;

    const NAME: &str = "sqlite_close";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_conn_arc(cached, 0)
    }

    fn eval(conn_arc: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let mut guard = conn_arc.lock();
                match guard.take() {
                    Some(conn) => {
                        drop(conn);
                        Value::Null
                    }
                    None => errf!("SqliteError", "connection already closed"),
                }
            })
            .await
            {
                Ok(v) => v,
                Err(e) => errf!("SqliteError", "spawn_blocking failed: {e}"),
            }
        }
    }
}

type SqliteClose = CachedArgsAsync<SqliteCloseEv>;

graphix_package_core::unit_image_state!(
    SqliteOpenEv,
    SqliteExecEv,
    SqliteExecBatchEv,
    SqliteCloseEv,
);

graphix_derive::defpackage! {
    builtins => [
        SqliteOpen,
        SqliteExec,
        SqliteExecBatch,
        SqliteQuery,
        SqliteBegin,
        SqliteCommit,
        SqliteRollback,
        SqliteClose,
    ],
}
