#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::{ArcStr, literal};
use graphix_compiler::errf;
use graphix_package_core::{
    CachedArgsAsync, CachedVals, EvalCachedAsync, ReadFormat, Reaped, TypedRead,
};
use netidx_value::{ValArray, Value};
use parking_lot::Mutex;
use poolshark::local::LPooled;
use std::sync::Arc;

/// A connection, closed on a blocking thread when the last handle goes:
/// closing a WAL database checkpoints and syncs.
type Conn = Arc<Reaped<Mutex<Option<rusqlite::Connection>>>>;

#[derive(Debug)]
struct ConnectionValue {
    inner: Conn,
}

graphix_package_core::impl_abstract_arc!(
    ConnectionValue,
    static CONNECTION_WRAPPER = "sqlite::Connection"
);

fn get_conn_arc(cached: &CachedVals, idx: usize) -> Option<Conn> {
    match cached.value(idx)? {
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

/// A parameter bound as it stands, borrowed: an `SqlVal` member, refused
/// otherwise.
struct Param<'a>(&'a Value);

impl rusqlite::types::ToSql for Param<'_> {
    fn to_sql(&self) -> rusqlite::Result<rusqlite::types::ToSqlOutput<'_>> {
        use rusqlite::types::{ToSqlOutput::Borrowed, ValueRef};
        Ok(Borrowed(match self.0 {
            Value::I64(i) => ValueRef::Integer(*i),
            Value::F64(f) => ValueRef::Real(*f),
            Value::String(s) => ValueRef::Text(s.as_bytes()),
            Value::Bytes(b) => ValueRef::Blob(b),
            Value::Null => ValueRef::Null,
            v => {
                return Err(rusqlite::Error::ToSqlConversionFailure(
                    format!("{v} is not an SqlVal").into(),
                ));
            }
        }))
    }
}

/// `ps`, bound borrowed.
fn params(ps: &ValArray) -> impl rusqlite::Params + '_ {
    rusqlite::params_from_iter(ps.iter().map(Param))
}

async fn with_conn<F>(conn_arc: Conn, f: F) -> Value
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
                Ok(Ok(conn)) => CONNECTION_WRAPPER.wrap(ConnectionValue {
                    inner: Arc::new(Reaped::new(Mutex::new(Some(conn)))),
                }),
            }
        }
    }
}

type SqliteOpen = CachedArgsAsync<SqliteOpenEv>;

#[derive(Debug, Default)]
struct SqliteExecEv;

impl EvalCachedAsync for SqliteExecEv {
    type Args = (Conn, ArcStr, ValArray);

    const NAME: &str = "sqlite_exec";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let conn = get_conn_arc(cached, 0)?;
        let sql = cached.get::<ArcStr>(1)?;
        let params = cached.get::<ValArray>(2)?;
        Some((conn, sql, params))
    }

    fn eval((conn_arc, sql, params): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            with_conn(conn_arc, move |conn| match conn.prepare_cached(&sql) {
                Err(e) => errf!("SqliteError", "{e}"),
                Ok(mut stmt) => match stmt.execute(self::params(&params)) {
                    Err(e) => errf!("SqliteError", "{e}"),
                    Ok(n) => Value::U64(n as u64),
                },
            })
            .await
        }
    }
}

type SqliteExec = CachedArgsAsync<SqliteExecEv>;

#[derive(Debug, Default)]
struct SqliteExecBatchEv;

impl EvalCachedAsync for SqliteExecBatchEv {
    type Args = (Conn, ArcStr);

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
    const FLAT_LISTS: bool = true;
    type Args = (Conn, ArcStr, ValArray);

    fn prepare_args(cached: &CachedVals) -> Option<Self::Args> {
        let conn = get_conn_arc(cached, 0)?;
        let sql = cached.get::<ArcStr>(1)?;
        let params = cached.get::<ValArray>(2)?;
        Some((conn, sql, params))
    }

    fn parse((conn_arc, sql, params): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            with_conn(conn_arc, move |conn| {
                let mut stmt = match conn.prepare_cached(&sql) {
                    Ok(s) => s,
                    Err(e) => return errf!("SqliteError", "{e}"),
                };
                let mut rows = match stmt.query(self::params(&params)) {
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
                        // a row reads as a struct or a map by column name
                        if let Some(w) = sorted_cols.windows(2).find(|w| w[0].1 == w[1].1) {
                            return errf!(
                                "SqliteError",
                                "two columns are named {}: give each its own name with AS",
                                w[0].1
                            );
                        }
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
            type Args = Conn;

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
    type Args = Conn;

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
