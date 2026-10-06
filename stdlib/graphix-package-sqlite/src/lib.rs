#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::ArcStr;
use graphix_compiler::{
    CompileCtx, ExecCtx, Node, Rt, Scope, UserEvent, errf,
    typ::{FnType, Type},
};
use graphix_package_core::{
    CachedArgsAsync, CachedVals, EvalCachedAsync, extract_cast_type,
};
use netidx_value::{ValArray, Value};
use poolshark::local::LPooled;
// CR claude for eric: [style] The connection's lock is a std::sync::Mutex taken with
// lock().unwrap() (lines 75 and 349), where parking_lot::Mutex is the house default. A
// panic under the lock would poison the connection for good, and every later call would
// fail as "spawn_blocking failed". The comment's reason for std (concurrent
// spawn_blocking calls serialize on it) holds for parking_lot as well. Use
// parking_lot::Mutex and drop the comment; the Arc stays std for impl_abstract_arc!.
// (x-alloc-15)
use std::sync::{Arc, Mutex};

// std::sync::Mutex: concurrent spawn_blocking calls serialize on it.
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

fn sqlite_to_value(v: rusqlite::types::ValueRef<'_>) -> Value {
    match v {
        rusqlite::types::ValueRef::Null => Value::Null,
        rusqlite::types::ValueRef::Integer(i) => Value::I64(i),
        rusqlite::types::ValueRef::Real(f) => Value::F64(f),
        rusqlite::types::ValueRef::Text(s) => {
            // CR claude for eric: [bug] A TEXT value that is not valid UTF-8 (another
            // writer's Latin-1, CAST(blob AS TEXT), char() of a surrogate) reads as ""
            // with no error. The program gets a wrong value, and writing the row back
            // erases the stored bytes. Core's bytes_to_string answers `EncodingError`
            // and rusqlite's own FromSql for String answers Utf8Error, so either fail
            // the query and name the column, or keep the valid content with
            // from_utf8_lossy. map_value also casts error values, so today an error
            // returned from eval reaches the program as `InvalidCast` wrapping the
            // `SqliteError` (try `SELEC 1`). probe:
            // design/review-2026-10-05/repro/http-sqlite-db1-11.gx prints ([{name: "",
            // stored: "436166E9"}], [{stored: ""}]). (http-sqlite-db1-11)
            Value::String(ArcStr::from(std::str::from_utf8(s).unwrap_or("")))
        }
        rusqlite::types::ValueRef::Blob(b) => {
            Value::Bytes(bytes::Bytes::copy_from_slice(b).into())
        }
    }
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
        let mut guard = conn_arc.lock().unwrap();
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

#[derive(Debug, Default, netidx_derive::Pack)]
struct SqliteQueryEv {
    cast_typ: Option<Type>,
}

graphix_package_core::pack_image_state!(SqliteQueryEv);

impl EvalCachedAsync for SqliteQueryEv {
    type Args = (Arc<Mutex<Option<rusqlite::Connection>>>, ArcStr, ValArray);

    const NAME: &str = "sqlite_query";

    fn init<R: Rt, E: UserEvent>(
        _ctx: &mut CompileCtx<R, E>,
        _typ: &FnType,
        resolved: Option<&FnType>,
        _scope: &Scope,
        _from: &[Node<R, E>],
        _top_id: graphix_compiler::expr::ExprId,
    ) -> Self {
        Self { cast_typ: extract_cast_type(resolved) }
    }

    fn typecheck0<R: Rt, E: UserEvent>(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> anyhow::Result<()> {
        Ok(())
    }

    fn typecheck1<R: Rt, E: UserEvent>(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> anyhow::Result<()> {
        self.cast_typ = extract_cast_type(Some(resolved));
        Ok(())
    }

    fn map_value<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        v: Value,
    ) -> Option<Value> {
        match self.cast_typ.as_ref() {
            Some(typ) => Some(typ.cast_value(&ctx.env, v)),
            None => Some(errf!(
                "SqliteError",
                "sqlite::query requires a concrete return type"
            )),
        }
    }

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
                let mut stmt = match conn.prepare_cached(&sql) {
                    Ok(s) => s,
                    Err(e) => return errf!("SqliteError", "{e}"),
                };
                // CR claude for eric: [bug] column_count and column_name are read from
                // the cached statement before it steps, and SQLite re-prepares a cached
                // statement on its first step after a schema change, so rows come back
                // with the new columns under the old labels: after DROP COLUMN b; ADD
                // COLUMN d, the same SELECT * returns {a: 1, b: 3, c: 4} for the row
                // (a, c, d) = (1, 3, 4). When the column count shrank,
                // row.get_ref(*idx).unwrap() (278) panics while with_conn holds the
                // std::sync::Mutex (15, 75), and the poisoned mutex fails every later
                // call on the connection, close (349) included, with spawn_blocking
                // failed: ... PoisonError. Take the columns from the stepped statement
                // (the first row's row.as_ref()), return an error instead of
                // unwrapping, and use parking_lot::Mutex as the db cursor does (the
                // comment at 17 holds for any mutex). probe:
                // design/review-2026-10-05/repro/http-sqlite-db1-18.gx
                // (http-sqlite-db1-18)
                let col_count = stmt.column_count();
                let col_names: LPooled<Vec<ArcStr>> = (0..col_count)
                    .map(|i| ArcStr::from(stmt.column_name(i).unwrap_or("")))
                    .collect();
                // Column order is computed once; rows share the ArcStr clones.
                let mut sorted_cols: LPooled<Vec<(usize, ArcStr)>> = col_names
                    .iter()
                    .enumerate()
                    .map(|(i, name)| (i, name.clone()))
                    .collect();
                sorted_cols.sort_by(|a, b| a.1.cmp(&b.1));
                let mut result_rows: LPooled<Vec<Value>> = LPooled::take();
                let mut rows = match stmt.query(param_refs.as_slice()) {
                    Ok(r) => r,
                    Err(e) => return errf!("SqliteError", "{e}"),
                };
                loop {
                    match rows.next() {
                        Err(e) => return errf!("SqliteError", "{e}"),
                        Ok(None) => break,
                        Ok(Some(row)) => {
                            let mut vals: LPooled<Vec<Value>> = sorted_cols
                                .iter()
                                .map(|(idx, name)| {
                                    Value::Array(
                                        [
                                            Value::String(name.clone()),
                                            sqlite_to_value(row.get_ref(*idx).unwrap()),
                                        ]
                                        .into(),
                                    )
                                })
                                .collect();
                            result_rows.push(Value::Array(ValArray::from_iter_exact(
                                vals.drain(..),
                            )));
                        }
                    }
                }
                Value::Array(ValArray::from_iter_exact(result_rows.drain(..)))
            })
            .await
        }
    }
}

type SqliteQuery = CachedArgsAsync<SqliteQueryEv>;

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
                let mut guard = conn_arc.lock().unwrap();
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
