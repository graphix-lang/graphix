#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::bail;
use arcstr::ArcStr;
use graphix_compiler::errf;
use graphix_compiler::typ::{FnType, Type};
use graphix_compiler::{ExecCtx, Node, Rt, Scope, TypecheckPhase, UserEvent};
use graphix_package_core::{
    extract_cast_type, CachedArgsAsync, CachedVals, EvalCachedAsync,
};
use netidx_value::{ValArray, Value};
use poolshark::local::LPooled;
use std::sync::{Arc, Mutex};

// ── ConnectionValue ───────────────────────────────────────────────

// Uses std::sync::Mutex so concurrent spawn_blocking calls serialize
// on the lock rather than racing.
#[derive(Debug)]
struct ConnectionValue {
    inner: Arc<Mutex<Option<rusqlite::Connection>>>,
}

graphix_package_core::impl_abstract_arc!(ConnectionValue, static CONNECTION_WRAPPER = [
    0x5e, 0x07, 0xd4, 0x1a, 0xbd, 0x33, 0x41, 0x94,
    0x8d, 0xc4, 0x85, 0xf9, 0x44, 0x85, 0xfc, 0x3e,
]);

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

// ── Value conversion ──────────────────────────────────────────────

fn sqlite_to_value(v: rusqlite::types::ValueRef<'_>) -> Value {
    match v {
        rusqlite::types::ValueRef::Null => Value::Null,
        rusqlite::types::ValueRef::Integer(i) => Value::I64(i),
        rusqlite::types::ValueRef::Real(f) => Value::F64(f),
        rusqlite::types::ValueRef::Text(s) => {
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

// ── Helper: lock inside spawn_blocking ────────────────────────────

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

// ── SqliteOpen ────────────────────────────────────────────────────

#[derive(Debug, Default)]
struct SqliteOpenEv;

impl EvalCachedAsync for SqliteOpenEv {
    const NAME: &str = "sqlite_open";
    const NEEDS_CALLSITE: bool = false;
    type Args = ArcStr;

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

// ── SqliteExec ────────────────────────────────────────────────────

#[derive(Debug, Default)]
struct SqliteExecEv;

impl EvalCachedAsync for SqliteExecEv {
    const NAME: &str = "sqlite_exec";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Arc<Mutex<Option<rusqlite::Connection>>>, ArcStr, ValArray);

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

// ── SqliteExecBatch ───────────────────────────────────────────────

#[derive(Debug, Default)]
struct SqliteExecBatchEv;

impl EvalCachedAsync for SqliteExecBatchEv {
    const NAME: &str = "sqlite_exec_batch";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Arc<Mutex<Option<rusqlite::Connection>>>, ArcStr);

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

// ── SqliteQuery ───────────────────────────────────────────────────

#[derive(Debug, Default)]
struct SqliteQueryEv {
    cast_typ: Option<Type>,
}

impl EvalCachedAsync for SqliteQueryEv {
    const NAME: &str = "sqlite_query";
    const NEEDS_CALLSITE: bool = true;
    type Args = (Arc<Mutex<Option<rusqlite::Connection>>>, ArcStr, ValArray);

    fn init<R: Rt, E: UserEvent>(
        _ctx: &mut ExecCtx<R, E>,
        _typ: &FnType,
        resolved: Option<&FnType>,
        _scope: &Scope,
        _from: &[Node<R, E>],
        _top_id: graphix_compiler::expr::ExprId,
    ) -> Self {
        Self { cast_typ: extract_cast_type(resolved) }
    }

    fn typecheck<R: Rt, E: UserEvent>(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        phase: TypecheckPhase<'_>,
    ) -> anyhow::Result<()> {
        match phase {
            TypecheckPhase::Lambda => Ok(()),
            TypecheckPhase::CallSite(resolved) => {
                self.cast_typ = extract_cast_type(Some(resolved));
                if self.cast_typ.is_none() {
                    bail!("sqlite::query requires a concrete return type annotation")
                }
                Ok(())
            }
        }
    }

    fn map_value<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
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
                let col_count = stmt.column_count();
                let col_names: LPooled<Vec<ArcStr>> = (0..col_count)
                    .map(|i| ArcStr::from(stmt.column_name(i).unwrap_or("")))
                    .collect();
                // Pre-compute sorted column order for struct-format rows.
                // Each row reuses the same ArcStr clones (just refcount bumps).
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

// ── Transaction builtins ──────────────────────────────────────────

macro_rules! simple_sql_builtin {
    ($ev_name:ident, $type_name:ident, $builtin_name:literal, $sql:literal) => {
        #[derive(Debug, Default)]
        struct $ev_name;

        impl EvalCachedAsync for $ev_name {
            const NAME: &str = $builtin_name;
            const NEEDS_CALLSITE: bool = false;
            type Args = Arc<Mutex<Option<rusqlite::Connection>>>;

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
    };
}

simple_sql_builtin!(SqliteBeginEv, SqliteBegin, "sqlite_begin", "BEGIN");
simple_sql_builtin!(SqliteCommitEv, SqliteCommit, "sqlite_commit", "COMMIT");
simple_sql_builtin!(SqliteRollbackEv, SqliteRollback, "sqlite_rollback", "ROLLBACK");

// ── SqliteClose ───────────────────────────────────────────────────

#[derive(Debug, Default)]
struct SqliteCloseEv;

impl EvalCachedAsync for SqliteCloseEv {
    const NAME: &str = "sqlite_close";
    const NEEDS_CALLSITE: bool = false;
    type Args = Arc<Mutex<Option<rusqlite::Connection>>>;

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

// ── Package registration ─────────────────────────────────────────

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
