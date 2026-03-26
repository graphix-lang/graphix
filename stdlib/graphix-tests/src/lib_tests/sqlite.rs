use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

run!(sqlite_open_memory, r#"{
    let db = sqlite::open(":memory:")?;
    sqlite::close(db)?;
    true
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// typed struct query: exec_batch creates schema, query reads back as structs
run!(sqlite_typed_query, r#"{
    let db = sqlite::open(":memory:")$;
    let setup = sqlite::exec_batch(db, "
        CREATE TABLE t(id INTEGER PRIMARY KEY, name TEXT);
        INSERT INTO t(id, name) VALUES(1, 'alice');
        INSERT INTO t(id, name) VALUES(2, 'bob');
    ")$;
    let rows: Array<{id: i64, name: string}> = sqlite::query(setup ~ db, "SELECT id, name FROM t ORDER BY id", [])$;
    (rows[0]$).name == "alice" && (rows[1]$).name == "bob" && (rows[0]$).id == 1
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// raw map query: same data, but annotated as Map
run!(sqlite_raw_map_query, r#"{
    type SqlVal = [i64, f64, string, bytes, null];
    let db = sqlite::open(":memory:")$;
    let setup = sqlite::exec_batch(db, "
        CREATE TABLE t(id INTEGER PRIMARY KEY, name TEXT);
        INSERT INTO t(id, name) VALUES(1, 'alice');
        INSERT INTO t(id, name) VALUES(2, 'bob');
    ")$;
    let rows: Array<Map<string, SqlVal>> = sqlite::query(setup ~ db, "SELECT id, name FROM t ORDER BY id", [])$;
    let r0 = rows[0]$;
    let r1 = rows[1]$;
    let n0 = r0{"name"}$;
    let n1 = r1{"name"}$;
    cast<string>(n0)? == "alice" && cast<string>(n1)? == "bob"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// exec with params, verify via typed query
run!(sqlite_exec_params, r#"{
    let db = sqlite::open(":memory:")$;
    let setup = sqlite::exec_batch(db, "CREATE TABLE t(id INTEGER PRIMARY KEY, val REAL)")$;
    let inserted = sqlite::exec(setup ~ db, "INSERT INTO t(id, val) VALUES(?, ?)", [1, 3.14])$;
    let rows: Array<{id: i64, val: f64}> = sqlite::query(inserted ~ db, "SELECT id, val FROM t", [])$;
    (rows[0]$).id == 1
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// transaction commit
run!(sqlite_transaction, r#"{
    let db = sqlite::open(":memory:")$;
    let setup = sqlite::exec_batch(db, "
        CREATE TABLE t(x INTEGER);
        BEGIN;
        INSERT INTO t(x) VALUES(10);
        INSERT INTO t(x) VALUES(20);
        COMMIT;
    ")$;
    let rows: Array<{x: i64}> = sqlite::query(setup ~ db, "SELECT x FROM t ORDER BY x", [])$;
    array::len(rows) == 2
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// rollback
run!(sqlite_rollback, r#"{
    let db = sqlite::open(":memory:")$;
    let setup = sqlite::exec_batch(db, "
        CREATE TABLE t(x INTEGER);
        INSERT INTO t(x) VALUES(1);
        BEGIN;
        INSERT INTO t(x) VALUES(2);
        ROLLBACK;
    ")$;
    let rows: Array<{x: i64}> = sqlite::query(setup ~ db, "SELECT x FROM t", [])$;
    array::len(rows) == 1
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// nullable fields: [i64, null] for a column that may be NULL
run!(sqlite_nullable_field, r#"{
    let db = sqlite::open(":memory:")$;
    let setup = sqlite::exec_batch(db, "
        CREATE TABLE t(x INTEGER);
        INSERT INTO t(x) VALUES(42);
        INSERT INTO t(x) VALUES(NULL);
    ")$;
    let rows: Array<{x: [i64, null]}> = sqlite::query(setup ~ db, "SELECT x FROM t ORDER BY rowid", [])$;
    let first = (rows[0]$).x;
    let second = (rows[1]$).x;
    let a = select first { i64 as n => n == 42, _ => false };
    let b = select second { null as _ => true, _ => false };
    a && b
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// empty result: typed query on empty table returns empty array
run!(sqlite_empty_result, r#"{
    let db = sqlite::open(":memory:")$;
    let setup = sqlite::exec_batch(db, "CREATE TABLE t(x INTEGER)")$;
    let rows: Array<{x: i64}> = sqlite::query(setup ~ db, "SELECT x FROM t", [])$;
    array::len(rows) == 0
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});
