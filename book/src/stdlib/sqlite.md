# sqlite

The `sqlite` module provides SQLite database access. `sqlite::query`
uses type-directed deserialization — annotate the result type to
control how rows are deserialized.

```graphix
/// A SQLite value: integer, float, string, bytes, or null.
type SqlVal = [i64, f64, string, bytes, null];

/// An opaque SQLite connection handle.
type Connection;

/// Open (or create) a SQLite database. Use ":memory:" for in-memory.
val open: fn(string) -> Result<Connection, `SqliteError(string)>;

/// Execute a non-returning statement (INSERT/UPDATE/DELETE/DDL) with params. Returns rows affected.
val exec: fn(Connection, string, Array<SqlVal>) -> Result<u64, `SqliteError(string)>;

/// Execute multiple semicolon-separated statements (no params). Good for schema setup.
val exec_batch: fn(Connection, string) -> Result<null, `SqliteError(string)>;

/// Query rows, deserializing each into the annotated type.
/// Annotate as Array<{...}> for typed structs, or Array<Map<string, SqlVal>> for raw maps.
val query: fn(Connection, string, Array<SqlVal>) -> Result<Array<'a>, [`SqliteError(string), `InvalidCast(string)]>;

/// Begin a transaction.
val begin: fn(Connection) -> Result<null, `SqliteError(string)>;

/// Commit the current transaction.
val commit: fn(Connection) -> Result<null, `SqliteError(string)>;

/// Rollback the current transaction.
val rollback: fn(Connection) -> Result<null, `SqliteError(string)>;

/// Close the connection explicitly (optional — connections close on drop).
val close: fn(Connection) -> Result<null, `SqliteError(string)>;
```

## Type-directed queries

The return type of `sqlite::query` determines how rows are deserialized.
Use struct types for named columns, or `Map<string, SqlVal>` for raw access.

```graphix
use sqlite;

let conn = sqlite::open(":memory:")?;
sqlite::exec_batch(conn, "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)")?;
sqlite::exec(conn, "INSERT INTO users VALUES (?, ?, ?)", [1, "Alice", 30])?;

// typed struct results
let users: Array<{id: i64, name: string, age: i64}> =
    sqlite::query(conn, "SELECT * FROM users", [])?;

// raw map results
let raw: Array<Map<string, SqlVal>> =
    sqlite::query(conn, "SELECT * FROM users", [])?;
```
