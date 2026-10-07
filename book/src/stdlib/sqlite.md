# sqlite

The `sqlite` module provides SQLite database access. `sqlite::query`
uses type-directed deserialization — annotate the result type to
control how rows are deserialized.

```graphix
{{#include ../../../stdlib/graphix-package-sqlite/src/graphix/mod.gxi}}
```

SQL is written in an ordinary Graphix string, and an ordinary string
interpolates every `[..]` in it. Pass values through the `?` params,
never by splicing them into the text: `"... WHERE name = '[name]'"` is
SQL injection. SQL that itself holds a `[` (a JSON path such as
`'$.items[0]'`) is written as a raw string `r"..."` or a template
`"""..."""`, where brackets are content.

## Type-directed queries

The return type of `sqlite::query` determines how rows are deserialized.
Use struct types for named columns, or `Map<string, SqlVal>` for raw access.

```graphix

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
