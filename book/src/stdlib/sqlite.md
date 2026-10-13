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

Calls on one connection are independent async calls, ordered only when
something sequences them: here a `seq`, so the table exists before the
insert and the row before the queries.

```graphix
{{#include ../examples/stdlib/sqlite_query.gx}}
```
