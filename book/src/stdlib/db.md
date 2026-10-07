# db

The `db` module provides an embedded key-value database (backed by sled)
with ACID transactions, typed trees, cursors, and reactive subscriptions.
A transaction's commit is on disk when it answers.

Tree key and value types are tracked at both compile time and run time —
if a tree is reopened with different types, `db::tree` returns a `DbErr`.

## Interface

```graphix
{{#include ../../../stdlib/graphix-package-db/src/graphix/mod.gxi}}
```

## db::cursor

Cursors iterate over tree entries reactively, advancing on each trigger.

```graphix
{{#include ../../../stdlib/graphix-package-db/src/graphix/cursor.gxi}}
```

## db::txn

Multi-tree ACID transactions. All trees must be opened before any data
operations.

```graphix
{{#include ../../../stdlib/graphix-package-db/src/graphix/txn.gxi}}
```

## db::subscription

Reactive subscriptions fire when entries are inserted or removed.

```graphix
{{#include ../../../stdlib/graphix-package-db/src/graphix/subscription.gxi}}
```
