# db

The `db` module provides an embedded key-value database (backed by sled)
with ACID transactions, typed trees, cursors, and reactive subscriptions.
A transaction's commit is on disk when it answers.

Tree key and value types are tracked at both compile time and run time —
if a tree is reopened with different types, `db::tree` returns a `DbErr`.

A tree orders and matches its keys by their data. For the primitive and
structural types that is the language's own order; a key of an abstract
type is stored as its `Cast` data, so a custom `Eq` or `Ord` on it does not
reach the tree, and its `to_value` decides the stored order.

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

Multi-tree ACID transactions, optimistic: an open transaction locks
nothing. Its reads see the trees as they stand and its own writes, and
its writes wait for the commit, which checks that every key it read
still holds what it read and applies the writes atomically, or fails
with `` `Conflict `` and applies none. A program that gets `` `Conflict ``
runs the transaction again. A commit is on disk when it answers; one that
was applied but whose flush failed answers `` `NotDurable ``, and is not
run again (`db::flush` retries the flush).

```graphix
{{#include ../../../stdlib/graphix-package-db/src/graphix/txn.gxi}}
```

## db::subscription

Reactive subscriptions fire when entries are inserted or removed.
`on_change` carries both, each key's in the order they happened, so a
copy of a tree follows it; a commit may arrive across several cycles.

```graphix
{{#include ../../../stdlib/graphix-package-db/src/graphix/subscription.gxi}}
```
