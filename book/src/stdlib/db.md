# db

The `db` module provides an embedded key-value database (backed by sled)
with ACID transactions, typed trees, cursors, and reactive subscriptions.
A transaction's commit is on disk when it answers. Database files persist
across program runs and Graphix builds; upgrading the executable does not
rewrite stored records.

Tree key and value types are tracked at both compile time and run time —
if a tree is reopened with different types, `db::tree` returns a `DbErr`.

A tree orders and matches its keys by their data. For the primitive and
structural types that is the language's own order; a key of an abstract
type is stored as its `Cast` data, so a custom `Eq` or `Ord` on it does not
reach the tree, and its `to_value` decides the stored order.

## Interface

Stored values preserve tuple, struct, variant and list kinds, including in
unions. This preserves structural kinds, not abstract identity: abstract
values are stored as their `Cast` data. With `type B = Abstract<i64>` and
the default `Cast` implementation, a `[B, i64]` holding `B(1)` reads back
as `i64`, and an `[Array<B>, Array<i64>]` holding `[B(1), B(2)]` reads
back as `Array<i64>`. Use distinct variant tags if those union members
must remain distinguishable after a read.

Older unmarked records remain readable when their tree metadata is
compatible, but an erased kind cannot be recovered at an ambiguous union.
In particular, a tuple written by a build that erased composite kinds
into a `[(i64, i64), Array<i64>]` value reads back as the array member.
Reopening the database with the same declared types does not detect this
change of member. Rewrite such records from a source that still knows
their original kind; upgrading Graphix alone cannot recover it.

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
