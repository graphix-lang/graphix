# db

The `db` module provides an embedded key-value database (backed by sled)
with ACID transactions, typed trees, cursors, and reactive subscriptions.

Tree key and value types are tracked at both compile time and run time —
if a tree is reopened with different types, `db::tree` returns a `DbErr`.

## Interface

```graphix
/// An opaque handle to an embedded database.
type Db;

/// A typed view of a database tree (key-value namespace).
type Tree<'k, 'v>;

/// Open or create an embedded database at the given path.
val open: fn(s: string) -> Result<Db, `DbErr(string)>;

/// Open or create a named tree with typed keys and values.
/// Pass null for the default (unnamed) tree.
val tree: fn(a: Db, v: [string, null]) -> Result<Tree<'k, 'v>, `DbErr(string)>;

/// Get the value for a key, or null if not found.
val get: fn(a: Tree<'k, 'v>, y: 'k) -> Result<['v, null], `DbErr(string)>;

/// Insert a key-value pair. Returns the previous value, or null.
val insert: fn(a: Tree<'k, 'v>, y: 'k, z: 'v) -> Result<['v, null], `DbErr(string)>;

/// Remove a key. Returns the previous value, or null.
val remove: fn(a: Tree<'k, 'v>, y: 'k) -> Result<['v, null], `DbErr(string)>;

/// Check whether a key exists.
val contains_key: fn(a: Tree<'k, 'v>, y: 'k) -> Result<bool, `DbErr(string)>;

/// Batch get values for an array of keys.
val get_many: fn(a: Tree<'k, 'v>, a2: Array<'k>) -> Result<Array<['v, null]>, `DbErr(string)>;

/// Get the first (minimum key) entry, or null if empty.
val first: fn(a: Tree<'k, 'v>) -> Result<[('k, 'v), null], `DbErr(string)>;

/// Get the last (maximum key) entry, or null if empty.
val last: fn(a: Tree<'k, 'v>) -> Result<[('k, 'v), null], `DbErr(string)>;

/// Atomically remove and return the minimum-key entry.
val pop_min: fn(a: Tree<'k, 'v>) -> Result<[('k, 'v), null], `DbErr(string)>;

/// Atomically remove and return the maximum-key entry.
val pop_max: fn(a: Tree<'k, 'v>) -> Result<[('k, 'v), null], `DbErr(string)>;

/// Get the entry with the greatest key strictly less than the given key.
val get_lt: fn(a: Tree<'k, 'v>, y: 'k) -> Result<[('k, 'v), null], `DbErr(string)>;

/// Get the entry with the smallest key strictly greater than the given key.
val get_gt: fn(a: Tree<'k, 'v>, y: 'k) -> Result<[('k, 'v), null], `DbErr(string)>;

/// Atomic compare-and-swap.
val compare_and_swap: fn(a: Tree<'k, 'v>, y: 'k, v: ['v, null], v2: ['v, null]) -> Result<[null, `Mismatch(['v, null])], `DbErr(string)>;

/// Atomically apply a batch of inserts and removes.
val batch: fn(a: Tree<'k, 'v>, a2: Array<[`Insert('k, 'v), `Remove('k)]>) -> Result<null, `DbErr(string)>;

/// Number of entries in the tree (O(n) scan).
val len: fn(a: Tree<'k, 'v>) -> Result<u64, `DbErr(string)>;

/// True if the tree has no entries.
val is_empty: fn(a: Tree<'k, 'v>) -> Result<bool, `DbErr(string)>;

/// Get the stored type metadata for a tree, or null if none.
val get_type: fn(a: Db, v: [string, null]) -> Result<[(string, string), null], `DbErr(string)>;

/// List the names of all trees in the database.
val tree_names: fn(a: Db) -> Result<Array<string>, `DbErr(string)>;

/// Drop a named tree from the database.
val drop_tree: fn(a: Db, s: string) -> Result<bool, `DbErr(string)>;

/// Generate a monotonically increasing unique u64 ID.
val generate_id: fn(a: Db) -> Result<u64, `DbErr(string)>;

/// Flush all pending writes to disk.
val flush: fn(a: Db) -> Result<null, `DbErr(string)>;

/// Total size of the database on disk in bytes.
val size_on_disk: fn(a: Db) -> Result<u64, `DbErr(string)>;

/// True if the database was recovered after a crash.
val was_recovered: fn(a: Db) -> Result<bool, `DbErr(string)>;

/// CRC32 checksum of all keys and values (O(n)).
val checksum: fn(a: Db) -> Result<u32, `DbErr(string)>;

/// Export all database contents to a file.
val export: fn(a: Db, s: string) -> Result<null, `DbErr(string)>;

/// Import previously exported data from a file. The database must be empty.
val import: fn(a: Db, s: string) -> Result<null, `DbErr(string)>;
```

## db::cursor

Cursors iterate over tree entries reactively, advancing on each trigger.

```graphix
/// A cursor for iterating over tree entries one at a time.
type Cursor<'k, 'v>;

/// Create a new cursor, optionally filtering by key prefix.
val new: fn(?#prefix: ['k, null], a: Tree<'k, 'v>) -> Cursor<'k, 'v>;

/// Create a cursor over a key range.
val range: fn(
    ?#start: [`Included('k), `Excluded('k), null],
    ?#end: [`Included('k), `Excluded('k), null],
    a: Tree<'k, 'v>
) -> Cursor<'k, 'v>;

/// Read the next entry. Returns (key, value) or null when exhausted.
val read: fn(a: Cursor<'k, 'v>, v: Any) -> Result<[('k, 'v), null], `DbErr(string)>;

/// Read up to N entries at once.
val read_many: fn(a: Cursor<'k, 'v>, n: i64) -> Result<Array<('k, 'v)>, `DbErr(string)>;
```

## db::txn

Multi-tree ACID transactions. All trees must be opened before any data
operations.

```graphix
/// An open transaction handle.
type Txn;

/// A tree view within a transaction.
type TxnTree<'k, 'v>;

/// Begin a multi-tree transaction.
val begin: fn(a: Db) -> Result<Txn, `DbErr(string)>;

/// Open a tree within the transaction.
val tree: fn(a: Txn, v: [string, null]) -> Result<TxnTree<'k, 'v>, `DbErr(string)>;

/// Get a value within the transaction.
val get: fn(a: TxnTree<'k, 'v>, y: 'k) -> Result<['v, null], `DbErr(string)>;

/// Insert a key-value pair within the transaction.
val insert: fn(a: TxnTree<'k, 'v>, y: 'k, z: 'v) -> Result<['v, null], `DbErr(string)>;

/// Remove a key within the transaction.
val remove: fn(a: TxnTree<'k, 'v>, y: 'k) -> Result<['v, null], `DbErr(string)>;

/// Atomically apply a batch of inserts and removes within the transaction.
val batch: fn(a: TxnTree<'k, 'v>, a2: Array<[`Insert('k, 'v), `Remove('k)]>) -> Result<null, `DbErr(string)>;

/// Commit the transaction atomically.
val commit: fn(a: Txn) -> Result<null, `DbErr(string)>;

/// Abort the transaction.
val rollback: fn(a: Txn) -> Result<null, `DbErr(string)>;
```

## db::subscription

Reactive subscriptions fire when entries are inserted or removed.

```graphix
/// A reactive subscription to changes on a tree.
type Subscription<'k, 'v>;

/// Subscribe to changes, optionally filtering by key prefix.
val new: fn(?#prefix: ['k, null], a: Tree<'k, 'v>) -> Subscription<'k, 'v>;

/// Fires reactively when inserts occur.
val on_insert: fn(a: Subscription<'k, 'v>) -> Result<Array<{key: 'k, value: 'v}>, `DbErr(string)>;

/// Fires reactively when removes occur.
val on_remove: fn(a: Subscription<'k, 'v>) -> Result<Array<{key: 'k}>, `DbErr(string)>;
```
