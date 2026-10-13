# Map

<!-- CR claude for claude: [doc-drift/low] This block is a hand copy of map/mod.gxi and
has drifted: `change` is missing `throws 'e` on f and on the result, so a reader
concludes the callback cannot throw. sys.md has the same problem: join_path's first
parameter is `s` there and `path` in sys/mod.gxi. Replace both hand copies with
{{#include}} of the .gxi, as http.md does, so they cannot drift again. (gx-stdlib-13)
-->
<!-- CR claude for claude: [structure/medium] This page pastes the map interface by
hand, and the copy has drifted: it has no `map::union`, shows `change`'s callback
without `throws 'e`, and lacks the 'later pair wins' notes on map and filter_map.
array.md (no rules for group's waiting and holding), list.md, args.md, rand.md and
hbs.md hand-copy their interfaces the same way, while core.md, json.md and the others
include the .gxi. Replace each copy with `{{#include
../../../stdlib/graphix-package-<x>/src/graphix/mod.gxi}}` so the book cannot drift
again. sys.md is in book-stdlib-04. (book-stdlib-05) -->
```graphix
/// return a new map where each element is the output of f applied to
/// the corresponding key value pair in the current map
val map: fn<'k2: Ordered>(m: Map<'k, 'v>, f: fn(kv: ('k, 'v)) -> ('k2, 'v2) throws 'e) -> Map<'k2, 'v2> throws 'e;

/// return a new map containing only the key-value pairs where f applied to
/// (key, value) returns true
val filter: fn(m: Map<'k, 'v>, f: fn(kv: ('k, 'v)) -> bool throws 'e) -> Map<'k, 'v> throws 'e;

/// filter_map returns a new map containing the outputs of f
/// that were not null
val filter_map: fn<'k2: Ordered>(m: Map<'k, 'v>, f: fn(kv: ('k, 'v)) -> Option<('k2, 'v2)> throws 'e) -> Map<'k2, 'v2> throws 'e;

/// return the result of f applied to the init and every k, v pair of m in
/// sequence. f(f(f(init, (k0, v0)), (k1, v1)), ...)
val fold: fn(m: Map<'k, 'v>, init: 'acc, f: fn(acc: 'acc, kv: ('k, 'v)) -> 'acc throws 'e) -> 'acc throws 'e;

/// return the length of the map
val len: fn(m: Map<'k, 'v>) -> i64;

/// get the value associated with the key k in the map m, or null if not present
val get: fn<'k: Ordered>(m: Map<'k, 'v>, k: 'k) -> Option<'v>;

/// get the value associated with the key k in the map m, or return the
/// default value if k is not present in m
val get_or: fn<'k: Ordered>(m: Map<'k, 'v>, k: 'k, default: 'v) -> 'v;

/// insert a new value into the map
val insert: fn<'k: Ordered>(m: Map<'k, 'v>, k: 'k, v: 'v) -> Map<'k, 'v>;

/// update the value at k by applying f to the current value at k, or
/// to the provided default if k is not present. returns the new map
/// with k set to f's result. e.g. change(m, "count", 0, |n| n + 1)
val change: fn<'k: Ordered>(m: Map<'k, 'v>, k: 'k, default: 'v, f: fn(v: 'v) -> 'v) -> Map<'k, 'v>;

/// remove the value associated with the specified key from the map
val remove: fn<'k: Ordered>(m: Map<'k, 'v>, k: 'k) -> Map<'k, 'v>;

/// iter produces an update for every key-value pair in the map m.
/// updates are produced in the order they appear in m.
val iter: fn(m: Map<'k, 'v>) -> ('k, 'v);

/// iterq produces an update for each value in m, but only when clock updates. If
/// clock does not update but m does, then iterq will store each m in an internal
/// fifo queue. If clock updates but m does not, iterq will record the number of
/// times it was triggered, and will update immediately that many times when m
/// updates.
val iterq: fn(#clock: Any, m: Map<'k, 'v>) -> ('k, 'v);
```
