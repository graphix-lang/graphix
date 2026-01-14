# Map

```graphix
/// return a new map where each element is the output of f applied to
/// the corresponding key value pair in the current map
val map: fn(Map<'k, 'v>, fn(('k, 'v)) -> ('k2, 'v2) throws 'e) -> Map<'k2, 'v2> throws 'e;

/// return a new map containing only the key-value pairs where f applied to
/// (key, value) returns true
val filter: fn(Map<'k, 'v>, fn(('k, 'v)) -> bool throws 'e) -> Map<'k, 'v> throws 'e;

/// filter_map returns a new map containing the outputs of f
/// that were not null
val filter_map: fn(Map<'k, 'v>, fn(('k, 'v)) -> Option<('k2, 'v2)> throws 'e) -> Map<'k2, 'v2> throws 'e;

/// return the result of f applied to the init and every k, v pair of m in
/// sequence. f(f(f(init, (k0, v0)), (k1, v1)), ...)
val fold: fn(Map<'k, 'v>, 'acc, fn('acc, ('k, 'v)) -> 'acc throws 'e) -> 'acc throws 'e;

/// return the length of the map
val len: fn(Map<'k, 'v>) -> i64;

/// get the value associated with the key k in the map m, or null if not present
val get: fn(Map<'k, 'v>, 'k) -> Option<'v>;

/// insert a new value into the map
val insert: fn(Map<'k, 'v>, 'k, 'v) -> Map<'k, 'v>;

/// remove the value associated with the specified key from the map
val remove: fn(Map<'k, 'v>, 'k) -> Map<'k, 'v>;

/// iter produces an update for every key-value pair in the map m.
/// updates are produced in the order they appear in m.
val iter: fn(Map<'k, 'v>) -> ('k, 'v);

/// iterq produces an update for each value in m, but only when clock updates. If
/// clock does not update but m does, then iterq will store each m in an internal
/// fifo queue. If clock updates but m does not, iterq will record the number of
/// times it was triggered, and will update immediately that many times when m
/// updates.
val iterq: fn(#clock: Any, Map<'k, 'v>) -> ('k, 'v);
```
