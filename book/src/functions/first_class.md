# Functions are First Class values

We can store a function in a structure, which can itself be stored in a data
structure, passed to and returned from functions. A function exists only in the
program that made it: it cannot be written to a file or sent across the
network (the json and pack writers refuse one). Here we build a struct that maintains a count, and a function to
operate on the count, returning a new struct of the same type with a different
count.

```graphix
type T = { count: i64, f: fn(t: T) -> T };
let t = { count: 0, f: |t: T| {t with count: t.count + 1} };
(t.f)(t)
```

when run this example will output,

```
{count: 1, f: Abstract(|t: T| { t with count: t.count + 1 })}
```

A function value prints as its source.
