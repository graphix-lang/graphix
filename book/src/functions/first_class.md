# Functions are First Class values

We can store a function in a structure, which can itself be stored in a data
structure, a file, or even sent across the network to another instance of the
same program. Here we build a struct that maintains a count, and a function to
operate on the count, returning a new struct of the same type with a different
count.

```graphix
type T = { count: i64, f: fn(t: T) -> T };
let t = { count: 0, f: |t: T| {t with count: t.count + 1} };
(t.f)(t)
```

when run this example will output,

```
{count: 1, f: 158}
```

158 is the lambda id, it's the actual value that is stored to represent a
function.
