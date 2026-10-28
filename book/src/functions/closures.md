# Lexical Closures

Functions can reference variables outside of their definition. These variables
are captured by the function definition, and remain valid no matter where the
closure is called. For example,

```graphix
let f = {
  let v: i64 = sys::net::subscribe("/local/foo")$;
  |n| v + n
};
f(2)
```

`f` captures `v` and can use it even when it is called from a scope where `v`
isn't visible. Closures allow functions to encapsulate data, just like an object
in OOP.

A closure keeps its captured variables alive after the collection slot or
function activation that created it is deleted. A call first made after
that deletion sees the retained values. Captures used by a nested closure
or by a labeled argument's default have the same lifetime. Keeping a
variable alive does not keep its former initializer or effects running.
