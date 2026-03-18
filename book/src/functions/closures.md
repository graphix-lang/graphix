# Lexical Closures

Functions can reference variables outside of their definition. These variables
are captured by the function definition, and remain valid no matter where the
closure is called. For example,

```graphix
let f = {
  let v = cast<i64>(sys::net::subscribe("/local/foo")$)$;
  |n| v + n
};
f(2)
```

`f` captures `v` and can use it even when it is called from a scope where `v`
isn't visible. Closures allow functions to encapsulate data, just like an object
in OOP.
