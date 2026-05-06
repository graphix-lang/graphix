# Labeled and Optional Arguments

Functions can have labeled and also optional arguments. Labeled arguments need
not be specified in order, and optional arguments don't need to be specified at
all. When declaring a function you must specify the labeled and optional
arguments before any non labeled arguments.

```graphix
let f = |#lbl1, #lbl2, arg| ...
```

In this case lbl1 and 2 are not optional, but are labeled. You can call f with
either labeled argument in either order. e.g. `f(#lbl2, #lbl1, a)`.

```graphix
let f = |#opt = null, a| ...
```

`opt` need not be specifed when `f` is called, if it isn't specified then it
will be `null`. e.g. `f(2)` is a valid way to call `f`. You can also apply type
constraints to labeled and optional arguments.

```graphix
let f = |#opt: [i64, null] = null, a| ..
```

Specifies that `opt` can be either an `i64` or `null` and by default it is null.
The compiler implements subtyping for functions with optional arguments. For
example if you write a function that takes a function with a labeled argument
`foo`, you can pass any function that has a labeled argument `foo`, even if it
also has other optional arguments. The non optional and non labeled arguments
must match, of course. For example,

```graphix
let f = |g: fn(#foo: i64, x: i64) -> i64, x: i64| g(#foo: x, x);
let g = |#foo:i64, #bar: i64 = 0, x: i64| foo + bar + x;
f(g, 42) // valid call
```

outputs

```
$ graphix test.gx
84
```
