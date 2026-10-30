# Labeled and Optional Arguments

Functions can have labeled and also optional arguments. Labeled arguments need
not be specified in order, and optional arguments don't need to be specified at
all. When declaring a function you must specify the labeled and optional
arguments before any non labeled arguments.

```graphix,ignore
let f = |#lbl1, #lbl2, arg| ...
```

In this case lbl1 and 2 are not optional, but are labeled. You can call f with
either labeled argument in either order. e.g. `f(#lbl2, #lbl1, a)`.

```graphix,ignore
let f = |#opt = null, a| ...
```

`opt` need not be specifed when `f` is called, if it isn't specified then it
will be `null`. e.g. `f(2)` is a valid way to call `f`. You can also apply type
constraints to labeled and optional arguments.

```graphix,ignore
let f = |#opt: [i64, null] = null, a| ..
```

Specifies that `opt` can be either an `i64` or `null` and by default it is null.
A default is checked where the function is defined: it must fit the
argument's type, and when that type is a declared type variable, the
variable's constraint. So `'a: [Int, Float] |#start: 'a = 0.0, x: 'a|`
is fine; a call that omits `start` takes `'a` from the default, and a
call that passes `#start: 1` takes it from the call.

A default that returns an enclosing function's open type requires a closed
parameter type. Annotate the enclosing parameter (`|z: i64|`) or the
defaulted parameter (`|#d: i64 = z, x|`). An unannotated factory such as
`|z| { let g = |#d = z, x| (d, x); g }` is refused, as is a default
like `#d: Array<'q> = [z]`: a call cannot safely infer the enclosing
function's type from an omitted argument.

A type variable the enclosing function declares is closed, since each
call of that function fixes it: `|z: 'a| { let g = |#d: 'a = z, y: i64| d;
g(1) }` is fine, as are `#d: Array<'a> = [z]` and an unannotated `#d = z`,
which takes the type `'a`. Such a default must fit its parameter whatever
`'a` is, so `|z: 'b, w: 'a| { let g = |#d: Array<'a> = [z], y: i64| d; g }`
is refused. A closure returned this way keeps its default per call of the
factory: `mk(1)` and `mk("s")` each omit `#d` at their own type. A type
written inside the default names the enclosing function's variables too,
as one written in its body does: `|#d: fn(q: i64) -> 'a = |q: i64| -> 'a
z, y: i64|` is a function returning the factory's `'a`.

The same holds inside a function passed for a polymorphic parameter
(`f: fn<'b: Any>(x: 'b) -> ..`): there `'b` is closed, so `app(|x| { let g =
|#d = x, y: i64| d; g })` gives `g` the type `fn(?#d: 'b, y: i64) -> 'b`,
which a parameter typed `fn<'b: Any>(x: 'b) -> fn(y: i64) -> string`
refuses.

The compiler implements subtyping for functions with optional arguments. For
example if you write a function that takes a function with a labeled argument
`foo`, you can pass any function that has a labeled argument `foo`, even if it
also has other optional arguments. The non optional and non labeled arguments
must match, of course. For example,

```graphix
let f = |g: fn(#foo: i64, x: i64) -> i64, x: i64| g(#foo: x, x);
let g = |#foo:i64, #bar: i64 = 0, x: i64| foo + bar + x;
// valid call
f(g, 42)
```

outputs

```
$ graphix test.gx
84
```
