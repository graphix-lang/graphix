# Polymorphism

While the compiler does a pretty good job of inferring the types of
functions, sometimes you want to express a constraint that can't be
inferred. Suppose we wanted to write a function that you can pass any
type of number to, but it has to be the same type for both arguments,
and the return type will be the same as the argument type. We can say
that using type variables and constraints in our annotations.

```graphix
〉let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y
〉f
-: fn<'a: unbound: Number + Singleton>(x: 'a: unbound, y: 'a: unbound) -> 'a: unbound
Abstract('a: Number |x: 'a, y: 'a| -> 'a x + y)
```

In type annotations of lambda expressions,
- The constraints come before the first `|`, separated by commas if there are
  multiple constrained type variables. e.g. `'a: Number`
- Each argument may optionally have a `: Type` after it, and this will set it's
  type, e.g. `x: 'a`
- After the second `|` you can optionally include an `-> Type` which will set
  the return type of the function, e.g. `-> 'a`
- After the return type, you can optionally specify a throws type, `throws
  Type`, which will set the type that is thrown by the function

When a function type is printed, the stuff between the `fn<>` are the
type constraints, the syntax in this readout is a colon separated list
of,

- type variable name, for example '_2073
- current value, or unbound if there is no current value
- constraint type

```
fn<'a: unbound: Number + Singleton>
(x: 'a: unbound, y: 'a: unbound) -> 'a: unbound
```

We can remove the (unbound) current values and it becomes easier to read,

```
fn<'a: Number + Singleton>
(x: 'a, y: 'a) -> 'a
```

We just have one variable now, `'a` representing both argument types
and the return type. The body's `+` added `Singleton` to the declared
`Number`: arithmetic needs one numeric type, not a union of several
like `[i64, f64]`.

Two bounds say how many types a variable may stand for. `Singleton`
says one type, not a union of several. `OneNumber` says at most one
numeric type, so a nullable number of one numeric type is

```graphix
let g = 'a: [Number, null] + OneNumber |x: 'a| -> 'a x
```

which takes an `[i64, null]` or an `[f64, null]`, and refuses an
`[i64, f64, null]`. The two-argument `f` above takes any one number
type,

```graphix
〉f(1.212, 2.0)
-: f64
3.2119999999999997
```

However notice that we get back the explicit type we passed in,

```graphix
〉f(2, 2)
-: i64
4
```

In one case `f64`, in the other `i64`. We can't pass numbers of
different types to the same call,

```graphix
〉f(1, 1.2)
error: in expr

Caused by:
    0: at: line: 1, column: 6, in: f64:1.2
    1: type mismatch 'a: i64 does not contain f64
```

Here the compiler is saying that `'a` is already initialized as `i64` and `i64`
doesn't unify with `f64`.

Declaring two variables promises the opposite: that the function works
for any `'a` and any `'b`, chosen independently. The body is checked
against that promise, so a body that only works when the two are the
same type is refused at the definition rather than at some later call,

```graphix
〉let g = 'a: Number, 'b: Number |x: 'a, y: 'b| -> ['a, 'b] x + y
error: ... type mismatch 'a does not contain 'b
```

`+` takes two values of one type, so `x + y` needs `'a` and `'b` to be
equal, which is exactly what the signature said they need not be. A
caller may still pass two values of the same type to a two-variable
function; the promise is about what the body may assume, not about what
callers may do.

A third bound, `Discernible`, says the values of a type can be told
apart at run time the way the type tells them apart. Nearly every type
can: a string, an array, a tuple, a struct, a list and each variant are
different values at run time, whatever they hold, and `==` compares a
reference by what it points to. The exception is functions: a function
value doesn't say its type, so in `[fn(x: i64) -> i64, fn(s: string) ->
string]` the two can't be told apart by a test. Comparing such values,
or hashing them, would mix the two up, so `==` and the other
comparisons, map keys, `uniq`, `min`, `max`, `array::sort` and the
other functions that compare or hash need `Discernible`. A function
whose body compares takes the bound on its type variable, as arithmetic
adds `Singleton`, and a call refuses a type that isn't `Discernible`,

```graphix
〉let eq = |x, y| x == y
〉let inc = |x: i64| x + 1
〉let v: [fn(x: i64) -> i64, fn(s: string) -> string] = inc
〉eq(v, v)
error: ... 'a must be Discernible, but [fn(x: i64) -> i64, fn(s: string) -> string]
holds fn(x: i64) -> i64 and fn(s: string) -> string, which have the same runtime
form; wrap them in distinct variants
```

Wrapping the members in variants of their own, `` [`I(fn(x: i64) ->
i64), `S(fn(s: string) -> string)] ``, tells them apart.

What orders or hashes values (`<` and the other orderings, map keys,
`min`, `max`, the sorts, `array::dedup`) needs `Ordered`, which is
`Discernible`, no references and no type holding two numeric types: a
reference has no order, and `==`, `!=` and `uniq` compare one by what it
points to (see [References](../udt/references.md)), and values of two
numeric types have no order between them.

## Higher Order Functions

Since functions are first class, they can take other functions as arguments, and
even return functions. These relationships can be often inferred automatically
without issue, but sometimes annotations are required.

```graphix
〉 let apply = |x: 'a, f: fn(x: 'a) -> 'b throws 'e| -> 'b throws 'e f(x)
〉 apply
-: fn<'e: unbound: _>(x: 'a: unbound, f: fn(x: 'a: unbound) -> 'b: unbound throws 'e: unbound) -> 'b: unbound throws 'e: unbound
Abstract(|x: 'a, f: fn(x: 'a) -> 'b throws 'e| -> 'b throws 'e f(x))
```

Here we've specified a single argument apply, it takes an argument, and a
function `f`, and calls `f` on the argument. Note that we've explicitly said
that whatever type of error `f` throws, `apply` will throw as well. That was
constrained by the compiler to `_` meaning basically this could throw anything
or also not throw at all, it just depends on `f`.

We can see a more practical example in the type of `array::map` (this
implementation of which I will not repeat here), which is,

```
fn(a: Array<'a>, f: fn(x: 'a) -> 'b throws 'e) -> Array<'b> throws 'e
```

So map takes an array of `'a`, and a function mapping `'a` to `'b` and possibly
throwing `'e` and returns an array of `'b` possibly throwing `'e`.

## Implicit Polymorphism

All functions are polymorphic, even without annotations, argument and return
types are inferred at each call site, and thus may differ from one site to
another. Any internal constraints are calculated when the definition is compiled
and are enforced at each call site. For example consider,

```graphix
let f = |x, y| x + y
```

Its type, with the unbound current values removed, is

```
fn<'_1: Number + Singleton>(x: '_1, y: '_1) -> '_1
```

One inferred variable stands for both arguments and the return type. The
body's `+` gave it the arithmetic bound: `Number + Singleton`, one numeric
type, not a union of several. So `f` takes any one number type,

```graphix
let f = |x, y| x + y;
let a = f(2, 2);
let b = f(1.5, 2.5);
(a, b)
```

but not two at once: `f(1.0, 1)` is refused (`type mismatch f64 does not
contain i64`), and a cast is the fix, `f(1.0, cast<f64>(1)$)`.

Because unchecked `+` wraps on overflow rather than throwing, there is
no `throws` clause in the type. If you want arithmetic errors
to be part of the type, use the checked operator `+?` instead, which
returns `[T, Error<`ArithError(string)>]`.
