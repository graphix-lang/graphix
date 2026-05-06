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
-: fn<'a: unbound: Number>(x: 'a: unbound, y: 'a: unbound) -> 'a: unbound
160
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
fn<'a: unbound: Number>
(x: 'a: unbound, y: 'a: unbound) -> 'a: unbound
```

We can remove the (unbound) current values and it becomes easier to read,

```
fn<'a: Number>
(x: 'a, y: 'a) -> 'a
```

We just have one variable now, `'a` representing both argument types
and the return type. Because unchecked `+` returns bottom on overflow
rather than throwing, there is no `throws` clause. We can
still call this `f` with any number type,

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

## Higher Order Functions

Since functions are first class, they can take other functions as arguments, and
even return functions. These relationships can be often inferred automatically
without issue, but sometimes annotations are required.

```graphix
〉 let apply = |x: 'a, f: fn(x: 'a) -> 'b throws 'e| -> 'b throws 'e f(x)
〉 apply
-: fn<'e: unbound: _>(x: 'a: unbound, f: fn(x: 'a: unbound) -> 'b: unbound throws 'e: unbound) -> 'b: unbound throws 'e: unbound
163
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
〉let f = |x, y| x + y
〉f
-: fn<'_2069: unbound: Number, '_2067: unbound: Number, '_2071: unbound: Number>(x: '_2067: unbound, y: '_2069: unbound) -> '_2071: unbound
159
```

The type is a bit of a mouthful, lets format it a bit so it's easier to read.

```
fn<'_2069: unbound: Number,
   '_2067: unbound: Number,
   '_2071: unbound: Number>
(x: '_2067: unbound, y: '_2069: unbound) -> '_2071: unbound
```

Removing the unbounds,

```
fn<'_2069: Number,
   '_2067: Number,
   '_2071: Number>
(x: '_2067, y: '_2069) -> '_2071
```

Here we can see that `'_2067`, `'_2069`, and `'_2071` represent the two
arguments and the return type of the function. They are all unbound, meaning
that when the function is used they can have any type. They are also all
constrained to `Number`, and this will be enforced when the function is called,
it's arguments must be numbers and it will return a number. We learned this
because internally the function uses `+`, which operates on numbers, this
constraint was then propagated to the otherwise free variables representing the
args and the return type.

So in plain English this says that the arguments to the function can by any type
as long as it is a number, and the function will return some type which is a
number. None of the three numbers need to be the same type of number.

Because unchecked `+` returns bottom on overflow rather than throwing,
there is no `throws` clause in the type. If you want arithmetic errors
to be part of the type, use the checked operator `+?` instead, which
returns `[T, Error<`ArithError(string)>]`.

We can indeed call `f` with different number types, and it works just fine,

```graphix
〉f(1.0, 1)
-: Number
2
```

The type we get back really depends on the values we pass. For example,

```graphix
〉f(1.1212, 1)
-: Number
2.1212
```

Wherever we use `f` the compiler will force us to handle every possible case in
the `Number` type
