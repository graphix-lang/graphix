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
-: fn<'a: Number + Singleton>(x: 'a: unbound within Number & Singleton, y: 'a: unbound within Number & Singleton) -> 'a: unbound within Number & Singleton
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

When a function type is printed, the `fn<>` header lists the bounds on
the type variables the definition declares, `'a: Number + Singleton`. The shell also prints each type variable where it occurs
with its current state: `'a: unbound` when nothing has bound it yet, the
type it is bound to when something has, and the bounds it must stay
within, joined by `&`,

```
'a: unbound within Number & Singleton
```

We can remove the current states and it becomes easier to read,

```
fn<'a: Number + Singleton>(x: 'a, y: 'a) -> 'a
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
error: in expr f(1, 1.2)

Caused by:
    0: at: line: 1, column: 6, in: 1.2
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
error: ... cannot compute 'a: unbound within Number + 'b: unbound within Number: arithmetic is fn<'a: Number + Singleton>(x: 'a, y: 'a) -> 'a — both operands must be one numeric type (cast one side explicitly)
```

`+` takes two values of one type, so `x + y` needs `'a` and `'b` to be
equal, which is exactly what the signature said they need not be. A
caller may still pass two values of the same type to a two-variable
function; the promise is about what the body may assume, not about what
callers may do.

A call chooses a type for each declared variable from its arguments, its
result and what it throws, so a variable must appear in the signature:
on an argument, the return type or the throws, or inside the bound of
another variable that does (`'a: Array<'b>, 'b: Int |x: 'a|`). One that
only the body names could be chosen by no call, and the definition is
refused,

```graphix
〉let h = 'a: Number |x: i64| -> i64 { let v: 'a = 1; x }
error: ... 'a is declared but the signature does not hold it: write it on a parameter, the return type, the throws or the bound of a quantifier the signature holds, or remove it
```

A written function type follows the same rule: `fn<'a: Number>(x: i64)
-> i64` is refused too.

A bound may not reach its own variable, directly (`'a: 'a`), inside
another type (`'a: Array<'a>`) or through the bound of another variable
(`'a: 'b, 'b: 'a`); a definition, a written function type and an impl
head are refused where they declare one. A recursive bound is written as
a recursive type, which the variable is then bounded by,

```graphix
〉let n = 'a: [i64, Array<'a>] |x: 'a| -> 'a x
error: ... 'a's bound names 'a: a bound may not reach its own variable; a recursive bound is a recursive type (`type T = [i64, Array<T>]`, `'a: T`)
〉type Nested = [i64, Array<Nested>]
〉let n = 'a: Nested |x: 'a| -> 'a x
〉n([1, [2, [3]]])
-: Array<[i64, Array<[i64, Array<i64>]>]>
[1, [2, [3]]]
```

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
`Discernible`, no references, no functions and no type holding two numeric
types: a reference has no order, and `==`, `!=` and `uniq` compare one by
what it points to (see [References](../udt/references.md)), a function has
no order either, and values of two numeric types have no order between
them.

## Higher Order Functions

Since functions are first class, they can take other functions as arguments, and
even return functions. These relationships can be often inferred automatically
without issue, but sometimes annotations are required.

```graphix
〉 let apply = |x: 'a, f: fn(x: 'a) -> 'b throws 'e| -> 'b throws 'e f(x)
〉 apply
-: fn(x: 'a: unbound, f: fn(x: 'a: unbound) -> 'b: unbound throws 'e: unbound) -> 'b: unbound throws 'e: unbound
Abstract(|x: 'a, f: fn(x: 'a) -> 'b throws 'e| -> 'b throws 'e f(x))
```

Here we've specified a single argument apply, it takes an argument, and a
function `f`, and calls `f` on the argument. Note that we've explicitly said
that whatever type of error `f` throws, `apply` will throw as well. `'e`
has no bound, so the header lists nothing: `f` may throw anything, or
nothing at all, and each call's `'e` is whatever its `f` throws.

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

The shell prints its type as

```
fn(x: '_1: unbound within Number & Singleton, y: '_1: unbound within Number & Singleton) -> '_1: unbound within Number & Singleton
```

The compiler named the variable it inferred (`'_1`; the number differs
from run to run), and the header lists only declared variables, so the
bounds show where it occurs.

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
