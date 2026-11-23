# Polymorphism

While the compiler does a pretty good job of inferring the types of
functions, sometimes you want to express a constraint that can't be
inferred. Suppose we wanted to write a function that you can pass any
type of number to, but it has to be the same type for both arguments,
and the return type will be the same as the argument type. We can say
that using type variables and constraints in our annotations.

```graphix
〉let f = 'a: Number + Singleton |x: 'a, y: 'a| -> 'a x + y
〉f
-: fn<'a: Number + Singleton>(x: 'a, y: 'a) -> 'a
Abstract('a: Number + Singleton |x: 'a, y: 'a| -> 'a x + y)
```

In type annotations of lambda expressions,
- The constraints come before the first `|`, separated by commas if there are
  multiple constrained type variables. e.g. `'a: Number + Singleton`
- Each argument may optionally have a `: Type` after it, and this will set it's
  type, e.g. `x: 'a`
- After the second `|` you can optionally include an `-> Type` which will set
  the return type of the function, e.g. `-> 'a`
- After the return type, you can optionally specify a throws type, `throws
  Type`, which will set the type that is thrown by the function

When a function type is printed, the `fn<>` header lists the bounds on
its type variables, `'a: Number + Singleton` here, and each variable
stands by its name where it occurs. There is one variable, `'a`, for
both argument types and the return type. Its bound says two things:
`Number`, and `Singleton`, since arithmetic needs one numeric type, not
a union of several like `[i64, f64]`. A variable's bound is exactly what
you write for it, so both are written; with `'a: Number` alone the body
is refused, and the message names what to write,

```graphix
〉let f2 = 'a: Number |x: 'a, y: 'a| -> 'a x + y
error: ... the body needs 'a within Singleton, which the signature does not declare: write 'a: Number + Singleton
```

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
    0: at line: 1, column: 1
    1: cannot compute with [i64, f64]: both operands must be one numeric type
```

Here the compiler is saying that this call makes `'a` an `[i64, f64]`, what
the two arguments are between them, and the `+` in `f` takes one numeric
type.

Declaring two variables promises the opposite: that the function works
for any `'a` and any `'b`, chosen independently. The body is checked
against that promise, so a body that only works when the two are the
same type is refused at the definition rather than at some later call,

```graphix
〉let g = 'a: Number + Singleton, 'b: Number + Singleton |x: 'a, y: 'b| -> 'a x + y
error: ... ['a, 'b] must be one type, but the declared variable 'a may differ from its other members
```

`+` takes two values of one type, so `x + y` needs `'a` and `'b` to be
equal, which is exactly what the signature said they need not be. A
caller may still pass two values of the same type to a two-variable
function; the promise is about what the body may assume, not about what
callers may do.

A call chooses a type for each declared variable from its arguments, its
result and what it throws, so a variable must appear in the signature:
on an argument, the return type or the throws, or inside the bound of
another variable it declares that does
(`'a: Array<'b>, 'b: Int |x: 'a|`); the bound of a variable an enclosing
definition declares names that definition's variables, never this one's.
One that only the body names could be chosen by no call, and the
definition is refused,

```graphix
〉let h = 'a: Number |x: i64| -> i64 { let v: 'a = 1; x }
error: ... 'a is declared but the signature does not hold it: write it on a parameter, the return type, the throws or the bound of a quantifier the signature holds, or remove it
```

A written function type follows the same rule: `fn<'a: Number>(x: i64)
-> i64` is refused too.

A variable's bound is exactly what you write for it, `Any` where you
write none, and a body may not ask more of the variable than that: a
call, an operator or a builtin that needs `Number`, `Singleton`,
`Ordered` or `Discernible` of a variable bounded by less is refused.
`|y: 'c| -> 'c f(y)` over an `f` that needs `Number` is refused until
the signature says `'c: Number`, and `|x: 'a, y: 'a| x == y` until it
says `'a: Discernible`, which the message names. Nothing is added to the
signature quietly: whoever reads `'a` with no bound may pass anything
for it. Where you want the check to work the bound out, leave the
parameter untyped: `|x, y| x + y` is generic over one numeric type with
nothing written.

In the body a declared variable is one of the types its bound allows,
whichever a call picks, and a `select` over a value of it tests those
(see [Select](../core/select.md)): a pattern takes the members of the
bound it names, and a name it binds is typed by the part that reaches
it. A name or `_` over the whole value keeps the variable,

```graphix
〉let g = 'b: [`A(i64), `B(i64)] |x: 'b| select x { `A(z) => z + 1, _ => 0 }
〉g(`A(3))
-: i64
4
〉let same = 'b: [`A(i64), `B(i64)] |x: 'b| -> 'b select x { v => v }
```

A destructuring `let` over such a value reads its parts from the bound
the same way (`let {x, y} = p` over a `p: 'b` with `'b: {x: i64, y:
i64}`); a destructuring parameter is annotated with the type it binds
(`|{x, y}: {x: i64, y: i64}|`). Beside other members, the variable's
values are covered only by a name or `_` at its position, or by tests
that split its bound:

```graphix
〉let h = 'b: [i64, string] |x: (bool, ['b, null])| select x { (true, _) => 1, (false, null) => 2 }
error: ... missing match cases: nothing matches (bool, 'b)
〉let h = 'b: [i64, string] |x: (bool, ['b, null])| select x { (true, _) => 1, (false, null) => 2, (false, v) => 3 }
```

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

A function type with variables of its own (`fn<'a: Number>(x: 'a) ->
'a`, or a trait as a parameter's type, `fn(c: Collection) -> i64`, which
declares one) stands only as a definition's own signature: an interface
`val`'s type, a trait method's, a builtin's at the top of its
declaration. Anywhere else, a parameter's type, a field, an element, a
`let` annotation, a typedef, another function type's argument or result,
it is refused. Declare the variable on the definition instead, which each
call picks, or bind the generic function with `let` and call it by name,

```graphix
〉let apply = |f: fn<'a: Number>(x: 'a) -> 'a| f(1)
error: ... a function type with type variables of its own (`fn<'a: ..>(..)`, or a trait as a parameter's type, `fn(c: Collection)`) stands only as a definition's signature: declare the variables on the definition (`'a: Number |f: fn(x: 'a) -> 'a| ..`), or bind the generic function with `let` and call it by name
〉let apply = 'a: Number |f: fn(x: 'a) -> 'a, x: 'a| f(x)
〉apply(|x| x * 2, 3)
-: i64
6
```

A function stays generic only where a binding names it: a `let` of a
lambda or of a data literal holding lambdas, a `let` of a generic name or
of a field of one (`let j = s.g`), or of a block ending in one of these,
a `val`, a trait method. Anywhere else, an element of a collection a call
returns, a callback's result, a field of a call's result, an argument, a
reference (`&f`), it is one instance,
which its uses decide between them. Here `g` is one instance of the
lambda the callback returns, and its one variable is what every call
passes: after `g(3)` a second call `g(1.5)` would make it an `[i64,
f64]`, which `x + x` refuses:

```graphix
let fs = array::map([1, 2], |r| 'a: Number + Singleton |x: 'a| -> 'a x + x);
let g = fs[0]$;
g(3)
```

The shell shows which one you have. The `fn<>` header lists a variable
only while the function is generic, each call still picking it. One
instance prints without the header, at the type its uses gave its
variable; one nothing has called stands at the variable's bound:

```graphix
〉let mk = |z: i64| 'a: Number |x: 'a| -> 'a x
〉let h = mk(0)
〉h
-: fn(x: Number) -> Number
Abstract('a: Number |x: 'a| -> 'a x)
〉let k = mk(1)
〉(h, k)
-: (fn(x: Number) -> Number, fn(x: Number) -> Number)
(Abstract('a: Number |x: 'a| -> 'a x), Abstract('a: Number |x: 'a| -> 'a x))
```

Each read of a generic binding is an instance of its own. A bare name
shows its binding's type, header and all; read into a tuple, each `id`
here is an instance nothing has called, whose variable has no bound and
no type yet, shown as `_`:

```graphix
〉let id = |x| x
〉id
-: fn(x: 'a) -> 'a
Abstract(|x| x)
〉(id, id)
-: (fn(x: _) -> _, fn(x: _) -> _)
(Abstract(|x| x), Abstract(|x| x))
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
whose body compares is judged at each call, which refuses a type that
isn't `Discernible`,

```graphix
〉let eq = |x, y| x == y
〉let inc = |x: i64| x + 1
〉let v: [fn(x: i64) -> i64, fn(s: string) -> string] = inc
〉eq(v, v)
error: ... [fn(x: i64) -> i64, fn(s: string) -> string] must be Discernible here,
but it holds fn(x: i64) -> i64 and fn(s: string) -> string, which have the same
runtime form; wrap them in distinct variants
```

Wrapping the members in variants of their own, `` [`I(fn(x: i64) ->
i64), `S(fn(s: string) -> string)] ``, tells them apart.

What orders or hashes values (`<` and the other orderings, map keys,
`min`, `max`, the sorts, `array::dedup`) needs `Ordered`, which is
`Discernible`, no references and no functions: a reference has no order,
and `==`, `!=` and `uniq` compare one by what it points to (see
[References](../udt/references.md)), and a function has no order either.
Every other two values have one, numbers by their value whatever their
types.

## Higher Order Functions

Since functions are first class, they can take other functions as arguments, and
even return functions. These relationships can be often inferred automatically
without issue, but sometimes annotations are required.

```graphix
〉 let apply = |x: 'a, f: fn(x: 'a) -> 'b throws 'e| -> 'b throws 'e f(x)
〉 apply
-: fn(x: 'a, f: fn(x: 'a) -> 'b throws 'c) -> 'b throws 'c
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
fn<'a: Number + Singleton>(x: 'a, y: 'a) -> 'a
```

The compiler named the variable it inferred, and the header gives its
bound.

One inferred variable stands for both arguments and the return type. The
body's `+` gave it the arithmetic bound: `Number + Singleton`, one numeric
type, not a union of several. So `f` takes any one number type,

```graphix
let f = |x, y| x + y;
let a = f(2, 2);
let b = f(1.5, 2.5);
(a, b)
```

but not two at once: `f(1.0, 1)` is refused (`cannot compute with [i64,
f64]: both operands must be one numeric type`), and a cast is the fix,
`f(1.0, cast<f64>(1)$)`.

Because unchecked `+` wraps on overflow rather than throwing, there is
no `throws` clause in the type. If you want arithmetic errors
to be part of the type, use the checked operator `+?` instead, which
returns `[T, Error<`ArithError(string)>]`.
