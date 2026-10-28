# Late Binding

A call whose function the compiler can see (a name bound to a lambda,
a function argument it can trace to one) is resolved when the program
compiles. A call through a function value it cannot trace, one read out
of a data structure for example, is late bound: the runtime figures out
which function it calls, and at compile time only the function's type
is known. Late binding is a powerful abstraction tool.

Writing a function variable fires its call's current result even when
the write gives it the same function. The call keeps its existing
instance: its counters and local state are not reset.

For example we can create two structs of type `T` that each contain a different
implementation of `f`, and we can use them interchangibly with any function that
accepts a `T`. In this simple example we create one implementation of `f` that
increments the count, and one that decrements it.

```graphix
type T = { count: i64, f: fn(t: T) -> T };
let ts: Array<T> = [
  { count: 0, f: |t: T| {t with count: t.count + 1} },
  { count: 0, f: |t: T| {t with count: t.count - 1} }
];
let t = array::iter(ts);
(t.f)(t)
```

when run this example will output,

```
{count: 1, f: Abstract(|t: T| { t with count: t.count + 1 })}
{count: -1, f: Abstract(|t: T| { t with count: t.count - 1 })}
```

You can see that `f` is bound to a different function in each struct,
and the call ran each one's body. While Graphix is not an object
oriented language, you can use closures and late binding to achieve some of the
same outcomes as OOP.
