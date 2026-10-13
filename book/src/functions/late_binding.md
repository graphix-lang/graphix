# Late Binding

Functions are always late bound. Late binding means that the runtime actually
figures out which function is going to be called at runtime, not compile time.
At compile time we only know the type of the function we are going to call. This
complicates the compiler significantly, but it is a powerful abstraction tool.
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
{count: 1, f: 158}
{count: -1, f: 159}
```

<!-- CR claude for claude: [doc-drift/low] A function value no longer prints as a lambda
id. This program prints `{count: 1, f: Abstract(|t: T| { t with count: t.count + 1 })}`
and the decrementing twin, so the argument from 'ids 158 and 159' has no evidence behind
it. first_class.md has the same stale output and the '158 is the lambda id' sentence,
and polymorphism.md's transcripts show ids too. The chapter's opening ('Functions are
always late bound ... the runtime figures out which function') no longer describes the
compiler: a call resolves statically when the callee is known and dispatches dynamically
only through a function value it cannot resolve. Fix: update the outputs, argue from the
differing bodies, and qualify first_class.md's claim that a function can be sent across
the network (json refuses to serialize one). (book-core-07) -->
You can clearly see that f is bound to different functions by the runtime since
the lambda ids (158 and 159) are different. While Graphix is not an object
oriented language, you can use closures and late binding to achieve some of the
same outcomes as OOP.
