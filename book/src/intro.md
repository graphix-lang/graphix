# Intro to Graphix

Graphix is a programming language using the dataflow paradigm. It is
particularly well suited to building user interfaces, and interacting
with resources in
[netidx](https://netidx.github.io/netidx-book). Dataflow languages
like Graphix are "reactive", like React or Vue, except at the language
level instead of just as a library. A Graphix program is compiled to a
directed graph, operations (such as +) are graph nodes, edges
represent paths data can take through the program. A simple expression like,

```graphix
2 + 2
```

will compile to a graph like

```
         
const(2) ──> + <── const(2)
         
```

The semantics of simple examples like this aren't noticibly different
from a normal programming language. However a more complex example
such as,

```graphix
let x = cast<i64>(net::subscribe("/foo")?)?;
print(x * 10)
```

compiles to a graph like

```
                                               const(10)
                                                   │
                                                   │
                                                   ▼
                                         
const("/foo") ──> net::subscribe ──> cast<i64> ──> * ──> print
                                         
```

Unlike the first example, the value of `net::subscribe` isn't a
constant, it can change if the value published in netidx changes. If
that happens the new value will flow through the graph and will be
printed again. If the published value of "/foo" is initially 10, and
then the value of "/foo" changes to 5 then the program will print.

```
100
50
```

It will keep running forever, if "/foo" changes again, it will print
more output. This is a powerful way to think about programming, and
it's especially well suited to building user interfaces and
transforming data streams.

## Dataflow but Otherwise Normal

Besides being a dataflow language Graphix tries hard to be a normal
functional language that would feel familiar to anyone who knows
Haskell, OCaml, F# or a similar ML derived language. Some of it's
features are,

- lexically scoped
- expression oriented
- strongly statically typed
- type inference
- structural type discipline with nomial abstract types
- parametric polymorphism
- algebraic data types
- pattern matching
- first class functions, and closures
- late binding

