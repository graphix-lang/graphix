# Parametric Polymorphism

We can define type variables as part of the definition of named types and then
use them in the type definition in order to create type aliases with type
parameters. For example, suppose in our foods example we wanted to specify that
`Other could carry a value other than a string,

```
type FoodKind<'a> = [
  `Vegetable,
  `Fruit,
  `Meat,
  `Grain,
  `Other('a)
];
let v: FoodKind<`Cookie> = `Other(`Cookie);
v
```

if we paste this program into a file and run it we get,

```
$ graphix test.gx
`Other(`Cookie)
```

<!-- CR claude for claude: [doc-drift/low] The programs and outputs on this page sit in
plain ``` fences, which examples_compile.rs never checks, and they have drifted. The
Point3 run prints {x: 1.0, y: 1.0, z: 3.14} (a whole float prints with its point), not
{x: 1, y: 1, z: 3.14}. The refusal reads "type mismatch Point3<'a: unbound within
Number> does not contain { x: string, y: string, z: string }", not the [Int, Real] text
shown. variants.md and named.md keep their programs in plain fences too. Tag the
runnable ones ```graphix, the refusal ```graphix,refused with its transcript, and fences
holding `...` ```graphix,ignore, so the gate holds them, then update the outputs. probe:
design/review-2026-11-16/repro/book-modules-udt-polymorphic-stale-output.sh
(book-modules-udt-09) -->
We can even place constraints on the type that a type variable can take. For example,

```
type Point3<'a: Number> = {x: 'a, y: 'a, z: 'a};
let f = |p: Point3<'a>, x: 'a| {p with x: p.x + x};
f({x: 0., y: 1., z: 3.14}, 1.)
```

Running this program we get,

```
$ graphix test.gx
{x: 1, y: 1, z: 3.14}
```

However, consider,

```
type Point3<'a: Number> = {x: 'a, y: 'a, z: 'a};
let v: Point3<'a> = {x: "foo", y: "bar", z: "baz"};
v
```

Running this, we can see that `'a` is indeed constrained, since we get

```
$ graphix test.gx
Error: in file "test.gx"

Caused by:
    0: at: line: 2, column: 21, in: { x: "foo", y: "bar", z: "baz" }
    1: type mismatch Point3<'a: [Int, Real]> does not contain {x: string, y: string, z: string}
```

Indicating that we can't construct a Point3 with the type parameter of `string`,
because the constraint is violated.
