# Dynamic Modules

Graphix programs can dynamically load modules at runtime. The loaded code will
be compiled, type checked, and the loader will return an error indicating any
failure in that process. Because Graphix is a statically typed language we must
know ahead of time what interface the dynamically loaded module will have. We do
this by defining a module signature. The signature also decides what the
dynamically loaded module may reference: it sees what the signature imports and
nothing else, so it cannot just call any function it likes. Lets dive right in
with an example,

```graphix
// the module source, which we will publish in netidx
let path = "/local/foo";
let source = "
    let add = |x| x + 1;
    let sub = |x| x - 1;
    let cfg = \[1, 2, 3, 4, 5\];
    let hidden = 42
";
sys::net::publish(path, source)$;

// now load the module
let status = mod foo dynamic {
    sig {
        val add: fn(x: i64) -> i64;
        val sub: fn(x: i64) -> i64;
        val cfg: Array<i64>
    };
    source sys::net::subscribe(path)$
};
select status {
    error as e => never(dbg(e)),
    null as _ => foo::add(foo::cfg[0]$)
}
```

running this we get,

```
$ graphix test.gx
2
```

In the first part of this program we just publish a string containing the source
code of the module we want to ultimately load. The second part is where it gets
interesting, lets break it down.

`mod foo dynamic` declares a dynamically loaded module named `foo`. In the rest
of our code we can refer (statically) to `foo` as if it was a normal module that
we loaded at compile time. There are two sections required to define a dynamic
module, they are required to be defined in order, sig and source,

- a `sig` statement is the type signature of the module, written as an
  [interface](./interfaces.md) is. Its statements are,
  - a val statement defines a value and it's type, `val add: fn(x: i64) -> i64` is
    an example of a val statement, it need not be a function it can be any type
  - a type statement defines a type in the loaded module, e.g. `type T = { foo:
    string, bar: string }` val statements that come after a type statement may
    use the defined type. The type statement is identical to the normal type
    statement in Graphix (so it can be polymorphic, recursive, etc).
  - an abstract type statement declares a type without defining it, e.g. `type T;`
    The loaded module must provide a concrete definition, but that definition is
    hidden from the loading code. This provides encapsulation - see the
    [Abstract Types](./interfaces.md#abstract-types) section for details.
  - a trait statement defines a trait, and an impl statement, `impl Show for
    T;`, declares an implementation the loaded module must provide.
  - a use statement imports names, e.g. `use sys::io::Read;`. What the
    signature imports is also everything the loaded module can see, as
    described below.

  Like an interface, the signature sees only what it imports or declares
  itself. A type of the loading program is imported, `use super::Config;`,
  and declared above the module; a signature naming a type it does not
  import is refused when the loading program is checked.

  A dynamic module has no sub-modules: its source is one piece of text, so
  it cannot declare a `mod`, and a `mod` in its sig is refused.
- a `source` statement defines where the source code for the dynamic module will
  come from. It's type must be a string. The source expression is part of the
  loading program, so it resolves in the enclosing scope — referencing `path`
  above works even though it appears inside the `mod foo dynamic` block.

The `mod foo dynamic ...` expression returns a value of type,

```[null, Error<`DynamicLoadError(string)>]```

<!-- CR claude for claude: [doc-drift/low] This paragraph says the previous module is
deleted whatever the outcome. A load that fails leaves the last good body running
(`Module::put_back`); only a load that succeeds retires the one before it, and the code
is the side that is right. Further up, the page says a `type T;` in the sig must be
defined by the loaded module: a source that never defines T loads. probe:
design/review-2026-11-16/repro/x-dyn-modules-failed-reload-keeps-body.gx
(x-dyn-modules-12) -->
<!-- CR claude for claude: [doc-drift/medium] This paragraph says the previous load is
deleted "regardless of the outcome". On a failed compile the code does the opposite:
Module::update puts the set-aside body back (put_back) and retires it only when the new
source checks. So after a bad source the status is the DynamicLoadError while foo::add
keeps answering from the last good load, and a reader will expect the module's values to
stop. Say that a failed load leaves the last good load running, and only a successful
one replaces it. probe:
design/review-2026-11-16/repro/book-modules-udt-dyn-failed-load-keeps-module.gx
(book-modules-udt-01) -->
The runtime will try to load the module every time the source updates. If it
succeeds it will update with `null`, if it fails it will update with an error
indicating what went wrong. Regardless of the outcome the previous loaded
module, if any, will be deleted. If compilation succeeded the new module will be
initialized, possibly causing values it exports to update.

A function taken out of the module and kept somewhere else (`saved <-
foo::add`) is the function of the body it was taken from. After a reload it
still runs that body's code, over that body's bindings as they last stood, and
it sees nothing of the new module: a trait method its code calls is the one that
body implemented, also where a function outside the module that it calls makes
the call, and a name it uses is what that body imported. Code that should follow
a reload calls through the module each time, `foo::add(x)`.

<!-- CR claude for claude: [doc-drift/low] This page never says that a hidden type the
sig declares (`type T;`) keeps the representation of the first load that defines it. A
reload that defines `T = Abstract<string>` after `Abstract<i64>` fails with "T is
already defined as Abstract<i64>: a value of it may still be held, so its representation
cannot change", and the old load runs on. The sig bullet on abstract types reads as if
each load may choose its own hidden definition. Add a sentence here: a sig-declared type
is one type across loads, so the first load fixes its representation, and changing it
takes a new name or a restart. probe:
design/review-2026-11-16/repro/book-modules-udt-dyn-sig-type-fixed.gx
(book-modules-udt-07) -->
An abstract type the module's body declares and its interface does not belongs
to that load alone. The next load may declare a type of the same name, even with
another representation, and it is a different type: a function kept from the
first body compares, sorts, prints and casts its values by the first body's
`Eq`, `Ord`, `Display` and `Cast`. A type the interface declares is one type for
every load, and its values compare and print by the implementation the current
load registered.

## What a Loaded Module Can See

A loaded module starts with nothing: no package, not even `core`, and none of
the loading program's bindings, types or modules. The module in the example
above uses only operators and literals, which need no import, so its signature
imports nothing. Everything else comes from a `use` in the signature:

```graphix
let limit = 10;
let status = mod foo dynamic {
    sig {
        use core;
        use array::{len, map};
        use super::limit;
        val small: fn(xs: Array<i64>) -> bool
    };
    source "let small = |xs| { dbg(len(xs)); len(xs) < limit }"
};
status
```

- `use core;` or any other package brings the whole package: the module can
  call `dbg`, `filter` and the rest of `core` by their bare names, as any
  program does. The type names `core` declares, such as `Result`, come with
  it too; without it only the built-in types (`i64`, `Array`, `Map` and the
  like) are there.
- `use array::{len, map};` brings those two functions and nothing else of
  `array`: `array::fold` is not defined inside the module.
- `use super::limit;` brings one binding of the loading program. It must be
  declared above the module.
- A glob, `use super::*;`, brings every name the enclosing scope declares, but
  never a package.

A signature import that names nothing is refused when the loading program is
checked. Nothing a signature imports is added to the module's interface:
`foo::limit` is not defined.

## Signature Checking

Obviously the loaded module must match the type signature defined in the dynamic
mod statement. However, the signature checking only cares that every item
mentioned in the signature is present in the dynamic module and that the types
match. If extra items are present in the dynamic module they will simply be
ignored, and will be inaccessible to the loading program.

<!-- CR claude for claude: [doc-drift/medium] This example predates sandboxing by sig
imports. The body writes super::y, which it can name only when the sig imports it. As
written, both the failing body and the fixed one (`let y: i64 = never()`, `val h: fn(x:
i64) -> i64`) fail to load with "super::y is undefined", not for the reason given. Add
`use super::y;` to both sigs. With it, the unannotated version fails as described ("it
holds the type of `y` ... declare y's type") and the annotated one loads. probe:
design/review-2026-11-16/repro/book-modules-udt-dyn-super-write-needs-import.gx
(book-modules-udt-02) -->
A load is checked after the program that loads it, so the program's types are
decided by then: what the program left undecided is `_`, and a load that would
decide it otherwise is refused, naming the binding to annotate, and leaves the
program's types as they were. A loaded body that writes a binding of the
program is held to its type. With `let y = never()` in the program, the body
`let h = |x| { super::y <- x; x }` takes only `_`, so a `sig { val h: fn(x: 'a)
-> 'a }` is refused; declare `let y: i64 = never()` and `val h: fn(x: i64) ->
i64`.
