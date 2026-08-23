# Traits

A trait names a set of operations a type can support. It is declared
once, implemented per type, and used as a bound on type variables —
Graphix's traits are Rust's, spelled in Graphix.

## Declaring a Trait

```graphix
trait Show {
    /// required: every implementor supplies it
    val show: fn(self) -> string;

    /// overridable: the body is the default
    val twice: fn(self) -> string = |s| "[show(s)] [show(s)]"
};
```

Each item is a `val` with a function type. The type `self` is the
receiver — the type implementing the trait — and every method has a
positional `self` parameter, written bare: `fn(self, n: u64) -> bytes`
is `self` followed by an ordinary named parameter. A method may use
`self` anywhere in its signature (`fn(self) -> self`).

A method with a `= default` body is inherited by implementors that
do not override it. Inside a default body the trait's other methods
are visible bare (`show(s)` above), as are the items of the declaring
module.

Trait declarations are legal in `.gx` files and in `.gxi` interface
files; an interface's declaration applies to the implementation file
automatically, like a `type`, and must not be repeated there.

## Implementing a Trait

```graphix
type Counter = Abstract<i64>;

impl Show for Counter {
    let show = |c| "Counter([c.0])"
};

impl Show for i64 {
    let show = |x| "int [x]";
    let twice = |x| "int [x], again"     // overriding the default
};
```

An `impl` block holds one `let` per method. Each binding is checked
against the trait's signature with `self` replaced by the target
(`fn(self: Counter) -> string` above), so the lambdas need no
annotations. A required method left out is a compile error; a method
the trait does not declare is too. The trait's methods are visible
bare inside the block, so one method may call another.

`impl Trait for Target;` (no body) implements a trait whose methods
all have defaults, and is the form an interface file uses to declare
that the implementation provides the impl (see below).

### Parameterized targets

```graphix
impl<'a: Show> Show for Array<'a> {
    let show = |xs| "\[[str::join(#sep: ", ", array::map(xs, Show::show))]\]"
}
```

The head's type variables are declared in `impl<..>` with their
bounds; every one must occur in the target. `Array<i64>` then
implements `Show` exactly when `i64` does.

### Where an impl may be written

Implementations are global facts: once the module holding one loads,
the impl applies everywhere the trait is used. To keep that coherent,
an `impl` is restricted to where its author answers for it:

- an **abstract type** (`Abstract<..>` or Rust-backed) may be
  implemented in the type's package or the trait's package;
- **any other type** — a primitive, `Array<..>`, a struct, a tuple, a
  variant, an alias — only inside the **trait's own package**.

A structural type is its shape, so `impl Show for {x: i64, y: i64}`
would apply to every `{x: i64, y: i64}` in the program; only the
trait's author may decide that. A type that should carry behaviour
gets a name: `type Point = Abstract<{x: i64, y: i64}>`. Unions are
never targets (each member is implemented separately), and one
implementation per (trait, type) is enforced.

## Calling Trait Methods

A trait's methods live under the trait's name, like a module's items:

```graphix
Show::show(Counter(42));

use Show::{show, twice};         // or `use Show::*`
show(7);
twice(Counter(1))
```

A call resolves to the implementation selected by the type of the
`self` argument — statically, when the program is compiled. That is
what makes a trait call as cheap as any other static call (and lets it
fuse into the JIT). If the self type is not known at the call, the
call is a compile error: annotate the argument.

### Dispatch over a union

When the self type is a union of implementors, the call becomes the
`select` you would otherwise write, one arm per member:

```graphix
let v: [i64, Counter] = select flag { true => 1, false => Counter(2) };
show(v)      // ≡ select v { i64 as x => show(x), Counter as x => show(x) }
```

Every member needs an implementation.

## Traits as Bounds

A trait names no value's type; it constrains a type variable:

```graphix
let describe = 'a: Show |x: 'a| "<[show(x)]>";
describe(3);
describe(Counter(4))
```

Inside `describe`, `show(x)` resolves per use: each call site of
`describe` elaborates the body with its own argument type, so `3`
dispatches to `i64`'s `show` and `Counter(4)` to `Counter`'s.

Writing the trait as a parameter's type is shorthand for a fresh
bounded variable per parameter:

```graphix
let both = |a: Show, b: Show| "[show(a)] [show(b)]";
// ≡ 'a: Show, 'b: Show |a: 'a, b: 'b| ...
```

Bounds combine with `+`: `'a: Show + Hash`, `fn<'s: Read + Write>(s: 's)`.
A trait anywhere else in a type — a return type, a field, an array
element — is an error, since no value has a trait as its type.

A function whose signature was written before an implementation
existed still accepts the new type: bounds are checked at each use,
against the implementations known then.

## Traits in Interface Files

```graphix
// m.gxi
trait Show { val show: fn(self) -> string };
type Counter;
impl Show for Counter;
val make: fn(x: i64) -> Counter
```

```graphix
// m.gx
type Counter = Abstract<i64>;
impl Show for Counter { let show = |c| "Counter([c.0])" };
let make = |x| Counter(x)
```

The interface declares the trait (with its defaults, if any) and
`impl Show for Counter;` promises the implementation; a consumer
compiles against the interface and calls `m::Show::show(m::make(9))`.
A trait declared only in the `.gx` file is private to that module.

## The Core Traits: Eq, Ord and Display

Three traits are built into core and consulted by the language
itself:

```graphix
trait Eq { val eq: fn(self, other: self) -> bool };
type Ordering = [`Less, `Equal, `Greater];
trait Ord { val cmp: fn(self, other: self) -> Ordering };
trait Display { val fmt: fn(self) -> string };
```

`==` and `!=` consult `Eq`; `<`, `>`, `<=` and `>=` consult `Ord`;
string interpolation, `print`, `println`, `dbg` and `log` consult
`Display`. The rule is the same for all three: at a site, look at the
static type; wherever an implementation exists — for the whole type,
or for a type nested anywhere inside it — call it; everywhere else
take the structural case (compare values, or print them as Graphix
syntax). Every type has the structural default, so the core traits
hold as bounds for every type, and the dispatchers work on every
value: `Eq::eq(1, 1)` is `1 == 1`.

A case-insensitive key, equal however it is capitalized:

```graphix
type Key = Abstract<string>;
impl Eq for Key { let eq = |a, b| str::to_lower(a.0) == str::to_lower(b.0) };

Key("Foo") == Key("FOO")              // true: the implementation
[Key("a")] == [Key("A")]              // true: called per element
(Key("x"), 1) == (Key("X"), 2)        // false: the i64s differ
```

A color that prints as its hex triplet, wherever it appears:

```graphix
type Color = Abstract<{r: i64, g: i64, b: i64}>;
impl Display for Color { let fmt = |c| "#[c.0.r][c.0.g][c.0.b]" };

let red = Color({r: 255, g: 0, b: 0});
println("[red]")                      // #25500
println([red, red])                   // [#25500, #25500]
println({fg: red, n: 3})              // {fg: #25500, n: 3}
```

Outside core only an abstract type may implement a core trait (the
rule of the previous section). A core trait's method runs inside the
comparison or the print, so it is implicitly `#[sync]` — a body that
waits on a timer is a compile error — and a method that produces no
value makes the comparison produce none.

A value of type `Any` is dispatched on its runtime tag: an abstract
value still finds its type's implementation, anything else is
structural. Map keys, `array::sort`, `min`/`max`, values sent over
the wire, and the REPL's echo of a bare expression compare and print
structurally.

## Current limits

- Traits take no type parameters and declare no associated types.
- `Hash` is not a trait: map keys hash structurally.
- A `==` or `<` whose implementation sits inside a composite (an
  `Array<Key>`) runs on the node-walk; one on the whole type is a
  static call and fuses like any call.
