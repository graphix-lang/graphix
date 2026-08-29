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
`Display`. The implementation is attached to the VALUE: wherever an
abstract value with an implementation is compared or printed —
directly, nested inside an array or a struct, as a map key, in
`array::sort`, `min`/`max` or `uniq` — the implementation is called;
everything else takes the structural case (compare values, or print
them as Graphix syntax). Every type has the structural default, so
the core traits hold as bounds for every type, and the dispatchers
work on every value: `Eq::eq(1, 1)` is `1 == 1`.

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

Because the implementation rides the value, a MAP is keyed by the
user's `Ord`:

```graphix
type T = Abstract<i64>;
impl Ord for T {
    // reversed
    let cmp = |x, y| select (x.0, y.0) {
        (a, b) if a < b => `Greater,
        (a, b) if a > b => `Less,
        _ => `Equal
    }
};
let m = {T(0) => 0, T(1) => 1, T(2) => 2};
"[m]"                  // {T(2) => 2, T(1) => 1, T(0) => 0}
```

and an `Ord` that calls two payloads equal unifies them as keys —
inserting `Key("A")` replaces `Key("a")` under a case-insensitive
order. A map consults `Ord` only (like a B-tree); keeping an `Eq`
implementation consistent with the order is the implementor's duty,
as it is in Rust. Likewise the implementation must be a consistent
total order — one that answers differently across cycles corrupts
the maps keyed by it.

If an implementation produces no value for some pair (an error
dropped with `$`, say), the comparison still needs an answer that
preserves the total order. The rule is per key, like NaN's: a key
the implementation bottoms on sorts below every real key and equals
its fellow bottom keys. A `fmt` that produces nothing prints the
structural form and logs a warning.

Values compared where no Graphix code can run — on the wire, in the
REPL's echo of a bare expression — compare and print structurally.

## Constructor Traits: `Collection`

A trait's receiver can be a type *constructor* rather than a type.
Writing `self<'a>` in a method signature — `self` applied to an
element type — makes the trait a constructor trait: a receiver's type
is decomposed on its outermost form, its last type parameter becomes
`'a`, and what remains (`Array<'_>`, `Map<string, '_>`, `List<'_>`, a
program's own `Tree<'_>`) selects the implementation. Core's
`Collection` is the one in the standard library:

```graphix
trait Collection {
    val fold: fn(self<'a>, init: 'b, f: fn(acc: 'b, x: 'a) -> 'b throws 'e) -> 'b throws 'e;
    val filter_map: fn(self<'a>, f: fn(x: 'a) -> Option<'b> throws 'e) -> self<'b> throws 'e;
    val flat_map: fn(self<'a>, f: fn(x: 'a) -> self<'b> throws 'e) -> self<'b> throws 'e;
    val map: fn(self<'a>, f: fn(x: 'a) -> 'b throws 'e) -> self<'b> throws 'e
        = |c, f| filter_map(c, |x| f(x));
    // filter, find, find_map and len are defaults too
};
```

`fold`, `filter_map` and `flat_map` are required; `map`, `filter`,
`find`, `find_map` and `len` derive from them, and an implementation
overrides any it can do better. Core implements it for `Array` and
`Map` (a map is a collection of its *values*: `map(m, f)` keeps the
keys), the list package for `List`, all three through their
intrinsics — so `use Collection::*` makes `map(c, f)` mean the same
thing whatever `c` is:

```graphix
use Collection::*;
map([1, 2, 3], |x| x * 2);                           // [2, 4, 6]
fold(list::from_array([1, 2, 3]), 0, |a, x| a + x);  // 6
len(filter({"a" => 1, "b" => 20}, |v| v > 5))        // 1
```

An implementation's head names the constructor with the hole `'_` as
its last parameter:

```graphix
type Bag<'a> = Abstract<Array<'a>>;
impl Collection for Bag<'_> {
    let fold = |b, init, f| array::fold(b.0, init, f);
    let filter_map = |b, f| Bag(array::filter_map(b.0, f));
    let flat_map = |b, f| Bag(array::flat_map(b.0, |x| f(x).0))
};
use Collection::*;
len(filter(Bag([1, 2, 3, 4]), |x| x > 2))            // 2, through the defaults
```

`'_` is legal only there. A program's own linear or tree structure
implements the three methods as recursions — an activation per
element, see [Recursion](../functions/recursion.md) — and gets the
rest.

A parameter typed by a constructor trait is a constructor variable
applied to a fresh element, and each call site resolves it for its
own receiver:

```graphix
let total = |c: Collection| fold(c, 0, |a, x| a + x);
total([1, 2]) + total(list::from_array([3, 4]))      // 10
```

`|c: Collection|` hides the constructor, which is enough when the
result is not the same shape (`total` returns a scalar). To *preserve*
the container — return the same kind of collection with a new element
— name the constructor and apply it:

```graphix
let sqr = 'c: Collection, 'a: Number |c: 'c<'a>| -> 'c<'a> map(c, |n| n * n);
sqr([1, 2, 3])                                       // Array<i64> [1, 4, 9]
sqr({"a" => 2, "b" => 3})                            // Map<string, i64>
```

`'c: Collection` binds the constructor; `'c<'a>` applies it to the
element, so `-> 'c<'a>` returns the *same* container: an `Array` in
yields an `Array` out, a `Map` a `Map` — checked, not widened to "some
collection." The bound stays on `'c`, the constructor; the `<'a>` only
marks where it is applied. `'a: Number` constrains the element. (There
is no `'c<'a>: Collection` quantifier form: the bound belongs on the
constructor `'c`, not on the application.)

The receiver must have a last type parameter: a union of collections,
a struct or a bare primitive is not a constructor, and an impl head
whose last parameter is filled (`impl Collection for Bag<i64>`) is
refused. A trait spells its receiver one way throughout — `self<'a>`
in every signature, or `self` in every signature.

## Current limits

- Traits take no type parameters and declare no associated types.
- A constructor trait decomposes its receiver on the outermost form
  only, and a union of collections has no implementation (dispatch
  over a union receiver is for plain traits).
- `Hash` is not a trait: map keys hash structurally.
- A `==` or `<` whose implementation sits inside a composite (an
  `Array<Key>`) runs on the node-walk; one on the whole type is a
  static call and fuses like any call.
