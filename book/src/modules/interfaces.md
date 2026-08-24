# Interface Files

Interface files (`.gxi` files) define the public API of a module. They serve a
similar purpose to `.mli` files in OCaml or header files in C: they declare what
a module exports without revealing the implementation details.

## Why Use Interface Files?

Interface files provide several benefits:

- **API Documentation**: They serve as clear documentation of a module's public API
- **Encapsulation**: Implementation details not in the interface are hidden from users
- **Type Checking**: The compiler verifies that implementations match their interfaces
- **Stability**: Changing internals won't break dependent code as long as the interface is preserved

## File Naming Convention

For a module named `foo`:
- Implementation file: `foo.gx`
- Interface file: `foo.gxi`

For hierarchical modules using a directory:
- Implementation file: `foo/mod.gx`
- Interface file: `foo/mod.gxi`

The interface file must be in the same directory as the implementation file.

## Interface Syntax

Interface files contain declarations of what the module exports. There are four
types of declarations:

### Value Declarations (val)

Declare exported values and their types using `val`:

```graphix
val add: fn(a: i64, b: i64) -> i64;
val greeting: string;
val config: Array<i64>;
```

The implementation must provide bindings with matching names and types.

### Type Definitions (type)

Export type definitions that users of the module can reference:

```graphix
type Color = [`Red, `Green, `Blue];
type Point = { x: f64, y: f64 };
type Result<'a, 'e> = ['a, Error<'e>];
type Handle;                      // abstract: defined in the implementation
type Meters = Abstract<f64>;      // a public newtype
```

Types can be polymorphic and recursive, just like in regular Graphix code.

### Module Declarations (mod)

Declare sub-modules that the module exports:

```graphix
mod utils;
mod parser;
```

Each declared sub-module should have its own implementation file (e.g.,
`utils.gx`) and optionally its own interface file (`utils.gxi`).

### Use Statements (use)

Import names the interface's own declarations need:

```graphix
use super::{Client, Response};
use sys::io::Read;
```

A `use` in the interface applies to the implementation automatically —
don't repeat it in the `.gx` file. It is a private import, not a
re-export: it does not make the imported names part of the module's
public API.

## A Complete Example

Let's create a simple math utilities module with an interface.

**math.gxi** (interface):
```graphix
/// Add two numbers
val add: fn(a: i64, b: i64) -> i64;

/// Subtract the second number from the first
val sub: fn(a: i64, b: i64) -> i64;

/// Common mathematical constants
type Constants = {
    pi: f64,
    e: f64
};

val constants: Constants;
```

**math.gx** (implementation):
```graphix
let add = |a, b| a + b;
let sub = |a, b| a - b;

let constants = { pi: 3.14159265359, e: 2.71828182845 };

let internal_helper = |x| x * 2
```

Note that the `Constants` type is defined in the interface and automatically
available in the implementation - it doesn't need to be repeated. Also,
`internal_helper` is not in the interface, so it is not accessible to users of
the module.

**main.gx** (usage):
```graphix
mod math;

let result = math::add(1, 2);
let pi = math::constants.pi;

// This would be an error - internal_helper is not exported:
// math::internal_helper(5)
```

## Interface and Implementation Relationship

When a module has an interface file:

1. **Type definitions, `mod` statements, and `use` statements** declared in the
   interface automatically apply to the implementation. You do not need to
   duplicate them in the `.gx` file.

2. **Value declarations (`val`)** specify what bindings must exist in the
   implementation with matching types.

3. **Extra items allowed**: The implementation may contain additional items not
   in the interface; these are simply not accessible to users of the module.

If the implementation doesn't match the interface, you'll get a compile-time error.

## Documentation Comments

Interface files support documentation comments using `///`. These comments
document the exported items and are the primary place to document your module's
public API:

```graphix
/// Filter an array, keeping only elements where the predicate returns true.
/// 
/// The predicate function is called for each element. Elements for which
/// the predicate returns true are included in the result.
val filter: fn(a: Array<'a>, f: fn(x: 'a) -> bool throws 'e) -> Array<'a> throws 'e;
```

## Polymorphic Functions

Interface files fully support polymorphic type signatures:

```graphix
/// Transform each element of an array using function f
val map: fn(a: Array<'a>, f: fn(x: 'a) -> 'b throws 'e) -> Array<'b> throws 'e;

/// Fold an array into a single value
val fold: fn(a: Array<'a>, init: 'b, f: fn(acc: 'b, x: 'a) -> 'b throws 'e) -> 'b throws 'e;
```

Type variables (like `'a`, `'b`, `'e`) work the same as in regular type
annotations.

## Module Hierarchies

For module hierarchies, each level can have its own interface. Here's an example
structure:

```
mylib/
  mod.gx      # Root implementation
  mod.gxi     # Root interface
  utils.gx    # Sub-module implementation
  utils.gxi   # Sub-module interface
  parser/
    mod.gx    # Nested module implementation
    mod.gxi   # Nested module interface
```

The root interface (`mod.gxi`) declares the sub-modules:

```graphix
// mod.gxi
type Config = { name: string, version: i64 };
val config: Config;

mod utils;
mod parser;
```

## Sub-module Visibility

A sub-module sees nothing of its parent implicitly — it imports what it
needs with `use super::...`. Because visibility is order-independent,
the position of the `mod` statement carries no meaning: declare it
wherever reads best, and a sub-module may import parent items declared
after it.

Privacy follows Rust's rule: a private item is visible to its defining
module and that module's descendants. So a sub-module may
`use super::private_setup` even when `private_setup` is not exported in
the parent's interface — privacy hides items from *users* of the
module, not from the module's own subtree.

Whether the sub-module itself is exported is controlled by the
interface: a `mod child;` in the `.gxi` exports it, a `mod child;` only
in the `.gx` keeps it private to the parent's subtree.

Example:

```graphix
// parent.gxi
val public_helper: fn(x: i64) -> i64;
mod child;
```

```graphix
// parent.gx
let private_setup = ...;
let public_helper = |x| x + 1;

mod child;
```

```graphix
// parent/child.gx
use super::{private_setup, public_helper};  // both visible here
```

## Interfaces with Netidx Modules

Interface files also work with modules stored in netidx. The naming convention
is the same as for files: if your module implementation is at
`/libs/graphix/mymodule.gx`, the interface would be at
`/libs/graphix/mymodule.gxi`.

## Interfaces and Dynamic Modules

Interface files work with static (file-based and netidx) modules. For dynamic
modules loaded at runtime, use the inline `sig { ... }` syntax described in the
[Dynamic Modules](./dynamic.md) chapter. The signature syntax in dynamic modules
uses the same declaration forms (`val`, `type`, `mod`) as interface files.

## Abstract Types

An abstract type hides the representation of a type from users of your
module — the encapsulation that lets you change the internals without
affecting code that uses the module. Graphix's abstract types are
**nominal**: a value of the type is a box carrying the type's identity,
minted only by the type's constructor, so two abstract types with the
same representation are different types at compile time *and* at
runtime.

### Declaring Abstract Types

In an interface file, declare an abstract type by omitting the body:

```graphix
type Handle;
type Container<'a>;
type NumericBox<'a: Number>;
```

The implementation defines it with an `Abstract<..>` body around the
representation:

```graphix
type Handle = Abstract<{ id: i64, name: string }>;
type Container<'a> = Abstract<Array<'a>>;
type NumericBox<'a: Number> = Abstract<{ value: 'a }>;
```

`Abstract<..>` is legal only as the whole body of a `type` definition —
the name is what gives the type its identity. A type hidden by an
interface must be defined this way (or be a Rust-backed type, declared
`type T;` on both sides); hiding a transparent alias is an error.

### The Three Faces

Inside the module — wherever the definition is visible — the type's
name is its constructor, `.0` reads the payload, and `T(pattern)`
destructures it:

```graphix
type Counter = Abstract<i64>;

let make = |x: i64| -> Counter Counter(x);          // construct
let get = |c: Counter| -> i64 c.0;                  // payload
let bump = |c: Counter| -> Counter {                // destructure
    let Counter(x) = c;
    Counter(x + 1)
};
let sign = |c: Counter| -> i64 select c {
    Counter(x) if x > 0 => 1,
    Counter(0) => 0,
    Counter(_) => -1
}
```

The payload keeps its shape: for `Abstract<{ a: i64, b: string }>`,
`x.0.a` reads a field and `T({ x.0 with a: 1 })` updates one.

Outside the module all three are compile errors — users can only call
the functions the interface exports. The type *test* `T as t` works
everywhere, since it compares the tag:

```graphix
let v: [a::A, b::B] = ...;
select v {
    a::A as _ => "an A",
    b::B as _ => "a B"
}
```

### Example: Encapsulated Counter

**counter.gxi**:
```graphix
/// An opaque counter type
type Counter;

/// Create a new counter starting at the given value
val make: fn(x: i64) -> Counter;

/// Get the current value
val get: fn(c: Counter) -> i64;

/// Increment the counter every time trig updates
val increment: fn(#trig: Any, c: &Counter) -> null;
```

**counter.gx**:
```graphix
// We could change the representation later without breaking users
type Counter = Abstract<i64>;

let make = |x: i64| -> Counter Counter(x);
let get = |c: Counter| -> i64 c.0;
let increment = |#trig: Any, c: &Counter| -> null {
    *c <- Counter((trig ~ *c).0 + 1);
    null
}
```

**main.gx**:
```graphix
mod counter;

let c = counter::make(0);
counter::increment(#trig:null, &c);
let value = counter::get(c)  // 1
```

### Public Newtypes

Nominal and hidden are independent. Put the `Abstract<..>` body in the
interface and the type is nominal but constructible by anyone — a
newtype whose representation is public:

```graphix
// interface
type Meters = Abstract<f64>;
val add: fn(a: Meters, b: Meters) -> Meters;
```

A module with no interface file exports its definitions too, so its
`Abstract<..>` types are public newtypes as well.

### Parameterized Abstract Types

Abstract types can have type parameters, allowing generic containers:

```graphix
// interface
type Box<'a>;
val wrap: fn(x: 'a) -> Box<'a>;
val unwrap: fn(b: Box<'a>) -> 'a;
```

```graphix
// implementation
type Box<'a> = Abstract<{ value: 'a }>;
let wrap = |x: 'a| -> Box<'a> Box({ value: x });
let unwrap = |b: Box<'a>| -> 'a b.0.value
```

### Constrained Type Parameters

Type parameters on abstract types can have constraints. The interface and
implementation must have matching constraints:

```graphix
// interface - constraint required
type NumericWrapper<'a: Number>;
val wrap: fn(x: 'a) -> NumericWrapper<'a>;
val double: fn(w: NumericWrapper<'a>) -> 'a;
```

```graphix
// implementation - same constraint required
type NumericWrapper<'a: Number> = Abstract<'a>;
let wrap = |x: 'a| -> NumericWrapper<'a> NumericWrapper(x);
let double = |w: NumericWrapper<'a>| -> 'a w.0 + w.0
```

### Abstract Types in Compound Types

Abstract types can be used within other type definitions in the interface:

```graphix
type Element;
type List = [`Cons(Element, List), `Nil];
type Pair = (Element, Element);
type Container = { items: Array<Element> };
```

This allows you to export complex data structures while keeping the element type
opaque.

### Equality and Printing

Two abstract values are equal when they carry the same tag and equal
payloads; a value prints as its constructor applied to its payload
(`Counter(5)`). The box costs one allocation per construction, so use
abstract types for handles and newtypes rather than for hot data
structures.

### Abstract Types vs Type Aliases

| Declaration | Meaning |
|-------------|---------|
| `type T;` | Abstract type — nominal, representation hidden |
| `type T = Abstract<i64>;` | Nominal newtype — representation public |
| `type T = i64;` | Type alias — `T` *is* `i64`, structurally |

Use abstract types when you want encapsulation or a distinct identity.
Use type aliases when you want to give a convenient name to a type that
users can still see and use directly.

## Best Practices

1. **Document in interfaces**: Put documentation comments in the `.gxi` file since that's what users see
2. **Minimal interfaces**: Only export what users need; keep implementation details private
3. **Stable interfaces**: Think carefully before changing an interface, as it may break dependent code
4. **Type aliases**: Export type aliases in the interface to give users convenient names for complex types
5. **Use abstract types for encapsulation**: When you want to hide implementation details and reserve the right to change them, use abstract types instead of exposing concrete types
