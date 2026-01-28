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
val add: fn(i64, i64) -> i64;
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
type Abstract;
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

Re-export items from other modules:

```graphix
use other::module;
```

## A Complete Example

Let's create a simple math utilities module with an interface.

**math.gxi** (interface):
```graphix
/// Add two numbers
val add: fn(i64, i64) -> i64 throws Error<ErrChain<`ArithError(string)>>;

/// Subtract the second number from the first
val sub: fn(i64, i64) -> i64 throws Error<ErrChain<`ArithError(string)>>;

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

let constants = Constants {
    pi: 3.14159265359,
    e: 2.71828182845
};

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
val filter: fn(Array<'a>, fn('a) -> bool throws 'e) -> Array<'a> throws 'e;
```

## Polymorphic Functions

Interface files fully support polymorphic type signatures:

```graphix
/// Transform each element of an array using function f
val map: fn(Array<'a>, fn('a) -> 'b throws 'e) -> Array<'b> throws 'e;

/// Fold an array into a single value
val fold: fn(Array<'a>, 'b, fn('b, 'a) -> 'b throws 'e) -> 'b throws 'e;
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

Sub-modules can see everything in their parent that was declared before the
`mod` statement that declared them. This includes private items not exported in
the interface.

The position of the `mod` statement controls what the sub-module can see:

- **Module declared only in interface**: The sub-module can see everything
  declared before the item it follows in the implementation. For example, if the
  interface has `val foo; mod child; val bar;`, and the implementation has 
  `let foo = ...; let bar = ...;`, then `child` can see `foo` but not `bar`.

- **Module declared only in implementation**: The sub-module can see everything
  declared before its `mod` statement, but it is not exported (not accessible to
  users of the parent module).

- **Module declared in both**: The position in the implementation controls what
  the sub-module can see, while the interface declaration exports it. Use this
  for precise control over sub-module visibility.

Example:

```graphix
// parent.gxi
val public_helper: fn(i64) -> i64;
mod child;
```

```graphix
// parent.gx
let private_setup = ...;
let public_helper = |x| x + 1;

mod child;  // child can see private_setup and public_helper
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

Abstract types allow you to hide the concrete representation of a type from users of
your module. This is a powerful encapsulation mechanism that lets you change the
internal representation without affecting code that uses your module.

### Declaring Abstract Types

In an interface file, declare an abstract type by omitting the `= definition` part:

```graphix
type Handle;
type Container<'a>;
type NumericBox<'a: Number>;
```

The implementation file must provide a concrete definition for each abstract type:

```graphix
type Handle = { id: i64, name: string };
type Container<'a> = Array<'a>;
type NumericBox<'a: Number> = { value: 'a };
```

### How Abstract Types Work

When code outside the module references an abstract type, it sees only the type name,
not the underlying representation. This means:

- Users cannot construct values of the abstract type directly
- Users cannot pattern match on the internal structure
- Users must use functions exported by the module to create and manipulate values

This provides true encapsulation - the implementation can change the concrete type
without breaking any code that uses the module, as long as the exported functions
still work.

### Example: Encapsulated Counter

**counter.gxi**:
```graphix
/// An opaque counter type
type Counter;

/// Create a new counter starting at the given value
val make: fn(i64) -> Counter;

/// Get the current value
val get: fn(Counter) -> i64;

/// Increment the counter every time trig updates
val increment: fn(#trig: Any, &Counter) -> null throws Error<ErrChain<`ArithError(string)>>;
```

**counter.gx**:
```graphix
// Implementation detail: counter is just an i64
// We could change this to a struct later without breaking users
type Counter = i64;

let make = |x: i64| -> Counter x;
let get = |c: Counter| -> i64 c;
let increment = |#trig: Any, c: &Counter| -> null { *c <- trig ~ *c + 1; null }
```

**main.gx**:
```graphix
mod counter;

let c = counter::make(0);
counter::increment(#trig:null, &c);
let value = counter::get(c)  // 1
```

### Parameterized Abstract Types

Abstract types can have type parameters, allowing generic containers:

```graphix
// interface
type Box<'a>;
val wrap: fn('a) -> Box<'a>;
val unwrap: fn(Box<'a>) -> 'a;
```

```graphix
// implementation
type Box<'a> = { value: 'a };
let wrap = |x: 'a| -> Box<'a> { value: x };
let unwrap = |b: Box<'a>| -> 'a b.value
```

### Constrained Type Parameters

Type parameters on abstract types can have constraints. The interface and
implementation must have matching constraints:

```graphix
// interface - constraint required
type NumericWrapper<'a: Number>;
val wrap: fn('a) -> NumericWrapper<'a>;
val double: fn(NumericWrapper<'a>) -> 'a throws Error<ErrChain<`ArithError(string)>>;
```

```graphix
// implementation - same constraint required
type NumericWrapper<'a: Number> = 'a;
let wrap = |x: 'a| -> NumericWrapper<'a> x;
let double = |w: NumericWrapper<'a>| -> 'a w + w
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

### Abstract Types vs Type Aliases

Don't confuse abstract types with type aliases:

| Declaration | Meaning |
|-------------|---------|
| `type T;` | Abstract type - concrete definition hidden |
| `type T = i64;` | Type alias - `T` is publicly known to be `i64` |

Use abstract types when you want encapsulation. Use type aliases when you want
to give a convenient name to a type that users can still see and use directly.

## Best Practices

1. **Document in interfaces**: Put documentation comments in the `.gxi` file since that's what users see
2. **Minimal interfaces**: Only export what users need; keep implementation details private
3. **Stable interfaces**: Think carefully before changing an interface, as it may break dependent code
4. **Type aliases**: Export type aliases in the interface to give users convenient names for complex types
5. **Use abstract types for encapsulation**: When you want to hide implementation details and reserve the right to change them, use abstract types instead of exposing concrete types
