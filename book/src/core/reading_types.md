# Reading Type Signatures

Throughout this book and in the standard library documentation, you'll encounter function type signatures. This guide will help you understand what they mean. Don't try to memorize everything here - just use this as a reference when you encounter unfamiliar notation.

## Basic Function Signatures

The simplest function signature looks like this:

```graphix
val double: fn(x: i64) -> i64
```

This breaks down into:
- `fn(...)` - this is a function
- `x: i64` - takes one parameter named `x` of type i64 (64-bit integer)
- `-> i64` - returns a value of type i64

Every positional parameter in a function type carries a name as well as a type. The name is documentation for the reader and is what your editor shows in hover and completion popups; it does not change the call (you still pass arguments positionally, in order).

Another example:

```graphix
val concat: fn(a: string, b: string) -> string
```

Takes two strings, returns a string.

## Type Parameters (Generics)

Type parameters (like generics in other languages) are written with a single quote followed by an identifier: `'a`, `'b`, `'e`, etc.

### Simple Type Parameters

```graphix
val identity: fn(x: 'a) -> 'a
```

This means: "takes a value of any type `'a` and returns a value of the same type `'a`". The `identity` function could work with integers, strings, or any other type.

```graphix
val first: fn(a: Array<'a>) -> 'a
```

This means: "takes an array of any type `'a` and returns a single element of type `'a`". If you pass an `Array<string>`, you get back a `string`. If you pass an `Array<i64>`, you get back an `i64`.

### Multiple Type Parameters

```graphix
val map: fn(a: Array<'a>, f: fn(x: 'a) -> 'b) -> Array<'b>
```

This function takes:
- An array of type `'a`
- A function that transforms `'a` into `'b`
- Returns an array of type `'b`

The types `'a` and `'b` can be the same or different.

## Optional Labeled Arguments

Arguments prefixed with `?#` are optional and labeled:

```graphix
val text: fn(?#style: Style, content: string) -> Widget
```

This function can be called in two ways:

```graphix
text("Hello")                           // style uses default value
text(#style: my_style, "Hello")        // style is specified
```

### Order Flexibility

Labeled arguments can be provided in any order, but must come before positional arguments:

```graphix
val widget: fn(?#width: i64, ?#height: i64, content: string) -> Widget

// All of these are valid:
widget("text")
widget(#width: 100, "text")
widget(#height: 50, #width: 100, "text")
widget(#height: 50, "text")
```

## Required Labeled Arguments

Arguments with `#` but no `?` are required but labeled:

```graphix
val input_handler: fn(
    #handle: fn(e: Event) -> Response,
    w: &Widget
) -> Widget
```

You must provide `#handle`, but it doesn't have to be in the first position. However, it must come before the unlabeled `&Widget` argument:

```graphix
input_handler(#handle: my_handler, &my_widget)
```

## Variadic Arguments

The `@args` notation means a function accepts any number of arguments:

```graphix
val sum: fn(@args: i64) -> i64
```

You can call this with any number of integers:

```graphix
sum(1, 2, 3)
sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
```

### Variadic with Required Arguments

Sometimes a function requires at least one argument:

```graphix
val max: fn(first: 'a, @args: 'a) -> 'a
```

The first `'a` is required, then any number of additional arguments of the same type.

## Reference Types

An ampersand `&` before a type means "reference to" rather than the value itself:

```graphix
val text: fn(content: &string) -> Widget
```

This takes a *reference* to a string, not the string value directly. References are important for:

1. **Efficiency** - avoid copying large data structures
2. **Reactivity** - updating a referenced value triggers updates without rebuilding entire structures

Create a reference with `&` and dereference (get the value) with `*`:

```graphix
let s = "Hello";
let r = &s;      // r is a reference to s
let v = *r;      // v is the value "Hello"
```

In function signatures, `&T` in a parameter position means the function expects a reference. In widget examples, you'll often see:

```graphix
block(#title: &line("My Title"), &my_widget)
```

The `&line(...)` creates a reference to the line, and `&my_widget` is a reference to the widget.

For a deeper dive, see [References](../udt/references.md).

## Error Types (throws)

When a function can throw errors, the signature includes `throws`:

```graphix
val divide: fn(a: i64, b: i64) -> i64 throws `DivideByZero
```

This function returns `i64` if successful, but might throw a `DivideByZero` error.

### Multiple Error Types

A function can throw multiple error types:

```graphix
val parse_and_divide: fn(num: string, denom: string) -> i64 throws [`ParseError, `DivideByZero]
```

### Generic Error Types

Often error types are parameterized:

```graphix
val filter: fn(v: 'a, f: fn(x: 'a) -> bool throws 'e) -> 'a throws 'e
```

This means: the `filter` function itself doesn't throw errors, but if the function you pass to it throws errors of type `'e`, then `filter` will also throw those same errors.

### Result Type

The `Result` type is a convenient way to represent success or error:

```graphix
type Result<'r, 'e> = ['r, Error<'e>]
```

So a function signature like:

```graphix
val parse: fn(s: string) -> Result<i64, `ParseError>
```

Returns either an `i64` (success) or an `Error<`ParseError>` (failure).

See [Error Handling](./error.md) for complete details on working with errors.

## Set Types

Square brackets `[...]` denote a set type - the value can be any one of the types in the set:

```graphix
val process: fn(x: [i64, string]) -> string
```

This function accepts either an `i64` or a `string`, and returns a `string`.

### Optional Types (Nullable)

The pattern `[T, null]` means "T or nothing":

```graphix
val find: fn(haystack: Array<string>, needle: string) -> [string, null]
```

Returns a string if found, `null` if not found. This is aliased as `Option<T>`:

```graphix
type Option<'a> = ['a, null]
val find: fn(haystack: Array<string>, needle: string) -> Option<string>
```

### Nested Sets

Types can nest arbitrarily:

```graphix
val sum: fn(@args: [Number, Array<[Number, Array<Number>]>]) -> Number
```

This accepts numbers, arrays of numbers, or even arrays of (numbers or arrays of numbers). The flexibility allows you to call:

```graphix
sum(1, 2, 3)
sum([1, 2], [3, 4])
sum(1, [2, 3], 4)
```

### Function Constraints

Type variables in functions can have constraints:

```graphix
let sum<'a: Number>(@args: 'a) -> 'a
```

This is subtly different from the `sum` examples earlier. This says,
`sum` is a function that takes any number of arguments of the same
type and returns the same type, and that type must be some kind of
number.

### Auto Parameters

The compiler often infers type variables (and constrains them) by
itself during the type inference process.

if we compile a function with no type constraints, such as:

```graphix
let f = |x, y| x + y
```

It's type will be something like:

```graphix
val f: fn<
  '_2069: Number,
  '_2067: Number,
  '_2071: Number
>(x: '_2067, y: '_2069) -> '_2071
```

The compiler has inferred a bunch of properties here,

- both arguments must be of type `Number`, that's what the constraints
on `'_2067: Number` and `_2069: Number` mean.
- both arguments need not be the same type, hence they are different type variables
- the return type will also be a number, hence `'_2071: Number`, but
  it may not be the same type of number as either of the arguments.

Because unchecked arithmetic operators like `+` log errors and return
bottom on overflow rather than throwing, there is no `throws` clause
in the type signature.

In the shell this type signature is made a bit more complex by the
shell also telling you what type variables are currently bound to, or
`unbound` if they aren't bound. So in the shell this pops out as,

```graphix
〉f
-: fn<'_2069: unbound: Number, '_2067: unbound: Number, '_2071: unbound: Number>(x: '_2067: unbound, y: '_2069: unbound) -> '_2071: unbound
```

The constraint `'_2069: unbound: Number` is read as. _2069 is not
currently bound to a type but is constrained to type `Number`. This is
all useful information, even though it's intimidating at first it's
worth putting in the work to learn to decipher it.

## Putting It All Together

Let's decode some complex real-world signatures:

### Example 1: TUI Table Widget

```graphix
val table: fn(
    ?#header: &Row,
    ?#selected: &i64,
    ?#row_highlight_style: &Style,
    ?#highlight_symbol: &string,
    rows: &Array<&Row>
) -> Widget
```

Breaking it down:
- `?#header: &Row` - optional labeled argument, reference to a Row
- `?#selected: &i64` - optional labeled argument, reference to selected index
- `?#row_highlight_style: &Style` - optional labeled argument, reference to a Style
- `?#highlight_symbol: &string` - optional labeled argument, reference to symbol string
- `rows: &Array<&Row>` - required positional argument, reference to array of row references
- `-> Widget` - returns a Widget

All parameters are references because the table needs to react to changes without rebuilding.

### Example 2: Filter Function

```graphix
val filter: fn(v: 'a, pred: fn(x: 'a) -> bool throws 'e) -> 'a throws 'e
```

Breaking it down:
- `v: 'a` - a value of any type
- `pred: fn(x: 'a) -> bool throws 'e` - a predicate function that:
  - Takes the same type `'a`
  - Returns bool
  - Might throw errors of type `'e`
- `-> 'a` - returns the same type as input
- `throws 'e` - propagates any errors from the predicate

### Example 3: Queue Function

```graphix
val queue: fn(#clock: Any, v: 'a) -> 'a
```

Breaking it down:
- `#clock: Any` - required labeled argument of any type, just used as an event source
- `v: 'a` - a value of any type
- `-> 'a` - returns values of the same type

Call it like: `queue(#clock: my_timer, my_value)`

### Example 4: Array Map

```graphix
val map: fn(a: Array<'a>, f: fn(x: 'a) -> 'b throws 'e) -> Array<'b> throws 'e
```

Breaking it down:
- `a: Array<'a>` - array of any type `'a`
- `f: fn(x: 'a) -> 'b throws 'e` - transformation function that:
  - Takes type `'a` (array element type)
  - Returns type `'b` (result element type)
  - Might throw errors of type `'e`
- `-> Array<'b>` - returns array of transformed type
- `throws 'e` - propagates errors from the transform function

## Quick Reference Table

| Notation | Meaning | Example |
|----------|---------|---------|
| `'a`, `'b`, `'e` | Type parameter (generic) | `fn(x: 'a) -> 'a` |
| `'_23`, `'_24`, `'_25` | Inferred type parameter (generic) | `fn(x: '_23) -> '_23` |
| `name: T` | Named positional argument | `fn(x: i64) -> i64` |
| `?#param` | Optional labeled argument | `fn(?#x: i64 = 0)` |
| `#param` | Required labeled argument | `fn(#x: i64)` |
| `@args` | Variadic (any number of args) | `fn(@args: i64)` |
| `&T` | Reference to type T | `fn(s: &string)` |
| `throws 'e` | Can throw errors of type 'e | `fn() -> i64 throws 'e` |
| `[T, U]` | T or U (set/union type) | `[i64, null]` |
| `->` | Returns | `fn(x: i64) -> string` |
| `Array<T>` | Array of T | `Array<string>` |
| `Map<K, V>` | Map with keys K, values V | `Map<string, i64>` |
| `Error<'e>` | Error containing type 'e | `Error<\`ParseError>` |
| `Result<'r, 'e>` | Success 'r or Error 'e | `Result<i64, \`Err>` |
| `Option<'a>` | Value 'a or null | `Option<string>` |

## Tips for Reading Signatures

1. **Start with the basics** - identify parameters and return type
2. **Look for type parameters** - they tell you about genericity
3. **Check for optional/labeled args** - they indicate flexibility in calling
4. **Note reference types** - important for reactivity
5. **Watch for throws** - you'll need error handling
6. **Don't panic at complexity** - break it down piece by piece

Remember: you don't need to memorize these patterns. As you use Graphix, you'll naturally become familiar with common signatures. This guide is here whenever you need a reminder!

## See Also

- [Fundamental Types](./fundamental_types.md) - Built-in types and type sets
- [Functions](../functions/overview.md) - Creating and using functions
- [Error Handling](./error.md) - Working with errors and the throws system
- [References](../udt/references.md) - Deep dive into reference types
- [User Defined Types](../udt/overview.md) - Structural typing and custom types
