# Fundamental Types

Graphix has a few fundamental data types, the Graphix shell is a good way to
explore them by trying out small Graphix expressions. You can run the Graphix
shell by invoking `graphix` with no arguments.

## Numbers

`i32`, `u32`, `i64`, `u64`, `f32`, `f64`, and `decimal` are the fundamental
numeric types in Graphix. Literals are written with their type prefixed, except
for `i64` and `f64` which are written bare. for example, `u32:3` is a `u32`
literal value.

`decimal` is an exact decimal representation for performing financial
calculations without rounding or floating point approximation errors.

The basic arithmetic operations are implemented on all the number types with all
the other number types.

| Operation | Operator |
|-----------|----------|
| Add       |     +    |
| Subtract  |     -    |
| Multiply  |     *    |
| Divide    |     /    |
| Mod       |     %    |

The compiler will let you do arithmetic on different types of numbers directly
without casting, however the return type of the operation will be the set of all
the types in the operation, representing that either type could be returned. If
you try to pass this result to a function that wants a specific numeric type, it
will fail at compile time.

```graphix
〉1. + 1
-: [i64, f64]
2
〉let f = |x: i64| x * 10
〉f(1. + 1)
error: in expr

Caused by:
    0: at: line: 1, column: 3, in: (f64:1. + i64:1)
    1: at: line: 1, column: 3, in: (f64:1. + i64:1)
    2: type mismatch '_1046: i64 does not contain [i64, f64]
```

Division by zero raises an error to the nearest error handler (see [Error Handling](error.md)). If unhandled, it will be printed to stderr by the shell.

```graphix
〉0 / 0
-: i64
unhandled error: error:"in expr at line: 1, column: 1 attempt to divide by zero"
```

The result type of the division is still `i64`, but the actual value is an error. You can handle division by zero using the `?` or `` ` `` operators:

```graphix
〉(0 / 0)?
-: Result<i64, Error<`ArithError(string)>>
error:["ArithError", "in expr at line: 1, column: 2 attempt to divide by zero"]
```

If division by zero occurs and is not handled, the expression will not update until the divisor changes to a non-zero value. This can cause issues if your program depends on continuous updates from that expression.

Overflow and underflow are handled by wrapping:

```graphix
〉u32:0 - u32:1
-: u32
4294967295
```

#### v32, z32, v64, z64

These number types are the same as the normal types except when they
are sent over the wire via netidx (or written to a file) they use
variable width encoding instead of normal encoding. The number of
bytes used varies for 64 bit numbers to between 1 and 10. Small
numbers (below 127) are encoded in 1 byte, larger number use more
bytes. The type correspondence is,

| Compact | Normal |
|---------|--------|
| v32     | u32    |
| z32     | i32    |
| v64     | u64    |
| z64     | i64    |


### Number Sets

There are a few sets of number types that classify numbers into various kinds.
`Number` being the most broad, it contains all the number types. `Int` contains
only integers, `Real` contains only reals (decimal plus the two float types),
`SInt` contains signed integers, `UInt` contains unsigned integers.

## Bool

Boolean literals are written as `true` and `false`, and the name of the boolean
type is `bool`.

Boolean expressions using `&&`, `||`, and `!` are supported. These operators
only operate on `bool`. They can be grouped with parenthesis. For example,

```graphix
〉true && false
-: bool
false
〉true || false
-: bool
true
〉!true
-: bool
false
〉!1
error: in expr

Caused by:
    0: at: line: 1, column: 2, in: i64:1
    1: type mismatch bool does not contain i64
```

## Duration

A time duration. The type name is `duration`, and the literals are written as,
`duration:1.0s`, `duration:1.0ms`, `duration:1.0us`, `duration:1.0ns`. Durations
can be added, and can be multiplied and divided by scalars.

```graphix
〉duration:1.0s + duration:1.0s
-: duration
2.s
〉duration:1.0s * 50
-: duration
50.s
〉duration:1.0s / 50
-: duration
0.02s
```

## DateTime

A date and time in the UTC time zone. The type name is `datetime` and literals
are written in RFC3339 format inside quotes. For example,
`datetime:"2020-01-01T00:00:00Z"`. You can add and subtract `duration` from
`datetime`.

```graphix
〉datetime:"2020-01-01T00:00:00Z" + duration:30.s
-: datetime
2020-01-01 00:00:30 UTC
```

You can enter `datetime` literals in local time and they will be converted to UTC. For example,

```graphix
〉datetime:"2020-01-01T00:00:00-04:00"
-: datetime
2020-01-01 04:00:00 UTC
```

## String

Strings in Graphix are UTF8 encoded text. The type name is `string` and the
literal is written in quotes `"this is a string"`. C style escape sequences are
supported, `"this is \" a string with a quote and a \n"`. Non printable
characters such as newline will be escaped by default when strings are printed
to the console, you can use `print` to print the raw string including control
characters.

### String Interpolation

String literals can contain expressions that will be evaluated and joined to the string,
such expressions are surrounded by unescaped `[]` in the string. For example,

```graphix
〉let row = 1
〉let column = 999
〉"/foo/bar/[row]/[column]"
-: string
"/foo/bar/1/999"
```

Values in an interpolation need not be strings, they will be cast to a string
when they are used. You can write a literal `[` or `]` in a string by escaping
it.

```graphix
〉"this is a string with a \[ and a \] but it isn't an interpolation"
-: string
"this is a string with a [ and a ] but it isn't an interpolation"
```

## Any

The `Any` type is a type that unifies with any other type, it corresponds to the
underlying variant type that represents all values in Graphix (and netidx). It
is not used very often, as it provides very few guarantees, however it has it's
place. For example, `Any` is the type returned by `net::subscribe`, indicating
that any valid netidx value can come from the network. Usually the first thing
you do with an `Any` type is call `cast` to turn it into the type you expect (or
an error), or use a `select` expression to match it's type (more on select later).

## Null

Null is nothing, just like in many other languages. Unlike most other languages
`null` is a type not a catch all. If the type of a value does not include `null`
then it can't be null. The set `['a, null]` (alias `Option<'a>`) is commonly
used to represent things that will sometimes return `null`.

## Array

Arrays are immutable, contiguous, and homogeneous. They are
parameterized, `Array<string>` indicates an array of strings. Arrays
are zero indexed `a[0]` is the first element. Array elements can be
any type, including other arrays at arbitrary levels of nesting. There
is a special `array` type that acts like `Array<Any>`, that represents
the fundamental array type in the underlying value representation.
Array literals are written like `[x, y, z]`. There are many functions
in the `array` module of the standard library for working with arrays.

### Array Slicing and Indexing

Graphix supports array subslicing, the syntax will be familiar to Rust programmers.

- `a[2..]` a slice from index 2 to the end of the array
- `a[..4]` a slice from the beginning of the array to index 3
- `a[1..3]` a slice from index 1 to index 2
- `a[-1]` the last element in the array
- `a[-2]` the second to last element in the array

`..=` is not supported however, the second part of the slice will always be the
exclusive bound. Literal numbers can always be replaced with a Graphix
expression, e.g. `a[i..j]` is perfectly valid.

### Mutability and Implementation

Arrays are not mutable, like all other Graphix values. All operations that
"change" an array, actually create a new array leaving the old one unchanged.
This is even true of the connect operator, which we will talk more about later.

There are a couple of important notes to understand about the implementation of
Arrays.

- Arrays are memory pooled, in almost all cases (besides really huge arrays)
  creating an array does not actually allocate any memory, it just reuses a
  previously used array that has since become unused. This makes using arrays a
  lot more efficient than you might expect.

- Arrays are contiguous in memory. This means they are generally very
  memory efficient, each element is 2 machine words, and O(1) to
  access. However there are a few cases where this causes a problem,
  such as building up an array by appending one element at a
  time. This is sadly an O(n^2) operation on arrays. You may wish to
  use another data structure, such as a map (which would be O(n
  log(n)) if you need to build a large data structure this way.

- Array slices are zero copy, and also pooled. They simply create a
  light weight view into the array. This means algorithms that
  progressively deconstruct an array by slicing are O(N) not O(N^2)
  and the constants are very fast.

## Tuples

Tuples are written `(x, y)`, they can be of arbitrary length, and each element
may have a different type. Tuples may be indexed using numeric field indexes.
Consider

```graphix
let x = (1, 2, 3, 4);
x.0
```

Will print 1.

## Map

Maps in Graphix are key-value data structures with O(log(N)) lookup, insert, and
remove operations. Maps are parameterized by their key and value type, for
example `Map<string, i64>` indicates a map with string keys and integer values.
There are many functions for working with maps in the `map` standard library
module

### Map Literals

Maps can be constructed using the `{key => value}` syntax:

```graphix
〉{"a" => 1, "b" => 2, "c" => 3}
-: Map<'_1893: string, '_1895: i64>
{"a" => 1, "b" => 2, "c" => 3}
```

Keys and values can be any Graphix type, for example here is a map where the key
is a `Map<string, i64>`.

```graphix
{{"foo" => 42} => "foo", {"bar" => 42} => "bar"}
-: Map<'_1919: Map<'_1915: string, '_1917: i64>, '_1921: string>
{{"bar" => 42} => "bar", {"foo" => 42} => "foo"}
```

### Map Indexing

Maps can be indexed using the `map{key}` syntax to retrieve values:

```graphix
〉let m = {"a" => 1, "b" => 2, "c" => 3}
〉m{"b"}
-: ['_1907: i64, Error<`MapKeyError(string)>]
2
```

If a key is not present in the map, indexing returns a `MapKeyError`:

```graphix
〉m{"missing"}
-: ['_1907: i64, Error<`MapKeyError(string)>]
error:["MapKeyError", "map key \"missing\" not found"]
```

### Mutability and Implementation

Like all Graphix values, maps are immutable. All operations that "change" a map
actually create a new map, leaving the original unchanged. Maps are memory
pooled and very efficient - creating new maps typically reuses existing memory
rather than allocating new memory.

Maps maintain their key-value pairs in a balanced tree structure, ensuring
O(log(N)) performance for all operations regardless of map size.

## Error

Error is the built in error type. It carries a type parameter indicating the
type of error, for example `Error<`MapKeyError(string)>` is an error that
carries a `` `MapKeyError `` variant. You can access the inner error value
using `e.0` e.g.,

```graphix
〉let e = error(`MapKeyError("no such key"))
〉e.0
-: `MapKeyError(string)
`MapKeyError("no such key")
```

More information about dealing with errors is available in the section on error
handling.
