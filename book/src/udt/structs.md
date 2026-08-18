# Structs

Structs allow you to define a type that groups any number of fields together as
one data object. The fields are accessible by name anywhere in the program. For
example,

```graphix
{ foo: string, bar: i64 }
```

defines a struct with two fields, `foo` and `bar`. `foo` has type `string` and
`bar` has type `i64`. We can assign a struct of this type to a variable, and
pass it around just like any other data object. For example,

```graphix
let s = { foo: "I am foo", bar: 42 }
println("the struct s is [s]")
```

will print

```
the struct s is {bar: 42, foo: "I am foo"}
```

## Field Names and Reserved Words

Field names may be any lowercase identifier — including reserved words
like `duration`, `string`, or `type`, which often appear in data
mirrored from outside. Reserved words still can't name bindings, so a
reserved-word field must always use the explicit `name: value` form:
shorthand like `{duration}` refers to a binding named `duration`, which
cannot exist.

```graphix
〉let p = { duration: duration:1.5s, string: "label" }
〉p.duration
-: duration
duration:1.5s
```

## Field References

Struct fields can be referenced with the .field notation. That is,

```graphix
〉s.foo
-: string
"I am foo"
```

A more complex expression that results in a struct (such as a function call),
must be placed in parenthesis before the .field. For example,

```graphix
〉let f = || s
〉(f()).foo
-: string
"I am foo"
```

## Mutability and Functional Update

Structs are not mutable, like everything else in Graphix. However There is a
quick way create a new struct from an existing struct with only some fields
changed. This is called functional struct update syntax. For example,

```graphix
{ s with bar: 21 }
```

Will create a new struct with all the same fields as `s` except `bar` which will be set to 21. e.g.

```graphix
〉{ s with bar: 21 }
-: {bar: i64, foo: string}
{bar: 21, foo: "I am foo"}
```

Notice that the type printed is the full type of the struct, this is because of structural typing.

## Implementation

Structs are implemented as a sorted array of pairs, the field name being the
first element of the pair, and the data value being the second. The array is
sorted by the field name, and because of this it is not necessary to do any
matching when the field is accessed at run time, the index of the field
reference is pre computed at compile time, so field references are always O(1).
The reason why the fields are stored at all is so they can be used on the
network and in files without losing information. Because structs are array
backed, they are also memory pooled, and so making a new struct does not usually
allocate any memory, but instead reuses objects from the pool.

The `cast` operator can cast an unsorted array of pairs where the first element
is a string to a struct type. For example,

```graphix
〉cast<{foo: string, bar: i64}>([["foo", "I am foo"], ["bar", 42]])$
-: {bar: i64, foo: string}
{bar: 42, foo: "I am foo"}
```

Structs along with `cast` can be used to communicate complex values over the
network as long as the two sides agree on what the type is supposed to be.
