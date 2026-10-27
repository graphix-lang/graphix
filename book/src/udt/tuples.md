# Tuples

Tuples are like structs where the field names are numbers, or like Arrays where
every element can be a different type and the length is known at compile time. For example,

```graphix,fragment
(string, i64, f64)
```

Is an example of a three tuple.

## Field Accessors

You can access the fields of a tuple by their field number, e.g. .0, .1, .2, etc.

```graphix
〉let t = (1, 2, 3)
〉t.0 == 1
-: bool
true
```

A field can be read from a union of tuple types when every member has that
field number, even when their lengths differ. Its type is the union of the
member fields' types.

Tuple fields may also be bound in a pattern match in a let bind, a select arm, or a function argument. For example,

```graphix
〉let (f0, f1, f2) = t
〉f0
-: i64
1
```
