# Tuples

Tuples are like structs where the field names are numbers, or like Arrays where
every element can be a different type and the length is known at compile time. For example,

```
(string, i64, f64)
```

Is an example of a three tuple.

## Field Accessors

You can access the fields of a tuple by their field number, e.g. .0, .1, .2, etc.

```
〉let t = (1, 2, 3)
〉t.0 == 1
-: bool
true
```

Tuple fields may also be bound in a pattern match in a let bind, a select arm, or a function argument. For example,

```
〉let (f0, f1, f2) = t
〉f0
-: i64
1
```
