# Named Types

You can name types to avoid having to type them more than once. A named type
is an alias for the full structure of the type it references, unless it is
defined as `Abstract<..>` (see [nominal types](#nominal-types) below). The
fully written out type is the same as the alias and visa versa. Lets go back to
our foods example from the section on variants.

```
type FoodKind = [
  `Vegetable,
  `Fruit,
  `Meat,
  `Grain,
  `Other(string)
];

let v: FoodKind = `Vegetable;
let f = |food: FoodKind| ...
```

Aliases are very useful for more complex types that are used many times.
Selective annotations can also help the type checker make sense of complex
program structures.

## Nominal Types

A type whose whole definition is `Abstract<..>` around a representation is
nominal: it is a new type, different from its representation and from any
other type with the same representation. Where its definition is visible, its
name is its constructor, `.0` reads the representation back, and the
constructor is also a pattern. A local definition is visible inside its
defining body, including each call of that body and its nested scopes; a
sibling block or sibling function does not gain access to the representation.

For example:

```graphix
type Meters = Abstract<f64>;
type Feet = Abstract<f64>;

let add = |a: Meters, b: Meters| -> Meters Meters(a.0 + b.0);
let to_meters = |f: Feet| -> Meters {
    let Feet(x) = f;
    Meters(x * 0.3048)
};
add(Meters(1.5), to_meters(Feet(10.0)))
```

Both types hold an `f64`, but `add(Meters(1.5), Feet(10.0))` is refused:
`Meters does not contain Feet`. A value carries its type's identity at run
time too, and prints as its constructor applied to its representation
(`Meters(4.548)`). The type test `Meters as m` tells nominal types apart in a
select. An interface can hide the representation, so that only the module
that defines the type can construct or read it: see
[abstract types](../modules/interfaces.md#abstract-types). Each construction
costs one allocation, so nominal types suit handles and newtypes better than
hot data.

A nominal type may take parameters, and they are invariant: the
representation is hidden, so nothing outside it can say a `Box<i64>` is a
`Box<[i64, string]>`. A constructed value's parameters are decided by where
it goes, with its payload's type as the least they can be:

```graphix
type Box<'a> = Abstract<'a>;
let mk = |x| Box(x);
let wide: Box<[i64, string]> = mk(1);
let either: [Box<i64>, Box<string>] = Box("hi");
let narrow = Box(2);
narrow.0 + 1
```

`wide` is a `Box<[i64, string]>` and `either` a `Box<string>`. `narrow`
goes nowhere wider, so it is a `Box<i64>`. `let b: Box<string> = Box(1)` is
refused: `Box<string> does not contain Box<i64>`.

In the next section you'll see that we can do a lot more with them.
