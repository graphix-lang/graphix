# Select

Select lets us create a graph node with multiple possible output paths that
will choose one path for each value based on a set of conditions. Kind of like,

```
                     ┌─────────────────────> if foo > 0 => ...
                     │
                     │
ref(foo) ──> select ─┼─────────────────────> if foo < 0 => ...
                     │
                     │
                     └─────────────────────> otherwise => ...
```

is written as

```graphix
select foo {
  n if n > 0 => ...,
  n if n < 0 => ...,
  n => ...
}
```

select takes an expression as an argument and then evaluates one or more "arms".
Each arm consists of an optional type predicate, a destructuring pattern, and an
optional guard clause. If the type predicate matches, the pattern matches, and
the guard evaluates to true then the arm is "selected". Only one arm may be
selected at a time, the arms are evaluated in lexical order, and first arm to be
selected is chosen as the one and only selected arm.

The code on the right side of the selected arm is the only code that is
evaluated by select, all other code is "asleep", it will not be evaluated
until it is selected (and if it has netidx subscriptions or published values
they will be unsubscribed and unpublished until it is selected again).

## Matching Types

Consider we want to select from a value of type `[Array<i64>, i64, null]`,

```graphix
let x: [Array<i64>, i64, null] = null;
x <- sys::time::timer(duration:1.s, false) ~ [1, 2, 3, 4, 5];
x <- sys::time::timer(duration:2.s, false) ~ 7;
select x {
  Array<i64> as a => array::fold(a, 0, |s, x| s + x),
  i64 as n => n,
  null as _ => 42
}
```

This program will print 42, 15, 7 and then wait. The compiler will check that
you have handled all the possible cases. If we remove the null case from this
select we will get a compile error.

```
$ graphix test.gx
Error: in file "test.gx"

Caused by:
    missing match cases type mismatch [i64, Array<i64>] does not contain [[i64, null], Array<i64>]
```

If you read this carefully you can see that the compiler is building up a set of
types that we did match, and checking that it contains the argument type. This
goes both ways, a match case that could never match is also an error.

```graphix
let x: [Array<i64>, i64, null] = null;
x <- sys::time::timer(duration:1.s, false) ~ [1, 2, 3, 4, 5];
x <- sys::time::timer(duration:2.s, false) ~ 7;
select x {
  Array<i64> as a => array::fold(a, 0, |s, x| s + x),
  i64 as n => n,
  f64 as n => cast<i64>(n)?,
  null as _ => 42
}
```

Here we've added an `f64` match case, but the argument type can never contain an
`f64` so we will get a compile error.

```
$ graphix test.gx
Error: in file "test.gx"

Caused by:
    pattern f64 will never match null, unused match cases
```

The diagnostic message gives you an insight into the compiler's thinking. What
it is saying is that, by the time it's gotten to looking at the `f64` pattern,
the only type left in the argument that hasn't already been matched is `null`,
and since `f64` doesn't unify with `null` it is sure this pattern can never
match.

Guarded patterns can always not match because of the guard, so they do not
subtract from the argument type set. You are required to match without a guard
at some point. No analysis is done to determine if your guard covers the entire
range of a type.

```graphix
let x: [Array<i64>, i64, null] = null;
x <- sys::time::timer(duration:1.s, false) ~ [1, 2, 3, 4, 5];
x <- sys::time::timer(duration:2.s, false) ~ 7;
select x {
  Array<i64> as a => array::fold(a, 0, |s, x| s + x),
  i64 as n if n > 10 => n,
  null as _ => 42
}
```

This will fail with a missing match case because the `i64` pattern is guarded
and no unguarded pattern exists that matches `i64`.

```
$ graphix test.gx
Error: in file "test.gx"

Caused by:
    missing match cases type mismatch [null, Array<i64>] does not contain [[i64, null], Array<i64>]
```

This is the same error you would get if you omitted the `i64` match case
entirely.

## Matching Structure

The type predicate is optional in a pattern, and the more commonly used pattern
is structural. Graphix supports several kinds of structural matching,

- array slices
- tuples
- structs
- variants
- literals, ignore

NB: In most contexts you can match the entire value as well as parts of it's
structure by adding a `v@` pattern before the pattern. You will see this in many
of the examples.

### Slice Patterns

Suppose we want to classify arrays that have at least two elements vs arrays
that don't, and we want to return a variant with a triple of the first two
elements and the rest of the array or `Short with the whole array.

```graphix
let a = [1, 2, 3, 4];
a <- [1];
a <- [5, 6];
select a {
  [x, y, tl..] => `Ok((x, y, tl)),
  a => `Short(a)
}
```

This program will print,

```
$ graphix test.gx
`Ok((1, 2, [3, 4]))
`Short([1])
`Ok((5, 6, []))
```

The following kinds of slice patterns are supported,

- whole slice, with binds, or literals, e.g. `[1, x, 2, y]` matches a 4 element
  array and binds it's 2nd and 4th element to `x` and `y` respectively.

- head pattern, like the above program, e.g. `[(x, y), ..]` matches the first
  pair in an array of pairs and ignores the rest of the array, binding the pair
  elements to `x` and `y`. You can also name the remainder, as we saw, e.g.
  `[(x, y), tl..]` does the same thing, but binds the rest of the array to `tl`

- tail pattern, just like the head pattern, but for the end of the array. e.g.
  `[hd.., {foo, bar}]` matches the last element of an array of structs with
  fields `foo` and `bar`, binding `hd` to the array minus the last element, and
  `foo` to field foo and `bar` to field bar.

Structure patterns (all of the different types) can be nested to any depth.

### Tuple Patterns

Tuple patterns allow you to match tuples. Compared to slice patterns they are
fairly simple. You must specify every field of the tuple, you can choose to bind
it, or ignore it with `_`. e.g.

```("I", "am", "a", "happy", "tuple", w, _, "patterns")```

### Struct Patterns

Struct patterns, like tuple patterns, are pretty simple.

- `{ x, y }` if you like the field names then there is no need to change them
- `{ x: x_coord, y: y_coord }` but if you need to use a different name you can
- `{ x, .. }` you don't have to write every field

Consider

```graphix
let a = {x: 54, y: 23};
a <- {x: 21, y: 88};
a <- {x: 5, y: 42};
a <- {x: 23, y: 32};
select a {
  {x, y: _} if (x < 10) || (x > 50) => `VWall,
  {y, x: _} if (y < 10) || (y > 40)  => `HWall,
  {x, y} => `Ok(x, y)
}
```

does some 2d bounds checking, and will output

```
$ graphix test.gx
`VWall
`HWall
`VWall
`Ok(23, 32)
```

You might be tempted to replace `y: _` with `..` as it would be shorter.
Unfortunately this will confuse the type checker, because the Graphix type system
is structural saying `{x, ..}` without any other information could match ANY
struct with a field called `x`. This is currently too much for the type checker
to handle,

```
$ graphix test.gx
Error: in file "test.gx"

Caused by:
    pattern {x: '_1040} will never match {x: i64, y: i64}, unused match cases
```

The error is slightly confusing at first, until you understand that since we
don't know the type of `{x, ..}` we don't think it will match the argument type,
and therefore the match case is unused. This actually saves us a lot of trouble
here, because the last match is exhaustive, if we didn't check for unused match
cases this program would compile, but it wouldn't work. You can easily fix this
by naming the type, and for larger structs it's often worth it if you only need
a few fields.

```graphix
type T = {x: i64, y: i64};
let a = {x: 54, y: 23};
a <- {x: 21, y: 88};
a <- {x: 5, y: 42};
a <- {x: 23, y: 32};
select a {
  T as {x, ..} if (x < 10) || (x > 50) => `VWall,
  T as {y, ..} if (y < 10) || (y > 40)  => `HWall,
  {x, y} => `Ok(x, y)
}
```

Here since we've included the type pattern `T` in our partial patterns the
program compiles and runs.

```
$ graphix test.gx
`VWall
`HWall
`VWall
`Ok(23, 32)
```

Note that we never told the compiler that `a` is of
type `T`. In fact `T` is just an alias for `{x: i64, y: i64}` which is the type
of `a`. We could in fact write our patterns without the alias,

```{x: i64, y: i64} as {x, ..} if (x < 10) || (x > 50) => `VWall```

The type alias just makes the code less verbose without changing the semantics.

### Variant Patterns

Variant patterns match variants. Consider,

```graphix
let v: [`Bare, `Arg(i64), `MoreArg(string, i64)] = `Bare;
v <- `Arg(42);
v <- `MoreArg("hello world", 42);
select v {
  `Bare => "it's bare, no argument",
  `Arg(i) => "it has an arg [i]",
  x@ `MoreArg(s, n) => "it's big [x] with args \"[s]\" and [n]"
}
```

produces

```
$ graphix test.gx
"it's bare, no argument"
"it has an arg 42"
"it's big `MoreArg(\"hello world\", 42) with args \"hello world\" and 42"
```

Variant patterns enforce the same kinds of match case checking as all the other pattern types

```graphix
let v: [`Bare, `Arg(i64), `MoreArg(string, i64)] = `Bare;
v <- `Arg(42);
v <- `MoreArg("hello world", 42);
select v {
  `Bare => "it's bare, no argument",
  `Arg(i) => "it has an arg [i]",
  x@ `MoreArg(s, n) => "it's big [x] with args \"[s]\" and [n]",
  `Wrong => "this won't compile"
}
```

yields

```
$ graphix test.gx
Error: in file "test.gx"

Caused by:
    pattern `Wrong will never match [`Arg(i64), `MoreArg(string, i64)], unused match cases
```

### Literals, Ignore

You can match literals as well as bind variables, as you may have noticed, and
the special pattern `_` means match anything and don't bind it to a variable.

### Missing Features

A significant missing feature from patterns vs other languages is support for
multiple alternative patterns in one arm. I plan to add this at some point.

## Select and Connect

Using select and connect together is one way to iterate in Graphix. Consider,

```graphix
let a = [1, 2, 3, 4, 5];
let len = 0;
select a {
  [x, tl..] => {
    len <- len + 1;
    a <- tl
  },
  _ => len
}
```

produces

```
$ graphix test.gx
5
```

This is not normally how we would get the length of an array in Graphix, or even
how we would do something with every element of an array (see `array::map` and
`array::fold`), however it illustrates the power of select and connect together.
