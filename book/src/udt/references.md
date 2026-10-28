# References

A reference value is not the thing itself, but a reference to it, just like a
pointer in C. This is kind of an odd thing to have in a very high level language
like Graphix, but there are good reasons for it. Before we get into those lets
see what one looks like.

```graphix
〉let v = &1
〉v
-: &i64
&ref
```

The `&` in front of the `1` creates a reference. You can create a reference to
any value. Note that the type isn't `i64` anymore but `&i64` indicating that `v`
is a reference to an `i64`. A reference prints as `&ref`, not the value it
refers to; its id belongs to the session (a cached program image relocates it),
so it is not shown. We get the value that the reference refers to with the
deref operator *.

```graphix
〉*v
-: i64
1
```

A union of reference types can be read: `*r` for `r: [&mut i64, &mut f64]`
has type `[i64, f64]`. A write through a union requires every member to be
writable and the new value to fit every referent type. A union containing a
non-reference cannot be dereferenced.

## But Why

Now that we've got the basic semantics out of the way, what is this good for?
Suppose we have a large struct, with many fields, or even a struct of structs of
structs with a lot of data. And suppose every time that struct updates we do a
bunch of work. This is exactly how UIs are built by the way, they are deeply
nested tree of structs. Under the normal semantics of Graphix, if any field
anywhere in our large tree of structs were to update, then we'd rebuild the
entire object (or at least a substantial part of it), and any function that
depended on it would have no way of knowing what changed, and thus would have to
do whatever huge amount of work it is supposed to do all over again. Consider a
constrained GUI type with just labels and boxes,

```graphix,fragment
type Gui = [
  `Label(string),
  `Box(Array<Gui>)
]
```

So we can build labels in boxes, and we can nest the boxes, laying out the
labels however we like (use your imagination). We have the same problem as the
more abstract example above, if we were mapping this onto a stateful GUI library
then every time a label text changed anywhere we'd have to destroy all the
widgets we had created and rebuild the entire UI from scratch. We'd like to be
able to just update the label text that changed, and we can, with a small change
to the type.

```graphix,fragment
type Gui = [
  `Label(&string),
  `Box(Array<Gui>)
]
```

Now, the string inside the label is a reference instead of the actual string.
Since references are assigned an id at compile time, they never change, and so
the layout of our GUI can never change just because a label text was updated.
Whatever is actually building the GUI will only see an update to the root when
the actual layout changes. To handle the labels it can just deref the string
reference in each label, and when that updates it can update the text of the
label, exactly what we wanted.

## Connect Deref

Suppose we want to write a function that can update the value a passed in
reference refers to, instead of the reference itself (which we can also do). We
can do that with,

```graphix,fragment
*r <- "new value"
```

A reference made with `&` is read-only; writing through one needs a writable
reference, made with `&mut` and typed `&mut T`. Consider,

```graphix
let f = |x: &mut i64| *x <- once(*x) + 1;
let v = 0;
f(&mut v);
println("[v]")
```

Running this program will output,

```
$ graphix test.gx
0
1
```

We were able to pass `v` into `f` by reference and it was able to update it,
even though the original bind of `v` isn't even in a scope that `f` can see.

## Read-Only and Writable References

`&T` can only be read, so it may refer to anything that is a `T`: `&[i64,
null]` accepts `&x` where `x: i64`, and a `&mut T` can be passed where a `&T` is
expected. This is what lets a widget take an optional `&[string, null]` that
you hand `&title`. A function parameter `&'a` can therefore infer the widest
referent type across its arguments, in either argument order. A parameter
`&mut 'a` requires all referents to agree exactly.

`&mut T` can also be written, so it must refer to exactly a `T`. If `&mut
[i64, null]` accepted `&mut x` with `x: i64`, writing `null` through it would
put a `null` into `x`, so the checker refuses it. When a function wants a value
it may or may not write, the parameter is an optional reference instead,
`[&mut i64, null]`, as `queuefn`'s `#count` is.

The grant is visible where the reference is made: a caller that writes `&x`
knows the callee cannot change `x`. The same rule holds for a union of
references: two read-only references may be one `&[i64, string]`, but a write
must fit every reference the value may hold.

## Comparing References

`==` and `!=` ask whether two references point to the same thing: the
same binding, or the same element or field of one. Two references made
separately are equal when they name the same place,

```graphix
let x = 1;
let a = [1, 2];
let r = &x;
[&x == r, &a[0] == &a[0], &a[0] == &a[1]]
```

is `[true, true, false]`. A reference made from a value rather than a
binding, `&mut 0`, is its own place, equal only to itself.

Equality still follows the target when a reference is held under `Any`,
including references inside collections and abstract values without an
`Eq` implementation. An abstract value's own `Eq` controls its equality.

References have no order, so `<` and the other orderings refuse them,
and so does anything that orders or hashes values: map keys, `min`,
`max`, the sorts and `array::dedup`. This also applies to references hidden
under `Any`: the operation logs a failure and produces no value, even for
a singleton collection. A cast that would create a map with such a key
returns an error. Other values under `Any` remain orderable, and an
abstract value's own `Ord` controls its ordering.
`uniq` compares references as `==` does.

## Referencing a Name, a Place or an Expression

`&` does different things depending on what follows it.

`&v`, where `v` is a binding, is a reference to that binding. Dereferencing it
reads the binding, so `*r` and `v` always agree — on the same cycle, in the same
update round.

```graphix
let v = 0;
let r = &v;
v <- 7;
// (0, 0) then (7, 7)
(v, *r)
```

### Places

An accessor chain over a binding, `&a[i]`, `&s.f`, `&t.0`, `&m{k}` or
any nesting of them, is a reference to a *place*: the root binding plus
a path into it. It is typed as a reference to the element (`&a[1]` is
`&i64` for `a: Array<i64>`). Reading it reads the root along the path,
and a write through a `&mut` place patches the root: the root is
rebuilt along the path and the new value delivered to the binding.

```graphix
let a = [1, 2, 3];
let s = {x: 1, y: 2};
let r = &mut a[1];
let q = &mut s.y;
*r <- 20;
*q <- 5;
// ([1, 2, 3], {x: 1, y: 2}) then ([1, 20, 3], {x: 1, y: 5})
(a, s)
```

A key that changes makes a moving reference: it points at whatever the
key names now, reads and writes there, and fires its readers when it
moves.

```graphix
let a = [10, 20, 30];
let i = 0;
let r = &a[i];
i <- 2;
// 10 then 30
*r
```

A read of a place that does not exist (an index past the end) is
bottom. A write to a key a map lacks inserts it, as `map::insert` would;
any other write into a place that does not exist is dropped and logged,
the root untouched. A write while the reference's address is bottom (a
moving reference whose key is bottom) does not happen, and when the key
returns nothing written meanwhile lands there. `&mut *r` is a place with
an empty path: a write through it lands where `r` points. Arrays are
immutable values, so a write through a place copies along the path:
fine for a form's state, O(n) per write for an array.

### Expressions

`&(a + b)` has no binding to name, so it makes a fresh cell that holds the
expression's value. The cell is updated in the same cycle as the expression, as
a `let` would be, so a deref sees each new value right away:

```graphix
let a = 2;
let b = 3;
let r = &(a + b);
a <- 10;
// (5, 5) then (13, 13)
(a + b, *r)
```

`&mut` of an expression that is not a place makes the same kind of cell,
writable: a write through it lands in that cell and reaches no binding.

So a reference to a name or an accessor chain is a reference to a place, and a
reference to any other expression is a reference to a value that is being kept
up to date for you.
