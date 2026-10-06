# References

A reference value is not the thing itself, but a reference to it, just like a
pointer in C. This is kind of an odd thing to have in a very high level language
like Graphix, but there are good reasons for it. Before we get into those lets
see what one looks like.

```graphix
〉let v = &1
〉v
-: &i64
727
```

The `&` in front of the `1` creates a reference. You can create a reference to
any value. Note that the type isn't `i64` anymore but `&i64` indicating that `v`
is a reference to an `i64`. Just like a function when printed the reference id
is printed, not the value it refers to. We get the value that this reference 727
refers to with the deref operator *.

```graphix
〉*v
-: i64
1
```

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

```graphix
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

```graphix
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

```graphix
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
you hand `&title`.

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

References have no order, so `<` and the other orderings refuse them,
and so does anything that orders or hashes values: map keys, `min`,
`max`, the sorts and `array::dedup`. `uniq` compares them as `==` does.

## Referencing a Name vs Referencing an Expression

`&` does slightly different things depending on what follows it, and the
difference is observable in *when* a deref sees an update.

`&v`, where `v` is a binding, is a reference to that binding. Dereferencing it
reads the binding, so `*r` and `v` always agree — on the same cycle, in the same
update round.

```graphix
let v = 0;
let r = &v;
v <- 7;
(v, *r)     // (0, 0) then (7, 7)
```

`&(a + b)` has no binding to name, so it makes one. The expression's value is
connected into a fresh cell, which is exactly

```graphix
let tmp = a + b;
tmp <- a + b;
&tmp
```

and it therefore paces like any other `<-`: the initial value is there
immediately, and every later update arrives on the *next* cycle.

```graphix
let a = 2;
let b = 3;
let r = &(a + b);
a <- 10;
(a + b, *r)   // (5, 5) then (13, 5) then (13, 13)
```

So a reference to a name is a reference to a place, and a reference to an
expression is a reference to a value that is being kept up to date for you.
That the two pace differently is an accepted wart — the alternative is either
making `&(expr)` illegal, which costs a lot of concision for a construct people
reach for constantly in UI code, or updating the cell mid-cycle, which would
make the value a deref sees depend on where it sits in the program rather than
on what it depends on.
