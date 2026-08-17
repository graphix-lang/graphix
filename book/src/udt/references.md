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

Consider,

```graphix
let f = |x: &i64| *x <- once(*x) + 1;
let v = 0;
f(&v);
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
