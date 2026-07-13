> **STATUS (2026-07-13):** The iterator/object exploration is retained, but
> its `sync`/`for` examples are obsolete. The sync subset was removed and
> built-in collection HOFs are compiler-owned Nodes; see
> `design/collection_intrinsics.md`.

We need some way to handle generic programming in graphix. For example instead
of hard coding the for loop to work with arrays lists, maps, etc, it would be
nice if there was a generic interface that the user could implement that allowed
for to work. Then user's own data structures could work with it. We already
kind of have a prototype for this behavior in the sys::io module. The concept is
that an interface is a concrete thing that you create from a specific type. So
how can this work in a user accessible way for an interface like Iter. Below is
a potential shape,


```
------------- iter.gxi ---------------
type Iter<'a>;

/// create a new iterator given a next and optional len
val new: fn(
	?len: fn(t: Any) -> [i64, null],
	#next: fn(t: Any) -> ['a, null]
) -> Iter<'a>;

/// get the next element, or null if the iterator is exhausted
val next: fn(Iter<'a>) -> ['a, null];

/// get the remaining length if the iterator knows it's length
val len: fn(Iter<'a>) -> [i64, null];

// other derived functions on iterators

------------- iter.gx ---------------
type Iter<'a> = {
	next: fn(t: Any) -> ['a, null],
	len: fn(t: Any) -> [i64, null]
};

let new = |
	#len: fn(t: Any) -> [i64, null] = |t| null,
	#next: fn(t: Any) -> ['a, null]
| -> Iter<'a> { next, len };
let next = |i: Iter<'a>| -> ['a, null] (i.next)(i);
let len = |i: Iter<'a>| -> [i64, null] (i.len)(i);

------------ array.gx --------------
let as_iter = |a: Array<'a>| -> Iter<'a> sync {
	let mut i = 0;
	let len = |t| sync<t> { array::len(a) - i };
	let next = |t| sync<t> {
		select i < array::len(a) {
			false => null,
			true => {
				let v = a[i]$;
				i = i + 1;
				v
			}
		}
	};
	iter::new(#len, #next)
}
```

## Worm Cans Opened

- nested closure definition in sync blocks
- shared mutable state captured by sync closures
- trigger arguments to sync, sync<t> is evaluated only then t updates
- figuring out how to statically call e.g. (i.next) to avoid murdering performance
- the exact semantics of code within sync blocks, I've written it as if it's "normal" imperative code

## The Payoff

- shared implementations of map, fold, filter, etc on Iter<'a>
- pluggable for loops like a "real" language
- a coherent story for generic coding in Graphix that is pretty powerful, and very simple (at least to the user)
- a chance to clarify our sync block semantics, and finally solve triggering for sequential code

## Triggering and async marking

The trigger-expression / `force` / `await` design that used to live
here grew into its own document: see `design/sync_control.md`. The
short version as it applies to Iter: `await` marks async reads inside
sync blocks (mandatory, checked both directions); `t ~! e` (the force
operator) gates a dormant subtree on a trigger with cached-pull
delivery, which gives zero-arg calls a firing schedule — so Iter's
natural shape works:

```
type Iter<'a> = {
	next: fn() -> ['a, null],
	len: fn() -> [i64, null]
}

let next = |i: Iter<'a>| -> ['a, null] i ~! (i.next)()
```

Inside a sync block (the common consumer — a `for` desugar), `next()`
is a plain sequential call and needs no trigger at all; `~!` matters
at the reactive boundary.

## Objects

This proposal is also the aspiration for an OBJECT story:
record-of-closures + an abstract type is objects done structurally —
the interface is the record type, the abstract type hides the
representation, and the shared captured muts (`i` in `as_iter`) are
the instance's hidden state. The load-bearing language change is the
escape rule: "freeze = escape" needs a carve-out where a closure's
captured mut places may escape ONLY inside a value whose type is
abstract, making the mutability provably unobservable from outside.
The static-dispatch problem for `(i.next)` is the same problem as
instance-body inlining (P4 perf item 3) — one solution serves both.

For now iteration over the built-in collections stays special-cased
(for-over-Map/List lands on the special-cased path) and migrates to
Iter when the object story is real.
