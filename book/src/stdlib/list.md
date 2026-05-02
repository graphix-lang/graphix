# list

The `list` module provides immutable singly-linked lists with structural
sharing. Two lists with a common tail share memory. Cons (prepend) is
O(1); indexed access is O(n).

```graphix
/// The singly linked list type.
type List<'a>;

/// Return an empty list.
val nil: fn(v: Any) -> List<'a>;

/// Prepend an element to the front of a list. O(1).
val cons: fn(x: 'a, a: List<'a>) -> List<'a>;

/// Return a list containing a single element.
val singleton: fn(x: 'a) -> List<'a>;

/// Return the first element, or null if empty.
val head: fn(a: List<'a>) -> Option<'a>;

/// Return the list without its first element, or null if empty.
val tail: fn(a: List<'a>) -> Option<List<'a>>;

/// Return both the head and tail as a pair, or null if empty.
val uncons: fn(a: List<'a>) -> Option<('a, List<'a>)>;

/// Return true if the list is empty.
val is_empty: fn(a: List<'a>) -> bool;

/// Return the element at position n (0-indexed), or null. O(n).
val nth: fn(a: List<'a>, n: i64) -> Option<'a>;

/// Return the number of elements. O(n).
val len: fn(a: List<'a>) -> i64;

/// Return the list in reverse order. O(n).
val reverse: fn(a: List<'a>) -> List<'a>;

/// Return the first n elements.
val take: fn(n: i64, a: List<'a>) -> List<'a>;

/// Return the list without its first n elements.
val drop: fn(n: i64, a: List<'a>) -> List<'a>;

/// Convert a list to an array.
val to_array: fn(a: List<'a>) -> Array<'a>;

/// Convert an array to a list.
val from_array: fn(a: Array<'a>) -> List<'a>;

/// Concatenate two or more lists.
val concat: fn(a: List<'a>, @args: List<'a>) -> List<'a>;

/// Flatten a list of lists into a single list.
val flatten: fn(a: List<List<'a>>) -> List<'a>;

/// Apply f to each element.
val map: fn(a: List<'a>, f: fn(x: 'a) -> 'b throws 'e) -> List<'b> throws 'e;

/// Keep elements where f returns true.
val filter: fn(a: List<'a>, f: fn(x: 'a) -> bool throws 'e) -> List<'a> throws 'e;

/// Keep non-null outputs of f.
val filter_map: fn(a: List<'a>, f: fn(x: 'a) -> Option<'b> throws 'e) -> List<'b> throws 'e;

/// Map and flatten: if f returns a list, inline its elements.
val flat_map: fn(a: List<'a>, f: fn(x: 'a) -> ['b, List<'b>] throws 'e) -> List<'b> throws 'e;

/// Left fold: f(f(f(init, a0), a1), ...).
val fold: fn(a: List<'a>, y: 'b, f: fn(x: 'b, y: 'a) -> 'b throws 'e) -> 'b throws 'e;

/// Return the first element where f returns true, or null.
val find: fn(a: List<'a>, f: fn(x: 'a) -> bool throws 'e) -> Option<'a> throws 'e;

/// Return the first non-null output of f.
val find_map: fn(a: List<'a>, f: fn(x: 'a) -> Option<'b> throws 'e) -> Option<'b> throws 'e;

type Direction = [`Ascending, `Descending];

/// Return a sorted copy of the list.
val sort: fn(?#dir: Direction, ?#numeric: bool, a: List<'a>) -> List<'a>;

/// Return a list of (index, element) pairs.
val enumerate: fn(a: List<'a>) -> List<(i64, 'a)>;

/// Zip two lists into a list of pairs.
val zip: fn(a: List<'a>, a2: List<'b>) -> List<('a, 'b)>;

/// Unzip a list of pairs into a pair of lists.
val unzip: fn(a: List<('a, 'b)>) -> (List<'a>, List<'b>);

/// Create a list of n elements where element i is f(i).
val init: fn(n: i64, f: fn(n: i64) -> 'a throws 'e) -> List<'a> throws 'e;

/// Produce an update for every element in the list.
val iter: fn(a: List<'a>) -> 'a;

/// Produce an update for each element, gated by clock updates.
val iterq: fn(#clock: Any, a: List<'a>) -> 'a;
```
