# Array

```graphix
/// filter returns a new array containing only elements where f returned true
val filter: fn(Array<'a>, fn('a) -> bool throws 'e) -> Array<'a> throws 'e;

/// filter_map returns a new array containing the outputs of f
/// that were not null
val filter_map: fn(Array<'a>, fn('a) -> Option<'b> throws 'e) -> Array<'b> throws 'e;

/// return a new array where each element is the output of f applied to the
/// corresponding element in a
val map: fn(Array<'a>, fn('a) -> 'b throws 'e) -> Array<'b> throws 'e;

/// return a new array where each element is the output of f applied to the
/// corresponding element in a, except that if f returns an array then it's
/// elements will be concatenated to the end of the output instead of nesting.
val flat_map: fn(Array<'a>, fn('a) -> ['b, Array<'b>] throws 'e) -> Array<'b> throws 'e;

/// return the result of f applied to the init and every element of a in
/// sequence. f(f(f(init, a[0]), a[1]), ...)
val fold: fn(Array<'a>, 'b, fn('b, 'a) -> 'b throws 'e) -> 'b throws 'e;

/// each time v updates group places the value of v in an internal buffer
/// and calls f with the length of the internal buffer and the value of v.
/// If f returns true then group returns the internal buffer as an array
/// otherwise group returns nothing.
val group: fn('a, fn(i64, 'a) -> bool throws 'e) -> Array<'a> throws 'e;

/// iter produces an update for every value in the array a. updates are produced
/// in the order they appear in a.
val iter: fn(Array<'a>) -> 'a;

/// iterq produces an update for each value in a, but only when clock updates. If
/// clock does not update but a does, then iterq will store each a in an internal
/// fifo queue. If clock updates but a does not, iterq will record the number of
/// times it was triggered, and will update immediately that many times when a
/// updates.
val iterq: fn(#clock:Any, Array<'a>) -> 'a;

/// returns the length of a
val len: fn(Array<'a>) -> i64;

/// returns the concatenation of two or more arrays. O(N) where
/// N is the size of the final array.
val concat: fn(Array<'a>, @args: Array<'a>) -> Array<'a>;

/// return an array with the args added to the end. O(N)
/// where N is the size of the final array
val push: fn(Array<'a>, @args: 'a) -> Array<'a>;

/// return an array with the args added to the front. O(N)
/// where N is the size of the final array
val push_front: fn(Array<'a>, @args: 'a) -> Array<'a>;

/// return an array no larger than #n with the args
/// added to the back. If pushing the args would cause the
/// array to become bigger than #n, remove values from the
/// front. O(N) where N is the window size.
val window: fn(#n:i64, Array<'a>, @args: 'a) -> Array<'a>;

/// flatten takes an array with two levels of nesting and produces a flat array
/// with all the nested elements concatenated together.
val flatten: fn(Array<Array<'a>>) -> Array<'a>;

/// applies f to every element in a and returns the first element for which f
/// returns true, or null if no element returns true
val find: fn(Array<'a>, fn('a) -> bool throws 'e) -> Option<'a> throws 'e;

/// applies f to every element in a and returns the first non null output of f
val find_map: fn(Array<'a>, fn('a) -> Option<'b> throws 'e) -> Option<'b> throws 'e;

type Direction = [
    `Ascending,
    `Descending
];

/// return a new copy of a sorted ascending (by default). If numeric is true then
/// values will be cast to numbers before comparison, resulting in a numeric sort
/// even if the values are strings.
val sort: fn(?#dir:Direction, ?#numeric:bool, Array<'a>) -> Array<'a>;

/// return an array of pairs where the first element is the index in
/// the array and the second element is the value.
val enumerate: fn(Array<'a>) -> Array<(i64, 'a)>;

/// given two arrays, return a single array of pairs where the first
/// element in the pair is from the first array and the second element in
/// the pair is from the second array. The final array's length will be the
/// minimum of the length of the input arrays
val zip: fn(Array<'a>, Array<'b>) -> Array<('a, 'b)>;

/// given an array of pairs, return two arrays with the first array
/// containing all the elements from the first pair element and second
/// array containing all the elements of the second pair element.
val unzip: fn(Array<('a, 'b)>) -> (Array<'a>, Array<'b>);
```
