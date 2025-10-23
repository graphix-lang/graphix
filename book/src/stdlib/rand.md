# Rand

```graphix
mod rand: sig {
    /// generate a random number between #start and #end (exclusive)
    /// every time #clock updates. If start and end are not specified,
    /// they default to 0.0 and 1.0
    val rand: fn<'a: [Int, Float]>(?#start:'a, ?#end:'a, #clock:Any) -> 'a;

    /// pick a random element from the array and return it. Update
    /// each time the array updates. If the array is empty return
    /// nothing.
    val pick: fn(Array<'a>) -> 'a;

    /// return a shuffled copy of a
    val shuffle: fn(Array<'a>) -> Array<'a>;
}
```
