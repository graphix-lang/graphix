# Core Language

This chapter introduces the core constructs that make Graphix work. If you're coming from imperative languages like Python, Java, or C, some of these concepts will feel familiar—but the reactive twist makes them work differently than you might expect.

## Types: Strong but Flexible

Graphix has a powerful static type system, but you'll rarely write type annotations. The compiler infers types for you using structural typing—types are compared by their shape, not their name. A struct `{x: i64, y: i64}` is the same type whether you call it `Point` or `Vector` or don't name it at all.

The **[Fundamental Types](./fundamental_types.md)** section covers the built-in numeric types (`i64`, `f64`, `u32`, etc.), booleans, strings, durations, and more. You'll learn how arithmetic works across different numeric types, how string interpolation works with `[...]` brackets, and why division by zero doesn't crash your program.

**[Reading Type Signatures](./reading_types.md)** teaches you how to read the type annotations you'll see throughout the documentation. Function types like `fn(Array<'a>, fn('a) -> 'b) -> Array<'b>` tell you exactly what a function expects and returns, including what errors it might throw.

## Binding Values and Building Blocks

In Graphix, you create bindings with `let`. Unlike variables in other languages, these bindings can update over time—they're more like pipes that different values flow through.

**[Let Binds](./let_binds.md)** explains how to create bindings, how shadowing works, and why every binding in Graphix is potentially reactive. When you write `let x = 42`, you're not just storing a value—you're creating a node in the dataflow graph.

**[Blocks](./block.md)** shows how to group expressions with `{...}` to create scopes, hide intermediate bindings, and build up complex expressions. Blocks are expressions too—they evaluate to their last value.

**[Use](./use.md)** lets you bring module names into scope so you can write `map(arr, f)` instead of `array::map(arr, f)`. Simple, but essential as your programs grow.

## Connect: The Heart of Reactivity

This is where Graphix becomes special. The `<-` operator (connect) is the only way to create cycles in your dataflow graph, and it's the key to writing reactive programs and loops.

**[Connect](./connect.md)** is the most important section in this chapter. When you write:

```graphix
let count = 0;
count <- timer ~ (count + 1)
```

You're telling Graphix: "When `timer` updates, increment `count` for the next cycle." Connect schedules updates for the future, which is how you build everything from simple counters to complex state machines. It's also the only looping construct in Graphix—there's no `for` or `while`, just connect and select working together.

## Select: Powerful Pattern Matching

The `select` expression is Graphix's answer to `switch`, `match`, and `if/else`. It lets you match on types, destructure complex data, and ensure at compile time that you've handled every case.

**[Select](./select.md)** shows you how to:
- Match on union types and ensure you handle all variants
- Destructure arrays with slice patterns like `[head, tail..]`
- Match structs with patterns like `{x, y}`
- Guard patterns with conditions like `n if n > 10`
- Build loops by combining select with connect

The compiler checks your select expressions exhaustively—if you forget a case, it won't compile. If you match a case that can never happen, it won't compile. This eliminates entire classes of bugs.

## Error Handling: Exceptions, Done Right

Graphix has first-class error handling with try/catch and the `?` operator. Errors are just values with the special `Error<'a>` type, and they're tracked through the type system.

**[Error Handling](./error.md)** explains:
- How `?` throws errors to the nearest try/catch in dynamic scope
- How error types are checked at compile time—you can't forget to handle an error type
- How the `$` operator silently swallows errors (use with caution!)
- How error chains track the full context of where errors originated

Every error that gets raised with `?` is wrapped in an `ErrChain` that captures the file, line, column, and full stack of previous errors. No more mystery exceptions.

## How It All Fits Together

These constructs combine to create the Graphix programming model:

1. You create **bindings** that hold values
2. You build **expressions** that compute new values from old ones
3. You use **select** to handle different cases and make decisions
4. You use **connect** to update bindings when events occur
5. The **type system** ensures everything is safe and correct
6. **Errors** propagate cleanly through try/catch

The result is a language where you describe relationships between values, and the runtime automatically maintains those relationships as things change. A temperature value updates, and the Fahrenheit conversion updates automatically. A timer fires, and your counter increments. A network subscription delivers new data, and your UI reflects it instantly.
