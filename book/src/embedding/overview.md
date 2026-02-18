# Embedding and Extending Graphix

There are multiple ways you can embed Graphix in your application and
extend it with Rust code.

## Packages

The recommended way to extend Graphix is by creating a
[package](../packages/overview.md). Packages let you bundle Rust built-in
functions and Graphix modules into a crate that can be installed with `graphix
package add`. The standard library itself is built as a set of packages using the
same tools available to third-party developers.

See [Packages](../packages/overview.md) for details.

## Writing Built-in Functions in Rust

For a simple pure function you can use the `CachedArgs` interface which takes
care of most of the details for you. You only need to implement one method to
evaluate changes to your arguments. For example, a function that finds the
minimum value of all its arguments:

```rust
use graphix_package_core::{deftype, CachedArgs, CachedVals, EvalCached};
use netidx_value::Value;

#[derive(Debug, Default)]
struct MinEv;

impl EvalCached for MinEv {
    const NAME: &str = "core_min";
    deftype!("fn('a, @args: 'a) -> 'a");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let mut res = None;
        for v in from.flat_iter() {
            match (res, v) {
                (None, None) | (Some(_), None) => return None,
                (None, Some(v)) => res = Some(v),
                (Some(v0), Some(v)) => {
                    res = if v < v0 { Some(v) } else { Some(v0) };
                }
            }
        }
        res
    }
}

type Min = CachedArgs<MinEv>;
```

Then register this built-in by listing it in your package's `defpackage!` macro,
and bind it in your Graphix module:

```graphix
let min = |@args| 'core_min
```

The special form function body `'core_min` references a built-in Rust function.

See [Writing Built in Functions](./builtins.md) for the full API details.

## Custom Embedded Applications

For most standalone binaries, the simplest approach is `graphix package
build-standalone` — see [Standalone Binaries](../packages/standalone.md).

If you need more control (custom module resolvers, embedded REPLs, compiler
flags, or integration with your own Rust application), you can use the
`graphix-shell` crate directly to build a custom application. See [Custom
Embedded Applications](./shell.md) for details.

## Embedding Graphix in Your Application

Using the `graphix-rt` crate you can embed the Graphix compiler and runtime in
your application. Then you can:

- compile and run Graphix code
- receive events from Graphix expressions
- inject events into Graphix pipelines
- call Graphix functions

The runtime uses tokio and runs in a background task so it integrates well into
a normal async workflow.

See [Using Graphix as Embedded Scripting](./rt.md) for details.
