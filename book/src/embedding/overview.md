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

A pure function of its arguments is a plain Rust function over their values,
which `graphix_package_core::fast_builtin!` turns into a builtin that runs in
the interpreter and in native code alike; see [The Simple
Path](../packages/creating.md#the-simple-path-fast_builtin) for an example
(`mylib_min`) and how to register and bind it.

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
