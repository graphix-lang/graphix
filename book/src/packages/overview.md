# Packages

Graphix has a package system that lets you extend the language with new
built-in functions written in Rust and new modules written in Graphix. Packages
are the primary mechanism for adding functionality beyond what the standard
library provides.

## How Packages Work

Each package is a Rust crate named `graphix-package-<name>`. A package can
contain:

- **Rust built-in functions** that are callable from Graphix
- **Graphix source modules** (`.gx` and `.gxi` files) that provide a Graphix-level API
- **Dependencies on other packages** via Cargo

When you install or remove a package, the `graphix` binary is rebuilt with the
new set of packages compiled in. This means packages run at native speed with no
FFI or serialization overhead.

Packages can depend on other packages through normal Cargo dependencies. For
example, the `net` package depends on `time`. If you remove `time` from your
installed packages but still have `net` installed, `time` remains available
because it's a transitive dependency. Cargo handles the dependency graph
automatically.

## The Standard Library as Packages

The standard library is itself implemented as a set of packages: `core`, `str`,
`array`, `map`, `time`, `net`, `re`, `rand`, `fs`, and `tui`. These ship with
the default `graphix` binary. The same `defpackage!` macro and `Package` trait
that power the standard library are available for third-party packages.

## Getting Started

- [Using Packages](./using.md) covers installing, removing, and managing packages
- [Creating Packages](./creating.md) covers building your own packages
