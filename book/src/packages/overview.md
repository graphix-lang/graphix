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

<!-- CR claude for claude: [doc-drift/low] This example is stale: removing `sys` while
`tui` is installed does not keep `sys` as a transitive dependency. graphix-package's
removal_cascade removes every installed stdlib package that depends on the one removed
(tui with sys, after asking, as using.md says). It refuses the removal outright when an
installed third-party package depends on it ("nothing removed: an installed external
package depends on ..."). Replace the example with the cascade, or keep the
transitive-dependency point for third-party packages only. Found by reading; not run.
(book-modules-udt-10) -->
Packages can depend on other packages through normal Cargo dependencies. For
example, the `tui` package depends on `sys`. If you remove `sys` from your
installed packages but still have `tui` installed, `sys` remains available
because it's a transitive dependency. Cargo handles the dependency graph
automatically.

## The Standard Library as Packages

The standard library is itself implemented as a set of packages, listed in
[Using Packages](using.md), which ship with the default `graphix` binary. The same `defpackage!` macro and `Package` trait
that power the standard library are available for third-party packages.

## Getting Started

- [Using Packages](./using.md) covers installing, removing, and managing packages
- [Creating Packages](./creating.md) covers building your own packages
- [Standalone Binaries](./standalone.md) covers building self-contained binaries from packages
