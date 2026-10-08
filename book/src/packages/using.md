# Using Packages

The `graphix package` command manages your installed packages. All package
operations rebuild the `graphix` binary to include the new set of packages.

## Using an Installed Package

Once installed, a package is available as a Graphix module with the same name
as the package. For example, the `sys` package provides netidx networking
functions under `sys::net`:

```graphix
use sys::net;
subscribe("/some/netidx/path")
```

Or access it without `use`:

```graphix
sys::net::subscribe("/some/netidx/path")
```

The standard library packages (`core`, `str`, `array`, `map`, `re`, `rand`,
`sys`, `http`, `json`, `toml`, `pack`, `xls`, `sqlite`, `db`, `list`, `args`,
`hbs`, `tui`, `gui`) are pre-installed and available by default.

## Searching for Packages

Search crates.io for packages matching a query:

```
graphix package search http
```

This searches for crates matching `graphix-package-*http*`. Results show the
package name, version, and description.

## Installing Packages

Install a package from crates.io:

```
graphix package add mypackage
```

Install a specific version:

```
graphix package add mypackage@1.2.0
```

Install from a local path (useful during development):

```
graphix package add mypackage --path /home/user/mypackage
```

### Alternative Registries

If your organization uses a private Cargo registry instead of crates.io, use
the `--skip-crates-io-check` flag to bypass the crates.io validation:

```
graphix package add mypackage@1.0.0 --skip-crates-io-check
```

Configure your alternative registry in `~/.cargo/config.toml` using Cargo's
standard [source replacement](https://doc.rust-lang.org/cargo/reference/source-replacement.html)
mechanism.

## Removing Packages

```
graphix package remove mypackage
```

Standard library packages can be removed too (all but `core`), and added
back with `graphix package add`. Removing one also removes the installed
standard library packages that depend on it: the command lists them and
asks before going on. With `--yes` (or `-y`) it removes them without
asking; without `--yes` and with stdin not a terminal, it removes
nothing.

If other installed packages depend on a removed third-party package via
Cargo, its modules will still be available (since it remains a transitive
dependency). This is by design -- Cargo manages the dependency graph.

## Listing Installed Packages

```
graphix package list
```

Shows the installed standard library packages, the removed ones, and the
third-party packages with their versions or paths.

## Rebuilding

If you manually edit the packages file, you can trigger a rebuild:

```
graphix package rebuild
```

A rebuild also picks up minor and patch version updates of third-party
packages automatically, since no `Cargo.lock` is generated -- Cargo resolves
the latest compatible version within each package's semver range.

## Updating

To update graphix and your packages to their latest versions:

```
graphix package update
```

This queries crates.io for the latest `graphix-shell` and the latest version
of each third-party package installed from crates.io, and lists the changes
it found: a newer shell (the standard library comes with it), the standard
library packages the newer shell ships that you have not installed or
removed, and the third-party updates. It then asks `Apply all changes?
[Y/e/n]`: `Y` applies them all, `e` lets you toggle items one by one, `n`
cancels. Declining the shell update also declines the new standard library
packages, which only build against it. With `--yes` (or `-y`) every change
is applied without asking; without `--yes` the command refuses to run when
stdin is not a terminal. `packages.toml` is written only after the rebuild
succeeds.

If nothing is newer, the command prints a message and exits without
rebuilding.

To update a third-party package to a new **major** version, edit the version
in `packages.toml` directly and run `graphix package rebuild`.

## Package Storage

The package list is stored in `packages.toml` in your platform's data
directory:

| Platform | Location |
|----------|----------|
| Linux | `~/.local/share/graphix/packages.toml` |
| macOS | `~/Library/Application Support/graphix/packages.toml` |
| Windows | `%APPDATA%\graphix\packages.toml` |

The file has two tables. `[stdlib]` lists the standard library packages by
name, the installed ones and the ones you removed (they track the shell's
version, so they carry none). `[packages]` maps third-party package names to
versions, or to a path with an inline table:

```toml
[stdlib]
installed = ["array", "core", "map", "str", "sys"]
removed = ["gui", "tui"]

[packages]
mypackage = "1.2.0"
another = { path = "/home/user/another" }
```

A file without a `[stdlib]` table is read in the older format, a single
`[packages]` table naming every package, standard library ones included:
any standard library package it does not name counts as removed. The
package commands write the file back in the current format.

## How the Rebuild Works

When you add or remove a package, the package manager:

1. Unpacks the `graphix-shell` source of the installed `graphix`'s version
   from the Cargo cache, or downloads it from crates.io
2. Adds your third-party packages to its `Cargo.toml` as dependencies (the
   shell registers every package its manifest names)
3. Backs up the previous binary with a timestamp
4. Runs `cargo install --force`, the installed standard library packages
   selected as Cargo features, to build and install the new binary

This means you need a working Rust toolchain installed. The rebuild takes
roughly the same time as compiling any Rust project of similar size.
