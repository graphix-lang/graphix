# Using Packages

The `graphix package` command manages your installed packages. All package
operations rebuild the `graphix` binary to include the new set of packages.

## Using an Installed Package

Once installed, a package is available as a Graphix module with the same name
as the package. For example, the `net` package provides netidx networking
functions:

```graphix
use net;
net::subscribe("/some/netidx/path")
```

Or access it without `use`:

```graphix
net::subscribe("/some/netidx/path")
```

The standard library packages (`core`, `str`, `array`, `map`, `time`, `net`,
`re`, `rand`, `fs`, `tui`) are pre-installed and available by default.

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

If other installed packages depend on the removed package via Cargo, the
removed package's modules will still be available (since it remains a transitive
dependency). This is by design -- Cargo manages the dependency graph.

## Listing Installed Packages

```
graphix package list
```

Shows all explicitly installed packages with their versions.

## Rebuilding

If you manually edit the packages file, you can trigger a rebuild:

```
graphix package rebuild
```

A rebuild also picks up minor and patch version updates of third-party
packages automatically, since no `Cargo.lock` is generated -- Cargo resolves
the latest compatible version within each package's semver range.

## Updating

To update graphix itself (and the standard library) to the latest version:

```
graphix package update
```

This queries crates.io for the latest `graphix-shell` version and the latest
version of each standard library package, updates `packages.toml` accordingly,
and triggers a full rebuild. Your third-party packages are left untouched.

If you're already on the latest version, the command prints a message and
exits without rebuilding.

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

The file is a simple TOML map of package names to versions:

```toml
[packages]
mypackage = "1.2.0"
another = "0.5.0"
```

Path dependencies use an inline table:

```toml
[packages]
mypackage = { path = "/home/user/mypackage" }
```

## How the Rebuild Works

When you add or remove a package, the package manager:

1. Unpacks the `graphix-shell` source from the Cargo cache
2. Updates its `Cargo.toml` to include your packages as dependencies
3. Generates a `deps.rs` that registers all packages
4. Runs `cargo install --force` to build and install the new binary
5. Backs up the previous binary with a timestamp

This means you need a working Rust toolchain installed. The rebuild takes
roughly the same time as compiling any Rust project of similar size.
