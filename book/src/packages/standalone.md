# Standalone Binaries

If you want to build a self-contained graphix binary with your package
and all it's dependencies baked in (for deployment, distribution, or
use as a custom application), you can create a `main.gx` program in
your packages' src/graphix directory, which will enable the package to
be built as a standalone binary.

## Adding a Main Program

Create `src/graphix/main.gx` in your package with the program to run at startup:

```graphix
"Hello from my standalone app!"
```

During normal library use (e.g. when loaded via `graphix package
add`), `main.gx` is excluded from the virtual filesystem and has no
effect. It only activates the package is building as standalone
binary. This is very similar to Python's

```Python
if __name__ == '__main__':
```

mechanism, except it is ... somewhat less ugly to look at.

## Building

From your package directory:

```
cd graphix-package-mylib
graphix package build-standalone
```

This builds a release-optimized `graphix` binary in the current
directory that includes your local package and all it's
dependencies. The build enables the `standalone` cargo feature on your
package, which causes the contents of `main.gx` to be appended to the
root module as the entry point program.

Scaffolded packages already include `[features] standalone = []` in their
`Cargo.toml`. If you created your package before this feature existed, add it
manually:

```toml
[features]
standalone = []
```

The resulting binary is fully standalone — it doesn't require `graphix package
add` or any runtime .gx files in order to run.
