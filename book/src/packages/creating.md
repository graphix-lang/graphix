# Creating Packages

## Scaffolding a New Package

Create a new package with:

```
graphix package create mylib
```

Or specify a directory:

```
graphix package create mylib --dir ~/projects
```

This creates a `graphix-package-mylib` directory with the following structure:

```
graphix-package-mylib/
  Cargo.toml
  README.md
  src/
    lib.rs
    graphix/
      mod.gx
      mod.gxi
```

## Package Structure

### `src/lib.rs` -- The Rust Entry Point

The heart of a package is `src/lib.rs`, which uses the `defpackage!` macro to
declare the package:

```rust
use graphix_derive::defpackage;
use graphix_package_core::{CachedArgs, CachedVals, EvalCached};

// ... builtin implementations ...

defpackage! {
    builtins => [
        MyBuiltin,
        MyCachedBuiltin,
    ]
}
```

The `defpackage!` macro generates:

- A `pub struct P` that implements the `Package` trait
- Registration code for all listed builtins
- Automatic inclusion of all `.gx` and `.gxi` files from `src/graphix/`
- Test infrastructure (`TEST_REGISTER`) for the test harness

### `src/graphix/` -- Graphix Source Modules

Graphix source files in `src/graphix/` are automatically included in the
package. These files provide the Graphix-level API for your package. The
directory structure maps to the module hierarchy: `src/graphix/foo.gx` becomes
the module `mylib::foo`.

The top-level module file is `src/graphix/mod.gx`. This is where you typically
bind your builtins to Graphix names and re-export them:

```graphix
let my_builtin = |arg| 'mylib_my_builtin;
let my_cached = |@args| 'mylib_my_cached;
```

### `src/graphix/mod.gxi` -- Interface File

The interface file declares the public API of your package:

```graphix
/// Check if a value is an error
val my_builtin: fn(Any) -> bool;

/// Logical OR of all arguments
val my_cached: fn(@args: bool) -> bool;
```

See [Interface Files](../modules/interfaces.md) for the full interface syntax.

## Writing Built-in Functions

There are two ways to write builtins: the simplified `CachedArgs` interface for
pure functions, and the full `BuiltIn` + `Apply` traits for functions that need
fine-grained control over the update cycle.

### Naming Convention

All builtin names **must** start with your package name. For a package named
`mylib`, builtins must be named `mylib_something`. The `defpackage!` macro
enforces this at compile time.

### The Simple Path: `EvalCached` / `CachedArgs`

For pure functions that just compute a result from their arguments, use
`EvalCached`:

```rust
use graphix_package_core::{deftype, CachedArgs, CachedVals, EvalCached};
use netidx_value::Value;

#[derive(Debug, Default)]
struct MyMinEv;

impl EvalCached for MyMinEv {
    const NAME: &str = "mylib_min";
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

type MyMin = CachedArgs<MyMinEv>;
```

Then list `MyMin` in your `defpackage!` builtins. `CachedArgs` handles all the
details of caching argument values, calling `eval` when arguments change, and
implementing the `Apply` trait.

### The Full-Control Path: `BuiltIn` + `Apply`

For builtins that need to interact with the execution context, manage internal
state across cycles, or work with higher-order functions, implement the
`BuiltIn` and `Apply` traits directly. See
[Writing Built in Functions](../embedding/builtins.md) for a deep dive.

Here is a minimal example -- `once` passes through exactly one update:

```rust
use anyhow::Result;
use graphix_compiler::{
    expr::ExprId, typ::FnType, Apply, BuiltIn, Event, ExecCtx, Node, Rt, Scope, UserEvent,
};
use graphix_package_core::deftype;
use netidx_value::Value;

#[derive(Debug)]
struct MyOnce {
    val: bool,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for MyOnce {
    const NAME: &str = "mylib_once";
    deftype!("fn('a) -> 'a");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(MyOnce { val: false }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for MyOnce {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        match from {
            [s] => s.update(ctx, event).and_then(|v| {
                if self.val {
                    None
                } else {
                    self.val = true;
                    Some(v)
                }
            }),
            _ => None,
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.val = false
    }
}
```

### Generic Builtins

If your builtin's type is parameterized over the runtime types `R` and `E`, use
the `as` syntax in the builtins list:

```rust
defpackage! {
    builtins => [
        MyGeneric as MyGeneric<GXRt<X>, X::UserEvent>,
    ]
}
```

## Custom Displays

Packages can provide custom display implementations (like the TUI package). Use
the `is_custom` and `init_custom` closures in `defpackage!`:

```rust
defpackage! {
    builtins => [...],
    is_custom => |gx, env, e| {
        // return true if this expression should use your custom display
        false
    },
    init_custom => |gx, env, stop, e| {
        // initialize and return a Box<dyn CustomDisplay<X>>
        unreachable!()
    }
}
```

## Dependencies Between Packages

Packages can depend on other packages via Cargo. Add the dependency to your
`Cargo.toml`:

```toml
[dependencies]
graphix-package-core = "0.3"
graphix-package-time = "0.3"
```

Your package's `register()` function (generated by `defpackage!`) automatically
calls `register()` on all its `graphix-package-*` dependencies before
registering itself. This ensures transitive dependencies are always available.

## Testing

The `defpackage!` macro generates a `TEST_REGISTER` constant that includes
register functions for all package dependencies. Use the test macros from
`graphix-package-core`:

```rust
#[cfg(test)]
mod test {
    use graphix_package_core::run;

    run!(my_test, "mylib::my_builtin(true)", |r| {
        matches!(r, Ok(netidx::subscriber::Value::Bool(true)))
    });
}
```

The `run!` macro sets up a full Graphix runtime with your package registered,
compiles the expression, and checks the result against your predicate.

## Publishing

Packages are published to crates.io like any other Rust crate:

```
cd graphix-package-mylib
cargo publish
```

Once published, anyone can install it with `graphix package add mylib`.
