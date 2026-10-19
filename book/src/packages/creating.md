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

<!-- CR claude for claude: [doc-drift/medium] The tree below leaves out build.rs, which
`create_package` writes, and the page never says a package needs it or the
`graphix-ast-pack` build-dependency. `defpackage!` includes `$OUT_DIR/graphix_ast.pack`,
which only that build script produces, so a crate set up from this page fails with a
bare "OUT_DIR not defined". `graphix_derive::check_invariants` does not check for it
either. Fix: list build.rs, show `graphix_ast_pack::emit()` and the build-dependency
here, and have `check_invariants` panic with that instruction when the build script or
the build-dependency is missing. Found by reading; not run. (book-modules-udt-05) -->
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

// ... builtin implementations (types are declared in .gx files) ...

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
the module `mylib::foo` (note you still need `mod foo` in mod.gx).

The top-level module file is `src/graphix/mod.gx`. This is where you typically
bind your builtins to Graphix names and re-export them. Builtin lambdas must
have full type annotations on all arguments and the return type:

```graphix
let my_builtin = |arg: Any| -> bool 'mylib_my_builtin;
let my_cached = |@args: bool| -> bool 'mylib_my_cached;
```

### `src/graphix/mod.gxi` -- Interface File

The interface file declares the public API of your package:

```graphix
/// Check if a value is an error
val my_builtin: fn(v: Any) -> bool;

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
`mylib`, builtins must be named `mylib_something` (or `mylib` itself). The `register` function
`defpackage!` generates refuses any other name when the package loads.

<!-- CR claude for claude: [doc-drift/low] The MinEv example below declares
`Effect::Stateless(None)` although this page says `None` is only for a raw Apply and
every pure CachedArgs builtin takes a fast fn (core's min is `fast_builtin!`). Its
handling of a missing argument is dead, because CachedArgs never passes one. The copy in
embedding/overview.md names it `core_min`, which `defpackage!` refuses in a third-party
package, and binds it without core's `'a: Ordered`. Fix: one example built with
`fast_builtin!`, named `mylib_min` and bound with `'a: Ordered`, kept here and linked
from the overview. Found by reading; not run. (book-modules-udt-09) -->
### The Simple Path: `EvalCached` / `CachedArgs`

For pure functions that just compute a result from their arguments, use
`EvalCached`:

```rust
use graphix_compiler::{Effect, ExecCtx, Rt, UserEvent};
use graphix_package_core::{CachedArgs, CachedVals, EvalCached, unit_image_state};
use netidx_value::Value;

#[derive(Debug, Default)]
struct MyMinEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for MyMinEv {
    const NAME: &str = "mylib_min";
    // Every output appears on the cycle its inputs arrive, and the
    // result depends only on the arguments; no fast call yet.
    const EFFECT: Effect = Effect::Stateless(None);

    fn eval(&mut self, _ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
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

// a payload with no state to image
unit_image_state!(MyMinEv);

type MyMin = CachedArgs<MyMinEv>;
```

Then list `MyMin` in your `defpackage!` builtins. `CachedArgs` handles all the
details of caching argument values, calling `eval` when arguments change, and
implementing the `Apply` trait.

`EFFECT` is the builtin's whole classification, three questions
answered in one place:

- `Effect::Async` — the result may arrive on a later cycle than its
  trigger, autonomously, or never (a timer, a network read). The
  conservative default.
- `Effect::Sync` — every output appears on the cycle of its trigger,
  but the instance keeps state across invocations (a tally, a memo of
  earlier calls), or its result depends on which arguments have been
  delivered.
- `Effect::Stateless(fast)` — same-cycle, and the result depends only
  on the arguments; never an effect. `fast` is the fast call described
  next, or `None` only for a raw `Apply` that answers before all its
  arguments have arrived. A wrong `Stateless` is a semantics bug: an
  arm holding the builtin skips sleep, and a wake recomputes it.
- `Effect::Io(io)` — same-cycle and stateless, but it acts on the world
  each time its trigger argument fires (printing, a log line, `exit`).
  An arm holding one sleeps, and a wake computes its value without the
  effect: `EvalCached::quiet` (its default is `eval`, so an `Io` builtin
  overrides it to skip the effect). `io` is an `IoCall`, how a fused
  kernel calls it: `result` computes the call's value, and each time
  argument `trigger` fires the kernel queues the call, which
  `BuiltIn::perform` performs after the kernel returns; `None` keeps the
  builtin out of native code.

Stateless matters twice: a tail recursion compiles to a native loop
only when every builtin it reaches is stateless, and fusion into native
code needs the fast call.

#### Fast calls

A builtin fuses into a native kernel only through a **fast call**: a
plain function the kernel calls directly on its argument values in
place, with no dispatch machinery and no marshal at all — a few
nanoseconds per call, where a node-walked call costs about 140 ns.
Without one the builtin stays correct and runs on the interpreter at
every site.

```rust
use graphix_compiler::{Effect, FastCall};
use graphix_package_core::fast_eval;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for MyLenEv {
    const NAME: &str = "mylib_len";
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(my_len)));

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(ctx, my_len, from)
    }
}

fn my_len(args: &[Value]) -> Option<Value> {
    match args {
        [Value::Array(a)] => Some(Value::I64(a.len() as i64)),
        _ => None,
    }
}
```

The contract: the function only ever sees present values (a missing or
bottom argument bottoms the call before it is invoked), `None` means
"no value this cycle", and the compiler decides whether the result
fired from the arguments. Use it for cheap pure functions — there is no
result cache, so the function runs on every kernel invocation in which
any input to the enclosing region fired. Having `eval` delegate to the
same function through `fast_eval` keeps the interpreter and the JIT on
one implementation. A builtin whose result is directed by its call
site's return type (`str::parse` casting to its target) declares
`FastCall::Typed` instead: the same contract, plus the resolved type
and the environment it resolves in.

A `CachedArgs` builtin never sees a missing argument: an invocation
with one undelivered or bottom is bottom before `eval` runs. So every
pure one can take a fast function, and the standard library's do,
through `graphix_package_core::fast_builtin!`, which writes the whole
shell from a name and the function. A builtin that must answer before
all its arguments arrive is a raw `Apply` (below), not an
`EvalCached`, and has no fast function.

### The Full-Control Path: `BuiltIn` + `Apply`

For builtins that need to interact with the execution context, manage internal
state across cycles, or work with higher-order functions, implement the
`BuiltIn` and `Apply` traits directly. See
[Writing Built in Functions](../embedding/builtins.md) for a deep dive.

Here is a minimal example -- `once` passes through exactly one update:

```rust
use anyhow::Result;
use graphix_compiler::{
    Apply, BuiltIn, CompileCtx, Effect, ExecCtx, Node, Rt, Scope, TagValue, TagView,
    UserEvent, expr::ExprId, image::ImageBuf, typ::FnType,
};
use netidx_core::pack::{Pack, PackError};

#[derive(Debug, Default)]
struct MyOnce {
    // whether the one update has passed
    done: bool,
    // the result slot `update` lends to the caller
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for MyOnce {
    const NAME: &str = "mylib_once";
    // it keeps state across invocations
    const EFFECT: Effect = Effect::Sync;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved_typ: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(MyOnce::default()))
    }

    // restore what `image_encode` wrote; the result slot is never imaged
    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        Ok(Box::new(MyOnce { done: bool::decode(buf)?, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for MyOnce {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.done.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        match from[0].update(ctx).view() {
            TagView::Fired(tv) if !self.done => {
                self.done = true;
                let v = tv.value_cloned();
                self.out.set(TagValue::fired(v))
            }
            // a bottom input before the update makes the result bottom
            TagView::FreshBottom if !self.done => self.out.set_bottom(true),
            TagView::StaleBottom if !self.done => self.out.set_bottom(false),
            _ => self.out.ride(),
        }
    }

    // once is a restart builtin: a woken once is a fresh one
    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.done = false
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

Packages can provide custom display implementations. Custom displays allow you
to do something special with a value returned to the shell by a script or in the
REPL. For example the TUI package uses a custom display to take control of the
terminal and render a terminal UI from the returned value.

### The `CustomDisplay` Trait

A custom display implements `CustomDisplay<X>`:

```rust
#[async_trait]
pub trait CustomDisplay<X: GXExt>: Any {
    /// Called when the shell wants to return to normal display mode,
    /// or when the custom display signals stop. Free any resources here.
    async fn clear(&mut self);

    /// Called on every update from the Graphix runtime.
    /// This includes all updates, not just ones related to the custom
    /// display. The future returned must resolve promptly or the shell
    /// will hang.
    async fn process_update(&mut self, env: &Env, id: ExprId, v: Value);

    /// The updates of one runtime cycle have all been processed: a
    /// display that redraws shows the cycle whole here.
    async fn batch_done(&mut self);
}
```

### Registering a Custom Display

To hook a custom display into the shell, provide `is_custom` and `init_custom`
closures in `defpackage!`:

- **`is_custom`** receives each compiled expression and returns `true` if your
  package should handle its display. The shell calls this to decide whether to
  use the default display or delegate to your package.
- **`init_custom`** constructs your `CustomDisplay`. It receives a `stop`
  channel (`graphix_package::Stop`), which the display keeps for as long as it
  lives. Send `Ok(())` on it when the program is done (the user closed the
  window, the script called your exit builtin) and `Err(e)` when the display
  cannot go on (the terminal went away). Dropping it unsent means the display
  died. The shell calls `clear()` before dropping the display; a script then
  exits, with the error if there was one, and the REPL returns to its prompt.
  It also receives `run_on_main`, for a display that needs the main thread.

Here is a minimal example that prints every update to stderr:

```rust
use async_trait::async_trait;
use graphix_compiler::{env::Env, expr::ExprId};
use graphix_package::{CustomDisplay, Stop};
use graphix_rt::GXExt;
use netidx_value::Value;

struct DebugDisplay {
    // held so the shell knows the display is alive
    _stop: Stop,
}

#[async_trait]
impl<X: GXExt> CustomDisplay<X> for DebugDisplay {
    async fn clear(&mut self) {
        eprintln!("[debug display] cleared");
    }

    async fn process_update(&mut self, _env: &Env, id: ExprId, v: Value) {
        eprintln!("[debug display] {id:?} = {v}");
    }

    async fn batch_done(&mut self) {}
}

defpackage! {
    builtins => [...],
    is_custom => |_gx, _env, e| {
        // claim all expressions whose result type is an array
        e.typ.with_deref(|t| {
            matches!(t, Some(graphix_compiler::typ::Type::Array(_)))
        })
    },
    init_custom => |_gx, _env, stop, _e, _run_on_main| {
        Ok(Box::new(DebugDisplay { _stop: stop }))
    }
}
```

The `e` parameter is a `CompExp` which has a `typ` field (the inferred result
type) and an `id` field (the expression ID). Typically `is_custom` checks
whether the result type matches something your display knows how to render, as
the TUI package does with its widget types. The custom display is responsible
for keeping the CompExp alive (if that is necessary), if it is dropped the
expression will be removed from the runtime (just like any other dropped
CompExp).

## Dependencies Between Packages

Packages can depend on other packages via Cargo. Add the dependency to your
`Cargo.toml`:

```toml
[dependencies]
graphix-package-core = "0.9"
graphix-package-sys = "0.9"
```

Your package's `register()` function (generated by `defpackage!`) automatically
calls `register()` on all its `graphix-package-*` dependencies before
registering itself. This ensures transitive dependencies are always available.

<!-- CR claude for claude: [doc-drift/low] The run! example below checks
`mylib::my_builtin(true)` against `Bool(true)`, but this page documents my_builtin as
"Check if a value is an error", so it yields false and the test fails in every mode. It
also names `netidx::subscriber::Value`, and a scaffolded package depends on
netidx-value, not netidx. Fix: test `my_builtin(error(`E))`, or expect `Bool(false)`,
and write `netidx_value::Value`. Found by reading; not run. (book-modules-udt-10) -->
## Testing

The `defpackage!` macro generates a `TEST_REGISTER` constant that includes
register functions for all package dependencies. Use the test macros from
`graphix-package-core`:

```rust
#[cfg(test)]
mod test {
    use graphix_package_core::{run, testing::FuseExpect};

    run!(my_test, "mylib::my_builtin(true)", |r| {
        matches!(r, Ok(netidx::subscriber::Value::Bool(true)))
    }; FuseExpect::None);
}
```

The `run!` macro sets up a full Graphix runtime with your package registered,
compiles `let result = <the expression>`, and checks the first value it
produces against your predicate, which receives a `Result<&Value>`: the
value, or the compile error. Each `run!` is four tests, one per
`testing::Mode`: `interp` (the node-walk), `jit` (fusion on), and each again
with every parallel fork point forked (`par`, `jit_par`). The predicate must
hold in every mode. `interp` also checks the program alone and fails unless
the check refuses exactly when the build does; `; build_only` after the
fusion expectation exempts a refusal only a build makes (a `#[native]`
assertion).

The fusion expectation is checked in `jit` mode: `FuseExpect::Jit` asserts
the program builds a fused kernel, `FuseExpect::None` that it builds none.
Leaving it out means `FuseExpect::Jit`, which fails for a builtin with no
fast call (`Effect::Stateless(None)`, or any `Sync` or `Async` builtin),
since a call to it does not fuse.

## Publishing

Packages are published to crates.io like any other Rust crate:

```
cd graphix-package-mylib
cargo publish
```

Once published, anyone can install it with `graphix package add mylib`.
