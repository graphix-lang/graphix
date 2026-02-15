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

## Building Stand Alone Graphix Applications

You can build single binary stand alone Graphix applications using the
`graphix-shell` crate. All your Graphix source code and packages are compiled
together with the compiler and runtime into a single binary.

```rust
use anyhow::Result;
use graphix_rt::NoExt;
use graphix_shell::{Mode, ShellBuilder};
use graphix_compiler::expr::Source;
use netidx::{
    publisher::{DesiredAuth, PublisherBuilder},
    subscriber::Subscriber,
};

pub async fn run(cfg: netidx::config::Config, auth: DesiredAuth) -> Result<()> {
    let publisher = PublisherBuilder::new(cfg.clone())
        .desired_auth(auth.clone())
        .build()
        .await?;
    let subscriber = Subscriber::new(cfg, auth)?;
    ShellBuilder::<NoExt>::default()
        .mode(Mode::Script(Source::from("main.gx")))
        .publisher(publisher)
        .subscriber(subscriber)
        .no_init(true)
        .build()?
        .run()
        .await
}
```

All installed packages are automatically registered. See [Stand Alone Graphix
Applications](./shell.md) for details.

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
