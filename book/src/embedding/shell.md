# Custom Embedded Applications

For most standalone binaries, `graphix package build-standalone` is the simplest
approach — see [Standalone Binaries](../packages/standalone.md). This section
covers the more advanced case where you need full control: custom module
resolvers, embedded REPLs, compiler flags, or integration with your own Rust
application.

Using the
[graphix-shell](https://docs.rs/graphix-shell/latest/graphix_shell) crate you
can build a custom Graphix application. All installed packages are
automatically registered, so your application gets the full standard library and
any additional packages out of the box.

## Basic Application

The shell needs a `MainThreadHandle` so widgets that must run on the
main thread (notably the GUI backend) can dispatch work back from the
tokio runtime. The standard pattern is to spawn tokio on its own thread
and pump the main-thread queue from `main`:

```rust
use anyhow::Result;
use graphix_compiler::expr::Source;
use graphix_rt::NoExt;
use graphix_shell::{MainThreadHandle, Mode, ShellBuilder};
use netidx::{
    publisher::{DesiredAuth, PublisherBuilder},
    subscriber::Subscriber,
};

async fn tokio_main(
    cfg: netidx::config::Config,
    auth: DesiredAuth,
    run_on_main: MainThreadHandle,
) -> Result<()> {
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
        .run(run_on_main)
        .await
}

fn main() -> Result<()> {
    let cfg = netidx::config::Config::load_default()?;
    let auth = DesiredAuth::Anonymous;
    let (handle, main_rx) = MainThreadHandle::new();
    let tokio_thread = std::thread::Builder::new()
        .name("tokio".into())
        .spawn(move || {
            tokio::runtime::Builder::new_multi_thread()
                .enable_all()
                .build()?
                .block_on(tokio_main(cfg, auth, handle))
        })
        .expect("spawn tokio thread");
    while let Ok(f) = main_rx.recv() {
        f();
    }
    tokio_thread.join().expect("tokio thread panicked")
}
```

`MainThreadHandle::new()` returns the handle you pass to `.run()` and a
receiver you drive on the main thread. The receiver yields closures the
shell wants executed there; calling each in turn is enough.

## Module Resolvers

If you want to bundle additional Graphix source files into your binary (beyond
what packages provide), you can add module resolvers. A VFS resolver maps
virtual paths to source code:

```rust
use arcstr::literal;
use graphix_compiler::expr::ModuleResolver;
use fxhash::FxHashMap;
use netidx_core::path::Path;

fn my_modules() -> ModuleResolver {
    ModuleResolver::VFS(FxHashMap::from_iter([
        (Path::from("/myapp"), literal!(include_str!("myapp/mod.gx"))),
        (Path::from("/myapp/util"), literal!(include_str!("myapp/util.gx"))),
    ]))
}

ShellBuilder::<NoExt>::default()
    .module_resolvers(vec![my_modules()])
    .mode(Mode::Script(Source::from("main.gx")))
    .publisher(publisher)
    .subscriber(subscriber)
    .build()?
    .run(run_on_main)
    .await
```

You can have as many module resolvers as you like. When loading modules they are
checked in order, so earlier ones shadow later ones.

Note that for most cases, creating a [package](../packages/creating.md) is
preferable to manually constructing VFS resolvers. Packages handle module
registration automatically through the `defpackage!` macro.

## Custom REPL

You can build a REPL with pre-loaded modules by setting the mode to
`Mode::Repl`:

```rust
ShellBuilder::<NoExt>::default()
    .module_resolvers(vec![my_modules()])
    .mode(Mode::Repl)
    .publisher(publisher)
    .subscriber(subscriber)
    .build()?
    .run(run_on_main)
    .await
```

This gives you a REPL with the standard library, all installed packages, and
your additional modules available.

## Compiler Flags

You can enable or disable compiler flags:

```rust
use graphix_compiler::CFlag;

ShellBuilder::<NoExt>::default()
    .enable_flags(CFlag::WarnUnused | CFlag::WarnUnhandled)
    .mode(Mode::Repl)
    // ...
```
