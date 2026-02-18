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

```rust
use anyhow::Result;
use graphix_compiler::expr::Source;
use graphix_rt::NoExt;
use graphix_shell::{Mode, ShellBuilder};
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
    .run()
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
    .run()
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
