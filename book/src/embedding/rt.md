# Embedded Scripting Systems

You can use Graphix as an embedded scripting engine in your application. For
this application we leave the shell behind and move to the
[`graphix-rt`](https://docs.rs/graphix-rt/latest/graphix_rt) crate. This is a
lower level crate that gives you a lot more control over the compiler and
runtime.

## Tokio

`graphix-rt` uses tokio internally. It will run the compiler and run loop in
its own tokio task. You interface with this task via a
[`GXHandle`](https://docs.rs/graphix-rt/latest/graphix_rt/struct.GXHandle.html).
As a result, all operations on Graphix objects are async, and the compiler/run
loop will run in parallel with your application.

## Setting it up

The shell itself is the best example of using the `graphix-rt` crate. As we get
further into internals, the details can change more often, but in general
getting a Graphix runtime going involves the following:

```rust
use graphix_rt::{GXConfig, GXRt, NoExt};
use tokio::sync::mpsc;

// set up an execution context using the generic runtime with no customization
let mut ctx = GXRt::<NoExt>::new_state()?;
// ... use the context to register all your built-ins, etc

// set up a channel to receive events from the RT
let (tx, rx) = mpsc::channel(100);
// build the config and start the runtime
let handle = GXConfig::builder(ctx, tx)
    // set up a root module. Note if you want the standard library you must
    // register packages or load modules manually. Otherwise you will get
    // the bare compiler with no standard library.
    .root(arcstr::literal!("mod mymod"))
    .build()?
    .start().await?;
```

Once that all succeeds you have a running compiler/run loop, and a handle that
can interact with it. You are expected to read the `rx` portion of the mpsc
channel. If you do not, the run loop will block waiting for you to read once the
mpsc channel fills up.

## Compiling Code, Getting Results

Once setup is complete, lets compile some code and get some results! To compile
code we call
[`compile`](https://docs.rs/graphix-rt/latest/graphix_rt/struct.GXHandle.html#method.compile)
on the handle. This results in one or more toplevel expressions and a copy of
the environment (or a compile error).

```rust
let cres = handle.compile(arcstr::literal!("2 + 2")).await?;

// in this case we know there is just one top level expression
let e = &cres.exprs[0];

// the actual result will come to us on the channel. If the expression kept
// producing results, we'd keep seeing updates for its id on the channel.
// This is a batch of GXEvents
let mut batch = rx.recv().await.ok_or_else(|| anyhow::anyhow!("the runtime is dead"))?;
let mut env = handle.get_env().await?;
for ev in batch.drain(..) {
    match ev {
        graphix_rt::GXEvent::Updated(id, v) if id == e.id => println!("2 + 2 = {v}"),
        graphix_rt::GXEvent::Env(e) => env = e,
        _ => ()
    }
}
```

## Refs and TRefs, Depending on Graphix Variables

If you want to be notified when a variable in Graphix updates you can register a
[`Ref`](https://docs.rs/graphix-rt/latest/graphix_rt/struct.Ref.html), or a
[`TRef`](https://docs.rs/graphix-rt/latest/graphix_rt/struct.TRef.html) if you
have a corresponding Rust type that implements
[`FromValue`](https://docs.rs/netidx-value/latest/netidx_value/trait.FromValue.html).
There are two ways to get a ref, by id and by name. By id is probably the most
common, because
[`BindId`](https://docs.rs/graphix-compiler/latest/graphix_compiler/struct.BindId.html)
will appear in any data structure that has a value passed by ref (e.g. `&v`),
which should be common for large structures that don't change often.

A Rust type that mirrors a Graphix type need not implement `FromValue` by hand:
`#[derive(FromValue, IntoValue)]` from the `netidx-derive` crate maps a struct's
named fields onto the Graphix struct's fields by name, and an enum's variants
onto Graphix variants (`` `Tag ``, `` `Tag(a, b) ``, `` `Tag({ .. }) ``). A field
passed by reference (`&T`) reads as a `BindId`, a reference's own value
(`graphix_compiler::reference::cell` reads one from a `Value`).

Every derive over a Graphix value, in either direction, carries
`#[value(shape = "graphix_compiler::rep::GraphixShape")]`. A Graphix
tuple, struct or variant value carries a kind marker the default
(netidx) shape does not know: without the attribute, `FromValue`
refuses every struct, tuple and variant the program sends, and
`IntoValue` builds values the program does not read as one. The
generated code names `netidx_value`, `anyhow` and `graphix_compiler`, so
the crate depends on all three.

```rust
// the Graphix type {x: f64, y: f64, label: &string}
#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
struct Point {
    x: f64,
    y: f64,
    label: BindId,
}
```

```rust
// assume we got id from a data structure and its type is &i64
let mut r = handle.compile_ref(id).await?;

// if the variable r is bound to has a value right now it will be in last
if let Some(v) = &r.last {
    println!("current value: {v}")
}

// now we will get an update whenever the variable updates
let mut batch = rx.recv().await.ok_or_else(|| anyhow::anyhow!("the runtime is dead"))?;
for ev in batch.drain(..) {
    match ev {
        graphix_rt::GXEvent::Updated(id, v) => {
            if let Some(v) = r.update(id, &v) {
                println!("current value {v}")
            }
        },
        graphix_rt::GXEvent::Env(_) => ()
    }
}
```

You can also set refs, which is exactly the same thing as the connect operator
`<-`, and does what you expect it should do.

### Ref By Name

We can also reference a variable by name:

```rust
let mut r = handle.compile_ref_by_name(
    &env,
    &graphix_compiler::Scope::root(),
    &graphix_compiler::ModPath::from(["foo"]),
).await?;

// the rest of the code is exactly the same
```

## Calling Graphix Functions

A function is called through a call site the runtime compiles for it,
a [`Callable`](https://docs.rs/graphix-rt/latest/graphix_rt/struct.Callable.html).
[`compile_callable`](https://docs.rs/graphix-rt/latest/graphix_rt/struct.GXHandle.html#method.compile_callable)
takes the function as a `Value`: one an expression returned, or a
variable's value. To call a function by name, take its value through a
ref first:

```rust
let r = handle.compile_ref_by_name(
    &env,
    &graphix_compiler::Scope::root(),
    &graphix_compiler::ModPath::from(["sum"]),
).await?;
let sum = r.last.clone().ok_or_else(|| anyhow::anyhow!("sum has no value yet"))?;
let f = handle.compile_callable(sum).await?;
```

[`call_answered`](https://docs.rs/graphix-rt/latest/graphix_rt/struct.Callable.html#method.call_answered)
calls it, and its return value arrives on the channel under the
callable's `answer` id:

```rust
f.call_answered(netidx_value::ValArray::from_iter_exact(
    [Value::from(1), Value::from(2), Value::from(3)]
)).await?;
'answer: loop {
    let mut batch = rx.recv().await.ok_or_else(|| anyhow::anyhow!("the runtime is dead"))?;
    for ev in batch.drain(..) {
        match ev {
            graphix_rt::GXEvent::Updated(id, v) if id == f.answer => {
                println!("sum returned {v}");
                break 'answer;
            }
            graphix_rt::GXEvent::Env(e) => env = e,
            _ => (),
        }
    }
}
```

`call` makes the same call without an answer, for a function called
for its effects (an event handler).
