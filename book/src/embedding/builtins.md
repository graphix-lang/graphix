# Writing Built in Functions

As mentioned in the introduction you can extend Graphix by writing
built in functions in Rust. This chapter will deep dive into the full
API. If you just want to write a pure function see the
[Overview](./overview.md), or better yet the
[Creating Packages](../packages/creating.md) guide.

Most users should create a [package](../packages/overview.md) rather
than using these traits directly. The `defpackage!` macro handles
registration and module setup automatically. This chapter is for
understanding the internals.

In order to implement a built-in Graphix function you must implement
two traits,
[`graphix_compiler::BuiltIn`](https://docs.rs/graphix-compiler/latest/graphix_compiler/trait.BuiltIn.html)
and
[`graphix_compiler::Apply`](https://docs.rs/graphix-compiler/latest/graphix_compiler/trait.Apply.html).
See the rustdoc for details. These two traits give you more control
than the `CachedArgs` method we covered in the overview. Lets look at
the simplest possible example.

## Understanding The Once Function

The `once` function evaluates its argument every cycle and passes
through one and only one update. The `update` method is the most
important method of `Apply`. It is called every cycle the node is
awake, and it returns a `TagValue` every time: the value the node
holds and whether it is new this cycle. A node `Fired` this cycle, or
holds a `Stale` value from before, or has no value at all
(`FreshBottom`, `StaleBottom`). `once` passes its argument's first
`Fired` through and is `Stale` from then on. Consider the example
program,

```graphix
let clock = sys::time::timer(1, true);
println(once(clock))
```

We expect this example to print the datetime exactly one time. The
clock created by `sys::time::timer` ticks once per second forever: the
built-in asks the
[`Rt`](https://docs.rs/graphix-compiler/latest/graphix_compiler/trait.Rt.html)
for a timer, which schedules a cycle a second from now and records that
the toplevel node (`let clock = ...`) depends on the timer's event.
When the timer fires, the approximate sequence of events is,

- `let clock = ..`: the toplevel `Bind` updates its right side
    - `sys::time::timer(1, true)` finds its event and returns `Fired(DateTime(..))`
    - the bind delivers `clock`'s new value to the event, and the
      runtime schedules what reads `clock`
- `println(once(clock))`: the toplevel `CallSite` updates its argument
    - `once` updates `clock`, which returns `Fired(DateTime(..))`
    - it is `once`'s first `Fired`, so `once` returns it `Fired`
    - `println` prints the datetime

At the next tick `clock` fires again, `once` returns its value `Stale`,
and `println` does not print.

## Implementing Once

```rust
use anyhow::Result;
use graphix_compiler::{
    Apply, BuiltIn, CompileCtx, Effect, ExecCtx, Node, Rt, Scope, TagValue, UserEvent,
    expr::ExprId, image::ImageBuf, typ::FnType,
};
use graphix_package_core::{bottom_of, seam_tick};
use netidx_core::pack::{Pack, PackError};

#[derive(Debug, Default)]
struct Once {
    // whether the one update has passed
    done: bool,
    // the result slot `update` lends to the caller
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Once {
    const NAME: &str = "core_once";
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
        Ok(Box::new(Once::default()))
    }

    // restore what `image_encode` wrote; the result slot is never imaged
    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        Ok(Box::new(Once { done: bool::decode(buf)?, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Once {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.done.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let x = from[0].update(ctx);
        if let Some(bottom) = bottom_of([x.tag()]) {
            return bottom;
        }
        match seam_tick(x) {
            Some(tv) if !self.done => {
                self.done = true;
                self.out.set(TagValue::fired(tv.value_cloned()))
            }
            _ => self.out.ride(),
        }
    }

    // sleep is a pause: the latch is kept across it
    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}

    // a seq step entered for its next run is a birth: forget the update
    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.done = false;
        self.out = TagValue::phantom()
    }
}
```

The `BuiltIn` trait is for construction. It declares the built-in's
name. The function's type is declared in the `.gx` file where the
builtin is bound — all arguments and the return type must be
annotated. For example, `once` would be bound as:

```graphix
let once = |v: 'a| -> 'a 'core_once
```

The `init` method is called when the built-in is instantiated at a
call site. It receives the compile context, the concrete function
type, the lexical scope, the argument nodes, and the top-level
expression id. In this case we don't care about any of that
information, but it will be useful later. `EFFECT` says what the
output depends on (`Sync`: the same cycle, with state kept across
invocations), and `image_encode`/`image_decode` write and restore the
state `init` built when the session is cached as an image.

The most important method of `Apply` is `update`. `once` reads its
argument through the seam helpers of `graphix_package_core`, the raw
`Apply` author's API. `bottom_of` takes the tags of the values the
builtin consumes and, when any is bottom, gives the bottom to return in
place of a ride: a bottom input makes the result bottom, before the
latch and after it, and the latched value shows again once the input
returns. `seam_tick` is `Some` only for a `Fired` delivery, the event
that advances a builtin's state; `seam_value` is `Some` for any
delivery holding a value, fired or stale, for an argument read as
configuration; `seam_arg` updates an argument node and returns its
value and whether it fired. Every argument is updated every cycle,
before any early return.

`sleep` is called when the node's select arm is unselected, and sleep
is a pause: values and other semantic state are kept, so the node
resumes from them when the arm wakes, and a wake starts nothing an
argument did not ask for. A builtin aborts an operation in flight that
it cannot pause and keeps its last result: the wake does not issue the
call again (a call that is not cancel-safe, a stream read for one,
finishes instead and answers at the wake). Only an event source clears
its output to `TagValue::phantom()`, since its output is its last
event. `reborn` is called before an update that follows a sleep and is
a birth, a seq step entered for its next run: that is where `once`
clears its flag, as a new one would hold nothing.

## Higher Order Functions

Now for a built-in that takes another Graphix function as an argument.
The simplest example in the standard library is `array::group`, `Group`
in `stdlib/graphix-package-array/src/lib.rs`; read it beside this
section.

This implements `array::group`, which given an argument, stores that
argument's updates internally, and creates an array out of them when
the predicate returns true. Its type is

```fn(v: 'a, f: fn(len: i64, x: 'a) -> bool throws 'e) -> Array<'a> throws 'e```

For example,

```graphix
let n = range(0, 100)$;
array::group(n, |_, n| (n == 50) || (n == 99))
```

`range(0, 100)` updates 100 times from 0 to 99. The `array::group` will
create two arrays, one containing `[0, .. 50]` and the other
containing `[51, .. 99]`.

The implementation needs a call of the predicate in the graph: a call
site node, which handles late binding, optional arguments, default
arguments and the rest. `genn::Callback` builds one and owns it. Its
`init` makes it from the predicate's resolved function type and a name
for each argument:

```rust,ignore
let pred = genn::Callback::new(ctx, &scope, &mftyp, &["n", "x"], top_id)?;
```

`Callback::new` binds a private variable for each argument and one for
the function, and builds the call over them. (`Callback::through` calls
the function a binding the builtin already holds, and
`Callback::of_value` a function value known when it is built.) From
then on the builtin drives it:

- `feed_fn(ctx, tv)` hands it the function, every time the function
  argument (`from[1]`) produces: functions are late bound, so the
  function a call reaches may change, and the call site follows it;
- `feed_arg(ctx, i, tv)` delivers argument `i` for this cycle;
- `update(ctx)` updates the call and returns its production, which
  fires when the predicate answers: maybe this cycle, maybe later, maybe
  never;
- `typecheck0`, `refs`, `sleep` and `delete` are forwarded from the
  builtin's own.

### Typecheck

An `Apply` has two typecheck hooks, both with defaults that do nothing.
`typecheck0(ctx, from)` runs once the argument nodes have typechecked;
a builtin that generated nodes (the predicate call site above)
typechecks them here. `typecheck1(ctx, from, resolved)` runs after
static resolution with the call site's resolved signature, and it is
where a builtin learns the types it acts on: a callback's signature, an
element type, a cast target.

#### Type-directed builtins

A builtin whose behaviour depends on a type the program chooses, such as
`str::parse` or `json::read` choosing what to decode, declares that type
`Concrete` in its signature, in the `.gx` and the `.gxi` alike:

```graphix,ignore
let parse = 'b: Concrete |s: string| -> Result<'b, `ParseError(string)> 'str_parse
val parse: fn<'b: Concrete>(s: string) -> Result<'b, `ParseError(string)>;
```

The type checker then refuses every program in which `'b` is not fully
known where it settles, at the call or inside a function that is never
called, and a generic wrapper (`let p = |s| str::parse(s)`) carries the
requirement to its own callers. The builtin's `typecheck1` only extracts
the type with `extract_cast_type(Some(resolved))`. It never refuses: a
program that type checked has a known type there.

#### Builtins that wrap a function

A builtin that takes any function and reads its signature, such as
`queuefn`, declares that parameter's type `Function`:

```graphix,ignore
let queuefn = 'a: Function |#count: [&mut i64, null] = null, #trigger: Any, f: 'a| -> 'a 'core_queuefn;
val queuefn: fn<'a: Function>(?#count: [&mut i64, null], #trigger:Any, f: 'a) -> 'a;
```

The type checker refuses every program that passes something else
there: a value that is not a function, a function with a variadic
argument or a defaulted label (the wrapper's generated call passes every
formal and no more), or one whose type stays unknown. The builtin's
`typecheck1` reads the signature from the resolved
argument (through type references) without refusing.

### BindIds and Refs

`BindId` names a variable. The
[`Event`](https://docs.rs/graphix-compiler/latest/graphix_compiler/struct.Event.html)
holds this cycle's deliveries in `variables`, indexed by it: a variable
that updated this cycle has its `TagValue` there. A `Callback`'s
arguments and function are variables of its own, and its call site's
argument nodes are references to them, so feeding an argument is
delivering to its variable (`args()` lists their ids, for a builtin that
writes one for a later cycle with `ctx.rt.set_var`).

### The store, refs, delete

When a call site is initialized for the first time, or a select arm
wakes from sleep, it needs the current value of every variable it
depends on, so the runtime keeps the current value of every variable
in its store. A builtin that sets a variable itself never writes
`ctx.event.variables` directly: `ExecCtx::deliver(id, tv)` delivers this
cycle and updates the store (a bottom stands there as a stale one), and
`ExecCtx::publish(id, tv)` delivers this cycle alone. Both mark the
variable's readers, so the walk visits them (`Callback::feed_arg` and
`feed_fn` deliver).

`refs` reports every variable a node depends on, so the runtime can
hand them their values when the node wakes from sleep or a call site
stands up for the first time; a builtin that owns a `Callback` forwards
it.

`delete` removes the node's part of the graph, which changes at run
time: a `Callback`'s `delete` drops its call and releases every
variable it owns. Graphix does this by hand rather than in `Drop`,
which would need every `Node` to hold a reference to the `ExecCtx`.
