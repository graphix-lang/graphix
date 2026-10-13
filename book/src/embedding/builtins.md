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

## Sleeping asynchronous builtins

If sleeping a builtin discards pending work or detaches an event source,
also reset its output to `TagValue::phantom()`. On wake, a restarted
operation must wait for its own completion instead of exposing the previous
activation's result. This applies outside `seq` as well as inside it.
Retain outputs for paused computations whose semantic value remains valid;
do not reset every builtin merely because it is classified asynchronous.

## Implementing Once

```rust
use anyhow::Result;
use graphix_compiler::{
    Apply, BuiltIn, CompileCtx, Effect, ExecCtx, Node, Rt, Scope, TagValue, TagView,
    UserEvent, expr::ExprId, image::ImageBuf, typ::FnType,
};
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

The most important method of `Apply` is `update`. `sleep` is called
when the node's select arm is unselected, and sleep is a pause: values
and other semantic state are kept, so the node resumes from them when
the arm wakes. Only a builtin that discards pending work or detaches
an event source clears its output to `TagValue::phantom()` (see
above). `once` clears its flag in `sleep` because it is one of the
restart builtins (`once`, `take`, `skip`, `uniq`, `hold`, `count`):
a woken one is a fresh one.

## Higher Order Functions

Right, now that the easy stuff is out of the way, lets see how we can
implement a built-in that takes another Graphix function as an
argument. This gets compiler guts all over the place, sorry about
that. The simplest example in the standard library is `array::group`,
`Group` in `stdlib/graphix-package-array/src/lib.rs`; read it beside
this section. Its `init` generates the nodes for a call of the
predicate:

- `genn::bind` makes two variables for the predicate's arguments, `n`
  and `x`, and returns their `BindId`s (`nid`, `xid`) and reference
  nodes;
- a third `BindId` (`pid`) names the predicate itself, and
  `genn::reference` makes a reference node to it;
- `genn::apply` builds a call site over that reference with the two
  argument references, the `pred` node.

Its `update` delivers values to those variables and updates `pred`.

<!-- CR claude for claude: [doc-drift/low] The group example below is refused: `range(0,
100)` now yields a `RangeError` Result, so `n == 50` compares `[i64, Error<..>]` with
i64 (probe: design/review-2026-10-12/repro/book-modules-udt-builtins-group.gx). Write
`range(0, 100)$`, and the type shown needs `throws 'e`. Other book programs are also
wrong. udt/polymorphic.md shows `{x: 0, ..}` for a call that adds 1 to x, but the output
is `{x: 1, y: 1, z: 3.14}`. udt/structs.md's first file example has two statements and
no `;`, which is a parse error. structs.md says a call's result must be parenthesized
before `.field`, but `f().foo` works. modules/implementation.md runs
`GRAPHIX_MODPATH=netidx:/local/graphix graphix test`, which fails with 'No such file';
the file argument must name a script. Fix each one; the doc test proposed in
book-modules-udt-09 would keep them honest. (book-modules-udt-14) -->
This implements `array::group`, which given an argument, stores that
argument's updates internally, and creates an array out of them when
the predicate returns true. Its type is

```fn(v: 'a, f: fn(len: i64, x: 'a) -> bool) -> Array<'a>```

For example,

```graphix
let n = range(0, 100);
array::group(n, |_, n| (n == 50) || (n == 99))
```

`range(0, 100)` updates 100 times from 0 to 99. The `array::group` will
create two arrays, one containing `[0, .. 50]` and the other
containing `[51, .. 99]`.

The implementation needs to build a `Node` representing the
predicate. `Node` is the fundamental type of everything in the graph.
Ultimately the entire program compiles to a node. The kind of node we
need to create here is a function call site, that will handle all the
details of late binding, optional arguments, default args, etc. The
`genn` module is specifically for generating nodes.

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

```graphix
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

```graphix
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

`BindId` is a very fundamental type in compiler guts. The
[`Event`](https://docs.rs/graphix-compiler/latest/graphix_compiler/struct.Event.html)
struct contains two tables indexed by it. The most important is `variables`.
Every bound variable has a `BindId`. If a variable has updated this cycle, then
its updated value, a `TagValue`, will be in the `variables` table indexed by
its `BindId`. In
order to call this predicate function we actually create three different
variables and store their `BindIds` as `xid`, `nid`, and `pid`.
`genn::reference` returns a reference `Node` and the `BindId` of the variable it
is referencing. Since those ref nodes become the arguments to the predicate call
site we create, `xid` and `nid` allow us to control the arguments passed into
the function. We just have to set `xid` and `nid` in `Event::variables` before
we update the predicate in order to `call` the function. This may cause it to
update immediately, or, it may depend on something else that needs to update
before it will update. Either way, once we've set `xid` and `nid` once and
called update on the predicate we've done our duty (it may never update, and
that's ok). That just leaves `pid`, what is it for? Well, earlier it was
mentioned that functions are always late bound. This is how that works. The
lambda argument we were passed `from[1]`, whatever kind of node it is, will
ultimately update and return a `Lambda`, which a compiled function. So every
cycle we need to call update on this node just like any other node, because the
`Lambda` we are calling might change, and if that happens the call site we
created with `genn::apply` needs to know about it. Luckily we don't have to
handle any of the wonderful details of late binding beyond this simple passing
through of updates, the call site will take care of that.

### The store, refs, delete

When a call site is initialized for the first time, or a select arm
wakes from sleep, it needs the current value of every variable it
depends on, so the runtime keeps the current value of every variable
in its store. A builtin that sets a variable itself updates both: the
store (`ctx.rt.store_insert(id, tv)`) and this cycle's event
(`ctx.event.variables.insert(id, TagValue::fired(v))`).

And this also explains another function that you have to implement
when you're generating nodes, which is `refs`. Turns out we need to know
all the variables a node depends on, so we can set them when it's
being woken up from sleep, or stood up at a call site for the first
time.

That just leaves `delete`. The structure of the graph changes at
runtime, and we need to keep everything straight. It would be nice if
we could do this with `Drop`, but that would require holding a
reference to the `ExecCtx` at every `Node`. I'd really rather not pay
to wrap every access to the context in a mutex, so we're doing it the
hard way (for now).
