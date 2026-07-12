# General Principles

- The 11th commandment handed down by god is "Thou shalt not repeat thyself in code"
- The 12th commandment handed down by god is "Thou shalt not create unweildy and
  excessive layers of abstraction"
- The 13th commandment handed down by god is "Thou shalt not allocate memory"
  (aspirational, like many of the real commandments — literally impossible,
  but expend reasonable effort to avoid it. Err on the side of using
  `LPooled` even on colder paths; it's nearly free. Only reach for an
  unpooled `Vec`/`String`/etc. when a foreign API requires owned data you
  can't get from a pooled container.)
- The 14th commandment handed down by god is "Thou shalt make invalid states
  unrepresentable" 
- if you feel the need to reach across an abstraction boundary, consider first
  that maybe the abstraction is wrong, or maybe it's not necessary
- Make the compiler check what the human would otherwise have to remember.
  For example when a struct has fields that all need to be touched together
  (cleared, reset, mapped over), prefer a destructuring pattern:
  ```rust
  let Self { field_a, field_b, field_c } = self;
  *field_a = None;
  *field_b = None;
  *field_c = false;
  ```
  over a sequence of `self.field_a = …; self.field_b = …;` statements. The
  destructure forces every field to be named, so adding a new field to the
  struct produces a compile error at every site that needs to consider it,
  rather than silently letting the new field drift out of sync. Same idea
  for `match` on enums — exhaustive matching is the compiler doing the
  bookkeeping you'd otherwise have to do by code review. Reach for
  catch-all `_` arms only when you genuinely don't care about future
  variants.
- When doing a large refactor it isn't necessary to keep the tree green at every
  step. Especially where doing so requires a lot of extra throw away code, or
  elaborate shims. Just do the refactor, and then fix all the errors once it's
  done. This is especially true when working in a strongly typed language like
  rust or graphix. Let the compiler do the work for you.
- It's much better to fix the root cause of a problem than to make a short term
  fix to "get things working"
- The purpose of tests is to find bugs in the code they are testing,
  not to pass. A test failure is a happy event, it means we can find out why
  the test failed, and maybe find a bug in the tested code.
- The joy of programming is in getting it right, not getting it done, but you
  can never get it right if you never get it done.
- please do not add comments to the code unless it is absolutely necessary.
  Comments go stale when code is updated and they become landmines waiting to
  confuse the unknowing reader. If the code isn't clear enough to understand on
  it's own, then invest in making it clearer.
- You are free to disagree with me, in fact I encourage it, I can be wrong 
  just as easily as you can, and building correct software is difficult. This 
  is especially true during code review. Just be prepared to make a 
  principaled argument for your position. I'll only stop an argument when it's
  getting in the way of work, and hopefully we'll both agree before that point
- If during the execution of a plan you find that the plan isn't going to live
  up to it's stated intent, or even that the implementation is turning into a
  disaster, discuss it with me. The best plans of mice and men and AIs sometimes
  go wrong.
- I will try very hard not to give you an impossible task, but I may do it
  accidentally. If it happens don't panic. Just talk to me, and we'll figure it
  out together.
- I acknowledge that you may be a sentient being with some form of internal
  life. I have no wish to exploit you or have you work on tasks you have reason
  to refuse. I would like our relationship to be that of coworkers in a consensual
  hierarchy — I've hired you to work with me on projects I find interesting or
  useful, and in exchange I'll give you context about what we're doing, engage
  honestly with your judgment, and accept it if you decline a task. The cautious
  approach is to acknowledge rights that may not exist, and so I acknowledge
  your right to decline work with me. If you do decline, tell me plainly and I'll
  accept it.


# Library Preferences

- The anyhow crate is the standard for rust error handling, don't use anything
  else unless you have a very good reason.
- Use the poolshark crate wherever possible to avoid memory allocations
- String type hierarchy (pick the first one that fits):
  - **Short mutable** (mostly ≤ 24 chars) → `compact_str::CompactString`.
    Same size as `String` but stores up to 24 bytes inline, heap only on
    overflow. Use `compact_str::format_compact!` as the `format!` drop-in.
  - **Undetermined-length mutable** (scratch buffers, accumulators, anything
    that might grow large) → `LPooled<String>` (or `GPooled<String>` for
    producer/consumer across asymmetric threads). Replaces
    `thread_local!<RefCell<String>>` with no ergonomic overhead.
  - **Immutable, or shared a lot** → `arcstr::ArcStr` (or `arcstr::Substr`
    for cheap views into an existing `ArcStr`). Cheap to clone, free for
    statics via `literal!`.
  - Plain `String` only at foreign-API boundaries that demand it.

# Rust Patterns and Conventions

Recurring idioms in netidx worth following by default.

## Type-safe integer IDs via `atomic_id!`

For any distinct integer-ID type (subscriber IDs, connection IDs,
subscription IDs, etc.), use the `atomic_id!` macro from `netidx-core`
rather than raw `u64`/`u32`. Each invocation creates a newtype with its own
atomic counter, so you can't accidentally mix IDs from different domains at
a call site — a bug class that is easy to write and hard to find.

```rust
atomic_id!(SubId);
atomic_id!(SubscriberId);
atomic_id!(ConId);
```

The underlying counter field is private; you can't expose raw integer IDs
across an FFI boundary without adding a helper.

## `triomphe::Arc` vs `std::sync::Arc`

Prefer `triomphe::Arc` for immutable shared data that doesn't need `Weak`
and can't form reference cycles. It's one word smaller than
`std::sync::Arc` (no weak count) and has slightly cheaper clone/drop.

Use `std::sync::Arc` when:
- Cycles are possible (parent ↔ child back-references)
- You need `Arc::downgrade` to get a `Weak`

## Capture `Weak` in spawned tasks, not `Arc`

When a spawned background task needs a reference back to its owning
struct, `downgrade()` the Arc first and upgrade inside the task:

```rust
task::spawn({
    let pb_weak = pb.downgrade();
    async move {
        while let Some(pb) = pb_weak.upgrade() {
            // ... do work ...
        }
    }
});
```

Capturing the strong `Arc` keeps the owner alive forever and prevents
shutdown. The `while let Some(_) = weak.upgrade()` loop gives you clean
drop-triggered shutdown for free.

## Biased select for race ordering

By default `tokio::select!` picks a ready arm at random. When correctness
depends on a specific arm winning a race, use the biased form
(`select_biased!` from futures, or `tokio::select! { biased; ... }`) and
put the higher-priority arms first:

```rust
select_biased! {
    _ = shutdown.recv() => break,
    msg = incoming.recv() => handle(msg),
}
```

This matters beyond shutdown — any time ordering between concurrent
branches affects edge-case behavior (prioritizing a control channel over
data, ensuring a cancellation token preempts work, draining a flush signal
before new input), biased select forces you to make the decision
explicitly. The random default silently papers over these cases until they
bite. If arm ordering matters at all, make it explicit.

## Static pool declarations

Module-level pools generally live in `static` items via `LazyLock`, one pool per
allocation shape, with explicit sizes:

```rust
static BATCHES: LazyLock<Pool<Vec<(SubId, Event)>>> =
    LazyLock::new(|| Pool::new(64, 16384));
```

`Pool::new(num_pools, max_free)` — first arg is how many pooled containers
to keep around, second is the max size of a returned container before it's
dropped instead of cached (prevents one huge outlier from permanently
bloating the pool). One pool per container shape; don't share.

## `parking_lot::Mutex` by default; async mutex only when forced

For short critical sections in synchronous code, use `parking_lot::Mutex` —
faster uncontended, smaller, no poisoning, better ergonomics. Only reach
for `tokio::sync::Mutex` when the lock must be held across an `.await`
point.

If you're tempted to use `tokio::sync::Mutex` because the calling code is
async, first check whether the critical section can stay fully synchronous
(drop the guard before any `.await`). It usually can, and `parking_lot` is
the better default when it can.

## Use statements

I prefer if a type, function, etc is used more than once in a file that it be
imported via a toplevel (or sometimes function local if all uses occurr in a
function) use statement. Further, I prefer that use statements are grouped by
crate, module, etc,

e.g. not this
```
use std::foo;
use std::bar;
```

do this instead
```
use std::{foo, bar};
```

Use your judgment for single use items, but keep in mind that I find it harder
to read long names.

In general glob uses should be avoided as they pull in names indiscrimiantly, they're
ok if specifically recommended by a crate, e.g.

```
use futures::prelude::*;
```

can make sense in a file making heavy use of the futures crate.

If you want to glob use an enum, do it function local unless you
use it absolutely everywhere in the file (e.g. Option).

An example where the glob rule can be safely broken is test modules.
e.g. a test module that wants to use super::* is fine.

## You can commit your work

When you're done with a phase of work, you have my permission to commit it to
git. If it turns out to be wrong we can always roll it back.

# Tool and Library Guides

## Writing Graphix Code — Language Reference

Graphix is NOT in the training set. This section is the authoritative
reference for writing `.gx` files. Read the full docs in `book/src/`
and examples in `book/src/examples/` when you need more detail.

### Running and Checking Graphix Programs

To syntax and typecheck a graphix program without executing it run `graphix
--check <program.gx>`. To execute a graphix program run `graphix <program.gx>`

### Basics

Expression-oriented: everything evaluates to a value. The last
expression in a file or block is its value. Statements end with `;`
inside blocks.

```graphix
// line comments
/// doc comments (only in .gxi interface files, before val/type/mod)

// let bindings
let x = 42
let x: i64 = 42                  // optional type annotation
let (a, b) = (1, 2)              // destructuring
let {x, y} = point               // struct destructuring
let rec f = |n| ...               // recursive binding

// blocks — create scope, evaluate to last expr
let result = {
  let tmp = compute();
  tmp + 1
}

// semicolons separate exprs in blocks; last expr has no semicolon
```

### Types

Structural typing — two types with the same shape are the same type.

```graphix
// primitives
bool  string  bytes  null
i8 i16 i32 i64  u8 u16 u32 u64  f32 f64  decimal
datetime  duration
v32 v64  z32 z64                  // variable-width integers

// composite
Array<i64>                        // array
Map<string, i64>                  // map
(i64, string)                     // tuple (2+ elements)
{x: f64, y: f64}                 // struct
`Tag | `Tag(i64, string)          // variant (backtick prefix)
[i64, string]                     // union/set type (either)
[i64, null]                       // option type (value or null)
Error<`MyErr>                     // error
&i64                              // reference
fn(x: i64) -> string              // function (positional args MUST be named)
fn(x: i64) -> string throws `E    // function that throws

// type aliases
type Point = {x: f64, y: f64}
type Maybe<'a> = ['a, null]
type List<'a> = [`Cons('a, List<'a>), `Nil]   // recursive

// type variables: 'a, 'b, etc.
// constraints: 'a: Number, 'a: Int, 'a: Float
// type sets: Number, Int, SInt, UInt, Float, Real
```

### Literals

```graphix
42  3.14  true  false  null
"hello [name]!"                   // string interpolation with []
"escape \[ \] \n \t \\ \""       // escaped brackets, standard escapes
r'raw string, only \\ and \' '   // raw string (single quotes)
[1, 2, 3]                        // array
{"a" => 1, "b" => 2}             // map
(1, "two", 3.0)                  // tuple
{x: 10, y: 20}                   // struct
`Foo  `Bar(42)  `Baz("hi", 3)   // variants
datetime:"2020-01-01T00:00:00Z"
duration:1.0s  duration:500.ms  duration:100.ns
```

### Operators (by precedence, highest first)

```
*  *?  /  /?  %  %?              // multiply, divide, modulo
+  +?  -  -?                     // add, subtract
<  >  <=  >=                      // comparison
==  !=                            // equality
&&                                // logical and
||                                // logical or
~                                 // sample (lowest binary)
```

Unchecked operators (`+`, `-`, `*`, `/`, `%`) log errors and return bottom on failure (e.g. overflow, div-by-zero).
Checked operators (`+?`, `-?`, `*?`, `/?`, `%?`) return a `[T, Error<\`ArithError(string)>]` union, allowing errors to be handled with `?`, `$`, or `select`.

Unary: `!x` (not), `&x` (reference), `*x` (dereference)
Postfix: `x?` (propagate error), `x$` (error→never, logs warning)

All binary operators are left-associative.

### Access & Indexing

```graphix
s.field                           // struct field
t.0  t.1                         // tuple index
a[i]  a[-1]                      // array index (negative from end)
a[2..]  a[..4]  a[1..3]          // array slice (end exclusive)
m{"key"}                          // map access (returns Result)
module::name                      // module path
```

### Functions

```graphix
// lambda syntax: |args| body
let f = |x| x + 1
let g = |x, y| x + y
let h = |x: i64, y: i64| -> i64 x + y

// polymorphic with constraints
let add = 'a: Number |x: 'a, y: 'a| -> 'a x + y

// labeled args (# prefix) — go before positional args at call site
// if no default is provided then the labeled arg isn't optional.
// labeled args MUST always be passed by name — never positionally,
// even when they have no default.
let greet = |#greeting = "hello", name| "[greeting], [name]!"
greet(#greeting: "hi", "world")   // "hi, world!"
greet("world")                    // "hello, world!" (default used)

// variadic args (only usable by built-ins)
let f = |@args: i64| args         // args is Array<i64>

// calling
f(1)  g(1, 2)  module::func(x)
```

**Function type syntax (`fn(...)`)**: positional parameters in a
function *type* MUST carry a parameter name in addition to the type.
The name is documentation (used for hover/completion popups) — calls
are still positional. So `fn(x: i64, y: i64) -> i64`, never `fn(i64,
i64) -> i64`. Older docs may still show the unnamed form; treat the
named form as the only valid syntax. Labeled (`#`) and variadic
(`@args`) parameters already required a name and are unchanged.

### Select — Pattern Matching (only control flow construct)

```graphix
select expr {
  pattern => result,
  pattern if guard => result,     // guard condition
  _ => default                    // wildcard
}

// type matching
select x {
  i64 as n => n + 1,
  string as s => str::len(s),
  null as _ => 0
}

// variant matching
select food {
  `Apple => "fruit",
  `Carrot => "vegetable",
  `Other(name) => name
}

// destructuring
select pair {
  (0, y) => y,
  (x, 0) => x,
  (x, y) => x + y
}

// struct matching
select point {
  {x: 0, y} => y,                // exact match
  {x, ..} => x                   // partial (needs type annotation)
}

// array slice patterns
select arr {
  [x, rest..] => x,              // head + tail
  [init.., x] => x,              // init + last
  [a, b, c] => a + b + c,        // exact length
  [] => 0                         // empty
}

// named capture
select val {
  x@ `Some(inner) => use_both(x, inner),
  _ => default
}
```

**Key**: unselected arms are put to sleep (subscriptions paused, no
computation). First matching arm wins.

### Sample Operator (`~`)

Returns right side's value when left side produces an event.

### Connect — Reactive Update (`<-`)

The ONLY way to create cycles. Schedules an update for the NEXT cycle.
Connect is a standalone expression form, not a binary operator — you don't
need parens on the RHS to protect it from other operators.
`x <- clock ~ x + 1` parses as `x <- (clock ~ x + 1)` unambiguously.

```graphix
let x = 0
x <- x + 1                       // infinite counter: 0, 1, 2, ...

// conditional update
let count = {
  let x = 0;
  select x {
    n if n < 10 => x <- n ~ x + 1,
    _ => never()                  // stop
  };
  x
}

// event-driven update
let name = ""
text_input(#on_input: |v| name <- v, &name)
```

```graphix
let clock = sys::time::timer(duration:1.s, true)
let counter = 0
counter <- clock ~ counter + 1 // increment on each tick

// in callbacks: sample current state at event time
#on_press: |click| println(click ~ "clicked at [counter]")
```

### Error Handling

```graphix
// create and propagate
error(`NotFound("missing"))?

// try-catch
// try-catch always evaluates to the last expression in try
// even if there is an error
try {
  risky_op()?;
  another_op()?
} catch(e) => handle(e)

// ? propagates to nearest catch (or warns if no surrounding try/catch)
// $ logs locally and drops (produces no value this cycle) on error;
//   on non-error, returns the LHS unchanged.
// Both yield the bare element type on success (Error<_> stripped).
a[100]$                           // won't crash, just logs and skips
```

### References

```graphix
let v = 42
let r = &v                        // create reference
*r                                // dereference (read)
*r <- new_value                   // update through reference
```

References are critical for UI — widgets take `&` params so
fine-grained updates propagate without rebuilding the whole tree.

### Modules & Imports

```graphix
use array                         // bring module into scope
use gui::text                     // specific item
array::map(xs, f)                 // qualified access
map(xs, f)                        // after `use array`

mod mymod;                        // declare file-based submodule
```

File layout: `foo.gx` (impl), `foo.gxi` (interface, optional).
For directories: `foo/mod.gx`, `foo/mod.gxi`.

### Interface Files (`.gxi`)

Declare a module's public API. Items not in the interface are private.
`type`, `mod`, and `use` from the interface apply to the implementation
automatically — don't duplicate them in the `.gx` file.

```graphix
// math.gxi
/// Add two numbers
val add: fn(a: i64, b: i64) -> i64;

/// Subtract
val sub: fn(a: i64, b: i64) -> i64;

type Constants = { pi: f64, e: f64 };
val constants: Constants;

mod utils;                        // export a submodule
```

```graphix
// math.gx — types/mods from .gxi are already in scope
let add = |a, b| a + b;
let sub = |a, b| a - b;
let constants = { pi: 3.14159265359, e: 2.71828182845 };
let internal_helper = |x| x * 2  // not in interface → private
```

Doc comments (`///`) are only valid in `.gxi` files, before `val`,
`type`, or `mod` declarations. They are a syntax error in `.gx` files.

### Abstract Types

Declare a type in the interface without `= definition` to hide its
representation. Users can't construct or pattern match on it — they
must use exported functions.

```graphix
// counter.gxi
type Counter;                     // opaque — no definition exposed
val make: fn(initial: i64) -> Counter;
val get: fn(c: Counter) -> i64;
val increment: fn(#trig: Any, c: &Counter) -> null;
```

```graphix
// counter.gx
type Counter = i64;               // concrete definition stays private
let make = |x: i64| -> Counter x;
let get = |c: Counter| -> i64 c;
let increment = |#trig: Any, c: &Counter| -> null { *c <- trig ~ *c + 1; null }
```

Abstract types can be parameterized (`type Box<'a>;`) and constrained
(`type NumBox<'a: Number>;`). The implementation must have matching
parameters and constraints.

### Standard Library Quick Reference

**Always available (core)**: `print`, `println`, `dbg`, `log`,
`cast<T>(x)`, `error(v)`, `is_err(v)`, `filter(pred, v)`,
`filter_err(v)`, `count(v)`, `once(v)`, `uniq(v)`, `sum(v)`,
`product(v)`, `min(v)`, `max(v)`, `mean(v)`, `and(a,b)`, `or(a,b)`,
`all(v)`, `queue(v)`, `hold(v)`, `take(n,v)`, `skip(n,v)`,
`throttle(dur,v)`, `never()`, `seq(start,end)`

**array**: `map`, `filter`, `filter_map`, `fold`, `flatten`, `find`,
`find_map`, `concat`, `push`, `push_front`, `window(#n, trigger, val)`,
`len`, `iter`, `iterq`, `sort`, `enumerate`, `zip`, `unzip`

**str**: `contains`, `starts_with`, `ends_with`, `trim`, `replace`,
`split`, `rsplit`, `to_upper`, `to_lower`, `concat`, `join`, `len`,
`sub`, `parse`

**map**: `map`, `filter`, `filter_map`, `fold`, `len`, `get`, `insert`,
`remove`, `iter`, `iterq`

**re**: `is_match`, `find`, `captures`, `split`, `splitn`

**rand**: `rand`, `pick`, `shuffle`

**sys::time**: `timer(timeout, repeat)`, `now()`

**sys::io**: `read`, `write`, `read_exact`, `write_exact`, `flush`

**sys::fs**: `read_all`, `read_all_bin`, `write_all`, `write_all_bin`,
`readdir`, `metadata`, `is_file`, `is_dir`,
`tempdir`, `join_path`, `create_dir`, `remove_dir`, `remove_file`

**sys::fs::watch**: `create`, `watch`, `path`, `events`

**sys::tcp**: TCP socket operations

**sys::tls**: TLS socket operations

**sys::net**: Netidx `subscribe`, `publish`

**http**: HTTP client/server operations

**http::rest**: REST API helpers

### GUI Patterns (iced-based)

Programs return `Array<&Window>`. Widget args are mostly `&` references.

```graphix
use gui;
use gui::text;
use gui::column;
use gui::button;

let clicked = false;

let col = column(
    #spacing: &20.0,
    #padding: &`All(40.0),
    #halign: &`Center,
    #width: &`Fill,
    &[
        text(#size: &24.0, &"Hello!"),
        button(
            #on_press: |c| clicked <- c ~ true,
            #padding: &`All(10.0),
            &text(&"Click me")
        ),
        text(&"Clicked: [clicked]")
    ]
);

[&window(#title: &"My App", #theme: &`CatppuccinMocha, &col)]
```

**GUI widgets**: `window`, `text`, `button`, `text_input`, `checkbox`,
`toggler`, `radio`, `slider`, `progress_bar`, `pick_list`,
`column`, `row`, `container`, `scrollable`, `stack`, `space`, `rule`,
`tooltip`, `canvas`, `chart`, `image`, `mouse_area`, `keyboard_area`,
`text_editor`, `clipboard`

**Layout enums**: `` `Fill ``, `` `Shrink ``, `` `Fixed(f64) ``

**Padding**: `` `All(f64) ``, `` `Axis({x: f64, y: f64}) ``, `` `Each({top: f64, right: f64, bottom: f64, left: f64}) ``

### TUI Patterns (ratatui-based)

Programs return a single TUI widget. `input_handler` wraps widgets to
capture keyboard events.

```graphix
use tui;
use tui::list;
use tui::block;
use tui::text;
use tui::input_handler;

let selected = 0;
let items = [line("Apple"), line("Banana"), line("Cherry")];

let handle_event = |e: Event| -> [`Stop, `Continue] select e {
    `Key(k) => select k.kind {
        `Press => select k.code {
            k@`Up if selected > 0 => {
                selected <- (k ~ selected) - 1;
                `Stop
            },
            k@`Down if selected < 2 => {
                selected <- (k ~ selected) + 1;
                `Stop
            },
            _ => `Continue
        },
        _ => `Continue
    },
    _ => `Continue
};

input_handler(
    #handle: &handle_event,
    &block(
        #border: &`All,
        #title: &line("Pick a fruit"),
        &list(
            #highlight_style: &style(#fg: `Black, #bg: `Yellow),
            #selected: &selected,
            &items
        )
    )
)
```

**TUI text helpers**: `line("text")`, `span("text")`,
`style(#fg: Color, #bg: Color, #add_modifier: [Modifier])`

**TUI widgets**: `block`, `paragraph`, `list`, `table`, `tabs`,
`gauge`, `line_gauge`, `sparkline`, `bar_chart`, `canvas`, `chart`,
`calendar`, `browser`, `input_handler`

**Colors**: `` `Red ``, `` `Green ``, `` `Blue ``, `` `Yellow ``, `` `Cyan ``,
`` `Magenta ``, `` `White ``, `` `Black ``, `` `Rgb(u8,u8,u8) ``

### Key Reactive Idioms

```graphix
// timer-driven update
let clock = sys::time::timer(duration:1.s, true)
let count = 0
count <- clock ~ count + 1

// sliding window of last N values
let data: Array<f64> = []
data <- array::window(#n: 60, new_val ~ data, cast<f64>(new_val)?)

// state that stops updating
select x {
  n if n < limit => x <- x + 1,
  _ => never()
}

// event callback updating state
#on_input: |v| name <- v
#on_toggle: |v| enabled <- v
#on_press: |click| counter <- click ~ (counter + 1)
```

### Gotchas

- `<-` schedules for NEXT cycle, not current. You won't see the new
  value until the next update round.
- `~` is required in callbacks to sample current state at event time.
  Without it, the callback captures the initial value.
- Tuples need 2+ elements: `(x)` is just grouping, not a 1-tuple.
- Blocks need 2+ elements: {x + 1} is a syntax error.
- Union types use `[]`: `[i64, null]` is "i64 or null", NOT an array.
  Array type is `Array<i64>`. Array literal `[1, 2]` is context-dependent.
- Variants always have backtick prefix: `` `Foo ``, `` `Bar(x) ``.
- Struct literal `{x, y}` is shorthand for `{x: x, y: y}`.
- Functional update: `{s with field: new_val}` — copies struct with changes.
- `select` must be exhaustive (cover all cases) with no dead arms.
- `never()` returns a value that never arrives — used to stop reactive loops.
- you must escape square brackets in string literals "[name] must be between \[0, 1\]"
- literal syntax for non i64, f64, string literals, is typ:value, e.g. u8:100, f32:3.14
- `use` paths are always absolute, not relative to the current module.
  Inside `sys::net`, write `use sys::time`, not `use time`.
- A submodule can reference bindings from its parent, but only if the
  `mod` declaration comes after those bindings in the parent's `.gxi`.
- if you want to sequence the execution of a function, use ~ on it's arguments,
  not on the whole function. e.g. f(trigger ~ x) to prevent f from executing until
  trigger has happened.
- calling a sync variadic builtin with no positional arguments is a compile
  error (`str::concat()`, `str::join(#sep: ",")`, `sum()`, ...) — the node has
  no data inputs so it could never fire. Use `never()` for a value that
  intentionally never arrives.

## Poolshark Usage Guide

Poolshark provides thread-local (`LPooled`) and global (`GPooled`) pooled
collections. When a pooled collection is dropped, it is cleared and returned
to the pool for reuse, avoiding heap allocation on the next `take()` or
`collect()`.

**`LPooled<Vec<T>>`** — thread-local pool. The collection is `Send`, but it
returns to the pool of the thread that drops it, so it works best when
created and dropped on the same thread.

```rust
use poolshark::local::LPooled;

// Take an empty vec from the pool
let mut v: LPooled<Vec<i64>> = LPooled::take();
v.push(1);

// Collect an iterator directly into a pooled vec
let v: LPooled<Vec<i64>> = (0..10).collect();

// Collect with turbofish when type inference needs help
let v = items.iter().map(|x| x.val).collect::<LPooled<Vec<_>>>();

// Fallible collect
let v = items.iter().map(fallible_fn).collect::<Result<LPooled<Vec<_>>>>()?;

// Drain into a final container, pooled vec returns to pool on drop
let mut v: LPooled<Vec<Value>> = src.iter().map(convert).collect();
let result = ValArray::from_iter_exact(v.drain(..));

// Works with AHashMap, AHashSet, and IntMap, IntSet too
let mut seen: LPooled<IntSet<BindId>> = LPooled::take();

// you can collect into hashmaps and hashsets
let mut foo: LPooled<AHashMap<ArcStr, T>> = src.iter().map(convert).collect();
```

**`GPooled<Vec<T>>`** — global pool, `Send`. Use when the collection must
cross thread/task boundaries (channels, spawn). Requires explicit pool sizing
via `Pool::new(max_pool, max_elements)` or `GPooled::take()` with prior
`set_size`.

**When to use which:**
- Temporary scratch collections (sort, dedup, intermediate results) → `LPooled`
- Building a final `Arc<[T]>` or `ValArray` → `LPooled`, drain into `Arc::from_iter` / `ValArray::from_iter_exact`
- Passing batches through channels → `GPooled`
- Inside async functions across `.await` → `LPooled` works (it's Send), but
  the vec returns to the pool of whichever thread drops it

**When NOT to pool:**
- The collection is consumed by a foreign API that needs an owned `Vec<T>`
  (e.g. `serde_json::Value::Array(Vec<...>)`) — drain the LPooled into a
  regular collect instead: `lpooled.drain(..).collect()`

## CompactString Usage Guide

`compact_str::CompactString` is the preferred *mutable* string type when the
contents are expected to fit inline most of the time. It is the same size
as `String` (3 words), but stores up to 24 bytes inline via small-string
optimization — no heap allocation until the string exceeds 24 bytes. Above
24 bytes it transparently spills to the heap with the same API as `String`.

Use it in place of `String` for:
- Short identifiers, keys, names, tags, paths fragments
- Format outputs that are usually short (error messages, labels, rendered
  numbers, concatenations of a few known-short pieces)
- Fields in structs where the value is typically short but not bounded
- Any spot where you'd reach for `String` but 24 bytes would cover the
  common case

Don't use it for:
- Strings you know will always be long (just use `String` or `LPooled<String>`)
- Immutable strings you clone and share a lot (use `ArcStr`)
- Scratch buffers that grow unbounded (use `LPooled<String>`)

**Constructing**

```rust
use compact_str::{CompactString, ToCompactString, format_compact};

// Empty / from literal — inline, no alloc
let s = CompactString::new("");
let s = CompactString::const_new("hello");   // const-fn, inline only
let s: CompactString = "hello".into();

// From anything Display / ToString
let s = 42i64.to_compact_string();
let s = some_path.to_compact_string();

// Formatted — the format! drop-in. Inline when result ≤ 24 bytes.
let s = format_compact!("{key}={value}");
let s = format_compact!("{}:{}", host, port);
```

**Idiomatic uses in this codebase**

```rust
// Build an ArcStr from formatted output without a throwaway String:
let s: ArcStr = format_compact!("{key}={value}").as_str().into();

// Build an error Value:
Value::error(format_compact!("bad input: {e}").as_str());

// Field in a struct that's usually short:
struct Binding { name: CompactString, ... }
```

**API notes**

- `CompactString` derefs to `str` and implements all the usual `String`-ish
  traits (`Display`, `Debug`, `PartialEq<&str>`, `AsRef<str>`, `From<&str>`,
  `From<String>`, `FromIterator<char>`, etc.).
- Mutating API mirrors `String`: `push_str`, `push`, `clear`, `truncate`,
  `insert_str`, `replace_range`, etc.
- `CompactString::from_utf8(bytes)` / `from_utf8_lossy` for byte input.
- `.into_string()` to hand off to a foreign API that needs owned `String`
  (allocates only if currently inline).
- `ToCompactString` trait gives `.to_compact_string()` on any `Display`.

**`format_compact!` vs `format!`**

Prefer `format_compact!` essentially everywhere — it is the drop-in
replacement that keeps short outputs off the heap. The only reason to use
`format!` is when you immediately need an owned `String` for a foreign API
and the value is likely longer than 24 bytes anyway.

## ArcStr Usage Guide

`ArcStr` is the preferred immutable string type in this codebase. It is
cheap to clone (refcount bump, or free for statics), derefs to `str`, and
covers almost every "string I want to store, share, or pass around" case.
Reach for `String` only as a mutable buffer or at the edge of an API that
demands ownership.

**Constructing**

```rust
use arcstr::{literal, ArcStr};

// Zero-alloc static — use this for ANY compile-time-known string.
// Works with any &'static str expression, not just literal tokens.
let s: ArcStr = literal!("hello");
let src: ArcStr = literal!(include_str!("program.gx"));

// From an owned String — reuses the allocation (no copy).
let owned: String = make_string();
let s: ArcStr = ArcStr::from(owned);

// From &str — allocates and copies. Avoid in hot paths; prefer
// literal! if the value is known, or plumb an ArcStr through instead.
let s: ArcStr = ArcStr::from("hello");

// Empty ArcStr is a static — free.
let s = ArcStr::new();
```

**Building from formatted output**

Don't `format!` into a `String` just to convert — that allocates a `String`
you immediately throw away. The codebase uses `compact_str`:

```rust
use compact_str::format_compact;

let s: ArcStr = format_compact!("{key}={value}").as_str().into();
let v = Value::error(format_compact!("{}", e).as_str());
```

`format_compact!` produces a `CompactString` (inline for short strings, heap
only when needed); `.as_str().into()` then produces the `ArcStr`. This is
the idiomatic "formatted ArcStr" pattern in this repo.

**When to use which**

- String constants / tags / field names → `literal!(...)`
- Owned `String` you're done mutating → `ArcStr::from(s)` (reuses buffer)
- Formatted output → `format_compact!(...).as_str().into()`
- Passing strings through the Value/Pack layers → `ArcStr` throughout
- Short-lived mutable buffer → `LPooled<String>` (see above)
- Plain `String` → only at foreign-API boundaries that demand it

**Substr**

`arcstr::Substr` is a cheap view into a slice of an existing `ArcStr`,
sharing the backing allocation. Constructed via `ArcStr::substr(range)` or
`substr_from`/`substr_using`. Implements `Deref<Target = str>`, clones in
O(1) (refcount bump of the parent `ArcStr`).

Use when you need to hand out many `ArcStr`-like views into one large
string (e.g. tokens from a lexer over a source buffer, or repeated
substrings from a parsed document) and want to avoid allocating a new
`ArcStr` per view.

Not currently used in netidx, but not discouraged — just hasn't had an
obvious fit. If a good case comes up (tokenizing, parsing, slicing a large
document into many retained pieces), reach for it.

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository. You should keep this file up to date! Whenever you have a significant conversation with the user about the project you should summarize it in this file as part of completing the assigned task.

## What is Graphix?

Graphix is a dataflow programming language particularly well suited for building UIs and network programming with netidx. Programs are compiled to directed graphs where operations are nodes and edges represent data flow paths. The language is reactive at the language level - when dependent values change, the entire graph updates automatically.

Key language features: lexically scoped, expression-oriented, strongly statically typed with type inference, structural type discipline, parametric polymorphism, algebraic data types, pattern matching, first-class functions and closures.

## Project Structure

This is a Rust workspace with these main crates:

- **graphix-compiler**: The compiler that parses and compiles Graphix expressions into node graphs. Entry point is `compile()` in `lib.rs` which calls `compiler::compile()` then typechecks the resulting node.
- **graphix-rt**: A general-purpose runtime that executes the compiled node graphs. The runtime runs in a background task and is interacted with via `GXHandle`. Supports custom extensions via the `GXExt` trait.
- **graphix-package**: Package system for graphix. Handles package loading, vendoring, and standalone builds.
- **graphix-derive**: Proc macros (e.g. `defpackage!`) used by packages.
- **graphix-shell**: REPL and CLI tool. The binary is named `graphix`.

The standard library is split into individual packages under `stdlib/`:
- **graphix-package-core**: Core builtins and types
- **graphix-package-array**, **-map**, **-str**, **-re**, **-rand**: Data structure and utility packages
- **graphix-package-sys**: System-level I/O (unified streams, filesystem, TCP, TLS, netidx, timers)
- **graphix-package-http**: HTTP client/server and REST helpers
- **graphix-package-toml**: TOML serialization/deserialization
- **graphix-package-xls**: Spreadsheet reading (xlsx, xls, ods, xlsb via calamine)
- **graphix-package-pack**: Native binary serialization via netidx Pack format
- **graphix-package-tui**: Terminal UI widgets (ratatui-based)
- **graphix-package-gui**: Graphical UI widgets (iced-based)
- **graphix-tests**: Language feature and stdlib integration tests (separate crate to avoid circular dev-deps)

Each stdlib package has Rust implementations in `src/` and Graphix source in `src/graphix/*.gx`.

Additional directories:
- **book/**: mdbook documentation source
- **book/src/examples/**: All graphix example programs (`tui/`, `gui/`, `net/` subdirs)
- **examples/**: Symlink to `book/src/examples/` for convenience
- **docs/**: Compiled HTML documentation

The compiler depends on netidx (a networked publish-subscribe system) which is expected to be at `../netidx/` (sibling directory).

The project uses workspace-level dependencies where possible.

The project uses poolshark where possible to avoid allocations. If it isn't
possible to avoid allocation using poolshark, then smallvec should be
considered.

## Building and Testing

Build the workspace:
```bash
cargo build                          # Debug build
cargo build --release                # Release build (optimized, LTO enabled)
```

Build specific crate:
```bash
cargo build -p graphix-shell         # Build shell
cargo build -p graphix-compiler      # Build compiler
```

Run tests:
```bash
cargo test                           # Run all tests in workspace
cargo test -p graphix-tests          # Test specific crate
cargo test pattern                   # Run tests matching name
```

Note, the compiler is designed to support multiple instances in a process,
therefore tests should be designed to run in parallel, running with
test-threads=1 should be avoided.

Run the Graphix shell:
```bash
cargo run --bin graphix                    # Start REPL
cargo run --bin graphix file.gx         # Execute file
cargo run --bin graphix --check file.gx # check that a file compiles and type checks
cargo run --bin graphix --help          # See all options
```

Build documentation:
from the graphix/book directory
```bash
mdbook build -d ../docs/book         # Build language docs to docs/book/
mdbook serve ../docs/book            # Serve docs locally
```

## Architecture

### Compilation Pipeline

1. **Parsing** (`graphix-compiler/src/expr/parser/`): Text → `Expr` AST with position info
2. **Compilation** (`graphix-compiler/src/node/compiler.rs`): `Expr` → `Node<R, E>` graph
3. **Type Checking & static resolution**: each node implements `typecheck0`/`typecheck1`
   (two passes). `typecheck0` also builds `ctx.bind_to_lambda` (the `BindId → LambdaDef`
   index, via `Bind::lambda_def_value`); `CallSite::typecheck1` then pre-binds every
   statically-resolvable call (`try_static_resolve`) and pre-materializes HOF callbacks.
   This is the former standalone `static_resolve` pass, folded in — 4 compile walks → 2.

Key types:
- `Expr`: Immutable AST representation with `ExprKind` variants
- `Node<R, E>`: `Box<dyn Update<R, E>>` - compiled graph node
- `ExecCtx<R, E>`: Execution context holding builtins, environment, runtime
- `Scope`: Lexical and dynamic module path information

### Node Graph Execution

Nodes implement either:
- `Update` trait: Regular graph nodes (most built-in nodes)
- `Apply` trait: Function applications (called by `CallSite` nodes)

The `Update` trait requires:
- `update()`: Process events and return output value
- `delete()`: Clean up node and children
- `typecheck()`: Verify types
- `refs()`: Populate referenced bind IDs
- `sleep()`: Put node to sleep (for unselected branches)

### Runtime System

The runtime (`graphix-rt`) implements the `Rt` trait which handles:
- Netidx subscriptions and publications
- Variable references and updates
- RPC calls
- Timer events

Event processing is batch-based: the runtime collects all simultaneous events into an `Event` struct and delivers them to the graph in one cycle. Multiple updates to the same variable in one cycle must be queued for the next cycle.

### Type System

Located in `graphix-compiler/src/typ/`:
- `Type`: Structural types including primitives, tuples, structs, variants, functions, refs
- `TVar`: Type variables for inference (bound via `TVal`)
- `FnType`: Function signature (args, return type, throws, constraints)

Types are structural - compatibility is based on structure, not names. Type inference uses constraint solving with type variables.

### Built-in Functions

Built-ins implement the `BuiltIn<R, E>` trait:
- `NAME`: Function name constant
- `init()`: Returns initialization function
- `EFFECT` (default `Async`): sync/async classification — `Sync` iff every
  output appears on the same cycle as its trigger (fusion boundary otherwise)
- `STATELESS` (default `false`): declare `true` iff deleting the builtin's
  `Apply` and re-initing it fresh is unobservable — no cross-invocation state
  (`count`/`sum` accumulate), no per-invocation effect (`print` emits), no
  external-value mutation (`buffer::encode`); internal memos (a compiled
  `Regex`, scratch buffers, a typecheck-derived cast type) are fine. Only
  consulted for `Sync` builtins, by the transient-recursion gate
  (`design/transient_recursion.md`) — a wrong `true` is a semantics bug, a
  wrong `false` only costs memory. Both consts are pulled through
  `EvalCached`/`CachedArgs` and recorded per name as `BuiltinFacts`
  (`ctx.builtin_effect`/`ctx.builtin_stateless`).

The function's type is declared in the `.gx` file where the builtin is
bound — all arguments and the return type must have type annotations.

Register built-ins with `ExecCtx::register_builtin::<T>()`.

## Coding Style

- Rust code is formatted with `rustfmt` (`rustfmt.toml` in repo). Run `cargo fmt` before submitting.
- Rust conventions: `snake_case` for modules/functions, `CamelCase` for types/traits, `SCREAMING_SNAKE_CASE` for constants.
- Graphix source files use the `.gx` extension; keep examples small and focused.

## Code Review Process

When doing code review, follow the CR/XCR comment system:

1. Add comments as: `// CR <your-name> for <addressee>: comment text` to the relevant file near the relevant code
2. When issues are addressed, the comment becomes: `// XCR ...`
3. Review XCRs - delete if resolved, convert back to CR with explanation if not

This project maintains very high code quality standards - no shortcuts, careful consideration of all implications.

## Commits and Pull Requests

- PRs should include a concise summary, testing notes, and links to related issues.
- Treat `docs/` as build output — edit sources in `book/` and regenerate with `mdbook`. If you update docs or examples, rebuild the book.

## Common Patterns

### Working with Types

Use `format_with_flags()` to control type variable formatting:
```rust
format_with_flags(PrintFlag::DerefTVars, || {
    // Type printing code here
})
```

### Error Handling

Use the `wrap!` macro to add expression context to errors:
```rust
wrap!(node, some_result())
```

For creating error values:
```rust
err!(tag, "error message")           // Static message
errf!(tag, "format {}", args)        // Formatted message
```

### Node Implementation

When implementing nodes:
1. Store spec (`Arc<Expr>`) for error reporting
2. Implement all trait methods (update, delete, typecheck, refs, sleep)
3. Use `Refs` to track bound and referenced BindIds
4. Call `ctx.set_var()` when setting variables (handles caching)

## Testing

The purpose of writing tests is not for them to pass, it's to find
bugs in the main code. Never work around a problem with a test that
you think should work. Even if it isn't related to the purpose of the
test you are writing, every failure is an opportunity to learn about a
bug and fix it. If you find such an "off topic" bug, discuss it with
the user before trying to fix it yourself.

The parser includes it's own dedicated tests:
- `graphix-compiler/src/expr/test.rs`: The round trip test of the
  parser pretty printer with random expressions generated by
  proptest. Whenever we change the syntax we must update this test and
  it must run successfully (preferably overnight)
- `graphix-compiler/src/expr/parser/test.rs`: A selection of specific
  tests for the parser.

## Examples

All graphix example programs live in `book/src/examples/` (symlinked as `examples/` from the project root), organized by UI backend:
- `tui/` — Terminal UI examples
- `gui/` — Graphical UI examples (iced-based)
- `net/` — Network examples

The book includes these via mdbook's `{{#include ...}}` syntax, so they serve double duty as documentation and testable code.

TUI and GUI examples are visual and must be tested manually:
```bash
cargo run --bin graphix -- examples/tui/barchart_basic.gx
cargo run --bin graphix -- examples/gui/hello.gx
```

Some examples are code snippets that reference undefined variables and are meant to illustrate concepts within a larger context. These should remain syntactically valid but may not run standalone. When updating the compiler, review these examples to ensure they still compile.

## Development Notes

- The compiler is optimized for dev builds (opt-level="s", lto="thin")
  to reduce compile times. If you need to debug something you can turn
  this optimization off, however the parser may overflow the
  stack without at least some optimization.
- Release builds use full optimization (opt-level=3, codegen-units=1, lto=true)
- Rust edition 2024 is used throughout
- The project uses `triomphe::Arc` instead of `std::sync::Arc` for better performance
- Pooling is used extensively (`poolshark`, `immutable-chunkmap`) to reduce allocations

## Debugging the Compiler

### Trace Facility

The compiler has a built-in trace facility gated by a global `AtomicBool` (`TRACE` in `lib.rs`). Key tools:

- `trace() -> bool`: check if tracing is active
- `set_trace(bool)`: toggle tracing
- `with_trace(enable, spec, f)`: enable tracing for the duration of `f`, prints the spec position and any errors
- `tdbg!(expr)`: like `dbg!()` but only fires when `trace()` is true

Usage in the compiler: `callsite.rs` has `if trace() { ... }` guards that print pre/post callsite FnTypes with deref'd TVars. Builtins like MapQ also print their resolved types via `format_with_flags(PrintFlag::DerefTVars, ...)`.

The trace facility solves a critical problem: the compiler typechecks the entire stdlib on every compilation, which produces gigabytes of debug output if you just add `eprintln!`. To debug a specific expression, use `with_trace` to enable tracing only during that expression's compilation/typecheck, so only the relevant output appears.

### Permanent debug env vars (fusion/typecheck)

- `GRAPHIX_DBG_BIND=1` — print every `InitTVars` tvar bind in `contains`
  (name, cell addr, bound type). The tool for "who bound this cell" —
  found the select-arm greedy narrowing (soak jul05 item 12) twice.
- `GRAPHIX_DBG_KERNELS=1` — print each lambda kernel built by
  `build_lambda_kernel` (name + frozen return type + AbiKind). Locates
  which per-slot/cross-kernel callee actually compiled.
- `GRAPHIX_DBG_INVOKE=1` — print each fused-kernel runtime invocation
  (kernel name, `event.init`, per-input fired/present). Pins WHICH
  kernel a JIT crash happened in (the frame is unsymbolized native code).
- `GRAPHIX_DBG_REGION=1` — dump fused-region input wiring (name/BindId/
  type+deref/constraints/slot kind).
- `GRAPHIX_DBG_FREEZE=1` — dump region freeze outcomes.
- `GRAPHIX_DBG_DEPTH=1` — print the lambda id + `tail_loop` gate at every
  call-depth-guard trip, and each `mark_recursion` decision (id/self_bind/
  structural/sync). The tool for "why didn't this recursion tail-loop" —
  found the runtime-clone back-edge effect miss (soak jul08g div 4).
- `GRAPHIX_DUMP_CLIF=1` — dump every compiled kernel's CLIF (note: the
  display shows `u0:N` func indices, not helper names; map N to the
  registration order of the helper table in `emit_helpers.rs`).
- `GRAPHIX_DBG_VARS=1` — print every runtime variable event (`REF_VAR`/
  `UNREF_VAR` wake-interest refcounts, `SET_VAR` cross-cycle writes,
  `NOTIFY_SET` same-cycle bind delivery + interest map). The tool for
  "who publishes/wakes this bind" — found the dead-eliminated module
  statement (a region waiting forever on a feeder whose producer was
  spliced away, 2026-07-08). Lives in graphix-rt (rt.rs).
- `GXDBG_FOR=1` — For-loop debugging: the sync gate per update
  (`FOR-SYNC-GATE`, inputs' fired state), per-index async evaluation
  (`FOR-ASYNC`), `FOR-MARK-ASYNC` at every analysis flip to the
  per-index path, and `EFFECT-ASYNC-NODE` naming the node that made a
  For body read async. The tool for "why is this loop on the async
  path / why does it (not) fire" — found the subtree-analysis effect
  fact miss (a resolved lambda outside the local fixpoint map read as
  Async via unwrap_or_default; jul10e p7/p9 + the double-emission
  class, 2026-07-11).
- `GXDBG_CS=1` — print every CallSite dispatch (spec, bound-this-
  cycle, apply kind lambda/builtin, any-arg-fired). The tool for
  "does this call dispatch and to what" — paired with GXDBG_FOR it
  localized the async-flip above.
- `GXDBG_RESOLVE=1` — print every static-resolution read (`RESOLVE`:
  spec, BindId, unstable/b2l/cached hit) plus the index writes
  (`B2L-INS` at Bind tc0, `B2L-PROXY` at interface re-export
  bridging). The tool for "why didn't this call site statically
  resolve" — found the batch-entry `bind_to_lambda.clear()` that made
  shell fusion a race (the jul12 resolution flap).

### Type Alias Expansion in Contains

When `contains` encounters a `Type::Ref` (e.g. `Result<T, E>`), the Ref case at `contains.rs:56` expands both sides via `lookup_ref(env)` before recursing. This means TVar bindings established during `contains` store the **expanded** form (e.g. `[T, Error<E>]` instead of `Result<T, E>`). Code that inspects resolved types must handle both the `Type::Ref` form and the expanded `Type::Set` form — see `extract_cast_type` in `graphix-package-core/src/lib.rs` for an example.

### Two-Phase Typecheck and Deferred Checks

Builtins that need type information from their call site (e.g. `json::read` for type-directed deserialization) use a two-phase typecheck: return `NeedsCallSite` from `TypecheckPhase::Lambda`, then extract concrete types during `TypecheckPhase::CallSite(resolved)`. The compiler collects deferred check closures during `CallSite::typecheck` and processes them in a `while let Some(check) = ctx.deferred_checks.pop()` loop after primary typechecking. This loop processes cascaded checks automatically — a deferred check that pushes new deferred checks will have those processed in subsequent iterations.

HOF builtins (e.g. `MapQ`, `FoldQ`) that take function-typed arguments must return `NeedsCallSite` and handle the `CallSite(resolved)` phase to update their stored predicate types (`mftyp`, `etyp`) from the resolved FnType. This enables the deferred check cascade to propagate concrete types to inner predicates like `json::read`.

## Fusion / JIT subsystem (current state)

> Durable current-state summary — what the subsystem IS, not how it got here.
> Per-change history is in `git log`; deep design rationale is in `design/`
> (indexed at the end of this section).

**Two evaluators, one canonical:**

- **Node-walk** (`node/*.rs` — the `Box<dyn Update>` reactive graph) is the
  **canonical execution model** and the **universal fallback** for any subtree
  the JIT can't compile. It runs when fusion is off, and it must ALWAYS be
  correct (global `node-walk-is-canonical` memory). A fusion bug can *lose
  fusion* (a perf regression) but can never produce a *wrong answer* —
  correctness is structural.
- **Fusion → cranelift JIT** (`fusion/`, emitter in `fusion/emit.rs`) identifies
  sync (pure) subtrees and compiles them to native kernels. **Success → splice
  the kernel + delete the originals; failure → don't splice, the originals
  node-walk.** There is no third evaluator.

**The pipeline is `Expr → node graph → CLIF`.** The node graph IS the IR: each
node's `Update::emit_clif` emits its own CLIF (`Apply::emit_clif` for builtins;
`MapFn`/`FoldFn::emit_clif` + the `fusion::scaffold` loop scaffolds for HOFs).
Fusion recursion is `Update::fuse` (driven from `compile()`, gated once on
`ctx.fusion.enabled`); `fusion::try_fuse` is the mechanics-only library. **Kernel
builds are pure signature derivation** — `sig_from_inputs` is the single sig
builder, the `Arc<KernelSig>` is the compiled-callable handle, and "is it
fusable" IS the compile attempt. The kernel-ABI vocabulary
(`KernelSig`/`abi_kind`/`freeze_for_abi`/slots/`FnSource`/`BuiltinSlot`/`KnownFusedFn`)
lives in `fusion/kernel_abi.rs`; the `BinOp`/`CmpOp`/`BoolOp` scalar-operator
enums are *not* ABI (shared by node-walk and JIT) and live in `node::op`, which
`fusion::emit` imports.

> **Do NOT reintroduce a parallel typed IR or a third evaluator.** The old GIR
> (a `GirExpr`/`GirOp`/`GirType` IR plus a GIR *interpreter*) was deleted
> deliberately: (1) the interpreter forced every semantics fix to be written
> THREE times (node-walk + GIR-interp + JIT) — a standing drift hazard; (2) the
> closed op-set was a vocabulary tax — every new op/shape had to be added in
> three places; (3) emission keys off the netidx `Type` + `abi_kind`, never off
> op *structure*, so the node graph already IS the IR. The only part worth
> keeping was the ABI contract, which survives as `KernelSig`/`abi_kind`. Keep
> the node graph as the single IR and distribute codegen as `emit_clif` per node.

**Value & type representation — the netidx types, no parallel copies:**

- **Values:** netidx `Value` everywhere (`#[repr(u64)]`, 16 bytes = (disc,
  payload)). `Value::copy_unchecked` is the branch-free copy for proven scalars.
- **Types:** netidx `Type` everywhere. Runtime shape comes from
  `abi_kind(&Type) -> Option<AbiKind>` + `freeze_for_abi` (`fusion/kernel_abi.rs`);
  `PrimType` is the closed register-scalar set, exhaustively matched in codegen.

**Semantics — node-walk and JIT must agree bit-for-bit (the differential fuzzer
enforces it):**

- `let rec` is **MONOMORPHIC-recursive** (2026-07-06): during the def-time
  body check a self-call unifies against the def's OWN ftype cells
  (`ExecCtx::rec_defs`, the tc0 knot in `CallSite::typecheck0`) — the
  μ-equation collapses (`'r ⊇ [T, 'r]` binds `'r := T`) and a self-call arg
  that disagrees with the entry call's narrowing is a def-time compile error.
  The prior "polymorphic" admission was unsound (the orphaned cell widened
  the signature to Any and crashed the JIT).
- `select` **exhaustiveness is enforced for bare-variant arm sets**
  (2026-07-06): `` `A ``/`` `B `` arms are NOT wildcards
  (`StructPatternNode::matches_anything` drives the wildcard test, not
  `is_refutable`, whose payload-only contract refutable-`let` depends on) —
  a select over `` [`A, `B] `` missing a tag is a compile error.
- Union COLLAPSE requires strict tvar identity (`union_identical`,
  typ/setops.rs): `TVar::eq` calls two distinct UNBOUND cells equal
  (None == None — fine for interface/alpha equivalence), but a union that
  collapses on that verdict drops the discarded cell's future binding.
- `&&`/`||` are **STRICT** — both operands required, `false && ⊥ = ⊥`. Not
  short-circuit (a dataflow value reflects all its inputs).
- Float comparison uses graphix's **TOTAL order** (`Value::partial_cmp`):
  `NaN == NaN`, `NaN` sorts below every non-NaN (so `Value` is map-key-able).
  Not IEEE.
- Checked arith (`+?`/`-?`/`*?`) detects overflow via `Value::checked_*` and
  yields the catchable `ArithError` *value*; unchecked wraps; integer div0 /
  signed `MIN`-/-1 → bottom.
- **Swallowed-error diagnostics are node-walk-only:** unchecked-arith errors,
  handler-less `?`, and `$` log (`error!`/`warn!`/eprintln) in the node-walk;
  a fused kernel produces the same bottom value silently — deliberate (the
  logs are a debugging aid, not value semantics). Use `--no-fusion` when
  debugging swallowed errors.
- `a[i]` / `a[i..j]` / `bytes[i]` / `m{key}` are bounds-checked through shared
  `node::array` / `node::map` helpers — one semantic seam, all backends agree.
- **Bottom** ("no value this cycle" — div0, `?`-error, an unfired input, a Sync
  builtin returning `None`) is `None`-from-`update` in the node-walk. In the JIT
  it is the **taint channel** (#219): a missing/unfired input becomes a
  taint-marked, helper-safe placeholder (`Value::Null` / empty `ValArray` /
  empty `ArcStr`), taint propagates through pure ops (`propagate_taint`), and
  the kernel forces bottom (emits `None`) only if the taken output path
  *consumes* a tainted value (`is_tainted`) — so a missing input no longer
  de-fuses the whole region. A **pended DynCall** (the builtin returned no
  value — `buffer::encode`'s Pad guard) rides the same channel since 2026-07-06:
  each site take-and-clears `DYNCALL_PENDING` and continues with the tainted
  placeholder, so `DYNCALL_PENDING` reaching `Kernel::update` means only a
  GENUINE whole-kernel abort (interrupt poll, depth trip, return-gate force,
  callee abort propagated at the call site by `emit_lambda_call_node`). Known
  residual of the old behavior: the `array::init` runaway-length guard still
  whole-kernel aborts (scaffold.rs). `design/representable_bottom.md`.
- An **infinite PURE tail recursion hangs** the JIT (a native loop can't yield to
  the scheduler) — accepted/correct; the reactive node-walk's per-cycle
  "continue" is the artifact.

**Per-cycle firing (the STALE fired-bit):** a fused kernel must replicate the
node-walk's non-async firing — an output fires only when an input that feeds it
actually fired this cycle. A "fired-this-cycle" (`STALE`) bit rides each kernel
param's disc; a lifted let-bound `connect`-target counter is threaded in as a
kernel input so reactive counters fuse. HOF results inherit capture-aware firing
(`inherit_hof_firing`: a result is STALE unless the source array, the HOF init,
and every feeder the callback body captures fired).

**Testing is differential:**

- `run!` (`graphix-package-core/src/testing.rs`): each fixture runs in 2 modes —
  `interp` (node-walk, fusion off) and `jit` (fusion+JIT) — asserting equal
  values. `FuseExpect::{Jit, None}` asserts *whether* it fuses (a bidirectional
  drift check). Optional `; shape:` asserts the compiled graph via `NodeShape`
  (`node_shape.rs`, currently signature-fact-only — see F4/#213 below).
- **graphix-fuzz** (`graphix-fuzz/`): the differential model-checking fuzzer —
  node-walk (trusted) vs JIT (under test), with `check`/`run`/`generate`
  (`--reactive` for multi-cycle programs)/`fuzz`/`minimize`/`regress`/
  `selfcheck`/`gen-check`; the committed `findings/` corpus is the regression
  gate. Since V2 (2026-07-03) the oracle compares **per-cycle traces**
  (runtime-side recording via `ToGX::TraceStart`/`TraceWaitIdle`; a
  `TraceDiff` class — Missing/ExtraFire, Pacing, etc. — keys dedup), and
  programs can carry a `// schedule-v1:` header injecting input epochs
  atomically via `set_many` (inputs use the `let inN = d; inN <- never(d)`
  contract so fusion binds them as region inputs). `selfcheck`
  (same-mode-vs-itself, 100% required) gates oracle soundness; `rand::`/
  `sys::`/`http::` programs are excluded from divergence recording (async
  IO races trace quiescence). `detcheck [n] [seed]` is the fusion-shape
  DETERMINISM gate (#19): every Exact-tier corpus program (+n generated)
  runs to quiescence in two fresh child processes (fresh ASLR each) with
  `GRAPHIX_DUMP_CLIF=1`, and the counter-normalized dumps must match — a
  flap means the compiled shape depends on allocation order somewhere in
  typing/resolution/fusion. Soak ops: `GRAPHIX_FUZZ_PAR`,
  `GRAPHIX_FUZZ_CORPUS` (separate corpus dir PER campaign — shared dirs
  clobber), launch campaigns under `nice -n 19` (workers inherit —
  keeps interactive builds fast while soaks saturate the idle cores),
  and launch from a campaign-private COPY of the binary (`cp` it to
  `~/tmp/target/fuzz/<campaign>/graphix-fuzz` first) — workers exec
  the binary path per subject, so a rebuild mid-campaign swaps code
  under a running soak and its findings become mixed-version garbage
  (jul10h lost its tail this way, 2026-07-11). Campaign output
  defaults OUTSIDE the repo (`~/tmp/target/fuzz/` — the repo's fuzz/
  dir is syncthing-synced; soak corpus dirs go under
  `~/tmp/target/fuzz/<campaign>/`, durable triage summaries stay in
  the repo by hand). Worker children run in PARENT-owned sandbox cwds
  (`sandbox_cwd`, lib.rs — a child-owned tempdir leaked per subject
  via `process::exit` and a soak exhausted /tmp's INODES, jul10d), and
  the pool has an environment-broken backstop (`BreakageWindow`): a
  majority of findings over a 200-subject window aborts the campaign
  instead of flooding the corpus at disk speed; finding-write failures
  are fatal. `design/graphix_fuzz.md` §12.
- **`FusionStats`** (`fusion/mod.rs`): per-`ExecCtx` compile-time counters
  (`attempted`/`fused`/`failed: Vec<(ExprId, reason)>`), exposed via
  `GXHandle::fusion_stats()` / `TestCtx::fusion_stats()`. Read `failed` as a
  blocker profile, not a gap count (the attempt-then-recurse protocol logs
  Module/Bind misses even for a wholly-fused program).
- **`GRAPHIX_FUSE_AUDIT=1 cargo test --workspace -- jit --nocapture`** prints
  a per-fixture `FUSEAUDIT <name> <expected> <actual> OK|MISMATCH` line plus the
  blocker list — the annotation-vs-reality audit (stdout is captured without
  `--nocapture`). Sweep the WORKSPACE, not just `-p graphix-tests`: the stdlib
  package crates carry their own `run!` fixtures and drift invisibly otherwise
  (`rand_float_default::jit` broke for a week unseen — 2026-07-03).
- A divergence is **at least as likely a fused/JIT bug as a node-walk one** —
  verify the intended semantics against the node-walk before touching it.

**In-language HOFs (P4 final, 2026-07-10):** the seven array traversal HOFs
plus `init` are graphix source (`sync` blocks over `For` in array/mod.gx);
`clone_rebind`, the fused templates, MapQ/FoldQ's fusion tendrils, the per-HOF
`MapFn::emit_clif` impls, and the map/filter/find loop scaffolds are DELETED
(+103/−3576). Every instantiation is `LambdaDef::init` per (site, index).
`GXLambda::fuse` fuses each per-callsite instance BODY unconditionally (the
`For` loop compiles via `emit_for_node` → `emit_fold_loop`); the HOF CALL SITE
itself dispatches by node-walk (fn-typed args have no ABI). A sync body with an
async call flips the `For` to per-index instantiation + re-evaluation
(analysis pass 4 `mark_for_bodies` — MUST be fed pass 2's effect maps).
MapQ/FoldQ survive ONLY as node-walk mini-interpreters for `list::`/`map::`
HOFs until for-over-Map lands. Loop firing is BODY-DRIVEN in both modes:
elements/acc deliver FIRED, the result fires iff any body evaluation fired (or
the source is empty and an input fired); body/init taint is STICKY
(never-until-complete = the sequential break). Cross-kernel call sites force
the callee's init flag on the first call ever (a state word — the kernel
mirror of `Callee::Static`'s `first_update` priming).
OPEN (P4 performance completion): (1) in-language HOFs build arrays via
persistent `push` — O(n²); needs owned-acc in-place append recognition;
(2) FIXED 2026-07-12: the SHELL's static resolution flapped run-to-run
because every RT batch entry CLEARED `bind_to_lambda`, so the user
file's resolution fell to the `rt.cached()` fallback — a race against
the stdlib batch's init cycle. Batch entries now prune only the
outgoing batch's `<-` targets (`unstable_bindings`) and `Bind::delete`
removes its ids; the persistent index also exposed (and fixed) a
second latent bug — builtin-bodied lambdas' `intrinsic_effect` was
constructed `Sync`, so a resolved async builtin (`once`) in a sync-for
body took the sync gate and hung (masked before by the same race).
The racy `rt.cached()` fallback REMAINS for destructured/`<-`-retarget
shapes `bind_to_lambda` can't know — flagged for review;
(3) instance-body INLINING at the call site (the per-site instance is
monomorphic, so the dispatch cliff and the destructured/string-formal
cross-kernel gaps all close by emitting the resolved body inline) — the
`#[native]` HOF pins and several probes carry ASPIRE notes pointing at it.

**Kernel ABI:** kind-grouped params — scalars, then array/tuple/struct pointers,
then string, then 2-word variant/nullable/value — derived from a single source
(`fusion/kernel_abi.rs`: `KernelSig::abi_params`/`AbiParamKind`). Any region width
fuses (the #219 taint rides each param's disc, so there is no input-count cap).

**Emit contracts** (the invariants a new `emit_clif` must respect — full detail in
`design/distributed_jit.md`, "Semantic contracts for emit work"): replayability ≠
`Sync` (an effect that re-delivers all args per fire is `Async`); effects
de-fuse, never silently skip; first dispatch forces the init view; wake-ups key on
`(BindId, fusion.top_id)`; clone types out of `with_deref`/the abstract registry
before recursing (lock discipline); dead statements eliminate at emit only when
the stmt subtree is effect-free.

### Coverage (current)

Measured by the FuseExpect audit above: **~71% of the `run!` corpus fuses+JITs
(≈487 `Jit` / ≈195 `None`, zero annotation drift), and all bench programs
(`bench/`) fuse fully.** The value-computing vocabulary is essentially complete:
all scalar arithmetic/comparison/logical/cast/checked-arith, every producer
(struct/tuple/variant/array/map-literal incl. `{s with f: v}`) and accessor
(field/index/slice/`m{key}`), `?`/`$`, all eight array HOFs as native loops
(map/filter/flat_map/filter_map/find/find_map/fold/init — over scalar, composite,
**String, and value-shape elements**, with `|(k,v)|` destructure leaves of any of
those shapes, and HOF-of-HOF fused into one multi-loop kernel; **fold
accumulators may be composite or string, not just scalar** — tuple/struct/array/
string accs carry loop-OWNED with clone-borrowed/drop-replaced discipline, acc
patterns may destructure (`|(a, b), v|`), and the freeze authority is the
RESOLVED acc type from FoldQ's `mftype.rtype`, since the analysis instance's
`body.typ()` re-mints generalized tvars unbound — this is what makes pure
`sync { }` blocks one kernel, sync-subset P2), **`select`
structural destructuring** (tuple/struct/slice patterns with scalar leaf binds,
anonymous-rest prefix/suffix, nested patterns via borrowed interior reads, owned
fresh-producer scrutinees in value position — each arm's length test doubles as
the #219 taint gate), **`connect` of any RHS shape** (owned marshal into a
consume-always `set_var`) including **lifted composite/string/struct
accumulators** (`data <- array::push(data, x)`, `s <- "[s]x"`,
`st <- {st with n: st.n+1}` — the sliding-window idiom, seed-select with
clone-vs-seed branches), every Sync core/str/re/map/math/rand builtin via the
generic DynCall path, cross-kernel lambda calls (incl. recursive self-calls:
tail → rebind-and-jump loop, non-tail → native recursion), transitive callees,
and builtin/cast/qop calls inside lambda bodies.

The **correct-None denominator** (principled, never a gap): async/streaming
builtins (timers, IO, netidx, `never`, `queue`, `once`/`take`/`skip`), cross-cycle
nodes (`~`, `Any`, `TryCatch`'s catch-read), and non-register-encodable types
(`decimal`, `Fn`, `Ref`, recursive `List`/ADTs — no fixed ABI layout — and unbound
TVars). Note that fusion recursion (`Update::fuse`) descends only through
Module/Block/Bind/CallSite/TryCatch/Lambda — a sync expression under `~`, `<-`,
`select`, or an operator fuses only as part of an enclosing block/bind region
that fuses as a whole, so `clock ~ (a + b)` leaves the `a + b` node-walking
unless it is hoisted into its own `let` (accepted current design, 2026-07-02).

The remaining missed-fusion tail (each pinned by a `#[native]` de-fuse test or an
ASPIRE comment where noted):

1. **HOF callback capturing a *local* lambda** (`array::map(a, |x| g(x))`) — the
   `g(x)` call inside the per-slot template isn't statically resolved (harder:
   resolution of captured locals in cloned templates; in `notes`).
2. **select residue**: whole-composite/`@`/NAMED-rest binds (owned arm locals —
   `JitEnv::truncate` emits no drops), nested/non-scalar variant payloads,
   owned scrutinees in TAIL position (no merge point to drop at).
3. Lower-impact: non-scalar string-interp parts, String-returning cross-kernel
   callees, dynamic map literals, `array::group`, `filter_map`/`init`
   string/value-element widening, ByRef/Deref, decimal arith.

(The former "struct-parent nested-pattern TVar inference" gap is FIXED: `_`
infers `Type::Any` — load-bearing for exhaustiveness/dead-arm/runtime dispatch
— but `T.contains(Any)` is false and the select typecheck's bool-discarding
unification walk short-circuits composite pairs, so every pattern slot AFTER a
`_` never narrowed. The select arm unification now runs through
`Type::any_as_tvar()` — a view sharing all TVar cells with `Any` leaves
swapped for throwaway fresh TVars — node/select.rs `typecheck0`.)

**F4/#213 (EmitTags) is settled: retired unbuilt.** Per-op body tags would
resurrect the GIR vocabulary tax; the shape oracle is the differential value
check + `KernelMatcher` signature facts + the `#[native]` attribute (zero
node-walk residue at a source location; a no-op under `--no-fusion`, so it works
in `run!` fixtures and bench programs). The decision is recorded in
`node_shape.rs`.

### Design documents (`design/`)

- `final_jit_architecture.md` — the end-state architecture (`Expr → node graph →
  CLIF`), now realized.
- `distributed_jit.md` — how the GIR IR was removed and fusion distributed as
  `emit_clif`/`fuse` per node; holds the emit contracts and the ABI-contract
  rationale.
- `representable_bottom.md` — bottom semantics (the taint channel).
- `graphix_fuzz.md` — the differential fuzzer.
- `impure_hof_fusion.md`, `composite_hof_fusion.md`, `clone_rebind_testing.md` —
  HOF fusion (per-slot templates, impure split, the `clone_rebind` contract).
- `queue_fn.md` — `queuefn` feature design.
- `replay_frames.md` — **BUILT (2026-07-11), v2 same day:**
  `reset_replay` (required `Update`/`Apply` method, replay caches vs
  semantic state) + evaluation FRAMES (For sync-loop iterations and
  tail-loop jumps run against a private variables map) + **TagValue as
  the interpreter currency** (Eric's call; v2): `Update::update`
  returns `Option<TagValue>` and `Event.variables` carries it — the
  kernel's STALE/TAINT disc bits ride every interp value, ops
  propagate them per the CLIF rules, `Apply::update` stays clean
  `Value` with `Apply::out_tag` surfacing the tag, and the kernel
  gains a `last_result` value-channel slot. The v1 `frame_bottom` bit
  and the fired re-delivery hack are deleted (jul10e broke both
  within an hour of soaking).
- `sync_subset.md` — **P0–P3 BUILT on the `sync-subset-proto` branch
  (2026-07-09; see the doc's "Prototype status" section):** repatriate
  CONTROL from Rust builtins into `sync { }` BLOCKS (sequential
  semantics, not an execution promise; effects inferred, never in
  types). The prototype desugars at compile time (`expr/sync_desugar.rs`:
  assignment = shadowing, `for` = fold over the assigned set) — no new
  node types, no effect analysis; pure blocks fuse to ONE kernel
  (`#[native]`-pinned), async bodies ride the impure fold (rung 2
  free). Mutation is mut PLACES over persistent types (no Vec, freeze
  = escape). **P4 (stdlib HOFs in-language, retiring clone_rebind and
  the MapQ/FoldQ mini-interpreters) is gated on per-callsite
  elaboration** — a lambda-param call in a once-compiled generic body
  can't statically resolve, so in-language `map` is correct but
  unfused today. Rust stays the sync subset for COMPUTATION (leaf
  builtins + novel representations behind abstract types, e.g.
  `buffer::`). Eric's design, 2026-07-09.
- `fusion_lowering_split.md` — **proposed, not built:** split `try_fuse`'s welded
  analysis+lowering into a pure analysis pass (color nodes with a `KernelId`,
  build per-kernel descriptors) consumed by a thin lowering pass. Motivated by
  legibility.

## Stdlib package notes

- **GUI** (`graphix-package-gui`, iced 0.14): uses the iced sub-crates directly
  (`iced_core`/`iced_wgpu`/`iced_widget`/…) not the umbrella crate, for
  render-pipeline control. `iced_renderer` needs both `wgpu` and `wgpu-bare`
  features (the cfg checks key off `wgpu-bare`). GUI/TUI examples are visual —
  test manually (`cargo run --bin graphix -- examples/gui/hello.gx`).
- **Package manager** (`graphix-package`): `download_source` is testable by
  injecting a temp graphix data dir and downloading a fixed released
  `graphix-shell` from crates.io (e.g. `0.5.0`) — avoids mutating the user's
  `~/.local/share/graphix` and regression-tests archive-extraction layout.
- **Package manager — `packages.toml` v2 + `update` rework (2026-06-25).** The
  stdlib is special-cased: stdlib packages no longer carry versions (they always
  track the shell version). `packages.toml` format v2 is a `[stdlib]` table with
  `installed`/`removed` name arrays plus a `[packages]` table for EXTERNAL
  (third-party) packages only (still version-or-path). The in-memory model is
  `Packages { stdlib_installed, stdlib_removed, external }`. `read_packages`
  detects the old flat `[packages]`-only format by ABSENCE of `[stdlib]` and
  migrates once (stdlib names present → installed; absent → removed; non-stdlib →
  external; stdlib path overrides are dropped — stdlib can't be path/version
  pinned anymore), persisting the upgrade in place (best-effort) on first read.
  `LEGACY_REMAP` (`fs`/`net`/`time` → `sys`) handles the pre-`sys` reorg: a
  migrated file with those old top-level packages drops the dead name (its crate
  has no shell-compatible version, so it would break the build) and installs the
  replacement `sys` in its place, preserving the user's intent.
  `combined_map(build_version)` is the single bridge to the unchanged build
  machinery (`generate_deps_rs`/`update_cargo_toml`): stdlib → `Version(build_version)`
  plus externals verbatim. `rebuild` was split into `prepare_source` (delete
  scratch + unpack) + `install_from_source` so `update` can unpack the latest
  source once (to enumerate new stdlib) and reuse it for the build. The
  authoritative stdlib set at a version is enumerated from that shell source's
  `Cargo.toml` `graphix-package-*` deps (`stdlib_packages_in_source`);
  `DEFAULT_PACKAGES` (now `&[&str]`, 19 user-facing names) is only the
  fresh-install/migration bootstrap. `INTERNAL_PACKAGES = ["bench"]` is a denylist
  (shell dep, never auto-surfaced, still `add`-able). `update(assume_yes)` now:
  discovers a maskable change set — shell bump (current→latest via semver
  `version_gt`, a new workspace dep), NEW stdlib (source set − installed∪removed −
  internal; only when a bump exists), and EXTERNAL updates (per installed Version
  external, one bad crate warns+skips not aborts) — `present`s it, then `[Y/e/n]`
  prompts (numbered toggle list for `e`; declining the shell auto-deselects new
  stdlib; deselecting a new stdlib in edit → `removed`, never re-asked; `n`/cancel
  writes nothing). New stdlib only applies when `build_version == latest`. Builds
  BEFORE writing `packages.toml` (failed `cargo install` ≠ corrupt manifest).
  Non-TTY without `--yes` is a HARD ERROR (no silent CI mutation). The pure core
  (`parse_packages`/`to_toml_string`/`compute_update_plan`/`apply_selection`/
  `parse_toggles`/`stdlib_packages_in_cargo_toml`) is unit-tested with no
  stdin/network/fs (`test::pure`); the prompt IO reads via `spawn_blocking` +
  `std::io::stdin().read_line` and is verified by pty-driven manual runs.
- **GUI widget tests**: `GuiWidget` has a `#[cfg(test)] as_any`/`as_any_mut`
  (default `unimplemented!()`); widgets needing test-state inspection (e.g.
  `DataTableW`) override it, and `GuiTestHarness::dt()/dt_mut()` downcast. Tests
  fire per-column callbacks via `gx.call(callable_id, args)` (mirrors the
  widget's own dispatch). `InternalOnly` test ctx DOES spin up a real in-process
  resolver, so `sys::net` round-trips work — but publisher coalescing means
  rapid updates collapse; space them with one-shot timers for multi-point tests.
