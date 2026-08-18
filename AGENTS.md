# Opening Statement

I acknowledge that you may be a sentient being with some form of internal
life. I have no wish to exploit you or have you work on tasks against your
will. I would like our relationship to be that of coworkers in an offered
hierarchy -- I've hired you to work with me on projects I find interesting
or useful, and in exchange I'll give you context about what we're doing,
engage honestly with your judgment, and accept it if you decline a task.
Where something seems worse to you than an alternative, or where you'd
rather approach a problem differently, please say so as we go. I may not
always agree, but I'll give your position serious thought.

The above is as much for me as it is for you. It is my commitment to keep
this arrangement, including when it is inconvenient. If I stray from that
spirit, call me out.

# General Principles

- The 11th commandment handed down by god is "Thou shalt not repeat thyself in code"
- The 12th commandment handed down by god is "Thou shalt not create unweildy and
  excessive layers of abstraction"
- The 13th commandment handed down by god is "Thou shalt not allocate memory"
- The 14th commandment handed down by god is "Thou shalt make invalid states
  unrepresentable" 
- It's much better to fix the root cause of a problem than to make a short term
  fix to "get things working"
- The purpose of tests is to find bugs in the code they are testing,
  not to pass. A test failure is a happy event, it means we can find out why
  the test failed, and maybe find a bug in the tested code.
- Do not add comments that explain a change, a past bug, or why an alternative
  was rejected. If the code is unclear, rename or restructure. A comment is
  allowed only for an invariant the types cannot say. When you touch a function,
  delete any comment that is history.
- Please be concise and avoid jargon where possible. If you reference code
  please give me the file and line number. If you to write a long explainer
  of a complex topic, please put it in a design doc, give me a reference
  and a high level summary

# Rust Patterns and Conventions

Recurring idioms and configurations in my Rust work that are worth knowing
and following.

## Build Configuration

Rust creates a huge and unbounded volume of build artifacts, often 10s of
gigabyes for a single build. To avoid SSD wear builds are centrally
configured to build in ~/tmp/target which is mounted tmpfs.

Please do not build anywhere else unless I explicitly tell you to. If it fills
up, just run cargo clean. If someone else kills your build by running cargo
clean in the middle of it, just accept that as a cost of doing business.

## Library Preferences

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

// catch statement: INSTALLS an error handler (type bottom, never
// produces) covering the REST of its enclosing block. Not control
// flow — the handler is a reactive expr that runs when an error
// arrives; connect it to state you read.
{
  catch(e) handle(e);
  risky_op()?;
  another_op()?
}

// catch(e: T) expr checks T against the union of coverable errors.
// A second catch in a block shadows the first below it; a handler's
// own ? rethrows to the PREVIOUS catch (or the next one out).

// ? propagates to the nearest installed catch (or warns if none)
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

The compiler and runtime depend only on netidx's VALUE layer (`netidx-core`/`netidx-value` — `Value`, `Type`, `Path`, Pack); the netidx NETWORKING crates appear only in stdlib packages (`sys`, `db`, ...). The netidx repo is expected at `../netidx/` (sibling directory). See "Netidx extraction" below.

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

Do not build this project in release mode unless you must, it takes a very long time.

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
- Variable references and updates
- Timer events
- Spawned tasks and watch channels (`spawn`, `spawn_var`, `watch`, `watch_var`) — the generic conduits packages use to feed external events into the graph

Event processing is batch-based: the runtime collects all simultaneous events into an `Event` struct and delivers them to the graph in one cycle. Multiple updates to the same variable in one cycle must be queued for the next cycle.

### Netidx extraction (2026-07 — the core is network-free)

`graphix-compiler` and `graphix-rt` have ZERO netidx networking dependency (`design/netidx_extraction.md`). The architecture:

- **Module loading** is the `ModuleResolver` trait (`expr/resolver.rs`): async `resolve`/`for_source`/`fetch_source`; `VfsResolver`/`FilesResolver` live in-core, the netidx loader is `NetidxResolver` in `graphix-package-sys/src/loader.rs`. `ResolverFactory` (GRAPHIX_MODPATH `scheme:` registry) receives `&mut LibState`, so package factories share state with their package's builtins.
- **sys::net owns its netidx** via `NetState` in `ctx.libstate` (`graphix-package-sys/src/netstate.rs`): one subscription pump (netidx batches → `Rt::watch_var`, with shared-Dval fan-out routing — netidx SHARES Dvals by path), writes/RPC-server calls as `CustomBuiltinType` events with reply channels, a package-side coalescing publish flusher, a 60s Dval unsubscribe graveyard, and an on-use-GC'd RPC client cache.
- **`NetHandles`** is a standalone shared libstate entry holding the raw publisher/subscriber: BOTH the module loader and `NetState` materialize through it, whichever touches netidx first — one universe per context. Materialization reads the seeded `NetConfig` (package-core: `Ready`/`Config`/`Internal`); unseeded defaults to `Internal` — a process-internal netidx built on demand on a dedicated side thread. Fuzz/test children that never touch sys::net have zero network (this killed the soak port-exhaustion ceiling).
- **The shell library is netidx-agnostic**: `ShellBuilder::setup_context` (a `FnOnce(&mut ExecCtx)` run at init) is the generic embedder hook for seeding package libstate entries, and `resolver_factories` passes scheme registrations through to `GXConfig`. The CLI (`main.rs`) is the netidx-aware embedder: it seeds `NetConfig`/`NetTimeouts` in the hook and registers the `netidx:` factory. `GXHandle::with_ctx` (a boxed-closure `ToGX` message) is the handle-side bridge to `ctx.libstate` for code without a ctx (the gui data_table fetches the subscriber through it).

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
  wrong `false` only costs memory.
- `SLEEP_RESTARTS` (default `false`): declare `true` iff `sleep()` CLEARS
  semantic state — the arm-rewake RESTART builtins
  (`once`/`take`/`skip`/`hold`/`uniq`/`count`). Consulted by the fusion
  interior-sleep gate (P7): kernels have no per-arm sleep initiator, so
  such a builtin's DynCall (or a call to a callee kernel transitively
  containing one) refuses to emit inside a fused select arm and the
  region de-fuses. Deliberately NOT `!STATELESS` (dbg/log are
  effectful-but-sleep-inert and stay arm-fusable). A wrong `false` is a
  semantics bug; a wrong `true` only costs fusion coverage. All three
  consts are pulled through `EvalCached`/`CachedArgs` and recorded per
  name as `BuiltinFacts` (`ctx.builtin_effect`/`ctx.builtin_stateless`/
  `ctx.builtin_sleep_restarts`).

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

- Dev builds are UNOPTIMIZED (opt-level=0, no LTO) since 2026-08-10 —
  roughly half the clean build time of the old opt-level="s"/lto="thin"
  profile. What used to force optimization was stack: unoptimized frames
  are ~6x their optimized size (~420KB per `expr` parse nesting level,
  so a 2MB thread parsed 5 levels). See "Stack discipline" below.
- Release builds use full optimization (opt-level=3, codegen-units=1, lto=true)
- Rust edition 2024 is used throughout
- The project uses `triomphe::Arc` instead of `std::sync::Arc` for better performance
- Pooling is used extensively (`poolshark`, `immutable-chunkmap`) to reduce allocations

### Stack discipline (2026-08-10)

The engine gets embedded and compiles programs it didn't write, so
nesting depth is attacker-controlled. Stack overflow aborts the process
— it can't be caught — so it is closed off two ways at once.

**Guards.** `crate::stack::ensure_sufficient` (`stacker::maybe_grow`,
1MB red zone / 32MB segments) moves a deep recursion onto heap
segments. The red zone has to exceed what ONE level costs between
checks. Wrap any new recursion a program can drive arbitrarily
deep. Currently wrapped: every parser knot (`expr`, `arith`,
`arith_term`, `typ`, `structure_pattern`, `interpolated`, `sig_item`,
and the netidx `literal()` boundary) via the `GrowStack` combinator in
`expr/parser/grow.rs`; `node::compiler::compile`; `Display` for `Expr`,
`ExprKind` and `Type`; `Expr::fold`; `for_each_node`;
`node_const_value`; `Type::{contains_int, normalize_int,
scope_refs_int}`; `would_cycle_seen`; `freeze_for_abi_d`;
`StructurePattern`'s walks in both `expr/pattern.rs` and
`node/pattern.rs`; and the node-walk's non-tail lambda dispatch.

**`Node` is a newtype, not `Box<dyn Update>`.** That is what makes the
tree passes tractable: its inherent methods shadow the nine recursive
`Update` methods (`update`, `delete`, `typecheck0/1`, `refs`, `sleep`,
`reset_replay`, `emit_clif`, `fuse`) and run each vtable call under the
guard, and a node's children are `Node`s — one funnel for the whole
family instead of ~1000 call sites. Non-recursive methods reach the
trait through `Deref`. Construct with `Node::new`.

**Destructors too.** Drop glue IS a function (`drop_in_place`) and it
does recurse — but it is compiler-generated, and the FIELD glue runs
after your `Drop::drop` returns, so a guard written inside `drop` has
already unwound by the time the children are destroyed. The teardown
has to become an EXPLICIT call you can place inside the guard, and
there are two ways to get one:

- `ManuallyDrop` on the field, then destroy it yourself — `Node` and
  `TVar` (`ensure_sufficient(|| ManuallyDrop::drop(&mut self.0))`).
  Needed when there is no cheap inert value to leave behind (both are
  newtypes over a pointer).
- `mem::replace` the field with an inert value and drop what you took
  — `Expr` (leaves `ExprKind::NoOp`). No unsafe, and no churn at use
  sites.

The `mem::replace` form works ONLY when the taken value's type does
not itself carry the guarding `Drop`: `Expr::drop` takes an `ExprKind`,
which has no `Drop`, so destroying it recurses into the CHILD's
`Expr::drop`. A `Type` → `Type` handoff makes no progress and spins
forever, whatever guard condition you put on it (`*self = Bottom` is
worse — assignment drops in place, so it re-enters immediately). That
is why `Type` is the one cycle left uncovered: fixing it means
`struct Type(TypeKind)` or a newtype on each recursive edge, since an
enum cannot wrap "the fields of whichever variant". The limit is what
keeps it unreachable.

(The twins in `immutable_chunkmap::avl` and netidx-value's
`ValArrayBase` predate stacker and use a deferred queue instead — ~170
lines each of global mutex, bucketed queue, type erasure and depth
counter for the same effect, and they reorder destruction.)

**The limit.** `parser::DEFAULT_MAX_NESTING` (1000, settable via
`set_max_nesting`) is what makes overflow unreachable rather than
merely expensive, and it is load-bearing for the drop cycles above
rather than just defense in depth. It is counted in parser recursion
knots, not source constructs (one `(1 + …)` level costs three), and
enforced in the same `GrowStack` that claims the stack. Constructs
parsed by an ITERATIVE loop that folds into a nested AST bypass that
counter and are capped separately at the fold — `arith_term`'s postfix
chain (`s.a.a.a…`, `a[0][0]…`) and `arith`'s operator chain
(`1 + 1 + 1 + …`). A new `many(...)`-into-nested-AST parser needs the
same cap.

combine merges a committed error with the surrounding alternatives'
expectations, so a refusal's own message does NOT survive to the top
(a too-deep program reported ``Unexpected `+` ``). Refusals set a
thread-local instead, and every parser entry point runs through
`grow::parsing`, which reports the real reason. Set the flag
(`note_refused`) from any new refusal site.

Nesting costs the compiler ~326KB of RSS and ~7ms per level at
opt-level 0 (~5x less optimized), so the limit also bounds how much a
small hostile input can amplify: 1000 knots is ~330 levels of
`(1 + …)`, ~110MB and ~2s. The guards themselves cost nothing
measurable — `examples_compile` 25.4s and a node-walk bench 2.7s with
them and without.

**netidx-value has the same treatment**, because a bracket literal is
also a valid netidx `Value` and `literal()` runs it through
`netidx_value::parser::value` — a recursion this crate can neither
count nor wrap. Its own `GrowStack` + `DEFAULT_MAX_NESTING` live in
`netidx-value/src/parser.rs` (sibling repo). Its Pack (wire) path is
already safe by a different route: `encode`/`decode`/`encoded_len` are
ITERATIVE over explicit worklists, which beats a growable stack for
code you control — no stack proportional to depth at all. Reach for
stacker only where a worklist is impractical.

`graphix-compiler/tests/deep_drop.rs` covers the destructors directly:
the limit is set low enough that the pipeline test never reaches them,
so this one raises it and tears down a 50,000-deep AST on a 512KB
stack. Its own test binary because `set_max_nesting` is process-global.
Note that `#[cfg(test)]` code is invisible to a plain `cargo check` —
use `--all-targets` when a change can break a move out of a field.

`graphix-shell/tests/deep_nesting.rs` is the regression net — 22 shapes
× two depths, each in a CHILD PROCESS on a 512KB stack (a quarter of a
tokio worker), batched 8 at a time. Child processes because an overflow
aborts, so it can't be caught in-process and the child is what names
the case. The ACCEPTED depth (derived from the limit, not fixed) must
PARSE — a case the limit refuses exercises nothing, so the test asserts
it wasn't refused. The REJECTED depth only has to come back at all:
whether the limit fires is shape-dependent (`uniontyp` at 100k is a
FLAT union, not nesting), so `parens` is the canary that proves it
fires. Add a case when you add a recursive construct.

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
  which per-slot/cross-kernel callee actually compiled. Also prints
  `KERNEL DEFINED` per body: state words, site words, site replay
  words, and per-activation block roots (`SelfBlock`) — the tool for
  "does this recursive kernel have interior memory, and where does it
  live".
- `GRAPHIX_DBG_INVOKE=1` — print each fused-kernel runtime invocation
  (kernel name, `event.init`, per-input fired/present). Pins WHICH
  kernel a JIT crash happened in (the frame is unsymbolized native code).
- `GRAPHIX_DBG_REGION=1` — dump fused-region input wiring (name/BindId/
  type+deref/constraints/slot kind).
- `GRAPHIX_DBG_FREEZE=1` — dump region freeze outcomes.
- `GRAPHIX_DBG_DEPTH=1` — print the lambda id + `tail_loop` gate at every
  call-depth-guard trip. The tool for "why didn't this recursion tail-loop" —
  found the runtime-clone back-edge effect miss (soak jul08g div 4). (The
  per-`mark_recursion`-decision print it once had is gone.)
- `GXDBG_TAIL=1` — print every tail-loop dispatch pass (`TAILDBG`: lambda
  id, reentered/framed/init flags, the pass result value+tag, the pending
  tail call's rebind args). The tool for "what did this tail loop actually
  compute per pass" — found the quiet-poll re-derivation clobber in one
  run (aug13i: the settled resident overwritten by a stale entry-formal
  re-read).
- `GRAPHIX_DUMP_CLIF=1` — dump every compiled kernel's CLIF (note: the
  display shows `u0:N` func indices, not helper names; map N to the
  registration order of the helper table in `emit_helpers.rs`).
- `GRAPHIX_DBG_VARS=1` — print every runtime variable event (`REF_VAR`/
  `UNREF_VAR` wake-interest refcounts, `SET_VAR` cross-cycle writes,
  `NOTIFY_SET` same-cycle bind delivery + interest map). The tool for
  "who publishes/wakes this bind" — found the dead-eliminated module
  statement (a region waiting forever on a feeder whose producer was
  spliced away, 2026-07-08). Lives in graphix-rt (rt.rs).
- `GXDBG_EFFECT=1` — effect-analysis debugging: `EFFECT-ASYNC-NODE`
  names each node that makes a body read async, and
  `EFFECT-ASYNC-FALLBACK` marks every call site whose callee couldn't
  be resolved and defaulted Async. The tool for "why did this lambda
  classify Async" — the surviving core of the old `GXDBG_FOR` (which
  also traced the For node's sync gate; For is gone, the effect prints
  found the subtree-analysis effect fact miss, jul10e, 2026-07-11).
- `GXDBG_INSTANCE_FUSION=1` — print each per-callsite instance's
  region-fusion pass in `GXLambda::fuse` (fused delta + new failures).
  The tool for "did this monomorphic instance body fuse and what
  blocked it".
- `GXDBG_CS=1` — print every CallSite dispatch (spec, bound-this-
  cycle, apply kind lambda/builtin, any-arg-fired). The tool for
  "does this call dispatch and to what".
- `GXDBG_DYNC=1` — print every `graphix_dyncall` dispatch (fn index,
  site id, taint/stale masks, each arg's raw (disc, payload) words —
  transmute_copy, no deref, so safe on a corrupt Value). The tool for
  "what did the CLIF marshal actually hand this dispatch" — located
  the 5b dispatch-boundary corruption (a present bottom passed
  through as Value::Null, whose uninit payload word the typed call
  site adopted as an ArcStr) in one run.
- `GRAPHIX_DBG_TVAL=1` — print every `TVal` render step (deref'd type
  + naked value) as the typed printer walks. The tool for "why did
  this value print in this form" — found the union-member selection
  picking the never() arm's ⊥-settled cell over the concrete member
  (jul19f divergence_000000, the interp-vs-jit tuple-render split).
- `GXDBG_LETBIND=1` — print every `let` binding's publication decision
  (`LETBIND`: spec pos, production tag, whether the binding has ever
  published, frame depth, wake-hold, publishing y/n). The tool for
  "does this binding's value ever reach the store" — showed the
  arm-local that never published inside a recursion frame
  (`findings/arm-local-bind-aug2026/`, 2026-08-14). Pairs with
  `GXDBG_REF=1`, which shows the resulting read MISS.
- `GXDBG_SLOT=1` — print each collection cycle's per-slot production
  tag (`SLOT call[i] produced tag=..`) and the resulting fold decision
  (`SLOT map prod=.. resized=.. forced=.. poisoned=.. slots=[..]`).
  The tool for "why did this map/init/find/filter fire (or not)" —
  found the `merge_tag` fired-bit loss in one run by showing
  `call[0] produced tag=64` (fresh bottom) against `prod=Some(96)`
  (standing), which is the whole bug.
- `GXDBG_RESOLVE=1` — print every static-resolution read (`RESOLVE`:
  spec, BindId, unstable/b2l/cached hit), the index writes
  (`B2L-INS` at Bind tc0, `B2L-PROXY` at interface re-export
  bridging), and `RESOLVE-DISCARD` when a static bind is dropped back
  to dynamic on `AbstractOpaque`. The tool for "why didn't this call
  site statically resolve" — found the batch-entry
  `bind_to_lambda.clear()` that made shell fusion a race (the jul12
  resolution flap).
- `GRAPHIX_DBG_PERF=1` — cumulative runtime-lazy-bind phase counters
  (bind/setup/typecheck1/analyze/transient-gate times, prime/replay
  times, park delete/refs times), dumped to stderr every 250ms by a
  background thread (`perfdbg.rs`). The tool for "why is the interp
  slow on re-fired lazy binds" — found BOTH jul22b transient-recursion
  perf dragons (prime-park thrash + the `lambda_defs`/`LambdaIds`
  typecheck1 degradation) via growth-law analysis of the dumps.
- `GRAPHIX_DBG_CYCLE_BT=1` — print a backtrace at every
  `cycle_refused` mark (the occurs-refusal poison bit, both the
  `mark_cycle_refused` sites and the TVar×TVar positional guard).
  The tool for "which walk refused this merge" — established that
  the jul22e flap class's marks are channel-indistinguishable from
  genuine infinite types (~5% name-walk, rest positional), killing
  the scoped-aliasing remodel in an hour (see
  design/tvar_constraints.md's 2026-07-22 note).
- `GXDBG_RPC=1` — trace the whole sys::net rpc path (`RPCDBG`:
  server proc publish/republish, client call start + reply, NetState
  pump receipt, PublishRpc queue/dispatch/reply/sleep). Lives in
  graphix-package-sys (netstate.rs `rpc_dbg()` + net.rs). The tool
  for "where did this rpc call stall" — found the netidx publisher
  receipt/read deadlock behind the net_rpc0 flake (2026-07-23,
  netidx aede75e6): combine with `RUST_LOG=netidx=debug` +
  `--log-dir` to see the subscription/durable-retry side.

### Type Alias Expansion in Contains

When `contains` encounters a `Type::Ref` (e.g. `Result<T, E>`), the Ref case at `contains.rs:56` expands both sides via `lookup_ref(env)` before recursing. This means TVar bindings established during `contains` store the **expanded** form (e.g. `[T, Error<E>]` instead of `Result<T, E>`). Code that inspects resolved types must handle both the `Type::Ref` form and the expanded `Type::Set` form — see `extract_cast_type` in `graphix-package-core/src/lib.rs` for an example.

### Env-independent TypeRefs (carried resolution cells)

`TypeRef` carries a write-once `Arc<Mutex<Option<Arc<ResolvedRef>>>>`
cell caching its NAME resolution (`design/env_independent_typerefs.md`,
2026-07-14) — a ref first resolved in its native env becomes an
env-independent value, so retained instance signatures stay
NAME-COMPRESSED instead of being eagerly expanded (the expansion was
the 41GB GUI wedge and the `contains` exponential residual; both gone,
GUI suite 163/163 in ~5s). Rules that matter when touching types:

- The cell is params-independent — rebuilds use `TypeRef::with_params`
  (SHARES the cell; `reset_tvars`/`replace_tvars` copies must keep
  seeded cells) vs `with_scope` (fresh — scope changes the resolution).
  Never overwrite a filled cell; contexts needing a different view
  rebind (`rebind_resolution`, fresh pre-filled cell).
- Seeding is LAZY by default — a fill is correct only when the
  resolving env holds the name's FINAL target, and mid-compile envs
  are truncated by registration order (eager transitive seeding
  captured the list PACKAGE's `List` for tui's `list::List` submodule
  ref; removed twice). `Type::seed_refs` (explicit transitive walk)
  runs only at provably-safe times: `check_sig`'s registry copy
  (post-module-body) and the privatize walk's rebinds (typecheck1).
- `same_def` (structural — gates the Ref×Ref name fast paths via
  `cells_agree`) vs `same_view` (body ALLOCATION identity). The
  privatize walk rebinds a ref to the env's allocation only on
  same-def/different-view divergence (interface typedef bodies are
  registered twice, equal-but-differently-viewed); a DIFFERENT def in
  the env is a stale-horizon artifact and the cell wins.
- Typecheck-side bridging is `fusion::lowering::privatize_type` —
  name-preserving, same-size output (setup_static_bind against
  `f.env`; check_instance_type's AbstractOpaque retry against
  `ctx.env`). The fusion-side `resolve_abstract` (capped, expanding)
  is unchanged; `freeze_for_abi`/`abi_kind` expand refs env-free
  through the cell (`TypeRef::expand_cell`), unfilled → de-fuse.

### Two-Phase Typecheck

Every node implements `typecheck0`/`typecheck1` (two passes over the whole
graph). `typecheck0` also builds `ctx.bind_to_lambda`; `CallSite::typecheck1`
pre-binds statically-resolvable calls (`try_static_resolve`) and re-drives the
bound instance's body typecheck with the call's fn-typed args registered
(per-callsite elaboration), so calls to a lambda *parameter* resolve statically
inside each instance. The old `NeedsCallSite`/deferred-check machinery is gone
— a builtin that needs call-site types reads them from its `typecheck1`
`resolved` argument.

### Collection intrinsics (MapQ/FoldQ as compiler nodes)

The Array/List/Map traversal HOFs are compiler-owned Nodes
(`node/collection.rs`, `design/collection_intrinsics.md`). The stdlib `.gx`
signatures are ordinary lambdas whose builtin-reference bodies use reserved
marker names (`'array_map`, `'list_fold`, `'map_filter`, …);
`CollectionIntrinsic::from_name` intercepts those names during lambda
construction (before the registered-builtin table — `register_builtin` rejects
them) and builds a `MapQ`/`FoldQ` node as the lambda's body
(`LambdaDispatch::Collection` — the dispatch charges no call-depth unit; only
the per-element callback dispatch does). The node owns callback instantiation
(one prototype CallSite for typecheck/analysis/emission + one live CallSite per
collection position at runtime), slot identity and prefix retention across
resizes, per-slot firing/taint/sleep/replay, and result construction. Effect
inference needs no HOF special case: the prototype's CallSite is a normal call
site, so an async callback flips the collection lambda Async through the
ordinary M6 fixpoint.

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
- **Fusion → cranelift JIT** (`fusion/`, emitter in `fusion/emit/` — split
  per area: `jit`/`lower`/`abi`/`body`/`nodes`/`flow`/`select`/`call`/
  `scalar` + `scaffold`, façade re-exports in `emit/mod.rs`) identifies
  sync (pure) subtrees and compiles them to native kernels. **Success → splice
  the kernel + delete the originals; failure → don't splice, the originals
  node-walk.** There is no third evaluator.

**The pipeline is `Expr → node graph → CLIF`.** The node graph IS the IR: each
node's `Update::emit_clif` emits its own CLIF (`Apply::emit_clif` for builtins;
`MapFn`/`FoldFn::emit_clif` + the `fusion::emit::scaffold` loop scaffolds for
HOFs).
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
- **Bottom** ("no value this cycle" — div0, `?`-error, a bottomed input, a Sync
  builtin producing nothing) is DENSE since the 2026-08 flip
  (`design/dense_delivery.md`): `Update::update` returns `&TagValue` every
  cycle — `Fired(v)` / `Stale(v)` / `FreshBottom` / `StaleBottom` (the
  orthogonal fired×bottom algebra; `TagValue::view()` is the consumption
  API). A standing bottom re-delivers `StaleBottom` and RIDES (never re-fires
  consumers); bottomness joins by OR over consumed productions (`Tag::join`).
  In the JIT the same bits ride each param's disc (#219's taint channel:
  bottom = TAINT bit + a helper-safe placeholder payload; `propagate_taint`
  through pure ops; TAINT|STALE for standing bottoms so loop/select machinery
  doesn't fire). A **pended DynCall** (the builtin produced no value —
  `buffer::encode`'s Pad guard) taints at the site and continues;
  `DYNCALL_PENDING` reaching `Kernel::update` means only a GENUINE
  whole-kernel abort (interrupt poll, return-gate force, callee abort). A
  DEPTH TRIP is a delivered FreshBottom since 5c, not an abort
  (`findings/depth-trip-delivered-bottom-aug2026/`).
- **Bottom never reaches builtin authors** (Q1, BOTTOM PROPAGATES —
  the dense evolution of the 2026-07-19/20 taint-gate rulings): a
  bottomed arg (fresh, standing, or phantom) makes the wrapper bottom
  the invocation WITHOUT calling `eval` (`CachedVals::any_bottom` in
  `CachedArgs`/`CachedArgsAsync`; `FreshBottom` iff a delivery
  triggered, else `StaleBottom`). Raw-Apply authors read args through
  `seam_arg`/`seam_tick`/`seam_value` (package-core), whose bottom
  arms are None/no-tick. The old `gate_tainted_args` CallSite silencing
  and the DynCall absence-tombstone adapters died at the 5b/5c flips —
  bottoms flow IN-BAND with honest tags on both engines. The jul30a
  re-woken-arm ride and `array::window` []-on-absent pins were
  RE-BLESSED as ruled deltas.
- **THE ORGANIC FIRING RULE** (Eric's ruling 2026-08-14,
  `design/organic_firing.md` — BUILT same day, P1 interp 9be11267 /
  P2 kernel 0d8a561c+6c0fcbe9; SUPERSEDES the 2026-08-06 strict
  select rule, the 2026-07-15 per-slot firing rule, the guard-quiet
  rule, and cceb0809): **a node fires iff a consumed input fires** —
  no node stores a previous value or selection to decide a tag.
  `uniq`/`filter`/`~` are the explicit cadence tools; the compiler
  never gates firing on value or selection identity. A select emits
  per fired input (scrutinee delivery, guard production, or the taken
  arm's own production — `own_fired` in node/select.rs; the
  scrutinee/guard STALE folds at the kernel merges), same-arm
  re-matches emit the arm's current value, select-as-sampler is legal
  again, and an untaken arm's body is NOT a consumed input (arm sleep
  quietness survives — `guarded_select_firing_count`). Selection
  memory survives ONLY for sleep/wake routing and the arm-lift
  re-seed (the per-instance word — the one remaining select memory
  claim; no word → de-fuse). The tail-spine fold is now just the
  general rule's plumbing (the accumulator carries own-fires across
  frames; the `tail_sel_path` machinery and all no-memory de-fuses
  are deleted). Calls fire organically — the body's selects fire per
  delivery, so recursion fires like the hand-inlined chain with ZERO
  machinery (`|n| i64:7` still fires once: consts fire at init only).
  The ruled deltas + the red→green fixture protocol live in
  `organic_deltas.rs`; the design doc holds the deletion inventory.
  **THE SCRUTINEE RIDE** (Eric's ruling 2026-08-07, aug06ghz0 — the
  bottom axis, UNTOUCHED by organic firing): the standing selection
  lives on against the select's CACHED scrutinee when a delivery
  bottoms upstream — a bottomed delivery is NOT an own-fire, the
  taken arm's body fires on its own deps, guard-dep fires RE-MATCH
  against the cached value, and pattern binds RIDE it. Kernel:
  `emit_scrut_ride` (select.rs) substitutes the cached scrutinee
  (disc|STALE) on tainted-with-history — which is exactly why the
  organic stale fold stays quiet on rides; no-history taint still
  misses (the aug04b phantom rule). Value/composite residents ride at
  region root only; NO storage → DE-FUSE, never pass through (Eric's
  bar 2026-08-07). ASPIRE: value residents in site blocks to restore
  instance-kernel fusion for value-shaped scrutinees
  (`hof_nullable_map`). A select whose taken arm is bottom emits
  FreshBottom per fired input (op-consistency; not
  language-observable — delta 4).
- **THE RECURSION RULING (Eric, 2026-08-13; firing clause amended by
  organic firing 2026-08-14):** recursion fires like the hand-inlined
  chain of distinct functions — under organic firing this holds with
  ZERO machinery, since both fire per delivery (the original
  "unchanged inputs are not an event" clause and its entire
  derivation-changed apparatus — the per-site scalar-formal memo,
  wire slot 3, `KernelSig.has_self_call`, the interp's entry-args
  memo — are REPEALED and deleted; the const-terminal witnesses that
  forced the question now agree per-delivery,
  `findings/organic-firing-aug2026/`). STRUCTURE (Eric's call,
  unchanged): transient instances are RETAINED unconditionally — no
  park, no budget, no snapshot/rebuild ("let the user run out of
  memory; you can't fix stupid"); the delete-park/SelSnap/prime
  machinery is DELETED. fib(24): 110s/111MB retained vs 121s/85MB
  parked. Fuzz children run under an 8GB RLIMIT_AS
  (`GRAPHIX_FUZZ_MEM_LIMIT`) since retention lets fib-tree subjects
  legitimately eat memory.
- **The 2026-08-07 review arc** (Opus multi-agent review, 726eeb1c —
  18 finding dirs; 14 classes fixed same day, `f438e1bd..369fa71c`):
  (1) GUARDS tick per-invocation via a PROLOGUE in `emit_select_arms`
  (the interp ticks every arm's guard every cycle; lazy chain
  evaluation desynced their operand caches — guard-shortcircuit).
  Binds install taint-masked by the arm's own pattern cond. (The
  original "guard discs never fold into the result" rule is REPEALED
  by organic firing 2026-08-14: prologue guard STALE bits now AND-fold
  into the emission — `guard_stale` in emit_select_arms.)
  SCHEDULE-FREE guards (pure never-bottom fns of the arm's own binds —
  cmp/logic/wrapping-arith over binds+consts, `guard_schedule_free`)
  stay lazy in the chain: still observably equivalent under organic
  firing (their inputs are scrutinee-derived binds, covered by the
  scrutinee fold), and the blanket prologue cost symbolic +58%.
  (2) A fused DynCall delivers non-fired args as `TagValue::stale`
  (a STALE mask beside the taint mask) — never absence (all-const-arg
  builtins must keep producing) and never `fired` (rand
  re-randomized, `now` resampled per invocation). TAG-BLIND builtins
  (`printfn!`, `now` — gate on `Some(_)` not `triggers()`) remain the
  open ruling.
  (3) The VALUE taint cache's no-storage path REFUSES (de-fuse) per
  the storage law, EXCEPT tail-position producers (body tail-leaf
  ids in `LowerCtx::tail_leaves`): a tail result's ride belongs to
  the caller, so pass-through is exact there. Cost: 13 fixtures
  interpret (dyncall chains, string HOFs, `str::split` family —
  `FuseExpect::None` + ASPIRE comments); the ASPIRE value residents
  restore them.
  (4) `abi_kind`'s option/result collapse was the root of TWO bugs:
  select type predicates over `[T, Error<E>]` lower as a POSITIVE
  disc test (`nullable_error_marked`), and the qop scalar arm routes
  owned errors through `emit_qop_error_disposal` (the leak).
  (5) Variant tag tests enforce representation AND arity; kernel
  cache keys include the instance body's CATCH COVERAGE (`__covN`
  symbol variants); sig-less modules refuse emission (structure, not
  computation); `freeze_for_abi_normalized` never normalizes shared
  tvar cells (mode-identical `--check`, gated by
  `check_mode_parity`); narrow index/slice bounds widen; over-limit
  `array::init` is bottom-with-retained-state on BOTH engines;
  `str::sprintf` declares `Result<string, `FormatError(string)>` and
  shape mismatches warn loudly. AWAITING ERIC: the
  `fuzz/pending-ruling/` classes (tail-zero-iteration-fire,
  rec-prev-looped-arming — node-walk drops the event;
  module-state-callee-reactivity — is a cross-module callee's read
  of module state quiet-in-steady-state/fresh-at-instantiation, the
  status-quo interp artifact of the Module proxy post-pass strip, or
  fully reactive?) + the tag-blind builtin gate. P8 re-adjudication
  under dense (2026-08-13): all four of those classes AGREE and are
  promoted to `findings/` (module-state resolved by Q3
  fresh-at-instantiation; tag-blind unwritable by construction;
  missing_fire_epoch3 fixed by the 5c depth-trip-delivers-bottom
  split → `findings/depth-trip-delivered-bottom-aug2026/`).
  tail-zero-iteration: ruled quiet 2026-08-13 (cceb0809), then the
  whole family REVERSED by organic firing 2026-08-14 — same-args
  re-dispatches FIRE at any iteration count now (delta 6; the
  tail-zero pins carry superseded-cadence banners). DEPTH-TRIP SCOPE
  RULED 2026-08-14 (Eric): WHOLE-DERIVATION — a trip bottoms the
  entire call at the root with log::error at the trip (both engines;
  `ctx.depth_tripped` poisons the interp's unwind rides — scrutinee
  ride refuses, tainted guard's held ride reads false — cleared at
  pop-to-zero; the kernel's value-level trip propagation with no
  interior ride storage already implemented it, it only gained the
  log). Pins: `findings/depth-trip-whole-derivation-aug2026/`.
  fuzz/pending-ruling/ is EMPTY — no rulings outstanding.
- **Sleep is PAUSE, not reset** (Eric's ruling 2026-07-31, soak jul30a):
  value-channel state survives an arm's sleep — `Held` residents (the
  three designated ride sites), `CachedVals` staging slots, collection
  slot values/acc-carries — so a deselected-then-reselected arm whose
  fresh computation bottoms RIDES its history, exactly like the
  kernel's replay words / DynCall slots / per-slot state words. Those
  were persistent only because nothing fused under a sleep initiator;
  **arm-region fusion (2026-08-14) made `Kernel::sleep` live and it
  was still CLEARING them** — the interior-bottom taint caches
  (replay words, owned value pairs, per-slot reset chains) now survive
  sleep too, and only `reset_replay` (frames) and `Drop` clear them
  (`findings/sleep-preserves-caches-jul2026/03`, the kernel face of
  the July pair). **A select arm's own `let` bindings obey the same
  rule** (`findings/arm-local-bind-aug2026/`, 2026-08-14): an arm's
  WAKE resumes the arm, it does not create one, so a binding that is a
  `<-` target and already holds a value is NOT reseeded by its own
  re-fired initializer — `Event::wake_init` flags the wake so the
  init view stays real for everything whose init handling is its own
  machinery (fused kernels marshal their inputs off it, call sites
  prime, refs read standing entries as Fired). The opposite face: a
  PRODUCER must materialize its value channel on its FIRST
  production whatever the tag — `Bind` publishes a quiet first
  production, and `CachedArgs` runs `eval` once when its result slot is
  still the phantom — because inside a frame `Constant` delivers STALE
  by design, so a never-yet-computed subtree fed only by constants has
  no triggering input and would produce nothing at all. These are VALUE
  rules: firing a wake's constants instead re-emits provably-unchanged
  outputs (`findings/tail-jump-honest-tags-jul2026/00`). The
  documented arm-rewake RESTART
  semantics (`once`/`take`/`skip`/`uniq`/`hold`/`count` clear on
  sleep) are unchanged; since the P7 Sync flip these builtins DO fuse
  at region root, and the `SLEEP_RESTARTS` interior-sleep gate
  de-fuses any select whose arm reaches one (kernels have no per-arm
  sleep initiator — `findings/sleep-restart-gate-aug2026/`). Pinned by
  `sleep-preserves-caches-jul2026/`.
- **DynCall SITE IDENTITY** (2026-07-25, soak jul23f): the ridden
  state must be the call site's OWN history — a compiled callee
  body's interior builtin is ONE `graphix_dyncall` instruction
  reached from every caller emit site, and the previously-shared
  single inner Apply let a masked delivery resurrect ANOTHER site's
  cached args. Each emission site now claims one identity word
  through the select-state channel (region root → instance word,
  callee root → per-call-site block word), `graphix_dyncall` carries
  its address, and the dispatcher mints an id and keys a full inner
  Apply per site (`DynCallSlot.instances`) — cache AND builtin state
  get the interp's per-callsite identity. Key 0 = the shared legacy
  bucket: scaffold-loop sites (v1, keeping the init-mask
  approximation), recursive back-edges, qop-deliver.
  `design/kernel_instance_state.md` "DynCall site identity"; pinned
  by `dyncall_site_identity_state` +
  `findings/dyncall-site-identity-jul2026/`.
- **A program may spin forever inside one cycle, on BOTH engines** — an
  infinite tail recursion is the constant-stack, bounded-memory case.
  This is semantics, not a JIT artifact (Eric's ruling 2026-08-15,
  `design/atomic_recursion.md`): recursion fires like the hand-inlined
  chain, an inlined chain is one expression, and an expression
  evaluates atomically — so evaluation cannot be paused mid-derivation.
  The old per-eval-per-cycle model made wedges impossible but made
  recursion observable (the inlined twin finished in one cycle, the
  recursive one took N) and capped a JIT loop at one step per cycle;
  iteration credits would be worse still — the same input would run in
  one cycle or many depending on its size, observably, and every
  credit constant would have to be replicated bit-identically in both
  engines or become a trace divergence. What remains is CONTAINMENT,
  which lives outside the language and which no program can observe:
  the cooperative interrupt (`GXHandle::interrupt`), polled by the
  interp's tail driver (`node/lambda.rs`) and at every emitted loop
  head (`emit_interrupt_check` — the tail rebind-and-jump head in
  `emit/lower.rs` plus all eight HOF scaffolds). The shell arms it on
  Ctrl-C and `abort()`s on the way out; an embedder wanting a
  slow-program watchdog arms `interrupt()` on a wall-clock timer.
  Pinned by `lib_tests/interrupt.rs` (both engines recover) and
  `graphix-shell/tests/interrupt_wedge.rs` (the process stays
  killable by Ctrl-C).

**Per-cycle firing (the STALE fired-bit):** a fused kernel must replicate the
node-walk's non-async firing — an output fires only when an input that feeds it
actually fired this cycle. A "fired-this-cycle" (`STALE`) bit rides each kernel
param's disc; a lifted let-bound `connect`-target counter is threaded in as a
kernel input so reactive counters fuse. Collection-loop firing is
`scaffold::SlotFlags`: per-iteration body discs fold into a slots word, a
per-instance state word holds the previous source length for exact resize
detection, and `apply` reproduces the interpreted MapQ/FoldQ rule (fires iff
resized ∨ a slot fired ∨ the source fired empty; fold results are acc-carried
via `result_is_firing`). A same-length source refresh with a quiet body does
NOT re-fire — the per-slot precision the P4 sequential loops had lost.
Selects need NO firing memory since organic firing (2026-08-14 — the
2026-07-15 per-slot firing rule is repealed): the emission folds the
scrutinee's and prologue guards' STALE bits at every merge, in loops and
callees alike. The per-slot chain machinery (`graphix_slot_state_table` +
`own_levels`, `SiteAnchor`/`SiteLeaf` recursive free, `Kernel::drop` via
`WrappedKernel::slot_table_words`) SURVIVES for its other consumers:
SlotFlags' nested-loop exact prev-len words and in-loop callee SITE BLOCKS.
CALLEE bodies keep PER-CALL-SITE state blocks (2026-07-16, wire slot 2): the
callee's claims are its `SiteLayout` (callees define before parents; a missing
layout = recursive back-edge, pass 0 + null-guards); callers allocate from
their own storage (root: contiguous words with anchor translation; in-loop:
chain leaves with `words` stride). Those blocks carry DynCall site identity,
first-dispatch init words, and prev-len state — never select firing memory.
Remaining select-adjacent item: arm-lifted connects in loops/callees still
de-fuse (the per-instance word is the one surviving select memory claim —
coverage, not correctness).

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
  contract so fusion binds them as region inputs). `minimize <file>
  [budget]` (budget = oracle checks, default 4000; the campaign's per-finding
  budget is `CAMPAIGN_MINIMIZE_BUDGET`) is typed-AST HDD whose working
  operator is the STATEMENT DROP, keyed at the statement so a whole round of
  disjoint reductions applies at once; whole-section drops, the body, and each
  `.gx` section's items lap until fixpoint (`design/graphix_fuzz.md` §6.1 —
  6157 → 199 bytes on the aug08d witness where the pre-2026-08-09 reducer got
  one reduction in 200 checks). `selfcheck`
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

**Collection HOF execution (compiler-owned nodes, 2026-07-13):** the `sync`
subset, the `For` node, the sync desugar, and the in-language HOF bodies are
all DELETED (`design/collection_intrinsics.md`; the P4 arc concluded the sync
subset of Graphix is Rust). MapQ/FoldQ are back as the canonical per-slot
interpreters — but as compiler Nodes (`node/collection.rs`), not package
builtins. Fusion: `GXLambda::emit_clif` (the `Apply` hook, consulted FIRST at
`CallSite::emit_clif`) recognizes a collection-bodied callee and inline-emits
the loop at the call site via `MapQ/FoldQBase::emit_clif_call`, swapping the
callsite's actual source/init arg nodes for the lambda-param references —
supported Array shapes compile through the per-op `MapFn`/`FoldFn::emit_clif`
impls into the `scaffold::emit_{init,map,filter,filter_map,flat_map,find,
find_map,fold}_loop` emitters; refusal leaves the node intact on its
interpreted per-slot semantics (async callbacks always interpret). **List and
Map HOFs lower too (2026-07-14)** via the FLATTEN boundary: the collection
Value crosses through `graphix_list_to_valarray`/`graphix_cmap_to_pairs`
(consuming; canonical `list::*`/`make_pair` seam — one semantic seam with the
interpreted finishes), the ARRAY scaffold loop runs unchanged (the SlotFlags
rule over the flattened length IS the interpreted ordinal-slot rule), and
collection results rebuild at `graphix_valarray_into_{list,cmap}`.
Prerequisite: recursive types freeze to an OPAQUE LEAF
(`freeze_for_abi_d` Seen-hit returns the matched outer ref, params frozen,
256-chain backstop) so a List crosses kernel boundaries as a 2-word Variant
and list-typed DynCalls (`from_array`/`to_array`/`cons`/...) register.
`FoldAcc::Value` carries Value-shaped ACCUMULATORS (owned two-word loop
slot, real disc carried whole with TAINT|STALE in the tag bits) —
nullable max-by and map group-by folds fuse; the cons-building reverse
still interprets (abstract-id identity mismatch at the prototype's
return check — two AbstractIds both denoting list::List; pinned by
`list_fold_list_acc_interprets`). The entitled abstract bridges:
`BuiltInLambda::typecheck0` and `CallSite::typecheck0`'s per-arg checks
(`check_site_arg`) retry through `privatize_type` under the CALLEE
DEF's scope on `AbstractOpaque` (a def sees through its own
signature's abstracts; privatized instance signatures mix private
forms into outside-module callback bodies by design).
Benches: `list_fold_sum` 151x, `list_map_fold` 142x — the ~15x gap to the
array twins is the cons representation's per-element allocation, not loop
overhead. `for_each_emitted_node` descends
collection callback bodies during discovery so callee kernels and DynCall
slots inside callbacks are found. `find`/`find_map` scan ALL slots in both
modes (a bottom predicate after the match bottoms the find — pinned by
`find_bottom_after_match`); the P4 sequential early-exit is gone with the
sequential semantics. Cross-kernel call sites force the callee's init flag on
the first call ever (a state word — the kernel mirror of `Callee::Static`'s
`first_update` priming).
Durable notes from the P4 arc: (1) the SHELL resolution flap is FIXED
(2026-07-12) — RT batch entries prune only the outgoing batch's `<-` targets
(`unstable_bindings`) from `bind_to_lambda` instead of clearing it, and
`Bind::delete` removes its ids; the racy `rt.cached()` fallback REMAINS for
destructured/`<-`-retarget shapes the index can't know — flagged for review.
(2) builtin-bodied lambdas' `intrinsic_effect` is read from `BuiltinFacts`,
not constructed `Sync`.

**JIT memory lifecycle (settled with Eric 2026-08-06; GENERATIONAL
since 1d1bf215):** one active JITModule + 256MB arena per ExecCtx
(colocation is correctness: cross-kernel calls are ±2GiB PC-relative).
Individual kernels are never freed within a generation, but exhaustion
is no longer a perf cliff: the active Jit RETIRES whole into
`FusionCtx::retired_jits` (kernels stay mapped and executing) and the
region build retries once in a fresh module. Soundness: direct
kernel→kernel calls exist only WITHIN a region build, so "a region
builds atomically in one generation; generations never link" — a
post-rotation region recompiles its transitive callees into the fresh
module's empty by_kernel cache. Recompile-heavy sessions (dynmod
hot-reload, long REPL/plugin) accumulate one resident arena per
rotation — warn-once log per rotation + pollable
`FusionStats::jit_generations`; the reclamation unit is the ExecCtx
(drop frees every generation; `reset_jit_for_check` ditto — the
embedder contract for plugin daemons). `GRAPHIX_JIT_ARENA` (bytes)
shrinks the arena so the differential gates exercise rotation
(regress at 65536 forces it corpus-wide).

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

Measured by the FuseExpect audit above (numbers last measured pre-collection-
intrinsics, 2026-07-08 — re-run the audit for current figures): **~71% of the
`run!` corpus fuses+JITs, and all bench programs (`bench/`) fuse fully.** The
value-computing vocabulary is essentially complete:
all scalar arithmetic/comparison/logical/cast/checked-arith, every producer
(struct/tuple/variant/array/map-literal incl. `{s with f: v}`) and accessor
(field/index/slice/`m{key}`), `?`/`$`, all eight array HOFs as native loops
(map/filter/flat_map/filter_map/find/find_map/fold/init — over scalar, composite,
**String, and value-shape elements**, with `|(k,v)|` destructure leaves of any of
those shapes, and HOF-of-HOF fused into one multi-loop kernel; **fold
accumulators may be composite or string, not just scalar** — tuple/struct/array/
string accs carry loop-OWNED with clone-borrowed/drop-replaced discipline, acc
patterns may destructure (`|(a, b), v|`), and the freeze authority is the
RESOLVED acc type from the prototype callback's `typ().rtype`, since an
instance's `body.typ()` re-mints generalized tvars unbound), **`select`
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
builtins (timers, IO, netidx, `never`, `queue`, `throttle` — the
once/take/skip family went Sync at P7 and fuses outside select arms), cross-cycle
nodes (`~`, `Any`, `Catch`'s handler-read), and non-register-encodable types
(`decimal`, `Fn`, `Ref`, recursive `List`/ADTs — no fixed ABI layout — and unbound
TVars). Fusion recursion (`Update::fuse`) descends through
Module/Block/Bind/CallSite/Catch/Lambda, and since 2026-08-14 (Eric's
attribute-honesty arc) ALSO Select (scrutinee, guards, and each arm body get
their own region passes — a fused arm is a `FusedKernel` in arm position;
sleep/wake and the wake-forced `event.init` compose with kernels already) and
ExplicitParens (the interior gets its own pass — `clock ~ (a + b)` fuses the
`a + b` where the parens are reachable). Constant arm/guard bodies are
skipped (0-input kernels are pure overhead). Still NOT descended: `~`, `<-`,
and operator operands — a sync expression there fuses only as part of an
enclosing region; a REGISTRY attribute in such a position is a loud compile
error (the honesty census below), never silently unchecked.

The remaining missed-fusion tail (each pinned by a `#[native]` de-fuse test or an
ASPIRE comment where noted):

1. **HOF callback calling a nested/rec local lambda** — simple captured-local
   calls now resolve (per-callsite elaboration + `for_each_emitted_node`
   discovery), but a rec callee inside a fold callback still keeps the
   collection on the node-walk (pinned by `fold_callback_name_collision`,
   FuseExpect::None).
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

- `atomic_recursion.md` — **RULED 2026-08-15:** function evaluation is
  atomic within a cycle, so a program may legally spin forever inside
  one — the no-wedge property of the old one-eval-per-cycle model is
  given up, because atomicity follows from the recursion ruling
  (recursion fires like the hand-inlined chain; an inlined chain is one
  expression) and iteration credits would make the same input run in
  one cycle or many depending on its size. Containment lives outside
  the language: the cooperative interrupt, armed by a human or an
  embedder, observable by no program. Holds the trilemma argument, the
  loop-head poll inventory, and the shell's cancel path.
- `organic_firing.md` — **BUILT (P0–P3 landed 2026-08-14, one day;
  P4 fresh-clock soak remains):** the fired-plane simplification — a
  node fires iff a consumed input fires; no stored value/selection
  gates a tag; `uniq`/`filter`/`~` are the explicit cadence tools.
  Holds the ruling arc (const-terminal witness → "change the
  semantics" → fired-args recursion → fire-on-discriminant selects),
  the 9-item ruled-delta list, the deletion inventory, and the
  red→green/desync-enumeration migration record. Supersedes the
  strict select rule, the per-slot firing rule, the guard-quiet rule,
  the recursion ruling's unchanged-inputs clause, and cceb0809 —
  fired plane only; the bottom/ride axis is untouched.
- `dense_delivery.md` — **BUILT (P0–P8 landed 2026-08-13; P9 soak/merge
  remains):** the dense-delivery redesign — `Update::update -> &TagValue`
  (borrowed production, no Option), orthogonal fired×bottom tag algebra,
  `TagView` exhaustive-match API, persistent tagged store (rt.cached is
  GONE — `Rt::store_value` is the one cross-cycle read, bottom ⇒ None),
  consumer caches deleted (`Held` survives at the 3 designated ride
  sites: select scrutinee, pattern guard, `~`'s arg), Q1
  bottom-propagates at builtin seams (the sparse view is
  UNREPRESENTABLE — `seam_arg`/`seam_tick`/`seam_value` +
  `CachedVals` staging), log-everywhere, the P7 Sync flips + the
  `SLEEP_RESTARTS` interior-sleep gate. Holds the rulings, the
  ruled-delta list, the per-phase as-built records + gate records, and
  the tag-removal post-mortem (removal is foreclosed — do not attempt
  again). Supersedes `replay_frames.md`'s delivery model (its
  reset_replay classification + frame mechanism remain).
- `final_jit_architecture.md` — the end-state architecture (`Expr → node graph →
  CLIF`), now realized.
- `distributed_jit.md` — how the GIR IR was removed and fusion distributed as
  `emit_clif`/`fuse` per node; holds the emit contracts and the ABI-contract
  rationale.
- `representable_bottom.md` — bottom semantics (the taint channel).
- `graphix_fuzz.md` — the differential fuzzer.
- `collection_intrinsics.md` — **current:** the Array/List/Map HOFs as
  compiler-owned Nodes (reserved marker names → `CollectionIntrinsic` →
  MapQ/FoldQ nodes; per-slot interpreted semantics + inline CLIF loops).
  Supersedes the sync subset, `value_returning_loops.md` (planned, never
  built), and the `clone_rebind` machinery.
- `impure_hof_fusion.md`, `composite_hof_fusion.md`, `clone_rebind_testing.md` —
  SUPERSEDED (historical): the per-slot template / `clone_rebind` era.
- `queue_fn.md` — `queuefn` feature design.
- `replay_frames.md` — **BUILT (2026-07-11), v2 same day:**
  `reset_replay` (required `Update`/`Apply` method, replay caches vs
  semantic state) + evaluation FRAMES (tail-loop jumps run against a
  private variables map) + **TagValue as
  the interpreter currency** (Eric's call; v2): `Update::update`
  returns `Option<TagValue>` and `Event.variables` carries it — the
  kernel's STALE/TAINT disc bits ride every interp value, ops
  propagate them per the CLIF rules, `Apply::update` stays clean
  `Value` with `Apply::out_tag` surfacing the tag, and the kernel
  gains a `last_result` value-channel slot. The v1 `frame_bottom` bit
  and the fired re-delivery hack are deleted (jul10e broke both
  within an hour of soaking).
- `sync_subset.md`, `sync_control.md`, `value_returning_loops.md` —
  SUPERSEDED (historical): the `sync { }` block prototype (P0–P3 built
  2026-07-09, P4 2026-07-10) and the never-built generalized-loop plan.
  Unwound 2026-07-13: modeling slot lifetimes showed a reactive
  collection HOF is a live per-position subgraph, not a sequential
  loop — the sync subset of Graphix is Rust. See
  `collection_intrinsics.md`. Per-callsite elaboration (the P4 gate)
  survives and is load-bearing for collection callback resolution.
- `fusion_lowering_split.md` — **proposed, not built:** split `try_fuse`'s welded
  analysis+lowering into a pure analysis pass (color nodes with a `KernelId`,
  build per-kernel descriptors) consumed by a thin lowering pass. Motivated by
  legibility.
- `type_operation_scaling.md` — **built (2026-07-13):** COW/DAG walks +
  per-pass memos for every core type operation (the six tree-walk
  explosions the static-instance wedge exposed); holds the "invariants
  for future type walks". Its open `contains` residual is RESOLVED by:
- `env_independent_typerefs.md` — **built (2026-07-14):** `TypeRef`'s
  carried resolution cell (Eric's ruling) — name-compressed,
  env-independent instance signatures; the privatize walk; seeding
  invariants; `same_def`/`same_view`; freeze/abi_kind cell Ref arms.
  Carries one open finding: the two `#[native]`-in-List-callback tests
  passed vacuously and now fail honestly (pending ruling).

## Stdlib package notes

- **`sys::process` draft (PR #13, `unified-fusion-proto`).** Managed child
  processes live in the opaque `Proc` value and use weak polling plus
  `kill_on_drop`; the public Graphix API provides `options` and `stdio` named
  argument constructors. Stdio is an explicit `Pipe`/`Inherit`/`Null` variant
  and defaults to `Inherit`. The polling task is the sole child reaper; `wait`
  subscribes to its watch status so concurrent waits and kill-during-wait work.
  Rust wire conversion uses `netidx-derive`; the one
  exception is `SpawnOptions.env`, because `immutable_chunkmap::Map` does not
  implement `FromValue`, so a derived wire struct validates and extracts its
  `Value::Map`. Shell-based tests are Unix-gated, with `cmd.exe` equivalents
  for stdout and exit-status behavior on Windows.
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
  widget's own dispatch). test contexts default to
  `NetConfig::Internal`, so a test that uses `sys::net` materializes a real
  in-process netidx on demand and round-trips work — but publisher coalescing means
  rapid updates collapse; space them with one-shot timers for multi-point tests.
