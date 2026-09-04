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
// line comments — legal ONLY on their own line directly above an
// expression, a select arm, an impl method, or a struct-literal field.
// Trailing (`x; // note`), interior (`1 + // n \n 2`) and dangling
// (before a closing `}`) comments are PARSE ERRORS.
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
"""bare " [ ] ok, splice \[x]"""  // TEMPLATE: brackets/quotes are content,
                                  // interp is MARKED \[expr]; \] is an error;
                                  // one newline after the opener stripped
r"verbatim"  r#"has "quotes""#    // raw: counted hashes, NO escapes at all
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
  {x, ..} => x                   // partial — completes from the scrutinee
}
// a partial matching SEVERAL union members must annotate the member:
// `S as {x, ..}`. An abstract type test `T as t` is a runtime tag
// comparison (legal on unions of abstracts); dissect [T, Error]
// unions with ? or $.

// array slice patterns — LENGTH coverage counts (2026-08-21):
// unguarded all-bind slice arms are exhaustive when their lengths
// cover 0..∞ (no wildcard needed), and a shadowed arm is a dead-arm
// error ([init.., x] after [x, rest..] both match every non-empty
// array; `_` after a complete ladder is dead too)
select arr {
  [] => 0,                       // empty
  [a, b, c] => a + b + c,        // exact length (order above the rest arm)
  [x, rest..] => x               // head + tail: every other length
}
// suffix form: [init.., x] binds init = all but last, x = last

// named capture
select val {
  x@ `Some(inner) => use_both(x, inner),
  _ => default
}
```

**Key**: unselected arms are put to sleep (subscriptions paused, no
computation). First matching arm wins.

### Seq — Straight-line ceremonies

`seq [trigger] { stmts }` runs the statements in order, one step per
async completion or connect. No trigger = once at init. A trigger
while a run is in progress is dropped. `until e` waits until a bool
level is true. `?` aborts the run (the block's `catch`, if any, is
cleanup; the error rethrows). Levels (`tui::suspend`, publish, …)
stay outside the block and are written from steps.

```graphix
let y = seq go {
  until ready;
  9
}
```

`if` and loops are not seq steps yet. Nested `select` is an ordinary
expression. The integer-sequence builtin is `range(i, j)`.

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

// catch(e: T) ascribes T to e; T must still cover every thrown error.
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

### Modules & Imports (Rust-2018-style, 2026-08-22)

`use` imports a NAME, not a module's contents. Every name in scope
arrived by a declaration, a `use`, or a prelude (core's root items;
installed package NAMES as path roots — `array::map` works bare-
qualified with no use).

```graphix
array::map(xs, f)                 // package prelude: no use needed
use array::map;                   // import the item
map(xs, f)                        // now bare
use str::join as sjoin;           // rename
use array::*;                     // glob (discouraged outside tests)
use tui::text::{self, *};         // widget-module idiom: the module
                                  // AND its contents (text(...) works)
use super::{helper, T};           // from the parent module
use package::a::b;                // from the current package root

mod mymod;                        // declare file-based submodule
```

Path roots (`self`/`super`/`package`/package names) work in
expression AND type positions (`super::m::f(x)`, `-> package::m::T`).
A submodule sees NOTHING of its parent implicitly — write
`use super::…` (privacy: parent private items ARE visible to the
subtree). `mod`/`use` position carries no visibility meaning.
Declarations shadow imports; imports shadow globs; two globs
providing one used name error at that use.

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

An abstract type is NOMINAL: a value is a box tagged with the type's
identity, minted only by the type's constructor (the type's NAME).
Declare it in the interface without a body; define it in the
implementation with an `Abstract<rep>` body — legal ONLY as the whole
body of a `type` definition (never `fn(x: Abstract<i64>)`). A type
hidden by a gxi must be `Abstract<..>` or Rust-backed (`type T;` on
both sides); hiding a transparent alias is an error.

```graphix
// counter.gxi
type Counter;                     // opaque — no definition exposed
val make: fn(initial: i64) -> Counter;
val get: fn(c: Counter) -> i64;
val increment: fn(#trig: Any, c: &Counter) -> null;
```

```graphix
// counter.gx
type Counter = Abstract<i64>;     // the representation stays private
let make = |x: i64| -> Counter Counter(x);            // construct: T(v)
let get = |c: Counter| -> i64 c.0;                    // payload: x.0
let increment = |#trig: Any, c: &Counter| -> null {
  *c <- Counter((trig ~ *c).0 + 1); null
};
let sign = |c: Counter| -> i64 select c {             // pattern: T(p)
  Counter(x) if x > 0 => 1, Counter(_) => 0
}
```

`T(v)`, `x.0` and the pattern `T(p)` (also irrefutable: `let T(x) = v`)
work only where the definition is visible — inside the defining module,
or anywhere for a PUBLIC newtype whose `Abstract<..>` body is in the
gxi (`type Meters = Abstract<f64>;`). The type test `T as t` works
everywhere, including over a union of abstracts. `.0` keeps the
payload's shape (`x.0.field`). Equality = same tag + equal payloads;
prints as `Counter(5)`. One allocation per construction — use for
handles and newtypes, not hot data. Parameterized: `type Box<'a> =
Abstract<{value: 'a}>`, `Box({value: x})`, `b.0.value`; constraints must
match the gxi (`type NumBox<'a: Number>;`).

### Traits (2026-08-22, Rust-style)

```graphix
trait Show {
  val show: fn(self) -> string;                          // required
  val twice: fn(self) -> string = |s| "[show(s)] [show(s)]"   // default
}
type Counter = Abstract<i64>;
impl Show for Counter { let show = |c| "Counter([c.0])" }
impl Show for i64 { let show = |x| "int [x]" }           // primitive: trait's package only
impl<'a: Show> Show for Array<'a> { let show = |xs| ... }  // parameterized head
impl Show for Counter;                                   // gxi: declares the impl

Show::show(Counter(1));  use Show::*; show(7); twice(Counter(1))
let f = 'a: Show |x: 'a| show(x);        // bound; resolved per instance
let g = |a: Show, b: Show| ...;          // ≡ 'a: Show, 'b: Show |a: 'a, b: 'b|
fn<'s: Read + Write>(s: 's) -> null      // `+` joins bounds
```

- `self` is the receiver TYPE in a method signature and the first
  positional param written bare (`fn(self, n: u64)`); `self` is also
  legal as an impl lambda's param name and in its body.
- Calls dispatch STATICALLY on the `self` argument's type; an unknown
  self type at a call is a compile error (annotate). A UNION self type
  compiles to a select over the members (each needs an impl).
- Trait methods are items under the trait's name (`Show::show`,
  `use Show::show`); trait NAMES are scoped like types, impls are
  global. Impl targets: an abstract type anywhere in the type's or the
  trait's package; any other type only in the trait's package; never a
  union; one impl per (trait, type).
- A trait in a non-parameter type position is an error. No trait
  params/associated types yet.
- CORE TRAITS (2026-08-23): `Eq { eq: fn(self, other: self) -> bool }`,
  `Ord { cmp: fn(self, other: self) -> Ordering }` (`Ordering =
  [`Less, `Equal, `Greater]`), `Display { fmt: fn(self) -> string }`.
  The impl rides the VALUE (the abstract-vtable seam): `==`/`<`/...,
  MAP KEYS (insert/lookup/order — a reversed Ord reverses the map),
  array::sort, min/max, uniq, interpolation/print/println/dbg/log all
  honor it, wherever the value sits (nested, under Any). Only
  abstract types may implement them outside core; methods implicitly
  `#[sync]`; they hold as bounds for EVERY type; `Eq::eq(a, b)` ≡
  `a == b`. A bottoming impl resolves per KEY like NaN (bottom keys
  sort below real ones, equal to each other). Maps consult Ord only;
  impls must be consistent total orders (Rust-style trust). The wire
  and the REPL echo stay structural. No `Hash`.

### Standard Library Quick Reference

**Always available (core)**: `print`, `println`, `dbg`, `log`,
`cast<T>(x)`, `error(v)`, `is_err(v)`, `filter(pred, v)`,
`filter_err(v)`, `count(v)`, `once(v)`, `uniq(v)`, `sum(v)`,
`product(v)`, `min(v)`, `max(v)`, `mean(v)`, `and(a,b)`, `or(a,b)`,
`all(v)`, `queue(v)`, `hold(v)`, `take(n,v)`, `skip(n,v)`,
`throttle(dur,v)`, `never()`, `range(start,end)`

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

**sys::io** (traits — `use sys::io::{Read, Write, Close, Lines}`):
`Read::{read, read_exact, read_all}`, `Lines::{lines, lines_batched}`,
`Write::{write, write_exact, flush}`, `Close::close`; plus
`io::stdin/stdout/stderr`. A stream's TYPE is its kind
(`sys::fs::File`, `sys::tcp::TcpStream`, `sys::tls::TlsStream`,
`sys::process::Pipe`, `sys::io::Stdio`) and the traits it implements
say what it can do; also `sys::fs::Seek::seek` and
`sys::tcp::Socket::{shutdown, peer_addr, local_addr}`. json/toml/pack/
xls parse `bytes`/`string`: `json::read(Read::read_all(f)?)`.

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
use gui::window;
use gui::text::{self, *};
use gui::column::{self, *};
use gui::button::{self, *};

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
use tui::{line, style};
use tui::list::{self, *};
use tui::block::{self, *};
use tui::text::{self, *};
use tui::input_handler::{self, *};

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
`calendar`, `browser`, `input_handler`, `overlay` (modal/popup stack:
`overlay(#layers: &Array<Layer>, base)` + `layer(#width?, #height?, child)`
— top layer captures input)

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
- Primitive type names (`duration`, `string`, `i64`, ...) are legal
  binding AND field names (since 2026-08-18); control keywords
  (`let`, `select`, `cast`, ...) and literal words (`true`, `null`,
  `ok`) are not — write those fields explicitly (`{type: v}`), never
  as shorthand. Exception: `bytes` is field-only (its base64 literal
  payload collides with annotated binds).
- `use` imports a NAME, never a module's contents: `use sys::net`
  gives you `net::subscribe`, NOT bare `subscribe` (import the item,
  or glob, for that). Paths lead with a package name or
  `self`/`super`/`package`, in expression and type positions alike.
- `use` groups like Rust: `use tui::{list, block, text}`, nesting,
  `self`, renames (`as`), and globs (`*`) included; works in `.gxi`
  too (a gxi use is a private import shared with the impl, not a
  re-export). The printer always regroups under the longest common
  prefix.
- A submodule sees NOTHING of its parent implicitly — write
  `use super::{...}` (parent PRIVATE items are visible to the
  subtree). `mod`/`use` position carries no visibility meaning.
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

### The `slow-tests` feature

A handful of tests dominate the suite's wall time, and what they cover
moves rarely — package builds, the stack-depth guards. They are marked
`#[cfg_attr(not(feature = "slow-tests"), ignore = "slow-tests")]`, so a
plain `cargo test --workspace` skips them (they report as `ignored,
slow-tests` — visible, not hidden) and the RELEASE GATE runs them:

```bash
cargo test --workspace --features slow-tests
```

They still COMPILE in the default build, so they cannot rot unnoticed.
Behind the feature today:

| test | crate | cost |
|---|---|---|
| `reduced_feature_build_drops_packages` | graphix-package | vendor.py + a full shell build |
| `created_package_compiles` | graphix-package | `cargo check` of a generated package |
| `build_standalone_produces_working_binary` | graphix-package | a full standalone build |
| `download_source_extracts_package_at_expected_root` | graphix-package | downloads a released crate (needs network) |
| `deep_ast_drops_without_overflow` | graphix-compiler | ~40s, 50k-deep AST teardown |
| `deep_nesting_does_not_overflow` | graphix-shell | ~80s, 22 shapes x 2 depths in child processes |

Measured 2026-08-24: **19.5 min of test wall time down to 6.1**, and
`graphix-package` alone goes 705.8s -> 0.03s (its other 26 tests were
never the cost). The rest stays in the default run on purpose —
`graphix-tests` (174s, 2129 fixtures), the compiler proptests (33s),
`examples_compile` (32s) and the GUI harness (23s) all find bugs on
ordinary changes. So does `graphix-fuzz`'s 66s, which is ONE test:
`jit_generated_sweep` runs 120 fixed-seed generated programs through
both engines — the oracle in miniature, and the one gate here that
works the way the fuzzer does. It also gets more reliable now that
705s of concurrent cargo builds is gone: it had to re-check any
timeout at 4x because the suite ran ~13x slower than a solo run.

Gate a new test only when it is BOTH slow and testing something that
moves rarely (a build, an environment, a hard limit); never gate a
language-semantics test. Toggling the feature rebuilds the crates that
declare it and their dependents, so the gate run pays one rebuild —
use a separate `--target-dir` if you want to keep the incremental
artifacts. A test that re-executes its own binary must pass
`--include-ignored` to the child (see `deep_nesting.rs`), or the child
skips the test and exits 0, which reads as success.

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
- `Scope`: the lexical path (module + block nesting, a `ModPath`) and the
  dynamic scope (`DynScope`: the chain of error handlers visible to a `?`,
  one node per `catch` install, following the CALL chain — an instantiated
  body starts from its call site's; `Scope::append` extends the lexical
  path only, `Scope::with_catch` the dynamic one)

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
- `STATELESS` (default `false`): declare `true` iff an invocation's result
  depends only on its arguments, never on prior invocations of the same
  instance — no cross-invocation state (`count`/`sum`/`min`/`uniq`/`once`
  accumulate or remember). Effects do NOT matter (`print`/`log`/`exit`
  are stateless — each invocation emits once whichever instance runs
  it), and internal memos/scratch buffers are fine. Only consulted for
  `Sync` builtins, by the TAIL-LOOP COLLAPSE GATE
  (`analysis::lambda_is_stateless`, `design/recursive_activations.md`
  §2, 2026-08-24): a tail-recursive body reuses ONE activation across
  its iterations only when every builtin it reaches is stateless;
  otherwise each iteration owns an activation like a collection slot.
  A wrong `true` is a semantics bug (iterations would share
  per-iteration state), a wrong `false` only costs the loop.
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
`node/pattern.rs`; the node-walk's non-tail lambda dispatch; and
`Type::is_a_int` (a runtime type test recurses through VALUE
structure, so a recursive ADT makes its depth program-driven —
found by P2b's fold_list fixture, 2026-08-25).

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
  (name, cell addr, bound type), plus `FIND-IMPL` (each impl head tried
  against a receiver, both verdicts), `APP-SPLIT` (a constructor
  recovered through the heads), `BIND ctor` (a constructor variable
  bound by name), `SETTLE-INFINITE` (the cell an occurs-check
  refusal left unbound), `CHK-CONTAINS`/`CONTAINS` (every top-level
  unification's operands and verdict — a passing check that prints NO
  interior events between them concluded on a fast path without
  committing anything), `SET-T` (the general Set⊇t arm's whole/prims
  probe verdicts) and `REF-MEMO-HIT` (the ref-expansion cycle memo
  answering a pair). The tool for "who bound this cell" — found the
  select-arm greedy narrowing (soak jul05 item 12) twice, the P2 trio
  (pre-unified return cell, alias-chain fact, union-scrutinee
  narrowing) in one afternoon, and the aug25a Set-equality fast-path
  hole (the verdict prints are what made "true with zero events"
  visible).
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
- `GXDBG_SHALLOW=1` — print each select arm's sealed shallow
  discriminator (`SHALLOW <pred> => <shallow>|deep`) at the select's
  first consult. The tool for "did this arm's type test stay O(1) or
  fall back to the deep walk" (`Type::shallow_discriminant`).
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
- Seeding is LAZY — a fill is correct only when the resolving env
  holds the name's FINAL target, and mid-compile envs are truncated by
  registration order (eager transitive seeding captured the list
  PACKAGE's `List` for tui's `list::List` submodule ref; removed
  twice). Refs fill when a typecheck-time walk needs them, plus ONE
  eager pass: `Env::seed_typedef_refs` walks every typedef body right
  before fusion (after typecheck — every name's final target is
  registered, the one order-correct moment), because a recursive
  type's INNER occurrence is reached by no typecheck walk (the
  Ref×Ref name fast path answers without expanding) and fusion must
  expand it env-free (`list::List` de-fused without it, 2026-08-22).
- `same_def` (structural — gates the Ref×Ref name fast paths via
  `cells_agree`); a DIFFERENT def in the env is a stale-horizon
  artifact and the cell wins. The privatize walk, `same_view`,
  `private_view` and `rebind_resolution` died with inside-module
  transparency (nominal abstract types, 2026-08-22).
- `freeze_for_abi`/`abi_kind` expand refs env-free through the cell
  (`TypeRef::expand_cell`), unfilled → de-fuse; the fusion-side
  `expand_refs` (capped, expanding, env-backed) is the pre-pass for
  kernel-sig derivation. An abstract type is an opaque 2-word Value
  to both.

### Two-Phase Typecheck

Every node implements `typecheck0`/`typecheck1` (two passes over the whole
graph). `typecheck0` also builds `ctx.bind_to_lambda`; `CallSite::typecheck1`
pre-binds statically-resolvable calls (`try_static_resolve`) and re-drives the
bound instance's body typecheck with the call's fn-typed args registered
(per-callsite elaboration), so calls to a lambda *parameter* resolve statically
inside each instance. The old `NeedsCallSite`/deferred-check machinery is gone
— a builtin that needs call-site types reads them from its `typecheck1`
`resolved` argument.

**The recursion knot keys on INSTANTIATION IDENTITY (2026-08-30,
Eric's call).** While an instance body typechecks, its def is entered in
`ctx.resolving_lambdas`; a site reaching the def again in that window
is a self-call and shares the resolving instance — that is what bounds
instantiation regress for recursion. Since `b386f97d` a HOF site's
callbacks premat INSIDE that window, so a use of the same HOF nested
under its own callback (`fold` inside `fold`'s callback, `apply(|y|
apply(g, y), x)`) also arrives with the def active — and keyed on def
alone the knot stamped it with the OUTER instance: `fold -> callback ->
fold` in the static graph, `mark_recursion` marked both sites cyclic,
the emitter refused the region ("mutually recursive static call edge"),
and the shape node-walked into the interp's per-slot lazy binds
(quadratic — the `LambdaIds` hub, open). The knot now compares
`FnArgIdentity` — per argument, the SOURCE lambda (`LambdaDef::source`,
the literal's `ExprId`, stable across instance re-compiles) it resolves
to — and `resolving_lambdas` holds a STACK per def (`ResolvingStack`):
same identity = self-call (reuse; `f(n-1)`, `f` through a forwarded
param, the CPS wrapper `f(n-1, |y| g(y+1))` knots at depth two because
the literal's source repeats); different identity = a fresh instance even
mid-resolution. A bare VALUE reference (`bind.rs`) has no arguments to
key on and takes the innermost active instance, as before. Pins:
`lang/collection.rs` `nested_same_intrinsic`/`nested_map_in_map`/
`user_hof_nested`/`nested_mixed_types` (all `Jit`),
`lang/functions.rs` `cps_wrapper_recursion`. Any special-casing of
collection intrinsics here is the wrong fix — it stops working the day
`fold` is written in Graphix.

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

> Current rules only. History and rationale live in `design/` (index:
> `design/README.md`), pins in `graphix-fuzz/findings/` and
> `stdlib/graphix-tests`, per-change detail in `git log`. When a rule
> here disagrees with the tree, the tree wins and this file is stale —
> fix it in the same change.

**Two evaluators, one canonical.** The node-walk (`node/*.rs`, the
`Box<dyn Update>` reactive graph) is the canonical execution model and
the universal fallback; it must always be correct. Fusion → cranelift JIT
(`fusion/`, emitter split per area under `fusion/emit/`) compiles sync
subtrees to native kernels: success splices the kernel and deletes the
originals, failure leaves the originals to node-walk. There is no third
evaluator, and **no parallel typed IR** — the node graph IS the IR
(`Expr → node graph → CLIF`; each node's `Update::emit_clif` emits its
own CLIF, `Apply::emit_clif` for builtins, the `scaffold` loops for
HOFs). The old GIR was deleted because it forced every semantics fix to
be written three times and taxed every new shape; only its ABI contract
survives (`KernelSig`/`abi_kind`/`freeze_for_abi` in
`fusion/kernel_abi.rs`; the scalar-operator enums in `node::op` are
shared, not ABI). Fusion recursion is `Update::fuse` (from `compile()`,
gated on `ctx.fusion.enabled`); kernel builds are pure signature
derivation (`sig_from_inputs`) and "is it fusable" IS the compile
attempt. Values are netidx `Value` (16 bytes, (disc, payload)); types are
netidx `Type`; `PrimType` is the closed register-scalar set.

**A fusion bug can lose fusion, never produce a wrong answer** — the
differential fuzzer enforces bit-for-bit agreement, and a divergence is
at least as likely a JIT bug as a node-walk one: adjudicate against the
INTENDED semantics, never by trusting either engine.

### Semantics both engines implement

- **`let rec` is monomorphic-recursive**: a def-time self-call unifies
  against the def's own cells (`ExecCtx::rec_defs`), the μ-equation
  collapses (`'r ⊇ [T, 'r]` binds `'r := T`), and the collapse looks
  through binding cells (`{let t = …; t}` spellings inherit the bare
  spelling's verdict). Bound-cell pairs walk their bindings; only a
  both-open constraint-graph cycle refuses. `let rec f = |n, acc| f` is
  refused at its definition. Pins: `lang/types.rs` `rec_block_*`,
  `connect_self_nesting_*`, `findings/bound-cell-cycle-accepts-aug2026/`.
- **Static-call instantiation keys on identity** (`FnArgIdentity`,
  "Two-Phase Typecheck" above): a nested use of a HOF under its own
  callback is a fresh instance, not recursion.
- **`select` exhaustiveness is enforced for bare-variant arm sets**
  (`StructPatternNode::matches_anything` drives the wildcard test, not
  `is_refutable`); slice-pattern length ladders count as coverage.
- **Union collapse requires strict tvar identity** (`union_identical`):
  two distinct unbound cells are not the same member.
- **`&&`/`||` are strict** (`false && ⊥ = ⊥`): a dataflow value reflects
  all its inputs.
- **Float comparison is Graphix's total order** (`Value::partial_cmp`:
  `NaN == NaN`, `NaN` below every non-NaN) so `Value` is map-key-able.
- **Checked arith** (`+?`…) yields the catchable `ArithError` value;
  unchecked wraps; integer div0 / signed `MIN/-1` → bottom. Swallowed-
  error DIAGNOSTICS (`error!`/`warn!` for unchecked arith, handler-less
  `?`, `$`) are node-walk-only — a kernel produces the same bottom
  silently; debug with `--no-fusion`.
- **Indexing** (`a[i]`, slices, `bytes[i]`, `m{key}`) is bounds-checked
  through the shared `node::array`/`node::map` helpers on all backends.
- **Bottom is dense** (`design/dense_delivery.md`): `Update::update`
  returns `&TagValue` every cycle — `Fired(v)`/`Stale(v)`/`FreshBottom`/
  `StaleBottom`, the orthogonal fired×bottom algebra (`TagValue::view()`
  is the consumption API). A standing bottom re-delivers `StaleBottom`
  and never re-fires consumers; bottomness joins by OR over consumed
  productions. In the JIT the same bits ride each param's disc (bottom =
  TAINT bit + a helper-safe placeholder payload; TAINT|STALE for a
  standing bottom). A pended DynCall taints at its site and continues;
  `DYNCALL_PENDING` reaching `Kernel::update` is a genuine whole-kernel
  abort only.
- **Bottom never reaches builtin authors**: a bottomed arg makes the
  wrapper bottom the invocation without calling `eval`
  (`CachedVals::any_bottom`); raw `Apply` authors read args through
  `seam_arg`/`seam_tick`/`seam_value` (package-core). Bottoms flow
  in-band with honest tags on both engines.
- **THE ORGANIC FIRING RULE** (`design/organic_firing.md`): a node fires
  iff a consumed input fires; nothing stores a previous value or
  selection to decide a tag; `uniq`/`filter`/`~` are the cadence tools.
  A select emits per fired input — scrutinee delivery, a CONSULTED
  guard's production, or the taken arm's own production (`own_fired`,
  node/select.rs; the kernel folds the scrutinee and prologue-guard
  STALE bits at every merge). Same-arm re-matches emit the arm's
  current value; an untaken arm's body is not a consumed input.
  Selection memory survives only for sleep/wake routing and the
  arm-lift re-seed. Constants fire at init only. Recursion fires like
  the hand-inlined chain with no extra machinery. Ruled deltas +
  red→green protocol: `organic_deltas.rs`.
- **Bottom scrutinee ⇒ bottom select** (Eric 2026-08-29): a select whose
  scrutinee bottoms produces nothing this cycle, no held-arm re-run,
  even if the taken arm is an active async producer — write `hold` on
  the scrutinee to persist across a bottom cycle. There is NO stored-
  selection ride of any kind (the scrutinee, guard, selection and
  unified rides are all deleted — do not reintroduce one). What
  survives is organic own-firing: a STALE-PRESENT scrutinee still routes
  the taken arm's own fires through the retained selection
  (`ChainOut::Quiet`), which is why `select p { null => 42, p =>
  subscribe(p) }` updates when `subscribe` does. Pins:
  `findings/{select-bottom-out-hold,tail-select-bottom-out}-aug2026`.
- **THE CONSULTED-GUARD RULE** (`design/activation_state.md`): arms are
  consulted top-down, structure first, guard second; a consulted guard
  whose current channel is bottom makes the selection undecidable (the
  chain stops, the select bottoms); guards of structure-failed or
  below-the-stop arms are irrelevant. A never-produced guard is unknown,
  not false (the init-phantom guard: a guarded select bottoms at init
  until the guard is evaluable).
- **The bottom-out rule + state multiplicity = activation multiplicity**
  (`design/activation_state.md`): held state never determines output
  bottomness (bottom in, bottom out); non-tail recursion is an
  activation per level, a STATELESS tail loop is one activation reusing
  its one state, collection slots are activations. A tail loop collapses
  to one activation only when the body is stateless
  (`analysis::lambda_is_stateless`, the `STATELESS` builtin const).
- **Recursion** (`design/recursive_activations.md`,
  `design/atomic_recursion.md`): activations ARE collection slots.
  Instances are retained unconditionally (no park, no budget — "you
  can't fix stupid"; fuzz children run under an 8GB `RLIMIT_AS`).
  **Shrink = delete**: a depth not reached this cycle is deleted and
  re-reaching it is a fresh activation (interp: `ctx.shrink_unwind`
  makes a cyclic-SCC `CallSite::sleep` delete its callee; JIT:
  `Kernel::update` reclaims `SelfBlock` subtrees not stamped with the
  current reach generation, in safe Rust). **No depth limit** — depth is
  bounded by memory on both engines (the kernel re-enters through a
  spill thunk on a fresh stack segment; `GRAPHIX_STACK_BUDGET` aborts a
  runaway like Ctrl-C). **Evaluation is atomic within a cycle**: a
  program may legally spin forever inside one on both engines;
  containment is the cooperative interrupt (`GXHandle::interrupt`,
  polled by the interp's tail driver and every emitted loop head),
  armed by the shell on Ctrl-C or by an embedder's watchdog, observable
  by no program. Pins: `recursion_shrink_deletes_unreached_activations`,
  `fused_recursion_sheds_unreached_blocks` (lift.rs),
  `lib_tests/interrupt.rs`, `graphix-shell/tests/interrupt_wedge.rs`.
- **Sleep is PAUSE, not reset** (Eric 2026-07-31): value-channel state
  survives an arm's sleep — `Held` residents at the three ride sites
  (select scrutinee, pattern guard, `~`'s arg), `CachedVals` staging,
  collection slot values, the kernel's interior-bottom taint caches
  (replay words, owned value pairs) — so a re-selected arm whose fresh
  computation bottoms rides its history. Slot CHAINS (`SiteAnchor`:
  selection memory, nested prev-length words, in-loop DynCall site
  identity) are semantic per-position state and survive frames as well
  as sleep; only `reset_replay` (frames) clears replay caches and only
  `Drop`/truncation frees chains. An arm's WAKE resumes it: a `let` that
  is a `<-` target and holds a value is not reseeded by its re-fired
  initializer (`Event::wake_init`). A producer materializes its value
  channel on its first production whatever the tag (`Bind` publishes a
  quiet first production; `CachedArgs` runs `eval` once from the
  phantom). The RESTART builtins (`once`/`take`/`skip`/`uniq`/`hold`/
  `count`, `SLEEP_RESTARTS`) clear on sleep; a select whose arm reaches
  one de-fuses (kernels have no per-arm sleep initiator). Pins:
  `findings/{sleep-preserves-caches-jul2026,arm-local-bind-aug2026,
  sleep-restart-gate-aug2026}/`.
- **THE QUIET FLAG**: a re-derivation inside a quiet frame
  (`frame_depth > 0 && !frame_init` — every framed pass of a tail chain
  on a non-init cycle) is NOT an init view. Only a site's first-ever
  dispatch is the forced init-view dispatch; a re-woken site is resumed,
  not re-primed; becoming-selected grants no init view in a frame. Wire
  slot 0 is a context word (bit 0 init, bit 1 quiet — set by the wrapper
  from the interp frame, by a tail-loop body for itself, inherited by
  callees). Three kernel mechanisms manufactured a false init view and
  are fixed (slot `fired` reset on sleep; the fused select's `woke`
  word; per-frame freeing of in-loop site identity). The symptom to
  recognize: a `let rec` chain re-derived by an input that is NOT
  consumed (read only by a structure-failed arm's guard) fires on the
  JIT every delivery and once on the interp. Pins:
  `findings/quiet-frame-init-view-aug2026/`.
- **DynCall SITE IDENTITY** (`design/kernel_instance_state.md`): a
  compiled callee's interior builtin is one `graphix_dyncall`
  instruction reached from many emit sites, so each site claims an
  identity word (region root: instance word; callee root: per-call-site
  block word; inside a scaffold loop: a per-slot chain leaf) and the
  dispatcher keys a full inner `Apply` per minted id — cache AND builtin
  state per site, like the interp's per-CallSite instances. Key 0 (no
  identity) remains only for qop-deliver and a callee site reached with
  a null site block. A self-call roots a lazily grown per-ACTIVATION
  block tree (`graphix_site_child_block`, one root per self-call site).
  Callee kernels define in TOPOLOGICAL order over the recorded call
  edges (a callee defined after its caller would run below a recursion
  with no interior memory). Pins: `dyncall_site_identity_state`,
  `findings/{dyncall-site-identity-jul2026,
  recursive-activation-blocks-aug2026}/`.
- **Guards in kernels** tick per invocation via a PROLOGUE in
  `emit_select_arms` (the interp ticks every arm's guard every cycle);
  schedule-free guards (pure never-bottom fns of the arm's own binds)
  stay lazy in the chain. A fused DynCall delivers non-fired args as
  `TagValue::stale` — never absence, never `fired` (`rand` would
  re-randomize). Tag-blind builtins (`printfn!`, `now`) gate on presence
  by design.
- **Per-cycle firing (the STALE bit)**: a kernel output fires only when
  an input feeding it fired; a lifted `<-`-target counter is threaded in
  as a kernel input so reactive counters fuse. Collection loops fire by
  `scaffold::SlotFlags`: per-slot discs fold into a slots word and a
  prev-length word gives exact resize detection — fires iff resized ∨ a
  slot fired ∨ the source fired empty; a same-length refresh with a
  quiet body does not fire. Callee bodies keep per-call-site state
  blocks (wire slot 2, `SiteLayout`) for site identity, first-dispatch
  init words and prev-len words — never select firing memory. Residue:
  arm-lifted connects in loops/callees de-fuse (coverage).
- **Collection HOFs** (`design/collection_intrinsics.md`): MapQ/FoldQ
  are compiler-owned nodes (`node/collection.rs`) — the canonical
  per-slot interpreters — and `GXLambda::emit_clif` inline-emits a
  collection-bodied callee as a native loop at the call site
  (`scaffold::emit_*_loop`); refusal leaves the per-slot node. List and
  Map lower through the FLATTEN boundary (`graphix_list_to_valarray`/
  `graphix_cmap_to_pairs` → the array loop → rebuild). `FoldAcc::Value`
  carries Value-shaped accumulators. `find`/`find_map` scan all slots in
  both modes (a bottom predicate after the match bottoms the find).
  Collection callbacks with labeled parameters interpret; a callback
  with only labeled parameters is a type error. Cross-kernel call sites
  force the callee's init view on the first call ever.

### Testing is differential

- `run!` (`graphix-package-core/src/testing.rs`) runs each fixture in
  `interp` and `jit` modes asserting equal values; `FuseExpect::{Jit,
  None}` asserts WHETHER it fuses (bidirectional — the harness demands
  the annotation match reality). `GRAPHIX_FUSE_AUDIT=1 cargo test --
  jit --nocapture` prints the per-fixture audit; sweep the workspace,
  the stdlib crates carry fixtures too.
- **graphix-fuzz** (`design/graphix_fuzz.md`): node-walk vs JIT with a
  per-cycle TRACE oracle; `check`/`run`/`generate [--reactive]`/`fuzz`/
  `minimize`/`regress`/`selfcheck`/`gen-check`/`detcheck`/`typemorph`.
  Programs may carry a `// schedule-v1:` header (input epochs via
  `set_many`; inputs use the `let inN = d; inN <- never(d)` contract)
  and a `callable-v1` header (the route matrix: in-language injection
  vs `compile_callable` dispatch); the metamorphic twin scan catches a
  bug that breaks every engine and route identically. `minimize` is
  typed-AST HDD (statement drop); `selfcheck` (same mode vs itself,
  100% required) gates oracle soundness; `detcheck` is the fusion-shape
  determinism gate (two fresh processes, normalized CLIF dumps must
  match). `rand::`/`sys::`/`http::`/`hold(` programs are excluded from
  divergence recording (their output depends on async timing). The
  committed `findings/` corpus is the regression gate (`regress`).
- **Soak ops**: campaigns run under `nice -n 19`, from a campaign-private
  COPY of the binary, with output OUTSIDE the repo
  (`~/tmp/target/fuzz/<campaign>/`), one corpus dir per campaign; the
  pool gives children `GRAPHIX_STACK_BUDGET=1GB`, parent-owned sandbox
  cwds, and a `BreakageWindow` backstop. **The fleet deploy is a
  script**: `graphix-fuzz/fleet.sh deploy <new> <base-seed> [old]` (steps
  `pull`/`stop`/`sync`/`launch`/`verify`/`status`; `FLEET_ONLY`/
  `FLEET_EXCLUDE` scope one box) — every step verifies a FACT (pgrep,
  content fingerprint, the campaign's own gate line with the embedded
  corpus count). Seeds are 10M apart in host-table order. Pulled
  findings go to `fuzz/pending-triage/<camp>/` (untracked); the triage
  record is its README.
- `FusionStats` (`attempted`/`fused`/`failed` with reasons, via
  `GXHandle::fusion_stats()`): read `failed` as a blocker profile, not a
  gap count.
- A stack-budget abort is `Outcome::Timeout` (containment, like the
  deadline); which limit stops a runaway first is a race between the
  engines' descent speeds, not a property of the program.

### Kernel infrastructure

- **JIT memory lifecycle**: one active JITModule + 256MB arena per
  ExecCtx (cross-kernel calls are ±2GiB PC-relative); kernels are never
  freed within a generation; on exhaustion the module RETIRES whole
  (`FusionCtx::retired_jits`, kernels stay mapped) and the region build
  retries in a fresh one — a region builds atomically in one generation
  and generations never link. The reclamation unit is the ExecCtx.
  `GRAPHIX_JIT_ARENA` shrinks the arena so gates exercise rotation.
- **Kernel ABI**: kind-grouped params (scalars, then array/tuple/struct
  pointers, then string, then 2-word variant/nullable/value) from one
  source (`KernelSig::abi_params`); any region width fuses. Recursive
  types freeze to an opaque leaf; an abstract type is an opaque 2-word
  `AbiKind::Value`.
- **Emit contracts** (`design/distributed_jit.md`): replayability ≠
  `Sync`; effects de-fuse, never silently skip (a skipped fn-formal arg
  with an effect de-fuses); first dispatch forces the init view;
  wake-ups key on `(BindId, fusion.top_id)`; clone types out of
  `with_deref` before recursing; dead statements eliminate at emit only
  when the statement subtree is effect-free, and a statement binds
  whatever its subtree binds. The Value-shape DynCall return folds
  `tagbits` like every other shape. Kernel cache keys carry the
  instance body's catch coverage and a resolution FINGERPRINT (same
  types + different callbacks ⇒ two kernels). Sig-less modules refuse
  emission. `freeze_for_abi_normalized` never normalizes shared tvar
  cells (`check_mode_parity` pins mode-identical `--check`), and a pass
  the fusion gate owns must never change what the typechecker sees
  (`Env::seed_typedef_refs` runs in both modes).

### Coverage (current)

By fixture annotation (2026-08-30): 452 `FuseExpect::Jit` vs 250
`FuseExpect::None` — ~64% of the pinned corpus fuses; every bench
program fuses fully. The vocabulary: all scalar arithmetic/comparison/
logical/cast/checked-arith, every producer and accessor, `?`/`$`, all
eight array HOFs as native loops over scalar/composite/String/value
elements (HOF-of-HOF and same-HOF nesting fuse as one multi-loop kernel;
fold accumulators may be composite or string), `select` structural
destructuring with scalar leaf binds, `connect` of any RHS shape
including lifted composite/string accumulators, every Sync builtin via
DynCall, cross-kernel lambda calls (recursive self-calls: tail →
rebind-and-jump, non-tail → native recursion), trait default bodies and
fn-formal forwarding/capture.

Fusion descends through Module/Block/Bind/CallSite/Catch/Lambda/Select
(scrutinee, guards and each arm body get their own region passes) and
ExplicitParens; not through `~`, `<-`, or operator operands (a sync
expression there fuses only as part of an enclosing region; a registry
attribute there is a loud compile error).

The correct-None denominator (principled): async/streaming builtins,
cross-cycle nodes (`~`, `Any`, `Catch`'s handler read), and non-register-
encodable types (`decimal`, `Fn`, `Ref`, unbound tvars). The missed-
fusion residue, each pinned by a `#[native]` de-fuse test or an ASPIRE
comment: select residue (whole-composite/`@`/named-rest binds, nested
non-scalar variant payloads, owned scrutinees in tail position);
union-self trait dispatch and abstract patterns in select; arm-lifted
connects in loops/callees; loop-carried Value rebinds (`lfold_rec`);
String-returning cross-kernel callees; non-scalar string-interp parts;
dynamic map literals; `array::group`; ByRef/Deref; decimal arith. The
intrinsics-deletion endgame is measured in `bench/collection/README.md`.

`#[native]` asserts zero node-walk residue at a source location (a no-op
under `--no-fusion`); `#[sync]`/`#[async]`/`#[tail_recursive]` assert
analysis facts. EmitTags (per-op body tags) were retired unbuilt — they
would resurrect the GIR vocabulary tax (`node_shape.rs`).

### Design documents

`design/README.md` is the index (built / proposed / superseded). The
docs hold the rationale and the as-built records; this file holds only
the rules.

## Language features (current)

- **Nominal abstract types** (`design/nominal_abstract_types.md`):
  `type T = Abstract<rep>` (only as a whole typedef body) has identity
  `AbstractId::of(scope, name)` (a path-derived v5 UUID, minted at
  `Env::deftype`) and values `Value::Abstract(GxAbstract { id, name,
  payload })` minted only by the constructor `T(v)`; `x.0` reads the
  payload, `T(p)` destructures, `T as t` is a nominal tag test
  (parameters are not carried at runtime: `Box<i64> as b` matches a
  `Box<string>`). The three faces compile only where the definition is
  visible (`Env::abstract_reps`, gated by `AbstractRep::public` or scope
  prefix); a gxi-hidden type must be `Abstract<..>` or Rust-backed. There
  is no inside-module transparency. Rust-backed abstracts register
  path-derived UUIDs (`abstract_wrapper!`, `impl_abstract_arc!`'s
  `= "pkg::mod::Type"` form), which is what makes a type test exact and
  trait dispatch over a union of them work. Abstract patterns de-fuse
  the select (coverage).
- **Traits v1** (`design/traits.md` §11–13): `trait T { val m: fn(self,
  ..) -> R [= default]; .. }`, `impl[<'a: C>] T for Target { let m = .. }`,
  `impl T for X;` in a gxi (the entry of record — the module's own impl
  fulfils it and consumers resolve to stable bindings across reloads),
  `'a: T + U` bounds, `fn(x: T)` ≡ a fresh bounded quantifier. Trait
  names are scoped like types, impls are global facts; `T::m`/`use T::m`
  ride the import engine. Dispatch is STATIC on the self argument's type
  (`CallSite::resolve_trait_call`); an open self type at a call is a
  compile error; a union self lowers to a synthesized select (de-fuses —
  coverage). Impl targets: an abstract type in the type's or the trait's
  package, anything else only in the trait's package, never a union, one
  impl per head. Constructor traits (`trait Collection`, the `'_` hole,
  `|c: Collection|` sugar) dispatch by decomposition on the receiver's
  outermost form. Core `Eq`/`Ord`/`Display` ride the VALUE through
  netidx's abstract vtable (map keys, sort, min/max, uniq, operators,
  printers — both engines); only abstract types may implement them
  outside core; a bottoming impl resolves per key like NaN. A core-trait
  impl for a Rust-backed abstract is refused (no payload to consult). Not
  built: trait params/associated types, trait aliases, `Hash`.
- **The io traits** (`design/traits.md` §13): a stream's TYPE is its kind
  (`sys::fs::File`, `sys::tcp::TcpStream`, `sys::tls::TlsStream`,
  `sys::process::Pipe`, `sys::io::Stdio` — five Rust types over one
  `StreamKind` via `stream_kinds!`) and `Read`/`Lines`/`Write`/`Close`/
  `Seek`/`Socket` say what it can do; `read` is the only required `Read`
  method, the derived ones are Graphix over it with native overrides.
  json/toml/pack/xls parse `bytes`/`string` only. A default's accumulator
  connect must be gated on the event (`acc <- b ~ concat(acc, b)`) — the
  ungated form is the counter idiom by accident. API breaks vs 0.9.0:
  `Read::read`, `Seek::seek`, `Socket::shutdown`; `process::Stdio` →
  `process::Redirect`; `Child`'s pipe fields are `[Pipe, null]`; a TLS
  upgrade consumes the TCP handle.
- **The module system** (`design/module_system.md`): Rust-2018-style
  imports — every name arrives by a declaration, a `use` (renames,
  globs, groups, `{self, *}`), or a prelude (core's root items; package
  names as path roots). Paths lead with a package name or
  `self`/`super`/`package`, in expression and type positions. A
  submodule sees nothing of its parent implicitly; `mod`/`use` position
  carries no visibility meaning; a gxi `use` is a private import, not a
  re-export (`pub use` reserved, unbuilt). Resolution: lexical chain →
  imports → globs (two providers of a used name error at first use) →
  package prelude → core prelude; declarations shadow imports, imports
  shadow globs. `Env.names` is a global per-scope registry (exempt from
  `restore_lexical_env`) so instance-side resolution consults the
  DEFINING module's table. `use` compiles to Nop. The widget-module
  `{self, *}` idiom is the one blessed glob in exemplar code.
- **Comments** are legal only above an expression, a select arm's
  pattern, an impl method, or a struct-literal field (`parser::decorate`
  attaches them; the printers hoist them back); interior, trailing and
  dangling comments are parse errors by design. The tree-sitter grammar
  treats `#[..]` attributes and comments as extras.
- **A free union member stays free**: in `contains`' Set×Set residue arm
  an unbound rhs member is residue, never covered by a concrete lhs
  member. A select's type is the union of its arm types; a free `'b` arm
  beside an `i64` arm is not inferred to `i64` — annotate.

## Stdlib package notes

- **`sys::process`**: managed children live in the opaque `Proc` value
  with weak polling + `kill_on_drop`; `options` and `stdio` named-arg
  constructors; redirects are an explicit `Pipe`/`Inherit`/`Null`
  variant (default `Inherit`); the polling task is the sole reaper and
  `wait` subscribes to its watch status. Wire conversion uses
  `netidx-derive` except `SpawnOptions.env` (`immutable_chunkmap::Map`
  has no `FromValue`). Shell tests are Unix-gated with `cmd.exe` twins.
- **GUI** (`graphix-package-gui`, iced 0.14) uses the iced sub-crates
  directly; `iced_renderer` needs both `wgpu` and `wgpu-bare`. GUI/TUI
  examples are visual — test manually. `GuiWidget` has a `#[cfg(test)]
  as_any`; `GuiTestHarness::dt()` downcasts; tests fire callbacks via
  `gx.call(callable_id, args)`. Test contexts default to
  `NetConfig::Internal` (a real in-process netidx on demand); publisher
  coalescing collapses rapid updates — space them with timers.
- **Package manager** (`graphix-package`): `packages.toml` v2 is a
  `[stdlib]` table (`installed`/`removed` names; stdlib always tracks
  the shell version) plus `[packages]` for EXTERNAL packages
  (version-or-path); the old flat format migrates once on read
  (`LEGACY_REMAP`: `fs`/`net`/`time` → `sys`). `combined_map` bridges to
  the build machinery; the stdlib set at a version is enumerated from
  that shell source's `Cargo.toml` (`stdlib_packages_in_source`);
  `DEFAULT_PACKAGES` is only the bootstrap; `INTERNAL_PACKAGES =
  ["bench"]`. `update` presents a maskable change set (shell bump, new
  stdlib, external updates), prompts `[Y/e/n]`, builds BEFORE writing
  the manifest, and hard-errors on non-TTY without `--yes`. The pure
  core is unit-tested (`test::pure`); `download_source` is testable
  against a fixed released `graphix-shell` in a temp data dir.

## The admin-TUI dogfood campaign

The netidx-admin ratatui TUI (~11k lines) is being rewritten in Graphix
as `graphix-package-netidx-admin`, which lives in the NETIDX repo (the
first real external package). Design + findings log:
`../netidx/design/graphix-admin.md`, `graphix-admin-findings.md`.
**The PRIMARY objective is finding and fixing Graphix problems; the TUI
is secondary** (Eric). No workarounds: an awkward idiom, slow compile,
bad diagnostic or missing capability means stop, log a finding, fix it
here (or consciously accept it), then continue — never move decision or
presentation logic into the package's Rust layer because Graphix was
painful. Measure `--check` time at every size milestone. State: paused
at Phase D since 2026-08-21 (its finding 1 produced the module system);
open prerequisites: terminal suspend/resume for `sudo`/`$EDITOR`
handoff; reserved-word parse diagnostics at package scale.
