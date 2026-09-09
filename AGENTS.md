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
~  ~!                             // sample (lowest binary): `e ~ v` is v at each
                                  // fire of e, BANKING a trigger that finds v absent
                                  // (paid at v's first value); `e ~! v` is the STRICT
                                  // sample — bottom when v is bottom, never counts
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
`throttle(dur,v)`, `seq(start,end)`; `never()` / `never<T>(args…)` is
SYNTAX, not a function (typed bottom, or `T`)

**opt** (core, over `['a, null]`): `is_some`, `is_none`, `or_never`
(null → never: `f(opt::or_never(x ~ maybe))` REPLACES the ladder
`select x ~ maybe { null as _ => never(), v => f(v) }`), `or_default`,
`or`, `and`, `xor`, `ok_or`, `ok_or_else`, `or_else`, `zip`, `unzip`,
`map`, `flat_map`, `filter`, `contains`, `is_some_and`, `is_none_or`

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

**sys::time**: `timer(timeout, repeat)` (timeout `[duration, Number, null]`:
NULL STOPS THE TIMER — a level effect exists while its key is present),
`after_idle(timeout, v)`, `now()`

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

**sys::net**: Netidx `subscribe(path)`, `publish(path, v)`, `rpc`, `list`,
`write` — the path is `[string, null]` and NULL TEARS THE EFFECT DOWN
(unsubscribe/unpublish); an arm never pauses them (2026-09-03)

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
- A `<-` inside a select arm fires when its RHS fires: a CONSTANT RHS
  (`\`Enter => screen <- \`Pick`) fires once when the arm becomes
  selected and not on a same-arm re-match — a trigger on the
  selection changing (an "on entering this state" write); a handler
  that must act on every event samples it (`screen <- ev ~ \`Pick`).
  No lint (Eric 2026-09-03): both are tools.
- Tuples need 2+ elements: `(x)` is just grouping, not a 1-tuple.
- Blocks need 2+ elements: {x + 1} is a syntax error.
- Union types use `[]`: `[i64, null]` is "i64 or null", NOT an array.
  Array type is `Array<i64>`. Array literal `[1, 2]` is context-dependent.
- Variants always have backtick prefix: `` `Foo ``, `` `Bar(x) ``.
- Struct literal `{x, y}` is shorthand for `{x: x, y: y}`.
- Functional update: `{s with field: new_val}` — copies struct with changes.
- `select` must be exhaustive (cover all cases) with no dead arms.
- `never()` is a value that never arrives (syntax, not a function; the
  args stay live and are consumed). Bare it types as bottom, which a
  select absorbs, and a `let` over a bare `never()` takes its type
  from its writers (`let res = never(); res <- v`); `never<T>()` when
  nothing else fixes the type (a field, an argument, a `let` nothing
  writes).
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
  trigger has happened. A call fires when ANY argument fires, so for an EFFECT
  with several inputs sample the WHOLE input value on the trigger, never one
  argument of it: `spawn(go ~ options(#args: a, prog))`, not
  `spawn(options(#args: a, go ~ prog))` — the second re-issues the spawn when
  `a` fires on its own (the admin handoff bug, 2026-09-03). Sampling one
  argument is right only when the others cannot fire independently.
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

Guidance for Claude Code in this repository. This file holds RULES ONLY:
how the project is built and tested, how the engine behaves, what the
language does. Rationale and as-built records live in `design/`
(index: `design/README.md`); history lives in `git log`. When a rule
here disagrees with the tree, the tree wins and this file is stale —
fix it in the same change. Keep it current and short: no dates, commit
hashes, campaign names or bug stories; a pointer to a design doc or a
pin is enough.

`AGENTS.md` is generated, never edited:
`cat ~/.claude/CLAUDE.md <(printf "\n") CLAUDE.md > AGENTS.md`.
Regenerate it whenever this file changes.

## What is Graphix?

A dataflow programming language for UIs and netidx network programming.
Programs compile to directed graphs: operations are nodes, edges are data
flow. The language is reactive at the language level — when a dependent
value changes the graph updates. Lexically scoped, expression-oriented,
statically typed with inference, structural types, parametric
polymorphism, algebraic data types, pattern matching, first-class
functions and closures.

## Project structure

Rust workspace:

- **graphix-compiler**: parser, compiler (`Expr` → node graph),
  typechecker, fusion/JIT. Entry point `compile()` in `lib.rs`.
- **graphix-rt**: the runtime that executes node graphs in a background
  task, driven through `GXHandle`; embedder extensions via `GXExt`.
- **graphix-package**: package manager (loading, vendoring, standalone
  builds). **graphix-derive**: proc macros (`defpackage!`).
- **graphix-shell**: REPL and CLI; the binary is `graphix`.
- **graphix-fuzz**: the differential fuzzer (`design/graphix_fuzz.md`).
- `stdlib/`: one crate per package — `core`, `array`, `map`, `str`, `re`,
  `rand`, `sys` (streams, fs, tcp, tls, netidx, timers, processes),
  `http`, `toml`, `xls`, `pack`, `tui` (ratatui), `gui` (iced); Rust in
  `src/`, Graphix in `src/graphix/*.gx`. `graphix-tests` holds the
  language and stdlib integration fixtures (a separate crate to avoid
  circular dev-deps).
- `book/`: mdbook source; `book/src/examples/` (symlinked as
  `examples/`) holds every example program (`tui/`, `gui/`, `net/`).
  `docs/` is BUILD OUTPUT — edit `book/`, then from `book/`:
  `mdbook build -d ../docs/book`.
- `design/`: design as built. `../netidx/` is expected as a sibling
  checkout; the compiler and runtime depend only on netidx's VALUE layer
  (`netidx-core`/`netidx-value`), the networking crates appear only in
  stdlib packages (`design/netidx_extraction.md`).

Workspace-level dependencies where possible; `poolshark` pools wherever
allocation can be avoided, `smallvec` where it cannot.

## Building and testing

Builds go to `~/tmp/target` (tmpfs; centrally configured — never build
elsewhere). Dev profile is `opt-level = "s"`, no debug info; release is
`opt-level = 3`, LTO, one codegen unit. Do not build release unless you
must.

```bash
cargo build                              # debug
cargo build -p graphix-shell             # one crate
cargo test                               # THE gate: whole workspace, from the root
cargo test -p graphix-tests              # one crate while iterating
cargo test --workspace --features slow-tests   # the release gate
cargo run --bin graphix -- file.gx       # run
cargo run --bin graphix -- --check file.gx     # compile + typecheck only
cargo run --bin graphix -- --expand file.gx    # check + print each seq's lowered machine
```

Tests run in parallel by design (the compiler supports many instances
per process); never rely on `--test-threads=1`. `rustfmt --edition 2024
<files>` scoped to the files you edited (`cargo fmt` follows `mod`s into
drifted files). Formatting: `rustfmt.toml`; `snake_case` / `CamelCase` /
`SCREAMING_SNAKE_CASE`; Rust edition 2024; `triomphe::Arc` unless a
`Weak` or a cycle is needed.

**slow-tests.** Tests that are slow AND cover something that rarely
moves (package builds, stack-depth guards, a network download) are
`#[cfg_attr(not(feature = "slow-tests"), ignore = "slow-tests")]`; a
plain `cargo test` skips them (they show as ignored), the release gate
runs them. Never gate a language-semantics test. A test that re-executes
its own binary must pass `--include-ignored` to the child.

**Tests exist to find bugs**, not to pass. A failure is the good case:
find out why. Never work around a failure in a test that should pass; an
off-topic failure is discussed with Eric before it is fixed.

## Architecture

**Pipeline.** Parse (`graphix-compiler/src/expr/parser/`) → `Expr` AST
with positions → compile (`node/compiler.rs`) → `Node<R, E>` graph →
typecheck (two passes, `typecheck0`/`typecheck1` on every node) → fuse
(`fusion/`, when enabled). `typecheck0` also builds `ctx.bind_to_lambda`;
`CallSite::typecheck1` pre-binds statically resolvable calls and
pre-materializes HOF callbacks.

Key types: `Expr`/`ExprKind` (immutable AST; `Expr::for_each_child` /
`map_children` are the ONE child enumeration — `fold`, the seq rewrite and
the fuzzer's preorder all ride them, so a new `ExprKind` child is added
there and nowhere else), `Node<R, E>` (a newtype over `Box<dyn Update>`;
construct with `Node::new`), `ExecCtx<R, E>` (builtins, env, runtime),
`Scope` (lexical `ModPath` + `DynScope`, the chain of error handlers a
`?` sees, following the CALL chain).

**Nodes** implement `Update` (regular nodes) or `Apply` (function
applications called by `CallSite`). `Update` requires `update`,
`delete`, `typecheck0/1`, `refs`, `sleep` (unselected arms pause), plus
`emit_clif`/`fuse` for fusion. When writing a node: store the spec
(`Arc<Expr>`) for errors, track bind ids with `Refs`, call
`ctx.set_var()` for variable writes. `wrap!(node, result)` adds
expression context to errors; `err!`/`errf!` build error values.

**Runtime.** `graphix-rt` implements `Rt`: variables, timers, spawned
tasks and watch channels (`spawn`, `spawn_var`, `watch`, `watch_var`)
that packages use to feed external events in. Event processing is
batched: all simultaneous events form one `Event` delivered in one
cycle; several writes to one variable in a cycle queue for the next.

**Module loading** is the `ModuleResolver` trait (`expr/resolver.rs`);
`VfsResolver`/`FilesResolver` are in-core, `NetidxResolver` is in
`graphix-package-sys`. `sys::net` owns its netidx through `NetState` in
`ctx.libstate`; `NetHandles` shares the raw publisher/subscriber between
the loader and `NetState`; unseeded contexts get a process-internal
netidx on demand, so programs that never touch `sys::net` have zero
network. The shell library is netidx-agnostic; the CLI is the
netidx-aware embedder (`ShellBuilder::setup_context`,
`resolver_factories`, `GXHandle::with_ctx`).

**Types** (`graphix-compiler/src/typ/`): `Type` (structural),
`TVar` (inference cells; `design/tvar_constraints.md`), `FnType`.
`contains` expands `Type::Ref` through `lookup_ref`, so bindings made
during `contains` hold the EXPANDED form — code inspecting resolved types
handles both. `TypeRef` carries a write-once resolution cell
(`design/env_independent_typerefs.md`): rebuilds share it via
`with_params`, `with_scope` makes a fresh one, never overwrite a filled
cell; `Env::seed_typedef_refs` runs right before fusion in both modes.
Format type variables with `format_with_flags(PrintFlag::DerefTVars, ..)`.

**Two-phase typecheck knot.** While an instance body typechecks, its
def is in `ctx.resolving_lambdas` (a stack per def); a site reaching the
def with the same `FnArgIdentity` (per argument, the SOURCE lambda it
resolves to) is a self-call and reuses the instance; a different
identity is a fresh instance even mid-resolution (a HOF nested under its
own callback is not recursion). An instantiation snapshots its def's
`LambdaIds`. Never special-case collection intrinsics here.

**Builtins** implement `BuiltIn<R, E>` (`NAME`, `init()`, `EFFECT`) and
register with `ExecCtx::register_builtin::<T>()`; the Graphix signature
lives in the package's `.gx` with every argument and the return type
annotated. `EFFECT` is the one classification (`effects.rs`):
`Async` (may produce later, autonomously, or never), `Sync` (same cycle
but keeps cross-invocation state, or depends on WHICH args arrived), or
`Stateless(Option<FastCall>)` (a pure function of its args; the payload
is the direct-call entry the JIT uses, `Plain` or `Typed` by the site's
resolved return type; `None` for effects and partial-delivery
producers). A wrong `Stateless` is a semantics bug (the tail-loop
collapse shares state across iterations); a wrong `Sync` only costs the
loop. Bottom never reaches builtin authors: a bottomed arg bottoms the
invocation before `eval`; raw `Apply` authors read args through
`seam_arg`/`seam_tick`/`seam_value`. Configuration a fast fn derives
from its args (a regex, a template registry) lives in a bounded
thread-local `FastMemo`, never in state.

**Collection intrinsics** (`node/collection.rs`,
`design/collection_intrinsics.md`): the Array/List/Map traversal HOFs
are compiler nodes (`MapQ`/`FoldQ`) built when a lambda body is a
reserved marker name (`'array_map`, …); they own callback
instantiation, slot identity, per-slot firing/taint/sleep and result
construction.

## The semantics both engines implement

The node-walk (`node/*.rs`) is the canonical evaluator and the universal
fallback; fusion → cranelift (`fusion/`) compiles pure sync subtrees to
native kernels, splicing on success and leaving the nodes on failure.
**A fusion bug may lose fusion, never produce a wrong answer**; the
fuzzer enforces bit-for-bit agreement, and a divergence is adjudicated
against the INTENDED semantics, never by trusting either engine. The
node graph IS the IR — there is no parallel typed IR
(`design/final_jit_architecture.md`, `design/distributed_jit.md`).

- **Strict fusion** (`design/strict_fusion.md`): fusion admits pure
  computation only. A builtin fuses iff its `Effect::Stateless` carries a
  `FastCall`; `?` fuses with or without a covering catch (a handler-ful
  raise is queued and delivered after the run through the same path
  `Qop` uses); everything else — stateful/effectful builtins, `connect`,
  `~`, `Any`, `Catch` — node-walks, transitively. A kernel's only
  cross-invocation memory is the firing boundary (prev-length words,
  first-call words, per-site/per-activation blocks); no replay caches,
  no selection memory. The runtime loans a kernel exactly `KERNEL_ABORT`,
  `KERNEL_ENV`, `QOP_RAISES` and the core-trait value hooks.
  `#[native]` asserts zero node-walk residue at a source location and is
  THE advertised performance model; `#[sync]`/`#[async]`/
  `#[tail_recursive]` assert analysis facts.
- **Bottom is dense** (`design/dense_delivery.md`,
  `design/representable_bottom.md`): `update` returns a `TagValue`
  every cycle — `Fired(v)`/`Stale(v)`/`FreshBottom`/`StaleBottom`, the
  orthogonal fired×bottom algebra. A standing bottom re-delivers
  `StaleBottom` and never re-fires consumers; bottomness ORs over
  consumed productions. In the JIT the bits ride each param's disc.
- **Organic firing** (`design/organic_firing.md`): a node fires iff a
  consumed input fires; nothing stores a previous value or selection to
  decide a tag; `uniq`/`filter`/`~` are the cadence tools. A select emits
  per fired input — scrutinee delivery, a CONSULTED guard, or the taken
  arm's own production; same-arm re-matches emit the arm's current
  value. Constants fire at init (and at an arm's wake). Kernel outputs
  fire only when an input feeding them fired; collection loops fire on
  resize, a fired slot, a fired empty source, or a fired fold carry.
- **Bottom scrutinee ⇒ bottom select.** No stored-selection ride of any
  kind; `hold` on the scrutinee is the tool. A STALE-PRESENT scrutinee
  still routes the taken arm's own fires. **Consulted-guard rule**: arms
  are consulted top-down, structure then guard; a consulted guard whose
  channel is bottom makes the selection undecidable; a never-produced
  guard is unknown, not false. `&&`/`||` are strict (`false && ⊥ = ⊥`).
- **Sleep is pause, not reset.** Value-channel state survives an arm's
  sleep (`Held` residents at the select scrutinee, pattern guard and
  `~`'s arg; `CachedVals` staging; collection slots; a `<-` target's
  value). **Wake catch-up** (`design/wake_catchup.md`): a reselected arm
  recomputes from the world as it stands, reading standing values STALE;
  the only events it re-raises are the fires no selected reader saw,
  once, at their current value (one fire bit per arm-body input per
  select, consumed by whichever arm reads it; pattern binds and a
  destructuring let's siblings are facets of one input). Sleep state is
  LOCAL: every skip-owning node owns a `slept` bit its `sleep()` sets and
  its next update takes — no ExecCtx globals (parallel compile and a
  parallel evaluator stay possible). The restart builtins
  (`once`/`take`/`skip`/`uniq`/`hold`/`count`) clear in their own
  `sleep()`. A labeled DEFAULT is born with the binding and delivers
  FIRED at a fresh callee's first dispatch. Async builtins clear their
  output on sleep (`design/async_sleep_outputs.md`). A pure non-recursive
  arm skips `sleep` and is not updated while untaken.
- **Activation state** (`design/activation_state.md`,
  `design/recursive_activations.md`, `design/atomic_recursion.md`):
  held state never decides output bottomness; activations ARE
  collection slots; non-tail recursion is an activation per level, a
  STATELESS tail loop is one activation; instances are retained
  unconditionally; shrink = delete (a depth not reached this cycle is
  deleted; re-reaching it is fresh). No depth limit; evaluation is
  atomic within a cycle; containment is the cooperative interrupt
  (`GXHandle::interrupt`, Ctrl-C, `GRAPHIX_STACK_BUDGET`). Kernel
  interior memory (`design/kernel_instance_state.md`) gives one compiled
  body the interp's per-slot/per-activation multiplicity for exactly the
  state that decides firing; the QUIET FLAG (a framed pass on a non-init
  cycle) is not an init view; only a site's first-ever dispatch is.
- **`let rec` is monomorphic-recursive**; union collapse requires strict
  tvar identity; a free union member stays free (annotate a select whose
  arms are `'b` and `i64`); float comparison is a total order (`NaN ==
  NaN`, below every number) so `Value` is map-key-able; checked arith
  (`+?` …) yields a catchable `ArithError`, unchecked wraps, integer
  div0 bottoms; indexing is bounds-checked through shared helpers on
  both backends; swallowed-error diagnostics are node-walk-only (debug
  with `--no-fusion`).
- **Emit contracts** (`design/distributed_jit.md`): effects de-fuse,
  never silently skip; owned select-arm binds drop at every arm exit
  (run `leakcheck` when adding an owned-local class); a bottom is a
  production whose STALE bit follows the same trigger fold as a value
  (`nodes::emit_bottom_placeholder` takes the governing discs); kernel
  cache keys carry catch coverage and a resolution fingerprint; a pass
  the fusion gate owns must never change what the typechecker sees.
- **JIT memory**: one JITModule + 256MB arena per ExecCtx; on exhaustion
  the module retires whole and the region rebuilds in a fresh one; the
  reclamation unit is the ExecCtx. Kernel ABI: kind-grouped params from
  `KernelSig::abi_params`; recursive types and abstract types are opaque
  2-word values (`design/unified_value_abi.md`).

Coverage today: scalar arithmetic/comparison/logic/casts, producers and
accessors, `?`/`$`, the eight array HOFs as native loops (nesting
included), structural select destructuring with scalar and variant
payload binds, or-patterns, list patterns, tail loops over any kernel
param kind, every fast-fn builtin and non-inline cast, cross-kernel
lambda calls, trait default bodies. Fusion descends through
Module/Block/Bind/CallSite/Catch/Lambda/Select/ExplicitParens; not
through `~`, `<-` or operator operands. `FusionStats.failed` is a
blocker profile, not a gap count.

## Testing is differential

- `run!` (`graphix-package-core/src/testing.rs`) runs a fixture in
  `interp` and `jit` modes asserting equal values; `FuseExpect::{Jit,
  None}` asserts WHETHER it fuses, bidirectionally.
  `GRAPHIX_FUSE_AUDIT=1 cargo test -- jit --nocapture` prints the audit.
- **graphix-fuzz** (`design/graphix_fuzz.md`): node-walk vs JIT with a
  per-cycle trace oracle; `check`/`run`/`generate`/`fuzz`/`minimize`/
  `regress`/`selfcheck`/`gen-check`/`detcheck`/`typemorph`. The
  committed `findings/` corpus is the regression gate. `rand::`/`sys::`/
  `http::`/`hold(` programs are excluded from divergence recording.
  Soaks run under `nice -n 19` from a campaign-private copy of the
  binary with output outside the repo; `graphix-fuzz/fleet.sh` deploys.
  A stack-budget abort is a `Timeout` outcome (containment).
- A semantics change is not landed until it has soaked; gates are not
  the fuzzer.

## Language features (current)

- **Sampling**: `e ~ v` is `v` at each fire of `e`, banking a trigger
  that finds `v` absent; `e ~! v` is strict (bottom, no bank). A connect
  writes when its RHS fires; a constant RHS in an arm fires once when the
  arm becomes selected (the "on entering this state" write); a handler
  that must act on every event samples it. Both are tools, not lints.
- **`never<T>()` is syntax** (`ExprKind::Never`): typed bottom or `T`;
  args stay live and are consumed. An unannotated `let` over a ⊥
  initializer takes its type from its writers.
- **Sets and coverage**: select exhaustiveness is enforced; slice-pattern
  length ladders count as coverage; bool literals pool per position
  inside composite patterns; set coverage distributes over product
  heads (`` [`P(A), `P(B)] ⊇ `P([A, B]) ``); a probe in progress for the
  same scrutinee ref claims nothing on re-entry.
- **Or-patterns** (`design/or_patterns.md`): select arms and bracketed
  element positions; alternatives bind the same names at exactly equal
  payload types; captures type as the union; one guard per arm; dead
  alternatives are errors; they fuse natively.
- **Native List** (`design/list_native.md`): `List<'a>` is a compiler
  constructor like `Array`; `[<1, 2>]` literals and `[<h, rest..>]`
  patterns (rest is the O(1) tail; the suffix form is refused); the rep
  is private to `node/collection.rs::list`.
- **Nominal abstract types** (`design/nominal_abstract_types.md`):
  `type T = Abstract<rep>`; `T(v)`, `x.0`, pattern `T(p)` only where the
  definition is visible; `T as t` is a nominal tag test anywhere.
- **Traits v1** (`design/traits.md`): static dispatch on the self
  argument's type; a union self lowers to a select; impls are global
  facts; core `Eq`/`Ord`/`Display` ride the value (map keys, sort,
  operators, printers, both engines). The io traits `Read`/`Lines`/
  `Write`/`Close`/`Seek`/`Socket` over five stream types.
- **Module system** (`design/module_system.md`): Rust-2018-style
  `use`; every name arrives by declaration, `use`, or prelude;
  `self`/`super`/`package` roots; declarations are statement-position
  only.
- **Place references** (`design/place_references.md`): `&a[i]`,
  `&s.f`, `&t.0`, `&m{k}` are root + path; writes patch the root at
  delivery; a dynamic key is a moving reference. References de-fuse.
- **`catch`** (`design/catch.md`) installs a handler for the rest of its
  block; it is not control flow.
- **`seq` / `seqq`** (`design/seq_blocks.md`): `seq [trigger] { stmt* }`
  desugars to a pc machine (one select arm per step, busy-drop, carried
  lets as cells, calls issued once per entry over an argument snapshot).
  `until`, `do { .. }`, `try { .. } with(e[: T]) { .. }` (the error
  branch; `catch` is refused in a seq body outside lambda literals).
  A step completes on a FIRED production after its entry, never on a
  standing value; a call-free step reads its level as it stands at
  entry. `seqq` queues triggers with captured values. `--expand` prints
  the machine. `range(i, j)` is the integer builtin (`` `RangeError ``).
- **Comments** are legal only above an expression, a select arm, an impl
  method or a struct-literal field; parse errors report the furthest
  point reached with the source line and a caret.

## Stack discipline

Nesting depth is attacker-controlled and overflow aborts, so it is
closed two ways: `crate::stack::ensure_sufficient` (stacker) wraps every
program-driven recursion — parser knots (`GrowStack`), `compile`,
`Display`, `fold`/`for_each_child`, type walks, pattern walks, seq
lowering, and the `Node`/`TVar`/`Expr` destructors (explicit teardown
inside the guard; `Type` is the one uncovered cycle, made unreachable by
the limit) — and `parser::DEFAULT_MAX_NESTING` (counted in parser knots;
iterative loops that fold into nested ASTs are capped at the fold).
Refusals set a thread-local (`note_refused`) because combine merges
messages. Pins: `graphix-compiler/tests/deep_drop.rs`,
`graphix-shell/tests/deep_nesting.rs` (add a case for a new recursive
construct). netidx-value has the same treatment for bracket literals.

## Debugging

`TRACE` (`set_trace`, `with_trace(enable, spec, f)`, `tdbg!`) scopes
compiler tracing to one expression — the stdlib typechecks on every
compile, so unscoped prints are gigabytes.

| env var | prints |
|---|---|
| `GRAPHIX_DBG_BIND=1` | every tvar bind, impl lookup, top-level `contains` verdict |
| `GRAPHIX_DBG_KERNELS=1` | each lambda kernel built: name, return type, ABI, state/site words |
| `GRAPHIX_DBG_INVOKE=1` | each fused-kernel invocation with per-input fired/present |
| `GRAPHIX_DBG_REGION=1` / `_FREEZE=1` | fused-region input wiring / freeze outcomes |
| `GRAPHIX_DUMP_CLIF=1` | every kernel's CLIF (`u0:N` = helper registration order in `emit_helpers.rs`) |
| `GRAPHIX_DBG_VARS=1` | runtime variable events (ref/unref, set, same-cycle notify) — graphix-rt |
| `GRAPHIX_DBG_PERF=1` | interp lazy-bind phase counters every 250ms |
| `GRAPHIX_DBG_TVAL=1` | typed-printer render steps |
| `GRAPHIX_DBG_CYCLE_BT=1` | a backtrace at every occurs-check refusal |
| `GXDBG_TAIL=1` | every tail-loop dispatch pass |
| `GXDBG_EFFECT=1` | why a lambda classified Async |
| `GXDBG_INSTANCE_FUSION=1` | per-instance region fusion passes |
| `GXDBG_CS=1` / `GXDBG_DYNC=1` | every CallSite dispatch and result tag / every fastcall trampoline dispatch |
| `GXDBG_TYPEREF=1` | scope table dump on an "undefined type" refusal |
| `GXDBG_LETBIND=1` / `GXDBG_REF=1` | let publication decisions / read misses |
| `GXDBG_SLOT=1` | per-slot production tags and the collection fold decision |
| `GXDBG_SHALLOW=1` | each select arm's shallow discriminator |
| `GXDBG_RESOLVE=1` | static-resolution reads and index writes |
| `GXDBG_RPC=1` | the sys::net rpc path (graphix-package-sys) |

Fusion bugs: write a triggering test before adversarial review; a hung
test is a result.

## Working conventions

- Code review uses `// CR <name> for <name>: text` near the code; when
  addressed it becomes `// XCR ...`; XCRs are deleted when resolved or
  turned back into CRs with an explanation.
- PRs carry a concise summary, testing notes and related issues. Rebuild
  the book when docs or examples change.
- Examples in `book/src/examples/` are documentation and test corpus at
  once; TUI/GUI examples are tested by hand
  (`cargo run --bin graphix -- examples/tui/barchart_basic.gx`). Some
  are snippets that reference undefined names on purpose; they must stay
  syntactically valid.
- A new compiler walk must name the loss without it before it is added;
  the typechecker must stay instant (measure the GUI suite after typing
  changes); predictable fusion is a core value — push on de-fuse corner
  cases rather than accept them.
- Hot operators log and bottom on failure; rare stdlib functions return a
  catchable `Error`.

## Stdlib notes

- `sys::process`: children live in the opaque `Proc` with weak polling
  and `kill_on_drop`; redirects are `Pipe`/`Inherit`/`Null`; the polling
  task is the sole reaper. Shell tests are Unix-gated with `cmd.exe`
  twins.
- GUI (iced): uses the iced sub-crates directly; `GuiTestHarness::dt()`
  downcasts; tests fire callbacks via `gx.call(callable_id, args)`;
  test contexts default to `NetConfig::Internal`; publisher coalescing
  collapses rapid updates — space them with timers.
- Package manager: `packages.toml` v2 (`[stdlib]` tracks the shell
  version, `[packages]` for externals); `update` presents a maskable
  change set and builds before writing the manifest; hard error on
  non-TTY without `--yes`.

## The admin-TUI dogfood campaign

netidx-admin's ratatui TUI is being rewritten in Graphix as
`graphix-package-netidx-admin` in the netidx repo (the first external
package). **The primary objective is finding and fixing Graphix
problems; the TUI is secondary.** No workarounds: an awkward idiom, slow
compile, bad diagnostic or missing capability means stop, log a
finding, fix it here (or consciously accept it), then continue — never
move decision or presentation logic into the package's Rust layer
because Graphix was painful. Design and the open-items ledger:
`../netidx/design/graphix-admin.md`, `graphix-admin-findings.md`.
Measure `--check` time at every size milestone. Run its tests
(`cd ../netidx && cargo test -p graphix-package-netidx-admin`) after any
change to seq or select semantics.
