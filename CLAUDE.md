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
- **graphix-package-array**, **-map**, **-str**, **-re**, **-rand**, **-time**: Data structure and utility packages
- **graphix-package-fs**, **-net**: Filesystem and network packages
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
cargo test -p graphix-compiler       # Test specific crate
cargo test pattern                   # Run tests matching name
```

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
mdbook build -d ../docs              # Build language docs to docs/
mdbook serve ../docs                    # Serve docs locally
```

## Architecture

### Compilation Pipeline

1. **Parsing** (`graphix-compiler/src/expr/parser/`): Text → `Expr` AST with position info
2. **Compilation** (`graphix-compiler/src/node/compiler.rs`): `Expr` → `Node<R, E>` graph
3. **Type Checking**: Each node implements `typecheck()` to verify type correctness

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
- `TYP`: Lazy-initialized function type
- `init()`: Returns initialization function

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

- Don't commit code unless the user explicitly asks for it
- Commit messages should be short, lowercase, and imperative (e.g., `fix many parser problems`).
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

## Recent Changes

### GUI Package (Feb 2026)

Added `graphix-package-gui` — an iced 0.14 based GUI backend. Uses iced sub-crates directly (`iced_core`, `iced_wgpu`, `iced_widget`, etc.) rather than the umbrella `iced` crate for low-level control over the rendering pipeline. Note: `iced_renderer` requires both `wgpu` and `wgpu-bare` features (the cfg checks use the `wgpu-bare` flag which `wgpu` alone doesn't set).

## Writing Graphix Code — Language Reference

Graphix is NOT in the training set. This section is the authoritative
reference for writing `.gx` files. Read the full docs in `book/src/`
and examples in `book/src/examples/` when you need more detail.

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
fn(i64) -> string                 // function
fn(i64) -> string throws `E      // function that throws

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
*  /  %                           // multiply, divide, modulo
+  -                              // add, subtract
<  >  <=  >=                      // comparison
==  !=                            // equality
&&                                // logical and
||                                // logical or
~                                 // sample (lowest binary)
```

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
let greet = |#greeting = "hello", name| "[greeting], [name]!"
greet(#greeting: "hi", "world")   // "hi, world!"
greet("world")                    // "hello, world!" (default used)

// variadic args (only usable by built-ins)
let f = |@args: i64| args         // args is Array<i64>

// calling
f(1)  g(1, 2)  module::func(x)
```

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
let clock = time::timer(duration:1.s, true)
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

// ? propagates to nearest catch
// $ swallows error (logs warning, returns never)
a[100]$                           // won't crash, just skips
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
val add: fn(i64, i64) -> i64;

/// Subtract
val sub: fn(i64, i64) -> i64;

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
val make: fn(i64) -> Counter;
val get: fn(Counter) -> i64;
val increment: fn(#trig: Any, &Counter) -> null;
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

**map**: `insert`, `remove`, `get`, `contains_key`, `keys`, `values`,
`len`, `iter`, `fold`

**time**: `timer(timeout, repeat)`

**re**: `regex`, `is_match`, `find`, `captures`, `replace`,
`replace_all`, `split`

**rand**: `random`, `range`

**fs**: `read`, `write`, `list`, `exists`

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

**Padding**: `` `All(f64) ``, `` `Xy(f64,f64) ``, `` `Ltrb(f64,f64,f64,f64) ``

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
let clock = time::timer(duration:1.s, true)
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
