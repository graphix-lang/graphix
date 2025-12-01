# Graphix

A reactive dataflow programming language for building user interfaces
and network applications.

[Join us on Discord](https://discord.gg/bQv4gNR8WK)

## What is Graphix?

Graphix is a statically-typed functional programming language that
compiles to reactive dataflow graphs. Unlike traditional imperative
languages, Graphix programs automatically propagate changes through
dependencies—when a value updates, all computations that depend on it
update automatically.

This makes Graphix particularly well-suited for:
- **User Interfaces** - Rich, interactive applications
- **Network Programming** - Reactive data streams with [netidx](https://netidx.github.io/netidx-book)
- **Real-time Data Processing** - Automatic updates when data changes

## Key Features

- **Reactive at the Language Level** - Changes propagate automatically through the dataflow graph
- **Strong Static Typing** - Type inference with structural type discipline
- **Parametric Polymorphism** - Generic types with constraints
- **Algebraic Data Types** - Structs, variants, and pattern matching
- **First-Class Functions** - Closures and higher-order functions
- **Comprehensive TUI Library** - Built on [ratatui](https://github.com/ratatui-org/ratatui) with full widget support
- **Module System** - Load code from files, netidx, or dynamically at runtime

## Quick Example

Here's a simple reactive counter that updates every second:

```graphix
use tui;
use tui::text;

let count = 0;
let timer = time::timer(duration:1.s, true);
count <- timer ~ (count + 1);

text(&"Count: [count]")
```

The `<-` operator (connect) schedules updates for the next cycle. When
`timer` fires, `count` increments, and the text widget automatically
displays the new value. The sample operator `~` returns the rhs when
the lhs updates.

![Graphix TUI Example](book/src/ui/tui/media/overview_first.gif)

## Installation

### Prerequisites

**Rust** - Install from [rustup.rs](https://rustup.rs)

**Linux only:**
- Debian/Ubuntu: `clang`, `libkrb5-dev`
- Fedora: `clang-devel`, `krb5-devel`

### Install Graphix

```bash
cargo install graphix-shell
```

### Optional: Netidx

For full networking capabilities, install and set up [netidx](https://netidx.github.io/netidx-book):

```bash
cargo install netidx-tools
```

## Getting Started

### Interactive REPL

Start the Graphix shell:

```bash
graphix
```

Try some expressions:

```graphix
〉2 + 2
-: i64
4

〉"Hello, [10 * 5]!"
-: string
"Hello, 50!"
```

Press `Ctrl+C` to stop running expressions.

### Run a File

Create `hello.gx`:

```graphix
use tui;
use tui::text;

let count = 0;
let timer = time::timer(duration:1.s, true);
count <- timer ~ (count + 1);

text(&"Count: [count]")
```

Run it:

```bash
graphix hello.gx
```

Press `Ctrl+C` to exit.

## TUI Examples

Graphix includes a comprehensive terminal UI library with support for:

### Interactive Tables
![Table Example](book/src/ui/tui/media/table_interactive.gif)

### Dynamic Charts
![Canvas Animation](book/src/ui/tui/media/canvas_animated.gif)

### Scrollable Content
![Paragraph Scrolling](book/src/ui/tui/media/scroll_paragraph.gif)

### Focus Management
![Layout Focus](book/src/ui/tui/media/layout_focus.gif)

## Documentation

📚 **[Full Documentation](https://graphix-lang.github.io/graphix/)** (or build locally with `mdbook`)

### Quick Links
- [Getting Started Tutorial](https://graphix-lang.github.io/graphix/getting_started.html)
- [Core Language](https://graphix-lang.github.io/graphix/core/overview.html)
- [Functions](https://graphix-lang.github.io/graphix/functions/overview.html)
- [Building TUIs](https://graphix-lang.github.io/graphix/ui/tui/overview.html)
- [Standard Library](https://graphix-lang.github.io/graphix/stdlib/overview.html)
- [Embedding Graphix](https://graphix-lang.github.io/graphix/embedding/overview.html)

## Language Highlights

### Pattern Matching with Select

```graphix
let status: [i64, string, null] = 42;

select status {
  i64 as n if n > 0 => "positive number: [n]",
  i64 as n => "non positive number: [n]",
  string as s => "got string: [s]",
  null as _ => "no value"
}
```

### Error Handling with Try/Catch

```graphix
try
  let arr = [1, 2, 3];
  arr[10]?
catch(e) => println("Error: [e]")
```

### Reactive Network Subscriptions

```graphix
let value = cast<i64>(net::subscribe("/sensor/temperature")?)?;
let fahrenheit = value * ((9 / 5) + 32);

text(&"Temperature: [fahrenheit]°F")
```

### Recursive Types

```graphix
type List<'a> = [`Cons('a, List<'a>), `Nil];

let rec map = |l: List<'a>, f: fn('a) -> 'b| -> List<'b>
  select l {
    `Cons(v, tl) => `Cons(f(v), map(tl, f)),
    `Nil => `Nil
  }
```

## Project Structure

This is a Rust workspace with four main crates:

- **graphix-compiler** - The compiler (parsing, type checking, graph generation)
- **graphix-rt** - General-purpose runtime for executing dataflow graphs
- **graphix-stdlib** - Standard library
- **graphix-shell** - REPL, CLI tool, and TUI widgets

## Building from Source

```bash
# Clone the repository
git clone https://github.com/graphix-lang/graphix.git
git clone https://github.com/netidx/netidx.git
cd graphix

# Build the workspace
cargo build --release

# Run tests
cargo test

# Build documentation
cd book
mdbook build -d ../docs
```

## Contributing

Graphix is under active development. Contributions are welcome!

## Examples

The `book/src/examples/` directory contains runnable examples:

```bash
# Run a TUI example
graphix book/src/examples/tui/barchart_basic.gx

# Check syntax without running
graphix --check book/src/examples/tui/chart_multi.gx
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.
